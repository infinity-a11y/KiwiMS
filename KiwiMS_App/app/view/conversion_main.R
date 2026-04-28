# app/view/conversion_card.R

box::use(
  shiny[moduleServer, NS],
  bslib[nav_insert],
)

box::use(
  app /
    logic /
    conversion_ui[
      conc_unit_input_ui,
      time_unit_input_ui,
      keybind_menu_ui,
      table_legend,
      sample_table_legend,
      conversion_declaration_ui,
      binding_results_ui,
      ki_kinact_results_ui,
      ki_kinact_concentrations_tabs,
    ],
  app /
    logic /
    conversion_functions[
      add_kobs_binding_result,
      add_ki_kinact_result,
      sample_handsontable,
      prot_comp_handsontable,
      set_selected_tab,
      format_scientific,
      make_binding_plot,
      multiple_spectra,
      render_hits_table,
      filter_hits_table,
      checkboxColumn,
      js_code_gen,
      new_sample_table,
      compute_replicate_labels,
      confirm_ui_changes,
      edit_ui_changes,
      table_observe,
      clean_prot_comp_table,
      clean_sample_table,
      handle_file_upload,
      fill_sample_table,
      transform_hits,
      get_cmp_colorScale,
      get_contrast_color,
      label_smart_clean,
      filter_color_list,
      render_table_view,
      filter_table_view,
      make_kobs_plot,
      empty_prot_comp_tbl,
      read_decon_metadata,
      read_decon_result,
      validate_decon_db,
      cmp_compound_distribution,
      prot_compound_distribution,
      smpl_compound_distribution,
    ],
  app /
    logic /
    helper_functions[
      safe_observe,
    ],
  app / logic / deconvolution_functions[spectrum_plot, ],
  app /
    logic /
    plot_download[setup_plot_dl, setup_table_dl, prepare_hits_export],
  app / logic / logging[get_session_prefix],
  app /
    logic /
    conversion_constants[
      empty_protein_table,
      chart_js,
      sequential_scales,
      qualitative_scales,
      gradient_scales,
      hits_table_names,
      popover_autoclose,
    ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  shiny::div(
    class = "conversion-main-spinner",
    shinycssloaders::withSpinner(
      shiny::uiOutput(ns("conversion_ui")),
      type = 1,
      color = "#7777f9"
    )
  )
}

#' @export
server <- function(
  id,
  conversion_sidebar_vars,
  deconvolution_main_vars,
  config_file
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Set file upload limit
    options(shiny.maxRequestSize = 10000 * 1024^2)

    # Conversion Declarations/Initiation ----

    ## Reactive variables ----

    # Reactive values declaration_vars
    declaration_vars <- shiny::reactiveValues(
      protein_table_active = TRUE,
      protein_table_status = FALSE,
      protein_table_disabled = FALSE,
      compound_table = NULL,
      compound_table_active = TRUE,
      compound_table_status = FALSE,
      compound_table_disabled = FALSE,
      sample_tab = NULL,
      sample_table_active = TRUE,
      sample_table_status = FALSE,
      samples_confirmed = FALSE,
      conversion_ready = FALSE,
      result = NULL
    )

    # Table render trigger reactive values
    protein_table_trigger <- shiny::reactiveVal(0)
    compound_table_trigger <- shiny::reactiveVal(0)
    sample_table_trigger <- shiny::reactiveVal(0)
    render_trigger <- shiny::reactiveVal(0)
    trigger_ki_kinact <- shiny::reactiveVal(0L)
    manual_render_spectrum <- shiny::reactiveVal(0L)
    manual_render_cmp_spectrum <- shiny::reactiveVal(0L)

    # Apply initial disabled styling for samples_fileinput after DOM is ready
    session$onFlushed(
      function() {
        shinyjs::addClass(
          selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
          class = "custom-disable"
        )
        shinyjs::addClass(
          selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
          class = "custom-disable"
        )
      },
      once = TRUE
    )

    # Prepare waiter spinner object
    w <- waiter::Waiter$new(
      id = ns("samples_table"),
      html = waiter::spin_wave()
    )

    ## Reactive functions ----
    # Throttled reactive for sample declaration table input
    tolerance <- shiny::reactive({
      peak_tol <- conversion_sidebar_vars$peak_tolerance()
      if (is.null(peak_tol)) 3 else peak_tol
    }) |>
      shiny::debounce(750)

    # Throttled reactive for protein declaration table input
    protein_table_input <- shiny::reactive({
      proteins_table <- input$proteins_table
      shiny::req(proteins_table)
      suppressWarnings({
        tab <- rhandsontable::hot_to_r(proteins_table)
      })

      # Trim whitespace
      dplyr::mutate(tab, across(where(is.character), trimws))
    }) |>
      shiny::debounce(millis = 500)

    # Throttled reactive for compound declaration table input
    compound_table_input <- shiny::reactive({
      compounds_table <- input$compounds_table
      shiny::req(compounds_table)
      suppressWarnings({
        tab <- rhandsontable::hot_to_r(compounds_table)
      })

      # Trim whitespace
      dplyr::mutate(tab, across(where(is.character), trimws))
    }) |>
      shiny::debounce(millis = 500)

    # Throttled reactive for sample declaration table input
    sample_table_input <- shiny::reactive({
      samples_table <- input$samples_table
      if (!is.null(samples_table)) {
        suppressWarnings(
          rhandsontable::hot_to_r(
            samples_table
          )
        )
      } else {
        NULL
      }
    }) |>
      shiny::debounce(millis = 500)

    ## Empty default tables ----
    protein_table_data <- shiny::reactiveVal(empty_prot_comp_tbl(
      type = "Protein"
    ))
    compound_table_data <- shiny::reactiveVal(empty_prot_comp_tbl(
      type = "Compound"
    ))
    sample_table_data <- shiny::reactiveVal()

    # Helper: compute and inject Replicate column (right after Sample)
    add_replicate_col <- function(tbl, config = NULL) {
      tbl$Replicate <- compute_replicate_labels(tbl$Sample, config)
      tbl[, c("Sample", "Replicate", setdiff(names(tbl), c("Sample", "Replicate")))]
    }

    # Helper: auto-fill sample table columns from config file
    apply_config_autofill <- function(tbl, cfg) {
      for (i in seq_len(nrow(tbl))) {
        sample_name <- tbl$Sample[i]
        match_idx <- which(cfg$Sample == sample_name)
        if (length(match_idx) == 1) {
          m <- cfg[match_idx, , drop = FALSE]
          if ("Protein" %in% names(cfg)) {
            val <- trimws(as.character(m$Protein))
            if (!is.na(val) && nchar(val) > 0) tbl$Protein[i] <- val
          }
          for (j in 1:5) {
            col_cfg <- paste0("Compound_", j)
            col_tbl <- paste0("Compound ", j)
            if (col_cfg %in% names(cfg) && col_tbl %in% names(tbl)) {
              val <- trimws(as.character(m[[col_cfg]]))
              if (!is.na(val) && nchar(val) > 0) tbl[[col_tbl]][i] <- val
            }
          }
          if (
            "Concentration" %in%
              names(tbl) &&
              "Compound_Concentration" %in% names(cfg)
          ) {
            val <- m$Compound_Concentration
            if (!is.na(val)) tbl$Concentration[i] <- as.numeric(val)
          }
          if ("Time" %in% names(tbl) && "Incubation_Time" %in% names(cfg)) {
            val <- m$Incubation_Time
            if (!is.na(val)) tbl$Time[i] <- as.numeric(val)
          }
        }
      }
      tbl
    }

    ## Concentration/Time UI ----
    # Conditional adaption of concentration/time input UI
    safe_observe(
      observer_name = "Conditional Adaption of Concentration/Time Input UI",
      handler_fn = function() {
        if (isTRUE(conversion_sidebar_vars$run_ki_kinact())) {
          shinyjs::removeClass(
            selector = ".unit-selectors .form-group .bootstrap-select",
            class = "custom-disable"
          )
          shinyjs::removeClass(
            selector = ".unit-selectors label",
            class = "custom-disable"
          )
        } else {
          shinyjs::addClass(
            selector = ".unit-selectors .form-group .bootstrap-select",
            class = "custom-disable"
          )
          shinyjs::addClass(
            selector = ".unit-selectors label",
            class = "custom-disable"
          )
        }
      }
    )

    ## Data table input events ----
    # Silently update table data table input status variables
    safe_observe(
      event_expr = input$proteins_table,
      observer_name = "Protein Table Input Status",
      handler_fn = function() {
        shiny::req(input$proteins_table)

        # Save current table input
        suppressWarnings({
          protein_table_data(rhandsontable::hot_to_r(input$proteins_table))
        })
      },
      priority = 100
    )

    safe_observe(
      event_expr = input$compounds_table,
      observer_name = "Compound Table Input Status",
      handler_fn = function() {
        shiny::req(input$compounds_table)

        # Save current table input
        suppressWarnings({
          compound_table_data(rhandsontable::hot_to_r(input$compounds_table))
        })
      },
      priority = 100
    )

    safe_observe(
      event_expr = input$samples_table,
      observer_name = "Samples Table Input Status",
      handler_fn = function() {
        shiny::req(input$samples_table)

        # Save current table input
        suppressWarnings({
          samples_table <- rhandsontable::hot_to_r(input$samples_table)
        })

        # Assign to reactive variable
        sample_table_data(samples_table)
      },
      priority = 100
    )

    ## Conversion Declaration UI ----
    output$conversion_ui <- shiny::renderUI(conversion_declaration_ui(ns))

    ### UI Render Functions ----
    #### Table render functions ----
    output$proteins_table <- rhandsontable::renderRHandsontable({
      prot_comp_handsontable(
        protein_table_data(),
        tolerance = tolerance(),
        disabled = declaration_vars$protein_table_disabled
      )
    }) |>
      shiny::bindEvent(
        list(tolerance(), protein_table_trigger()),
        ignoreInit = FALSE
      )

    output$compounds_table <- rhandsontable::renderRHandsontable({
      prot_comp_handsontable(
        compound_table_data(),
        tolerance = tolerance(),
        disabled = declaration_vars$compound_table_disabled
      )
    }) |>
      shiny::bindEvent(
        list(tolerance(), compound_table_trigger()),
        ignoreInit = FALSE
      )

    output$samples_table <- rhandsontable::renderRHandsontable({
      shiny::req(
        sample_table_data()
      )

      sample_handsontable(
        tab = sample_table_data(),
        proteins = declaration_vars$protein_table$Protein,
        compounds = declaration_vars$compound_table$Compound,
        disabled = ifelse(
          is.null(protein_table_data()) ||
            is.null(compound_table_data()) ||
            isTRUE(declaration_vars$protein_table_active) ||
            isTRUE(declaration_vars$compound_table_active) ||
            isTRUE(declaration_vars$samples_confirmed),
          TRUE,
          FALSE
        )
      )
    }) |>
      shiny::bindEvent(
        list(sample_table_trigger()),
        ignoreInit = FALSE
      )

    #### Tab info text ----
    output$declaration_info_ui <- shiny::renderUI({
      shiny::req(input$tabs %in% c("Proteins", "Compounds", "Samples"))

      if (input$tabs == "Proteins") {
        hints <- "Upload a CSV|TSV|TXT|Excel file or manually enter names and mass values of the <strong>proteins</strong> into the table."
      } else if (input$tabs == "Compounds") {
        hints <- "Upload a CSV|TSV|TXT|Excel file or manually enter names and mass values of the <strong>compounds</strong> into the table."
      } else if (input$tabs == "Samples") {
        hints <- "Assign <strong>protein-compound complexes</strong> to deconvoluted samples."
      }
      # TODO
      # Add hints to result interface
      # else if (input$tabs == "Binding") {
      #   hints <- shiny::HTML(
      #     "Global fit of a concentration series of binding curves determining binding parameters for the selected complex."
      #   )
      # } else if (input$tabs == "Hits") {
      #   hints <- shiny::HTML(
      #     "The 'Hits' tab shows all signals assigned to the currently selected complex and respectively inferred parameters."
      #   )
      # } else {
      #   hints <- "%-Binding inferred from time series measurements of a single concentration."
      # }

      shiny::HTML(paste(
        '<i class="fa-solid fa-circle-info"></i> &nbsp;&nbsp;',
        hints
      ))
    })

    ## Table loading special events ----

    ### Result file input loading feedback ----
    shinyjs::onevent(
      "change",
      "samples_fileinput",
      {
        # Block UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "block";'
        ))

        # Disable result file input
        shinyjs::disable("samples_fileinput")
        shinyjs::addClass(
          selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
          class = "custom-disable"
        )
        shinyjs::addClass(
          selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
          class = "custom-disable"
        )

        # Update info text
        shinyjs::removeClass(
          "samples_table_info",
          "table-info-green"
        )
        shinyjs::removeClass(
          "samples_table_info",
          "table-info-red"
        )
        output$samples_table_info <- shiny::renderText(
          "Loading ..."
        )
      }
    )

    ### Table pasting feedback ----
    safe_observe(
      event_expr = input$table_paste_instant,
      observer_name = "Table Pasting Feedback",
      handler_fn = function() {
        # Block UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "block";'
        ))

        # Update info text
        shinyjs::removeClass(
          paste0(tolower(input$tabs), "_table_info"),
          "table-info-green"
        )
        shinyjs::removeClass(
          paste0(tolower(input$tabs), "_table_info"),
          "table-info-red"
        )
        output[[paste0(
          tolower(input$tabs),
          "_table_info"
        )]] <- shiny::renderText(
          "Loading ..."
        )
      }
    )

    ## Clear tables ----
    safe_observe(
      event_expr = input$clear_proteins,
      observer_name = "Clear Protein Table",
      handler_fn = function() {
        protein_table_data(empty_prot_comp_tbl(type = "Protein"))

        protein_table_trigger(protein_table_trigger() + 1)
      }
    )

    safe_observe(
      event_expr = input$clear_compounds,
      observer_name = "Clear Compound Table",
      handler_fn = function() {
        compound_table_data(empty_prot_comp_tbl(type = "Compound"))

        compound_table_trigger(compound_table_trigger() + 1)
      }
    )

    safe_observe(
      event_expr = input$clear_samples,
      observer_name = "Clear Sample Table",
      handler_fn = function() {
        sample_table_data(add_replicate_col(new_sample_table(
          result = declaration_vars$result,
          protein_table = declaration_vars$protein_table,
          compound_table = declaration_vars$compound_table,
          ki_kinact = conversion_sidebar_vars$run_ki_kinact()
        ), config_file()))
        sample_table_trigger(sample_table_trigger() + 1)
      }
    )

    ## File upload handler ----
    ### Protein table file upload ----
    safe_observe(
      event_expr = input$proteins_fileinput,
      observer_name = "Protein Table File Upload",
      handler_fn = function() {
        protein_table_input <- handle_file_upload(
          file_input = input$proteins_fileinput,
          type = "Protein",
          output = output,
          declaration_vars = declaration_vars
        )

        if (!is.null(protein_table_input)) {
          # Assign new table data to reactive variable and mark table status as TRUE to trigger table observer
          protein_table_data(protein_table_input)
          declaration_vars$protein_table_status <- TRUE

          # Set disable status to FALSE to allow confirm button activation
          # and table observer activation
          declaration_vars$protein_table_disabled <- FALSE

          # Trigger table render
          protein_table_trigger(protein_table_trigger() + 1)
        }
      }
    )

    ### Compound table file upload ----
    safe_observe(
      event_expr = input$compounds_fileinput,
      observer_name = "Compound Table File Upload",
      handler_fn = function() {
        compound_table_data(handle_file_upload(
          file_input = input$compounds_fileinput,
          type = "compound",
          output = output,
          declaration_vars = declaration_vars
        ))
        declaration_vars$compound_table_disabled <- FALSE
        compound_table_trigger(compound_table_trigger() + 1)
      }
    )

    ### Sample file upload ----
    safe_observe(
      event_expr = input$samples_fileinput,
      observer_name = "Sample Table File Upload",
      handler_fn = function() {
        shiny::req(
          declaration_vars$protein_table,
          declaration_vars$compound_table
        )

        # Read metadata from selected result DB (fast — sample names only)
        file_path <- file.path(input$samples_fileinput$datapath)
        db_err <- validate_decon_db(file_path)
        if (!is.null(db_err)) {
          shinyjs::runjs(paste0(
            'document.getElementById("blocking-overlay").style.display ',
            '= "none";'
          ))
          shinyjs::enable("samples_fileinput")
          shinyjs::removeClass(
            selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
            class = "custom-disable"
          )
          shinyjs::removeClass(
            selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
            class = "custom-disable"
          )
          output$samples_table_info <- shiny::renderText(
            "Add Deconvoluted Samples"
          )
          shinyWidgets::show_toast(db_err, type = "info", timer = 6000)
          return()
        }
        meta <- read_decon_metadata(file_path)
        declaration_vars$result <- c(meta, list(.db_path = file_path))

        # New table data
        sample_table_data(add_replicate_col(new_sample_table(
          result = declaration_vars$result,
          protein_table = declaration_vars$protein_table,
          compound_table = declaration_vars$compound_table,
          ki_kinact = conversion_sidebar_vars$run_ki_kinact()
        ), config_file()))
        sample_table_trigger(sample_table_trigger() + 1)
      }
    )

    ## Config autofill button state ----
    safe_observe(
      observer_name = "Use Config Button State",
      handler_fn = function() {
        can_use <- !is.null(config_file()) &&
          !isTRUE(declaration_vars$samples_confirmed) &&
          !is.null(sample_table_data()) &&
          nrow(sample_table_data()) > 0
        if (can_use) {
          shinyjs::enable("use_config")
        } else {
          shinyjs::disable("use_config")
        }
      }
    )

    ## Config autofill ----
    safe_observe(
      event_expr = input$use_config,
      observer_name = "Config Autofill",
      handler_fn = function() {
        shiny::req(input$use_config > 0)
        shiny::req(!is.null(sample_table_data()))
        shiny::req(nrow(sample_table_data()) > 0)
        shiny::req(!is.null(config_file()))

        cfg <- config_file()
        cleared_tbl <- sample_table_data()
        # Exclude Replicate from clearing — it is auto-computed, not user-entered
        non_sample_cols <- setdiff(names(cleared_tbl), c("Sample", "Replicate"))
        for (col in non_sample_cols) {
          cleared_tbl[[col]] <- if (is.numeric(cleared_tbl[[col]])) {
            NA_real_
          } else {
            ""
          }
        }

        # If config has concentration/time and columns are missing, add them before
        # autofill so apply_config_autofill can fill the values in one pass
        has_conc <- "Compound_Concentration" %in%
          names(cfg) &&
          any(!is.na(cfg$Compound_Concentration))
        has_time <- "Incubation_Time" %in%
          names(cfg) &&
          any(!is.na(cfg$Incubation_Time))
        if (
          has_conc &&
            has_time &&
            !all(c("Concentration", "Time") %in% names(cleared_tbl))
        ) {
          cleared_tbl$Concentration <- NA_real_
          cleared_tbl$Time <- NA_real_
        }

        new_tbl <- apply_config_autofill(cleared_tbl, cfg)
        new_tbl <- add_replicate_col(new_tbl, cfg)

        if (!identical(new_tbl, sample_table_data())) {
          sample_table_data(new_tbl)
          sample_table_trigger(sample_table_trigger() + 1)
        }

        if (
          has_conc &&
            has_time &&
            !isTRUE(conversion_sidebar_vars$run_ki_kinact())
        ) {
          trigger_ki_kinact(trigger_ki_kinact() + 1L)
        }
      },
      priority = -5
    )

    ## Replicate column — refresh when config changes ----
    safe_observe(
      event_expr = config_file(),
      observer_name = "Replicate Column — Config Change",
      handler_fn = function() {
        tbl <- sample_table_data()
        if (is.null(tbl) || nrow(tbl) == 0) return()
        updated <- add_replicate_col(tbl, config_file())
        if (!identical(updated$Replicate, tbl$Replicate)) {
          sample_table_data(updated)
          sample_table_trigger(sample_table_trigger() + 1)
        }
      }
    )

    ## Table status observer ----
    ### Observe table status for protein table ----
    safe_observe(
      observer_name = "Protein Table Status Observer",
      handler_fn = function() {
        shiny::req(
          protein_table_input(),
          declaration_vars$protein_table_active
        )

        protein_table <- clean_prot_comp_table(
          tab = "Protein",
          table = protein_table_input(),
          full = FALSE
        )

        # Conditional observe actions
        declaration_vars$protein_table_status <- table_observe(
          tab = "proteins",
          table = protein_table,
          output = output,
          ns = ns
          #, tolerance = tolerance()
        )

        # Unblock UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "none";'
        ))
      },
      priority = 100
    )

    ### Observe table status for compound table ----
    safe_observe(
      observer_name = "Compound Table Status Observer",
      handler_fn = function() {
        shiny::req(
          compound_table_input(),
          declaration_vars$compound_table_active
          #,conversion_sidebar_vars$peak_tolerance()
        )

        compound_table <- clean_prot_comp_table(
          tab = "Compound",
          table = compound_table_input(),
          full = FALSE
        )

        # Conditional observe actions
        declaration_vars$compound_table_status <- table_observe(
          tab = "compounds",
          table = compound_table,
          output = output,
          ns = ns
          #,tolerance = tolerance()
        )

        # Unblock UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "none";'
        ))
      },
      priority = 100
    )

    ### Observe table status for samples table ----
    safe_observe(
      observer_name = "Sample Table Status Observer",
      handler_fn = function() {
        samples_table_input <- sample_table_input()

        # Conditional observe actions
        if (
          isTRUE(declaration_vars$protein_table_active) ||
            isTRUE(declaration_vars$compound_table_active)
        ) {
          # Update info text
          output$samples_table_info <- shiny::renderText({
            "Enter Proteins and Compounds first"
          })
          shinyjs::removeClass(
            "samples_table_info",
            "table-info-green"
          )
          shinyjs::removeClass(
            "samples_table_info",
            "table-info-red"
          )

          # Disable file upload
          shinyjs::disable("samples_fileinput")
          shinyjs::addClass(
            selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
            class = "custom-disable"
          )
          shinyjs::addClass(
            selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
            class = "custom-disable"
          )

          # Disable confirm button
          shinyjs::disable("confirm_samples")
        } else if (is.null(samples_table_input)) {
          # if protein/compound declaration confirmed

          output$samples_table_info <- shiny::renderText({
            "Add Deconvoluted Samples"
          })

          # Enable file upload
          shinyjs::enable("samples_fileinput")
          shinyjs::removeClass(
            selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
            class = "custom-disable"
          )
          shinyjs::removeClass(
            selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
            class = "custom-disable"
          )

          # Disable confirm button
          shinyjs::disable("confirm_samples")
        } else if (isTRUE(declaration_vars$sample_table_active)) {
          # Enable file upload
          shinyjs::enable("samples_fileinput")
          shinyjs::removeClass(
            selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
            class = "custom-disable"
          )
          shinyjs::removeClass(
            selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
            class = "custom-disable"
          )

          declaration_vars$sample_table_status <- table_observe(
            tab = "samples",
            table = clean_sample_table(
              samples_table_input
            ),
            output = output,
            ns = ns,
            proteins = declaration_vars$protein_table$Protein,
            compounds = declaration_vars$compound_table$Compound
          )
        }

        # Unblock UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "none";'
        ))
      },
      priority = 100
    )

    ## Event activate ki_kinact analysis ----
    safe_observe(
      event_expr = conversion_sidebar_vars$run_ki_kinact(),
      observer_name = "Ki/kinact Activation",
      handler_fn = function() {
        shiny::req(
          input$samples_table,
          sample_table_data()
        )

        has_conc_time <- any(grepl(
          "^Concentration",
          names(sample_table_data())
        )) &&
          any(grepl("^Time", names(sample_table_data())))

        if (
          isTRUE(declaration_vars$samples_confirmed) &&
            isTRUE(conversion_sidebar_vars$run_ki_kinact())
        ) {
          # Make UI changes
          edit_ui_changes(
            tab = "Samples",
            session = session,
            output = output
          )

          # New editable full sample table data
          declaration_vars$samples_confirmed <- FALSE

          # Activate table observer
          declaration_vars$sample_table_active <- TRUE

          # Fill sample table — use grepl-based detection so unit-suffixed
          # names like "Concentration [M]" / "Time [s]" are handled correctly
          sample_table <- fill_sample_table(
            sample_table_data(),
            ki_kinact = has_conc_time
          )

          # Only append empty Conc/Time when they were absent before;
          # fill_sample_table already restores them when ki_kinact = TRUE
          if (!has_conc_time) {
            sample_table_data(cbind(
              sample_table,
              Concentration = as.numeric(NA),
              Time = as.numeric(NA)
            ))
          } else {
            sample_table_data(sample_table)
          }
        } else if (isTRUE(conversion_sidebar_vars$run_ki_kinact())) {
          if (!has_conc_time) {
            sample_table_data(cbind(
              sample_table_data(),
              Concentration = as.numeric(NA),
              Time = as.numeric(NA)
            ))
          }
        } else {
          sample_table_data(sample_table_data()[,
            -grep("Concentration|Time", names(sample_table_data()))
          ])
        }

        sample_table_trigger(sample_table_trigger() + 1)
      }
    )

    ## Edit button event ----
    safe_observe(
      event_expr = list(
        input$edit_proteins,
        input$edit_compounds,
        input$edit_samples
      ),
      observer_name = "Edit Table Event",
      handler_fn = function() {
        shiny::req(input$tabs)
        shiny::req(
          isTRUE(input$edit_proteins > 0) ||
            isTRUE(input$edit_compounds > 0) ||
            isTRUE(input$edit_samples > 0)
        )

        # If edit applied always activate edit mode for sample table if present
        if (!is.null(input$samples_table)) {
          # Make UI changes
          edit_ui_changes(
            tab = "Samples",
            session = session,
            output = output
          )

          # New editable full sample table data
          declaration_vars$samples_confirmed <- FALSE
          sample_table_data(fill_sample_table(
            sample_table_data(),
            ki_kinact = conversion_sidebar_vars$run_ki_kinact()
          ))
          sample_table_trigger(sample_table_trigger() + 1)

          # Activate table observer
          declaration_vars$sample_table_active <- TRUE
        }

        # Edit Protein/Compound
        if (input$tabs == "Proteins") {
          # Make UI changes
          edit_ui_changes(
            tab = input$tabs,
            session = session,
            output = output
          )

          # Trigger re-render of table with changes
          protein_table_data(clean_prot_comp_table(
            tab = "Protein",
            table = protein_table_input(),
            full = TRUE
          ))
          declaration_vars$protein_table_disabled <- FALSE
          protein_table_trigger(protein_table_trigger() + 1)

          # Make table observer active
          declaration_vars$protein_table_active <- TRUE
        } else if (input$tabs == "Compounds") {
          # Make UI changes
          edit_ui_changes(
            tab = input$tabs,
            session = session,
            output = output
          )

          # Trigger re-render of table with changes
          compound_table_data(clean_prot_comp_table(
            tab = "Compound",
            table = compound_table_input(),
            full = TRUE
          ))
          declaration_vars$compound_table_disabled <- FALSE
          compound_table_trigger(compound_table_trigger() + 1)

          # Make table observer active
          declaration_vars$compound_table_active <- TRUE
        }
      }
    )

    ## Confirm button event ----
    safe_observe(
      event_expr = list(
        input$confirm_proteins,
        input$confirm_compounds,
        input$confirm_samples
      ),
      observer_name = "Confirm Table Event",
      handler_fn = function() {
        # Guard against spurious fires when buttons are re-initialized to 0
        shiny::req(
          isTRUE(input$confirm_proteins > 0) ||
            isTRUE(input$confirm_compounds > 0) ||
            isTRUE(input$confirm_samples > 0)
        )

        # Block UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "block";'
        ))

        if (input$tabs == "Proteins" && declaration_vars$protein_table_status) {
          protein_table <- clean_prot_comp_table(
            tab = "Protein",
            table = protein_table_input(),
            full = FALSE
          )
          declaration_vars$protein_table <- protein_table
          protein_table_data(protein_table)
          declaration_vars$protein_table_disabled <- TRUE
          protein_table_trigger(protein_table_trigger() + 1)

          # Mark UI as done
          confirm_ui_changes(
            tab = input$tabs,
            session = session,
            output = output
          )

          # Render sample table with new input
          if (!is.null(input$samples_table)) {
            sample_table_data(add_replicate_col(
              new_sample_table(
                result = declaration_vars$result,
                protein_table = protein_table,
                compound_table = declaration_vars$compound_table,
                ki_kinact = conversion_sidebar_vars$run_ki_kinact()
              ),
              config_file()
            ))
            sample_table_trigger(sample_table_trigger() + 1)
          }

          # Jump to next tab depending on compound table status
          if (isTRUE(declaration_vars$compound_table_active)) {
            set_selected_tab("Compounds", session)
          } else {
            set_selected_tab("Samples", session)
          }

          # Inactivate table observer
          declaration_vars$protein_table_active <- FALSE
        } else if (
          input$tabs == "Compounds" && declaration_vars$compound_table_status
        ) {
          compound_table <- clean_prot_comp_table(
            tab = "Compound",
            table = compound_table_input(),
            full = FALSE
          )
          declaration_vars$compound_table <- compound_table
          compound_table_data(compound_table)
          declaration_vars$compound_table_disabled <- TRUE
          compound_table_trigger(compound_table_trigger() + 1)

          # Mark UI as done
          confirm_ui_changes(
            tab = input$tabs,
            session = session,
            output = output
          )

          # Render sample table with new input
          if (!is.null(input$samples_table)) {
            sample_table_data(add_replicate_col(
              new_sample_table(
                result = declaration_vars$result,
                protein_table = declaration_vars$protein_table,
                compound_table = compound_table,
                ki_kinact = conversion_sidebar_vars$run_ki_kinact()
              ),
              config_file()
            ))
            sample_table_trigger(sample_table_trigger() + 1)
          }

          # Jump to next tab depending on compound table status
          if (isTRUE(declaration_vars$protein_table_active)) {
            set_selected_tab("Proteins", session)
          } else {
            set_selected_tab("Samples", session)
          }

          # Inactivate table observer
          declaration_vars$compound_table_active <- FALSE
        } else if (
          input$tabs == "Samples" && declaration_vars$sample_table_status
        ) {
          # Get clean non-NA sample table
          sample_table <- clean_sample_table(
            sample_table_input(),
            units = list(conc = input$conc_unit, time = input$time_unit)
          )

          # Assign table to reactive variables
          declaration_vars$sample_table <- sample_table
          sample_table_data(sample_table)

          # Trigger re-rendering of sample table
          declaration_vars$samples_confirmed <- TRUE
          sample_table_trigger(sample_table_trigger() + 1)

          # Mark UI as done
          confirm_ui_changes(
            tab = input$tabs,
            session = session,
            output = output
          )

          # Inactivate table observer
          declaration_vars$sample_table_active <- FALSE
        }

        # Unblock UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "none";'
        ))
      }
    )

    ## Observe conversion readiness ----
    safe_observe(
      observer_name = "Conversion Readiness Observer",
      handler_fn = function() {
        if (
          isTRUE(declaration_vars$protein_table_status) &
            isTRUE(
              declaration_vars$compound_table_status
            ) &
            isTRUE(declaration_vars$sample_table_status) &
            isFALSE(declaration_vars$sample_table_active)
        ) {
          declaration_vars$conversion_ready <- TRUE
        } else {
          declaration_vars$conversion_ready <- FALSE
        }
      }
    )

    ## Continuation from deconvolution to conversion ----
    ### Transfer results from deconvolution to sample table ----
    safe_observe(
      event_expr = deconvolution_main_vars$continue_conversion(),
      observer_name = "Deconvolution Results Transfer",
      handler_fn = function() {
        # If present sample table ask confirmation
        if (!is.null(input$samples_table)) {
          # Switch to samples table tab
          set_selected_tab("Samples", session)

          # Show confirmation dialogue
          shiny::showModal(
            shiny::div(
              class = "conversion-modal",
              shiny::modalDialog(
                title = htmltools::tags$span("Upload new result file?"),
                easyClose = FALSE,
                footer = shiny::tagList(
                  shiny::actionButton(
                    ns("conversion_cont_cancel"),
                    "Cancel",
                    width = "auto"
                  ),
                  shiny::actionButton(
                    ns("conversion_cont_conf"),
                    "Continue",
                    class = "load-db",
                    width = "auto"
                  )
                ),
                shiny::fluidRow(
                  shiny::br(),
                  shiny::column(
                    width = 12,
                    shiny::div(
                      shiny::p(
                        shiny::HTML(
                          '<b>Sample table is already present.</b><br><br>'
                        )
                      ),
                      shiny::div(
                        class = "info-symbol-container",
                        shiny::HTML(paste0(
                          '<i class="fa-solid fa-circle-exclamation" style="font-size:',
                          '1em; color:black; margin-right: 10px;"></i>'
                        )),
                        shiny::HTML(
                          'Continuing will delete all entries and reevaluate the table with the new results.'
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        } else {
          # Block UI
          shinyjs::runjs(paste0(
            'document.getElementById("blocking-overlay").style.display ',
            '= "block";'
          ))

          # Read metadata only from result DB (fast — sample names only)
          db_path <- deconvolution_main_vars$continue_conversion()
          db_err <- validate_decon_db(db_path)
          if (!is.null(db_err)) {
            shinyjs::runjs(paste0(
              'document.getElementById("blocking-overlay").style.display ',
              '= "none";'
            ))
            shinyWidgets::show_toast(db_err, type = "info", timer = 6000)
            return()
          }
          meta <- read_decon_metadata(db_path)
          declaration_vars$result <- c(meta, list(.db_path = db_path))

          # New table data
          sample_table_data(add_replicate_col(new_sample_table(
            result = declaration_vars$result,
            protein_table = declaration_vars$protein_table,
            compound_table = declaration_vars$compound_table,
            ki_kinact = conversion_sidebar_vars$run_ki_kinact()
          ), config_file()))
          sample_table_trigger(sample_table_trigger() + 1)
        }

        # If user already declared proteins/compounds switch to Samples tab
        if (
          isFALSE(declaration_vars$protein_table_active) ||
            isFALSE(declaration_vars$compound_table_active)
        ) {
          set_selected_tab("Samples", session)
        }

        # Unblock UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "none";'
        ))
      }
    )

    ### User confirms samples table overwrite ----
    safe_observe(
      event_expr = input$conversion_cont_conf,
      observer_name = "Samples Table Overwrite",
      handler_fn = function() {
        # UI Blocking
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "block";'
        ))

        result_list <- conversion_sidebar_vars$result_list()

        if (!is.null(result_list)) {
          # Show declaration interface
          output$conversion_ui <- shiny::renderUI(
            conversion_declaration_ui(
              ns,
              proteins_status = "confirmed",
              compounds_status = "confirmed"
            )
          )

          # # Reset reactive variables
          # conversion_vars$modified_results <- NULL
          # conversion_vars$select_concentration <- NULL
          # conversion_vars$conc_colors <- NULL
          # conversion_vars$expand_helper <- FALSE
          # conversion_vars$hits_summary <- NULL
          # hits_summary <- NULL
        }

        # Activate sample table observer and unconfirm status variable
        declaration_vars$sample_table_active <- TRUE
        declaration_vars$samples_confirmed <- FALSE

        # Read metadata only from result DB (fast — sample names only)
        db_path <- deconvolution_main_vars$continue_conversion()
        meta <- read_decon_metadata(db_path)
        declaration_vars$result <- c(meta, list(.db_path = db_path))

        # New table data
        sample_table_data(add_replicate_col(new_sample_table(
          result = declaration_vars$result,
          protein_table = declaration_vars$protein_table,
          compound_table = declaration_vars$compound_table,
          ki_kinact = conversion_sidebar_vars$run_ki_kinact()
        ), config_file()))
        sample_table_trigger(sample_table_trigger() + 1)

        protein_table_active <- declaration_vars$protein_table_active
        compound_table_active <- declaration_vars$compound_table_active

        # Post-Render Updates
        session$onFlushed(
          function() {
            # Prepare sample table declaration tab
            # Change buttons
            shiny::updateActionButton(
              session = session,
              "confirm_samples",
              icon = shiny::icon("bookmark")
            )
            shinyjs::enable("confirm_samples")
            shinyjs::disable("edit_samples")

            # Enable file upload
            shinyjs::enable("samples_fileinput")
            shinyjs::removeClass(
              selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
              class = "custom-disable"
            )
            shinyjs::removeClass(
              selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
              class = "custom-disable"
            )

            # Adapt table status tab indicator
            shinyjs::delay(
              250,
              shinyjs::runjs(
                'document.querySelector(".nav-link[data-value=\'Samples\']").classList.remove("done");'
              )
            )

            # Adapt protein and compound declaration tabs
            if (!is.null(result_list)) {
              if (isFALSE(compound_table_active)) {
                shinyjs::delay(
                  250,
                  {
                    shinyjs::disable("confirm_compounds")
                    shinyjs::disable("clear_compounds")
                    shinyjs::enable("edit_compounds")
                    shinyjs::runjs(
                      'document.querySelector(".nav-link[data-value=\'Compounds\']").classList.add("done");'
                    )
                  }
                )
              }

              if (isFALSE(protein_table_active)) {
                shinyjs::delay(
                  250,
                  {
                    shinyjs::disable("confirm_proteins")
                    shinyjs::disable("clear_proteins")
                    shinyjs::enable("edit_proteins")
                    shinyjs::runjs(
                      'document.querySelector(".nav-link[data-value=\'Proteins\']").classList.add("done");'
                    )
                  }
                )
              }
            }

            # Select samples tab
            set_selected_tab("Samples", session)

            # Cleanup interface
            shiny::removeModal()

            # Unblock UI
            shinyjs::runjs(paste0(
              'document.getElementById("blocking-overlay").style.display ',
              '= "none";'
            ))
          },
          once = TRUE
        )
      }
    )

    ### User cancels samples table overwrite ----
    shiny::observeEvent(input$conversion_cont_cancel, {
      # Remove dialogue window
      shiny::removeModal()
    })

    # Conversion Results ------------------

    ## Reactive variables ----
    # Reactive values conversion_vars
    conversion_vars <- shiny::reactiveValues(
      modified_results = NULL,
      select_concentration = NULL,
      conc_colors = NULL,
      expand_helper = FALSE,
      hits_summary = NULL
    )

    # Reactive value to track current hits data frame
    relbinding_hits_current <- shiny::reactiveVal()
    relbinding_hits_raw <- shiny::reactiveVal()
    kikinact_hits_current <- shiny::reactiveVal()
    kikinact_hits_raw <- shiny::reactiveVal()

    # Raw data frames for table exports
    samples_table_view_raw <- shiny::reactiveVal()
    compounds_table_view_raw <- shiny::reactiveVal()
    proteins_table_view_raw <- shiny::reactiveVal()
    kobs_result_raw <- shiny::reactiveVal()

    ## Reactive functions ----
    # Infer Ki/kinact result from selected samples
    ki_kinact_result <- shiny::reactive({
      shiny::req(conversion_sidebar_vars$result_list())

      if (is.null(conversion_vars$modified_results)) {
        result_list <- conversion_sidebar_vars$result_list()
      } else {
        # Block UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "block";'
        ))

        Sys.sleep(1)

        result_list <- conversion_vars$modified_results

        # Unblock UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "none";'
        ))
      }

      return(result_list$ki_kinact_result$Params)
    })

    ## Render conversion results interface ----

    # Activate observer on analysis launch
    safe_observe(
      observer_name = "Results Observer Activation",
      handler_fn = function() {
        shiny::req(results_observer)

        if (is.null(conversion_sidebar_vars$result_list())) {
          results_observer$suspend()
        } else {
          results_observer$resume()
        }
      }
    )

    # Observer rendering UI on conditions
    results_observer <- safe_observe(
      observer_name = "Conditional Results Rendering",
      handler_fn = function() {
        # Block UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "block";'
        ))

        result_list <- conversion_sidebar_vars$result_list()

        analysis_select <- conversion_sidebar_vars$analysis_select()
        shiny::req(length(analysis_select) > 0)

        shiny::isolate({
          run_ki_kinact <- conversion_sidebar_vars$run_ki_kinact()
          select_concentration <- conversion_vars$select_concentration
        })

        if (is.null(result_list)) {
          #### Reset results ui elements ----

          # Null kinetics interface
          output$kikinact_hits_tab <- NULL
          output$kinact <- NULL
          output$Ki <- NULL
          output$Ki_kinact <- NULL
          output$kobs_result <- NULL
          output$binding_plot <- NULL
          output$kobs_plot <- NULL

          if (!is.null(conversion_vars$select_concentration)) {
            lapply(names(conversion_vars$select_concentration), function(id) {
              output[[paste0("concentration_tab", id)]] <- NULL
              output[[paste0("concentration_tab_", id, "_hits")]] <- NULL
              output[[paste0(
                "concentration_tab_",
                id,
                "_binding_plot"
              )]] <- NULL
              output[[paste0("concentration_tab_", id, "_spectra")]] <- NULL
            })
          }

          # Reset render trigger so bindEvent-guarded plots don't fire stale data
          render_trigger(0)
          manual_render_spectrum(0L)

          # Null binding interface
          output$relbinding_hits_tab <- NULL
          output$samples_selected_protein <- NULL
          output$samples_total_pct_binding <- NULL
          output$samples_compound_distribution_ui <- NULL
          output$samples_present_compounds_na <- NULL
          output$samples_compound_distribution <- NULL
          output$samples_annotated_spectrum <- NULL
          output$samples_table_view <- NULL
          output$compounds_selected_compound <- NULL
          output$compounds_total_pct_binding <- NULL
          output$compounds_compound_distribution_ui <- NULL
          output$compounds_compound_distribution <- NULL
          output$compounds_present_compounds_na <- NULL
          output$compounds_distribution_labels_ui <- NULL
          output$cmp_annotated_spectrum_na <- NULL
          output$cmp_annotated_spectrum_container <- NULL
          output$compounds_annotated_spectrum <- NULL
          output$compounds_spectrum_labels_ui <- NULL
          output$compounds_table_view <- NULL
          output$proteins_selected_protein <- NULL
          output$proteins_total_pct_binding <- NULL
          output$total_pct_prot_binding <- NULL
          output$proteins_present_compounds_ui <- NULL
          output$proteins_present_compounds_na <- NULL
          output$proteins_compound_distribution <- NULL
          output$protein_distribution_labels_ui <- NULL
          output$annotated_spectrum_container <- NULL
          output$proteins_annotated_spectrum <- NULL
          output$proteins_spectrum_labels_ui <- NULL
          output$proteins_table_view <- NULL
          output$color_variable_ui <- NULL

          #### Render declaration ui ----
          output$conversion_ui <- shiny::renderUI(
            conversion_declaration_ui(
              ns,
              proteins_status = "confirmed",
              compounds_status = "confirmed",
              samples_status = "confirmed"
            )
          )

          shinyjs::delay(250, {
            shinyjs::runjs(
              'document.querySelector(".nav-link[data-value=\'Proteins\']").classList.add("done");'
            )
            shinyjs::runjs(
              'document.querySelector(".nav-link[data-value=\'Compounds\']").classList.add("done");'
            )
            shinyjs::runjs(
              'document.querySelector(".nav-link[data-value=\'Samples\']").classList.add("done");'
            )
          })

          # Unblock UI
          shinyjs::runjs(paste0(
            'document.getElementById("blocking-overlay").style.display ',
            '= "none";'
          ))

          # Select samples tab
          set_selected_tab("Samples", session)
        } else if (!is.null(result_list)) {
          ### Compute hits summary ----
          hits_summary <- transform_hits(result_list$"hits_summary")

          # Get concentration and time units
          units <- c(
            names(hits_summary)[grep("Conc.", names(hits_summary))],
            names(hits_summary)[grep("Time", names(hits_summary))]
          )
          if (length(units) == 2) {
            names(units) <- c("Concentration", "Time")
          }

          conversion_vars$units <- units

          # Rearrange hits summary
          if (length(units) == 2) {
            hits_summary <- hits_summary |>
              dplyr::mutate(dplyr::across(all_of(unname(units)), as.numeric)) |>
              dplyr::arrange(
                `Protein`,
                `Cmp Name`,
                as.numeric(!!rlang::sym(units[["Concentration"]])),
                as.numeric(!!rlang::sym(units[["Time"]]))
              )
          } else {
            hits_summary <- hits_summary |>
              dplyr::arrange(
                `Protein`,
                `Cmp Name`,
                `Total %-Binding`,
                `%-Binding`
              )
          }

          #### Append truncated sample IDs ----
          # Create a mapping data frame
          mapping <- data.frame(
            original = unique(hits_summary$`Sample ID`),
            truncated = label_smart_clean(unique(
              hits_summary$`Sample ID`
            ))
          )

          # Add column with truncated IDs
          hits_summary$truncSample_ID <- mapping$truncated[match(
            hits_summary$`Sample ID`,
            mapping$original
          )]
          conversion_vars$hits_summary <- hits_summary

          ### Render result interfaces ----
          if (analysis_select == 1) {
            #### Render relative binding interface ----
            output$conversion_ui <- shiny::renderUI({
              binding_results_ui(ns, hits_summary)
            })

            ##### Hits tab ----
            ###### Hits table ----
            output$relbinding_hits_tab <- DT::renderDT(
              {
                shiny::req(
                  hits_summary,
                  input$relbinding_hits_tab_sample_select,
                  input$color_variable,
                  !is.null(input$truncate_names),
                  input$color_scale
                )

                # Arrange table
                num_sort_cols <- c()
                if ("Concentration" %in% names(units)) {
                  num_sort_cols <- c(num_sort_cols, units[["Concentration"]])
                }
                if ("Time" %in% names(units)) {
                  num_sort_cols <- c(num_sort_cols, units[["Time"]])
                }

                hits_table <- hits_summary |>
                  dplyr::arrange(
                    Protein,
                    dplyr::across(
                      dplyr::all_of(num_sort_cols),
                      ~ as.numeric(as.character(.x))
                    )
                  )

                # Prefiltering of  table
                hits_table <- filter_hits_table(
                  hits_table,
                  selected_cols = input$relbinding_hits_tab_col_select,
                  compounds = input$relbinding_hits_tab_compound_select,
                  samples = input$relbinding_hits_tab_sample_select,
                  expand = input$relbinding_hits_tab_expand,
                  na_include = input$relbinding_hits_tab_na,
                  units = units
                )

                # Assign filtered hits table to reactive for eventual export
                relbinding_hits_raw(hits_table)

                # Create DT table
                hits_datatable <- render_hits_table(
                  hits_table = hits_table,
                  concentration_colors = NULL,
                  bar_chart = input$relbinding_binding_chart,
                  colors = get_cmp_colorScale(
                    filtered_table = hits_table,
                    scale = input$color_scale,
                    variable = input$color_variable,
                    trunc = input$truncate_names
                  ),
                  color_variable = input$color_variable,
                  truncated = input$truncate_names,
                  clickable = c("Sample ID", "Protein", "Cmp Name"),
                  units = units
                )

                # Save datatable in reactive variable
                relbinding_hits_current(hits_datatable)

                return(hits_datatable)
              },
              server = FALSE
            ) |>
              shiny::bindEvent(
                render_trigger(),
                input$color_scale,
                input$relbinding_hits_tab_col_select,
                input$relbinding_binding_chart,
                input$relbinding_hits_tab_compound_select,
                input$relbinding_hits_tab_sample_select,
                input$relbinding_hits_tab_na
              )

            ###### Hits table export ----
            setup_table_dl(
              input,
              output,
              session,
              "relbinding_hits_tab",
              data_fn = function() prepare_hits_export(relbinding_hits_raw()),
              filename_fn = function() {
                paste0(get_session_prefix(), "_Hits_Table")
              }
            )

            ####### Hits table clicking observer ----
            safe_observe(
              event_expr = input$relbinding_hits_tab_cell_clicked,
              observer_name = "Hits Table Clicking Observer (Rel. Binding)",
              handler_fn = function() {
                shiny::req(
                  input$relbinding_hits_tab_cell_clicked,
                  relbinding_hits_current()
                )

                # Get client side click information
                cell_clicked <- input$relbinding_hits_tab_cell_clicked

                if (
                  !is.null(cell_clicked) &&
                    length(cell_clicked) &&
                    !is.na(relbinding_hits_current()$x$data[
                      input$relbinding_hits_tab_cell_clicked$row,
                      input$relbinding_hits_tab_cell_clicked$col + 1
                    ])
                ) {
                  # Get current column indeces of sample and compound columns
                  cols <- names(relbinding_hits_current()$x$data)
                  sample_col <- which(cols == "Sample ID") - 1
                  prot_col <- which(cols == "Protein") - 1
                  cmp_col <- which(cols == "Cmp Name") - 1

                  # Actions if click corresponds to sample or compound
                  if (length(sample_col) && cell_clicked$col == sample_col) {
                    shinyWidgets::updatePickerInput(
                      session,
                      "conversion_sample_picker",
                      selected = cell_clicked$value
                    )

                    set_selected_tab("Samples View", session)
                  } else if (length(prot_col) && cell_clicked$col == prot_col) {
                    shinyWidgets::updatePickerInput(
                      session,
                      "conversion_protein_picker",
                      selected = cell_clicked$value
                    )

                    set_selected_tab("Proteins View", session)
                  } else if (length(cmp_col) && cell_clicked$col == cmp_col) {
                    shinyWidgets::updatePickerInput(
                      session,
                      "conversion_compound_picker",
                      selected = cell_clicked$value
                    )

                    set_selected_tab("Compounds View", session)
                  }
                }
              }
            )

            ##### Sample view tab ----

            ###### Selected protein info ----
            output$samples_selected_protein <- shiny::renderUI(
              {
                shiny::req(
                  hits_summary,
                  input$conversion_sample_picker
                )
                selected <- input$conversion_sample_picker

                protein <- unique(hits_summary$`Protein`[
                  hits_summary$`Sample ID` == selected
                ])

                # Get protein signal
                measured_protein_mw <- hits_summary$`Meas. Prot. [Da]`[
                  hits_summary$`Sample ID` == selected
                ]

                # Filter out NAs
                measured_protein_mw <- measured_protein_mw[
                  !is.na(measured_protein_mw)
                ]
                if (length(measured_protein_mw)) {
                  signal_average <- paste(
                    format(
                      round(mean(measured_protein_mw), 2),
                      big.mark = ",",
                      scientific = FALSE
                    ),
                    "Da"
                  )
                } else {
                  signal_average <- "No signal"
                }

                theor_protein_mw <- hits_summary$`Theor. Prot. [Da]`[
                  hits_summary$`Sample ID` == selected
                ]

                shiny::div(
                  class = "conversion-sample-protein-box",
                  shiny::div(
                    class = "conversion-sample-protein-names",
                    shiny::HTML("Name<br>Mw<br>Signal")
                  ),
                  shiny::div(
                    class = "conversion-sample-protein",
                    shiny::HTML(paste(
                      protein,
                      "<br>",
                      format(
                        theor_protein_mw[1],
                        big.mark = ",",
                        scientific = FALSE
                      ),
                      "Da <br>",
                      signal_average
                    ))
                  )
                )
              }
            )

            ###### Total %-binding ----
            output$samples_total_pct_binding <- shiny::renderUI({
              shiny::req(
                hits_summary,
                input$conversion_sample_picker
              )

              tbl <- hits_summary[
                hits_summary$`Sample ID` == input$conversion_sample_picker,
              ]

              if (all(is.na(tbl$`Cmp Name`))) {
                return(shiny::div("N/A", class = "na-placeholder"))
              }

              shiny::div(
                class = "conversion-sample-protein-box",
                shiny::div(
                  class = "conversion-sample-protein-names",
                  shiny::HTML(paste(
                    "Mass Shifts<br>Selected<br>Binding"
                  ))
                ),
                shiny::div(
                  class = "conversion-sample-protein",
                  shiny::HTML(
                    paste0(
                      length(unique(tbl$`Theor. Cmp [Da]`[!is.na(tbl$`Cmp Name`)])),
                      "<br>",
                      unique(tbl$`Cmp Name`[!is.na(tbl$`Cmp Name`)]),
                      "<br>",
                      round(mean(tbl$`Total %-Binding`), 2),
                      "%"
                    )
                  )
                )
              )
            })

            ###### Compound distribution ----
            output$samples_compound_distribution_ui <- shiny::renderUI({
              shiny::req(
                hits_summary,
                input$conversion_sample_picker
              )

              tbl <- hits_summary |>
                dplyr::filter(
                  `Sample ID` == input$conversion_sample_picker
                )

              if (anyNA(tbl)) {
                shiny::textOutput(ns("samples_present_compounds_na"))
              } else {
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(
                    ns("samples_compound_distribution"),
                    height = "100%"
                  ),
                  type = 1,
                  color = "#7777f9"
                )
              }
            })

            output$samples_present_compounds_na <- shiny::renderText(
              "No binding events"
            )

            output$samples_compound_distribution <- plotly::renderPlotly({
              shiny::req(
                hits_summary,
                input$conversion_sample_picker,
                input$color_scale,
                input$color_variable,
                !is.null(input$truncate_names)
              )

              smpl_compound_distribution(
                hits_summary = hits_summary,
                sample = input$conversion_sample_picker,
                color_variable = input$color_variable,
                truncate_names = input$truncate_names,
                color_scale = input$color_scale
              )
            }) |>
              shiny::bindEvent(
                render_trigger(),
                input$conversion_sample_picker,
                input$color_scale,
                input$truncate_names
              )

            ###### Annotated spectrum ----
            output$samples_annotated_spectrum <- plotly::renderPlotly({
              shiny::req(
                hits_summary,
                input$color_variable,
                input$conversion_sample_picker,
                !is.null(input$truncate_names),
                input$color_scale
              )

              color_scale <- input$color_scale
              color_variable <- input$color_variable
              selected_sample <- input$conversion_sample_picker

              # Filter table for selected sample
              tbl <- hits_summary |>
                dplyr::filter(
                  `Sample ID` == selected_sample
                )

              spectrum_plot(
                sample = result_list$deconvolution[[
                  selected_sample
                ]],
                color_cmp = get_cmp_colorScale(
                  filtered_table = tbl,
                  scale = color_scale,
                  variable = color_variable,
                  trunc = input$truncate_names
                ),
                color_variable = color_variable,
                show_peak_labels = ifelse(
                  is.null(input$sample_view_spectrum_annotation),
                  FALSE,
                  input$sample_view_spectrum_annotation
                ),
                show_mass_diff = ifelse(
                  is.null(input$sample_view_spectrum_diff),
                  TRUE,
                  input$sample_view_spectrum_diff
                )
              )
            }) |>
              shiny::bindEvent(
                render_trigger(),
                input$conversion_sample_picker,
                input$truncate_names,
                input$color_scale,
                input$sample_view_spectrum_annotation,
                input$sample_view_spectrum_diff
              )

            ###### Samples view table ----
            output$samples_table_view <- DT::renderDataTable(
              {
                shiny::req(
                  hits_summary,
                  input$conversion_sample_picker,
                  input$color_variable,
                  !is.null(input$truncate_names),
                  input$color_scale
                )
                tbl <- hits_summary |>
                  dplyr::filter(
                    `Sample ID` == input$conversion_sample_picker &
                      !is.na(`Cmp Name`)
                  )

                # If table empty
                if (!nrow(tbl)) {
                  empty_df <- data.frame(rep(list(as.character()), 5)) |>
                    stats::setNames(c(
                      "Sample ID",
                      "Cmp Name",
                      "Mass Shift",
                      "%-Binding",
                      "Total %"
                    ))

                  # Assign filtered table to reactive for eventual export
                  samples_table_view_raw(empty_df)

                  return(
                    DT::datatable(
                      empty_df,
                      selection = "none",
                      class = "order-column",
                      options = list(
                        dom = 't',
                        paging = FALSE
                      )
                    )
                  )
                }

                # Summarize inputs
                inputs <- list(
                  binding_bar = input$samples_table_view_binding_bar,
                  tot_binding_bar = input$samples_table_view_tot_binding_bar,
                  truncate_names = input$truncate_names,
                  color_variable = input$color_variable
                )

                # Get colors
                colors <- get_cmp_colorScale(
                  filtered_table = tbl,
                  scale = input$color_scale,
                  variable = input$color_variable,
                  trunc = input$truncate_names
                )

                # Prefiltering of table
                tbl <- filter_table_view(
                  table = tbl,
                  colors = colors,
                  inputs = inputs,
                  units = units
                )

                # Assign filtered table to reactive for eventual export
                samples_table_view_raw(tbl)

                # Create DT table
                render_table_view(
                  table = tbl,
                  colors = colors,
                  tab = "Samples",
                  inputs = inputs,
                  units = units
                )
              },
              server = FALSE
            ) |>
              shiny::bindEvent(
                input$conversion_sample_picker,
                render_trigger(),
                input$truncate_names,
                input$color_scale,
                input$samples_table_view_binding_bar,
                input$samples_table_view_tot_binding_bar
              )

            ##### Compound View tab ----
            ###### Selected Compound info ----
            output$compounds_selected_compound <- shiny::renderUI({
              shiny::req(
                hits_summary,
                input$conversion_compound_picker
              )
              selected <- input$conversion_compound_picker

              theor_cmp_mw <- hits_summary[
                hits_summary$`Cmp Name` %in% selected,
              ]

              if (length(unique(theor_cmp_mw$`Theor. Cmp [Da]`))) {
                shiny::div(
                  class = "conversion-sample-protein-box",
                  shiny::div(
                    class = "conversion-sample-protein-names",
                    shiny::HTML("Name<br>Mass Shifts<br>Mw")
                  ),
                  shiny::div(
                    class = "conversion-sample-protein",
                    shiny::HTML(paste(
                      selected,
                      "<br>",
                      length(unique(theor_cmp_mw$`Theor. Cmp [Da]`)),
                      "<br>",
                      if (length(unique(theor_cmp_mw$`Theor. Cmp [Da]`)) > 1) {
                        paste(
                          format(
                            c(
                              min(as.numeric(gsub(
                                " Da",
                                "",
                                unique(theor_cmp_mw$`Theor. Cmp [Da]`)
                              ))),
                              max(as.numeric(gsub(
                                " Da",
                                "",
                                unique(theor_cmp_mw$`Theor. Cmp [Da]`)
                              )))
                            ),
                            big.mark = ",",
                            scientific = FALSE
                          ),
                          collapse = " - "
                        )
                      } else {
                        format(
                          as.numeric(gsub(
                            " Da",
                            "",
                            unique(theor_cmp_mw$`Theor. Cmp [Da]`)
                          )),
                          big.mark = ",",
                          scientific = FALSE
                        )
                      },
                      "Da"
                    ))
                  )
                )
              }
            })

            ###### Total %-binding ----
            output$compounds_total_pct_binding <- shiny::renderUI({
              shiny::req(hits_summary)

              if (is.null(input$conversion_compound_picker)) {
                return(shiny::div("N/A", class = "na-placeholder"))
              }

              total_bind <- hits_summary$`Total %-Binding`[
                hits_summary$`Cmp Name` == input$conversion_compound_picker &
                  !is.na(hits_summary$`Cmp Name`)
              ]

              if (length(total_bind) == 1) {
                msg <- paste0(total_bind, "%")
              } else {
                msg <- shiny::div(
                  class = "conversion-sample-protein-box",
                  shiny::div(
                    class = "conversion-sample-protein-names",
                    shiny::HTML(paste(
                      "Range<br>Mean",
                      if (length(total_bind) > 1) "± SD"
                    ))
                  ),
                  shiny::div(
                    class = "conversion-sample-protein",
                    shiny::HTML(
                      paste0(
                        round(min(total_bind), 2),
                        "% - ",
                        round(max(total_bind), 2),
                        "%<br>",
                        round(mean(total_bind), 2),
                        "%",
                        if (length(total_bind) > 1) {
                          paste0(
                            " ± ",
                            round(stats::sd(total_bind), 2)
                          )
                        }
                      )
                    )
                  )
                )
              }

              return(msg)
            })

            ###### Compound distribution ----
            output$compounds_compound_distribution_ui <- shiny::renderUI({
              shiny::req(hits_summary)

              if (is.null(input$conversion_compound_picker)) {
                shiny::textOutput(ns("compounds_present_compounds_na"))
              } else {
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(
                    ns("compounds_compound_distribution"),
                    height = "100%"
                  ),
                  type = 1,
                  color = "#7777f9"
                )
              }
            })

            output$compounds_present_compounds_na <- shiny::renderText(
              "No binding events"
            )

            output$compounds_compound_distribution <- plotly::renderPlotly({
              shiny::req(
                hits_summary,
                input$conversion_compound_picker,
                input$color_variable,
                !is.null(input$truncate_names),
                input$color_scale
              )

              cmp_compound_distribution(
                hits_summary = hits_summary,
                compound = input$conversion_compound_picker,
                color_variable = input$color_variable,
                truncate_names = input$truncate_names,
                color_scale = input$color_scale,
                distribution_scale = input$cmp_distribution_scale,
                distribution_labels = input$cmp_distribution_labels
              )
            }) |>
              shiny::bindEvent(
                input$conversion_compound_picker,
                render_trigger(),
                input$truncate_names,
                input$color_scale,
                input$cmp_distribution_labels,
                input$cmp_distribution_scale
              )

            ####### Show label input UI ----
            output$compounds_distribution_labels_ui <- shiny::renderUI({
              shiny::req(hits_summary, input$conversion_compound_picker)

              tbl <- hits_summary |>
                dplyr::filter(`Cmp Name` == input$conversion_compound_picker)

              if (input$truncate_names) {
                sample_ids <- tbl$`truncSample_ID`
              } else {
                sample_ids <- tbl$`Sample ID`
              }

              # condition <- max(nchar(unique(sample_ids))) <= 22 |
              #   nrow(tbl) < 4
              condition <- TRUE

              shinyWidgets::materialSwitch(
                ns("cmp_distribution_labels"),
                label = "Show Labels",
                value = condition,
                right = TRUE
              )
            })

            ###### Annotated spectrum ----

            compounds_labels_val <- shiny::reactiveVal(local({
              cmp <- unique(hits_summary$`Cmp Name`)[1]
              tbl <- hits_summary[hits_summary$`Cmp Name` == cmp, ]
              if (is.na(cmp) || nrow(tbl) < 2) {
                return(TRUE)
              }
              ids <- tbl$`Sample ID`
              length(unique(ids)) <= 8 & max(nchar(as.character(ids))) <= 20
            }))

            shiny::observeEvent(
              input$conversion_compound_picker,
              {
                manual_render_cmp_spectrum(0L)
              },
              ignoreInit = TRUE
            )

            shiny::observeEvent(input$render_cmp_annotated_spectrum_btn, {
              manual_render_cmp_spectrum(manual_render_cmp_spectrum() + 1L)
            })

            shiny::observeEvent(
              list(
                input$truncate_names,
                input$color_variable,
                input$color_scale
              ),
              {
                shiny::req(hits_summary, input$conversion_compound_picker)
                n_samples <- length(unique(hits_summary$`Sample ID`[
                  hits_summary$`Cmp Name` == input$conversion_compound_picker
                ]))
                if (n_samples >= 30) manual_render_cmp_spectrum(0L)
              },
              ignoreInit = TRUE
            )

            output$cmp_annotated_spectrum_na <- shiny::renderText("N/A")

            output$cmp_annotated_spectrum_container <- shiny::renderUI({
              shiny::req(hits_summary)

              if (is.null(input$conversion_compound_picker)) {
                return(shiny::textOutput(ns("cmp_annotated_spectrum_na")))
              }

              n_samples <- length(unique(hits_summary$`Sample ID`[
                hits_summary$`Cmp Name` == input$conversion_compound_picker
              ]))

              if (n_samples < 30 || manual_render_cmp_spectrum() > 0L) {
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(
                    ns("compounds_annotated_spectrum"),
                    height = "100%"
                  ),
                  type = 1,
                  color = "#7777f9"
                )
              } else {
                shiny::div(
                  class = "spectrum-render-prompt",
                  shiny::p(
                    sprintf(
                      "Auto-render disabled for large datasets (%d samples). Click to render manually (Processing can take a while).",
                      n_samples
                    )
                  ),
                  shiny::actionButton(
                    ns("render_cmp_annotated_spectrum_btn"),
                    label = "Render Spectrum",
                    icon = shiny::icon("chart-line"),
                    class = "btn-outline-primary btn-sm"
                  )
                )
              }
            })

            output$compounds_annotated_spectrum <- plotly::renderPlotly({
              shiny::req(
                hits_summary,
                input$conversion_compound_picker,
                !is.null(input$truncate_names),
                input$color_variable,
                input$color_scale
              )

              n_samples_check <- length(unique(hits_summary$`Sample ID`[
                hits_summary$`Cmp Name` == input$conversion_compound_picker
              ]))
              shiny::req(
                n_samples_check < 30 || manual_render_cmp_spectrum() > 0L
              )

              # Block UI
              shinyjs::runjs(paste0(
                'document.getElementById("blocking-overlay").style.display ',
                '= "block";'
              ))

              color_scale <- input$color_scale
              color_variable <- input$color_variable
              conversion_compound_picker <- input$conversion_compound_picker
              truncate_names <- input$truncate_names

              # Filter hits for selected compound
              tbl <- dplyr::filter(
                hits_summary,
                `Cmp Name` == conversion_compound_picker
              )

              # Make compound color scale
              colors <- get_cmp_colorScale(
                filtered_table = tbl,
                scale = color_scale,
                variable = color_variable,
                trunc = truncate_names
              )

              # Create spectra plot
              if (nrow(tbl) == 1) {
                plot <- spectrum_plot(
                  sample = result_list$deconvolution[[tbl$`Sample ID`]],
                  color_cmp = colors,
                  color_variable = color_variable,
                  show_peak_labels = TRUE,
                  show_mass_diff = FALSE
                )
              } else {
                plot <- multiple_spectra(
                  results_list = result_list,
                  samples = unique(hits_summary$`Sample ID`[
                    hits_summary$`Cmp Name` == conversion_compound_picker
                  ]),
                  cubic = is.null(input$compounds_spectrum_kind) ||
                    input$compounds_spectrum_kind == "3D",
                  color_cmp = colors,
                  truncated = if (truncate_names) mapping else FALSE,
                  color_variable = color_variable,
                  hits_summary = hits_summary,
                  labels_show = compounds_labels_val()
                )
              }

              # Unblock UI
              shinyjs::runjs(paste0(
                'document.getElementById("blocking-overlay").style.display ',
                '= "none";'
              ))

              return(plot)
            }) |>
              shiny::bindEvent(
                input$conversion_compound_picker,
                render_trigger(),
                input$truncate_names,
                input$color_scale,
                input$compounds_spectrum_kind,
                compounds_labels_val(),
                manual_render_cmp_spectrum()
              )

            ####### Show label input UI ----
            shiny::observe({
              shiny::req(hits_summary, input$conversion_compound_picker)

              tbl <- hits_summary |>
                dplyr::filter(`Cmp Name` == input$conversion_compound_picker)

              if (nrow(tbl) < 2) {
                compounds_labels_val(TRUE)
                shinyWidgets::updateMaterialSwitch(
                  session,
                  "compounds_spectrum_labels",
                  value = TRUE
                )
                shinyjs::disable("compounds_spectrum_labels")
                return()
              }

              shinyjs::enable("compounds_spectrum_labels")

              if (input$truncate_names) {
                sample_ids <- tbl$`truncSample_ID`
              } else {
                sample_ids <- tbl$`Sample ID`
              }

              labels_show <- (length(unique(sample_ids)) <= 8 &
                max(nchar(as.character(sample_ids))) <= 20)

              compounds_labels_val(labels_show)
              shinyWidgets::updateMaterialSwitch(
                session,
                "compounds_spectrum_labels",
                value = labels_show
              )
            })

            shiny::observeEvent(
              input$compounds_spectrum_labels,
              {
                compounds_labels_val(input$compounds_spectrum_labels)
              },
              ignoreInit = TRUE
            )

            ###### Compounds view table ----
            output$compounds_table_view <- DT::renderDataTable(
              {
                shiny::req(
                  hits_summary,
                  input$color_variable,
                  !is.null(input$truncate_names),
                  input$color_scale
                )

                tbl <- if (is.null(input$conversion_compound_picker)) {
                  hits_summary[0, ]
                } else {
                  hits_summary |>
                    dplyr::filter(
                      `Cmp Name` == input$conversion_compound_picker
                    )
                }

                # Summarize inputs
                inputs <- list(
                  binding_bar = input$compounds_table_view_binding_bar,
                  tot_binding_bar = input$compounds_table_view_tot_binding_bar,
                  truncate_names = input$truncate_names,
                  color_variable = input$color_variable
                )

                # Get colors
                colors <- get_cmp_colorScale(
                  filtered_table = tbl,
                  scale = input$color_scale,
                  variable = input$color_variable,
                  trunc = input$truncate_names
                )

                # Prefiltering of table
                tbl <- filter_table_view(
                  table = tbl,
                  colors = colors,
                  inputs = inputs,
                  units = units
                )

                # Assign filtered table to reactive for eventual export
                compounds_table_view_raw(tbl)

                # Create DT table
                render_table_view(
                  table = tbl,
                  colors = colors,
                  tab = "Compounds",
                  inputs = inputs,
                  units = units
                )
              },
              server = FALSE
            ) |>
              shiny::bindEvent(
                input$conversion_compound_picker,
                render_trigger(),
                input$truncate_names,
                input$color_scale,
                input$compounds_table_view_binding_bar,
                input$compounds_table_view_tot_binding_bar
              )

            ##### Protein View tab ----

            ###### Selected protein info ----
            output$proteins_selected_protein <- shiny::renderUI(
              {
                shiny::req(
                  hits_summary,
                  input$conversion_protein_picker
                )

                selected <- input$conversion_protein_picker

                # Get all protein signals
                measured_protein_mw <- hits_summary$`Meas. Prot. [Da]`[
                  hits_summary$Protein == selected
                ]
                # Filter out NAs
                measured_protein_mw <- measured_protein_mw[
                  !is.na(measured_protein_mw)
                ]
                if (length(measured_protein_mw)) {
                  signal_average <- paste(
                    format(
                      round(mean(measured_protein_mw), 2),
                      big.mark = ",",
                      scientific = FALSE
                    ),
                    "Da"
                  )
                } else {
                  signal_average <- "No signal"
                }

                # Get theoretical protein mw
                theor_protein_mw <- hits_summary$`Theor. Prot. [Da]`[
                  hits_summary$Protein == selected
                ]

                shiny::div(
                  class = "conversion-sample-protein-box",
                  shiny::div(
                    class = "conversion-sample-protein-names",
                    shiny::HTML("Name<br>Mw<br>Signal")
                  ),
                  shiny::div(
                    class = "conversion-sample-protein",
                    shiny::HTML(paste(
                      selected,
                      "<br>",
                      format(
                        theor_protein_mw[1],
                        big.mark = ",",
                        scientific = FALSE
                      ),
                      "Da <br>",
                      signal_average,
                      if (length(measured_protein_mw) > 1) {
                        "±"
                      },
                      if (length(measured_protein_mw) > 1) {
                        round(stats::sd(measured_protein_mw), 2)
                      }
                    ))
                  )
                )
              }
            )

            ###### Total %-binding for one compound across samples ----
            output$proteins_total_pct_binding <- shiny::renderUI({
              shiny::req(
                hits_summary,
                input$conversion_sample_picker
              )

              choices <- unique(hits_summary$`Cmp Name`[
                hits_summary$`Protein` == input$conversion_protein_picker &
                  !is.na(hits_summary$`Cmp Name`)
              ])

              if (!length(choices)) {
                choices <- NULL
              }

              shiny::div(
                class = "prot-binding-ui",
                shiny::selectInput(
                  ns("total_pct_prot_binding_select"),
                  "Select Compound",
                  choices = choices
                )
              )
            })

            output$total_pct_prot_binding <- shiny::renderUI({
              shiny::req(
                hits_summary,
                input$conversion_protein_picker
              )

              # Prefilter hits by selected protein and non-NA compound
              total_bind_pre <- hits_summary[
                hits_summary$`Protein` == input$conversion_protein_picker,
              ]

              if (all(is.na(total_bind_pre$`Cmp Name`))) {
                return(shiny::div("N/A", class = "na-placeholder"))
              }

              # Get selected compound
              total_pct_prot_binding_select <- ifelse(
                !is.null(input$total_pct_prot_binding_select),
                input$total_pct_prot_binding_select,
                total_bind_pre$`Cmp Name`[!is.na(total_bind_pre$`Cmp Name`)][1]
              )

              # Filter by selected compound
              total_bind <- total_bind_pre[
                total_bind_pre$`Cmp Name` == total_pct_prot_binding_select &
                  !is.na(total_bind_pre$`Cmp Name`),
              ]

              msg <- shiny::div(
                class = "conversion-sample-protein-box",
                shiny::div(
                  class = "conversion-sample-protein-names",
                  shiny::HTML(paste(
                    "No. Compounds<br>Selected<br>",
                    if (length(total_bind$`Total %-Binding`) > 1) "Range<br>",
                    "Mean",
                    if (length(total_bind$`Total %-Binding`) > 1) "± SD"
                  ))
                ),
                shiny::div(
                  class = "conversion-sample-protein",
                  shiny::HTML(
                    paste0(
                      length(unique(total_bind_pre$`Cmp Name`[
                        !is.na(total_bind_pre$`Cmp Name`)
                      ])),
                      "<br>",
                      total_pct_prot_binding_select,
                      "<br>",
                      if (length(total_bind$`Total %-Binding`) > 1) {
                        paste0(
                          round(min(total_bind$`Total %-Binding`), 2),
                          "% - ",
                          round(max(total_bind$`Total %-Binding`), 2),
                          "%<br>"
                        )
                      },
                      round(mean(total_bind$`Total %-Binding`), 2),
                      "%",
                      if (length(total_bind$`Total %-Binding`) > 1) {
                        paste0(
                          " ± ",
                          round(stats::sd(total_bind$`Total %-Binding`), 2)
                        )
                      }
                    )
                  )
                )
              )

              return(msg)
            })

            output$proteins_present_compounds_ui <- shiny::renderUI({
              shiny::req(
                hits_summary,
                input$conversion_protein_picker
              )

              tbl <- hits_summary |>
                dplyr::filter(
                  `Protein` == input$conversion_protein_picker &
                    !is.na(`Cmp Name`)
                )

              if (nrow(tbl) < 1) {
                shiny::textOutput(ns("proteins_present_compounds_na"))
              } else {
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(
                    ns("proteins_compound_distribution"),
                    height = "100%"
                  ),
                  type = 1,
                  color = "#7777f9"
                )
              }
            })

            output$proteins_present_compounds_na <- shiny::renderText(
              "No binding events"
            )

            ###### Compound distribution ----
            output$proteins_compound_distribution <- plotly::renderPlotly({
              shiny::req(
                hits_summary,
                input$conversion_protein_picker,
                input$color_variable,
                !is.null(input$truncate_names),
                input$color_scale
              )

              prot_compound_distribution(
                hits_summary = hits_summary,
                protein = input$conversion_protein_picker,
                color_variable = input$color_variable,
                truncate_names = input$truncate_names,
                color_scale = input$color_scale,
                distribution_scale = input$protein_distribution_scale,
                distribution_labels = input$protein_distribution_labels
              )
            }) |>
              shiny::bindEvent(
                render_trigger(),
                input$color_scale,
                input$conversion_protein_picker,
                input$truncate_names,
                input$protein_distribution_scale,
                input$protein_distribution_labels
              )

            ####### Show label input UI ----
            output$protein_distribution_labels_ui <- shiny::renderUI({
              shiny::req(
                hits_summary,
                input$conversion_protein_picker,
                input$truncate_names
              )

              tbl <- hits_summary |>
                dplyr::filter(`Protein` == input$conversion_protein_picker)

              if (input$truncate_names) {
                sample_ids <- tbl$`truncSample_ID`
              } else {
                sample_ids <- tbl$`Sample ID`
              }

              condition <- ifelse(
                length(unique(tbl$`Cmp Name`)) > 1,
                max(nchar(unique(sample_ids))) <= 22,
                max(nchar(unique(tbl$`Cmp Name`))) <= 22
              )

              shinyWidgets::materialSwitch(
                ns("protein_distribution_labels"),
                label = "Show Labels",
                value = condition,
                right = TRUE
              )
            })

            ###### Annotated spectrum ----

            proteins_labels_val <- shiny::reactiveVal(local({
              prot <- unique(hits_summary$`Protein`)[1]
              tbl <- hits_summary[hits_summary$`Protein` == prot, ]
              if (is.na(prot) || nrow(tbl) < 2) return(TRUE)
              ids <- tbl$`Sample ID`
              length(unique(ids)) <= 8 & max(nchar(as.character(ids))) <= 20
            }))

            shiny::observeEvent(
              input$conversion_protein_picker,
              {
                manual_render_spectrum(0L)
              },
              ignoreInit = TRUE
            )

            shiny::observeEvent(input$render_annotated_spectrum_btn, {
              manual_render_spectrum(manual_render_spectrum() + 1L)
            })

            shiny::observeEvent(
              list(
                input$truncate_names,
                input$color_variable,
                input$color_scale
              ),
              {
                shiny::req(hits_summary, input$conversion_protein_picker)
                n_samples <- length(unique(hits_summary$`Sample ID`[
                  hits_summary$`Protein` == input$conversion_protein_picker
                ]))
                if (n_samples >= 30) manual_render_spectrum(0L)
              },
              ignoreInit = TRUE
            )

            output$annotated_spectrum_container <- shiny::renderUI({
              shiny::req(hits_summary, input$conversion_protein_picker)

              n_samples <- length(unique(hits_summary$`Sample ID`[
                hits_summary$`Protein` == input$conversion_protein_picker
              ]))

              if (n_samples < 30 || manual_render_spectrum() > 0L) {
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(
                    ns("proteins_annotated_spectrum"),
                    height = "100%"
                  ),
                  type = 1,
                  color = "#7777f9"
                )
              } else {
                shiny::div(
                  class = "spectrum-render-prompt",
                  shiny::p(
                    sprintf(
                      "Auto-render disabled for large datasets (%d samples). Click to render manually (Processing can take a while).",
                      n_samples
                    )
                  ),
                  shiny::actionButton(
                    ns("render_annotated_spectrum_btn"),
                    label = "Render Spectrum",
                    icon = shiny::icon("chart-line"),
                    class = "btn-outline-primary btn-sm"
                  )
                )
              }
            })

            output$proteins_annotated_spectrum <- plotly::renderPlotly({
              shiny::req(
                hits_summary,
                input$conversion_protein_picker,
                !is.null(input$truncate_names),
                input$color_variable,
                input$color_scale
              )

              n_samples_check <- length(unique(hits_summary$`Sample ID`[
                hits_summary$`Protein` == input$conversion_protein_picker
              ]))
              shiny::req(n_samples_check < 30 || manual_render_spectrum() > 0L)

              # Block UI
              shinyjs::runjs(paste0(
                'document.getElementById("blocking-overlay").style.display ',
                '= "block";'
              ))

              color_scale <- input$color_scale
              color_variable <- input$color_variable
              conversion_compound_picker <- input$conversion_protein_picker
              truncate_names <- input$truncate_names

              # Filter hits for selected compound
              tbl <- dplyr::filter(
                hits_summary,
                `Protein` == input$conversion_protein_picker
              )

              if (nrow(tbl)) {
                # Make compound color scale
                colors <- get_cmp_colorScale(
                  filtered_table = tbl,
                  scale = color_scale,
                  variable = color_variable,
                  trunc = truncate_names
                )
              }

              samples <- unique(hits_summary$`Sample ID`[
                hits_summary$`Protein` == input$conversion_protein_picker
              ])

              # Create spectra plot
              if (length(samples) == 1) {
                plot <- spectrum_plot(
                  sample = result_list$deconvolution[[
                    samples
                  ]],
                  color_cmp = colors,
                  color_variable = color_variable,
                  show_peak_labels = TRUE,
                  show_mass_diff = FALSE
                )
              } else {
                plot <- multiple_spectra(
                  results_list = result_list,
                  samples = unique(hits_summary$`Sample ID`[
                    hits_summary$`Protein` == input$conversion_protein_picker
                    # &
                    #   hits_summary$`Meas. Prot.` != "N/A"
                  ]),
                  cubic = is.null(input$proteins_spectrum_kind) ||
                    input$proteins_spectrum_kind == "3D",
                  color_cmp = colors,
                  truncated = if (truncate_names) mapping else FALSE,
                  color_variable = color_variable,
                  hits_summary = hits_summary,
                  labels_show = proteins_labels_val()
                )
              }

              # Unblock UI
              shinyjs::runjs(paste0(
                'document.getElementById("blocking-overlay").style.display ',
                '= "none";'
              ))

              return(plot)
            }) |>
              shiny::bindEvent(
                render_trigger(),
                input$color_scale,
                input$conversion_protein_picker,
                input$truncate_names,
                input$proteins_spectrum_kind,
                proteins_labels_val(),
                manual_render_spectrum()
              )

            ####### Show label input UI ----
            shiny::observe({
              shiny::req(hits_summary, input$conversion_protein_picker)

              tbl <- hits_summary |>
                dplyr::filter(`Protein` == input$conversion_protein_picker)

              if (nrow(tbl) < 2) {
                proteins_labels_val(TRUE)
                shinyWidgets::updateMaterialSwitch(
                  session,
                  "proteins_spectrum_labels",
                  value = TRUE
                )
                shinyjs::disable("proteins_spectrum_labels")
                return()
              }

              shinyjs::enable("proteins_spectrum_labels")

              if (input$truncate_names) {
                sample_ids <- tbl$`truncSample_ID`
              } else {
                sample_ids <- tbl$`Sample ID`
              }

              labels_show <- (length(unique(sample_ids)) <= 8 &
                max(nchar(as.character(sample_ids))) <= 20)

              proteins_labels_val(labels_show)
              shinyWidgets::updateMaterialSwitch(
                session,
                "proteins_spectrum_labels",
                value = labels_show
              )
            })

            shiny::observeEvent(
              input$proteins_spectrum_labels,
              {
                proteins_labels_val(input$proteins_spectrum_labels)
              },
              ignoreInit = TRUE
            )

            ###### Proteins view table ----
            output$proteins_table_view <- DT::renderDataTable(
              {
                shiny::req(
                  hits_summary,
                  input$conversion_protein_picker,
                  input$color_variable,
                  !is.null(input$truncate_names),
                  input$color_scale
                )

                tbl <- hits_summary |>
                  dplyr::filter(`Protein` == input$conversion_protein_picker)

                # Summarize inputs
                inputs <- list(
                  binding_bar = input$proteins_table_view_binding_bar,
                  tot_binding_bar = input$proteins_table_view_tot_binding_bar,
                  truncate_names = input$truncate_names,
                  color_variable = input$color_variable
                )

                # Get colors
                colors <- get_cmp_colorScale(
                  filtered_table = tbl,
                  scale = input$color_scale,
                  variable = input$color_variable,
                  trunc = input$truncate_names
                )

                # Prefiltering of table
                tbl <- filter_table_view(
                  table = tbl,
                  colors = colors,
                  inputs = inputs,
                  units = units
                )

                # Assign filtered table to reactive for eventual export
                proteins_table_view_raw(tbl)

                # Create DT table
                render_table_view(
                  table = tbl,
                  colors = colors,
                  tab = "Proteins",
                  inputs = inputs,
                  units = units
                )
              },
              server = FALSE
            ) |>
              shiny::bindEvent(
                render_trigger(),
                input$color_scale,
                input$conversion_protein_picker,
                input$truncate_names,
                input$proteins_table_view_binding_bar,
                input$proteins_table_view_tot_binding_bar
              )

            ##### Color variable UI ----
            output$color_variable_ui <- shiny::renderUI({
              shiny::req(hits_summary)

              shiny::selectInput(
                ns("color_variable"),
                label = NULL,
                choices = c("Samples", "Compounds"),
                selected = ifelse(
                  length(unique(hits_summary$`Cmp Name`[
                    !is.na(hits_summary$`Cmp Name`)
                  ])) ==
                    1,
                  "Samples",
                  "Compounds"
                )
              )
            })

            # Switch to Hits tab
            set_selected_tab("Hits", session)
          } else if (analysis_select == 2) {
            #### Render Ki/kinact interface ----
            # Assign formatted hits to reactive variable
            conversion_vars$formatted_hits <- hits_summary

            # Get concentration colors
            concentration_colors <- RColorBrewer::brewer.pal(
              n = length(unique(hits_summary[[units[[
                "Concentration"
              ]]]])),
              name = "Set3"
            )

            names(concentration_colors) <- unique(hits_summary[[units[[
              "Concentration"
            ]]]])

            # Assign colors to reactive variable
            conversion_vars$conc_colors <- concentration_colors

            # Assign concentrations to reactive variable
            conversion_vars$concentrations <- concentrations <- dplyr::filter(
              hits_summary,
              `Cmp Name` != "N/A"
            ) |>
              dplyr::count(
                !!rlang::sym(units[[
                  "Concentration"
                ]])
              ) |>
              dplyr::filter(n > 2) |>
              dplyr::select(1) |>
              unlist() |>
              unname() |>
              as.character()

            conc_selected <- rep(TRUE, length(concentrations))
            names(conc_selected) <- concentrations
            conversion_vars$select_concentration <- conc_selected

            # Define a set of IDs for the dynamic concentration tabs
            dynamic_ui_ids <- paste0("concentration_tab_", concentrations)

            # Call function to render Ki/kinact results interface
            output$conversion_ui <- shiny::renderUI({
              ki_kinact_results_ui(
                ns,
                hits_summary,
                concentrations,
                dynamic_ui_ids
              )
            })

            ##### Hits tab ----
            ###### Hits table ----
            output$kikinact_hits_tab <- DT::renderDT({
              shiny::req(
                conversion_vars$formatted_hits,
                conversion_vars$conc_colors
              )

              # Arrange table
              hits_table <- dplyr::arrange(
                hits_summary,
                `Protein`,
                as.numeric(!!rlang::sym(units[["Concentration"]])),
                as.numeric(!!rlang::sym(units[["Time"]]))
              )

              hits_table <- filter_hits_table(
                hits_table,
                selected_cols = input$relbinding_hits_tab_col_select,
                compounds = input$relbinding_hits_tab_compound_select,
                samples = input$relbinding_hits_tab_sample_select,
                expand = input$relbinding_hits_tab_expand,
                na_include = input$relbinding_hits_tab_na,
                units = units
              )

              # Assign filtered hits table to reactive for eventual export
              kikinact_hits_raw(hits_table)

              # Create DT table
              hits_datatable <- render_hits_table(
                hits_table = hits_table,
                concentration_colors = conversion_vars$conc_colors,
                bar_chart = input$kikinact_binding_chart,
                truncated = input$truncate_names,
                clickable = conversion_vars$units[["Concentration"]],
                units = units
              )

              # Save datatable in reactive variable
              kikinact_hits_current(hits_datatable)

              return(hits_datatable)
            }) |>
              shiny::bindEvent(
                render_trigger(),
                input$color_scale,
                input$kikinact_hits_tab_col_select,
                input$kikinact_binding_chart,
                input$kikinact_hits_tab_compound_select,
                input$kikinact_hits_tab_sample_select,
                input$kikinact_hits_tab_na
              )

            ###### Hits table export ----
            setup_table_dl(
              input,
              output,
              session,
              "kikinact_hits_tab",
              data_fn = function() prepare_hits_export(kikinact_hits_raw()),
              filename_fn = function() {
                paste0(get_session_prefix(), "_Hits_Table")
              }
            )

            ###### Hits table clicking observer ----
            safe_observe(
              event_expr = input$kikinact_hits_tab_cell_clicked,
              observer_name = "Hits Table Clicking Observer (Ki/kinact)",
              handler_fn = function() {
                shiny::req(
                  input$kikinact_hits_tab_cell_clicked,
                  kikinact_hits_current()
                )

                # Get client side click information
                cell_clicked <- input$kikinact_hits_tab_cell_clicked

                if (
                  !is.null(cell_clicked) &&
                    length(cell_clicked) &&
                    !is.na(kikinact_hits_current()$x$data[
                      input$kikinact_hits_tab_cell_clicked$row,
                      input$kikinact_hits_tab_cell_clicked$col + 1
                    ])
                ) {
                  # Get current column indeces of sample and compound columns
                  cols <- names(kikinact_hits_current()$x$data)
                  concentration_col <- which(
                    cols == conversion_vars$units["Concentration"]
                  ) -
                    1

                  # Actions if click corresponds to sample or compound
                  if (
                    length(concentration_col) &&
                      cell_clicked$col == concentration_col
                  ) {
                    set_selected_tab(
                      paste0("[", gsub(" µM|mM", "", cell_clicked$value), "]"),
                      session
                    )
                  }
                }
              }
            )

            ##### Binding tab ----

            ###### Calculated kinact value ----
            output$kinact <- shiny::renderUI({
              shiny::div(
                class = "result-card-content",
                shiny::div(
                  class = "main-result",
                  shiny::HTML(paste(
                    format_scientific(ki_kinact_result()[1, 1]),
                    paste0(gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]), "⁻¹")
                  ))
                ),
                shiny::div(
                  class = "error-result",
                  shiny::HTML(paste(
                    "±",
                    format_scientific(ki_kinact_result()[1, 2])
                  ))
                ),
                shiny::div(
                  class = "param-result",
                  shiny::HTML(
                    paste(
                      "<b>t value</b>&nbsp;",
                      format_scientific(ki_kinact_result()[1, 3])
                    )
                  )
                ),
                shiny::div(
                  class = "param-result",
                  shiny::HTML(
                    paste(
                      "<b>Pr(>|t|)</b>&nbsp;",
                      format_scientific(ki_kinact_result()[1, 4])
                    )
                  )
                )
              )
            })

            ###### Calculated Ki value ----
            output$Ki <- shiny::renderUI({
              shiny::div(
                class = "result-card-content",
                shiny::div(
                  class = "main-result",
                  shiny::HTML(paste(
                    format_scientific(ki_kinact_result()[2, 1]),
                    paste0(
                      gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]])
                    )
                  ))
                ),
                shiny::div(
                  class = "error-result",
                  shiny::HTML(paste(
                    "±",
                    format_scientific(ki_kinact_result()[2, 2])
                  ))
                ),
                shiny::div(
                  class = "param-result",
                  shiny::HTML(
                    paste(
                      "<b>t value</b>&nbsp;",
                      format_scientific(ki_kinact_result()[2, 3])
                    )
                  )
                ),
                shiny::div(
                  class = "param-result",
                  shiny::HTML(
                    paste(
                      "<b>Pr(>|t|)</b>&nbsp;",
                      format_scientific(ki_kinact_result()[2, 4])
                    )
                  )
                )
              )
            })

            ###### Calculated Ki/kinact value ----
            output$Ki_kinact <- shiny::renderUI({
              shiny::div(
                class = "result-card-content",
                shiny::div(
                  class = "main-result",
                  shiny::HTML(paste(
                    format_scientific(
                      ki_kinact_result()[1, 1] / ki_kinact_result()[2, 1]
                    ),
                    "<br>",
                    paste0(
                      gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
                      "⁻¹ ",
                      gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
                      "⁻¹"
                    )
                  ))
                )
              )
            })

            ###### Kobs result table ----
            output$kobs_result <- DT::renderDT(
              {
                shiny::req(
                  result_list
                )

                # Get results
                kobs_results <- result_list$binding_kobs_result$kobs_result_table

                kobs_results <- kobs_results |>
                  dplyr::mutate(
                    concentration = as.numeric(rownames(kobs_results)),
                    kobs = as.numeric(format(kobs, digits = 3)),
                    v = as.numeric(format(v, digits = 3)),
                    plateau = as.numeric(format(plateau, digits = 3))
                  ) |>
                  dplyr::relocate(concentration, .before = kobs) |>
                  stats::setNames(c(
                    paste0(
                      "Conc. [",
                      gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
                      "]"
                    ),
                    paste0(
                      "kobs [",
                      gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]]),
                      "\u207b\u00b9]"
                    ),
                    "Velocity",
                    "Plateau [%]"
                  ))

                kobs_result_raw(kobs_results)

                kobs_results <- kobs_results |>
                  dplyr::mutate(
                    Included = checkboxColumn(
                      nrow(kobs_results),
                      5,
                      value = TRUE
                    )
                  )

                # Kobs present concentrations
                kobs_conc <- kobs_results[[paste0(
                  "Conc. [",
                  gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
                  "]"
                )]]

                DT::datatable(
                  data = kobs_results,
                  rownames = FALSE,
                  selection = "none",
                  escape = FALSE,
                  class = "order-column",
                  options = list(
                    dom = "t",
                    paging = FALSE,
                    autoWidth = TRUE,
                    scrollX = TRUE,
                    scrollY = TRUE,
                    scrollCollapse = TRUE,
                    fixedHeader = TRUE,
                    stripe = FALSE,
                    columnDefs = list(
                      list(targets = -1, className = 'dt-last-col')
                    )
                  ),
                  editable = list(
                    target = "cell",
                    disable = list(
                      columns = which(names(kobs_results) != "Included") - 1
                    )
                  ),
                  callback = htmlwidgets::JS(js_code_gen(
                    "kobs_result",
                    which(names(kobs_results) == "Included"),
                    ns = session$ns
                  ))
                ) |>
                  DT::formatStyle(
                    columns = paste0(
                      "Conc. [",
                      gsub(".*\\[(.+)\\].*", "\\1", units[["Concentration"]]),
                      "]"
                    ),
                    target = 'row',
                    backgroundColor = DT::styleEqual(
                      levels = as.character(kobs_conc),
                      values = unname(concentration_colors[match(
                        kobs_conc,
                        names(concentration_colors)
                      )])
                    ),
                    color = DT::styleEqual(
                      levels = as.character(kobs_conc),
                      values = unname(get_contrast_color(concentration_colors[match(
                        kobs_conc,
                        names(concentration_colors)
                      )]))
                    )
                  ) |>
                  DT::formatStyle(
                    1:4,
                    `border-right` = "solid 1px #0000005c"
                  )
              },
              server = FALSE
            )

            ###### Binding plot ----
            output$binding_plot <- plotly::renderPlotly({
              shiny::req(result_list)

              make_binding_plot(
                kobs_result = result_list$binding_kobs_result,
                colors = concentration_colors,
                units = units
              )
            })

            ###### Kobs plot ----
            output$kobs_plot <- plotly::renderPlotly({
              shiny::req(result_list)

              if (is.null(conversion_vars$modified_results)) {
                result_list <- result_list
              } else {
                result_list <- conversion_vars$modified_results
              }

              make_kobs_plot(
                ki_kinact_result = result_list$ki_kinact_result,
                colors = concentration_colors,
                units = units
              )
            })

            setup_plot_dl(
              input,
              output,
              session,
              "binding",
              build_fn = function(theme) {
                shiny::req(result_list)
                make_binding_plot(
                  kobs_result = result_list$binding_kobs_result,
                  colors = concentration_colors,
                  units = units,
                  theme = theme
                )
              },
              filename_fn = function() {
                paste0(get_session_prefix(), "_Binding_Curve")
              }
            )

            setup_plot_dl(
              input,
              output,
              session,
              "kobs",
              build_fn = function(theme) {
                shiny::req(result_list)
                rl <- if (is.null(conversion_vars$modified_results)) {
                  result_list
                } else {
                  conversion_vars$modified_results
                }
                make_kobs_plot(
                  ki_kinact_result = rl$ki_kinact_result,
                  colors = concentration_colors,
                  units = units,
                  theme = theme
                )
              },
              filename_fn = function() {
                paste0(get_session_prefix(), "_kobs_Curve")
              }
            )

            ##### Concentration tabs ----

            # Add tabs for each present concentration
            # for (i in seq_along(concentrations)) {
            #   concentration <- concentrations[[i]]
            #   ui_id <- dynamic_ui_ids[[i]]

            #   bslib::nav_insert(
            #     "tabs",
            #     bslib::nav_panel(
            #       title = paste0("[", concentration, "]"),
            #       shiny::div(
            #         class = "conversion-result-wrapper",
            #         shiny::uiOutput(ns(ui_id))
            #       ),
            #       shiny::tags$script(
            #         popover_autoclose
            #       )
            #     )
            #   )
            # }

            # Assign output names according to present concentrations
            lapply(names(output), function(name) {
              if (grepl("^concentration_tab_", name)) {
                output[[name]] <- NULL
              }
            })

            for (i in seq_along(concentrations)) {
              concentration <- concentrations[[i]]
              ui_id <- dynamic_ui_ids[[i]]

              local({
                local_concentration <- concentration
                local_ui_id <- ui_id
                conc_tbl_raw <- shiny::reactiveVal()

                conc_result <- result_list$binding_kobs_result[[
                  local_concentration
                ]]

                ###### Render concentration interface UI ----
                output[[local_ui_id]] <- shiny::renderUI({
                  ki_kinact_concentrations_tabs(
                    ns,
                    local_ui_id,
                    conc_result,
                    units
                  )
                })

                ###### Table view ----
                output[[paste0(local_ui_id, "_hits")]] <- DT::renderDT({
                  tbl <- hits_summary |>
                    dplyr::filter(
                      !!rlang::sym(units["Concentration"]) ==
                        local_concentration
                    )

                  # Summarize inputs
                  inputs <- list(
                    truncate_names = TRUE,
                    color_variable = units["Concentration"],
                    binding_bar = input[[paste0(
                      local_ui_id,
                      "concentrations_table_view_binding_bar"
                    )]],
                    tot_binding_bar = input[[paste0(
                      local_ui_id,
                      "concentrations_table_view_tot_binding_bar"
                    )]]
                  )

                  # Prefiltering of table
                  tbl <- filter_table_view(
                    table = tbl,
                    colors = conversion_vars$conc_colors,
                    inputs = inputs,
                    units = units
                  )

                  # Assign filtered table to reactive for eventual export
                  conc_tbl_raw(tbl)

                  # Create DT table
                  render_table_view(
                    table = tbl,
                    colors = conversion_vars$conc_colors,
                    tab = "Concentration",
                    inputs = inputs,
                    units = units
                  )
                }) |>
                  shiny::bindEvent(
                    input[[paste0(
                      local_ui_id,
                      "concentrations_table_view_binding_bar"
                    )]],
                    input[[paste0(
                      local_ui_id,
                      "concentrations_table_view_tot_binding_bar"
                    )]]
                  )

                ###### Concentration table export ----
                setup_table_dl(
                  input,
                  output,
                  session,
                  paste0(local_ui_id, "_hits"),
                  data_fn = function() prepare_hits_export(conc_tbl_raw()),
                  filename_fn = function() {
                    paste0(
                      get_session_prefix(),
                      "_Table_View_",
                      local_concentration
                    )
                  }
                )

                ###### Binding plot ----
                output[[paste0(
                  local_ui_id,
                  "_binding_plot"
                )]] <- plotly::renderPlotly({
                  make_binding_plot(
                    kobs_result = result_list$binding_kobs_result,
                    filter_conc = local_concentration,
                    colors = concentration_colors,
                    units = units
                  )
                })

                ###### Multiple spectra plot ----
                output[[paste0(
                  local_ui_id,
                  "_spectra"
                )]] <- plotly::renderPlotly({
                  decon_samples <- gsub(
                    "o",
                    ".",
                    sapply(
                      strsplit(
                        names(
                          result_list$deconvolution
                        ),
                        "_"
                      ),
                      `[`,
                      3
                    )
                  )

                  multiple_spectra(
                    results_list = result_list,
                    samples = names(
                      result_list$deconvolution
                    )[which(
                      decon_samples == local_concentration
                    )],
                    cubic = ifelse(
                      is.null(input[[paste0(local_ui_id, "_kind")]]) ||
                        input[[paste0(local_ui_id, "_kind")]] == "3D",
                      TRUE,
                      FALSE
                    ),
                    time = TRUE,
                    hits_summary = hits_summary,
                    units = units
                  )
                }) |>
                  shiny::bindEvent(input[[paste0(
                    local_ui_id,
                    "_kind"
                  )]])

                setup_plot_dl(
                  input,
                  output,
                  session,
                  paste0(local_ui_id, "_binding"),
                  build_fn = function(theme) {
                    make_binding_plot(
                      kobs_result = result_list$binding_kobs_result,
                      filter_conc = local_concentration,
                      colors = concentration_colors,
                      units = units,
                      theme = theme
                    )
                  },
                  filename_fn = function() {
                    paste0(get_session_prefix(), "_Binding_Curve")
                  }
                )

                decon_samples_local <- gsub(
                  "o",
                  ".",
                  sapply(
                    strsplit(names(result_list$deconvolution), "_"),
                    `[`,
                    3
                  )
                )
                setup_plot_dl(
                  input,
                  output,
                  session,
                  paste0(local_ui_id, "_spectra"),
                  build_fn = function(theme) {
                    multiple_spectra(
                      results_list = result_list,
                      samples = names(result_list$deconvolution)[which(
                        decon_samples_local == local_concentration
                      )],
                      cubic = ifelse(
                        is.null(input[[paste0(local_ui_id, "_kind")]]) ||
                          input[[paste0(local_ui_id, "_kind")]] == "3D",
                        TRUE,
                        FALSE
                      ),
                      time = TRUE,
                      hits_summary = hits_summary,
                      units = units,
                      theme = theme
                    )
                  },
                  filename_fn = function() {
                    paste0(get_session_prefix(), "_Mass_Spectra")
                  }
                )
              })
            }

            # Select binding results tab
            set_selected_tab("Hits", session)
          }
        }

        # Unblock UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "none";'
        ))
      },
      suspended = TRUE
    )

    ## Plot download handlers ----

    setup_plot_dl(
      input,
      output,
      session,
      "samples_spectrum",
      build_fn = function(theme) {
        result_list <- conversion_sidebar_vars$result_list()
        selected_sample <- input$conversion_sample_picker
        shiny::req(result_list, selected_sample)
        tbl <- conversion_vars$hits_summary |>
          dplyr::filter(`Sample ID` == selected_sample)
        spectrum_plot(
          sample = result_list$deconvolution[[selected_sample]],
          color_cmp = get_cmp_colorScale(
            filtered_table = tbl,
            scale = input$color_scale,
            variable = input$color_variable,
            trunc = input$truncate_names
          ),
          color_variable = input$color_variable,
          show_peak_labels = isTRUE(input$sample_view_spectrum_annotation),
          show_mass_diff = !isFALSE(input$sample_view_spectrum_diff),
          theme = theme
        )
      },
      filename_fn = function() {
        paste0(get_session_prefix(), "_Annotated_Spectrum")
      }
    )

    setup_plot_dl(
      input,
      output,
      session,
      "samples_cmp_dist",
      build_fn = function(theme) {
        shiny::req(
          conversion_vars$hits_summary,
          input$conversion_sample_picker,
          input$color_scale,
          input$color_variable,
          !is.null(input$truncate_names)
        )
        smpl_compound_distribution(
          hits_summary = conversion_vars$hits_summary,
          sample = input$conversion_sample_picker,
          color_variable = input$color_variable,
          truncate_names = input$truncate_names,
          color_scale = input$color_scale,
          theme = theme
        )
      },
      filename_fn = function() {
        paste0(get_session_prefix(), "_Compound_Distribution")
      }
    )

    setup_plot_dl(
      input,
      output,
      session,
      "compounds_spectrum",
      build_fn = function(theme) {
        hits_summary <- conversion_vars$hits_summary
        shiny::req(
          hits_summary,
          input$conversion_compound_picker,
          !is.null(input$truncate_names),
          input$color_variable,
          input$color_scale
        )
        result_list <- conversion_sidebar_vars$result_list()
        tbl <- dplyr::filter(
          hits_summary,
          `Cmp Name` == input$conversion_compound_picker
        )
        colors <- get_cmp_colorScale(
          filtered_table = tbl,
          scale = input$color_scale,
          variable = input$color_variable,
          trunc = input$truncate_names
        )
        samples <- unique(hits_summary$`Sample ID`[
          hits_summary$`Cmp Name` == input$conversion_compound_picker
        ])
        if (length(samples) == 1) {
          spectrum_plot(
            sample = result_list$deconvolution[[samples]],
            color_cmp = colors,
            color_variable = input$color_variable,
            show_peak_labels = TRUE,
            show_mass_diff = FALSE,
            theme = theme
          )
        } else {
          id_mapping <- data.frame(
            original = unique(hits_summary$`Sample ID`),
            truncated = label_smart_clean(unique(hits_summary$`Sample ID`))
          )
          multiple_spectra(
            results_list = result_list,
            samples = samples,
            cubic = is.null(input$compounds_spectrum_kind) ||
              input$compounds_spectrum_kind == "3D",
            color_cmp = colors,
            truncated = if (input$truncate_names) id_mapping else FALSE,
            color_variable = input$color_variable,
            hits_summary = hits_summary,
            labels_show = input$compounds_spectrum_labels,
            theme = theme
          )
        }
      },
      filename_fn = function() {
        paste0(get_session_prefix(), "_Annotated_Spectrum")
      }
    )

    setup_plot_dl(
      input,
      output,
      session,
      "compounds_cmp_dist",
      build_fn = function(theme) {
        shiny::req(
          conversion_vars$hits_summary,
          input$conversion_compound_picker,
          input$color_variable,
          !is.null(input$truncate_names),
          input$color_scale
        )
        cmp_compound_distribution(
          hits_summary = conversion_vars$hits_summary,
          compound = input$conversion_compound_picker,
          color_variable = input$color_variable,
          truncate_names = input$truncate_names,
          color_scale = input$color_scale,
          distribution_scale = input$cmp_distribution_scale,
          distribution_labels = input$cmp_distribution_labels,
          theme = theme
        )
      },
      filename_fn = function() {
        paste0(get_session_prefix(), "_Compound_Distribution")
      }
    )

    setup_plot_dl(
      input,
      output,
      session,
      "proteins_spectrum",
      build_fn = function(theme) {
        hits_summary <- conversion_vars$hits_summary
        shiny::req(
          hits_summary,
          input$conversion_protein_picker,
          !is.null(input$truncate_names),
          input$color_variable,
          input$color_scale
        )
        result_list <- conversion_sidebar_vars$result_list()
        tbl <- dplyr::filter(
          hits_summary,
          `Protein` == input$conversion_protein_picker
        )
        colors <- if (nrow(tbl)) {
          get_cmp_colorScale(
            filtered_table = tbl,
            scale = input$color_scale,
            variable = input$color_variable,
            trunc = input$truncate_names
          )
        } else {
          NULL
        }
        samples <- unique(hits_summary$`Sample ID`[
          hits_summary$`Protein` == input$conversion_protein_picker
        ])
        if (length(samples) == 1) {
          spectrum_plot(
            sample = result_list$deconvolution[[samples]],
            color_cmp = colors,
            color_variable = input$color_variable,
            show_peak_labels = TRUE,
            show_mass_diff = FALSE,
            theme = theme
          )
        } else {
          id_mapping <- data.frame(
            original = unique(hits_summary$`Sample ID`),
            truncated = label_smart_clean(unique(hits_summary$`Sample ID`))
          )
          multiple_spectra(
            results_list = result_list,
            samples = samples,
            cubic = is.null(input$proteins_spectrum_kind) ||
              input$proteins_spectrum_kind == "3D",
            color_cmp = colors,
            truncated = if (input$truncate_names) id_mapping else FALSE,
            color_variable = input$color_variable,
            hits_summary = hits_summary,
            labels_show = input$proteins_spectrum_labels,
            theme = theme
          )
        }
      },
      filename_fn = function() {
        paste0(get_session_prefix(), "_Annotated_Spectrum")
      }
    )

    setup_plot_dl(
      input,
      output,
      session,
      "proteins_cmp_dist",
      build_fn = function(theme) {
        shiny::req(
          conversion_vars$hits_summary,
          input$conversion_protein_picker,
          input$color_variable,
          !is.null(input$truncate_names),
          input$color_scale
        )
        prot_compound_distribution(
          hits_summary = conversion_vars$hits_summary,
          protein = input$conversion_protein_picker,
          color_variable = input$color_variable,
          truncate_names = input$truncate_names,
          color_scale = input$color_scale,
          distribution_scale = input$protein_distribution_scale,
          distribution_labels = input$protein_distribution_labels,
          theme = theme
        )
      },
      filename_fn = function() {
        paste0(get_session_prefix(), "_Compound_Distribution")
      }
    )

    setup_table_dl(
      input,
      output,
      session,
      "samples_table_view",
      data_fn = function() prepare_hits_export(samples_table_view_raw()),
      filename_fn = function() {
        paste0(get_session_prefix(), "_Table_View_Samples")
      }
    )

    setup_table_dl(
      input,
      output,
      session,
      "compounds_table_view",
      data_fn = function() prepare_hits_export(compounds_table_view_raw()),
      filename_fn = function() {
        paste0(get_session_prefix(), "_Table_View_Compounds")
      }
    )

    setup_table_dl(
      input,
      output,
      session,
      "proteins_table_view",
      data_fn = function() prepare_hits_export(proteins_table_view_raw()),
      filename_fn = function() {
        paste0(get_session_prefix(), "_Table_View_Proteins")
      }
    )

    setup_table_dl(
      input,
      output,
      session,
      "kobs_result",
      data_fn = function() {
        tbl <- kobs_result_raw()
        tbl$Included <- unname(conversion_vars$select_concentration)
        tbl
      },
      filename_fn = function() paste0(get_session_prefix(), "_Binding_Analysis")
    )

    ## Observer for conversion result interface ----
    ### Update label inputs depending on truncated samples ----
    safe_observe(
      event_expr = input$truncate_names,
      observer_name = "Truncated Label Input Updater",
      handler_fn = function() {
        shiny::req(
          conversion_vars$hits_summary,
          !is.null(conversion_sidebar_vars$run_ki_kinact()),
          input$truncate_names,
          input$conversion_compound_picker
        )

        tbl <- dplyr::filter(
          conversion_vars$hits_summary,
          `Cmp Name` == input$conversion_compound_picker
        )

        if (input$truncate_names) {
          sample_ids <- tbl$`truncSample_ID`
        } else {
          sample_ids <- tbl$`Sample ID`
        }

        shinyWidgets::updateMaterialSwitch(
          session = session,
          "cmp_distribution_labels",
          value = max(nchar(unique(sample_ids))) <= 22 |
            nrow(tbl) < 4
        )

        shinyWidgets::updateMaterialSwitch(
          session = session,
          "compounds_spectrum_labels",
          value = length(unique(sample_ids)) <= 8 &
            max(nchar(as.character(sample_ids))) <= 20
        )
      }
    )

    ### Enable/Disable hits table expand samples input ----
    safe_observe(
      observer_name = "Enable/Disable Hits Table Expand Samples Input",
      handler_fn = function() {
        shiny::req(conversion_vars$hits_summary)

        if (!is.null(input$relbinding_hits_tab_expand)) {
          shinyjs::toggleState(
            id = "relbinding_hits_tab_expand",
            condition = any(duplicated(
              conversion_vars$hits_summary$`Sample ID`
            ))
          )
          shinyjs::toggleClass(
            selector = ".hits-tab-expand-box .checkbox",
            class = "checkbox-disable"
          )
        }

        if (!is.null(input$kikinact_hits_tab_expand)) {
          shinyjs::toggleState(
            id = "kikinact_hits_tab_expand",
            condition = any(duplicated(
              conversion_vars$hits_summary$`Sample ID`
            ))
          )
          shinyjs::toggleClass(
            selector = ".hits-tab-expand-box .checkbox",
            class = "checkbox-disable"
          )
        }
      }
    )

    ### Enable/Disable hits table NA exclude input ----
    safe_observe(
      observer_name = "Enable/Disable Hits Table NA Exclude Input",
      handler_fn = function() {
        shiny::req(conversion_vars$hits_summary)

        if (!is.null(input$relbinding_hits_tab_na)) {
          shinyjs::toggleState(
            id = "relbinding_hits_tab_na",
            condition = anyNA(conversion_vars$hits_summary) &
              !all(is.na(conversion_vars$hits_summary$`Cmp Name`))
          )
          shinyjs::toggleClass(
            selector = ".hits-tab-na-box .checkbox",
            class = "checkbox-disable"
          )
        }

        if (!is.null(input$kikinact_hits_tab_na)) {
          shinyjs::toggleState(
            id = "kikinact_hits_tab_na",
            condition = anyNA(conversion_vars$hits_summary) &
              !all(is.na(conversion_vars$hits_summary$`Cmp Name`))
          )
          shinyjs::toggleClass(
            selector = ".hits-tab-na-box .checkbox",
            class = "checkbox-disable"
          )
        }
      }
    )

    ## Events for conversion result interface ----

    ### Reevaluate color scales depending on n of unique variable values ----
    safe_observe(
      event_expr = list(
        input$color_variable,
        conversion_sidebar_vars$analysis_select(),
        conversion_sidebar_vars$run_analysis()
      ),
      observer_name = "Color Scale Evaluation",
      handler_fn = function() {
        shiny::req(
          conversion_vars$hits_summary,
          input$color_variable,
          conversion_sidebar_vars$analysis_select() == 1
        )

        color_scale <- input$color_scale
        hits_summary <- conversion_vars$hits_summary

        scales <- filter_color_list(
          list(
            Qualitative = qualitative_scales,
            Sequential = sequential_scales
          ),
          length(unique(
            if (input$color_variable == "Samples") {
              hits_summary$`Sample ID`
            } else {
              hits_summary$`Cmp Name`
            }
          ))
        )

        scales[["Gradient"]] <- gradient_scales

        if (!is.null(color_scale) && color_scale %in% unlist(scales)) {
          selected <- color_scale
          render_trigger(render_trigger() + 1)
        } else {
          selected <- "plasma"
        }

        shiny::updateSelectInput(
          session = session,
          "color_scale",
          choices = scales,
          selected = selected
        )
      }
    )

    ### Expand samples from hits table ----
    safe_observe(
      event_expr = input$relbinding_hits_tab_expand,
      observer_name = "Color Scale Evaluation",
      handler_fn = function() {
        if (isFALSE(conversion_vars$expand_helper)) {
          shinyjs::removeClass(
            selector = ".hits-tab-col-select-ui .form-group",
            class = "custom-disable"
          )

          choices <- hits_table_names[
            !hits_table_names %in%
              c(
                "Sample ID",
                "Cmp Name",
                if (length(units) == 2) {
                  c(
                    conversion_vars$units[["Concentration"]],
                    conversion_vars$units[["Time"]]
                  )
                },
                "truncSample_ID"
              )
          ]

          selected <- hits_table_names[
            !hits_table_names %in%
              c(
                "Sample ID",
                "Cmp Name",
                if (length(units) == 2) {
                  c(
                    conversion_vars$units[["Concentration"]],
                    conversion_vars$units[["Time"]]
                  )
                },
                "truncSample_ID"
              )
          ][-c(1:2, 4:5, 7, 9)]

          if (!is.null(input$relbinding_hits_tab_col_select)) {
            shinyWidgets::updatePickerInput(
              session,
              "relbinding_hits_tab_col_select",
              choices = choices,
              selected = selected
            )
          }

          if (!is.null(input$kikinact_hits_tab_col_select)) {
            shinyWidgets::updatePickerInput(
              session,
              "kikinact_hits_tab_col_select",
              choices = choices,
              selected = selected
            )
          }

          if (!is.null(input$relbinding_binding_chart)) {
            shinyWidgets::updatePickerInput(
              session,
              "relbinding_binding_chart",
              choices = c("%-Binding", "Total %-Binding"),
              selected = "Total %-Binding",
            )
          }

          if (!is.null(input$kikinact_binding_chart)) {
            shinyWidgets::updatePickerInput(
              session,
              "kikinact_binding_chart",
              choices = c("%-Binding", "Total %-Binding"),
              selected = "Total %-Binding",
            )
          }

          conversion_vars$expand_helper <- TRUE
        } else {
          shinyjs::addClass(
            selector = ".hits-tab-col-select-ui .form-group",
            class = "custom-disable"
          )

          if (!is.null(input$relbinding_hits_tab_col_select)) {
            shinyWidgets::updatePickerInput(
              session,
              "relbinding_hits_tab_col_select",
              choices = c("Theor. Prot. [Da]", "Total %-Binding"),
              selected = c("Theor. Prot. [Da]", "Total %-Binding")
            )
          }

          if (!is.null(input$kikinact_hits_tab_col_select)) {
            shinyWidgets::updatePickerInput(
              session,
              "kikinact_hits_tab_col_select",
              choices = c("Theor. Prot. [Da]", "Total %-Binding"),
              selected = c("Theor. Prot. [Da]", "Total %-Binding")
            )
          }

          if (!is.null(input$relbinding_binding_chart)) {
            shinyWidgets::updatePickerInput(
              session,
              "relbinding_binding_chart",
              choices = "Total %-Binding",
              selected = "Total %-Binding"
            )
          }

          if (!is.null(input$kikinact_binding_chart)) {
            shinyWidgets::updatePickerInput(
              session,
              "kikinact_binding_chart",
              choices = "Total %-Binding",
              selected = "Total %-Binding"
            )
          }

          conversion_vars$expand_helper <- FALSE
        }
      }
    )

    ### Recalculate results depending on excluded concentrations ----
    safe_observe(
      event_expr = input[["kobs_result_cell_edit"]],
      observer_name = "Deconvolution Results Transfer",
      handler_fn = function() {
        # Apply changes to included concentrations
        conversion_vars$select_concentration[
          input[["kobs_result_cell_edit"]]$row
        ] <- input[[
          "kobs_result_cell_edit"
        ]]$value

        # Check number of selected concentrations
        if (sum(conversion_vars$select_concentration) < 3) {
          shinyWidgets::show_toast(
            "≥ 3 concentrations needed",
            type = "warning",
            timer = 3000
          )

          # Dont apply changes
          return(NULL)
        }

        # Recalculate result object according to included concentrations
        result_list <- conversion_sidebar_vars$result_list()

        # Transformed units argument
        units_adapt <- c(
          Concentration = gsub(
            ".*\\[(.+)\\].*",
            "\\1",
            conversion_vars$units[["Concentration"]]
          ),
          Time = gsub(".*\\[(.+)\\].*", "\\1", conversion_vars$units[["Time"]])
        )

        # Add binding/kobs results to result list
        result_list$binding_kobs_result <- add_kobs_binding_result(
          result_list$hits_summary,
          concentrations_select = names(
            conversion_vars$select_concentration
          )[which(conversion_vars$select_concentration)],
          units = units_adapt,
          conc_time = conversion_vars$units
        )

        # Add Ki/kinact results to result list
        result_list$ki_kinact_result <- add_ki_kinact_result(
          result_list,
          units = units_adapt
        )

        # Assign modified results to reactive variable
        conversion_vars$modified_results <- result_list
      }
    )

    # Tooltips ----

    ## Sample declaration tooltip ----
    safe_observe(
      event_expr = input$resultinput_tooltip_bttn,
      observer_name = "Tooltips Displayer",
      handler_fn = function() {
        shiny::showModal(
          shiny::div(
            class = "tip-modal",
            shiny::modalDialog(
              shiny::column(
                width = 12,
                shiny::div(
                  class = "tooltip-text",
                  "Continue from Deconvolution or upload a result file containing deconvolved samples."
                ),
                shiny::br(),
                shiny::div(
                  class = "tooltip-text",
                  "Assign each sample their contained protein and compound(s). If a Ki/kinact analysis is intended, samples need to be annotated with their corresponding compound concentration and incubation time. Sample annotation can be performed via file upload or by filling the table directly. The table also supports copy/paste for efficient filling."
                )
              ),
              title = "Samples Declaration",
              easyClose = TRUE,
              footer = shiny::tagList(
                shiny::modalButton("Dismiss")
              )
            )
          )
        )
      }
    )

    ## Fileinput tooltips ----
    safe_observe(
      event_expr = input$fileinput_tooltip_bttn,
      observer_name = "Tooltips Displayer",
      handler_fn = function() {
        shiny::req(input$tabs)

        if (input$tabs == "Proteins") {
          ## Protein declaration ----
          title <- "Protein Declaration"
          hints <- shiny::column(
            width = 12,
            shiny::div(
              class = "tooltip-text",
              "One or more proteins can be screened for. The protein names/IDs together with their mass values [Da] can be defined either via file upload or by entering the values into the table. The table also supports copy/paste for efficient filling."
            ),
            shiny::br(),
            shiny::div(
              class = "tooltip-img-text",
              "The format requires the name/ID as first column and up to nine columns the theoretical mass as well as any mass shifts per protein. Headers are optional."
            ),
            shiny::br(),
            shiny::tags$img(src = "static/protein_table.png"),
            shiny::br()
          )
        } else if (input$tabs == "Compounds") {
          ## Compound declaration ----
          title <- "Compound Declaration"
          hints <- shiny::column(
            width = 12,
            shiny::div(
              class = "tooltip-text",
              "One or more compounds can be screened for. The compound names/IDs together with their mass values [Da] can be defined either via file upload or by entering the values into the table. The table also supports copy/paste for efficient filling."
            ),
            shiny::br(),
            shiny::div(
              class = "tooltip-img-text",
              "The format requires the name/ID as first column and up to nine columns the theoretical mass as well as any mass shifts per compound. Headers are optional."
            ),
            shiny::br(),
            shiny::tags$img(
              style = "width: 60vw;",
              src = "static/compound_table.png"
            ),
            shiny::br()
          )
        }
        # TODO
        # Add tooltips for result interface
        # else if (input$tabs == "Binding") {
        #   ## Binding analysis ----
        #   title <- "Binding Analysis"
        #   hints <- shiny::column(
        #     width = 12,
        #     shiny::withMathJax(),
        #     shiny::div(
        #       class = "tooltip-text",
        #       shiny::p(
        #         "This tab displays the complete concentration series for a single compound, enabling global determination of the two fundamental covalent binding parameters: ",
        #         shiny::strong("k", htmltools::tags$sub("inact")),
        #         " (maximum inactivation rate at saturation) and ",
        #         shiny::strong("K", htmltools::tags$sub("i")),
        #         " (apparent dissociation constant of the initial reversible complex)."
        #       ),
        #       shiny::p(
        #         "Individual time-courses are fitted to extract k",
        #         htmltools::tags$sub("obs"),
        #         " values, which are then globally fitted to the hyperbolic two-step model to yield the second-order rate constant ",
        #         shiny::strong(
        #           "k",
        #           htmltools::tags$sub("inact"),
        #           " / K",
        #           htmltools::tags$sub("i")
        #         ),
        #         " — the gold-standard metric of covalent binder efficiency at low occupancy."
        #       )
        #     )
        #   )
        # } else if (conversion_main_vars$selected_tab() == "Hits") {
        #   ## Hits table ----
        #   title <- "Hits Table"
        #   hints <- shiny::fluidRow(
        #     shiny::br(),
        #     shiny::column(
        #       width = 11,
        #       shiny::div(
        #         class = "tooltip-text",
        #         shiny::p(
        #           "The hits table lists all peak signals that correspond to the declared proteins and compounds with respect to their molecular weights including mass shifts and multiple binding (stoichiometry). Signals that fall within the user-determined peak tolerance values are considered."
        #         ),
        #         htmltools::tags$ul(
        #           htmltools::tags$li(
        #             shiny::strong("Well / Sample ID"),
        #             " – plate well and sample name or ID"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("Conc."),
        #             " – compound concentration"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("Time"),
        #             " – incubation time point"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("Theor. Prot."),
        #             " – theoretical mass of the unmodified protein"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("Meas. Prot."),
        #             " – measured deconvolved mass of the protein species"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("Δ Prot."),
        #             " – difference between theoretical and measured deconvolved protein mass"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("Int. Prot."),
        #             " – relative intensity of the unmodified protein peak [%]"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("Peak Signal"),
        #             " – raw signal intensity for present peak"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("Int. Cmp"),
        #             " – intensity of the peak representing the protein together with a compound adduct [%]"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("Cmp Name"),
        #             " – compound name or ID of the bound compound"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("Theor. Cmp"),
        #             " – theoretical mass of the bound compound"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("Δ Cmp"),
        #             " – difference between theoretical complex and the obtained deconvolved mass [Da]"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("Bind. Stoich."),
        #             " – detected binding stoichiometry (no. of bound compounds)"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("%-Binding"),
        #             " – percentage of protein that has formed the covalent adduct at this time point"
        #           ),
        #           htmltools::tags$li(
        #             shiny::strong("Total %-Binding"),
        #             " – cumulative %-binding (identical to %-Binding when only one adduct is present)"
        #           )
        #         ),
        #         shiny::p(
        #           "The ",
        #           shiny::strong("%-Binding"),
        #           " (or ",
        #           shiny::strong("Total %-Binding"),
        #           ") values are used to construct the binding curve and to derive ",
        #           shiny::strong("k", htmltools::tags$sub("obs")),
        #           ", plateau, and initial velocity (v)."
        #         )
        #       )
        #     )
        #   )
        # } else {
        #   ## Concentration time series ----
        #   title <- "Single Concentration Time Series"
        #   hints <- "Binding parameters derived from mass spectra of time series measurements of a single concentration."
        # }

        shiny::showModal(
          shiny::div(
            class = "tip-modal",
            shiny::modalDialog(
              hints,
              title = title,
              easyClose = TRUE,
              footer = shiny::tagList(
                shiny::modalButton("Dismiss")
              )
            )
          )
        )
      }
    )

    ## Binding curve ----
    shiny::observeEvent(input$binding_curve_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span("Binding Curve"),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "Time-course of covalent adduct formation (% modified protein) measured by intact-mass MS at each compound concentration."
                  ),
                  shiny::p("Each trace is individually fitted to:"),
                  shiny::div(
                    class = "math-display",
                    "$$\\%\\,\\text{binding} = 100 \\times \\frac{v}{k_{\\text{obs}}} \\times \\big(1 - \\exp(-k_{\\text{obs}} \\cdot t)\\big)$$"
                  ),
                  shiny::p(
                    "An anchor point is forced at (t = 0, binding = 0). The plateau ",
                    shiny::strong("100 × v / k", htmltools::tags$sub("obs")),
                    " is the maximum covalent occupancy achievable."
                  )
                )
              )
            )
          )
        )
      )
    })

    ## Kobs curve ----
    shiny::observeEvent(input$kobs_curve_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span(
              "k",
              htmltools::tags$sub("obs"),
              " Curve"
            ),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "Plot of observed rate constants ",
                    shiny::strong("k", htmltools::tags$sub("obs")),
                    " versus compound concentration [C]."
                  ),
                  shiny::p(
                    "Global fit to the two-step covalent binding model:"
                  ),
                  shiny::div(
                    class = "math-display",
                    "$$k_{\\text{obs}} = \\frac{k_{\\text{inact}} \\times [C]}{K_{\\text{i}} + [C]}$$"
                  ),
                  shiny::p(
                    "Yields ",
                    shiny::strong("k", htmltools::tags$sub("inact")),
                    " (maximum covalent rate) and ",
                    shiny::strong("K", htmltools::tags$sub("i")),
                    " (apparent affinity of the initial reversible complex)."
                  )
                )
              )
            )
          )
        )
      )
    })

    ## Binding analysis ----
    shiny::observeEvent(input$binding_analysis_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = "Binding Analysis Table",
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "Results from individual exponential fits per concentration:"
                  ),
                  htmltools::tags$ul(
                    htmltools::tags$li(
                      shiny::strong("k", htmltools::tags$sub("obs")),
                      " – observed rate constant (time⁻¹)"
                    ),
                    htmltools::tags$li(
                      shiny::strong("v"),
                      " – fitted initial velocity parameter"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Plateau"),
                      " – max % modification = 100 × v / k",
                      htmltools::tags$sub("obs")
                    )
                  ),
                  shiny::p(
                    "The ",
                    shiny::strong("Included"),
                    " checkbox controls inclusion in the global fit for ",
                    shiny::strong("k", htmltools::tags$sub("inact")),
                    " and ",
                    shiny::strong("K", htmltools::tags$sub("i")),
                    "."
                  )
                )
              )
            )
          )
        )
      )
    })

    ## Kinact value ----
    shiny::observeEvent(input$kinact_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span("k", htmltools::tags$sub("inact")),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    shiny::strong("k", htmltools::tags$sub("inact")),
                    " is the maximum first-order rate constant for covalent bond formation when the target is fully saturated (EC → EC*)."
                  ),
                  shiny::p(
                    "It measures intrinsic warhead reactivity and transition-state stabilization in the reversible complex, independent of binding affinity."
                  )
                )
              )
            )
          )
        )
      )
    })

    ## Kobs value ----
    shiny::observeEvent(input$kobs_value_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span("k", htmltools::tags$sub("obs")),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    shiny::strong("k", htmltools::tags$sub("obs")),
                    " is the ",
                    shiny::em("observed pseudo-first-order rate constant"),
                    " for covalent adduct formation at this specific compound concentration."
                  ),
                  shiny::p(
                    "It is extracted from the single-exponential fit of the binding curve shown."
                  ),
                  shiny::p(
                    "Units: typically min⁻¹ or s⁻¹. Under the two-step model:"
                  ),
                  shiny::div(
                    class = "math-display",
                    "$$k_{\\text{obs}} = \\frac{k_{\\text{inact}} \\times [C]}{K_{\\text{i}} + [C]}$$"
                  ),
                  shiny::p(
                    "At low [C], k",
                    htmltools::tags$sub("obs"),
                    " ∝ [C]; at high [C], k",
                    htmltools::tags$sub("obs"),
                    " → k",
                    htmltools::tags$sub("inact"),
                    "."
                  )
                )
              )
            )
          )
        )
      )
    })

    ## Ki value ----
    shiny::observeEvent(input$Ki_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span("K", htmltools::tags$sub("i")),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    shiny::strong("K", htmltools::tags$sub("i")),
                    " is the apparent dissociation constant of the initial non-covalent complex (E + C ⇌ EC)."
                  ),
                  shiny::p(
                    "Lower K",
                    htmltools::tags$sub("i"),
                    " indicates stronger reversible binding before covalency."
                  )
                )
              )
            )
          )
        )
      )
    })

    ## Ki/kinact value ----
    shiny::observeEvent(input$Ki_kinact_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span(
              "K",
              htmltools::tags$sub("i"),
              " / k",
              htmltools::tags$sub("inact")
            ),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "The key potency metric is the ",
                    shiny::strong("second-order rate constant"),
                    ":"
                  ),
                  shiny::div(
                    class = "math-display",
                    "$$\\frac{k_{\\text{inact}}}{K_{\\text{i}}}\\;(\\text{M}^{-1}\\text{s}^{-1})$$"
                  ),
                  shiny::p(
                    "Higher values indicate more efficient covalent labeling at low concentration."
                  )
                )
              )
            )
          )
        )
      )
    })

    ## Mass spectra ----
    shiny::observeEvent(input$mass_spectra_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span("Mass Spectra"),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "Intact-protein mass spectra acquired at different incubation time points with the compound (",
                    shiny::strong("[C]"),
                    " = current concentration)."
                  ),
                  shiny::p(
                    "Each trace shows the charge-state envelope of the protein. Progression from unmodified (lower m/z) to covalently modified species (higher m/z) is visible over time."
                  ),
                  shiny::p(
                    "The theoretical unmodified protein mass is marked. Mass shifts correspond to covalent adduct formation (+ compound mass)."
                  ),
                  shiny::p(
                    "Deconvolution into zero-charge mass distribution yields the % modified protein used to construct the binding curve."
                  )
                )
              )
            )
          )
        )
      )
    })

    ## Single binding plot ----
    shiny::observeEvent(input$binding_curve_single_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span(
              "Binding Curve (Single Concentration)"
            ),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "Time-resolved covalent adduct formation at a ",
                    shiny::strong("single fixed compound concentration"),
                    " derived from deconvolved intact-mass spectra."
                  ),
                  shiny::p(
                    "Fitted individually to the integrated rate equation:"
                  ),
                  shiny::div(
                    class = "math-display",
                    "$$\\%\\,\\text{binding} = 100 \\times \\frac{v}{k_{\\text{obs}}} \\times \\big(1 - \\exp(-k_{\\text{obs}} \\cdot t)\\big)$$"
                  ),
                  shiny::p(
                    "Yields ",
                    shiny::strong("k", htmltools::tags$sub("obs")),
                    " (observed rate constant), ",
                    shiny::strong("v"),
                    " (initial velocity), and ",
                    shiny::strong("Plateau"),
                    " (maximum achievable labeling at this concentration)."
                  ),
                  shiny::p(
                    "An anchor point at (t = 0, binding = 0) is forced for numerical stability."
                  )
                )
              )
            )
          )
        )
      )
    })

    ## Hits table ----
    shiny::observeEvent(input$hits_table_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal hits-table-tooltip",
          shiny::modalDialog(
            title = "Hits Table",
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "Detailed per-time-point results from intact-mass MS deconvolution and peak assignment at the currently selected compound concentration."
                  ),
                  htmltools::tags$ul(
                    htmltools::tags$li(
                      shiny::strong("Well / Sample ID"),
                      " – plate well and sample name or ID"
                    ),
                    htmltools::tags$li(
                      shiny::strong("[Cmp]"),
                      " – compound concentration"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Time"),
                      " – incubation time point"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Theor. Prot. [Da]"),
                      " – theoretical mass of the unmodified protein"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Meas. Prot. [Da]"),
                      " – measured deconvolved mass of the protein species"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Δ Prot. [Da]"),
                      " – difference between theoretical and measured deconvolved protein mass"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Int. Prot."),
                      " – relative intensity of the unmodified protein peak [%]"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Peak Signal [Da]"),
                      " – measured peak mass of the compound"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Int. Cmp"),
                      " – intensity of the peak representing the protein together with a compound adduct [%]"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Cmp Name"),
                      " – compound name or ID of the bound compound"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Theor. Cmp [Da]"),
                      " – theoretical mass of the bound compound"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Δ Cmp [Da]"),
                      " – difference between theoretical complex and the obtained deconvolved mass"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Bind. Stoich."),
                      " – detected binding stoichiometry (no. of bound compounds)"
                    ),
                    htmltools::tags$li(
                      shiny::strong("%-Binding"),
                      " – percentage of protein that has formed the covalent adduct at this time point"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Total %-Binding"),
                      " – cumulative %-binding (identical to %-Binding when only one adduct is present)"
                    )
                  ),
                  shiny::p(
                    "The ",
                    shiny::strong("%-Binding"),
                    " (or ",
                    shiny::strong("Total %-Binding"),
                    ") values are used to construct the binding curve and to derive ",
                    shiny::strong("k", htmltools::tags$sub("obs")),
                    ", plateau, and initial velocity (v)."
                  )
                )
              )
            )
          )
        )
      )
    })

    ## Binding plateau ----
    shiny::observeEvent(input$binding_plateau_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = "Binding Plateau",
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "The ",
                    shiny::strong("Binding Plateau"),
                    " is the maximum percentage of covalently modified protein achieved at this compound concentration after long incubation."
                  ),
                  shiny::p(
                    "It is calculated as: ",
                    shiny::strong(
                      "Plateau = 100 × v / k",
                      htmltools::tags$sub("obs")
                    )
                  ),
                  shiny::p(
                    "Values < 100% indicate that even at infinite time, not all protein molecules form the covalent adduct — often due to competing off-pathway reactions or incomplete saturation of the reversible EC complex."
                  ),
                  shiny::p(
                    "Plateau approaches 100% only when [C] ≫ K",
                    htmltools::tags$sub("i"),
                    " and the covalent step is strongly favored."
                  )
                )
              )
            )
          )
        )
      )
    })

    ## Velocity v ----
    shiny::observeEvent(input$v_value_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = "v (Initial Velocity Parameter)",
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    shiny::strong("v"),
                    " is a fitted parameter representing the initial rate of adduct formation scaled by the plateau."
                  ),
                  shiny::p(
                    "From the fitting model: ",
                    shiny::strong(
                      "Plateau = 100 × v / k",
                      htmltools::tags$sub("obs")
                    ),
                    " → v = (Plateau × k",
                    htmltools::tags$sub("obs"),
                    ") / 100"
                  ),
                  shiny::p(
                    "It has units of %·time⁻¹ and is useful mainly as an intermediate for calculating the plateau."
                  ),
                  shiny::p(
                    "In practice, v is tightly correlated with k",
                    htmltools::tags$sub("obs"),
                    " and plateau; only k",
                    htmltools::tags$sub("obs"),
                    " is used in the global k",
                    htmltools::tags$sub("inact"),
                    "/K",
                    htmltools::tags$sub("i"),
                    " analysis."
                  )
                )
              )
            )
          )
        )
      )
    })

    # Eagerly render startup outputs so they are computed in the first reactive
    # flush and included in the same browser message as waiter_hide().
    shiny::outputOptions(output, "conversion_ui", suspendWhenHidden = FALSE)
    shiny::outputOptions(
      output,
      "declaration_info_ui",
      suspendWhenHidden = FALSE
    )

    # Return server values ----
    list(
      conversion_ready = shiny::reactive(declaration_vars$conversion_ready),
      input_list = shiny::reactive(list(
        Protein_Table = protein_table_data(),
        Compound_Table = compound_table_data(),
        Samples_Table = declaration_vars$sample_table,
        result = if (!is.null(declaration_vars$result$.db_path)) {
          read_decon_result(declaration_vars$result$.db_path)
        } else {
          declaration_vars$result
        }
      )),
      samples_confirmed = shiny::reactive(declaration_vars$samples_confirmed),
      cancel_continuation = shiny::reactive(input$conversion_cont_cancel),
      activate_ki_kinact = shiny::reactive(trigger_ki_kinact())
    )
  })
}
