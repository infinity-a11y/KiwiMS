# app/view/conversion_card.R

box::use(
  shiny[moduleServer, NS],
  bslib[nav_insert],
)

box::use(
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
      checkboxColumn,
      js_code_gen,
      new_sample_table,
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
    ],
  app / logic / deconvolution_functions[spectrum_plot, ],
  app /
    logic /
    conversion_constants[
      keybind_menu_ui,
      table_legend,
      sample_table_legend,
      empty_protein_table,
      conc_unit_input_ui,
      time_unit_input_ui,
      chart_js,
      sequential_scales,
      qualitative_scales,
      gradient_scales,
    ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  # Tab UI for whole protein conversion module
  bslib::navset_card_tab(
    id = ns("tabs"),
    bslib::nav_panel(
      "Proteins",
      shinyjs::useShinyjs(),
      waiter::useWaiter(),
      shiny::div(
        class = "comp-prot-controls",
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::div(
              class = "table-input",
              shiny::fileInput(
                ns("proteins_fileinput"),
                "",
                multiple = FALSE,
                accept = c(".csv", ".tsv", ".xlsx", ".xls", ".txt")
              )
            )
          ),
          shiny::column(
            width = 2,
            shiny::div(
              class = "conversion-checkbox",
              shiny::checkboxInput(
                ns("proteins_header_checkbox"),
                "Has header",
                value = FALSE
              )
            )
          ),
          shiny::column(
            width = 3,
            shiny::textOutput(ns("proteins_table_info")),
          ),
          shiny::column(
            width = 2,
            shiny::div(
              class = "full-width-btn",
              shinyjs::disabled(
                shiny::actionButton(
                  ns("confirm_proteins"),
                  label = "Save",
                  icon = shiny::icon("bookmark")
                )
              )
            )
          ),
          shiny::column(
            width = 2,
            shiny::div(
              class = "full-width-btn",
              shinyjs::disabled(
                shiny::actionButton(
                  ns("edit_proteins"),
                  label = "Edit",
                  icon = shiny::icon("pen-to-square")
                )
              )
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          rhandsontable::rHandsontableOutput(
            ns("proteins_table")
          ),
          table_legend
        )
      ),
      keybind_menu_ui
    ),
    bslib::nav_panel(
      "Compounds",
      shiny::div(
        class = "comp-prot-controls",
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::div(
              class = "table-input",
              shiny::fileInput(
                ns("compounds_fileinput"),
                "",
                multiple = FALSE,
                accept = c(".csv", ".tsv", ".xlsx", ".xls", ".txt")
              )
            )
          ),
          shiny::column(
            width = 2,
            shiny::div(
              class = "conversion-checkbox",
              shiny::checkboxInput(
                ns("compounds_header_checkbox"),
                "Has header",
                value = FALSE
              )
            )
          ),
          shiny::column(
            width = 3,
            shiny::textOutput(ns("compounds_table_info"))
          ),
          shiny::column(
            width = 2,
            shiny::div(
              class = "full-width-btn",
              shinyjs::disabled(
                shiny::actionButton(
                  ns("confirm_compounds"),
                  label = "Save",
                  icon = shiny::icon("bookmark")
                )
              )
            )
          ),
          shiny::column(
            width = 2,
            shiny::div(
              class = "full-width-btn",
              shinyjs::disabled(
                shiny::actionButton(
                  ns("edit_compounds"),
                  label = "Edit",
                  icon = shiny::icon("pen-to-square")
                )
              )
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          rhandsontable::rHandsontableOutput(ns("compounds_table")),
          table_legend
        )
      ),
      keybind_menu_ui
    ),
    bslib::nav_panel(
      "Samples",
      shiny::div(
        class = "samples-controls",
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::div(
              class = "table-input",
              shiny::fileInput(
                ns("samples_fileinput"),
                "Select File",
                multiple = FALSE,
                accept = c(".rds")
              )
            )
          ),
          shiny::column(
            width = 2,
            shiny::textOutput(ns("sample_number_info"))
          ),
          shiny::column(
            width = 3,
            shiny::textOutput(ns("samples_table_info"))
          ),
          shiny::column(
            width = 1,
            shiny::div(
              class = "full-width-btn",
              bslib::tooltip(
                shinyjs::disabled(
                  shiny::actionButton(
                    ns("confirm_samples"),
                    label = "",
                    icon = shiny::icon("bookmark")
                  )
                ),
                "Confirm Sample Table",
                placement = "top"
              )
            )
          ),
          shiny::column(
            width = 1,
            shiny::div(
              class = "full-width-btn",
              bslib::tooltip(
                shinyjs::disabled(
                  shiny::actionButton(
                    ns("edit_samples"),
                    label = "",
                    icon = shiny::icon("pen-to-square")
                  )
                ),
                "Edit Sample Table",
                placement = "top"
              )
            )
          ),
          shiny::column(
            width = 1,
            shiny::div(
              class = "unit-selectors",
              conc_unit_input_ui(ns)
            )
          ),
          shiny::column(
            width = 1,
            shiny::div(
              class = "unit-selectors",
              time_unit_input_ui(ns)
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 10,
          rhandsontable::rHandsontableOutput(ns("samples_table"))
        ),
        shiny::column(
          width = 2,
          rhandsontable::rHandsontableOutput(ns("samples_table_conc_time"))
        )
      ),
      sample_table_legend,
      keybind_menu_ui
    )
  )
}

#' @export
server <- function(id, conversion_sidebar_vars, deconvolution_main_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Set file upload limit
    options(shiny.maxRequestSize = 1000 * 1024^2)

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
      suppressWarnings(
        rhandsontable::hot_to_r(
          proteins_table
        )
      )
    }) |>
      shiny::debounce(millis = 500)

    # Throttled reactive for compound declaration table input
    compound_table_input <- shiny::reactive({
      compounds_table <- input$compounds_table
      shiny::req(compounds_table)
      suppressWarnings(
        rhandsontable::hot_to_r(
          compounds_table
        )
      )
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
    protein_table_data <- shiny::reactiveVal({
      na_num <- matrix(NA_real_, nrow = 9, ncol = 9) |>
        as.data.frame() |>
        stats::setNames(paste("Mass", 1:9))

      cbind(Protein = as.character(rep(NA, 9)), na_num)
    })
    compound_table_data <- shiny::reactiveVal({
      na_num <- matrix(NA_real_, nrow = 9, ncol = 9) |>
        as.data.frame() |>
        stats::setNames(paste("Mass", 1:9))

      cbind(Compound = as.character(rep(NA, 9)), na_num)
    })
    sample_table_data <- shiny::reactiveVal()
    conc_time_table_data <- shiny::reactiveVal()

    ## Concentration/Time UI ----
    # Conditional adaption of concentration/time input UI
    shiny::observe({
      if (
        isTRUE(conversion_sidebar_vars$run_ki_kinact()) &&
          isFALSE(declaration_vars$samples_confirmed)
      ) {
        shinyjs::removeClass(
          selector = ".unit-selectors .form-group .bootstrap-select",
          class = "custom-disable"
        )
        shinyjs::removeClass(
          selector = ".unit-selectors label",
          class = "custom-disable"
        )
        shinyjs::removeClass(
          selector = "#app-conversion_main-samples_table_conc_time",
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
        shinyjs::addClass(
          selector = "#app-conversion_main-samples_table_conc_time",
          class = "custom-disable"
        )
      }
    })

    # Render concentration / time input table
    output$samples_table_conc_time <- rhandsontable::renderRHandsontable({
      shiny::req(sample_table_data())

      shiny::isolate({
        if (is.null(conc_time_table_data())) {
          conc_time_tbl <- data.frame(
            Concentration = rep("", nrow(sample_table_data())),
            Time = rep("", nrow(sample_table_data()))
          )
        } else {
          conc_time_tbl <- conc_time_table_data()
        }
      })

      if (isFALSE(declaration_vars$samples_confirmed)) {
        rhandsontable::rhandsontable(
          conc_time_tbl,
          rowHeaders = NULL,
          height = 28 + 23 * nrow(conc_time_tbl),
          stretchH = "all"
        )
      } else {
        NULL
      }
    })

    ## Data table input events ----
    # Silently update table data table input status variables
    shiny::observeEvent(
      input$proteins_table,
      {
        shiny::req(input$proteins_table)

        # Save current table input
        suppressWarnings({
          protein_table_data(rhandsontable::hot_to_r(input$proteins_table))
        })
      },
      priority = 100
    )

    shiny::observeEvent(
      input$compounds_table,
      {
        shiny::req(input$compounds_table)

        # Save current table input
        suppressWarnings({
          compound_table_data(rhandsontable::hot_to_r(input$compounds_table))
        })
      },
      priority = 100
    )

    shiny::observeEvent(
      input$samples_table,
      {
        shiny::req(input$samples_table)

        # Save current table input
        suppressWarnings({
          samples_table <- rhandsontable::hot_to_r(input$samples_table)
        })

        # Remove concentration / time columns if present
        conc_col <- grep("Concentration", colnames(samples_table))
        time_col <- grep("Time", colnames(samples_table))
        if (length(conc_col) & length(time_col)) {
          samples_table <- samples_table[, -c(conc_col, time_col)]
        }

        # Assign to reactive variable
        sample_table_data(samples_table)
      },
      priority = 100
    )

    shiny::observeEvent(
      input$samples_table_conc_time,
      {
        shiny::req(input$samples_table_conc_time)

        # Save current table input
        suppressWarnings({
          conc_time_table_data(rhandsontable::hot_to_r(
            input$samples_table_conc_time
          ))
        })
      },
      priority = 100
    )

    ## UI Render Functions ----
    ### Table render functions ----
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
        sample_table_data(),
        declaration_vars$protein_table,
        declaration_vars$compound_table
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

    ### Sample number info ----
    output$sample_number_info <- shiny::renderText({
      paste(
        ifelse(
          !is.null(declaration_vars$result),
          length(declaration_vars$result$deconvolution),
          0
        ),
        "deconvoluted sample(s)"
      )
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
    shiny::observeEvent(
      input$table_paste_instant,
      {
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

    ## File upload handler ----
    ### Protein table file upload ----
    shiny::observeEvent(input$proteins_fileinput, {
      protein_table_data(handle_file_upload(
        file_input = input$proteins_fileinput,
        header_checkbox = input$proteins_header_checkbox,
        type = "protein",
        output = output,
        declaration_vars = declaration_vars
      ))
      declaration_vars$protein_table_disabled <- FALSE
      protein_table_trigger(protein_table_trigger() + 1)
    })

    ### Compound table file upload ----
    shiny::observeEvent(input$compounds_fileinput, {
      compound_table_data(handle_file_upload(
        file_input = input$compounds_fileinput,
        header_checkbox = input$compounds_header_checkbox,
        type = "compound",
        output = output,
        declaration_vars = declaration_vars
      ))
      declaration_vars$compound_table_disabled <- FALSE
      compound_table_trigger(compound_table_trigger() + 1)
    })

    ### Sample file upload ----
    shiny::observeEvent(
      input$samples_fileinput,
      {
        shiny::req(
          declaration_vars$protein_table,
          declaration_vars$compound_table
        )

        # Read results .rds file from selected path
        file_path <- file.path(input$samples_fileinput$datapath)
        declaration_vars$result <- readRDS(file_path)

        # Reset concentration / time input table
        conc_time_table_data(NULL)

        # New table data
        sample_table_data(new_sample_table(
          result = declaration_vars$result,
          protein_table = declaration_vars$protein_table,
          compound_table = declaration_vars$compound_table,
          ki_kinact = conversion_sidebar_vars$run_ki_kinact()
        ))
        sample_table_trigger(sample_table_trigger() + 1)
      }
    )

    ## Table status observer ----
    ### Observe table status for protein table ----
    shiny::observe(
      {
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
    shiny::observe(
      {
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
    shiny::observe({
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
          table = clean_sample_table(samples_table_input),
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
    })

    ## Edit button event ----
    shiny::observeEvent(
      input$edit_proteins | input$edit_compounds | input$edit_samples,
      {
        shiny::req(input$tabs)

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
            sample_table_data()
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
    shiny::observeEvent(
      input$confirm_proteins |
        input$confirm_compounds |
        input$confirm_samples,
      {
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
          if (isTRUE(conversion_sidebar_vars$run_ki_kinact())) {
            # Attach concentration/time table to clean non-NA sample table input
            samples_table_conc_time <- rhandsontable::hot_to_r(
              input$samples_table_conc_time
            )

            # Set colnames with selected time / concentration units
            colnames(samples_table_conc_time) <- c(
              paste0("Concentration [", input$conc_unit, "]"),
              paste0("Time [", input$time_unit, "]")
            )

            # Merge with samples table
            sample_table <- cbind(
              clean_sample_table(sample_table_input()),
              samples_table_conc_time
            )
          } else {
            # Get clean non-NA sample table without concentration/time input
            sample_table <- clean_sample_table(sample_table_input())
          }

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
    shiny::observe({
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
    })

    ## Continuiation from deconvolution to conversion ----
    ### User cancels samples table overwrite ----
    shiny::observeEvent(input$conversion_cont_cancel, {
      # Remove dialogue window
      shiny::removeModal()
    })

    ### User confirms samples table overwrite ----
    shiny::observeEvent(input$conversion_cont_conf, {
      # Block UI
      shinyjs::runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "block";'
      ))

      # Remove dialogue window
      shiny::removeModal()

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

      # Mark tab as undone
      shinyjs::runjs(
        'document.querySelector(".nav-link[data-value=\'Samples\']").classList.remove("done");'
      )

      # Activate table observer
      declaration_vars$sample_table_active <- TRUE

      # Read results .rds file from previous deconvolution
      declaration_vars$result <- readRDS(
        deconvolution_main_vars$continue_conversion()
      )

      # Reset concentration / time input table
      conc_time_table_data(NULL)

      # New table data
      sample_table_data(new_sample_table(
        result = declaration_vars$result,
        protein_table = declaration_vars$protein_table,
        compound_table = declaration_vars$compound_table
      ))
      sample_table_trigger(sample_table_trigger() + 1)

      # Unblock UI
      shinyjs::runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "none";'
      ))
    })

    ### Transfer results from deconvolution to sample table ----
    shiny::observeEvent(
      deconvolution_main_vars$continue_conversion(),
      {
        shiny::req(deconvolution_main_vars$continue_conversion())

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

          # Read results .rds file from previous deconvolution
          declaration_vars$result <- readRDS(
            deconvolution_main_vars$continue_conversion()
          )

          # Reset concentration / time input table
          conc_time_table_data(NULL)

          # New table data
          sample_table_data(new_sample_table(
            result = declaration_vars$result,
            protein_table = declaration_vars$protein_table,
            compound_table = declaration_vars$compound_table
          ))
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

    # Conversion Results ------------------

    ## Reactive variables ----
    # Reactive values conversion_vars
    conversion_vars <- shiny::reactiveValues(
      modified_results = NULL,
      select_concentration = NULL,
      formatted_hits = NULL,
      conc_colors = NULL,
      expand_helper = FALSE
    )

    # Reactive value to track current hits data frame
    hits_datatable_current <- shiny::reactiveVal()

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
    # If result was computed show result interface and hide declaration interace tabs
    shiny::observeEvent(conversion_sidebar_vars$run_analysis(), {
      if (!is.null(conversion_sidebar_vars$result_list())) {
        # Hide declaration tabs
        bslib::nav_hide(
          "tabs",
          "Proteins"
        )
        bslib::nav_hide(
          "tabs",
          "Compounds"
        )
        bslib::nav_hide(
          "tabs",
          "Samples"
        )

        if (isTRUE(conversion_sidebar_vars$run_ki_kinact())) {
          ### Ki/kinact analysis interface ----
          #### Transform hits for summary table ----
          hits_summary <- transform_hits(
            hits_summary = conversion_sidebar_vars$result_list()$"hits_summary",
            run_ki_kinact = TRUE
          )

          # Assign formatted hits to reactive variable
          conversion_vars$formatted_hits <- hits_summary

          #### Get concentrations and colors ----
          binding_kobs_result_names <- names(
            conversion_sidebar_vars$result_list()$binding_kobs_result
          )
          concentrations <- binding_kobs_result_names[
            !binding_kobs_result_names %in%
              c("binding_table", "binding_plot", "kobs_result_table")
          ]
          conc_selected <- rep(TRUE, length(concentrations))
          names(conc_selected) <- concentrations
          conversion_vars$select_concentration <- conc_selected

          # Assign colors to present concentrations
          concentration_colors <- rev(RColorBrewer::brewer.pal(
            n = length(concentrations),
            name = "Set1"
          ))
          names(concentration_colors) <- concentrations

          # Assign colors to reactive variable
          conversion_vars$conc_colors <- concentration_colors

          #### Binding tab ----
          bslib::nav_insert(
            "tabs",
            bslib::nav_panel(
              title = "Binding",
              shiny::div(
                class = "conversion-result-wrapper",
                shiny::uiOutput(ns("binding_tab"))
              )
            )
          )

          ##### Render binding tab UI ----
          output$binding_tab <- shiny::renderUI(
            shiny::div(
              class = "binding-analysis-tab",
              shiny::div(
                class = "card-custom",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    class = "bg-dark help-header",
                    "Binding Curve",
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("binding_curve_tooltip_bttn"),
                        label = "",
                        icon = shiny::icon("circle-question")
                      )
                    )
                  ),
                  bslib::card_body(
                    shinycssloaders::withSpinner(
                      plotly::plotlyOutput(
                        ns("binding_plot"),
                        height = "100%"
                      ),
                      type = 1,
                      color = "#7777f9"
                    )
                  )
                )
              ),
              shiny::div(
                class = "card-custom",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    class = "bg-dark help-header",
                    htmltools::tagList(
                      shiny::div(
                        "k",
                        htmltools::tags$sub("obs"),
                        " Curve"
                      )
                    ),
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("kobs_curve_tooltip_bttn"),
                        label = "",
                        icon = shiny::icon("circle-question")
                      )
                    )
                  ),
                  bslib::card_body(
                    shinycssloaders::withSpinner(
                      plotly::plotlyOutput(
                        ns("kobs_plot"),
                        height = "100%"
                      ),
                      type = 1,
                      color = "#7777f9"
                    )
                  )
                )
              ),
              shiny::div(
                class = "card-custom",
                bslib::card(
                  full_screen = TRUE,
                  bslib::card_header(
                    class = "bg-dark help-header",
                    "Binding Analysis",
                    shiny::div(
                      class = "tooltip-bttn",
                      shiny::actionButton(
                        ns("binding_analysis_tooltip_bttn"),
                        label = "",
                        icon = shiny::icon("circle-question")
                      )
                    )
                  ),
                  bslib::card_body(
                    shinycssloaders::withSpinner(
                      DT::DTOutput(ns("kobs_result")),
                      type = 1,
                      color = "#7777f9"
                    )
                  )
                )
              ),
              shiny::div(
                class = "result-cards",
                shiny::div(
                  class = "card-custom",
                  bslib::card(
                    bslib::card_header(
                      class = "bg-dark help-header",
                      htmltools::tagList(
                        shiny::div(
                          "k",
                          htmltools::tags$sub("inact")
                        )
                      ),
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::actionButton(
                          ns("kinact_tooltip_bttn"),
                          label = "",
                          icon = shiny::icon("circle-question")
                        )
                      )
                    ),
                    shiny::div(
                      class = "kobs-val",
                      shinycssloaders::withSpinner(
                        shiny::uiOutput(ns("kinact")),
                        type = 1,
                        color = "#7777f9"
                      )
                    )
                  )
                ),
                shiny::div(
                  class = "card-custom",
                  bslib::card(
                    bslib::card_header(
                      class = "bg-dark help-header",
                      htmltools::tagList(
                        shiny::div(
                          "K",
                          htmltools::tags$sub("i")
                        )
                      ),
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::actionButton(
                          ns("Ki_tooltip_bttn"),
                          label = "",
                          icon = shiny::icon("circle-question")
                        )
                      )
                    ),
                    shiny::div(
                      class = "kobs-val",
                      shinycssloaders::withSpinner(
                        shiny::uiOutput(ns("Ki")),
                        type = 1,
                        color = "#7777f9"
                      )
                    )
                  )
                ),
                shiny::div(
                  class = "card-custom",
                  bslib::card(
                    bslib::card_header(
                      class = "bg-dark help-header",
                      htmltools::tagList(
                        shiny::div(
                          "K",
                          htmltools::tags$sub("i"),
                          "/ k",
                          htmltools::tags$sub("inact"),
                        )
                      ),
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::actionButton(
                          ns("Ki_kinact_tooltip_bttn"),
                          label = "",
                          icon = shiny::icon("circle-question")
                        )
                      )
                    ),
                    shiny::div(
                      class = "kobs-val",
                      shinycssloaders::withSpinner(
                        shiny::uiOutput(ns("Ki_kinact")),
                        type = 1,
                        color = "#7777f9"
                      )
                    )
                  )
                )
              )
            )
          )

          ##### Calculated kinact value ----
          output$kinact <- shiny::renderUI({
            shiny::div(
              class = "result-card-content",
              shiny::div(
                class = "main-result",
                paste(
                  format_scientific(ki_kinact_result()[1, 1]),
                  "s⁻¹"
                )
              ),
              shiny::div(
                class = "error-result",
                paste(
                  "±",
                  format_scientific(ki_kinact_result()[1, 2])
                )
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

          ##### Calculated Ki value ----
          output$Ki <- shiny::renderUI({
            shiny::div(
              class = "result-card-content",
              shiny::div(
                class = "main-result",
                paste(
                  format_scientific(ki_kinact_result()[2, 1]),
                  "M⁻¹"
                )
              ),
              shiny::div(
                class = "error-result",
                paste(
                  "±",
                  format_scientific(ki_kinact_result()[2, 2])
                )
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

          ##### Calculated Ki/kinact value ----
          output$Ki_kinact <- shiny::renderUI({
            shiny::div(
              class = "result-card-content",
              shiny::div(
                class = "main-result",
                paste(
                  format_scientific(ki_kinact_result()[1, 1]),
                  "M⁻¹ s⁻¹"
                )
              )
            )
          })

          ##### Kobs result table ----
          output$kobs_result <- DT::renderDT(
            {
              shiny::req(
                conversion_sidebar_vars$result_list(),
                conversion_vars$conc_colors
              )

              # Get results
              kobs_results <- conversion_sidebar_vars$result_list()$binding_kobs_result$kobs_result_table

              kobs_results <- kobs_results |>
                dplyr::mutate(
                  kobs = paste(format(kobs, digits = 3), "s⁻¹"),
                  v = format(v, digits = 3),
                  plateau = paste(format(plateau, digits = 3), "%")
                )

              # Add concentration column
              kobs_results <- kobs_results |>
                dplyr::mutate(
                  Concentration = paste(rownames(kobs_results), "µM"),
                  .before = "kobs"
                )

              # Determine checkbox index
              checkbox_col_index <- 5

              # Add the checkbox
              kobs_results <- kobs_results |>
                dplyr::mutate(
                  Included = checkboxColumn(
                    nrow(kobs_results),
                    checkbox_col_index,
                    value = TRUE
                  )
                )

              # Set names
              colnames(kobs_results) <- c(
                "Concentration",
                "kobs",
                "Velocity",
                "Plateau",
                "Included"
              )

              DT::datatable(
                data = kobs_results,
                rownames = FALSE,
                selection = "none",
                escape = FALSE,
                class = "compact row-border nowrap",
                options = list(
                  dom = "t",
                  autoWidth = TRUE,
                  scrollX = TRUE,
                  scrollY = TRUE,
                  scrollCollapse = TRUE,
                  fixedHeader = TRUE,
                  stripe = FALSE
                ),
                editable = list(
                  target = "cell",
                  disable = list(columns = checkbox_col_index)
                ),
                callback = htmlwidgets::JS(js_code_gen(
                  "kobs_result",
                  checkbox_col_index,
                  ns = session$ns
                ))
              ) |>
                DT::formatStyle(
                  columns = 'Concentration',
                  target = 'row',
                  backgroundColor = DT::styleEqual(
                    levels = kobs_results$Concentration,
                    values = gsub(
                      ",1)",
                      ",0.6)",
                      plotly::toRGB(conversion_vars$conc_colors)
                    )
                  )
                ) |>
                DT::formatStyle(
                  1:4,
                  `border-right` = "solid 1px #0000005c"
                )
            },
            server = FALSE
          )

          ##### Binding plot ----
          output$binding_plot <- plotly::renderPlotly({
            shiny::req(conversion_sidebar_vars$result_list())

            conversion_sidebar_vars$result_list()$binding_kobs_result$binding_plot
          })

          ##### Kobs plot ----
          output$kobs_plot <- plotly::renderPlotly({
            shiny::req(conversion_sidebar_vars$result_list())

            if (is.null(conversion_vars$modified_results)) {
              result_list <- conversion_sidebar_vars$result_list()
            } else {
              result_list <- conversion_vars$modified_results
            }

            result_list$ki_kinact_result$kobs_plot
          })

          #### Hits tab ----
          bslib::nav_insert(
            "tabs",
            bslib::nav_panel(
              title = "Hits",
              shiny::div(
                class = "conversion-result-wrapper",
                # shiny::div(
                #   class = "tooltip-bttn hits-tab-tooltip",
                #   shiny::actionButton(
                #     ns("hits_table_tooltip_bttn"),
                #     label = "",
                #     icon = shiny::icon("circle-question")
                #   )
                # ),
                shinycssloaders::withSpinner(
                  DT::DTOutput(ns("hits_tab")),
                  type = 1,
                  color = "#7777f9"
                )
              )
            )
          )

          ##### Hits table ----
          output$hits_tab <- DT::renderDT({
            shiny::req(
              conversion_vars$formatted_hits,
              conversion_vars$conc_colors
            )

            render_hits_table(
              hits_table = conversion_vars$formatted_hits,
              concentration_colors = conversion_vars$conc_colors,
              withzero = any(
                conversion_vars$formatted_hits[["[Cmp]"]] == "0 µM"
              )
            )
          })

          #### Concentration tabs ----
          # Define a set of IDs for the dynamic concentration tabs
          dynamic_ui_ids <- paste0("concentration_tab_", concentrations)

          # Add tabs for each present concentration
          for (i in seq_along(concentrations)) {
            concentration <- concentrations[[i]]
            ui_id <- dynamic_ui_ids[[i]]

            bslib::nav_insert(
              "tabs",
              bslib::nav_panel(
                title = paste0("[", concentration, "]"),
                shiny::div(
                  class = "conversion-result-wrapper",
                  shiny::uiOutput(ns(ui_id))
                )
              )
            )
          }

          # Assign output names according to present concentrations
          lapply(names(output), function(name) {
            if (grepl("^concentration_tab_", name)) {
              output[[name]] <- NULL
            }
          })

          ##### Concentration tab loop ----
          for (i in seq_along(concentrations)) {
            concentration <- concentrations[[i]]
            ui_id <- dynamic_ui_ids[[i]]

            local({
              local_concentration <- concentration
              local_ui_id <- ui_id

              conc_result <- conversion_sidebar_vars$result_list()$binding_kobs_result[[
                local_concentration
              ]]

              ##### Render concentration interface UI ----
              output[[local_ui_id]] <- shiny::renderUI({
                shiny::div(
                  class = "result-conc-tab",
                  shiny::div(
                    class = "card-custom spectrum",
                    bslib::card(
                      bslib::card_header(
                        class = "bg-dark help-header",
                        "Mass Spectra",
                        shiny::div(
                          class = "spectrum-radio-button",
                          shinyWidgets::radioGroupButtons(
                            ns(paste0(
                              local_ui_id,
                              "_kind"
                            )),
                            choices = c("3D", "Planar")
                          )
                        ),
                        shiny::div(
                          class = "tooltip-bttn",
                          shiny::actionButton(
                            ns("mass_spectra_tooltip_bttn"),
                            label = "",
                            icon = shiny::icon("circle-question")
                          )
                        )
                      ),
                      full_screen = TRUE,
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput(
                          ns(paste0(local_ui_id, "_spectra")),
                          height = "100%"
                        ),
                        type = 1,
                        color = "#7777f9"
                      )
                    )
                  ),
                  shiny::div(
                    class = "card-custom binding",
                    bslib::card(
                      bslib::card_header(
                        class = "bg-dark help-header",
                        "Binding Curve",
                        shiny::div(
                          class = "tooltip-bttn",
                          shiny::actionButton(
                            ns("binding_curve_single_tooltip_bttn"),
                            label = "",
                            icon = shiny::icon("circle-question")
                          )
                        )
                      ),
                      full_screen = TRUE,
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput(
                          ns(paste0(
                            local_ui_id,
                            "_binding_plot"
                          )),
                          height = "100%"
                        ),
                        type = 1,
                        color = "#7777f9"
                      )
                    )
                  ),
                  shiny::div(
                    class = "result-cards",
                    shiny::div(
                      class = "card-custom",
                      bslib::card(
                        bslib::card_header(
                          class = "bg-dark help-header",
                          htmltools::tagList(
                            shiny::div(
                              "k",
                              htmltools::tags$sub("obs")
                            )
                          ),
                          shiny::div(
                            class = "tooltip-bttn",
                            shiny::actionButton(
                              ns("kobs_value_tooltip_bttn"),
                              label = "",
                              icon = shiny::icon("circle-question")
                            )
                          )
                        ),
                        shiny::div(
                          class = "kobs-val",
                          format_scientific(conc_result$kobs)
                        )
                      )
                    ),
                    shiny::div(
                      class = "card-custom",
                      bslib::card(
                        bslib::card_header(
                          class = "bg-dark help-header",
                          "Binding Plateau",
                          shiny::div(
                            class = "tooltip-bttn",
                            shiny::actionButton(
                              ns("binding_plateau_tooltip_bttn"),
                              label = "",
                              icon = shiny::icon("circle-question")
                            )
                          )
                        ),
                        shiny::div(
                          class = "kobs-val",
                          paste0(format_scientific(conc_result$plateau), "%")
                        )
                      )
                    ),
                    shiny::div(
                      class = "card-custom",
                      bslib::card(
                        bslib::card_header(
                          class = "bg-dark help-header",
                          "Velocity v",
                          shiny::div(
                            class = "tooltip-bttn",
                            shiny::actionButton(
                              ns("v_value_tooltip_bttn"),
                              label = "",
                              icon = shiny::icon("circle-question")
                            )
                          )
                        ),
                        shiny::div(
                          class = "kobs-val",
                          format_scientific(conc_result$v)
                        )
                      )
                    )
                  ),
                  shiny::div(
                    class = "card-custom hits",
                    bslib::card(
                      bslib::card_header(
                        class = "bg-dark help-header",
                        "Hits",
                        shiny::div(
                          class = "tooltip-bttn",
                          shiny::actionButton(
                            ns("hits_table_tooltip_bttn"),
                            label = "",
                            icon = shiny::icon("circle-question")
                          )
                        )
                      ),
                      full_screen = TRUE,
                      shiny::div(
                        class = "conc-hits-table",
                        shinycssloaders::withSpinner(
                          DT::DTOutput(ns(paste0(local_ui_id, "_hits"))),
                          type = 1,
                          color = "#7777f9"
                        )
                      )
                    )
                  )
                )
              })

              ##### Hits table ----
              output[[paste0(local_ui_id, "_hits")]] <- DT::renderDT({
                render_hits_table(
                  hits_table = hits_summary |>
                    dplyr::filter(
                      `[Cmp]` == paste(local_concentration, "µM")
                    ),
                  concentration_colors = concentration_colors,
                  single_conc = local_concentration
                )
              })

              ##### Binding plot ----
              output[[paste0(
                local_ui_id,
                "_binding_plot"
              )]] <- plotly::renderPlotly({
                make_binding_plot(
                  kobs_result = conversion_sidebar_vars$result_list()$binding_kobs_result,
                  filter_conc = local_concentration
                )
              })

              ##### Multiple spectra plot ----
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
                        conversion_sidebar_vars$result_list()$deconvolution
                      ),
                      "_"
                    ),
                    `[`,
                    3
                  )
                )

                multiple_spectra(
                  results_list = conversion_sidebar_vars$result_list(),
                  samples = names(
                    conversion_sidebar_vars$result_list()$deconvolution
                  )[which(
                    decon_samples == local_concentration
                  )],
                  cubic = ifelse(
                    input[[paste0(local_ui_id, "_kind")]] == "3D",
                    TRUE,
                    FALSE
                  ),
                  time = TRUE
                )
              })
            })
          }

          # Select binding results tab
          set_selected_tab("Binding", session)
        } else if (isFALSE(conversion_sidebar_vars$run_ki_kinact())) {
          ### Protein conversion interface ----

          #### Transform hits for summary table ----
          hits_summary <- transform_hits(
            hits_summary = conversion_sidebar_vars$result_list()$"hits_summary",
            run_ki_kinact = FALSE
          )

          #### Append truncated sample IDs ----
          # Create a mapping data frame
          mapping <- data.frame(
            original = unique(hits_summary$`Sample ID`),
            truncated = label_smart_clean(unique(hits_summary$`Sample ID`))
          )

          # Add column with truncated IDs
          hits_summary$truncSample_ID <- mapping$truncated[match(
            hits_summary$`Sample ID`,
            mapping$original
          )]

          #### Hits table tab ----
          bslib::nav_insert(
            "tabs",
            bslib::nav_panel(
              title = "Hits",
              shiny::div(
                class = "conversion-result-wrapper",
                # shiny::div(
                shiny::fluidRow(
                  shiny::column(
                    width = 2,
                    align = "center",
                    shiny::div(
                      class = "hits-tab-expand",
                      shiny::checkboxInput(
                        ns("hits_tab_expand"),
                        label = "Expand Samples",
                        value = TRUE
                      )
                    )
                  ),
                  shiny::column(
                    width = 2,
                    align = "center",
                    shinyWidgets::pickerInput(
                      ns("hits_tab_sample_select"),
                      label = "Select Samples",
                      choices = unique(hits_summary$`Sample ID`),
                      selected = unique(hits_summary$`Sample ID`),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE
                      )
                    )
                  ),
                  shiny::column(
                    width = 2,
                    align = "center",
                    shinyWidgets::pickerInput(
                      ns("hits_tab_compound_select"),
                      label = "Select Compounds",
                      choices = unique(hits_summary$`Cmp Name`),
                      selected = unique(hits_summary$`Cmp Name`),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE
                      )
                    )
                  ),
                  shiny::column(
                    width = 2,
                    align = "center",
                    shiny::div(
                      class = "hits-tab-col-select-ui",
                      shinyWidgets::pickerInput(
                        ns("hits_tab_col_select"),
                        label = "Select Columns",
                        choices = names(hits_summary)[
                          !names(hits_summary) %in%
                            c("Sample ID", "Cmp Name", "truncSample_ID")
                        ],
                        selected = names(hits_summary)[
                          !names(hits_summary) %in%
                            c("Sample ID", "Cmp Name", "truncSample_ID")
                        ][-c(1:2, 4:5, 7, 9)],
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE
                        )
                      )
                    )
                  ),
                  shiny::column(
                    width = 2,
                    align = "center",
                    shinyWidgets::pickerInput(
                      ns("binding_chart"),
                      label = "Show Binding Bars",
                      choices = c("%-Binding", "Total %-Binding"),
                      selected = "Total %-Binding",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE
                      )
                    )
                  )
                ),
                # ),
                shinycssloaders::withSpinner(
                  DT::DTOutput(ns("conversion_hits_tab")),
                  type = 1,
                  color = "#7777f9"
                )
              )
            )
          )

          ##### Render hits table ----
          output$conversion_hits_tab <- DT::renderDT(
            {
              shiny::req(input$hits_tab_col_select)

              hits_datatable <- render_hits_table(
                hits_table = hits_summary,
                concentration_colors = NULL,
                withzero = FALSE,
                selected_cols = input$hits_tab_col_select,
                bar_chart = input$binding_chart,
                compounds = input$hits_tab_compound_select,
                samples = input$hits_tab_sample_select,
                select = TRUE,
                color_scale = input$color_scale,
                expand = input$hits_tab_expand
              )

              hits_datatable_current(hits_datatable)

              return(hits_datatable)
            },
            server = FALSE
          )

          ##### Hits table clicking observer ----
          shiny::observe({
            shiny::req(
              input$conversion_hits_tab_cell_clicked,
              hits_datatable_current()
            )

            # Get client side click information
            cell_clicked <- input$conversion_hits_tab_cell_clicked

            if (
              !is.null(cell_clicked) &&
                length(cell_clicked)
            ) {
              # Get current column indeces of sample and compound columns
              cols <- names(hits_datatable_current()$x$data)
              sample_col <- which(cols == "Sample ID") - 1
              cmp_col <- which(cols == "Cmp Name") - 1

              # Actions if click corresponds to sample or compound
              if (length(sample_col) && cell_clicked$col == sample_col) {
                shinyWidgets::updatePickerInput(
                  session,
                  "conversion_sample_picker",
                  selected = cell_clicked$value
                )

                set_selected_tab("Samples View", session)
              } else if (length(cmp_col) && cell_clicked$col == cmp_col) {
                shinyWidgets::updatePickerInput(
                  session,
                  "conversion_compound_picker",
                  selected = cell_clicked$value
                )

                set_selected_tab("Compounds View", session)
              }
            }
          })

          #### Sample view tab ----
          bslib::nav_insert(
            "tabs",
            bslib::nav_panel(
              title = "Samples View",
              shiny::div(
                class = "conversion-result-wrapper",
                shiny::div(
                  class = "conversion-samples-wrapper",
                  shiny::div(
                    class = "conversion-samples-control",
                    shinyWidgets::pickerInput(
                      ns("conversion_sample_picker"),
                      "Select Sample",
                      choices = unique(hits_summary$`Sample ID`)
                    ),
                    shiny::div(
                      class = "conversion-samples-stats",
                      shiny::div(
                        class = "card-custom",
                        bslib::card(
                          bslib::card_header(
                            class = "bg-dark help-header",
                            "Protein",
                            shiny::div(
                              class = "tooltip-bttn",
                              shiny::actionButton(
                                ns("conversion_samples_protein_tooltip_bttn"),
                                label = "",
                                icon = shiny::icon("circle-question")
                              )
                            )
                          ),
                          shiny::div(
                            class = "kobs-val",
                            shinycssloaders::withSpinner(
                              shiny::uiOutput(ns("conversion_cmp_protein")),
                              type = 1,
                              color = "#7777f9"
                            )
                          )
                        )
                      ),
                      shiny::div(
                        class = "card-custom",
                        bslib::card(
                          bslib::card_header(
                            class = "bg-dark help-header",
                            "Total %-Binding",
                            shiny::div(
                              class = "tooltip-bttn",
                              shiny::actionButton(
                                ns("total_pct_bind_tooltip_bttn"),
                                label = "",
                                icon = shiny::icon("circle-question")
                              )
                            )
                          ),
                          shiny::div(
                            class = "kobs-val",
                            shinycssloaders::withSpinner(
                              shiny::uiOutput(ns("total_pct_binding")),
                              type = 1,
                              color = "#7777f9"
                            )
                          )
                        )
                      )
                    )
                  ),
                  shiny::div(
                    class = "card-custom cmp-table",
                    bslib::card(
                      bslib::card_header(
                        class = "bg-dark help-header",
                        "Present Compounds"
                      ),
                      shinycssloaders::withSpinner(
                        DT::DTOutput(
                          ns("conversion_present_compounds_table")
                        ),
                        type = 1,
                        color = "#7777f9"
                      ),
                      full_screen = TRUE
                    )
                  ),
                  shiny::div(
                    class = "card-custom",
                    bslib::card(
                      bslib::card_header(
                        class = "bg-dark help-header",
                        "Compound Distribution",
                        shiny::div(
                          class = "tooltip-bttn",
                          shiny::actionButton(
                            ns("mass_spectra_tooltip_bttn"),
                            label = "",
                            icon = shiny::icon("circle-question")
                          )
                        )
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput(
                          ns("conversion_present_compounds_pie"),
                          height = "100%"
                        ),
                        type = 1,
                        color = "#7777f9"
                      ),
                      full_screen = TRUE
                    )
                  ),
                  shiny::div(
                    class = "card-custom",
                    bslib::card(
                      bslib::card_header(
                        class = "bg-dark help-header",
                        "Annotated Spectrum"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput(
                          ns("conversion_sample_spectrum"),
                          height = "100%"
                        ),
                        type = 1,
                        color = "#7777f9"
                      ),
                      full_screen = TRUE
                    )
                  )
                )
              )
            )
          )

          ##### Total %-binding for one sample across compounds ----
          output$total_pct_binding <- shiny::renderUI({
            shiny::req(input$conversion_sample_picker)

            hits_summary$`Total %-Binding`[
              hits_summary$`Sample ID` == input$conversion_sample_picker
            ][1]
          })

          ##### Compound distribution donut chart ----
          output$conversion_present_compounds_pie <- plotly::renderPlotly({
            shiny::req(input$conversion_sample_picker)

            tbl <- hits_summary |>
              dplyr::filter(
                `Sample ID` == input$conversion_sample_picker
              )

            cmp_table <- tbl |>
              dplyr::group_by(`Cmp Name`) |>
              dplyr::arrange(dplyr::desc(`Theor. Cmp`), `Bind. Stoich.`) |>
              dplyr::reframe(
                total_bind = `Total %-Binding`,
                mass_shift = `Theor. Cmp`,
                mass_stoich = paste0(
                  "[",
                  `Theor. Cmp`,
                  "]",
                  sapply(`Bind. Stoich.`, function(x) {
                    as.character(htmltools::tags$sub(x))
                  })
                ),
                relBinding = as.numeric(gsub("%", "", `%-Binding`)) / 100
              ) |>
              rbind(
                data.frame(
                  tbl$`Cmp Name`[1],
                  tbl$`Total %-Binding`[1],
                  "empty",
                  "Unbound",
                  1 -
                    as.numeric(gsub(
                      "%",
                      "",
                      tbl$`Total %-Binding`[1]
                    )) /
                      100
                ) |>
                  stats::setNames(c(
                    "Cmp Name",
                    "total_bind",
                    "mass_shift",
                    "mass_stoich",
                    "relBinding"
                  ))
              )

            # Prepare compound marker colors
            colors <- c(
              "#e5e5e5",
              get_cmp_colorScale(
                filtered_table = tbl,
                scale = input$color_scale
              )
            )
            names(colors) <- c(
              "empty",
              names(colors)[-1]
            )

            cmp_table$mw_color <- colors[match(
              cmp_table$mass_shift,
              names(colors)
            )]

            plotly::plot_ly(
              data = cmp_table,
              labels = ~mass_stoich,
              values = ~relBinding,
              sort = FALSE,
              type = 'pie',
              hole = 0.4,
              textinfo = 'label+percent',
              texttemplate = "%{label}<br>%{percent}",
              hoverinfo = 'skip',
              textposition = 'auto',
              outsidetextfont = list(color = 'white'),
              marker = list(
                colors = ~ I(mw_color),
                line = list(color = '#e5e5e5', width = 1)
              )
            ) |>
              plotly::layout(
                showlegend = FALSE,
                annotations = list(
                  list(
                    x = 0.5,
                    y = 0.5,
                    text = paste0(
                      "<b>",
                      cmp_table$total_bind[1],
                      "</b><br>Bound"
                    ),
                    xref = "paper",
                    yref = "paper",
                    xanchor = "center",
                    yanchor = "middle",
                    showarrow = FALSE,
                    font = list(size = 22, color = "white")
                  )
                )
              )
          })

          ##### Sample spectrum plot ----
          output$conversion_sample_spectrum <- plotly::renderPlotly({
            selected_sample <- input$conversion_sample_picker
            shiny::req(selected_sample)

            # Filter table for selected sample
            tbl <- hits_summary |>
              dplyr::filter(
                `Sample ID` == selected_sample
              )

            # Prepare compound coloring
            colors <- get_cmp_colorScale(
              filtered_table = tbl,
              scale = input$color_scale
            )

            spectrum_plot(
              sample = conversion_sidebar_vars$result_list()$deconvolution[[
                selected_sample
              ]],
              color_cmp = colors
            )
          })

          ##### Samples view table ----
          output$conversion_present_compounds_table <- DT::renderDataTable(
            {
              shiny::req(input$conversion_sample_picker)

              tbl <- hits_summary |>
                dplyr::filter(
                  `Sample ID` == input$conversion_sample_picker
                )

              # Prepare compound coloring
              colors <- get_cmp_colorScale(
                filtered_table = tbl,
                scale = input$color_scale
              )

              cmp_table <- tbl |>
                dplyr::group_by(`Cmp Name`) |>
                dplyr::arrange(dplyr::desc(`Theor. Cmp`), `Bind. Stoich.`) |>
                dplyr::reframe(
                  `Mass Shift` = `Theor. Cmp`,
                  Stoichiometry = `Bind. Stoich.`,
                  `%-Binding` = as.numeric(gsub(
                    "%",
                    "",
                    `%-Binding`
                  ))
                ) |>
                DT::datatable(
                  extensions = "RowGroup",
                  rownames = FALSE,
                  selection = "none",
                  class = "order-column",
                  options = list(
                    dom = 't',
                    scrollY = "175px",
                    scrollCollapse = TRUE,
                    rowGroup = list(dataSrc = 0),
                    columnDefs = list(
                      list(visible = FALSE, targets = 0),
                      list(className = 'dt-center', targets = "_all"),
                      list(
                        targets = 3,
                        render = htmlwidgets::JS(chart_js)
                      ),
                      list(
                        targets = -1,
                        className = 'dt-last-col'
                      )
                    )
                  )
                ) |>
                DT::formatStyle(
                  columns = "Mass Shift",
                  target = 'row',
                  backgroundColor = DT::styleEqual(
                    levels = names(colors),
                    values = colors
                  ),
                  color = DT::styleEqual(
                    levels = names(colors),
                    values = get_contrast_color(colors)
                  )
                )

              return(cmp_table)
            }
          )

          #### Compound View tab ----
          bslib::nav_insert(
            "tabs",
            bslib::nav_panel(
              title = "Compounds View",
              shiny::div(
                class = "conversion-result-wrapper",
                shiny::div(
                  class = "conversion-samples-wrapper",
                  shiny::div(
                    class = "conversion-samples-control",
                    shinyWidgets::pickerInput(
                      ns("conversion_compound_picker"),
                      "Select Compound",
                      choices = unique(hits_summary$`Cmp Name`)
                    ),
                    shiny::div(
                      class = "conversion-samples-stats",
                      shiny::div(
                        class = "card-custom",
                        bslib::card(
                          bslib::card_header(
                            class = "bg-dark help-header",
                            "Protein",
                            shiny::div(
                              class = "tooltip-bttn",
                              shiny::actionButton(
                                ns("conversion_samples_protein_tooltip_bttn"),
                                label = "",
                                icon = shiny::icon("circle-question")
                              )
                            )
                          ),
                          shiny::div(
                            class = "kobs-val",
                            shinycssloaders::withSpinner(
                              shiny::uiOutput(ns("conversion_sample_protein")),
                              type = 1,
                              color = "#7777f9"
                            )
                          )
                        )
                      ),
                      shiny::div(
                        class = "card-custom",
                        bslib::card(
                          bslib::card_header(
                            class = "bg-dark help-header",
                            "Total %-Binding",
                            shiny::div(
                              class = "tooltip-bttn",
                              shiny::actionButton(
                                ns("total_pct_bind_tooltip_bttn"),
                                label = "",
                                icon = shiny::icon("circle-question")
                              )
                            )
                          ),
                          shiny::div(
                            class = "kobs-val",
                            shinycssloaders::withSpinner(
                              shiny::uiOutput(ns("total_pct_cmps_binding")),
                              type = 1,
                              color = "#7777f9"
                            )
                          )
                        )
                      )
                    )
                  ),
                  shiny::div(
                    class = "card-custom cmp-table",
                    bslib::card(
                      bslib::card_header(
                        class = "bg-dark help-header",
                        "Present Compounds"
                      ),
                      # shiny::div(
                      shinycssloaders::withSpinner(
                        DT::DTOutput(
                          ns("conversion_cmp_table")
                        ),
                        type = 1,
                        color = "#7777f9"
                      ),
                      # ),
                      full_screen = TRUE
                    )
                  ),
                  shiny::div(
                    class = "card-custom",
                    bslib::card(
                      bslib::card_header(
                        class = "bg-dark help-header",
                        "Compound Distribution",
                        shiny::div(
                          class = "tooltip-bttn",
                          shiny::actionButton(
                            ns("mass_spectra_tooltip_bttn"),
                            label = "",
                            icon = shiny::icon("circle-question")
                          )
                        )
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput(
                          ns("cmp_distribution"),
                          height = "100%"
                        ),
                        type = 1,
                        color = "#7777f9"
                      ),
                      full_screen = TRUE
                    )
                  ),
                  shiny::div(
                    class = "card-custom",
                    bslib::card(
                      bslib::card_header(
                        class = "bg-dark help-header",
                        "Annotated Spectrum"
                      ),
                      shinycssloaders::withSpinner(
                        plotly::plotlyOutput(
                          ns("conversion_cmp_spectra"),
                          height = "100%"
                        ),
                        type = 1,
                        color = "#7777f9"
                      ),
                      full_screen = TRUE
                    )
                  )
                )
              )
            )
          )

          ##### Total %-binding for one compound across samples ----
          output$total_pct_cmps_binding <- shiny::renderUI({
            shiny::req(input$conversion_sample_picker)

            total_bind <- hits_summary$`Total %-Binding`[
              hits_summary$`Cmp Name` == input$conversion_compound_picker
            ]

            if (length(total_bind) == 1) {
              msg <- total_bind
            } else {
              total_bind_num <- as.numeric(gsub("%", "", total_bind))
              msg <- shiny::div(
                class = "conversion-sample-protein-box",
                shiny::div(
                  class = "conversion-sample-protein-names",
                  shiny::HTML("Range<br>Mean ± SD")
                ),
                shiny::div(
                  class = "conversion-sample-protein",
                  shiny::HTML(
                    paste0(
                      min(total_bind_num),
                      "% - ",
                      max(total_bind_num),
                      "%<br>",
                      mean(total_bind_num),
                      "% ± ",
                      round(stats::sd(total_bind_num), 2)
                    )
                  )
                )
              )
            }

            return(msg)
          })

          ##### Compound distribution bar chart ----
          output$cmp_distribution <- plotly::renderPlotly({
            shiny::req(input$conversion_compound_picker)

            # Filter hits for selected compound
            tbl <- hits_summary |>
              dplyr::filter(`Cmp Name` == input$conversion_compound_picker) |>
              dplyr::mutate(
                `Sample ID` = if (input$truncate_names) {
                  `truncSample_ID`
                } else {
                  `Sample ID`
                },
                mass_stoich = paste0(
                  "[",
                  `Theor. Cmp`,
                  "]",
                  sapply(`Bind. Stoich.`, function(x) {
                    as.character(htmltools::tags$sub(x))
                  })
                )
              )

            # Make compound color scale
            colors <- get_cmp_colorScale(
              filtered_table = tbl,
              scale = input$color_scale
            )

            # Assign font colors to match background brightness
            tbl <- tbl |>
              dplyr::mutate(
                bg_hex = colors[as.character(`Theor. Cmp`)],
                label_color = get_contrast_color(bg_hex),
                mass_stoich = paste0(
                  "<span style='color:",
                  label_color,
                  "'>",
                  mass_stoich,
                  "</span>"
                )
              )

            # Pre-calculate totals for the top labels
            totals <- dplyr::group_by(tbl, `Sample ID`) |>
              dplyr::summarize(
                total_val = sum(as.numeric(gsub("%", "", `%-Binding`)))
              )

            # Create plotly bar chart
            plotly::plot_ly(data = tbl) |>
              plotly::add_trace(
                x = ~`Sample ID`,
                y = ~ as.numeric(gsub("%", "", `%-Binding`)),
                color = ~`Theor. Cmp`,
                colors = colors,
                type = 'bar',
                name = ~mass_stoich,
                hovertemplate = ~ paste0(
                  "<span style='opacity: 0.8'>Mass Shift:</span> <b>",
                  `Theor. Cmp`,
                  "</b><br>",
                  "<span style='opacity: 0.8'>Stoichiometry:</span> <b>",
                  `Bind. Stoich.`,
                  "</b><br>",
                  "<span style='opacity: 0.8'>%-Binding:</span> <b>",
                  `%-Binding`,
                  "</b>",
                  "<extra><div style='text-align: left;'>",
                  "<span style='opacity: 0.8;;'>Cmp Name: </span><b>",
                  `Cmp Name`,
                  "</b><br>",
                  "<span style='opacity: 0.8;'>Sample ID: </span><b>",
                  `Sample ID`,
                  "</b>",
                  "</div></extra>"
                ),
                hoverlabel = list(align = "left", valign = "middle"),
                text = ~mass_stoich,
                textposition = 'inside',
                marker = list(line = list(color = 'white', width = 1)),
                showlegend = FALSE
              ) |>
              plotly::add_trace(
                data = totals,
                x = ~`Sample ID`,
                y = ~total_val,
                type = 'scatter',
                mode = 'text',
                text = ~ paste0("Total ", total_val, "%"),
                textposition = 'top center',
                showlegend = FALSE,
                hoverinfo = 'none',
                inherit = FALSE,
                textfont = list(color = '#ffffff', size = 16)
              ) |>
              plotly::layout(
                barmode = 'stack',
                bargap = 0.5,
                paper_bgcolor = 'rgba(0,0,0,0)',
                plot_bgcolor = 'rgba(0,0,0,0)',
                xaxis = list(
                  title = list(text = NULL),
                  showgrid = FALSE,
                  zeroline = FALSE,
                  color = '#ffffff'
                ),
                yaxis = list(
                  title = list(text = "%-Binding"),
                  zeroline = FALSE,
                  gridcolor = "#7f7f7fff",
                  color = '#ffffff'
                )
              )
          })

          ##### Compound spectra plots ----
          output$conversion_cmp_spectra <- plotly::renderPlotly({
            shiny::req(input$conversion_compound_picker)

            # Filter hits for selected compound
            tbl <- dplyr::filter(
              hits_summary,
              `Cmp Name` == input$conversion_compound_picker
            )

            # Make compound color scale
            colors <- get_cmp_colorScale(
              filtered_table = tbl,
              scale = input$color_scale
            )

            # Create spectra plot
            multiple_spectra(
              results_list = conversion_sidebar_vars$result_list(),
              samples = unique(hits_summary$`Sample ID`[
                hits_summary$`Cmp Name` == input$conversion_compound_picker
              ]),
              cubic = ifelse(
                TRUE,
                TRUE,
                FALSE
              ),
              color_cmp = colors,
              truncated = if (input$truncate_names) mapping else FALSE
            )
          })

          ##### Compounds view table ----
          output$conversion_cmp_table <- DT::renderDataTable({
            shiny::req(input$conversion_compound_picker)

            # Filter table for selected compound
            tbl <- hits_summary |>
              dplyr::filter(
                `Cmp Name` == input$conversion_compound_picker
              )

            # Prepare compound coloring
            colors <- get_cmp_colorScale(
              filtered_table = tbl,
              scale = input$color_scale
            )

            # Create data table
            cmp_table <- tbl |>
              dplyr::group_by(`Sample ID`) |>
              dplyr::arrange(dplyr::desc(`Theor. Cmp`), `Bind. Stoich.`) |>
              dplyr::reframe(
                `Mass Shift` = `Theor. Cmp`,
                `Stoichiometry` = `Bind. Stoich.`,
                `Sample ID` = `Sample ID`,
                `%-Binding` = as.numeric(gsub(
                  "%",
                  "",
                  `%-Binding`
                )),
                `Total %-Binding` = `Total %-Binding`
              ) |>
              dplyr::relocate(`Mass Shift`, .before = 1) |>
              DT::datatable(
                extensions = "RowGroup",
                rownames = FALSE,
                class = "order-column",
                selection = "none",
                options = list(
                  dom = 't',
                  scrollY = "175px",
                  scrollCollapse = TRUE,
                  rowGroup = list(dataSrc = 1),
                  columnDefs = list(
                    list(visible = FALSE, targets = 1),
                    list(className = 'dt-center', targets = "_all"),
                    list(
                      targets = 3,
                      render = htmlwidgets::JS(chart_js)
                    ),
                    list(
                      targets = -1,
                      className = 'dt-last-col'
                    )
                  )
                )
              ) |>
              DT::formatStyle(
                columns = "Mass Shift",
                target = 'row',
                backgroundColor = DT::styleEqual(
                  levels = names(colors),
                  values = colors
                ),
                color = DT::styleEqual(
                  levels = names(colors),
                  values = get_contrast_color(colors)
                )
              )

            return(cmp_table)
          })

          ### Insert tabset input panel ----
          bslib::nav_insert(
            id = "tabs",
            bslib::nav_item(
              class = "conversion-tab-item-wrapper",
              shiny::div(
                class = "conversion-tab-items",
                shiny::div(
                  class = "conversion-tab-items-truncate",
                  shiny::div(
                    class = "conversion-tab-items-label",
                    shiny::HTML("Shorten Samples")
                  ),
                  shinyWidgets::materialSwitch(
                    ns("truncate_names"),
                    label = NULL,
                    value = TRUE
                  )
                ),
                shiny::selectInput(
                  ns("color_scale"),
                  label = NULL,
                  choices = list(
                    Qualitative = qualitative_scales,
                    Sequential = sequential_scales,
                    Gradient = gradient_scales
                  ),
                  selected = "viridis"
                ),
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("conversion_tooltip_bttn"),
                    label = "",
                    icon = shiny::icon("circle-question")
                  )
                )
              )
            ),
            target = NULL,
            position = "after",
            select = FALSE
          )

          ### Selected protein info ----
          output$conversion_cmp_protein <- output$conversion_sample_protein <- shiny::renderUI(
            {
              shiny::div(
                class = "conversion-sample-protein-box",
                shiny::div(
                  class = "conversion-sample-protein-names",
                  shiny::HTML("Name<br>Mw")
                ),
                shiny::div(
                  class = "conversion-sample-protein",
                  shiny::HTML(paste(
                    "RACA<br>",
                    format(27234, big.mark = ",", scientific = FALSE),
                    "Da"
                  ))
                )
              )
            }
          )

          # Switch to Hits tab
          set_selected_tab("Hits", session)
        }
      } else {
        ### Result reset ----
        # If results were reset load conversion declaration interface

        #### Reset ui elements ----
        output$binding_tab <- NULL
        output$kinact <- NULL
        output$Ki <- NULL
        output$Ki_kinact <- NULL
        output$kobs_result <- NULL
        output$binding_plot <- NULL
        output$kobs_plot <- NULL
        output$hits_tab <- NULL
        output$conversion_hits_tab <- NULL
        output$total_pct_binding <- NULL
        output$conversion_present_compounds_pie <- NULL
        output$conversion_sample_spectrum <- NULL
        output$conversion_present_compounds_table <- NULL
        output$total_pct_cmps_binding <- NULL
        output$cmp_distribution <- NULL
        output$conversion_cmp_spectra <- NULL
        output$conversion_cmp_table <- NULL
        output$conversion_cmp_protein <- NULL
        output$conversion_sample_protein <- NULL

        #### Show declaration tabs ----
        bslib::nav_show(
          "tabs",
          "Proteins"
        )
        bslib::nav_show(
          "tabs",
          "Compounds"
        )
        bslib::nav_show(
          "tabs",
          "Samples"
        )

        #### Remove results tabs ----
        bslib::nav_remove("tabs", "Binding")
        bslib::nav_remove("tabs", "Hits")
        for (i in names(conversion_vars$select_concentration)) {
          bslib::nav_remove(
            "tabs",
            paste0("[", i, "]")
          )
        }
        bslib::nav_remove("tabs", "Compounds View")
        bslib::nav_remove("tabs", "Samples View")

        #### Reset reactive variables ----
        conversion_vars <- shiny::reactiveValues(
          modified_results = NULL,
          select_concentration = NULL,
          formatted_hits = NULL,
          conc_colors = NULL,
          expand_helper = FALSE
        )

        # Select samples tab
        set_selected_tab("Samples", session)
      }
    })

    ## Events for conversion result interface ----
    ### Expand samples from hits table ----
    shiny::observeEvent(input$hits_tab_expand, {
      if (isFALSE(conversion_vars$expand_helper)) {
        shinyjs::removeClass(
          selector = ".hits-tab-col-select-ui .form-group",
          class = "custom-disable"
        )

        shinyWidgets::updatePickerInput(
          session,
          "binding_chart",
          choices = c("%-Binding", "Total %-Binding"),
          selected = "Total %-Binding",
        )

        conversion_vars$expand_helper <- TRUE
      } else {
        shinyjs::addClass(
          selector = ".hits-tab-col-select-ui .form-group",
          class = "custom-disable"
        )

        shinyWidgets::updatePickerInput(
          session,
          "binding_chart",
          choices = "Total %-Binding",
          selected = "Total %-Binding"
        )

        conversion_vars$expand_helper <- FALSE
      }
    })

    ### Recalculate results depending on excluded concentrations ----
    shiny::observeEvent(input[["kobs_result_cell_edit"]], {
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

      # Add binding/kobs results to result list
      result_list$binding_kobs_result <- add_kobs_binding_result(
        result_list,
        concentrations_select = names(
          conversion_vars$select_concentration
        )[which(conversion_vars$select_concentration)]
      )

      # Add Ki/kinact results to result list
      result_list$ki_kinact_result <- add_ki_kinact_result(
        result_list
      )

      # Assign modified results to reactive variable
      conversion_vars$modified_results <- result_list
    })

    # Tooltips ----
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
                      shiny::strong("Theor. Prot."),
                      " – theoretical mass of the unmodified protein"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Meas. Prot."),
                      " – measured deconvolved mass of the protein species"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Δ Prot."),
                      " – difference between theoretical and measured deconvolved protein mass"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Ⅰ Prot."),
                      " – relative intensity of the unmodified protein peak [%]"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Peak Signal"),
                      " – raw signal intensity for present peak"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Ⅰ Cmp"),
                      " – intensity of the peak representing the protein together with a compound adduct [%]"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Cmp Name"),
                      " – compound name or ID of the bound compound"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Theor. Cmp"),
                      " – theoretical mass of the bound compound"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Δ Cmp"),
                      " – difference between theoretical complex and the obtained deconvolved mass [Da]"
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

    # Return server values ----
    list(
      selected_tab = shiny::reactive(input$tabs),
      conversion_ready = shiny::reactive(declaration_vars$conversion_ready),
      input_list = shiny::reactive(list(
        Protein_Table = protein_table_data(),
        Compound_Table = compound_table_data(),
        Samples_Table = declaration_vars$sample_table,
        ConcTime_Table = conc_time_table_data(),
        result = declaration_vars$result
      )),
      samples_confirmed = shiny::reactive(declaration_vars$samples_confirmed),
      cancel_continuation = shiny::reactive(input$conversion_cont_cancel)
    )
  })
}
