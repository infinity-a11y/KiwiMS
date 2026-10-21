# app/view/conversion_card.R

box::use(
  shiny[moduleServer, NS],
  bslib[nav_insert],
)

box::use(
  app /
    logic /
    conversion_ui[
      keybind_menu_ui,
      table_legend,
      sample_table_legend,
      conversion_declaration_ui,
      binding_results_ui,
      kinact_ki_results_ui,
      summary_results_ui,
      hits_results_ui,
      hits_col_picker,
      overview_compound_picker_options,
    ],
  app / logic / ms_formats[ms_sample_base],
  app /
    logic /
    conversion_functions[
      add_kobs_binding_result,
      add_kinact_ki_result,
      sample_handsontable,
      prot_comp_handsontable,
      set_selected_tab,
      format_scientific,
      make_binding_plot,
      make_binding_surface_plot,
      kobs_model_at,
      multiple_spectra,
      spectrum_sample_ids,
      binding_ordered_hits,
      peaks_trace_indices,
      restyle_peak_symbols,
      relayout_spectrum_legend,
      relayout_spectrum_labels,
      render_hits_table,
      render_hits_view,
      filter_hits_table,
      hits_bar_cols,
      hits_base_col,
      hits_col_choices,
      hits_view_table,
      sort_hits_table,
      transform_per_adduct,
      checkboxColumn,
      js_code_gen,
      new_sample_table,
      restore_conc_time,
      restore_inclusion,
      sample_inclusion,
      confirm_sample_table,
      conversion_input_result,
      order_sample_cols,
      compute_replicate_labels,
      confirm_ui_changes,
      edit_ui_changes,
      table_observe,
      clean_prot_comp_table,
      clean_sample_table,
      handle_file_upload,
      fill_sample_table,
      transform_hits,
      get_contrast_color,
      label_smart_clean,
      color_key,
      key_colors,
      key_marker_symbols,
      key_symbols,
      default_color_role,
      sample_view_colors,
      sample_level_colors,
      render_table_view,
      filter_table_view,
      table_view_col_choices,
      make_kobs_plot,
      make_kinetics_residual_plot,
      make_kinetics_plateau_plot,
      make_kinetics_series_plot,
      make_kinetics_saturation_plot,
      empty_prot_comp_tbl,
      read_decon_metadata,
      validate_decon_db,
      overview_choices,
      overview_selection,
      overview_status_note,
      overview_subset,
      overview_colors,
      overview_compound_distribution,
      overview_distribution_note,
      overview_has_distribution,
      sample_view_col_kind,
      smpl_compound_distribution,
      stats_histogram,
      stats_boxplot,
      stats_scatter,
      stats_violin,
      batch_plate_heatmap,
      make_unit_view,
      conc_display,
      convert_conc_keys,
      convert_hits_units,
      convert_kobs_result_units,
      convert_kinact_ki_params,
      convert_kinact_ki_units,
      convert_result_list_units,
      has_proteoforms,
      is_complex_row,
      proteoform_binding,
      proteoform_limit_note,
      proteoform_kinetics,
      select_complex_kinetics,
      proteoform_comparison_table,
      proteoform_kobs_plot,
      proteoform_paired_plot,
      proteoform_paired_has_limits,
      proteoform_colors,
      collapse_species,
      declared_masses,
      compound_mass_entries,
      protein_mass_entries,
      mass_shift_card,
      complex_key
    ],
  app /
    logic /
    helper_functions[
      config_units,
    ],
  app / logic / deconvolution_functions[spectrum_plot, unmatched_trace_tag, ],
  app /
    logic /
    plot_download[
      setup_plot_dl,
      setup_table_dl,
      prepare_hits_export,
      plot_dl_popover,
      card_settings_popover
    ],
  app / logic / logging[get_session_prefix, write_log],
  app / logic / palette[tint],
  app / logic / help_modal[bind_help],
  app / logic / help_pages[conversion_help],
  app /
    logic /
    conversion_constants[
      empty_protein_table,
      chart_js,
      hits_table_names,
      popover_autoclose,
    ],
)

# Fit warnings and log entries are plain text (they also go to the protocol
# log); set the kinetic parameters with their subscripts where shown as HTML
kinetic_symbols_html <- function(x) {
  x <- htmltools::htmlEscape(x)
  x <- gsub("\\bkinact\\b", "k<sub>inact</sub>", x, perl = TRUE)
  x <- gsub("\\bKI\\b", "K<sub>I</sub>", x, perl = TRUE)
  gsub("\\bk_obs\\b", "k<sub>obs</sub>", x, perl = TRUE)
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  shiny::div(
    class = "conversion-main-spinner",
    # Fits the Batch Control plates to their cards (batch_plate_heatmap())
    shiny::tags$script(src = "static/js/plate_heatmap.js"),
    shiny::tags$script(shiny::HTML(
      "
      (function() {
        var _kiwiClickObserver = null;

        function bindHeatmapClicks(inputId) {
          document.querySelectorAll(
            '.batch-heatmap-grid .js-plotly-plot:not([data-kiwiclick])'
          ).forEach(function(el) {
            el.setAttribute('data-kiwiclick', '1');
            el.on('plotly_click', function(d) {
              if (d && d.points && d.points.length > 0) {
                var pt = d.points[0];
                console.log('[KiwiMS] heatmap click x=' + pt.x + ' y=' + pt.y);
                Shiny.setInputValue(inputId, {x: pt.x, y: pt.y}, {priority: 'event'});
              }
            });
          });
        }

        Shiny.addCustomMessageHandler('kiwiMS_attachHeatmapClicks', function(msg) {
          var inputId = msg.inputId;

          // Bind any already-rendered elements immediately
          bindHeatmapClicks(inputId);

          // Replace the persistent observer so future renders (lazy outputs
          // that fire when the Batch Control tab becomes visible) are caught
          if (_kiwiClickObserver) _kiwiClickObserver.disconnect();
          _kiwiClickObserver = new MutationObserver(function() {
            bindHeatmapClicks(inputId);
          });
          _kiwiClickObserver.observe(document.body, {childList: true, subtree: true});
        });

        var _scatterClickObservers = {};

        function bindScatterClicks(inputId, plotElId) {
          var container = document.getElementById(plotElId);
          if (!container) return;
          var el = container.classList.contains('js-plotly-plot')
            ? container
            : container.querySelector('.js-plotly-plot');
          if (!el) return;

          // Remove previous handler before re-binding so Plotly re-renders
          // (which call newPlot and clear the event system) don't leave us
          // without a listener. Unlike heatmap tiles (recreated via renderUI),
          // this element persists in the DOM, so we cannot rely on an
          // already-bound attribute guard.
          if (el._kiwiScatterHandler) {
            try { el.removeListener('plotly_click', el._kiwiScatterHandler); } catch(e) {}
          }
          el._kiwiScatterHandler = function(d) {
            if (d && d.points && d.points.length > 0) {
              var pt = d.points[0];
              console.log('[KiwiMS] scatter click sample=' + pt.customdata);
              Shiny.setInputValue(inputId, {sample: pt.customdata}, {priority: 'event'});
            }
          };
          el.on('plotly_click', el._kiwiScatterHandler);
        }

        Shiny.addCustomMessageHandler('kiwiMS_attachScatterClicks', function(msg) {
          var inputId = msg.inputId;
          var plotElId = msg.plotElId;
          bindScatterClicks(inputId, plotElId);
          // Each plot gets its own observer so sending messages for multiple
          // plots does not disconnect the observers of the earlier ones.
          if (_scatterClickObservers[plotElId]) {
            _scatterClickObservers[plotElId].disconnect();
          }
          var obs = new MutationObserver(function() {
            bindScatterClicks(inputId, plotElId);
          });
          obs.observe(document.body, {childList: true, subtree: true});
          _scatterClickObservers[plotElId] = obs;
        });
      })();
    "
    )),
    shinycssloaders::withSpinner(
      shiny::uiOutput(ns("conversion_ui"), class = "conversion-result-surface"),
      type = 1,
      color = "#7777f9"
    ),
    # Persistent host for the result interfaces. Each Results Menu entry gets
    # its own panel that is filled the first time the entry is selected and
    # then kept in the DOM, so switching entries only moves the active class
    # instead of re-rendering the plots and tables. The panels are emptied
    # again when the results are reset.
    shiny::div(
      id = ns("result_interface_stack"),
      class = "result-interface-stack",
      lapply(
        c("binding", "kinetics", "summary", "hits"),
        function(key) {
          shiny::div(
            class = "result-interface-panel",
            `data-iface` = key,
            shiny::uiOutput(
              ns(paste0("iface_", key)),
              class = "conversion-result-surface"
            )
          )
        }
      )
    )
  )
}

#' @export
server <- function(
  id,
  conversion_sidebar_vars,
  deconvolution_main_vars,
  config_file,
  config_apply_trigger = NULL,
  restore = NULL
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
    # Sample file upload whose table is still on its way to the client:
    # list(id, rendered), NULL when none
    sample_upload_pending <- shiny::reactiveVal(NULL)
    sample_upload_count <- shiny::reactiveVal(0L)
    render_trigger <- shiny::reactiveVal(0)
    show_completion_toast <- shiny::reactiveVal(NULL)
    trigger_kinact_ki <- shiny::reactiveVal(0L)
    manual_render_spectrum <- shiny::reactiveVal(0L)
    heatmap_pending_sample <- shiny::reactiveVal(NULL)
    stats_scatter_pending_sample <- shiny::reactiveVal(NULL)
    stats_boxplot_pending_sample <- shiny::reactiveVal(NULL)
    stats_violin_pending_sample <- shiny::reactiveVal(NULL)

    # Selection of the Overview tab of the Relative Binding interface: one
    # protein and the compounds picked for it. Held here rather than read from
    # the pickers, so a change of protein can swap the compounds in one step
    # instead of waiting for the compound picker to come back updated.
    overview_protein <- shiny::reactiveVal(NULL)
    overview_compounds <- shiny::reactiveVal(character(0))

    # Built annotated-spectrum figures, so returning to a selection that was
    # already drawn is instant. Per session on purpose: the cache key
    # describes the selection, not the dataset, so a shared cache could hand a
    # second session a figure built from someone else's results. For the same
    # reason it is emptied whenever the results are reset. Bounded, since a
    # built figure is several MB.
    spectrum_cache <- cachem::cache_mem(max_size = 256 * 1024^2)

    # Last unit selection of the declaration interface. Held separately from
    # the inputs because a reset re-renders the pickers, which would otherwise
    # fall back to their first choice instead of what was analysed.
    conc_unit_selected <- shiny::reactiveVal(NULL)
    time_unit_selected <- shiny::reactiveVal(NULL)

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

    max_stoichiometry <- shiny::reactive({
      max_mult <- conversion_sidebar_vars$max_multiples()
      if (is.null(max_mult)) 4 else max_mult
    }) |>
      shiny::debounce(750)

    # Validate the sample table, including the mass ambiguities of its protein
    # and compound combinations under the current peak tolerance and maximum
    # stoichiometry. Both are read isolated: the observers calling this react
    # to table edits, and the settings are re-checked by an observer of their
    # own below.
    observe_sample_table <- function(table) {
      table_observe(
        tab = "samples",
        table = table,
        output = output,
        ns = ns,
        proteins = declaration_vars$protein_table$Protein,
        compounds = declaration_vars$compound_table$Compound,
        protein_table = declaration_vars$protein_table,
        compound_table = declaration_vars$compound_table,
        tolerance = shiny::isolate(tolerance()),
        max_multiples = shiny::isolate(max_stoichiometry())
      )
    }

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

    ## Restored state ----
    # A page reloaded after a dropped connection takes back the declaration
    # its last session parked (park() below, session_restore.R). Set before
    # the first flush, so the declaration interface is drawn in that state
    # and the observers of the tables see it as their starting point.
    if (!is.null(restore)) {
      for (field in names(restore$declaration)) {
        declaration_vars[[field]] <- restore$declaration[[field]]
      }
      protein_table_data(restore$protein_table_data)
      compound_table_data(restore$compound_table_data)
      sample_table_data(restore$sample_table_data)
      conc_unit_selected(restore$conc_unit)
      time_unit_selected(restore$time_unit)
      saved_tabs <- c(
        proteins = isFALSE(restore$declaration$protein_table_active),
        compounds = isFALSE(restore$declaration$compound_table_active),
        samples = isTRUE(restore$declaration$samples_confirmed)
      )
      for (tab in names(saved_tabs)[saved_tabs]) {
        output[[paste0(tab, "_table_info")]] <- shiny::renderText(
          "Table saved!"
        )
      }
    }
    # The kinact/KI checkbox comes back with its restored value: not a change
    # of it (see kinact/KI Activation)
    kinact_ki_last <- restore$run_kinact_ki
    # Restored results are not announced as a finished analysis
    completion_toast_muted <- isTRUE(restore$has_results)

    # Helper: compute and inject Replicate column (right after Sample)
    add_replicate_col <- function(tbl, config = NULL) {
      tbl$Replicate <- compute_replicate_labels(tbl$Sample, config)
      order_sample_cols(tbl)
    }

    # Helper: auto-fill sample table columns from config file
    apply_config_autofill <- function(tbl, cfg) {
      # Sample names may carry a file extension on one side only, as in the
      # replicate lookup (add_replicate_col())
      cfg_key <- ms_sample_base(cfg$Sample)
      tbl_key <- ms_sample_base(tbl$Sample)
      for (i in seq_len(nrow(tbl))) {
        match_idx <- which(cfg_key == tbl_key[i])
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
    # Remember the picked units so they survive the re-render on reset
    shiny::observeEvent(
      input$conc_unit,
      {
        shiny::req(input$conc_unit)
        conc_unit_selected(input$conc_unit)
      },
      ignoreInit = TRUE,
      label = "Concentration Unit Memory"
    )

    shiny::observeEvent(
      input$time_unit,
      {
        shiny::req(input$time_unit)
        time_unit_selected(input$time_unit)
      },
      ignoreInit = TRUE,
      label = "Time Unit Memory"
    )

    # Conditional adaption of concentration/time input UI
    shiny::observe(
      {
        if (isTRUE(conversion_sidebar_vars$run_kinact_ki())) {
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
      },
      label = "Conditional Adaption of Concentration/Time Input UI"
    )

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
      ignoreInit = TRUE,
      priority = 100,
      label = "Protein Table Input Status"
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
      ignoreInit = TRUE,
      priority = 100,
      label = "Compound Table Input Status"
    )

    shiny::observeEvent(
      input$samples_table,
      {
        shiny::req(input$samples_table)

        # Save current table input
        suppressWarnings({
          samples_table <- rhandsontable::hot_to_r(input$samples_table)
        })

        # Concentration/Time columns come back as character (we render them as
        # text to prevent handsontable's numeric type from rounding values);
        # convert back to numeric here so downstream code gets numeric columns.
        conc_time_cols <- grep(
          "^Concentration|^Time",
          names(samples_table),
          value = TRUE
        )
        for (col in conc_time_cols) {
          if (is.character(samples_table[[col]])) {
            samples_table[[col]] <- suppressWarnings(
              as.numeric(samples_table[[col]])
            )
          }
        }

        # Assign to reactive variable
        sample_table_data(samples_table)
      },
      ignoreInit = TRUE,
      priority = 100,
      label = "Samples Table Input Status"
    )

    ## Conversion Declaration UI ----
    # A freshly rendered declaration interface shows its first tab and no tab
    # as saved. This brings it back to the state of the tables: saved tabs get
    # their checkmark, and a confirmed sample table keeps its file input and
    # unit pickers locked and is the tab shown, as after an aborted run.
    restore_declaration_state <- function(
      proteins_saved,
      compounds_saved,
      samples_confirmed
    ) {
      saved_tabs <- c("Proteins", "Compounds", "Samples")[c(
        proteins_saved,
        compounds_saved,
        samples_confirmed
      )]
      if (length(saved_tabs)) {
        shinyjs::delay(250, {
          for (saved_tab in saved_tabs) {
            shinyjs::runjs(sprintf(
              'document.querySelector(".nav-link[data-value=\'%s\']").classList.add("done");',
              saved_tab
            ))
          }
        })
      }

      if (!samples_confirmed) {
        return(invisible(NULL))
      }

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
          shinyjs::disable("conc_unit")
          shinyjs::addClass(
            selector = ".shiny-input-container:has(#app-conversion_main-conc_unit) .bootstrap-select",
            class = "custom-disable"
          )
          shinyjs::disable("time_unit")
          shinyjs::addClass(
            selector = ".shiny-input-container:has(#app-conversion_main-time_unit) .bootstrap-select",
            class = "custom-disable"
          )
        },
        once = TRUE
      )

      set_selected_tab("Samples", session)
      invisible(NULL)
    }

    # Rendered at start and again when a run ends without results (no hits):
    # the run's spinner replaces the interface, so it comes back rebuilt and
    # needs the state of the tables restored
    output$conversion_ui <- shiny::renderUI({
      shiny::req(!conversion_sidebar_vars$analysis_running())
      shiny::req(is.null(conversion_sidebar_vars$result_list()))
      shiny::isolate({
        proteins_saved <- isFALSE(declaration_vars$protein_table_active)
        compounds_saved <- isFALSE(declaration_vars$compound_table_active)
        samples_confirmed <- isTRUE(declaration_vars$samples_confirmed)

        ui <- conversion_declaration_ui(
          ns,
          proteins_status = if (proteins_saved) "confirmed" else "",
          compounds_status = if (compounds_saved) "confirmed" else "",
          samples_status = if (samples_confirmed) "confirmed" else "",
          conc_unit = conc_unit_selected(),
          time_unit = time_unit_selected()
        )
        restore_declaration_state(
          proteins_saved,
          compounds_saved,
          samples_confirmed
        )
        ui
      })
    })

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
      ) |>
        # Reports each finished render, also when the table data is unchanged
        # and rhandsontable's input update is dropped as a duplicate
        htmlwidgets::onRender(sprintf(
          "function(el, x) {
            Shiny.setInputValue('%s', Math.random(), {priority: 'event'});
          }",
          ns("samples_table_rendered")
        ))
    }) |>
      shiny::bindEvent(
        list(sample_table_trigger()),
        ignoreInit = FALSE
      )

    #### Samples left out of the conversion ----
    # Counted on the table as edited, so the count follows every click on an
    # Include box
    output$samples_table_exclusion <- shiny::renderUI({
      tbl <- sample_table_data()
      shiny::req(is.data.frame(tbl), nrow(tbl) > 0)
      n_out <- sum(!sample_inclusion(tbl))
      if (n_out == 0) {
        return(NULL)
      }
      shiny::div(
        class = "table-hint table-hint-exclusion",
        shiny::icon("eye-slash"),
        shiny::span(
          class = "table-hint-text",
          sprintf("%d of %d samples excluded", n_out, nrow(tbl))
        )
      )
    })

    #### Tab info text ----
    output$declaration_info_ui <- shiny::renderUI({
      # The Relative Binding tabs share the "tabs" id; checked first, so this
      # does not follow them while the results are shown
      shiny::req(is.null(conversion_sidebar_vars$result_list()))
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
      #   hints <- "Binding [%] inferred from time series measurements of a single concentration."
      # }

      # Help modal for the active tab, shown next to its hint
      help_input <- switch(
        input$tabs,
        Proteins = "proteins_tooltip_bttn",
        Compounds = "compounds_tooltip_bttn",
        "resultinput_tooltip_bttn"
      )

      shiny::div(
        class = "declaration-info",
        shiny::span(
          class = "declaration-hint",
          shiny::HTML(paste(
            '<i class="fa-solid fa-circle-info"></i> &nbsp;&nbsp;',
            hints
          ))
        ),
        bslib::tooltip(
          shiny::tags$button(
            type = "button",
            class = "btn declaration-help-btn",
            `aria-label` = "Help",
            onclick = sprintf(
              "Shiny.setInputValue('%s', Math.random(), {priority: 'event'});",
              ns(help_input)
            ),
            shiny::icon("question")
          ),
          "Help",
          placement = "bottom"
        )
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
      },
      ignoreInit = TRUE,
      label = "Table Pasting Feedback"
    )

    ## Clear tables ----
    shiny::observeEvent(
      input$clear_proteins,
      {
        write_log("Protein table cleared")
        protein_table_data(empty_prot_comp_tbl(type = "Protein"))

        protein_table_trigger(protein_table_trigger() + 1)
      },
      ignoreInit = TRUE,
      label = "Clear Protein Table"
    )

    shiny::observeEvent(
      input$clear_compounds,
      {
        write_log("Compound table cleared")
        compound_table_data(empty_prot_comp_tbl(type = "Compound"))

        compound_table_trigger(compound_table_trigger() + 1)
      },
      ignoreInit = TRUE,
      label = "Clear Compound Table"
    )

    shiny::observeEvent(
      input$clear_samples,
      {
        write_log("Sample table cleared")
        sample_table_data(add_replicate_col(
          new_sample_table(
            result = declaration_vars$result,
            protein_table = declaration_vars$protein_table,
            compound_table = declaration_vars$compound_table,
            kinact_ki = conversion_sidebar_vars$run_kinact_ki()
          ),
          config_file()
        ))
        sample_table_trigger(sample_table_trigger() + 1)
      },
      ignoreInit = TRUE,
      label = "Clear Sample Table"
    )

    ## File upload handler ----
    ### Protein table file upload ----
    shiny::observeEvent(
      input$proteins_fileinput,
      {
        protein_table_input <- handle_file_upload(
          file_input = input$proteins_fileinput,
          type = "Protein",
          output = output,
          declaration_vars = declaration_vars
        )

        if (!is.null(protein_table_input)) {
          write_log(paste(
            "Protein table loaded from file:",
            input$proteins_fileinput$name
          ))
          # Assign new table data to reactive variable and mark table status as TRUE to trigger table observer
          protein_table_data(protein_table_input)
          declaration_vars$protein_table_status <- TRUE

          # Set disable status to FALSE to allow confirm button activation
          # and table observer activation
          declaration_vars$protein_table_disabled <- FALSE

          # Trigger table render
          protein_table_trigger(protein_table_trigger() + 1)
        }
      },
      ignoreInit = TRUE,
      label = "Protein Table File Upload"
    )

    ### Compound table file upload ----
    shiny::observeEvent(
      input$compounds_fileinput,
      {
        write_log(paste(
          "Compound table loaded from file:",
          input$compounds_fileinput$name
        ))
        compound_table_data(handle_file_upload(
          file_input = input$compounds_fileinput,
          type = "compound",
          output = output,
          declaration_vars = declaration_vars
        ))
        declaration_vars$compound_table_disabled <- FALSE
        compound_table_trigger(compound_table_trigger() + 1)
      },
      ignoreInit = TRUE,
      label = "Compound Table File Upload"
    )

    ### Sample file upload ----
    shiny::observeEvent(
      input$samples_fileinput,
      {
        shiny::req(
          declaration_vars$protein_table,
          declaration_vars$compound_table
        )

        # Read metadata from selected result DB (fast — sample names only)
        file_path <- file.path(input$samples_fileinput$datapath)
        file_ext <- tolower(tools::file_ext(input$samples_fileinput$name))
        db_err <- if (!file_ext %in% c("db", "sqlite", "sqlite3")) {
          "Invalid file type: please upload a .db or .sqlite result file."
        } else {
          validate_decon_db(file_path)
        }
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
        # Moved out of the upload folder, which Shiny deletes when the session
        # ends: a page reloaded after a dropped connection keeps the results
        # (session_restore.R), and the conversion reads them from here
        kept_dir <- file.path(tempdir(), "kiwims_uploads")
        dir.create(kept_dir, showWarnings = FALSE)
        kept_path <- file.path(
          kept_dir,
          paste0(basename(tempfile("result_")), ".", file_ext)
        )
        if (file.rename(file_path, kept_path) ||
          file.copy(file_path, kept_path)) {
          file_path <- kept_path
        }
        meta <- read_decon_metadata(file_path)
        declaration_vars$result <- c(meta, list(.db_path = file_path))
        write_log(paste(
          "Deconvolution results loaded from file:",
          input$samples_fileinput$name,
          "—",
          length(meta$samples),
          "sample(s)"
        ))

        # Reset confirmation state — a new file always needs re-confirmation,
        # and ensures the status observer fires even when re-uploading the same file
        declaration_vars$samples_confirmed <- FALSE
        declaration_vars$sample_table_active <- TRUE

        # New table data
        sample_table_data(add_replicate_col(
          new_sample_table(
            result = declaration_vars$result,
            protein_table = declaration_vars$protein_table,
            compound_table = declaration_vars$compound_table,
            kinact_ki = conversion_sidebar_vars$run_kinact_ki()
          ),
          config_file()
        ))
        # Keep the UI blocked until the new table has been rendered and
        # validated once (see Sample Table Render Feedback), instead of
        # validating here and again when the rendered table comes back
        upload_id <- sample_upload_count() + 1L
        sample_upload_count(upload_id)
        sample_upload_pending(list(id = upload_id, rendered = FALSE))
        sample_table_trigger(sample_table_trigger() + 1)

        # Safety net in case the render is never reported
        shinyjs::delay(10000, finish_sample_upload(upload_id))
      },
      ignoreInit = TRUE,
      label = "Sample Table File Upload"
    )

    ### Sample file upload completion ----
    # Enable the sample table controls and validate the table
    activate_sample_table_ui <- function(table) {
      shinyjs::enable("samples_fileinput")
      shinyjs::removeClass(
        selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
        class = "custom-disable"
      )
      shinyjs::removeClass(
        selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
        class = "custom-disable"
      )

      # Enable clear button and unit selectors while table is active
      shinyjs::enable("clear_samples")
      shinyjs::enable("conc_unit")
      shinyjs::enable("time_unit")

      declaration_vars$sample_table_status <- observe_sample_table(table)
    }

    # Completes a pending upload the Sample Table Status Observer has not
    # completed, i.e. when the rendered table came back unchanged
    finish_sample_upload <- function(upload_id) {
      pending <- shiny::isolate(sample_upload_pending())
      if (!identical(pending$id, upload_id)) {
        return(invisible(NULL))
      }
      sample_upload_pending(NULL)

      activate_sample_table_ui(clean_sample_table(
        shiny::isolate(sample_table_data())
      ))
      shinyjs::runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "none";'
      ))
    }

    shiny::observeEvent(
      input$samples_table_rendered,
      {
        pending <- sample_upload_pending()
        shiny::req(!is.null(pending), !isTRUE(pending$rendered))
        sample_upload_pending(list(id = pending$id, rendered = TRUE))

        # A changed table reaches the server together with this event and
        # passes the 500 ms debounce of sample_table_input() before the delay
        # ends; its status observer run then completes the upload. An
        # unchanged table is not sent again and is completed here.
        shinyjs::delay(750, finish_sample_upload(pending$id))
      },
      ignoreInit = TRUE,
      label = "Sample Table Render Feedback"
    )

    ## Sample table re-check on changed screening settings ----
    # Whether masses are ambiguous depends on the peak tolerance and the
    # maximum stoichiometry, which are set in the sidebar and can change after
    # the table was filled
    shiny::observeEvent(
      list(tolerance(), max_stoichiometry()),
      {
        shiny::req(isTRUE(declaration_vars$sample_table_active))
        table <- tryCatch(
          clean_sample_table(sample_table_data()),
          error = function(e) NULL
        )
        shiny::req(is.data.frame(table), nrow(table) > 0)

        declaration_vars$sample_table_status <- observe_sample_table(table)
      },
      ignoreInit = TRUE
    )

    ## Config autofill availability ----
    # Reason the sample table cannot take a config autofill right now, or NULL
    # when it can. Shared by the use_config button and the config modal.
    config_apply_block <- shiny::reactive({
      if (isTRUE(declaration_vars$samples_confirmed)) {
        "Sample table is confirmed. Click Edit to apply the config."
      } else if (
        is.null(sample_table_data()) || nrow(sample_table_data()) == 0
      ) {
        "Sample table is empty."
      } else {
        NULL
      }
    })

    ## Config autofill button state ----
    shiny::observe(
      {
        if (!is.null(config_file()) && is.null(config_apply_block())) {
          shinyjs::enable("use_config")
        } else {
          shinyjs::disable("use_config")
        }
      },
      label = "Use Config Button State"
    )

    ## Config autofill ----
    apply_config_to_samples <- function() {
      shiny::req(is.null(config_apply_block()))
      shiny::req(!is.null(config_file()))

      cfg <- config_file()
      cleared_tbl <- sample_table_data()
      # Exclude Replicate from clearing — it is auto-computed, not user-entered —
      # and Include, which is the user's choice of samples, not a declaration
      non_sample_cols <- setdiff(
        names(cleared_tbl),
        c("Include", "Sample", "Replicate")
      )
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
        write_log("Config autofill applied to sample table")
        sample_table_data(new_tbl)
        sample_table_trigger(sample_table_trigger() + 1)
      }

      if (
        has_conc &&
          has_time &&
          !isTRUE(conversion_sidebar_vars$run_kinact_ki())
      ) {
        trigger_kinact_ki(trigger_kinact_ki() + 1L)
      }
    }

    shiny::observeEvent(
      input$use_config,
      {
        shiny::req(input$use_config > 0)
        apply_config_to_samples()
      },
      ignoreInit = TRUE,
      priority = -5,
      label = "Config Autofill"
    )

    # Same autofill, requested from the config modal ("Confirm & Apply")
    if (!is.null(config_apply_trigger)) {
      shiny::observeEvent(
        config_apply_trigger(),
        {
          shiny::req(config_apply_trigger() > 0)
          apply_config_to_samples()
        },
        ignoreInit = TRUE,
        priority = -5,
        label = "Config Autofill — Modal"
      )
    }

    ## Replicate column — refresh when config changes ----
    shiny::observeEvent(
      config_file(),
      {
        tbl <- sample_table_data()
        if (is.null(tbl) || nrow(tbl) == 0) {
          return()
        }
        updated <- add_replicate_col(tbl, config_file())
        if (!identical(updated$Replicate, tbl$Replicate)) {
          sample_table_data(updated)
          sample_table_trigger(sample_table_trigger() + 1)
        }
      },
      ignoreInit = TRUE,
      label = "Replicate Column — Config Change"
    )

    ## Unit selection — adopt the units declared in the config ----
    # The config carries concentration and time as bare numbers, so its
    # optional unit columns are the only way it can say what they mean.
    shiny::observeEvent(
      config_file(),
      {
        cfg_units <- config_units(config_file())
        if (!is.null(cfg_units$conc)) {
          conc_unit_selected(cfg_units$conc)
          shinyWidgets::updatePickerInput(
            session,
            "conc_unit",
            selected = cfg_units$conc
          )
        }
        if (!is.null(cfg_units$time)) {
          time_unit_selected(cfg_units$time)
          shinyWidgets::updatePickerInput(
            session,
            "time_unit",
            selected = cfg_units$time
          )
        }
        declared <- c(
          if (!is.null(cfg_units$conc)) cfg_units$conc,
          if (!is.null(cfg_units$time)) cfg_units$time
        )
        if (length(declared) > 0) {
          write_log(paste(
            "Units adopted from config:",
            paste(declared, collapse = " / ")
          ))
        }
      },
      ignoreInit = TRUE,
      label = "Unit Selection — Config Change"
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
      priority = 100,
      label = "Protein Table Status Observer"
    )

    ### Observe table status for compound table ----
    shiny::observe(
      {
        shiny::req(
          compound_table_input(),
          declaration_vars$compound_table_active
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
      priority = 100,
      label = "Compound Table Status Observer"
    )

    ### Observe table status for samples table ----
    shiny::observe(
      {
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

          # Disable confirm button, clear button, and unit selectors
          shinyjs::disable("confirm_samples")
          shinyjs::disable("clear_samples")
          shinyjs::disable("conc_unit")
          shinyjs::disable("time_unit")
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

          # Disable confirm button, clear button, and unit selectors
          shinyjs::disable("confirm_samples")
          shinyjs::disable("clear_samples")
          shinyjs::disable("conc_unit")
          shinyjs::disable("time_unit")
        } else if (isTRUE(declaration_vars$sample_table_active)) {
          # The table of an uploaded file has not been rendered yet, so the
          # input still holds the previous table: keep the UI blocked
          pending <- shiny::isolate(sample_upload_pending())
          if (!is.null(pending) && !isTRUE(pending$rendered)) {
            return()
          }

          activate_sample_table_ui(clean_sample_table(samples_table_input))
        }

        # Unblock UI
        sample_upload_pending(NULL)
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "none";'
        ))
      },
      priority = 100,
      label = "Sample Table Status Observer"
    )

    ## Event activate kinact_ki analysis ----
    shiny::observeEvent(
      conversion_sidebar_vars$run_kinact_ki(),
      {
        # Its first value after a restore is the state the table is in already
        if (!is.null(kinact_ki_last)) {
          unchanged <- identical(
            isTRUE(conversion_sidebar_vars$run_kinact_ki()),
            isTRUE(kinact_ki_last)
          )
          kinact_ki_last <<- NULL
          if (unchanged) {
            return()
          }
        }
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
            isTRUE(conversion_sidebar_vars$run_kinact_ki())
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
            kinact_ki = has_conc_time
          )

          # Only append empty Conc/Time when they were absent before;
          # fill_sample_table already restores them when kinact_ki = TRUE
          if (!has_conc_time) {
            sample_table_data(cbind(
              sample_table,
              Concentration = as.numeric(NA),
              Time = as.numeric(NA)
            ))
          } else {
            sample_table_data(sample_table)
          }
        } else if (isTRUE(conversion_sidebar_vars$run_kinact_ki())) {
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
      },
      ignoreInit = TRUE,
      label = "kinact/KI Activation"
    )

    ## Edit button event ----
    shiny::observeEvent(
      list(
        input$edit_proteins,
        input$edit_compounds,
        input$edit_samples
      ),
      {
        shiny::req(input$tabs)
        shiny::req(
          isTRUE(input$edit_proteins > 0) ||
            isTRUE(input$edit_compounds > 0) ||
            isTRUE(input$edit_samples > 0)
        )

        # If edit applied always activate edit mode for sample table if present
        if (!is.null(input$samples_table)) {
          if (input$tabs == "Samples") {
            write_log("Sample table reopened for editing")
          }
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
            kinact_ki = conversion_sidebar_vars$run_kinact_ki()
          ))
          sample_table_trigger(sample_table_trigger() + 1)

          # Activate table observer
          declaration_vars$sample_table_active <- TRUE
        }

        # Edit Protein/Compound
        if (input$tabs == "Proteins") {
          write_log("Protein table reopened for editing")
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
          write_log("Compound table reopened for editing")
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
      },
      ignoreInit = TRUE,
      label = "Edit Table Event"
    )

    ## Confirm button event ----
    shiny::observeEvent(
      list(
        input$confirm_proteins,
        input$confirm_compounds,
        input$confirm_samples
      ),
      {
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
          write_log(paste(
            "Protein table confirmed:",
            nrow(protein_table),
            "protein(s)"
          ))
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
            old_table <- sample_table_data()
            new_table <- add_replicate_col(
              new_sample_table(
                result = declaration_vars$result,
                protein_table = protein_table,
                compound_table = declaration_vars$compound_table,
                kinact_ki = conversion_sidebar_vars$run_kinact_ki()
              ),
              config_file()
            )
            new_table <- restore_inclusion(
              restore_conc_time(new_table, old_table),
              old_table
            )
            sample_table_data(new_table)
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
          write_log(paste(
            "Compound table confirmed:",
            nrow(compound_table),
            "compound(s)"
          ))
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
            old_table <- sample_table_data()
            new_table <- add_replicate_col(
              new_sample_table(
                result = declaration_vars$result,
                protein_table = declaration_vars$protein_table,
                compound_table = compound_table,
                kinact_ki = conversion_sidebar_vars$run_kinact_ki()
              ),
              config_file()
            )
            new_table <- restore_inclusion(
              restore_conc_time(new_table, old_table),
              old_table
            )
            sample_table_data(new_table)
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
          shiny::req(sample_table_input())
          raw_input <- sample_table_input()
          # Concentration/Time come back as character from hot_to_r (text-type
          # cells); convert to numeric before further processing
          for (.col in grep(
            "^Concentration|^Time",
            names(raw_input),
            value = TRUE
          )) {
            if (is.character(raw_input[[.col]])) {
              raw_input[[.col]] <- suppressWarnings(as.numeric(raw_input[[
                .col
              ]]))
            }
          }
          # Concentration/time are declared as bare numbers, so the picked
          # units are the only record of what they mean — they end up in the
          # column labels and travel through every result table and export.
          # Refuse to confirm rather than stamping a default onto the data.
          conc_time_cols <- grep(
            "^Concentration|^Time",
            names(raw_input),
            value = TRUE
          )
          if (length(conc_time_cols) == 2) {
            missing_units <- c(
              if (!shiny::isTruthy(input$conc_unit)) "concentration",
              if (!shiny::isTruthy(input$time_unit)) "time"
            )
            if (length(missing_units) > 0) {
              shinyWidgets::show_toast(
                "Unit missing",
                text = paste0(
                  "Select the ",
                  paste(missing_units, collapse = " and "),
                  " unit before confirming the sample table."
                ),
                type = "warning",
                timer = 6000,
                timerProgressBar = TRUE
              )
              # Release the overlay the handler put up before bailing out
              shinyjs::runjs(paste0(
                'document.getElementById("blocking-overlay").style.display ',
                '= "none";'
              ))
              return()
            }
          }

          # The checked table can lag the last click on an Include box by the
          # debounce; never hand on a table without samples
          if (!any(sample_inclusion(raw_input))) {
            shinyjs::runjs(paste0(
              'document.getElementById("blocking-overlay").style.display ',
              '= "none";'
            ))
            return()
          }

          # The table shown keeps every sample, the left-out ones unticked, so
          # they can be included again; the conversion gets the included ones
          confirmed <- confirm_sample_table(
            raw_input,
            units = list(conc = input$conc_unit, time = input$time_unit)
          )
          sample_table <- confirmed$shown
          converted_table <- confirmed$converted
          excluded <- confirmed$excluded
          write_log(paste0(
            "Sample table confirmed: ",
            nrow(converted_table),
            " sample(s)",
            if (length(excluded)) {
              paste0(" (", length(excluded), " excluded)")
            },
            if (length(conc_time_cols) == 2) {
              paste0(
                " · units: ",
                input$conc_unit,
                " / ",
                input$time_unit
              )
            }
          ))
          if (length(excluded)) {
            write_log(paste(
              "Samples excluded from the conversion:",
              paste(excluded, collapse = ", ")
            ))
          }

          # Assign table to reactive variables
          declaration_vars$sample_table <- converted_table
          sample_table_data(sample_table)

          # Trigger re-rendering of sample table
          declaration_vars$samples_confirmed <- TRUE
          sample_table_trigger(sample_table_trigger() + 1)

          # Disable clear button and unit selectors once table is confirmed
          shinyjs::disable("clear_samples")
          shinyjs::disable("conc_unit")
          shinyjs::disable("time_unit")

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
      },
      ignoreInit = TRUE,
      label = "Confirm Table Event"
    )

    ## Observe conversion readiness ----
    shiny::observe(
      {
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
      },
      label = "Conversion Readiness Observer"
    )

    ## Continuation from deconvolution to conversion ----
    ### Transfer results from deconvolution to sample table ----
    shiny::observeEvent(
      deconvolution_main_vars$continue_conversion(),
      {
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
          write_log(paste(
            "Deconvolution results transferred:",
            basename(db_path),
            "—",
            length(meta$samples),
            "sample(s)"
          ))

          # New table data
          sample_table_data(add_replicate_col(
            new_sample_table(
              result = declaration_vars$result,
              protein_table = declaration_vars$protein_table,
              compound_table = declaration_vars$compound_table,
              kinact_ki = conversion_sidebar_vars$run_kinact_ki()
            ),
            config_file()
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
      },
      ignoreInit = TRUE,
      label = "Deconvolution Results Transfer"
    )

    ### User confirms samples table overwrite ----
    shiny::observeEvent(
      input$conversion_cont_conf,
      {
        # UI Blocking
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "block";'
        ))

        result_list <- conversion_sidebar_vars$result_list()

        if (!is.null(result_list)) {
          # Drop the result panels before the declaration interface takes the
          # stage again — the two must not share the DOM
          reset_result_interfaces()

          # Show declaration interface
          output$conversion_ui <- shiny::renderUI(
            conversion_declaration_ui(
              ns,
              proteins_status = "confirmed",
              compounds_status = "confirmed",
              conc_unit = shiny::isolate(conc_unit_selected()),
              time_unit = shiny::isolate(time_unit_selected())
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
        write_log(paste(
          "Deconvolution results transferred (overwrite):",
          basename(db_path),
          "—",
          length(meta$samples),
          "sample(s)"
        ))

        # New table data
        sample_table_data(add_replicate_col(
          new_sample_table(
            result = declaration_vars$result,
            protein_table = declaration_vars$protein_table,
            compound_table = declaration_vars$compound_table,
            kinact_ki = conversion_sidebar_vars$run_kinact_ki()
          ),
          config_file()
        ))
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
      },
      ignoreInit = TRUE,
      label = "Samples Table Overwrite"
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
      hits_summary = NULL,
      hits_summary_adduct = NULL
    )

    # Reactive value to track current hits data frame
    hits_unified_current <- shiny::reactiveVal()
    hits_unified_raw <- shiny::reactiveVal()
    hits_pending_nav <- shiny::reactiveVal(NULL)

    # Raw data frames for table exports
    samples_table_view_raw <- shiny::reactiveVal()
    overview_table_view_raw <- shiny::reactiveVal()
    kobs_result_raw <- shiny::reactiveVal()

    # Concentration ticks not yet applied: rapid clicks in the Per-Concentration
    # Fits table collect here and refit once the clicking pauses
    kobs_selection_pending <- shiny::reactiveVal(NULL)
    kobs_selection_settled <- shiny::debounce(kobs_selection_pending, 600)

    ## Reactive functions ----
    # The results with the kinetics of the complex picked in the Results Menu.
    # Only the kinetics differ between complexes; the hits stay those of the
    # whole run.
    kinetics_result_list <- shiny::reactive({
      select_complex_kinetics(
        conversion_sidebar_vars$result_list(),
        conversion_sidebar_vars$complex()
      )
    })

    # Infer kinact/KI result from selected samples
    kinact_ki_result <- shiny::reactive({
      shiny::req(conversion_sidebar_vars$result_list())

      if (is.null(conversion_vars$modified_results)) {
        result_list <- kinetics_result_list()
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

      return(result_list$kinact_ki_result)
    })

    ## Render conversion results interface ----

    ### Result interface panels ----
    # The four Results Menu entries each own a panel in the persistent
    # #result_interface_stack container. A panel is filled the first time its
    # entry is selected and then stays in the DOM; every later switch only
    # moves the active class, so the plots, tables and spectra behind it are
    # never recomputed. The panels are emptied again when the results are
    # reset, which also keeps stale figures from flashing up while the next
    # analysis renders.
    iface_keys <- c("binding", "kinetics", "summary", "hits")

    iface_state <- new.env(parent = emptyenv())
    iface_state$built <- character(0)
    iface_state$observers <- list()
    # The inputs there were before the first panel of a result was built, and
    # every input a result has added since (see wipe_result_state())
    iface_state$input_baseline <- NULL
    iface_state$result_inputs <- character(0)

    # Observers created while building a panel belong to that build. They are
    # destroyed on reset; otherwise every re-run would stack another copy on
    # the same inputs, and copies updating their own inputs loop forever.
    track_iface_observer <- function(observer) {
      iface_state$observers <- c(iface_state$observers, list(observer))
      invisible(observer)
    }

    # The columns picked in a hits table column picker (hits_col_picker()),
    # starting at its default. Only a change re-renders the table: a picker in
    # a settings popover is bound on its first opening only and then reports
    # the default it already has.
    picked_cols <- function(id, selected) {
      cols <- shiny::reactiveVal(selected)
      track_iface_observer(shiny::observeEvent(
        input[[id]],
        {
          if (!identical(cols(), input[[id]])) {
            cols(input[[id]])
          }
        },
        ignoreInit = TRUE,
        ignoreNULL = FALSE
      ))
      cols
    }

    # The columns of a Table View: Binding [%], and Preferred while the rows
    # shown (`shown()`, a subset of hits_summary) have a reading that was not
    # preferred. The picker (`<id>` swapped for `<id>_ui`, rendered when its
    # popover is first opened) follows that default; picks are kept until the
    # default changes.
    table_view_cols <- function(id, hits_summary, units, shown) {
      choices <- hits_col_choices(hits_summary, units)$choices
      default_cols <- function() {
        table_view_col_choices(hits_summary, shown(), units)$selected
      }

      default <- shiny::reactiveVal(shiny::isolate(default_cols()))
      track_iface_observer(shiny::observe(default(default_cols())))

      cols <- picked_cols(id, shiny::isolate(default()))
      track_iface_observer(shiny::observeEvent(
        default(),
        cols(default()),
        ignoreInit = TRUE
      ))

      output[[sub("select$", "ui", id)]] <- shiny::renderUI(
        hits_col_picker(ns(id), list(choices = choices, selected = default()))
      )
      cols
    }

    # The plots colour either compounds or samples; a sample's colour may
    # stand for its concentration, time or protein (see overview_colors())
    color_mode <- function(variable) {
      if (identical(variable, "Compounds")) "Compounds" else "Samples"
    }

    iface_key <- function(analysis_select) {
      switch(
        as.character(analysis_select),
        "1" = "summary",
        "2" = "binding",
        "3" = "kinetics",
        "4" = "hits",
        NULL
      )
    }

    # The panel outputs are never suspended: an unfilled panel has no size and
    # an inactive one is only hidden visually, and in both cases Shiny would
    # otherwise refuse to render (or re-render) their content.
    lapply(iface_keys, function(key) {
      output[[paste0("iface_", key)]] <- shiny::renderUI(NULL)
      shiny::outputOptions(
        output,
        paste0("iface_", key),
        suspendWhenHidden = FALSE
      )
    })

    # Inactive panels keep their box size and stay "visible" to Shiny (they are
    # only made transparent to the eye and to the pointer) so the outputs they
    # contain are not suspended and do not re-execute when shown again.
    set_active_result_interface <- function(key) {
      shinyjs::runjs(sprintf(
        paste0(
          "(function(){var s=document.getElementById('%s'); if(!s) return;",
          "Array.prototype.forEach.call(s.children,function(c){",
          "c.classList.toggle('result-interface-panel--active',",
          "c.getAttribute('data-iface') === '%s');});",
          "window.dispatchEvent(new Event('resize'));})();"
        ),
        ns("result_interface_stack"),
        if (is.null(key)) "" else key
      ))
    }

    ### Wiping a result ----
    # Emptying the panels removes their elements, but not what Shiny keeps of
    # them: the server keeps the last value of every input, and the browser the
    # last value of every output. A panel built for the next result has the
    # same ids, so it rendered the old figures from the browser's copy for as
    # long as the new ones took, and its observers read the old picks (the
    # Group By of a result with many compounds stayed on "Compound" for one
    # with a single compound). Everything a result added is therefore dropped
    # on both sides. What a result added is what was not there when its first
    # panel was built: the declaration's inputs and outputs stay, as do the
    # export handlers, registered once per session (setup_plot_dl()).
    result_js_state <- paste0(
      "var st = (window.kiwiResultState = window.kiwiResultState || {});",
      "st = st[p] = st[p] || {known: {}};"
    )

    mark_result_baseline <- function() {
      iface_state$input_baseline <- setdiff(
        shiny::isolate(names(input)),
        iface_state$result_inputs
      )
      shinyjs::runjs(sprintf(
        paste0(
          "(function(p){",
          result_js_state,
          "st.baseline = {};",
          "var app = Shiny.shinyapp;",
          "[app.$values, app.$errors].forEach(function(store){",
          "Object.keys(store || {}).forEach(function(k){",
          "if (k.indexOf(p) === 0 && !st.known[k]) st.baseline[k] = true;",
          "});});})('%s');"
        ),
        ns("")
      ))
    }

    # Drops the browser's copies of the outputs in the panel `key`
    forget_panel_outputs <- function(key) {
      shinyjs::runjs(sprintf(
        paste0(
          "(function(){var s=document.getElementById('%s'); if(!s) return;",
          "var panel = s.querySelector('[data-iface=\"%s\"]'); if(!panel) return;",
          "var app = Shiny.shinyapp;",
          "panel.querySelectorAll('.shiny-bound-output').forEach(function(el){",
          "if (!el.id || el.id.indexOf('%s') === 0 ||",
          "el.classList.contains('shiny-download-link')) return;",
          "delete app.$values[el.id]; if (app.$errors) delete app.$errors[el.id];",
          "});})();"
        ),
        ns("result_interface_stack"),
        key,
        ns("iface_")
      ))
    }

    wipe_result_state <- function() {
      # Inputs: cleared on the server at once, so nothing built afterwards
      # reads them, and in the browser, whose record of what it last sent
      # would otherwise hold back a value equal to the old one
      ids <- iface_state$result_inputs
      if (!is.null(iface_state$input_baseline)) {
        ids <- union(
          ids,
          setdiff(shiny::isolate(names(input)), iface_state$input_baseline)
        )
      }
      iface_state$result_inputs <- ids
      if (length(ids) > 0) {
        session$manageInputs(
          stats::setNames(vector("list", length(ids)), ns(ids)),
          now = TRUE
        )
      }

      # Outputs: the browser's copies of everything not in the baseline. The
      # export handlers (dl_*) and the panel containers stay.
      shinyjs::runjs(sprintf(
        paste0(
          "(function(p, inputs){",
          result_js_state,
          "var app = Shiny.shinyapp;",
          "if (st.baseline) {",
          "var keep = [p + 'dl_', p + 'iface_'];",
          "[app.$values, app.$errors].forEach(function(store){",
          "Object.keys(store || {}).forEach(function(k){",
          "if (k.indexOf(p) !== 0 || st.baseline[k]) return;",
          "if (keep.some(function(q){ return k.indexOf(q) === 0; })) return;",
          "st.known[k] = true; delete store[k];",
          "});});}",
          "inputs.forEach(function(id){ Shiny.setInputValue(id, null); });",
          "})('%s', %s);"
        ),
        ns(""),
        jsonlite::toJSON(ns(ids))
      ))

      # Server-side state of the result
      conversion_vars$modified_results <- NULL
      conversion_vars$select_concentration <- NULL
      conversion_vars$conc_colors <- NULL
      conversion_vars$expand_helper <- FALSE
      conversion_vars$hits_summary <- NULL
      conversion_vars$hits_summary_adduct <- NULL
      conversion_vars$formatted_hits <- NULL
      conversion_vars$concentrations <- NULL
      conversion_vars$units <- NULL
      conversion_vars$color_key <- NULL
      hits_unified_current(NULL)
      hits_unified_raw(NULL)
      hits_pending_nav(NULL)
      samples_table_view_raw(NULL)
      overview_table_view_raw(NULL)
      kobs_result_raw(NULL)
      kobs_selection_pending(NULL)
      heatmap_pending_sample(NULL)
      stats_scatter_pending_sample(NULL)
      stats_boxplot_pending_sample(NULL)
      stats_violin_pending_sample(NULL)
      overview_protein(NULL)
      overview_compounds(character(0))
      spectrum_cache$reset()
      invisible(NULL)
    }

    render_result_interface <- function(key, ui) {
      # The first panel of a result: what exists now is not the result's
      if (length(iface_state$built) == 0) {
        mark_result_baseline()
      }
      # A panel built again (the kinetics of another complex) would show the
      # figures of its last build until the new ones arrive
      forget_panel_outputs(key)
      # The declaration interface and the result panels are mutually
      # exclusive; clearing it here also keeps its tab ids unique. It is
      # emptied in the browser at once too: the emptied output arrives with
      # the panel, which the browser can insert first.
      shinyjs::runjs(sprintf(
        paste0(
          "(function(){var el=document.getElementById('%s');",
          "if(el && el.children.length){Shiny.unbindAll(el); el.innerHTML='';}",
          "})();"
        ),
        ns("conversion_ui")
      ))
      output$conversion_ui <- shiny::renderUI(NULL)
      output[[paste0("iface_", key)]] <- shiny::renderUI(ui)
      iface_state$built <- unique(c(iface_state$built, key))
      set_active_result_interface(key)
    }

    reset_result_interfaces <- function() {
      lapply(iface_state$observers, function(observer) observer$destroy())
      iface_state$observers <- list()
      lapply(iface_state$kinetics_observers, function(observer) {
        observer$destroy()
      })
      iface_state$kinetics_observers <- NULL
      iface_state$built <- character(0)
      iface_state$overview_choices <- NULL
      iface_state$overview_nav <- NULL
      iface_state$kinetics_nav <- NULL
      iface_state$kinetics_fitted_conc <- NULL
      iface_state$complex <- NULL
      iface_state$result_token <- NULL

      # Before the panels are emptied: their observers are gone, so nothing
      # reacts to the inputs being cleared
      wipe_result_state()

      # Empty the panels in the browser at once. The emptied outputs below
      # arrive with the flush, together with the declaration interface a reset
      # renders next, which the browser can insert first - its inputs would
      # meet the panels' (both have "tabs") and the old content would linger
      # while it is built.
      shinyjs::runjs(sprintf(
        paste0(
          "(function(){var s=document.getElementById('%s'); if(!s) return;",
          "s.querySelectorAll('.result-interface-panel > .shiny-html-output')",
          ".forEach(function(el){Shiny.unbindAll(el); el.innerHTML='';});})();"
        ),
        ns("result_interface_stack")
      ))
      lapply(iface_keys, function(key) {
        output[[paste0("iface_", key)]] <- shiny::renderUI(NULL)
      })
      set_active_result_interface(NULL)
      invisible(NULL)
    }

    ### Navigation into the result panels ----
    # Point the Overview compound picker at the compounds of the selection's
    # protein (`choices` from overview_choices())
    update_overview_compound_picker <- function(choices, sel) {
      cmps <- if (is.na(sel$protein)) {
        data.frame(compound = character(0), hit = logical(0))
      } else {
        choices$compounds[[sel$protein]]
      }
      shinyWidgets::updatePickerInput(
        session,
        "overview_compound_picker",
        choices = cmps$compound,
        selected = sel$compounds,
        choicesOpt = list(subtext = ifelse(cmps$hit, "", "No hits")),
        options = overview_compound_picker_options(nrow(cmps) > 0)
      )
    }

    # Show `protein` with `compounds` (NULL: all of its compounds) in the
    # Overview tab of the Relative Binding interface. A panel not built yet is
    # built with this selection, the Overview being its first tab.
    navigate_overview <- function(protein, compounds = NULL) {
      choices <- iface_state$overview_choices
      if (!"binding" %in% iface_state$built || is.null(choices)) {
        iface_state$overview_nav <- list(
          protein = protein,
          compounds = compounds
        )
        return(invisible(NULL))
      }

      sel <- overview_selection(choices, protein, compounds)
      overview_protein(sel$protein)
      overview_compounds(sel$compounds)
      shinyWidgets::updatePickerInput(
        session,
        "overview_protein_picker",
        selected = sel$protein
      )
      update_overview_compound_picker(choices, sel)
      set_selected_tab("Overview", session)
      invisible(NULL)
    }

    # Open the Concentrations tab of the Kinetic Analysis on `conc`, a fitted
    # concentration, for the complex `key` (NULL: the one picked in the
    # Results Menu). The complex is picked in the Results Menu, which rebuilds
    # the panel; a panel still to be built opens on the concentration itself.
    navigate_kinetics <- function(conc, key = NULL) {
      result_list <- shiny::isolate(conversion_sidebar_vars$result_list())
      if (!length(result_list$kinetics)) {
        return(invisible(NULL))
      }
      if (!is.null(key) && !key %in% names(result_list$kinetics)) {
        key <- NULL
      }

      if (
        !is.null(key) &&
          !identical(key, shiny::isolate(conversion_sidebar_vars$complex()))
      ) {
        shinyWidgets::updatePickerInput(
          session$rootScope(),
          sub("conversion_main", "conversion_sidebar", ns("complex"), fixed = TRUE),
          selected = key
        )
      }

      if (
        "kinetics" %in% iface_state$built &&
          (is.null(key) || identical(key, iface_state$complex))
      ) {
        iface_state$kinetics_nav <- NULL
        if (conc %in% iface_state$kinetics_fitted_conc) {
          shinyWidgets::updatePickerInput(
            session,
            "conc_tab_select",
            selected = conc
          )
          set_selected_tab("Concentrations", session, id = "kinetics_tabs")
        }
      } else {
        iface_state$kinetics_nav <- list(
          complex = key,
          conc = conc,
          time = Sys.time()
        )
      }
      invisible(NULL)
    }

    # Activate observer on analysis launch
    shiny::observe(
      {
        shiny::req(results_observer)

        if (is.null(conversion_sidebar_vars$result_list())) {
          results_observer$suspend()
        } else {
          results_observer$resume()
        }
      },
      label = "Results Observer Activation"
    )

    # Observer rendering UI on conditions
    results_observer <- shiny::observe(
      {
        result_list <- kinetics_result_list()

        analysis_select <- conversion_sidebar_vars$analysis_select()
        shiny::req(length(analysis_select) > 0)

        iface <- iface_key(analysis_select)

        # Panels of another result are wiped first, also when it arrives
        # without the reset in between (this observer is suspended while
        # there is no result, so it may never have seen that)
        run_result <- shiny::isolate(conversion_sidebar_vars$result_list())
        result_token <- if (!is.null(run_result)) {
          rlang::obj_address(run_result)
        }
        if (
          !is.null(result_token) &&
            !identical(result_token, iface_state$result_token)
        ) {
          if (length(iface_state$built) > 0) {
            reset_result_interfaces()
          }
          iface_state$result_token <- result_token
        }

        # Another complex picked: the kinetics panel is built anew for it, the
        # other panels do not depend on the complex
        complex <- result_list$kinetics_complex$key
        if (!identical(complex, iface_state$complex)) {
          iface_state$complex <- complex
          iface_state$built <- setdiff(iface_state$built, "kinetics")
        }

        # Nothing to build: the selected interface is already rendered, so the
        # switch is a pure visibility change and needs no blocking overlay.
        if (
          !is.null(result_list) &&
            !is.null(iface) &&
            iface %in% iface_state$built
        ) {
          set_active_result_interface(iface)
          return(invisible(NULL))
        }

        # Block UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "block";'
        ))

        shiny::isolate({
          run_kinact_ki <- conversion_sidebar_vars$run_kinact_ki()
          select_concentration <- conversion_vars$select_concentration
        })

        if (is.null(result_list)) {
          #### Reset results ui elements ----

          # Drop the mounted result panels so their figures cannot flash up
          # again when the next analysis renders
          reset_result_interfaces()

          # Null kinetics interface
          output$kinact <- NULL
          output$Ki <- NULL
          output$Kinact_Ki <- NULL
          output$kobs_result <- NULL
          output$proteoform_kobs_plot <- NULL
          output$proteoform_paired_plot <- NULL
          output$proteoform_table <- NULL
          output$binding_plot <- NULL
          output$kobs_plot <- NULL

          output$conc_tab_kobs_value <- NULL
          output$conc_tab_plateau_value <- NULL
          output$conc_tab_reached_value <- NULL
          output$conc_tab_hits <- NULL
          output$conc_tab_binding_plot <- NULL
          output$conc_tab_spectra <- NULL

          # Reset render trigger so bindEvent-guarded plots don't fire stale data
          render_trigger(0)
          manual_render_spectrum(0L)
          show_completion_toast(NULL)

          # The cached spectra are keyed by selection, not by dataset
          spectrum_cache$reset()
          overview_protein(NULL)
          overview_compounds(character(0))
          iface_state$overview_nav <- NULL
          iface_state$kinetics_nav <- NULL

          # Null binding interface
          output$hits_unified_tab <- NULL
          output$samples_selected_protein <- NULL
          output$samples_quality <- NULL
          output$samples_compound_distribution_ui <- NULL
          output$samples_present_compounds_na <- NULL
          output$samples_compound_distribution <- NULL
          output$samples_annotated_spectrum <- NULL
          output$samples_table_view <- NULL
          output$overview_protein_mass_shifts <- NULL
          output$overview_compound_mass_shifts <- NULL
          output$overview_distribution_ui <- NULL
          output$overview_distribution_na <- NULL
          output$overview_compound_distribution <- NULL
          output$overview_spectrum_container <- NULL
          output$overview_spectrum_na <- NULL
          output$overview_spectrum_prompt <- NULL
          output$overview_annotated_spectrum <- NULL
          output$overview_table_view <- NULL
          output$color_variable_ui <- NULL

          # Null summary interface
          output$summary_protocol <- NULL
          output$stats_histogram <- NULL
          output$stats_boxplot <- NULL
          output$stats_scatter <- NULL
          output$stats_violin <- NULL
          output$batch_heatmap_cards <- NULL
          lapply(
            c(
              "batch_heatmap_total_pct",
              "batch_heatmap_pct_cmp",
              "batch_heatmap_compound",
              "batch_heatmap_protein",
              "batch_heatmap_concentration",
              "batch_heatmap_time"
            ),
            function(id) output[[id]] <- NULL
          )
          lapply(
            c(
              "pstat_n_samples",
              "pstat_n_proteins",
              "pstat_n_compounds",
              "pstat_n_hits",
              "pstat_correct",
              "pstat_correct_stat",
              "pstat_unmatched",
              "pstat_unmatched_stat",
              "pstat_peak_tol",
              "pstat_max_stoich",
              "pstat_alerts",
              "pstat_warnings"
            ),
            function(id) output[[id]] <- NULL
          )

          #### Render declaration ui ----
          output$conversion_ui <- shiny::renderUI(
            conversion_declaration_ui(
              ns,
              proteins_status = "confirmed",
              compounds_status = "confirmed",
              samples_status = "confirmed",
              conc_unit = shiny::isolate(conc_unit_selected()),
              time_unit = shiny::isolate(time_unit_selected())
            )
          )

          # Mark all tabs saved, lock the sample inputs, select samples tab
          restore_declaration_state(TRUE, TRUE, TRUE)

          # Unblock UI
          shinyjs::runjs(paste0(
            'document.getElementById("blocking-overlay").style.display ',
            '= "none";'
          ))
        } else if (!is.null(result_list)) {
          shiny::req(!is.null(iface))

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
                `Tot. Binding [%]`,
                `Binding [%]`,
                `Sample ID`
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

          # Colours of the result, fixed from all of it before any view
          # filters it (see color_key()): every interface and export reads
          # them from here
          ckey <- color_key(
            hits_summary,
            sample_order = sort_hits_table(hits_summary, units)$`Sample ID`
          )
          conversion_vars$color_key <- ckey

          shinyWidgets::updateMaterialSwitch(
            session,
            "stats_boxplot_show_points",
            value = nrow(hits_summary) <= 100
          )

          # Without concentrations the spectra are never grouped by
          # concentration, so the switch that undoes that grouping has nothing
          # to offer and stays out of the settings popovers.
          conc_col <- names(hits_summary)[
            grep("Conc.", names(hits_summary), fixed = TRUE)
          ][1]
          has_concentration <- !is.na(conc_col) &&
            any(!is.na(suppressWarnings(as.numeric(hits_summary[[conc_col]]))))

          ### Render result interfaces ----
          if (iface == "binding") {
            #### Render relative binding interface ----
            # The Overview opens on the default protein with all of its
            # compounds, or on what a click in the Hits table asked for
            ov_choices <- overview_choices(hits_summary)
            ov_nav <- iface_state$overview_nav
            iface_state$overview_nav <- NULL
            ov_initial <- overview_selection(
              ov_choices,
              protein = ov_nav$protein,
              compounds = ov_nav$compounds
            )
            overview_protein(ov_initial$protein)
            overview_compounds(ov_initial$compounds)

            # Identifies this build in the spectrum cache keys
            iface_state$binding_build <- (iface_state$binding_build %||% 0L) +
              1L
            binding_build <- iface_state$binding_build

            render_result_interface(
              "binding",
              binding_results_ui(
                ns,
                hits_summary,
                units = units,
                show_sort_binding = has_concentration || isTRUE(run_kinact_ki),
                overview = ov_initial
              )
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

                sample_rows <- hits_summary[
                  hits_summary$`Sample ID` == selected,
                ]

                protein <- unique(sample_rows$Protein)

                # One entry per declared protein species — a protein may carry
                # several masses (e.g. a modified form), so theoretical and
                # measured mass are read as pairs
                species <- sample_rows[
                  !duplicated(sample_rows$`Theor. Prot. [Da]`),
                  c("Theor. Prot. [Da]", "Meas. Prot. [Da]")
                ]

                # Convert to numeric (columns may be character after display formatting)
                theor_protein_mw <- suppressWarnings(as.numeric(
                  species$`Theor. Prot. [Da]`
                ))
                measured_protein_mw <- suppressWarnings(as.numeric(
                  species$`Meas. Prot. [Da]`
                ))
                species_order <- order(theor_protein_mw)
                theor_protein_mw <- theor_protein_mw[species_order]
                measured_protein_mw <- measured_protein_mw[species_order]

                fmt_mw <- function(x, nsmall = 0) {
                  vapply(
                    x,
                    function(v) {
                      format(
                        v,
                        big.mark = ",",
                        nsmall = nsmall,
                        scientific = FALSE
                      )
                    },
                    character(1)
                  )
                }

                # Every row lists one value per species; past two species the
                # rest moves into the hover text of a trailing ellipsis
                if (all(is.na(measured_protein_mw))) {
                  signal_average <- "No signal"
                } else {
                  signal_average <- collapse_species(ifelse(
                    is.na(measured_protein_mw),
                    "No signal",
                    paste(fmt_mw(round(measured_protein_mw, 1), 1), "Da")
                  ))
                }

                # Binding of each species on its own, in the order of the
                # masses above. A value pinned by an undetected peak, or not
                # resolvable because its complex fits another species, is
                # marked and explained on hover.
                species_binding <- proteoform_binding(sample_rows)
                sb <- species_binding[match(
                  theor_protein_mw,
                  species_binding$species
                ), ]
                binding_text <- collapse_species(ifelse(
                  sb$limit %in% "shared",
                  sprintf(
                    "<span class=\"protocol-stat-warn\" data-bs-toggle=\"tooltip\" title=\"%s\">N/A</span>",
                    proteoform_limit_note("shared")
                  ),
                  ifelse(
                    is.na(sb$binding),
                    "N/A",
                    ifelse(
                      is.na(sb$limit),
                      sprintf("%.2f%%", sb$binding),
                      sprintf(
                        "<span class=\"protocol-stat-warn\" data-bs-toggle=\"tooltip\" title=\"%s\">%.2f%%</span>",
                        proteoform_limit_note(sb$limit),
                        sb$binding
                      )
                    )
                  )
                ))

                shiny::div(
                  class = "conversion-sample-protein-box",
                  shiny::div(
                    class = "conversion-sample-protein-names",
                    shiny::HTML("Name<br>Mw<br>Signal<br>Binding")
                  ),
                  shiny::div(
                    class = "conversion-sample-protein",
                    shiny::HTML(paste(
                      protein,
                      "<br>",
                      collapse_species(paste(fmt_mw(theor_protein_mw), "Da")),
                      "<br>",
                      signal_average,
                      "<br>",
                      binding_text
                    ))
                  )
                )
              }
            )

            ###### Quality metrics ----
            output$samples_quality <- shiny::renderUI({
              shiny::req(
                hits_summary,
                input$conversion_sample_picker
              )

              selected_sample <- input$conversion_sample_picker
              tbl <- hits_summary[
                hits_summary$`Sample ID` == selected_sample,
              ]

              # Sample-level values, repeated on every hit row of the sample
              correct <- suppressWarnings(as.numeric(tbl$`Correct [%]`[1]))
              unmatched <- suppressWarnings(as.numeric(
                tbl$`Unmatched [%]`[1]
              ))

              if (!nrow(tbl) || (is.na(correct) && is.na(unmatched))) {
                return(shiny::div("N/A", class = "na-placeholder"))
              }

              # Peak count behind the metrics: assigned are the protein
              # species and complexes, as in the metric itself
              sample <- result_list$deconvolution[[selected_sample]]
              peak_mass <- sample$peaks$mass
              assigned <- c(
                sample$hits$`Measured Mw Protein [Da]`,
                sample$hits$`Peak [Da]`
              )
              n_peaks <- length(peak_mass)
              n_matched <- sum(peak_mass %in% assigned)

              # Same warning levels as the Protocol tab's quality cards
              fmt_metric <- function(value, warn, err) {
                if (is.na(value)) {
                  return("N/A")
                }
                cls <- if (err(value)) {
                  "protocol-stat-err"
                } else if (warn(value)) {
                  "protocol-stat-warn"
                } else {
                  NULL
                }
                as.character(shiny::span(
                  class = cls,
                  sprintf("%.2f%%", value)
                ))
              }

              shiny::div(
                class = "conversion-sample-protein-box",
                shiny::div(
                  class = "conversion-sample-protein-names",
                  shiny::HTML("Correct<br>Unmatched<br>Peaks")
                ),
                shiny::div(
                  class = "conversion-sample-protein",
                  shiny::HTML(paste0(
                    fmt_metric(
                      correct,
                      warn = function(x) x < 50,
                      err = function(x) x < 10
                    ),
                    "<br>",
                    fmt_metric(
                      unmatched,
                      warn = function(x) x > 50,
                      err = function(x) x > 90
                    ),
                    "<br>",
                    sprintf("%d / %d matched", n_matched, n_peaks)
                  ))
                )
              )
            })

            ###### Compound distribution ----
            output$samples_compound_distribution_ui <- shiny::renderUI({
              shiny::req(
                hits_summary,
                input$conversion_sample_picker
              )

              # A sample can hold rows without an adduct for protein species
              # that carry no complex, so the chart is empty only when the
              # sample has no binding event at all
              tbl <- hits_summary[
                hits_summary$`Sample ID` %in% input$conversion_sample_picker &
                  is_complex_row(hits_summary),
              ]

              if (nrow(tbl) < 1) {
                shiny::div(
                  class = "plot-placeholder",
                  shiny::textOutput(ns("samples_present_compounds_na"))
                )
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

            output$samples_present_compounds_na <- shiny::renderText({
              # Without any protein or complex peak there was nothing to
              # measure, which is not the same as no binding
              sample_total <- hits_summary$`Tot. Binding [%]`[
                hits_summary$`Sample ID` %in% input$conversion_sample_picker
              ]
              if (length(sample_total) && all(is.na(sample_total))) {
                "No protein or complex peak found"
              } else {
                "No binding events"
              }
            })

            output$samples_compound_distribution <- plotly::renderPlotly({
              shiny::req(
                hits_summary,
                input$conversion_sample_picker,
                input$color_variable,
                !is.null(input$truncate_names)
              )

              smpl_compound_distribution(
                hits_summary = hits_summary,
                sample = input$conversion_sample_picker,
                color_variable = color_mode(input$color_variable),
                truncate_names = input$truncate_names,
                key = ckey,
                colors = sample_view_colors(
                  hits_summary,
                  ckey,
                  input$color_variable,
                  input$conversion_sample_picker,
                  trunc = input$truncate_names
                )
              )
            }) |>
              shiny::bindEvent(
                render_trigger(),
                input$conversion_sample_picker,
                input$truncate_names
              )

            # Settings-menu inputs are inside bslib::popover (display:none until opened).
            # Shiny suspends hidden outputs, so their inputs fire NULL→value on first
            # open. Using observeEvent(ignoreInit=TRUE) + reactiveVal ensures those
            # initialization events never trigger a re-render.
            smpl_spectrum_settings <- shiny::reactiveVal(0L)
            ov_dist_settings <- shiny::reactiveVal(0L)

            track_iface_observer(shiny::observeEvent(
              list(
                input$sample_view_spectrum_annotation,
                input$sample_view_spectrum_diff,
                input$sample_view_spectrum_unmatched
              ),
              smpl_spectrum_settings(smpl_spectrum_settings() + 1L),
              ignoreInit = TRUE,
              ignoreNULL = TRUE
            ))
            track_iface_observer(shiny::observeEvent(
              list(
                input$overview_distribution_labels,
                input$overview_distribution_scale,
                input$overview_distribution_sort_binding,
                input$overview_distribution_unmatched
              ),
              ov_dist_settings(ov_dist_settings() + 1L),
              ignoreInit = TRUE,
              ignoreNULL = TRUE
            ))

            # Columns of the Sample View's Table View: the rows of the
            # selected sample
            smpl_table_cols <- table_view_cols(
              "samples_table_col_select",
              hits_summary,
              units,
              shiny::reactive(
                hits_summary[
                  hits_summary$`Sample ID` %in% input$conversion_sample_picker,
                  ,
                  drop = FALSE
                ]
              )
            )

            ###### Annotated spectrum ----
            output$samples_annotated_spectrum <- plotly::renderPlotly({
              shiny::req(
                hits_summary,
                input$color_variable,
                input$conversion_sample_picker,
                !is.null(input$truncate_names)
              )

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
                color_cmp = sample_view_colors(
                  hits_summary,
                  ckey,
                  color_variable,
                  selected_sample
                ),
                symbol_cmp = if (color_variable == "Compounds") {
                  key_marker_symbols(
                    ckey,
                    "Compounds",
                    unique(stats::na.omit(tbl$`Cmp Name`))
                  )
                },
                color_variable = color_mode(color_variable),
                show_peak_labels = ifelse(
                  is.null(input$sample_view_spectrum_annotation),
                  FALSE,
                  input$sample_view_spectrum_annotation
                ),
                show_mass_diff = ifelse(
                  is.null(input$sample_view_spectrum_diff),
                  TRUE,
                  input$sample_view_spectrum_diff
                ),
                show_unmatched = isTRUE(input$sample_view_spectrum_unmatched)
              )
            }) |>
              shiny::bindEvent(
                render_trigger(),
                input$conversion_sample_picker,
                input$truncate_names,
                smpl_spectrum_settings()
              )

            ###### Samples view table ----
            # The hits table of the selected sample
            output$samples_table_view <- DT::renderDataTable(
              {
                shiny::req(
                  hits_summary,
                  input$conversion_sample_picker,
                  input$color_variable,
                  !is.null(input$truncate_names)
                )
                tbl <- hits_view_table(
                  hits_summary,
                  smpl_table_cols(),
                  units = units,
                  samples = input$conversion_sample_picker
                )

                # Assign filtered table to reactive for eventual export
                samples_table_view_raw(tbl)

                render_hits_view(
                  tbl,
                  colors = sample_view_colors(
                    hits_summary,
                    ckey,
                    input$color_variable,
                    input$conversion_sample_picker,
                    trunc = input$truncate_names
                  ),
                  color_variable = color_mode(input$color_variable),
                  truncate_names = input$truncate_names,
                  units = units
                )
              },
              server = FALSE
            ) |>
              shiny::bindEvent(
                input$conversion_sample_picker,
                render_trigger(),
                input$truncate_names,
                smpl_table_cols()
              )

            ##### Overview tab ----
            # One protein with the compounds picked for it. The selection is
            # held in overview_protein() / overview_compounds(); the pickers
            # write to it, so a new protein brings its compounds along at once
            # instead of after the compound picker has come back updated.
            iface_state$overview_choices <- ov_choices

            ov_selection <- shiny::reactive({
              list(
                protein = overview_protein(),
                compounds = overview_compounds()
              )
            })

            ov_subset <- shiny::reactive({
              overview_subset(
                hits_summary,
                overview_protein(),
                overview_compounds()
              )
            })

            # Columns of the Overview's Table View: the rows of the selected
            # protein and compounds
            ov_table_cols <- table_view_cols(
              "overview_table_col_select",
              hits_summary,
              units,
              ov_subset
            )

            ov_n_samples <- shiny::reactive({
              length(unique(stats::na.omit(ov_subset()$`Sample ID`)))
            })

            ###### Pickers ----
            # A new protein: its compounds, all of them picked
            track_iface_observer(shiny::observeEvent(
              input$overview_protein_picker,
              {
                protein <- input$overview_protein_picker
                shiny::req(!identical(protein, overview_protein()))

                sel <- overview_selection(ov_choices, protein = protein)
                overview_protein(sel$protein)
                overview_compounds(sel$compounds)
                update_overview_compound_picker(ov_choices, sel)
              },
              ignoreInit = TRUE
            ))

            # Compounds picked for the current protein. One left over from the
            # previous protein, sent before its picker was updated, is dropped.
            track_iface_observer(shiny::observeEvent(
              input$overview_compound_picker,
              {
                protein <- overview_protein()
                declared <- if (is.null(protein) || is.na(protein)) {
                  character(0)
                } else {
                  ov_choices$compounds[[protein]]$compound
                }
                overview_compounds(
                  declared[declared %in% input$overview_compound_picker]
                )
              },
              ignoreNULL = FALSE,
              ignoreInit = TRUE
            ))

            ###### Protein mass shifts ----
            output$overview_protein_mass_shifts <- shiny::renderUI({
              protein <- overview_protein()
              if (is.null(protein) || is.na(protein)) {
                return(shiny::div("N/A", class = "na-placeholder"))
              }

              mass_shift_card(protein_mass_entries(
                hits_summary[hits_summary$Protein %in% protein, ],
                declared = declared_masses(protein_table_data(), protein)
              ))
            })

            ###### Compound mass shifts ----
            output$overview_compound_mass_shifts <- shiny::renderUI({
              compounds <- overview_compounds()
              if (!length(compounds)) {
                protein <- overview_protein()
                declared <- !is.null(protein) &&
                  !is.na(protein) &&
                  nrow(ov_choices$compounds[[protein]]) > 0
                return(shiny::div(
                  if (declared) "No compound selected" else "No compounds declared",
                  class = "na-placeholder"
                ))
              }

              mass_shift_card(compound_mass_entries(
                ov_subset(),
                compounds,
                compound_table = compound_table_data()
              ))
            })

            ###### Compound distribution ----
            # Kept apart from the selection, so the plot output is only
            # replaced when there starts or stops being something to plot:
            # a sample with a binding, 0 % included, or an Unmatched [%]
            ov_has_binding <- shiny::reactiveVal(NA)
            track_iface_observer(shiny::observe({
              sel <- ov_selection()
              ov_has_binding(overview_has_distribution(
                hits_summary,
                sel$protein,
                sel$compounds
              ))
            }))

            output$overview_distribution_ui <- shiny::renderUI({
              shiny::req(!is.na(ov_has_binding()))

              if (!ov_has_binding()) {
                shiny::div(
                  class = "plot-placeholder",
                  shiny::textOutput(ns("overview_distribution_na"))
                )
              } else {
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(
                    ns("overview_compound_distribution"),
                    height = "100%"
                  ),
                  type = 1,
                  color = "#7777f9"
                )
              }
            })

            output$overview_distribution_na <- shiny::renderText({
              overview_distribution_note(
                hits_summary,
                overview_protein(),
                overview_compounds()
              )
            })

            output$overview_compound_distribution <- plotly::renderPlotly({
              shiny::req(
                input$color_variable,
                !is.null(input$truncate_names)
              )
              sel <- ov_selection()

              plot <- overview_compound_distribution(
                hits_summary = hits_summary,
                protein = sel$protein,
                compounds = sel$compounds,
                color_variable = input$color_variable,
                truncate_names = input$truncate_names,
                key = ckey,
                distribution_scale = input$overview_distribution_scale,
                distribution_labels = input$overview_distribution_labels,
                # The order of the table and the sample colours, unless
                # ranked by binding
                sample_order = if (
                  !isTRUE(input$overview_distribution_sort_binding)
                ) {
                  ckey$samples
                },
                # On until switched off (NULL before the popover is opened)
                show_unmatched = !isFALSE(input$overview_distribution_unmatched)
              )
              shiny::req(plot)
              plot
            }) |>
              shiny::bindEvent(
                ov_selection(),
                render_trigger(),
                input$truncate_names,
                ov_dist_settings()
              )

            # A click on a bar or an Unmatched [%] marker opens the sample in
            # the Sample View (the plot carries the full ID as customdata)
            session$sendCustomMessage(
              "kiwiMS_attachScatterClicks",
              list(
                inputId = session$ns("overview_distribution_click"),
                plotElId = session$ns("overview_compound_distribution")
              )
            )
            track_iface_observer(shiny::observeEvent(
              input$overview_distribution_click,
              {
                click <- input$overview_distribution_click
                shiny::req(
                  !is.null(click$sample),
                  nzchar(trimws(as.character(click$sample)))
                )
                hits_pending_nav(list(
                  type = "sample",
                  value = as.character(click$sample)
                ))
              },
              ignoreNULL = TRUE,
              ignoreInit = TRUE
            ))

            ####### Show labels (update static switch) ----
            # Compound names are the axis labels once several compounds are
            # plotted, sample names otherwise
            track_iface_observer(shiny::observeEvent(
              list(ov_selection(), input$truncate_names),
              {
                tbl <- ov_subset()
                # Every picked compound is plotted, with a hit or without
                cmps <- unique(stats::na.omit(tbl$`Cmp Name`))
                sample_ids <- if (isTRUE(input$truncate_names)) {
                  tbl$truncSample_ID
                } else {
                  tbl$`Sample ID`
                }
                sample_ids <- unique(as.character(stats::na.omit(sample_ids)))

                shinyWidgets::updateMaterialSwitch(
                  session,
                  "overview_distribution_labels",
                  value = if (length(cmps) > 1) {
                    max(nchar(cmps)) <= 22
                  } else {
                    length(sample_ids) <= 50 ||
                      max(nchar(sample_ids), 0) <= 22
                  }
                )
              },
              ignoreInit = TRUE
            ))

            ###### Annotated spectrum ----

            # Spectra stack in the order of the table and the sample colours
            # unless ranked by binding (switch offered with concentrations)
            ov_sort_binding <- shiny::reactive({
              isTRUE(input$overview_spectrum_sort_binding)
            })

            # Sample labels and peak symbols the figure is built with. A new
            # selection resets them to what suits its number of spectra, and
            # the figure is rebuilt with them; the switches change them on the
            # figure on screen.
            ov_view_defaults <- function(tbl, truncate_names) {
              ids <- if (isTRUE(truncate_names)) {
                tbl$truncSample_ID
              } else {
                tbl$`Sample ID`
              }
              ids <- unique(as.character(stats::na.omit(ids)))
              list(
                labels = length(ids) < 2 ||
                  (length(ids) <= 8 && max(nchar(ids)) <= 20),
                symbols = length(ids) <= 20
              )
            }

            ov_initial_view <- ov_view_defaults(
              overview_subset(
                hits_summary,
                ov_initial$protein,
                ov_initial$compounds
              ),
              truncate_names = TRUE
            )
            ov_labels_val <- shiny::reactiveVal(ov_initial_view$labels)
            ov_symbols_val <- shiny::reactiveVal(ov_initial_view$symbols)

            # What the card shows: nothing to draw, the prompt of a large
            # selection, or the figure. The figure's output is only replaced
            # when this changes, so another selection is redrawn in place.
            ov_spectrum_mode <- shiny::reactiveVal(NULL)
            track_iface_observer(shiny::observe({
              n_samples <- ov_n_samples()
              ov_spectrum_mode(
                if (!n_samples) {
                  "none"
                } else if (n_samples < 30 || manual_render_spectrum() > 0L) {
                  "plot"
                } else {
                  "prompt"
                }
              )
            }))

            # Proxy updates need the figure on screen
            ov_spectrum_on_screen <- function() {
              identical(shiny::isolate(ov_spectrum_mode()), "plot")
            }

            track_iface_observer(shiny::observeEvent(
              ov_selection(),
              {
                manual_render_spectrum(0L)
              },
              ignoreInit = TRUE
            ))

            track_iface_observer(shiny::observeEvent(
              input$render_overview_spectrum_btn,
              {
                manual_render_spectrum(manual_render_spectrum() + 1L)
              }
            ))

            track_iface_observer(shiny::observeEvent(
              list(
                input$truncate_names,
                input$color_variable
              ),
              {
                if (ov_n_samples() >= 30) manual_render_spectrum(0L)
              },
              ignoreInit = TRUE
            ))

            output$overview_spectrum_na <- shiny::renderText({
              if (length(overview_compounds())) "N/A" else "No compound selected"
            })

            output$overview_spectrum_prompt <- shiny::renderText({
              sprintf(
                "Auto-render disabled for large datasets (%d samples). Click to render manually (Processing can take a while).",
                ov_n_samples()
              )
            })

            output$overview_spectrum_container <- shiny::renderUI({
              mode <- ov_spectrum_mode()
              shiny::req(mode)

              switch(
                mode,
                none = shiny::div(
                  class = "plot-placeholder",
                  shiny::textOutput(ns("overview_spectrum_na"))
                ),
                plot = shinycssloaders::withSpinner(
                  plotly::plotlyOutput(
                    ns("overview_annotated_spectrum"),
                    height = "100%"
                  ),
                  type = 1,
                  color = "#7777f9"
                ),
                prompt = shiny::div(
                  class = "plot-placeholder spectrum-render-prompt",
                  shiny::p(shiny::textOutput(
                    ns("overview_spectrum_prompt"),
                    inline = TRUE
                  )),
                  shiny::actionButton(
                    ns("render_overview_spectrum_btn"),
                    label = "Render Spectrum",
                    icon = shiny::icon("chart-line"),
                    class = "btn-outline-primary btn-sm"
                  )
                )
              )
            })

            # Built figure kept in its own reactive so the proxy observers
            # below can look up the peak-symbol trace indices without
            # rebuilding, and so the result can be cached.
            overview_spectrum_plot <- shiny::reactive({
              shiny::req(
                !is.null(input$truncate_names),
                input$color_variable
              )

              sel <- ov_selection()
              samples <- spectrum_sample_ids(
                ov_subset(),
                "Protein",
                sel$protein,
                sort_by_binding = ov_sort_binding(),
                by_mean = TRUE
              )
              if (!ov_sort_binding()) {
                samples <- samples[order(match(samples, ckey$samples))]
              }
              shiny::req(
                length(samples),
                length(samples) < 30 || manual_render_spectrum() > 0L
              )

              # Block UI
              shinyjs::runjs(paste0(
                'document.getElementById("blocking-overlay").style.display ',
                '= "block";'
              ))
              on.exit(shinyjs::runjs(paste0(
                'document.getElementById("blocking-overlay").style.display ',
                '= "none";'
              )))

              color_variable <- input$color_variable
              truncate_names <- input$truncate_names

              # Shared with the distribution plot and the table, so a sample
              # or compound has the same colour on all three
              colors <- overview_colors(
                hits_summary,
                sel$protein,
                sel$compounds,
                variable = color_variable,
                key = ckey,
                trunc = truncate_names
              )
              # Past the colours, compound peaks change shape
              ov_symbols <- if (identical(color_variable, "Compounds")) {
                key_marker_symbols(ckey, "Compounds", names(colors))
              }

              if (length(samples) == 1) {
                plot <- spectrum_plot(
                  sample = result_list$deconvolution[[samples]],
                  color_cmp = colors,
                  symbol_cmp = ov_symbols,
                  color_variable = color_mode(color_variable),
                  show_peak_labels = TRUE,
                  show_mass_diff = FALSE,
                  show_unmatched = isTRUE(shiny::isolate(
                    input$overview_spectrum_unmatched
                  ))
                )
              } else {
                plot <- multiple_spectra(
                  results_list = result_list,
                  samples = samples,
                  cubic = is.null(input$overview_spectrum_kind) ||
                    input$overview_spectrum_kind == "Cubic",
                  color_cmp = colors,
                  symbol_cmp = ov_symbols,
                  truncated = if (truncate_names) mapping else FALSE,
                  color_variable = color_mode(color_variable),
                  hits_summary = hits_summary,
                  # View-only settings are isolated: they are applied to the
                  # live figure by the proxy observers below, so reading them
                  # here must not schedule a rebuild. They are still read, so
                  # that a rebuild triggered by something else starts from the
                  # view the user is currently looking at.
                  labels_show = shiny::isolate(ov_labels_val()),
                  symbols_show = shiny::isolate(ov_symbols_val()),
                  legend_show = shiny::isolate(input$overview_spectrum_legend),
                  unmatched_show = isTRUE(shiny::isolate(
                    input$overview_spectrum_unmatched
                  ))
                )
              }

              # Build here rather than letting renderPlotly do it, so the proxy
              # observers can address traces by index without a second build.
              plotly::plotly_build(plot)
            }) |>
              # The key has to name everything the body reads, not just what
              # triggers it — color_variable in particular changes the figure
              # without being an event trigger.
              shiny::bindCache(
                binding_build,
                ov_selection(),
                render_trigger(),
                input$truncate_names,
                input$color_variable,
                input$overview_spectrum_kind,
                ov_sort_binding(),
                manual_render_spectrum(),
                shiny::isolate(ov_labels_val()),
                shiny::isolate(ov_symbols_val()),
                shiny::isolate(input$overview_spectrum_legend),
                shiny::isolate(input$overview_spectrum_unmatched),
                cache = spectrum_cache
              ) |>
              shiny::bindEvent(
                ov_selection(),
                render_trigger(),
                input$truncate_names,
                input$overview_spectrum_kind,
                ov_sort_binding(),
                manual_render_spectrum()
              )

            output$overview_annotated_spectrum <- plotly::renderPlotly({
              overview_spectrum_plot()
            })

            ####### View defaults of a new selection ----
            # Runs ahead of the figure (priority), which a new selection
            # rebuilds with these values, so they need no proxy update
            track_iface_observer(shiny::observeEvent(
              list(ov_selection(), input$truncate_names),
              {
                view <- ov_view_defaults(ov_subset(), input$truncate_names)
                ov_labels_val(view$labels)
                ov_symbols_val(view$symbols)

                shinyWidgets::updateMaterialSwitch(
                  session,
                  "overview_spectrum_symbols",
                  value = view$symbols
                )
                shinyWidgets::updateMaterialSwitch(
                  session,
                  "overview_spectrum_labels",
                  value = view$labels
                )

                # A single spectrum is drawn flat, without sample labels
                if (ov_n_samples() < 2) {
                  shinyjs::disable("overview_spectrum_labels")
                } else {
                  shinyjs::enable("overview_spectrum_labels")
                }
              },
              ignoreInit = TRUE,
              priority = 10
            ))

            ####### Switches on the figure on screen ----
            # A switch only set to the selection's default (above) changes
            # nothing; the user's flip is applied to the live figure.
            track_iface_observer(shiny::observeEvent(
              input$overview_spectrum_labels,
              {
                shiny::req(!identical(
                  input$overview_spectrum_labels,
                  ov_labels_val()
                ))
                ov_labels_val(input$overview_spectrum_labels)
                if (ov_spectrum_on_screen()) {
                  relayout_spectrum_labels(
                    session,
                    "overview_annotated_spectrum",
                    input$overview_spectrum_labels,
                    cubic = is.null(input$overview_spectrum_kind) ||
                      input$overview_spectrum_kind == "Cubic"
                  )
                }
              },
              ignoreInit = TRUE
            ))

            # Show/hide the peak symbols and their legend rows on the figure
            # that is already on screen. The traces are always built; only
            # their visibility changes, so this never touches the data.
            track_iface_observer(shiny::observeEvent(
              input$overview_spectrum_symbols,
              {
                shiny::req(!identical(
                  input$overview_spectrum_symbols,
                  ov_symbols_val()
                ))
                ov_symbols_val(input$overview_spectrum_symbols)
                if (ov_spectrum_on_screen()) {
                  restyle_peak_symbols(
                    session,
                    "overview_annotated_spectrum",
                    overview_spectrum_plot,
                    input$overview_spectrum_symbols
                  )
                }
              },
              ignoreInit = TRUE
            ))

            track_iface_observer(shiny::observeEvent(
              input$overview_spectrum_unmatched,
              {
                shiny::req(ov_spectrum_on_screen())
                restyle_peak_symbols(
                  session,
                  "overview_annotated_spectrum",
                  overview_spectrum_plot,
                  input$overview_spectrum_unmatched,
                  tag = unmatched_trace_tag
                )
              },
              ignoreInit = TRUE
            ))

            track_iface_observer(shiny::observeEvent(
              input$overview_spectrum_legend,
              {
                shiny::req(ov_spectrum_on_screen())
                relayout_spectrum_legend(
                  session,
                  "overview_annotated_spectrum",
                  input$overview_spectrum_legend,
                  cubic = is.null(input$overview_spectrum_kind) ||
                    input$overview_spectrum_kind == "Cubic"
                )
              },
              ignoreInit = TRUE
            ))

            ###### Overview table ----
            # The hits table of the selected protein and compounds
            output$overview_table_view <- DT::renderDataTable(
              {
                shiny::req(
                  input$color_variable,
                  !is.null(input$truncate_names)
                )
                sel <- ov_selection()
                tbl <- hits_view_table(ov_subset(), ov_table_cols(), units)

                # Assign filtered table to reactive for eventual export
                overview_table_view_raw(tbl)

                render_hits_view(
                  tbl,
                  colors = overview_colors(
                    hits_summary,
                    sel$protein,
                    sel$compounds,
                    variable = input$color_variable,
                    key = ckey,
                    trunc = input$truncate_names
                  ),
                  color_variable = color_mode(input$color_variable),
                  truncate_names = input$truncate_names,
                  units = units,
                  # Nothing picked, or a protein declared without compounds
                  empty_message = if (!length(sel$compounds)) {
                    "No compound selected"
                  }
                )
              },
              server = FALSE
            ) |>
              shiny::bindEvent(
                ov_selection(),
                render_trigger(),
                input$truncate_names,
                ov_table_cols()
              )

            ##### Color variable UI ----
            output$color_variable_ui <- shiny::renderUI({
              shiny::req(hits_summary)

              # Every variable the samples here can differ in (one protein
              # at a time); opens on the experiment's main factor
              choices <- c(
                Compound = "Compounds",
                Concentration = if ("Concentration" %in% names(units)) {
                  "Concentration"
                },
                Time = if ("Time" %in% names(units)) "Time",
                Sample = "Samples"
              )
              role <- default_color_role(
                ckey,
                c(
                  "compound",
                  if ("Concentration" %in% names(units)) "concentration",
                  if ("Time" %in% names(units)) "time",
                  "sample"
                )
              )

              bslib::tooltip(
                shiny::selectInput(
                  ns("color_variable"),
                  label = NULL,
                  choices = choices,
                  selected = switch(
                    role,
                    compound = "Compounds",
                    concentration = "Concentration",
                    time = "Time",
                    "Samples"
                  ),
                  width = "140px"
                ),
                "Color mapping",
                placement = "top"
              )
            })
          } else if (iface == "kinetics") {
            #### Render kinact/KI interface ----
            # Reset any prior concentration exclusions so plots match the table
            conversion_vars$modified_results <- NULL

            # The observers of a previous build of this panel (another
            # complex) would keep writing their stale values
            lapply(iface_state$kinetics_observers, function(o) o$destroy())
            iface_state$kinetics_observers <- NULL

            # Kinetics of the complex picked in the Results Menu: its samples,
            # with the hits of its compound only
            kinetics_complex <- result_list$kinetics_complex
            if (!is.null(kinetics_complex)) {
              result_list$hits_summary <- result_list$kinetics_hits
              hits_summary <- hits_summary[
                hits_summary$`Sample ID` %in%
                  result_list$kinetics_hits$Sample &
                  hits_summary$Protein %in% kinetics_complex$protein &
                  hits_summary$`Cmp Name` %in%
                    c(NA, "N/A", kinetics_complex$compound),
                ,
                drop = FALSE
              ]
            }

            # A click on a concentration in the Hits table opens the panel on
            # it, once the panel is built for the complex the click asked for.
            # Left unused for long, it is dropped rather than catch a later
            # visit by surprise.
            kinetics_nav <- iface_state$kinetics_nav
            if (
              !is.null(kinetics_nav) &&
                difftime(Sys.time(), kinetics_nav$time, units = "secs") > 30
            ) {
              kinetics_nav <- iface_state$kinetics_nav <- NULL
            }
            if (
              !is.null(kinetics_nav) &&
                !is.null(kinetics_nav$complex) &&
                !identical(kinetics_nav$complex, kinetics_complex$key)
            ) {
              kinetics_nav <- NULL
            }
            if (!is.null(kinetics_nav)) {
              iface_state$kinetics_nav <- NULL
            }
            iface_state$kinetics_fitted_conc <- character(0)

            # A complex without k_obs has nothing to show but the reason
            if (is.null(result_list$binding_kobs_result)) {
              render_result_interface(
                "kinetics",
                bslib::card(
                  class = "kinetics-unavailable",
                  bslib::card_body(
                    shiny::h4(kinetics_complex$key),
                    shiny::p(paste0(
                      "No binding kinetics for this complex: ",
                      if (is.null(kinetics_complex$reason)) {
                        "no k_obs could be fitted"
                      } else {
                        kinetics_complex$reason
                      },
                      ". The protocol log has the details."
                    ))
                  )
                )
              )
              shinyjs::runjs(paste0(
                'document.getElementById("blocking-overlay").style.display ',
                '= "none";'
              ))
              return(invisible(NULL))
            }

            # Assign formatted hits to reactive variable
            conversion_vars$formatted_hits <- hits_summary

            # Concentration colours, fixed from all concentrations of the
            # result (see color_key()); everything keyed by concentration
            # (curves, tables, cards) reads from here. A function of the theme,
            # as the dark export draws the dark variant.
            conc_levels <- unique(hits_summary[[units[["Concentration"]]]])

            build_concentration_colors <- function(theme = "light") {
              key_colors(ckey, "Concentration", conc_levels, theme = theme)
            }
            iface_state$kinetics_observers <- list()

            # Assign concentrations to reactive variable
            conversion_vars$concentrations <- concentrations <- hits_summary[
              is_complex_row(hits_summary),
            ] |>
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

            all_fitted_conc <- rownames(
              result_list$binding_kobs_result$kobs_result_table
            )
            iface_state$kinetics_fitted_conc <- all_fitted_conc

            # Concentration the panel opens on (see kinetics_nav above)
            nav_conc <- if (
              !is.null(kinetics_nav) && kinetics_nav$conc %in% all_fitted_conc
            ) {
              kinetics_nav$conc
            }

            conc_selected <- rep(TRUE, length(all_fitted_conc))
            names(conc_selected) <- all_fitted_conc
            conversion_vars$select_concentration <- conc_selected
            kobs_selection_pending(NULL)

            ##### Unit view ----
            # The "Unit View" pickers let the user read the results in units
            # other than the ones the samples were declared in. The fitted
            # result objects stay in the declared units; everything rendered
            # below goes through these converted views.
            unit_view <- shiny::reactive({
              make_unit_view(
                units,
                conc_unit = input$conc_unit_results,
                time_unit = input$time_unit_results
              )
            })

            view_units <- shiny::reactive(unit_view()$units)

            view_hits <- shiny::reactive({
              convert_hits_units(hits_summary, units, unit_view())
            })

            # The function is what the export builders call with their theme,
            # the reactive is what the on-screen renders read.
            build_view_colors <- function(theme = "light") {
              conc_colors <- build_concentration_colors(theme)

              stats::setNames(
                conc_colors,
                unname(convert_conc_keys(
                  names(conc_colors),
                  unit_view()
                ))
              )
            }

            view_colors <- shiny::reactive(build_view_colors())

            view_fitted_conc <- shiny::reactive({
              convert_conc_keys(all_fitted_conc, unit_view())
            })

            # Marker symbol per concentration, fixed from all concentrations
            # of the result so every plot (Binding, concentration tabs, fit
            # diagnostics) and every complex shows a concentration with the
            # same shape
            view_symbols <- shiny::reactive({
              sym <- key_symbols(
                ckey,
                unique(c(all_fitted_conc, as.character(ckey$concentrations)))
              )
              stats::setNames(
                sym,
                unname(convert_conc_keys(names(sym), unit_view()))
              )
            })

            # Results currently on display: the recomputed ones when
            # concentrations were excluded, otherwise the original fit
            view_results <- shiny::reactive({
              convert_result_list_units(
                if (is.null(conversion_vars$modified_results)) {
                  result_list
                } else {
                  conversion_vars$modified_results
                },
                unit_view()
              )
            })

            # Per-proteoform kinetics are offered when the complex's protein
            # was declared with several masses, the condition
            # proteoform_kinetics() fits them under
            proteoform_species_binding <- proteoform_binding(
              result_list$hits_summary
            )
            show_proteoforms <- has_proteoforms(proteoform_species_binding)
            if (!show_proteoforms) {
              proteoform_species_binding <- NULL
            }

            # Call function to render kinact/KI results interface
            render_result_interface(
              "kinetics",
              kinact_ki_results_ui(
                ns,
                hits_summary,
                all_fitted_conc,
                units = units,
                proteoforms = show_proteoforms,
                paired_limits = show_proteoforms &&
                  proteoform_paired_has_limits(proteoform_species_binding),
                selected_conc = nav_conc
              )
            )

            ##### Binding tab ----

            ###### Result cards ----
            # The cards read the global fit. kinact and KI are only shown when
            # the data saturates; otherwise their cards say why they are not
            # determinable, and the fit warnings (also listed under Warnings
            # in the Summary Statistics) appear in the cards they concern.
            view_kinact_ki <- shiny::reactive({
              convert_kinact_ki_units(kinact_ki_result(), unit_view())
            })

            # format_scientific() returns a tag list for powers of ten; render it
            # to one HTML string so it can be pasted into labels
            fmt_num <- function(x) as.character(format_scientific(x))

            # A hover tooltip in the same visual language as the Alerts /
            # Warnings list (bold title, dimmer detail line below). Used
            # wherever a short label needs an on-hover explanation, instead
            # of the native title attribute, which browsers render as a
            # plain unstyled system tooltip.
            hover_info <- function(
              trigger,
              detail,
              title = NULL,
              accent = "#8f9bb3"
            ) {
              bslib::tooltip(
                trigger,
                shiny::div(
                  style = "text-align:left; max-width:22rem;",
                  if (!is.null(title)) {
                    shiny::div(
                      style = paste0(
                        "font-weight:700; font-size:0.85rem; color:",
                        accent,
                        ";"
                      ),
                      shiny::HTML(title)
                    )
                  },
                  shiny::div(
                    style = paste0(
                      "font-size:0.78rem; color:#fff; opacity:0.9;",
                      if (!is.null(title)) " margin-top:0.15rem;" else ""
                    ),
                    shiny::HTML(detail)
                  )
                ),
                placement = "auto"
              )
            }

            # Concentrations that carried data but could not be fitted. They
            # are absent from the Per-Concentration Fits table and from the global
            # fit, so without this chip the only trace would be the protocol
            # log — and a missing concentration silently narrows the measured
            # range the saturation diagnostics are read against.
            skipped_conc_warning <- function(skipped, conc_unit) {
              if (is.null(skipped) || nrow(skipped) == 0) {
                return(NULL)
              }
              n <- nrow(skipped)
              detail <- paste(
                vapply(
                  seq_len(n),
                  function(i) {
                    paste0(
                      "<b>",
                      skipped$concentration[i],
                      " ",
                      conc_unit,
                      "</b> — ",
                      skipped$reason[i]
                    )
                  },
                  character(1)
                ),
                collapse = "<br>"
              )
              shiny::div(
                class = "result-warnings",
                hover_info(
                  shiny::div(
                    class = "result-warning",
                    shiny::icon("triangle-exclamation"),
                    sprintf(
                      "%d concentration%s excluded",
                      n,
                      if (n == 1) "" else "s"
                    )
                  ),
                  detail = detail,
                  title = "Not included in the fit",
                  accent = "#ffa53a"
                )
              )
            }

            card_warnings <- function(
              res,
              codes = NULL,
              exclude = NULL,
              class = NULL
            ) {
              warnings <- res$Warnings
              if (!is.null(codes)) {
                warnings <- Filter(function(w) w$code %in% codes, warnings)
              }
              if (!is.null(exclude)) {
                warnings <- Filter(function(w) !w$code %in% exclude, warnings)
              }
              if (length(warnings) == 0) {
                return(NULL)
              }
              shiny::div(
                class = paste(c("result-warnings", class), collapse = " "),
                lapply(warnings, function(w) {
                  hover_info(
                    shiny::div(
                      class = "result-warning",
                      shiny::icon("triangle-exclamation"),
                      shiny::span(shiny::HTML(kinetic_symbols_html(w$title)))
                    ),
                    detail = kinetic_symbols_html(w$detail),
                    accent = "#ffa53a"
                  )
                })
              )
            }

            ci_text <- function(ci) {
              if (any(is.na(ci))) {
                return(NULL)
              }
              paste0("95% CI ", fmt_num(ci[1]), " – ", fmt_num(ci[2]))
            }

            # Compact card layout: the value and unit on the left, the
            # statistics beside it as short items that wrap onto further lines
            # only when the card is narrow, warnings below. The container
            # scrolls instead of clipping when the card is small.
            kinetic_card <- function(...) {
              shiny::div(
                class = "result-card-content kinetic-card-content",
                ...
              )
            }

            kinetic_line <- function(
              html,
              class = "kinetic-detail",
              title = NULL
            ) {
              if (is.null(html) || length(html) == 0) {
                return(NULL)
              }
              el <- shiny::div(class = class, shiny::HTML(html))
              if (is.null(title)) el else hover_info(el, detail = title)
            }

            kinetic_value <- function(value, unit_label) {
              shiny::div(
                class = "kinetic-main",
                shiny::HTML(paste0(
                  value,
                  " <span class='kinetic-unit'>",
                  unit_label,
                  "</span>"
                ))
              )
            }

            # Statistics as inline items (± SE, CI, t/p) that share lines
            kinetic_stats <- function(...) {
              items <- Filter(function(x) length(x) > 0, list(...))
              if (length(items) == 0) {
                return(NULL)
              }
              shiny::div(
                class = "kinetic-stats",
                lapply(items, function(item) {
                  shiny::span(class = "kinetic-stat", shiny::HTML(item))
                })
              )
            }

            # Card content when the kinact/KI fit failed; the k_obs table and
            # plots below still show what was measured
            fit_failed_card <- function() {
              kinetic_card(
                shiny::div(class = "kinetic-main result-nd", "N/A"),
                kinetic_stats(paste0(
                  "The k<sub>inact</sub>/K<sub>I</sub> fit failed - ",
                  "see the protocol log"
                ))
              )
            }

            param_card <- function(res, row, unit_label) {
              params <- res$Params
              if (res$Status != "saturated" || is.na(params[row, 1])) {
                # The warnings say why (saturation not reached / KI not
                # determinable), so the value itself only reads n.d. The card
                # holds nothing else, so the badges can be larger.
                warnings <- card_warnings(
                  res,
                  c("no_saturation", "ki_undetermined"),
                  class = "result-warnings-lg"
                )
                return(kinetic_card(
                  hover_info(
                    shiny::div(class = "kinetic-main result-nd", "n.d."),
                    detail = "not determinable"
                  ),
                  if (is.null(warnings)) {
                    kinetic_stats("not determinable")
                  } else {
                    warnings
                  }
                ))
              }

              # No t / p: they test the constant against zero, which any
              # binding data passes and which reads like a quality measure;
              # the confidence interval answers how well it is determined
              kinetic_card(
                kinetic_value(fmt_num(params[row, 1]), unit_label),
                kinetic_stats(
                  paste0("± ", fmt_num(params[row, 2])),
                  ci_text(res$Params_CI[row, ])
                )
              )
            }

            ###### Calculated kinact value ----
            output$kinact <- shiny::renderUI({
              res <- view_kinact_ki()
              if (is.null(res)) {
                return(fit_failed_card())
              }
              param_card(res, "kinact", paste0(unit_view()$time_unit, "⁻¹"))
            })

            ###### Calculated KI value ----
            output$Ki <- shiny::renderUI({
              res <- view_kinact_ki()
              if (is.null(res)) {
                return(fit_failed_card())
              }
              param_card(res, "KI", unit_view()$conc_unit)
            })

            ###### Calculated kinact/KI value ----
            output$Kinact_Ki <- shiny::renderUI({
              res <- view_kinact_ki()
              if (is.null(res)) {
                return(fit_failed_card())
              }
              ratio <- res$Ratio

              model_label <- if (res$Model == "hyperbolic") {
                "hyperbolic fit"
              } else {
                "linear fit (no saturation)"
              }

              series_text <- if (!is.null(res$Series)) {
                paste0(
                  "<b>Series</b> ",
                  paste(
                    vapply(
                      seq_len(nrow(res$Series)),
                      function(i) {
                        paste(
                          res$Series$series[i],
                          fmt_num(res$Series$ratio[i])
                        )
                      },
                      character(1)
                    ),
                    collapse = " · "
                  )
                )
              }

              kinetic_card(
                kinetic_value(
                  fmt_num(ratio[["Estimate"]]),
                  paste0(
                    unit_view()$time_unit,
                    "⁻¹ ",
                    unit_view()$conc_unit,
                    "⁻¹"
                  )
                ),
                kinetic_stats(
                  if (!is.na(ratio[["Std. Error"]])) {
                    paste0("± ", fmt_num(ratio[["Std. Error"]]))
                  },
                  ci_text(ratio[c("CI 2.5%", "CI 97.5%")])
                ),
                kinetic_line(
                  paste0("<b>Model</b> ", model_label),
                  title = sprintf(
                    "Global fit of %d samples at %d concentrations; curvature p = %s",
                    res$Fit$n_points,
                    res$Fit$n_concentrations,
                    signif(res$Fit$p_curvature, 2)
                  )
                ),
                kinetic_line(
                  series_text,
                  title = "kinact/KI fitted to each replicate series on its own"
                ),
                # Cross-check against the classic two-step fit
                if (!is.null(res$Two_Step)) {
                  outside <- isFALSE(res$Two_Step$within_ci)
                  kinetic_line(
                    paste0(
                      "<b>Two-step (classic)</b> ",
                      fmt_num(res$Two_Step$ratio)
                    ),
                    title = paste0(
                      "For comparison only: the k_obs of each concentration ",
                      "fitted to a hyperbola without weights; kinact/KI is ",
                      "the quotient of the two fitted constants. Shown ",
                      "dash-dotted in the k_obs Curve (enable in the plot ",
                      "settings). ",
                      if (outside) {
                        paste0(
                          "Outside the 95 % CI of the global fit: the least ",
                          "certain k_obs at high concentrations dominate it."
                        )
                      } else {
                        "It agrees with the global fit within its 95 % CI."
                      }
                    )
                  )
                },
                # Saturation is already stated by the model line and shown
                # in the kinact and KI cards
                card_warnings(
                  res,
                  exclude = c("no_saturation", "ki_undetermined")
                ),
                skipped_conc_warning(
                  convert_kobs_result_units(
                    result_list$binding_kobs_result,
                    unit_view()
                  )$skipped,
                  unit_view()$conc_unit
                )
              )
            })

            ###### Kobs result table ----
            output$kobs_result <- DT::renderDT(
              {
                shiny::req(
                  result_list
                )

                view <- unit_view()

                # Get results - always the full fit, exclusions are applied
                # through the checkbox column
                kobs_results <- convert_kobs_result_units(
                  result_list$binding_kobs_result,
                  view
                )$kobs_result_table

                conc_col <- paste0("Conc. [", view$conc_unit, "]")

                # The global fit with all concentrations (the table always
                # shows the full fit; the ticks below drive the refit)
                full_kin <- convert_kinact_ki_units(
                  result_list$kinact_ki_result,
                  view
                )
                conc_num <- as.numeric(rownames(kobs_results))
                pl <- full_kin$Plateaus
                pl_row <- if (is.null(pl)) {
                  rep(NA_integer_, length(conc_num))
                } else {
                  vapply(
                    conc_num,
                    function(c0) {
                      i <- which(abs(pl$conc - c0) <= 1e-9 * max(abs(c0), 1e-300))
                      if (length(i)) i[1] else NA_integer_
                    },
                    integer(1)
                  )
                }
                kobs_global <- ifelse(
                  is.na(pl_row),
                  NA_real_,
                  kobs_model_at(full_kin, conc_num)
                )
                plateau_global <- ifelse(
                  is.na(pl_row),
                  "\u2013",
                  paste0(
                    format(round(pl$plateau[pl_row], 1), nsmall = 1),
                    ifelse(pl$own[pl_row] %in% TRUE, "", " (shared)")
                  )
                )

                kobs_results <- data.frame(
                  concentration = conc_num,
                  kobs = as.numeric(format(kobs_results$kobs, digits = 3)),
                  kobs_se = ifelse(
                    is.na(kobs_results$kobs_se),
                    "N/A",
                    as.character(signif(kobs_results$kobs_se, 3))
                  ),
                  kobs_global = ifelse(
                    is.na(kobs_global),
                    "\u2013",
                    as.character(signif(kobs_global, 3))
                  ),
                  plateau = as.numeric(format(kobs_results$plateau, digits = 3)),
                  plateau_global = plateau_global
                ) |>
                  stats::setNames(c(
                    conc_col,
                    paste0("kobs alone [", view$time_unit, "\u207b\u00b9]"),
                    paste0("SE [", view$time_unit, "\u207b\u00b9]"),
                    paste0("kobs global [", view$time_unit, "\u207b\u00b9]"),
                    "Plateau alone [%]",
                    "Plateau global [%]"
                  ))

                kobs_result_raw(kobs_results)

                # On screen the concentration reads like the plots (8×10⁻⁵ in
                # molar units); the export above keeps the number
                conc_key <- as.character(kobs_results[[conc_col]])
                kobs_results[[conc_col]] <- conc_display(conc_key)

                # Preserve the current exclusions across a re-render (e.g. when
                # the displayed unit changes), including ticks not yet applied
                included <- shiny::isolate(
                  kobs_selection_pending() %||%
                    conversion_vars$select_concentration
                )
                included <- if (is.null(included)) {
                  rep(TRUE, nrow(kobs_results))
                } else {
                  selected <- unname(included[all_fitted_conc])
                  ifelse(is.na(selected), TRUE, selected)
                }

                kobs_results <- kobs_results |>
                  dplyr::mutate(
                    Included = checkboxColumn(
                      nrow(kobs_results),
                      7,
                      value = included
                    )
                  )

                # Kobs present concentrations
                kobs_conc <- kobs_results[[conc_col]]
                # Rows in a light tint of the curves' colours, read in black
                conc_colors <- tint(view_colors())

                DT::datatable(
                  data = kobs_results,
                  rownames = FALSE,
                  selection = "none",
                  escape = FALSE,
                  class = "order-column",
                  options = list(
                    dom = "t",
                    paging = FALSE,
                    scrollX = TRUE,
                    scrollY = TRUE,
                    scrollCollapse = TRUE,
                    fixedHeader = TRUE,
                    stripe = FALSE,
                    # Full card width: the columns are sized once the table
                    # is laid out, not fixed while its tab may still be hidden
                    initComplete = htmlwidgets::JS(
                      "function(settings, json) {",
                      "  var api = this.api();",
                      "  setTimeout(function() { api.columns.adjust(); }, 50);",
                      "}"
                    ),
                    columnDefs = list(
                      list(targets = "_all", className = 'dt-center'),
                      list(targets = -1, className = 'dt-last-col dt-center')
                    )
                  ),
                  editable = FALSE,
                  callback = htmlwidgets::JS(js_code_gen(
                    "kobs_result",
                    which(names(kobs_results) == "Included"),
                    ns = session$ns
                  ))
                ) |>
                  DT::formatStyle(
                    columns = conc_col,
                    target = 'row',
                    backgroundColor = DT::styleEqual(
                      levels = as.character(kobs_conc),
                      values = unname(conc_colors[match(
                        conc_key,
                        names(conc_colors)
                      )])
                    ),
                    color = DT::styleEqual(
                      levels = as.character(kobs_conc),
                      values = unname(get_contrast_color(conc_colors[match(
                        conc_key,
                        names(conc_colors)
                      )]))
                    )
                  ) |>
                  DT::formatStyle(
                    1:6,
                    `border-right` = "solid 1px #0000005c"
                  )
              },
              server = FALSE
            )

            ###### Binding plot ----
            # Mean ± SD or the individual samples (both together clutter the
            # full plot); the export follows the same choice
            binding_points_mode <- shiny::reactive({
              if (identical(input$binding_points, "mean")) {
                "mean"
              } else {
                "samples"
              }
            })

            # Settings of the other view are greyed out and disabled: the
            # log axis is 3D only, data points and single fits 2D only
            shiny::observeEvent(input$binding_view, {
              is_3d <- identical(input$binding_view, "3d")
              shinyjs::toggleClass(
                "surface_log_conc_wrap",
                "kiwi-setting-off",
                condition = !is_3d
              )
              shinyjs::toggleState("surface_log_conc", condition = is_3d)
              shinyjs::toggleClass(
                "binding_2d_settings",
                "kiwi-setting-off",
                condition = is_3d
              )
              shinyjs::toggleState("binding_points", condition = !is_3d)
              shinyjs::toggleState("binding_show_single", condition = !is_3d)
            })

            # 2D: one curve per concentration; 3D: the same global fit as one
            # surface over time and concentration. Screen and export share it.
            build_binding <- function(colors, theme) {
              res <- view_results()
              if (identical(input$binding_view, "3d")) {
                shiny::req(res$kinact_ki_result)
                return(make_binding_surface_plot(
                  res$kinact_ki_result,
                  res$binding_kobs_result,
                  colors,
                  view_units(),
                  theme,
                  log_conc = isTRUE(input$surface_log_conc)
                ))
              }
              make_binding_plot(
                kobs_result = res$binding_kobs_result,
                colors = colors,
                symbol_map = view_symbols(),
                units = view_units(),
                theme = theme,
                points = binding_points_mode(),
                kinact_ki_result = res$kinact_ki_result,
                show_single = isTRUE(input$binding_show_single)
              )
            }

            output$binding_plot <- plotly::renderPlotly({
              shiny::req(result_list)
              build_binding(view_colors(), "light")
            })

            ###### Kobs plot ----
            # Proteoform overlay of the k_obs curve: on by default whenever
            # proteoforms exist (the setting reads NULL until its popover is
            # first opened). proteoform_entries() is defined with the
            # Proteoforms tab below and only exists when show_proteoforms is.
            kobs_overlay <- function() {
              if (show_proteoforms && !isFALSE(input$kobs_show_proteoforms)) {
                proteoform_entries()
              }
            }

            output$kobs_plot <- plotly::renderPlotly({
              shiny::req(result_list)

              make_kobs_plot(
                kinact_ki_result = view_results()$kinact_ki_result,
                colors = view_colors(),
                symbol_map = view_symbols(),
                units = view_units(),
                show_extrapolation = isTRUE(input$kobs_show_extrapolation),
                show_constants = isTRUE(input$kobs_show_constants),
                show_two_step = isTRUE(input$kobs_show_two_step),
                log_conc = isTRUE(input$kobs_log_conc),
                proteoforms = kobs_overlay(),
                proteoform_palette = if (show_proteoforms) proteoform_palette,
                kobs_table = view_results()$binding_kobs_result$kobs_result_table
              )
            })

            setup_plot_dl(
              input,
              output,
              session,
              "binding",
              build_fn = function(theme) {
                shiny::req(result_list)
                build_binding(build_view_colors(theme), theme)
              },
              filename_fn = function() {
                paste0(
                  get_session_prefix(),
                  if (identical(input$binding_view, "3d")) {
                    "_Binding_Surface"
                  } else {
                    "_Binding_Curve"
                  }
                )
              }
            )

            setup_plot_dl(
              input,
              output,
              session,
              "kobs",
              build_fn = function(theme) {
                shiny::req(result_list)
                make_kobs_plot(
                  kinact_ki_result = view_results()$kinact_ki_result,
                  colors = build_view_colors(theme),
                  symbol_map = view_symbols(),
                  units = view_units(),
                  theme = theme,
                  show_extrapolation = isTRUE(input$kobs_show_extrapolation),
                  show_constants = isTRUE(input$kobs_show_constants),
                  show_two_step = isTRUE(input$kobs_show_two_step),
                  log_conc = isTRUE(input$kobs_log_conc),
                  proteoforms = kobs_overlay(),
                  proteoform_palette = if (show_proteoforms) {
                    proteoform_colors(proteoform_species_binding$species, theme)
                  },
                  kobs_table = view_results()$binding_kobs_result$kobs_result_table
                )
              },
              filename_fn = function() {
                paste0(get_session_prefix(), "_kobs_Curve")
              }
            )

            ##### Proteoforms tab ----
            if (show_proteoforms) {
              proteoform_palette <- proteoform_colors(
                proteoform_species_binding$species
              )

              # Every proteoform fitted on its own, over the concentrations the
              # pooled fit currently includes, so both compare like for like
              proteoform_fit <- shiny::reactive({
                select <- conversion_vars$select_concentration
                proteoform_kinetics(
                  result_list$hits_summary,
                  units = c(
                    Concentration = gsub(
                      ".*\\[(.+)\\].*",
                      "\\1",
                      units[["Concentration"]]
                    ),
                    Time = gsub(".*\\[(.+)\\].*", "\\1", units[["Time"]])
                  ),
                  conc_time = units,
                  concentrations_select = if (!is.null(select)) {
                    names(select)[which(select)]
                  }
                )
              })

              # The same in the displayed units
              proteoform_view <- shiny::reactive({
                view <- unit_view()
                lapply(proteoform_fit(), function(k) {
                  k$binding_kobs_result <- convert_kobs_result_units(
                    k$binding_kobs_result,
                    view
                  )
                  k$kinact_ki_result <- convert_kinact_ki_units(
                    k$kinact_ki_result,
                    view
                  )
                  k
                })
              })

              # k_obs table and fit of every proteoform, as the k_obs plots
              # take them; also read by the k_obs curve of the Binding tab
              proteoform_entries <- function() {
                lapply(proteoform_view(), function(k) {
                  list(
                    kobs = k$binding_kobs_result$kobs_result_table,
                    kinact_ki = k$kinact_ki_result,
                    # Share of limit values; above half the species is drawn
                    # hidden (see add_proteoform_kobs_traces())
                    limit_share = if (k$n_samples > 0) {
                      k$n_limit / k$n_samples
                    }
                  )
                })
              }

              build_proteoform_kobs_plot <- function(theme = "light") {
                pooled <- view_results()
                entries <- c(
                  list(
                    Pooled = list(
                      kobs = pooled$binding_kobs_result$kobs_result_table,
                      kinact_ki = pooled$kinact_ki_result
                    )
                  ),
                  proteoform_entries()
                )
                proteoform_kobs_plot(
                  entries,
                  colors = proteoform_colors(
                    proteoform_species_binding$species,
                    theme
                  ),
                  units = view_units(),
                  theme = theme
                )
              }

              build_proteoform_table <- function() {
                proteoform_comparison_table(
                  proteoform_view(),
                  pooled = view_kinact_ki(),
                  binding = proteoform_species_binding,
                  view = unit_view()
                )
              }

              output$proteoform_kobs_plot <- plotly::renderPlotly({
                shiny::req(result_list)
                build_proteoform_kobs_plot()
              })

              output$proteoform_paired_plot <- plotly::renderPlotly({
                proteoform_paired_plot(
                  proteoform_species_binding,
                  colors = proteoform_palette,
                  show_limits = isTRUE(input$paired_show_limits)
                )
              })

              output$proteoform_table <- DT::renderDT({
                tbl <- build_proteoform_table()
                DT::datatable(
                  tbl,
                  escape = FALSE,
                  rownames = FALSE,
                  selection = "none",
                  class = "order-column",
                  # Scrolling, header and cell layout as in the Binding
                  # Analysis table (kobs_result), which shares its styles
                  options = list(
                    dom = "t",
                    paging = FALSE,
                    ordering = FALSE,
                    scrollX = TRUE,
                    scrollY = TRUE,
                    scrollCollapse = TRUE,
                    fixedHeader = TRUE,
                    stripe = FALSE,
                    initComplete = htmlwidgets::JS(
                      "function(settings, json) {",
                      "  var api = this.api();",
                      "  setTimeout(function() { api.columns.adjust(); }, 50);",
                      "}"
                    ),
                    columnDefs = list(
                      list(targets = "_all", className = "dt-center"),
                      list(targets = -1, className = "dt-last-col dt-center")
                    )
                  )
                ) |>
                  # The table has no row colouring of its own and would inherit
                  # the dark card's text colour; light rows as in the Hits
                  # table, the pooled fit set apart as the reference row
                  DT::formatStyle(
                    columns = "Proteoform",
                    target = "row",
                    color = "black",
                    backgroundColor = DT::styleEqual(
                      "Pooled",
                      "#d4d4d4",
                      default = "#f2f2f2"
                    ),
                    fontWeight = DT::styleEqual("Pooled", "bold")
                  )
              })

              setup_plot_dl(
                input,
                output,
                session,
                "proteoform_kobs",
                build_fn = function(theme) {
                  shiny::req(result_list)
                  build_proteoform_kobs_plot(theme)
                },
                filename_fn = function() {
                  paste0(get_session_prefix(), "_kobs_per_Proteoform")
                }
              )

              setup_plot_dl(
                input,
                output,
                session,
                "proteoform_paired",
                build_fn = function(theme) {
                  proteoform_paired_plot(
                    proteoform_species_binding,
                    colors = proteoform_colors(
                      proteoform_species_binding$species,
                      theme
                    ),
                    theme = theme,
                    show_limits = isTRUE(input$paired_show_limits)
                  )
                },
                filename_fn = function() {
                  paste0(get_session_prefix(), "_Paired_Binding")
                }
              )

              setup_table_dl(
                input,
                output,
                session,
                "proteoform_table",
                data_fn = function() {
                  # Plain-text headers and values for the file
                  strip <- function(x) {
                    x <- gsub("<sup>(-?[0-9]+)</sup>", "^\\1", x)
                    x <- gsub("&thinsp;", "", x, fixed = TRUE)
                    trimws(gsub("\\s+", " ", gsub("<[^>]+>", "", x)))
                  }
                  tbl <- build_proteoform_table()
                  names(tbl) <- strip(names(tbl))
                  tbl[] <- lapply(tbl, strip)
                  tbl
                },
                filename_fn = function() {
                  paste0(get_session_prefix(), "_Proteoform_Kinetics")
                }
              )
            }

            ##### Fit diagnostics tab ----
            # Each diagnostics plot is rendered on screen and exported with the
            # same builder, so the export follows the chosen theme
            diagnostics_plots <- list(
              diag_residuals = list(
                build = function(res, colors, units, theme) {
                  make_kinetics_residual_plot(
                    res,
                    colors,
                    units,
                    theme,
                    view_symbols()
                  )
                },
                file = "_Fit_Residuals"
              ),
              diag_plateaus = list(
                build = function(res, colors, units, theme) {
                  make_kinetics_plateau_plot(
                    res,
                    colors,
                    units,
                    theme,
                    view_symbols()
                  )
                },
                file = "_Plateaus"
              ),
              diag_series = list(
                build = function(res, colors, units, theme) {
                  make_kinetics_series_plot(res, units, theme)
                },
                file = "_kinact_Ki_Series"
              ),
              diag_saturation = list(
                build = function(res, colors, units, theme) {
                  make_kinetics_saturation_plot(
                    res,
                    colors,
                    units,
                    theme,
                    view_symbols()
                  )
                },
                file = "_Saturation_Coverage"
              )
            )

            lapply(names(diagnostics_plots), function(id) {
              spec <- diagnostics_plots[[id]]

              output[[paste0(id, "_plot")]] <- plotly::renderPlotly({
                res <- view_results()$kinact_ki_result
                shiny::req(res)
                spec$build(res, view_colors(), view_units(), "light")
              })

              setup_plot_dl(
                input,
                output,
                session,
                id,
                build_fn = function(theme) {
                  res <- view_results()$kinact_ki_result
                  shiny::req(res)
                  spec$build(res, build_view_colors(theme), view_units(), theme)
                },
                filename_fn = function() {
                  paste0(get_session_prefix(), spec$file)
                }
              )
            })

            ##### Concentrations tab ----
            # One concentration at a time, picked in the tab. The picker values
            # are the declared concentration keys, so the selection survives a
            # change of the displayed unit; only the labels follow it.
            conc_tab_conc <- shiny::reactive({
              conc <- input$conc_tab_select
              shiny::req(conc, conc %in% all_fitted_conc)
              conc
            })

            conc_tab_result <- shiny::reactive({
              result_list$binding_kobs_result[[conc_tab_conc()]]
            })

            # Same concentration expressed in the displayed unit
            conc_tab_view_conc <- shiny::reactive({
              unname(view_fitted_conc()[conc_tab_conc()])
            })

            # Samples measured at the selected concentration
            conc_tab_samples <- shiny::reactive({
              conc_sample_ids <- unique(hits_summary$`Sample ID`[
                hits_summary[[units["Concentration"]]] == conc_tab_conc()
              ])
              names(result_list$deconvolution)[
                names(result_list$deconvolution) %in% conc_sample_ids
              ]
            })

            # The first update keeps the concentration the panel was opened
            # on; the input may still hold the pick of the previous panel
            conc_picker_selected <- nav_conc
            conc_picker_observer <- track_iface_observer(shiny::observe({
              view_conc <- view_fitted_conc()
              selected <- if (is.null(conc_picker_selected)) {
                shiny::isolate(input$conc_tab_select)
              } else {
                conc_picker_selected
              }
              conc_picker_selected <<- NULL
              shinyWidgets::updatePickerInput(
                session,
                "conc_tab_select",
                choices = stats::setNames(
                  all_fitted_conc,
                  paste(
                    conc_display(unname(view_conc[all_fitted_conc])),
                    unit_view()$conc_unit
                  )
                ),
                selected = selected
              )
            }))
            iface_state$kinetics_observers <- c(
              iface_state$kinetics_observers,
              list(conc_picker_observer)
            )

            # The selected concentration in the global fit (current
            # exclusions applied): k_obs of the model, its plateau, whether the
            # plateau is its own and how far the curve gets. NULL when the
            # concentration is not part of the global fit.
            conc_tab_global <- shiny::reactive({
              kin <- view_results()$kinact_ki_result
              pl <- kin$Plateaus
              conc <- suppressWarnings(as.numeric(conc_tab_view_conc()))
              if (is.null(pl) || !length(conc) || is.na(conc)) {
                return(NULL)
              }
              i <- which(abs(pl$conc - conc) <= 1e-9 * max(abs(conc), 1e-300))
              if (!length(i)) {
                return(NULL)
              }
              list(
                kobs = kobs_model_at(kin, pl$conc[i[1]]),
                plateau = pl$plateau[i[1]],
                own = isTRUE(pl$own[i[1]]),
                reached = pl$reached[i[1]]
              )
            })

            # Value cards: the global fit as the main value, the concentration
            # fitted on its own beneath it for comparison. Laid out inline: the
            # card body's large font suits a single number, not extra lines.
            conc_card <- function(main, ...) {
              shiny::div(
                style = paste(
                  "width: 100%; text-align: center; padding: 0 10px;",
                  "line-height: 1.25;"
                ),
                shiny::div(
                  style = "font-size: 0.85em; white-space: nowrap;",
                  shiny::HTML(main)
                ),
                lapply(Filter(Negate(is.null), list(...)), function(line) {
                  shiny::div(
                    style = "font-size: 0.42em; opacity: 0.85; margin-top: 6px;",
                    shiny::HTML(line)
                  )
                })
              )
            }
            # Power-of-ten notation with 3 significant digits; a standard
            # error shares the exponent of the value: (9.59 ± 0.69) × 10⁻⁴
            sci_num <- function(x, se = NA) {
              if (!is.finite(x)) {
                return("N/A")
              }
              e <- if (x == 0) 0 else floor(log10(abs(x)))
              m <- signif(x / 10^e, 3)
              if (abs(m) >= 10) {
                m <- m / 10
                e <- e + 1
              }
              fmt <- function(v) format(v, scientific = FALSE, drop0trailing = TRUE)
              mant <- fmt(m)
              if (isTRUE(is.finite(se))) {
                mant <- paste0(mant, " ± ", fmt(signif(se / 10^e, 2)))
                if (e != 0) mant <- paste0("(", mant, ")")
              }
              if (e == 0) {
                return(mant)
              }
              paste0(mant, " × 10<sup>", sub("-", "−", e), "</sup>")
            }

            ###### Calculated kobs value ----
            output$conc_tab_kobs_value <- shiny::renderUI({
              view <- unit_view()
              conc_result <- conc_tab_result()
              kobs <- conc_result$kobs / view$time_factor
              kobs_se <- conc_result$kobs_se / view$time_factor
              alone <- sci_num(kobs, kobs_se)
              global <- conc_tab_global()
              unit <- paste0(" ", view$time_unit, "⁻¹")

              if (is.null(global)) {
                conc_card(paste0(sci_num(kobs), unit), "fitted alone · not in the global fit")
              } else {
                conc_card(
                  paste0(sci_num(global$kobs), unit),
                  "global fit",
                  paste0("fitted alone: ", alone)
                )
              }
            })

            ###### Binding plateau value ----
            output$conc_tab_plateau_value <- shiny::renderUI({
              global <- conc_tab_global()
              alone <- paste0(round(conc_tab_result()$plateau, 1), " %")
              if (is.null(global)) {
                conc_card(alone, "fitted alone · not in the global fit")
              } else {
                conc_card(
                  paste0(round(global$plateau, 1), " %"),
                  if (global$own) "global fit · own plateau" else "global fit · shared plateau",
                  paste0("fitted alone: ", alone)
                )
              }
            })

            ###### Curve completion ----
            # Share of its plateau the curve reaches by the last time point in
            # the global fit; at least plateau_min_reached gives the
            # concentration a plateau of its own
            output$conc_tab_reached_value <- shiny::renderUI({
              global <- conc_tab_global()
              if (is.null(global)) {
                return(conc_card("N/A", "not in the global fit"))
              }
              conc_card(
                paste0(round(100 * global$reached), " %"),
                "of the plateau reached",
                "by the last time point"
              )
            })

            ###### Table view ----
            conc_tbl_raw <- shiny::reactiveVal()

            output$conc_tab_hits <- DT::renderDT({
              view_units_local <- view_units()

              tbl <- view_hits() |>
                dplyr::filter(
                  !!rlang::sym(view_units_local["Concentration"]) ==
                    conc_tab_view_conc()
                )

              # Summarize inputs
              inputs <- list(
                truncate_names = TRUE,
                color_variable = view_units_local["Concentration"],
                binding_bar = input$conc_tab_table_view_binding_bar,
                tot_binding_bar = input$conc_tab_table_view_tot_binding_bar
              )

              # Prefiltering of table
              tbl <- filter_table_view(
                table = tbl,
                colors = view_colors(),
                inputs = inputs,
                units = view_units_local
              ) |>
                dplyr::arrange(
                  as.numeric(!!rlang::sym(view_units_local[["Time"]]))
                )

              # Assign filtered table to reactive for eventual export
              conc_tbl_raw(tbl)

              # Create DT table
              render_table_view(
                table = tbl,
                colors = view_colors(),
                tab = "Concentration",
                inputs = inputs,
                units = view_units_local
              )
            }) |>
              shiny::bindEvent(
                conc_tab_conc(),
                input$conc_tab_table_view_binding_bar,
                input$conc_tab_table_view_tot_binding_bar,
                unit_view(),
                view_colors()
              )

            ###### Concentration table export ----
            setup_table_dl(
              input,
              output,
              session,
              "conc_tab_hits",
              data_fn = function() prepare_hits_export(conc_tbl_raw()),
              filename_fn = function() {
                paste0(
                  get_session_prefix(),
                  "_Table_View_",
                  conc_tab_conc()
                )
              }
            )

            ###### Binding plot ----
            conc_tab_binding_points_mode <- shiny::reactive({
              if (identical(input$conc_tab_binding_points, "mean")) {
                "mean"
              } else {
                "samples"
              }
            })

            output$conc_tab_binding_plot <- plotly::renderPlotly({
              make_binding_plot(
                kobs_result = convert_kobs_result_units(
                  result_list$binding_kobs_result,
                  unit_view()
                ),
                filter_conc = conc_tab_view_conc(),
                colors = view_colors(),
                symbol_map = view_symbols(),
                units = view_units(),
                points = conc_tab_binding_points_mode(),
                kinact_ki_result = view_results()$kinact_ki_result,
                show_single = TRUE
              )
            })

            setup_plot_dl(
              input,
              output,
              session,
              "conc_tab_binding",
              build_fn = function(theme) {
                make_binding_plot(
                  kobs_result = convert_kobs_result_units(
                    result_list$binding_kobs_result,
                    unit_view()
                  ),
                  filter_conc = conc_tab_view_conc(),
                  colors = build_view_colors(theme),
                  symbol_map = view_symbols(),
                  units = view_units(),
                  theme = theme,
                  points = conc_tab_binding_points_mode(),
                  kinact_ki_result = view_results()$kinact_ki_result,
                  show_single = TRUE
                )
              },
              filename_fn = function() {
                paste0(get_session_prefix(), "_Binding_Curve")
              }
            )

            ###### Multiple spectra plot ----
            conc_tab_cubic <- function() {
              is.null(input$conc_tab_kind) || input$conc_tab_kind == "Cubic"
            }

            output$conc_tab_spectra <- plotly::renderPlotly({
              multiple_spectra(
                results_list = result_list,
                samples = conc_tab_samples(),
                cubic = conc_tab_cubic(),
                time = TRUE,
                hits_summary = view_hits(),
                units = view_units(),
                time_factor = unit_view()$time_factor,
                key = ckey
              )
            }) |>
              shiny::bindEvent(
                conc_tab_conc(),
                input$conc_tab_kind,
                unit_view()
              )

            setup_plot_dl(
              input,
              output,
              session,
              "conc_tab_spectra",
              build_fn = function(theme) {
                multiple_spectra(
                  results_list = result_list,
                  samples = conc_tab_samples(),
                  cubic = conc_tab_cubic(),
                  time = TRUE,
                  hits_summary = view_hits(),
                  units = view_units(),
                  time_factor = unit_view()$time_factor,
                  key = ckey,
                  theme = theme
                )
              },
              filename_fn = function() {
                paste0(get_session_prefix(), "_Mass_Spectra")
              }
            )
          } else if (iface == "summary") {
            #### Render Summary interface ----
            render_result_interface(
              "summary",
              summary_results_ui(
                ns,
                batch_control = "Well" %in%
                  names(hits_summary) &&
                  !all(is.na(hits_summary$Well)) &&
                  !all(
                    trimws(as.character(hits_summary$Well)) %in%
                      c("", "NA", "N/A")
                  )
              )
            )

            output$summary_protocol <- shiny::renderUI({
              snapshot <- conversion_sidebar_vars$console_log_snapshot()
              shiny::req(!is.null(snapshot))
              shiny::tags$pre(
                id = ns("protocol_log"),
                shiny::HTML(snapshot)
              )
            })

            track_iface_observer(shiny::observeEvent(
              conversion_sidebar_vars$console_log_snapshot(),
              {
                shiny::req(conversion_sidebar_vars$console_log_snapshot())
                shinyjs::runjs(sprintf(
                  "
                (function() {
                  var el = document.getElementById('%s');
                  var t  = document.getElementById('%s');
                  var b  = document.getElementById('%s');
                  if (!el || !t || !b) return;
                  function update() {
                    t.disabled = el.scrollTop <= 10;
                    b.disabled = (el.scrollHeight - el.scrollTop - el.clientHeight) <= 10;
                  }
                  el.onscroll = update;
                  t.onclick = function() { el.scrollTo({ top: 0, behavior: 'smooth' }); };
                  b.onclick = function() { el.scrollTo({ top: el.scrollHeight, behavior: 'smooth' }); };
                  update();
                })();
              ",
                  ns("protocol_log"),
                  ns("protocol_scroll_top"),
                  ns("protocol_scroll_bot")
                ))
              },
              ignoreNULL = TRUE
            ))

            output$stats_histogram <- plotly::renderPlotly({
              rl <- conversion_sidebar_vars$result_list()
              shiny::req(rl, rl$hits_summary)
              stats_histogram(
                rl$hits_summary,
                theme = "light",
                show = input$stats_show_metric %||% "Correct"
              )
            })

            output$stats_boxplot <- plotly::renderPlotly({
              rl <- conversion_sidebar_vars$result_list()
              shiny::req(rl, rl$hits_summary)

              stats_boxplot(
                rl$hits_summary,
                theme = "light",
                show_points = isTRUE(input$stats_boxplot_show_points),
                show = input$stats_show_metric %||% "Correct",
                fit_box = isTRUE(input$stats_boxplot_fit_box)
              )
            })

            output$stats_scatter <- plotly::renderPlotly({
              rl <- conversion_sidebar_vars$result_list()
              shiny::req(rl, rl$hits_summary)
              fs <- isTRUE(input$stats_scatter_full_scale)
              grp <- if (is.null(input$stats_scatter_groupby)) {
                "Protein"
              } else {
                input$stats_scatter_groupby
              }
              stats_scatter(
                rl$hits_summary,
                full_scale = fs,
                group_by = grp,
                key = conversion_vars$color_key,
                theme = "light",
                show = input$stats_show_metric %||% "Correct"
              )
            })

            output$stats_violin <- plotly::renderPlotly({
              rl <- conversion_sidebar_vars$result_list()
              shiny::req(rl, rl$hits_summary)
              grp <- if (is.null(input$stats_violin_groupby)) {
                "Protein"
              } else {
                input$stats_violin_groupby
              }
              stats_violin(
                rl$hits_summary,
                group_by = grp,
                theme = "light",
                key = conversion_vars$color_key,
                inner = if (is.null(input$stats_violin_inner)) {
                  "Box"
                } else {
                  input$stats_violin_inner
                },
                show = input$stats_show_metric %||% "Correct"
              )
            })

            batch_heatmap_var_map <- list(
              list(
                value = "Total % Binding",
                label = "Total % Binding",
                id = "batch_heatmap_total_pct",
                is_pct = TRUE,
                is_combined = FALSE
              ),
              list(
                value = NULL,
                values = c("% Correct", "% Unmatched"),
                var_labels = c(
                  "Correct" = "% Correct",
                  "Unmatched" = "% Unmatched"
                ),
                label = "Hit Rate",
                id = "batch_heatmap_pct_cmp",
                is_pct = TRUE,
                is_combined = TRUE
              ),
              list(
                value = "Compound",
                label = "Compound",
                id = "batch_heatmap_compound",
                is_pct = FALSE,
                is_combined = FALSE
              ),
              list(
                value = "Protein",
                label = "Protein",
                id = "batch_heatmap_protein",
                is_pct = FALSE,
                is_combined = FALSE
              ),
              list(
                value = "Concentration",
                label = "Concentration",
                id = "batch_heatmap_concentration",
                is_pct = FALSE,
                is_combined = FALSE
              ),
              list(
                value = "Time",
                label = "Time",
                id = "batch_heatmap_time",
                is_pct = FALSE,
                is_combined = FALSE
              )
            )

            output$batch_heatmap_cards <- shiny::renderUI({
              rl <- conversion_sidebar_vars$result_list()
              shiny::req(rl, rl$hits_summary)
              hs <- rl$hits_summary
              available <- c(
                "Total % Binding",
                "% Correct",
                "% Unmatched",
                "Compound",
                "Protein"
              )
              # Concentration and time columns carry their unit
              if (any(startsWith(names(hs), "Concentration"))) {
                available <- c(available, "Concentration")
              }
              if (any(startsWith(names(hs), "Time"))) {
                available <- c(available, "Time")
              }

              cards <- lapply(batch_heatmap_var_map, function(vm) {
                is_avail <- if (isTRUE(vm$is_combined)) {
                  any(vm$values %in% available)
                } else {
                  vm$value %in% available
                }
                if (!is_avail) {
                  return(NULL)
                }

                # Only the variable cards without a setting go without the
                # settings menu
                has_settings <- isTRUE(vm$is_combined) || isTRUE(vm$is_pct)
                settings_content <- shiny::div(
                  if (isTRUE(vm$is_combined)) {
                    shinyWidgets::radioGroupButtons(
                      ns(paste0(vm$id, "_var_select")),
                      label = NULL,
                      choices = vm$var_labels[vm$var_labels %in% available],
                      selected = vm$values[1],
                      size = "sm"
                    )
                  },
                  if (isTRUE(vm$is_pct)) {
                    shinyWidgets::materialSwitch(
                      ns(paste0(vm$id, "_pct_scale_100")),
                      label = "Full Scale (0–100%)",
                      value = FALSE,
                      right = TRUE
                    )
                  },
                  style = "margin-right:20px;"
                )
                settings <- if (has_settings) {
                  card_settings_popover(settings_content)
                }
                shiny::div(
                  class = "card-custom",
                  bslib::card(
                    full_screen = TRUE,
                    bslib::card_header(
                      class = "bg-dark help-header d-flex justify-content-between",
                      vm$label,
                      shiny::div(
                        class = "box-header-settings-help",
                        settings,
                        plot_dl_popover(ns, vm$id),
                        bslib::tooltip(
                          shiny::div(
                            class = "tooltip-bttn",
                            shiny::actionButton(
                              ns(paste0(vm$id, "_help")),
                              NULL,
                              icon = shiny::icon("circle-question")
                            )
                          ),
                          "Help",
                          placement = "top"
                        )
                      )
                    ),
                    bslib::card_body(shinycssloaders::withSpinner(
                      plotly::plotlyOutput(ns(vm$id), height = "100%"),
                      type = 1,
                      color = "#7777f9"
                    ))
                  )
                )
              })
              cards <- Filter(Negate(is.null), cards)
              # Six cards go three by two, four two by two (main.scss)
              shiny::div(
                class = paste0(
                  "batch-heatmap-grid batch-heatmap-grid-",
                  length(cards)
                ),
                do.call(shiny::tagList, cards)
              )
            })

            lapply(batch_heatmap_var_map, function(vm) {
              local({
                v <- vm$value
                combined_values <- vm$values
                plot_id <- vm$id
                pct <- isTRUE(vm$is_pct)
                is_combined <- isTRUE(vm$is_combined)

                build_heatmap <- function(theme) {
                  rl <- conversion_sidebar_vars$result_list()
                  shiny::req(rl, rl$hits_summary)
                  hs <- rl$hits_summary
                  active_v <- if (is_combined) {
                    radio_val <- input[[paste0(plot_id, "_var_select")]]
                    if (is.null(radio_val)) combined_values[1] else radio_val
                  } else {
                    v
                  }
                  if (active_v %in% c("Concentration", "Time")) {
                    shiny::req(any(startsWith(names(hs), active_v)))
                  }
                  sm <- if (
                    pct && isTRUE(input[[paste0(plot_id, "_pct_scale_100")]])
                  ) {
                    "min100"
                  } else {
                    "minmax"
                  }
                  batch_plate_heatmap(
                    hs,
                    variable = active_v,
                    key = conversion_vars$color_key,
                    scale_mode = sm,
                    theme = theme
                  )
                }

                output[[plot_id]] <- plotly::renderPlotly(build_heatmap("light"))

                setup_plot_dl(
                  input,
                  output,
                  session,
                  plot_id,
                  build_fn = build_heatmap,
                  filename_fn = function() {
                    active_v <- if (is_combined) {
                      radio_val <- input[[paste0(plot_id, "_var_select")]]
                      if (is.null(radio_val)) combined_values[1] else radio_val
                    } else {
                      v
                    }
                    paste0(
                      get_session_prefix(),
                      "_Batch_Heatmap_",
                      gsub(
                        "_+",
                        "_",
                        gsub("[^A-Za-z0-9]+", "_", trimws(active_v))
                      )
                    )
                  }
                )
              })
            })

            # Start polling JS to bind click handlers to rendered plotly tiles
            session$sendCustomMessage(
              "kiwiMS_attachHeatmapClicks",
              list(inputId = session$ns("heatmap_well_click"))
            )
            session$sendCustomMessage(
              "kiwiMS_attachScatterClicks",
              list(
                inputId = session$ns("stats_scatter_click"),
                plotElId = session$ns("stats_scatter")
              )
            )
            session$sendCustomMessage(
              "kiwiMS_attachScatterClicks",
              list(
                inputId = session$ns("stats_boxplot_click"),
                plotElId = session$ns("stats_boxplot")
              )
            )
            session$sendCustomMessage(
              "kiwiMS_attachScatterClicks",
              list(
                inputId = session$ns("stats_violin_click"),
                plotElId = session$ns("stats_violin")
              )
            )

            # Well click → navigate to Relative Binding / Sample View.
            # ignoreInit = TRUE prevents the newly-created observer from firing
            # with a stale heatmap_well_click value left over from a previous
            # click (which would immediately re-navigate every time results_observer
            # re-runs with analysis_select == 1).
            track_iface_observer(shiny::observeEvent(
              input$heatmap_well_click,
              {
                click <- input$heatmap_well_click
                shiny::req(!is.null(click))
                rl <- conversion_sidebar_vars$result_list()
                shiny::req(rl, rl$hits_summary)
                hs <- rl$hits_summary
                # result_list()$hits_summary is raw (untransformed): column is
                # "Sample", but after transform_hits it becomes "Sample ID".
                # Picker choices come from the transformed version; values are identical.
                sample_col <- if ("Sample ID" %in% names(hs)) {
                  "Sample ID"
                } else {
                  "Sample"
                }
                shiny::req("Well" %in% names(hs), sample_col %in% names(hs))
                # The heatmaps place the rows by position, 1 = A
                well_id <- paste0(
                  LETTERS[as.integer(click$y)],
                  as.integer(click$x)
                )
                norm_wells <- gsub(
                  "^([A-Z]+)0*(\\d+)$",
                  "\\1\\2",
                  toupper(as.character(hs[["Well"]]))
                )
                idx <- match(well_id, norm_wells)
                shiny::req(!is.na(idx))
                sample_id <- hs[[sample_col]][idx]
                shiny::req(
                  !is.na(sample_id),
                  nzchar(trimws(as.character(sample_id)))
                )
                heatmap_pending_sample(as.character(sample_id))
                shinyjs::runjs(
                  "document.querySelector('#app-conversion_sidebar-analysis_select input[value=\"2\"]').click();"
                )
              },
              ignoreNULL = TRUE,
              ignoreInit = TRUE
            ))

            # Apply pending sample once Relative Binding interface is active
            # and the picker has been rendered (req on picker avoids racing the renderUI)
            track_iface_observer(shiny::observe({
              pending <- heatmap_pending_sample()
              shiny::req(!is.null(pending))
              shiny::req(conversion_sidebar_vars$analysis_select() == 2)
              shiny::req(!is.null(input$conversion_sample_picker))
              shinyWidgets::updatePickerInput(
                session,
                "conversion_sample_picker",
                selected = pending
              )
              set_selected_tab("Sample View", session)
              heatmap_pending_sample(NULL)
            }))

            # Scatter click → navigate to Relative Binding / Sample View
            track_iface_observer(shiny::observeEvent(
              input$stats_scatter_click,
              {
                click <- input$stats_scatter_click
                shiny::req(
                  !is.null(click),
                  !is.null(click$sample),
                  nzchar(trimws(as.character(click$sample)))
                )
                stats_scatter_pending_sample(as.character(click$sample))
                shinyjs::runjs(
                  "document.querySelector('#app-conversion_sidebar-analysis_select input[value=\"2\"]').click();"
                )
              },
              ignoreNULL = TRUE,
              ignoreInit = TRUE
            ))

            track_iface_observer(shiny::observe({
              pending <- stats_scatter_pending_sample()
              shiny::req(!is.null(pending))
              shiny::req(conversion_sidebar_vars$analysis_select() == 2)
              shiny::req(!is.null(input$conversion_sample_picker))
              shinyWidgets::updatePickerInput(
                session,
                "conversion_sample_picker",
                selected = pending
              )
              set_selected_tab("Sample View", session)
              stats_scatter_pending_sample(NULL)
            }))

            # Boxplot point click → navigate to Relative Binding / Sample View
            track_iface_observer(shiny::observeEvent(
              input$stats_boxplot_click,
              {
                click <- input$stats_boxplot_click
                shiny::req(
                  !is.null(click),
                  !is.null(click$sample),
                  nzchar(trimws(as.character(click$sample)))
                )
                stats_boxplot_pending_sample(as.character(click$sample))
                shinyjs::runjs(
                  "document.querySelector('#app-conversion_sidebar-analysis_select input[value=\"2\"]').click();"
                )
              },
              ignoreNULL = TRUE,
              ignoreInit = TRUE
            ))

            track_iface_observer(shiny::observe({
              pending <- stats_boxplot_pending_sample()
              shiny::req(!is.null(pending))
              shiny::req(conversion_sidebar_vars$analysis_select() == 2)
              shiny::req(!is.null(input$conversion_sample_picker))
              shinyWidgets::updatePickerInput(
                session,
                "conversion_sample_picker",
                selected = pending
              )
              set_selected_tab("Sample View", session)
              stats_boxplot_pending_sample(NULL)
            }))

            # Violin point click → navigate to Relative Binding / Sample View
            track_iface_observer(shiny::observeEvent(
              input$stats_violin_click,
              {
                click <- input$stats_violin_click
                shiny::req(
                  !is.null(click),
                  !is.null(click$sample),
                  nzchar(trimws(as.character(click$sample)))
                )
                stats_violin_pending_sample(as.character(click$sample))
                shinyjs::runjs(
                  "document.querySelector('#app-conversion_sidebar-analysis_select input[value=\"2\"]').click();"
                )
              },
              ignoreNULL = TRUE,
              ignoreInit = TRUE
            ))

            track_iface_observer(shiny::observe({
              pending <- stats_violin_pending_sample()
              shiny::req(!is.null(pending))
              shiny::req(conversion_sidebar_vars$analysis_select() == 2)
              shiny::req(!is.null(input$conversion_sample_picker))
              shinyWidgets::updatePickerInput(
                session,
                "conversion_sample_picker",
                selected = pending
              )
              set_selected_tab("Sample View", session)
              stats_violin_pending_sample(NULL)
            }))

            output$pstat_n_samples <- shiny::renderUI({
              shiny::div(
                shiny::div(
                  class = "protocol-stat-value",
                  length(unique(hits_summary$`Sample ID`))
                ),
                shiny::div(
                  class = "protocol-stat-sub",
                  "samples subjected to analysis"
                )
              )
            })

            output$pstat_n_hits <- shiny::renderUI({
              rl <- conversion_sidebar_vars$result_list()
              shiny::req(rl, rl$hits_summary)

              n <- sum(is_complex_row(rl$hits_summary))
              shiny::div(
                shiny::div(class = "protocol-stat-value", n),
                shiny::div(
                  class = "protocol-stat-sub",
                  "binding compounds observed"
                )
              )
            })

            # Protocol tab cards - use captured hits_summary, no filtering
            output$pstat_correct <- shiny::renderUI({
              hs <- dplyr::distinct(hits_summary, `Sample ID`, .keep_all = TRUE)
              vals <- suppressWarnings(as.numeric(hs[["Correct [%]"]]))
              m <- mean(vals, na.rm = TRUE)
              s <- stats::sd(vals, na.rm = TRUE)
              cls <- if (!is.na(m) && m < 10) {
                "protocol-stat-value protocol-stat-err"
              } else if (!is.na(m) && m < 50) {
                "protocol-stat-value protocol-stat-warn"
              } else {
                "protocol-stat-value"
              }
              shiny::div(
                shiny::div(class = cls, sprintf("%.2f%%", m)),
                shiny::div(
                  class = "protocol-stat-sub",
                  sprintf("+/- %.2f%% SD", s)
                )
              )
            })

            output$pstat_unmatched <- shiny::renderUI({
              hs <- dplyr::distinct(hits_summary, `Sample ID`, .keep_all = TRUE)
              vals <- suppressWarnings(as.numeric(hs[["Unmatched [%]"]]))
              m <- mean(vals, na.rm = TRUE)
              s <- stats::sd(vals, na.rm = TRUE)
              cls <- if (!is.na(m) && m > 90) {
                "protocol-stat-value protocol-stat-err"
              } else if (!is.na(m) && m > 50) {
                "protocol-stat-value protocol-stat-warn"
              } else {
                "protocol-stat-value"
              }
              shiny::div(
                shiny::div(class = cls, sprintf("%.2f%%", m)),
                shiny::div(
                  class = "protocol-stat-sub",
                  sprintf("+/- %.2f%% SD", s)
                )
              )
            })

            # Statistics tab cards
            output$pstat_correct_stat <- shiny::renderUI({
              hs <- dplyr::distinct(hits_summary, `Sample ID`, .keep_all = TRUE)
              vals <- suppressWarnings(as.numeric(hs[["Correct [%]"]]))
              m <- mean(vals, na.rm = TRUE)
              s <- stats::sd(vals, na.rm = TRUE)
              cls <- if (!is.na(m) && m < 10) {
                "protocol-stat-value protocol-stat-err"
              } else if (!is.na(m) && m < 50) {
                "protocol-stat-value protocol-stat-warn"
              } else {
                "protocol-stat-value"
              }
              shiny::div(
                shiny::div(class = cls, sprintf("%.2f%%", m)),
                shiny::div(
                  class = "protocol-stat-sub",
                  sprintf("+/- %.2f%% SD", s)
                )
              )
            })

            output$pstat_unmatched_stat <- shiny::renderUI({
              hs <- dplyr::distinct(hits_summary, `Sample ID`, .keep_all = TRUE)
              vals <- suppressWarnings(as.numeric(hs[["Unmatched [%]"]]))
              m <- mean(vals, na.rm = TRUE)
              s <- stats::sd(vals, na.rm = TRUE)
              cls <- if (!is.na(m) && m > 90) {
                "protocol-stat-value protocol-stat-err"
              } else if (!is.na(m) && m > 50) {
                "protocol-stat-value protocol-stat-warn"
              } else {
                "protocol-stat-value"
              }
              shiny::div(
                shiny::div(class = cls, sprintf("%.2f%%", m)),
                shiny::div(
                  class = "protocol-stat-sub",
                  sprintf("+/- %.2f%% SD", s)
                )
              )
            })

            # Shared helpers for Alerts / Warnings cards
            clean_log_msg <- function(x) {
              x <- gsub("<[^>]+>", "", x)
              x <- gsub("&amp;", "&", x, fixed = TRUE)
              x <- gsub("&lt;", "<", x, fixed = TRUE)
              x <- gsub("&gt;", ">", x, fixed = TRUE)
              x <- sub("^.*?⚠\\s*", "", x)
              x <- trimws(x)
              x <- sub("^Hit duplicates at .+$", "Hit duplicates", x)
              # One entry per resolution, counted over peaks and samples
              x <- sub(
                "^Ambiguous assignment at .+? Da \\((.+)\\)$",
                "Ambiguous assignment: a peak fits several readings, \\1",
                x
              )
              x <- sub(
                "^(\\d+) sample\\(s\\) ignored due to missing hits$",
                "Samples ignored due to missing hits ×\\1",
                x
              )
              x
            }

            # The card only shows the count; the messages are listed in a
            # tooltip, each with its title (the text before the first ": ")
            # in bold, the explanation below it and how often it occurred.
            # Styled inline: bslib's tooltip web component reparents this
            # element into a Bootstrap tooltip popup, and stylesheet classes
            # on it are unreliable there, so every rule that matters for
            # legibility is written directly onto the tags.
            pstat_accent <- function(item_cls) {
              if (identical(item_cls, "pstat-msg-err")) "#ff6b66" else "#ffa53a"
            }

            make_pstat_tooltip <- function(msgs, item_cls) {
              tbl <- sort(table(msgs), decreasing = TRUE)
              accent <- pstat_accent(item_cls)
              items <- lapply(seq_along(tbl), function(i) {
                txt <- names(tbl)[i]
                cnt <- as.integer(tbl[[i]])
                # Counts folded into the message ("... ×3")
                folded <- regmatches(txt, regexpr("\\s*×\\d+$", txt))
                if (length(folded) > 0) {
                  cnt <- cnt * as.integer(sub("^\\s*×", "", folded))
                  txt <- sub("\\s*×\\d+$", "", txt)
                }
                split_at <- regexpr(": ", txt, fixed = TRUE)
                title <- if (split_at > 0) substr(txt, 1, split_at - 1) else txt
                detail <- if (split_at > 0) {
                  substr(txt, split_at + 2, nchar(txt))
                }
                shiny::div(
                  style = paste(
                    "text-align:left; padding:0.35rem 0 0.35rem 0.6rem;",
                    "border-left:3px solid",
                    paste0(accent, ";"),
                    "line-height:1.35;"
                  ),
                  shiny::div(
                    style = paste(
                      "display:flex; justify-content:space-between;",
                      "align-items:baseline; gap:0.75rem;",
                      "font-weight:700; font-size:0.85rem; color:",
                      paste0(accent, ";")
                    ),
                    shiny::span(shiny::HTML(kinetic_symbols_html(title))),
                    if (cnt > 1) {
                      shiny::span(
                        style = paste(
                          "flex-shrink:0; font-weight:400; opacity:0.8;",
                          "font-variant-numeric:tabular-nums;"
                        ),
                        paste0("×", cnt)
                      )
                    }
                  ),
                  if (!is.null(detail)) {
                    shiny::div(
                      style = paste(
                        "font-size:0.78rem; color:#fff; opacity:0.9;",
                        "margin-top:0.15rem;"
                      ),
                      shiny::HTML(kinetic_symbols_html(detail))
                    )
                  }
                )
              })
              shiny::div(
                style = paste(
                  "display:flex; flex-direction:column; gap:0.5rem;",
                  "max-height:50vh; max-width:100%;",
                  "overflow-y:auto; text-align:left;"
                ),
                items
              )
            }

            pstat_count_card <- function(n, msgs, item_cls) {
              cls <- if (n > 0) {
                "protocol-stat-value protocol-stat-warn"
              } else {
                "protocol-stat-value"
              }
              if (n == 0) {
                return(shiny::div(
                  shiny::div(class = cls, n),
                  shiny::div(class = "protocol-stat-sub", "No alerts")
                ))
              }
              bslib::tooltip(
                shiny::div(
                  class = "pstat-count-trigger",
                  shiny::div(class = cls, n),
                  shiny::div(
                    class = "protocol-stat-sub",
                    shiny::icon("circle-info"),
                    "Hover for details"
                  )
                ),
                make_pstat_tooltip(msgs, item_cls),
                placement = "auto"
              )
            }

            parse_log_lines <- function(snapshot) {
              n_err <- 0L
              n_warn <- 0L
              err_msgs <- character(0)
              warn_msgs <- character(0)
              lines <- character(0)
              if (!is.null(snapshot) && nzchar(snapshot)) {
                lines <- strsplit(snapshot, "<br>", fixed = TRUE)[[1]]
                err_idx <- grep("class=\"log-err\"", lines, fixed = TRUE)
                warn_idx <- grep("class=\"log-warn\"", lines, fixed = TRUE)
                excl_idx <- grep("Unmatched:|Correct:", lines)
                warn_idx <- setdiff(warn_idx, excl_idx)
                n_err <- length(err_idx)
                n_warn <- length(warn_idx)
                if (n_err > 0) {
                  err_msgs <- clean_log_msg(lines[err_idx])
                }
                if (n_warn > 0) {
                  omit_label <- "Omitted concentrations after filtering"
                  warn_msgs <- vapply(
                    warn_idx,
                    function(wi) {
                      msg <- clean_log_msg(lines[wi])
                      if (msg == omit_label) {
                        cnt <- 0L
                        j <- wi + 1L
                        while (j <= length(lines)) {
                          plain <- gsub("<[^>]+>", "", lines[j])
                          if (grepl("[├└]─\\s*\\S", plain)) {
                            cnt <- cnt + 1L
                          } else {
                            break
                          }
                          j <- j + 1L
                        }
                        if (cnt > 0L) {
                          sprintf("%s ×%d", omit_label, cnt)
                        } else {
                          msg
                        }
                      } else {
                        msg
                      }
                    },
                    character(1)
                  )
                }
              }
              # Sum true event counts: messages with embedded ×N contribute N,
              # plain messages contribute 1 each.
              n_warn_total <- if (length(warn_msgs) == 0L) {
                0L
              } else {
                sum(vapply(
                  warn_msgs,
                  function(msg) {
                    m <- regmatches(msg, regexpr("×(\\d+)$", msg))
                    if (length(m) > 0L && nzchar(m)) {
                      as.integer(sub("^×", "", m))
                    } else {
                      1L
                    }
                  },
                  integer(1)
                ))
              }
              list(
                n_err = n_err,
                n_warn = n_warn,
                n_warn_total = n_warn_total,
                err_msgs = err_msgs,
                warn_msgs = warn_msgs
              )
            }

            output$pstat_alerts <- shiny::renderUI({
              shiny::req(conversion_sidebar_vars$console_log_snapshot())
              parsed <- parse_log_lines(conversion_sidebar_vars$console_log_snapshot())
              pstat_count_card(parsed$n_err, parsed$err_msgs, "pstat-msg-err")
            })

            output$pstat_warnings <- shiny::renderUI({
              shiny::req(conversion_sidebar_vars$console_log_snapshot())
              parsed <- parse_log_lines(conversion_sidebar_vars$console_log_snapshot())
              pstat_count_card(
                parsed$n_warn_total,
                parsed$warn_msgs,
                "pstat-msg-warn"
              )
            })

            output$pstat_peak_tol <- shiny::renderUI({
              val <- conversion_sidebar_vars$peak_tolerance()
              shiny::div(
                shiny::div(
                  class = "protocol-stat-value",
                  sprintf("%g Da", if (is.null(val)) 3 else val)
                ),
                shiny::div(
                  class = "protocol-stat-sub",
                  "maximum acceptable mass deviation"
                )
              )
            })

            output$pstat_max_stoich <- shiny::renderUI({
              val <- conversion_sidebar_vars$max_multiples()
              shiny::div(
                shiny::div(
                  class = "protocol-stat-value",
                  if (is.null(val)) 5 else val
                ),
                shiny::div(
                  class = "protocol-stat-sub",
                  "maximum no. of compounds bound"
                )
              )
            })

            output$pstat_n_proteins <- shiny::renderUI({
              detected <- length(unique(stats::na.omit(hits_summary[[
                "Protein"
              ]])))
              declared <- sum(
                !is.na(protein_table_data()$Protein) &
                  nzchar(trimws(as.character(protein_table_data()$Protein)))
              )
              cls <- if (detected < declared) {
                "protocol-stat-value protocol-stat-warn"
              } else {
                "protocol-stat-value"
              }
              shiny::div(
                shiny::div(class = cls, detected),
                shiny::div(
                  class = "protocol-stat-sub",
                  sprintf("of %d declared", declared)
                )
              )
            })

            output$pstat_n_compounds <- shiny::renderUI({
              # Counted from the adducts: a declared compound that was not
              # found still names its rows without one
              cmp_vals <- as.character(hits_summary$`Cmp Name`[
                is_complex_row(hits_summary)
              ])
              detected <- length(unique(cmp_vals[nzchar(trimws(cmp_vals))]))
              declared <- sum(
                !is.na(compound_table_data()$Compound) &
                  nzchar(trimws(as.character(compound_table_data()$Compound)))
              )
              cls <- if (detected < declared) {
                "protocol-stat-value protocol-stat-warn"
              } else {
                "protocol-stat-value"
              }
              shiny::div(
                shiny::div(class = cls, detected),
                shiny::div(
                  class = "protocol-stat-sub",
                  sprintf("of %d declared", declared)
                )
              )
            })

            track_iface_observer(shiny::observe({
              rl <- conversion_sidebar_vars$result_list()
              shiny::req(rl, rl$hits_summary)
              hs <- rl$hits_summary
              conc_col <- grep("^Concentration", names(hs), value = TRUE)[1]
              time_col <- grep("^Time", names(hs), value = TRUE)[1]
              # Every variable the samples can differ in; opens on the
              # experiment's main factor, as every coloured view does
              choices <- c(
                Protein = "Protein",
                Compound = "Compound",
                Concentration = if (!is.na(conc_col)) conc_col,
                Time = if (!is.na(time_col)) time_col
              )
              default_grp <- switch(
                default_color_role(
                  conversion_vars$color_key %||% color_key(hs),
                  c(
                    "protein",
                    "compound",
                    if (!is.na(conc_col)) "concentration",
                    if (!is.na(time_col)) "time"
                  )
                ),
                protein = "Protein",
                compound = "Compound",
                concentration = conc_col,
                time = time_col,
                "Compound"
              )
              # Isolated: reacting to the inputs updated below would re-trigger
              # this observer with every selection it sends
              cur_violin <- shiny::isolate(input$stats_violin_groupby)
              cur_scatter <- shiny::isolate(input$stats_scatter_groupby)
              sel_violin <- if (
                !is.null(cur_violin) && cur_violin %in% choices
              ) {
                cur_violin
              } else {
                default_grp
              }
              sel_scatter <- if (
                !is.null(cur_scatter) && cur_scatter %in% choices
              ) {
                cur_scatter
              } else {
                default_grp
              }

              shiny::updateSelectInput(
                session,
                "stats_violin_groupby",
                choices = choices,
                selected = sel_violin
              )
              shiny::updateSelectInput(
                session,
                "stats_scatter_groupby",
                choices = choices,
                selected = sel_scatter
              )
            }))

            # The Batch Control tab is built only when the samples have wells
            # (summary_results_ui()) and Protocol is the first tab, so the
            # panel needs no tab commands - sent with the panel's first render
            # they reach the browser before the tabs exist.
          } else if (iface == "hits") {
            #### Render unified Hits interface ----
            render_result_interface(
              "hits",
              hits_results_ui(ns, hits_summary, units, key = ckey)
            )

            ##### Hits unified table ----
            hits_col_selection <- picked_cols(
              "hits_tab_col_select",
              hits_col_choices(hits_summary, units)$selected
            )

            # Sample View of the picked samples and compounds, one row per
            # sample (transform_per_adduct()). Filtered before the rows are
            # combined, so Cmp Name and the compound columns follow the
            # compound selection.
            sample_view_table <- function(compounds = NULL, samples = NULL) {
              transform_per_adduct(
                filter_hits_table(
                  sort_hits_table(hits_summary, units),
                  selected_cols = names(hits_summary),
                  compounds = compounds,
                  samples = samples,
                  units = units
                ),
                proteins_table = protein_table_data(),
                compounds_table = compound_table_data(),
                samples_table = declaration_vars$sample_table,
                max_multiples = shiny::isolate(
                  conversion_sidebar_vars$result_list()
                )$max_multiples
              )
            }

            # The table has its own colour settings: the Relative Binding
            # colour kick (render_trigger) is not one of its events, so it is
            # not rebuilt while the other panels are used. The Display is one
            # of them, so a switch always renders (and releases the overlay
            # its click put up, see hits_results_ui()).
            hits_tab_trigger <- shiny::reactive({
              list(
                per_adduct = input$hits_per_adduct,
                color_variable = input$hits_color_variable,
                cols = hits_col_selection(),
                compounds = input$hits_tab_compound_select,
                samples = input$hits_tab_sample_select
              )
            }) |>
              shiny::debounce(200)

            output$hits_unified_tab <- DT::renderDT({
              # Waits for the debounced pickers, not only the inputs: the
              # output's first run comes once the pickers are bound but before
              # the debounce has passed them on, and its table was built again
              # 200 ms later. That run leaves the output (and its spinner) as
              # it is rather than clearing it.
              tab_inputs <- hits_tab_trigger()
              shiny::req(
                hits_summary,
                tab_inputs$samples,
                tab_inputs$compounds,
                tab_inputs$color_variable,
                cancelOutput = TRUE
              )
              if (isTRUE(input$hits_per_adduct == "Sample View")) {
                hits_table <- sample_view_table(
                  compounds = input$hits_tab_compound_select,
                  samples = input$hits_tab_sample_select
                )
                # Binding, total and mass-shift columns are always shown; a
                # picked column brings the columns of all proteoforms
                always_cols <- c(
                  "Tot. Binding [%]",
                  names(hits_table)[
                    !is.na(sample_view_col_kind(names(hits_table)))
                  ]
                )
                hits_table <- filter_hits_table(
                  hits_table,
                  selected_cols = names(hits_table)[
                    names(hits_table) %in% always_cols |
                      hits_base_col(names(hits_table)) %in%
                        hits_col_selection()
                  ],
                  units = units
                )
              } else {
                hits_table <- hits_view_table(
                  hits_summary,
                  hits_col_selection(),
                  units = units,
                  compounds = input$hits_tab_compound_select,
                  samples = input$hits_tab_sample_select
                )
              }

              hits_unified_raw(hits_table)

              clickable_cols <- c("Sample ID", "Protein", "Cmp Name")
              if ("Concentration" %in% names(units)) {
                clickable_cols <- c(clickable_cols, units[["Concentration"]])
              }

              hits_datatable <- render_hits_table(
                hits_table = hits_table,
                concentration_colors = if (
                  input$hits_color_variable == "Concentration" &&
                    "Concentration" %in% names(units)
                ) {
                  key_colors(
                    ckey,
                    "Concentration",
                    unique(hits_table[[units[["Concentration"]]]])
                  )
                } else {
                  NULL
                },
                # Every percentage as a bar in its cell
                bar_chart = hits_bar_cols(hits_table),
                # Compounds by their row's compound; samples by their own
                # colour, their protein's or their time's
                colors = if (input$hits_color_variable == "Compounds") {
                  key_colors(ckey, "Compounds", unique(hits_table$`Cmp Name`))
                } else if (
                  input$hits_color_variable %in% c("Samples", "Protein", "Time")
                ) {
                  sample_level_colors(
                    ckey,
                    input$hits_color_variable,
                    unique(hits_table$`Sample ID`)
                  )
                } else {
                  NULL
                },
                color_variable = color_mode(input$hits_color_variable),
                truncated = FALSE,
                clickable = clickable_cols,
                per_adduct = input$hits_per_adduct,
                units = units
              )

              hits_unified_current(hits_datatable)
              return(hits_datatable)
            }) |>
              shiny::bindEvent(hits_tab_trigger(), ignoreNULL = FALSE)

            ##### Hits unified table export ----
            setup_table_dl(
              input,
              output,
              session,
              "hits_unified_tab",
              data_fn = function() prepare_hits_export(hits_unified_raw()),
              filename_fn = function() {
                paste0(get_session_prefix(), "_Hits_Table")
              }
            )

            ##### Update column selector when display mode changes ----
            track_iface_observer(shiny::observeEvent(
              input$hits_per_adduct,
              {
                always_excluded <- c(
                  "Sample ID",
                  "Protein",
                  "Cmp Name",
                  "truncSample_ID",
                  "Tot. Binding [%]",
                  if ("Concentration" %in% names(units)) {
                    units[["Concentration"]]
                  } else {
                    NULL
                  },
                  if ("Time" %in% names(units)) units[["Time"]] else NULL
                )

                if (input$hits_per_adduct == "Sample View") {
                  # A proteoform's columns are picked by their plain name
                  adduct_cols <- unique(hits_base_col(names(
                    sample_view_table()
                  )))
                  kind <- sample_view_col_kind(adduct_cols)
                  adduct_binding_cols <- adduct_cols[
                    kind %in% c("binding", "total", "complex")
                  ]
                  adduct_mass_cols <- adduct_cols[kind %in% "mass"]
                  new_choices <- adduct_cols[
                    !adduct_cols %in%
                      c(always_excluded, adduct_binding_cols, adduct_mass_cols)
                  ]
                  adduct_selected <- new_choices[
                    new_choices %in%
                      c(
                        "Theor. Prot. [Da]",
                        "Int. Prot. [%]",
                        "Theor. Cmp [Da]",
                        "Bind. Stoich.",
                        "Binding [%]",
                        "Correct [%]"
                      )
                  ]
                  hits_col_selection(adduct_selected)
                  shinyWidgets::updatePickerInput(
                    session,
                    "hits_tab_col_select",
                    choices = new_choices,
                    selected = adduct_selected
                  )
                } else {
                  per_hit <- hits_col_choices(hits_summary, units)
                  hits_col_selection(per_hit$selected)
                  shinyWidgets::updatePickerInput(
                    session,
                    "hits_tab_col_select",
                    choices = per_hit$choices,
                    selected = per_hit$selected
                  )
                }
              },
              ignoreInit = TRUE
            ))

            ##### Hits unified table clicking observer ----
            track_iface_observer(shiny::observeEvent(
              input$hits_unified_tab_cell_clicked,
              {
                shiny::req(
                  input$hits_unified_tab_cell_clicked,
                  hits_unified_current()
                )

                cell_clicked <- input$hits_unified_tab_cell_clicked

                if (
                  !is.null(cell_clicked) &&
                    length(cell_clicked) &&
                    !is.na(hits_unified_current()$x$data[
                      cell_clicked$row,
                      cell_clicked$col + 1
                    ])
                ) {
                  data <- hits_unified_current()$x$data
                  cols <- names(data)
                  sample_col <- which(cols == "Sample ID") - 1
                  prot_col <- which(cols == "Protein") - 1
                  cmp_col <- which(cols == "Cmp Name") - 1
                  conc_col <- if ("Concentration" %in% names(units)) {
                    which(cols == units[["Concentration"]]) - 1
                  } else {
                    integer(0)
                  }

                  # The protein and compound of the clicked row: a compound
                  # opens with the protein it was found on, a concentration
                  # with the kinetics of the row's complex
                  row_value <- function(col) {
                    if (!col %in% cols) {
                      return(NA_character_)
                    }
                    as.character(data[[col]][cell_clicked$row])
                  }
                  # A Sample View row lists every compound of its sample
                  # ("A, B"); it opens with the first one
                  first_compound <- function(value) {
                    known <- unique(stats::na.omit(hits_summary$`Cmp Name`))
                    if (is.na(value) || value %in% known) {
                      return(value)
                    }
                    listed <- known[startsWith(value, paste0(known, ", "))]
                    if (length(listed)) {
                      listed[which.max(nchar(listed))]
                    } else {
                      value
                    }
                  }
                  row_protein <- row_value("Protein")
                  row_compound <- first_compound(row_value("Cmp Name"))

                  select_interface <- function(value) {
                    shinyjs::runjs(sprintf(
                      "document.querySelector('#%s input[value=\"%s\"]').click();",
                      sub(
                        "conversion_main",
                        "conversion_sidebar",
                        ns("analysis_select"),
                        fixed = TRUE
                      ),
                      value
                    ))
                  }

                  if (length(sample_col) && cell_clicked$col == sample_col) {
                    hits_pending_nav(list(
                      type = "sample",
                      value = cell_clicked$value
                    ))
                    select_interface(2)
                  } else if (length(prot_col) && cell_clicked$col == prot_col) {
                    navigate_overview(as.character(cell_clicked$value))
                    select_interface(2)
                  } else if (length(cmp_col) && cell_clicked$col == cmp_col) {
                    compound <- row_compound
                    protein <- row_protein
                    # Without the row's protein, the first protein the
                    # compound was declared with
                    if (is.na(protein)) {
                      choices <- overview_choices(hits_summary)
                      with_cmp <- vapply(
                        choices$compounds,
                        function(d) compound %in% d$compound,
                        logical(1)
                      )
                      protein <- choices$proteins$protein[
                        choices$proteins$protein %in% names(with_cmp)[with_cmp]
                      ][1]
                    }
                    navigate_overview(protein, compound)
                    select_interface(2)
                  } else if (length(conc_col) && cell_clicked$col == conc_col) {
                    shiny::req(length(result_list$kinetics) > 0)
                    navigate_kinetics(
                      as.character(cell_clicked$value),
                      key = if (!is.na(row_protein) && !is.na(row_compound)) {
                        complex_key(row_protein, row_compound)
                      }
                    )
                    select_interface(3)
                  }
                }
              },
              ignoreNULL = TRUE,
              ignoreInit = TRUE
            ))
          }

          # Counted over the whole run: the kinetics panel narrows
          # hits_summary to the picked complex, and a changed count would
          # announce a finished analysis on every switch of the complex
          n_hits_detected <- sum(is_complex_row(conversion_vars$hits_summary))
          if (!identical(show_completion_toast(), n_hits_detected)) {
            show_completion_toast(n_hits_detected)
          }
        }

        # Unblock UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "none";'
        ))
      },
      suspended = TRUE,
      label = "Conditional Results Rendering"
    )

    shiny::observeEvent(
      show_completion_toast(),
      {
        n_hits <- show_completion_toast()
        shiny::req(!is.null(n_hits))
        # Results restored after a dropped connection were not just computed
        if (completion_toast_muted) {
          completion_toast_muted <<- FALSE
          return()
        }
        shinyWidgets::show_toast(
          title = "Analysis completed",
          text = paste0(n_hits, " hit(s) detected."),
          type = "success",
          timer = 4000,
          timerProgressBar = TRUE
        )
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    ## Hits pending navigation observer ----
    # Fires after a click on a sample in the unified Hits table switched
    # analysis_select. Waits for the Relative Binding interface to render, then
    # picks the sample and opens the Sample View. Protein, compound and
    # concentration clicks set their selection directly (navigate_overview(),
    # navigate_kinetics()).
    shiny::observe({
      nav <- hits_pending_nav()
      shiny::req(!is.null(nav), identical(nav$type, "sample"))
      shiny::req(conversion_sidebar_vars$analysis_select() == 2)
      shiny::req(!is.null(input$conversion_sample_picker))

      shinyWidgets::updatePickerInput(
        session,
        "conversion_sample_picker",
        selected = nav$value
      )
      set_selected_tab("Sample View", session)
      hits_pending_nav(NULL)
    })

    ## Conversion Log Copy/Save handlers ----
    shiny::observeEvent(input$copy_protocol_log, {
      shinyjs::runjs(sprintf(
        "var el = document.getElementById('%s');
         if (el) navigator.clipboard.writeText(el.innerText);",
        ns("protocol_log")
      ))
      shinyWidgets::show_toast(
        "Protocol copied to clipboard",
        text = NULL,
        type = "success",
        timer = 3000,
        timerProgressBar = TRUE
      )
    })

    shiny::observeEvent(
      input$save_protocol_log,
      {
        fname <- paste0(get_session_prefix(), "_Protocol.txt")
        shinyjs::runjs(sprintf(
          "var el = document.getElementById('%s');
           if (el) {
             var a = document.createElement('a');
             a.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(el.innerText));
             a.setAttribute('download', '%s');
             a.style.display = 'none';
             document.body.appendChild(a);
             a.click();
             document.body.removeChild(a);
           }",
          ns("protocol_log"),
          fname
        ))
      },
      ignoreInit = TRUE,
      label = "Conversion Log Saver"
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
          color_cmp = sample_view_colors(
            conversion_vars$hits_summary,
            conversion_vars$color_key,
            input$color_variable,
            selected_sample,
            theme = theme
          ),
          symbol_cmp = if (identical(input$color_variable, "Compounds")) {
            key_marker_symbols(
              conversion_vars$color_key,
              "Compounds",
              unique(stats::na.omit(tbl$`Cmp Name`))
            )
          },
          color_variable = color_mode(input$color_variable),
          show_peak_labels = isTRUE(input$sample_view_spectrum_annotation),
          show_mass_diff = !isFALSE(input$sample_view_spectrum_diff),
          show_unmatched = isTRUE(input$sample_view_spectrum_unmatched),
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
          input$color_variable,
          !is.null(input$truncate_names)
        )
        smpl_compound_distribution(
          hits_summary = conversion_vars$hits_summary,
          sample = input$conversion_sample_picker,
          color_variable = color_mode(input$color_variable),
          truncate_names = input$truncate_names,
          key = conversion_vars$color_key,
          theme = theme,
          colors = sample_view_colors(
            conversion_vars$hits_summary,
            conversion_vars$color_key,
            input$color_variable,
            input$conversion_sample_picker,
            trunc = input$truncate_names,
            theme = theme
          )
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
      "overview_spectrum",
      build_fn = function(theme) {
        hits_summary <- conversion_vars$hits_summary
        protein <- overview_protein()
        compounds <- overview_compounds()
        shiny::req(
          hits_summary,
          length(compounds),
          !is.null(input$truncate_names),
          input$color_variable
        )
        result_list <- conversion_sidebar_vars$result_list()
        colors <- overview_colors(
          hits_summary,
          protein,
          compounds,
          variable = input$color_variable,
          key = conversion_vars$color_key,
          trunc = input$truncate_names,
          theme = theme
        )
        ov_symbols <- if (identical(input$color_variable, "Compounds")) {
          key_marker_symbols(
            conversion_vars$color_key,
            "Compounds",
            names(colors)
          )
        }
        samples <- spectrum_sample_ids(
          overview_subset(hits_summary, protein, compounds),
          "Protein",
          protein,
          sort_by_binding = isTRUE(input$overview_spectrum_sort_binding),
          by_mean = TRUE
        )
        if (!isTRUE(input$overview_spectrum_sort_binding)) {
          samples <- samples[order(match(samples, conversion_vars$color_key$samples))]
        }
        shiny::req(length(samples))
        if (length(samples) == 1) {
          spectrum_plot(
            sample = result_list$deconvolution[[samples]],
            color_cmp = colors,
            symbol_cmp = ov_symbols,
            color_variable = color_mode(input$color_variable),
            show_peak_labels = TRUE,
            show_mass_diff = FALSE,
            show_unmatched = isTRUE(input$overview_spectrum_unmatched),
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
            cubic = is.null(input$overview_spectrum_kind) ||
              input$overview_spectrum_kind == "Cubic",
            color_cmp = colors,
            symbol_cmp = ov_symbols,
            truncated = if (input$truncate_names) id_mapping else FALSE,
            color_variable = color_mode(input$color_variable),
            hits_summary = hits_summary,
            labels_show = input$overview_spectrum_labels,
            symbols_show = input$overview_spectrum_symbols,
            legend_show = input$overview_spectrum_legend,
            unmatched_show = isTRUE(input$overview_spectrum_unmatched),
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
      "overview_cmp_dist",
      build_fn = function(theme) {
        shiny::req(
          conversion_vars$hits_summary,
          length(overview_compounds()),
          input$color_variable,
          !is.null(input$truncate_names)
        )
        plot <- overview_compound_distribution(
          hits_summary = conversion_vars$hits_summary,
          protein = overview_protein(),
          compounds = overview_compounds(),
          color_variable = input$color_variable,
          truncate_names = input$truncate_names,
          key = conversion_vars$color_key,
          distribution_scale = input$overview_distribution_scale,
          distribution_labels = input$overview_distribution_labels,
          theme = theme,
          sample_order = if (!isTRUE(input$overview_distribution_sort_binding)) {
            conversion_vars$color_key$samples
          },
          show_unmatched = !isFALSE(input$overview_distribution_unmatched)
        )
        shiny::req(plot)
        plot
      },
      filename_fn = function() {
        paste0(get_session_prefix(), "_Compound_Distribution")
      }
    )

    setup_plot_dl(
      input,
      output,
      session,
      "stats_histogram",
      build_fn = function(theme) {
        rl <- conversion_sidebar_vars$result_list()
        shiny::req(rl, rl$hits_summary)
        stats_histogram(
          rl$hits_summary,
          theme = theme,
          show = input$stats_show_metric %||% "Correct"
        )
      },
      filename_fn = function() {
        paste0(get_session_prefix(), "_Statistics_Histogram")
      }
    )

    setup_plot_dl(
      input,
      output,
      session,
      "stats_boxplot",
      build_fn = function(theme) {
        rl <- conversion_sidebar_vars$result_list()
        shiny::req(rl, rl$hits_summary)
        stats_boxplot(
          rl$hits_summary,
          theme = theme,
          show_points = isTRUE(input$stats_boxplot_show_points),
          show = input$stats_show_metric %||% "Correct",
          fit_box = isTRUE(input$stats_boxplot_fit_box)
        )
      },
      filename_fn = function() {
        paste0(get_session_prefix(), "_Statistics_BoxPlot")
      }
    )

    setup_plot_dl(
      input,
      output,
      session,
      "stats_scatter",
      build_fn = function(theme) {
        rl <- conversion_sidebar_vars$result_list()
        shiny::req(rl, rl$hits_summary)
        fs <- isTRUE(input$stats_scatter_full_scale)
        grp <- if (is.null(input$stats_scatter_groupby)) {
          "Protein"
        } else {
          input$stats_scatter_groupby
        }
        stats_scatter(
          rl$hits_summary,
          full_scale = fs,
          group_by = grp,
          key = conversion_vars$color_key,
          theme = theme,
          show = input$stats_show_metric %||% "Correct"
        )
      },
      filename_fn = function() {
        paste0(get_session_prefix(), "_Statistics_Scatter")
      }
    )

    setup_plot_dl(
      input,
      output,
      session,
      "stats_violin",
      build_fn = function(theme) {
        rl <- conversion_sidebar_vars$result_list()
        shiny::req(rl, rl$hits_summary)
        grp <- if (is.null(input$stats_violin_groupby)) {
          "Protein"
        } else {
          input$stats_violin_groupby
        }
        stats_violin(
          rl$hits_summary,
          group_by = grp,
          theme = theme,
          key = conversion_vars$color_key,
          inner = if (is.null(input$stats_violin_inner)) {
            "Box"
          } else {
            input$stats_violin_inner
          },
          show = input$stats_show_metric %||% "Correct"
        )
      },
      filename_fn = function() {
        paste0(get_session_prefix(), "_Statistics_Violin")
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
      "overview_table_view",
      data_fn = function() prepare_hits_export(overview_table_view_raw()),
      filename_fn = function() {
        paste0(get_session_prefix(), "_Table_View_Overview")
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
      filename_fn = function() paste0(get_session_prefix(), "_Per_Concentration_Fits")
    )

    ## Events for conversion result interface ----

    ### Re-render the Relative Binding views on another colour variable ----
    # Only on a change of the variable itself. A built panel stays rendered
    # while other interfaces or tabs are shown, so switching the Results Menu
    # must not trigger it; a newly built panel renders on its own, and the
    # variable arriving with it (or changing from the previous build's) counts
    # as a change.
    shiny::observeEvent(
      input$color_variable,
      {
        shiny::req(conversion_vars$hits_summary, input$color_variable)
        render_trigger(render_trigger() + 1)
      },
      ignoreInit = TRUE,
      label = "Color Variable Change"
    )

    ### Recalculate results depending on excluded concentrations ----
    shiny::observeEvent(
      input[["kobs_result_cell_edit"]],
      {
        # Resolve row index to concentration name to avoid positional offset bugs
        result_list_local <- kinetics_result_list()
        shiny::req(result_list_local)
        all_conc <- rownames(
          result_list_local$binding_kobs_result$kobs_result_table
        )
        edited_conc <- all_conc[input[["kobs_result_cell_edit"]]$row]
        shiny::req(!is.null(edited_conc), !is.na(edited_conc))

        # Build on the ticks still waiting to be applied, if any
        select <- kobs_selection_pending() %||%
          conversion_vars$select_concentration

        # Expand the selection if this concentration is not yet tracked
        if (!(edited_conc %in% names(select))) {
          new_entry <- TRUE
          names(new_entry) <- edited_conc
          select <- c(select, new_entry)
        }

        select[edited_conc] <- input[["kobs_result_cell_edit"]]$value
        kobs_selection_pending(select)
      },
      ignoreInit = TRUE,
      label = "Concentration Selection Tick"
    )

    shiny::observeEvent(
      kobs_selection_settled(),
      {
        select <- kobs_selection_settled()
        kobs_selection_pending(NULL)

        # Ticked back to the applied selection: nothing to refit
        if (identical(select, conversion_vars$select_concentration)) {
          return(NULL)
        }

        # Apply changes to included concentrations
        conversion_vars$select_concentration <- select

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

        # Recalculate result object according to included concentrations, on
        # the samples of the picked complex
        result_list <- kinetics_result_list()
        shiny::req(result_list)

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
          result_list$kinetics_hits %||% result_list$hits_summary,
          concentrations_select = names(
            conversion_vars$select_concentration
          )[which(conversion_vars$select_concentration)],
          units = units_adapt,
          conc_time = conversion_vars$units
        )

        # None of the remaining concentrations could be fitted — keep the
        # previous results instead of replacing them with empty tables
        if (nrow(result_list$binding_kobs_result$kobs_result_table) == 0) {
          shinyWidgets::show_toast(
            "No concentration could be fitted",
            type = "warning",
            timer = 3000
          )
          return(NULL)
        }

        # Add kinact/KI results to result list
        result_list$kinact_ki_result <- add_kinact_ki_result(
          result_list,
          units = units_adapt
        )

        # Assign modified results to reactive variable
        conversion_vars$modified_results <- result_list
      },
      ignoreInit = TRUE,
      label = "Deconvolution Results Transfer"
    )

    # Help modals ----
    bind_help(input, conversion_help)

    # Eagerly render startup outputs so they are computed in the first reactive
    # flush and included in the same browser message as waiter_hide().
    shiny::outputOptions(output, "conversion_ui", suspendWhenHidden = FALSE)
    shiny::outputOptions(
      output,
      "declaration_info_ui",
      suspendWhenHidden = FALSE
    )

    # State of the module for a page reloaded after a dropped connection
    # (session_restore.R): the declaration with its tables as edited. The
    # result interfaces are rebuilt from the sidebar's result list.
    park <- function() {
      shiny::isolate(list(
        declaration = shiny::reactiveValuesToList(declaration_vars),
        protein_table_data = protein_table_data(),
        compound_table_data = compound_table_data(),
        sample_table_data = sample_table_data(),
        conc_unit = conc_unit_selected(),
        time_unit = time_unit_selected(),
        run_kinact_ki = conversion_sidebar_vars$run_kinact_ki(),
        has_results = !is.null(conversion_sidebar_vars$result_list())
      ))
    }

    # Return server values ----
    list(
      park = park,
      conversion_ready = shiny::reactive(declaration_vars$conversion_ready),
      input_list = shiny::reactive(list(
        Protein_Table = protein_table_data(),
        Compound_Table = compound_table_data(),
        Samples_Table = declaration_vars$sample_table,
        # The samples of the confirmed table only; those left out of the
        # conversion are not read at all
        result = conversion_input_result(
          declaration_vars$result,
          samples = declaration_vars$sample_table$Sample
        )
      )),
      samples_confirmed = shiny::reactive(declaration_vars$samples_confirmed),
      config_apply_block = config_apply_block,
      cancel_continuation = shiny::reactive(input$conversion_cont_cancel),
      activate_kinact_ki = shiny::reactive(trigger_kinact_ki())
    )
  })
}
