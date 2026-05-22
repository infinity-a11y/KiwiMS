# app/view/upload_spectra.R

box::use(
  bslib[card, card_body, card_header, sidebar, tooltip],
  shiny[actionButton, fileInput, NS, textInput, moduleServer, reactive],
)

box::use(
  app /
    logic /
    conversion_functions[
      add_hits,
      summarize_hits,
      check_filter_hits,
      add_kobs_binding_result,
      add_ki_kinact_result,
      conversion_tracking_js,
      log_binding_kinetics,
      log_filtered_samples,
      log_filtered_concentrations,
    ],
  app /
    logic /
    helper_functions[
      config_badge,
      safe_observe,
    ],
  app /
    logic /
    logging[
      write_log,
      get_session_id,
    ],
  app / logic / user_settings[read_user_settings, update_user_setting],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bslib::sidebar(
    class = "conversion-sidebar",
    width = "18%",
    shinyjs::useShinyjs(),
    shiny::uiOutput(ns("conversion_sidebar_ui"))
  )
}

#' @export
server <- function(
  id,
  conversion_main_vars,
  deconvolution_main_vars,
  config_file,
  config_filename = shiny::reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Declare reactive vars ----
    result_list <- shiny::reactiveVal(NULL)
    complexes <- shiny::reactiveVal(NULL)
    analysis_status <- shiny::reactiveVal("pending")
    ki_kinact_available <- shiny::reactiveVal(FALSE)
    console_log_snapshot <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$console_log_snapshot, {
      console_log_snapshot(input$console_log_snapshot)
    })

    # Render sidebar UI ----
    output$conversion_sidebar_ui <- shiny::renderUI({
      shiny::div(
        class = "conversion-sidebar-ui",
        shiny::uiOutput(ns("conversion_analysis_controls_ui")),
        shiny::uiOutput(ns("conversion_result_controls_ui"))
      )
    })

    ## Analysis controls UI ----
    output$conversion_analysis_controls_ui <- shiny::renderUI({
      saved <- read_user_settings()
      pt_default <- saved$peak_tolerance
      mm_default <- saved$max_multiples

      shiny::div(
        class = "sidebar-section",
        shiny::div(
          class = "sidebar-title custom-sidebar-title",
          "Binding Analysis"
        ),
        shiny::numericInput(
          ns("peak_tolerance"),
          shiny::div(
            class = "label-tooltip",
            shiny::tags$label("Peak Tolerance [Da]"),
            shiny::div(
              class = "label-save-button",
              tooltip(
                shiny::div(
                  class = "save-button",
                  shiny::actionButton(
                    ns("save_peak_tol_btn"),
                    label = NULL,
                    icon = shiny::icon("floppy-disk"),
                    class = "btn-default"
                  )
                ),
                "Save Setting",
                placement = "top"
              ),
              tooltip(
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("peak_tol_tooltip_bttn"),
                    label = NULL,
                    icon = shiny::icon("circle-question")
                  )
                ),
                "Help",
                placement = "top"
              )
            )
          ),
          value = pt_default,
          min = 0,
          max = 20,
          step = 0.1,
          width = "100%"
        ),
        shiny::numericInput(
          ns("max_multiples"),
          shiny::div(
            class = "label-tooltip",
            shiny::tags$label("Max. Stoichiometry"),

            shiny::div(
              class = "label-save-button",
              tooltip(
                shiny::div(
                  class = "save-button",
                  shiny::actionButton(
                    ns("save_max_mult_btn"),
                    label = NULL,
                    icon = shiny::icon("floppy-disk"),
                    class = "btn-default"
                  )
                ),
                "Save Setting",
                placement = "top"
              ),
              tooltip(
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("max_mult_tooltip_bttn"),
                    label = NULL,
                    icon = shiny::icon("circle-question")
                  )
                ),
                "Help",
                placement = "top"
              )
            )
          ),
          value = mm_default,
          min = 1,
          max = 20,
          step = 1,
          width = "100%"
        ),
        shiny::div(
          class = "ki-kinact-checkbox",
          shiny::checkboxInput(
            ns("run_ki_kinact"),
            shiny::span(
              class = "ki-kinact-label",
              "Run",
              shiny::div(
                class = "ki-kinact-highlight",
                " K",
                htmltools::tags$sub("i"),
                " / k",
                htmltools::tags$sub("inact")
              ),
              " Analysis"
            ),
            value = FALSE
          )
        ),
        shiny::uiOutput(ns("run_button_wrapper"))
      )
    })

    ## Result controls UI ----
    # Switches between Experiment Configuration (analysis pending) and
    # Results Menu (analysis done) based on analysis_status().
    output$conversion_result_controls_ui <- shiny::renderUI({
      if (analysis_status() == "pending") {
        shiny::div(
          class = "sidebar-section",
          shiny::div(
            class = "sidebar-title custom-sidebar-title",
            "Experiment Configuration"
          ),
          shiny::uiOutput(ns("conversion_config_status_ui"))
        )
      } else {
        shiny::div(
          class = "sidebar-section",
          shiny::div(
            class = "sidebar-title custom-sidebar-title",
            "Results Menu"
          ),
          shiny::div(
            class = "result-interface-selector-ui",
            shiny::uiOutput(ns("analysis_select_ui")),
            shiny::div(
              class = "complex-picker-ui",
              shiny::div(id = "complex-picker-connector"),
              shiny::div(
                # class = "complex-picker custom-disable",
                class = "complex-picker",
                shinyWidgets::pickerInput(
                  ns("complex"),
                  NULL,
                  choices = complexes()
                )
              )
            )
          )
          # ,
          # bslib::tooltip(
          #   shiny::div(
          #     shinyjs::disabled(
          #       shiny::actionButton(
          #         ns("report_conversion_results"),
          #         "Report",
          #         icon = shiny::icon("square-poll-vertical"),
          #         width = "100%"
          #       )
          #     )
          #   ),
          #   "Report generation is temporarily unavailable",
          #   placement = "top"
          # )
        )
      }
    })

    ## Experiment Configuration status panel ----
    output$conversion_config_status_ui <- shiny::renderUI({
      active <- !is.null(config_file())
      badge <- if (active) {
        config_badge("ok", "Active", config_filename())
      } else {
        config_badge("err", "Not loaded")
      }
      shiny::div(
        class = "sidebar-config-status sidebar-config-status--conversion",
        shiny::tags$p(
          class = "sidebar-config-description",
          "Maps samples to experimental metadata to auto-fill the samples table."
        ),
        badge,
        shiny::actionButton(
          ns("open_config_btn"),
          "Experiment Configuration",
          icon = shiny::icon("upload"),
          class = "btn btn-sm btn-default"
        )
      )
    })

    # Activate ki_kinact from config autofill signal ----
    safe_observe(
      event_expr = conversion_main_vars$activate_ki_kinact(),
      observer_name = "Activate Ki/kinact from Config",
      handler_fn = function() {
        shiny::req(conversion_main_vars$activate_ki_kinact() > 0)
        shiny::updateCheckboxInput(session, "run_ki_kinact", value = TRUE)
      }
    )

    # Analysis launch UI ----
    ## Idle checkbox ----
    # Temporarily blocking UI and wait for conversion main module server answer to allow Start button to react
    safe_observe(
      event_expr = input$run_ki_kinact,
      observer_name = "Checkbox Idle",
      handler_fn = function() {
        # Block UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "block";'
        ))

        # Unblock UI
        shinyjs::delay(
          500,
          shinyjs::runjs(paste0(
            'document.getElementById("blocking-overlay").style.display ',
            '= "none";'
          ))
        )
      }
    )

    ## Checkbox Toggle ----
    # safe_observe(
    #   event_expr = conversion_main_vars$samples_confirmed(),
    #   observer_name = "Checkbox Toggle",
    #   handler_fn = function() {
    #     shinyjs::toggleState("run_ki_kinact")
    #     shinyjs::toggleClass(selector = ".checkbox", class = "checkbox-disable")
    #   }
    # )

    ## Conditional tooltip for launch button ----
    output$run_button_wrapper <- shiny::renderUI({
      if (isTRUE(conversion_main_vars$conversion_ready())) {
        return(
          shiny::div(
            class = "start-button",
            shiny::actionButton(
              ns("run_binding_analysis"),
              "Start",
              icon = shiny::icon("circle-play"),
              width = "100%",
              class = "btn-highlight"
            )
          )
        )
      } else {
        return(
          bslib::tooltip(
            shiny::div(
              class = "start-button",
              shinyjs::disabled(
                shiny::actionButton(
                  ns("run_binding_analysis"),
                  "Start",
                  icon = shiny::icon("circle-play"),
                  width = "100%"
                )
              )
            ),
            "Confirm all tables first",
            placement = "bottom"
          )
        )
      }
    })

    # Update UI on reset results event ----
    safe_observe(
      event_expr = deconvolution_main_vars$continue_conversion(),
      observer_name = "Results Resetter",
      handler_fn = function() {
        shiny::updateActionButton(
          session = session,
          "run_binding_analysis",
          label = "Start",
          icon = shiny::icon("play")
        )
        # Enable ki/kinact analysis checkbox
        shinyjs::enable("run_ki_kinact")
        shinyjs::removeClass(
          selector = ".checkbox",
          class = "checkbox-disable"
        )
        shinyjs::enable("peak_tolerance")
        shinyjs::enable("max_multiples")
        analysis_status("pending")
      }
    )

    # Analysis run event ----

    ## Running analysis UI ----
    safe_observe(
      event_expr = input$run_binding_analysis,
      observer_name = "Conversion Analysis UI",
      handler_fn = function() {
        shiny::req(analysis_status() == "pending")

        shiny::showModal(
          shiny::div(
            class = "start-modal log-modal",
            shiny::modalDialog(
              shiny::fluidRow(
                shiny::br(),
                shiny::column(
                  width = 12,
                  shinyjs::useShinyjs(),
                  shiny::div(
                    class = "conversion-progress",
                    shinyWidgets::progressBar(
                      id = ns("conversion_progress"),
                      value = 0,
                      title = "Initiating Conversion",
                      display_pct = TRUE
                    )
                  ),
                  shiny::div(
                    class = "console-container",
                    shiny::tags$pre(id = ns("console_log")),
                    shiny::actionButton(
                      ns("scroll_btn"),
                      NULL,
                      icon = shiny::icon("arrow-down"),
                      class = "btn-info btn-sm"
                    )
                  )
                )
              ),
              title = "Binding Analysis",
              easyClose = FALSE,
              footer = shiny::tagList(
                shiny::div(
                  class = "conversion-footer",
                  shiny::div(
                    class = "conversion-save-button",
                    shiny::div(
                      class = "modal-button",
                      shinyjs::disabled(shiny::actionButton(
                        ns("copy_conversion_log"),
                        "Clip",
                        icon = shiny::icon("clipboard")
                      ))
                    ),
                    shiny::div(
                      class = "modal-button",
                      shinyjs::disabled(shiny::actionButton(
                        ns("save_conversion_log"),
                        "Save",
                        icon = shiny::icon("download"),
                        width = "auto"
                      ))
                    )
                  ),
                  shiny::div(
                    class = "modal-button",
                    shinyjs::disabled(shiny::actionButton(
                      ns("dismiss_conversion"),
                      "Dismiss"
                    ))
                  )
                )
              )
            )
          )
        )
      },
      priority = 10
    )

    ## Conditional processing ----
    safe_observe(
      event_expr = input$run_binding_analysis,
      observer_name = "Conversion Processing",
      handler_fn = function() {
        if (analysis_status() == "pending") {
          write_log("Conversion initiated")
          write_log(paste(
            "Conversion parameters:\n",
            paste(
              c(
                paste("Ki/kinact =", isTRUE(input$run_ki_kinact)),
                paste("Peak Tolerance =", input$peak_tolerance, "Da"),
                paste("Max. Stoichiometry =", input$max_multiples)
              ),
              collapse = "\n "
            )
          ))
          # Disable ki/kinact analysis checkbox
          shinyjs::disable("run_ki_kinact")
          shinyjs::addClass(selector = ".checkbox", class = "checkbox-disable")

          # Delay conversion start
          Sys.sleep(1)

          # Preset logical flags
          ki_kinact_check <- FALSE
          no_hits_found <- FALSE

          # Activate JS function for conversion process tracking
          shinyjs::runjs(sprintf(
            conversion_tracking_js,
            ns("console_log"),
            ns("scroll_btn")
          ))

          # Add hits
          withCallingHandlers(
            expr = {
              result_with_hits <- add_hits(
                conversion_main_vars$input_list()$result,
                sample_table = conversion_main_vars$input_list()$Samples_Table,
                protein_table = conversion_main_vars$input_list()$Protein_Table,
                compound_table = conversion_main_vars$input_list()$Compound_Table,
                peak_tolerance = input$peak_tolerance,
                max_multiples = input$max_multiples,
                session = session,
                ns = ns,
                ki_kinact = isTRUE(input$run_ki_kinact),
                config = config_file()
              )

              result_with_hits$hits_summary <- summarize_hits(
                result_with_hits,
                sample_table = conversion_main_vars$input_list()$Samples_Table
              )

              if (sum(!is.na(result_with_hits$hits_summary$Compound)) == 0) {
                no_hits_found <- TRUE
                message(
                  "No hits detected — result interface will not be loaded.\n"
                )
              }

              # If Ki/kinact analysis is set to be performed
              if (!no_hits_found && input$run_ki_kinact) {
                message(paste("COMPUTING BINDING KINETICS\n  │"))

                # Get concentration and time units
                conc_time <- names(result_with_hits$hits_summary)[unlist(sapply(
                  c("Concentration", "Time"),
                  grep,
                  names(result_with_hits$hits_summary)
                ))]
                units <- gsub("Concentration |Time |\\[|\\]", "", conc_time)
                names(units) <- c("Concentration", "Time")

                # Log initiation of binding kinetics analysis
                log_binding_kinetics(
                  concentrations = result_with_hits$hits_summary[[conc_time[
                    1
                  ]]],
                  times = result_with_hits$hits_summary[[conc_time[2]]],
                  units = units
                )

                # Perform checks for binding kinetics analysis prerequisites
                hits_summary_filtered <- check_filter_hits(
                  result_with_hits
                )

                ki_kinact_check <- is.data.frame(hits_summary_filtered)
                ki_kinact_available(ki_kinact_check)

                if (ki_kinact_check) {
                  # Log filtered samples
                  log_filtered_samples(
                    diff = nrow(result_with_hits$hits_summary) -
                      nrow(hits_summary_filtered)
                  )

                  # Log filtered concentrations
                  log_filtered_concentrations(
                    initial_tbl = result_with_hits$hits_summary,
                    filtered_tbl = hits_summary_filtered,
                    conc_time = conc_time
                  )

                  # Add binding/kobs results to result list
                  shinyWidgets::updateProgressBar(
                    session = session,
                    id = ns("conversion_progress"),
                    value = 85,
                    title = "Computing kobs / binding curves..."
                  )
                  result_with_hits$binding_kobs_result <- add_kobs_binding_result(
                    hits_summary_filtered,
                    conc_time = conc_time,
                    units = units
                  )

                  # Add Ki/kinact results to result list
                  shinyWidgets::updateProgressBar(
                    session = session,
                    id = ns("conversion_progress"),
                    value = 93,
                    title = "Computing Ki / kinact..."
                  )

                  result_with_hits$ki_kinact_result <- add_ki_kinact_result(
                    result_with_hits,
                    units = units
                  )

                  shinyWidgets::updateProgressBar(
                    session = session,
                    id = ns("conversion_progress"),
                    value = 100,
                    title = "Binding kinetics analysis complete."
                  )
                }
              }
            },
            message = function(m) {
              clean_msg <- gsub("\\", "\\\\", m$message, fixed = TRUE)
              clean_msg <- gsub("'", "\\'", clean_msg, fixed = TRUE)
              clean_msg <- gsub("\n", "<br>", clean_msg, fixed = TRUE)

              js_cmd <- sprintf(
                "
            var el = document.getElementById('%s');
            if (el) {
              el.innerHTML += '%s';
              el.doAutoScroll();
            }
              ",
                ns("console_log"),
                clean_msg
              )

              shinyjs::runjs(js_cmd)
            }
          )

          if (no_hits_found) {
            # No hits — log, re-enable inputs, let user dismiss and stay in declaration
            write_log("Conversion finalized — no hits detected")

            shinyjs::enable("run_ki_kinact")
            shinyjs::removeClass(
              selector = ".checkbox",
              class = "checkbox-disable"
            )
            shinyjs::enable("peak_tolerance")
            shinyjs::enable("max_multiples")
            shiny::updateActionButton(
              session = session,
              "run_binding_analysis",
              label = "Start",
              icon = shiny::icon("play"),
              disabled = FALSE
            )

            shinyjs::enable("save_conversion_log")
            shinyjs::enable("copy_conversion_log")
            shinyjs::enable("dismiss_conversion")
            shinyjs::addClass(
              id = "dismiss_conversion",
              class = "btn-highlight"
            )
          } else {
            # Assign result list and hits table to reactive vars
            write_log(paste(
              "Conversion finalized —",
              sum(!is.na(result_with_hits$hits_summary$Compound)),
              "hit(s)"
            ))
            result_list(result_with_hits)

            # Save distinct protein - compound combinations/complexes
            complex_df <- dplyr::distinct(
              result_with_hits$hits_summary,
              Protein,
              Compound
            ) |>
              dplyr::filter(!is.na(Compound))

            choice_values <- stats::setNames(
              complex_df$Compound,
              complex_df$Compound
            )

            complexes <- split(choice_values, complex_df$Protein)

            complexes(complexes)

            # Update sidebar control inputs
            shiny::updateActionButton(
              session = session,
              "run_binding_analysis",
              label = "Reset",
              icon = shiny::icon("repeat")
            )
            shinyjs::disable("peak_tolerance")
            shinyjs::disable("max_multiples")

            # Enable modal window buttons
            shinyjs::enable("save_conversion_log")
            shinyjs::enable("copy_conversion_log")
            shinyjs::enable("dismiss_conversion")
            shinyjs::addClass(
              id = "dismiss_conversion",
              class = "btn-highlight"
            )

            shinyjs::runjs(paste0(
              "var el = document.getElementById('",
              ns("console_log"),
              "');",
              "if (el) Shiny.setInputValue('",
              ns("console_log_snapshot"),
              "', el.innerHTML, {priority: 'event'});"
            ))

            analysis_status("done")
          }
        } else {
          write_log("Conversion reset")
          result_list(NULL)
          console_log_snapshot(NULL)
          ki_kinact_available(FALSE)
          analysis_status("pending")

          # Enable ki/kinact analysis checkbox
          shinyjs::enable("run_ki_kinact")
          shinyjs::removeClass(
            selector = ".checkbox",
            class = "checkbox-disable"
          )

          # Disable result interface radio buttons
          shinyjs::addClass(
            selector = paste(
              "#app-conversion_sidebar-analysis_select .radio:nth-child(1),",
              "#app-conversion_sidebar-analysis_select .radio:nth-child(2),",
              "#app-conversion_sidebar-analysis_select .radio:nth-child(3)"
            ),
            class = "custom-disable"
          )
          if (input$run_ki_kinact) {
            shinyjs::addClass(
              selector = "#app-conversion_sidebar-analysis_select .radio:nth-child(4)",
              class = "custom-disable"
            )
          }

          # Disable complex picker
          shinyjs::addClass(
            selector = ".complex-picker .form-group .bootstrap-select",
            class = "custom-disable"
          )
          shinyjs::removeClass(
            id = "complex-picker-connector",
            class = "complex-picker-connector-color",
            asis = TRUE
          )

          # Reset analysis interface selection to Relative Binding
          shiny::updateRadioButtons(session, "analysis_select", selected = 2)

          # Reenable conversion parameter inputs
          shinyjs::enable("peak_tolerance")
          shinyjs::enable("max_multiples")

          shiny::updateActionButton(
            session = session,
            "run_binding_analysis",
            label = "Start",
            icon = shiny::icon("play"),
            disabled = FALSE
          )
        }
      }
    )

    ## Render analysis select input element ----
    output$analysis_select_ui <- shiny::renderUI({
      ki <- ki_kinact_available()
      shiny::tagList(
        shiny::radioButtons(
          inputId = ns("analysis_select"),
          label = NULL,
          choiceNames = list(
            "Summary Statistics",
            "Hits Table",
            "Relative Binding",
            shiny::span(
              "K",
              htmltools::tags$sub("i"),
              " / k",
              htmltools::tags$sub("inact")
            )
          ),
          choiceValues = list(1, 4, 2, 3)
        ),
        if (!ki) {
          shiny::tags$script(shiny::HTML(paste0(
            "(function() {",
            "  $('#",
            ns("analysis_select"),
            " .radio:nth-child(4)')",
            "    .addClass('custom-disable');",
            "})()"
          )))
        }
      )
    })

    ## Dismiss conversion ----
    shiny::observeEvent(input$dismiss_conversion, {
      shiny::removeModal()
    })

    ## Copy conversion log to clipboard ----
    shiny::observeEvent(input$copy_conversion_log, {
      shinyjs::runjs(sprintf(
        "
    var text = document.getElementById('%s').innerText;
    navigator.clipboard.writeText(text).then(function() {
      alert('Log copied to clipboard!');
    });
  ",
        session$ns("console_log")
      ))
    })

    ## Save conversion log ----
    safe_observe(
      event_expr = input$save_conversion_log,
      observer_name = "Conversion Log Saver",
      handler_fn = function() {
        # Generate a filename with a timestamp
        fname <- paste0(
          "conversion_SESSION",
          get_session_id(),
          ".txt"
        )

        shinyjs::runjs(sprintf(
          "
    var text = document.getElementById('app-conversion_sidebar-console_log').innerText;
    var element = document.createElement('a');
    element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
    element.setAttribute('download', '%s');
    element.style.display = 'none';
    document.body.appendChild(element);
    element.click();
    document.body.removeChild(element);
  ",
          fname
        ))
      }
    )

    # Analysis select UI conditional rendering ----
    ## Toggle classes ----
    safe_observe(
      observer_name = "Toggle Converion Sidebar Inputs",
      handler_fn = function() {
        shiny::req(input$analysis_select)

        # Block UI
        # shinyjs::runjs(paste0(
        #   'document.getElementById("blocking-overlay").style.display ',
        #   '= "block";'
        # ))

        if (input$analysis_select %in% c(1, 2, 4)) {
          shinyjs::addClass(
            selector = ".complex-picker .form-group .bootstrap-select",
            class = "custom-disable"
          )
          shinyjs::removeClass(
            id = "complex-picker-connector",
            class = "complex-picker-connector-color",
            asis = TRUE
          )
        } else {
          shinyjs::removeClass(
            selector = ".complex-picker .form-group .bootstrap-select",
            class = "custom-disable"
          )
          shinyjs::addClass(
            id = "complex-picker-connector",
            class = "complex-picker-connector-color",
            asis = TRUE
          )
        }
      }
    )

    shiny::observeEvent(input$run_binding_analysis, {
      shinyjs::addClass(
        selector = ".complex-picker .form-group .bootstrap-select",
        class = "custom-disable"
      )
      shinyjs::removeClass(
        id = "complex-picker-connector",
        class = "complex-picker-connector-color",
        asis = TRUE
      )
    })

    # Save default value buttons ----
    shiny::observeEvent(input$save_peak_tol_btn, {
      val <- input$peak_tolerance
      if (!is.null(val) && !is.na(val)) {
        update_user_setting("peak_tolerance", val)
        shinyWidgets::show_toast(
          paste0("Peak Tolerance default set to ", val, " Da"),
          text = NULL,
          type = "success",
          timer = 3000,
          timerProgressBar = TRUE
        )
      }
    })

    shiny::observeEvent(input$save_max_mult_btn, {
      val <- input$max_multiples
      if (!is.null(val) && !is.na(val)) {
        update_user_setting("max_multiples", val)
        shinyWidgets::show_toast(
          paste0("Max. Stoichiometry default set to ", val),
          text = NULL,
          type = "success",
          timer = 3000,
          timerProgressBar = TRUE
        )
      }
    })

    # Tooltips ----
    ## Peak tolerance ----
    shiny::observeEvent(input$peak_tol_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = "Peak Tolerance",
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    'The ',
                    shiny::strong("Peak Tolerance"),
                    ' sets the maximum acceptable ',
                    shiny::strong("mass error"),
                    ' (or deviation) around the theoretical molecular mass of your target compound.'
                  ),
                  shiny::p(
                    'A measured mass peak is considered a ',
                    shiny::strong("Hit"),
                    ' only if it falls within this acceptable range.'
                  ),
                  shiny::br(),
                  shiny::h5("Example:"),
                  shiny::p(
                    shiny::div(shiny::strong("Theoretical Mass:"), ' 105 Da'),
                    shiny::div(shiny::strong("Tolerance:"), ' ± 2 Da'),
                    shiny::div(
                      shiny::strong("Accepted Range:"),
                      ' [103 Da, 107 Da]'
                    )
                  ),
                  shiny::tags$ul(
                    shiny::tags$li(
                      shiny::HTML(
                        'Measured peak at <strong>104 Da</strong> &rightarrow; Hit <b>&check;</b> (Within the range [103 Da, 107 Da])'
                      )
                    ),
                    shiny::tags$li(
                      shiny::HTML(
                        'Measured peak at <strong>107 Da</strong> &rightarrow; Hit <b>&check;</b> (Exactly on the upper boundary)'
                      )
                    ),
                    shiny::tags$li(
                      shiny::HTML(
                        'Measured peak at <strong>102 Da</strong> &rightarrow; No Hit <b>&times;</b> (Outside the lower boundary of 103 Da)'
                      )
                    ),
                    shiny::tags$li(
                      shiny::HTML(
                        'Measured peak at <strong>109 Da</strong> &rightarrow; No Hit <b>&times;</b> (Outside the upper boundary of 107 Da)'
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    })

    ## Maximum stoichiometry ----
    shiny::observeEvent(input$max_mult_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = "Maximum Stoichiometry",
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "The maximum number of compound molecules bound to the target protein."
                  ),
                  shiny::p(
                    shiny::div(
                      'If the',
                      shiny::em('Max. Stoichiometry'),
                      ' value is set to',
                      shiny::strong(3),
                      "protein-compound complexes of up to three bound compounds are screened for."
                    ),
                    shiny::div(
                      "Complexes with four bound compounds would not be accounted for in the analysis."
                    )
                  )
                )
              )
            )
          )
        )
      )
    })

    # Eagerly render all sidebar outputs that are visible on first tab visit so
    # they are computed in the first reactive flush alongside waiter_hide().
    shiny::outputOptions(
      output,
      "conversion_sidebar_ui",
      suspendWhenHidden = FALSE
    )
    shiny::outputOptions(
      output,
      "conversion_analysis_controls_ui",
      suspendWhenHidden = FALSE
    )
    shiny::outputOptions(
      output,
      "conversion_result_controls_ui",
      suspendWhenHidden = FALSE
    )
    shiny::outputOptions(
      output,
      "conversion_config_status_ui",
      suspendWhenHidden = FALSE
    )
    shiny::outputOptions(
      output,
      "run_button_wrapper",
      suspendWhenHidden = FALSE
    )
    shiny::outputOptions(
      output,
      "analysis_select_ui",
      suspendWhenHidden = FALSE
    )

    # Server return values ----
    return(
      shiny::reactiveValues(
        result_list = shiny::reactive(result_list()),
        complex = shiny::reactive(input$complex),
        run_analysis = shiny::reactive(input$run_binding_analysis),
        peak_tolerance = shiny::reactive(input$peak_tolerance),
        max_multiples = shiny::reactive(input$max_multiples),
        run_ki_kinact = shiny::reactive(input$run_ki_kinact),
        analysis_select = shiny::reactive(input$analysis_select),
        open_config_clicked = shiny::reactive(input$open_config_btn),
        console_log_snapshot = shiny::reactive(console_log_snapshot())
      )
    )
  })
}
