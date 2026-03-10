# app/view/upload_spectra.R

box::use(
  bslib[card, card_body, card_header, sidebar],
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
      safe_observe,
    ],
  app /
    logic /
    logging[
      write_log,
      get_session_id,
    ]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bslib::sidebar(
    class = "conversion-sidebar",
    width = "15%",
    shinyjs::useShinyjs(),
    shiny::uiOutput(ns("conversion_sidebar_ui"))
  )
}

#' @export
server <- function(id, conversion_main_vars, deconvolution_main_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Declare reactive vars ----
    result_list <- shiny::reactiveVal(NULL)
    complexes <- shiny::reactiveVal(NULL)
    analysis_status <- shiny::reactiveVal("pending")

    # Render sidebar UI ----
    output$conversion_sidebar_ui <- shiny::renderUI({
      shiny::div(
        class = "conversion-sidebar-ui",
        shiny::uiOutput(ns("conversion_analysis_controls_ui")),
        shiny::uiOutput(ns("conversion_result_controls_ui"))
      )
    })

    ## Analysis controls UI ----
    output$conversion_analysis_controls_ui <- shiny::renderUI(
      shiny::div(
        class = "interaction-analysis-flex",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "sidebar-title conversion-title",
              "Binding Analysis"
            ),
            shiny::numericInput(
              ns("peak_tolerance"),
              shiny::div(
                class = "label-tooltip",
                shiny::tags$label("Peak Tolerance [Da]"),
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("peak_tol_tooltip_bttn"),
                    label = NULL,
                    icon = shiny::icon("circle-question")
                  )
                )
              ),
              value = 3,
              min = 0,
              max = 20,
              step = 0.1
            ),
            shiny::numericInput(
              ns("max_multiples"),
              shiny::div(
                class = "label-tooltip",
                shiny::tags$label("Max. Stoichiometry"),
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("max_mult_tooltip_bttn"),
                    label = NULL,
                    icon = shiny::icon("circle-question")
                  )
                )
              ),
              value = 4,
              min = 1,
              max = 20,
              step = 1
            ),
            shiny::div(
              class = "ki-kinact-checkbox",
              shiny::checkboxInput(
                ns("run_ki_kinact"),
                shiny::div(
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
        )
      )
    )

    ## Result controls UI ----
    output$conversion_result_controls_ui <- shiny::renderUI({
      shiny::div(
        class = "interaction-analysis-flex",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "sidebar-title conversion-title",
              "Results Menu"
            ),
            shiny::div(
              class = "result-interface-selector-ui",
              shiny::uiOutput(ns("analysis_select_ui")),
              shiny::div(
                class = "complex-picker-ui",
                shiny::div(id = "complex-picker-connector"),
                shiny::div(
                  class = "complex-picker custom-disable",
                  shinyWidgets::pickerInput(
                    ns("complex"),
                    NULL,
                    choices = complexes()
                  )
                )
              )
            ),
            shinyjs::disabled(
              shiny::actionButton(
                ns("report_conversion_results"),
                "Report",
                icon = shiny::icon("square-poll-vertical"),
                width = "100%"
              )
            )
          )
        )
      )
    })

    # Analysis launch UI ----
    ## Toggle checkbox ----
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
        return(shiny::actionButton(
          ns("run_binding_analysis"),
          "Start",
          icon = shiny::icon("play"),
          width = "100%",
          class = "btn-highlight"
        ))
      } else {
        return(
          bslib::tooltip(
            shiny::div(
              style = "width: 100%;",
              shinyjs::disabled(
                shiny::actionButton(
                  ns("run_binding_analysis"),
                  "Start",
                  icon = shiny::icon("play"),
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
          label = "Run",
          icon = shiny::icon("play")
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
                    class = "conversion-save-buttons",
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
          # Delay conversion start
          Sys.sleep(1)

          # Preset logical ki_kinact_check
          ki_kinact_check <- FALSE

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
                ns = ns
              )

              result_with_hits$hits_summary <- summarize_hits(
                result_with_hits,
                sample_table = conversion_main_vars$input_list()$Samples_Table
              )

              message(paste("COMPUTING BINDING KINETICS\n  │"))

              # If Ki/kinact analysis is set to be performed
              if (input$run_ki_kinact) {
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
                  result_with_hits$binding_kobs_result <- add_kobs_binding_result(
                    hits_summary_filtered,
                    conc_time = conc_time,
                    units = units
                  )

                  # Add Ki/kinact results to result list
                  result_with_hits$ki_kinact_result <- add_ki_kinact_result(
                    result_with_hits,
                    units = units
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

          # # TODO
          # # Dev Mode
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_TEST.rds"
          # )
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits1.rds"
          # )
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_7.rds"
          # )
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_13.rds"
          # )
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_19.rds"
          # )
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_25.rds"
          # )
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_33.rds"
          # )
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_42.rds"
          # )
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_55.rds"
          # )
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_61.rds"
          # )

          ## TESTING

          # # Single entry
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\one_entry.rds"
          # )

          # # NA entry
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\one_entryNA.rds"
          # )

          # # Two NA entries
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\two_entryNA.rds"
          # )

          # # NA diff
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\NA_diff.rds"
          # )

          # # NA diff2
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\NA_diff2.rds"
          # )

          # # HiDrive-kinact-K
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\HiDrive-kinact-K.rds"
          # )

          # # 2025-12-10_MS_in-house_protein
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\2025-12-10_MS_in-house_protein.rds"
          # )

          # # HiDrive-kinact-KI Testdaten
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\results.rds"
          # )

          # # HiDrive-2025-09-04_New-Test-data
          # result_with_hits <- readRDS(
          #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\results_conversion.rds"
          # )

          # # Test
          # result_with_hits <- readRDS(
          #   "C:\\Users\\marian\\Desktop\\KF_Testing\\test.rds"
          # )

          # # Assign result list and hits table to reactive vars

          result_with_hits1 <<- result_with_hits

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

          shinyjs::delay(
            500,
            shinyjs::removeClass(
              selector = "#app-conversion_sidebar-analysis_select .radio:nth-child(1)",
              class = "custom-disable"
            )
          )
          if (ki_kinact_check) {
            shinyjs::delay(
              500,
              shinyjs::removeClass(
                selector = "#app-conversion_sidebar-analysis_select .radio:nth-child(2)",
                class = "custom-disable"
              )
            )
          }

          analysis_status("done")
        } else {
          result_list(NULL)
          analysis_status("pending")

          shinyjs::addClass(
            selector = "#app-conversion_sidebar-analysis_select .radio:nth-child(1)",
            class = "custom-disable"
          )
          if (input$run_ki_kinact) {
            shinyjs::addClass(
              selector = "#app-conversion_sidebar-analysis_select .radio:nth-child(2)",
              class = "custom-disable"
            )
          }
          shinyjs::addClass(
            selector = ".complex-picker .form-group .bootstrap-select",
            class = "custom-disable"
          )
          shinyjs::removeClass(
            id = "complex-picker-connector",
            class = "complex-picker-connector-color",
            asis = TRUE
          )

          shiny::updateRadioButtons(session, "analysis_select", selected = 1)

          shinyjs::enable("peak_tolerance")
          shinyjs::enable("max_multiples")

          shiny::updateActionButton(
            session = session,
            "run_binding_analysis",
            label = "Run",
            icon = shiny::icon("play"),
            disabled = FALSE
          )
        }
      }
    )

    ## Render analysis select input element ----
    output$analysis_select_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::radioButtons(
          inputId = ns("analysis_select"),
          label = NULL,
          choiceNames = list(
            "Relative Binding",
            shiny::span(
              "K",
              htmltools::tags$sub("i"),
              " / k",
              htmltools::tags$sub("inact")
            )
          ),
          choiceValues = list(1, 2)
        ),
        shiny::tags$script(paste0(
          "
      (function() {
        var selector = '#",
          ns("analysis_select"),
          " .radio:nth-child(1), #",
          ns("analysis_select"),
          " .radio:nth-child(2)';
        $(selector).addClass('custom-disable');
      })();
    "
        ))
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

        if (input$analysis_select == 1) {
          shiny::updateRadioButtons(session, "analysis_select", selected = 1)

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

    # Server return values ----
    return(
      shiny::reactiveValues(
        result_list = shiny::reactive(result_list()),
        complex = shiny::reactive(input$complex),
        run_analysis = shiny::reactive(input$run_binding_analysis),
        peak_tolerance = shiny::reactive(input$peak_tolerance),
        run_ki_kinact = shiny::reactive(input$run_ki_kinact),
        analysis_select = shiny::reactive(input$analysis_select)
      )
    )
  })
}
