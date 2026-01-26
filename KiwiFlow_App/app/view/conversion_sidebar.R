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
      add_kobs_binding_result,
      add_ki_kinact_result,
      conversion_tracking_js,
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
    analysis_status <- shiny::reactiveVal("pending")

    # Render sidebar UI ----
    output$conversion_sidebar_ui <- shiny::renderUI({
      shiny::div(
        class = "conversion-sidebar-ui",
        # shiny::uiOutput(ns("conversion_info_ui")),
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
                    label = "",
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
                    label = "",
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
            # shinyjs::disabled(
            shiny::actionButton(
              ns("run_binding_analysis"),
              "Run",
              icon = shiny::icon("play"),
              width = "100%"
            )
            # )
          )
        )
      )
    )

    ## Result controls UI ----
    output$conversion_result_controls_ui <- shiny::renderUI({
      complexes <- list(
        "Global Overview",
        "COOB" = paste0("Cmp-", 30:40),
        "KRAS" = paste0("ALMP3kALMP3kALMP3k-", 1:5)
      )

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
                  class = "complex-picker",
                  shinyWidgets::pickerInput(
                    ns("complex"),
                    NULL,
                    choices = complexes
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
    shiny::observeEvent(conversion_main_vars$samples_confirmed(), {
      shinyjs::toggleState("run_ki_kinact")
      shinyjs::toggleClass(selector = ".checkbox", class = "checkbox-disable")
    })

    ## Toggle button ----
    shiny::observe({
      if (isTRUE(conversion_main_vars$conversion_ready())) {
        shinyjs::enable("run_binding_analysis")
        shinyjs::addClass(
          id = "run_binding_analysis",
          class = "btn-highlight"
        )
      } else {
        shinyjs::disable("run_binding_analysis")
        shinyjs::removeClass(
          id = "run_binding_analysis",
          class = "btn-highlight"
        )
      }
    })

    # Update UI on reset results event ----
    shiny::observeEvent(deconvolution_main_vars$continue_conversion(), {
      shiny::updateActionButton(
        session = session,
        "run_binding_analysis",
        label = "Run",
        icon = shiny::icon("play")
      )
      shinyjs::enable("peak_tolerance")
      shinyjs::enable("max_multiples")
      analysis_status("pending")
    })

    # Analysis run event ----

    ## Running analysis UI ----
    shiny::observeEvent(
      input$run_binding_analysis,
      {
        shiny::req(analysis_status() == "pending")

        shiny::showModal(
          shiny::div(
            class = "start-modal conversion-log-modal",
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
      },
      priority = 10
    )

    ## Conditional processing ----
    shiny::observeEvent(input$run_binding_analysis, {
      if (analysis_status() == "pending") {
        # Delay conversion start
        # Sys.sleep(1)

        # # Activate JS function for conversion process tracking
        # shinyjs::runjs(sprintf(
        #   conversion_tracking_js,
        #   ns("console_log"),
        #   ns("scroll_btn")
        # ))

        # # Add hits
        # withCallingHandlers(
        #   expr = {
        #     result_with_hits <- add_hits(
        #       conversion_main_vars$input_list()$result,
        #       sample_table = conversion_main_vars$input_list()$Samples_Table,
        #       protein_table = conversion_main_vars$input_list()$Protein_Table,
        #       compound_table = conversion_main_vars$input_list()$Compound_Table,
        #       peak_tolerance = input$peak_tolerance,
        #       max_multiples = input$max_multiples,
        #       session = session,
        #       ns = ns
        #     )

        #     result_with_hits1 <<- result_with_hits

        #     # If Ki/kinact analysis is set to be performed
        #     if (input$run_ki_kinact) {
        #       result_with_hits$hits_summary <- summarize_hits(
        #         result_with_hits,
        #         conc_time = conversion_main_vars$input_list()$ConcTime_Table
        #       )

        #       # Add binding/kobs results to result list
        #       result_with_hits$binding_kobs_result <- add_kobs_binding_result(
        #         result_with_hits
        #       )

        #       # Add Ki/kinact results to result list
        #       result_with_hits$ki_kinact_result <- add_ki_kinact_result(
        #         result_with_hits
        #       )
        #     } else {
        #       # Only protein conversion without Ki/kinact analysus
        #       result_with_hits$hits_summary <- summarize_hits(
        #         result_with_hits,
        #         conc_time = NULL
        #       )
        #     }
        #   },
        #   message = function(m) {
        #     clean_msg <- gsub("\\", "\\\\", m$message, fixed = TRUE)
        #     clean_msg <- gsub("'", "\\'", clean_msg, fixed = TRUE)
        #     clean_msg <- gsub("\n", "<br>", clean_msg, fixed = TRUE)

        #     js_cmd <- sprintf(
        #       "
        #     var el = document.getElementById('%s');
        #     if (el) {
        #       el.innerHTML += '%s';
        #       el.doAutoScroll();
        #     }
        #       ",
        #       ns("console_log"),
        #       clean_msg
        #     )

        #     shinyjs::runjs(js_cmd)
        #   }
        # )

        # # Assign result list and hits table to reactive vars
        # result_list(result_with_hits)

        # TODO
        # Dev Mode

        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits1.rds"
        # ))
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_7.rds"
        # ))
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_13.rds"
        # ))
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_19.rds"
        # ))
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_25.rds"
        # ))
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_33.rds"
        # ))
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_42.rds"
        # ))
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_55.rds"
        # ))
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\result_with_hits_61.rds"
        # ))

        ### TESTING

        # Single entry
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\one_entry.rds"
        # ))

        # NA entry
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\one_entryNA.rds"
        # ))

        # Two NA entries
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\two_entryNA.rds"
        # ))

        # NA diff
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\NA_diff.rds"
        # ))

        # NA diff2
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\NA_diff2.rds"
        # ))

        # HiDrive-kinact-K
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\HiDrive-kinact-K.rds"
        # ))

        # 2025-12-10_MS_in-house_protein
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\2025-12-10_MS_in-house_protein.rds"
        # ))

        # HiDrive-kinact-KI Testdaten
        result_list(readRDS(
          "C:\\Users\\Marian\\Desktop\\KF_Testing\\results.rds"
        ))

        # HiDrive-2025-09-04_New-Test-data
        # result_list(readRDS(
        #   "C:\\Users\\Marian\\Desktop\\KF_Testing\\results_conversion.rds"
        # ))

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

        shinyjs::removeClass(
          selector = "#app-conversion_sidebar-analysis_select > div > div:nth-child(1)",
          class = "custom-disable"
        )
        if (input$run_ki_kinact) {
          shinyjs::removeClass(
            selector = "#app-conversion_sidebar-analysis_select > div > div:nth-child(2)",
            class = "custom-disable"
          )
        }

        analysis_status("done")
      } else {
        result_list(NULL)
        analysis_status("pending")

        shinyjs::addClass(
          selector = "#app-conversion_sidebar-analysis_select > div > div:nth-child(1)",
          class = "custom-disable"
        )
        if (input$run_ki_kinact) {
          shinyjs::addClass(
            selector = "#app-conversion_sidebar-analysis_select > div > div:nth-child(2)",
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
    })

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
        # This script runs the moment this UI is added to the page
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
    shiny::observeEvent(input$save_conversion_log, {
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
    })

    # Analysis select UI conditional rendering ----
    ## Toggle classes ----
    shiny::observe({
      shiny::req(input$analysis_select)

      # Block UI
      shinyjs::runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "block";'
      ))

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
    })

    # shiny::observeEvent(input$run_binding_analysis, {
    #   shinyjs::addClass(
    #     selector = ".complex-picker .form-group .bootstrap-select",
    #     class = "custom-disable"
    #   )
    #   shinyjs::removeClass(
    #     id = "complex-picker-connector",
    #     class = "complex-picker-connector-color",
    #     asis = TRUE
    #   )
    # })

    ## Initial disable of analysis select input ----
    shinyjs::addClass(
      selector = "#app-conversion_sidebar-analysis_select > div > div:nth-child(1)",
      class = "custom-disable"
    )
    shinyjs::addClass(
      selector = "#app-conversion_sidebar-analysis_select > div > div:nth-child(2)",
      class = "custom-disable"
    )

    # TODO Info / Hints ----
    ## Info UI ----
    output$conversion_info_ui <- shiny::renderUI({
      shiny::req(conversion_main_vars$selected_tab())

      if (conversion_main_vars$selected_tab() == "Proteins") {
        hints <- shiny::HTML(
          "Upload a CSV|TSV|TXT|Excel file or manually enter names and mass values of the <strong>proteins</strong> into the table."
        )
      } else if (conversion_main_vars$selected_tab() == "Compounds") {
        hints <- shiny::HTML(
          "Upload a CSV|TSV|TXT|Excel file or manually enter names and mass values of the <strong>compounds</strong> into the table."
        )
      } else if (conversion_main_vars$selected_tab() == "Samples") {
        hints <- shiny::HTML(
          "Assign <strong>protein-compound complexes</strong> to deconvoluted samples."
        )
      } else if (conversion_main_vars$selected_tab() == "Binding") {
        hints <- shiny::HTML(
          "Global fit of a concentration series of binding curves determining binding parameters for the selected complex."
        )
      } else if (conversion_main_vars$selected_tab() == "Hits") {
        hints <- shiny::HTML(
          "The 'Hits' tab shows all signals assigned to the currently selected complex and respectively inferred parameters."
        )
      } else {
        hints <- "%-Binding inferred from time series measurements of a single concentration."
      }

      shiny::div(
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "conversion-sidebar-title",
              shiny::uiOutput(ns("selected_module"))
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "instruction-info",
              hints
            )
          )
        )
      )
    })

    ## Info title ----
    output$selected_module <- shiny::renderUI({
      shiny::req(conversion_main_vars$selected_tab())

      if (
        conversion_main_vars$selected_tab() %in%
          c("Proteins", "Compounds", "Samples")
      ) {
        title_add <- "Declaration"
      } else if (
        conversion_main_vars$selected_tab() %in% c("Binding", "Hits")
      ) {
        title_add <- ""
      } else {
        title_add <- "Concentration"
      }

      shiny::div(
        class = "sidebar-title conversion-title",
        paste(conversion_main_vars$selected_tab(), title_add),
        shiny::div(
          class = "tooltip-bttn",
          shiny::actionButton(
            ns("sidebar_tooltip_bttn"),
            label = "",
            icon = shiny::icon("circle-question")
          )
        )
      )
    })

    # Tooltips ----
    shiny::observeEvent(input$sidebar_tooltip_bttn, {
      if (conversion_main_vars$selected_tab() == "Proteins") {
        ## Protein declaration ----
        title <- "Protein Declaration"
        hints <- shiny::column(
          width = 12,
          shiny::div(
            class = "tooltip-text",
            "One or more proteins can be screened for. The protein names/IDs together with their Mw values [Da] can be defined either via file upload or by entering the values into the table. The table also supports copy/paste for efficient filling."
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
      } else if (conversion_main_vars$selected_tab() == "Compounds") {
        ## Compound declaration ----
        title <- "Compound Declaration"
        hints <- shiny::column(
          width = 12,
          shiny::div(
            class = "tooltip-text",
            "One or more compounds can be screened for. The compound names/IDs together with their Mw values [Da] can be defined either via file upload or by entering the values into the table. The table also supports copy/paste for efficient filling."
          ),
          shiny::br(),
          shiny::div(
            class = "tooltip-img-text",
            "The format requires the name/ID as first column and up to nine columns the theoretical mass as well as any mass shifts per compound. Headers are optional."
          ),
          shiny::br(),
          shiny::tags$img(
            src = "static/compound_table.png"
          ),
          shiny::br()
        )
      } else if (conversion_main_vars$selected_tab() == "Samples") {
        ## Sample declaration ----
        title <- "Samples Declaration"
        hints <- shiny::column(
          width = 12,
          shiny::div(
            class = "tooltip-text",
            "One or more compounds can be screened for. The compound names/IDs together with their Mw values [Da] can be defined either via file upload or by entering the values into the table. The table also supports copy/paste for efficient filling."
          ),
          shiny::br(),
          shiny::div(
            class = "tooltip-img-text",
            "The format requires the name/ID as first column and up to nine columns the theoretical mass as well as any mass shifts per compound. Headers are optional."
          ),
          shiny::br(),
          shiny::tags$img(
            src = "static/compound_table.png"
          ),
          shiny::br()
        )
      } else if (conversion_main_vars$selected_tab() == "Binding") {
        ## Binding analysis ----
        title <- "Binding Analysis"
        hints <- shiny::column(
          width = 12,
          shiny::withMathJax(),
          shiny::div(
            class = "tooltip-text",
            shiny::p(
              "This tab displays the complete concentration series for a single compound, enabling global determination of the two fundamental covalent binding parameters: ",
              shiny::strong("k", htmltools::tags$sub("inact")),
              " (maximum inactivation rate at saturation) and ",
              shiny::strong("K", htmltools::tags$sub("i")),
              " (apparent dissociation constant of the initial reversible complex)."
            ),
            shiny::p(
              "Individual time-courses are fitted to extract k",
              htmltools::tags$sub("obs"),
              " values, which are then globally fitted to the hyperbolic two-step model to yield the second-order rate constant ",
              shiny::strong(
                "k",
                htmltools::tags$sub("inact"),
                " / K",
                htmltools::tags$sub("i")
              ),
              " — the gold-standard metric of covalent binder efficiency at low occupancy."
            )
          )
        )
      } else if (conversion_main_vars$selected_tab() == "Hits") {
        ## Hits table ----
        title <- "Hits Table"
        hints <- shiny::fluidRow(
          shiny::br(),
          shiny::column(
            width = 11,
            shiny::div(
              class = "tooltip-text",
              shiny::p(
                "The hits table lists all peak signals that correspond to the declared proteins and compounds with respect to their molecular weights including mass shifts and multiple binding (stoichiometry). Signals that fall within the user-determined peak tolerance values are considered."
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
      } else {
        ## Concentration time series ----
        title <- "Single Concentration Time Series"
        hints <- "Binding parameters derived from mass spectra of time series measurements of a single concentration."
      }

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
    })

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
