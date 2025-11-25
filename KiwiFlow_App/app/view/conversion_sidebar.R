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
    ],
  app /
    logic /
    logging[
      write_log,
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
server <- function(
  id,
  selected_tab,
  set_selected_tab,
  conversion_ready,
  input_list
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Declare reactive vars
    result_list <- shiny::reactiveVal(NULL)
    analysis_status <- shiny::reactiveVal("pending")

    # Render sidebar ui
    output$conversion_sidebar_ui <- shiny::renderUI({
      shiny::div(
        class = "conversion-sidebar-ui",
        shiny::uiOutput(ns("conversion_info_ui")),
        shiny::uiOutput(ns("conversion_analysis_controls_ui")),
        shiny::uiOutput(ns("conversion_result_controls_ui"))
      )
    })

    # Render conversion result controls UI
    output$conversion_result_controls_ui <- shiny::renderUI({
      # shiny::req(nrow(result_list()[["hits_summary"]]) > 0)

      # List of protein-compound-complexes
      complexes <- list(
        "Global Overview",
        "COOB" = paste0("Cmp-", 30:40),
        "KRAS" = paste0("ALMP3kALMP3kALMP3k-", 1:5)
      )

      # shinyjs::disabled(
      picker_ui <- shinyWidgets::pickerInput(
        ns("sample_picker"),
        shiny::div(
          class = "label-tooltip",
          shiny::tags$label("Complexes"),
          shiny::div(
            class = "tooltip-bttn",
            shiny::actionButton(
              ns("max_mult_tooltip_bttn"),
              label = "",
              icon = shiny::icon("circle-question")
            )
          )
        ),
        choices = complexes
      )
      # )

      shiny::div(
        class = "interaction-analysis-flex",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "sidebar-title conversion-title",
              "Results"
            ),
            picker_ui,
            shiny::actionButton(
              ns("report_conversion_results"),
              "Report",
              icon = shiny::icon("square-poll-vertical"),
              width = "100%"
            )
          )
        )
      )
    })

    # Render conversion analysis controls UI
    output$conversion_analysis_controls_ui <- shiny::renderUI(
      shiny::div(
        class = "interaction-analysis-flex",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "sidebar-title conversion-title",
              "Analysis"
            ),
            shinyjs::disabled(
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
              )
            ),
            shinyjs::disabled(
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
              )
            ),
            #TODO
            # shinyjs::disabled(
            shiny::actionButton(
              ns("run_binding_analysis"),
              "Run",
              icon = shiny::icon("play"),
              width = "100%"
            ),
            # )
          )
        )
      )
    )

    # Enable/Disable conversion parameter and launch input UI
    shiny::observe({
      shiny::req(conversion_ready(), input_list())

      shinyjs::toggleState("max_multiples", conversion_ready())
      shinyjs::toggleState("peak_tolerance", conversion_ready())
      shinyjs::toggleState("run_binding_analysis", conversion_ready())
      shinyjs::toggleClass(
        "run_binding_analysis",
        "btn-highlight",
        conversion_ready()
      )
    })

    # Event run conversion
    shiny::observeEvent(input$run_binding_analysis, {
      # shiny::req(input_list(), input$peak_tolerance, input$max_multiples)

      # Block UI
      shinyjs::runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "block";'
      ))

      if (analysis_status() == "pending") {
        # # Search and add hits to result list
        # result_with_hits <- add_hits(
        #   input_list()$result,
        #   sample_table = input_list()$Samples_Table,
        #   protein_table = input_list()$Protein_Table,
        #   compound_table = input_list()$Compound_Table,
        #   peak_tolerance = input$peak_tolerance,
        #   max_multiples = input$max_multiples
        # )

        # # Add summarized hits table to result list
        # result_with_hits$hits_summary <- summarize_hits(result_with_hits)

        # result_with_hits <<- result_with_hits

        # # Add binding/kobs results to result list
        # result_with_hits$binding_kobs_result <- add_kobs_binding_result(
        #   result_with_hits
        # )

        # # Add Ki/kinact results to result list
        # result_with_hits$ki_kinact_result <- add_ki_kinact_result(
        #   result_with_hits
        # )

        # # Assign result list and hits table to reactive vars
        # result_list(result_with_hits)

        # results <<- result_with_hits
        # TODO
        # Dev Mode
        result_list(readRDS(
          "C:\\Users\\Marian\\Desktop\\KF_Testing\\results.rds"
        ))

        shiny::updateActionButton(
          session = session,
          "run_binding_analysis",
          label = "Reset",
          icon = shiny::icon("repeat")
        )

        analysis_status("done")
      } else {
        result_list(NULL)
        analysis_status("pending")

        shiny::updateActionButton(
          session = session,
          "run_binding_analysis",
          label = "Run",
          icon = shiny::icon("play"),
          disabled = TRUE
        )
      }

      # Unblock UI
      shinyjs::runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "none";'
      ))
    })

    output$conversion_info_ui <- shiny::renderUI({
      shiny::req(selected_tab())

      if (selected_tab() == "Proteins") {
        hints <- shiny::HTML(
          "Upload a CSV|TSV|TXT|Excel file or manually enter names and mass values of the <strong>proteins</strong> into the table."
        )
      } else if (selected_tab() == "Compounds") {
        hints <- shiny::HTML(
          "Upload a CSV|TSV|TXT|Excel file or manually enter names and mass values of the <strong>compounds</strong> into the table."
        )
      } else if (selected_tab() == "Samples") {
        hints <- shiny::HTML(
          "Assign <strong>protein-compound complexes</strong> and analysis parameters to the deconvoluted samples. Continue with the results from the previous deconvolution or upload a .rds results file."
        )
      } else if (selected_tab() == "Binding") {
        hints <- shiny::div()
      } else if (selected_tab() == "Hits") {
        hints <- ""
      } else {
        hints <- ""
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

    output$selected_module <- shiny::renderUI({
      shiny::req(selected_tab())

      title_add <- ifelse(
        selected_tab() %in% c("Proteins", "Compounds", "Samples"),
        "Declaration",
        ""
      )

      shiny::div(
        class = "sidebar-title conversion-title",
        paste(selected_tab(), title_add),
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

    # Tooltip events
    shiny::observeEvent(input$sidebar_tooltip_bttn, {
      test <<- selected_tab()
      if (selected_tab() == "Proteins") {
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
      } else if (selected_tab() == "Compounds") {
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
      } else if (selected_tab() == "Samples") {
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
      } else if (selected_tab() == "Binding") {
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
      } else if (selected_tab() == "Hits") {
        hints <- shiny::column(
          width = 12
        )
      } else {
        hints <- shiny::column(
          width = 12
        )
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

    # Server return values
    return(
      shiny::reactiveValues(
        result_list = shiny::reactive(result_list()),
        sample_picker = shiny::reactive(input$sample_picker),
        run_analysis = shiny::reactive(input$run_binding_analysis)
      )
    )
  })
}
