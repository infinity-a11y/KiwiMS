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
    shiny::uiOutput(ns("module_sidebar")),
    shiny::hr(style = "margin: 1em 0;"),
    shiny::div(
      class = "interaction-analysis-flex",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::div(
            class = "conversion-sidebar-title",
            "Interaction Analysis"
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
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
          )
        ),
        shiny::column(
          width = 6,
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
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          align = "center",
          #TODO
          # shinyjs::disabled(
          shiny::actionButton(
            ns("run_binding_analysis"),
            "Run Analysis"
          )
          # )
        )
      ),
      shiny::hr(style = "margin: 1em 0;"),
      shiny::uiOutput(ns("result_menu"))
    )
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

    # Render result menu
    output$result_menu <- shiny::renderUI({
      shiny::req(nrow(result_list()[["hits_summary"]]) > 0)

      shiny::fluidRow(
        shiny::column(
          width = 12,
          align = "center",
          shinyWidgets::pickerInput(
            ns("sample_picker"),
            "Sample",
            choices = c("Kinetics", result_list()$hits_summary$Sample)
          )
        )
      )
    })

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

      # Unblock UI
      shinyjs::runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "none";'
      ))
    })

    output$module_sidebar <- shiny::renderUI({
      shiny::req(selected_tab())

      if (selected_tab() == "Proteins") {
        hints <- shiny::div(
          class = "conversion-hint",
          "Upload a CSV|TSV|TXT|Excel file or manually enter names and mass values of the proteins into the table.",
          shiny::div(
            class = "tooltip-bttn",
            shiny::actionButton(
              ns("declaration_prot_tooltip_bttn"),
              label = "",
              icon = shiny::icon("circle-question")
            )
          )
        )
      } else if (selected_tab() == "Compounds") {
        hints <- shiny::div(
          class = "conversion-hint",
          "Upload a CSV|TSV|TXT|Excel file or manually enter names and mass values of the compounds into the table.",
          shiny::div(
            class = "tooltip-bttn",
            shiny::actionButton(
              ns("declaration_cmp_tooltip_bttn"),
              label = "",
              icon = shiny::icon("circle-question")
            )
          )
        )
      } else {
        hints <- shiny::div(class = "conversion-hint", "")
      }

      shiny::div(
        class = "file-selection-flex",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "conversion-sidebar-title",
              shiny::textOutput(ns("selected_module"))
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            hints
          )
        )
      )
    })

    output$selected_module <- shiny::renderText({
      shiny::req(selected_tab())

      paste(selected_tab(), "Declaration")
    })

    # Tooltip events
    shiny::observeEvent(input$declaration_prot_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "tip-modal",
          shiny::modalDialog(
            shiny::column(
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
            ),
            title = "Protein Input",
            easyClose = TRUE,
            footer = shiny::tagList(
              shiny::modalButton("Dismiss")
            )
          )
        )
      )
    })

    shiny::observeEvent(input$declaration_cmp_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "tip-modal",
          shiny::modalDialog(
            shiny::column(
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
            ),
            title = "Compound Input",
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
        sample_picker = shiny::reactive(input$sample_picker)
      )
    )
  })
}
