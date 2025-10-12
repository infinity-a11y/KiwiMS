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
    ]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bslib::sidebar(
    class = "conversion-sidebar",
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
          shinyjs::disabled(
            shiny::actionButton(
              ns("run_binding_analysis"),
              "Run Analysis",
              icon = shiny::icon("play")
            )
          )
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

    # Render result menu
    output$result_menu <- shiny::renderUI({
      shiny::req(result_hits_test(), hits())

      shiny::fluidRow(
        shiny::column(
          width = 12,
          align = "center",
          shinyWidgets::pickerInput(
            ns("sample_picker"),
            "Sample",
            choices = names(result_hits_test())
          )
        )
      )
    })

    shiny::observe({
      shiny::req(conversion_ready(), input_list())

      shinyjs::toggleState("run_binding_analysis", conversion_ready())
    })

    result_hits_test <- shiny::reactiveVal(NULL)
    hits <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run_binding_analysis, {
      shiny::req(input_list())

      result_with_hits <- add_hits(
        input_list()$result,
        protein_table = input_list()$Protein_Table,
        compound_table = input_list()$Compound_Table,
        peak_tolerance = 3,
        max_multiples = 4
      )
      compound_table <<- input_list()$Compound_Table
      protein_table <<- input_list()$Protein_Table
      result_hits <<- result_with_hits

      result_hits_test(result_with_hits)

      hits(summarize_hits(result_with_hits))
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
      } else if (selected_tab() == "Samples") {
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
            width = 12
            # ,hints
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
            title = "Peak Tolerance",
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
            title = "Peak Tolerance",
            easyClose = TRUE,
            footer = shiny::tagList(
              shiny::modalButton("Dismiss")
            )
          )
        )
      )
    })

    # Rückgabe des reactiveValues-Objekts, damit es an den übergeordneten Server übergeben werden kann
    return(
      shiny::reactiveValues(
        run_conversion = shiny::reactive(input$run_binding_analysis),
        result_hits = shiny::reactive(result_hits_test()),
        hits = shiny::reactive(hits()),
        sample_picker = shiny::reactive(input$sample_picker)
      )
    )
  })
}
