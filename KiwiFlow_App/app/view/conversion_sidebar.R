# app/view/upload_spectra.R

box::use(
  bslib[card, card_body, card_header, sidebar],
  shiny[actionButton, fileInput, NS, textInput, moduleServer, reactive],
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
      )
    )
  )
}

#' @export
server <- function(id, selected_tab, set_selected_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$module_sidebar <- shiny::renderUI({
      shiny::req(selected_tab())

      if (selected_tab() == "Proteins") {
        file_select_ui <- fileInput(
          ns("proteins_fileinput"),
          "Select File",
          multiple = FALSE,
          accept = c(".csv", ".tsv", ".xlsx", ".xls", ".txt")
        )

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
        file_select_ui <- fileInput(
          ns("compounds_fileinput"),
          "Select File",
          multiple = FALSE,
          accept = c(".csv", ".tsv", ".xlsx", ".xls", ".txt")
        )

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
        file_select_ui <- fileInput(
          ns("result_input"),
          "Select File",
          multiple = FALSE,
          accept = c(".rds")
        )

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
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "file-select-nomargin",
              file_select_ui
            )
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

    # Reactive value for uploaded result list path
    shiny::reactiveValues(
      result = shiny::reactive(input$result_input$datapath)
    )
  })
}
