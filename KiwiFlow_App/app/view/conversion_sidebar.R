# app/view/upload_spectra.R

box::use(
  bslib[card, card_body, card_header, sidebar],
  shiny[actionButton, fileInput, NS, textInput, moduleServer, reactive],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  sidebar(
    shinyjs::useShinyjs(),
    shiny::div(
      class = "flowchart-container",
      shiny::div(
        id = ns("module_Proteins_box"),
        class = "module-box not-done",
        "Proteins"
      ),
      shiny::div(class = "arrow"),
      shiny::div(
        id = ns("module_Compounds_box"),
        class = "module-box not-done",
        "Compounds"
      ),
      shiny::div(class = "arrow"),
      shiny::div(
        id = ns("module_Samples_box"),
        class = "module-box not-done",
        "Samples"
      ),
    ),
    shiny::hr(),
    shiny::uiOutput(ns("module_sidebar"))
  )
}

#' @export
server <- function(id, selected_tab, set_selected_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # On button click, switch to Tab 2
    shiny::observeEvent(input$confirm_module, {
      if (selected_tab() == "Proteins") {
        set_selected_tab("Compounds")
      } else if (selected_tab() == "Compounds") {
        set_selected_tab("Samples")
      }
    })

    # Handle clicks on module boxes to switch tabs
    shinyjs::onclick(id = "module_Proteins_box", {
      set_selected_tab("Proteins")
    })
    shinyjs::onclick(id = "module_Compounds_box", {
      set_selected_tab("Compounds")
    })
    shinyjs::onclick(id = "module_Samples_box", {
      set_selected_tab("Samples")
    })

    # Toggle class on button click
    shiny::observeEvent(input$confirm_module, {
      shiny::req(selected_tab())

      id_module <- paste0("module_", selected_tab(), "_box")

      shinyjs::toggleClass(id = id_module, class = "done")
      shinyjs::toggleClass(id = id_module, class = "not-done")
    })

    shiny::reactiveValues(
      result = shiny::reactive(input$result_input$datapath)
    )

    output$module_sidebar <- shiny::renderUI({
      if (selected_tab() == "Proteins") {}

      shiny::div(
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::textOutput(ns("selected_module"))
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            fileInput(
              ns("result_input"),
              "Select Results File",
              multiple = FALSE,
              accept = c(".rds")
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::actionButton(
              ns("confirm_module"),
              label = "Confirm",
              icon = shiny::icon("check")
            )
          ),
          shiny::column(
            width = 6,
            shiny::actionButton(
              ns("edit_module"),
              label = "Edit",
              icon = shiny::icon("pen-to-square")
            )
          )
        )
      )
    })

    output$selected_module <- shiny::renderText({
      shiny::req(selected_tab())

      paste(selected_tab(), "Declaration")
    })
  })
}
