# app/view/upload_spectra.R

box::use(
  bslib[card, card_body, card_header, sidebar],
  shiny[actionButton, fileInput, NS, textInput, moduleServer, reactive],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  sidebar(
    title = "File Upload",
    fileInput(
      ns("result_input"),
      "Select Results File",
      multiple = FALSE,
      accept = c(".rds")
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::reactiveValues(
      result = reactive(input$result_input$datapath)
    )
  })
}
