# app/view/conversion_card.R

box::use(
  DT[DTOutput, renderDataTable],
  rhino[rhinos],
  shiny[moduleServer, need, NS, validate],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  DTOutput(ns("conversion_table"))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$conversion_table <- renderDataTable({
      # placeholder dataset
      rhinos
      },
      filter = "top",
      style = "bootstrap",
      class = "table-bordered stripe",
      options = list(sDom = "B<\"top\">rt<\"bottom\">lip", scrollX = TRUE)
    )
  })
}
