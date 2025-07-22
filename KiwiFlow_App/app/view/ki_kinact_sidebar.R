# app/view/ki_kinact.R

box::use(
  bslib[sidebar],
  shiny[actionButton, br, selectInput, textInput, NS],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  sidebar(
    title = "Parameter Settings",
    selectInput(
      ns("units"),
      "Select units for Kobs/KI calculations",
      choices = c("\U003BCM - minutes", "M - seconds"),
      selected = "\U003BCM - minutes"
    ),
    br(),
    actionButton(ns("run_ki"), "Calculate KI/Kinact")
  )
}
