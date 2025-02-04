# app/view/upload_spectra.R

box::use(
  bslib[card, card_body, card_header, sidebar],
  shiny[actionButton, br, fileInput, textInput, NS],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  sidebar(
    title = "File Upload",
    fileInput(
      ns("raw_input"),
      "Select Input File",
      multiple = FALSE,
      accept = c(".txt", ".tab")),
    fileInput(
      ns("mass_input"),
      "Select ExpMW File",
      multiple = FALSE,
      accept = c("text/tab-separated-values")),
    textInput(ns("protein_mass"), "Protein Mass", ""),
    card(
      card_header(
        class = "bg-dark",
        "Set noise level (+/-)"
      ),
      card_body(
        textInput(ns("cmpd_label"), "Compound Labeling", "4"),
        textInput(ns("prot_peak"), "Protein Peak", "10") 
      )
    ),
    card(
      card_header(
        class = "bg-dark",
        "Customize"
      ),
      card_body(
        textInput(ns("n_label"), "Considered number of labeling", "4"),
        textInput(ns("n_spectra"), "Considered number of spectra", "20")
      )
    ),
    actionButton(ns("run_conversion_function"), "Calculate Conversions")
  )
}