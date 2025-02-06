# app/view/deconvolution_card.R

box::use(
  shiny,
)

box::use(
  app/logic/deconvolution_functions[deconvolute],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  
  shiny$fluidRow(
    shiny$column(
      width = 12,
      shiny$uiOutput(ns("deconvolute_start_ui"))
    )
  )
}

#' @export
server <- function(id, waters_dir) {
  shiny$moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    raw_dir <- shiny$reactive({waters_dir()})
    
    output$deconvolute_start_ui <- shiny$renderUI({
      shiny$validate(
        shiny$need(dir.exists(raw_dir()), "No Waters .raw selected"))
      shiny$actionButton(ns("deconvolute_start"), "Run Deconvolution")
    })
    
    shiny$observeEvent(input$deconvolute_start, {
      deconvolute(parent_dir = raw_dir(), 
                  py_script = file.path(getwd(), 
                                        "app/logic/run_unidec.py"))
    })
  })
}
