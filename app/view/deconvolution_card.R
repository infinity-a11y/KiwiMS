# app/view/deconvolution_card.R

box::use(
  shiny[NS, reactive, renderPlot, need, validate, plotOutput, moduleServer],
)

box::use(
  app/logic/deconvolution_functions[plot_ms_spec],
  app/view/deconvolution_sidebar,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  shiny::fluidRow(
    shiny::uiOutput(ns("deconvolute_start_ui")),
    plotOutput(ns("ms_spectrum")) 
  )
}

#' @export
server <- function(id, waters_dir) {
  moduleServer(id, function(input, output, session) {
    
    rslt_dir <- reactive({gsub(".raw", "_unidecfiles", waters_dir())})
    
    output$ms_spectrum <- renderPlot({
      validate(need(dir.exists(rslt_dir()), "No result file available."))
      
      plot_ms_spec(rslt_dir())
    })
  })
}

