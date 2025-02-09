# app/view/deconvolution_card.R

box::use(
  shiny,
  shinyjs[disable, runjs]
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
    
    # get selected raw files directory
    raw_dir <- shiny$reactive({waters_dir()})
    
    # render deconvolute start button if directory selected
    output$deconvolute_start_ui <- shiny$renderUI({
      shiny$validate(
        shiny$need(dir.exists(raw_dir()), "No Waters .raw selected"))
      shiny$actionButton(ns("deconvolute_start"), "Run Deconvolution")
    })
    
    # execute deconvolution
    shiny$observeEvent(input$deconvolute_start, {
      disable(ns("deconvolute_start"))
      runjs('document.getElementById("blocking-overlay").style.display = "block";')
      
      deconvolute(parent_dir = raw_dir())
      
      runjs('document.getElementById("blocking-overlay").style.display = "none";')
    })
  })
}
