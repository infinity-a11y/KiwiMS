# app/view/deconvolution_sidebar.R

box::use(
  fs[path_home],
  shiny[actionButton, br, h6, icon, observeEvent, moduleServer, NS, reactive,
        renderPrint, renderUI, uiOutput, verbatimTextOutput, validate, need],
  shinyFiles[parseDirPath, shinyDirButton, shinyDirChoose],
  bslib[sidebar],
)

box::use(
  app/logic/deconvolution_functions[deconvolute],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  sidebar(
    title = "File Upload",
    shinyDirButton(
      ns("folder"),
      "Select folder(s)",
      icon = icon("folder-open"),
      title = "Select Waters .raw Folder(s)",
      buttonType = "default",
      root = path_home()
    ),
    h6("Selected directory:"),
    verbatimTextOutput(ns("path_selected")),
    br(),
    uiOutput(ns("deconvolute_start_ui"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Define roots for directory browsing
    roots <- c(
      Home = path_home(),
      C = "C:/",
      D = "D:/" 
    )
    
    # Initialize directory selection
    shinyDirChoose(
      input,
      id = "folder",
      roots = roots,
      defaultRoot = "Home",
      session = session
    )
    
    waters_dir <- reactive({
      validate(need(input$folder, "Nothing selected"))
      parseDirPath(roots, input$folder)
    })
    
    output$path_selected <- renderPrint({
      if (!is.null(waters_dir()) && length(waters_dir()) > 0) {
        waters_dir()
      } else {
        cat("Nothing selected")
      }
    })
    
    output$deconvolute_start_ui <- renderUI({
      if (!is.null(waters_dir()) && length(waters_dir()) > 0) {
        actionButton(ns("deconvolute_start"), "Run Deconvolution")
      }
    })
    
    observeEvent(input$deconvolute_start, {
      deconvolute(waters_dir = waters_dir(), 
                  py_script = file.path(getwd(), "app/logic/run_unidec.py"))
    })
    
    return(waters_dir)
  })
}
