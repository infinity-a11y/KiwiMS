# app/view/deconvolution_sidebar.R

box::use(
  fs[path_home],
  shiny[column, div, fileInput, fluidRow, h6, NS, moduleServer, reactive,
        reactiveValues],
  shinyFiles[parseDirPath, shinyDirButton, shinyDirChoose, shinyFilesButton],
  bslib[card, card_body, card_header, sidebar],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  sidebar(
    title = "File Upload",
    shinyFilesButton(
      ns("files"),
      "Select Files(s)",
      multiple = TRUE,
      icon = shiny::icon("file"),
      title = "Select Waters .raw Folder(s)",
      buttonType = "default",
      root = path_home()
    ),
    shiny::verbatimTextOutput(ns("files_selected")),
    shiny::hr(),
    shinyDirButton(
      ns("folder"),
      "Select Root Folder",
      icon = shiny::icon("folder-open"),
      title = "Select Root Folder",
      buttonType = "default",
      root = path_home()
    ),
    shiny::verbatimTextOutput(ns("path_selected")),
    fileInput("batch_selection", "Select Batch File")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

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
      shiny::validate(shiny::need(input$folder, "Nothing selected"))
      parseDirPath(roots, input$folder)
    })

    output$path_selected <- shiny::renderPrint({
      if (!is.null(waters_dir()) && length(waters_dir()) > 0) {
        waters_dir()
      } else {
        cat("Nothing selected")
      }
    })

    reactiveValues(
      dir = waters_dir,
      config_startz = reactive(input$startz),
      config_endz = reactive(input$endz),
      config_minmz = reactive(input$minmz),
      config_maxmz = reactive(input$maxmz)
    )
  })
}
