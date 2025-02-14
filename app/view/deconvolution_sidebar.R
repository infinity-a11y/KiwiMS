# app/view/deconvolution_sidebar.R

box::use(
  bslib[card, card_body, card_header, sidebar],
  fs[path_home],
  readxl[read_excel],
  shiny[column, div, fluidRow, h6, NS, moduleServer, reactive, reactiveValues],
  shinyFiles[parseDirPath, parseFilePaths, shinyDirButton, shinyDirChoose],
  shinyjs[disabled]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  sidebar(
    title = "File Upload",
    h6("Multiple Target Files",
       style = paste0("font-weight: 700; margin-left: 1em; text-align: center;",
                      "margin-bottom: -5px;")),
    shinyDirButton(
      ns("folder"),
      "Select Root Folder",
      icon = shiny::icon("folder-open"),
      title = "Select Folder",
      buttonType = "default",
      root = path_home()
    ),
    shiny::verbatimTextOutput(ns("path_selected")),
    div(class = "batch-file",
        shiny::fileInput(ns("batch_selection"), "Select Batch File",
                         accept = c(".csv", ".xlsx"))),
    shiny::uiOutput(ns("batch_id_col_ui")),
    shiny::uiOutput(ns("batch_vial_col_ui")),
    shiny::hr(style = "margin: 1rem 0; opacity: 1;"),
    h6("Individual Target File",
       style = paste0("font-weight: 700; margin-left: 1em; text-align: center;",
                      "margin-bottom: -5px;")),
    shinyDirButton(
      ns("file"),
      "Select Single File",
      multiple = TRUE,
      icon = shiny::icon("file"),
      title = "Select Waters .raw Folder",
      buttonType = "default",
      root = path_home()
    ),
    shiny::verbatimTextOutput(ns("file_selected"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected <- shiny::reactiveVal("")

    # Define roots for directory browsing
    roots <- c(Home = path_home(), C = "C:/", D = "D:/")

    # Initialize root folder selection
    shinyDirChoose(input, id = "folder", roots = roots,
                   defaultRoot = "Home", session = session)

    # Initialize individual file selection
    shinyDirChoose(input, id = "file", roots = roots,
                   defaultRoot = "Home", session = session)

    # Get selected paths
    root_dir <- reactive({
      shiny::validate(shiny::need(input$folder, "Nothing selected"))
      parseDirPath(roots, input$folder)
    })
    file_path <- reactive({
      shiny::validate(shiny::need(input$file, "Nothing selected"))
      parseDirPath(roots, input$file)
    })
    batch_path <- reactive({
      input$batch_selection
    })

    # File selection feedback
    output$file_selected <- shiny::renderPrint(cat("Nothing selected"))
    output$path_selected <- shiny::renderPrint(cat("Nothing selected"))

    shiny::observeEvent(input$file, {
      selected("file")

      # Adjust UI elements
      output$path_selected <- shiny::renderPrint(cat("Nothing selected"))
      shinyjs::reset("batch_selection")
      shiny::updateSelectInput(session, "vial_column", choices = "",
                               selected = "")
      shinyjs::disable("vial_column")
      shiny::updateSelectInput(session, "id_column", choices = "",
                               selected = "")
      shinyjs::disable("id_column")

      output$file_selected <- shiny::renderPrint({
        input$file
        if (!is.null(file_path()) && length(file_path()) > 0) {
          file_path()
        } else {
          cat("Nothing selected")
        }
      })
    })
    shiny::observeEvent(input$folder, {
      selected("folder")

      # Adjust UI elements
      output$file_selected <- shiny::renderPrint(cat("Nothing selected"))

      output$path_selected <- shiny::renderPrint({
        input$folder
        if (!is.null(root_dir()) && length(root_dir()) > 0) {
          root_dir()
        } else {
          cat("Nothing selected")
        }
      })
    })

    shiny::observeEvent(input$batch_selection, {
      output$file_selected <- shiny::renderPrint(cat("Nothing selected"))
    })

    # Render batch column selection UI
    output$batch_id_col_ui <- shiny::renderUI({

      tryCatch({
        if (!is.null(input$batch_selection)) {

          file_path <- file.path(dirname(input$batch_selection$datapath),
                                 basename(input$batch_selection$datapath))
          batch <- read_excel(file_path)
          choices <- colnames(batch)
          select <- shiny::selectInput(ns("id_column"), "", choices = choices)
        } else {
          select <- disabled(shiny::selectInput(ns("id_column"), "",
                                                choices = ""))
        }

        fluidRow(
          column(
            width = 5,
            h6("Sample ID Column", style = "font-size: small")
          ),
          column(
            width = 7,
            div(class = "batch-select", select)
          )
        )
      }, error = function(e) {
        NULL
      })
    })

    output$batch_vial_col_ui <- shiny::renderUI({
      tryCatch({
        if (!is.null(input$batch_selection)) {
          file_path <- file.path(dirname(input$batch_selection$datapath),
                                 basename(input$batch_selection$datapath))
          batch <- read_excel(file_path)
          choices <- colnames(batch)[colnames(batch) != input$id_column]
          select <- shiny::selectInput(ns("vial_column"), "", choices = choices)
        } else {
          select <- disabled(shiny::selectInput(ns("vial_column"), "",
                                                choices = ""))
        }

        fluidRow(
          column(
            width = 5,
            h6("Vial Column", style = "font-size: small")
          ),
          column(
            width = 7,
            div(class = "batch-select", select)
          )
        )
      }, error = function(e) {
        NULL
      })
    })

    # Return paths
    reactiveValues(dir = root_dir, file = file_path, batch_file = batch_path,
                   selected = selected)
  })
}
