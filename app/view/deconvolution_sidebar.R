# app/view/deconvolution_sidebar.R

box::use(
  bslib[sidebar],
  fs[path_home],
  shiny[column, div, fluidRow, h6, NS, moduleServer, reactive, reactiveValues],
  shinyFiles[parseDirPath, parseFilePaths, shinyDirButton, shinyDirChoose],
  shinyjs[enable, disable, disabled],
  shinyWidgets[radioGroupButtons],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  sidebar(
    title = "File Upload",
    h6(
      "Select Deconvolution Mode",
      style = "color: RGBA(var(--bs-emphasis-color-rgb, 0, 0, 0), 0.5); font-style: italic;"
    ),
    radioGroupButtons(
      ns("deconvolution_mode"),
      "",
      c("Multiple", "Single")
    ),
    shiny::hr(style = "margin: 0.5rem 0; opacity: 0.8;"),
    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'Multiple'",
        ns("deconvolution_mode")
      ),
      shinyDirButton(
        ns("folder"),
        "Select Root Folder",
        icon = shiny::icon("folder-open"),
        title = "Select Folder",
        buttonType = "default",
        root = path_home()
      ),
      shiny::verbatimTextOutput(ns("path_selected")),
      shiny::uiOutput(ns("root_dir_check")),
      shiny::checkboxInput(
        ns("batch_mode"),
        "Batch Processing Mode",
        value = FALSE
      ),
      shiny::conditionalPanel(
        condition = sprintf(
          "input['%s'] == true",
          ns("batch_mode")
        ),
        shiny::uiOutput(ns("batch_file_ui")),
        shiny::uiOutput(ns("batch_id_col_ui")),
        shiny::uiOutput(ns("batch_vial_col_ui"))
      )
    ),
    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'Single'",
        ns("deconvolution_mode")
      ),
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
    shinyDirChoose(
      input,
      id = "folder",
      roots = roots,
      defaultRoot = "Home",
      session = session
    )

    # Initialize individual file selection
    shinyDirChoose(
      input,
      id = "file",
      roots = roots,
      defaultRoot = "Home",
      session = session
    )

    # Get selected paths
    root_dir <- reactive({
      if (is.null(input$folder)) {
        character()
      } else {
        parseDirPath(roots, input$folder)
      }
    })

    file_path <- reactive({
      if (is.null(input$file)) {
        character()
      } else {
        parseDirPath(roots, input$file)
      }
    })

    batch_file <- reactive({
      if (is.null(input$batch_selection)) {
        character()
      } else {
        file_path <- file.path(
          dirname(input$batch_selection$datapath),
          basename(input$batch_selection$datapath)
        )
        utils::read.csv(file_path)
      }
    })

    # Collection of reactive vars
    rootdir <- shiny::reactiveVal(character())
    filepath <- shiny::reactiveVal(character())
    batchfile <- shiny::reactiveVal(character())

    shiny::observe({
      filepath(file_path())
      rootdir(root_dir())
      batchfile(batch_file())
    })

    output$root_dir_check <- shiny::renderUI({
      if (!is.null(root_dir()) && length(root_dir()) > 0) {
        raw_dirs <- list.dirs(
          root_dir(),
          full.names = TRUE,
          recursive = FALSE
        )
        raw_dirs <- raw_dirs[grep("\\.raw$", raw_dirs)]

        if (length(raw_dirs)) {
          enable(selector = "#app-deconvolution_pars-batch_mode")

          shiny::p(
            shiny::HTML(
              paste0(
                '<i class="fa-solid fa-circle-check" style="font-size:1em; color:#8BC34A; margin-right: 10px;"></i>',
                paste(
                  "<b>",
                  length(raw_dirs),
                  "</b> .raw directories in directory"
                )
              )
            )
          )
        } else {
          shiny::updateCheckboxInput(session, "batch_mode", value = FALSE)
          disable(selector = "#app-deconvolution_pars-batch_mode")

          shiny::p(
            shiny::HTML(
              paste0(
                '<i class="fa-solid fa-circle-exclamation" style="font-size:1em; color:black; margin-right: 10px;"></i>',
                "<b>No</b> .raw directories in directory"
              )
            )
          )
        }
      } else {
        shiny::updateCheckboxInput(session, "batch_mode", value = FALSE)
        disable(selector = "#app-deconvolution_pars-batch_mode")

        shiny::p(
          shiny::HTML(
            "Select directory containing .raw result folders"
          )
        )
      }
    })

    # Render batch selection input element
    batch_selection <- div(
      class = "batch-file",
      shiny::fileInput(
        ns("batch_selection"),
        "Select Batch File",
        accept = c(".csv", ".xlsx")
      )
    )

    output$batch_file_ui <- shiny::renderUI(
      batch_selection
    )

    # File selection feedback
    output$file_selected <- shiny::renderPrint(cat("Nothing selected"))
    output$path_selected <- shiny::renderPrint(cat("Nothing selected"))

    shiny::observeEvent(input$file, {
      selected("file")
      output$batch_file_ui <- shiny::renderUI(
        batch_selection
      )
      rootdir(character())

      # Adjust UI elements
      output$path_selected <- shiny::renderPrint(cat("Nothing selected"))
      shinyjs::reset("batch_selection")
      shiny::updateSelectInput(
        session,
        "vial_column",
        choices = "",
        selected = ""
      )
      shinyjs::disable("vial_column")
      shiny::updateSelectInput(
        session,
        "id_column",
        choices = "",
        selected = ""
      )
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
      filepath(character())
      output$batch_file_ui <- shiny::renderUI(
        batch_selection
      )

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
      if (!is.null(input$batch_selection)) {
        batch <- batch_file()
        choices <- colnames(batch)
        select <- shiny::selectInput(ns("id_column"), "", choices = choices)
      } else {
        select <- disabled(shiny::selectInput(
          ns("id_column"),
          "",
          choices = ""
        ))
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
    })

    output$batch_vial_col_ui <- shiny::renderUI({
      tryCatch(
        {
          if (!is.null(input$batch_selection)) {
            batch <- batch_file()
            choices <- colnames(batch)[colnames(batch) != input$id_column]
            select <- shiny::selectInput(
              ns("vial_column"),
              "",
              choices = choices
            )
          } else {
            select <- disabled(shiny::selectInput(
              ns("vial_column"),
              "",
              choices = ""
            ))
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
        },
        error = function(e) {
          NULL
        }
      )
    })

    vial_column_reactive <- reactive({
      input$vial_column
    })

    id_column_reactive <- reactive({
      input$id_column
    })

    # Return paths
    reactiveValues(
      dir = rootdir,
      file = filepath,
      batch_file = batchfile,
      selected = selected,
      id_column = id_column_reactive,
      vial_column = vial_column_reactive
    )
  })
}
