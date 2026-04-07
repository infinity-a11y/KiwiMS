# app/view/deconvolution_sidebar.R

box::use(
  bslib[sidebar, tooltip],
  fs[path_home],
  shiny[
    actionButton,
    checkboxInput,
    div,
    hr,
    icon,
    moduleServer,
    NS,
    reactive,
    reactiveValues,
    renderUI,
    uiOutput,
  ],
  shinyFiles[parseDirPath, shinyDirButton, shinyDirChoose],
  shinyjs[disable, disabled, enable, runjs],
  shinyWidgets[radioGroupButtons],
)

box::use(
  app / logic / helper_functions[config_badge, get_volumes],
  app / logic / logging[get_log],
)


#' @export
ui <- function(id) {
  ns <- NS(id)

  sidebar(
    class = "deconvolution-sidebar",
    width = "23rem",
    div(
      class = "deconvolution-sidebar-ui",

      # --- Section 1: File Selection ---
      div(
        class = "deconvolution-section",
        div(class = "sidebar-title custom-sidebar-title", "Select Files"),
        shiny::uiOutput(ns("dir_check")),
        shiny::tags$div(
          style = "display: flex; gap: 0.5em;",
          shinyDirButton(
            ns("folder"),
            "Multiple",
            icon = shiny::icon("folder-open"),
            title = "Select location with multiple Waters .raw folders",
            buttonType = "default",
            root = path_home()
          ),
          shinyDirButton(
            ns("file"),
            "Single",
            multiple = TRUE,
            icon = shiny::icon("file"),
            title = "Select individual Waters .raw folder",
            buttonType = "default",
            root = path_home()
          )
        ),
        shiny::verbatimTextOutput(ns("path_selected")),
        shiny::uiOutput(ns("targetpath_check")),
        shinyDirButton(
          ns("target_folder"),
          "Select Destination Folder",
          icon = shiny::icon("file-export"),
          title = "Select destination folder",
          buttonType = "default",
          root = path_home()
        ),
        shiny::verbatimTextOutput(ns("targetpath_selected"))
      ),

      # --- Section 2: Experiment Configuration ---
      div(
        class = "deconvolution-section",
        div(
          class = "sidebar-title custom-sidebar-title",
          "Experiment Configuration"
        ),
        uiOutput(ns("config_status_ui"))
      )
    )
  )
}

#' @export
server <- function(id, reset_button, config_file, config_filename) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected <- shiny::reactiveVal("")

    # Define roots for directory browsing
    roots <- get_volumes()

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

    # Specify destination path for results
    shinyDirChoose(
      input,
      id = "target_folder",
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

    target_path <- reactive({
      if (is.null(input$target_folder)) {
        character()
      } else {
        parseDirPath(roots, input$target_folder)
      }
    })

    # Collection of reactive vars
    rootdir <- shiny::reactiveVal(character())
    filepath <- shiny::reactiveVal(character())
    targetpath <- shiny::reactiveVal(character())

    shiny::observe({
      filepath(file_path())
      rootdir(root_dir())
      targetpath(target_path())
    })

    # Render initial file selection information field
    output$dir_check <- shiny::renderUI(
      shiny::p(
        shiny::HTML(
          "Select either multiple or one individual .raw folder."
        )
      )
    )

    output$targetpath_check <- shiny::renderUI({
      reset_button()

      if (!is.null(target_path()) && length(target_path())) {
        runjs(paste0(
          '$("#app-deconvolution_pars-targetpath_selected").css({"border-color": "#8BC34A"})'
        ))

        shiny::p(
          shiny::HTML(
            paste0(
              '<i class="fa-solid fa-circle-check" style="font-size:1em; c',
              'olor:#000000; margin-right: 10px;"></i>',
              "Destination path is valid."
            )
          )
        )
      } else {
        runjs(paste0(
          '$("#app-deconvolution_pars-targetpath_selected").css({"border-color": ""})'
        ))

        shiny::p(
          shiny::HTML(
            "Choose where to save the resulting files."
          )
        )
      }
    })

    # Initial file selection feedback
    output$path_selected <- shiny::renderPrint(cat("Nothing selected"))
    output$targetpath_selected <- shiny::renderPrint({
      if (!is.null(targetpath()) && length(targetpath()) > 0) {
        targetpath()
      } else {
        cat("Nothing selected")
      }
    })

    shiny::observeEvent(input$file, {
      selected("file")
      rootdir(character())

      # Render information field
      output$dir_check <- shiny::renderUI({
        if (length(filepath())) {
          if (
            grepl("\\.raw$", filepath(), ignore.case = TRUE) &&
              dir.exists(filepath())
          ) {
            # Highlight path field border color
            runjs(paste0(
              '$("#app-deconvolution_pars-path_selected").css({"border-color": "#8BC34A"})'
            ))

            shiny::p(
              shiny::HTML(
                paste0(
                  '<i class="fa-solid fa-circle-check" style="font-size:1em; c',
                  'olor:#000000; margin-right: 10px;"></i>',
                  "Directory is a valid .raw result folder."
                )
              )
            )
          } else {
            # Highlight path field border color
            runjs(paste0(
              '$("#app-deconvolution_pars-path_selected").css({"border-color": "#D17050"})'
            ))

            shiny::p(
              shiny::HTML(
                paste0(
                  '<i class="fa-solid fa-circle-exclamation" style="font-size:1e',
                  'm; color:black; margin-right: 10px;"></i>',
                  "Directory is <b>not</b> a .raw result folder."
                )
              )
            )
          }
        } else {
          # Highlight path field border color
          runjs(paste0(
            '$("#app-deconvolution_pars-path_selected").css({"border-color": "#D17050"})'
          ))

          shiny::p(
            shiny::HTML(
              "Select directory containing .raw result folder."
            )
          )
        }
      })

      # Adjust UI elements
      output$path_selected <- shiny::renderPrint(cat("Nothing selected"))

      output$path_selected <- shiny::renderPrint({
        input$file
        if (!is.null(filepath()) && length(filepath()) > 0) {
          filepath()
        } else {
          runjs(paste0(
            '$("#app-deconvolution_pars-path_selected").css({"border-color": "#D17050"})'
          ))
          cat("Nothing selected")
        }
      })

      # Focus file button
      runjs(paste0(
        '$("#app-deconvolution_pars-file").css({"background": "#DDDDDE"})'
      ))

      # Unfocus folder button
      runjs(paste0(
        '$("#app-deconvolution_pars-folder").css({"background": "#FFFFFF"})'
      ))
    })

    shiny::observeEvent(input$folder, {
      selected("folder")
      filepath(character())

      # Render information field
      output$dir_check <- shiny::renderUI({
        if (!is.null(rootdir()) && length(rootdir()) > 0) {
          raw_dirs <- list.dirs(
            rootdir(),
            full.names = TRUE,
            recursive = FALSE
          )
          raw_dirs <- raw_dirs[grep("\\.raw$", raw_dirs)]

          if (length(raw_dirs)) {
            # Highlight path field border color
            runjs(paste0(
              '$("#app-deconvolution_pars-path_selected").css({"border-color": "#8BC34A"})'
            ))

            # Inform # of valid raw files
            shiny::p(
              shiny::HTML(
                paste0(
                  '<i class="fa-solid fa-circle-check" style="font-size:1em; col',
                  'or:#000000; margin-right: 10px;"></i>',
                  paste(
                    "<b>",
                    length(raw_dirs),
                    "</b> .raw directories in directory."
                  )
                )
              )
            )
          } else {
            # Highlight path field border color
            runjs(paste0(
              '$("#app-deconvolution_pars-path_selected").css({"border-color": "#D17050"})'
            ))

            shiny::p(
              shiny::HTML(
                paste0(
                  '<i class="fa-solid fa-circle-exclamation" style="font-size:1e',
                  'm; color:black; margin-right: 10px;"></i>',
                  "<b>No</b> .raw directories in directory."
                )
              )
            )
          }
        } else {
          # Highlight path field border color
          runjs(paste0(
            '$("#app-deconvolution_pars-path_selected").css({"border-color": "#D17050"})'
          ))

          shiny::p(
            shiny::HTML(
              "Select directory containing .raw result folders."
            )
          )
        }
      })

      # Adjust UI elements
      output$path_selected <- shiny::renderPrint({
        input$folder
        if (!is.null(root_dir()) && length(root_dir()) > 0) {
          root_dir()
        } else {
          runjs(paste0(
            '$("#app-deconvolution_pars-path_selected").css({"border-color": "#D17050"})'
          ))
          cat("Nothing selected")
        }
      })

      # Focus folder button
      runjs(paste0(
        '$("#app-deconvolution_pars-folder").css({"background": "#DDDDDE"})'
      ))

      # Unfocus file button
      runjs(paste0(
        '$("#app-deconvolution_pars-file").css({"background": "#FFFFFF"})'
      ))
    })

    # Experiment configuration status panel
    output$config_status_ui <- renderUI({
      active <- !is.null(config_file())
      badge <- if (active) {
        config_badge("ok", "Active", config_filename())
      } else {
        config_badge("err", "Not loaded")
      }
      chk <- checkboxInput(
        ns("use_config"),
        "Use Config in Analysis",
        value = active
      )
      div(
        class = "sidebar-config-status",
        shiny::tags$p(
          class = "sidebar-config-description",
          "Maps samples to experimental metadata e.g. plate well positions."
        ),
        badge,
        actionButton(
          ns("open_config_btn"),
          "Experiment Configuration",
          icon = icon("upload"),
          class = "btn btn-sm btn-default"
        ),
        if (active) chk else shinyjs::disabled(chk)
      )
    })

    # Eagerly render all sidebar outputs that are visible on app launch so they
    # are computed in the first reactive flush alongside waiter_hide().
    shiny::outputOptions(output, "dir_check", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "targetpath_check", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "config_status_ui", suspendWhenHidden = FALSE)

    # Return paths and config state
    reactiveValues(
      dir = rootdir,
      file = filepath,
      targetpath = targetpath,
      selected = selected,
      use_config = shiny::reactive(
        isTRUE(input$use_config) && !is.null(config_file())
      ),
      open_config_clicked = shiny::reactive(input$open_config_btn)
    )
  })
}
