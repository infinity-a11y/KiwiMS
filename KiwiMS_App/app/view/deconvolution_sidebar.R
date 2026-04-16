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
        class = "sidebar-section",
        div(class = "sidebar-title custom-sidebar-title", "Select Files"),
        shiny::uiOutput(ns("dir_check")),
        shiny::tags$div(
          class = "sample-file-row",
          shiny::div(
            shinyDirButton(
              ns("folder"),
              "Select Input",
              icon = shiny::icon("file-import"),
              title = "Select a .raw folder or a directory containing multiple .raw folders",
              buttonType = "default",
              root = path_home()
            ),
            bslib::tooltip(
              shiny::div(
                class = "save-button",
                actionButton(
                  ns("save_input_dir"),
                  label = NULL,
                  icon = icon("floppy-disk"),
                  class = "btn-default"
                )
              ),
              "Save setting",
              placement = "bottom"
            )
          ),
          shiny::verbatimTextOutput(ns("path_selected"))
        ),
        shiny::uiOutput(ns("targetpath_check")),
        shiny::div(
          class = "dest-folder-row",
          shiny::div(
            shinyDirButton(
              ns("target_folder"),
              "Select Output Path",
              icon = shiny::icon("file-export"),
              title = "Select output path",
              buttonType = "default",
              root = path_home()
            ),
            bslib::tooltip(
              shiny::div(
                class = "save-button",
                actionButton(
                  ns("save_output_dir"),
                  label = NULL,
                  icon = icon("floppy-disk"),
                  class = "btn-default"
                )
              ),
              "Save setting",
              placement = "bottom"
            )
          ),
          shiny::verbatimTextOutput(ns("targetpath_selected"))
        )
      ),

      # --- Section 2: Experiment Configuration ---
      div(
        class = "sidebar-section",
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
server <- function(
  id,
  reset_button,
  config_file,
  config_filename,
  default_dest_path = shiny::reactive(NULL),
  default_input_path = shiny::reactive(NULL)
) {
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

    # Specify output path for results
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

    target_path <- reactive({
      if (is.null(input$target_folder)) {
        character()
      } else {
        parseDirPath(roots, input$target_folder)
      }
    })

    # Collection of reactive vars
    rootdir <- shiny::reactiveVal(character())
    targetpath <- shiny::reactiveVal(character())

    # Apply default output path once on init (if configured)
    shiny::observe({
      def <- default_dest_path()
      tp <- targetpath()
      if (
        length(def) == 1L &&
          nzchar(def) &&
          dir.exists(def) &&
          (length(tp) == 0L || !nzchar(tp))
      ) {
        targetpath(def)
      }
    })

    # Apply default input path once on init (if configured)
    shiny::observe({
      def <- default_input_path()
      rd <- rootdir()
      if (
        length(def) == 1L &&
          nzchar(def) &&
          dir.exists(def) &&
          (length(rd) == 0L || !nzchar(rd))
      ) {
        rootdir(def)
        selected("folder")
      }
    })

    shiny::observe({
      rootdir(root_dir())
      p <- target_path()
      if (length(p) && nzchar(p)) targetpath(p)
    })

    # Render file selection information field (reacts to rootdir for default path on startup)
    output$dir_check <- shiny::renderUI({
      rd <- rootdir()
      if (!is.null(rd) && length(rd) > 0 && nzchar(rd)) {
        if (grepl("\\.raw$", rd, ignore.case = TRUE) && dir.exists(rd)) {
          runjs(paste0(
            '$("#app-deconvolution_pars-path_selected").css({"border-color": "#8BC34A"})'
          ))
          shiny::p(shiny::HTML(paste0(
            '<i class="fa-solid fa-circle-check" style="font-size:1em; c',
            'olor:#000000; margin-right: 10px;"></i>',
            "Selected folder is a valid .raw folder."
          )))
        } else if (dir.exists(rd)) {
          raw_dirs <- list.dirs(rd, full.names = TRUE, recursive = FALSE)
          raw_dirs <- raw_dirs[grep("\\.raw$", raw_dirs)]
          if (length(raw_dirs)) {
            runjs(paste0(
              '$("#app-deconvolution_pars-path_selected").css({"border-color": "#8BC34A"})'
            ))
            shiny::p(shiny::HTML(paste0(
              '<i class="fa-solid fa-circle-check" style="font-size:1em; col',
              'or:#000000; margin-right: 10px;"></i>',
              paste("<b>", length(raw_dirs), "</b> .raw folders in directory.")
            )))
          } else {
            runjs(paste0(
              '$("#app-deconvolution_pars-path_selected").css({"border-color": "#D17050"})'
            ))
            shiny::p(shiny::HTML(paste0(
              '<i class="fa-solid fa-circle-exclamation" style="font-size:1e',
              'm; color:black; margin-right: 10px;"></i>',
              "<b>No</b> .raw folders found in directory."
            )))
          }
        }
      } else {
        shiny::p(shiny::HTML(
          "Select a .raw folder or a directory containing multiple .raw folders."
        ))
      }
    })

    output$targetpath_check <- shiny::renderUI({
      reset_button()
      tp <- targetpath()

      if (length(tp) && nzchar(tp)) {
        runjs(paste0(
          '$("#app-deconvolution_pars-targetpath_selected").css({"border-color": "#8BC34A"})'
        ))

        shiny::p(
          shiny::HTML(
            paste0(
              '<i class="fa-solid fa-circle-check" style="font-size:1em; c',
              'olor:#000000; margin-right: 10px;"></i>',
              "Output path is valid."
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
    output$path_selected <- shiny::renderPrint({
      rd <- rootdir()
      if (length(rd) > 0 && nzchar(rd)) {
        runjs(paste0(
          '$("#app-deconvolution_pars-path_selected").css({"border-color": "#8BC34A"})'
        ))
        cat(rd)
      } else {
        runjs(paste0(
          '$("#app-deconvolution_pars-path_selected").css({"border-color": ""})'
        ))
        cat("Nothing selected")
      }
    })
    output$targetpath_selected <- shiny::renderText({
      tp <- targetpath()
      if (length(tp) > 0 && nzchar(tp)) tp else "Nothing selected"
    })

    shiny::observeEvent(input$folder, {
      selected("folder")

      # Render information field
      output$dir_check <- shiny::renderUI({
        rd <- root_dir()
        if (!is.null(rd) && length(rd) > 0) {
          # Check if selection itself is a .raw folder
          if (
            grepl("\\.raw$", rd, ignore.case = TRUE) &&
              dir.exists(rd)
          ) {
            runjs(paste0(
              '$("#app-deconvolution_pars-path_selected").css({"border-color": "#8BC34A"})'
            ))

            shiny::p(
              shiny::HTML(
                paste0(
                  '<i class="fa-solid fa-circle-check" style="font-size:1em; c',
                  'olor:#000000; margin-right: 10px;"></i>',
                  "Selected folder is a valid .raw folder."
                )
              )
            )
          } else {
            raw_dirs <- list.dirs(
              rd,
              full.names = TRUE,
              recursive = FALSE
            )
            raw_dirs <- raw_dirs[grep("\\.raw$", raw_dirs)]

            if (length(raw_dirs)) {
              runjs(paste0(
                '$("#app-deconvolution_pars-path_selected").css({"border-color": "#8BC34A"})'
              ))

              shiny::p(
                shiny::HTML(
                  paste0(
                    '<i class="fa-solid fa-circle-check" style="font-size:1em; col',
                    'or:#000000; margin-right: 10px;"></i>',
                    paste(
                      "<b>",
                      length(raw_dirs),
                      "</b> .raw folders in directory."
                    )
                  )
                )
              )
            } else {
              runjs(paste0(
                '$("#app-deconvolution_pars-path_selected").css({"border-color": "#D17050"})'
              ))

              shiny::p(
                shiny::HTML(
                  paste0(
                    '<i class="fa-solid fa-circle-exclamation" style="font-size:1e',
                    'm; color:black; margin-right: 10px;"></i>',
                    "<b>No</b> .raw folders found in directory."
                  )
                )
              )
            }
          }
        } else {
          runjs(paste0(
            '$("#app-deconvolution_pars-path_selected").css({"border-color": "#D17050"})'
          ))

          shiny::p(
            shiny::HTML(
              "Select a .raw folder or a directory containing multiple .raw folders."
            )
          )
        }
      })

      # Adjust UI elements
      output$path_selected <- shiny::renderPrint({
        input$folder
        rd_active <- root_dir()
        if (!is.null(rd_active) && length(rd_active) > 0) {
          cat(rd_active)
        } else {
          runjs(paste0(
            '$("#app-deconvolution_pars-path_selected").css({"border-color": "#D17050"})'
          ))
          cat("Nothing selected")
        }
      })
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
      targetpath = targetpath,
      selected = selected,
      use_config = shiny::reactive(
        isTRUE(input$use_config) && !is.null(config_file())
      ),
      open_config_clicked = shiny::reactive(input$open_config_btn),
      open_settings_clicked = shiny::reactive(input$save_output_dir),
      save_input_dir_clicked = shiny::reactive(input$save_input_dir)
    )
  })
}
