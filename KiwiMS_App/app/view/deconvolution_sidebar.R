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
    reactiveValues,
    renderUI,
    uiOutput,
  ],
  shinyjs[disable, disabled, enable, runjs],
  shinyWidgets[radioGroupButtons],
)

box::use(
  app / logic / folder_picker[folder_picker],
  app / logic / helper_functions[config_badge],
  app / logic / logging[get_log],
)


#' @export
ui <- function(id) {
  ns <- NS(id)

  sidebar(
    class = "deconvolution-sidebar",
    # width = "23rem",
    width = "18%",
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
            bslib::tooltip(
              actionButton(
                ns("folder"),
                "Select Input",
                icon = shiny::icon("file-import")
              ),
              "Select a .raw folder or a directory containing multiple .raw folders",
              placement = "top"
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
              "Save Setting",
              placement = "top"
            )
          ),
          shiny::verbatimTextOutput(ns("path_selected"))
        ),
        shiny::uiOutput(ns("targetpath_check")),
        shiny::div(
          class = "dest-folder-row",
          shiny::div(
            bslib::tooltip(
              actionButton(
                ns("target_folder"),
                "Select Output Path",
                icon = shiny::icon("file-export")
              ),
              "Select the output path for deconvolution results",
              placement = "top"
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
              "Save Setting",
              placement = "top"
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

    # Collection of reactive vars
    rootdir <- shiny::reactiveVal(character())
    targetpath <- shiny::reactiveVal(character())

    # Where the dialog opens: whatever is already chosen, else the configured
    # default, else the user's profile. Deliberately not validated with
    # dir.exists() - that is a stat, and a stat on a disconnected share is
    # exactly the multi-second block this module exists to avoid. The shell
    # treats an unreachable start folder as advisory and opens at This PC.
    opening_dir <- function(current, configured) {
      function() {
        for (candidate in list(current, configured)) {
          value <- shiny::isolate(candidate())
          if (length(value) == 1L && !is.na(value) && nzchar(value)) {
            return(value)
          }
        }
        path_home()
      }
    }

    # Native shell dialog rather than shinyFiles: the tree is enumerated by the
    # OS in its own process, so browsing no longer blocks R. See folder_picker.R.
    root_dir <- folder_picker(
      input,
      session,
      "folder",
      title = "Select a .raw folder or a directory containing .raw folders",
      initial_dir = opening_dir(rootdir, default_input_path)
    )

    target_path <- folder_picker(
      input,
      session,
      "target_folder",
      title = "Select the output path",
      initial_dir = opening_dir(targetpath, default_dest_path)
    )

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

    # dir_check and path_selected above already re-render from rootdir(), so the
    # click handler that used to re-assign both outputs here has been dropped.
    # With shinyFiles, input$folder only changed once a directory was actually
    # chosen; an actionButton fires on every click including Cancel, and the old
    # handler would then have repainted the panel as "nothing selected" and
    # wiped a configured default path off the screen.
    shiny::observeEvent(root_dir(), {
      if (length(root_dir()) && nzchar(root_dir())) {
        selected("folder")
      }
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
