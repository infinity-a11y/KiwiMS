# app/main.R

box::use(
  bslib,
  shiny,
  shinyjs[disable, enable, hide, hidden, show, runjs, useShinyjs],
  shinyWidgets[show_toast],
  waiter[useWaiter, waiter_hide, waiterShowOnLoad],
)

box::use(
  app / logic / dev_utils,
  app /
    logic /
    main_ui[licence_modal_body, unidec_modal_body, update_modal_body],
  app /
    logic /
    main_ui[licence_modal_body, unidec_modal_body, update_modal_body],
  app / view / conversion_main,
  app / view / conversion_sidebar,
  app / view / deconvolution_main,
  app / view / deconvolution_sidebar,
  app / view / log_view,
  app / view / log_sidebar,
  app / logic / logging[start_logging, write_log, close_logging],
  app /
    logic /
    user_settings[
      read_user_settings,
      save_user_settings,
      update_user_setting,
    ],
  app /
    logic /
    helper_functions[
      check_github_version,
      config_badge,
      get_kiwims_version,
      get_latest_release_url,
      get_volumes,
      normalize_colnames,
      read_config_file,
      validate_config,
    ],
)

suppressWarnings(library(logr))

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    dev_utils$add_dev_headers(),
    shiny$div(id = "blocking-overlay"),
    useWaiter(),
    waiterShowOnLoad(
      html = shiny$tags$div(
        style = "text-align: center;",
        shiny$tags$img(
          src = "static/logo_animated.svg",
          width = "400px",
          height = "400px"
        ),
        shiny$tags$div(
          style = paste0(
            "font-family: monospace; font-size: 50px; color: blac",
            "k; opacity: 0; animation: fadeIn 1s ease-in forwards",
            "; animation-delay: 1s;"
          ),
          "KiwiMS"
        )
      )
    ),
    useShinyjs(),
    bslib$page_navbar(
      id = ns("tabs"),
      title = shiny$tags$div(
        shiny$tags$img(
          src = "static/logo.svg",
          height = "42rem",
          style = "margin-right: 5px; margin-top: -2px"
        ),
        shiny$tags$span(
          "KiwiMS",
          style = "font-size: 21px; font-family: monospace;"
        )
      ),
      window_title = paste("KiwiMS", get_kiwims_version()["version"]),
      navbar_options = bslib$navbar_options(underline = TRUE),
      bslib$nav_panel(
        title = "Deconvolution",
        bslib$page_sidebar(
          sidebar = deconvolution_sidebar$ui(
            ns("deconvolution_pars")
          ),
          deconvolution_main$ui(
            ns("deconvolution_main")
          )
        )
      ),
      bslib$nav_panel(
        title = "Protein Conversion",
        bslib$page_sidebar(
          sidebar = conversion_sidebar$ui(ns("conversion_sidebar")),
          conversion_main$ui(ns("conversion_main"))
        )
      ),
      bslib$nav_panel(
        title = "Logs",
        icon = shiny::icon("list-check"),
        bslib$page_sidebar(
          sidebar = log_sidebar$ui(ns("log_sidebar")),
          bslib$card(
            class = "logs-card",
            log_view$ui(ns("logs"))
          )
        )
      ),
      bslib$nav_spacer(),
      bslib$nav_item(shiny::uiOutput(ns("config_nav_btn"))),
      bslib$nav_item(
        shiny::actionButton(
          ns("settings"),
          "Settings",
          icon = shiny::icon("gear"),
          class = "nav-link"
        )
      ),
      bslib$nav_item(
        shiny::actionButton(
          ns("licence"),
          "License",
          icon = shiny::icon("info"),
          class = "nav-link"
        )
      ),
      bslib$nav_item(shiny::uiOutput(
        ns("update_button"),
        class = "nav-link",
        style = "cursor: pointer;",
        onclick = "Shiny.setInputValue('app-open_update_modal', Math.random());"
      )),
      bslib$nav_item(
        shiny::tags$a(
          id = "unidec-tag",
          style = "cursor: pointer;",
          onclick = "Shiny.setInputValue('app-unidec_click', Math.random());",
          shiny::tags$img(
            src = "static/UniDec.png",
            width = "auto",
            style = "    top: -1px;
    position: relative;",
            height = "18px"
          ),
          "UniDec"
        )
      )
      # bslib$nav_menu(
      #   title = "Links",
      #   align = "right",
      #   icon = shiny$icon("link"),
      #   bslib$nav_item(
      #     shiny$tags$a(
      #       shiny$tags$span(
      #         shiny$tags$i(class = "fa-brands fa-github me-1"),
      #         "KiwiMS GitHub"
      #       ),
      #       href = "https://github.com/infinity-a11y/MSFlow",
      #       target = "_blank",
      #       class = "nav-link"
      #     )
      #   ),
      #   bslib$nav_item(
      #     shiny$tags$a(
      #       shiny$tags$span(
      #         shiny$tags$i(class = "fa-brands fa-github me-1"),
      #         "UniDec GitHub"
      #       ),
      #       href = "https://github.com/michaelmarty/UniDec",
      #       target = "_blank",
      #       class = "nav-link"
      #     )
      #   ),
      #   bslib$nav_item(
      #     shiny$tags$a(
      #       shiny$tags$span(
      #         shiny$tags$img(
      #           src = "static/liora_logo.png",
      #           style = "height: 1em; margin-right: 5px;"
      #         ),
      #         "Liora Bioinformatics"
      #       ),
      #       href = "https://www.liora-bioinformatics.com",
      #       target = "_blank",
      #       class = "nav-link"
      #     )
      #   )
      # )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    Sys.setenv(CONDA_DLL_SEARCH_MODIFICATION_ENABLE = "1")
    ns <- session$ns

    # Kill server on session end
    session$onSessionEnded(function() {
      write_log("Session closed")
      shiny$stopApp()
    })

    shiny::observeEvent(input$quit_kiwims, {
      shiny::stopApp() # This signals the mother process that the app is done
    })

    # Initiate logging
    start_logging()
    write_log("Session started")

    # Log view server
    active_tab_reactive <- shiny$reactive({
      input$tabs
    })
    log_buttons <- log_sidebar$server("log_sidebar")
    log_view$server("logs", active_tab_reactive, log_buttons)

    reset_button <- shiny$reactiveVal(0)
    configfile <- shiny$reactiveVal(NULL)
    pending_config <- shiny$reactiveVal(NULL)
    config_modal_state <- shiny$reactiveVal("upload")
    config_filename <- shiny$reactiveVal(NULL)

    # User settings persistence
    settings_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "KiwiMS", "settings")
    dest_settings_file <- file.path(settings_dir, "default_dest_path.rds")
    dest_settings <- shiny$reactiveVal(
      if (file.exists(dest_settings_file)) {
        readRDS(dest_settings_file)
      } else {
        list(path = "", enabled = FALSE)
      }
    )

    # Reusable function to open the settings modal
    # initial_path: pre-fill dest folder from caller (e.g. currently active path)
    open_settings_modal <- function(initial_path = NULL) {
      s <- dest_settings()
      base <- if (length(initial_path) == 1L && nzchar(initial_path)) {
        initial_path
      } else {
        s$path
      }
      us <- read_user_settings()

      shiny$showModal(
        shiny$div(
          class = "unidec-modal",
          shiny$modalDialog(
            title = "Settings",
            size = "l",
            easyClose = TRUE,
            shiny$div(
              class = "settings-modal-body",
              shiny$tags$table(
                class = "table table-sm table-bordered settings-table",
                shiny$tags$thead(
                  shiny$tags$tr(
                    shiny$tags$th("Setting"),
                    shiny$tags$th("Default Value"),
                    shiny$tags$th("Status")
                  )
                ),
                shiny$tags$tbody(
                  # --- General ---
                  shiny$tags$tr(
                    shiny$tags$td(
                      colspan = "3",
                      class = "settings-section-header",
                      "General"
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Destination Folder"
                    ),
                    shiny$tags$td(
                      shiny$textInput(
                        ns("settings_dest_path"),
                        label = NULL,
                        value = base,
                        placeholder = "Paste or type an absolute folder path",
                        width = "100%"
                      ),
                      shiny$div(
                        class = "settings-dest-row",
                        shiny$checkboxInput(
                          ns("settings_dest_enabled"),
                          label = "Use as default",
                          value = isTRUE(s$enabled)
                        )
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_dest_path_display"))
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Keep UniDec output files",
                      shiny$tags$span(
                        class = "settings-info",
                        "Keeps *_rawdata.txt and *_rawdata_unidecfiles/ after analysis"
                      )
                    ),
                    shiny$tags$td(
                      shiny$checkboxInput(
                        ns("settings_keep_raw_output"),
                        label = "Enable",
                        value = isTRUE(us$deconv_keep_raw_output)
                      )
                    ),
                    shiny$tags$td(class = "settings-table-feedback")
                  ),
                  # --- Default Input Values ---
                  shiny$tags$tr(
                    shiny$tags$td(
                      colspan = "3",
                      class = "settings-section-header",
                      "Default Input Values",
                      shiny$tags$span(
                        class = "settings-info",
                        "Restored at the start of each session."
                      )
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Min. charge state [z]"
                    ),
                    shiny$tags$td(
                      shiny$numericInput(
                        ns("settings_startz"),
                        label = NULL,
                        min = 1,
                        max = 100,
                        value = us$deconv_startz,
                        step = 1,
                        width = "200px"
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_startz_feedback"))
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Max. charge state [z]"
                    ),
                    shiny$tags$td(
                      shiny$numericInput(
                        ns("settings_endz"),
                        label = NULL,
                        min = 1,
                        max = 100,
                        value = us$deconv_endz,
                        step = 1,
                        width = "200px"
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_endz_feedback"))
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Lower deconvolution range [m/z]"
                    ),
                    shiny$tags$td(
                      shiny$numericInput(
                        ns("settings_minmz"),
                        label = NULL,
                        min = 1,
                        max = 100000,
                        value = us$deconv_minmz,
                        step = 1,
                        width = "200px"
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_minmz_feedback"))
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Upper deconvolution range [m/z]"
                    ),
                    shiny$tags$td(
                      shiny$numericInput(
                        ns("settings_maxmz"),
                        label = NULL,
                        min = 1,
                        max = 100000,
                        value = us$deconv_maxmz,
                        step = 1,
                        width = "200px"
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_maxmz_feedback"))
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Lower mass range [Da]"
                    ),
                    shiny$tags$td(
                      shiny$numericInput(
                        ns("settings_masslb"),
                        label = NULL,
                        min = 1,
                        max = 2000000,
                        value = us$deconv_masslb,
                        step = 1,
                        width = "200px"
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_masslb_feedback"))
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Upper mass range [Da]"
                    ),
                    shiny$tags$td(
                      shiny$numericInput(
                        ns("settings_massub"),
                        label = NULL,
                        min = 1,
                        max = 2000000,
                        value = us$deconv_massub,
                        step = 1,
                        width = "200px"
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_massub_feedback"))
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Elution start time [min]"
                    ),
                    shiny$tags$td(
                      shiny$numericInput(
                        ns("settings_time_start"),
                        label = NULL,
                        min = 0,
                        max = 100,
                        value = us$deconv_time_start,
                        step = 0.05,
                        width = "200px"
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_time_start_feedback"))
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Elution end time [min]"
                    ),
                    shiny$tags$td(
                      shiny$numericInput(
                        ns("settings_time_end"),
                        label = NULL,
                        min = 0,
                        max = 100,
                        value = us$deconv_time_end,
                        step = 0.05,
                        width = "200px"
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_time_end_feedback"))
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Detection window"
                    ),
                    shiny$tags$td(
                      shiny$numericInput(
                        ns("settings_peakwindow"),
                        label = NULL,
                        min = 1,
                        max = 500,
                        value = us$deconv_peakwindow,
                        step = 1,
                        width = "200px"
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_peakwindow_feedback"))
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Peak normalization"
                    ),
                    shiny$tags$td(
                      shiny$div(
                        class = "settings-peaknorm",
                        shiny$selectInput(
                          ns("settings_peaknorm"),
                          label = NULL,
                          choices = c(
                            "No normalization" = 0,
                            "Max Normalization" = 1,
                            "Normalization to Sum" = 2
                          ),
                          selected = us$deconv_peaknorm,
                          width = "200px"
                        )
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_peaknorm_feedback"))
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Peak threshold"
                    ),
                    shiny$tags$td(
                      shiny$numericInput(
                        ns("settings_peakthresh"),
                        label = NULL,
                        min = 0,
                        max = 1,
                        value = us$deconv_peakthresh,
                        step = 0.01,
                        width = "200px"
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_peakthresh_feedback"))
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Peak Tolerance [Da]"
                    ),
                    shiny$tags$td(
                      shiny$numericInput(
                        ns("settings_peak_tol"),
                        label = NULL,
                        value = us$peak_tolerance,
                        min = 0,
                        max = 20,
                        step = 0.1,
                        width = "200px"
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_peak_tol_feedback"))
                    )
                  ),
                  shiny$tags$tr(
                    shiny$tags$td(
                      class = "settings-table-label",
                      "Max. Stoichiometry"
                    ),
                    shiny$tags$td(
                      shiny$numericInput(
                        ns("settings_max_mult"),
                        label = NULL,
                        value = us$max_multiples,
                        min = 1,
                        max = 20,
                        step = 1,
                        width = "200px"
                      )
                    ),
                    shiny$tags$td(
                      class = "settings-table-feedback",
                      shiny$uiOutput(ns("settings_max_mult_feedback"))
                    )
                  ),
                  shiny$tags$tr(
                    style = "margin-top: 1rem;",
                    shiny$tags$td(
                      class = "settings-table-label",
                    ),
                    shiny$tags$td(
                      colspan = "2",
                      shiny$actionButton(
                        ns("reset_default"),
                        "Reset All",
                        width = "100%"
                      )
                    )
                  )
                )
              )
            ),
            footer = shiny$tagList(
              shiny$modalButton("Dismiss"),
              shiny$actionButton(
                ns("save_settings"),
                "Save",
                icon = shiny$icon("floppy-disk"),
                class = "load-db"
              )
            )
          )
        )
      )
    }

    shiny$observeEvent(input$reset_default, {
      # Reset default deconvolution values
      shiny::updateNumericInput(
        session = session,
        "settings_startz",
        value = 1
      )
      shiny::updateNumericInput(
        session = session,
        "settings_endz",
        value = 50
      )
      shiny::updateNumericInput(
        session = session,
        "settings_minmz",
        value = 710
      )
      shiny::updateNumericInput(
        session = session,
        "settings_maxmz",
        value = 1100
      )
      shiny::updateNumericInput(
        session = session,
        "settings_masslb",
        value = 10000
      )
      shiny::updateNumericInput(
        session = session,
        "settings_massub",
        value = 60000
      )
      shiny::updateNumericInput(
        session = session,
        "settings_peak_tol",
        value = 3
      )
      shiny::updateNumericInput(
        session = session,
        "settings_time_start",
        value = 0.5
      )
      shiny::updateNumericInput(
        session = session,
        "settings_time_end",
        value = 1.5
      )
      shiny::updateNumericInput(
        session = session,
        "settings_peakwindow",
        value = 40
      )
      shiny::updateSelectInput(
        session = session,
        "settings_peaknorm",
        selected = 2
      )
      shiny::updateNumericInput(
        session = session,
        "settings_peakthresh",
        value = 0.07
      )
      shiny::updateNumericInput(
        session = session,
        "settings_massbins",
        value = 0.5
      )

      # Reset default conversion values
      shiny::updateNumericInput(
        session = session,
        "settings_peak_tol",
        value = 3
      )
      shiny::updateNumericInput(
        session = session,
        "settings_max_mult",
        value = 4
      )

      shiny::updateCheckboxInput(
        session = session,
        "settings_keep_raw_output",
        value = FALSE
      )
    })

    # Resolve typed/pasted path from the text input
    settings_dest_picked <- shiny$reactive({
      p <- input$settings_dest_path
      trimws(if (!is.null(p)) p else dest_settings()$path)
    })

    # Settings opened from nav button
    shiny$observeEvent(input$settings, {
      open_settings_modal()
    })

    # Live feedback for destination folder path inside modal
    output$settings_dest_path_display <- shiny$renderUI({
      path <- settings_dest_picked()
      if (!nzchar(path)) {
        return(NULL)
      }
      if (dir.exists(path)) {
        shiny$div(
          class = "settings-dest-feedback settings-dest-feedback--valid",
          shiny$icon("circle-check"),
          " Folder exists"
        )
      } else {
        shiny$div(
          class = "settings-dest-feedback settings-dest-feedback--invalid",
          shiny$icon("triangle-exclamation"),
          " Folder not found"
        )
      }
    })

    # Helper tags for settings validation feedback
    settings_ok_tag <- function(msg = "Valid") {
      shiny$div(
        class = "settings-feedback settings-feedback--valid",
        shiny$icon("circle-check"),
        paste0(" ", msg)
      )
    }
    settings_err_tag <- function(msg) {
      shiny$div(
        class = "settings-feedback settings-feedback--invalid",
        shiny$icon("triangle-exclamation"),
        paste0(" ", msg)
      )
    }

    # Peak Tolerance [Da] — min 0, max 20
    output$settings_peak_tol_feedback <- shiny$renderUI({
      val <- input$settings_peak_tol
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 0 || val > 20) {
        return(settings_err_tag("Must be between 0 and 20 Da"))
      }
      settings_ok_tag("Valid")
    })

    # Max. Stoichiometry — min 1, max 20, integer
    output$settings_max_mult_feedback <- shiny$renderUI({
      val <- input$settings_max_mult
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1 || val > 20) {
        return(settings_err_tag("Must be between 1 and 20"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      settings_ok_tag("Valid")
    })

    # Min. charge state [z] — min 1, max 100, integer, < endz
    output$settings_startz_feedback <- shiny$renderUI({
      val <- input$settings_startz
      endz <- input$settings_endz
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1) {
        return(settings_err_tag("Must be at least 1"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      if (!is.null(endz) && !is.na(endz) && val >= endz) {
        return(settings_err_tag("Must be less than max. charge state"))
      }
      settings_ok_tag("Valid")
    })

    # Max. charge state [z] — min 1, max 100, integer, > startz
    output$settings_endz_feedback <- shiny$renderUI({
      val <- input$settings_endz
      startz <- input$settings_startz
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1) {
        return(settings_err_tag("Must be at least 1"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      if (!is.null(startz) && !is.na(startz) && val <= startz) {
        return(settings_err_tag("Must be greater than min. charge state"))
      }
      settings_ok_tag("Valid")
    })

    # Lower deconvolution range [m/z] — min 1, max 100000, integer, < maxmz
    output$settings_minmz_feedback <- shiny$renderUI({
      val <- input$settings_minmz
      maxmz <- input$settings_maxmz
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1) {
        return(settings_err_tag("Must be at least 1"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      if (!is.null(maxmz) && !is.na(maxmz) && val >= maxmz) {
        return(settings_err_tag("Must be less than upper m/z"))
      }
      settings_ok_tag("Valid")
    })

    # Upper deconvolution range [m/z] — min 1, max 100000, integer, > minmz
    output$settings_maxmz_feedback <- shiny$renderUI({
      val <- input$settings_maxmz
      minmz <- input$settings_minmz
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1) {
        return(settings_err_tag("Must be at least 1"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      if (!is.null(minmz) && !is.na(minmz) && val <= minmz) {
        return(settings_err_tag("Must be greater than lower m/z"))
      }
      settings_ok_tag("Valid")
    })

    # Lower mass range [Da] — min 1, max 2000000, integer, < massub
    output$settings_masslb_feedback <- shiny$renderUI({
      val <- input$settings_masslb
      massub <- input$settings_massub
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1) {
        return(settings_err_tag("Must be at least 1 Da"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      if (!is.null(massub) && !is.na(massub) && val >= massub) {
        return(settings_err_tag("Must be less than upper mass"))
      }
      settings_ok_tag("Valid")
    })

    # Upper mass range [Da] — min 1, max 2000000, integer, > masslb
    output$settings_massub_feedback <- shiny$renderUI({
      val <- input$settings_massub
      masslb <- input$settings_masslb
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1) {
        return(settings_err_tag("Must be at least 1 Da"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      if (!is.null(masslb) && !is.na(masslb) && val <= masslb) {
        return(settings_err_tag("Must be greater than lower mass"))
      }
      settings_ok_tag("Valid")
    })

    # Elution start time [min] — min 0, max 100, < time_end
    output$settings_time_start_feedback <- shiny$renderUI({
      val <- input$settings_time_start
      time_end <- input$settings_time_end
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 0 || val > 100) {
        return(settings_err_tag("Must be between 0 and 100 min"))
      }
      if (!is.null(time_end) && !is.na(time_end) && val >= time_end) {
        return(settings_err_tag("Must be earlier than end time"))
      }
      settings_ok_tag("Valid")
    })

    # Elution end time [min] — min 0, max 100, > time_start
    output$settings_time_end_feedback <- shiny$renderUI({
      val <- input$settings_time_end
      time_start <- input$settings_time_start
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 0 || val > 100) {
        return(settings_err_tag("Must be between 0 and 100 min"))
      }
      if (!is.null(time_start) && !is.na(time_start) && val <= time_start) {
        return(settings_err_tag("Must be later than start time"))
      }
      settings_ok_tag("Valid")
    })

    # Detection window [Da] — min 1, max 500, integer
    output$settings_peakwindow_feedback <- shiny$renderUI({
      val <- input$settings_peakwindow
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 1 || val > 500) {
        return(settings_err_tag("Must be between 1 and 500 Da"))
      }
      if (val != floor(val)) {
        return(settings_err_tag("Must be a whole number"))
      }
      settings_ok_tag("Valid")
    })

    # Peak normalization — always valid (fixed choices)
    output$settings_peaknorm_feedback <- shiny$renderUI({
      settings_ok_tag("Valid")
    })

    # Peak threshold — min 0, max 1
    output$settings_peakthresh_feedback <- shiny$renderUI({
      val <- input$settings_peakthresh
      if (is.null(val) || is.na(val)) {
        return(settings_err_tag("Enter a valid number"))
      }
      if (val < 0 || val > 1) {
        return(settings_err_tag("Must be between 0 and 1"))
      }
      settings_ok_tag("Valid")
    })

    shiny$observeEvent(input$save_settings, {
      # --- Destination folder ---
      path <- settings_dest_picked()
      enabled <- isTRUE(input$settings_dest_enabled)
      if (enabled && !nzchar(path)) {
        shiny$showNotification(
          "Select a folder first.",
          type = "error",
          duration = 4
        )
        return()
      }
      if (enabled && !dir.exists(path)) {
        shiny$showNotification(
          "Folder not found — settings not saved.",
          type = "error",
          duration = 4
        )
        return()
      }
      if (!dir.exists(settings_dir)) {
        dir.create(settings_dir, recursive = TRUE)
      }
      new_dest <- list(path = path, enabled = enabled)
      saveRDS(new_dest, dest_settings_file)
      dest_settings(new_dest)

      # --- Numeric defaults — only overwrite keys whose input passes validation.
      # Empty/invalid fields are left at their stored value (or built-in default).
      # Note: is.numeric(NA) is FALSE in R, so we use is.na() directly.
      current <- read_user_settings() # already NA-sanitised; contains stored or defaults
      ok <- function(v) !is.null(v) && !is.na(v)
      int_ok <- function(v) ok(v) && v == floor(v)

      pt <- input$settings_peak_tol
      if (ok(pt) && pt >= 0 && pt <= 20) {
        current$peak_tolerance <- pt
      }

      mm <- input$settings_max_mult
      if (int_ok(mm) && mm >= 1 && mm <= 20) {
        current$max_multiples <- mm
      }

      pw <- input$settings_peakwindow
      if (int_ok(pw) && pw >= 1 && pw <= 500) {
        current$deconv_peakwindow <- pw
      }

      current$deconv_peaknorm <- as.numeric(input$settings_peaknorm)

      p2 <- input$settings_peakthresh
      if (ok(p2) && p2 >= 0 && p2 <= 1) {
        current$deconv_peakthresh <- p2
      }

      # Paired fields: save the pair only when both individually valid AND ordered;
      # save each side independently when its counterpart is absent/invalid.
      sz <- input$settings_startz
      sz_ok <- int_ok(sz) && sz >= 1
      ez <- input$settings_endz
      ez_ok <- int_ok(ez) && ez >= 1
      if (sz_ok && ez_ok) {
        if (sz < ez) {
          current$deconv_startz <- sz
          current$deconv_endz <- ez
        }
      } else {
        if (sz_ok) {
          current$deconv_startz <- sz
        }
        if (ez_ok) current$deconv_endz <- ez
      }

      mn <- input$settings_minmz
      mn_ok <- int_ok(mn) && mn >= 1
      mx <- input$settings_maxmz
      mx_ok <- int_ok(mx) && mx >= 1
      if (mn_ok && mx_ok) {
        if (mn < mx) {
          current$deconv_minmz <- mn
          current$deconv_maxmz <- mx
        }
      } else {
        if (mn_ok) {
          current$deconv_minmz <- mn
        }
        if (mx_ok) current$deconv_maxmz <- mx
      }

      lb <- input$settings_masslb
      lb_ok <- int_ok(lb) && lb >= 1
      ub <- input$settings_massub
      ub_ok <- int_ok(ub) && ub >= 1
      if (lb_ok && ub_ok) {
        if (lb < ub) {
          current$deconv_masslb <- lb
          current$deconv_massub <- ub
        }
      } else {
        if (lb_ok) {
          current$deconv_masslb <- lb
        }
        if (ub_ok) current$deconv_massub <- ub
      }

      ts <- input$settings_time_start
      ts_ok <- ok(ts) && ts >= 0 && ts <= 100
      te <- input$settings_time_end
      te_ok <- ok(te) && te >= 0 && te <= 100
      if (ts_ok && te_ok) {
        if (ts < te) {
          current$deconv_time_start <- ts
          current$deconv_time_end <- te
        }
      } else {
        if (ts_ok) {
          current$deconv_time_start <- ts
        }
        if (te_ok) current$deconv_time_end <- te
      }

      current$deconv_keep_raw_output <- isTRUE(input$settings_keep_raw_output)

      save_user_settings(current)

      shiny$removeModal()
      shinyWidgets::show_toast(
        "Settings saved.",
        text = NULL,
        type = "success",
        timer = 3000,
        timerProgressBar = TRUE
      )
    })

    # Deconvolution sidebar server
    deconvolution_sidebar_vars <- deconvolution_sidebar$server(
      "deconvolution_pars",
      reset_button = reset_button,
      config_file = configfile,
      config_filename = config_filename,
      default_dest_path = shiny$reactive({
        s <- dest_settings()
        if (isTRUE(s$enabled) && nzchar(s$path)) s$path else NULL
      })
    )

    # Settings opened from sidebar gear button — pre-fill with currently active path
    shiny$observeEvent(
      deconvolution_sidebar_vars$open_settings_clicked(),
      {
        open_settings_modal(
          initial_path = deconvolution_sidebar_vars$targetpath()
        )
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Deconvolution process server
    deconvolution_main_vars <- deconvolution_main$server(
      "deconvolution_main",
      deconvolution_sidebar_vars,
      conversion_main_vars,
      reset_button = reset_button,
      config_file = configfile
    )

    # Conversion sidebar server
    conversion_sidebar_vars <- conversion_sidebar$server(
      "conversion_sidebar",
      conversion_main_vars,
      deconvolution_main_vars,
      config_file = configfile,
      config_filename = config_filename
    )

    # Conversion main server
    conversion_main_vars <- conversion_main$server(
      "conversion_main",
      conversion_sidebar_vars,
      deconvolution_main_vars,
      config_file = configfile
    )

    # Check update availability
    version_info <- readLines("resources/version.txt", warn = FALSE)

    local_version <- sub(".*=", "", version_info[1])
    release <- sub(".*=", "", version_info[2])
    url <- sub(".*=", "", version_info[3])
    remote_version <- sub(".*=", "", check_github_version())

    if (identical(local_version, remote_version)) {
      # Variables for modal
      message <- "KiwiMS is up-to-date"
      hint <- "No action needed. Update anyway?"
      release_url <- get_latest_release_url()
      link <- ifelse(
        is.null(release_url),
        "https://github.com/infinity-a11y/KiwiMS/tree/master",
        release_url
      )

      # Variables for button
      icon <- shiny$icon("circle-info")
      label <- "Version"

      write_log(paste("KiWiFlow Version", local_version, "-", message))
    } else {
      # Variables for modal
      message <- "Update available"
      hint <- paste(
        "Download the latest version <strong>",
        remote_version,
        "</strong>from the release page:"
      )
      release_url <- get_latest_release_url()
      link <- ifelse(
        is.null(release_url),
        "https://github.com/infinity-a11y/KiwiMS/tree/master",
        release_url
      )

      # Variables for button
      icon <- shiny$icon("circle-exclamation")
      label <- "Update"

      write_log(paste("KiWiFlow Version", local_version, "-", message))
    }

    output$update_button <- shiny$renderUI({
      shiny$req(icon, label)

      shiny::tags$a(
        icon,
        label
      )

      # shiny$actionButton(
      #   inputId = ns("open_update_modal"),
      #   label = label,
      #   icon = icon,
      #   class = "nav-link"
      # )
    })

    # Switch Protein Conversion tab when user forwards
    shiny$observeEvent(deconvolution_main_vars$forward_deconvolution(), {
      bslib::nav_select(
        "tabs",
        session = session,
        "Protein Conversion"
      )
    })

    # Switch back to Deconvolution module when user forwards
    shiny$observeEvent(conversion_main_vars$cancel_continuation(), {
      bslib::nav_select(
        "tabs",
        session = session,
        "Deconvolution"
      )
    })

    # Config Modal Window ----

    # Nav button — filled green circle = active, outlined black circle = none
    output$config_nav_btn <- shiny$renderUI({
      indicator <- if (!is.null(configfile())) {
        shiny$tags$i(class = "fa-solid fa-circle config-nav-indicator--active")
      } else {
        shiny$tags$span(class = "config-nav-indicator--inactive")
      }
      shiny$actionButton(
        ns("config"),
        shiny$tagList(indicator, " Config"),
        class = "nav-link"
      )
    })

    # Download handler for example config file
    output$download_example_config <- shiny$downloadHandler(
      filename = "example_config.csv",
      content = function(file) {
        example <- data.frame(
          Sample = c("sample_1.raw", "sample_2.raw", "sample_3.raw"),
          Well = c("A1", "A2", "A3"),
          Compound_Concentration = c(100, 200, 100),
          Incubation_Time = c(120, 120, 60),
          Protein = c("RACA", "RACA", "RACA"),
          Compound_1 = c("Cmp1", "Cmp1", "Cmp2"),
          Compound_2 = c("Cmp2", "Cmp2", "Cmp3"),
          Compound_3 = c("Cmp3", "Cmp3", "Cmp4"),
          Compound_4 = c("Cmp4", "Cmp4", "Cmp5"),
          Compound_5 = c("Cmp5", "Cmp5", "Cmp6"),
          stringsAsFactors = FALSE
        )
        utils::write.csv2(example, file, row.names = FALSE)
      }
    )

    # Modal body — three pages: "upload", "preview", "confirmed"
    output$config_modal_body <- shiny$renderUI({
      state <- config_modal_state()

      if (state == "upload") {
        shiny$div(
          class = "config-modal-body",
          shiny$tags$p(
            "Upload a semicolon- or comma-separated ",
            shiny$tags$b(".csv"),
            " or ",
            shiny$tags$b(".xlsx"),
            " file that maps your sample files to experimental metadata."
          ),
          shiny$tags$table(
            class = "config-ref-table",
            shiny$tags$thead(
              shiny$tags$tr(
                shiny$tags$th("Column"),
                shiny$tags$th("Required"),
                shiny$tags$th("Format / Notes")
              )
            ),
            shiny$tags$tbody(
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Sample")),
                shiny$tags$td(class = "config-col-required", "Yes"),
                shiny$tags$td("Unique identifier per row, no duplicates")
              ),
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Protein")),
                shiny$tags$td(class = "config-col-required", "Yes"),
                shiny$tags$td("Protein name, no empty values")
              ),
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Well")),
                shiny$tags$td(class = "config-col-optional", "Optional"),
                shiny$tags$td(
                  "Valid well plate ID up to 384-well format (A1\u2013P24) \u00b7 all filled or all empty"
                )
              ),
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Compound_Concentration")),
                shiny$tags$td(class = "config-col-optional", "Optional"),
                shiny$tags$td("Numeric \u00b7 all filled or all empty")
              ),
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Incubation_Time")),
                shiny$tags$td(class = "config-col-optional", "Optional"),
                shiny$tags$td("Numeric \u00b7 all filled or all empty")
              ),
              shiny$tags$tr(
                shiny$tags$td(shiny$tags$code("Compound_1 \u2013 Compound_5")),
                shiny$tags$td(class = "config-col-required", "Min. 1"),
                shiny$tags$td(
                  "Compound names \u00b7 no duplicates within a row"
                )
              )
            )
          ),
          shiny$div(
            class = "config-upload-row",
            shiny$fileInput(
              ns("experiment_config"),
              label = NULL,
              placeholder = "Select .csv or .xlsx",
              accept = c(".csv", ".xlsx")
            ),
            shiny$downloadButton(
              ns("download_example_config"),
              "Example Table",
              class = "btn-sm btn-default"
            )
          ),
          shiny$uiOutput(ns("config_check"))
        )
      } else if (state == "preview") {
        df <- pending_config()
        n_compounds <- length(grep("^Compound_\\d+$", names(df)))
        shiny$div(
          class = "config-modal-body",
          shiny$tags$p(
            class = "config-preview-intro",
            "Please verify the table below matches your experiment layout.",
            " Confirm to activate this config across all modules."
          ),
          config_badge(
            "ok",
            "Valid",
            paste0(
              nrow(df),
              " samples \u00b7 ",
              n_compounds,
              " compound column(s)"
            )
          ),
          shiny$hr(class = "config-section-hr"),
          shiny$div(
            class = "config-table-scroll",
            shinycssloaders::withSpinner(
              shiny$tableOutput(ns("config_table")),
              type = 1,
              color = "#7777f9"
            )
          )
        )
      } else {
        df <- configfile()
        n_compounds <- length(grep("^Compound_\\d+$", names(df)))
        shiny$div(
          class = "config-modal-body",
          shiny$tags$p("A configuration file is currently active."),
          config_badge(
            "ok",
            "Active",
            paste0(
              nrow(df),
              " samples \u00b7 ",
              n_compounds,
              " compound column(s)"
            )
          ),
          shiny$tags$p(
            class = "config-filename-label",
            shiny$icon("file"),
            shiny$tags$span(class = "config-filename-text", config_filename())
          ),
          shiny$hr(class = "config-section-hr"),
          shiny$div(
            class = "config-table-scroll",
            shiny$tableOutput(ns("confirmed_config_table"))
          )
        )
      }
    })

    # Modal footer — three states (always includes Dismiss)
    output$config_modal_footer <- shiny$renderUI({
      state <- config_modal_state()
      if (state == "upload") {
        shiny$modalButton("Dismiss")
      } else if (state == "preview") {
        shiny$tagList(
          shiny$actionButton(
            ns("confirm_config"),
            "Confirm",
            class = "btn btn-default"
          ),
          shiny$modalButton("Dismiss")
        )
      } else {
        shiny$tagList(
          shiny$actionButton(
            ns("remove_config"),
            "Remove Config",
            class = "btn btn-default"
          ),
          shiny$modalButton("Dismiss")
        )
      }
    })

    # Preloaded modal bodies (defined here, referenced via uiOutput in showModal)
    output$licence_modal_body <- shiny$renderUI(licence_modal_body())
    output$unidec_modal_body <- shiny$renderUI(unidec_modal_body())
    output$update_modal_body <- shiny$renderUI({
      shiny$req(local_version, release, message, link, hint)
      update_modal_body(local_version, release, message, link, hint)
    })

    # Eagerly render all modal outputs even before the modal is opened
    shiny$outputOptions(output, "config_modal_body", suspendWhenHidden = FALSE)
    shiny$outputOptions(
      output,
      "config_modal_footer",
      suspendWhenHidden = FALSE
    )
    shiny$outputOptions(output, "licence_modal_body", suspendWhenHidden = FALSE)
    shiny$outputOptions(output, "unidec_modal_body", suspendWhenHidden = FALSE)
    shiny$outputOptions(output, "update_modal_body", suspendWhenHidden = FALSE)

    # Pending config table — Sys.sleep drives the spinner for 1 second
    output$config_table <- shiny$renderTable(
      {
        shiny$req(pending_config())
        Sys.sleep(1)
        pending_config()
      },
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE,
      spacing = "xs",
      na = ""
    )

    # Confirmed config table (no spinner needed)
    output$confirmed_config_table <- shiny$renderTable(
      {
        shiny$req(configfile())
        configfile()
      },
      striped = TRUE,
      hover = TRUE,
      bordered = TRUE,
      spacing = "xs",
      na = ""
    )

    # Validate on upload — 1-second spinner buffer before showing preview page
    shiny$observeEvent(input$experiment_config, {
      shiny$req(input$experiment_config)

      path <- input$experiment_config$datapath
      ext <- tolower(tools::file_ext(input$experiment_config$name))
      df <- tryCatch(read_config_file(path, ext), error = function(e) NULL)

      if (is.null(df)) {
        output$config_check <- shiny$renderUI(config_badge(
          "err",
          "Error",
          "Failed to read file."
        ))
        return()
      }
      if (nrow(df) == 0) {
        output$config_check <- shiny$renderUI(config_badge(
          "err",
          "Error",
          "File is empty."
        ))
        return()
      }

      df <- normalize_colnames(df)
      issues <- validate_config(df)

      if (length(issues) > 0) {
        output$config_check <- shiny$renderUI(
          config_badge("err", paste(length(issues), "issue(s)"), issues)
        )
        return()
      }

      # Clear pending first, switch UI (flush 1 → DOM element created with spinner),
      # then set data in the next flush so the spinner is visible.
      pending_config(NULL)
      config_modal_state("preview")
      df_captured <- df
      session$onFlushed(
        function() {
          pending_config(df_captured)
        },
        once = TRUE
      )
    })

    # Confirm — write to configfile, store filename, close modal, toast
    shiny$observeEvent(input$confirm_config, {
      configfile(pending_config())
      config_filename(input$experiment_config$name)
      pending_config(NULL)
      shiny$removeModal()
      show_toast(
        "Config saved!",
        text = NULL,
        type = "success",
        timer = 2000,
        timerProgressBar = TRUE
      )
    })

    # Cancel — discard pending, close modal
    shiny$observeEvent(input$cancel_config, {
      pending_config(NULL)
      shiny$removeModal()
    })

    # Remove — clear confirmed config, reset check output, switch to upload page
    shiny$observeEvent(input$remove_config, {
      configfile(NULL)
      config_filename(NULL)
      pending_config(NULL)
      output$config_check <- shiny$renderUI(NULL)
      config_modal_state("upload")
      show_toast(
        "Config removed",
        text = NULL,
        type = "warning",
        timer = 3000,
        timerProgressBar = TRUE
      )
    })

    # Shared helper — opens the config modal (used by nav button and sidebar shortcut)
    open_config_modal <- function(force_upload = FALSE) {
      pending_config(NULL)
      output$config_check <- shiny$renderUI(NULL)
      if (!force_upload && !is.null(configfile())) {
        config_modal_state("confirmed")
      } else {
        config_modal_state("upload")
      }
      shiny$showModal(
        shiny$div(
          class = "unidec-modal",
          shiny$modalDialog(
            title = "Experiment Configuration",
            size = "l",
            easyClose = TRUE,
            shiny$uiOutput(ns("config_modal_body")),
            footer = shiny$uiOutput(ns("config_modal_footer"))
          )
        )
      )
    }

    # Open modal via nav button
    shiny$observeEvent(input$config, {
      open_config_modal()
    })

    # Open modal via deconvolution sidebar shortcut
    shiny$observeEvent(
      deconvolution_sidebar_vars$open_config_clicked(),
      {
        open_config_modal()
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Open modal via conversion sidebar shortcut
    shiny$observeEvent(
      conversion_sidebar_vars$open_config_clicked(),
      {
        open_config_modal()
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Licence Modal Window ----
    shiny::observeEvent(input$licence, {
      shiny$showModal(
        shiny$div(
          class = "unidec-modal",
          shiny$modalDialog(
            title = "End-User License Agreement (GPL v3)",
            size = "l",
            easyClose = TRUE,
            shiny$uiOutput(ns("licence_modal_body")),
            footer = shiny$modalButton("Dismiss")
          )
        )
      )
    })

    # Unidec Modal Window ----
    shiny::observeEvent(input$unidec_click, {
      shiny$showModal(
        shiny$div(
          class = "unidec-modal",
          shiny$modalDialog(
            title = "UniDec - Acknowledgement",
            size = "l",
            easyClose = TRUE,
            shiny$uiOutput(ns("unidec_modal_body")),
            footer = shiny$modalButton("Dismiss")
          )
        )
      )
    })

    # Update modal
    shiny$observeEvent(input$open_update_modal, {
      shiny$showModal(
        shiny$div(
          class = "start-modal",
          shiny$modalDialog(
            title = "Version and Update",
            easyClose = TRUE,
            shiny$uiOutput(ns("update_modal_body")),
            footer = shiny$modalButton("Dismiss")
          )
        )
      )
    })

    session$onFlushed(
      function() {
        waiter_hide()
      },
      once = TRUE
    )
  })
}
