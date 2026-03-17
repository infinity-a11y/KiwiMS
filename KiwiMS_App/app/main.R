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
    helper_functions[
      check_github_version,
      config_badge,
      get_kiwims_version,
      get_latest_release_url,
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
      ),
      bslib$nav_menu(
        title = "Links",
        align = "right",
        icon = shiny$icon("link"),
        bslib$nav_item(
          shiny$tags$a(
            shiny$tags$span(
              shiny$tags$i(class = "fa-brands fa-github me-1"),
              "KiwiMS GitHub"
            ),
            href = "https://github.com/infinity-a11y/MSFlow",
            target = "_blank",
            class = "nav-link"
          )
        ),
        bslib$nav_item(
          shiny$tags$a(
            shiny$tags$span(
              shiny$tags$i(class = "fa-brands fa-github me-1"),
              "UniDec GitHub"
            ),
            href = "https://github.com/michaelmarty/UniDec",
            target = "_blank",
            class = "nav-link"
          )
        ),
        bslib$nav_item(
          shiny$tags$a(
            shiny$tags$span(
              shiny$tags$img(
                src = "static/liora_logo.png",
                style = "height: 1em; margin-right: 5px;"
              ),
              "Liora Bioinformatics"
            ),
            href = "https://www.liora-bioinformatics.com",
            target = "_blank",
            class = "nav-link"
          )
        )
      )
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

    # Deconvolution sidebar server
    deconvolution_sidebar_vars <- deconvolution_sidebar$server(
      "deconvolution_pars",
      reset_button = reset_button,
      config_file = configfile,
      config_filename = config_filename
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
      config_file = configfile
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
        timer = 3000,
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

    # Open modal via sidebar shortcut
    shiny$observeEvent(
      deconvolution_sidebar_vars$open_config_clicked(),
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

    # Hide waiter
    waiter_hide()
  })
}
