# app/main.R

box::use(
  bslib,
  shiny,
  shinyjs[disable, enable, hide, hidden, show, runjs, useShinyjs],
  waiter[useWaiter, waiter_hide, waiterShowOnLoad],
)

box::use(
  app / logic / dev_utils,
  app / view / conversion_main,
  app / view / conversion_sidebar,
  app / view / deconvolution_process,
  app / view / deconvolution_sidebar,
  app / view / ki_kinact_sidebar,
  app / view / log_view,
  app / view / log_sidebar,
  app / logic / logging[start_logging, write_log, close_logging],
  app /
    logic /
    helper_functions[
      check_github_version,
      get_kiwiflow_version,
      get_latest_release_url,
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
          "KiwiFlow"
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
          "KiwiFlow",
          style = "font-size: 21px; font-family: monospace;"
        )
      ),
      window_title = paste("KiwiFlow", get_kiwiflow_version()["version"]),
      underline = TRUE,
      bslib$nav_panel(
        title = "Deconvolution",
        bslib$page_sidebar(
          sidebar = deconvolution_sidebar$ui(
            ns("deconvolution_pars")
          ),
          deconvolution_process$ui(
            ns("deconvolution_process")
          )
        )
      ),
      bslib$nav_panel(
        title = "Protein Conversion",
        class = "locked-panel",
        shiny$div(id = "overlay-message", "Module still in work ..."),
        bslib$page_sidebar(
          sidebar = conversion_sidebar$ui(ns("protein_conversion")),
          bslib$card(
            bslib$card_header("Conversion Table"),
            conversion_main$ui(ns("conversion_card"))
          )
        )
      ),
      bslib$nav_panel(
        title = "kinact/KI",
        class = "locked-panel",
        shiny$div(id = "overlay-message", "Module still in work ..."),
        bslib$page_sidebar(
          sidebar = ki_kinact_sidebar$ui(ns("ki")),
          bslib$navset_card_tab(
            bslib$nav_panel(title = "Kobs Table"),
            bslib$nav_panel(title = "Kinact Table")
          )
        )
      ),
      bslib$nav_panel(
        title = "Logs",
        bslib$page_sidebar(
          sidebar = log_sidebar$ui(ns("log_sidebar")),
          bslib$card(
            log_view$ui(ns("logs"))
          )
        )
      ),
      bslib$nav_spacer(),
      bslib$nav_menu(
        title = "Links",
        align = "right",
        icon = shiny$icon("link"),
        bslib$nav_item(
          shiny$tags$a(
            shiny$tags$span(
              shiny$tags$i(class = "fa-brands fa-github me-1"),
              "GitHub"
            ),
            href = "https://github.com/infinity-a11y/MSFlow",
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
            href = "https://www.liora-bioinformatics.com/home",
            target = "_blank",
            class = "nav-link"
          )
        )
      ),
      bslib$nav_item(
        shiny$uiOutput(ns("update_button"))
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Kill server on session end
    session$onSessionEnded(function() {
      write_log("Session closed")
      shiny$stopApp()
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

    # Conversion server
    conversion_main$server("conversion_card")

    reset_button <- shiny$reactiveVal(0)

    # Deconvolution sidebar server
    dirs <- deconvolution_sidebar$server(
      "deconvolution_pars",
      reset_button = reset_button
    )

    # Deconvolution process server
    deconvolution_process$server(
      "deconvolution_process",
      dirs,
      reset_button = reset_button
    )

    # Check update availability
    version_info <- readLines("resources/version.txt", warn = FALSE)

    local_version <- sub(".*=", "", version_info[1])
    release <- sub(".*=", "", version_info[2])
    url <- sub(".*=", "", version_info[3])
    remote_version <- sub(".*=", "", check_github_version())

    if (identical(local_version, remote_version)) {
      # Variables for modal
      message <- "KiwiFlow is up-to-date"
      hint <- "No action needed. Update anyway?"
      release_url <- get_latest_release_url()
      link <- ifelse(
        is.null(release_url),
        "https://github.com/infinity-a11y/KiwiFlow/tree/master",
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
        "https://github.com/infinity-a11y/KiwiFlow/tree/master",
        release_url
      )

      # Variables for button
      icon <- shiny$icon("circle-exclamation")
      label <- "Update"

      write_log(paste("KiWiFlow Version", local_version, "-", message))
    }

    output$update_button <- shiny$renderUI({
      shiny$req(icon, label)

      shiny$actionButton(
        inputId = ns("open_update_modal"),
        label = label,
        icon = icon,
        class = "nav-link"
      )
    })

    # Update modal
    shiny$observeEvent(input$open_update_modal, {
      shiny$req(local_version, release, message, link, hint)

      shiny$showModal(
        shiny$div(
          class = "start-modal",
          shiny$modalDialog(
            shiny$fluidRow(
              shiny$br(),
              shiny$column(
                width = 11,
                shiny$fluidRow(
                  shiny$column(
                    width = 6,
                    shiny$p("Current Version")
                  ),
                  shiny$column(
                    width = 6,
                    shiny$p(local_version, style = "font-style: italic")
                  )
                ),
                shiny$fluidRow(
                  shiny$column(
                    width = 6,
                    shiny$p("Release date")
                  ),
                  shiny$column(
                    width = 6,
                    shiny$p(release, style = "font-style: italic")
                  )
                ),
                shiny$br(),
                shiny$fluidRow(
                  shiny$column(
                    width = 12,
                    shiny$h6(message, style = "font-weight: bold"),
                    shiny$p(
                      shiny$HTML(hint),
                      style = "font-style: italic; margin-top: 1rem;"
                    ),
                    shiny$tags$a(href = link, link, target = "_blank")
                  )
                )
              )
            ),
            title = "Version and Update",
            easyClose = TRUE,
            footer = shiny$tagList(
              shiny$modalButton("Dismiss")
            )
          )
        )
      )
    })

    # Hide waiter
    Sys.sleep(2)
    waiter_hide()
  })
}
