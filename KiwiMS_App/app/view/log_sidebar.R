# app/view/log_sidebar.R

box::use(
  bslib[sidebar, tooltip],
  shiny[
    actionButton,
    br,
    div,
    icon,
    moduleServer,
    NS,
    column,
    reactiveValues,
    reactive,
    renderPrint,
    verbatimTextOutput,
    observeEvent,
  ],
)

box::use(
  app / logic / logging[get_log, get_log_dir, get_session_id, get_session_start]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  sidebar(
    class = "deconvolution-sidebar",
    # width = "23rem",
    width = "17%",
    div(
      class = "deconvolution-sidebar-ui",
      div(
        class = "sidebar-section log-session-section",
        div(class = "sidebar-title custom-sidebar-title", "Current Session"),
        div(
          class = "session-info",
          div(
            class = "session-info-row",
            shiny::span(class = "session-info-label", "Date"),
            shiny::span(
              class = "session-info-value",
              format(Sys.Date(), "%Y-%m-%d")
            )
          ),
          div(
            class = "session-info-row",
            shiny::span(class = "session-info-label", "Started"),
            shiny::span(
              class = "session-info-value",
              format(get_session_start(), "%H:%M:%S")
            )
          ),
          div(
            class = "session-info-row",
            shiny::span(class = "session-info-label", "ID"),
            shiny::span(class = "session-info-value", get_session_id())
          )
        )
      ),
      div(
        class = "sidebar-section",
        div(class = "sidebar-title custom-sidebar-title", "Log Actions"),
        div(
          class = "log-button repeat",
          actionButton(
            ns("refresh_logs"),
            "Refresh",
            icon = icon("repeat")
          )
        ),
        div(
          class = "log-button",
          actionButton(
            ns("copy_logs"),
            "Clipboard",
            icon = icon("clipboard")
          )
        ),
        div(
          class = "log-button",
          actionButton(
            ns("show_log_file"),
            "Show File",
            icon = icon("magnifying-glass")
          )
        )
      ),
      div(
        class = "sidebar-section",
        div(class = "sidebar-title custom-sidebar-title", "Session Directory"),
        div(class = "sidebar-desc-label", "Current logging directory"),
        div(
          class = "log-dir-row",
          verbatimTextOutput(
            ns("log_dir_display")
          ),
          tooltip(
            div(
              class = "save-button",
              actionButton(
                ns("open_log_settings"),
                label = NULL,
                icon = icon("gear"),
                class = "btn-default"
              )
            ),
            "Log Settings",
            placement = "top"
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$show_log_file, {
      log_file <- get_log()
      if (file.exists(log_file)) {
        shell.exec(normalizePath(log_file))
      }
    })

    output$log_dir_display <- renderPrint({
      cat(normalizePath(get_log_dir(), winslash = "\\", mustWork = FALSE))
    })

    refresh_logs <- reactive({
      input$refresh_logs
    })

    copy_logs <- reactive({
      input$copy_logs
    })

    open_settings <- reactive({
      input$open_log_settings
    })

    reactiveValues(
      refresh = refresh_logs,
      copy = copy_logs,
      open_settings = open_settings
    )
  })
}
