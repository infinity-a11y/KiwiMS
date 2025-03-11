# app/view/log_view.R

box::use(
  bslib[card, card_header],
  shiny[
    tagList,
    div,
    htmlOutput,
    NS,
    moduleServer,
    observeEvent,
    HTML,
    renderUI,
    reactiveVal,
    observe
  ],
  clipr[write_clip],
)

box::use(
  app / logic / logging[get_log, format_log]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      id = ns("log-output"),
      htmlOutput(ns("log_output"))
    )
  )
}

#' @export
server <- function(id, active_tab_reactive, log_buttons) {
  moduleServer(id, function(input, output, session) {
    log_lines <- reactiveVal("")

    observe({
      if (active_tab_reactive() == "Logs") {
        log <- if (file.exists(get_log()))
          readLines(get_log(), warn = FALSE) else c("No logs yet.")

        log_lines(log)

        output$log_output <- renderUI({
          HTML(c(format_log(paste(log_lines(), collapse = "\n")), rep("", 5)))
        })
      }
    })

    observeEvent(log_buttons$refresh(), {
      if (active_tab_reactive() == "Logs") {
        log <- if (file.exists(get_log()))
          readLines(get_log(), warn = FALSE) else c("No logs yet.")

        log_lines(log)

        output$log_output <- renderUI({
          HTML(c(format_log(paste(log_lines(), collapse = "\n")), rep("", 5)))
        })
      }
    })

    observeEvent(log_buttons$copy(), {
      write_clip(log_lines())
    })
  })
}
