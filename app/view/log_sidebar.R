# app/view/log_sidebar.R

box::use(
  bslib[sidebar],
  shiny[
    actionButton,
    br,
    div,
    icon,
    downloadButton,
    moduleServer,
    NS,
    column,
    reactiveValues,
    reactive,
    downloadHandler
  ],
)

box::use(
  app / logic / logging[get_log]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  sidebar(
    title = "Log Actions",
    column(
      width = 12,
      align = "center",
      br(),
      br(),
      br(),
      div(
        class = "log-button",
        actionButton(
          ns("refresh_logs"),
          "Refresh Log",
          icon = icon("repeat")
        )
      ),
      br(),
      br(),
      div(
        class = "log-button",
        actionButton(
          ns("copy_logs"),
          "Clipboard",
          icon = icon("clipboard")
        )
      ),
      br(),
      br(),
      div(
        class = "log-button",
        downloadButton(
          ns("download_logs"),
          "Save File",
          icon = icon("download")
        )
      ),
      br()
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$download_logs <- downloadHandler(
      filename = function() {
        basename(get_log())
      },
      content = function(file) {
        if (file.exists(get_log())) {
          file.copy(get_log(), file)
        }
      }
    )

    refresh_logs <- reactive({
      input$refresh_logs
    })

    copy_logs <- reactive({
      input$copy_logs
    })

    reactiveValues(
      refresh = refresh_logs,
      copy = copy_logs
    )
  })
}
