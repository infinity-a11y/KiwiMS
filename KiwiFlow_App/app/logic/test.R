library(shiny)
library(rhandsontable)

ui <- fluidPage(
  rHandsontableOutput("hot"),
  verbatimTextOutput("table_output")
)

server <- function(input, output, session) {
  # Initial Data
  df <- data.frame(
    ID = 1:3,
    Value = c(10, 20, 30), # This column's initial R class is numeric
    stringsAsFactors = FALSE
  )

  # Render the table: allow invalid input (red highlight)
  output$hot <- renderRHandsontable({
    rhandsontable(df, strict = FALSE) %>%
      hot_col("Value", type = "numeric", allowInvalid = TRUE)
  })

  # Output the retrieved table from hot_to_r()
  output$table_output <- renderPrint({
    if (!is.null(input$hot)) {
      # *** KEY CHANGE IS HERE ***
      # By default, hot_to_r() tries to keep the original R column types (numeric)
      # which turns "a" into NA.
      # To preserve the character input, we must explicitly tell hot_to_r() to
      # treat the 'Value' column as a character vector.
      hot_to_r(
        input$hot,
        colClasses = c("ID" = "numeric", "Value" = "character")
      )
    }
  })
}

shinyApp(ui, server)
