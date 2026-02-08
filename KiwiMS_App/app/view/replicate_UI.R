# Install required packages if needed (run once)
# install.packages(c("shiny", "rhandsontable", "dplyr"))

library(shiny)
library(rhandsontable)
library(dplyr)

# Sample data
initial_data <- data.frame(
  Sample_ID = paste("Sample", 1:10),
  Parameter1 = rnorm(10, mean = 50, sd = 5),
  Parameter2 = rnorm(10, mean = 100, sd = 10),
  Replicate_Group = rep(NA_character_, 10), # Use NA_character_ for string column
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  titlePanel("Lab Experiment Analysis: Marking Replicates with rhandsontable"),

  sidebarLayout(
    sidebarPanel(
      h4("Actions"),
      actionButton("mark_dup", "Mark Selected as Duplicates"),
      actionButton("mark_trip", "Mark Selected as Triplicates"),
      actionButton("ungroup", "Ungroup Selected"),
      textInput("group_id", "Custom Group ID (optional):", value = ""),
      br(),
      p(
        "Select rows by clicking and dragging over the row headers or cells (use Shift for contiguous ranges). Then use buttons to mark. Grouped rows get colored backgrounds."
      )
    ),

    mainPanel(
      rHandsontableOutput("params_table", height = "500px"), # Explicit height for visibility
      verbatimTextOutput("debug_output")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value for data
  rv <- reactiveValues(data = initial_data)

  # Render the rhandsontable
  output$params_table <- renderRHandsontable({
    message(
      "Column names on render: ",
      paste(colnames(rv$data), collapse = ", ")
    ) # Debug column names
    rhandsontable(
      rv$data,
      rowHeaders = TRUE,
      colHeaders = TRUE,
      stretchH = "all",
      selectCallback = TRUE, # NEW: Enable selection callback for input$..._select
      contextMenu = TRUE
    ) %>%
      hot_col(1, readOnly = TRUE) %>% # Index 1 for Sample_ID
      hot_col(4, readOnly = TRUE) %>% # Index 4 for Replicate_Group
      hot_cols(
        renderer = "
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          var data = instance.getSourceDataAtRow(row);
          var group = data.Replicate_Group;
          if (group && group !== null && group !== 'NA') {
            var hash = 0;
            for (var i = 0; i < group.length; i++) {
              hash = group.charCodeAt(i) + ((hash << 5) - hash);
            }
            var color = '#' + (Math.abs(hash) % 0xFFFFFF).toString(16).padStart(6, '0');
            td.style.backgroundColor = color + '33';  // Semi-transparent
          }
          return td;
        }
      "
      ) # Custom renderer for row coloring based on group
  })

  # Handle table changes (e.g., edits to parameters)
  observeEvent(input$params_table, {
    rv$data <- hot_to_r(input$params_table)
  })

  # Function to get selected rows (with improved null checks and nested select access)
  get_selected_rows <- function() {
    sel <- input$params_table_select
    if (
      is.null(sel) ||
        is.null(sel$select) ||
        is.null(sel$select$r) ||
        is.null(sel$select$r2) ||
        !is.numeric(sel$select$r) ||
        !is.numeric(sel$select$r2)
    ) {
      return(integer(0))
    }
    from <- min(sel$select$r, sel$select$r2)
    to <- max(sel$select$r, sel$select$r2)
    unique(seq(from, to)) # Rows are 1-indexed
  }

  # Function to mark replicates
  mark_replicates <- function(type) {
    selected <- get_selected_rows()
    if (length(selected) == 0) {
      return()
    }

    group_id <- if (input$group_id != "") {
      input$group_id
    } else {
      paste(type, sample(1000:9999, 1), sep = "-")
    }
    count <- if (type == "Duplicate") {
      2
    } else if (type == "Triplicate") {
      3
    } else {
      length(selected)
    }

    if (length(selected) != count) {
      showNotification(
        paste("Select exactly", count, "rows for", type),
        type = "warning"
      )
      return()
    }

    rv$data$Replicate_Group[selected] <- group_id
  }

  # Observe buttons
  observeEvent(input$mark_dup, {
    mark_replicates("Duplicate")
  })

  observeEvent(input$mark_trip, {
    mark_replicates("Triplicate")
  })

  observeEvent(input$ungroup, {
    selected <- get_selected_rows()
    if (length(selected) > 0) {
      rv$data$Replicate_Group[selected] <- NA_character_
    }
  })

  # Debug output
  output$debug_output <- renderPrint({
    paste(
      "Current data rows:",
      nrow(rv$data),
      "\nSelected rows:",
      paste(get_selected_rows(), collapse = ", ")
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
