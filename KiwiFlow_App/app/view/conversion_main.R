# app/view/conversion_card.R

box::use(
  shiny[moduleServer, NS],
  rhandsontable[rhandsontable, rHandsontableOutput, renderRHandsontable],
  utils[head],
)

box::use(
  app /
    logic /
    conversion_functions[
      sample_handsontable,
    ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bslib::navset_card_tab(
    id = ns("tabs"),
    bslib::nav_panel(
      "Proteins",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          rHandsontableOutput(ns("protein_table"))
        )
      )
    ),
    bslib::nav_panel(
      "Compounds",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          rHandsontableOutput(ns("compound_table"))
        )
      )
    ),
    bslib::nav_panel(
      "Samples",
      shiny::fluidRow(
        shiny::column(
          width = 2,
          shiny::actionButton(
            ns("add_compound"),
            label = "",
            icon = shiny::icon("plus")
          )
        ),
        shiny::column(
          width = 2,
          shiny::actionButton(
            ns("remove_compound"),
            label = "",
            icon = shiny::icon("minus")
          )
        )
      ),
      rHandsontableOutput(ns("sample_table"))
    )
  )
}

#' @export
server <- function(id, conversion_dirs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Set file upload limit
    options(shiny.maxRequestSize = 1000 * 1024^2)

    # Function to set the selected tab
    set_selected_tab <- function(tab_name) {
      bslib::nav_select(
        id = "tabs",
        selected = tab_name,
        session = session
      )
    }

    # Define reactive variables
    vars <- shiny::reactiveValues()
    vars$sample_tab <- NULL

    # Add/Remove compound columns
    shiny::observeEvent(input$add_compound, {
      shiny::req(input$sample_table)

      # Get client side table
      tab <- rhandsontable::hot_to_r(input$sample_table)

      # Add compound column to table
      n <- sum(grepl("Compound", colnames(tab)))
      colname <- paste0("Compound#", n + 1)
      tab[[colname]] <- ""

      output$sample_table <- renderRHandsontable({
        sample_handsontable(
          tab = tab,
          proteins = vars$protein_table$Protein,
          compounds = vars$compounds
        )
      })
    })

    shiny::observeEvent(input$remove_compound, {
      shiny::req(input$sample_table)

      # Get client side table
      tab <- rhandsontable::hot_to_r(input$sample_table)

      # Remove last compound column from table
      remove <- utils::tail(grep("Compound", colnames(tab)), 1)
      tab <- tab[, -(remove)]

      output$sample_table <- renderRHandsontable({
        sample_handsontable(
          tab = tab,
          proteins = vars$protein_table$Protein,
          compounds = vars$compounds
        )
      })
    })

    # Deactivate add/remove buttons when limits reached
    shiny::observe({
      shiny::req(input$sample_table)

      tab <- rhandsontable::hot_to_r(input$sample_table)

      shinyjs::toggleState(
        id = "add_compound",
        condition = length(grep("Compound", colnames(tab))) < 9
      )

      shinyjs::toggleState(
        id = "remove_compound",
        condition = length(grep("Compound", colnames(tab))) > 1
      )
    })

    # Observe sample input
    shiny::observe({
      # if (is.null(conversion_dirs)) {
      #   message("NULL")
      # } else {
      #   message(class(conversion_dirs))
      # }
      # message(class(conversion_dirs$result()))
      shiny::req(conversion_dirs$result())
      # message(conversion_dirs$result())
      file_path <- file.path(conversion_dirs$result())
      message(file_path)
      result <- readRDS(file_path)

      protein <- ifelse(length(vars$protein_table) == 1, vars$protein_table, "")
      compound <- ifelse(length(vars$compounds) == 1, vars$compounds, "")

      vars$sample_tab <- data.frame(
        Sample = head(names(result), -2),
        Protein = protein,
        Compound = compound
      )
    })

    # Render sample table
    shiny::observe({
      shiny::req(vars$sample_tab)

      # If already present render only client side state
      if (is.null(input$sample_table)) {
        tab <- vars$sample_tab
      } else {
        tab <- rhandsontable::hot_to_r(input$sample_table)
      }

      vars$trigger
      shiny::isolate(
        if (isTRUE(vars$col_add)) {
          tab[[vars$test]] <- ""

          vars$col_add <- FALSE
        }
      )

      output$sample_table <- renderRHandsontable({
        sample_handsontable(
          tab = tab,
          proteins = vars$protein_table$Protein,
          compounds = vars$compounds
        )
      })
    })

    # Observe compound IDs
    shiny::observe({
      vars$compounds <- rhandsontable::hot_to_r(input$compound_table)$Compound
    })

    # Render compound table
    shiny::observe({
      tab <- data.frame(
        Compound = as.character(rep(NA, 9)),
        mass_shift1 = as.numeric(rep(NA, 9)),
        mass_shift3 = as.numeric(rep(NA, 9)),
        mass_shift3 = as.numeric(rep(NA, 9)),
        mass_shift4 = as.numeric(rep(NA, 9)),
        mass_shift5 = as.numeric(rep(NA, 9)),
        mass_shift6 = as.numeric(rep(NA, 9)),
        mass_shift7 = as.numeric(rep(NA, 9)),
        mass_shift8 = as.numeric(rep(NA, 9)),
        mass_shift9 = as.numeric(rep(NA, 9))
      )

      colnames(tab) <- c(
        "Compound",
        "Mass 1",
        "Mass 2",
        "Mass 3",
        "Mass 4",
        "Mass 5",
        "Mass 6",
        "Mass 7",
        "Mass 8",
        "Mass 9"
      )

      output$compound_table <- renderRHandsontable({
        rhandsontable(
          tab,
          rowHeaders = NULL,
          stretchH = "all",
          width = "100%"
        ) |>
          rhandsontable::hot_cols(fixedColumnsLeft = 1) |>
          rhandsontable::hot_table(
            contextMenu = TRUE,
            highlightCol = TRUE,
            highlightRow = TRUE
          ) |>
          rhandsontable::hot_context_menu(
            allowRowEdit = TRUE,
            allowColEdit = FALSE
          ) |>
          rhandsontable::hot_validate_numeric(
            cols = 2:ncol(tab),
            min = 1,
            allowInvalid = TRUE
          )
      })
    })

    # Observe protein IDs
    shiny::observe({
      tryCatch(
        {
          vars$protein_table <- rhandsontable::hot_to_r(input$protein_table)
        }
      )
    })

    # Render compound table
    shiny::observe({
      tab <- data.frame(
        Protein = as.character(rep(NA, 9)),
        mass_shift1 = as.numeric(rep(NA, 9)),
        mass_shift3 = as.numeric(rep(NA, 9)),
        mass_shift3 = as.numeric(rep(NA, 9)),
        mass_shift4 = as.numeric(rep(NA, 9)),
        mass_shift5 = as.numeric(rep(NA, 9)),
        mass_shift6 = as.numeric(rep(NA, 9)),
        mass_shift7 = as.numeric(rep(NA, 9)),
        mass_shift8 = as.numeric(rep(NA, 9)),
        mass_shift9 = as.numeric(rep(NA, 9))
      )

      colnames(tab) <- c(
        "Protein",
        "Mass 1",
        "Mass 2",
        "Mass 3",
        "Mass 4",
        "Mass 5",
        "Mass 6",
        "Mass 7",
        "Mass 8",
        "Mass 9"
      )

      output$protein_table <- renderRHandsontable({
        output_tab <- rhandsontable(
          tab,
          rowHeaders = NULL,
          stretchH = "all",
          width = "100%"
        ) |>
          rhandsontable::hot_cols(fixedColumnsLeft = 1, ) |>
          rhandsontable::hot_table(
            contextMenu = TRUE,
            highlightCol = TRUE,
            highlightRow = TRUE
          ) |>
          rhandsontable::hot_context_menu(
            allowRowEdit = TRUE,
            allowColEdit = FALSE
          ) |>
          rhandsontable::hot_validate_numeric(
            cols = 2:ncol(tab),
            min = 1,
            allowInvalid = TRUE
          )

        return(output_tab)
      })
    })

    # Return currently selected tab
    list(
      selected_tab = shiny::reactive(input$tabs),
      set_selected_tab = set_selected_tab
    )
  })
}
