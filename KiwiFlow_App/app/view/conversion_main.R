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
      rHandsontableOutput(ns("protein_table"))
    ),
    bslib::nav_panel(
      "Compounds",
      rHandsontableOutput(ns("compound_table"))
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
          proteins = vars$proteins,
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
          proteins = vars$proteins,
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
    # shiny::observe({
    #   shiny::req(conversion_dirs$result())

    #   file_path <- file.path(conversion_dirs$result())
    #   result <- readRDS(file_path)

    #   protein <- ifelse(length(vars$proteins) == 1, vars$proteins, "")
    #   compound <- ifelse(length(vars$compounds) == 1, vars$compounds, "")

    #   vars$sample_tab <- data.frame(
    #     Sample = head(names(result), -2),
    #     Protein = protein,
    #     Compound = compound
    #   )
    # })

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
          proteins = vars$proteins,
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
        Compound = "",
        mass_shift1 = "",
        mass_shift3 = "",
        mass_shift3 = "",
        mass_shift4 = "",
        mass_shift5 = "",
        mass_shift6 = "",
        mass_shift7 = "",
        mass_shift8 = "",
        mass_shift9 = ""
      )

      colnames(tab) <- c(
        "Compound",
        "Mass Shift #1",
        "Mass Shift #2",
        "Mass Shift #3",
        "Mass Shift #4",
        "Mass Shift #5",
        "Mass Shift #6",
        "Mass Shift #7",
        "Mass Shift #8",
        "Mass Shift #9"
      )

      output$compound_table <- renderRHandsontable({
        rhandsontable(tab, rowHeaders = NULL) |>
          rhandsontable::hot_cols(fixedColumnsLeft = 1) |>
          rhandsontable::hot_table(
            contextMenu = FALSE,
            highlightCol = TRUE,
            highlightRow = TRUE
          )
      })
    })

    # Observe protein IDs
    shiny::observe({
      vars$proteins <- rhandsontable::hot_to_r(input$protein_table)$Protein
    })

    # Render compound table
    shiny::observe({
      tab <- data.frame(
        Protein = "",
        mass_shift1 = "",
        mass_shift3 = "",
        mass_shift3 = "",
        mass_shift4 = "",
        mass_shift5 = "",
        mass_shift6 = "",
        mass_shift7 = "",
        mass_shift8 = "",
        mass_shift9 = ""
      )

      colnames(tab) <- c(
        "Protein",
        "Mass Shift #1",
        "Mass Shift #2",
        "Mass Shift #3",
        "Mass Shift #4",
        "Mass Shift #5",
        "Mass Shift #6",
        "Mass Shift #7",
        "Mass Shift #8",
        "Mass Shift #9"
      )

      output$protein_table <- renderRHandsontable({
        rhandsontable(tab, rowHeaders = NULL) |>
          rhandsontable::hot_cols(fixedColumnsLeft = 1) |>
          rhandsontable::hot_table(
            contextMenu = FALSE,
            highlightCol = TRUE,
            highlightRow = TRUE
          )
      })
    })

    # Return currently selected tab
    list(
      selected_tab = shiny::reactive(input$tabs),
      set_selected_tab = set_selected_tab
    )
  })
}
