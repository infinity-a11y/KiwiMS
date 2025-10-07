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
      prot_comp_handsontable,
      check_table,
      slice_tab,
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
          width = 3,
          shiny::div(
            class = "full-width-btn",
            shiny::actionButton(
              ns("confirm_proteins"),
              label = "Confirm",
              icon = shiny::icon("check")
            )
          )
        ),
        shiny::column(
          width = 2,
          shiny::div(
            class = "full-width-btn",
            shiny::actionButton(
              ns("edit_proteins"),
              label = "Edit",
              icon = shiny::icon("pen-to-square")
            )
          )
        ),
        shiny::column(
          width = 3,
          shiny::textOutput(ns("protein_table_info"))
        )
      ),
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
          width = 3,
          shiny::div(
            class = "full-width-btn",
            shiny::actionButton(
              ns("confirm_compounds"),
              label = "Confirm",
              icon = shiny::icon("check")
            )
          )
        ),
        shiny::column(
          width = 2,
          shiny::div(
            class = "full-width-btn",
            shiny::actionButton(
              ns("edit_compounds"),
              label = "Edit",
              icon = shiny::icon("pen-to-square")
            )
          )
        ),
        shiny::column(
          width = 3,
          shiny::textOutput(ns("compound_table_info"))
        )
      ),
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
          width = 3,
          shiny::div(
            class = "full-width-btn",
            shiny::actionButton(
              ns("confirm_samples"),
              label = "Confirm",
              icon = shiny::icon("check")
            )
          )
        ),
        shiny::column(
          width = 2,
          shiny::div(
            class = "full-width-btn",
            shiny::actionButton(
              ns("edit_samples"),
              label = "Edit",
              icon = shiny::icon("pen-to-square")
            )
          )
        ),
        shiny::column(
          width = 3,
          shiny::textOutput(ns("sample_table_info"))
        ),
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
      shiny::fluidRow(
        shiny::column(
          width = 12,
          rHandsontableOutput(ns("sample_table"))
        )
      )
    )
  )
}

#' @export
server <- function(id, conversion_dirs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Set file upload limit
    options(shiny.maxRequestSize = 1000 * 1024^2)

    # Preefine reactive variables
    vars <- shiny::reactiveValues(
      protein_table = NULL,
      compound_table = NULL,
      sample_tab = NULL
    )

    # Function to set the selected tab
    set_selected_tab <- function(tab_name) {
      bslib::nav_select(
        id = "tabs",
        selected = tab_name,
        session = session
      )
    }

    # Render table information
    output$compound_table_info <- output$protein_table_info <- shiny::renderText(
      "Editing table ..."
    )

    output$sample_table_info <- shiny::renderText(
      "Enter Proteins and Compounds first"
    )

    # Actions on edit button click
    shiny::observeEvent(
      input$edit_proteins | input$edit_compounds | input$edit_samples,
      {
        shiny::req(input$tabs)

        output$sample_table_info <- shiny::renderText({
          "Enter Proteins and Compounds first"
        })

        if (input$tabs == "Proteins") {
          # Mark tab as undone
          shinyjs::runjs(
            'document.querySelector(".nav-link[data-value=\'Proteins\']").classList.remove("done");'
          )

          # Render editable table
          output$protein_table <- rhandsontable::renderRHandsontable(
            prot_comp_handsontable(vars$protein_table, disabled = FALSE)
          )

          # Message info
          output$protein_table_info <- shiny::renderText(
            "Editing table ..."
          )
        } else if (input$tabs == "Compounds") {
          # Mark tab as undone
          shinyjs::runjs(
            'document.querySelector(".nav-link[data-value=\'Compounds\']").classList.remove("done");'
          )

          # Render editable table
          output$compound_table <- rhandsontable::renderRHandsontable(
            prot_comp_handsontable(vars$compound_table, disabled = FALSE)
          )

          # Message info
          output$compound_table_info <- shiny::renderText(
            "Editing table ..."
          )
        } else if (input$tabs == "Samples") {
          shinyjs::runjs(
            'document.querySelector(".nav-link[data-value=\'Samples\']").classList.remove("done");'
          )

          # Message info
          output$sample_table_info <- shiny::renderText(
            "Editing table ..."
          )
        }

        # Toggle class on button click
        id_module <- paste0("module_", input$tabs, "_box")

        shinyjs::removeClass(id = id_module, class = "done")
      }
    )

    # Actions on confirming input table
    shiny::observeEvent(
      input$confirm_proteins |
        input$confirm_compounds |
        input$confirm_samples,
      {
        if (input$tabs == "Proteins") {
          shiny::req(input$protein_table)

          # Retrieve sliced user input table
          protein_table <- slice_tab(rhandsontable::hot_to_r(
            input$protein_table
          ))

          # Validate correct input
          protein_table_status <- check_table(
            protein_table,
            col_limit = 10
          )

          if (isTRUE(protein_table_status)) {
            # If table validation successful

            # Mark as done
            shinyjs::runjs(
              'document.querySelector(".nav-link[data-value=\'Proteins\']").classList.add("done");'
            )

            # Message success
            output$protein_table_info <- shiny::renderText("Table saved!")

            # Render table disabled
            output$protein_table <- rhandsontable::renderRHandsontable(
              prot_comp_handsontable(protein_table, disabled = TRUE)
            )

            # Render sample table with new input
            if (!is.null(input$sample_table)) {
              output$sample_table <- rhandsontable::renderRHandsontable(
                sample_handsontable(
                  tab = rhandsontable::hot_to_r(input$sample_table),
                  proteins = protein_table$Protein,
                  compounds = vars$compound_table$Compound
                )
              )

              # Jump to next tab module
              set_selected_tab("Samples")
            } else {
              # Jump to next tab module
              set_selected_tab("Compounds")
            }

            # Assign user input to reactive table variable
            vars$protein_table <- protein_table
          } else {
            # If protein table validation unsuccessful

            output$protein_table_info <- shiny::renderText(protein_table_status)
          }
        } else if (input$tabs == "Compounds") {
          shiny::req(input$compound_table)

          # Retrieve sliced user input table
          compound_table <- slice_tab(rhandsontable::hot_to_r(
            input$compound_table
          ))

          # Validate correct input
          compound_table_status <- check_table(
            compound_table,
            col_limit = 10
          )

          if (isTRUE(compound_table_status)) {
            # If table validation successful

            # Mark as done
            shinyjs::runjs(
              'document.querySelector(".nav-link[data-value=\'Compounds\']").classList.add("done");'
            )

            # Message success
            output$compound_table_info <- shiny::renderText("Table saved!")

            # Render table disabled
            output$compound_table <- rhandsontable::renderRHandsontable(
              prot_comp_handsontable(compound_table, disabled = TRUE)
            )

            # Render sample table with new input
            if (!is.null(input$sample_table)) {
              output$sample_table <- rhandsontable::renderRHandsontable({
                sample_handsontable(
                  tab = rhandsontable::hot_to_r(input$sample_table),
                  proteins = vars$protein_table$Protein,
                  compounds = compound_table$Compound
                )
              })
            }

            # Assign user input to reactive table variable
            vars$compound_table <- compound_table

            # Jump to next tab module
            set_selected_tab("Samples")
          } else {
            # If protein table validation unsuccessful
            output$compound_table_info <- shiny::renderText(
              compound_table_status
            )
          }
        }
      }
    )

    # Add/Remove compound columns
    shiny::observeEvent(input$add_compound, {
      shiny::req(input$sample_table)

      # Get client side table
      tab <- rhandsontable::hot_to_r(input$sample_table)

      # Add compound column to table
      n <- sum(grepl("Compound", colnames(tab)))
      colname <- paste0("Compound#", n + 1)
      tab[[colname]] <- ""

      output$sample_table <- rhandsontable::renderRHandsontable({
        sample_handsontable(
          tab = tab,
          proteins = vars$protein_table$Protein,
          compounds = vars$compound_table$Compound
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

      output$sample_table <- rhandsontable::renderRHandsontable({
        sample_handsontable(
          tab = tab,
          proteins = vars$protein_table$Protein,
          compounds = vars$compound_table$Compound
        )
      })
    })

    # Deactivate add/remove buttons when limits reached
    shiny::observe({
      shiny::req(input$sample_table)

      shinyjs::toggleState(
        id = "add_compound",
        condition = length(grep(
          "Compound",
          colnames(rhandsontable::hot_to_r(input$sample_table))
        )) <
          9
      )

      shinyjs::toggleState(
        id = "remove_compound",
        condition = length(grep(
          "Compound",
          colnames(rhandsontable::hot_to_r(input$sample_table))
        )) >
          1
      )
    })

    # Observe sample input
    shiny::observe({
      if (is.null(vars$protein_table) || is.null(vars$compound_table)) {
        output$sample_table_info <- shiny::renderText({
          "Enter Proteins and Compounds first"
        })
      } else if (is.null(conversion_dirs$result())) {
        output$sample_table_info <- shiny::renderText({
          "Upload result file"
        })
      } else {
        output$sample_table_info <- shiny::renderText({
          "Editing table ..."
        })

        file_path <- file.path(conversion_dirs$result())
        result <- readRDS(file_path)

        sample_tab <- data.frame(
          Sample = head(names(result), -2),
          Protein = ifelse(
            length(vars$protein_table$Protein) == 1,
            vars$protein_table$Protein,
            ""
          ),
          Compound = ifelse(
            length(vars$compound_table$Compound) == 1,
            vars$compound_table$Compound,
            ""
          )
        )

        if (!isTRUE(vars$sample_tab_initial)) {
          output$sample_table <- rhandsontable::renderRHandsontable({
            sample_handsontable(
              tab = sample_tab,
              proteins = vars$protein_table$Protein,
              compounds = vars$compound_table$Compound
            )
          })
        }

        vars$sample_tab_initial <- TRUE
      }
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

      output$compound_table <- rhandsontable::renderRHandsontable({
        prot_comp_handsontable(tab)
      })
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

      output$protein_table <- rhandsontable::renderRHandsontable({
        prot_comp_handsontable(tab)
      })
    })

    # Return currently selected tab
    list(
      selected_tab = shiny::reactive(input$tabs),
      set_selected_tab = set_selected_tab
    )
  })
}
