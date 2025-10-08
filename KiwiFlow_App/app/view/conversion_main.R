# app/view/conversion_card.R

box::use(
  shiny[moduleServer, NS],
  rhandsontable[rhandsontable, rHandsontableOutput, renderRHandsontable],
  utils[head],
  readxl[read_excel],
  tools[file_ext],
)

box::use(
  app /
    logic /
    conversion_functions[
      sample_handsontable,
      prot_comp_handsontable,
      check_table,
      slice_tab,
      set_selected_tab
    ],
  app /
    logic /
    conversion_constants[
      empty_tab,
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
            class = "table-input",
            shiny::fileInput(
              ns("proteins_fileinput"),
              "",
              multiple = FALSE,
              accept = c(".csv", ".tsv", ".xlsx", ".xls", ".txt")
            )
          )
        ),
        shiny::column(
          width = 3,
          shiny::textOutput(ns("protein_table_info"))
        ),
        shiny::column(
          width = 2,
          shiny::div(
            class = "full-width-btn",
            shinyjs::disabled(
              shiny::actionButton(
                ns("confirm_proteins"),
                label = "Save",
                icon = shiny::icon("bookmark")
              )
            )
          )
        ),
        shiny::column(
          width = 2,
          shiny::div(
            class = "full-width-btn",
            shinyjs::disabled(
              shiny::actionButton(
                ns("edit_proteins"),
                label = "Edit",
                icon = shiny::icon("pen-to-square")
              )
            )
          )
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
            class = "table-input",
            shiny::fileInput(
              ns("compounds_fileinput"),
              "",
              multiple = FALSE,
              accept = c(".csv", ".tsv", ".xlsx", ".xls", ".txt")
            )
          )
        ),
        shiny::column(
          width = 3,
          shiny::textOutput(ns("compound_table_info"))
        ),
        shiny::column(
          width = 2,
          shiny::div(
            class = "full-width-btn",
            shinyjs::disabled(
              shiny::actionButton(
                ns("confirm_compounds"),
                label = "Save",
                icon = shiny::icon("bookmark")
              )
            )
          )
        ),
        shiny::column(
          width = 2,
          shiny::div(
            class = "full-width-btn",
            shinyjs::disabled(
              shiny::actionButton(
                ns("edit_compounds"),
                label = "Edit",
                icon = shiny::icon("pen-to-square")
              )
            )
          )
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
            class = "table-input",
            shiny::fileInput(
              ns("result_input"),
              "Select File",
              multiple = FALSE,
              accept = c(".rds")
            )
          )
        ),
        shiny::column(
          width = 2,
          shiny::div(
            class = "full-width-btn",
            shiny::actionButton(
              ns("confirm_samples"),
              label = "Save",
              icon = shiny::icon("bookmark")
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

    # Predefine reactive variables
    vars <- shiny::reactiveValues(
      protein_table = NULL,
      protein_table_active = TRUE,
      protein_table_status = FALSE,
      compound_table = NULL,
      compound_table_active = TRUE,
      compound_table_status = FALSE,
      sample_tab = NULL
    )

    # Helper function to read uploaded files
    read_uploaded_file <- function(file_path, ext) {
      tryCatch(
        {
          if (ext %in% c("csv", "txt")) {
            # Try reading with header first
            df_header <- read.csv(
              file_path,
              stringsAsFactors = FALSE,
              header = TRUE
            )
            has_header <- any(is.na(as.numeric(colnames(df_header)[-1])))
            if (!has_header) {
              df <- read.csv(
                file_path,
                stringsAsFactors = FALSE,
                header = FALSE
              )
            } else {
              df <- df_header
            }
          } else if (ext == "tsv") {
            df_header <- read.delim(
              file_path,
              stringsAsFactors = FALSE,
              header = TRUE
            )
            has_header <- any(is.na(as.numeric(colnames(df_header)[-1])))
            if (!has_header) {
              df <- read.delim(
                file_path,
                stringsAsFactors = FALSE,
                header = FALSE
              )
            } else {
              df <- df_header
            }
          } else if (ext %in% c("xlsx", "xls")) {
            df_header <- read_excel(file_path, col_names = TRUE)
            df_header_test <<- df_header
            has_header <- any(is.na(as.numeric(colnames(df_header)[-1])))
            has_header_test <<- has_header
            if (!has_header) {
              df <- read_excel(file_path, col_names = FALSE)
            } else {
              df <- df_header
            }
          } else {
            stop("Unsupported file format")
          }
          # Ensure column names are standardized
          colnames(df) <- trimws(colnames(df))
          return(df)
        },
        error = function(e) {
          shinyWidgets::show_toast(
            "Error reading file",
            text = e$message,
            type = "error",
            timer = 5000
          )
          return(NULL)
        }
      )
    }

    # Helper function to process uploaded table
    process_uploaded_table <- function(df, type, header) {
      testest <<- df
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }

      expected_cols <- if (type == "protein") {
        c("Protein", paste("Mass", 1:9))
      } else {
        c("Compound", paste("Mass", 1:9))
      }

      # Take first up to 10 columns
      num_cols <- min(ncol(df), 10)
      df <- df[, 1:num_cols, drop = FALSE]

      # Rename columns to expected
      colnames(df) <- expected_cols[1:num_cols]

      # Add missing columns with NAs if less than 10
      if (num_cols < 10) {
        for (i in (num_cols + 1):10) {
          df[[expected_cols[i]]] <- NA
        }
      }

      # Convert mass columns to numeric
      mass_cols <- paste("Mass", 1:9)
      for (col in mass_cols) {
        if (col %in% colnames(df)) {
          original <- df[[col]]
          numeric_vals <- suppressWarnings(as.numeric(original))
          if (any(is.na(numeric_vals) & !is.na(original))) {
            shinyWidgets::show_toast(
              "Conversion error",
              text = paste(
                "Column",
                col,
                "contains non-numeric values that cannot be converted."
              ),
              type = "error",
              timer = 5000
            )
            return(NULL)
          }
          df[[col]] <- numeric_vals
        }
      }

      return(df)
    }

    # Observe protein file upload
    shiny::observeEvent(input$proteins_fileinput, {
      shiny::req(input$proteins_fileinput)

      df <- read_uploaded_file(
        input$proteins_fileinput$datapath,
        tolower(file_ext(input$proteins_fileinput$name))
      )

      file_path <<- input$proteins_fileinput$datapath

      df <- process_uploaded_table(df, "protein")

      if (!is.null(df)) {
        vars$protein_table <- df
        vars$protein_table_status <- TRUE

        output$protein_table <- rhandsontable::renderRHandsontable(
          prot_comp_handsontable(df, disabled = FALSE)
        )

        shinyWidgets::show_toast(
          "Protein table loaded!",
          type = "success",
          timer = 3000
        )
      } else {
        shinyWidgets::show_toast(
          "Loading protein table failed!",
          type = "error",
          timer = 3000
        )
      }
    })

    # Observe compound file upload
    shiny::observeEvent(input$compounds_fileinput, {
      shiny::req(input$compounds_fileinput)

      df <- read_uploaded_file(
        input$compounds_fileinput$datapath,
        tolower(file_ext(input$compounds_fileinput$name))
      )

      df <- process_uploaded_table(df, "compound")

      if (!is.null(df)) {
        vars$compound_table <- df
        vars$compound_table_status <- TRUE

        output$compound_table <- rhandsontable::renderRHandsontable(
          prot_comp_handsontable(df, disabled = FALSE)
        )

        shinyWidgets::show_toast(
          "Compound table loaded!",
          type = "success",
          timer = 3000
        )
      } else {
        shinyWidgets::show_toast(
          "Loading compound table failed!",
          type = "error",
          timer = 3000
        )
      }
    })

    # Observe table status for protein table
    shiny::observe({
      shiny::req(input$protein_table, vars$protein_table_active)

      # Retrieve sliced user input table
      protein_table <- slice_tab(rhandsontable::hot_to_r(
        input$protein_table
      ))

      # If table non-empty check for correctness
      if (nrow(protein_table) < 1) {
        # Set status variable to FALSE
        vars$protein_table_status <- FALSE

        # UI feedback
        shinyjs::removeClass(
          "protein_table_info",
          "table-info-green"
        )
        shinyjs::addClass(
          "protein_table_info",
          "table-info-red"
        )
        output$protein_table_info <- shiny::renderText(
          "Fill table ..."
        )
        shinyjs::disable("confirm_proteins")
      } else {
        # Validate correct input
        protein_table_status <- check_table(
          protein_table,
          col_limit = 10
        )

        if (isTRUE(protein_table_status)) {
          # Set status variable to TRUE
          vars$protein_table_status <- TRUE

          # UI feedback
          shinyjs::removeClass(
            "protein_table_info",
            "table-info-red"
          )
          shinyjs::addClass(
            "protein_table_info",
            "table-info-green"
          )
          output$protein_table_info <- shiny::renderText(
            "Table can be saved"
          )
          shinyjs::enable("confirm_proteins")
        } else {
          # Set status variable to FALSE
          vars$protein_table_status <- FALSE

          # UI feedback
          shinyjs::removeClass(
            "protein_table_info",
            "table-info-green"
          )
          shinyjs::addClass(
            "protein_table_info",
            "table-info-red"
          )
          output$protein_table_info <- shiny::renderText(protein_table_status)
          shinyjs::disable("confirm_proteins")
        }
      }
    })

    # Observe table status for compound table
    shiny::observe({
      shiny::req(input$compound_table, vars$compound_table_active)

      test <<- rhandsontable::hot_to_r(
        input$compound_table
      )

      # Retrieve sliced user input table
      compound_table <- slice_tab(rhandsontable::hot_to_r(
        input$compound_table
      ))

      # If table non-empty check for correctness
      if (nrow(compound_table) < 1) {
        # Set status variable to FALSE
        vars$compound_table_status <- FALSE

        # UI feedback
        shinyjs::removeClass(
          "compound_table_info",
          "table-info-green"
        )
        shinyjs::addClass(
          "compound_table_info",
          "table-info-red"
        )
        output$compound_table_info <- shiny::renderText(
          "Fill table ..."
        )
        shinyjs::disable("confirm_compounds")
      } else {
        # Validate correct input
        compound_table_status <- check_table(
          compound_table,
          col_limit = 10
        )

        if (isTRUE(compound_table_status)) {
          # Set status variable to TRUE
          vars$compound_table_status <- TRUE

          # UI feedback
          shinyjs::removeClass(
            "compound_table_info",
            "table-info-red"
          )
          shinyjs::addClass(
            "compound_table_info",
            "table-info-green"
          )
          output$compound_table_info <- shiny::renderText(
            "Table can be saved"
          )
          shinyjs::enable("confirm_compounds")
        } else {
          # Set status variable to FALSE
          vars$compound_table_status <- FALSE

          # UI feedback
          shinyjs::removeClass(
            "compound_table_info",
            "table-info-green"
          )
          shinyjs::addClass(
            "compound_table_info",
            "table-info-red"
          )
          output$compound_table_info <- shiny::renderText(compound_table_status)
          shinyjs::disable("confirm_compounds")
        }
      }
    })

    # Actions on edit button click
    shiny::observeEvent(
      input$edit_proteins | input$edit_compounds | input$edit_samples,
      {
        shiny::req(input$tabs)

        if (input$tabs == "Proteins") {
          # Make table observer active
          vars$protein_table_active <- TRUE

          # Mark tab as undone
          shinyjs::runjs(
            'document.querySelector(".nav-link[data-value=\'Proteins\']").classList.remove("done");'
          )

          # Render editable table
          output$protein_table <- rhandsontable::renderRHandsontable(
            prot_comp_handsontable(vars$protein_table, disabled = FALSE)
          )

          # Change buttons
          shiny::updateActionButton(
            session = session,
            "confirm_proteins",
            label = "Save",
            icon = shiny::icon("bookmark")
          )
          shinyjs::enable("confirm_proteins")
          shinyjs::disable("edit_proteins")
        } else if (input$tabs == "Compounds") {
          # Make table observer active
          vars$compound_table_active <- TRUE

          # Mark tab as undone
          shinyjs::runjs(
            'document.querySelector(".nav-link[data-value=\'Compounds\']").classList.remove("done");'
          )

          # Render editable table
          output$compound_table <- rhandsontable::renderRHandsontable(
            prot_comp_handsontable(vars$compound_table, disabled = FALSE)
          )

          # Change buttons
          shiny::updateActionButton(
            session = session,
            "confirm_compounds",
            label = "Save",
            icon = shiny::icon("bookmark")
          )
          shinyjs::enable("confirm_compounds")
          shinyjs::disable("edit_compounds")
        } else if (input$tabs == "Samples") {
          # Mark tab as undone
          shinyjs::runjs(
            'document.querySelector(".nav-link[data-value=\'Samples\']").classList.remove("done");'
          )
        }
      }
    )

    # Actions on confirming input table
    shiny::observeEvent(
      input$confirm_proteins |
        input$confirm_compounds |
        input$confirm_samples,
      {
        if (input$tabs == "Proteins") {
          # If table can be saved perform actions
          if (vars$protein_table_status) {
            shiny::req(input$protein_table)

            # Retrieve sliced user input table
            protein_table <- slice_tab(rhandsontable::hot_to_r(
              input$protein_table
            ))

            # Mark UI as done
            shinyjs::runjs(
              'document.querySelector(".nav-link[data-value=\'Proteins\']").classList.add("done");'
            )
            shinyWidgets::show_toast(
              "Table saved!",
              text = NULL,
              type = "success",
              timer = 3000,
              timerProgressBar = TRUE
            )
            shiny::updateActionButton(
              session = session,
              "confirm_proteins",
              label = "Saved",
              icon = shiny::icon("check")
            )
            shinyjs::disable("confirm_proteins")
            shinyjs::enable("edit_proteins")

            # Keep table editable
            output$protein_table <- rhandsontable::renderRHandsontable(
              prot_comp_handsontable(protein_table, disabled = TRUE)
            )

            # Inactivate table observer
            vars$protein_table_active <- FALSE

            # Show table message
            output$protein_table_info <- shiny::renderText("Table saved!")

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
              set_selected_tab("Samples", session)
            } else {
              # Jump to next tab module
              set_selected_tab("Compounds", session)
            }

            # Assign user input to reactive table variable
            vars$protein_table <- protein_table
          }
        } else if (input$tabs == "Compounds") {
          # If table can be saved perform actions
          if (vars$compound_table_status) {
            shiny::req(input$compound_table)

            # Retrieve sliced user input table
            compound_table <- slice_tab(rhandsontable::hot_to_r(
              input$compound_table
            ))

            # Mark UI as done
            shinyjs::runjs(
              'document.querySelector(".nav-link[data-value=\'Compounds\']").classList.add("done");'
            )
            shinyWidgets::show_toast(
              "Table saved!",
              text = NULL,
              type = "success",
              timer = 3000,
              timerProgressBar = TRUE
            )
            shiny::updateActionButton(
              session = session,
              "confirm_compounds",
              label = "Saved",
              icon = shiny::icon("check")
            )
            shinyjs::disable("confirm_compounds")
            shinyjs::enable("edit_compounds")

            # Keep table editable
            output$compound_table <- rhandsontable::renderRHandsontable(
              prot_comp_handsontable(compound_table, disabled = TRUE)
            )

            # Inactivate table observer
            vars$compound_table_active <- FALSE

            # Show table message
            output$compound_table_info <- shiny::renderText("Table saved!")

            # Render sample table with new input
            if (!is.null(input$sample_table)) {
              output$sample_table <- rhandsontable::renderRHandsontable(
                sample_handsontable(
                  tab = rhandsontable::hot_to_r(input$sample_table),
                  proteins = vars$protein_table$Protein,
                  compounds = compound_table$Compound
                )
              )
            }

            # Jump to next tab module
            set_selected_tab("Samples", session)

            # Assign user input to reactive table variable
            vars$compound_table <- compound_table
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
        shinyjs::addClass(
          "sample_table_info",
          "table-info-red"
        )
        output$sample_table_info <- shiny::renderText({
          "Enter Proteins and Compounds first"
        })
      } else if (is.null(conversion_dirs$result())) {
        shinyjs::removeClass(
          "sample_table_info",
          "table-info-green"
        )
        shinyjs::addClass(
          "sample_table_info",
          "table-info-red"
        )
        output$sample_table_info <- shiny::renderText({
          "Upload result file"
        })
      } else {
        shinyjs::removeClass(
          "sample_table_info",
          "table-info-red"
        )
        shinyjs::removeClass(
          "sample_table_info",
          "table-info-green"
        )
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
      if (is.null(vars$compound_table)) {
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
      }
    })

    # Render protein table
    shiny::observe({
      if (is.null(vars$protein_table)) {
        empty_protein_tab <- empty_tab
        colnames(empty_protein_tab) <- c(
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

        output$protein_table <- rhandsontable::renderRHandsontable(
          prot_comp_handsontable(empty_protein_tab)
        )
      }
    })

    # Return currently selected tab
    list(
      selected_tab = shiny::reactive(input$tabs),
      set_selected_tab = set_selected_tab
    )
  })
}
