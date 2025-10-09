# app/view/conversion_card.R

box::use(
  shiny[moduleServer, NS],
)

box::use(
  app /
    logic /
    conversion_functions[
      sample_handsontable,
      prot_comp_handsontable,
      check_table,
      check_sample_table,
      slice_tab,
      slice_sample_tab,
      set_selected_tab,
      read_uploaded_file,
      process_uploaded_table,
    ],
  app /
    logic /
    conversion_constants[
      empty_tab,
      keybind_menu_ui,
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
          width = 2,
          shiny::div(
            class = "conversion-checkbox",
            shiny::checkboxInput(
              ns("proteins_header_checkbox"),
              "Has header",
              value = FALSE
            )
          )
        ),
        shiny::column(
          width = 3,
          shiny::textOutput(ns("protein_table_info")),
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
          rhandsontable::rHandsontableOutput(ns("protein_table"))
        )
      ),
      keybind_menu_ui
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
          width = 2,
          shiny::div(
            class = "conversion-checkbox",
            shiny::checkboxInput(
              ns("compounds_header_checkbox"),
              "Has header",
              value = FALSE
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
          rhandsontable::rHandsontableOutput(ns("compound_table"))
        )
      ),
      keybind_menu_ui
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
          width = 3,
          shiny::textOutput(ns("sample_table_info"))
        ),
        shiny::column(
          width = 2,
          shiny::div(
            class = "full-width-btn",
            shinyjs::disabled(
              shiny::actionButton(
                ns("confirm_samples"),
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
            shiny::actionButton(
              ns("edit_samples"),
              label = "",
              icon = shiny::icon("pen-to-square")
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          rhandsontable::rHandsontableOutput(ns("sample_table"))
        )
      ),
      keybind_menu_ui
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
      sample_tab = NULL,
      sample_table_active = FALSE,
      sample_table_status = FALSE,
      conversion_ready = FALSE
    )

    shiny::observeEvent(conversion_dirs$hits(), {
      # shiny::req(conversion_dirs())
      # # conversion_dirs()$run_conversion, {

      hits <- conversion_dirs$hits()

      bslib::nav_insert(
        "tabs",
        bslib::nav_panel(
          title = "Results",
          shiny::fluidRow(
            shiny::column(width = 1),
            shiny::column(
              width = 10,
              rhandsontable::rHandsontableOutput(
                ns("conversion_result_table")
              )
            )
          )
        )
      )

      set_selected_tab("Results", session)
    })

    shiny::observe({
      shiny::req(conversion_dirs$hits())

      message(TRUE)

      output$conversion_result_table <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(conversion_dirs$hits())
      })
    })

    # Observe protein file upload
    shiny::observeEvent(input$proteins_fileinput, {
      shiny::req(input$proteins_fileinput)

      table_upload <- read_uploaded_file(
        input$proteins_fileinput$datapath,
        tolower(tools::file_ext(input$proteins_fileinput$name)),
        input$proteins_header_checkbox
      )

      table_upload_processed <- process_uploaded_table(table_upload, "protein")

      if (!is.null(table_upload_processed)) {
        vars$protein_table <- table_upload_processed
        vars$protein_table_status <- TRUE

        output$protein_table <- rhandsontable::renderRHandsontable(
          prot_comp_handsontable(table_upload_processed, disabled = FALSE)
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

      table_upload <- read_uploaded_file(
        input$compounds_fileinput$datapath,
        tolower(tools::file_ext(input$compounds_fileinput$name)),
        input$compounds_header_checkbox
      )

      table_upload_processed <- process_uploaded_table(table_upload, "compound")

      if (!is.null(table_upload_processed)) {
        vars$compound_table <- table_upload_processed
        vars$compound_table_status <- TRUE

        output$compound_table <- rhandsontable::renderRHandsontable(
          prot_comp_handsontable(table_upload_processed, disabled = FALSE)
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

    # Observe table status for compound table
    shiny::observe({
      shiny::req(input$protein_table, vars$protein_table_active)

      # Show waiter
      waiter::waiter_show(
        id = ns("protein_table_info"),
        html = waiter::spin_throbber()
      )
      Sys.sleep(0.5)

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

      waiter::waiter_hide(id = ns("protein_table_info"))
    })

    # Observe table status for compound table
    shiny::observe({
      shiny::req(input$compound_table, vars$compound_table_active)

      # Show waiter
      waiter::waiter_show(
        id = ns("compound_table_info"),
        html = waiter::spin_throbber()
      )
      Sys.sleep(0.5)

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

      waiter::waiter_hide(id = ns("compound_table_info"))
    })

    # Observe table status for samples table
    shiny::observe({
      shiny::req(input$sample_table)

      # Show waiter
      waiter::waiter_show(
        id = ns("sample_table_info"),
        html = waiter::spin_throbber()
      )
      Sys.sleep(0.5)

      sample_table <- rhandsontable::hot_to_r(
        input$sample_table
      )

      # If table non-empty check for correctness
      if (is.null(sample_table) || nrow(sample_table) < 1) {
        # Set status variable to FALSE
        vars$sample_table_status <- FALSE

        # UI feedback
        shinyjs::removeClass(
          "sample_table_info",
          "table-info-green"
        )
        shinyjs::addClass(
          "sample_table_info",
          "table-info-red"
        )
        output$sample_table_info <- shiny::renderText(
          "Table could not be read"
        )
        shinyjs::disable("confirm_samples")
      } else {
        # Validate correct input
        sample_table_status <- check_sample_table(
          sample_table,
          vars$protein_table$Protein,
          vars$compound_table$Compound
        )

        if (isTRUE(sample_table_status)) {
          # Set status variable to TRUE
          vars$sample_table_status <- TRUE

          # UI feedback
          shinyjs::removeClass(
            "sample_table_info",
            "table-info-red"
          )
          shinyjs::addClass(
            "sample_table_info",
            "table-info-green"
          )
          output$sample_table_info <- shiny::renderText(
            "Table can be saved"
          )
          shinyjs::enable("confirm_samples")
        } else {
          # Set status variable to FALSE
          vars$sample_table_status <- FALSE

          # UI feedback
          shinyjs::removeClass(
            "sample_table_info",
            "table-info-green"
          )
          shinyjs::addClass(
            "sample_table_info",
            "table-info-red"
          )
          output$sample_table_info <- shiny::renderText(sample_table_status)
          shinyjs::disable("confirm_samples")
        }
      }

      waiter::waiter_hide(id = ns("sample_table_info"))
    })

    # Actions on edit button click
    shiny::observeEvent(
      input$edit_proteins | input$edit_compounds | input$edit_samples,
      {
        shiny::req(input$tabs)

        if (input$tabs == "Proteins") {
          # Make table observer active
          vars$protein_table_active <- TRUE

          # Enable file upload
          shinyjs::enable("proteins_fileinput")
          shinyjs::removeClass(
            selector = ".btn-file:has(#app-conversion_main-proteins_fileinput)",
            class = "custom-disable"
          )
          shinyjs::removeClass(
            selector = ".input-group:has(#app-conversion_main-proteins_fileinput) > .form-control",
            class = "custom-disable"
          )

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

          # Enable file upload
          shinyjs::enable("compounds_fileinput")
          shinyjs::removeClass(
            selector = ".btn-file:has(#app-conversion_main-compounds_fileinput)",
            class = "custom-disable"
          )
          shinyjs::removeClass(
            selector = ".input-group:has(#app-conversion_main-compounds_fileinput) > .form-control",
            class = "custom-disable"
          )

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
          # Make table observer active
          vars$compound_table_active <- TRUE

          # Enable file upload
          shinyjs::enable("result_input")
          shinyjs::removeClass(
            selector = ".btn-file:has(#app-conversion_main-result_input)",
            class = "custom-disable"
          )
          shinyjs::removeClass(
            selector = ".input-group:has(#app-conversion_main-result_input) > .form-control",
            class = "custom-disable"
          )

          # Mark tab as undone
          shinyjs::runjs(
            'document.querySelector(".nav-link[data-value=\'Samples\']").classList.remove("done");'
          )

          # Render editable table
          output$compound_table <- rhandsontable::renderRHandsontable(
            sample_handsontable(vars$sample_table, disabled = FALSE)
          )

          # Change buttons
          shiny::updateActionButton(
            session = session,
            "confirm_samples",
            label = "Save",
            icon = shiny::icon("bookmark")
          )
          shinyjs::enable("confirm_samples")
          shinyjs::disable("edit_samples")
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
            shinyjs::disable("proteins_fileinput")
            shinyjs::addClass(
              selector = ".btn-file:has(#app-conversion_main-proteins_fileinput)",
              class = "custom-disable"
            )
            shinyjs::addClass(
              selector = ".input-group:has(#app-conversion_main-proteins_fileinput) > .form-control",
              class = "custom-disable"
            )

            # Render table uneditable
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
                  tab = slice_sample_tab(rhandsontable::hot_to_r(
                    input$sample_table
                  )),
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
            shinyjs::disable("compounds_fileinput")
            shinyjs::addClass(
              selector = ".btn-file:has(#app-conversion_main-compounds_fileinput)",
              class = "custom-disable"
            )
            shinyjs::addClass(
              selector = ".input-group:has(#app-conversion_main-compounds_fileinput) > .form-control",
              class = "custom-disable"
            )

            # Render table uneditable
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
                  tab = slice_sample_tab(rhandsontable::hot_to_r(
                    input$sample_table
                  )),
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
        } else if (input$tabs == "Samples") {
          # If table can be saved perform actions
          if (vars$sample_table_status) {
            shiny::req(input$sample_table)

            # Mark UI as done
            shinyjs::runjs(
              'document.querySelector(".nav-link[data-value=\'Samples\']").classList.add("done");'
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
              "confirm_samples",
              label = "Saved",
              icon = shiny::icon("check")
            )
            shinyjs::disable("confirm_samples")
            shinyjs::enable("edit_samples")
            shinyjs::disable("result_input")
            shinyjs::addClass(
              selector = ".btn-file:has(#app-conversion_main-result_input)",
              class = "custom-disable"
            )
            shinyjs::addClass(
              selector = ".input-group:has(#app-conversion_main-result_input) > .form-control",
              class = "custom-disable"
            )

            # Retrieve sliced user input table
            sample_table <- slice_sample_tab(rhandsontable::hot_to_r(
              input$sample_table
            ))

            # Render table uneditable
            output$sample_table <- rhandsontable::renderRHandsontable(
              sample_handsontable(sample_table, disabled = TRUE)
            )

            # Inactivate table observer
            vars$sample_table_active <- FALSE

            # Show table message
            output$sample_table_info <- shiny::renderText("Table saved!")

            # Assign user input to reactive table variable
            vars$sample_table <- sample_table
          }
        }
      }
    )

    shiny::observe({
      if (
        isTRUE(vars$protein_table_status) &
          isTRUE(
            vars$compound_table_status
          ) &
          isTRUE(vars$sample_table_status)
      ) {
        protein_table <<- vars$protein_table
        compound_table <<- vars$compound_table
        sample_table <<- vars$sample_table
        result <<- vars$result

        vars$conversion_ready <- TRUE
      } else {
        vars$conversion_ready <- FALSE
      }
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
      } else if (is.null(input$result_input)) {
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
          "Fill table ..."
        })

        file_path <- file.path(input$result_input$datapath)
        vars$result <- readRDS(file_path)

        sample_tab <- data.frame(
          Sample = utils::head(names(vars$result), -2),
          Protein = ifelse(
            length(vars$protein_table$Protein) == 1,
            vars$protein_table$Protein,
            ""
          ),
          Compound = ifelse(
            length(vars$compound_table$Compound) == 1,
            vars$compound_table$Compound,
            ""
          ),
          cmp2 = NA,
          cmp3 = NA,
          cmp4 = NA,
          cmp5 = NA,
          cmp6 = NA,
          cmp7 = NA,
          cmp8 = NA,
          cmp9 = NA
        )

        colnames(sample_tab) <- c("Sample", "Protein", paste("Compound", 1:9))

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
      set_selected_tab = set_selected_tab,
      conversion_ready = shiny::reactive(vars$conversion_ready),
      input_list = shiny::reactive(list(
        Protein_Table = vars$protein_table,
        Compound_Table = vars$compound_table,
        Samples_Table = vars$sample_table,
        result = vars$result
      ))
    )
  })
}
