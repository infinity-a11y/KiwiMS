# app/view/conversion_card.R

box::use(
  shiny[moduleServer, NS],
  bslib[nav_insert],
)

box::use(
  app /
    logic /
    conversion_functions[
      add_kobs_binding_result,
      add_ki_kinact_result,
      sample_handsontable,
      prot_comp_handsontable,
      check_table,
      check_sample_table,
      slice_tab,
      slice_sample_tab,
      set_selected_tab,
      read_uploaded_file,
      process_uploaded_table,
      format_scientific,
      make_binding_plot,
      multiple_spectra,
      render_hits_table,
      checkboxColumn,
      js_code_gen,
      new_sample_table,
      confirm_ui_changes,
      edit_ui_changes,
      table_observe,
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

  # Tab UI for whole protein conversion module
  bslib::navset_card_tab(
    id = ns("tabs"),
    bslib::nav_panel(
      "Proteins",
      shinyjs::useShinyjs(),
      waiter::useWaiter(),
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
          shiny::textOutput(ns("proteins_table_info")),
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
          shiny::div(
            class = "handsontable-wrapper",
            rhandsontable::rHandsontableOutput(ns("proteins_table"))
          )
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
          shiny::textOutput(ns("compounds_table_info"))
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
          shiny::div(
            class = "handsontable-wrapper",
            rhandsontable::rHandsontableOutput(ns("compounds_table"))
          )
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
              ns("samples_fileinput"),
              "Select File",
              multiple = FALSE,
              accept = c(".rds")
            )
          )
        ),
        shiny::column(
          width = 2,
          shiny::textOutput(ns("sample_number_info"))
        ),
        shiny::column(
          width = 3,
          shiny::textOutput(ns("samples_table_info"))
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
            shinyjs::disabled(
              shiny::actionButton(
                ns("edit_samples"),
                label = "",
                icon = shiny::icon("pen-to-square")
              )
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::div(
            class = "sample-table",
            rhandsontable::rHandsontableOutput(ns("samples_table"))
          )
        )
      ),
      keybind_menu_ui
    )
  )
}

#' @export
server <- function(id, conversion_sidebar_vars, deconvolution_main_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Set file upload limit
    options(shiny.maxRequestSize = 1000 * 1024^2)

    ################ Conversion Declarations ------------------

    ## Reactive variables for conversion declarations ----
    declaration_vars <- shiny::reactiveValues(
      protein_table = NULL,
      protein_table_active = TRUE,
      protein_table_status = FALSE,
      compound_table = NULL,
      compound_table_active = TRUE,
      compound_table_status = FALSE,
      sample_tab = NULL,
      sample_table_active = TRUE,
      sample_table_status = FALSE,
      conversion_ready = FALSE,
      result = NULL
    )

    # Prepare waiter spinner object
    w <- waiter::Waiter$new(
      id = ns("samples_table"),
      html = waiter::spin_wave()
    )

    output$sample_number_info <- shiny::renderText({
      if (is.null(declaration_vars$result)) {
        "Add samples ..."
      } else {
        paste(
          length(declaration_vars$result$deconvolution),
          "deconvoluted samples"
        )
      }
    })

    # On result file input execute immediately
    shinyjs::onevent(
      "change",
      "samples_fileinput",
      {
        # Block UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "block";'
        ))

        # Disable result file input
        shinyjs::disable("samples_fileinput")
        shinyjs::addClass(
          selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
          class = "custom-disable"
        )
        shinyjs::addClass(
          selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
          class = "custom-disable"
        )

        # Update info text
        shinyjs::removeClass(
          "samples_table_info",
          "table-info-green"
        )
        shinyjs::addClass(
          "samples_table_info",
          "table-info-red"
        )
        output$samples_table_info <- shiny::renderText(
          "Loading ..."
        )
      }
    )

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
        declaration_vars$protein_table_status <- TRUE

        output$proteins_table <- rhandsontable::renderRHandsontable(
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
        declaration_vars$compound_table_status <- TRUE

        output$compounds_table <- rhandsontable::renderRHandsontable(
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

    # Observe table status for protein table
    shiny::observe({
      shiny::req(
        input$proteins_table,
        shiny::isolate(declaration_vars$protein_table_active)
      )

      # Retrieve sliced user input table
      protein_table <- slice_tab(rhandsontable::hot_to_r(
        input$proteins_table
      ))

      # Conditional observe actions
      declaration_vars$protein_table_status <- table_observe(
        tab = "proteins",
        table = protein_table,
        output = output,
        ns = ns
      )
    })

    # Observe table status for compound table
    shiny::observe({
      shiny::req(
        input$compounds_table,
        shiny::isolate(declaration_vars$compound_table_active)
      )

      # Retrieve sliced user input table
      compound_table <- slice_tab(rhandsontable::hot_to_r(
        input$compounds_table
      ))

      # Conditional observe actions
      declaration_vars$compound_table_status <- table_observe(
        tab = "compounds",
        table = compound_table,
        output = output,
        ns = ns
      )
    })

    # Observe table status for samples table
    shiny::observe({
      shiny::req(
        input$samples_table,
        declaration_vars$protein_table_sliced,
        shiny::isolate(declaration_vars$sample_table_active)
      )

      # Table info UI changes
      sample_table <- rhandsontable::hot_to_r(
        input$samples_table
      )

      # Conditional observe actions
      declaration_vars$sample_table_status <- table_observe(
        tab = "samples",
        table = sample_table,
        output = output,
        ns = ns,
        proteins = declaration_vars$protein_table_sliced$Protein,
        compounds = declaration_vars$compound_table_sliced$Compound
      )

      # Unblock UI
      shinyjs::runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "none";'
      ))
    })

    # Actions on edit button click
    shiny::observeEvent(
      input$edit_proteins | input$edit_compounds | input$edit_samples,
      {
        shiny::req(input$tabs)

        # If edit applied always activate edit mode for sample table if present
        if (!is.null(declaration_vars$sample_table_sliced)) {
          # Make UI changes
          edit_ui_changes(
            tab = "Samples",
            table = declaration_vars$sample_table,
            session = session,
            output = output
          )

          # Activate table observer
          declaration_vars$sample_table_active <- TRUE
        }

        # Edit Protein/Compound
        if (input$tabs == "Proteins") {
          # Make UI changes
          edit_ui_changes(
            tab = input$tabs,
            table = declaration_vars$protein_table,
            session = session,
            output = output
          )

          # Make table observer active
          declaration_vars$protein_table_active <- TRUE
        } else if (input$tabs == "Compounds") {
          # Make UI changes
          edit_ui_changes(
            tab = input$tabs,
            table = declaration_vars$compound_table,
            session = session,
            output = output
          )

          # Make table observer active
          declaration_vars$compound_table_active <- TRUE
        }
      }
    )

    # Actions on confirming input table
    shiny::observeEvent(
      input$confirm_proteins |
        input$confirm_compounds |
        input$confirm_samples,
      {
        # Block UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "block";'
        ))

        if (input$tabs == "Proteins" && declaration_vars$protein_table_status) {
          # Retrieve client side table input
          protein_table <- rhandsontable::hot_to_r(
            input$proteins_table
          )
          declaration_vars$protein_table <- protein_table

          # Slice table input
          protein_table_sliced <- slice_tab(protein_table)
          declaration_vars$protein_table_sliced <- protein_table_sliced

          # Mark UI as done
          confirm_ui_changes(
            tab = input$tabs,
            table = protein_table_sliced,
            session = session,
            output = output
          )

          # Render sample table with new input
          if (!is.null(input$samples_table)) {
            output$samples_table <- rhandsontable::renderRHandsontable({
              sample_handsontable(
                # slice_sample_tab(
                tab = rhandsontable::hot_to_r(input$samples_table),
                # )
                proteins = protein_table_sliced$Protein,
                compounds = declaration_vars$compound_table_sliced$Compound
              )
            })
          }

          # Jump to next tab depending on compound table status
          if (isTRUE(declaration_vars$compound_table_active)) {
            set_selected_tab("Compounds", session)
          } else {
            set_selected_tab("Samples", session)
          }

          # Inactivate table observer
          declaration_vars$protein_table_active <- FALSE
        } else if (
          input$tabs == "Compounds" && declaration_vars$compound_table_status
        ) {
          # Retrieve client side table input
          compound_table <- rhandsontable::hot_to_r(
            input$compounds_table
          )
          declaration_vars$compound_table <- compound_table

          # Slice table input
          compound_table_sliced <- slice_tab(compound_table)
          declaration_vars$compound_table_sliced <- compound_table_sliced

          # Mark UI as done
          confirm_ui_changes(
            tab = input$tabs,
            table = compound_table_sliced,
            session = session,
            output = output
          )

          # Render sample table with new input
          if (!is.null(input$samples_table)) {
            output$samples_table <- rhandsontable::renderRHandsontable({
              sample_handsontable(
                # slice_sample_tab(
                tab = rhandsontable::hot_to_r(input$samples_table),
                # )
                proteins = declaration_vars$protein_table_sliced$Protein,
                compounds = compound_table$Compound
              )
            })
          }

          # Jump to next tab depending on compound table status
          if (isTRUE(declaration_vars$protein_table_active)) {
            set_selected_tab("Proteins", session)
          } else {
            set_selected_tab("Samples", session)
          }

          # Inactivate table observer
          declaration_vars$compound_table_active <- FALSE
        } else if (
          input$tabs == "Samples" && declaration_vars$sample_table_status
        ) {
          # Retrieve client side table input
          sample_table <- rhandsontable::hot_to_r(
            input$samples_table
          )
          declaration_vars$sample_table <- sample_table

          # Slice table input
          sample_table_sliced <- slice_tab(sample_table)
          declaration_vars$sample_table_sliced <- sample_table_sliced

          # Mark UI as done
          confirm_ui_changes(
            tab = input$tabs,
            table = sample_table_sliced,
            session = session,
            output = output
          )

          # Inactivate table observer
          declaration_vars$sample_table_active <- FALSE
        }

        # Unblock UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "none";'
        ))
      }
    )

    # Observe conversion declaration readiness
    shiny::observe({
      if (
        isTRUE(declaration_vars$protein_table_status) &
          isTRUE(
            declaration_vars$compound_table_status
          ) &
          isTRUE(declaration_vars$sample_table_status) &
          isFALSE(declaration_vars$sample_table_active)
      ) {
        declaration_vars$conversion_ready <- TRUE
      } else {
        declaration_vars$conversion_ready <- FALSE
      }
    })

    # Observe sample input
    shiny::observeEvent(
      input$samples_fileinput,
      {
        shinyjs::removeClass(
          "samples_table_info",
          "table-info-green"
        )
        shinyjs::addClass(
          "samples_table_info",
          "table-info-red"
        )

        shinyjs::removeClass(
          selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
          class = "custom-disable"
        )
        shinyjs::removeClass(
          selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
          class = "custom-disable"
        )

        output$samples_table_info <- shiny::renderText({
          "Fill table ..."
        })

        # Read results .rds file from selected path
        file_path <- file.path(input$samples_fileinput$datapath)
        declaration_vars$result <- readRDS(file_path)

        # if (!isTRUE(declaration_vars$sample_tab_initial)) {
        output$samples_table <- rhandsontable::renderRHandsontable({
          sample_handsontable(
            tab = new_sample_table(
              result = declaration_vars$result,
              protein_table = declaration_vars$protein_table_sliced,
              compound_table = declaration_vars$compound_table_sliced
            ),
            proteins = declaration_vars$protein_table_sliced$Protein,
            compounds = declaration_vars$compound_table_sliced$Compound
          )
        })
      }

      # declaration_vars$sample_tab_initial <- TRUE
    )

    # Event user cancels samples table overwrite with new deconvolution results
    shiny::observeEvent(input$conversion_cont_cancel, {
      # Remove dialogue window
      shiny::removeModal()
    })

    # Event user confirms samples table overwrite with new deconvolution results
    shiny::observeEvent(input$conversion_cont_conf, {
      # Block UI
      shinyjs::runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "block";'
      ))

      # Remove dialogue window
      shiny::removeModal()

      # Change buttons
      shiny::updateActionButton(
        session = session,
        "confirm_samples",
        label = "Save",
        icon = shiny::icon("bookmark")
      )
      shinyjs::enable("confirm_samples")
      shinyjs::disable("edit_samples")

      # Enable file upload
      shinyjs::enable("samples_fileinput")
      shinyjs::removeClass(
        selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
        class = "custom-disable"
      )
      shinyjs::removeClass(
        selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
        class = "custom-disable"
      )

      # Mark tab as undone
      shinyjs::runjs(
        'document.querySelector(".nav-link[data-value=\'Samples\']").classList.remove("done");'
      )

      # Activate table observer
      declaration_vars$sample_table_active <- TRUE

      shinyjs::removeClass(
        "samples_table_info",
        "table-info-green"
      )
      shinyjs::addClass(
        "samples_table_info",
        "table-info-red"
      )
      shinyjs::removeClass(
        selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
        class = "custom-disable"
      )
      shinyjs::removeClass(
        selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
        class = "custom-disable"
      )
      output$samples_table_info <- shiny::renderText({
        "Fill table ..."
      })

      # Read results .rds file from previous deconvolution
      declaration_vars$result <- readRDS(
        deconvolution_main_vars$continue_conversion()
      )

      # if (!isTRUE(shiny::isolate(declaration_vars$sample_tab_initial))) {
      output$samples_table <- rhandsontable::renderRHandsontable({
        sample_handsontable(
          tab = new_sample_table(
            result = declaration_vars$result,
            protein_table = declaration_vars$protein_table_sliced,
            compound_table = declaration_vars$compound_table_sliced
          ),
          proteins = declaration_vars$protein_table_sliced$Protein,
          compounds = declaration_vars$compound_table_sliced$Compound,
          disabled = ifelse(
            is.null(declaration_vars$protein_table_sliced) ||
              is.null(declaration_vars$compound_table_sliced) ||
              isTRUE(declaration_vars$protein_table_active) ||
              isTRUE(declaration_vars$compound_table_active),
            TRUE,
            FALSE
          )
        )
      })
      # }
      # shiny::isolate(declaration_vars$sample_tab_initial <- TRUE)

      # Unblock UI
      shinyjs::runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "none";'
      ))
    })

    # Transfer results from deconvolution to sample table
    shiny::observeEvent(
      deconvolution_main_vars$continue_conversion(),
      {
        shiny::req(deconvolution_main_vars$continue_conversion())

        # If present sample table ask confirmation
        if (isFALSE(declaration_vars$sample_table_active)) {
          # Switch to samples table tab
          set_selected_tab("Samples", session)

          # Show confirmation dialogue
          shiny::showModal(
            shiny::div(
              class = "conversion-modal",
              shiny::modalDialog(
                title = htmltools::tags$span("Upload new result file?"),
                easyClose = FALSE,
                footer = shiny::tagList(
                  shiny::actionButton(
                    ns("conversion_cont_cancel"),
                    "Cancel",
                    width = "auto"
                  ),
                  shiny::actionButton(
                    ns("conversion_cont_conf"),
                    "Continue",
                    class = "load-db",
                    width = "auto"
                  )
                ),
                shiny::fluidRow(
                  shiny::br(),
                  shiny::column(
                    width = 12,
                    shiny::div(
                      shiny::p(
                        shiny::HTML(
                          '<b>Sample table is already present.</b><br><br>'
                        )
                      ),
                      shiny::div(
                        class = "info-symbol-container",
                        shiny::HTML(paste0(
                          '<i class="fa-solid fa-circle-exclamation" style="font-size:',
                          '1em; color:black; margin-right: 10px;"></i>'
                        )),
                        shiny::HTML(
                          'Continuing will delete all entries and reevaluate the table with the new results.'
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        } else {
          # Block UI
          shinyjs::runjs(paste0(
            'document.getElementById("blocking-overlay").style.display ',
            '= "block";'
          ))

          # Read results .rds file from previous deconvolution
          declaration_vars$result <- readRDS(
            deconvolution_main_vars$continue_conversion()
          )

          # Render rhandsontable
          # if (!isTRUE(shiny::isolate(declaration_vars$sample_tab_initial))) {
          output$samples_table <- rhandsontable::renderRHandsontable({
            sample_handsontable(
              tab = new_sample_table(
                result = declaration_vars$result,
                protein_table = declaration_vars$protein_table_sliced,
                compound_table = declaration_vars$compound_table_sliced
              ),
              proteins = declaration_vars$protein_table_sliced$Protein,
              compounds = declaration_vars$compound_table_sliced$Compound,
              disabled = ifelse(
                is.null(declaration_vars$protein_table_sliced) ||
                  is.null(declaration_vars$compound_table_sliced) ||
                  isTRUE(declaration_vars$protein_table_active) ||
                  isTRUE(declaration_vars$compound_table_active),
                TRUE,
                FALSE
              )
            )
          })
          # }
          # shiny::isolate(declaration_vars$sample_tab_initial <- TRUE)
        }

        # If user already declared proteins/compounds switch to Samples tab
        if (
          isFALSE(declaration_vars$protein_table_active) ||
            isFALSE(declaration_vars$compound_table_active)
        ) {
          set_selected_tab("Samples", session)
        }

        # Unblock UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "none";'
        ))
      }
    )

    # Observe sample tab activation status
    shiny::observe({
      if (
        # if protein/compound declaration unconfirmed
        is.null(declaration_vars$protein_table_sliced) ||
          is.null(declaration_vars$compound_table_sliced) ||
          isTRUE(declaration_vars$protein_table_active) ||
          isTRUE(declaration_vars$compound_table_active)
      ) {
        # Disable file upload
        shinyjs::disable("samples_fileinput")
        shinyjs::addClass(
          selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
          class = "custom-disable"
        )
        shinyjs::addClass(
          selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
          class = "custom-disable"
        )

        # Disable confirm button
        shinyjs::disable("confirm_samples")

        # Update table info
        output$samples_table_info <- shiny::renderText({
          "Enter Proteins and Compounds first"
        })
        shinyjs::removeClass(
          "samples_table_info",
          "table-info-green"
        )
        shinyjs::addClass(
          "samples_table_info",
          "table-info-red"
        )
      } else {
        # if protein/compound declaration confirmed

        # Enable file upload
        shinyjs::enable("samples_fileinput")
        shinyjs::removeClass(
          selector = ".btn-file:has(#app-conversion_main-samples_fileinput)",
          class = "custom-disable"
        )
        shinyjs::removeClass(
          selector = ".input-group:has(#app-conversion_main-samples_fileinput) > .form-control",
          class = "custom-disable"
        )

        # Enable confirm button
        shinyjs::enable("confirm_samples")

        # Update table info
        output$samples_table_info <- shiny::renderText({
          "Upload result file"
        })
        shinyjs::removeClass(
          "samples_table_info",
          "table-info-green"
        )
        shinyjs::addClass(
          "samples_table_info",
          "table-info-red"
        )
      }
    })

    # Render compound table
    shiny::observe({
      if (is.null(declaration_vars$compound_table_sliced)) {
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

        output$compounds_table <- rhandsontable::renderRHandsontable({
          prot_comp_handsontable(tab)
        })
      }
    })

    # Render protein table
    shiny::observe({
      if (is.null(declaration_vars$protein_table_sliced)) {
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

        output$proteins_table <- rhandsontable::renderRHandsontable(
          prot_comp_handsontable(empty_protein_tab)
        )
      }
    })

    ################ Conversion Results ------------------

    ## Reactive variables for conversion results ----
    conversion_vars <- shiny::reactiveValues(
      modified_results = NULL,
      select_concentration = NULL,
      formatted_hits = NULL,
      conc_colors = NULL
    )

    # Trigger loading of binding analysis results interface
    shiny::observeEvent(conversion_sidebar_vars$run_analysis(), {
      if (!is.null(conversion_sidebar_vars$result_list())) {
        # Summarize hits to table
        hits_summary <- conversion_sidebar_vars$result_list()$"hits_summary" |>
          dplyr::mutate(
            Intensity = scales::percent(
              Intensity / 100,
              accuracy = 0.1
            ),
            `% Binding` = scales::percent(
              `% Binding`,
              accuracy = 0.1
            ),
            `Total % Binding` = scales::percent(
              `Total % Binding`,
              accuracy = 0.1
            ),
            `Protein Intensity` = scales::percent(
              `Protein Intensity` / 100,
              accuracy = 0.1
            ),
            time = paste(as.character(time), "min"),
            concentration = paste(concentration, "µM"),
            `Mw Protein [Da]` = dplyr::if_else(
              is.na(`Mw Protein [Da]`),
              "N/A",
              paste(
                format(`Mw Protein [Da]`, nsmall = 1, trim = TRUE),
                "Da"
              )
            ),
            `Measured Mw Protein [Da]` = dplyr::if_else(
              is.na(`Measured Mw Protein [Da]`),
              "N/A",
              paste(
                format(`Measured Mw Protein [Da]`, nsmall = 1, trim = TRUE),
                "Da"
              )
            ),
            `Delta Mw Protein [Da]` = dplyr::if_else(
              is.na(`Delta Mw Protein [Da]`),
              "N/A",
              paste(
                format(`Delta Mw Protein [Da]`, nsmall = 1, trim = TRUE),
                "Da"
              )
            ),
            `Compound Mw [Da]` = dplyr::if_else(
              is.na(`Compound Mw [Da]`),
              "N/A",
              paste(
                format(`Compound Mw [Da]`, nsmall = 1, trim = TRUE),
                "Da"
              )
            ),
            `Delta Mw Compound [Da]` = dplyr::if_else(
              is.na(`Delta Mw Compound [Da]`),
              "N/A",
              paste(
                format(`Delta Mw Compound [Da]`, nsmall = 1, trim = TRUE),
                "Da"
              )
            ),
            `Peak [Da]` = dplyr::if_else(
              is.na(`Peak [Da]`),
              "N/A",
              paste(
                format(`Peak [Da]`, nsmall = 1, trim = TRUE),
                "Da"
              )
            )
          ) |>
          dplyr::select(-c(3, 17)) |>
          dplyr::relocate(
            c(concentration, time),
            .before = `Mw Protein [Da]`
          ) |>
          dplyr::relocate(`Total % Binding`, .after = "% Binding")

        # Change column names
        colnames(hits_summary) <- c(
          "Well",
          "Sample ID",
          "[Cmp]",
          "Time",
          "Theor. Prot.",
          "Meas. Prot.",
          "Δ Prot.",
          "Ⅰ Prot.",
          "Peak Signal",
          "Ⅰ Cmp",
          "Cmp Name",
          "Theor. Cmp",
          "Δ Cmp",
          "Bind. Stoich.",
          "%-Binding",
          "Total %-Binding"
        )
        # Assign formatted hits to reactive variable
        conversion_vars$formatted_hits <- hits_summary

        # Get concentrations
        binding_kobs_result_names <- names(
          conversion_sidebar_vars$result_list()$binding_kobs_result
        )
        concentrations <- binding_kobs_result_names[
          !binding_kobs_result_names %in%
            c("binding_table", "binding_plot", "kobs_result_table")
        ]
        conc_selected <- rep(TRUE, length(concentrations))
        names(conc_selected) <- concentrations
        conversion_vars$select_concentration <- conc_selected

        # Assign colors to present concentrations
        concentration_colors <- rev(RColorBrewer::brewer.pal(
          n = length(concentrations),
          name = "Set1"
        ))
        names(concentration_colors) <- concentrations

        # Assign colors to reactive variable
        conversion_vars$conc_colors <- concentration_colors

        # Hide declaration tabs
        bslib::nav_hide(
          "tabs",
          "Proteins"
        )
        bslib::nav_hide(
          "tabs",
          "Compounds"
        )
        bslib::nav_hide(
          "tabs",
          "Samples"
        )

        # Add "Binding" tab
        bslib::nav_insert(
          "tabs",
          bslib::nav_panel(
            title = "Binding",
            shiny::div(
              class = "conversion-result-wrapper",
              shiny::uiOutput(ns("binding_tab"))
            )
          )
        )

        # Add "Hits" tab
        bslib::nav_insert(
          "tabs",
          bslib::nav_panel(
            title = "Hits",
            shiny::div(
              class = "conversion-result-wrapper",
              # shiny::div(
              #   class = "tooltip-bttn hits-tab-tooltip",
              #   shiny::actionButton(
              #     ns("hits_table_tooltip_bttn"),
              #     label = "",
              #     icon = shiny::icon("circle-question")
              #   )
              # ),
              shinycssloaders::withSpinner(
                DT::DTOutput(ns("hits_tab")),
                type = 1,
                color = "#7777f9"
              )
            )
          )
        )

        # Define a set of IDs for the dynamic concentration tabs
        dynamic_ui_ids <- paste0("concentration_tab_", concentrations)

        # Add tabs for each present concentration
        for (i in seq_along(concentrations)) {
          concentration <- concentrations[[i]]
          ui_id <- dynamic_ui_ids[[i]]

          bslib::nav_insert(
            "tabs",
            bslib::nav_panel(
              title = paste0("[", concentration, "]"),
              shiny::div(
                class = "conversion-result-wrapper",
                shiny::uiOutput(ns(ui_id))
              )
            )
          )
        }

        # Assign output names according to present concentrations
        lapply(names(output), function(name) {
          if (grepl("^concentration_tab_", name)) {
            output[[name]] <- NULL
          }
        })

        # Loop through the list of concentrations to render UI
        for (i in seq_along(concentrations)) {
          concentration <- concentrations[[i]]
          ui_id <- dynamic_ui_ids[[i]]

          local({
            local_concentration <- concentration
            local_ui_id <- ui_id

            conc_result <- conversion_sidebar_vars$result_list()$binding_kobs_result[[
              local_concentration
            ]]

            # Render hits table for concentration tab
            output[[paste0(local_ui_id, "_hits")]] <- DT::renderDT({
              render_hits_table(
                hits_table = hits_summary |>
                  dplyr::filter(
                    `[Cmp]` == paste(local_concentration, "µM")
                  ),
                concentration_colors = concentration_colors,
                single_conc = local_concentration
              )
            })

            # Render binding plot for concentration tab
            output[[paste0(
              local_ui_id,
              "_binding_plot"
            )]] <- plotly::renderPlotly({
              make_binding_plot(
                kobs_result = conversion_sidebar_vars$result_list()$binding_kobs_result,
                filter_conc = local_concentration
              )
            })

            # Render multiple spectra plot
            output[[paste0(
              local_ui_id,
              "_spectra"
            )]] <- plotly::renderPlotly({
              decon_samples <- gsub(
                "o",
                ".",
                sapply(
                  strsplit(
                    names(conversion_sidebar_vars$result_list()$deconvolution),
                    "_"
                  ),
                  `[`,
                  3
                )
              )

              multiple_spectra(
                results_list = conversion_sidebar_vars$result_list(),
                samples = names(
                  conversion_sidebar_vars$result_list()$deconvolution
                )[which(
                  decon_samples == local_concentration
                )],
                cubic = ifelse(
                  input[[paste0(local_ui_id, "_kind")]] == "3D",
                  TRUE,
                  FALSE
                )
              )
            })

            # Assign the renderUI to the dynamically created output slot
            output[[local_ui_id]] <- shiny::renderUI({
              shiny::div(
                class = "result-conc-tab",
                shiny::div(
                  class = "card-custom spectrum",
                  bslib::card(
                    bslib::card_header(
                      class = "bg-dark help-header",
                      "Mass Spectra",
                      shiny::div(
                        class = "spectrum-radio-button",
                        shinyWidgets::radioGroupButtons(
                          ns(paste0(
                            local_ui_id,
                            "_kind"
                          )),
                          choices = c("3D", "Planar")
                        )
                      ),
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::actionButton(
                          ns("mass_spectra_tooltip_bttn"),
                          label = "",
                          icon = shiny::icon("circle-question")
                        )
                      )
                    ),
                    full_screen = TRUE,
                    shinycssloaders::withSpinner(
                      plotly::plotlyOutput(
                        ns(paste0(local_ui_id, "_spectra")),
                        height = "100%"
                      ),
                      type = 1,
                      color = "#7777f9"
                    )
                  )
                ),
                shiny::div(
                  class = "card-custom binding",
                  bslib::card(
                    bslib::card_header(
                      class = "bg-dark help-header",
                      "Binding Curve",
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::actionButton(
                          ns("binding_curve_single_tooltip_bttn"),
                          label = "",
                          icon = shiny::icon("circle-question")
                        )
                      )
                    ),
                    full_screen = TRUE,
                    shinycssloaders::withSpinner(
                      plotly::plotlyOutput(
                        ns(paste0(
                          local_ui_id,
                          "_binding_plot"
                        )),
                        height = "100%"
                      ),
                      type = 1,
                      color = "#7777f9"
                    )
                  )
                ),
                shiny::div(
                  class = "result-cards",
                  shiny::div(
                    class = "card-custom",
                    bslib::card(
                      bslib::card_header(
                        class = "bg-dark help-header",
                        htmltools::tagList(
                          shiny::div(
                            "k",
                            htmltools::tags$sub("obs")
                          )
                        ),
                        shiny::div(
                          class = "tooltip-bttn",
                          shiny::actionButton(
                            ns("kobs_value_tooltip_bttn"),
                            label = "",
                            icon = shiny::icon("circle-question")
                          )
                        )
                      ),
                      shiny::div(
                        class = "kobs-val",
                        format_scientific(conc_result$kobs)
                      )
                    )
                  ),
                  shiny::div(
                    class = "card-custom",
                    bslib::card(
                      bslib::card_header(
                        class = "bg-dark help-header",
                        "Binding Plateau",
                        shiny::div(
                          class = "tooltip-bttn",
                          shiny::actionButton(
                            ns("binding_plateau_tooltip_bttn"),
                            label = "",
                            icon = shiny::icon("circle-question")
                          )
                        )
                      ),
                      shiny::div(
                        class = "kobs-val",
                        paste0(format_scientific(conc_result$plateau), "%")
                      )
                    )
                  ),
                  shiny::div(
                    class = "card-custom",
                    bslib::card(
                      bslib::card_header(
                        class = "bg-dark help-header",
                        "Velocity v",
                        shiny::div(
                          class = "tooltip-bttn",
                          shiny::actionButton(
                            ns("v_value_tooltip_bttn"),
                            label = "",
                            icon = shiny::icon("circle-question")
                          )
                        )
                      ),
                      shiny::div(
                        class = "kobs-val",
                        format_scientific(conc_result$v)
                      )
                    )
                  )
                ),
                shiny::div(
                  class = "card-custom hits",
                  bslib::card(
                    bslib::card_header(
                      class = "bg-dark help-header",
                      "Hits",
                      shiny::div(
                        class = "tooltip-bttn",
                        shiny::actionButton(
                          ns("hits_table_tooltip_bttn"),
                          label = "",
                          icon = shiny::icon("circle-question")
                        )
                      )
                    ),
                    full_screen = TRUE,
                    shiny::div(
                      class = "conc-hits-table",
                      shinycssloaders::withSpinner(
                        DT::DTOutput(ns(paste0(local_ui_id, "_hits"))),
                        type = 1,
                        color = "#7777f9"
                      )
                    )
                  )
                )
              )
            })
          })
        }

        # Select binding results tab
        set_selected_tab("Binding", session)
      } else {
        # If results were reset load conversion declaration interface
        # Show declaration tabs
        bslib::nav_show(
          "tabs",
          "Proteins"
        )
        bslib::nav_show(
          "tabs",
          "Compounds"
        )
        bslib::nav_show(
          "tabs",
          "Samples"
        )

        # Remove results tabs
        bslib::nav_remove("tabs", "Binding")
        bslib::nav_remove("tabs", "Hits")
        for (i in names(conversion_vars$select_concentration)) {
          bslib::nav_remove(
            "tabs",
            paste0("[", i, "]")
          )
        }

        # Reset reactive variables
        conversion_vars <- shiny::reactiveValues(
          modified_results = NULL,
          select_concentration = NULL,
          formatted_hits = NULL,
          conc_colors = NULL
        )

        # Select samples tab
        set_selected_tab("Samples", session)
      }
    })

    output$binding_tab <- shiny::renderUI(
      shiny::div(
        class = "binding-analysis-tab",
        shiny::div(
          class = "card-custom",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              class = "bg-dark help-header",
              "Binding Curve",
              shiny::div(
                class = "tooltip-bttn",
                shiny::actionButton(
                  ns("binding_curve_tooltip_bttn"),
                  label = "",
                  icon = shiny::icon("circle-question")
                )
              )
            ),
            bslib::card_body(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("binding_plot"),
                  height = "100%"
                ),
                type = 1,
                color = "#7777f9"
              )
            )
          )
        ),
        shiny::div(
          class = "card-custom",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              class = "bg-dark help-header",
              htmltools::tagList(
                shiny::div(
                  "k",
                  htmltools::tags$sub("obs"),
                  " Curve"
                )
              ),
              shiny::div(
                class = "tooltip-bttn",
                shiny::actionButton(
                  ns("kobs_curve_tooltip_bttn"),
                  label = "",
                  icon = shiny::icon("circle-question")
                )
              )
            ),
            bslib::card_body(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(
                  ns("kobs_plot"),
                  height = "100%"
                ),
                type = 1,
                color = "#7777f9"
              )
            )
          )
        ),
        shiny::div(
          class = "card-custom",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              class = "bg-dark help-header",
              "Binding Analysis",
              shiny::div(
                class = "tooltip-bttn",
                shiny::actionButton(
                  ns("binding_analysis_tooltip_bttn"),
                  label = "",
                  icon = shiny::icon("circle-question")
                )
              )
            ),
            bslib::card_body(
              shinycssloaders::withSpinner(
                DT::DTOutput(ns("kobs_result")),
                type = 1,
                color = "#7777f9"
              )
            )
          )
        ),
        shiny::div(
          class = "result-cards",
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header",
                htmltools::tagList(
                  shiny::div(
                    "k",
                    htmltools::tags$sub("inact")
                  )
                ),
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("kinact_tooltip_bttn"),
                    label = "",
                    icon = shiny::icon("circle-question")
                  )
                )
              ),
              shiny::div(
                class = "kobs-val",
                shinycssloaders::withSpinner(
                  shiny::uiOutput(ns("kinact")),
                  type = 1,
                  color = "#7777f9"
                )
              )
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header",
                htmltools::tagList(
                  shiny::div(
                    "K",
                    htmltools::tags$sub("i")
                  )
                ),
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("Ki_tooltip_bttn"),
                    label = "",
                    icon = shiny::icon("circle-question")
                  )
                )
              ),
              shiny::div(
                class = "kobs-val",
                shinycssloaders::withSpinner(
                  shiny::uiOutput(ns("Ki")),
                  type = 1,
                  color = "#7777f9"
                )
              )
            )
          ),
          shiny::div(
            class = "card-custom",
            bslib::card(
              bslib::card_header(
                class = "bg-dark help-header",
                htmltools::tagList(
                  shiny::div(
                    "K",
                    htmltools::tags$sub("i"),
                    "/ k",
                    htmltools::tags$sub("inact"),
                  )
                ),
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("Ki_kinact_tooltip_bttn"),
                    label = "",
                    icon = shiny::icon("circle-question")
                  )
                )
              ),
              shiny::div(
                class = "kobs-val",
                shinycssloaders::withSpinner(
                  shiny::uiOutput(ns("Ki_kinact")),
                  type = 1,
                  color = "#7777f9"
                )
              )
            )
          )
        )
      )
    )

    # Show calculated Ki/kinact value
    output$Ki_kinact <- shiny::renderUI({
      shiny::div(
        class = "result-card-content",
        shiny::div(
          class = "main-result",
          paste(
            format_scientific(ki_kinact_result()[1, 1]),
            "M⁻¹ s⁻¹"
          )
        )
      )
    })

    # Show calculated kinact value
    output$kinact <- shiny::renderUI({
      shiny::div(
        class = "result-card-content",
        shiny::div(
          class = "main-result",
          paste(
            format_scientific(ki_kinact_result()[1, 1]),
            "s⁻¹"
          )
        ),
        shiny::div(
          class = "error-result",
          paste(
            "±",
            format_scientific(ki_kinact_result()[1, 2])
          )
        ),
        shiny::div(
          class = "param-result",
          shiny::HTML(
            paste(
              "<b>t value</b>&nbsp;",
              format_scientific(ki_kinact_result()[1, 3])
            )
          )
        ),
        shiny::div(
          class = "param-result",
          shiny::HTML(
            paste(
              "<b>Pr(>|t|)</b>&nbsp;",
              format_scientific(ki_kinact_result()[1, 4])
            )
          )
        )
      )
    })

    # Show calculated Ki value
    output$Ki <- shiny::renderUI({
      shiny::div(
        class = "result-card-content",
        shiny::div(
          class = "main-result",
          paste(
            format_scientific(ki_kinact_result()[2, 1]),
            "M⁻¹"
          )
        ),
        shiny::div(
          class = "error-result",
          paste(
            "±",
            format_scientific(ki_kinact_result()[2, 2])
          )
        ),
        shiny::div(
          class = "param-result",
          shiny::HTML(
            paste(
              "<b>t value</b>&nbsp;",
              format_scientific(ki_kinact_result()[2, 3])
            )
          )
        ),
        shiny::div(
          class = "param-result",
          shiny::HTML(
            paste(
              "<b>Pr(>|t|)</b>&nbsp;",
              format_scientific(ki_kinact_result()[2, 4])
            )
          )
        )
      )
    })

    ki_kinact_result <- shiny::reactive({
      shiny::req(conversion_sidebar_vars$result_list())

      if (is.null(conversion_vars$modified_results)) {
        result_list <- conversion_sidebar_vars$result_list()
      } else {
        # Block UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "block";'
        ))

        Sys.sleep(1)

        result_list <- conversion_vars$modified_results

        # Unblock UI
        shinyjs::runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "none";'
        ))
      }

      result_list$ki_kinact_result$Params
    })

    # UI output for hits tab
    output$hits_tab <- DT::renderDT({
      shiny::req(
        conversion_vars$formatted_hits,
        conversion_vars$conc_colors
      )

      render_hits_table(
        hits_table = conversion_vars$formatted_hits,
        concentration_colors = conversion_vars$conc_colors,
        withzero = any(conversion_vars$formatted_hits[["[Cmp]"]] == "0 µM")
      )
    })

    # Recalculate results depending on excluded concentrations
    shiny::observeEvent(input[["kobs_result_cell_edit"]], {
      # Apply changes to included concentrations
      conversion_vars$select_concentration[
        input[["kobs_result_cell_edit"]]$row
      ] <- input[[
        "kobs_result_cell_edit"
      ]]$value

      # Check number of selected concentrations
      if (sum(conversion_vars$select_concentration) < 3) {
        shinyWidgets::show_toast(
          "≥ 3 concentrations needed",
          type = "warning",
          timer = 3000
        )

        # Dont apply changes
        return(NULL)
      }

      # Recalculate result object according to included concentrations

      result_list <- conversion_sidebar_vars$result_list()

      # Add binding/kobs results to result list
      result_list$binding_kobs_result <- add_kobs_binding_result(
        result_list,
        concentrations_select = names(
          conversion_vars$select_concentration
        )[which(conversion_vars$select_concentration)]
      )

      # Add Ki/kinact results to result list
      result_list$ki_kinact_result <- add_ki_kinact_result(
        result_list
      )

      # Assign modified results to reactive variable
      conversion_vars$modified_results <- result_list
    })

    # shiny::observeEvent(input[["kobs_result_cell_edit"]], {
    #   shiny::req(conversion_sidebar_vars$result_list())

    #   # Check number of selected concentrations
    #   if (length(input$select_concentration) < 3) {
    #     shinyWidgets::show_toast(
    #       "≥ 3 concentrations needed",
    #       type = "warning",
    #       timer = 3000
    #     )

    #     # Assign concentrations selected before to checkbox input
    #     shiny::updateCheckboxGroupInput(
    #       session = session,
    #       inputId = "select_concentration",
    #       selected = conversion_vars$select_concentration
    #     )

    #     return(NULL)
    #   }

    #   conversion_vars$select_concentration <- input$select_concentration

    #   result_list <- conversion_sidebar_vars$result_list()

    #   # Add binding/kobs results to result list
    #   result_list$binding_kobs_result <- add_kobs_binding_result(
    #     result_list,
    #     concentrations_select = conversion_vars$select_concentration
    #   )

    #   # Add Ki/kinact results to result list
    #   result_list$ki_kinact_result <- add_ki_kinact_result(
    #     result_list
    #   )

    #   conversion_vars$modified_results <- result_list
    # })

    # output$concentration_select <- shiny::renderUI({
    #   shiny::req(conversion_sidebar_vars$result_list()$binding_kobs_result)

    #   # Get included concentrations
    #   concentrations <- which(
    #     !names(conversion_sidebar_vars$result_list()$binding_kobs_result) %in%
    #       c("binding_table", "binding_plot", "kobs_result_table")
    #   )

    #   # Define choices
    #   choices <- names(conversion_sidebar_vars$result_list()$binding_kobs_result)[
    #     concentrations
    #   ]

    #   shiny::checkboxGroupInput(
    #     ns("select_concentration"),
    #     label = "Include Concentrations",
    #     choices = choices,
    #     selected = choices
    #   )
    # })

    # Render kobs result table
    output$kobs_result <- DT::renderDT(
      {
        shiny::req(
          conversion_sidebar_vars$result_list(),
          conversion_vars$conc_colors
        )

        # Get results
        kobs_results <- conversion_sidebar_vars$result_list()$binding_kobs_result$kobs_result_table

        kobs_results <- kobs_results |>
          dplyr::mutate(
            kobs = paste(format(kobs, digits = 3), "s⁻¹"),
            v = format(v, digits = 3),
            plateau = paste(format(plateau, digits = 3), "%")
          )

        # Add concentration column
        kobs_results <- kobs_results |>
          dplyr::mutate(
            Concentration = paste(rownames(kobs_results), "µM"),
            .before = "kobs"
          )

        # Determine checkbox index
        checkbox_col_index <- 5

        # Add the checkbox
        kobs_results <- kobs_results |>
          dplyr::mutate(
            Included = checkboxColumn(
              nrow(kobs_results),
              checkbox_col_index,
              value = TRUE
            )
          )

        # Set names
        colnames(kobs_results) <- c(
          "Concentration",
          "kobs",
          "Velocity",
          "Plateau",
          "Included"
        )

        DT::datatable(
          data = kobs_results,
          rownames = FALSE,
          selection = "none",
          escape = FALSE,
          class = "compact row-border nowrap",
          options = list(
            dom = "t",
            autoWidth = TRUE,
            scrollX = TRUE,
            scrollY = TRUE,
            scrollCollapse = TRUE,
            fixedHeader = TRUE,
            stripe = FALSE
          ),
          editable = list(
            target = "cell",
            disable = list(columns = checkbox_col_index)
          ),
          callback = htmlwidgets::JS(js_code_gen(
            "kobs_result",
            checkbox_col_index,
            ns = session$ns
          ))
        ) |>
          DT::formatStyle(
            columns = 'Concentration',
            target = 'row',
            backgroundColor = DT::styleEqual(
              levels = kobs_results$Concentration,
              values = gsub(
                ",1)",
                ",0.6)",
                plotly::toRGB(conversion_vars$conc_colors)
              )
            )
          ) |>
          DT::formatStyle(
            1:4,
            `border-right` = "solid 1px #0000005c"
          )
      },
      server = FALSE
    )

    output$binding_plot <- plotly::renderPlotly({
      shiny::req(conversion_sidebar_vars$result_list())

      conversion_sidebar_vars$result_list()$binding_kobs_result$binding_plot
    })

    output$kobs_plot <- plotly::renderPlotly({
      shiny::req(conversion_sidebar_vars$result_list())

      if (is.null(conversion_vars$modified_results)) {
        result_list <- conversion_sidebar_vars$result_list()
      } else {
        result_list <- conversion_vars$modified_results
      }

      result_list$ki_kinact_result$kobs_plot
    })

    # Tooltip modal windows
    shiny::observeEvent(input$binding_curve_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span("Binding Curve"),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "Time-course of covalent adduct formation (% modified protein) measured by intact-mass MS at each compound concentration."
                  ),
                  shiny::p("Each trace is individually fitted to:"),
                  shiny::div(
                    class = "math-display",
                    "$$\\%\\,\\text{binding} = 100 \\times \\frac{v}{k_{\\text{obs}}} \\times \\big(1 - \\exp(-k_{\\text{obs}} \\cdot t)\\big)$$"
                  ),
                  shiny::p(
                    "An anchor point is forced at (t = 0, binding = 0). The plateau ",
                    shiny::strong("100 × v / k", htmltools::tags$sub("obs")),
                    " is the maximum covalent occupancy achievable."
                  )
                )
              )
            )
          )
        )
      )
    })

    shiny::observeEvent(input$kobs_curve_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span(
              "k",
              htmltools::tags$sub("obs"),
              " Curve"
            ),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "Plot of observed rate constants ",
                    shiny::strong("k", htmltools::tags$sub("obs")),
                    " versus compound concentration [C]."
                  ),
                  shiny::p(
                    "Global fit to the two-step covalent binding model:"
                  ),
                  shiny::div(
                    class = "math-display",
                    "$$k_{\\text{obs}} = \\frac{k_{\\text{inact}} \\times [C]}{K_{\\text{i}} + [C]}$$"
                  ),
                  shiny::p(
                    "Yields ",
                    shiny::strong("k", htmltools::tags$sub("inact")),
                    " (maximum covalent rate) and ",
                    shiny::strong("K", htmltools::tags$sub("i")),
                    " (apparent affinity of the initial reversible complex)."
                  )
                )
              )
            )
          )
        )
      )
    })

    shiny::observeEvent(input$binding_analysis_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = "Binding Analysis Table",
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "Results from individual exponential fits per concentration:"
                  ),
                  htmltools::tags$ul(
                    htmltools::tags$li(
                      shiny::strong("k", htmltools::tags$sub("obs")),
                      " – observed rate constant (time⁻¹)"
                    ),
                    htmltools::tags$li(
                      shiny::strong("v"),
                      " – fitted initial velocity parameter"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Plateau"),
                      " – max % modification = 100 × v / k",
                      htmltools::tags$sub("obs")
                    )
                  ),
                  shiny::p(
                    "The ",
                    shiny::strong("Included"),
                    " checkbox controls inclusion in the global fit for ",
                    shiny::strong("k", htmltools::tags$sub("inact")),
                    " and ",
                    shiny::strong("K", htmltools::tags$sub("i")),
                    "."
                  )
                )
              )
            )
          )
        )
      )
    })

    shiny::observeEvent(input$kinact_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span("k", htmltools::tags$sub("inact")),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    shiny::strong("k", htmltools::tags$sub("inact")),
                    " is the maximum first-order rate constant for covalent bond formation when the target is fully saturated (EC → EC*)."
                  ),
                  shiny::p(
                    "It measures intrinsic warhead reactivity and transition-state stabilization in the reversible complex, independent of binding affinity."
                  )
                )
              )
            )
          )
        )
      )
    })

    shiny::observeEvent(input$Ki_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span("K", htmltools::tags$sub("i")),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    shiny::strong("K", htmltools::tags$sub("i")),
                    " is the apparent dissociation constant of the initial non-covalent complex (E + C ⇌ EC)."
                  ),
                  shiny::p(
                    "Lower K",
                    htmltools::tags$sub("i"),
                    " indicates stronger reversible binding before covalency."
                  )
                )
              )
            )
          )
        )
      )
    })

    shiny::observeEvent(input$Ki_kinact_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span(
              "K",
              htmltools::tags$sub("i"),
              " / k",
              htmltools::tags$sub("inact")
            ),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "The key potency metric is the ",
                    shiny::strong("second-order rate constant"),
                    ":"
                  ),
                  shiny::div(
                    class = "math-display",
                    "$$\\frac{k_{\\text{inact}}}{K_{\\text{i}}}\\;(\\text{M}^{-1}\\text{s}^{-1})$$"
                  ),
                  shiny::p(
                    "Higher values indicate more efficient covalent labeling at low concentration."
                  )
                )
              )
            )
          )
        )
      )
    })

    shiny::observeEvent(input$mass_spectra_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span("Mass Spectra"),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "Intact-protein mass spectra acquired at different incubation time points with the compound (",
                    shiny::strong("[C]"),
                    " = current concentration)."
                  ),
                  shiny::p(
                    "Each trace shows the charge-state envelope of the protein. Progression from unmodified (lower m/z) to covalently modified species (higher m/z) is visible over time."
                  ),
                  shiny::p(
                    "The theoretical unmodified protein mass is marked. Mass shifts correspond to covalent adduct formation (+ compound mass)."
                  ),
                  shiny::p(
                    "Deconvolution into zero-charge mass distribution yields the % modified protein used to construct the binding curve."
                  )
                )
              )
            )
          )
        )
      )
    })

    shiny::observeEvent(input$binding_curve_single_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span(
              "Binding Curve (Single Concentration)"
            ),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "Time-resolved covalent adduct formation at a ",
                    shiny::strong("single fixed compound concentration"),
                    " derived from deconvolved intact-mass spectra."
                  ),
                  shiny::p(
                    "Fitted individually to the integrated rate equation:"
                  ),
                  shiny::div(
                    class = "math-display",
                    "$$\\%\\,\\text{binding} = 100 \\times \\frac{v}{k_{\\text{obs}}} \\times \\big(1 - \\exp(-k_{\\text{obs}} \\cdot t)\\big)$$"
                  ),
                  shiny::p(
                    "Yields ",
                    shiny::strong("k", htmltools::tags$sub("obs")),
                    " (observed rate constant), ",
                    shiny::strong("v"),
                    " (initial velocity), and ",
                    shiny::strong("Plateau"),
                    " (maximum achievable labeling at this concentration)."
                  ),
                  shiny::p(
                    "An anchor point at (t = 0, binding = 0) is forced for numerical stability."
                  )
                )
              )
            )
          )
        )
      )
    })

    shiny::observeEvent(input$hits_table_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal hits-table-tooltip",
          shiny::modalDialog(
            title = "Hits Table",
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "Detailed per-time-point results from intact-mass MS deconvolution and peak assignment at the currently selected compound concentration."
                  ),
                  htmltools::tags$ul(
                    htmltools::tags$li(
                      shiny::strong("Well / Sample ID"),
                      " – plate well and sample name or ID"
                    ),
                    htmltools::tags$li(
                      shiny::strong("[Cmp]"),
                      " – compound concentration"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Time"),
                      " – incubation time point"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Theor. Prot."),
                      " – theoretical mass of the unmodified protein"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Meas. Prot."),
                      " – measured deconvolved mass of the protein species"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Δ Prot."),
                      " – difference between theoretical and measured deconvolved protein mass"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Ⅰ Prot."),
                      " – relative intensity of the unmodified protein peak [%]"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Peak Signal"),
                      " – raw signal intensity for present peak"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Ⅰ Cmp"),
                      " – intensity of the peak representing the protein together with a compound adduct [%]"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Cmp Name"),
                      " – compound name or ID of the bound compound"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Theor. Cmp"),
                      " – theoretical mass of the bound compound"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Δ Cmp"),
                      " – difference between theoretical complex and the obtained deconvolved mass [Da]"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Bind. Stoich."),
                      " – detected binding stoichiometry (no. of bound compounds)"
                    ),
                    htmltools::tags$li(
                      shiny::strong("%-Binding"),
                      " – percentage of protein that has formed the covalent adduct at this time point"
                    ),
                    htmltools::tags$li(
                      shiny::strong("Total %-Binding"),
                      " – cumulative %-binding (identical to %-Binding when only one adduct is present)"
                    )
                  ),
                  shiny::p(
                    "The ",
                    shiny::strong("%-Binding"),
                    " (or ",
                    shiny::strong("Total %-Binding"),
                    ") values are used to construct the binding curve and to derive ",
                    shiny::strong("k", htmltools::tags$sub("obs")),
                    ", plateau, and initial velocity (v)."
                  )
                )
              )
            )
          )
        )
      )
    })

    shiny::observeEvent(input$kobs_value_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = htmltools::tags$span("k", htmltools::tags$sub("obs")),
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    shiny::strong("k", htmltools::tags$sub("obs")),
                    " is the ",
                    shiny::em("observed pseudo-first-order rate constant"),
                    " for covalent adduct formation at this specific compound concentration."
                  ),
                  shiny::p(
                    "It is extracted from the single-exponential fit of the binding curve shown."
                  ),
                  shiny::p(
                    "Units: typically min⁻¹ or s⁻¹. Under the two-step model:"
                  ),
                  shiny::div(
                    class = "math-display",
                    "$$k_{\\text{obs}} = \\frac{k_{\\text{inact}} \\times [C]}{K_{\\text{i}} + [C]}$$"
                  ),
                  shiny::p(
                    "At low [C], k",
                    htmltools::tags$sub("obs"),
                    " ∝ [C]; at high [C], k",
                    htmltools::tags$sub("obs"),
                    " → k",
                    htmltools::tags$sub("inact"),
                    "."
                  )
                )
              )
            )
          )
        )
      )
    })

    shiny::observeEvent(input$binding_plateau_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = "Binding Plateau",
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    "The ",
                    shiny::strong("Binding Plateau"),
                    " is the maximum percentage of covalently modified protein achieved at this compound concentration after long incubation."
                  ),
                  shiny::p(
                    "It is calculated as: ",
                    shiny::strong(
                      "Plateau = 100 × v / k",
                      htmltools::tags$sub("obs")
                    )
                  ),
                  shiny::p(
                    "Values < 100% indicate that even at infinite time, not all protein molecules form the covalent adduct — often due to competing off-pathway reactions or incomplete saturation of the reversible EC complex."
                  ),
                  shiny::p(
                    "Plateau approaches 100% only when [C] ≫ K",
                    htmltools::tags$sub("i"),
                    " and the covalent step is strongly favored."
                  )
                )
              )
            )
          )
        )
      )
    })

    shiny::observeEvent(input$v_value_tooltip_bttn, {
      shiny::showModal(
        shiny::div(
          class = "conversion-modal",
          shiny::modalDialog(
            title = "v (Initial Velocity Parameter)",
            easyClose = TRUE,
            footer = shiny::modalButton("Dismiss"),
            shiny::withMathJax(),
            shiny::fluidRow(
              shiny::br(),
              shiny::column(
                width = 11,
                shiny::div(
                  class = "tooltip-text",
                  shiny::p(
                    shiny::strong("v"),
                    " is a fitted parameter representing the initial rate of adduct formation scaled by the plateau."
                  ),
                  shiny::p(
                    "From the fitting model: ",
                    shiny::strong(
                      "Plateau = 100 × v / k",
                      htmltools::tags$sub("obs")
                    ),
                    " → v = (Plateau × k",
                    htmltools::tags$sub("obs"),
                    ") / 100"
                  ),
                  shiny::p(
                    "It has units of %·time⁻¹ and is useful mainly as an intermediate for calculating the plateau."
                  ),
                  shiny::p(
                    "In practice, v is tightly correlated with k",
                    htmltools::tags$sub("obs"),
                    " and plateau; only k",
                    htmltools::tags$sub("obs"),
                    " is used in the global k",
                    htmltools::tags$sub("inact"),
                    "/K",
                    htmltools::tags$sub("i"),
                    " analysis."
                  )
                )
              )
            )
          )
        )
      )
    })

    ################ Return server values ------------------
    list(
      selected_tab = shiny::reactive(input$tabs),
      set_selected_tab = set_selected_tab,
      conversion_ready = shiny::reactive(declaration_vars$conversion_ready),
      input_list = shiny::reactive(list(
        Protein_Table = declaration_vars$protein_table_sliced,
        Compound_Table = declaration_vars$compound_table_sliced,
        Samples_Table = declaration_vars$sample_table_sliced,
        result = declaration_vars$result
      )),
      cancel_continuation = shiny::reactive(input$conversion_cont_cancel)
    )
  })
}
