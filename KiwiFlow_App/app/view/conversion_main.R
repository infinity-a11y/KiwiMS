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

    # Reactive variables for conversion declarations
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
      conversion_ready = FALSE
    )

    # Reactive variables for conversion results
    conversion_vars <- shiny::reactiveValues(
      modified_results = NULL,
      select_concentration = NULL,
      formatted_hits = NULL,
      conc_colors = NULL
    )

    # Trigger
    shiny::observeEvent(conversion_dirs$result_list(), {
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

      # Add binding tab
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

      # Add hits tab
      bslib::nav_insert(
        "tabs",
        bslib::nav_panel(
          title = "Hits",
          shiny::div(
            class = "conversion-result-wrapper",
            DT::DTOutput(ns("hits_tab"))
          )
        )
      )

      # Summarize hits to table
      hits_summary <- conversion_dirs$result_list()$"hits_summary" |>
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
        dplyr::relocate(c(concentration, time), .before = `Mw Protein [Da]`) |>
        dplyr::relocate(`Total % Binding`, .after = "% Binding")

      # Change column names
      colnames(hits_summary) <- c(
        "Well",
        "Sample ID",
        "[Cmp.]",
        "Time",
        "Theor. Prot.",
        "Meas. Prot.",
        "Δ Prot.",
        "Ⅰ Prot.",
        "Peak Signal",
        "Ⅰ Cmp.",
        "Cmp. Name",
        "Theor. Cmp.",
        "Δ Cmp.",
        "Bind. Stoich.",
        "%-Binding",
        "Total %-Binding"
      )
      # Assign formatted hits to reactive variable
      conversion_vars$formatted_hits <- hits_summary

      # Get concentrations
      binding_kobs_result_names <- names(
        conversion_dirs$result_list()$binding_kobs_result
      )
      concentrations <- binding_kobs_result_names[
        !binding_kobs_result_names %in%
          c("binding_table", "binding_plot", "kobs_result_table")
      ]

      # Assign colors to present concentrations
      n_colors <- length(unique(hits_summary[["[Cmp.]"]]))
      concentration_colors <- rev(RColorBrewer::brewer.pal(
        n = max(3, n_colors),
        name = "Set1"
      )[1:n_colors])
      names(concentration_colors) <- c(concentrations, "0")
      concentration_colors[which(
        names(concentration_colors) == "0"
      )] <- "#ddddde"
      # Assign colors to reactive variable
      conversion_vars$conc_colors <- concentration_colors

      # Define a set of IDs for the dynamic tabs
      dynamic_ui_ids <- paste0("concentration_tab_", concentrations)

      # Add all present concentrations as results
      for (i in seq_along(concentrations)) {
        concentration <- concentrations[[i]]
        ui_id <- dynamic_ui_ids[[i]]

        bslib::nav_insert(
          "tabs",
          bslib::nav_panel(
            title = concentration,
            shiny::div(
              class = "conversion-result-wrapper",
              # Use namespaced ID for UI
              shiny::uiOutput(ns(ui_id))
            )
          )
        )
      }

      lapply(names(output), function(name) {
        if (grepl("^concentration_tab_", name)) {
          output[[name]] <- NULL
        }
      })

      # Loop through the list of concentrations
      for (i in seq_along(concentrations)) {
        concentration <- concentrations[[i]]
        ui_id <- dynamic_ui_ids[[i]]

        local({
          local_concentration <- concentration
          local_ui_id <- ui_id

          conc_result <- conversion_dirs$result_list()$binding_kobs_result[[
            local_concentration
          ]]

          # Render hits table
          output[[paste0(local_ui_id, "_hits")]] <- DT::renderDT({
            message(local_concentration)
            render_hits_table(
              hits_table = hits_summary |>
                dplyr::filter(
                  `[Cmp.]` == paste(local_concentration, "µM")
                ),
              concentration_colors = concentration_colors,
              single_conc = local_concentration
            )
          })

          # Render binding plot
          output[[paste0(
            local_ui_id,
            "_binding_plot"
          )]] <- plotly::renderPlotly({
            waiter::waiter_show(
              id = ns(paste0(
                local_ui_id,
                "_binding_plot"
              )),
              html = waiter::spin_wandering_cubes()
            )

            plot <- make_binding_plot(
              kobs_result = conversion_dirs$result_list()$binding_kobs_result,
              filter_conc = local_concentration
            )

            waiter::waiter_hide(
              id = ns(paste0(
                local_ui_id,
                "_binding_plot"
              ))
            )

            plot
          })

          # Render spectrum
          output[[paste0(
            local_ui_id,
            "_spectra"
          )]] <- plotly::renderPlotly({
            waiter::waiter_show(
              id = ns(paste0(
                local_ui_id,
                "_spectra"
              )),
              html = waiter::spin_wandering_cubes()
            )

            decon_samples <- gsub(
              "o",
              ".",
              sapply(
                strsplit(
                  names(conversion_dirs$result_list()$deconvolution),
                  "_"
                ),
                `[`,
                3
              )
            )

            plot <- multiple_spectra(
              results_list = conversion_dirs$result_list(),
              samples = names(
                conversion_dirs$result_list()$deconvolution
              )[which(
                decon_samples == local_concentration
              )],
              cubic = ifelse(
                input[[ns(paste0(
                  local_ui_id,
                  "_kind"
                ))]] ==
                  "3D",
                TRUE,
                FALSE
              )
            )

            waiter::waiter_hide(
              id = ns(paste0(
                local_ui_id,
                "_spectra"
              ))
            )

            plot
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
                    shiny::fluidRow(
                      shiny::column(8, "Spectrum Control"),
                      shiny::column(
                        4,
                        shinyWidgets::radioGroupButtons(
                          ns(paste0(
                            local_ui_id,
                            "_kind"
                          )),
                          choices = c("3D", "Planar")
                        )
                      )
                    )
                  ),
                  full_screen = TRUE,
                  plotly::plotlyOutput(
                    ns(paste0(local_ui_id, "_spectra")),
                    height = "100%"
                  )
                )
              ),
              shiny::div(
                class = "card-custom binding",
                bslib::card(
                  bslib::card_header(
                    class = "bg-dark help-header",
                    "% Binding",
                  ),
                  full_screen = TRUE,
                  plotly::plotlyOutput(
                    ns(paste0(
                      local_ui_id,
                      "_binding_plot"
                    )),
                    height = "100%"
                  )
                )
              ),
              shiny::div(
                class = "kobs-cards",
                shiny::div(
                  class = "card-custom",
                  bslib::card(
                    bslib::card_header(
                      class = "bg-dark help-header",
                      htmltools::tagList(
                        "k",
                        htmltools::tags$sub("obs")
                      )
                    ),
                    shiny::div(
                      class = "kobs_val",
                      format_scientific(conc_result$kobs)
                    )
                  )
                ),
                shiny::div(
                  class = "card-custom",
                  bslib::card(
                    bslib::card_header(
                      class = "bg-dark help-header",
                      "Plateau"
                    ),
                    shiny::div(
                      class = "kobs_val",
                      format_scientific(conc_result$plateau)
                    )
                  )
                ),
                shiny::div(
                  class = "card-custom",
                  bslib::card(
                    bslib::card_header(
                      class = "bg-dark help-header",
                      "v"
                    ),
                    shiny::div(
                      class = "kobs_val",
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
                  ),
                  full_screen = TRUE,
                  shiny::div(
                    class = "conc-hits-table",
                    DT::DTOutput(ns(paste0(local_ui_id, "_hits")))
                  )
                )
              )
            )
          })
        })
      }

      # Select binding results tab
      set_selected_tab("Binding", session)
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
            ),
            bslib::card_body(
              plotly::plotlyOutput(
                ns("binding_plot"),
                height = "100%"
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
              "kobs Curve",
            ),
            bslib::card_body(
              plotly::plotlyOutput(
                ns("kobs_plot"),
                height = "100%"
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
            ),
            bslib::card_body(
              shiny::tableOutput(ns("kobs_result"))
            )
          )
        ),
        shiny::div(
          class = "card-custom",
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              class = "bg-dark help-header",
              "Ki / kinact Analysis",
              htmltools::tagList(
                "K",
                htmltools::tags$sub("i"),
                " / k",
                htmltools::tags$sub("inact"),
                " Analysis"
              )
            ),
            bslib::card_body(
              shiny::div(
                class = "ki-kinact-ui",
                shiny::uiOutput(ns("concentration_select")),
                shiny::tableOutput(ns("ki_kinact_result"))
              )
            )
          )
        )
      )
    )

    # UI output for hits tab
    output$hits_tab <- DT::renderDT({
      shiny::req(
        conversion_vars$formatted_hits,
        conversion_vars$conc_colors
      )

      render_hits_table(
        hits_table = conversion_vars$formatted_hits,
        concentration_colors = conversion_vars$conc_colors
      )
    })

    # Recalculate results depending on excluded concentrations
    shiny::observeEvent(input$select_concentration, {
      shiny::req(conversion_dirs$result_list())

      # Check number of selected concentrations
      if (length(input$select_concentration) < 3) {
        shinyWidgets::show_toast(
          "≥ 3 concentrations needed",
          type = "warning",
          timer = 3000
        )

        # Assign concentrations selected before to checkbox input
        shiny::updateCheckboxGroupInput(
          session = session,
          inputId = "select_concentration",
          selected = conversion_vars$select_concentration
        )

        return(NULL)
      }

      conversion_vars$select_concentration <- input$select_concentration

      result_list <- conversion_dirs$result_list()

      # Add binding/kobs results to result list
      result_list$binding_kobs_result <- add_kobs_binding_result(
        result_list,
        concentrations_select = conversion_vars$select_concentration
      )

      # Add Ki/kinact results to result list
      result_list$ki_kinact_result <- add_ki_kinact_result(
        result_list
      )

      conversion_vars$modified_results <- result_list
    })

    output$concentration_select <- shiny::renderUI({
      shiny::req(conversion_dirs$result_list()$binding_kobs_result)

      # Get included concentrations
      concentrations <- which(
        !names(conversion_dirs$result_list()$binding_kobs_result) %in%
          c("binding_table", "binding_plot", "kobs_result_table")
      )

      # Define choices
      choices <- names(conversion_dirs$result_list()$binding_kobs_result)[
        concentrations
      ]

      shiny::checkboxGroupInput(
        ns("select_concentration"),
        label = "Include Concentrations",
        choices = choices,
        selected = choices
      )
    })

    output$kobs_result <- shiny::renderTable(
      {
        shiny::req(conversion_dirs$result_list())

        conversion_dirs$result_list()$binding_kobs_result$kobs_result_table
      },
      spacing = "xs",
      rownames = TRUE
    )

    output$ki_kinact_result <- shiny::renderTable(
      {
        shiny::req(conversion_dirs$result_list())

        if (is.null(conversion_vars$modified_results)) {
          result_list <- conversion_dirs$result_list()
        } else {
          result_list <- conversion_vars$modified_results
        }

        result_list$ki_kinact_result$Params
      },
      spacing = "xs",
      rownames = TRUE
    )

    output$binding_plot <- plotly::renderPlotly({
      shiny::req(conversion_dirs$result_list())

      conversion_dirs$result_list()$binding_kobs_result$binding_plot
    })

    output$kobs_plot <- plotly::renderPlotly({
      shiny::req(conversion_dirs$result_list())

      if (is.null(conversion_vars$modified_results)) {
        result_list <- conversion_dirs$result_list()
      } else {
        result_list <- conversion_vars$modified_results
      }

      result_list$ki_kinact_result$kobs_plot
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
        declaration_vars$protein_table <- table_upload_processed
        declaration_vars$protein_table_status <- TRUE

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
        declaration_vars$compound_table <- table_upload_processed
        declaration_vars$compound_table_status <- TRUE

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
      shiny::req(
        input$protein_table,
        shiny::isolate(declaration_vars$protein_table_active)
      )

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
        declaration_vars$protein_table_status <- FALSE

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
          declaration_vars$protein_table_status <- TRUE

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
          declaration_vars$protein_table_status <- FALSE

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
      shiny::req(
        input$compound_table,
        shiny::isolate(declaration_vars$compound_table_active)
      )

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
        declaration_vars$compound_table_status <- FALSE

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
          declaration_vars$compound_table_status <- TRUE

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
          declaration_vars$compound_table_status <- FALSE

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
      shiny::req(
        input$sample_table,
        shiny::isolate(declaration_vars$sample_table_active)
      )

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
        declaration_vars$sample_table_status <- FALSE

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
          declaration_vars$protein_table$Protein,
          declaration_vars$compound_table$Compound
        )

        if (isTRUE(sample_table_status)) {
          # Set status variable to TRUE
          declaration_vars$sample_table_status <- TRUE

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
          declaration_vars$sample_table_status <- FALSE

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

        # If edit applied always activate edit mode for sample table if present
        if (!is.null(declaration_vars$sample_table)) {
          # Make table observer active
          declaration_vars$sample_table_active <- TRUE

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
            sample_handsontable(declaration_vars$sample_table, disabled = FALSE)
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

        # Edit Protein/Compound
        if (input$tabs == "Proteins") {
          # Make table observer active
          declaration_vars$protein_table_active <- TRUE

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
            prot_comp_handsontable(
              declaration_vars$protein_table,
              disabled = FALSE
            )
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
          declaration_vars$compound_table_active <- TRUE

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
            prot_comp_handsontable(
              declaration_vars$compound_table,
              disabled = FALSE
            )
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
          if (declaration_vars$protein_table_status) {
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
            declaration_vars$protein_table_active <- FALSE

            # Show table message
            output$protein_table_info <- shiny::renderText("Table saved!")

            # Render sample table with new input
            if (!is.null(input$sample_table)) {
              output$sample_table <- rhandsontable::renderRHandsontable({
                # waiter::waiter_show(
                #   id = ns("sample_table"),
                #   html = waiter::spin_throbber()
                # )

                sample_handsontable(
                  tab = slice_sample_tab(rhandsontable::hot_to_r(
                    input$sample_table
                  )),
                  proteins = protein_table$Protein,
                  compounds = declaration_vars$compound_table$Compound
                )

                # waiter::waiter_hide(id = ns("sample_table"))
              })

              # Jump to next tab module
              set_selected_tab("Samples", session)
            } else {
              # Jump to next tab module
              set_selected_tab("Compounds", session)
            }

            # Assign user input to reactive table variable
            declaration_vars$protein_table <- protein_table
          }
        } else if (input$tabs == "Compounds") {
          # If table can be saved perform actions
          if (declaration_vars$compound_table_status) {
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
            declaration_vars$compound_table_active <- FALSE

            # Show table message
            output$compound_table_info <- shiny::renderText("Table saved!")

            # Render sample table with new input
            if (!is.null(input$sample_table)) {
              output$sample_table <- rhandsontable::renderRHandsontable(
                sample_handsontable(
                  tab = slice_sample_tab(rhandsontable::hot_to_r(
                    input$sample_table
                  )),
                  proteins = declaration_vars$protein_table$Protein,
                  compounds = compound_table$Compound
                )
              )
            }

            # Jump to next tab module
            set_selected_tab("Samples", session)

            # Assign user input to reactive table variable
            declaration_vars$compound_table <- compound_table
          }
        } else if (input$tabs == "Samples") {
          # If table can be saved perform actions
          if (declaration_vars$sample_table_status) {
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
            declaration_vars$sample_table_active <- FALSE

            # Show table message
            output$sample_table_info <- shiny::renderText("Table saved!")

            # Assign user input to reactive table variable
            declaration_vars$sample_table <- sample_table
          }
        }
      }
    )

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
    shiny::observe({
      if (
        is.null(declaration_vars$protein_table) ||
          is.null(declaration_vars$compound_table) ||
          isTRUE(declaration_vars$protein_table_active) ||
          isTRUE(declaration_vars$compound_table_active)
      ) {
        shinyjs::removeClass(
          "sample_table_info",
          "table-info-green"
        )
        shinyjs::addClass(
          "sample_table_info",
          "table-info-red"
        )
        shinyjs::disable("confirm_samples")
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
        shinyjs::disable("confirm_samples")
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
        declaration_vars$result <- readRDS(file_path)

        sample_tab <- data.frame(
          Sample = names(declaration_vars$result$deconvolution),
          Protein = ifelse(
            length(declaration_vars$protein_table$Protein) == 1,
            declaration_vars$protein_table$Protein,
            ""
          ),
          Compound = ifelse(
            length(declaration_vars$compound_table$Compound) == 1,
            declaration_vars$compound_table$Compound,
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

        if (!isTRUE(declaration_vars$sample_tab_initial)) {
          output$sample_table <- rhandsontable::renderRHandsontable({
            # waiter::waiter_show(
            #   id = ns("sample_table"),
            #   html = waiter::spin_throbber()
            # )

            sample_handsontable(
              tab = sample_tab,
              proteins = declaration_vars$protein_table$Protein,
              compounds = declaration_vars$compound_table$Compound
            )

            # waiter::waiter_hide(id = ns("sample_table"))
          })
        }

        declaration_vars$sample_tab_initial <- TRUE
      }
    })

    # Render compound table
    shiny::observe({
      if (is.null(declaration_vars$compound_table)) {
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
      if (is.null(declaration_vars$protein_table)) {
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
      conversion_ready = shiny::reactive(declaration_vars$conversion_ready),
      input_list = shiny::reactive(list(
        Protein_Table = declaration_vars$protein_table,
        Compound_Table = declaration_vars$compound_table,
        Samples_Table = declaration_vars$sample_table,
        result = declaration_vars$result
      ))
    )
  })
}
