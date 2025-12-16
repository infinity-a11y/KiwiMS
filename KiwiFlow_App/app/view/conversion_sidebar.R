# app/view/upload_spectra.R

box::use(
  bslib[card, card_body, card_header, sidebar],
  shiny[actionButton, fileInput, NS, textInput, moduleServer, reactive],
)

box::use(
  app /
    logic /
    conversion_functions[
      add_hits,
      summarize_hits,
      add_kobs_binding_result,
      add_ki_kinact_result,
    ],
  app /
    logic /
    logging[
      write_log,
    ]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bslib::sidebar(
    class = "conversion-sidebar",
    width = "15%",
    shinyjs::useShinyjs(),
    shiny::uiOutput(ns("conversion_sidebar_ui"))
  )
}

#' @export
server <- function(id, conversion_main_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Declare reactive vars
    result_list <- shiny::reactiveVal(NULL)
    analysis_status <- shiny::reactiveVal("pending")

    # Render sidebar ui
    output$conversion_sidebar_ui <- shiny::renderUI({
      shiny::div(
        class = "conversion-sidebar-ui",
        # shiny::uiOutput(ns("conversion_info_ui")),
        shiny::uiOutput(ns("conversion_analysis_controls_ui")),
        shiny::uiOutput(ns("conversion_result_controls_ui"))
      )
    })

    # Render conversion result controls UI
    output$conversion_result_controls_ui <- shiny::renderUI({
      # List of protein-compound-complexes
      complexes <- list(
        "Global Overview",
        "COOB" = paste0("Cmp-", 30:40),
        "KRAS" = paste0("ALMP3kALMP3kALMP3k-", 1:5)
      )

      # shinyjs::disabled(
      picker_ui <- shinyWidgets::pickerInput(
        ns("sample_picker"),
        shiny::div(
          class = "label-tooltip",
          shiny::tags$label("Complexes"),
          shiny::div(
            class = "tooltip-bttn",
            shiny::actionButton(
              ns("max_mult_tooltip_bttn"),
              label = "",
              icon = shiny::icon("circle-question")
            )
          )
        ),
        choices = complexes
      )
      # )

      shiny::div(
        class = "interaction-analysis-flex",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "sidebar-title conversion-title",
              "Results"
            ),
            picker_ui,
            shiny::actionButton(
              ns("report_conversion_results"),
              "Report",
              icon = shiny::icon("square-poll-vertical"),
              width = "100%"
            )
          )
        )
      )
    })

    # Render conversion analysis controls UI
    output$conversion_analysis_controls_ui <- shiny::renderUI(
      shiny::div(
        class = "interaction-analysis-flex",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "sidebar-title conversion-title",
              "Binding Analysis"
            ),
            shiny::numericInput(
              ns("peak_tolerance"),
              shiny::div(
                class = "label-tooltip",
                shiny::tags$label("Peak Tolerance [Da]"),
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("peak_tol_tooltip_bttn"),
                    label = "",
                    icon = shiny::icon("circle-question")
                  )
                )
              ),
              value = 3,
              min = 0,
              max = 20,
              step = 0.1
            ),
            shiny::numericInput(
              ns("max_multiples"),
              shiny::div(
                class = "label-tooltip",
                shiny::tags$label("Max. Stoichiometry"),
                shiny::div(
                  class = "tooltip-bttn",
                  shiny::actionButton(
                    ns("max_mult_tooltip_bttn"),
                    label = "",
                    icon = shiny::icon("circle-question")
                  )
                )
              ),
              value = 4,
              min = 1,
              max = 20,
              step = 1
            ),
            shiny::div(
              class = "ki-kinact-checkbox",
              shiny::checkboxInput(
                ns("run_ki_kinact"),
                shiny::div(
                  class = "ki-kinact-label",
                  "Run",
                  shiny::div(
                    class = "ki-kinact-highlight",
                    " K",
                    htmltools::tags$sub("i"),
                    " / k",
                    htmltools::tags$sub("inact")
                  ),
                  " Analysis"
                ),
                value = FALSE
              )
            ),
            #TODO
            # shinyjs::disabled(
            shiny::actionButton(
              ns("run_binding_analysis"),
              "Run",
              icon = shiny::icon("play"),
              width = "100%"
            ),
            # )
          )
        )
      )
    )

    shiny::observeEvent(conversion_main_vars$samples_confirmed(), {
      shinyjs::toggleState("run_ki_kinact")
      shinyjs::toggleClass(selector = ".checkbox", class = "checkbox-disable")
    })

    # Enable/Disable conversion parameter and launch input UI
    shiny::observe({
      shiny::req(
        conversion_main_vars$conversion_ready(),
        conversion_main_vars$input_list()
      )

      shinyjs::toggleState(
        "max_multiples",
        conversion_main_vars$conversion_ready()
      )
      shinyjs::toggleState(
        "peak_tolerance",
        conversion_main_vars$conversion_ready()
      )
      shinyjs::toggleState(
        "run_binding_analysis",
        conversion_main_vars$conversion_ready()
      )
      shinyjs::toggleClass(
        "run_binding_analysis",
        "btn-highlight",
        conversion_main_vars$conversion_ready()
      )
    })

    # Event run conversion
    shiny::observeEvent(input$run_binding_analysis, {
      # shiny::req(conversion_main_vars$input_list(), input$peak_tolerance, input$max_multiples)

      # Block UI
      shinyjs::runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "block";'
      ))

      if (analysis_status() == "pending") {
        # Search and add hits to result list
        # result_with_hits <- add_hits(
        #   conversion_main_vars$input_list()$result,
        #   sample_table = conversion_main_vars$input_list()$Samples_Table,
        #   protein_table = conversion_main_vars$input_list()$Protein_Table,
        #   compound_table = conversion_main_vars$input_list()$Compound_Table,
        #   peak_tolerance = input$peak_tolerance,
        #   max_multiples = input$max_multiples
        # )

        # # Add summarized hits table to result list
        # result_with_hits$hits_summary <- summarize_hits(result_with_hits)

        # result_with_hits <<- result_with_hits

        # # Add binding/kobs results to result list
        # result_with_hits$binding_kobs_result <- add_kobs_binding_result(
        #   result_with_hits
        # )

        # # Add Ki/kinact results to result list
        # result_with_hits$ki_kinact_result <- add_ki_kinact_result(
        #   result_with_hits
        # )

        # # Assign result list and hits table to reactive vars
        # result_list(result_with_hits)

        # results <<- result_with_hits
        # TODO
        # Dev Mode
        result_list(readRDS(
          "C:\\Users\\Marian\\Desktop\\KF_Testing\\results.rds"
        ))

        shiny::updateActionButton(
          session = session,
          "run_binding_analysis",
          label = "Reset",
          icon = shiny::icon("repeat")
        )

        analysis_status("done")
      } else {
        result_list(NULL)
        analysis_status("pending")

        shiny::updateActionButton(
          session = session,
          "run_binding_analysis",
          label = "Run",
          icon = shiny::icon("play"),
          disabled = TRUE
        )
      }

      # Unblock UI
      shinyjs::runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "none";'
      ))
    })

    output$conversion_info_ui <- shiny::renderUI({
      shiny::req(conversion_main_vars$selected_tab())

      if (conversion_main_vars$selected_tab() == "Proteins") {
        hints <- shiny::HTML(
          "Upload a CSV|TSV|TXT|Excel file or manually enter names and mass values of the <strong>proteins</strong> into the table."
        )
      } else if (conversion_main_vars$selected_tab() == "Compounds") {
        hints <- shiny::HTML(
          "Upload a CSV|TSV|TXT|Excel file or manually enter names and mass values of the <strong>compounds</strong> into the table."
        )
      } else if (conversion_main_vars$selected_tab() == "Samples") {
        hints <- shiny::HTML(
          "Assign <strong>protein-compound complexes</strong> to deconvoluted samples."
        )
      } else if (conversion_main_vars$selected_tab() == "Binding") {
        hints <- shiny::HTML(
          "Global fit of a concentration series of binding curves determining binding parameters for the selected complex."
        )
      } else if (conversion_main_vars$selected_tab() == "Hits") {
        hints <- shiny::HTML(
          "The 'Hits' tab shows all signals assigned to the currently selected complex and respectively inferred parameters."
        )
      } else {
        hints <- "%-Binding inferred from time series measurements of a single concentration."
      }

      shiny::div(
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "conversion-sidebar-title",
              shiny::uiOutput(ns("selected_module"))
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "instruction-info",
              hints
            )
          )
        )
      )
    })

    output$selected_module <- shiny::renderUI({
      shiny::req(conversion_main_vars$selected_tab())

      if (
        conversion_main_vars$selected_tab() %in%
          c("Proteins", "Compounds", "Samples")
      ) {
        title_add <- "Declaration"
      } else if (
        conversion_main_vars$selected_tab() %in% c("Binding", "Hits")
      ) {
        title_add <- ""
      } else {
        title_add <- "Concentration"
      }

      shiny::div(
        class = "sidebar-title conversion-title",
        paste(conversion_main_vars$selected_tab(), title_add),
        shiny::div(
          class = "tooltip-bttn",
          shiny::actionButton(
            ns("sidebar_tooltip_bttn"),
            label = "",
            icon = shiny::icon("circle-question")
          )
        )
      )
    })

    # Tooltip events
    shiny::observeEvent(input$sidebar_tooltip_bttn, {
      if (conversion_main_vars$selected_tab() == "Proteins") {
        title <- "Protein Declaration"
        hints <- shiny::column(
          width = 12,
          shiny::div(
            class = "tooltip-text",
            "One or more proteins can be screened for. The protein names/IDs together with their Mw values [Da] can be defined either via file upload or by entering the values into the table. The table also supports copy/paste for efficient filling."
          ),
          shiny::br(),
          shiny::div(
            class = "tooltip-img-text",
            "The format requires the name/ID as first column and up to nine columns the theoretical mass as well as any mass shifts per protein. Headers are optional."
          ),
          shiny::br(),
          shiny::tags$img(src = "static/protein_table.png"),
          shiny::br()
        )
      } else if (conversion_main_vars$selected_tab() == "Compounds") {
        title <- "Compound Declaration"
        hints <- shiny::column(
          width = 12,
          shiny::div(
            class = "tooltip-text",
            "One or more compounds can be screened for. The compound names/IDs together with their Mw values [Da] can be defined either via file upload or by entering the values into the table. The table also supports copy/paste for efficient filling."
          ),
          shiny::br(),
          shiny::div(
            class = "tooltip-img-text",
            "The format requires the name/ID as first column and up to nine columns the theoretical mass as well as any mass shifts per compound. Headers are optional."
          ),
          shiny::br(),
          shiny::tags$img(
            src = "static/compound_table.png"
          ),
          shiny::br()
        )
      } else if (conversion_main_vars$selected_tab() == "Samples") {
        title <- "Samples Declaration"
        hints <- shiny::column(
          width = 12,
          shiny::div(
            class = "tooltip-text",
            "One or more compounds can be screened for. The compound names/IDs together with their Mw values [Da] can be defined either via file upload or by entering the values into the table. The table also supports copy/paste for efficient filling."
          ),
          shiny::br(),
          shiny::div(
            class = "tooltip-img-text",
            "The format requires the name/ID as first column and up to nine columns the theoretical mass as well as any mass shifts per compound. Headers are optional."
          ),
          shiny::br(),
          shiny::tags$img(
            src = "static/compound_table.png"
          ),
          shiny::br()
        )
      } else if (conversion_main_vars$selected_tab() == "Binding") {
        title <- "Binding Analysis"
        hints <- shiny::column(
          width = 12,
          shiny::withMathJax(),
          shiny::div(
            class = "tooltip-text",
            shiny::p(
              "This tab displays the complete concentration series for a single compound, enabling global determination of the two fundamental covalent binding parameters: ",
              shiny::strong("k", htmltools::tags$sub("inact")),
              " (maximum inactivation rate at saturation) and ",
              shiny::strong("K", htmltools::tags$sub("i")),
              " (apparent dissociation constant of the initial reversible complex)."
            ),
            shiny::p(
              "Individual time-courses are fitted to extract k",
              htmltools::tags$sub("obs"),
              " values, which are then globally fitted to the hyperbolic two-step model to yield the second-order rate constant ",
              shiny::strong(
                "k",
                htmltools::tags$sub("inact"),
                " / K",
                htmltools::tags$sub("i")
              ),
              " — the gold-standard metric of covalent binder efficiency at low occupancy."
            )
          )
        )
      } else if (conversion_main_vars$selected_tab() == "Hits") {
        title <- "Hits Table"
        hints <- shiny::fluidRow(
          shiny::br(),
          shiny::column(
            width = 11,
            shiny::div(
              class = "tooltip-text",
              shiny::p(
                "The hits table lists all peak signals that correspond to the declared proteins and compounds with respect to their molecular weights including mass shifts and multiple binding (stoichiometry). Signals that fall within the user-determined peak tolerance values are considered."
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
      } else {
        title <- "Single Concentration Time Series"
        hints <- "Binding parameters derived from mass spectra of time series measurements of a single concentration."
      }

      shiny::showModal(
        shiny::div(
          class = "tip-modal",
          shiny::modalDialog(
            hints,
            title = title,
            easyClose = TRUE,
            footer = shiny::tagList(
              shiny::modalButton("Dismiss")
            )
          )
        )
      )
    })

    # Server return values
    return(
      shiny::reactiveValues(
        result_list = shiny::reactive(result_list()),
        sample_picker = shiny::reactive(input$sample_picker),
        run_analysis = shiny::reactive(input$run_binding_analysis),
        peak_tolerance = shiny::reactive(input$peak_tolerance),
        run_ki_kinact = shiny::reactive(input$run_ki_kinact)
      )
    )
  })
}
