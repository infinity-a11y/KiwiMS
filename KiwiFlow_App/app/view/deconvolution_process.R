# app/view/deconvolution_process.R

box::use(
  bslib[card, card_body, card_header, tooltip],
  fs[dir_ls],
  plotly[event_data, event_register, plotlyOutput, renderPlotly],
  processx[process],
  shiny,
  shinyjs[delay, disable, disabled, enable, hide, show, hidden, runjs],
  shinyWidgets[
    pickerInput,
    progressBar,
    radioGroupButtons,
    updateProgressBar
  ],
  clipr[write_clip],
  utils[head, tail],
  waiter[useWaiter, spin_wandering_cubes, waiter_show, waiter_hide, withWaiter],
)

box::use(
  app /
    logic /
    deconvolution_functions[
      create_384_plate_heatmap,
      spectrum_plot
    ],
  app / logic / helper_functions[fill_empty],
  app / logic / logging[write_log, get_log],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$div(
    class = "row-fullheight",
    shiny$fluidRow(
      shiny$column(
        width = 12,
        shiny$uiOutput(ns("deconvolution_init_ui")),
        shiny$uiOutput(ns("deconvolution_running_ui"))
      )
    )
  )
}

#' @export
server <- function(id, dirs) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    settings_dir <- file.path(
      Sys.getenv("LOCALAPPDATA"),
      "KiwiFlow",
      "settings"
    )
    results_dir <- file.path(
      Sys.getenv("USERPROFILE"),
      "Documents",
      "KiwiFlow",
      "results"
    )
    log_path <- get_log()

    ### Reactive variables declaration ----
    reactVars <- shiny$reactiveValues(
      isRunning = FALSE,
      completedFiles = 0,
      expectedFiles = 0,
      current_total_files = 0,
      initialFileCount = 0,
      lastCheck = 0,
      lastCheckresults = 0,
      count = 0,
      rep_count = 0,
      rslt_df = data.frame(),
      logs = "",
      deconv_report_status = "idle"
    )

    result_files_sel <- shiny$reactiveVal()
    target_selector_sel <- shiny$reactiveVal()

    shiny$observe({
      if (!is.null(input$result_picker)) {
        result_files_sel(input$result_picker)
      }
      if (!is.null(input$target_selector)) {
        target_selector_sel(input$target_selector)
      }
    })

    decon_process_data <- shiny$reactiveVal(NULL)

    ### Deconvolution initiation interface ----

    deconvolution_init_ui <- shiny$div(
      shiny$fluidRow(
        shiny$column(
          width = 12,
          shiny$fluidRow(
            shiny$column(
              width = 12,
              shiny$div(
                class = "sidebar-title",
                shiny$HTML("Configure Spectrum Deconvolution")
              )
            )
          ),
          shiny$fluidRow(
            shiny$column(
              width = 8,
              shiny$div(
                class = "deconvolution_info",
                shiny$HTML(
                  paste(
                    "1. Use the sidebar to select the Waters .raw folder(s) for processing.",
                    "<br/>",
                    "2. Check and configure parameters in the main panel and start deconvolution."
                  )
                )
              ),
              shiny$br()
            ),
            shiny$column(
              width = 4,
              shiny$checkboxInput(
                ns("show_advanced"),
                "Edit advanced settings",
                value = FALSE
              )
            )
          ),
          shiny$fluidRow(
            shiny$column(
              width = 4,
              shiny$div(
                class = "card-custom",
                card(
                  card_header(
                    class = "bg-dark",
                    "Charge state [z]",
                    tooltip(
                      shiny$icon("circle-question"),
                      "The number of charges the ionized molecule is expected to carry.",
                      placement = "right"
                    )
                  ),
                  card_body(
                    shiny$fluidRow(
                      shiny$column(
                        width = 4,
                        shiny$h6("Low", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 6,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("startz"),
                            "",
                            min = 0,
                            max = 100,
                            value = 1
                          )
                        )
                      )
                    ),
                    shiny$fluidRow(
                      shiny$column(
                        width = 4,
                        shiny$h6("High", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 6,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("endz"),
                            "",
                            min = 0,
                            max = 100,
                            value = 50
                          )
                        )
                      )
                    )
                  )
                )
              ),
              shiny$div(
                class = "card-custom",
                card(
                  card_header(
                    class = "bg-dark",
                    "Spectrum range [m/z]",
                    tooltip(
                      shiny$icon("circle-question"),
                      "The span of molecular weights to be analyzed.",
                      placement = "right"
                    )
                  ),
                  card_body(
                    shiny$fluidRow(
                      shiny$column(
                        width = 4,
                        shiny$h6("Low", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 6,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("minmz"),
                            "",
                            min = 0,
                            max = 100000,
                            value = 710
                          )
                        )
                      )
                    ),
                    shiny$fluidRow(
                      shiny$column(
                        width = 4,
                        shiny$h6("High", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 6,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("maxmz"),
                            "",
                            min = 0,
                            max = 100000,
                            value = 1100
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            shiny$column(
              width = 4,
              shiny$div(
                class = "card-custom",
                card(
                  card_header(
                    class = "bg-dark",
                    "Mass range [Mw]",
                    tooltip(
                      shiny$icon("circle-question"),
                      "The range of mass-to-charge ratios to be detected.",
                      placement = "right"
                    )
                  ),
                  card_body(
                    shiny$fluidRow(
                      shiny$column(
                        width = 4,
                        shiny$h6("Low", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 6,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("masslb"),
                            "",
                            min = 0,
                            max = 100000,
                            value = 35000
                          )
                        )
                      )
                    ),
                    shiny$fluidRow(
                      shiny$column(
                        width = 4,
                        shiny$h6("High", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 6,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("massub"),
                            "",
                            min = 0,
                            max = 100000,
                            value = 42000
                          )
                        )
                      )
                    )
                  )
                )
              ),
              shiny$div(
                class = "card-custom",
                card(
                  card_header(
                    class = "bg-dark",
                    "Retention time [min]",
                    tooltip(
                      shiny$icon("circle-question"),
                      "The anticipated time for the analyte to travel through a chromatography column.",
                      placement = "right"
                    )
                  ),
                  card_body(
                    shiny$fluidRow(
                      shiny$column(
                        width = 4,
                        shiny$h6("Start", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 6,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("time_start"),
                            "",
                            min = 0,
                            max = 100,
                            value = 1,
                            step = 0.05
                          )
                        )
                      )
                    ),
                    shiny$fluidRow(
                      shiny$column(
                        width = 4,
                        shiny$h6("End", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 6,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("time_end"),
                            "",
                            min = 0,
                            max = 100,
                            value = 1.35,
                            step = 0.05
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            shiny$column(
              width = 4,
              shiny$div(
                class = "card-custom",
                card(
                  card_header(
                    class = "bg-dark",
                    "Peak parameters",
                    tooltip(
                      shiny$icon("circle-question"),
                      "Expected characteristics of spectral peaks.",
                      placement = "right"
                    )
                  ),
                  card_body(
                    shiny$fluidRow(
                      shiny$column(
                        width = 4,
                        shiny$h6("Window", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 6,
                        shiny$div(
                          class = "deconv-param-input-adv",
                          disabled(
                            shiny$numericInput(
                              ns("peakwindow"),
                              "",
                              min = 0,
                              max = 1000,
                              value = 40
                            )
                          )
                        )
                      )
                    ),
                    shiny$fluidRow(
                      shiny$column(
                        width = 4,
                        shiny$h6("Norm", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 6,
                        shiny$div(
                          class = "deconv-param-input-adv",
                          disabled(
                            shiny$numericInput(
                              ns("peaknorm"),
                              "",
                              min = 0,
                              max = 100,
                              value = 2
                            )
                          )
                        )
                      )
                    ),
                    shiny$fluidRow(
                      shiny$column(
                        width = 4,
                        shiny$h6(
                          "Threshold",
                          style = "font-size: 0.9em; margin-top: 8px;"
                        )
                      ),
                      shiny$column(
                        width = 6,
                        shiny$div(
                          class = "deconv-param-input-adv",
                          disabled(
                            shiny$numericInput(
                              ns("peakthresh"),
                              "",
                              min = 0,
                              max = 1,
                              value = 0.07,
                              step = 0.01
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              shiny$div(
                class = "card-custom",
                card(
                  card_header(
                    class = "bg-dark",
                    "Mass Bins",
                    tooltip(
                      shiny$icon("circle-question"),
                      "Discrete intervals of mass values for the spectra.",
                      placement = "right"
                    )
                  ),
                  card_body(
                    shiny$fluidRow(
                      shiny$column(
                        width = 4,
                        shiny$h6("Size", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 6,
                        shiny$div(
                          class = "deconv-param-input-adv",
                          disabled(
                            shiny$numericInput(
                              ns("massbins"),
                              "",
                              min = 0,
                              max = 100,
                              value = 0.5,
                              step = 0.1
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      shiny$div(
        class = "align-row",
        shiny$fluidRow(
          shiny$column(
            width = 12,
            align = "center",
            shiny$uiOutput(ns("deconvolute_start_ui")),
            shiny$br(),
            shiny$uiOutput(ns("deconvolute_progress"))
          )
        )
      )
    )

    output$deconvolution_init_ui <- shiny$renderUI({
      deconvolution_init_ui
    })

    # Conditional enabling of advanced settings
    shiny$observe({
      if (isTRUE(input$show_advanced)) {
        shinyjs::enable(
          selector = ".deconv-param-input-adv",
          asis = TRUE
        )
      } else {
        shinyjs::disable(
          selector = ".deconv-param-input-adv",
          asis = TRUE
        )
      }
    })

    ### Deconvolution running interface ----
    deconvolution_running_ui_plate <- shiny$column(
      width = 12,
      useWaiter(),
      shiny$fluidRow(
        shiny$column(
          width = 2,
          align = "center",
          shinyjs::hidden(
            shiny$div(
              id = ns("processing"),
              shiny$HTML(
                paste0(
                  '<i class="fa fa-spinner fa-spin fa-fw fa-2x" style="color: ',
                  '#38387C; margin-top: 0.5em"></i>'
                )
              )
            )
          ),
          shinyjs::hidden(
            shiny$div(
              id = ns("processing_stop"),
              shiny$HTML(
                paste0(
                  '<i class="fa-solid fa-spinner fa-2x" style="color: ',
                  '#38387C; margin-top: 0.5em"></i>'
                )
              )
            )
          ),
          shinyjs::hidden(
            shiny$div(
              id = ns("processing_fin"),
              shiny$HTML(
                paste0(
                  '<i class="fa-solid fa-circle-check fa-2x" style="color: ',
                  '#38387C; margin-top: 0.5em"></i>'
                )
              )
            )
          )
        ),
        shiny$column(
          width = 6,
          progressBar(
            id = ns("progressBar"),
            value = 0,
            title = "Initiating Deconvolution",
            display_pct = TRUE
          )
        ),
        shiny$column(
          width = 2,
          shiny$actionButton(
            ns("deconvolute_end"),
            "Abort",
            icon = shiny$icon("circle-stop")
          )
        ),
        shiny$column(
          width = 2,
          shiny$div(
            class = "decon-btn",
            disabled(
              shiny$actionButton(
                ns("forward_deconvolution"),
                "Next Step",
                icon = shiny$icon("forward-fast")
              )
            )
          )
        )
      ),
      shiny$hr(style = "margin: 1.5rem 0; opacity: 0.8;"),
      shiny$fluidRow(
        shiny$column(
          width = 3,
          shiny$div(
            class = "decon-btn",
            shiny$actionButton(
              ns("show_log"),
              "Show Log",
              icon = shiny$icon("code")
            )
          )
        ),
        shiny$column(
          width = 3,
          shiny$div(
            class = "decon-btn",
            disabled(
              shiny$actionButton(
                ns("deconvolution_report"),
                "Get Report",
                icon = shiny$icon("square-poll-vertical")
              )
            )
          )
        ),
        shiny$column(
          width = 3,
          align = "center",
          shiny$uiOutput(ns("result_picker_ui"))
        ),
        shiny$column(
          width = 3,
          align = "center",
          disabled(
            radioGroupButtons(
              ns("toggle_result"),
              choiceNames = c("Deconvoluted", "Raw m/z"),
              choiceValues = c(FALSE, TRUE)
            )
          )
        )
      ),
      shiny$fluidRow(
        shiny$column(
          width = 6,
          shiny$br(),
          shiny$div(
            class = "card-custom-plate",
            card(
              card_header(
                class = "bg-dark",
                "384-Well Plate Heatmap"
              ),
              card_body(
                withWaiter(
                  plotlyOutput(ns("heatmap"))
                )
              )
            )
          )
        ),
        shiny$column(
          width = 6,
          shiny$div(
            class = "card-custom-plate2",
            card(
              card_header(
                class = "bg-dark",
                "Spectrum"
              ),
              card_body(
                plotlyOutput(ns("spectrum"))
              )
            )
          )
        )
      )
    )

    deconvolution_running_ui_noplate <- shiny$column(
      width = 12,
      useWaiter(),
      shiny$fluidRow(
        shiny$column(
          width = 2,
          align = "center",
          shinyjs::hidden(
            shiny$div(
              id = ns("processing"),
              shiny$HTML(
                paste0(
                  '<i class="fa fa-spinner fa-spin fa-fw fa-2x" style="color: ',
                  '#38387C; margin-top: 0.5em"></i>'
                )
              )
            )
          ),
          shinyjs::hidden(
            shiny$div(
              id = ns("processing_stop"),
              shiny$HTML(
                paste0(
                  '<i class="fa-solid fa-spinner fa-2x" style="color: ',
                  '#38387C; margin-top: 0.5em"></i>'
                )
              )
            )
          ),
          shinyjs::hidden(
            shiny$div(
              id = ns("processing_fin"),
              shiny$HTML(
                paste0(
                  '<i class="fa-solid fa-circle-check fa-2x" style="color: ',
                  '#38387C; margin-top: 0.5em"></i>'
                )
              )
            )
          )
        ),
        shiny$column(
          width = 6,
          progressBar(
            id = ns("progressBar"),
            value = 0,
            title = "Initiating Deconvolution",
            display_pct = TRUE
          )
        ),
        shiny$column(
          width = 2,
          align = "left",
          shiny$actionButton(
            ns("deconvolute_end"),
            "Abort",
            icon = shiny$icon("circle-stop")
          )
        ),
        shiny$column(
          width = 2,
          shiny$div(
            class = "decon-btn",
            disabled(
              shiny$actionButton(
                ns("forward_deconvolution"),
                "Next Step",
                icon = shiny$icon("forward-fast")
              )
            )
          )
        )
      ),
      shiny$hr(style = "margin: 1.5rem 0; opacity: 0.8;"),
      shiny$fluidRow(
        shiny$column(
          width = 3,
          shiny$div(
            class = "decon-btn",
            shiny$actionButton(
              ns("show_log"),
              "Show Log",
              icon = shiny$icon("code")
            )
          )
        ),
        shiny$column(
          width = 3,
          shiny$div(
            class = "decon-btn",
            disabled(
              shiny$actionButton(
                ns("deconvolution_report"),
                "Create Report",
                icon = shiny$icon("square-poll-vertical")
              )
            )
          )
        ),
        shiny$column(
          width = 3,
          align = "center",
          shiny$uiOutput(ns("result_picker_ui"))
        ),
        shiny$column(
          width = 3,
          align = "center",
          disabled(
            radioGroupButtons(
              ns("toggle_result"),
              choiceNames = c("Deconvoluted", "Raw m/z"),
              choiceValues = c(FALSE, TRUE)
            )
          )
        )
      ),
      shiny$fluidRow(
        shiny$column(2),
        shiny$column(
          width = 8,
          shiny$div(
            class = "card-custom-plate2",
            card(
              card_header(
                class = "bg-dark",
                "Spectrum"
              ),
              card_body(
                plotlyOutput(ns("spectrum"))
              )
            )
          )
        )
      )
    )

    ### Validate start button ----
    output$deconvolute_start_ui <- shiny$renderUI({
      shiny$validate(
        shiny$need(
          ((!is.null(dirs$file()) && length(dirs$file()) > 0) ||
            (!is.null(dirs$dir()) && length(dirs$dir()) > 0)),
          "Select target file(s) from the sidebar to start"
        )
      )

      shiny$validate(
        shiny$need(
          input$startz < input$endz,
          "High charge z must be greater than low charge z"
        )
      )

      shiny$validate(
        shiny$need(
          input$minmz < input$maxmz,
          "High m/z must be greater than low m/z"
        )
      )

      shiny$validate(
        shiny$need(
          input$masslb < input$massub,
          "High mass Mw must be greater than low mass Mw"
        )
      )

      shiny$validate(
        shiny$need(
          input$time_start < input$time_end,
          "Retention start time must be earlier than end time"
        )
      )

      if (dirs$selected() == "folder") {
        valid_folder <- length(dir_ls(
          dirs$dir(),
          glob = "*.raw"
        )) !=
          0

        shiny$validate(
          shiny$need(
            valid_folder,
            "No valid target folder selected"
          )
        )
      } else if (dirs$selected() == "file") {
        valid_file <- (length(dirs$file()) &&
          grepl("\\.raw$", dirs$file(), ignore.case = TRUE) &&
          dir.exists(dirs$file()))

        shiny$validate(
          shiny$need(
            valid_file,
            "No valid target file selected"
          )
        )
      }

      shiny$div(
        shiny$actionButton(ns("deconvolute_start"), "Run Deconvolution")
      )
    })

    ### Functions ----
    #### check_progress ----
    check_progress <- function(raw_dirs) {
      message("Checking progress at: ", Sys.time())
      fin_dirs <- gsub(".raw", "_rawdata_unidecfiles", raw_dirs)
      peak_files <- file.path(fin_dirs, "plots.rds")
      finished_files <- file.exists(peak_files)
      count <- sum(finished_files)
      message("Found files: ", count)

      count
    }

    #### reset_progress ----
    reset_progress <- function() {
      reactVars$isRunning <- FALSE
      reactVars$heatmap_ready <- FALSE
      reactVars$completedFiles <- 0
      reactVars$sample_names <- NULL
      reactVars$wells <- NULL
      reactVars$rslt_df <- data.frame()
      reactVars$lastCheck <- Sys.time()
      reactVars$lastCheckresults <- Sys.time()
      reactVars$heatmap_ready <- FALSE

      output$spectrum <- NULL
    }

    ### Event start deconvolution ----

    #### Confirmation modal ----

    shiny$observeEvent(input$deconvolute_start, {
      shiny$showModal(
        shiny$div(
          class = "start-modal",
          shiny$modalDialog(
            shiny$fluidRow(
              shiny$br(),
              shiny$column(
                width = 11,
                shiny$uiOutput(ns("message_ui")),
                shiny$uiOutput(ns("target_sel_ui")),
                shiny$br(),
                shiny$uiOutput(ns("warning_ui")),
                shiny$uiOutput(ns("selector_ui"))
              )
            ),
            title = "Start Deconvolution",
            easyClose = TRUE,
            footer = shiny$tagList(
              shiny$modalButton("Dismiss"),
              shiny$actionButton(
                ns("deconvolute_start_conf"),
                "Continue",
                class = "load-db",
                width = "auto"
              )
            )
          )
        )
      )
    })

    output$selector_ui <- shiny$renderUI({
      input$deconvolute_start

      select <- NULL

      if (dirs$selected() == "folder") {
        finished_files <- dir_ls(
          dirs$dir(),
          glob = "*_rawdata_unidecfiles"
        )

        if (
          isTRUE(dirs$batch_mode()) &&
            length(dirs$batch_file())
        ) {
          batch_sel <- gsub(
            ".raw",
            "_rawdata_unidecfiles",
            dirs$batch_file()[[dirs$id_column()]]
          )

          intersect <- batch_sel %in% basename(finished_files)

          if (any(intersect)) {
            select <- shiny$radioButtons(
              ns("decon_select"),
              "",
              c("Overwrite Files", "Skip Files")
            )
          }
        } else if (!is.null(input$target_selector)) {
          intersect <- gsub(
            ".raw",
            "_rawdata_unidecfiles",
            input$target_selector
          ) %in%
            basename(finished_files)

          if (any(intersect)) {
            select <- shiny$radioButtons(
              ns("decon_select"),
              "",
              c("Overwrite Files", "Skip Files")
            )
          }
        }
      }

      return(select)
    })

    output$message_ui <- shiny$renderUI({
      input$deconvolute_start
      enable(
        selector = "#app-deconvolution_process-deconvolute_start_conf"
      )
      message <- NULL

      if (dirs$selected() == "folder") {
        raw_dirs <- dir_ls(
          dirs$dir(),
          glob = "*.raw"
        )

        if (isTRUE(dirs$batch_mode()) & length(dirs$batch_file())) {
          presence <- dirs$batch_file()[[dirs$id_column()]] %in%
            basename(raw_dirs)

          if (all(presence)) {
            message <- shiny$p(
              shiny$HTML(
                paste0(
                  "<b>Multiple target file(s) selected</b><br><br><b>",
                  nrow(dirs$batch_file()),
                  "</b> raw file(s) present in ",
                  "the batch file are queried for deconvolution."
                )
              )
            )
          } else if (sum(presence) == 0) {
            disable(
              selector = "#app-deconvolution_process-deconvolute_start_conf"
            )

            message <- shiny$p(
              shiny$HTML(
                paste0(
                  "<b>Multiple target file(s) selected</b><br><br>",
                  '<i class="fa-solid fa-circle-exclamation" style="font-size:',
                  '1em; color:black; margin-right: 10px;"></i>',
                  "<i>None of the raw file(s) present in ",
                  "the batch file can be found in the root folder.</i>"
                )
              )
            )
          } else {
            message <- shiny$p(
              shiny$HTML(
                paste0(
                  "<b>Multiple target file(s) selected</b><br><br><b>",
                  sum(presence),
                  "</b> raw file(s) present in ",
                  "the batch file are queried for deconvolution.<br><br>",
                  '<i class="fa-solid fa-circle-exclamation" style="font-size:',
                  '1em; color:black; margin-right: 10px;"></i><i><b>',
                  sum(!presence),
                  "</b> of the batch raw file(s) are<b> NOT</b> prese",
                  "nt in the selected root folder.</i>"
                )
              )
            )
          }

          # if duplicates present disable continue button
          if (any(duplicated(dirs$batch_file()[[dirs$id_column()]]))) {
            disable(
              selector = "#app-deconvolution_process-deconvolute_start_conf"
            )
          }
        } else {
          if (is.null(input$target_selector)) {
            num_targets <- 0
          } else {
            num_targets <- length(input$target_selector)
          }

          if (num_targets == 0) {
            disable(
              selector = "#app-deconvolution_process-deconvolute_start_conf"
            )
          }

          message <- shiny$p(
            shiny$HTML(
              paste0(
                "<b>Multiple target file(s) selected</b><br><br><b>",
                num_targets,
                "</b> raw file(s) in the",
                " selected directory are currently queried for deconvolution.",
                " If you wish to process only a subset select the respective",
                " target files or dismiss and upload a batch file."
              )
            )
          )
        }
      } else {
        name <- basename(dirs$file())

        message <- shiny$p(
          shiny$HTML(
            paste0(
              "<b>Individual target file selected</b><br><br>",
              name,
              " is queried for deconvolution."
            )
          )
        )
      }

      return(message)
    })

    output$warning_ui <- shiny$renderUI({
      input$deconvolute_start

      warning <- NULL
      reactVars$overwrite <- FALSE

      if (dirs$selected() == "folder") {
        finished_files <- dir_ls(
          dirs$dir(),
          glob = "*_rawdata_unidecfiles"
        )

        if (
          isTRUE(dirs$batch_mode()) &&
            length(dirs$batch_file())
        ) {
          # check if any duplicated targets in batch
          if (any(duplicated(dirs$batch_file()[[dirs$id_column()]]))) {
            dup_count <- sum(duplicated(dirs$batch_file()[[dirs$id_column()]]))
            msg <- ifelse(
              dup_count > 1,
              "</b> targets are duplicated in the batch. ",
              "</b> target is duplicated in the batch. "
            )
            warning <- shiny$p(
              shiny$HTML(
                paste0(
                  '<i class="fa-solid fa-circle-exclamation" style="font-size:',
                  '1em; color:red; margin-right: 10px;"></i>',
                  "<b>",
                  dup_count,
                  msg,
                  "Modify the batch file and try again."
                )
              )
            )
          } else {
            batch_sel <- gsub(
              ".raw",
              "_rawdata_unidecfiles",
              dirs$batch_file()[[dirs$id_column()]]
            )

            intersect <- batch_sel %in% basename(finished_files)

            if (any(intersect)) {
              reactVars$overwrite <- batch_sel[intersect]

              warning <- shiny$p(
                shiny$HTML(
                  paste0(
                    '<i class="fa-solid fa-circle-exclamation" style="font-size:',
                    '1em; color:black; margin-right: 10px;"></i>',
                    "<b>",
                    sum(intersect),
                    paste0(
                      "</b> file(s) queried for deconvolution appear to have ",
                      "already been processed. Please choose how to proceed:"
                    )
                  )
                )
              )
            }
          }
        } else if (!is.null(input$target_selector)) {
          intersect <- gsub(
            ".raw",
            "_rawdata_unidecfiles",
            input$target_selector
          ) %in%
            basename(finished_files)

          if (sum(intersect) > 0) {
            reactVars$overwrite <- gsub(
              ".raw",
              "_rawdata_unidecfiles",
              input$target_selector[intersect]
            )

            warning <- shiny$p(
              shiny$HTML(
                paste0(
                  '<i class="fa-solid fa-circle-exclamation" style="font-size:',
                  '1em; color:black; margin-right: 10px;"></i>',
                  "<b>",
                  sum(intersect),
                  paste0(
                    "</b> file(s) queried for deconvolution appear to have ",
                    "already been processed. Please choose how to proceed:"
                  )
                )
              )
            )
          }
        }
      } else if (
        dir.exists(gsub(".raw", "_rawdata_unidecfiles", dirs$file()))
      ) {
        reactVars$overwrite <- gsub(".raw", "_rawdata_unidecfiles", dirs$file())
        reactVars$duplicated <- "Overwrite Files"
        warning <- shiny$p(
          shiny$HTML(
            paste0(
              '<i class="fa-solid fa-circle-exclamation" style="font-size:1e',
              'm; color:black; margin-right: 10px;"></i>',
              "The file queried for deconvolution appears to have already",
              " been processed. Choosing to continue will overwrite",
              " already the present result."
            )
          )
        )
      }

      return(warning)
    })

    output$target_sel_ui <- shiny$renderUI({
      input$deconvolute_start

      picker <- NULL

      if (
        dirs$selected() == "folder" &&
          (isFALSE(dirs$batch_mode()) || length(dirs$batch_file()) == 0)
      ) {
        picker <- pickerInput(
          ns("target_selector"),
          "",
          choices = basename(dir_ls(dirs$dir(), glob = "*.raw")),
          selected = basename(dir_ls(dirs$dir(), glob = "*.raw")),
          options = list(
            `live-search` = TRUE,
            `actions-box` = TRUE,
            size = 10,
            style = "border-color: black;"
          ),
          multiple = TRUE
        )
      }

      return(picker)
    })

    shiny$observe({
      if (!is.null(input$decon_select)) {
        reactVars$duplicated <- input$decon_select
      }
    })

    #### Deconvolution start ----
    shiny$observeEvent(input$deconvolute_start_conf, {
      # Reset modal and previous processes
      shiny$removeModal()
      reset_progress()
      write_log("Deconvolution initiated")

      if (!dir.exists(results_dir)) {
        dir.create(results_dir)
      }

      if (file.exists(file.path(results_dir, "result.rds"))) {
        file.remove(file.path(results_dir, "result.rds"))
      }
      if (file.exists(file.path(results_dir, "heatmap.rds"))) {
        file.remove(file.path(results_dir, "heatmap.rds"))
      }

      # UI changes
      runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "block";'
      ))
      runjs(paste0(
        "document.getElementById('deconvolution_process-deconvo",
        "lute_start').style.animation = 'none';"
      ))
      delay(
        500,
        runjs(paste0(
          "document.querySelector('.bslib-sidebar-layout.sidebar-coll",
          "apsed>.collapse-toggle').style.display = 'none';"
        ))
      )

      ##### Deconvolution init and mode ----
      if (dirs$selected() == "folder") {
        raw_dirs <- list.dirs(
          dirs$dir(),
          full.names = TRUE,
          recursive = FALSE
        )
        raw_dirs <- raw_dirs[grep("\\.raw$", raw_dirs)]

        if (
          isTRUE(dirs$batch_mode()) &&
            length(dirs$batch_file())
        ) {
          write_log("Multiple target deconvolution mode (with batch file)")

          batch <- dirs$batch_file()
          sample_names <- batch[[dirs$id_column()]]
          raw_dirs <- raw_dirs[basename(raw_dirs) %in% sample_names]

          # Prepare heatmap variables
          reactVars$sample_names <- gsub(
            ".raw",
            "",
            dirs$batch_file()[[dirs$id_column()]]
          )
          reactVars$wells <- gsub(
            ",",
            "",
            sub("^.*:", "", batch[[dirs$vial_column()]])
          )
        } else {
          write_log("Multiple target deconvolution mode (no batch file)")

          raw_dirs <- raw_dirs[basename(raw_dirs) %in% target_selector_sel()]
        }

        write_log(paste(
          length(raw_dirs),
          "targets. Directory:",
          dirname(raw_dirs[1])
        ))
      } else if (dirs$selected() == "file") {
        write_log("Single target deconvolution mode")
        raw_dirs <- dirs$file()
        write_log(paste("Target:", dirs$file()))
      }

      # Overwrite or skip already present result dirs
      if (!isFALSE(reactVars$overwrite)) {
        if (reactVars$duplicated == "Overwrite Files") {
          # Remove result files and dirs
          if (dirs$selected() == "file") {
            rslt_dirs <- gsub(".raw", "_rawdata_unidecfiles", dirs$file())

            write_log(paste("Overwriting existing", rslt_dirs))
          } else {
            rslt_dirs <- file.path(
              dirs$dir(),
              reactVars$overwrite
            )

            write_log(paste(
              "Overwriting",
              length(rslt_dirs),
              "existing result file(s)"
            ))
          }

          rslt_dirs <- rslt_dirs[dir.exists(rslt_dirs)]
          unlink(rslt_dirs, recursive = TRUE)

          txt_files <- gsub(
            "_rawdata_unidecfiles",
            "_rawdata.txt",
            rslt_dirs
          )
          txt_files <- txt_files[file.exists(txt_files)]
          file.remove(txt_files)
        } else if (reactVars$duplicated == "Skip Files") {
          raw_dirs <- raw_dirs[
            !basename(raw_dirs) %in%
              gsub("_rawdata_unidecfiles", ".raw", reactVars$overwrite)
          ]

          write_log(paste(
            "Skipping",
            length(raw_dirs),
            "existing result file(s)"
          ))
        }
      }

      # Render disabled results picker
      output$result_picker_ui <- shiny$renderUI(
        shiny$div(
          class = "result-picker",
          disabled(shiny$selectInput(ns("result_picker"), "", choices = ""))
        )
      )

      # Apply JS modifications for picker
      session$sendCustomMessage("selectize-init", "result_picker")

      reactVars$trigger <- TRUE

      # Initialization variables
      reactVars$isRunning <- TRUE
      reactVars$expectedFiles <- length(raw_dirs)
      reactVars$initialFileCount <- check_progress(raw_dirs)
      message("Initial file count: ", reactVars$initialFileCount)

      #### Start computation ----

      # save config parameter
      config <- list(
        params = data.frame(
          startz = input$startz,
          endz = input$endz,
          minmz = input$minmz,
          maxmz = input$maxmz,
          masslb = input$masslb,
          massub = input$massub,
          massbins = input$massbins,
          peakthresh = input$peakthresh,
          peakwindow = input$peakwindow,
          peaknorm = input$peaknorm,
          time_start = input$time_start,
          time_end = input$time_end
        ),
        dirs = raw_dirs,
        selected = dirs$selected()
      )

      temp <- tempdir()
      config_path <- file.path(temp, "config.rds")
      saveRDS(config, config_path)

      reactVars$decon_process_out <- file.path(temp, "output.txt")
      write("", reactVars$decon_process_out)

      rx_process <- process$new(
        "Rscript.exe",
        args = c(
          "app/logic/deconvolution_execute.R",
          temp,
          log_path,
          getwd()
        ),
        stdout = reactVars$decon_process_out,
        stderr = reactVars$decon_process_out
      )

      decon_process_data(rx_process)

      # Log
      write_log("Deconvolution started")
      formatted_params <- apply(config$params, 1, function(row) {
        paste(names(config$params), row, sep = " = ", collapse = " | ")
      })
      write_log(paste(
        "Deconvolution parameters:\n",
        paste(formatted_params, collapse = "\n")
      ))

      reactVars$process_observer <- shiny$observe({
        proc <- decon_process_data()
        completed_files <- reactVars$completedFiles
        expected_files <- reactVars$expectedFiles

        session$onSessionEnded(function() {
          if (!is.null(proc) && proc$is_alive()) {
            write_log(paste(
              "Deconvolution cancelled with",
              completed_files,
              "out of",
              expected_files,
              "target(s) completed"
            ))

            proc$kill_tree()
          }
        })
      })

      #### Results tracking observer for heatmap ----
      if (dirs$selected() == "folder") {
        reactVars$results_observer <- shiny$observe({
          shiny$invalidateLater(10000)

          runjs(paste0(
            'document.getElementById("blocking-overlay").style.display ',
            '= "block";'
          ))

          if (
            difftime(
              Sys.time(),
              reactVars$lastCheckresults,
              units = "secs"
            ) >=
              10
          ) {
            if (
              isTRUE(dirs$batch_mode()) &&
                length(dirs$batch_file()) &&
                nrow(reactVars$rslt_df) < reactVars$completedFiles
            ) {
              shiny$req(reactVars$sample_names, reactVars$wells)

              results_all <- dir_ls(
                dirs$dir(),
                glob = "*_rawdata_unidecfiles"
              )

              results <- results_all[
                basename(results_all) %in%
                  paste0(
                    reactVars$sample_names,
                    "_rawdata_unidecfiles"
                  )
              ]

              if (length(results_all)) {
                if (is.null(reactVars$rslt_df) || nrow(reactVars$rslt_df) < 1) {
                  well <- character()
                  value <- numeric()
                  sample_names <- character()
                  for (i in seq_along(results)) {
                    sample_names[i] <- gsub(
                      "_rawdata_unidecfiles",
                      "",
                      basename(results[i])
                    )
                    well[i] <- reactVars$wells[which(
                      reactVars$sample_names == sample_names[i]
                    )]
                    peak_file <- file.path(
                      results[i],
                      paste0(sample_names[i], "_rawdata_peaks.dat")
                    )
                    if (file.exists(peak_file)) {
                      peaks <- utils::read.delim(
                        peak_file,
                        header = FALSE,
                        sep = " "
                      )
                      max <- max(peaks$V1)
                      if (length(max) && !is.na(max)) {
                        value[i] <- max
                      } else {
                        value[i] <- NA
                      }
                    } else {
                      value[i] <- NA
                    }
                  }
                  rslt_df <- data.frame(
                    sample = sample_names,
                    well_id = well,
                    value = value
                  )
                  reactVars$rslt_df <- rslt_df[
                    !as.logical(
                      rowSums(is.na(rslt_df))
                    ),
                  ]
                } else {
                  new <- !gsub(
                    "_rawdata_unidecfiles",
                    "",
                    basename(results)
                  ) %in%
                    reactVars$rslt_df$sample
                  new_results <- results[new]

                  if (length(new_results)) {
                    well <- character()
                    value <- numeric()
                    sample_names <- character()
                    for (i in seq_along(new_results)) {
                      sample_names[i] <- gsub(
                        "_rawdata_unidecfiles",
                        "",
                        basename(new_results[i])
                      )
                      well[i] <- reactVars$wells[which(
                        reactVars$sample_names == sample_names[i]
                      )]

                      peak_file <- file.path(
                        new_results[i],
                        paste0(sample_names[i], "_rawdata_peaks.dat")
                      )

                      if (file.exists(peak_file)) {
                        get_peaks <- tryCatch(
                          {
                            peaks <- utils::read.delim(
                              peak_file,
                              header = FALSE,
                              sep = " "
                            )
                          },
                          error = function(e) {
                            NULL
                          }
                        )

                        if (is.null(peaks)) {
                          value[i] <- NA
                          next
                        }

                        max <- max(peaks$V1)
                        if (length(max) && !is.na(max)) {
                          value[i] <- max
                        } else {
                          value[i] <- NA
                        }
                      } else {
                        value[i] <- NA
                      }
                    }

                    new_rslt_df <- data.frame(
                      sample = sample_names,
                      well_id = well,
                      value = value
                    )
                    new_rslt_df <- new_rslt_df[
                      !as.logical(
                        rowSums(is.na(new_rslt_df))
                      ),
                    ]

                    reactVars$rslt_df <- rbind(reactVars$rslt_df, new_rslt_df)
                  }
                }

                ##### Render heatmap & result picker ----
                if (nrow(reactVars$rslt_df) > 0) {
                  enable(selector = "#app-deconvolution_process-toggle_result")

                  # Render results picker
                  output$result_picker_ui <- shiny$renderUI(
                    shiny$div(
                      class = "result-picker",
                      shiny$selectInput(
                        ns("result_picker"),
                        "",
                        choices = gsub(
                          "_rawdata_unidecfiles",
                          ".raw",
                          basename(results)
                        ),
                        selected = result_files_sel()
                      )
                    )
                  )
                  # Apply JS modifications for picker
                  session$sendCustomMessage("selectize-init", "result_picker")

                  output$heatmap <- renderPlotly({
                    waiter_show(
                      id = ns("heatmap"),
                      html = spin_wandering_cubes()
                    )
                    heatmap <- create_384_plate_heatmap(reactVars$rslt_df) |>
                      event_register("plotly_click")
                    waiter_hide(id = ns("heatmap"))
                    heatmap
                  })

                  reactVars$heatmap_ready <- TRUE
                  reactVars$trigger <- TRUE
                }
              }
            } else {
              selected_files <- file.path(dirs$dir(), target_selector_sel())
              fin_dirs <- gsub(".raw", "_rawdata_unidecfiles", selected_files)
              peak_files <- file.path(fin_dirs, "plots.rds")
              finished_files <- file.exists(peak_files)

              if (sum(finished_files) > 0) {
                choices <- basename(selected_files)[finished_files]
                selected <- ifelse(
                  is.null(result_files_sel()),
                  choices[1],
                  result_files_sel()
                )

                enable(selector = "#app-deconvolution_process-toggle_result")

                output$result_picker_ui <- shiny$renderUI(
                  shiny$div(
                    class = "result-picker",
                    shiny$selectInput(
                      ns("result_picker"),
                      "",
                      choices = choices,
                      selected = selected
                    )
                  )
                )
                # Apply JS modifications for picker
                session$sendCustomMessage("selectize-init", "result_picker")
              }

              count <- sum(finished_files)
              message("Found files: ", count)
              count
            }

            reactVars$lastCheckresults <- Sys.time()
          }

          runjs(paste0(
            'document.getElementById("blocking-overlay").style.display ',
            '= "none";'
          ))
        })
      }

      #### Progress tracking observer ----
      reactVars$progress_observer <- shiny$observe({
        shiny$req(reactVars$isRunning)
        shiny$invalidateLater(1000)

        if (difftime(Sys.time(), reactVars$lastCheck, units = "secs") >= 0.5) {
          reactVars$current_total_files <- check_progress(raw_dirs)
          reactVars$completedFiles <-
            reactVars$current_total_files - reactVars$initialFileCount
          reactVars$lastCheck <- Sys.time()

          progress_pct <- min(
            100,
            round(
              100 * reactVars$completedFiles / reactVars$expectedFiles
            )
          )

          message(
            "Updating progress: ",
            progress_pct,
            "% (",
            reactVars$completedFiles,
            "/",
            reactVars$expectedFiles,
            ")"
          )

          if (reactVars$count < 3) {
            reactVars$count <- reactVars$count + 1
          } else {
            reactVars$count <- 0
          }

          if (reactVars$current_total_files == 0) {
            title <- paste0(
              "Initializing ",
              paste0(rep(".", reactVars$count), collapse = "")
            )
          } else if (progress_pct != 100) {
            title <- paste0(
              sprintf(
                "Processing Files (%d/%d) ",
                reactVars$completedFiles,
                reactVars$expectedFiles
              ),
              paste0(rep(".", reactVars$count), collapse = "")
            )
          } else {
            title <- paste0(
              "Saving Results ",
              paste0(rep(".", reactVars$count), collapse = "")
            )

            result_files <- gsub(".raw", "_rawdata_unidecfiles", raw_dirs)

            # check if deconvolution finished for all target files
            if (
              all(file.exists(file.path(result_files, "plots.rds"))) &&
                file.exists(file.path(results_dir, "result.rds"))
            ) {
              # stop observers
              if (!is.null(reactVars$progress_observer)) {
                reactVars$progress_observer$destroy()
              }
              if (!is.null(reactVars$process_observer)) {
                reactVars$process_observer$destroy()
              }
              if (
                dirs$selected() == "folder" &&
                  !is.null(reactVars$results_observer)
              ) {
                reactVars$results_observer$destroy()
              }

              reactVars$isRunning <- FALSE

              # final result check for heatmap update
              if (
                dirs$selected() == "folder" &&
                  isTRUE(dirs$batch_mode()) &&
                  length(dirs$batch_file())
              ) {
                results <- result_files[
                  basename(result_files) %in%
                    paste0(
                      reactVars$sample_names,
                      "_rawdata_unidecfiles"
                    )
                ]

                new <- !gsub("_rawdata_unidecfiles", "", basename(results)) %in%
                  reactVars$rslt_df$sample
                new_results <- results[new]

                if (length(new_results)) {
                  well <- character()
                  value <- numeric()
                  sample_names <- character()
                  for (i in seq_along(new_results)) {
                    sample_names[i] <- gsub(
                      "_rawdata_unidecfiles",
                      "",
                      basename(new_results[i])
                    )
                    well[i] <- reactVars$wells[which(
                      reactVars$sample_names == sample_names[i]
                    )]

                    peak_file <- file.path(
                      new_results[i],
                      paste0(sample_names[i], "_rawdata_peaks.dat")
                    )

                    if (file.exists(peak_file)) {
                      get_peaks <- tryCatch(
                        {
                          peaks <- utils::read.delim(
                            peak_file,
                            header = FALSE,
                            sep = " "
                          )
                        },
                        error = function(e) {
                          NULL
                        }
                      )

                      if (is.null(peaks)) {
                        value[i] <- NA
                        next
                      }

                      max <- max(peaks$V1)
                      if (length(max) && !is.na(max)) {
                        value[i] <- max
                      } else {
                        value[i] <- NA
                      }
                    } else {
                      value[i] <- NA
                    }
                  }

                  new_rslt_df <- data.frame(
                    sample = sample_names,
                    well_id = well,
                    value = value
                  )
                  new_rslt_df <- new_rslt_df[
                    !as.logical(
                      rowSums(is.na(new_rslt_df))
                    ),
                  ]

                  reactVars$rslt_df <- rbind(reactVars$rslt_df, new_rslt_df)
                }

                # Save heatmap
                if (!file.exists("results/heatmap.rds")) {
                  heatmap <- create_384_plate_heatmap(reactVars$rslt_df)

                  saveRDS(heatmap, file.path(results_dir, "heatmap.rds"))
                }
              } else {
                if (dirs$selected() == "folder") {
                  selected_files <- file.path(dirs$dir(), target_selector_sel())
                } else {
                  selected_files <- raw_dirs
                }

                fin_dirs <- gsub(".raw", "_rawdata_unidecfiles", selected_files)
                peak_files <- file.path(fin_dirs, "plots.rds")
                finished_files <- file.exists(peak_files)

                if (sum(finished_files) > 0) {
                  choices <- basename(selected_files)[finished_files]

                  selected <- ifelse(
                    is.null(result_files_sel()),
                    choices[1],
                    result_files_sel()
                  )

                  enable(selector = "#app-deconvolution_process-toggle_result")

                  output$result_picker_ui <- shiny$renderUI(
                    shiny$div(
                      class = "result-picker",
                      shiny$selectInput(
                        ns("result_picker"),
                        "",
                        choices = choices,
                        selected = selected
                      )
                    )
                  )
                  # Apply JS modifications for picker
                  session$sendCustomMessage("selectize-init", "result_picker")
                }
              }

              shiny$updateActionButton(
                session,
                "deconvolute_end",
                label = "Reset",
                icon = shiny$icon("repeat")
              )

              title <- "Finalized!"

              write_log("Deconvolution finalized")

              enable(
                selector = "#app-deconvolution_process-deconvolution_report"
              )
              hide(selector = "#app-deconvolution_process-processing")
              show(selector = "#app-deconvolution_process-processing_fin")
            }
          }

          updateProgressBar(
            session = session,
            id = ns("progressBar"),
            value = progress_pct,
            title = title
          )
        }
      })

      #### Heatmap click observer ----
      if (
        dirs$selected() == "folder" &&
          isTRUE(dirs$batch_mode()) &&
          length(dirs$batch_file())
      ) {
        reactVars$click_observer <- shiny$observe({
          if (isTRUE(reactVars$heatmap_ready)) {
            click_data <- event_data("plotly_click")
            if (!is.null(click_data)) {
              # Get the clicked point's row and column
              row <- LETTERS[16 - floor(click_data$y) + 1]
              col <- round(click_data$x)
              well_id <- paste0(row, col)

              # Find the corresponding sample in the data
              shiny$isolate(
                clicked_sample <-
                  reactVars$rslt_df$sample[reactVars$rslt_df$well_id == well_id]
              )

              result_files_sel(paste0(clicked_sample, ".raw"))
            }
          }
        })
      }

      #### Switch to running UI ----
      runjs("document.querySelector('button.collapse-toggle').click();")
      output$deconvolution_init_ui <- NULL

      output$deconvolution_running_ui <- shiny$renderUI({
        if (
          dirs$selected() == "folder" &&
            isTRUE(dirs$batch_mode()) &&
            length(dirs$batch_file())
        ) {
          deconvolution_running_ui_plate
        } else {
          deconvolution_running_ui_noplate
        }
      })

      delay(1000, show(selector = "#app-deconvolution_process-processing"))

      ### Render result spectrum ----
      output$spectrum <- renderPlotly({
        waiter_show(id = ns("spectrum"), html = spin_wandering_cubes())

        shiny$req(result_files_sel(), input$toggle_result)

        reactVars$trigger

        if (dirs$selected() == "folder") {
          result_dir <- file.path(
            dirs$dir(),
            gsub(".raw", "_rawdata_unidecfiles", result_files_sel())
          )
        } else if (dirs$selected() == "file") {
          result_dir <- gsub(".raw", "_rawdata_unidecfiles", dirs$file())
        }

        if (dir.exists(result_dir)) {
          # Generate the spectrum plot
          spectrum <- spectrum_plot(result_dir, input$toggle_result)

          waiter_hide(id = ns("spectrum"))

          return(spectrum)
        }
      })

      runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "none";'
      ))
    })

    ### Event end/reset deconvolution ----
    shiny$observeEvent(input$deconvolute_end, {
      if (reactVars$isRunning) {
        shiny$showModal(
          shiny$div(
            class = "start-modal",
            shiny$modalDialog(
              shiny$fluidRow(
                shiny$br(),
                shiny$column(
                  width = 11,
                  shiny$p(
                    shiny$HTML(
                      "Are you sure you want to cancel the deconvolution?"
                    )
                  )
                ),
                shiny$br()
              ),
              title = "Abort Deconvolution",
              easyClose = TRUE,
              footer = shiny$tagList(
                shiny$modalButton("Dismiss"),
                shiny$actionButton(
                  ns("deconvolute_end_conf"),
                  "Abort",
                  class = "load-db",
                  width = "auto"
                )
              )
            )
          )
        )
      } else {
        runjs(paste0(
          'document.getElementById("blocking-overlay").styl',
          'e.display = "block";'
        ))

        hide(selector = "#app-deconvolution_process-processing")
        hide(selector = "#app-deconvolution_process-processing_stop")
        hide(selector = "#app-deconvolution_process-processing_fin")

        # stop observers
        if (!is.null(reactVars$progress_observer)) {
          reactVars$progress_observer$destroy()
        }
        if (!is.null(reactVars$process_observer)) {
          reactVars$process_observer$destroy()
        }
        if (
          dirs$selected() == "folder" &&
            !is.null(reactVars$results_observer)
        ) {
          reactVars$results_observer$destroy()
        }
        reset_progress()

        write_log("Deconvolution resetted")

        output$decon_rep_logtext <- NULL
        output$decon_rep_logtext_ui <- NULL
        reactVars$deconv_report_status <- "idle"

        output$deconvolution_running_ui <- NULL
        output$heatmap <- NULL

        output$deconvolution_init_ui <- shiny$renderUI(
          deconvolution_init_ui
        )

        runjs(paste0(
          'document.getElementById("blocking-overlay").styl',
          'e.display = "none";'
        ))
        runjs(paste0(
          "document.querySelector('.bslib-sidebar-layout.sidebar-coll",
          "apsed>.collapse-toggle').style.display = 'block';"
        ))
        runjs("document.querySelector('button.collapse-toggle').click();")
      }
    })

    shiny$observeEvent(input$deconvolute_end_conf, {
      proc <- decon_process_data()
      if (!is.null(proc) && proc$is_alive()) {
        proc$kill_tree()
      }

      write_log(paste(
        "Deconvolution cancelled with",
        reactVars$completedFiles,
        "out of",
        reactVars$expectedFiles,
        "target(s) completed"
      ))

      updateProgressBar(
        session = session,
        id = ns("progressBar"),
        value = 0,
        title = "Processing aborted"
      )

      hide(selector = "#app-deconvolution_process-processing")
      show(selector = "#app-deconvolution_process-processing_stop")

      shiny$updateActionButton(
        session,
        "deconvolute_end",
        label = "Reset",
        icon = shiny$icon("repeat")
      )

      if (!is.null(reactVars$progress_observer)) {
        reactVars$progress_observer$destroy()
      }
      if (dirs$selected() == "folder" && !is.null(reactVars$results_observer)) {
        reactVars$results_observer$destroy()
      }
      if (!is.null(reactVars$process_observer)) {
        reactVars$process_observer$destroy()
      }
      reactVars$isRunning <- FALSE

      shiny$removeModal()
    })

    ### Logging events  ----

    #### Show Log ----
    shiny$observeEvent(input$show_log, {
      output$logtext <- shiny$renderText({
        shiny$invalidateLater(2000)

        if (
          !is.null(reactVars$decon_process_out) &&
            file.exists(reactVars$decon_process_out)
        ) {
          reactVars$deconvolution_log <- paste(
            readLines(reactVars$decon_process_out, warn = FALSE),
            collapse = "\n"
          )
        } else {
          reactVars$deconvolution_log <- "Log file not found."
        }

        reactVars$deconvolution_log
      })

      shiny$showModal(
        shiny$div(
          class = "start-modal",
          shiny$modalDialog(
            shiny$fluidRow(
              shiny$br(),
              shiny$column(
                width = 12,
                shiny$verbatimTextOutput(ns("logtext"))
              )
            ),
            title = "Deconvolution Output",
            easyClose = TRUE,
            footer = shiny$tagList(
              shiny$div(
                class = "modal-button",
                shiny$modalButton("Dismiss")
              ),
              shiny$div(
                class = "modal-button",
                shiny$actionButton(
                  ns("copy_deconvolution_log"),
                  "Clip",
                  icon = shiny$icon("clipboard")
                )
              ),
              shiny$div(
                class = "modal-button",
                shiny$downloadButton(
                  ns("save_deconvolution_log"),
                  "Save",
                  class = "load-db",
                  width = "auto"
                )
              )
            )
          )
        )
      )

      delay(2000, runjs("App.smartScroll('deconvolution_process-logtext')"))
    })

    #### Save log ----
    output$save_deconvolution_log <- shiny$downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_Deconvolution_Log.txt")
      },
      content = function(file) {
        file.copy(reactVars$decon_process_out, file)
      }
    )

    #### Clip log ----
    shiny$observeEvent(input$copy_deconvolution_log, {
      shiny$req(reactVars$deconvolution_log)

      write_clip(reactVars$deconvolution_log, allow_non_interactive = TRUE)
    })

    ### Report events ----

    shiny$observeEvent(input$deconvolution_report, {
      if (reactVars$deconv_report_status == "running") {
        label <- "Cancel"
      } else if (reactVars$deconv_report_status == "finished") {
        label <- "Open"
      } else {
        label <- "Make Report"
      }

      shiny$showModal(
        shiny$div(
          class = "decon-report-modal",
          shiny$modalDialog(
            shiny$column(
              width = 12,
              shiny$uiOutput(ns("decon_report_ui")),
              shiny$fluidRow(
                shiny$column(
                  width = 12,
                  shiny$uiOutput(ns("decon_rep_logtext_ui"))
                )
              )
            ),
            title = "Deconvolution Report",
            easyClose = FALSE,
            footer = shiny$tagList(
              shiny$modalButton("Dismiss"),
              shiny$actionButton(
                ns("make_deconvolution_report"),
                label = label,
                class = "load-db",
                width = "auto"
              )
            )
          )
        )
      )

      delay(
        2000,
        runjs("App.smartScroll('deconvolution_process-decon_rep_logtext')")
      )
    })

    decon_rep_process_data <- shiny$reactiveVal(NULL)

    shiny$observeEvent(
      input$make_deconvolution_report,
      {
        output$decon_rep_logtext_ui <- shiny$renderUI(
          shiny$verbatimTextOutput(ns("decon_rep_logtext"))
        )

        output$decon_rep_logtext <- shiny$renderText({
          shiny$invalidateLater(1000)

          if (
            !is.null(reactVars$decon_rep_process_out) &&
              file.exists(reactVars$decon_rep_process_out)
          ) {
            log <- paste(
              readLines(reactVars$decon_rep_process_out, warn = FALSE),
              collapse = "\n"
            )
          } else {
            log <- "Initiating Report Generation ..."
          }

          session_id <- regmatches(
            basename(log_path),
            regexpr("id\\d+", basename(log_path))
          )
          report_fin <- paste0(
            "deconvolution_report_",
            Sys.Date(),
            "_",
            session_id,
            ".html"
          )

          clean_log <- gsub("\\s*\\|[ .]*\\|\\s*", "", log, perl = TRUE)

          filename <- gsub(
            ".log",
            "_deconvolution_report.html",
            basename(log_path)
          )
          filename_path <- file.path(dirname(log_path), filename)

          if (
            grepl(paste("Output created:", report_fin), log) &
              file.exists(filename_path)
          ) {
            reactVars$deconv_report_status <- "finished"
            title <- "Report Generated!"
            value <- 100
          } else {
            value <- regmatches(
              clean_log,
              gregexpr("(?<=\\|)\\d+%", clean_log, perl = TRUE)
            )[[1]]
            title <- "Generating Report ..."
          }

          updateProgressBar(
            session = session,
            id = ns("progressBar"),
            value = tail(as.integer(gsub("%", "", value)), 1),
            title = title
          )

          clean_log
        })

        if (reactVars$deconv_report_status == "running") {
          proc <- decon_rep_process_data()

          if (!is.null(proc) && proc$is_alive()) {
            write_log("Deconvolution report generation cancelled")

            proc$kill_tree()
          }

          updateProgressBar(
            session = session,
            id = ns("progressBar"),
            value = 0,
            title = "Report Generation Aborted"
          )

          output$decon_rep_logtext <- NULL
          output$decon_rep_logtext_ui <- NULL

          hide(selector = "#app-deconvolution_process-processing")
          show(selector = "#app-deconvolution_process-processing_stop")

          reactVars$deconv_report_status <- "idle"

          shiny$removeModal()
        } else if (reactVars$deconv_report_status == "finished") {
          # Open in browser
          filename <- gsub(
            ".log",
            "_deconvolution_report.html",
            basename(log_path)
          )
          filename_path <- file.path(dirname(log_path), filename)

          if (file.exists(filename_path)) {
            utils::browseURL(filename_path)
          }

          shiny$removeModal()
        } else {
          write_log("Deconvolution report generation initiated")
          reactVars$deconv_report_status <- "running"

          reactVars$decon_rep_process_out <- file.path(
            tempdir(),
            "rep_output.txt"
          )
          write("", reactVars$decon_rep_process_out)

          output$decon_report_ui <- shiny$renderUI(
            shiny$fluidRow(
              shiny$br(),
              shiny$fluidRow(
                shiny$column(
                  width = 2,
                  shiny$div(
                    id = ns("generating_report"),
                    shiny$HTML(
                      paste0(
                        '<i class="fa fa-spinner fa-spin fa-fw fa-2x" style="color: ',
                        '#38387C; margin-top: 0.25em"></i>'
                      )
                    )
                  )
                ),
                shiny$column(
                  width = 10,
                  shiny$p(
                    "Generating this report might take some time. Please wait ..."
                  )
                )
              )
            )
          )

          if (isTRUE(input$decon_save)) {
            # Set up settings directory
            if (!dir.exists(settings_dir)) {
              dir.create(settings_dir, recursive = TRUE)
            }

            # Save report input settings
            rep_input <- c(
              input$decon_rep_title,
              input$decon_rep_author,
              input$decon_rep_desc
            )
            saveRDS(
              rep_input,
              file.path(settings_dir, "decon_rep_settings.rds")
            )
          }

          session_id <- regmatches(
            basename(log_path),
            regexpr("id\\d+", basename(log_path))
          )

          # Prepare arguments
          script_dir <- file.path(
            Sys.getenv("USERPROFILE"),
            "Documents",
            "KiwiFlow",
            "report"
          )
          script <- "deconvolution_report.R"
          output_file <- paste0(
            "deconvolution_report_",
            Sys.Date(),
            "_",
            session_id,
            ".html"
          )

          args <- c(
            script,
            fill_empty(input$decon_rep_title),
            fill_empty(input$decon_rep_author),
            fill_empty(input$decon_rep_desc),
            output_file,
            log_path
          )

          # Construct the command for Windows
          cmd <- paste(
            "conda activate kiwiflow &&",
            "cd",
            script_dir,
            "&& Rscript",
            paste(args, collapse = " ")
          )

          # Start the process
          rep_process <- process$new(
            command = "cmd.exe",
            args = c("/c", cmd),
            stdout = reactVars$decon_rep_process_out,
            stderr = reactVars$decon_rep_process_out
          )

          decon_rep_process_data(rep_process)
        }

        delay(
          2000,
          runjs(
            "App.smartScroll('deconvolution_process-decon_rep_logtext')"
          )
        )
      }
    )

    shiny$observe({
      if (reactVars$deconv_report_status == "idle") {
        output$decon_report_ui <- shiny$renderUI({
          if (file.exists(file.path(settings_dir, "decon_rep_settings.rds"))) {
            rep_input <- readRDS(file.path(
              settings_dir,
              "decon_rep_settings.rds"
            ))
            title <- rep_input[1]
            author <- rep_input[2]
            comment <- rep_input[3]
            label <- "Overwrite previous report settings?"
          } else {
            title <- "Deconvolution Report"
            author <- "Author"
            comment <- ""
            label <- "Save report settings for the next time?"
          }

          shiny$fluidRow(
            shiny$br(),
            shiny$column(
              width = 11,
              shiny$div(
                class = "deconv-rep-element",
                shiny$textInput(
                  ns("decon_rep_title"),
                  "Title",
                  value = title
                )
              ),
              shiny$div(
                class = "deconv-rep-element",
                shiny$textInput(
                  ns("decon_rep_author"),
                  "Author",
                  value = author
                )
              ),
              shiny$div(
                class = "deconv-rep-element",
                shiny$textAreaInput(
                  ns("decon_rep_desc"),
                  "Description",
                  value = comment,
                  placeholder = "Description and comments about the experiment..."
                )
              ),
              shiny$div(
                class = "deconv-rep-check",
                shiny$checkboxInput(
                  ns("decon_save"),
                  label,
                  value = FALSE
                )
              )
            )
          )
        })
      } else if (reactVars$deconv_report_status == "finished") {
        output$decon_report_ui <- shiny$renderUI(
          shiny$fluidRow(
            shiny$br(),
            shiny$fluidRow(
              shiny$column(
                width = 2,
                shiny$div(
                  id = ns("generating_report"),
                  shiny$HTML(
                    paste0(
                      '<i class="fa-solid fa-circle-check fa-2x" style="color:',
                      '#38387C; margin-top: 0.5em"></i>'
                    )
                  )
                )
              ),
              shiny$column(
                width = 10,
                shiny$p(
                  "Report successfully generated!",
                  style = "margin-top: 1.3em;"
                )
              )
            )
          )
        )
      }
    })

    shiny$observe({
      if (reactVars$deconv_report_status == "running") {
        hide(selector = "#app-deconvolution_process-processing_stop")
        hide(selector = "#app-deconvolution_process-processing_fin")
        show(selector = "#app-deconvolution_process-processing")

        runjs("App.disableDismiss()")

        shiny$updateActionButton(
          session,
          "make_deconvolution_report",
          label = "Cancel"
        )
      } else if (reactVars$deconv_report_status == "finished") {
        write_log("Deconvolution report generation finalized")

        hide(selector = "#app-deconvolution_process-processing")
        show(selector = "#app-deconvolution_process-processing_fin")

        runjs("App.enableDismiss()")

        shiny$updateActionButton(
          session,
          "make_deconvolution_report",
          label = "Open"
        )
      }
    })
  })
}
