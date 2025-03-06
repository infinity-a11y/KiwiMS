# app/view/deconvolution_process.R

box::use(
  bslib[card, card_body, card_header],
  fs[dir_ls],
  parallel[detectCores, makeCluster],
  plotly[event_data, event_register, plotlyOutput, renderPlotly],
  processx[process],
  shiny,
  shinyjs[delay, disable, disabled, enable, hide, show, hidden, runjs],
  shinyWidgets[
    addSpinner,
    radioGroupButtons,
    pickerInput,
    progressBar,
    updateProgressBar
  ],
  waiter[useWaiter, spin_wandering_cubes, waiter_show, waiter_hide, withWaiter],
)

box::use(
  app /
    logic /
    deconvolution_functions[
      deconvolute,
      create_384_plate_heatmap,
      spectrum_plot
    ],
  app / logic / helper_functions[collapsiblePanelUI],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$fluidRow(
    shiny$column(
      width = 12,
      shiny$uiOutput(ns("deconvolution_init_ui")),
      shiny$uiOutput(ns("deconvolution_running_ui"))
    )
  )
}

#' @export
server <- function(id, dirs) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    process_data <- shiny$reactiveVal(NULL)

    ### Deconvolution initiation interface ----

    deconvolution_init_ui <- shiny$column(
      width = 12,
      shiny$fluidRow(
        shiny$column(
          width = 8,
          shiny$fluidRow(
            shiny$column(
              width = 6,
              shiny$div(
                class = "card-custom",
                card(
                  card_header(
                    class = "bg-dark",
                    "Charge state [z]"
                  ),
                  card_body(
                    shiny$fluidRow(
                      shiny$column(
                        width = 3,
                        shiny$h6("Low", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 3,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("startz"),
                            "",
                            min = 0,
                            max = 100,
                            value = 1,
                            width = "130%"
                          )
                        )
                      ),
                      shiny$column(
                        width = 3,
                        shiny$h6("High", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 3,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("endz"),
                            "",
                            min = 0,
                            max = 100,
                            value = 50,
                            width = "130%"
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
                    "Spectrum range [m/z]"
                  ),
                  card_body(
                    shiny$fluidRow(
                      shiny$column(
                        width = 3,
                        shiny$h6("Low", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 3,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("minmz"),
                            "",
                            min = 0,
                            max = 100000,
                            value = 710,
                            width = "130%"
                          )
                        )
                      ),
                      shiny$column(
                        width = 3,
                        shiny$h6("High", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 3,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("maxmz"),
                            "",
                            min = 0,
                            max = 100000,
                            value = 1100,
                            width = "130%"
                          )
                        )
                      )
                    )
                  )
                )
              )
            ),
            shiny$column(
              width = 6,
              shiny$div(
                class = "card-custom",
                card(
                  card_header(
                    class = "bg-dark",
                    "Mass range [Mw]"
                  ),
                  card_body(
                    shiny$fluidRow(
                      shiny$column(
                        width = 3,
                        shiny$h6("Low", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 3,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("masslb"),
                            "",
                            min = 0,
                            max = 100000,
                            value = 35000,
                            width = "130%"
                          )
                        )
                      ),
                      shiny$column(
                        width = 3,
                        shiny$h6("High", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 3,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("massub"),
                            "",
                            min = 0,
                            max = 100000,
                            value = 42000,
                            width = "130%"
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
                    "Retention time [min]"
                  ),
                  card_body(
                    shiny$fluidRow(
                      shiny$column(
                        width = 3,
                        shiny$h6("Start", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 3,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("time_start"),
                            "",
                            min = 0,
                            max = 100,
                            value = 1,
                            width = "130%",
                            step = 0.05
                          )
                        )
                      ),
                      shiny$column(
                        width = 3,
                        shiny$h6("End", style = "margin-top: 8px;")
                      ),
                      shiny$column(
                        width = 3,
                        shiny$div(
                          class = "deconv-param-input",
                          shiny$numericInput(
                            ns("time_end"),
                            "",
                            min = 0,
                            max = 100,
                            value = 1.35,
                            width = "130%",
                            step = 0.05
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          shiny$fluidRow(
            shiny$column(
              width = 12,
              collapsiblePanelUI(
                "advanced_params",
                "Advanced Parameter Settings",
                shiny$fluidRow(
                  shiny$column(
                    width = 7,
                    shiny$div(
                      class = "card-custom",
                      card(
                        card_header(
                          class = "bg-dark",
                          "Peak parameters"
                        ),
                        card_body(
                          shiny$fluidRow(
                            shiny$column(
                              width = 3,
                              shiny$h6("Window", style = "margin-top: 8px;")
                            ),
                            shiny$column(
                              width = 3,
                              shiny$div(
                                class = "deconv-param-input-adv",
                                shiny$numericInput(
                                  ns("peakwindow"),
                                  "",
                                  min = 0,
                                  max = 1000,
                                  value = 40,
                                  width = "130%"
                                )
                              )
                            ),
                            shiny$column(
                              width = 3,
                              shiny$h6("Norm", style = "margin-top: 8px;")
                            ),
                            shiny$column(
                              width = 3,
                              shiny$div(
                                class = "deconv-param-input-adv",
                                shiny$numericInput(
                                  ns("peaknorm"),
                                  "",
                                  min = 0,
                                  max = 100,
                                  value = 2,
                                  width = "130%"
                                )
                              )
                            )
                          ),
                          shiny$fluidRow(
                            shiny$column(
                              width = 3,
                              shiny$h6(
                                "Threshold",
                                style = "font-size: small; margin-top: 8px;"
                              )
                            ),
                            shiny$column(
                              width = 3,
                              shiny$div(
                                class = "deconv-param-input-adv",
                                shiny$numericInput(
                                  ns("peakthresh"),
                                  "",
                                  min = 0,
                                  max = 1,
                                  value = 0.07,
                                  width = "130%",
                                  step = 0.01
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  shiny$column(
                    width = 5,
                    shiny$div(
                      class = "card-custom",
                      card(
                        card_header(
                          class = "bg-dark",
                          "Mass Bins"
                        ),
                        card_body(
                          shiny$fluidRow(
                            shiny$column(
                              width = 6,
                              shiny$h6("Size", style = "margin-top: 8px;")
                            ),
                            shiny$column(
                              width = 5,
                              shiny$div(
                                class = "deconv-param-input-adv",
                                shiny$numericInput(
                                  ns("massbins"),
                                  "",
                                  min = 0,
                                  max = 100,
                                  value = 0.5,
                                  step = 0.1,
                                  width = "130%"
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
          )
        ),
        shiny$column(
          width = 4,
          align = "center",
          shiny$br(),
          shiny$br(),
          shiny$br(),
          shiny$br(),
          shiny$br(),
          shiny$br(),
          shiny$br(),
          shiny$br(),
          shiny$br(),
          shiny$br(),
          shiny$uiOutput(ns("deconvolute_start_ui")),
          shiny$br(),
          shiny$uiOutput(ns("deconvolute_progress"))
        )
      )
    )

    output$deconvolution_init_ui <- shiny$renderUI(
      deconvolution_init_ui
    )

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
                '<i class="fa fa-spinner fa-spin fa-fw fa-2x" style="color: #38387C; margin-top: 0.5em"></i>'
              )
            )
          )
        ),
        shiny$column(
          width = 7,
          progressBar(
            id = ns("progressBar"),
            value = 0,
            title = "Initiating Deconvolution",
            display_pct = TRUE
          )
        ),
        shiny$column(
          width = 3,
          align = "center",
          shiny$actionButton(ns("deconvolute_end"), "Abort")
        )
      ),
      shiny$hr(style = "margin: 1.5rem 0; opacity: 0.8;"),
      shiny$fluidRow(
        shiny$column(6),
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
                  addSpinner(
                    plotlyOutput(ns("heatmap")),
                    spin = "cube",
                    color = "#38387c"
                  )
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
                addSpinner(
                  plotlyOutput(ns("spectrum")),
                  spin = "cube",
                  color = "#38387c"
                )
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
                '<i class="fa fa-spinner fa-spin fa-fw fa-2x" style="color: #38387C; margin-top: 0.5em"></i>'
              )
            )
          )
        ),
        shiny$column(
          width = 7,
          progressBar(
            id = ns("progressBar"),
            value = 0,
            title = "Initiating Deconvolution",
            display_pct = TRUE
          )
        ),
        shiny$column(
          width = 3,
          align = "center",
          shiny$actionButton(ns("deconvolute_end"), "Abort")
        )
      ),
      shiny$hr(style = "margin: 1.5rem 0; opacity: 0.8;"),
      shiny$fluidRow(
        shiny$column(2),
        shiny$column(
          width = 4,
          shiny$uiOutput(ns("result_picker_ui"))
        ),
        shiny$column(
          width = 4,
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
                addSpinner(
                  plotlyOutput(ns("spectrum")),
                  spin = "cube",
                  color = "#38387c"
                )
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

      shiny$actionButton(ns("deconvolute_start"), "Run Deconvolution")
    })

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
      rslt_df = data.frame()
    )

    result_files_sel <- shiny$reactiveVal()
    target_selector_sel <- shiny$reactiveVal()

    shiny$observe({
      if (!is.null(input$result_picker)) result_files_sel(input$result_picker)
      if (!is.null(input$target_selector)) {
        target_selector_sel(input$target_selector)
      }
    })

    ### Functions ----
    #### check_progress ----
    check_progress <- function(raw_dirs) {
      message("Checking progress at: ", Sys.time())
      fin_dirs <- gsub(".raw", "_rawdata_unidecfiles", raw_dirs)
      peak_files <- file.path(fin_dirs, "plots.rds")
      finished_files <- file.exists(peak_files)

      if (dirs$selected() == "file" && sum(finished_files) > 0) {
        choices <- basename(raw_dirs)[finished_files]
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

        reactVars$trigger <- TRUE
      }

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

        if (length(dirs$batch_file())) {
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

        if (length(dirs$batch_file())) {
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
                  '<i class="fa-solid fa-circle-exclamation" style="font-size:1em; color:black; margin-right: 10px;"></i>',
                  "<b>",
                  sum(intersect),
                  paste0(
                    "</b> file(s) queried for deconvolution appear to have already",
                    " been processed. Please choose how to proceed:"
                  )
                )
              )
            )
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
                  '<i class="fa-solid fa-circle-exclamation" style="font-size:1em; color:black; margin-right: 10px;"></i>',
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

      ##### Deconvolution init and mode ----
      if (dirs$selected() == "folder") {
        raw_dirs <- list.dirs(
          dirs$dir(),
          full.names = TRUE,
          recursive = FALSE
        )
        raw_dirs <- raw_dirs[grep("\\.raw$", raw_dirs)]

        if (length(dirs$batch_file())) {
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
            sub("^.*:", "", dirs$batch_file()[[dirs$vial_column()]])
          )
        } else {
          raw_dirs <- raw_dirs[basename(raw_dirs) %in% target_selector_sel()]
        }
      } else if (dirs$selected() == "file") {
        raw_dirs <- dirs$file()
      }

      # Overwrite or skip already present result dirs
      if (!isFALSE(reactVars$overwrite)) {
        if (reactVars$duplicated == "Overwrite Files") {
          # Remove result files and dirs
          if (dirs$selected() == "file") {
            rslt_dirs <- gsub(".raw", "_rawdata_unidecfiles", dirs$file())
          } else {
            rslt_dirs <- file.path(
              dirs$dir(),
              reactVars$overwrite
            )
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
        }
      }

      # Validate inputs
      if (length(raw_dirs) == 0) {
        shiny$showNotification(
          paste0("No .raw directories found in ", dirs$dir()),
          type = "error",
          duration = NULL
        )
        reset_progress()
        return()
      }

      # UI changes if inputs valid
      runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "block";'
      ))
      runjs(paste0(
        "document.getElementById('app-deconvolution_process-deconvo",
        "lute_start').style.animation = 'none';"
      ))

      message(sprintf("Found %d .raw directories to process", length(raw_dirs)))

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
      conf <- list(
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

      tmp <- file.path(tempdir(), "conf.rds")
      saveRDS(conf, tmp)

      rx_process <- process$new(
        "Rscript",
        args = c("app/logic/deconvolution_execute.R", tmp),
        stdout = "|",
        stderr = "|"
      )

      process_data(rx_process)

      reactVars$process_observer <- shiny$observe({
        proc <- process_data()

        session$onSessionEnded(function() {
          if (!is.null(proc) && proc$is_alive()) {
            proc$kill()
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
                  for (i in 1:length(results)) {
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
                        header = F,
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
                    for (i in 1:length(new_results)) {
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
                              header = F,
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
                    reactVars$heatmap_ready <- TRUE
                    waiter_hide(id = ns("heatmap"))
                    heatmap
                  })

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

                output$heatmap <- renderPlotly({
                  waiter_show(
                    id = ns("heatmap"),
                    html = spin_wandering_cubes()
                  )
                  heatmap <- create_384_plate_heatmap(reactVars$rslt_df) |>
                    event_register("plotly_click")
                  reactVars$heatmap_ready <- TRUE
                  waiter_hide(id = ns("heatmap"))
                  heatmap
                })

                reactVars$trigger <- TRUE
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
              "Finalizing ",
              paste0(rep(".", reactVars$count), collapse = "")
            )

            result_files <- gsub(".raw", "_rawdata_unidecfiles", raw_dirs)

            # check if deconvolution finished for all target files
            if (all(file.exists(file.path(result_files, "plots.rds")))) {
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
              if (dirs$selected() == "folder" && length(dirs$batch_file())) {
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
                  for (i in 1:length(new_results)) {
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
                            header = F,
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

              shiny$updateActionButton(
                session,
                "deconvolute_end",
                label = "Reset"
              )

              title <- "Finalized!"

              hide(selector = "#app-deconvolution_process-processing")
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
      if (dirs$selected() == "folder" && length(dirs$batch_file())) {
        reactVars$click_observer <- shiny$observe({
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

        output$deconvolution_running_ui <- NULL
        output$heatmap <- NULL
        output$deconvolution_init_ui <- shiny$renderUI(
          deconvolution_init_ui
        )

        runjs(paste0(
          'document.getElementById("blocking-overlay").styl',
          'e.display = "none";'
        ))
        runjs("document.querySelector('button.collapse-toggle').click();")
      }
    })

    shiny$observeEvent(input$deconvolute_end_conf, {
      proc <- process_data()
      if (!is.null(proc) && proc$is_alive()) {
        proc$kill()
      }

      updateProgressBar(
        session = session,
        id = ns("progressBar"),
        value = 0,
        title = "Processing aborted"
      )

      hide(selector = "#app-deconvolution_process-processing")

      shiny$updateActionButton(
        session,
        "deconvolute_end",
        label = "Reset"
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
  })
}
