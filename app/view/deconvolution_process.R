# app/view/deconvolution_process.R

box::use(
  bslib[card, card_body, card_header],
  fs[dir_ls],
  parallel[detectCores, makeCluster],
  plotly[event_data, event_register, plotlyOutput, renderPlotly],
  processx[process],
  shiny,
  shinyjs[delay, disabled, enable, runjs],
  shinyWidgets[radioGroupButtons, progressBar, updateProgressBar],
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
    deconvolution_running_ui <- shiny$column(
      width = 12,
      shiny$fluidRow(
        shiny$column(width = 1),
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
          width = 4,
          align = "center",
          shiny$actionButton(ns("deconvolute_end"), "Abort")
        )
      ),
      shiny$hr(),
      shiny$fluidRow(
        shiny$column(
          width = 6,
          shiny$br(),
          plotlyOutput(ns("heatmap"))
        ),
        shiny$column(
          width = 6,
          shiny$fluidRow(
            shiny$column(
              width = 6,
              shiny$uiOutput(ns("result_picker_ui"))
            ),
            shiny$column(
              width = 6,
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
              width = 12,
              plotlyOutput(ns("spectrum"))
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

    shiny$observe({
      if (!is.null(input$result_picker)) result_files_sel(input$result_picker)
    })

    ### Functions ----
    #### check_progress ----
    check_progress <- function(dir_path) {
      message("Checking progress at: ", Sys.time())
      files <- dir_ls(dir_path, glob = "*_rawdata.txt")
      count <- length(files)
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
    }

    ### Event start deconvolution ----

    #### Start confirmation modal ----

    shiny$observeEvent(input$deconvolute_start, {
      if (dirs$selected() == "folder") {
        if (length(dirs$batch_file())) {
          message <- paste0(
            "<b>Multiple target file(s) selected</b><br><br>",
            nrow(dirs$batch_file()),
            " raw file(s) present in ",
            "the batch file will be deconvoluted."
          )
        } else {
          message <- paste0(
            "<b>Multiple target file(s) selected</b><br><br>",
            "No batch file uploaded. All raw files in the",
            " selected directory will be deconvoluted."
          )
        }
      } else {
        name <- basename(dirs$file())
        message <- paste0(
          "<b>Individual target file selected</b><br><br>",
          name,
          " will be deconvoluted."
        )
      }

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
                    message
                  )
                )
              ),
              shiny$br()
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

    #### Confirmed deconvolution start ----
    shiny$observeEvent(input$deconvolute_start_conf, {
      # Reset modal and previous processes
      shiny$removeModal()
      reset_progress()

      ##### Determine deconvolution mode ----
      if (dirs$selected() == "folder") {
        if (length(dirs$batch_file())) {
          raw_dirs <- list.dirs(
            dirs$dir(),
            full.names = TRUE,
            recursive = FALSE
          )
          raw_dirs <- raw_dirs[grep("\\.raw$", raw_dirs)]

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
          raw_dirs <- list.dirs(
            dirs$dir(),
            full.names = TRUE,
            recursive = FALSE
          )
        }
      }

      #### Validate inputs ----
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

      # Initialization variables
      reactVars$isRunning <- TRUE
      reactVars$expectedFiles <- length(raw_dirs)
      reactVars$initialFileCount <- check_progress(dirs$dir())
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

      #### Results tracking observer ----
      reactVars$results_observer <- shiny$observe({
        shiny$req(reactVars$sample_names, reactVars$wells)
        shiny$invalidateLater(10000)

        if (
          nrow(reactVars$rslt_df) < reactVars$completedFiles &&
            difftime(Sys.time(), reactVars$lastCheckresults, units = "secs") >=
              10
        ) {
          results_all <- fs::dir_ls(dirs$dir(), glob = "*_rawdata_unidecfiles")

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
                  peaks <- utils::read.delim(peak_file, header = F, sep = " ")
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

            ##### Render heatmap & result picker ----
            if (nrow(reactVars$rslt_df) > 0) {
              # enable("app-deconvolution_process-toggle_result")
              # enable(ns("toggle_result"))
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
                      "",
                      basename(results)
                    ),
                    selected = result_files_sel()
                  )
                )
              )
              # Apply JS modifications for picker
              session$sendCustomMessage("selectize-init", "result_picker")

              output$heatmap <- renderPlotly({
                heatmap <- create_384_plate_heatmap(reactVars$rslt_df) |>
                  event_register("plotly_click")
                reactVars$heatmap_ready <- TRUE
                heatmap
              })

              reactVars$trigger <- TRUE
            }
          }

          reactVars$lastCheckresults <- Sys.time()
        }
      })

      #### Progress tracking observer ----
      reactVars$progress_observer <- shiny$observe({
        shiny$req(reactVars$isRunning)
        shiny$invalidateLater(1000)

        if (difftime(Sys.time(), reactVars$lastCheck, units = "secs") >= 0.5) {
          reactVars$current_total_files <- check_progress(dirs$dir())
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

          result_files_sel(clicked_sample)
        }
      })

      #### Switch to running UI ----
      runjs("document.querySelector('button.collapse-toggle').click();")
      output$deconvolution_init_ui <- NULL

      output$deconvolution_running_ui <- shiny$renderUI({
        deconvolution_running_ui
      })

      runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "none";'
      ))
    })

    ### Render result spectrum ----
    output$spectrum <- renderPlotly({
      shiny$req(result_files_sel(), input$toggle_result)

      reactVars$trigger

      result_dir <- file.path(
        dirs$dir(),
        paste0(result_files_sel(), "_rawdata_unidecfiles")
      )

      if (dir.exists(result_dir)) {
        runjs(paste0(
          'document.getElementById("blocking-overlay").st',
          'yle.display = "block";'
        ))

        spectrum <- spectrum_plot(result_dir, input$toggle_result)

        runjs(paste0(
          'document.getElementById("blocking-overlay").st',
          'yle.display = "none";'
        ))
        spectrum
      }
    })

    ### Event end deconvolution ----
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

        reactVars$progress_observer$destroy()
        reactVars$process_observer$destroy()
        reactVars$results_observer$destroy()
        reactVars$click_observer$destroy()
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

      shiny$updateActionButton(
        session,
        "deconvolute_end",
        label = "Reset"
      )

      reactVars$progress_observer$destroy()
      reactVars$results_observer$destroy()
      reactVars$process_observer$destroy()
      reactVars$isRunning <- FALSE

      shiny$removeModal()
      shiny$showNotification("Process terminated", type = "message")
    })
  })
}
