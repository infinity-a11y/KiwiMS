# app/view/deconvolution_process.R

box::use(
  bslib[card, card_body, card_header],
  fs[dir_ls],
  future[future, plan, multisession],
  plotly[event_data, plotlyOutput, renderPlotly],
  promises[catch, then],
  shiny,
  shinyjs[disabled, enable, runjs],
  shinyWidgets[radioGroupButtons, progressBar, updateProgressBar],
)

box::use(
  app/logic/deconvolution_functions[deconvolute, create_384_plate_heatmap,
                                    spectrum_plot],
  app/logic/helper_functions[collapsiblePanelUI],
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

  plan(multisession)

  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Deconvolution initiation interface

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
                              shiny$h6("Threshold",
                                       style = "font-size: small; margin-top: 8px;")
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
          shiny$br(), shiny$br(), shiny$br(), shiny$br(), shiny$br(),
          shiny$br(), shiny$br(), shiny$br(), shiny$br(), shiny$br(),
          shiny$uiOutput(ns("deconvolute_start_ui")),
          shiny$br(),
          shiny$uiOutput(ns("deconvolute_progress"))
        )
      )
    )

    output$deconvolution_init_ui <- shiny$renderUI({
      deconvolution_init_ui
    })

    # Deconvolution running interface
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
          shiny$br(), shiny$br(),
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
              radioGroupButtons(
                ns("toggle_result"),
                choiceNames = c("Deconvoluted", "Raw m/z"),
                choiceValues = c(FALSE, TRUE)
              )
            )
          ),
          shiny$fluidRow(
            shiny$column(
              width = 12,
              # shiny$verbatimTextOutput(ns("click_info")),
              plotlyOutput(ns("spectrum"))
            )
          )
        )
      )
    )

    output$result_picker_ui <- shiny$renderUI({
      if (!is.null(reactVars$result_files) &&
         length(reactVars$result_files) > 0) {
        select <- shiny$selectInput("result_picker", "",
                                    choices = reactVars$result_files)
      } else {
        select <- disabled(shiny$selectInput("result_picker", "", choices = ""))
      }

      session$sendCustomMessage("selectize-init", "result_picker")

      shiny$div(
        class = "result-picker",
        select
      )
    })

    # Render result spectrum when available
    output$spectrum <- renderPlotly({
      click_data <- event_data("plotly_click")
      if (!is.null(click_data)) {
        runjs(paste0('document.getElementById("blocking-overlay").styl',
                     'e.display = "block";'))

        # Get the clicked point's row and column
        row <- LETTERS[16 - floor(click_data$y) + 1]
        col <- round(click_data$x)
        well_id <- paste0(row, col)
        # Find the corresponding sample in the data
        clicked_sample <-
          reactVars$rslt_df$sample[reactVars$rslt_df$well_id == well_id]

        spectrum <- NULL
        if (length(clicked_sample) > 0) {
          spectrum <- spectrum_plot(
            file.path(dirs$dir(),
                      paste0(clicked_sample, "_rawdata_unidecfiles")),
            input$toggle_result)
        }

        runjs(paste0('document.getElementById("blocking-overlay").st',
                     'yle.display = "none";'))
        spectrum
      }
    })

    # Render start button conditionally
    output$deconvolute_start_ui <- shiny$renderUI({
      # root_dir_presence <- !is.null(dirs$dir()) && length(dirs$dir()) > 0
      # batch_file_presence <- !is.null(dirs$batch_file()) && length(dirs$batch_file()) > 0
      # single_file_presence <- !is.null(dirs$file()) && length(dirs$file()) > 0
      #
      # print(paste("reactVars$isRunning", reactVars$isRunning))
      # print(paste("dirs$dir()", dirs$dir()))
      # print(root_dir_presence)
      # print(paste("dirs$batch_file()", dirs$batch_file()))
      # print(batch_file_presence)
      # print(paste("dirs$file", dirs$file()))
      # print(single_file_presence)

      # if (isFALSE(reactVars$isRunning)) {

        # shiny$validate(
        #   shiny$need(dir.exists(dirs$dir()), "No Waters .raw selected"))

        button <- shiny$actionButton(ns("deconvolute_start"),
                                     "Run Deconvolution")

        shiny$validate(
          shiny$need(input$startz < input$endz,
                     "High charge z must be greater than low charge z"))

        shiny$validate(
          shiny$need(input$minmz < input$maxmz,
                     "High m/z must be greater than low m/z"))

        shiny$validate(
          shiny$need(input$masslb < input$massub,
                     "High mass Mw must be greater than low mass Mw"))

        shiny$validate(
          shiny$need(input$time_start < input$time_end,
                     "Retention start time must be earlier than end time"))

        if (length(input$startz) && length(input$endz) &&
            length(input$minmz) && length(input$maxmz) &&
            length(input$masslb) && length(input$massub) &&
            length(input$massbins) && length(input$peakthresh) &&
            length(input$peakwindow) && length(input$peaknorm) &&
            length(input$time_start) && length(input$time_end)
            # &&
            # ((root_dir_presence && batch_file_presence) ||
            #  single_file_presence)
            ) {
          button
        } else {
          disabled(button)
        }
      # }
    })

    # Reactive variables to track deconvolution process
    reactVars <- shiny$reactiveValues(
      isRunning = FALSE,
      completedFiles = 0,
      expectedFiles = 0,
      current_total_files = 0,
      initialFileCount = 0,
      lastCheck = 0,
      count = 0
    )

    checkProgress <- function(dir_path) {
      message("Checking progress at: ", Sys.time())
      files <- dir_ls(dir_path, glob = "*_rawdata.txt")
      count <- length(files)
      message("Found files: ", count)
      reactVars$result_files <- gsub(".txt", "", basename(files))
      count
    }

    reset_progress <- function() {
      reactVars$isRunning <- TRUE
      reactVars$completedFiles <- 0
      reactVars$lastCheck <- Sys.time()
      updateProgressBar(
        session = session,
        id = ns("progressBar"),
        value = 0,
        title = "Starting Process..."
      )
      output$progress <- shiny$renderText("Initializing...")
      output$status <- shiny$renderText("")
    }

    progress_observer <- NULL

    shiny$observeEvent(input$deconvolute_start, {

      reset_progress()

      # Check processing mode & get target files
      if (dirs$selected() == "folder") {
        raw_dirs <- list.dirs(dirs$dir(), full.names = TRUE, recursive = FALSE)
        raw_dirs <- raw_dirs[grep("\\.raw$", raw_dirs)]

        batch <- dirs$batch_file()
        sample_names <- batch[[dirs$id_column()]]

        raw_dirs <- raw_dirs[basename(raw_dirs) %in% sample_names]
      }

      if (length(raw_dirs) == 0) {
        shiny$showNotification(
          paste0("No .raw directories found in ", dirs$dir()),
          type = "error",
          duration = NULL
        )
        reactVars$isRunning <- FALSE
        return()
      }

      runjs(paste0('document.getElementById("blocking-overlay").style.display ',
                   '= "block";'))
      runjs(paste0("document.getElementById('deconvolute_start').style.animati",
                   "on = none;"))

      message(sprintf("Found %d .raw directories to process", length(raw_dirs)))

      reactVars$expectedFiles <- length(raw_dirs)
      reactVars$initialFileCount <- checkProgress(dirs$dir())
      message("Initial file count: ", reactVars$initialFileCount)

      if (!is.null(progress_observer)) {
        progress_observer$destroy()
      }

      # Get parameter
      startz <- input$startz
      endz <- input$endz
      minmz <- input$minmz
      maxmz <- input$maxmz
      masslb <- input$masslb
      massub <- input$massub
      massbins <- input$massbins
      peakthresh <- input$peakthresh
      peakwindow <- input$peakwindow
      peaknorm <- input$peaknorm
      time_start <- input$time_start
      time_end <- input$time_end

      # Start deconvolution
      future({
        deconvolute(raw_dirs, startz = startz, endz = endz,
                    minmz = minmz, maxmz = maxmz,
                    masslb = masslb, massub = massub,
                    massbins = massbins, peakthresh = peakthresh,
                    peakwindow = peakwindow, peaknorm = peaknorm,
                    time_start = time_start, time_end = time_end)
      }) |>
        # on successful completion
        then(
          onFulfilled = function(value) {
            message("Future completed successfully")
            reactVars$isRunning <- FALSE
            progress_observer$destroy()

            updateProgressBar(
              session = session,
              id = ns("progressBar"),
              value = 100,
              title = "Processing Complete!"
            )

            output$status <- shiny$renderText("Process completed successfully!")
            enable(ns("deconvolute_start"))

            # # Render plate result visualization
            # batch <- dirs$batch_file()
            # sample_names <- batch[[dirs$id_column()]]
            # sample_names <- gsub(".raw", "", sample_names)
            #
            # results_all <- fs::dir_ls(dirs$dir(), glob = "*_rawdata_unidecfiles")
            #
            # results <- results_all[basename(results_all) %in% paste0(
            #   sample_names, "_rawdata_unidecfiles")]
            #
            # wells <- gsub(",", "", sub("^.*:", "", batch[[dirs$vial_column()]]))
            #
            # value <- numeric()
            # for (i in 1:nrow(batch)) {
            #
            #   sample_name <- gsub(".raw", "", batch$sample[i])
            #
            #   rslt_folder <- file.path(
            #     dirs$dir(), paste0(sample_name, "_rawdata_unidecfiles"))
            #
            #   peak_file <- file.path(
            #     dirs$dir(),
            #     paste0(sample_name, "_rawdata_unidecfiles"),
            #     paste0(sample_name, "_rawdata_peaks.dat"))
            #
            #   if (dir.exists(rslt_folder) && file.exists(peak_file)) {
            #     peaks <- utils::read.delim(peak_file, header = F, sep = " ")
            #     value[i] <- max(peaks$V1)
            #   } else {
            #     value[i] <- NA
            #   }
            # }
            #
            # reactVars$rslt_df <- data.frame(sample = sample_names, well_id = wells,
            #                       value = value)
            #
            # output$heatmap <- renderPlotly({
            #   create_384_plate_heatmap(reactVars$rslt_df)
            # })
            #
            # output$spectrum <- renderPlotly({
            #   click_data <- event_data("plotly_click")
            #   if (!is.null(click_data)) {
            #     runjs(paste0('document.getElementById("blocking-overlay").styl',
            #                  'e.display = "block";'))
            #
            #     # Get the clicked point's row and column
            #     row <- LETTERS[16 - floor(click_data$y) + 1]
            #     col <- round(click_data$x)
            #     well_id <- paste0(row, col)
            #     # Find the corresponding sample in the data
            #     clicked_sample <- reactVars$rslt_df$sample[reactVars$rslt_df$well_id == well_id]
            #     if (length(clicked_sample) > 0) {
            #       spectrum <- spectrum_plot(
            #         file.path(dirs$dir(),
            #                   paste0(clicked_sample, "_rawdata_unidecfiles")),
            #         input$toggle_result)
            #
            #       runjs(paste0('document.getElementById("blocking-overlay").st',
            #                    'yle.display = "none";'))
            #
            #       spectrum
            #     }
            #   }
            # })
          }
        ) |>
        # on failing process
        catch(
          function(error) {
            message("Future failed with error: ", error$message)
            reactVars$isRunning <- FALSE
            progress_observer$destroy()

            updateProgressBar(
              session = session,
              id = ns("progressBar"),
              value = 0,
              title = "Process Failed!"
            )

            output$status <- shiny$renderText(
              paste("Process failed!", error$message)
            )
            enable(ns("deconvolute_start"))
          }
        )

      # Progress tracking observer
      progress_observer <- shiny$observe({
        shiny$req(reactVars$isRunning)
        shiny$invalidateLater(1000)

        if (difftime(Sys.time(), reactVars$lastCheck, units = "secs") >= 0.5) {
          current_total_files <- checkProgress(dirs$dir())

          if(current_total_files > reactVars$current_total_files) {
            # Render plate result visualization
            batch <- dirs$batch_file()
            sample_names <- batch[[dirs$id_column()]]
            sample_names <- gsub(".raw", "", sample_names)

            results_all <- fs::dir_ls(dirs$dir(), glob = "*_rawdata_unidecfiles")

            results <- results_all[basename(results_all) %in% paste0(
              sample_names, "_rawdata_unidecfiles")]

            wells <- gsub(",", "", sub("^.*:", "", batch[[dirs$vial_column()]]))

            value <- numeric()
            for (i in 1:nrow(batch)) {

              sample_name <- gsub(".raw", "", batch$sample[i])

              rslt_folder <- file.path(
                dirs$dir(), paste0(sample_name, "_rawdata_unidecfiles"))

              peak_file <- file.path(
                dirs$dir(),
                paste0(sample_name, "_rawdata_unidecfiles"),
                paste0(sample_name, "_rawdata_peaks.dat"))

              if (dir.exists(rslt_folder) && file.exists(peak_file)) {
                peaks <- utils::read.delim(peak_file, header = F, sep = " ")
                value[i] <- max(peaks$V1)
              } else {
                value[i] <- NA
              }
            }

            reactVars$rslt_df <- data.frame(sample = sample_names, well_id = wells,
                                  value = value)

            output$heatmap <- renderPlotly({
              create_384_plate_heatmap(reactVars$rslt_df)
            })
          }

          reactVars$current_total_files <- current_total_files
          reactVars$completedFiles <-
            reactVars$current_total_files - reactVars$initialFileCount
          reactVars$lastCheck <- Sys.time()

          progress_pct <- min(100, round(
            100 * reactVars$completedFiles / reactVars$expectedFiles))

          message("Updating progress: ", progress_pct, "% (",
                  reactVars$completedFiles, "/", reactVars$expectedFiles, ")")

          if (reactVars$count < 3) {
            reactVars$count <- reactVars$count + 1
          } else {
            reactVars$count <- 0
          }

          if (progress_pct != 100) {
            title <- paste0(
              sprintf("Processing Files (%d/%d) ",
                      reactVars$completedFiles, reactVars$expectedFiles),
              paste0(rep(".", reactVars$count), collapse = ""))
          } else {
            title <- paste0("Finalizing ",
                            paste0(rep(".", reactVars$count), collapse = ""))
          }

          updateProgressBar(
            session = session,
            id = ns("progressBar"),
            value = progress_pct,
            title = title
          )
        }
      })

      # close parameter settings menu
      runjs("document.querySelector('button.collapse-toggle').click();")
      output$deconvolution_init_ui <- NULL

      output$deconvolution_running_ui <- shiny$renderUI({
        deconvolution_running_ui
      })

      runjs(paste0('document.getElementById("blocking-overlay").style.display ',
                   '= "none";'))
    })

    shiny$observeEvent(input$deconvolute_end, {
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
              shiny$actionButton("deconvolute_end_conf", "Abort",
                           class = "load-db", width = "100px")
            )
          )
        )
      )
    })

    shiny$observeEvent(input$deconvolute_end_conf, {
      # TODO
    })
  })
}
