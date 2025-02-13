# app/view/deconvolution_process.R

box::use(
  bslib[card, card_body, card_header],
  fs[dir_ls],
  future[future],
  promises[catch, then],
  shiny,
  shinyjs[disabled, enable, runjs],
  shinyWidgets[progressBar, updateProgressBar],
)

# Import the deconvolution function
box::use(
  app/logic/deconvolution_functions[deconvolute],
  app/logic/helper_functions[collapsiblePanelUI],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$column(
    width = 12,
    shiny$fluidRow(
      shiny$column(
        width = 4,
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
        ),
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
      ),
      shiny$column(
        width = 4,
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
        ),
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
      ),
      shiny$column(
        width = 4,
        align = "center",
        shiny$uiOutput(ns("deconvolute_start_ui")),
        shiny$br(),
        shiny$uiOutput(ns("deconvolute_progress"))
      )
    ),
    shiny$fluidRow(
      shiny$column(
        width = 8,
        collapsiblePanelUI(
          "advanced_params",
          "Advanced Parameter Settings",
          shiny$fluidRow(
            shiny$column(
              width = 7,
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
            ),
            shiny$column(
              width = 5,
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
}

#' @export
server <- function(id, dirs) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get selected directories
    raw_dir <- shiny$reactive({
      dirs$dir()
    })

    # Render start button conditionally
    output$deconvolute_start_ui <- shiny$renderUI({
      if (isFALSE(reactVars$isRunning)) {

        shiny$validate(
          shiny$need(dir.exists(raw_dir()), "No Waters .raw selected"))
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
            length(input$time_start) && length(input$time_end)) {
          button
        } else {
          disabled(button)
        }
      }
    })

    # Reactive variables to track deconvolution process
    reactVars <- shiny$reactiveValues(
      isRunning = FALSE,
      completedFiles = 0,
      expectedFiles = 0,
      initialFileCount = 0,
      lastCheck = 0,
      count = 0
    )

    checkProgress <- function(dir_path) {
      message("Checking progress at: ", Sys.time())
      files <- dir_ls(dir_path, glob = "*_rawdata.txt")
      count <- length(files)
      message("Found files: ", count)
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

      raw_dirs <- list.dirs(raw_dir(), full.names = TRUE, recursive = FALSE)
      raw_dirs <- raw_dirs[grep("\\.raw$", raw_dirs)]

      if (length(raw_dirs) == 0) {
        shiny$showNotification(
          paste0("No .raw directories found in ", raw_dir()),
          type = "error",
          duration = NULL
        )
        reactVars$isRunning <- FALSE
        return()
      }

      message(sprintf("Found %d .raw directories to process", length(raw_dirs)))
      shiny$showNotification(
        sprintf("Found %d .raw directories to process", length(raw_dirs)),
        type = "message", duration = NULL)

      reactVars$expectedFiles <- length(raw_dirs)
      reactVars$initialFileCount <- checkProgress(raw_dir())
      message("Initial file count: ", reactVars$initialFileCount)

      if (!is.null(progress_observer)) {
        progress_observer$destroy()
      }

      # close parameter settings menu
      runjs('document.querySelector("button.collapse-toggle").click();')

      # render deconvolution progress bar
      output$deconvolute_progress <- shiny$renderUI(
        progressBar(
          id = ns("progressBar"),
          value = 0,
          title = "Initiating Deconvolution",
          display_pct = TRUE
        )
      )

      # hide start and render terminate button
      output$deconvolute_start_ui <- shiny$renderUI(
        shiny$actionButton(ns("deconvolute_end"), "Abort Deconvolution")
      )

      print(input)

      # Start deconvolution
      future({
        message("Starting deconvolution in parallel session")
      deconvolute(raw_dirs, config_startz = input$startz,
                  config_endz = input$endz,
                  config_minmz = input$minmz,
                  config_maxmz = input$maxmz,
                  config_masslb = input$masslb,
                  config_massub = input$massub,
                  config_massbins = input$massbins,
                  config_peakthresh = input$peakthresh,
                  config_peakwindow = input$window,
                  config_peaknorm = input$peaknorm,
                  config_time_start = input$time_start,
                  config_time_end = input$time_end)
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
        shiny$invalidateLater(500)

        if (difftime(Sys.time(), reactVars$lastCheck, units = "secs") >= 0.5) {
          current_total_files <- checkProgress(raw_dir())
          reactVars$completedFiles <-
            current_total_files - reactVars$initialFileCount
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
