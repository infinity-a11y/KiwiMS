# app/view/deconvolution_main.R

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
  waiter[useWaiter, spin_wave, waiter_show, waiter_hide, withWaiter],
)

box::use(
  app /
    logic /
    deconvolution_functions[
      create_384_plate_heatmap,
      spectrum_plot
    ],
  app / logic / helper_functions[fill_empty, get_kiwiflow_version],
  app / logic / logging[write_log, get_log],
  app /
    view /
    deconvolution_constants[
      deconvolution_running_ui_plate,
      deconvolution_running_ui_noplate,
      deconvolution_init_ui
    ],
  app /
    logic /
    logging[
      write_log,
      get_session_id,
    ]
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$div(
    class = "deconvolution-running-interface",
    shiny$uiOutput(ns("deconvolution_init_ui")),
    shiny$uiOutput(ns("deconvolution_running_ui"))
  )
}

#' @export
server <- function(
  id,
  deconvolution_sidebar_vars,
  conversion_main_vars,
  reset_button
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get kiwiflow user settings
    settings_dir <- file.path(
      Sys.getenv("LOCALAPPDATA"),
      "KiwiFlow",
      "settings"
    )

    # Define log location
    log_path <- get_log()

    # Make temp dir for session
    temp <- tempdir()

    ### Reactive variables declaration ----
    reactVars <- shiny$reactiveValues(
      is_running = FALSE,
      completed_files = 0,
      expected_files = 0,
      current_total_files = 0,
      initial_file_count = 0,
      last_check = 0,
      results_last_check = 0,
      count = 0,
      rep_count = 0,
      rslt_df = data.frame(),
      logs = "",
      deconv_report_status = NULL,
      continue_conversion = NULL
    )

    decon_rep_process_data <- shiny$reactiveVal(NULL)
    result_files_sel <- shiny$reactiveVal(NULL)
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
    output$deconvolution_init_ui <- shiny$renderUI({
      deconvolution_init_ui(ns)
    })

    # Conditional enabling of advanced settings
    shiny$observe({
      if (isTRUE(input$show_advanced)) {
        enable(
          selector = ".deconv-param-input-adv",
          asis = TRUE
        )
      } else {
        disable(
          selector = ".deconv-param-input-adv",
          asis = TRUE
        )
      }
    })

    ### Validate start button ----
    output$deconvolute_start_ui <- shiny$renderUI({
      reset_button()

      shiny$validate(
        shiny$need(
          ((!is.null(deconvolution_sidebar_vars$file()) &&
            length(deconvolution_sidebar_vars$file()) > 0) ||
            (!is.null(deconvolution_sidebar_vars$dir()) &&
              length(deconvolution_sidebar_vars$dir()) > 0)),
          "Select target file(s) from the sidebar to start ..."
        )
      )

      if (
        !is.null(deconvolution_sidebar_vars$targetpath()) &&
          length(deconvolution_sidebar_vars$targetpath()) > 0
      ) {
        sessionId <- gsub(".log", "", basename(log_path))
        result_files <- list.files(deconvolution_sidebar_vars$targetpath())

        if (any(grepl(sessionId, gsub("_RESULT.rds", "", result_files)))) {
          valid_destination <- FALSE
        } else {
          valid_destination <- TRUE
        }
      } else {
        valid_destination <- FALSE
      }

      shiny$validate(
        shiny$need(
          valid_destination,
          "Select destination for result file(s) from the sidebar to start ..."
        )
      )

      shiny$validate(
        shiny$need(
          input$startz < input$endz,
          "High charge z must be greater than low charge z ..."
        )
      )

      shiny$validate(
        shiny$need(
          input$minmz < input$maxmz,
          "High m/z must be greater than low m/z ..."
        )
      )

      shiny$validate(
        shiny$need(
          input$masslb < input$massub,
          "High mass Mw must be greater than low mass Mw ..."
        )
      )

      shiny$validate(
        shiny$need(
          input$time_start < input$time_end,
          "Retention start time must be earlier than end time ..."
        )
      )

      if (deconvolution_sidebar_vars$selected() == "folder") {
        valid_folder <- length(dir_ls(
          deconvolution_sidebar_vars$dir(),
          glob = "*.raw"
        )) !=
          0

        shiny$validate(
          shiny$need(
            valid_folder,
            "No valid target folder selected ..."
          )
        )
      } else if (deconvolution_sidebar_vars$selected() == "file") {
        valid_file <- (length(deconvolution_sidebar_vars$file()) &&
          grepl(
            "\\.raw$",
            deconvolution_sidebar_vars$file(),
            ignore.case = TRUE
          ) &&
          dir.exists(deconvolution_sidebar_vars$file()))

        shiny$validate(
          shiny$need(
            valid_file,
            "No valid target file selected ..."
          )
        )
      }

      shiny$actionButton(ns("deconvolute_start"), "Run Deconvolution")
    })

    ### Functions ----
    #### check_progress ----
    check_progress <- function(raw_dirs) {
      message("Checking progress at: ", Sys.time())
      fin_dirs <- file.path(
        deconvolution_sidebar_vars$targetpath(),
        basename(gsub(
          ".raw",
          "_rawdata_unidecfiles",
          raw_dirs
        ))
      )
      peak_files <- file.path(fin_dirs, "plots.rds")
      finished_files <- file.exists(peak_files)
      count <- sum(finished_files)
      message("Found files: ", count)

      count
    }

    #### reset_progress ----
    reset_progress <- function() {
      reactVars$is_running <- FALSE
      reactVars$heatmap_ready <- FALSE
      reactVars$completed_files <- 0
      reactVars$sample_names <- NULL
      reactVars$wells <- NULL
      reactVars$rslt_df <- data.frame()
      reactVars$last_check <- Sys.time()
      reactVars$results_last_check <- Sys.time()
      reactVars$heatmap_ready <- FALSE
      reactVars$deconv_report_status <- NULL

      decon_rep_process_data(NULL)

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

    # Dynamic rendering of skip/overwrite selection button
    output$selector_ui <- shiny$renderUI({
      input$deconvolute_start

      select <- NULL

      if (deconvolution_sidebar_vars$selected() == "folder") {
        finished_files <- dir_ls(
          deconvolution_sidebar_vars$targetpath(),
          glob = "*_rawdata_unidecfiles"
        )

        if (
          isTRUE(deconvolution_sidebar_vars$batch_mode()) &&
            length(deconvolution_sidebar_vars$batch_file())
        ) {
          batch_sel <- gsub(
            ".raw",
            "_rawdata_unidecfiles",
            deconvolution_sidebar_vars$batch_file()[[deconvolution_sidebar_vars$id_column()]]
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

    # Dynamic rendering of message with info for selected files
    output$message_ui <- shiny$renderUI({
      input$deconvolute_start
      enable(
        selector = "#app-deconvolution_main-deconvolute_start_conf"
      )
      # shinyjs::addClass("deconvolute_start_conf", "btn-highlight")

      message <- NULL

      if (deconvolution_sidebar_vars$selected() == "folder") {
        raw_dirs <- dir_ls(
          deconvolution_sidebar_vars$dir(),
          glob = "*.raw"
        )

        if (
          isTRUE(deconvolution_sidebar_vars$batch_mode()) &
            length(deconvolution_sidebar_vars$batch_file())
        ) {
          presence <- deconvolution_sidebar_vars$batch_file()[[deconvolution_sidebar_vars$id_column()]] %in%
            basename(raw_dirs)

          if (all(presence)) {
            message <- shiny$p(
              shiny$HTML(
                paste0(
                  "<b>Multiple target file(s) selected</b><br><br><b>",
                  nrow(deconvolution_sidebar_vars$batch_file()),
                  "</b> raw file(s) present in ",
                  "the batch file are queried for deconvolution."
                )
              )
            )
          } else if (sum(presence) == 0) {
            disable(
              selector = "#app-deconvolution_main-deconvolute_start_conf"
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
          if (
            any(duplicated(deconvolution_sidebar_vars$batch_file()[[deconvolution_sidebar_vars$id_column()]]))
          ) {
            disable(
              selector = "#app-deconvolution_main-deconvolute_start_conf"
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
              selector = "#app-deconvolution_main-deconvolute_start_conf"
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
        name <- basename(deconvolution_sidebar_vars$file())

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

    # Dynamic rendering of warnings for file selection
    output$warning_ui <- shiny$renderUI({
      input$deconvolute_start

      warning <- NULL
      reactVars$overwrite <- FALSE

      # Get finished files in destination path
      finished_files <- dir_ls(
        deconvolution_sidebar_vars$targetpath(),
        glob = "*_rawdata_unidecfiles"
      )

      if (deconvolution_sidebar_vars$selected() == "folder") {
        if (
          isTRUE(deconvolution_sidebar_vars$batch_mode()) &&
            length(deconvolution_sidebar_vars$batch_file())
        ) {
          # check if any duplicated targets in batch
          if (
            any(duplicated(deconvolution_sidebar_vars$batch_file()[[deconvolution_sidebar_vars$id_column()]]))
          ) {
            dup_count <- sum(duplicated(deconvolution_sidebar_vars$batch_file()[[deconvolution_sidebar_vars$id_column()]]))
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
              deconvolution_sidebar_vars$batch_file()[[deconvolution_sidebar_vars$id_column()]]
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
        gsub(
          ".raw",
          "_rawdata_unidecfiles",
          basename(deconvolution_sidebar_vars$file())
        ) %in%
          basename(finished_files)
      ) {
        reactVars$overwrite <- gsub(
          ".raw",
          "_rawdata_unidecfiles",
          deconvolution_sidebar_vars$file()
        )
        reactVars$duplicated <- "Overwrite Files"
        warning <- shiny$p(
          shiny$HTML(
            paste0(
              '<i class="fa-solid fa-circle-exclamation" style="font-size:1e',
              'm; color:black; margin-right: 10px;"></i>',
              "The file queried for deconvolution appears to have already",
              " been processed. Choosing to continue will overwrite",
              " the present result."
            )
          )
        )
      }

      return(warning)
    })

    # Dynamic rendering of target file selector
    output$target_sel_ui <- shiny$renderUI({
      input$deconvolute_start

      picker <- NULL

      if (
        deconvolution_sidebar_vars$selected() == "folder" &&
          (isFALSE(deconvolution_sidebar_vars$batch_mode()) ||
            length(deconvolution_sidebar_vars$batch_file()) == 0)
      ) {
        picker <- pickerInput(
          ns("target_selector"),
          "",
          choices = basename(dir_ls(
            deconvolution_sidebar_vars$dir(),
            glob = "*.raw"
          )),
          selected = basename(dir_ls(
            deconvolution_sidebar_vars$dir(),
            glob = "*.raw"
          )),
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

    # Observe choice for overwrite/skip already present results
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

      # UI changes
      runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "block";'
      ))
      runjs(paste0(
        "document.getElementById('deconvolution_main-deconvo",
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
      if (deconvolution_sidebar_vars$selected() == "folder") {
        raw_dirs <- list.dirs(
          deconvolution_sidebar_vars$dir(),
          full.names = TRUE,
          recursive = FALSE
        )
        raw_dirs <- raw_dirs[grep("\\.raw$", raw_dirs)]

        if (
          isTRUE(deconvolution_sidebar_vars$batch_mode()) &&
            length(deconvolution_sidebar_vars$batch_file())
        ) {
          write_log("Multiple target deconvolution mode (with batch file)")

          batch <- deconvolution_sidebar_vars$batch_file()
          sample_names <- batch[[deconvolution_sidebar_vars$id_column()]]
          raw_dirs <- raw_dirs[basename(raw_dirs) %in% sample_names]

          # Prepare heatmap variables
          reactVars$sample_names <- gsub(
            ".raw",
            "",
            deconvolution_sidebar_vars$batch_file()[[deconvolution_sidebar_vars$id_column()]]
          )
          reactVars$wells <- gsub(
            ",",
            "",
            sub("^.*:", "", batch[[deconvolution_sidebar_vars$vial_column()]])
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
      } else if (deconvolution_sidebar_vars$selected() == "file") {
        write_log("Single target deconvolution mode")
        raw_dirs <- deconvolution_sidebar_vars$file()
        write_log(paste("Target:", deconvolution_sidebar_vars$file()))
      }
      write_log(paste(
        "Destination path:",
        deconvolution_sidebar_vars$targetpath()
      ))

      # Overwrite or skip already present result dirs
      if (!isFALSE(reactVars$overwrite)) {
        if (reactVars$duplicated == "Overwrite Files") {
          # Remove result files and dirs
          rslt_dirs <- file.path(
            deconvolution_sidebar_vars$targetpath(),
            basename(reactVars$overwrite)
          )

          if (deconvolution_sidebar_vars$selected() == "file") {
            write_log(paste("Overwriting existing", rslt_dirs))
          } else {
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

      # Initialization variables
      reactVars$is_running <- TRUE
      reactVars$catch_error <- FALSE
      reactVars$expected_files <- length(raw_dirs)
      reactVars$initial_file_count <- check_progress(raw_dirs)
      message("Initial file count: ", reactVars$initial_file_count)

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
        selected = deconvolution_sidebar_vars$selected()
      )

      # Place config parameter in temporary file
      config_path <- file.path(temp, "config.rds")
      saveRDS(config, config_path)

      # Initiate output file
      reactVars$decon_process_out <- file.path(temp, "output.txt")
      write("", reactVars$decon_process_out)

      # Launch external deconvolution process
      # tryCatch(
      #   {
      rx_process <- process$new(
        "Rscript.exe",
        args = c(
          "app/logic/deconvolution_execute.R",
          temp,
          log_path,
          getwd(),
          deconvolution_sidebar_vars$targetpath()
        ),
        stdout = reactVars$decon_process_out,
        stderr = reactVars$decon_process_out
      )
      #   },
      #   error = function(e) {
      #     # Activate error catching variable
      #     reactVars$catch_error <- TRUE

      #     # Stop spinner for spectrum and heatmap plot
      #     waiter_hide(id = ns("heatmap"))
      #     waiter_hide(id = ns("spectrum"))

      #     error_msg <- paste("Failed to start deconvolution:", e$message)
      #     write_log(error_msg)

      #     # Show error notification
      #     shiny$showNotification(
      #       error_msg,
      #       type = "error",
      #       duration = 5
      #     )
      #   }
      # )

      # Abort deconvolution if process initiation fails
      if (reactVars$catch_error == TRUE) {
        # Reset reactive error catch variable
        reactVars$catch_error <- FALSE

        # Set reactive status variables
        reactVars$is_running <- FALSE
        reactVars$deconv_report_status <- NULL

        # End mouse pointer blocking overlay
        runjs(paste0(
          'document.getElementById("blocking-overlay").style.display ',
          '= "none";'
        ))

        # Stop execution of following expressions
        return()
      }

      # Track process metadata in reactive variable
      decon_process_data(rx_process)

      # Track process exit status for errors
      shiny$observe({
        shiny$req(decon_process_data())

        if (isTRUE(reactVars$is_running)) {
          shiny$invalidateLater(2000)

          # Check if the process is still alive
          if (!decon_process_data()$is_alive()) {
            # Retrieve exit status
            exit_status <- decon_process_data()$get_exit_status()

            # Check if the exit status indicates an error (non-zero)
            if (exit_status != 0) {
              write_log("Error in deconvolution execution")

              # Change UI elements to indicate error
              shiny$updateActionButton(
                session,
                "deconvolute_end",
                label = "Reset",
                icon = shiny$icon("repeat")
              )

              updateProgressBar(
                session = session,
                id = ns("progressBar"),
                value = 0,
                title = "Deconvolution aborted ..."
              )

              hide(selector = "#app-deconvolution_main-processing")
              show(selector = "#app-deconvolution_main-processing_error")

              shiny$showNotification(
                "Deconvolution execution failed",
                type = "error",
                duration = 5
              )

              delay(
                1000,
                runjs(
                  "document.querySelector('#app-deconvolution_main-show_log').click();"
                )
              )

              # Stop spinner for spectrum and heatmap plot
              waiter_hide(id = ns("heatmap"))
              waiter_hide(id = ns("spectrum"))

              # Stop observers
              if (!is.null(reactVars$progress_observer)) {
                reactVars$progress_observer$destroy()
              }
              if (!is.null(reactVars$process_observer)) {
                reactVars$process_observer$destroy()
              }
              if (
                deconvolution_sidebar_vars$selected() == "folder" &&
                  !is.null(reactVars$results_observer)
              ) {
                reactVars$results_observer$destroy()
              }
            }

            # Set reactive status variables
            reactVars$is_running <- FALSE
            reactVars$deconv_report_status <- NULL
          }
        }
      })

      # Log deconvolution initiation parameter
      write_log("Deconvolution started")
      formatted_params <- apply(config$params, 1, function(row) {
        paste(names(config$params), row, sep = " = ", collapse = " | ")
      })
      write_log(paste(
        "Deconvolution parameters:\n",
        paste(formatted_params, collapse = "\n")
      ))

      # On app close kill deconvolution process and log status
      reactVars$process_observer <- shiny$observe({
        proc <- decon_process_data()

        completed_files <- reactVars$completed_files
        expected_files <- reactVars$expected_files

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
      if (deconvolution_sidebar_vars$selected() == "folder") {
        reactVars$results_observer <- shiny$observe({
          shiny$invalidateLater(10000)

          runjs(paste0(
            'document.getElementById("blocking-overlay").style.display ',
            '= "block";'
          ))

          if (
            difftime(
              Sys.time(),
              reactVars$results_last_check,
              units = "secs"
            ) >=
              10
          ) {
            if (
              isTRUE(deconvolution_sidebar_vars$batch_mode()) &&
                length(deconvolution_sidebar_vars$batch_file()) &&
                nrow(reactVars$rslt_df) < reactVars$completed_files
            ) {
              shiny$req(reactVars$sample_names, reactVars$wells)

              results_all <- dir_ls(
                deconvolution_sidebar_vars$targetpath(),
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

                ##### Render result picker with updated choices ----
                if (nrow(reactVars$rslt_df) > 0) {
                  enable(selector = "#app-deconvolution_main-toggle_result")

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
                }
              }
            } else {
              selected_files <- file.path(
                deconvolution_sidebar_vars$dir(),
                target_selector_sel()
              )
              fin_dirs <- file.path(
                deconvolution_sidebar_vars$targetpath(),
                basename(gsub(
                  ".raw",
                  "_rawdata_unidecfiles",
                  selected_files
                ))
              )
              peak_files <- file.path(fin_dirs, "plots.rds")
              finished_files <- file.exists(peak_files)

              if (sum(finished_files) > 0) {
                choices <- basename(selected_files)[finished_files]
                selected <- ifelse(
                  is.null(result_files_sel()),
                  choices[1],
                  result_files_sel()
                )

                enable(selector = "#app-deconvolution_main-toggle_result")

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

            reactVars$results_last_check <- Sys.time()
          }

          runjs(paste0(
            'document.getElementById("blocking-overlay").style.display ',
            '= "none";'
          ))
        })
      }

      #### Progress tracking observer ----
      reactVars$progress_observer <- shiny$observe({
        shiny$invalidateLater(1000)

        if (difftime(Sys.time(), reactVars$last_check, units = "secs") >= 0.5) {
          reactVars$current_total_files <- check_progress(raw_dirs)
          reactVars$completed_files <-
            reactVars$current_total_files - reactVars$initial_file_count
          reactVars$last_check <- Sys.time()

          progress_pct <- min(
            100,
            round(
              100 * reactVars$completed_files / reactVars$expected_files
            )
          )

          message(
            "Updating progress: ",
            progress_pct,
            "% (",
            reactVars$completed_files,
            "/",
            reactVars$expected_files,
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
                reactVars$completed_files,
                reactVars$expected_files
              ),
              paste0(rep(".", reactVars$count), collapse = "")
            )
          } else {
            title <- paste0(
              "Saving Results ",
              paste0(rep(".", reactVars$count), collapse = "")
            )

            result_files <- file.path(
              deconvolution_sidebar_vars$targetpath(),
              basename(gsub(".raw", "_rawdata_unidecfiles", raw_dirs))
            )

            # Check if deconvolution finished for all target files
            if (
              all(file.exists(file.path(result_files, "plots.rds"))) &&
                file.exists(file.path(
                  deconvolution_sidebar_vars$targetpath(),
                  gsub(
                    ".log",
                    "_RESULT.rds",
                    basename(log_path)
                  )
                ))
            ) {
              # Stop observers
              if (!is.null(reactVars$progress_observer)) {
                reactVars$progress_observer$destroy()
              }
              if (!is.null(reactVars$process_observer)) {
                reactVars$process_observer$destroy()
              }
              if (
                deconvolution_sidebar_vars$selected() == "folder" &&
                  !is.null(reactVars$results_observer)
              ) {
                reactVars$results_observer$destroy()
              }

              # Set reactive status variable "is_running" to FALSE
              reactVars$is_running <- FALSE

              # final result check for heatmap update
              if (
                deconvolution_sidebar_vars$selected() == "folder" &&
                  isTRUE(deconvolution_sidebar_vars$batch_mode()) &&
                  length(deconvolution_sidebar_vars$batch_file())
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
                if (!file.exists(file.path(temp, "heatmap.rds"))) {
                  heatmap <- create_384_plate_heatmap(reactVars$rslt_df)
                  saveRDS(heatmap, file.path(temp, "heatmap.rds"))
                }
              } else {
                if (deconvolution_sidebar_vars$selected() == "folder") {
                  selected_files <- file.path(
                    deconvolution_sidebar_vars$dir(),
                    target_selector_sel()
                  )
                } else {
                  selected_files <- raw_dirs
                }

                fin_dirs <- file.path(
                  deconvolution_sidebar_vars$targetpath(),
                  basename(gsub(".raw", "_rawdata_unidecfiles", selected_files))
                )
                peak_files <- file.path(fin_dirs, "plots.rds")
                finished_files <- file.exists(peak_files)

                if (sum(finished_files) > 0) {
                  # Enable spectrum toggle button
                  enable(selector = "#app-deconvolution_main-toggle_result")

                  # Update choices and selected sample of results picker
                  choices <- basename(selected_files)[finished_files]

                  selected <- ifelse(
                    is.null(result_files_sel()),
                    choices[1],
                    result_files_sel()
                  )

                  # Render sample picker with updated choices
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

              # update "Abort" button to "Reset"
              shiny$updateActionButton(
                session,
                "deconvolute_end",
                label = "Reset",
                icon = shiny$icon("repeat")
              )

              # Change progress bar title to "Finalized!"
              title <- "Finalized!"

              # Enable deconvolution report
              reactVars$deconv_report_status <- "idle"
              enable(
                selector = "#app-deconvolution_main-deconvolution_report"
              )

              # Enable continuation button to protein conversion
              enable(
                selector = "#app-deconvolution_main-forward_deconvolution"
              )

              # Change spinner to finished
              hide(selector = "#app-deconvolution_main-processing")
              show(selector = "#app-deconvolution_main-processing_fin")

              write_log("Deconvolution finalized")
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
        deconvolution_sidebar_vars$selected() == "folder" &&
          isTRUE(deconvolution_sidebar_vars$batch_mode()) &&
          length(deconvolution_sidebar_vars$batch_file())
      ) {
        # Observe clicks on interactive heatmap to show spectra
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
      # Toggle to hide sidebar
      runjs("document.querySelector('button.collapse-toggle').click();")
      output$deconvolution_init_ui <- NULL

      # Conditional rendering of running deconvolution UI
      output$deconvolution_running_ui <- shiny$renderUI({
        if (
          deconvolution_sidebar_vars$selected() == "folder" &&
            isTRUE(deconvolution_sidebar_vars$batch_mode()) &&
            length(deconvolution_sidebar_vars$batch_file())
        ) {
          deconvolution_running_ui_plate(ns)
        } else {
          deconvolution_running_ui_noplate(ns)
        }
      })

      # Render status spinner icon
      delay(1000, show(selector = "#app-deconvolution_main-processing"))

      ### Render result spectrum

      # Define reactive helper variable to control spinner display
      allow_spinner_spectrum <- shiny$reactiveVal(TRUE)

      output$spectrum <- renderPlotly({
        # Show spinner only once before plot fully rendered
        if (shiny$isolate(allow_spinner_spectrum()) == TRUE) {
          waiter_show(id = ns("spectrum"), html = spin_wave())
          allow_spinner_spectrum(FALSE)
        }

        shiny$req(result_files_sel(), input$toggle_result)

        if (deconvolution_sidebar_vars$selected() == "folder") {
          result_dir <- file.path(
            deconvolution_sidebar_vars$targetpath(),
            gsub(".raw", "_rawdata_unidecfiles", result_files_sel())
          )
        } else if (deconvolution_sidebar_vars$selected() == "file") {
          result_dir <- file.path(
            deconvolution_sidebar_vars$targetpath(),
            basename(gsub(
              ".raw",
              "_rawdata_unidecfiles",
              deconvolution_sidebar_vars$file()
            ))
          )
        }

        if (dir.exists(result_dir)) {
          # Generate the spectrum plot
          spectrum <- spectrum_plot(
            result_path = result_dir,
            raw = as.logical(input$toggle_result)
          )

          # Hide spinner and activate reactive spinner variable again
          waiter_hide(id = ns("spectrum"))
          allow_spinner_spectrum(TRUE)

          return(spectrum)
        }
      })

      ### Render heatmap for batch mode
      if (
        deconvolution_sidebar_vars$selected() == "folder" &&
          isTRUE(deconvolution_sidebar_vars$batch_mode()) &&
          length(deconvolution_sidebar_vars$batch_file())
      ) {
        # Define reactive helper variable to control spinner display
        allow_spinner_heatmap <- shiny$reactiveVal(TRUE)

        output$heatmap <- renderPlotly({
          if (shiny$isolate(allow_spinner_heatmap()) == TRUE) {
            waiter_show(
              id = ns("heatmap"),
              html = spin_wave()
            )
            allow_spinner_heatmap(FALSE)
          }

          shiny$req(reactVars$rslt_df)

          if (nrow(reactVars$rslt_df) > 0) {
            heatmap <- create_384_plate_heatmap(reactVars$rslt_df) |>
              event_register("plotly_click")

            # Hide spinner
            waiter_hide(id = ns("heatmap"))

            # Activate spinner reactivation
            allow_spinner_heatmap(TRUE)

            # Activate click observer
            reactVars$heatmap_ready <- TRUE

            return(heatmap)
          }
        })
      }

      # Unblock mouse pointer
      runjs(paste0(
        'document.getElementById("blocking-overlay").style.display ',
        '= "none";'
      ))
    })

    ### Event end/reset deconvolution ----
    shiny$observeEvent(input$deconvolute_end, {
      if (reactVars$is_running) {
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
        # Block mouse pointer
        runjs(paste0(
          'document.getElementById("blocking-overlay").styl',
          'e.display = "block";'
        ))

        # Hide status indication spinners
        hide(selector = "#app-deconvolution_main-processing")
        hide(selector = "#app-deconvolution_main-processing_stop")
        hide(selector = "#app-deconvolution_main-processing_fin")

        # Stop observers
        if (!is.null(reactVars$progress_observer)) {
          reactVars$progress_observer$destroy()
        }
        if (!is.null(reactVars$process_observer)) {
          reactVars$process_observer$destroy()
        }
        if (
          deconvolution_sidebar_vars$selected() == "folder" &&
            !is.null(reactVars$results_observer)
        ) {
          reactVars$results_observer$destroy()
        }

        # Reset reactive status variables
        reset_progress()

        # Null dynamic UI
        output$decon_rep_logtext <- NULL
        output$decon_rep_logtext_ui <- NULL
        output$deconvolution_running_ui <- NULL
        output$heatmap <- NULL

        # Render deconvolution initiation UI
        output$deconvolution_init_ui <- shiny$renderUI(
          deconvolution_init_ui(ns)
        )

        # Unblock mouse pointer
        runjs(paste0(
          'document.getElementById("blocking-overlay").styl',
          'e.display = "none";'
        ))

        # Toggle sidebar for deconvolution initiation UI
        runjs(paste0(
          "document.querySelector('.bslib-sidebar-layout.sidebar-coll",
          "apsed>.collapse-toggle').style.display = 'block';"
        ))
        runjs("document.querySelector('button.collapse-toggle').click();")

        # Signal sidebar module to reevaluate
        reset_button(reset_button() + 1)

        write_log("Deconvolution resetted")
      }
    })

    # Manually cancelled deconvolution
    shiny$observeEvent(input$deconvolute_end_conf, {
      # Kill system process
      proc <- decon_process_data()
      if (!is.null(proc) && proc$is_alive()) {
        proc$kill_tree()
      }

      # Update progress bar to show cancellation
      updateProgressBar(
        session = session,
        id = ns("progressBar"),
        value = 0,
        title = "Processing aborted"
      )

      # Change spinner icons to stop
      hide(selector = "#app-deconvolution_main-processing")
      show(selector = "#app-deconvolution_main-processing_stop")

      # Update button to show "Reset"
      shiny$updateActionButton(
        session,
        "deconvolute_end",
        label = "Reset",
        icon = shiny$icon("repeat")
      )

      # Stop observers
      if (!is.null(reactVars$progress_observer)) {
        reactVars$progress_observer$destroy()
      }
      if (
        deconvolution_sidebar_vars$selected() == "folder" &&
          !is.null(reactVars$results_observer)
      ) {
        reactVars$results_observer$destroy()
      }
      if (!is.null(reactVars$process_observer)) {
        reactVars$process_observer$destroy()
      }

      # Set reactive status variable "is_running" to FALSE
      reactVars$is_running <- FALSE

      # Remove modal dialogue window
      shiny$removeModal()

      # Stop spinner for spectrum and heatmap plot
      waiter_hide(id = ns("spectrum"))
      waiter_hide(id = ns("heatmap"))

      write_log(paste(
        "Deconvolution cancelled with",
        reactVars$completed_files,
        "out of",
        reactVars$expected_files,
        "target(s) completed"
      ))
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

      delay(2000, runjs("App.smartScroll('deconvolution_main-logtext')"))
    })

    #### Save log ----
    output$save_deconvolution_log <- shiny$downloadHandler(
      filename = function() {
        paste0("deconvolution_SESSION", get_session_id(), ".txt")
      },
      content = function(file) {
        file.copy(reactVars$decon_process_out, file)
      }
    )

    #### Clip log ----
    shiny$observeEvent(input$copy_deconvolution_log, {
      shiny$req(reactVars$deconvolution_log)

      shinyjs::runjs("alert('Log copied to clipboard!');")
      write_clip(reactVars$deconvolution_log, allow_non_interactive = TRUE)
    })

    ### Report events ----
    shiny$observeEvent(input$deconvolution_report, {
      if (reactVars$deconv_report_status == "running") {
        label <- "Cancel"
      } else if (reactVars$deconv_report_status == "finished") {
        label <- "Open"
      } else if (reactVars$deconv_report_status == "error") {
        label <- "Cancel"
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

      # Activate smart scroll on reevaluating logtext field
      delay(
        2000,
        runjs("App.smartScroll('deconvolution_main-decon_rep_logtext')")
      )
    })

    # Actions on make report action button
    shiny$observeEvent(
      input$make_deconvolution_report,
      {
        if (reactVars$deconv_report_status != "error") {
          # If report generation active button clicks cancel the process
          if (reactVars$deconv_report_status == "running") {
            # Kill system process
            proc <- decon_rep_process_data()
            if (!is.null(proc) && proc$is_alive()) {
              write_log("Deconvolution report generation cancelled")

              proc$kill_tree()
            }

            # Update progress bar title and value
            updateProgressBar(
              session = session,
              id = ns("progressBar"),
              value = 0,
              title = "Report Generation Aborted"
            )

            # Null dynamic report UI
            output$decon_rep_logtext <- NULL
            output$decon_rep_logtext_ui <- NULL

            hide(selector = "#app-deconvolution_main-processing")
            show(selector = "#app-deconvolution_main-processing_stop")

            # Set reactive report status variable to "idle"
            reactVars$deconv_report_status <- "idle"

            # Close modal dialogue window
            shiny$removeModal()
          } else if (reactVars$deconv_report_status == "finished") {
            # Define report filename
            filename <- gsub(
              ".log",
              "_deconvolution_report.html",
              basename(log_path)
            )
            filename_path <- file.path(
              deconvolution_sidebar_vars$targetpath(),
              filename
            )

            # If report generation successfully finished open the report on button click
            if (file.exists(filename_path)) {
              utils::browseURL(filename_path)
            }

            # Close modal dialogue window
            shiny$removeModal()
          } else {
            # If report generation not yet initiated or idle then initate report generation on button click
            write_log("Deconvolution report generation initiated")

            # Render logtext UI
            output$decon_rep_logtext_ui <- shiny$renderUI(
              shiny$verbatimTextOutput(ns("decon_rep_logtext"))
            )

            # Render report generation logtext and progress bar
            output$decon_rep_logtext <- shiny$renderText({
              shiny$req(reactVars$deconv_report_status)

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

              # Define report filename
              filename <- gsub(
                ".log",
                "_deconvolution_report.html",
                basename(log_path)
              )
              filename_path <- file.path(
                deconvolution_sidebar_vars$targetpath(),
                filename
              )

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

              # Update progress bar according to report render progress
              if (reactVars$deconv_report_status != "error") {
                updateProgressBar(
                  session = session,
                  id = ns("progressBar"),
                  value = tail(as.integer(gsub("%", "", value)), 1),
                  title = title
                )
              } else {
                updateProgressBar(
                  session = session,
                  id = ns("progressBar"),
                  value = tail(as.integer(gsub("%", "", value)), 1),
                  title = "Report Generation Failed."
                )
              }

              clean_log
            })

            # Initialization variables
            reactVars$deconv_report_status <- "running"
            reactVars$catch_error <- FALSE

            # Define temporary output file location
            reactVars$decon_rep_process_out <- file.path(
              temp,
              "rep_output.txt"
            )
            write("", reactVars$decon_rep_process_out)

            # Render report generation interface
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
                          '#7777f9; margin-top: 0.25em"></i>'
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

            # Save report input settings
            if (isTRUE(input$decon_save)) {
              # Set up settings directory
              if (!dir.exists(settings_dir)) {
                dir.create(settings_dir, recursive = TRUE)
              }

              rep_input <- c(
                input$decon_rep_title,
                input$decon_rep_author,
                input$decon_rep_desc
              )

              # Save report settings in user settings
              saveRDS(
                rep_input,
                file.path(settings_dir, "decon_rep_settings.rds")
              )
            }

            ### Prepare process parameter
            # Get isolated session id
            session_id <- regmatches(
              basename(log_path),
              regexpr("id\\d+", basename(log_path))
            )

            # Get user documents path to retrieve files needed for report generation
            script_dir <- file.path(
              Sys.getenv("USERPROFILE"),
              "Documents",
              "KiwiFlow",
              "report"
            )

            # Set html report output filename
            output_file <- paste0(
              "deconvolution_report_",
              Sys.Date(),
              "_",
              session_id,
              ".html"
            )

            # Summarize args in vector
            args <- c(
              "deconvolution_report.R",
              fill_empty(input$decon_rep_title),
              fill_empty(input$decon_rep_author),
              fill_empty(input$decon_rep_desc),
              output_file,
              log_path,
              deconvolution_sidebar_vars$targetpath(),
              get_kiwiflow_version()["version"],
              get_kiwiflow_version()["date"],
              temp
            )

            # Construct the system command
            cmd <- paste(
              "conda activate kiwiflow &&",
              "cd",
              script_dir,
              "&& Rscript",
              paste(args, collapse = " ")
            )

            # Start external report generation process
            tryCatch(
              {
                rep_process <- process$new(
                  command = "cmd.exe",
                  args = c("/c", cmd),
                  stdout = reactVars$decon_rep_process_out,
                  stderr = reactVars$decon_rep_process_out
                )

                # Track report generation process status in reactive variable
                decon_rep_process_data(rep_process)
              },
              error = function(e) {
                # Activate error catching variable
                reactVars$catch_error <- TRUE

                # Get error message
                error_msg <- paste(
                  "Failed to initiate report generation:",
                  e$message
                )

                write_log(error_msg)

                # Display error message on modal window
                output$decon_rep_logtext <- shiny$renderText(error_msg)
              }
            )

            # Abort deconvolution if process initiation fails
            if (reactVars$catch_error == TRUE) {
              # Reset reactive error catch variable
              reactVars$catch_error <- FALSE

              # Set report generation status to "idle"
              reactVars$deconv_report_status <- "error"

              # Show error notification
              shiny$showNotification(
                "Report generation failed",
                type = "error",
                duration = 5
              )

              # Stop execution of following expressions
              return()
            }
          }

          # Activate smart scroll on reevaluating logtext
          delay(
            2000,
            runjs(
              "App.smartScroll('deconvolution_main-decon_rep_logtext')"
            )
          )
        } else {
          # If report generation erroneous remove modal on button click
          shiny$removeModal()
        }
      }
    )

    # Track process exit status for errors in report generation
    shiny$observe({
      shiny$req(
        decon_rep_process_data(),
        reactVars$deconv_report_status
      )

      if (reactVars$deconv_report_status == "running") {
        shiny$invalidateLater(2000)

        # Check if the process is still alive
        if (!decon_rep_process_data()$is_alive()) {
          # Retrieve exit status
          exit_status <- decon_rep_process_data()$get_exit_status()

          # Check if the exit status indicates an error (non-zero)
          if (exit_status != 0) {
            write_log("Failed to generate deconvolution report")

            reactVars$deconv_report_status <- "error"
            decon_rep_process_data(NULL)
          }
        }
      }
    })

    # Observe report generation to adapt UI
    shiny$observe({
      shiny$req(reactVars$deconv_report_status)

      if (reactVars$deconv_report_status == "idle") {
        # Report generation UI when idle
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
      } else if (reactVars$deconv_report_status == "running") {
        # Report generation UI when running
        hide(selector = "#app-deconvolution_main-processing_stop")
        hide(selector = "#app-deconvolution_main-processing_fin")
        show(selector = "#app-deconvolution_main-processing")

        runjs("App.disableDismiss()")

        shiny$updateActionButton(
          session,
          "make_deconvolution_report",
          label = "Cancel"
        )
      } else if (reactVars$deconv_report_status == "finished") {
        # Report generation UI when finished

        write_log("Deconvolution report generation finalized")

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
                      '#7777f9; margin-top: 0.5em"></i>'
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

        hide(selector = "#app-deconvolution_main-processing")
        show(selector = "#app-deconvolution_main-processing_fin")

        runjs("App.enableDismiss()")

        updateProgressBar(
          session = session,
          id = ns("progressBar"),
          value = 100,
          title = "Report generated!"
        )

        shiny$updateActionButton(
          session,
          "make_deconvolution_report",
          label = "Open"
        )
      } else if (reactVars$deconv_report_status == "error") {
        # Report generation UI when report erroneous
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
                      '<i class="fa-solid fa-circle-exclamation fa-2x" style="color: ',
                      '#D17050; margin-top: -4px;"></i>'
                    )
                  )
                )
              ),
              shiny$column(
                width = 10,
                shiny$p(
                  "Report generation process failed ..."
                )
              )
            )
          )
        )

        hide(selector = "#app-deconvolution_main-processing")
        hide(selector = "#app-deconvolution_main-processing_fin")
        show(
          selector = "#app-deconvolution_main-processing_error"
        )

        runjs("App.enableDismiss()")

        shiny$updateActionButton(
          session,
          "make_deconvolution_report",
          label = "Cancel"
        )
      }
    })

    # Event continue to protein conversion
    shiny$observeEvent(input$forward_deconvolution, {
      # Return result file path as module output
      reactVars$continue_conversion <- file.path(
        deconvolution_sidebar_vars$targetpath(),
        gsub(".log", "_RESULT.rds", basename(log_path))
      )

      # Disable continuation/forward button
      shinyjs::disable("forward_deconvolution")
    })

    # Event continuation and overwrite present sample table cancelled
    shiny::observeEvent(conversion_main_vars$cancel_continuation(), {
      # Reenable continuiation/forward button
      shinyjs::enable("forward_deconvolution")

      # Set "continue_conversion" reactive variable back to NULL
      reactVars$continue_conversion <- NULL
    })

    ### Tooltip events ----
    shiny$observeEvent(input$detection_window_tooltip_bttn, {
      shiny$showModal(
        shiny$div(
          class = "start-modal",
          shiny$modalDialog(
            shiny$fluidRow(
              shiny$br(),
              shiny$column(
                width = 11,
                shiny$div(
                  class = "tooltip-text",
                  "The peak detection range specifies the local window to consider when detecting a peak. A peak needs to be the local max within a window of +/- this range to be considered a peak. For example, if you set the window as 10 Da, only peaks within a window of +/- 10 Da will be considered peak. Any other local maximum are ignored."
                ),
                shiny$br(),
                shiny$a(
                  href = "https://github.com/michaelmarty/UniDec/wiki/Peak-Selection-and-Plotting#picking-peaks",
                  "UniDec Wiki - Picking Peaks",
                  target = "_blank"
                )
              )
            ),
            title = "Peak Detection Range (Da)",
            easyClose = TRUE,
            footer = shiny$tagList(
              shiny$modalButton("Dismiss")
            )
          )
        )
      )
    })

    shiny$observeEvent(input$threshold_tooltip_bttn, {
      shiny$showModal(
        shiny$div(
          class = "start-modal",
          shiny$modalDialog(
            shiny$fluidRow(
              shiny$br(),
              shiny$column(
                width = 11,
                shiny$div(
                  class = "tooltip-text",
                  "The peak detection threshold specifies how tall the relative peak height (normalized to a max spectrum intensity of 1) needs to be to considered a peak. For example, a threshold of 0.1 would mean that any peaks below a 10% max intensity would be ignored. If you set this to 0, any local maximum (within the defined detection range) are counted."
                ),
                shiny$br(),
                shiny$a(
                  href = "https://github.com/michaelmarty/UniDec/wiki/Peak-Selection-and-Plotting#picking-peaks",
                  "UniDec Wiki - Picking Peaks",
                  target = "_blank"
                )
              )
            ),
            title = "Peak Detection Threshold",
            easyClose = TRUE,
            footer = shiny$tagList(
              shiny$modalButton("Dismiss")
            )
          )
        )
      )
    })

    shiny$observeEvent(input$charge_range_tooltip_bttn, {
      shiny$showModal(
        shiny$div(
          class = "start-modal",
          shiny$modalDialog(
            shiny$fluidRow(
              shiny$br(),
              shiny$column(
                width = 11,
                shiny$div(
                  class = "tooltip-text",
                  "The charge range sets a range of charges that can be assigned for the m/z peaks in the mass spectrum. If we set a minimum of 10 and a maximum of 25, then UniDec cannot assign a charge state of 9 or lower, nor a charge state of 26 or higher. Picking a charge range that does not include the true charge states for the m/z peaks will result in a distorted deconvolved mass spectrum or an error message. It is often better to start with a wider range of charge states and then narrow the range to the charge state distribution of interest. You can also narrow the charge range to remove artifacts."
                ),
                shiny$br(),
                shiny$a(
                  href = "https://github.com/michaelmarty/UniDec/wiki/Deconvolution-Parameters#charge-range",
                  "UniDec Wiki - Charge Range",
                  target = "_blank"
                )
              )
            ),
            title = "Charge Range",
            easyClose = TRUE,
            footer = shiny$tagList(
              shiny$modalButton("Dismiss")
            )
          )
        )
      )
    })

    shiny$observeEvent(input$mass_range_tooltip_bttn, {
      shiny$showModal(
        shiny$div(
          class = "start-modal",
          shiny$modalDialog(
            shiny$fluidRow(
              shiny$br(),
              shiny$column(
                width = 11,
                shiny$div(
                  class = "tooltip-text",
                  "Like the charge range, the mass range sets a range of masses, in Da, that can be assigned for the m/z peaks in the spectrum. Unlike zooming into the m/z range, the mass range sets the allowed deconvolved masses for the available data. Setting this mass range lower or higher than the true masses will either create artifacts, cut off certain analytes, or give an error message. Similar to the charge range, it is often better to start with a wider range then narrow the range later. Narrowing the mass range can help remove artifacts."
                ),
                shiny$br(),
                shiny$a(
                  href = "https://github.com/michaelmarty/UniDec/wiki/Deconvolution-Parameters#mass-range",
                  "UniDec Wiki - Mass Range",
                  target = "_blank"
                )
              )
            ),
            title = "Mass Range",
            easyClose = TRUE,
            footer = shiny$tagList(
              shiny$modalButton("Dismiss")
            )
          )
        )
      )
    })

    shiny$observeEvent(input$sample_rate_tooltip_bttn, {
      shiny$showModal(
        shiny$div(
          class = "start-modal",
          shiny$modalDialog(
            shiny$fluidRow(
              shiny$br(),
              shiny$column(
                width = 11,
                shiny$div(
                  class = "tooltip-text",
                  "The Sample Mass Every parameter sets a sample rate for the deconvolved mass spectrum. If a sample rate of 10 Da is set, then there will be a mass data point every 10 Da. Every mass data point would fall on an even 10, such that a data point would not appear at 66,417 Da but rather would appear at 66,420 Da in the resulting deconvolved mass spectrum. Thus, each peak will be rounded to the nearest 10 Da in this example. A sample rate of 1 Da would be needed to read a mass peak at 66,417 Da. Note: setting a smaller sample rate will slow down the algorithm in UniDec but will improve the precision."
                ),
                shiny$br(),
                shiny$a(
                  href = "https://github.com/michaelmarty/UniDec/wiki/Deconvolution-Parameters#sample-rate",
                  "UniDec Wiki - Sample Rate",
                  target = "_blank"
                )
              )
            ),
            title = "Sample Rate",
            easyClose = TRUE,
            footer = shiny$tagList(
              shiny$modalButton("Dismiss")
            )
          )
        )
      )
    })

    return(
      shiny::reactiveValues(
        forward_deconvolution = shiny::reactive(input$forward_deconvolution),
        continue_conversion = shiny::reactive(reactVars$continue_conversion)
      )
    )
  })
}
