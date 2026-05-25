# app/view/deconvolution_main.R

box::use(
  bslib[card, card_body, card_header, tooltip],
  DBI[dbConnect, dbDisconnect, dbExistsTable, dbGetQuery],
  RSQLite[SQLite, SQLITE_RO],
  fs[dir_ls],
  plotly[
    event_data,
    event_register,
    plotlyOutput,
    plotlyProxy,
    plotlyProxyInvoke,
    renderPlotly
  ],
  processx[process],
  shiny,
  shinyjs[delay, disable, disabled, enable, hide, show, hidden, runjs],
  shinyWidgets[
    progressBar,
    radioGroupButtons,
    updateProgressBar
  ],
  clipr[write_clip],
  utils[capture.output, head, tail],
  waiter[useWaiter, spin_wave, waiter_show, waiter_hide, withWaiter],
)

box::use(
  app /
    logic /
    deconvolution_functions[
      plate_heatmap,
      spectrum_plot,
      decon_progress_count,
      decon_is_complete,
      decon_failed_samples,
      process_plot_data_db,
      cleanup_wal
    ],
  app / logic / helper_functions[fill_empty, get_kiwims_version],
  app / logic / plot_download[setup_plot_dl, setup_table_dl],
  app / logic / logging[write_log, get_log, get_session_prefix],
  app / logic / user_settings[update_user_setting, read_user_settings],
  app / logic / conversion_functions[read_decon_metadata, read_decon_peaks_max],
  app /
    logic /
    deconvolution_ui[
      deconvolution_results_ui,
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

  shiny$tagList(
    shiny$tags$head(
      shiny$tags$script(src = "static/js/deconvolution.js")
    ),
    shiny$div(
      class = "deconvolution-ui-interface",
      shiny::div(
        id = ns("deconvolution_ui_container"),
        class = "conversion-main-spinner deconv-pre-init",
        shinycssloaders::withSpinner(
          shiny$uiOutput(ns("deconvolution_ui")),
          type = 1,
          color = "#7777f9"
        )
      )
    )
  )
}

#' @export
server <- function(
  id,
  deconvolution_sidebar_vars,
  conversion_main_vars,
  reset_button,
  config_file
) {
  shiny$moduleServer(id, function(input, output, session) {
    Sys.setenv(CONDA_DLL_SEARCH_MODIFICATION_ENABLE = "1")

    ns <- session$ns

    # Get kiwims user settings
    settings_dir <- file.path(
      Sys.getenv("LOCALAPPDATA"),
      "KiwiMS",
      "settings"
    )

    # Define log location
    log_path <- get_log()

    # Make temp dir for session
    temp <- tempdir()

    # deconv-pre-init is removed from inside renderUI so the JS fires in the
    # same WebSocket message as the rendered HTML — by the time the class is
    # removed the output is already in the DOM and no spinner flash occurs.

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
      failed_samples = character(0),
      logs = "",
      deconv_report_status = NULL,
      continue_conversion = NULL,
      stale_unidec_output = character(0),
      overwrite = character(0),
      duplicated = "Overwrite Samples"
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

    ### Smart analysis name suggestion ----
    # Base session name, e.g. "KiwiMS_2026-04-03_id1234"
    session_base_name <- gsub("\\.log$", "", basename(log_path))

    # Compute the lowest non-existing name in the target folder
    smart_analysis_name <- shiny$reactive({
      reset_button() # re-evaluate after reset so file.exists() sees newly created .db files
      base <- session_base_name
      target <- deconvolution_sidebar_vars$targetpath()
      if (
        is.null(target) ||
          length(target) == 0 ||
          !nzchar(target) ||
          !dir.exists(target)
      ) {
        return(base)
      }
      if (!file.exists(file.path(target, paste0(base, ".db")))) {
        return(base)
      }
      n <- 2L
      repeat {
        candidate <- paste0(base, "_", n)
        if (!file.exists(file.path(target, paste0(candidate, ".db")))) {
          return(candidate)
        }
        n <- n + 1L
      }
    })

    # Tentative destination (live) = targetpath / analysis_name
    effective_dest <- shiny$reactive({
      target <- deconvolution_sidebar_vars$targetpath()
      if (is.null(target) || length(target) == 0 || !nzchar(target)) {
        return(NULL)
      }
      target
    })

    # Locked destination — set once when deconvolute_start_conf fires
    analysis_dest <- shiny$reactiveVal(NULL)

    # Update the text input when the output path changes
    shiny$observeEvent(deconvolution_sidebar_vars$targetpath(), {
      suggested <- smart_analysis_name()
      shiny$updateTextInput(
        session,
        "analysis_name",
        value = suggested,
        placeholder = suggested
      )
    })

    ### Deconvolution interface (init or running, one output) ----
    output$deconvolution_ui <- shiny$renderUI({
      deconvolution_init_ui(
        ns,
        analysis_name_default = shiny$isolate(smart_analysis_name())
      )
    })

    ### Validation state reactive ----
    # Returns NULL when all conditions are met, otherwise the first failing message.
    deconv_validation_msg <- shiny$reactive({
      files_ok <-
        !is.null(deconvolution_sidebar_vars$dir()) &&
        length(deconvolution_sidebar_vars$dir()) > 0
      if (!files_ok) {
        return("Select target file(s) from the sidebar to start ...")
      }

      target <- deconvolution_sidebar_vars$targetpath()
      if (is.null(target) || length(target) == 0 || !nzchar(target)) {
        return("Select an output path from the sidebar to start ...")
      }

      if (!is.null(input$startz) && !is.null(input$endz)) {
        if (is.na(input$startz) || is.na(input$endz)) {
          return("Charge z range requires valid whole numbers ...")
        }
        if (input$startz < 1 || input$endz < 1) {
          return("Charge z values must be at least 1 ...")
        }
        if (
          input$startz != floor(input$startz) || input$endz != floor(input$endz)
        ) {
          return("Charge z values must be whole numbers ...")
        }
        if (input$startz >= input$endz) {
          return("High charge z must be greater than low charge z ...")
        }
      }

      if (!is.null(input$minmz) && !is.null(input$maxmz)) {
        if (is.na(input$minmz) || is.na(input$maxmz)) {
          return("m/z range requires valid whole numbers ...")
        }
        if (input$minmz < 1 || input$maxmz < 1) {
          return("m/z values must be at least 1 ...")
        }
        if (
          input$minmz != floor(input$minmz) || input$maxmz != floor(input$maxmz)
        ) {
          return("m/z values must be whole numbers ...")
        }
        if (input$minmz >= input$maxmz) {
          return("High m/z must be greater than low m/z ...")
        }
      }

      if (!is.null(input$masslb) && !is.null(input$massub)) {
        if (is.na(input$masslb) || is.na(input$massub)) {
          return("Mass Mw range requires valid whole numbers ...")
        }
        if (input$masslb < 1 || input$massub < 1) {
          return("Mass Mw values must be at least 1 Da ...")
        }
        if (
          input$masslb != floor(input$masslb) ||
            input$massub != floor(input$massub)
        ) {
          return("Mass Mw values must be whole numbers ...")
        }
        if (input$masslb >= input$massub) {
          return("High mass Mw must be greater than low mass Mw ...")
        }
      }

      if (!is.null(input$massbins)) {
        if (is.na(input$massbins)) {
          return("Sample Rate requires a valid value ...")
        }
        if (input$massbins < 0.1 || input$massbins > 10) {
          return("Sample Rate must be between 0.1 and 10 Da ...")
        }
      }

      if (!is.null(input$peakwindow)) {
        if (is.na(input$peakwindow)) {
          return("Detection window requires a valid value ...")
        }
        if (input$peakwindow < 1 || input$peakwindow > 500) {
          return("Detection window must be between 1 and 500 Da ...")
        }
        if (input$peakwindow != floor(input$peakwindow)) {
          return("Detection window must be a whole number ...")
        }
      }

      if (!is.null(input$peakthresh)) {
        if (is.na(input$peakthresh)) {
          return("Peak threshold requires a valid value ...")
        }
        if (input$peakthresh < 0 || input$peakthresh > 1) {
          return("Peak threshold must be between 0 and 1 ...")
        }
      }

      if (!is.null(input$time_start) && !is.null(input$time_end)) {
        if (is.na(input$time_start) || is.na(input$time_end)) {
          return("Retention time range requires valid values ...")
        }
        if (input$time_start >= input$time_end) {
          return("Retention start time must be earlier than end time ...")
        }
      }

      sel <- deconvolution_sidebar_vars$selected()
      if (!is.null(sel) && sel == "folder") {
        dir_path <- deconvolution_sidebar_vars$dir()
        if (length(dir_path) == 0 || !nzchar(dir_path)) {
          return("Select target file(s) from the sidebar to start ...")
        }
        is_raw_itself <- grepl("\\.raw$", dir_path, ignore.case = TRUE) &&
          dir.exists(dir_path)
        if (!is_raw_itself && length(dir_ls(dir_path, glob = "*.raw")) == 0) {
          return("No valid target folder selected ...")
        }
      }

      NULL
    })

    ### Analysis name path feedback ----
    output$analysis_name_feedback <- shiny$renderUI({
      msg <- deconv_validation_msg()

      if (!is.null(msg)) {
        return(shiny$div(
          class = "analysis-name-feedback-row",
          shiny$tags$span(
            style = "color: #D17050; flex-shrink:0;",
            shiny$icon("triangle-exclamation")
          ),
          shiny$HTML(paste0(" ", msg))
        ))
      }

      # All valid — show DB file path feedback
      target <- deconvolution_sidebar_vars$targetpath()
      name <- trimws(
        if (!is.null(input$analysis_name)) input$analysis_name else ""
      )
      if (!nzchar(name)) {
        name <- smart_analysis_name()
      }

      full_path <- file.path(target, paste0(name, ".db"))

      db_exists <- file.exists(full_path)
      shiny$div(
        class = "analysis-name-feedback-row analysis-path-feedback",
        shiny$tags$span(
          class = "analysis-path-label",
          if (db_exists) {
            "Will extend existing analysis:"
          } else {
            "Will be saved as:"
          }
        ),
        shiny$tags$pre(
          class = "path-selector-pre",
          style = "border-color: rgb(139, 195, 74);",
          full_path
        )
      )
    })

    ### Running output path display ----
    output$running_dest_ui <- shiny$renderUI({
      dest <- analysis_dest()
      if (is.null(dest)) {
        return(NULL)
      }

      base_name <- basename(dest)
      max_len <- 40L
      display_path <- if (nchar(dest) <= max_len) {
        dest
      } else {
        suffix <- paste0("/", base_name)
        prefix_len <- max_len - nchar(suffix) - 1L
        if (prefix_len <= 0) {
          paste0("\u2026", suffix)
        } else {
          paste0(substr(dest, 1L, prefix_len), "\u2026", suffix)
        }
      }

      tooltip(
        shiny$div(
          style = "cursor:pointer; margin-bottom: 5px;",
          onclick = paste0(
            "Shiny.setInputValue('",
            ns("open_dest"),
            "', Math.random())"
          ),
          shiny$tags$span(
            style = "color:#5cb85c; flex-shrink:0;",
            shiny$icon("folder-open")
          ),
          shiny$HTML(paste(
            "<code style='cursor:pointer;'>",
            "Saved in:",
            display_path,
            "</code>"
          ))
        ),
        "Click to open in File Explorer",
        placement = "bottom"
      )
    })

    shiny$observeEvent(input$open_dest, {
      dest <- analysis_dest()
      if (!is.null(dest) && dir.exists(dest)) {
        if (.Platform$OS.type == "windows") {
          shell.exec(dest)
        } else {
          utils::browseURL(dest)
        }
      }
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

    ### Save-default handlers for parameter inputs ----
    shiny$observeEvent(
      input$save_startz_btn,
      {
        if (!is.null(input$startz) && !is.na(input$startz)) {
          update_user_setting("deconv_startz", input$startz)
          shinyWidgets::show_toast(
            paste0("Min. charge state default set to ", input$startz),
            text = NULL,
            type = "success",
            timer = 3000,
            timerProgressBar = TRUE
          )
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    shiny$observeEvent(
      input$save_endz_btn,
      {
        if (!is.null(input$endz) && !is.na(input$endz)) {
          update_user_setting("deconv_endz", input$endz)
          shinyWidgets::show_toast(
            paste0("Max. charge state default set to ", input$endz),
            text = NULL,
            type = "success",
            timer = 3000,
            timerProgressBar = TRUE
          )
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
    shiny$observeEvent(
      input$save_minmz_btn,
      {
        if (!is.null(input$minmz) && !is.na(input$minmz)) {
          update_user_setting("deconv_minmz", input$minmz)
          shinyWidgets::show_toast(
            paste0(
              "Min. m/z ratio default set to ",
              input$minmz,
              " [m/z]"
            ),
            text = NULL,
            type = "success",
            timer = 3000,
            timerProgressBar = TRUE
          )
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
    shiny$observeEvent(
      input$save_maxmz_btn,
      {
        if (!is.null(input$maxmz) && !is.na(input$maxmz)) {
          update_user_setting("deconv_maxmz", input$maxmz)
          shinyWidgets::show_toast(
            paste0(
              "Max. m/z ratio default set to ",
              input$maxmz,
              " [m/z]"
            ),
            text = NULL,
            type = "success",
            timer = 3000,
            timerProgressBar = TRUE
          )
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
    shiny$observeEvent(
      input$save_masslb_btn,
      {
        if (!is.null(input$masslb) && !is.na(input$masslb)) {
          update_user_setting("deconv_masslb", input$masslb)
          shinyWidgets::show_toast(
            paste0(
              "Min. mass default set to ",
              input$masslb,
              " [Da]"
            ),
            text = NULL,
            type = "success",
            timer = 3000,
            timerProgressBar = TRUE
          )
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
    shiny$observeEvent(
      input$save_massub_btn,
      {
        if (!is.null(input$massub) && !is.na(input$massub)) {
          update_user_setting("deconv_massub", input$massub)
          shinyWidgets::show_toast(
            paste0(
              "Max. mass default set to ",
              input$massub,
              " [Da]"
            ),
            text = NULL,
            type = "success",
            timer = 3000,
            timerProgressBar = TRUE
          )
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
    shiny$observeEvent(
      input$save_time_start_btn,
      {
        if (!is.null(input$time_start) && !is.na(input$time_start)) {
          update_user_setting("deconv_time_start", input$time_start)
          shinyWidgets::show_toast(
            paste0(
              "Default elution start ",
              input$time_start,
              " [min]"
            ),
            text = NULL,
            type = "success",
            timer = 3000,
            timerProgressBar = TRUE
          )
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
    shiny$observeEvent(
      input$save_time_end_btn,
      {
        if (!is.null(input$time_end) && !is.na(input$time_end)) {
          update_user_setting("deconv_time_end", input$time_end)
          shinyWidgets::show_toast(
            paste0(
              "Default elution end ",
              input$time_end,
              " [min]"
            ),
            text = NULL,
            type = "success",
            timer = 3000,
            timerProgressBar = TRUE
          )
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
    shiny$observeEvent(
      input$save_peakwindow_btn,
      {
        if (!is.null(input$peakwindow) && !is.na(input$peakwindow)) {
          update_user_setting("deconv_peakwindow", input$peakwindow)
          shinyWidgets::show_toast(
            paste0(
              "Default peak window set to ",
              input$peakwindow,
              " [Da]"
            ),
            text = NULL,
            type = "success",
            timer = 3000,
            timerProgressBar = TRUE
          )
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
    shiny$observeEvent(
      input$save_peaknorm_btn,
      {
        if (!is.null(input$peaknorm)) {
          update_user_setting("deconv_peaknorm", input$peaknorm)
          shinyWidgets::show_toast(
            paste0(
              "Default peak normalization set to ",
              input$peaknorm
            ),
            text = NULL,
            type = "success",
            timer = 3000,
            timerProgressBar = TRUE
          )
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
    shiny$observeEvent(
      input$save_peakthresh_btn,
      {
        if (!is.null(input$peakthresh) && !is.na(input$peakthresh)) {
          update_user_setting("deconv_peakthresh", input$peakthresh)
          shinyWidgets::show_toast(
            paste0(
              "Default peak threshold set to ",
              input$peaknorm
            ),
            text = NULL,
            type = "success",
            timer = 3000,
            timerProgressBar = TRUE
          )
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
    shiny$observeEvent(
      input$save_massbins_btn,
      {
        if (!is.null(input$massbins) && !is.na(input$massbins)) {
          update_user_setting("deconv_massbins", input$massbins)
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    ### Start button ----
    output$deconvolute_start_ui <- shiny$renderUI({
      reset_button()
      btn <- shiny$div(
        class = "start-button",
        style = "height: 100%;",
        shiny$actionButton(
          ns("deconvolute_start"),
          "Start",
          icon = shiny$icon("circle-play"),
          width = "100%"
        )
      )
      if (!is.null(deconv_validation_msg())) disabled(btn) else btn
    })

    # Eagerly render startup outputs so they are computed in the first reactive
    # flush and included in the same browser message as waiter_hide().
    shiny$outputOptions(
      output,
      "deconvolution_ui",
      suspendWhenHidden = FALSE
    )
    shiny$outputOptions(
      output,
      "deconvolute_start_ui",
      suspendWhenHidden = FALSE
    )
    shiny$outputOptions(
      output,
      "analysis_name_feedback",
      suspendWhenHidden = FALSE
    )

    ### Functions ----

    #### check_progress ----
    # Counts only the samples in raw_dirs — never inflated by pre-existing
    # done records from a previous run in the same DB (extend case).
    check_progress <- function(raw_dirs) {
      message("Checking progress at: ", Sys.time())
      db <- file.path(
        analysis_dest(),
        paste0(trimws(input$analysis_name), ".db")
      )
      sample_bases <- gsub(
        "\\.raw$",
        "",
        basename(raw_dirs),
        ignore.case = TRUE
      )
      count <- decon_progress_count(db, sample_bases)
      message("Done count from DB: ", count)
      count
    }

    #### reset_progress ----
    reset_progress <- function() {
      reactVars$is_running <- FALSE
      reactVars$heatmap_ready <- 0L
      reactVars$completed_files <- 0
      reactVars$current_total_files <- 0
      reactVars$expected_files <- 0
      reactVars$initial_file_count <- 0
      reactVars$count <- 0
      reactVars$sample_names <- NULL
      reactVars$wells <- NULL
      reactVars$rslt_df <- data.frame()
      reactVars$failed_samples <- character(0)
      reactVars$last_check <- Sys.time()
      reactVars$results_last_check <- Sys.time()
      reactVars$deconv_report_status <- NULL

      decon_rep_process_data(NULL)

      output$deconvolution_data <- DT::renderDataTable(
        DT::datatable(data.frame(), options = list(dom = "", paging = FALSE)),
        server = FALSE
      )
      output$result_picker_ui <- shiny$renderUI(NULL)
      output$spectrum_failure_msg <- shiny$renderUI(NULL)
      output$metrics_failure_msg <- shiny$renderUI(NULL)
      result_files_sel(NULL)

      # Immediately clear the progress bar title so a stale title from a prior
      # run is never visible during the setup phase of the next run.
      updateProgressBar(
        session = session,
        id = ns("progressBar"),
        value = 0,
        title = "Ready"
      )
    }

    ### Event start deconvolution ----

    #### Confirmation modal ----
    shiny$observeEvent(input$deconvolute_start, {
      shiny$showModal(
        shiny$div(
          class = "start-modal deconvolute-modal",
          shiny$modalDialog(
            shiny$fluidRow(
              shiny$br(),
              shiny$column(
                width = 12,
                shiny$uiOutput(ns("message_ui")),
                shiny$uiOutput(ns("target_sel_ui")),
                shiny$br(),
                shiny$uiOutput(ns("warning_ui")),
                shiny$uiOutput(ns("selector_ui")),
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

    # Dynamic rendering of warnings for the start dialog.
    # Covers two cases:
    #   1. Stale UniDec output files in destination (keep_raw_output=TRUE only)
    #   2. Samples already present (done) in an existing analysis DB
    output$warning_ui <- shiny$renderUI({
      input$deconvolute_start
      reactVars$stale_unidec_output <- character(0)
      reactVars$overwrite <- character(0)

      dest_dir <- effective_dest() %||% deconvolution_sidebar_vars$targetpath()

      # ── Collect queried sample base names ──────────────────────────────────
      # Use target_selector_sel() (persisted) not input$target_selector which
      # may still be NULL while the picker inside the modal is rendering.
      dir_path_modal <- deconvolution_sidebar_vars$dir()
      raw_dirs_all <- if (
        grepl("\\.raw$", dir_path_modal, ignore.case = TRUE) &&
          dir.exists(dir_path_modal)
      ) {
        dir_path_modal
      } else {
        dir_ls(dir_path_modal, glob = "*.raw")
      }
      sample_bases <- if (
        isTRUE(deconvolution_sidebar_vars$use_config()) &&
          length(config_file())
      ) {
        samps <- config_file()[["Sample"]]
        samps <- samps[samps %in% basename(raw_dirs_all)]
        gsub("\\.raw$", "", samps, ignore.case = TRUE)
      } else {
        sel <- target_selector_sel()
        bases <- if (length(sel) > 0) sel else basename(raw_dirs_all)
        gsub("\\.raw$", "", bases, ignore.case = TRUE)
      }
      if (length(sample_bases) == 0 || !nzchar(sample_bases[1])) {
        return(NULL)
      }

      warnings_list <- list()

      # ── Warning 1: stale UniDec output (only when keeping raw output) ──────
      if (
        !is.null(dest_dir) &&
          dir.exists(dest_dir) &&
          isTRUE(read_user_settings()$deconv_keep_raw_output)
      ) {
        stale <- character(0)
        for (s in sample_bases) {
          txt <- file.path(dest_dir, paste0(s, "_rawdata.txt"))
          udir <- file.path(dest_dir, paste0(s, "_rawdata_unidecfiles"))
          if (file.exists(txt)) {
            stale <- c(stale, txt)
          }
          if (dir.exists(udir)) stale <- c(stale, udir)
        }
        if (length(stale) > 0) {
          reactVars$stale_unidec_output <- stale
          n_aff <- length(unique(
            gsub("(_rawdata\\.txt|_rawdata_unidecfiles)$", "", basename(stale))
          ))
          warnings_list <- c(
            warnings_list,
            list(shiny$p(shiny$HTML(paste0(
              '<i class="fa-solid fa-circle-exclamation" style="font-size:1em;',
              ' color:black; margin-right:10px;"></i>',
              "<b>",
              n_aff,
              "</b> sample(s) have leftover UniDec output in the",
              " output path. Continuing will delete these files before reprocessing."
            ))))
          )
        }
      }

      # ── Warning 2: samples already done in the existing DB ─────────────────
      db_path_chk <- if (
        !is.null(dest_dir) &&
          nzchar(trimws(input$analysis_name %||% ""))
      ) {
        file.path(dest_dir, paste0(trimws(input$analysis_name), ".db"))
      } else {
        NULL
      }

      if (!is.null(db_path_chk) && file.exists(db_path_chk)) {
        done_samples <- tryCatch(
          {
            con_w <- DBI::dbConnect(
              RSQLite::SQLite(),
              db_path_chk,
              flags = RSQLite::SQLITE_RO
            )
            on.exit(DBI::dbDisconnect(con_w), add = TRUE)
            if (DBI::dbExistsTable(con_w, "status")) {
              DBI::dbGetQuery(
                con_w,
                "SELECT sample FROM status WHERE state = 'done'"
              )$sample
            } else {
              character(0)
            }
          },
          error = function(e) character(0)
        )

        dup_bases <- intersect(sample_bases, done_samples)
        if (length(dup_bases) > 0) {
          reactVars$overwrite <- dup_bases
          warnings_list <- c(
            warnings_list,
            list(shiny$p(shiny$HTML(paste0(
              '<i class="fa-solid fa-circle-exclamation" style="font-size:1em;',
              ' color:black; margin-right:10px;"></i>',
              "<b>",
              length(dup_bases),
              "</b> sample(s) queried for deconvolution",
              " are already present in the existing analysis database.",
              " Please choose how to proceed:"
            ))))
          )
        }
      }

      if (length(warnings_list) == 0) {
        return(NULL)
      }
      shiny$tagList(warnings_list)
    })

    # Skip/Overwrite radio buttons — shown when duplicate DB samples are detected
    output$selector_ui <- shiny$renderUI({
      input$deconvolute_start
      if (length(reactVars$overwrite) == 0) {
        return(NULL)
      }
      shiny$radioButtons(
        ns("decon_select"),
        "",
        choices = c("Overwrite Samples", "Skip Samples"),
        selected = "Overwrite Samples"
      )
    })

    # Persist the user's overwrite/skip choice
    shiny$observe({
      if (!is.null(input$decon_select)) {
        reactVars$duplicated <- input$decon_select
      }
    })

    # Dynamic rendering of message with info for selected files
    output$message_ui <- shiny$renderUI({
      input$deconvolute_start
      enable(selector = "#app-deconvolution_main-deconvolute_start_conf")

      icon_warn <- '<i class="fa-solid fa-circle-exclamation" style="font-size:1em; color:black; margin-right:10px;"></i>'
      icon_info <- '<i class="fa-solid fa-circle-info" style="margin-right:4px;"></i>'

      make_warning <- function(text) {
        paste0(icon_warn, "<i>", text, "</i>")
      }

      make_details <- function(label, items) {
        paste0(
          "<details style='font-size:0.85em; color:gray; cursor:pointer;'>",
          "<summary style='user-select:none;'>",
          icon_info,
          label,
          "</summary>",
          "<div style='margin-top:6px; max-height:150px; overflow-y:auto;",
          " border:1px solid #ddd; border-radius:4px; padding:6px; background:#f8f8f8;'>",
          "<div style='font-family:monospace; font-size:0.9em;'>",
          paste(items, collapse = "<br>"),
          "</div></div></details>"
        )
      }

      message <- NULL

      if (deconvolution_sidebar_vars$selected() == "folder") {
        dir_path_msg <- deconvolution_sidebar_vars$dir()
        raw_dirs <- if (
          grepl("\\.raw$", dir_path_msg, ignore.case = TRUE) &&
            dir.exists(dir_path_msg)
        ) {
          dir_path_msg
        } else {
          dir_ls(dir_path_msg, glob = "*.raw")
        }

        if (
          isTRUE(deconvolution_sidebar_vars$use_config()) &&
            length(config_file())
        ) {
          presence <- config_file()[["Sample"]] %in% basename(raw_dirs)
          extras <- basename(raw_dirs)[
            !basename(raw_dirs) %in% config_file()[["Sample"]]
          ]

          html <- if (sum(presence) == 0) {
            disable(selector = "#app-deconvolution_main-deconvolute_start_conf")
            paste0(
              "<b>Multiple target file(s) selected</b><br><br>",
              make_warning(
                "None of the sample(s) present in the config file can be found in the selected folder."
              )
            )
          } else {
            parts <- paste0(
              "<b>Multiple target file(s) selected</b><br><br>",
              "<b>",
              sum(presence),
              "</b> sample(s) present in the config file are queried for deconvolution."
            )
            if (!all(presence)) {
              missing <- config_file()[["Sample"]][!presence]
              parts <- paste0(
                parts,
                "<br><br>",
                make_warning(paste0(
                  "<b>",
                  sum(!presence),
                  "</b> of the samples specified in the config file are <b>NOT</b> present in the selected folder."
                )),
                "<br>",
                make_details("View missing sample(s)", missing)
              )
            }
            parts
          }

          if (length(extras) > 0) {
            html <- paste0(
              html,
              "<br>",
              make_warning(paste0(
                "<b>",
                length(extras),
                "</b> sample(s) present in the selected folder are <b>NOT</b> in the experiment config and will not be deconvoluted."
              )),
              "<br>",
              make_details("View unqueued sample(s)", extras)
            )
          }

          if (any(duplicated(config_file()[["Sample"]]))) {
            disable(selector = "#app-deconvolution_main-deconvolute_start_conf")
          }

          message <- shiny$p(shiny$HTML(html))
        } else {
          dir_path_msg <- deconvolution_sidebar_vars$dir()
          if (
            grepl("\\.raw$", dir_path_msg, ignore.case = TRUE) &&
              dir.exists(dir_path_msg)
          ) {
            message <- shiny$p(shiny$HTML(paste0(
              "<b>Single target file selected</b><br><br>",
              "<span style='white-space:nowrap;'>",
              basename(dir_path_msg),
              "</span>",
              " is queried for deconvolution."
            )))
          } else {
            num_targets <- length(input$target_selector) %||% 0

            if (num_targets == 0) {
              disable(
                selector = "#app-deconvolution_main-deconvolute_start_conf"
              )
            }

            message <- shiny$p(shiny$HTML(paste0(
              "<b>Multiple target file(s) selected</b><br><br>",
              "<b>",
              num_targets,
              "</b> raw file(s) in the selected directory are currently",
              " queried for deconvolution. If you wish to process only a subset select the",
              " respective target files."
            )))
          }
        }
      }

      return(message)
    })

    # Dynamic rendering of target file selector
    output$target_sel_ui <- shiny$renderUI({
      input$deconvolute_start

      picker <- NULL

      dir_sel <- deconvolution_sidebar_vars$dir()
      is_raw_itself <- grepl("\\.raw$", dir_sel, ignore.case = TRUE) &&
        dir.exists(dir_sel)

      if (
        !is_raw_itself &&
          deconvolution_sidebar_vars$selected() == "folder" &&
          (isFALSE(deconvolution_sidebar_vars$use_config()) ||
            length(config_file()) == 0)
      ) {
        files <- basename(dir_ls(dir_sel, glob = "*.raw"))
        picker <- shiny$div(
          class = "kiwi-file-selector",
          shiny$div(
            class = "kiwi-file-actions",
            shiny$actionButton(
              ns("target_select_all"),
              "Select All",
              class = "btn btn-default btn-sm"
            ),
            shiny$actionButton(
              ns("target_deselect_all"),
              "Deselect All",
              class = "btn btn-default btn-sm"
            )
          ),
          shiny$div(
            class = "kiwi-file-list",
            shiny$checkboxGroupInput(
              ns("target_selector"),
              NULL,
              choices = files,
              selected = files,
              width = "100%"
            )
          )
        )
      }

      return(picker)
    })

    shiny$observeEvent(input$target_select_all, {
      dir_sel <- deconvolution_sidebar_vars$dir()
      files <- basename(dir_ls(dir_sel, glob = "*.raw"))
      shiny$updateCheckboxGroupInput(
        session,
        "target_selector",
        selected = files
      )
    })

    shiny$observeEvent(input$target_deselect_all, {
      shiny$updateCheckboxGroupInput(
        session,
        "target_selector",
        selected = character(0)
      )
    })

    #### Deconvolution start ----
    shiny$observeEvent(input$deconvolute_start_conf, {
      # Reset modal and previous processes
      shiny$removeModal()
      reset_progress()

      # Show that setup is underway (reset_progress already cleared to "Ready";
      # this immediately reflects the new run phase before the observer fires).
      updateProgressBar(
        session = session,
        id = ns("progressBar"),
        value = 0,
        title = "Setting up ..."
      )

      # Lock in analysis destination (sidebar path — no subfolder created)
      analysis_dest(effective_dest())

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
        dir_path <- deconvolution_sidebar_vars$dir()
        if (
          grepl("\\.raw$", dir_path, ignore.case = TRUE) && dir.exists(dir_path)
        ) {
          raw_dirs <- dir_path
        } else {
          raw_dirs <- list.dirs(dir_path, full.names = TRUE, recursive = FALSE)
          raw_dirs <- raw_dirs[grep("\\.raw$", raw_dirs)]
        }

        if (
          isTRUE(deconvolution_sidebar_vars$use_config()) &&
            length(config_file())
        ) {
          write_log("Multiple target deconvolution mode (with config file)")

          sample_names <- config_file()[["Sample"]]
          raw_dirs <- raw_dirs[basename(raw_dirs) %in% sample_names]

          # Prepare heatmap variables — restrict to samples present in folder
          present_in_folder <- sample_names %in% basename(raw_dirs)
          reactVars$sample_names <- gsub(
            "\\.raw$",
            "",
            sample_names[present_in_folder],
            ignore.case = TRUE
          )
          reactVars$sample_names <- reactVars$sample_names[nzchar(trimws(
            reactVars$sample_names
          ))]
          reactVars$wells <- gsub(
            ",",
            "",
            sub("^.*:", "", config_file()[["Well"]][present_in_folder])
          )
        } else if (
          grepl("\\.raw$", dir_path, ignore.case = TRUE) && dir.exists(dir_path)
        ) {
          write_log("Single target deconvolution mode")
        } else {
          write_log("Multiple target deconvolution mode (no config file)")

          raw_dirs <- raw_dirs[basename(raw_dirs) %in% target_selector_sel()]
        }

        write_log(paste(
          length(raw_dirs),
          "targets. Directory:",
          dirname(raw_dirs[1])
        ))
      }
      write_log(paste(
        "Output path:",
        analysis_dest()
      ))

      # Handle duplicate samples already present (done) in an existing DB
      if (length(reactVars$overwrite) > 0) {
        choice <- reactVars$duplicated %||% "Overwrite Samples"
        if (choice == "Skip Samples") {
          # Remove already-done samples from the queue
          raw_dirs <- raw_dirs[
            !gsub("\\.raw$", "", basename(raw_dirs), ignore.case = TRUE) %in%
              reactVars$overwrite
          ]
          write_log(paste(
            "Skipping",
            length(reactVars$overwrite),
            "already-processed sample(s)"
          ))
          if (length(raw_dirs) == 0) {
            shiny$showNotification(
              "All selected samples are already processed — nothing to do.",
              type = "warning",
              duration = 5
            )
            runjs(paste0(
              'document.getElementById("blocking-overlay").style.display = "none";'
            ))
            reset_progress()
            return()
          }
        } else {
          write_log(paste(
            "Overwriting",
            length(reactVars$overwrite),
            "already-processed sample(s)"
          ))
          # The worker's DB init deletes status + per-sample data rows for
          # these samples before reprocessing, so no additional cleanup needed here.
        }
      }

      # Delete stale UniDec output (txt + unidecfiles dirs) that would cause
      # os.rename() FileExistsError inside the Python worker.
      # Done here — after skip filtering — so files are only deleted for
      # samples that will actually be processed (never when all are skipped).
      stale_all <- reactVars$stale_unidec_output
      if (length(stale_all) > 0) {
        active_bases <- gsub(
          "\\.raw$",
          "",
          basename(raw_dirs),
          ignore.case = TRUE
        )
        stale_active <- stale_all[
          gsub(
            "(_rawdata\\.txt|_rawdata_unidecfiles)$",
            "",
            basename(stale_all)
          ) %in%
            active_bases
        ]
        if (length(stale_active) > 0) {
          for (path in stale_active) {
            if (dir.exists(path)) {
              unlink(path, recursive = TRUE)
            } else if (file.exists(path)) {
              file.remove(path)
            }
          }
          write_log(paste(
            "Deleted",
            length(stale_active),
            "stale UniDec output file(s) before reprocessing"
          ))
        }
        reactVars$stale_unidec_output <- character(0)
      }

      # Render disabled results picker
      output$result_picker_ui <- shiny$renderUI(
        shiny$div(
          class = "result-picker",
          disabled(shiny$selectInput(
            ns("result_picker"),
            "Select Sample",
            choices = ""
          ))
        )
      )
      # Apply JS modifications for picker
      session$sendCustomMessage("selectize-init", "result_picker")

      # Initialization variables
      reactVars$is_running <- TRUE
      reactVars$catch_error <- FALSE
      reactVars$expected_files <- length(raw_dirs)
      reactVars$initial_file_count <- 0L # check_progress already scopes to raw_dirs

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
      output_path <- file.path(
        Sys.getenv("LOCALAPPDATA"),
        "KiwiMS",
        "deconvolution.log"
      )
      dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
      file.create(output_path)
      reactVars$decon_process_out <- output_path
      write("", reactVars$decon_process_out)

      # Pre-clear stale DB state before spawning the worker to eliminate race
      # conditions where the progress observer fires before the worker has had
      # time to initialise the DB:
      #   1. Drop 'completed' sentinel — prevents immediate "Finalized!" flash.
      #   2. Delete status rows for the samples being processed — prevents a
      #      100% progress / "Saving Results" flash when old done records exist.
      tryCatch(
        {
          db_path_pre <- file.path(
            analysis_dest(),
            paste0(trimws(input$analysis_name), ".db")
          )
          if (file.exists(db_path_pre)) {
            active_bases <- gsub(
              "\\.raw$",
              "",
              basename(raw_dirs),
              ignore.case = TRUE
            )
            con_pre <- DBI::dbConnect(RSQLite::SQLite(), db_path_pre)
            if (DBI::dbExistsTable(con_pre, "completed")) {
              DBI::dbExecute(con_pre, "DROP TABLE completed")
            }
            if (DBI::dbExistsTable(con_pre, "status")) {
              for (s in active_bases) {
                DBI::dbExecute(
                  con_pre,
                  "DELETE FROM status WHERE sample = ?",
                  params = list(s)
                )
              }
            }
            DBI::dbDisconnect(con_pre)
          }
        },
        error = function(e) {
          message(
            "Warning: could not pre-clear DB state before worker start: ",
            e$message
          )
        }
      )

      # Launch external deconvolution process
      updateProgressBar(
        session = session,
        id = ns("progressBar"),
        value = 0,
        title = "Launching worker ..."
      )
      tryCatch(
        {
          rscript_path <- Sys.getenv("KIWIMS_RSCRIPT")
          if (!nzchar(rscript_path) || !file.exists(rscript_path)) {
            rscript_path <- file.path(R.home("bin"), "Rscript.exe")
          }
          message("Launching subprocess: ", rscript_path)

          # Store pre-launch diagnostics in a reactive variable so they survive
          # processx truncating the log file when the subprocess starts.
          reactVars$decon_prelaunch_info <- paste(
            c(
              sprintf(
                "[%s] === Deconvolution subprocess launch ===",
                format(Sys.time(), "%H:%M:%S")
              ),
              sprintf("  Rscript          : %s", rscript_path),
              sprintf(
                "  R_HOME           : %s",
                Sys.getenv("R_HOME", unset = "(not set)")
              ),
              sprintf(
                "  PYTHONHOME       : %s",
                Sys.getenv("PYTHONHOME", unset = "(not set)")
              ),
              sprintf(
                "  RETICULATE_PYTHON: %s",
                Sys.getenv("RETICULATE_PYTHON", unset = "(not set)")
              ),
              sprintf(
                "  KIWIMS_RSCRIPT   : %s",
                Sys.getenv("KIWIMS_RSCRIPT", unset = "(not set)")
              ),
              sprintf("  Working dir      : %s", getwd()),
              "---"
            ),
            collapse = "\n"
          )

          rx_process <- process$new(
            rscript_path,
            args = c(
              "app/logic/deconvolution_execute.R",
              temp,
              log_path,
              getwd(),
              analysis_dest(),
              Sys.getenv("KIWIMS_DEV_MODE"),
              file.path(
                analysis_dest(),
                paste0(trimws(input$analysis_name), ".db")
              ),
              as.character(isTRUE(read_user_settings()$deconv_keep_raw_output))
            ),
            stdout = reactVars$decon_process_out,
            stderr = reactVars$decon_process_out
          )
        },
        error = function(e) {
          # Activate error catching variable
          reactVars$catch_error <- TRUE

          # Stop spinner for spectrum and heatmap plot
          waiter_hide(id = ns("heatmap"))
          waiter_hide(id = ns("spectrum"))

          error_msg <- paste("Failed to start deconvolution:", e$message)
          write_log(error_msg)

          # Show error notification
          shiny$showNotification(
            error_msg,
            type = "error",
            duration = 5
          )
        }
      )

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

              # Set reactive status variables (error path only — for successful
              # completion the progress observer sets is_running=FALSE and
              # updates the button to "Reset" atomically at lines ~1918/2090)
              reactVars$is_running <- FALSE
              reactVars$deconv_report_status <- NULL
            }
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

      # On app close: kill process and checkpoint DB to remove WAL sidecar files
      reactVars$process_observer <- shiny$observe({
        proc <- decon_process_data()
        db_snap <- file.path(
          analysis_dest(),
          paste0(trimws(input$analysis_name), ".db")
        )

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
            Sys.sleep(0.75) # let the OS release file handles before connecting
          }
          cleanup_wal(db_snap)
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
              isTRUE(deconvolution_sidebar_vars$use_config()) &&
                length(config_file()) &&
                "Well" %in% names(config_file()) &&
                any(
                  !is.na(config_file()[["Well"]]) &
                    nzchar(trimws(as.character(config_file()[["Well"]])))
                ) &&
                nrow(reactVars$rslt_df) < reactVars$completed_files
            ) {
              shiny$req(reactVars$sample_names, reactVars$wells)

              db_pth <- file.path(
                analysis_dest(),
                paste0(trimws(input$analysis_name), ".db")
              )
              done_ids <- tryCatch(
                {
                  con_chk <- dbConnect(SQLite(), db_pth, flags = SQLITE_RO)
                  on.exit(dbDisconnect(con_chk), add = TRUE)
                  if (dbExistsTable(con_chk, "status")) {
                    dbGetQuery(
                      con_chk,
                      "SELECT sample FROM status WHERE state='done'"
                    )$sample
                  } else {
                    character(0)
                  }
                },
                error = function(e) character(0)
              )
              done_in_run <- intersect(done_ids, reactVars$sample_names)
              results <- file.path(
                analysis_dest(),
                paste0(done_in_run, "_rawdata_unidecfiles")
              )

              if (length(done_in_run)) {
                peaks_from_db <- tryCatch(
                  read_decon_peaks_max(db_pth, done_in_run),
                  error = function(e) {
                    data.frame(sample = character(), max_mass = numeric())
                  }
                )
                if (nrow(peaks_from_db) > 0) {
                  peaks_from_db$well_id <- reactVars$wells[
                    match(peaks_from_db$sample, reactVars$sample_names)
                  ]
                  peaks_from_db <- peaks_from_db[
                    !is.na(peaks_from_db$well_id) &
                      !is.na(peaks_from_db$max_mass),
                  ]
                  if (nrow(peaks_from_db) > 0) {
                    reactVars$rslt_df <- data.frame(
                      sample = peaks_from_db$sample,
                      well_id = peaks_from_db$well_id,
                      value = peaks_from_db$max_mass
                    )
                  }
                }

                ##### Render result picker with updated choices ----
                choices_ok <- gsub(
                  "_rawdata_unidecfiles",
                  ".raw",
                  basename(results)
                )

                failed_in_run <- reactVars$failed_samples[
                  reactVars$failed_samples %in% reactVars$sample_names
                ]
                failed_in_run <- failed_in_run[nzchar(failed_in_run)]

                choices_failed <- character(0)
                if (length(failed_in_run)) {
                  choices_failed <- paste0(failed_in_run, ".raw")
                }

                named_choices <- character(0)
                if (length(choices_ok) > 0) {
                  named_choices <- c(named_choices, choices_ok)
                  names(named_choices)[seq_along(choices_ok)] <- choices_ok
                }
                if (length(choices_failed) > 0) {
                  prev_len <- length(named_choices)
                  named_choices <- c(named_choices, choices_failed)
                  names(named_choices)[prev_len + seq_along(choices_failed)] <-
                    paste0(failed_in_run, " (failed)")
                }
                if (length(named_choices) > 0) {
                  cur_sel <- result_files_sel()
                  if (
                    is.null(cur_sel) ||
                      !nzchar(cur_sel) ||
                      !(cur_sel %in% named_choices)
                  ) {
                    result_files_sel(unname(named_choices[1]))
                  }

                  output$result_picker_ui <- shiny$renderUI({
                    shiny$div(
                      class = "result-picker",
                      shiny$selectInput(
                        ns("result_picker"),
                        "Select Sample",
                        choices = named_choices,
                        selected = result_files_sel()
                      )
                    )
                  })
                  session$sendCustomMessage("selectize-init", "result_picker")
                }
              }
            } else {
              dir_sel_res <- deconvolution_sidebar_vars$dir()
              is_raw_res <- grepl("\\.raw$", dir_sel_res, ignore.case = TRUE) &&
                dir.exists(dir_sel_res)
              selected_files <- if (is_raw_res) {
                dir_sel_res
              } else {
                file.path(dir_sel_res, target_selector_sel())
              }
              sel_base <- gsub(
                "\\.raw$",
                "",
                basename(selected_files),
                ignore.case = TRUE
              )
              db <- file.path(
                analysis_dest(),
                paste0(trimws(input$analysis_name), ".db")
              )
              done_samples <- tryCatch(
                {
                  con_chk <- dbConnect(SQLite(), db, flags = SQLITE_RO)
                  on.exit(dbDisconnect(con_chk), add = TRUE)
                  if (dbExistsTable(con_chk, "status")) {
                    dbGetQuery(
                      con_chk,
                      "SELECT sample FROM status WHERE state='done'"
                    )$sample
                  } else {
                    character(0)
                  }
                },
                error = function(e) character(0)
              )
              finished_files <- sel_base %in% done_samples
              failed_mask <- sel_base %in%
                reactVars$failed_samples &
                !finished_files

              choices_ok <- basename(selected_files)[finished_files]
              choices_failed <- basename(selected_files)[failed_mask]
              named_choices <- character(0)
              if (length(choices_ok) > 0) {
                named_choices <- c(named_choices, choices_ok)
                names(named_choices)[seq_along(choices_ok)] <- choices_ok
              }
              if (length(choices_failed) > 0) {
                prev_len <- length(named_choices)
                named_choices <- c(named_choices, choices_failed)
                names(named_choices)[prev_len + seq_along(choices_failed)] <-
                  paste0(
                    gsub("\\.raw$", "", choices_failed, ignore.case = TRUE),
                    " (failed)"
                  )
              }

              if (length(named_choices) > 0) {
                cur_sel <- result_files_sel()
                sel_default <- if (
                  !is.null(cur_sel) &&
                    nzchar(cur_sel) &&
                    cur_sel %in% named_choices
                ) {
                  cur_sel
                } else {
                  unname(named_choices[1])
                }

                output$result_picker_ui <- shiny$renderUI({
                  shiny$div(
                    class = "result-picker",
                    shiny$selectInput(
                      ns("result_picker"),
                      "Select Sample",
                      choices = named_choices,
                      selected = sel_default
                    )
                  )
                })
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
          # Scan only for sentinels that belong to the current raw_dirs so that
          # residual files from other runs in the same directory are ignored.
          current_base_names <- gsub(
            "\\.raw$",
            "",
            basename(raw_dirs),
            ignore.case = TRUE
          )
          db_pth <- file.path(
            analysis_dest(),
            paste0(trimws(input$analysis_name), ".db")
          )
          failed_in_db <- decon_failed_samples(db_pth)
          newly_failed <- setdiff(
            intersect(current_base_names, failed_in_db),
            reactVars$failed_samples
          )
          newly_failed <- newly_failed[nzchar(trimws(newly_failed))]
          if (length(newly_failed) > 0) {
            reactVars$failed_samples <- c(
              reactVars$failed_samples,
              newly_failed
            )
          }

          reactVars$current_total_files <- check_progress(raw_dirs)
          reactVars$completed_files <-
            (reactVars$current_total_files - reactVars$initial_file_count) +
            length(reactVars$failed_samples)
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

          result_files <- file.path(
            analysis_dest(),
            basename(gsub(".raw", "_rawdata_unidecfiles", raw_dirs))
          )

          # Poll completion sentinel on every cycle
          all_processed <- decon_is_complete(
            file.path(
              analysis_dest(),
              paste0(trimws(input$analysis_name), ".db")
            )
          )

          if (reactVars$completed_files == 0) {
            title <- paste0(
              "Initializing ",
              paste0(rep(".", reactVars$count), collapse = "")
            )
          } else if (!all_processed) {
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

            if (all_processed) {
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
                  isTRUE(deconvolution_sidebar_vars$use_config()) &&
                  length(config_file()) &&
                  "Well" %in% names(config_file()) &&
                  any(
                    !is.na(config_file()[["Well"]]) &
                      nzchar(trimws(as.character(config_file()[["Well"]])))
                  )
              ) {
                new_sample_names <- setdiff(
                  reactVars$sample_names,
                  reactVars$rslt_df$sample
                )

                if (length(new_sample_names)) {
                  db_path_hm <- file.path(
                    analysis_dest(),
                    paste0(trimws(input$analysis_name), ".db")
                  )
                  peaks_max <- tryCatch(
                    read_decon_peaks_max(db_path_hm, new_sample_names),
                    error = function(e) {
                      data.frame(sample = character(), max_mass = numeric())
                    }
                  )

                  well <- character()
                  value <- numeric()
                  sample_names <- new_sample_names
                  for (i in seq_along(sample_names)) {
                    well[i] <- reactVars$wells[which(
                      reactVars$sample_names == sample_names[i]
                    )]
                    row <- peaks_max[peaks_max$sample == sample_names[i], ]
                    value[i] <- if (nrow(row) > 0 && !is.na(row$max_mass[1])) {
                      row$max_mass[1]
                    } else {
                      NA
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

                # Update result picker with done + failed samples
                done_bases <- reactVars$rslt_df$sample
                failed_in_run <- reactVars$failed_samples[
                  reactVars$failed_samples %in% reactVars$sample_names
                ]
                failed_in_run <- failed_in_run[nzchar(failed_in_run)]
                choices_ok <- paste0(done_bases, ".raw")
                choices_failed <- character(0)
                if (length(failed_in_run)) {
                  choices_failed <- paste0(failed_in_run, ".raw")
                }
                named_choices <- character(0)
                if (length(choices_ok) > 0) {
                  named_choices <- c(named_choices, choices_ok)
                  names(named_choices)[seq_along(choices_ok)] <- choices_ok
                }
                if (length(choices_failed) > 0) {
                  prev_len <- length(named_choices)
                  named_choices <- c(named_choices, choices_failed)
                  names(named_choices)[prev_len + seq_along(choices_failed)] <-
                    paste0(failed_in_run, " (failed)")
                }
                if (length(named_choices) > 0) {
                  if (is.null(result_files_sel())) {
                    result_files_sel(unname(named_choices[1]))
                  }
                  output$result_picker_ui <- shiny$renderUI(
                    shiny$div(
                      class = "result-picker",
                      shiny$selectInput(
                        ns("result_picker"),
                        "Select Sample",
                        choices = named_choices,
                        selected = result_files_sel()
                      )
                    )
                  )
                  session$sendCustomMessage("selectize-init", "result_picker")
                }

                # Save heatmap
                if (!file.exists(file.path(temp, "heatmap.rds"))) {
                  heatmap <- plate_heatmap(
                    reactVars$rslt_df,
                    all_wells = config_file()[["Well"]]
                  )
                  saveRDS(heatmap, file.path(temp, "heatmap.rds"))
                }
              } else {
                if (deconvolution_sidebar_vars$selected() == "folder") {
                  dir_sel_res2 <- deconvolution_sidebar_vars$dir()
                  is_raw_res2 <- grepl(
                    "\\.raw$",
                    dir_sel_res2,
                    ignore.case = TRUE
                  ) &&
                    dir.exists(dir_sel_res2)
                  selected_files <- if (is_raw_res2) {
                    dir_sel_res2
                  } else {
                    file.path(dir_sel_res2, target_selector_sel())
                  }
                } else {
                  selected_files <- raw_dirs
                }

                # Build picker: successful samples + failed samples (labelled)
                sel_base <- gsub(
                  "\\.raw$",
                  "",
                  basename(selected_files),
                  ignore.case = TRUE
                )
                db_fin <- file.path(
                  analysis_dest(),
                  paste0(trimws(input$analysis_name), ".db")
                )
                done_samples_fin <- tryCatch(
                  {
                    con_fin <- dbConnect(SQLite(), db_fin, flags = SQLITE_RO)
                    on.exit(dbDisconnect(con_fin), add = TRUE)
                    if (dbExistsTable(con_fin, "status")) {
                      dbGetQuery(
                        con_fin,
                        "SELECT sample FROM status WHERE state='done'"
                      )$sample
                    } else {
                      character(0)
                    }
                  },
                  error = function(e) character(0)
                )
                finished_files <- sel_base %in% done_samples_fin
                failed_mask <- sel_base %in%
                  reactVars$failed_samples &
                  !finished_files

                choices_ok <- basename(selected_files)[finished_files]
                choices_failed <- basename(selected_files)[failed_mask]

                named_choices <- character(0)
                if (length(choices_ok) > 0) {
                  named_choices <- c(named_choices, choices_ok)
                  names(named_choices)[seq_along(choices_ok)] <- choices_ok
                }
                if (length(choices_failed) > 0) {
                  prev_len <- length(named_choices)
                  named_choices <- c(named_choices, choices_failed)
                  names(named_choices)[prev_len + seq_along(choices_failed)] <-
                    paste0(
                      gsub("\\.raw$", "", choices_failed, ignore.case = TRUE),
                      " (failed)"
                    )
                }

                if (length(named_choices) > 0) {
                  sel_default <- if (!is.null(result_files_sel())) {
                    result_files_sel()
                  } else {
                    unname(named_choices[1])
                  }

                  output$result_picker_ui <- shiny$renderUI(
                    shiny$div(
                      class = "result-picker",
                      shiny$selectInput(
                        ns("result_picker"),
                        "Select Sample",
                        choices = named_choices,
                        selected = sel_default
                      )
                    )
                  )
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

              # Report generation is temporarily disabled

              # Enable continuation button to protein conversion
              enable(
                selector = "#app-deconvolution_main-forward_deconvolution"
              )

              # Change spinner: error icon when all samples failed, check otherwise
              hide(selector = "#app-deconvolution_main-processing")
              n_succeeded <- reactVars$current_total_files -
                reactVars$initial_file_count
              n_failed <- length(reactVars$failed_samples)
              n_total <- reactVars$expected_files
              if (n_succeeded == 0 && n_failed > 0) {
                show(selector = "#app-deconvolution_main-processing_error")
                title <- paste0(
                  "Finalized with errors (",
                  n_failed,
                  "/",
                  n_total,
                  " failed)"
                )
                write_log(paste(
                  "Deconvolution finalized — all",
                  n_failed,
                  "/",
                  n_total,
                  "sample(s) failed"
                ))
              } else {
                show(selector = "#app-deconvolution_main-processing_fin")
                if (n_failed > 0) {
                  title <- paste0(
                    "Finalized (",
                    n_failed,
                    "/",
                    n_total,
                    " failed)"
                  )
                  write_log(paste(
                    "Deconvolution finalized —",
                    n_succeeded,
                    "succeeded,",
                    n_failed,
                    "/",
                    n_total,
                    "failed"
                  ))
                } else {
                  write_log("Deconvolution finalized")
                }
              }
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
          isTRUE(deconvolution_sidebar_vars$use_config()) &&
          length(config_file()) &&
          "Well" %in% names(config_file()) &&
          any(
            !is.na(config_file()[["Well"]]) &
              nzchar(trimws(as.character(config_file()[["Well"]])))
          )
      ) {
        # Observe clicks on interactive heatmap to show spectra
        reactVars$click_observer <- shiny$observe({
          click_data <- event_data("plotly_click")
          if (shiny$isolate(reactVars$heatmap_ready) > 0L) {
            # DEBUG — remove once click behaviour is confirmed
            message("=== HEATMAP CLICK ===")
            message("click_data is.null: ", is.null(click_data))
            if (!is.null(click_data)) {
              message("  curveNumber : ", click_data$curveNumber)
              message(
                "  x           : ",
                click_data$x,
                " (class: ",
                class(click_data$x),
                ")"
              )
              message(
                "  y           : ",
                click_data$y,
                " (class: ",
                class(click_data$y),
                ")"
              )
              message("  pointNumber : ", click_data$pointNumber)
              message("  full dump   : ")
              message(paste(capture.output(print(click_data)), collapse = "\n"))
            }

            # curveNumber 0 = heatmap; 1 = selection scatter overlay (ignore)
            if (
              !is.null(click_data) &&
                isTRUE(click_data$curveNumber == 0) &&
                is.numeric(click_data$x)
            ) {
              y_val <- click_data$y
              row <- if (is.character(y_val) && y_val %in% LETTERS[1:16]) {
                y_val
              } else if (is.numeric(y_val)) {
                LETTERS[16 - floor(y_val) + 1]
              } else {
                NULL
              }

              message("  resolved row: ", if (is.null(row)) "NULL" else row)
              message("  resolved col: ", round(click_data$x))

              if (is.null(row)) {
                return()
              }
              col <- round(click_data$x)
              well_id <- paste0(row, col)

              message("  well_id: ", well_id)

              shiny$isolate(
                clicked_sample <-
                  reactVars$rslt_df$sample[reactVars$rslt_df$well_id == well_id]
              )

              message(
                "  clicked_sample: ",
                paste(clicked_sample, collapse = ", ")
              )

              if (length(clicked_sample) > 0 && nzchar(clicked_sample[1])) {
                runjs(paste0(
                  'document.getElementById("blocking-overlay").styl',
                  'e.display = "block";'
                ))
                result_files_sel(paste0(clicked_sample[1], ".raw"))
                # Unblock after renders complete (delay covers spectrum + table)
                delay(
                  2000,
                  runjs(paste0(
                    'document.getElementById("blocking-overlay").styl',
                    'e.display = "none";'
                  ))
                )
              }
            }
          }
        })

        #### Heatmap selection highlight observer ----
        # Draws a green shape rectangle (data coords) around the selected well
        shiny$observe({
          shiny$req(result_files_sel(), reactVars$heatmap_ready > 0L)

          sample_name <- gsub("\\.raw$", "", result_files_sel())
          well_id <- shiny$isolate(
            reactVars$rslt_df$well_id[reactVars$rslt_df$sample == sample_name]
          )

          if (length(well_id) > 0 && nzchar(well_id[1])) {
            row_letter <- substring(well_id[1], 1, 1)
            col_num <- as.numeric(substring(well_id[1], 2))
            # y-axis is categorical: A=index 0, B=1, ... P=15
            row_idx <- match(row_letter, LETTERS[1:16]) - 1

            delay(400, {
              plotlyProxy("heatmap", session) |>
                plotlyProxyInvoke(
                  "relayout",
                  list(
                    shapes = list(list(
                      type = "rect",
                      xref = "x",
                      yref = "y",
                      x0 = col_num - 0.5,
                      x1 = col_num + 0.5,
                      y0 = row_idx - 0.5,
                      y1 = row_idx + 0.5,
                      line = list(color = "rgba(80,200,100,0.95)", width = 3),
                      fillcolor = "rgba(0,0,0,0)"
                    ))
                  )
                )
            })
          }
        })
      } # end heatmap-click block

      #### Switch to running UI ----
      # Toggle to hide sidebar
      runjs("document.querySelector('button.collapse-toggle').click();")
      output$deconvolution_ui <- shiny$renderUI({
        has_wells <- "Well" %in%
          names(config_file()) &&
          any(
            !is.na(config_file()[["Well"]]) &
              nzchar(trimws(as.character(config_file()[["Well"]])))
          )
        show_heatmap <- deconvolution_sidebar_vars$selected() == "folder" &&
          isTRUE(deconvolution_sidebar_vars$use_config()) &&
          !is.null(config_file()) &&
          has_wells
        deconvolution_results_ui(ns, show_heatmap)
      })

      # Render status spinner icon
      delay(1000, show(selector = "#app-deconvolution_main-processing"))

      ### Render result spectrum
      spectrum_ready <- shiny$reactiveVal(FALSE)
      shiny$observeEvent(
        result_files_sel(),
        {
          spectrum_ready(FALSE)
        },
        ignoreNULL = FALSE
      )

      setup_plot_dl(
        input,
        output,
        session,
        "decon_spectrum",
        build_fn = function(theme) {
          shiny$req(result_files_sel())
          result_dir <- file.path(
            analysis_dest(),
            gsub(".raw", "_rawdata_unidecfiles", result_files_sel())
          )
          sel_base <- gsub(
            "\\.raw$",
            "",
            result_files_sel(),
            ignore.case = TRUE
          )
          db_sp <- file.path(
            analysis_dest(),
            paste0(trimws(input$analysis_name), ".db")
          )
          is_raw_toggle <- as.logical(ifelse(
            !is.null(input$toggle_result),
            input$toggle_result,
            FALSE
          ))
          plot_data <- if (file.exists(db_sp)) {
            process_plot_data_db(db_sp, sel_base, raw = is_raw_toggle)
          } else {
            NULL
          }
          show_labels <- ifelse(
            is.null(input$spectrum_annotation),
            TRUE,
            input$spectrum_annotation
          )

          if (!is.null(plot_data)) {
            spectrum_plot(
              plot_data = plot_data,
              raw = is_raw_toggle,
              show_peak_labels = show_labels,
              show_mass_diff = FALSE,
              theme = theme
            )
          } else if (dir.exists(result_dir)) {
            spectrum_plot(
              result_path = result_dir,
              raw = is_raw_toggle,
              show_peak_labels = show_labels,
              show_mass_diff = FALSE,
              theme = theme
            )
          } else {
            shiny$req(FALSE)
          }
        },
        filename_fn = function() paste0(get_session_prefix(), "_Spectrum"),
        available_fn = spectrum_ready
      )

      output$spectrum <- renderPlotly({
        shiny$req(result_files_sel())
        waiter_show(id = ns("spectrum"), html = spin_wave())

        result_dir <- file.path(
          analysis_dest(),
          gsub(".raw", "_rawdata_unidecfiles", result_files_sel())
        )

        # Check DB for failure / get plot data
        sel_base <- gsub("\\.raw$", "", result_files_sel(), ignore.case = TRUE)
        db_sp <- file.path(
          analysis_dest(),
          paste0(trimws(input$analysis_name), ".db")
        )
        is_raw_toggle <- as.logical(ifelse(
          !is.null(input$toggle_result),
          input$toggle_result,
          FALSE
        ))

        if (file.exists(db_sp) && sel_base %in% decon_failed_samples(db_sp)) {
          waiter_hide(id = ns("spectrum"))
          return(
            plotly::plot_ly(type = "scatter", mode = "markers") |>
              plotly::layout(
                paper_bgcolor = "rgba(0,0,0,0)",
                plot_bgcolor = "rgba(0,0,0,0)",
                xaxis = list(visible = FALSE),
                yaxis = list(visible = FALSE)
              )
          )
        }

        # Try DB first (works even when raw files were cleaned up)
        plot_data <- if (file.exists(db_sp)) {
          process_plot_data_db(db_sp, sel_base, raw = is_raw_toggle)
        } else {
          NULL
        }

        # Fall back to file-based reader when DB not available (e.g. older runs)
        if (is.null(plot_data) && dir.exists(result_dir)) {
          spectrum <- spectrum_plot(
            result_path = result_dir,
            raw = is_raw_toggle,
            show_peak_labels = ifelse(
              is.null(input$spectrum_annotation),
              TRUE,
              input$spectrum_annotation
            ),
            show_mass_diff = FALSE
          )
          waiter_hide(id = ns("spectrum"))
          spectrum_ready(TRUE)
          return(spectrum)
        }

        if (!is.null(plot_data)) {
          spectrum <- spectrum_plot(
            plot_data = plot_data,
            raw = is_raw_toggle,
            show_peak_labels = ifelse(
              is.null(input$spectrum_annotation),
              TRUE,
              input$spectrum_annotation
            ),
            show_mass_diff = FALSE
          )
          waiter_hide(id = ns("spectrum"))
          spectrum_ready(TRUE)
          return(spectrum)
        }
      })

      output$deconvolution_data <- DT::renderDataTable(server = FALSE, {
        shiny$req(result_files_sel())

        waiter_show(id = ns("deconvolution_data"), html = spin_wave())

        result_dir <- file.path(
          analysis_dest(),
          gsub(".raw", "_rawdata_unidecfiles", result_files_sel())
        )

        # Check DB for failure before rendering metrics
        sel_base_dt <- gsub(
          "\\.raw$",
          "",
          result_files_sel(),
          ignore.case = TRUE
        )
        db_dt <- file.path(
          analysis_dest(),
          paste0(trimws(input$analysis_name), ".db")
        )
        if (
          file.exists(db_dt) && sel_base_dt %in% decon_failed_samples(db_dt)
        ) {
          waiter_hide(id = ns("deconvolution_data"))
          return(
            DT::datatable(
              data = data.frame(),
              options = list(dom = '', paging = FALSE)
            )
          )
        }

        # Try DB first, fall back to file
        db_metrics <- file.path(
          analysis_dest(),
          paste0(trimws(input$analysis_name), ".db")
        )
        error_rows <- if (file.exists(db_metrics)) {
          tryCatch(
            {
              con_m <- dbConnect(SQLite(), db_metrics, flags = SQLITE_RO)
              on.exit(dbDisconnect(con_m), add = TRUE)
              if (dbExistsTable(con_m, "error")) {
                dbGetQuery(
                  con_m,
                  "SELECT Key, Value FROM error WHERE sample = ?",
                  params = list(sel_base_dt)
                )
              } else {
                data.frame()
              }
            },
            error = function(e) data.frame()
          )
        } else {
          data.frame()
        }

        if (nrow(error_rows) == 0) {
          # Fall back to file when DB doesn't have the data yet
          deconvolution_data_path <- file.path(
            result_dir,
            paste0(gsub("_unidecfiles", "", basename(result_dir)), "_error.txt")
          )
          if (dir.exists(result_dir) && file.exists(deconvolution_data_path)) {
            raw_lines <- readLines(deconvolution_data_path)
            error_rows <- data.frame(
              Key = sub(" =.*", "", raw_lines),
              Value = as.numeric(sub(".*= ([^ ]+).*", "\\1", raw_lines))
            )
          }
        }

        if (nrow(error_rows) > 0) {
          tbl <- data.frame(
            Parameter = c(
              "Fitting Error",
              "Computation Time [s]",
              "Iteration Count",
              "UniScore (Quality)",
              "Sigma [m/z]",
              "Charge Sigma [z]",
              "Beat (Suppression)",
              "Point Sigma"
            )[seq_len(nrow(error_rows))],
            Value = round(error_rows$Value, 6)
          )

          waiter_hide(id = ns("deconvolution_data"))

          DT::datatable(
            data = tbl,
            escape = FALSE,
            rownames = FALSE,
            colnames = NULL,
            class = "order-column",
            selection = "none",
            options = list(
              dom = 't',
              paging = FALSE,
              scrollY = TRUE,
              scrollCollapse = TRUE,
              ordering = FALSE
            )
          ) |>
            DT::formatStyle(
              "Value",
              textAlign = "right"
            )
        }
      })

      ### Deconvolution metrics export ----
      deconvolution_metrics_raw <- shiny$reactive({
        shiny$req(result_files_sel())

        result_dir <- file.path(
          analysis_dest(),
          gsub(".raw", "_rawdata_unidecfiles", result_files_sel())
        )
        sel_base <- gsub("\\.raw$", "", result_files_sel(), ignore.case = TRUE)
        db_path <- file.path(
          analysis_dest(),
          paste0(trimws(input$analysis_name), ".db")
        )

        if (
          file.exists(db_path) && sel_base %in% decon_failed_samples(db_path)
        ) {
          return(data.frame())
        }

        error_rows <- if (file.exists(db_path)) {
          tryCatch(
            {
              con_m <- dbConnect(SQLite(), db_path, flags = SQLITE_RO)
              on.exit(dbDisconnect(con_m), add = TRUE)
              if (dbExistsTable(con_m, "error")) {
                dbGetQuery(
                  con_m,
                  "SELECT Key, Value FROM error WHERE sample = ?",
                  params = list(sel_base)
                )
              } else {
                data.frame()
              }
            },
            error = function(e) data.frame()
          )
        } else {
          data.frame()
        }

        if (nrow(error_rows) == 0) {
          data_path <- file.path(
            result_dir,
            paste0(gsub("_unidecfiles", "", basename(result_dir)), "_error.txt")
          )
          if (dir.exists(result_dir) && file.exists(data_path)) {
            raw_lines <- readLines(data_path)
            error_rows <- data.frame(
              Key = sub(" =.*", "", raw_lines),
              Value = as.numeric(sub(".*= ([^ ]+).*", "\\1", raw_lines))
            )
          }
        }

        shiny$req(nrow(error_rows) > 0)

        data.frame(
          Parameter = c(
            "Fitting error",
            "Computation time [s]",
            "Iteration count",
            "UniScore (Quality)",
            "m/z sigma [m/z]",
            "z (Charge) sigma [z]",
            "beat (Suppression)",
            "Point sigma"
          )[seq_len(nrow(error_rows))],
          Value = round(error_rows$Value, 6)
        )
      })

      setup_table_dl(
        input,
        output,
        session,
        "deconvolution_data",
        data_fn = function() deconvolution_metrics_raw(),
        filename_fn = function() {
          paste0(get_session_prefix(), "_Deconvolution_Metrics")
        }
      )

      ### Failure overlay messages for Spectrum and Metrics cards
      failure_msg_ui <- function(output_id) {
        shiny$renderUI({
          shiny$req(result_files_sel())
          sel <- gsub("\\.raw$", "", result_files_sel(), ignore.case = TRUE)
          db_fm <- file.path(
            analysis_dest(),
            paste0(trimws(input$analysis_name), ".db")
          )
          is_failed <- file.exists(db_fm) &&
            sel %in% decon_failed_samples(db_fm)
          if (is_failed) {
            shiny$div(
              class = "sample-failed-msg",
              "Sample failed to deconvolute."
            )
          }
        })
      }
      output$spectrum_failure_msg <- failure_msg_ui("spectrum_failure_msg")
      output$metrics_failure_msg <- failure_msg_ui("metrics_failure_msg")

      ### Render heatmap when config has wells specified
      if (
        deconvolution_sidebar_vars$selected() == "folder" &&
          isTRUE(deconvolution_sidebar_vars$use_config()) &&
          length(config_file()) &&
          "Well" %in% names(config_file()) &&
          any(
            !is.na(config_file()[["Well"]]) &
              nzchar(trimws(as.character(config_file()[["Well"]])))
          )
      ) {
        output$heatmap <- renderPlotly({
          shiny$req(nrow(reactVars$rslt_df) > 0)
          waiter_show(id = ns("heatmap"), html = spin_wave())

          heatmap <- plate_heatmap(
            reactVars$rslt_df,
            all_wells = config_file()[["Well"]]
          ) |>
            event_register("plotly_click")

          waiter_hide(id = ns("heatmap"))

          # Activate click observer / signal highlight observer to re-apply shape
          reactVars$heatmap_ready <- shiny$isolate(reactVars$heatmap_ready) + 1L

          return(heatmap)
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

        cleanup_wal(file.path(
          analysis_dest(),
          paste0(trimws(input$analysis_name), ".db")
        ))

        # Reset reactive status variables
        reset_progress()

        # Null dynamic UI
        output$decon_rep_logtext <- NULL
        output$decon_rep_logtext_ui <- NULL
        output$heatmap <- NULL

        # Switch back to initiation UI
        output$deconvolution_ui <- shiny$renderUI(
          deconvolution_init_ui(
            ns,
            analysis_name_default = smart_analysis_name()
          )
        )

        # Unblock mouse pointer
        runjs(paste0(
          'document.getElementById("blocking-overlay").styl',
          'e.display = "none";'
        ))

        # Re-open the main page sidebar
        runjs(paste0(
          "var aside = document.querySelector('aside.deconvolution-sidebar');",
          "var mainSb = aside ? aside.closest('.bslib-sidebar-layout') : null;",
          "if (mainSb && mainSb.classList.contains('sidebar-collapsed')) {",
          "  mainSb.classList.remove('sidebar-collapsed');",
          "  aside.removeAttribute('aria-hidden');",
          "  var tog = mainSb.querySelector('button.collapse-toggle');",
          "  if (tog) {",
          "    tog.style.display = '';",
          "    tog.setAttribute('aria-expanded', 'true');",
          "  }",
          "}"
        ))

        # bslib sets .transitioning during the toggle animation which hides
        # sidebar content. Force-remove it after the animation completes so
        # the sidebar content becomes visible again.
        delay(
          400,
          runjs(paste0(
            "document.querySelectorAll('.bslib-sidebar-layout')",
            ".forEach(function(el){el.classList.remove('transitioning');});"
          ))
        )

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
        Sys.sleep(0.75) # let OS release file handles before connecting
      }

      cleanup_wal(file.path(
        analysis_dest(),
        paste0(trimws(input$analysis_name), ".db")
      ))

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

        file_content <- if (
          !is.null(reactVars$decon_process_out) &&
            file.exists(reactVars$decon_process_out)
        ) {
          paste(
            readLines(reactVars$decon_process_out, warn = FALSE),
            collapse = "\n"
          )
        } else {
          "(log file not found)"
        }

        prelaunch <- reactVars$decon_prelaunch_info
        reactVars$deconvolution_log <- if (
          !is.null(prelaunch) && nzchar(prelaunch)
        ) {
          paste0(prelaunch, "\n", file_content)
        } else {
          file_content
        }

        reactVars$deconvolution_log
      })

      shiny$showModal(
        shiny$div(
          class = "start-modal log-modal",
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
              analysis_dest(),
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
                analysis_dest(),
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
              "KiwiMS",
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
              analysis_dest(),
              get_kiwims_version()["version"],
              get_kiwims_version()["date"],
              temp
            )

            # Start external report generation process
            tryCatch(
              {
                rep_process <- process$new(
                  command = file.path(R.home("bin"), "Rscript.exe"),
                  args = args,
                  wd = script_dir,
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
        analysis_dest(),
        paste0(trimws(input$analysis_name), ".db")
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
    shiny$observeEvent(input$peak_parameter_tooltip_bttn, {
      shiny$showModal(
        shiny$div(
          class = "start-modal",
          shiny$modalDialog(
            shiny$fluidRow(
              shiny$br(),
              shiny$column(
                width = 11,
                shiny$h5(
                  "Peak Detection Threshold"
                ),
                shiny$div(
                  class = "tooltip-text",
                  "The peak detection range specifies the local window to consider when detecting a peak. A peak needs to be the local max within a window of +/- this range to be considered a peak. For example, if you set the window as 10 Da, only peaks within a window of +/- 10 Da will be considered peak. Any other local maximum are ignored."
                ),
                shiny$br()
              )
            ),
            shiny$fluidRow(
              shiny$br(),
              shiny$column(
                width = 11,
                shiny$h5(
                  "Peak Detection Range (Da)"
                ),
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
            title = "Peak Parameter",
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
