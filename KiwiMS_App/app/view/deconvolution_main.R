# app/view/deconvolution_main.R

box::use(
  bslib[card, card_body, card_header, tooltip],
  DBI[dbConnect, dbDisconnect, dbExistsTable, dbGetQuery],
  RSQLite[SQLite, SQLITE_RO],
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
    updateProgressBar,
    pickerInput,
    pickerOptions,
    updatePickerInput
  ],
  clipr[write_clip],
  utils[capture.output, head, tail],
  waiter[useWaiter, spin_wave, waiter_show, waiter_hide, withWaiter],
)

box::use(
  app /
    logic /
    deconvolution_functions[
      add_metrics_table,
      plate_heatmap,
      plate_highlight,
      plate_layout,
      plate_sample_at,
      read_decon_metrics,
      read_decon_peak_counts,
      spectrum_plot,
      decon_progress_count,
      decon_is_complete,
      decon_failed_samples,
      decon_failure_detail,
      decon_planned_samples,
      decon_sample_cap_message,
      decon_samples_with_state,
      process_plot_data_db,
      cleanup_wal
    ],
  app / logic / helper_functions[fill_empty, get_kiwims_version],
  app / logic / help_modal[bind_help],
  app / logic / help_pages[deconvolution_help],
  app / logic / plot_download[setup_plot_dl],
  app / logic / logging[write_log, get_log, get_session_prefix],
  app / logic / user_settings[update_user_setting, read_user_settings],
  app / logic / conversion_functions[read_decon_metadata, multiple_spectra],
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
    ],
  app /
    logic /
    ms_formats[
      is_ms_input,
      list_ms_inputs,
      ms_result_dirname,
      ms_sample_base,
      ms_sample_in,
      ms_duplicate_samples,
      describe_duplicate_samples
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
      # Well Plate: tile layout of the run (plate_layout()) and the detected
      # peak count of every sample done so far
      plate = NULL,
      peak_counts = data.frame(sample = character(0), n_peaks = integer(0)),
      # Samples drawn by "Show All" once the run is finished
      show_all_samples = character(0),
      failed_samples = character(0),
      logs = "",
      deconv_report_status = NULL,
      continue_conversion = NULL,
      stale_unidec_output = character(0),
      overwrite = character(0),
      duplicated = "Overwrite Samples"
    )

    # Observers created anew on every deconvolution start, one slot per role.
    # Re-creating a role destroys its predecessor so repeated runs in a session
    # do not stack copies that handle the same events several times.
    run_observers <- new.env(parent = emptyenv())
    set_run_observer <- function(name, observer) {
      if (!is.null(run_observers[[name]])) {
        run_observers[[name]]$destroy()
      }
      run_observers[[name]] <- observer
      invisible(observer)
    }

    decon_rep_process_data <- shiny$reactiveVal(NULL)
    result_files_sel <- shiny$reactiveVal(NULL)
    target_selector_sel <- shiny$reactiveVal()

    # Picker value of the "Show All" entry; no sample name looks like this
    show_all_value <- "__show_all__"
    is_show_all <- function(sel = result_files_sel()) {
      identical(sel, show_all_value)
    }

    shiny$observe({
      if (!is.null(input$result_picker)) {
        result_files_sel(input$result_picker)
      }
      if (!is.null(input$target_selector)) {
        target_selector_sel(input$target_selector)
      }
    })

    # The sample inputs (Waters .raw folders, Thermo .raw files, mzML, mzXML)
    # the current run targets.
    #
    # With a config attached the targets come from its Sample column and the
    # target selector is never filled in — so anything that reads
    # target_selector_sel() to find the run's samples comes up empty in config
    # mode, however many samples the engine actually finished.
    run_target_files <- function() {
      dir_path <- deconvolution_sidebar_vars$dir()

      if (is.null(dir_path) || !nzchar(dir_path)) {
        return(character(0))
      }

      if (is_ms_input(dir_path)) {
        return(dir_path)
      }

      if (!dir.exists(dir_path)) {
        return(character(0))
      }

      if (
        isTRUE(deconvolution_sidebar_vars$use_config()) &&
          length(config_file()) &&
          "Sample" %in% names(config_file())
      ) {
        # Matched on sample name, so a config listing "A1" finds A1.raw (or
        # A1.mzML); return the inputs themselves, in config order.
        inputs <- list_ms_inputs(dir_path)
        hit <- match(
          ms_sample_base(config_file()[["Sample"]]),
          ms_sample_base(inputs)
        )
        return(inputs[hit[!is.na(hit)]])
      }

      file.path(dir_path, target_selector_sel())
    }

    decon_process_data <- shiny$reactiveVal(NULL)

    # The input files the run would queue, by the rules the start handler
    # applies: the single selected sample, the config's samples found in the
    # folder, or the picker's selection (everything while it is empty).
    planned_inputs <- function() {
      dir_path <- deconvolution_sidebar_vars$dir()
      if (is.null(dir_path) || !nzchar(dir_path)) {
        return(character(0))
      }
      # A Thermo .raw or an mzML is a file, so dir.exists() alone would treat a
      # single selected sample as nothing selected.
      if (is_ms_input(dir_path)) {
        return(dir_path)
      }
      if (!dir.exists(dir_path)) {
        return(character(0))
      }
      raw_dirs <- list_ms_inputs(dir_path)
      if (
        isTRUE(deconvolution_sidebar_vars$use_config()) &&
          length(config_file())
      ) {
        return(raw_dirs[ms_sample_in(raw_dirs, config_file()[["Sample"]])])
      }
      # The live selection of the start dialog's picker, so the checks follow
      # every tick; the persisted one while the picker is still rendering
      sel <- input$target_selector %||% target_selector_sel()
      if (length(sel) > 0) raw_dirs[basename(raw_dirs) %in% sel] else raw_dirs
    }

    # One line for the start dialog when two queued inputs share a sample name
    duplicate_sample_message <- function(inputs) {
      dups <- ms_duplicate_samples(inputs)
      if (!length(dups)) {
        return(NULL)
      }
      paste0(
        "<b>",
        length(dups),
        "</b> sample name(s) belong to more than one file: ",
        htmltools::htmlEscape(describe_duplicate_samples(dups)),
        ". Each sample needs a unique name &mdash; rename or move one of the",
        " files",
        if (
          isTRUE(deconvolution_sidebar_vars$use_config()) &&
            length(config_file())
        ) {
          "."
        } else {
          ", or deselect it."
        }
      )
    }

    # Samples the run would leave in its analysis database (see
    # decon_planned_samples()): the queried inputs plus those already done in
    # the database the analysis name points at
    planned_db_samples <- function() {
      dest_dir <- effective_dest() %||% deconvolution_sidebar_vars$targetpath()
      name <- trimws(input$analysis_name %||% "")
      db_path <- if (!is.null(dest_dir) && nzchar(name)) {
        file.path(dest_dir, paste0(name, ".db"))
      }
      decon_planned_samples(planned_inputs(), db_path)
    }

    # One line for the start dialog when the run would exceed the sample cap
    sample_cap_message <- decon_sample_cap_message

    ### Smart analysis name suggestion ----
    # Base session name, e.g. "KiwiMS_2026-04-03_id1234"
    session_base_name <- gsub("\\.log$", "", basename(log_path))

    # Compute the lowest non-existing name in the target folder
    smart_analysis_name <- shiny$reactive({
      reset_button() # re-evaluate after reset so file.exists() sees newly created .db files
      dir_path <- deconvolution_sidebar_vars$dir()
      base <- if (
        !is.null(dir_path) &&
          length(dir_path) == 1 &&
          !is.na(dir_path) &&
          nzchar(dir_path)
      ) {
        ms_sample_base(dir_path)
      } else {
        session_base_name
      }
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

    # Update the text input when the output path or input path changes
    shiny$observeEvent(
      list(
        deconvolution_sidebar_vars$targetpath(),
        deconvolution_sidebar_vars$dir()
      ),
      {
        suggested <- smart_analysis_name()
        shiny$updateTextInput(
          session,
          "analysis_name",
          value = suggested,
          placeholder = suggested
        )
      }
    )

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

      # m/z and mass are continuous quantities and UniDec stores both as
      # doubles -- its own fallback for an unset m/z range is the data's own
      # min/max, which is fractional. Requiring whole numbers here rejected
      # perfectly good bounds (a measured envelope starts at 512.4, not 512)
      # for no reason on either side. Charge state above is different: a charge
      # really is an integer count, so that check stays.
      if (!is.null(input$minmz) && !is.null(input$maxmz)) {
        if (is.na(input$minmz) || is.na(input$maxmz)) {
          return("m/z range requires valid numbers ...")
        }
        if (input$minmz < 1 || input$maxmz < 1) {
          return("m/z values must be at least 1 ...")
        }
        if (input$minmz >= input$maxmz) {
          return("High m/z must be greater than low m/z ...")
        }
      }

      if (!is.null(input$masslb) && !is.null(input$massub)) {
        if (is.na(input$masslb) || is.na(input$massub)) {
          return("Mass Mw range requires valid numbers ...")
        }
        if (input$masslb < 1 || input$massub < 1) {
          return("Mass Mw values must be at least 1 Da ...")
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

      # Either elution bound may be blank: blank start reads from the first
      # scan, blank end to the last.
      ts <- input$time_start
      te <- input$time_end
      if (!is.null(ts) && !is.na(ts) && !is.null(te) && !is.na(te)) {
        if (ts >= te) {
          return("Retention start time must be earlier than end time ...")
        }
      }

      sel <- deconvolution_sidebar_vars$selected()
      if (!is.null(sel) && sel == "folder") {
        dir_path <- deconvolution_sidebar_vars$dir()
        if (length(dir_path) == 0 || !nzchar(dir_path)) {
          return("Select target file(s) from the sidebar to start ...")
        }
        is_raw_itself <- is_ms_input(dir_path)
        if (!is_raw_itself && length(list_ms_inputs(dir_path)) == 0) {
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
        # Blank is saved too: it is the open bound (first scan), not an error.
        ts <- input$time_start
        blank <- is.null(ts) || is.na(ts)
        update_user_setting("deconv_time_start", if (blank) NA_real_ else ts)
        shinyWidgets::show_toast(
          if (blank) {
            "Default elution start: first scan"
          } else {
            paste0("Default elution start ", ts, " [min]")
          },
          text = NULL,
          type = "success",
          timer = 3000,
          timerProgressBar = TRUE
        )
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
    shiny$observeEvent(
      input$save_time_end_btn,
      {
        # Blank is saved too: it is the open bound (last scan), not an error.
        te <- input$time_end
        blank <- is.null(te) || is.na(te)
        update_user_setting("deconv_time_end", if (blank) NA_real_ else te)
        shinyWidgets::show_toast(
          if (blank) {
            "Default elution end: last scan"
          } else {
            paste0("Default elution end ", te, " [min]")
          },
          text = NULL,
          type = "success",
          timer = 3000,
          timerProgressBar = TRUE
        )
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
      sample_bases <- ms_sample_base(basename(raw_dirs))
      count <- decon_progress_count(db, sample_bases)
      message("Done count from DB: ", count)
      count
    }

    #### render_result_picker ----
    # Re-rendered only when its choices change: a re-render every poll would
    # close the dropdown under a user who is just picking a sample.
    picker_state <- new.env(parent = emptyenv())
    render_result_picker <- function(choices, selected) {
      if (identical(picker_state$choices, choices)) {
        return(invisible(NULL))
      }
      picker_state$choices <- choices

      output$result_picker_ui <- shiny$renderUI(
        shiny$div(
          class = "result-picker",
          pickerInput(
            ns("result_picker"),
            "Select Sample",
            choices = choices,
            selected = selected,
            options = pickerOptions(
              liveSearch = TRUE,
              liveSearchPlaceholder = "Search samples ..."
            )
          )
        )
      )
      session$sendCustomMessage("selectize-init", "result_picker")
    }

    #### refresh_results ----
    # Reads which samples of the run are done or failed, and the number of
    # peaks detected in each done one, for the Well Plate and the sample
    # picker. Picker values are sample names in every mode. With `final` (the
    # run is over) and more than one sample deconvoluted, "Show All" heads the
    # picker and is selected.
    refresh_results <- function(final = FALSE) {
      shiny$isolate({
        db_path <- file.path(
          analysis_dest(),
          paste0(trimws(input$analysis_name), ".db")
        )
        samples <- reactVars$sample_names
        done_db <- decon_samples_with_state(db_path, samples, "done")
        done <- samples[samples %in% done_db]
        failed <- samples[
          samples %in% reactVars$failed_samples & !samples %in% done
        ]

        counts <- read_decon_peak_counts(db_path, done)
        if (!identical(counts, reactVars$peak_counts)) {
          reactVars$peak_counts <- counts
        }

        choices <- stats::setNames(
          c(done, failed),
          c(done, sprintf("%s (failed)", failed))
        )
        show_all <- final && length(done) > 1
        if (show_all) {
          reactVars$show_all_samples <- done
          choices <- c(stats::setNames(show_all_value, "Show All"), choices)
        }
        if (length(choices) == 0) {
          return(invisible(NULL))
        }

        sel <- result_files_sel()
        if (show_all) {
          sel <- show_all_value
        } else if (is.null(sel) || !sel %in% choices) {
          sel <- unname(choices[1])
        }
        result_files_sel(sel)
        render_result_picker(choices, sel)
      })
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
      reactVars$plate <- NULL
      reactVars$peak_counts <- data.frame(
        sample = character(0),
        n_peaks = integer(0)
      )
      reactVars$show_all_samples <- character(0)
      reactVars$failed_samples <- character(0)
      reactVars$last_check <- Sys.time()
      reactVars$results_last_check <- Sys.time()
      reactVars$deconv_report_status <- NULL

      decon_rep_process_data(NULL)

      output$result_picker_ui <- shiny$renderUI(NULL)
      picker_state$choices <- NULL
      runjs("document.body.classList.remove('decon-show-all');")
      output$spectrum_container <- shiny$renderUI(
        withWaiter(plotlyOutput(ns("spectrum"), height = "100%"))
      )
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
        is_ms_input(dir_path_modal)
      ) {
        dir_path_modal
      } else {
        list_ms_inputs(dir_path_modal)
      }
      sample_bases <- if (
        isTRUE(deconvolution_sidebar_vars$use_config()) &&
          length(config_file())
      ) {
        samps <- config_file()[["Sample"]]
        samps <- samps[ms_sample_in(samps, raw_dirs_all)]
        ms_sample_base(samps)
      } else {
        sel <- target_selector_sel()
        bases <- if (length(sel) > 0) sel else basename(raw_dirs_all)
        ms_sample_base(bases)
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

      # ── Warning 3: sample cap of an analysis database ──────────────────────
      cap_msg <- sample_cap_message(planned_db_samples())
      if (!is.null(cap_msg)) {
        warnings_list <- c(
          list(shiny$p(shiny$HTML(paste0(
            '<i class="fa-solid fa-circle-xmark" style="font-size:1em;',
            ' color:#ff5a23; margin-right:10px;"></i>',
            cap_msg
          )))),
          warnings_list
        )
      }

      # ── Warning 4: two inputs under one sample name ────────────────────────
      dup_msg <- duplicate_sample_message(planned_inputs())
      if (!is.null(dup_msg)) {
        warnings_list <- c(
          list(shiny$p(shiny$HTML(paste0(
            '<i class="fa-solid fa-circle-xmark" style="font-size:1em;',
            ' color:#ff5a23; margin-right:10px;"></i>',
            dup_msg
          )))),
          warnings_list
        )
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
          is_ms_input(dir_path_msg)
        ) {
          dir_path_msg
        } else {
          list_ms_inputs(dir_path_msg)
        }

        if (
          isTRUE(deconvolution_sidebar_vars$use_config()) &&
            length(config_file())
        ) {
          presence <- ms_sample_in(config_file()[["Sample"]], raw_dirs)
          extras <- basename(raw_dirs)[
            !ms_sample_in(raw_dirs, config_file()[["Sample"]])
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
            is_ms_input(dir_path_msg)
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

      # Over the sample cap: Continue stays disabled, the red line of the
      # warnings below says why. Decided here, after the enable() above, so
      # the two never race; re-evaluated on every change of the selection.
      if (!is.null(sample_cap_message(planned_db_samples()))) {
        disable(selector = "#app-deconvolution_main-deconvolute_start_conf")
      }
      # Same for two inputs sharing a sample name (red line in the warnings)
      if (!is.null(duplicate_sample_message(planned_inputs()))) {
        disable(selector = "#app-deconvolution_main-deconvolute_start_conf")
      }

      return(message)
    })

    # Dynamic rendering of target file selector
    output$target_sel_ui <- shiny$renderUI({
      input$deconvolute_start

      picker <- NULL

      dir_sel <- deconvolution_sidebar_vars$dir()
      is_raw_itself <- is_ms_input(dir_sel)

      if (
        !is_raw_itself &&
          deconvolution_sidebar_vars$selected() == "folder" &&
          (isFALSE(deconvolution_sidebar_vars$use_config()) ||
            length(config_file()) == 0)
      ) {
        files <- basename(list_ms_inputs(dir_sel))
        # Key chips as in the conversion tab's shortcut bar
        key <- function(k) shiny$span(class = "key", k)
        shortcut <- function(...) shiny$div(class = "shortcut-item", ...)
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
          ),
          # Mouse and keyboard handling lives in static/js/deconvolution.js
          shiny$div(
            class = "kiwi-file-hint",
            shortcut(key("Shift"), " + ", key("Click"), " / ", key("Drag"), " Range"),
            shortcut(key("↑"), key("↓"), " Move"),
            shortcut(key("Shift"), " + ", key("↑"), key("↓"), " Extend"),
            shortcut(key("Enter"), " Toggle"),
            shortcut(key("Ctrl"), " + ", key("A"), " All / None")
          )
        )
      }

      return(picker)
    })

    shiny$observeEvent(input$target_select_all, {
      dir_sel <- deconvolution_sidebar_vars$dir()
      files <- basename(list_ms_inputs(dir_sel))
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
      # Hard cap. Continue is disabled over the cap (see message_ui); this is
      # the safety net should it ever be pressed anyway. The dialog stays open
      # and its red line says why, so no toast (it would sit behind the dialog).
      if (!is.null(sample_cap_message(planned_db_samples()))) {
        write_log("Deconvolution refused - sample cap exceeded")
        return(NULL)
      }
      if (!is.null(duplicate_sample_message(planned_inputs()))) {
        write_log("Deconvolution refused - duplicate sample names")
        return(NULL)
      }

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

      # Snapshot the inputs this run was started with. Everything rendered or
      # polled for this run reads the snapshot: reading config_file() and the
      # sidebar reactively re-rendered the whole running interface (progress,
      # heatmap, buttons) whenever a different config was loaded afterwards.
      run_config <- config_file()
      run_use_config <- isTRUE(deconvolution_sidebar_vars$use_config())
      run_selected <- deconvolution_sidebar_vars$selected()

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
      if (run_selected == "folder") {
        dir_path <- deconvolution_sidebar_vars$dir()
        if (is_ms_input(dir_path)) {
          raw_dirs <- dir_path
        } else {
          raw_dirs <- list_ms_inputs(dir_path)
        }

        if (
          run_use_config &&
            length(run_config)
        ) {
          write_log("Multiple target deconvolution mode (with config file)")

          sample_names <- run_config[["Sample"]]
          raw_dirs <- raw_dirs[ms_sample_in(raw_dirs, sample_names)]
        } else if (is_ms_input(dir_path)) {
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
            !ms_sample_base(basename(raw_dirs)) %in%
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
        active_bases <- ms_sample_base(basename(raw_dirs))
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

      # Well Plate: the queued samples, on the config's wells when it gives
      # every one of them a well, in queue order otherwise (plate_layout())
      reactVars$sample_names <- ms_sample_base(basename(raw_dirs))
      run_wells <- if (
        run_use_config &&
          length(run_config) &&
          all(c("Sample", "Well") %in% names(run_config))
      ) {
        # "Plate:A1" -> "A1"
        cfg_wells <- gsub(",", "", sub("^.*:", "", run_config[["Well"]]))
        cfg_wells[match(
          reactVars$sample_names,
          ms_sample_base(run_config[["Sample"]])
        )]
      }
      reactVars$plate <- plate_layout(reactVars$sample_names, run_wells)

      # Render disabled results picker
      output$result_picker_ui <- shiny$renderUI(
        shiny$div(
          class = "result-picker",
          disabled(pickerInput(
            ns("result_picker"),
            "Select Sample",
            choices = "",
            options = pickerOptions(
              liveSearch = TRUE,
              liveSearchPlaceholder = "Search samples ..."
            )
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
          # Blank bounds stay NA (open window); NULL would break data.frame().
          time_start = input$time_start %||% NA_real_,
          time_end = input$time_end %||% NA_real_
        ),
        dirs = raw_dirs,
        selected = run_selected
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
            active_bases <- ms_sample_base(basename(raw_dirs))
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
      set_run_observer("exit_status", shiny$observe({
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
                run_selected == "folder" &&
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
      }))

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

      #### Results tracking observer for picker and Well Plate ----
      if (run_selected == "folder") {
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
            refresh_results()

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
          current_base_names <- ms_sample_base(basename(raw_dirs))
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
            ms_result_dirname(raw_dirs)
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
                run_selected == "folder" &&
                  !is.null(reactVars$results_observer)
              ) {
                reactVars$results_observer$destroy()
              }

              # Set reactive status variable "is_running" to FALSE
              reactVars$is_running <- FALSE

              # Final picker (with "Show All") and Well Plate state
              refresh_results(final = TRUE)

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

      run_db_path <- function() {
        file.path(
          analysis_dest(),
          paste0(trimws(input$analysis_name), ".db")
        )
      }

      #### Well Plate click observer ----
      # A click on a done or failed sample's tile selects it, in the picker
      # too. The plate has its own plotly source, so clicks on the spectrum
      # never land here. event_data() warns until the plate is first drawn and
      # its click event registered, which is expected right after the start.
      set_run_observer("heatmap_click", shiny$observeEvent(
        suppressWarnings(event_data("plotly_click", source = "decon_plate")),
        {
          click <- suppressWarnings(
            event_data("plotly_click", source = "decon_plate")
          )
          shiny$req(is.numeric(click$x), is.numeric(click$y))

          clicked_sample <- plate_sample_at(reactVars$plate, click$x, click$y)
          # Pending samples have nothing to show yet
          shiny$req(
            length(clicked_sample) == 1,
            clicked_sample %in% picker_state$choices
          )

          runjs(paste0(
            'document.getElementById("blocking-overlay").styl',
            'e.display = "block";'
          ))
          result_files_sel(clicked_sample)
          updatePickerInput(session, "result_picker", selected = clicked_sample)
          # Unblock after renders complete (delay covers the spectrum)
          delay(
            2000,
            runjs(paste0(
              'document.getElementById("blocking-overlay").styl',
              'e.display = "none";'
            ))
          )
        }
      ))

      #### Well Plate selection highlight observer ----
      # Frames the selected sample's tile; "Show All" clears the frame
      set_run_observer("heatmap_highlight", shiny$observe({
        sel <- result_files_sel()
        shiny$req(reactVars$heatmap_ready > 0L)

        shapes <- plate_highlight(
          shiny$isolate(reactVars$plate),
          if (!is.null(sel) && !is_show_all(sel)) ms_sample_base(sel)
        )

        delay(400, {
          plotlyProxy("heatmap", session) |>
            plotlyProxyInvoke("relayout", list(shapes = shapes))
        })
      }))

      #### Switch to running UI ----
      # Toggle to hide sidebar
      runjs("document.querySelector('button.collapse-toggle').click();")
      output$deconvolution_ui <- shiny$renderUI({
        deconvolution_results_ui(ns)
      })

      # Render status spinner icon
      delay(1000, show(selector = "#app-deconvolution_main-processing"))

      ### Render result spectrum
      spectrum_ready <- shiny$reactiveVal(FALSE)
      set_run_observer("spectrum_ready", shiny$observeEvent(
        result_files_sel(),
        {
          spectrum_ready(FALSE)
        },
        ignoreNULL = FALSE
      ))

      # The Spectrum settings offer the Cubic/Planar switch for "Show All"
      # and the single-sample ones otherwise. They live in a popover that is
      # not in the DOM while closed, so the swap is a body class main.scss
      # keys on rather than hiding the inputs themselves.
      set_run_observer("spectrum_settings_mode", shiny$observe({
        runjs(sprintf(
          "document.body.classList.toggle('decon-show-all', %s);",
          tolower(is_show_all())
        ))
      }))

      #### All spectra ("Show All") ----
      # Read once per finished run; switching Cubic/Planar only redraws
      show_all_data <- shiny$reactive({
        samples <- reactVars$show_all_samples
        shiny$req(length(samples) > 1)
        db <- run_db_path()
        stats::setNames(
          lapply(samples, function(s) process_plot_data_db(db, s)),
          samples
        )
      })

      all_spectra_plot <- function(theme = "light") {
        data <- show_all_data()
        data <- data[!vapply(data, is.null, logical(1))]
        shiny$req(length(data) > 0)
        samples <- names(data)

        multiple_spectra(
          results_list = NULL,
          samples = samples,
          cubic = !identical(input$spectrum_kind, "Planar"),
          color_cmp = stats::setNames(
            substr(viridisLite::viridis(length(samples), end = 0.9), 1, 7),
            samples
          ),
          color_variable = "Samples",
          # Fewer points per trace the more samples share the figure
          max_points = max(500, min(4000, floor(200000 / length(samples)))),
          theme = theme,
          plot_data = data
        )
      }

      #### Spectrum of the selection ----
      # The selected sample's spectrum, with its fit statistics beside it
      # while Show Metrics is on, or all spectra for "Show All". NULL when
      # there is no data for the sample.
      build_spectrum <- function(theme = "light") {
        sel <- result_files_sel()
        if (is_show_all(sel)) {
          return(all_spectra_plot(theme))
        }

        sel_base <- ms_sample_base(sel)
        result_dir <- file.path(analysis_dest(), ms_result_dirname(sel))
        db_sp <- run_db_path()
        is_raw_toggle <- isTRUE(as.logical(input$toggle_result))
        show_labels <- !isFALSE(input$spectrum_annotation)

        # Try DB first (works even when raw files were cleaned up), and fall
        # back to the file-based reader when it is not available (older runs)
        plot_data <- if (file.exists(db_sp)) {
          process_plot_data_db(db_sp, sel_base, raw = is_raw_toggle)
        }
        plot <- if (!is.null(plot_data)) {
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
        }

        if (!is.null(plot) && !isFALSE(input$spectrum_metrics)) {
          plot <- add_metrics_table(
            plot,
            read_decon_metrics(db_sp, sel_base, result_dir),
            theme = theme
          )
        }
        plot
      }

      setup_plot_dl(
        input,
        output,
        session,
        "decon_spectrum",
        build_fn = function(theme) {
          shiny$req(result_files_sel())
          plot <- build_spectrum(theme)
          shiny$req(plot)
          plot
        },
        filename_fn = function() paste0(get_session_prefix(), "_Spectrum"),
        available_fn = spectrum_ready
      )

      output$spectrum <- renderPlotly({
        shiny$req(result_files_sel())

        # spectrum_container decides whether to mount this output at all --
        # a failed sample gets the failure message in its place instead
        # (see below). This is just a defensive backstop against a stale
        # binding rather than something expected to fire in normal use.
        db_sp <- run_db_path()
        shiny$req(
          is_show_all() ||
            !(file.exists(db_sp) &&
              ms_sample_base(result_files_sel()) %in%
                decon_failed_samples(db_sp))
        )

        waiter_show(id = ns("spectrum"), html = spin_wave())
        on.exit(waiter_hide(id = ns("spectrum")))

        spectrum <- build_spectrum()
        spectrum_ready(TRUE)
        if (!is.null(spectrum)) {
          return(spectrum)
        }

        # Neither the DB nor the on-disk UniDec output had data to show.
        # For the raw m/z view this is expected when "Keep UniDec output files" was
        # off during deconvolution, since the raw files were never written
        # to disk. Surface a hint instead of leaving a blank plot.
        is_raw_toggle <- isTRUE(as.logical(input$toggle_result))
        hint_text <- if (is_raw_toggle) {
          if (!isTRUE(read_user_settings()$deconv_keep_raw_output)) {
            paste0(
              "No raw m/z spectrum available.<br>",
              "Enable \"Keep UniDec output files\" in Settings before running ",
              "deconvolution to store raw spectra for this view."
            )
          } else {
            "No raw m/z spectrum available for this sample."
          }
        } else {
          "No spectrum data available for this sample."
        }

        plotly::plot_ly(type = "scatter", mode = "markers") |>
          plotly::layout(
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(list(
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "middle",
              text = hint_text,
              showarrow = FALSE,
              font = list(size = 14, color = "white")
            ))
          )
      })

      ### Failure state for the Spectrum card ----

      # current_failed_selection(): (sample, cause, detail) for whichever
      # sample is selected right now, or NULL if it did not fail ----
      # A single cheap SQLite read; called fresh wherever it's needed rather
      # than cached, so it always reflects what is currently selected.
      current_failed_selection <- function() {
        shiny$req(result_files_sel())
        if (is_show_all()) {
          return(NULL)
        }
        sel <- ms_sample_base(result_files_sel())
        db_fm <- run_db_path()
        if (!file.exists(db_fm) || !(sel %in% decon_failed_samples(db_fm))) {
          return(NULL)
        }
        list(sample = sel, info = decon_failure_detail(db_fm, sel))
      }

      #### Spectrum card: plot, or the failure message in its place ----
      # A failed sample's plotlyOutput is never mounted at all (rather than
      # mounted-but-blank with the message overlaid on top of it): plotly
      # builds its own layered DOM under the hood, and that layering was
      # swallowing clicks on the Copy button placed over it. Showing one or
      # the other as the card's only content sidesteps that entirely.
      set_run_observer("copy_error", shiny$observeEvent(input$spectrum_copy_error, {
        current <- current_failed_selection()
        shiny$req(current)
        text <- paste(
          c(
            paste("Sample failed to deconvolute:", current$sample),
            if (!is.null(current$info)) current$info$cause,
            if (!is.null(current$info) && !is.null(current$info$detail)) {
              c("", current$info$detail)
            }
          ),
          collapse = "\n"
        )
        write_clip(text, allow_non_interactive = TRUE)
        runjs("alert('Error message copied to clipboard!');")
      }))

      output$spectrum_container <- shiny$renderUI({
        current <- current_failed_selection()
        if (is.null(current)) {
          return(withWaiter(plotlyOutput(ns("spectrum"), height = "100%")))
        }
        info <- current$info
        shiny$div(
          class = "sample-failed-msg",
          shiny$div(
            class = "sample-failed-msg-title",
            "Sample failed to deconvolute"
          ),
          if (!is.null(info)) {
            shiny$div(class = "sample-failed-msg-cause", info$cause)
          },
          if (!is.null(info) && !is.null(info$detail)) {
            shiny$tags$pre(class = "sample-failed-msg-detail", info$detail)
          },
          shiny$actionButton(
            ns("spectrum_copy_error"),
            "Copy",
            icon = shiny$icon("clipboard"),
            class = "sample-failed-copy-btn"
          )
        )
      })

      ### Render Well Plate ----
      # Drawn from the start, every sample of the run pending, and redrawn as
      # samples finish (peak counts) or fail (crosses)
      output$heatmap <- renderPlotly({
        plate <- reactVars$plate
        shiny$req(plate)
        waiter_show(id = ns("heatmap"), html = spin_wave())
        on.exit(waiter_hide(id = ns("heatmap")))

        heatmap <- plate_heatmap(
          plate,
          peak_counts = reactVars$peak_counts,
          failed = reactVars$failed_samples,
          source = "decon_plate"
        ) |>
          event_register("plotly_click")

        # Signal the highlight observer to re-apply the selection frame
        reactVars$heatmap_ready <- shiny$isolate(reactVars$heatmap_ready) + 1L

        heatmap
      })

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

            # Get path to the files needed for report generation. A per-user install
            # puts them in the user's Documents, an all-users install in Public
            # Documents, so fall back to the shared copy when the personal one is
            # absent.
            script_dir <- local({
              user_dir <- file.path(
                Sys.getenv("USERPROFILE"), "Documents", "KiwiMS", "report"
              )
              public_dir <- file.path(
                Sys.getenv("PUBLIC"), "Documents", "KiwiMS", "report"
              )
              if (dir.exists(user_dir)) user_dir else public_dir
            })

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

    ### Help modals ----
    bind_help(input, deconvolution_help)

    return(
      shiny::reactiveValues(
        forward_deconvolution = shiny::reactive(input$forward_deconvolution),
        continue_conversion = shiny::reactive(reactVars$continue_conversion)
      )
    )
  })
}
