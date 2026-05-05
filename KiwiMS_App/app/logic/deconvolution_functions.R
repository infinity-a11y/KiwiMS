# app/logic/deconvolution_functions.R

box::use(
  data.table[fread, setnames, data.table, as.data.table],
  DBI[
    dbConnect,
    dbDisconnect,
    dbExecute,
    dbWriteTable,
    dbGetQuery,
    dbExistsTable
  ],
  dplyr[left_join, mutate, n_distinct],
  ggplot2,
  parallel[
    clusterExport,
    clusterEvalQ,
    detectCores,
    makeCluster,
    parLapply,
    stopCluster
  ],
  plotly[config, event_register, layout],
  reticulate[use_python, py_config, py_run_string],
  RSQLite[SQLite, SQLITE_RO],
  scales[percent_format],
  utils[read.delim, read.table],
)

# db_with_retry(): BEGIN IMMEDIATE + body + COMMIT with R-level retry ----
# Retries the full transaction cycle on any lock/busy error, with random jitter.
db_with_retry <- function(con, expr, max_wait_s = 300) {
  deadline <- proc.time()[["elapsed"]] + max_wait_s
  repeat {
    ok <- tryCatch(
      {
        DBI::dbExecute(con, "BEGIN IMMEDIATE")
        tryCatch(
          {
            force(expr)
            DBI::dbExecute(con, "COMMIT")
          },
          error = function(e) {
            tryCatch(DBI::dbExecute(con, "ROLLBACK"), error = function(e2) NULL)
            stop(e)
          }
        )
        TRUE
      },
      error = function(e) {
        if (
          grepl("locked|busy", e$message, ignore.case = TRUE) &&
            proc.time()[["elapsed"]] < deadline
        ) {
          Sys.sleep(runif(1, 0.3, 1.2))
          FALSE
        } else {
          stop(e)
        }
      }
    )
    if (isTRUE(ok)) break
  }
}

# write_sample_status(): Write per-sample done/failed status to the shared DB ----
write_sample_status <- function(
  db_path,
  sample_name,
  state,
  reason = NULL,
  error_msg = NULL
) {
  tryCatch(
    {
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbExecute(con, "PRAGMA busy_timeout=300000")
      db_with_retry(con, {
        DBI::dbExecute(
          con,
          "INSERT OR REPLACE INTO status(sample,state,reason,error_msg,timestamp)
         VALUES (?,?,?,?,?)",
          params = list(
            sample_name,
            state,
            reason %||% NA_character_,
            error_msg %||% NA_character_,
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          )
        )
      })
    },
    error = function(e) {
      message("Could not write status to DB for ", sample_name, ": ", e$message)
    }
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# process_single_dir(): Processing a single waters dir ----
#' @export
process_single_dir <- function(
  waters_dir,
  result_dir,
  startz,
  endz,
  minmz,
  maxmz,
  masslb,
  massub,
  massbins,
  peakthresh,
  peakwindow,
  peaknorm,
  time_start,
  time_end,
  db_path,
  keep_raw_output = FALSE
) {
  input_path <- gsub("\\\\", "/", waters_dir)
  result_dir <- gsub("\\\\", "/", result_dir)

  # Derive sample base name before tryCatch so it is available in the error handler
  sample_basename <- gsub(
    "\\.raw$",
    "",
    basename(input_path),
    ignore.case = TRUE
  )

  # When discarding raw output, route UniDec intermediates to a per-sample
  # temp dir so the target directory stays clean throughout the run.
  # The temp dir is deleted after the DB write regardless of success/failure.
  if (!isTRUE(keep_raw_output)) {
    work_dir <- file.path(tempdir(), paste0("kiwims_", sample_basename))
    dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  } else {
    work_dir <- result_dir
  }

  # Function to properly format parameters for Python
  format_param <- function(x) {
    if (is.character(x) && x == "") {
      return("''")
    } else {
      return(as.character(x))
    }
  }

  # Create parameters string for Python
  params_string <- sprintf(
    paste0(
      '"startz": %s, "endz": %s, "minmz": %s, "maxmz": %s, "masslb": %s',
      ', "massub": %s, "massbins": %s, "peakthresh": %s, "peakwindow": ',
      '%s, "peaknorm": %s, "time_start": %s, "time_end": %s'
    ),
    format_param(startz),
    format_param(endz),
    format_param(minmz),
    format_param(maxmz),
    format_param(masslb),
    format_param(massub),
    format_param(massbins),
    format_param(peakthresh),
    format_param(peakwindow),
    format_param(peaknorm),
    format_param(time_start),
    format_param(time_end)
  )

  # Set up Conda environment
  tryCatch(
    {
      #       # 1. Dynamically locate the environment path
      #       base_system <- "C:/ProgramData/miniconda3/envs/kiwims"
      #       base_user <- file.path(
      #         Sys.getenv("LOCALAPPDATA"),
      #         "miniconda3/envs/kiwims"
      #       )

      #       # Choose the one that actually exists
      #       if (dir.exists(base_system)) {
      #         env_base <- base_system
      #       } else if (dir.exists(base_user)) {
      #         env_base <- base_user
      #       } else {
      #         stop("Kiwims environment not found in ProgramData or Local AppData.")
      #       }

      #       # Construct the bin path
      #       envBin <- file.path(env_base, "Library/bin")

      #       # 2. Update PATH
      #       Sys.setenv(PATH = paste(Sys.getenv("PATH"), envBin, sep = ";"))

      #       # 3. Tell Python where to look for DLLs
      #       # We use shQuote to handle spaces in folder names safely
      #       reticulate::py_run_string(sprintf(
      #         "
      # import os
      # os.add_dll_directory(r'%s')
      # ",
      #         envBin
      #       ))

      #       # 3. Optional DLL search env variable
      #       Sys.setenv(CONDA_DLL_SEARCH_MODIFICATION_ENABLE = "1")

      # Run unidec with python
      reticulate::py_run_string(sprintf(
        '
import sys
import unidec
import re
import os
import shutil
      
# Parameters passed from R
params = {%s}
input_file = r"%s"
result_dir = r"%s"
      
# Initialize UniDec engine
engine = unidec.UniDec()

# Convert Waters .raw to txt
engine.raw_process(input_file)
    
# Move processed file to output directory
txt_file = input_file.removesuffix(".raw") + "_rawdata.txt"
output = os.path.join(result_dir, os.path.basename(txt_file))
shutil.move(txt_file, output)

# Make result directory
engine.open_file(output)

# Set configuration parameters
engine.config.startz = params["startz"]
engine.config.endz = params["endz"]
engine.config.minmz = params["minmz"]
engine.config.maxmz = params["maxmz"]
engine.config.masslb = params["masslb"]
engine.config.massub = params["massub"]
engine.config.massbins = params["massbins"]
engine.config.peakthresh = params["peakthresh"]
engine.config.peakwindow = params["peakwindow"]
engine.config.peaknorm = params["peaknorm"]
engine.config.time_start = params["time_start"]
engine.config.time_end = params["time_end"]

# Process and deconvolve the data
engine.process_data()
engine.run_unidec()
engine.pick_peaks()
',
        params_string,
        input_path,
        work_dir
      ))

      # Write per-sample data to DB or record failure if output is missing/incomplete
      result <- file.path(
        work_dir,
        gsub(".raw", "_rawdata_unidecfiles", basename(input_path))
      )
      raw_name <- paste0(sample_basename, "_rawdata")
      mass_file <- file.path(result, paste0(raw_name, "_mass.txt"))
      peaks_file <- file.path(result, paste0(raw_name, "_peaks.dat"))

      if (
        dir.exists(result) && file.exists(mass_file) && file.exists(peaks_file)
      ) {
        conf_df <- read_file_safe(file.path(
          result,
          paste0(raw_name, "_conf.dat")
        ))
        if (nrow(conf_df) > 0) {
          conf_df <- conf_df[, 1:2]
          conf_df <- data.table::as.data.table(t(conf_df))
          data.table::setnames(conf_df, as.character(conf_df[1, ]))
          conf_df <- conf_df[-1, , drop = FALSE]
        }
        peaks_df <- read_file_safe(
          file.path(result, paste0(raw_name, "_peaks.dat")),
          c("mass", "intensity")
        )
        error_df <- read_file_safe(file.path(
          result,
          paste0(raw_name, "_error.txt")
        ))
        if (nrow(error_df) > 0) {
          error_df <- data.table::data.table(
            Key = as.character(error_df$V1),
            Value = as.numeric(error_df$V3)
          )
        }
        mass_df <- read_file_safe(
          file.path(result, paste0(raw_name, "_mass.txt")),
          c("mass", "intensity")
        )

        con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
        on.exit(DBI::dbDisconnect(con), add = TRUE)
        DBI::dbExecute(con, "PRAGMA busy_timeout=300000")

        write_tbl <- function(tbl, df) {
          if (is.null(df) || nrow(df) == 0) {
            return(invisible(NULL))
          }
          df <- as.data.frame(df)
          df$sample <- sample_basename
          DBI::dbWriteTable(con, tbl, df, append = TRUE)
        }

        db_with_retry(con, {
          for (tbl_name in c("peaks", "mass_data", "error", "config")) {
            if (DBI::dbExistsTable(con, tbl_name)) {
              DBI::dbExecute(
                con,
                sprintf("DELETE FROM %s WHERE sample = ?", tbl_name),
                params = list(sample_basename)
              )
            }
          }

          write_tbl("peaks", peaks_df)
          write_tbl("mass_data", mass_df)

          if (!is.null(error_df) && nrow(error_df) > 0) {
            err_df <- as.data.frame(error_df)
            err_df$sample <- sample_basename
            DBI::dbWriteTable(con, "error", err_df, append = TRUE)
          }

          if (!is.null(conf_df) && nrow(conf_df) > 0 && ncol(conf_df) > 0) {
            config_long <- data.frame(
              sample = sample_basename,
              key = names(conf_df),
              value = as.character(unlist(conf_df[1, ])),
              stringsAsFactors = FALSE
            )
            DBI::dbWriteTable(con, "config", config_long, append = TRUE)
          }

          DBI::dbExecute(
            con,
            "INSERT OR REPLACE INTO status(sample,state,reason,error_msg,timestamp)
             VALUES (?,?,?,?,?)",
            params = list(
              sample_basename,
              "done",
              NA_character_,
              NA_character_,
              format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            )
          )
        })
      } else {
        write_sample_status(db_path, sample_basename, "failed", "no_output_dir")
      }
    },
    error = function(e) {
      py_err <- reticulate::py_last_error()
      err_detail <- if (!is.null(py_err)) {
        paste(c(e$message, as.character(py_err)), collapse = "\n")
      } else {
        e$message
      }

      message("Error in single deconvolution processing: ", err_detail)
      cat(
        "Error in process_single_dir for",
        waters_dir,
        ":\n",
        err_detail,
        "\n"
      )

      write_sample_status(
        db_path,
        sample_basename,
        "failed",
        "error",
        err_detail
      )
    }
  )
}

# deconvolute(): Deconvolution ----
#' @export
deconvolute <- function(
  raw_dirs,
  result_dir,
  db_path,
  keep_raw_output = FALSE,
  num_cores = detectCores() - 2,
  startz = 1,
  endz = 50,
  minmz = "",
  maxmz = "",
  masslb = 5000,
  massub = 500000,
  massbins = 10,
  peakthresh = 0.1,
  peakwindow = 500,
  peaknorm = 1,
  time_start = "",
  time_end = ""
) {
  # Evaluate processing mode: parallel or sequential
  if (length(raw_dirs) > 40 && num_cores > 1) {
    message("Initiating ", num_cores, " cores for parallel processing ...")

    # Validate portable Python environment
    python_exe <- Sys.getenv("RETICULATE_PYTHON")
    if (!nzchar(python_exe) || !file.exists(python_exe)) {
      stop(
        "Python interpreter not found. RETICULATE_PYTHON is not set or points to a missing file."
      )
    } else {
      message("Python found: ", python_exe)
    }

    # Create log directory and define outfile
    outfile <- file.path(
      Sys.getenv("LOCALAPPDATA"),
      "KiwiMS",
      "last_cluster_log.txt"
    )
    writeLines(paste("Deconvolution Cluster Output", Sys.time()), outfile)

    # Set up the cluster
    message("Inducing cluster ...")
    cl <- parallel::makeCluster(num_cores, outfile = outfile)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Initialize reticulate and Conda environment in each worker
    message("Setting python environment for each worker ...")
    worker_lib_paths <- .libPaths()
    clusterExport(cl, "worker_lib_paths", envir = environment())
    clusterEvalQ(cl, .libPaths(worker_lib_paths))
    clusterEvalQ(cl, {
      library(reticulate)
      library(DBI)
      library(RSQLite)
      library(data.table)
    })

    # Initialize Python fully in each worker, one at a time.
    #
    # Python initialization across workers.
    #
    # conda DLL activation hooks write __conda_tmp_*.txt on every PyInitialize().
    # Simultaneous init across workers causes GetTempFileName collisions (Error 127).
    # The hook fires inside the DLL load itself and cannot be suppressed via env vars.
    # Workers must be initialized one at a time: clusterEvalQ on a single-node
    # cluster subset is synchronous and blocks until that worker finishes before
    # moving to the next.
    worker_python <- python_exe
    clusterExport(cl, "worker_python", envir = environment())
    for (i in seq_along(cl)) {
      clusterEvalQ(cl[i], {
        reticulate::use_python(worker_python, required = TRUE)
        reticulate::py_run_string("None") # force PyInitialize() now, not lazily
      })
    }

    # Create wrapper function that includes all parameters
    process_wrapper <- function(dir, params) {
      do.call(process_single_dir, c(list(waters_dir = dir), params))
    }

    # List of all parameters to pass to workers
    params_list <- list(
      result_dir = result_dir,
      db_path = db_path,
      keep_raw_output = keep_raw_output,
      startz = startz,
      endz = endz,
      minmz = minmz,
      maxmz = maxmz,
      masslb = masslb,
      massub = massub,
      massbins = massbins,
      peakthresh = peakthresh,
      peakwindow = peakwindow,
      peaknorm = peaknorm,
      time_start = time_start,
      time_end = time_end
    )

    # Export environment
    message("Passing functions and parameter to the workers ...")
    parallel::clusterExport(
      cl,
      c(
        "process_single_dir",
        "write_sample_status",
        "db_with_retry",
        "read_file_safe",
        "%||%",
        "process_wrapper",
        "params_list"
      ),
      envir = environment()
    )

    # Run parLapply with error handling and collect results.
    # capture.output suppresses worker stdout; par_results holds the actual
    # return values (NULL on error, non-NULL on success).
    message("Running parallel deconvolution ...")
    par_results <- NULL
    invisible(capture.output(
      {
        par_results <- parallel::parLapply(cl, raw_dirs, function(dir) {
          tryCatch(
            {
              process_wrapper(dir, params_list)
              TRUE
            },
            error = function(e) {
              message("Error processing ", dir, ": ", e$message)
              NULL
            }
          )
        })
      },
      type = "output"
    ))

    message("Parallel processing finalized.")

    # Check for errors: NULL return means the worker's tryCatch caught an error.
    failed_idx <- vapply(par_results, is.null, logical(1))
    if (any(failed_idx)) {
      warning(
        "Errors occurred for ",
        sum(failed_idx),
        " sample(s): ",
        paste(basename(raw_dirs[failed_idx]), collapse = ", ")
      )
    }
  } else {
    # Validate portable Python environment
    python_exe <- Sys.getenv("RETICULATE_PYTHON")
    if (!nzchar(python_exe) || !file.exists(python_exe)) {
      stop(
        "Python interpreter not found. RETICULATE_PYTHON is not set or points to a missing file."
      )
    } else {
      message("Python found: ", python_exe)
    }

    tryCatch(
      {
        use_python(python_exe, required = TRUE)
      },
      error = function(e) {
        message("Error initialising Python: ", e$message)
        return(NULL)
      }
    )

    message("Sequential processing started ...")
    tryCatch(
      {
        for (dir in seq_along(raw_dirs)) {
          process_single_dir(
            waters_dir = raw_dirs[dir],
            result_dir = result_dir,
            db_path = db_path,
            keep_raw_output = keep_raw_output,
            startz,
            endz,
            minmz,
            maxmz,
            masslb,
            massub,
            massbins,
            peakthresh,
            peakwindow,
            peaknorm,
            time_start,
            time_end
          )
        }

        message("Sequential processing finalized.")
      },
      error = function(e) {
        message("Error in sequential processing for dir ", dir, ": ", e$message)
        return(NULL)
      }
    )
  }
}

# create_384_plate_heatmap(): Make 384 well plate layout ----
#' @export
create_384_plate_heatmap <- function(data) {
  # A at top (index 1), P at bottom (index 16) — yaxis autorange="reversed"
  # maps them so A appears at the top of the plot
  rows <- LETTERS[1:16]
  cols <- 1:24

  plate_layout <- expand.grid(row = rows, col = cols) |>
    mutate(well_id = paste0(row, col))

  plate_data <- left_join(plate_layout, data, by = "well_id")

  # Build z matrix (rows = A..P, cols = 1..24) and tooltip text matrix
  z_mat <- matrix(0, nrow = 16, ncol = 24, dimnames = list(rows, cols))
  text_mat <- matrix("", nrow = 16, ncol = 24, dimnames = list(rows, cols))

  for (r in rows) {
    for (c in cols) {
      wid <- paste0(r, c)
      d <- plate_data[plate_data$well_id == wid, ]
      if (nrow(d) > 0 && !is.na(d$value[1])) {
        z_mat[r, as.character(c)] <- 1
        text_mat[r, as.character(c)] <- paste0(
          "Well: ",
          wid,
          "<br>Sample: ",
          d$sample[1]
        )
      } else {
        text_mat[r, as.character(c)] <- paste0("Well: ", wid, "<br>Empty")
      }
    }
  }

  # Colorscale:
  #   0 = empty well  — solid dark fill so xgap/ygap border is visible
  #   1 = occupied    — white
  colorscale <- list(c(0, "rgba(42,44,52,1)"), c(1, "white"))

  plotly::plot_ly(
    z = z_mat,
    x = cols,
    y = rows,
    type = "heatmap",
    colorscale = colorscale,
    showscale = FALSE,
    zmin = 0,
    zmax = 1,
    xgap = 2,
    ygap = 2,
    text = text_mat,
    hovertemplate = "%{text}<extra></extra>"
  ) |>
    layout(
      dragmode = FALSE,
      showlegend = FALSE,
      hoverlabel = list(
        bgcolor = "#38387Cdb",
        font = list(size = 14, color = "white"),
        bordercolor = "white"
      ),
      xaxis = list(
        side = "top",
        tickmode = "array",
        tickvals = cols,
        ticktext = as.character(cols),
        tickfont = list(color = "white", size = 12),
        tickangle = 0,
        ticklen = 0,
        showgrid = FALSE,
        zeroline = FALSE,
        automargin = FALSE,
        scaleanchor = "y",
        scaleratio = 1
      ),
      yaxis = list(
        autorange = "reversed",
        tickfont = list(color = "white", size = 12),
        ticklen = 0,
        showgrid = FALSE,
        zeroline = FALSE,
        scaleanchor = "x",
        scaleratio = 1,
        automargin = FALSE
      ),
      margin = list(t = 25, r = 0, b = 0, l = 30),
      plot_bgcolor = "rgba(160,160,170,0.25)",
      paper_bgcolor = "rgba(0,0,0,0)"
    ) |>
    config(
      displayModeBar = "hover",
      scrollZoom = FALSE,
      modeBarButtons = list(list(
        "zoom2d",
        "toImage",
        "autoScale2d",
        "resetScale2d",
        "zoomIn2d",
        "zoomOut2d"
      )),
      toImageButtonOptions = list(
        filename = paste0(Sys.Date(), "_Plate_Heatmap")
      )
    )
}

# process_plot_data(): Helper function to harmonize data for plotting ----
#' @export
process_plot_data <- function(
  sample = NULL,
  result_path = NULL,
  raw = FALSE,
  bin_width = 0.01
) {
  if (is.null(sample) & is.null(result_path)) {
    message(
      "Provide either the path to a '.rds' result file or a list object carrying sample results"
    )
    return(NULL)
  }

  if (!is.null(result_path)) {
    # Get file paths from deconvolution result
    base <- gsub("_unidecfiles", "", basename(result_path))
    raw_file <- file.path(result_path, paste0(base, "_rawdata.txt"))
    mass_file <- file.path(result_path, paste0(base, "_mass.txt"))
    peaks_file <- file.path(result_path, paste0(base, "_peaks.dat"))

    # Abort if files missing
    if (!file.exists(mass_file) || !file.exists(peaks_file)) {
      message("Mass or peak file missing in ", result_path)
      return()
    }

    if (raw) {
      mass <- data.table::fread(
        raw_file,
        sep = " ",
        col.names = c("mass", "intensity")
      )
      mass[, bin := floor(mass / bin_width) * bin_width + bin_width / 2]
      mass <- mass[, .(intensity = sum(intensity)), by = bin]
      data.table::setnames(mass, "bin", "mass")
      mass$intensity <- (mass$intensity - min(mass$intensity)) /
        (max(mass$intensity) - min(mass$intensity)) *
        100
      highlight_peaks <- NULL
    } else {
      # Read mass spectrum and filter zero intensity values
      mass <- utils::read.delim(
        mass_file,
        sep = " ",
        header = FALSE,
        col.names = c("mass", "intensity")
      ) |>
        dplyr::filter(intensity != 0)

      # Cut off outer limits
      mass <- mass[-c(1, nrow(mass)), ]

      # Read detected peaks
      peaks <- utils::read.delim(
        peaks_file,
        sep = " ",
        header = FALSE,
        col.names = c("mass", "intensity")
      )

      # Normalize intensities
      mass$intensity <- (mass$intensity - min(mass$intensity)) /
        (max(mass$intensity) - min(mass$intensity)) *
        100

      # Match peaks to spectrum
      highlight_peaks <- mass[mass$mass %in% peaks$mass, ]
    }
  } else if (!is.null(sample)) {
    if (is.null(sample$hits)) {
      message(
        "Sample has no annotated hits. See: 'add_hits()' applied to a result list."
      )
      return()
    }

    # Read mass spectrum and filter zero intensity values
    mass <- sample$mass |> dplyr::filter(intensity != 0) |> as.data.frame()

    # Cut off outer limits
    mass <- mass[-c(1, nrow(mass)), ]

    # Normalize intensities
    mass$intensity <- (mass$intensity - min(mass$intensity)) /
      (max(mass$intensity) - min(mass$intensity)) *
      100

    # Merge non-preferred hits per peak: sort preferred first, then collapse
    # multiple interpretations of the same peak into a single combined label.
    compound_hits <- sample$hits |>
      dplyr::arrange(
        `Peak [Da]`,
        dplyr::desc(Preferred == "TRUE"),
        dplyr::desc(suppressWarnings(as.numeric(`Compound Mw [Da]`)))
      ) |>
      dplyr::group_by(`Peak [Da]`) |>
      dplyr::reframe(
        Compound                = Compound[Preferred == "TRUE"][1],
        `Compound Mw [Da]`      = `Compound Mw [Da]`[Preferred == "TRUE"][1],
        `Binding Stoichiometry` = `Binding Stoichiometry`[Preferred == "TRUE"][1],
        mass_stoich_label       = paste(
          paste0("[", `Compound Mw [Da]`, "] x", `Binding Stoichiometry`),
          collapse = " + "
        )
      )

    # Peaks: protein + one entry per unique compound peak
    peaks <- c(
      unique(sample$hits$`Measured Mw Protein [Da]`),
      compound_hits$`Peak [Da]`
    )

    # Match peaks to mass spectrum
    indices <- match(peaks, mass$mass)
    peak_df <- mass[indices, ]

    # Get protein and compound names
    name <- c(
      unique(sample$hits$Protein),
      compound_hits$Compound
    )

    # Get molecular weights (preferred hit's theoretical mass)
    mw <- c(
      unique(sample$hits$`Mw Protein [Da]`),
      compound_hits$`Compound Mw [Da]`
    )

    # Get stoichiometry values (preferred hit)
    multiple <- c(1, compound_hits$`Binding Stoichiometry`)

    # Combined mass-shift label for hover; NA for the protein peak
    mass_stoich_label <- c(NA_character_, compound_hits$mass_stoich_label)

    # Summarize in data frame - one row per unique peak
    highlight_peaks <- cbind(peak_df, name, mw, multiple, mass_stoich_label) |>
      dplyr::filter(!is.na(name))
  }

  return(list(mass = mass, highlight_peaks = highlight_peaks))
}

# spectrum_plot(): Make spectrum plot interactively (plotly) or non-interactively (ggplot2) ----
#' @export
spectrum_plot <- function(
  result_path = NULL,
  sample = NULL,
  plot_data = NULL,
  raw = FALSE,
  interactive = TRUE,
  bin_width = 0.01,
  theme = "dark",
  color_cmp = NULL,
  color_variable = NULL,
  show_peak_labels = TRUE,
  show_mass_diff = TRUE
) {
  if (is.null(plot_data)) {
    plot_data <- process_plot_data(
      sample,
      result_path,
      raw = raw,
      bin_width = bin_width
    )
  }

  # Theme Styling Logic
  marker_fill_color <- "#ffa100"
  if (tolower(theme) == "light") {
    bg_color <- "rgba(0,0,0,0)"
    plot_bg_color <- "rgba(0,0,0,0)"
    font_color <- "black"
    grid_color <- "rgba(0, 0, 0, 0.1)"
    zeroline_color <- "rgba(0, 0, 0, 0.5)"
    data_line_color <- "black"
    marker_border_color <- "#000000"
  } else {
    bg_color <- "rgba(0,0,0,0)"
    plot_bg_color <- "rgba(0,0,0,0)"
    font_color <- "white"
    grid_color <- "rgba(255, 255, 255, 0.2)"
    zeroline_color <- "rgba(255, 255, 255, 0.5)"
    data_line_color <- "white"
    marker_border_color <- "#ffffff"
  }

  if (identical(color_variable, "Samples") && !is.null(color_cmp)) {
    data_line_color <- color_cmp
  }

  # ggplot (non-interactive) section
  if (!interactive) {
    plot <- ggplot2::ggplot(
      plot_data$mass,
      ggplot2::aes(
        x = mass,
        y = intensity,
        group = 1,
        text = paste0(
          "Mass: ",
          mass,
          " Da\nIntensity: ",
          round(intensity, 2)
        )
      )
    ) +
      ggplot2::geom_line(color = data_line_color) +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1))

    if (tolower(theme) == "dark") {
      plot <- plot + ggplot2::theme_dark()
    } else {
      plot <- plot + ggplot2::theme_minimal()
    }

    if (raw) {
      plot <- plot + ggplot2::labs(y = "Intensity [%]", x = "m/z [Th]")
    } else {
      plot <- plot +
        ggplot2::geom_point(
          data = plot_data$highlight_peaks,
          ggplot2::aes(x = mass, y = intensity),
          fill = "#e8cb97",
          colour = marker_border_color,
          shape = 21,
          size = 2
        ) +
        ggplot2::labs(y = "Intensity [%]", x = "Mass [Da]")
    }
    return(plot)
  }

  # Interactive plotly
  if (raw) {
    plot <- plotly::plot_ly(
      plot_data$mass,
      x = ~mass,
      y = ~intensity,
      type = "scattergl",
      mode = "lines",
      color = I(data_line_color),
      hoverinfo = "text",
      text = ~ paste0(
        "Mass: ",
        mass,
        " Da\nIntensity: ",
        round(intensity, 2),
        "%"
      )
    ) |>
      plotly::layout(
        hovermode = "closest",
        paper_bgcolor = bg_color,
        plot_bgcolor = plot_bg_color,
        font = list(size = 14, color = font_color),
        yaxis = list(
          title = "Intensity [%]",
          color = font_color,
          showgrid = TRUE,
          gridcolor = grid_color,
          zeroline = FALSE,
          zerolinecolor = zeroline_color,
          ticks = "outside",
          tickcolor = "transparent"
        ),
        xaxis = list(
          title = "m/z [Th]",
          color = font_color,
          showgrid = TRUE,
          gridcolor = grid_color,
          zeroline = FALSE,
          zerolinecolor = zeroline_color
        ),
        margin = list(t = 0, r = 0, b = 0, l = 50)
      ) |>
      plotly::config(
        displayModeBar = "hover",
        scrollZoom = FALSE,
        modeBarButtons = list(list(
          "zoom2d",
          "toImage",
          "autoScale2d",
          "resetScale2d",
          "zoomIn2d",
          "zoomOut2d"
        ))
      )
  } else {
    plot <- plotly::plot_ly(
      plot_data$mass,
      x = ~mass,
      y = ~intensity,
      type = "scattergl",
      mode = "lines",
      color = I(data_line_color),
      hoverinfo = "text",
      text = ~ paste0(
        "Mass: ",
        mass,
        " Da\nIntensity: ",
        round(intensity, 2),
        "%"
      )
    )

    # Prepare yaxis list
    yaxis_list <- list(
      title = "Intensity [%]",
      color = font_color,
      showgrid = TRUE,
      gridcolor = grid_color,
      zeroline = FALSE,
      zerolinecolor = zeroline_color,
      ticks = "outside",
      tickcolor = "transparent"
    )

    # Prepare xaxis list
    xaxis_list <- list(
      title = "Mass [Da]",
      color = font_color,
      showgrid = TRUE,
      gridcolor = grid_color,
      zeroline = FALSE,
      zerolinecolor = zeroline_color
    )

    # If annotated peaks present add markers
    if (
      isFALSE(
        nrow(plot_data$highlight_peaks) == 1 & anyNA(plot_data$highlight_peaks)
      )
    ) {
      if (is.null(sample)) {
        # Simple peaks from file or DB — no compound annotation columns
        plot <- plotly::add_markers(
          plot,
          data = plot_data$highlight_peaks,
          x = ~mass,
          y = ~intensity,
          marker = list(
            color = marker_fill_color,
            line = list(
              color = marker_border_color,
              width = 1,
              zindex = 100
            ),
            symbol = "circle",
            size = 12,
            zindex = 100
          ),
          hoverinfo = "text",
          text = ~ paste0(
            "Mass: ",
            mass,
            " Da\nIntensity: ",
            round(intensity, 2),
            "%"
          ),
          showlegend = FALSE
        )
      } else {
        if (!is.null(color_cmp)) {
          # Prepare marker symbols
          plot_data$highlight_peaks <- dplyr::mutate(
            plot_data$highlight_peaks,
            symbol = ifelse(
              name == plot_data$highlight_peaks$name[1],
              "diamond",
              "circle"
            ),
            color = ifelse(
              name == plot_data$highlight_peaks$name[1],
              marker_fill_color,
              if (tolower(theme) == "light") "#e0e0e0" else "#333333"
            ),
            linecolor = marker_border_color
          )

          # Prepare marker colors
          if (color_variable == "Compounds") {
            color_cmp <- c(marker_fill_color, color_cmp)
            names(color_cmp) <- c(
              plot_data$highlight_peaks$name[
                !plot_data$highlight_peaks$name %in% names(color_cmp)
              ],
              names(color_cmp)[-1]
            )

            plot_data$highlight_peaks$color <- color_cmp[match(
              if (color_variable == "Samples") {
                as.character(plot_data$highlight_peaks$mw)
              } else if (color_variable == "Compounds") {
                plot_data$highlight_peaks$name
              },
              names(color_cmp)
            )]
          }
        }

        plot <- plotly::add_markers(
          plot,
          data = plot_data$highlight_peaks,
          x = ~mass,
          y = ~intensity,
          marker = list(
            color = if (!is.null(color_cmp)) {
              ~ I(color)
            } else {
              marker_fill_color
            },
            line = list(
              # color = marker_border_color,
              color = ~ I(linecolor),
              width = 1
            ),
            symbol = ~ I(symbol),
            size = 12,
            zindex = 100
          ),
          hoverinfo = "text",
          text = ~ paste0(
            "Name: ", name,
            "\nMeasured: ", mass, " Da",
            "\nIntensity: ", round(intensity, 2), "%\n",
            ifelse(
              is.na(mass_stoich_label),
              paste0("Theor. Mw: ", mw),
              paste0("Mass Shifts: ", mass_stoich_label)
            )
          ),
          showlegend = FALSE
        )
      }

      #   # Annotation logic for mass difference + per-peak labels
      #   shapes <- NULL
      #   annotations <- NULL
      #   unique_masses <- unique(plot_data$highlight_peaks$mass)

      #   # Mass difference connector (if enabled and exactly two unique masses)
      #   if (show_mass_diff && length(unique_masses) == 2) {
      #     x1 <- min(unique_masses)
      #     x2 <- max(unique_masses)
      #     diff <- x2 - x1
      #     i1 <- plot_data$highlight_peaks$intensity[
      #       plot_data$highlight_peaks$mass == x1
      #     ][1]
      #     i2 <- plot_data$highlight_peaks$intensity[
      #       plot_data$highlight_peaks$mass == x2
      #     ][1]
      #     y_max_peak <- max(i1, i2, na.rm = TRUE)
      #     y_offset <- 5
      #     y_line <- y_max_peak + y_offset
      #     y_text <- y_line + (y_offset / 2)
      #     mid_x <- (x1 + x2) / 2
      #     diff_text <- sprintf("%.2f Da", diff)

      #     shapes <- list(
      #       list(
      #         type = "line",
      #         x0 = x1,
      #         y0 = i1,
      #         x1 = x1,
      #         y1 = y_line,
      #         line = list(color = font_color, width = 1, dash = "dot")
      #       ),
      #       list(
      #         type = "line",
      #         x0 = x2,
      #         y0 = i2,
      #         x1 = x2,
      #         y1 = y_line,
      #         line = list(color = font_color, width = 1, dash = "dot")
      #       ),
      #       list(
      #         type = "line",
      #         x0 = x1,
      #         y0 = y_line,
      #         x1 = x2,
      #         y1 = y_line,
      #         line = list(color = font_color, width = 1, dash = "dot")
      #       )
      #     )

      #     annotations <- list(
      #       list(
      #         x = mid_x,
      #         y = y_text,
      #         text = diff_text,
      #         showarrow = FALSE,
      #         font = list(color = font_color, size = 12)
      #       )
      #     )
      #   }

      #   # Add diagonal leader and text label for each peak
      #   peak_labels <- list()
      #   leader_lines <- list()

      #   if (show_peak_labels) {
      #     # Compute ranges
      #     if (nrow(plot_data$mass) > 0) {
      #       x_min <- min(plot_data$mass$mass, na.rm = TRUE)
      #       x_max <- max(plot_data$mass$mass, na.rm = TRUE)
      #       x_range <- x_max - x_min
      #     } else {
      #       x_range <- 1000 # fallback reasonable default
      #     }

      #     # Temporary y_range estimate (will finalize later)
      #     temp_y_range <- 100 + 20 # rough estimate including buffers

      #     # Assumed plot aspect ratio (width / height) - adjust if your typical plot size differs
      #     assumed_aspect <- 1.8 # Typical for wide plots; e.g., 900x500 => 1.8

      #     # Desired vertical rise in y-data units (reduced for shorter lines)
      #     delta_y <- 2 # % units; reduced from 4

      #     # Compute delta_x for ~45-degree visual angle
      #     delta_x <- delta_y * x_range / (assumed_aspect * temp_y_range)

      #     # Fallback if ranges are zero/invalid
      #     if (!is.finite(delta_x) || delta_x <= 0) {
      #       delta_x <- 25 # reduced fallback in Da
      #     }

      #     for (i in seq_len(nrow(plot_data$highlight_peaks))) {
      #       px <- plot_data$highlight_peaks$mass[i]
      #       py <- plot_data$highlight_peaks$intensity[i]

      #       # Diagonal end point: up and right
      #       end_x <- px + delta_x
      #       end_y <- py + delta_y

      #       # Leader line (short segment + arrowhead at END for pointing up-right)
      #       leader_lines[[length(leader_lines) + 1]] <- list(
      #         type = "line",
      #         x0 = px,
      #         y0 = py,
      #         x1 = end_x,
      #         y1 = end_y,
      #         line = list(color = font_color, width = 1.5),
      #         arrowhead = 2, # arrow at end
      #         arrowsize = 0.9,
      #         arrowwidth = 1.3,
      #         standoff = 3, # small gap at start
      #         layer = "below" # <-- KEY CHANGE: place behind markers
      #       )

      #       # Text label slightly right and above the arrow end
      #       label_x <- end_x + (delta_x * 0.02)
      #       label_y <- end_y

      #       # Format mass nicely
      #       label_text <- sprintf("%.1f Da", px)

      #       peak_labels[[length(peak_labels) + 1]] <- list(
      #         x = label_x,
      #         y = label_y,
      #         text = label_text,
      #         showarrow = FALSE,
      #         font = list(color = font_color, size = 11),
      #         xanchor = "left",
      #         yanchor = "bottom"
      #       )
      #     }
      #   }

      #   # Combine all shapes and annotations
      #   all_shapes <- c(shapes, leader_lines)
      #   all_annotations <- c(annotations, peak_labels)

      #   # Precisely calculate the minimum required max_y_needed
      #   max_peak_y <- if (nrow(plot_data$highlight_peaks) > 0) {
      #     max(plot_data$highlight_peaks$intensity, na.rm = TRUE)
      #   } else {
      #     0
      #   }

      #   max_shape_y <- if (length(all_shapes) > 0) {
      #     max(
      #       sapply(all_shapes, function(s) max(c(s$y0, s$y1), na.rm = TRUE)),
      #       na.rm = TRUE
      #     )
      #   } else {
      #     0
      #   }

      #   max_anno_y <- if (length(all_annotations) > 0) {
      #     max(
      #       sapply(all_annotations, function(a) if (!is.null(a$y)) a$y else 0),
      #       na.rm = TRUE
      #     )
      #   } else {
      #     0
      #   }

      #   overall_max_y <- max(
      #     c(100, max_peak_y, max_shape_y, max_anno_y),
      #     na.rm = TRUE
      #   )

      #   # Add buffer depending on whether annotations are present
      #   if (show_peak_labels && nrow(plot_data$highlight_peaks) > 0) {
      #     # When peak labels are active → need more headroom for text above the highest peak
      #     text_buffer <- 5 # increased to prevent clipping of highest label
      #   } else if (show_mass_diff && length(unique_masses) == 2) {
      #     # When only mass diff is active → smaller buffer is usually enough
      #     text_buffer <- 3
      #   } else {
      #     # No annotations → minimal or no extra buffer needed
      #     text_buffer <- 2
      #   }

      #   max_y_needed <- overall_max_y + text_buffer

      #   yaxis_list$range <- c(0, max_y_needed)
      # } else {
      #   all_shapes <- NULL
      #   all_annotations <- NULL
      # }

      # plot <- plotly::layout(
      #   plot,
      #   hovermode = "closest",
      #   paper_bgcolor = bg_color,
      #   plot_bgcolor = plot_bg_color,
      #   font = list(size = 14, color = font_color),
      #   yaxis = yaxis_list,
      #   xaxis = xaxis_list,
      #   shapes = all_shapes,
      #   annotations = all_annotations,
      #   margin = list(t = 0, r = 0, b = 0, l = 50)
      # ) |>
      #   plotly::config(
      #     displayModeBar = "hover",
      #     scrollZoom = FALSE,
      #     modeBarButtons = list(list(
      #       "zoom2d",
      #       "toImage",
      #       "autoScale2d",
      #       "resetScale2d",
      #       "zoomIn2d",
      #       "zoomOut2d"
      #     ))
      #   )
      # Annotation logic for mass difference + per-peak labels
      shapes <- NULL
      annotations <- NULL
      unique_masses <- sort(unique(plot_data$highlight_peaks$mass))

      # Mass difference connector (if enabled and two or more unique masses)
      if (show_mass_diff && length(unique_masses) >= 2) {
        base_mass <- unique_masses[1]
        other_masses <- unique_masses[-1]
        base_i <- plot_data$highlight_peaks$intensity[
          plot_data$highlight_peaks$mass == base_mass
        ][1]
        global_max_i <- max(plot_data$highlight_peaks$intensity, na.rm = TRUE)
        y_offset <- 5 # Initial offset above global max intensity
        line_spacing <- 5 # Spacing between each difference line (adjust if text overlaps)

        # Calculate the maximum y_line needed for the base vertical
        num_diffs <- length(other_masses)
        max_y_line <- global_max_i + y_offset + (num_diffs - 1) * line_spacing

        # Add single vertical line for the base peak up to the highest y_line
        shapes <- list(
          list(
            type = "line",
            x0 = base_mass,
            y0 = base_i,
            x1 = base_mass,
            y1 = max_y_line,
            line = list(color = font_color, width = 1, dash = "dot")
          )
        )

        # Add branches for each other peak
        for (j in seq_along(other_masses)) {
          x2 <- other_masses[j]
          diff <- x2 - base_mass
          i2 <- plot_data$highlight_peaks$intensity[
            plot_data$highlight_peaks$mass == x2
          ][1]
          y_line <- global_max_i + y_offset + (j - 1) * line_spacing
          mid_x <- (base_mass + x2) / 2
          diff_text <- sprintf("%.2f Da", diff)

          # Vertical line from the other peak up to its y_line
          shapes[[length(shapes) + 1]] <- list(
            type = "line",
            x0 = x2,
            y0 = i2,
            x1 = x2,
            y1 = y_line,
            line = list(color = font_color, width = 1, dash = "dot")
          )

          # Horizontal line from base to other at y_line
          shapes[[length(shapes) + 1]] <- list(
            type = "line",
            x0 = base_mass,
            y0 = y_line,
            x1 = x2,
            y1 = y_line,
            line = list(color = font_color, width = 1, dash = "dot")
          )

          # Text annotation above the horizontal line
          y_text <- y_line + 1.5 # Adjust this offset if needed to position text nicely
          annotations[[length(annotations) + 1]] <- list(
            x = mid_x,
            y = y_text,
            text = diff_text,
            showarrow = FALSE,
            font = list(color = font_color, size = 12)
          )
        }
      }

      # NEW: Add short diagonal leader + text label for EACH peak (if enabled)
      peak_labels <- list()
      leader_lines <- list()

      if (show_peak_labels) {
        # Compute ranges
        if (nrow(plot_data$mass) > 0) {
          x_min <- min(plot_data$mass$mass, na.rm = TRUE)
          x_max <- max(plot_data$mass$mass, na.rm = TRUE)
          x_range <- x_max - x_min
        } else {
          x_range <- 1000 # fallback reasonable default
        }

        # Temporary y_range estimate (will finalize later)
        temp_y_range <- 100 + 20 # rough estimate including buffers

        # Assumed plot aspect ratio (width / height) - adjust if your typical plot size differs
        assumed_aspect <- 1.8 # Typical for wide plots; e.g., 900x500 => 1.8

        # Desired vertical rise in y-data units (reduced for shorter lines)
        delta_y <- 2 # % units; reduced from 4

        # Compute delta_x for ~45-degree visual angle
        delta_x <- delta_y * x_range / (assumed_aspect * temp_y_range)

        # Fallback if ranges are zero/invalid
        if (!is.finite(delta_x) || delta_x <= 0) {
          delta_x <- 25 # reduced fallback in Da
        }

        for (i in seq_len(nrow(plot_data$highlight_peaks))) {
          px <- plot_data$highlight_peaks$mass[i]
          py <- plot_data$highlight_peaks$intensity[i]

          # Diagonal end point: up and right
          end_x <- px + delta_x
          end_y <- py + delta_y

          # Leader line (short segment + arrowhead at END for pointing up-right)
          leader_lines[[length(leader_lines) + 1]] <- list(
            type = "line",
            x0 = px,
            y0 = py,
            x1 = end_x,
            y1 = end_y,
            line = list(color = font_color, width = 1.5),
            arrowhead = 2, # arrow at end
            arrowsize = 0.9,
            arrowwidth = 1.3,
            standoff = 3, # small gap at start
            layer = "below" # <-- KEY CHANGE: place behind markers
          )

          # Text label slightly right and above the arrow end
          label_x <- end_x + (delta_x * 0.02)
          label_y <- end_y

          # Format mass nicely
          label_text <- sprintf("%.1f Da", px)

          peak_labels[[length(peak_labels) + 1]] <- list(
            x = label_x,
            y = label_y,
            text = label_text,
            showarrow = FALSE,
            font = list(color = font_color, size = 11),
            xanchor = "left",
            yanchor = "bottom"
          )
        }
      }

      # Combine all shapes and annotations
      all_shapes <- c(shapes, leader_lines)
      all_annotations <- c(annotations, peak_labels)

      # Precisely calculate the minimum required max_y_needed
      max_peak_y <- if (nrow(plot_data$highlight_peaks) > 0) {
        max(plot_data$highlight_peaks$intensity, na.rm = TRUE)
      } else {
        0
      }

      max_shape_y <- if (length(all_shapes) > 0) {
        max(
          sapply(all_shapes, function(s) max(c(s$y0, s$y1), na.rm = TRUE)),
          na.rm = TRUE
        )
      } else {
        0
      }

      max_anno_y <- if (length(all_annotations) > 0) {
        max(
          sapply(all_annotations, function(a) if (!is.null(a$y)) a$y else 0),
          na.rm = TRUE
        )
      } else {
        0
      }

      overall_max_y <- max(
        c(100, max_peak_y, max_shape_y, max_anno_y),
        na.rm = TRUE
      )

      # Add buffer depending on whether annotations are present
      if (show_peak_labels && nrow(plot_data$highlight_peaks) > 0) {
        # When peak labels are active → need more headroom for text above the highest peak
        text_buffer <- 5 # increased to prevent clipping of highest label
      } else if (show_mass_diff && length(unique_masses) >= 2) {
        # When only mass diff is active → smaller buffer is usually enough
        text_buffer <- 3
      } else {
        # No annotations → minimal or no extra buffer needed
        text_buffer <- 2
      }

      max_y_needed <- overall_max_y + text_buffer

      yaxis_list$range <- c(0, max_y_needed)
    } else {
      all_shapes <- NULL
      all_annotations <- NULL
    }

    plot <- plotly::layout(
      plot,
      hovermode = "closest",
      paper_bgcolor = bg_color,
      plot_bgcolor = plot_bg_color,
      font = list(size = 14, color = font_color),
      yaxis = yaxis_list,
      xaxis = xaxis_list,
      shapes = all_shapes,
      annotations = all_annotations,
      margin = list(t = 0, r = 0, b = 0, l = 50)
    ) |>
      plotly::config(
        displayModeBar = "hover",
        scrollZoom = FALSE,
        modeBarButtons = list(list(
          "zoom2d",
          "toImage",
          "autoScale2d",
          "resetScale2d",
          "zoomIn2d",
          "zoomOut2d"
        ))
      )
  }

  return(plot)
}

# read_file_safe(): Optimized file reader function ----
read_file_safe <- function(filename, col_names = NULL) {
  if (!file.exists(filename)) {
    return(data.frame())
  }
  df <- data.table::fread(
    filename,
    header = FALSE,
    sep = " ",
    fill = TRUE,
    showProgress = FALSE
  )
  if (!is.null(col_names)) {
    data.table::setnames(df, col_names)
  }
  return(df)
}

# decon_progress_count(): Count done samples, optionally restricted to a set ----
# Pass `samples` (character vector of sample base names) to count only the
# samples being processed in the current run — avoids inflated counts from
# pre-existing done records when extending an existing DB.
#' @export
decon_progress_count <- function(db_path, samples = NULL) {
  tryCatch(
    {
      con <- DBI::dbConnect(
        RSQLite::SQLite(),
        db_path,
        flags = RSQLite::SQLITE_RO
      )
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      if (!DBI::dbExistsTable(con, "status")) {
        return(0L)
      }
      if (!is.null(samples) && length(samples) > 0) {
        ph <- paste(rep("?", length(samples)), collapse = ",")
        DBI::dbGetQuery(
          con,
          sprintf(
            "SELECT COUNT(*) AS n FROM status WHERE state='done' AND sample IN (%s)",
            ph
          ),
          params = as.list(samples)
        )$n
      } else {
        DBI::dbGetQuery(
          con,
          "SELECT COUNT(*) AS n FROM status WHERE state='done'"
        )$n
      }
    },
    error = function(e) 0L
  )
}

# decon_is_complete(): TRUE when the 'completed' sentinel table exists ----
#' @export
decon_is_complete <- function(db_path) {
  tryCatch(
    {
      con <- DBI::dbConnect(
        RSQLite::SQLite(),
        db_path,
        flags = RSQLite::SQLITE_RO
      )
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbExistsTable(con, "completed")
    },
    error = function(e) FALSE
  )
}

# decon_failed_samples(): Return character vector of sample names that failed ----
#' @export
decon_failed_samples <- function(db_path) {
  tryCatch(
    {
      con <- DBI::dbConnect(
        RSQLite::SQLite(),
        db_path,
        flags = RSQLite::SQLITE_RO
      )
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      if (!DBI::dbExistsTable(con, "status")) {
        return(character(0))
      }
      DBI::dbGetQuery(
        con,
        "SELECT sample FROM status WHERE state='failed'"
      )$sample
    },
    error = function(e) character(0)
  )
}

# process_plot_data_db(): Load spectrum data for a single sample from the DB ----
#' @export
process_plot_data_db <- function(
  db_path,
  sample_name,
  raw = FALSE,
  bin_width = 0.01
) {
  tryCatch(
    {
      con <- DBI::dbConnect(
        RSQLite::SQLite(),
        db_path,
        flags = RSQLite::SQLITE_RO
      )
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      if (raw) {
        if (!DBI::dbExistsTable(con, "rawdata")) {
          return(NULL)
        }
        mass <- DBI::dbGetQuery(
          con,
          "SELECT mass, intensity FROM rawdata WHERE sample = ?",
          params = list(sample_name)
        )
        if (nrow(mass) == 0) {
          return(NULL)
        }
        mass <- as.data.table(mass)
        mass[, bin := floor(mass / bin_width) * bin_width + bin_width / 2]
        mass <- mass[, .(intensity = sum(intensity)), by = bin]
        data.table::setnames(mass, "bin", "mass")
        mass$intensity <- (mass$intensity - min(mass$intensity)) /
          (max(mass$intensity) - min(mass$intensity)) *
          100
        return(list(mass = as.data.frame(mass), highlight_peaks = NULL))
      } else {
        if (!DBI::dbExistsTable(con, "mass_data")) {
          return(NULL)
        }
        mass <- DBI::dbGetQuery(
          con,
          "SELECT mass, intensity FROM mass_data WHERE sample = ? AND intensity != 0",
          params = list(sample_name)
        )
        if (nrow(mass) < 3) {
          return(NULL)
        }
        mass <- mass[-c(1, nrow(mass)), ]
        mass$intensity <- (mass$intensity - min(mass$intensity)) /
          (max(mass$intensity) - min(mass$intensity)) *
          100

        peaks <- if (DBI::dbExistsTable(con, "peaks")) {
          DBI::dbGetQuery(
            con,
            "SELECT mass, intensity FROM peaks WHERE sample = ?",
            params = list(sample_name)
          )
        } else {
          data.frame(mass = numeric(0), intensity = numeric(0))
        }

        highlight_peaks <- mass[mass$mass %in% peaks$mass, ]
        return(list(mass = mass, highlight_peaks = highlight_peaks))
      }
    },
    error = function(e) {
      message("process_plot_data_db error for ", sample_name, ": ", e$message)
      NULL
    }
  )
}

# generate_decon_rslt(): Finalise the SQLite DB after all workers complete ----
# Per-sample tables are already written by workers in process_single_dir().
# This function appends session/output_log and marks the run as completed.
#' @export
generate_decon_rslt <- function(
  log = NULL,
  output = NULL,
  db_path
) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, busy_timeout = 30000)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(
    con,
    "session",
    data.frame(line_num = seq_along(log), line = log, stringsAsFactors = FALSE),
    overwrite = TRUE
  )
  DBI::dbWriteTable(
    con,
    "output_log",
    data.frame(
      line_num = seq_along(output),
      line = output,
      stringsAsFactors = FALSE
    ),
    overwrite = TRUE
  )

  # Indexes for fast per-sample queries on large tables (idempotent)
  for (tbl in c(
    "rawdata",
    "input_dat",
    "peaks",
    "mass_data",
    "error",
    "config"
  )) {
    if (DBI::dbExistsTable(con, tbl)) {
      DBI::dbExecute(
        con,
        sprintf(
          "CREATE INDEX IF NOT EXISTS idx_%s_sample ON %s(sample)",
          tbl,
          tbl
        )
      )
    }
  }

  # Completion marker — Shiny observer polls for this table
  DBI::dbWriteTable(
    con,
    "completed",
    data.frame(
      finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    ),
    overwrite = TRUE
  )

  invisible(db_path)
}

# Checkpoint WAL into the main DB and remove sidecar files.
# Call after kill_tree() (with a preceding Sys.sleep for handle release).
# WAL is only deleted when already empty; SHM is always safe to remove.
#' @export
cleanup_wal <- function(db_path) {
  if (!file.exists(db_path)) return(invisible(NULL))
  tryCatch(
    {
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      DBI::dbExecute(con, "PRAGMA busy_timeout=8000")
      DBI::dbExecute(con, "PRAGMA wal_checkpoint(TRUNCATE)")
      DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
      DBI::dbDisconnect(con)
    },
    error = function(e) NULL
  )
  wal <- paste0(db_path, "-wal")
  shm <- paste0(db_path, "-shm")
  if (file.exists(wal) && file.size(wal) == 0)
    tryCatch(file.remove(wal), error = function(e) NULL)
  if (file.exists(shm))
    tryCatch(file.remove(shm), error = function(e) NULL)
  invisible(NULL)
}
