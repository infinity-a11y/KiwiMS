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
    clusterApplyLB,
    clusterCall,
    clusterExport,
    detectCores,
    makePSOCKcluster,
    stopCluster
  ],
  plotly[config, event_register, layout],
  reticulate[py_config, py_last_error, py_run_string],
  RSQLite[SQLite, SQLITE_RO],
  scales[percent_format],
  utils[read.delim, read.table],
)

# db_with_retry(): BEGIN IMMEDIATE + body + COMMIT with R-level retry ----
# Retries the full transaction cycle on any lock/busy error, with random jitter.
# The body is captured unevaluated and re-evaluated on every attempt: forcing a
# promise runs it only once, so a COMMIT that lost the race would otherwise be
# retried around an empty transaction and the writes would vanish silently.
#' @export
db_with_retry <- function(con, expr, max_wait_s = 300) {
  body <- substitute(expr)
  body_env <- parent.frame()
  deadline <- proc.time()[["elapsed"]] + max_wait_s
  repeat {
    ok <- tryCatch(
      {
        DBI::dbExecute(con, "BEGIN IMMEDIATE")
        tryCatch(
          {
            eval(body, body_env)
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
          # stats:: qualified: box modules do not attach stats, so a bare runif()
          # would turn the very lock contention this loop exists for into an
          # immediate "could not find function" error.
          Sys.sleep(stats::runif(1, 0.3, 1.2))
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
# Deconvolutes one sample and writes its rows plus a status record into the
# shared DB.  Never signals: every outcome is recorded as "done" or "failed".
#
# max_attempts: UniDec runs as an external .exe and occasionally dies with an
# access violation when many copies run at once.  Those crashes are transient --
# the same sample succeeds on a second run -- so one retry is worth far more
# than a spurious failed sample in the results.
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
  keep_raw_output = FALSE,
  max_attempts = 2L
) {
  input_path <- gsub("\\\\", "/", waters_dir)
  result_dir <- gsub("\\\\", "/", result_dir)

  # Derive sample base name up front so it is available in every error path
  sample_basename <- gsub(
    "\\.raw$",
    "",
    basename(input_path),
    ignore.case = TRUE
  )

  # When discarding raw output, route UniDec intermediates to a per-sample
  # temp dir so the target directory stays clean throughout the run.
  # The temp dir is deleted after the DB write regardless of success/failure.
  #
  # raw_stub names the scratch files UniDec writes.  When output is kept it
  # must be sample_basename, because deconvolution_main.R's result picker
  # matches result folders back to samples by that name.  When output is
  # discarded it is a short synthetic id instead: UniDec derives every
  # intermediate filename from it, nested three levels deep under a temp dir
  # (<tmp>/<stub>/<stub>_rawdata_unidecfiles/<stub>_rawdata_conf.dat, etc.),
  # and a long sample name easily pushes that past Windows' legacy 260-
  # character MAX_PATH. UniDec enforces that limit itself and, having no
  # output to report on, fails silently -- no R or Python exception, just a
  # generic "no output produced" a few lines down. A short id sidesteps the
  # limit regardless of how the operator names their samples.
  if (!isTRUE(keep_raw_output)) {
    raw_stub <- paste0("s", Sys.getpid())
    work_dir <- file.path(tempdir(), raw_stub)
    dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  } else {
    raw_stub <- sample_basename
    work_dir <- result_dir
  }

  raw_name <- paste0(raw_stub, "_rawdata")
  result <- file.path(work_dir, paste0(raw_name, "_unidecfiles"))

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

  # attempt(): One full UniDec run plus DB write for this sample ----
  # Returns list(ok, reason, detail); it writes the "done" status itself but
  # leaves failure reporting to the caller so a retry is not recorded as a
  # failure.
  attempt <- function() {
    tryCatch(
      {
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
out_stub = r"%s"

# Initialize UniDec engine
engine = unidec.UniDec()

# Convert Waters .raw to txt
engine.raw_process(input_file)

# Move processed file to output directory under the out_stub name (not the
# source file own name -- see the R-side comment on raw_stub for why).
txt_file = input_file.removesuffix(".raw") + "_rawdata.txt"
output = os.path.join(result_dir, out_stub + "_rawdata.txt")
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
          work_dir,
          raw_stub
        ))

        # Write per-sample data to DB, or report failure if output is
        # missing/incomplete
        mass_file <- file.path(result, paste0(raw_name, "_mass.txt"))
        peaks_file <- file.path(result, paste0(raw_name, "_peaks.dat"))

        if (
          !dir.exists(result) ||
            !file.exists(mass_file) ||
            !file.exists(peaks_file)
        ) {
          # With output kept (raw_stub = sample_basename), a long sample name
          # can still push this path over Windows' legacy 260-character
          # MAX_PATH -- the scratch case above sidesteps it with a short id,
          # but a kept result still has to live under a sample-named file.
          # Flag that specific, actionable cause when it applies.
          longest <- max(nchar(mass_file), nchar(peaks_file))
          if (longest > 259) {
            return(list(
              ok = FALSE,
              reason = "path_too_long",
              detail = sprintf(
                "Working path is %d characters, over Windows' 260-character limit: %s",
                longest,
                mass_file
              )
            ))
          }
          return(list(
            ok = FALSE,
            reason = "no_output_dir",
            detail = NA_character_
          ))
        }

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
        peaks_df <- read_file_safe(peaks_file, c("mass", "intensity"))
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
        mass_df <- read_file_safe(mass_file, c("mass", "intensity"))

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

        list(ok = TRUE, reason = NA_character_, detail = NA_character_)
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

        list(ok = FALSE, reason = "error", detail = err_detail)
      }
    )
  }

  outcome <- list(ok = FALSE, reason = "error", detail = NA_character_)
  for (i in seq_len(max(1L, as.integer(max_attempts)))) {
    if (i > 1L) {
      message(
        "Retrying ",
        sample_basename,
        " (attempt ",
        i,
        " of ",
        max_attempts,
        ") after: ",
        outcome$reason
      )
      # Drop only this sample's leftovers.  With keep_raw_output = TRUE the
      # work directory is the shared result directory, so a blanket wipe would
      # destroy other samples' output.
      unlink(result, recursive = TRUE)
      unlink(file.path(work_dir, paste0(raw_name, ".txt")))
    }
    outcome <- attempt()
    if (isTRUE(outcome$ok)) {
      return(invisible(TRUE))
    }
  }

  write_sample_status(
    db_path,
    sample_basename,
    "failed",
    outcome$reason,
    outcome$detail
  )
  invisible(FALSE)
}

# decon_worker_count(): Worker count for a run ----
# Never spawn more workers than there are samples, always leave two logical
# cores for the Shiny app and the OS, and allow an override for testing/support.
#' @export
decon_worker_count <- function(n_samples, num_cores = NULL) {
  override <- suppressWarnings(as.integer(Sys.getenv("KIWIMS_DECON_WORKERS")))
  if (!is.na(override) && override > 0) {
    num_cores <- override
  } else if (is.null(num_cores)) {
    num_cores <- parallel::detectCores() - 2
  }
  if (is.na(num_cores)) {
    num_cores <- 1L
  }
  max(1L, min(as.integer(num_cores), as.integer(n_samples)))
}

# decon_worker_init(): One-shot bring-up of a parallel worker ----
# Runs INSIDE a worker.  Returns "ok" or an "error: ..." string and never
# signals a condition, so a single bad worker cannot abort the whole run.
#' @export
decon_worker_init <- function(python_exe, lib_paths) {
  tryCatch(
    {
      .libPaths(lib_paths)

      # Give every worker its own Windows TEMP before Python is touched.
      # Conda DLL activation hooks call GetTempFileName() during
      # PyInitialize(); on a shared TEMP, simultaneous initialisation across
      # workers can collide (seen as "Error 127").  tempdir() is already unique
      # per R process, so a subdirectory of it is collision-free by design and
      # is cleaned up automatically when the worker exits.
      py_tmp <- file.path(tempdir(), "pytmp")
      dir.create(py_tmp, showWarnings = FALSE, recursive = TRUE)
      Sys.setenv(TMP = py_tmp, TEMP = py_tmp, TMPDIR = py_tmp)

      # Bind reticulate through RETICULATE_PYTHON instead of use_python().
      # use_python(required = TRUE) probes the interpreter in a subprocess and
      # reticulate probes it again on first use; the env var skips that
      # duplicate probe (~4 s per worker).  The binding is verified below.
      Sys.setenv(RETICULATE_PYTHON = python_exe, PYTHONNOUSERSITE = "1")
      Sys.unsetenv("PYTHONHOME")

      suppressMessages({
        library(reticulate)
        library(DBI)
        library(RSQLite)
        library(data.table)
      })

      # Forces PyInitialize() and warms the UniDec import so the first sample
      # this worker handles pays no interpreter or import cost.
      reticulate::py_run_string("import unidec")

      canon <- function(p) {
        tolower(normalizePath(p, winslash = "/", mustWork = FALSE))
      }
      bound <- tryCatch(
        canon(reticulate::py_config()$python),
        error = function(e) ""
      )
      if (nzchar(bound) && !identical(bound, canon(python_exe))) {
        message(
          "Worker bound to Python '",
          bound,
          "' rather than the requested '",
          python_exe,
          "'."
        )
      }

      "ok"
    },
    error = function(e) paste0("error: ", conditionMessage(e))
  )
}

# decon_start_cluster(): Build and initialise the worker pool ----
# Returns list(cl = <full cluster>, healthy = <indices ready for work>).
# The caller must stop `cl` (the full pool) even when only a subset is used.
#
# Bring-up is concurrent by default.  It falls back to one-worker-at-a-time
# initialisation if the concurrent pass fails outright, retries individual
# workers that reported an error, and finally reports which workers are usable
# so the run can continue on a reduced pool instead of failing.
#' @export
decon_start_cluster <- function(n_workers, python_exe, lib_paths, outfile) {
  make_pool <- function() {
    # --vanilla: workers skip the project .Rprofile (renv activation) and the
    # site profile.  Both are pure overhead here -- .libPaths() is set
    # explicitly during init -- and renv activation alone costs several seconds
    # across a full pool.
    # useXDR = FALSE: no byte-order conversion needed for a local pool.
    parallel::makePSOCKcluster(
      n_workers,
      outfile = outfile,
      rscript_args = "--vanilla",
      useXDR = FALSE
    )
  }

  init_all <- function(cl) {
    tryCatch(
      parallel::clusterCall(cl, decon_worker_init, python_exe, lib_paths),
      error = function(e) e
    )
  }

  init_one <- function(cl, i) {
    tryCatch(
      parallel::clusterCall(cl[i], decon_worker_init, python_exe, lib_paths)[[1]],
      error = function(e) paste0("error: ", conditionMessage(e))
    )
  }

  cl <- make_pool()
  report <- init_all(cl)

  if (inherits(report, "error")) {
    # A worker connection dropped mid-bring-up.  Rebuild the pool and
    # initialise sequentially, which is immune to whatever raced.
    message(
      "Concurrent worker start-up failed (",
      conditionMessage(report),
      "); retrying one worker at a time ..."
    )
    try(parallel::stopCluster(cl), silent = TRUE)
    cl <- make_pool()
    report <- lapply(seq_along(cl), function(i) init_one(cl, i))
  }

  status <- vapply(
    report,
    function(x) {
      if (is.character(x) && length(x) == 1L) x else "error: bad init result"
    },
    character(1)
  )

  # Give any worker that reported an error one serialised second chance.
  for (i in which(status != "ok")) {
    status[i] <- init_one(cl, i)
  }

  healthy <- which(status == "ok")
  if (length(healthy) < length(cl)) {
    for (i in setdiff(seq_along(cl), healthy)) {
      message("Worker ", i, " unavailable: ", status[i])
    }
  }

  list(cl = cl, healthy = healthy, status = status)
}

# decon_mark_unprocessed(): Record samples the run never reached ----
# A worker that dies outright leaves its sample without a status row.  Marking
# those explicitly keeps the DB a complete record of the run, so the UI reports
# a definite outcome for every requested sample instead of stalling.
#' @export
decon_mark_unprocessed <- function(
  db_path,
  sample_names,
  reason = "not_processed"
) {
  tryCatch(
    {
      if (length(sample_names) == 0) {
        return(invisible(0L))
      }
      con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbExecute(con, "PRAGMA busy_timeout=300000")
      if (!DBI::dbExistsTable(con, "status")) {
        return(invisible(0L))
      }
      known <- DBI::dbGetQuery(con, "SELECT sample FROM status")$sample
      missing <- setdiff(sample_names, known)
      if (length(missing) == 0) {
        return(invisible(0L))
      }
      db_with_retry(con, {
        for (s in missing) {
          DBI::dbExecute(
            con,
            "INSERT OR REPLACE INTO status(sample,state,reason,error_msg,timestamp)
             VALUES (?,?,?,?,?)",
            params = list(
              s,
              "failed",
              reason,
              NA_character_,
              format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            )
          )
        }
      })
      invisible(length(missing))
    },
    error = function(e) {
      message("Could not mark unprocessed samples: ", e$message)
      invisible(0L)
    }
  )
}

# decon_samples_with_state(): Which of `sample_bases` are in a given state ----
# Internal helper shared by deconvolute()'s pool-recovery paths, so "which of
# *these* samples ended up done/failed" is read the same way everywhere.
#' @export
decon_samples_with_state <- function(db_path, sample_bases, state) {
  tryCatch(
    {
      if (length(sample_bases) == 0) {
        return(character(0))
      }
      con <- DBI::dbConnect(
        RSQLite::SQLite(),
        db_path,
        flags = RSQLite::SQLITE_RO
      )
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      if (!DBI::dbExistsTable(con, "status")) {
        return(character(0))
      }
      ph <- paste(rep("?", length(sample_bases)), collapse = ",")
      DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT sample FROM status WHERE state = ? AND sample IN (%s)",
          ph
        ),
        params = c(list(state), as.list(sample_bases))
      )$sample
    },
    error = function(e) character(0)
  )
}

# decon_python_exe(): Resolve and validate the portable interpreter ----
decon_python_exe <- function() {
  python_exe <- Sys.getenv("RETICULATE_PYTHON")
  if (!nzchar(python_exe) || !file.exists(python_exe)) {
    stop(
      "Python interpreter not found. RETICULATE_PYTHON is not set or points to a missing file."
    )
  }
  message("Python found: ", python_exe)
  python_exe
}

# deconvolute(): Deconvolution ----
# Runs every sample through UniDec, writing results into the shared SQLite DB.
#
# num_cores    NULL uses the machine size (see decon_worker_count()).
# min_parallel Sample count from which the worker pool is worth starting.  One
#              sample costs tens of seconds of UniDec time while the pool costs
#              a few seconds to bring up, so two samples already pay for it.
#' @export
deconvolute <- function(
  raw_dirs,
  result_dir,
  db_path,
  keep_raw_output = FALSE,
  num_cores = NULL,
  min_parallel = 2,
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
  python_exe <- decon_python_exe()

  sample_bases <- gsub("\\.raw$", "", basename(raw_dirs), ignore.case = TRUE)

  # Parameters shared by every sample, identical in both processing modes.
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

  run_sequential <- function(dirs) {
    if (length(dirs) == 0) {
      return(invisible(NULL))
    }
    message("Sequential processing started ...")
    # Same interpreter hygiene the workers apply: reticulate binds through
    # RETICULATE_PYTHON, and a stale PYTHONHOME breaks a conda-style env with
    # "No module named 'encodings'".
    Sys.setenv(RETICULATE_PYTHON = python_exe, PYTHONNOUSERSITE = "1")
    Sys.unsetenv("PYTHONHOME")
    for (dir in dirs) {
      tryCatch(
        do.call(process_single_dir, c(list(waters_dir = dir), params_list)),
        error = function(e) {
          message("Error processing ", dir, ": ", e$message)
        }
      )
    }
    message("Sequential processing finalized.")
  }

  n_workers <- decon_worker_count(length(raw_dirs), num_cores)
  use_parallel <- length(raw_dirs) >= min_parallel && n_workers > 1

  if (!use_parallel) {
    run_sequential(raw_dirs)
    decon_mark_unprocessed(db_path, sample_bases)
    return(invisible(NULL))
  }

  message("Initiating ", n_workers, " cores for parallel processing ...")

  outfile <- file.path(
    Sys.getenv("LOCALAPPDATA"),
    "KiwiMS",
    "last_cluster_log.txt"
  )
  outfile <- tryCatch(
    {
      dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
      writeLines(paste("Deconvolution Cluster Output", Sys.time()), outfile)
      outfile
    },
    error = function(e) {
      message("Could not prepare cluster log: ", e$message)
      ""
    }
  )

  t_start <- proc.time()[["elapsed"]]
  message("Starting worker pool ...")
  pool <- decon_start_cluster(n_workers, python_exe, .libPaths(), outfile)
  cl <- pool$cl
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

  if (length(pool$healthy) == 0) {
    message("No worker could be initialised; falling back to sequential run.")
    try(parallel::stopCluster(cl), silent = TRUE)
    run_sequential(raw_dirs)
    decon_mark_unprocessed(db_path, sample_bases)
    return(invisible(NULL))
  }

  cl_use <- cl[pool$healthy]
  message(sprintf(
    "%d of %d worker(s) ready after %.1f s.",
    length(pool$healthy),
    n_workers,
    proc.time()[["elapsed"]] - t_start
  ))

  parallel::clusterExport(
    cl_use,
    c(
      "process_single_dir",
      "write_sample_status",
      "db_with_retry",
      "read_file_safe",
      "%||%",
      "params_list"
    ),
    envir = environment()
  )

  # clusterApplyLB hands out one sample at a time, so a slow sample cannot stall
  # a whole pre-assigned chunk the way parLapply's static split does.
  # capture.output suppresses worker stdout; par_results holds the return values
  # (NULL when the worker-side tryCatch caught an error).
  message("Running parallel deconvolution ...")
  par_results <- NULL
  dispatch_error <- NULL
  invisible(capture.output(
    {
      dispatch_error <- tryCatch(
        {
          par_results <- parallel::clusterApplyLB(cl_use, raw_dirs, function(d) {
            tryCatch(
              {
                do.call(
                  process_single_dir,
                  c(list(waters_dir = d), params_list)
                )
                TRUE
              },
              error = function(e) {
                message("Error processing ", d, ": ", e$message)
                NULL
              }
            )
          })
          NULL
        },
        error = function(e) e
      )
    },
    type = "output"
  ))

  if (!is.null(dispatch_error)) {
    # The pool broke down mid-run.  Every sample already marked "done" is
    # kept; everything else -- including samples a worker had already given
    # up on -- is retried sequentially so the run still completes instead of
    # aborting with a half-filled database.
    message(
      "Parallel dispatch failed (",
      conditionMessage(dispatch_error),
      "); completing the remaining samples sequentially ..."
    )
    try(parallel::stopCluster(cl), silent = TRUE)
    done <- decon_samples_with_state(db_path, sample_bases, "done")
    run_sequential(raw_dirs[!sample_bases %in% done])
  } else {
    message("Parallel processing finalized.")
    failed_idx <- vapply(par_results, is.null, logical(1))
    if (any(failed_idx)) {
      warning(
        "Errors occurred for ",
        sum(failed_idx),
        " sample(s): ",
        paste(basename(raw_dirs[failed_idx]), collapse = ", ")
      )
    }

    # A sample can still be marked failed here purely from contention: UniDec
    # runs as an external .exe, and process_single_dir's own in-worker retry
    # can land while every other worker is still saturating the machine, so a
    # second attempt fails for the same reason as the first.  The pool is idle
    # the moment this run finishes, so give those samples one more attempt
    # with the machine to themselves before accepting the result.
    still_failed <- decon_samples_with_state(db_path, sample_bases, "failed")
    if (length(still_failed) > 0) {
      message(
        length(still_failed),
        " sample(s) still failed after the parallel pass; ",
        "retrying sequentially now that the pool is free ..."
      )
      try(parallel::stopCluster(cl), silent = TRUE)
      run_sequential(raw_dirs[sample_bases %in% still_failed])
    }
  }

  decon_mark_unprocessed(db_path, sample_bases)
  invisible(NULL)
}

# plate_heatmap(): Well plate occupancy heatmap ----
# failed_wells: data.frame(sample, well_id) for samples that failed to
# deconvolute (well_id in the same format as `data$well_id` / `all_wells`,
# e.g. "A1"). Rendered in a distinct colour with the sample name on hover, so
# a failure is visible -- and identifiable -- across the whole plate at a
# glance, rather than being indistinguishable from a well nothing was ever
# run for.
#' @export
plate_heatmap <- function(
  data,
  all_wells = NULL,
  failed_wells = NULL,
  theme = "dark"
) {
  font_color <- if (theme == "light") "black" else "white"
  empty_color <- if (theme == "light") {
    "rgba(195,197,205,1)"
  } else {
    "rgba(42,44,52,1)"
  }
  tile_bg <- if (theme == "light") {
    "rgba(148,150,158,0.35)"
  } else {
    "rgba(118,120,128,0.55)"
  }
  # Matches the app's existing error accent (.table-info-red / .table-hint-red
  # in main.scss) so a failed well reads the same as other failure cues.
  failed_color <- "#ff5a23"

  # A caller with nothing done yet (e.g. every sample so far has failed) may
  # pass a bare, columnless data.frame() rather than one shaped like a real
  # result set -- normalise so the join below always has a well_id column to
  # join on instead of erroring.
  if (is.null(data) || !is.data.frame(data) || !"well_id" %in% names(data)) {
    data <- data.frame(
      sample = character(0),
      well_id = character(0),
      value = numeric(0)
    )
  }

  all_rows <- LETTERS[1:16]
  all_cols <- 1:24

  # Normalize well IDs ("A01" → "A1")
  norm_well_id <- function(w) {
    gsub("^([A-Za-z]+)0*(\\d+)$", "\\1\\2", toupper(trimws(w)))
  }

  # Determine bounding rectangle from all_wells if provided, else from data
  if (!is.null(all_wells) && length(all_wells) > 0) {
    nw <- norm_well_id(stats::na.omit(as.character(all_wells)))
    nw <- nw[nzchar(nw)]
    row_letters <- sub("\\d+$", "", nw)
    col_nums <- as.integer(sub("^[A-Za-z]+", "", nw))
    used_row_idx <- range(
      match(row_letters, all_rows, nomatch = NA_integer_),
      na.rm = TRUE
    )
    used_col_idx <- range(col_nums, na.rm = TRUE)
    row_range <- seq(used_row_idx[1], used_row_idx[2])
    col_range <- seq(used_col_idx[1], used_col_idx[2])
  } else {
    row_range <- seq_len(16)
    col_range <- seq_len(24)
  }

  rows <- all_rows[row_range]
  cols <- all_cols[col_range]
  nr <- length(rows)
  nc <- length(cols)

  plate_layout <- expand.grid(row = rows, col = cols) |>
    dplyr::mutate(well_id = paste0(row, col))

  plate_data <- dplyr::left_join(plate_layout, data, by = "well_id")

  # failed_lookup: normalised well_id -> sample, for the failed branch below.
  if (
    is.null(failed_wells) ||
      !is.data.frame(failed_wells) ||
      nrow(failed_wells) == 0 ||
      !all(c("sample", "well_id") %in% names(failed_wells))
  ) {
    failed_lookup <- stats::setNames(character(0), character(0))
  } else {
    fw_id <- norm_well_id(as.character(failed_wells$well_id))
    keep <- nzchar(fw_id) & !is.na(fw_id)
    failed_lookup <- stats::setNames(
      as.character(failed_wells$sample[keep]),
      fw_id[keep]
    )
    # A well listed more than once keeps its first sample; duplicates are not
    # expected, but must not error.
    failed_lookup <- failed_lookup[!duplicated(names(failed_lookup))]
  }

  # z: 0 = empty/untargeted, 1 = done (drawn white), 2 = failed (drawn in
  # failed_color).  A well present in both `data` and `failed_wells` -- not
  # expected, but not impossible if a well maps to more than one sample --
  # shows as done, since that reflects a completed result existing for it.
  z_mat <- matrix(
    0,
    nrow = nr,
    ncol = nc,
    dimnames = list(rows, as.character(cols))
  )
  text_mat <- matrix(
    "",
    nrow = nr,
    ncol = nc,
    dimnames = list(rows, as.character(cols))
  )

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
      } else if (wid %in% names(failed_lookup)) {
        z_mat[r, as.character(c)] <- 2
        text_mat[r, as.character(c)] <- paste0(
          "Well: ",
          wid,
          "<br>Sample: ",
          failed_lookup[[wid]],
          "<br>Failed"
        )
      } else {
        text_mat[r, as.character(c)] <- paste0("Well: ", wid, "<br>Empty")
      }
    }
  }

  colorscale <- list(
    c(0, empty_color),
    c(0.5, "white"),
    c(1, failed_color)
  )

  plotly::plot_ly(
    z = z_mat,
    x = cols,
    y = rows,
    type = "heatmap",
    colorscale = colorscale,
    showscale = FALSE,
    zmin = 0,
    zmax = 2,
    xgap = 3,
    ygap = 3,
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
        tickfont = list(color = font_color, size = 12),
        tickangle = 0,
        ticklen = 0,
        showgrid = FALSE,
        zeroline = FALSE,
        automargin = FALSE,
        range = c(min(cols) - 0.5, max(cols) + 0.5),
        scaleanchor = "y",
        scaleratio = 1
      ),
      yaxis = list(
        autorange = "reversed",
        tickfont = list(color = font_color, size = 12),
        ticklen = 0,
        showgrid = FALSE,
        zeroline = FALSE,
        scaleanchor = "x",
        scaleratio = 1,
        automargin = FALSE
      ),
      margin = list(t = 25, r = 0, b = 0, l = 30),
      plot_bgcolor = tile_bg,
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
      mass$intensity <- mass$intensity / max(mass$intensity) * 100

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
    mass$intensity <- mass$intensity / max(mass$intensity) * 100

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
        Compound = Compound[Preferred == "TRUE"][1],
        `Compound Mw [Da]` = `Compound Mw [Da]`[Preferred == "TRUE"][1],
        `Binding Stoichiometry` = `Binding Stoichiometry`[Preferred == "TRUE"][
          1
        ],
        mass_stoich_label = paste(
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
      showlegend = FALSE,
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
      tickcolor = "transparent",
      range = list(0, 105)
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
    if (!all(is.na(plot_data$highlight_peaks$mass))) {
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

        peaks_data <- plot_data$highlight_peaks[
          !is.na(plot_data$highlight_peaks$intensity),
        ]
        for (peak_name in unique(peaks_data$name)) {
          nd <- peaks_data[peaks_data$name == peak_name, ]
          plot <- plotly::add_markers(
            plot,
            data = nd,
            x = ~mass,
            y = ~intensity,
            name = peak_name,
            marker = list(
              color = if (!is.null(color_cmp)) {
                ~ I(color)
              } else {
                marker_fill_color
              },
              line = list(color = ~ I(linecolor), width = 1),
              symbol = ~ I(symbol),
              size = 12,
              zindex = 100
            ),
            hoverinfo = "text",
            text = ~ paste0(
              "Name: ",
              name,
              "\nMeasured: ",
              mass,
              " Da",
              "\nIntensity: ",
              round(intensity, 2),
              "%\n",
              ifelse(
                is.na(mass_stoich_label),
                paste0("Theor. Mw: ", mw),
                paste0("Mass Shifts: ", mass_stoich_label)
              )
            ),
            showlegend = TRUE
          )
        }
      }

      # Annotation logic for mass difference + per-peak labels
      shapes <- NULL
      annotations <- NULL
      unique_masses <- sort(unique(plot_data$highlight_peaks$mass))

      # The mass-difference label is anchored just above its connector, which
      # sits at the very top of the y range - and the layout below asks for a
      # top margin of 0, which leaves no paper above the plot area for the
      # label to be drawn on, so it is cut off at the edge of the figure.
      # Reserve a strip for it whenever that label is going to be drawn.
      top_margin <- if (show_mass_diff && length(unique_masses) >= 2) 28 else 0

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

          # Text annotation above the horizontal line.
          #
          # Anchored by its bottom edge to the connector rather than centred on
          # a fixed data-space offset above it. The old +1.5 offset was a
          # constant in y units while the text height is set by the font, so a
          # larger label grew down past the offset and sat on the very line it
          # labels. yshift is a pixel gap, which keeps clear of the line at any
          # font size and scales with the label size on export.
          annotations[[length(annotations) + 1]] <- list(
            x = mid_x,
            y = y_line,
            text = diff_text,
            showarrow = FALSE,
            yanchor = "bottom",
            yshift = 3,
            font = list(color = font_color, size = 12)
          )
        }
      }

      # Add short diagonal leader + text label for each peak (if enabled)
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

      yaxis_list$range <- c(0, max(max_y_needed, 105))
    } else {
      top_margin <- 0
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
      showlegend = !is.null(sample),
      legend = list(
        bgcolor = "rgba(0,0,0,0)",
        font = list(color = font_color, size = 12),
        itemclick = FALSE,
        itemdoubleclick = FALSE
      ),
      margin = list(t = top_margin, r = 0, b = 0, l = 50)
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

# decon_failure_detail(): Human-readable reason a sample failed ----
# Turns the status table's `reason` code and raw `error_msg` (which can be a
# multi-line R + Python traceback) into a short cause line plus an optional
# detail block, so the UI can show something more useful than a generic
# "failed" message.  Returns NULL when the sample is not on record as failed.
#' @export
decon_failure_detail <- function(db_path, sample) {
  tryCatch(
    {
      con <- DBI::dbConnect(
        RSQLite::SQLite(),
        db_path,
        flags = RSQLite::SQLITE_RO
      )
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      if (!DBI::dbExistsTable(con, "status")) {
        return(NULL)
      }
      row <- DBI::dbGetQuery(
        con,
        "SELECT reason, error_msg FROM status WHERE sample = ? AND state = 'failed'",
        params = list(sample)
      )
      if (nrow(row) == 0) {
        return(NULL)
      }

      reason <- row$reason[1]
      reason <- if (is.na(reason)) "" else reason
      detail <- row$error_msg[1]
      detail <- if (is.na(detail)) "" else trimws(detail)
      # Python tracebacks can run long; cap what the UI has to lay out. The
      # full text is always still available via the downloadable session log.
      if (nchar(detail) > 4000) {
        detail <- paste0(substr(detail, 1, 4000), "\n... (truncated)")
      }

      cause <- switch(
        reason,
        no_output_dir = paste(
          "UniDec produced no output for this sample.",
          "The .raw file may be empty, corrupted, or in an unsupported format."
        ),
        path_too_long = paste(
          "The output path for this sample exceeded Windows' 260-character",
          "path limit. Shorten the sample's file name, choose a shorter",
          "destination folder, or move the destination closer to the drive",
          "root, then rerun this sample."
        ),
        not_processed = paste(
          "This sample was never picked up by a worker",
          "(the worker pool broke down before reaching it)."
        ),
        error = "UniDec raised an error while processing this sample.",
        if (nzchar(reason)) reason else "Deconvolution failed for this sample."
      )

      list(
        cause = cause,
        detail = if (nzchar(detail)) detail else NULL
      )
    },
    error = function(e) NULL
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
  if (!file.exists(db_path)) {
    return(invisible(NULL))
  }
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
  if (file.exists(wal) && file.size(wal) == 0) {
    tryCatch(file.remove(wal), error = function(e) NULL)
  }
  if (file.exists(shm)) {
    tryCatch(file.remove(shm), error = function(e) NULL)
  }
  invisible(NULL)
}
