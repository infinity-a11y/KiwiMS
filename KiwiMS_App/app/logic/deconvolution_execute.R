# app/logic/deconvolution_execute.R

# Checking library Paths
message(paste("Current library paths: \n", paste(.libPaths(), collapse = "\n")))

# In dev mode manually add library paths
# if (commandArgs(trailingOnly = TRUE)[5] == "TRUE") {
#   .libPaths(c(
#     normalizePath(file.path(
#       Sys.getenv("LOCALAPPDATA"),
#       "R",
#       "win-library",
#       "4.5"
#     )),
#     .libPaths()
#   ))

#   message(paste(
#     "Modified library paths: \n",
#     paste(.libPaths(), collapse = "\n")
#   ))
# }

# Sourcing deconvolution functions
source_file <- file.path(
  commandArgs(trailingOnly = TRUE)[3],
  "app/logic/deconvolution_functions.R"
)
message(paste("Sourcing", source_file))
tryCatch(
  {
    source(source_file)
  },
  error = function(e) {
    message("Error sourcing deconvolution functions: ", e$message)
    stop("Deconvolution failed.")
  }
)

# Setting deconvolution parameter
message("Setting deconvolution parameter ...")
tryCatch(
  {
    temp <- commandArgs(trailingOnly = TRUE)[1]
    conf <- readRDS(file.path(temp, "config.rds"))
    logfile <- commandArgs(trailingOnly = TRUE)[2]
    result_dir <- commandArgs(trailingOnly = TRUE)[4]
    db_path <- commandArgs(trailingOnly = TRUE)[6]
    keep_raw_output <- isTRUE(as.logical(commandArgs(trailingOnly = TRUE)[7]))
    output_path <- file.path(
      Sys.getenv("LOCALAPPDATA"),
      "KiwiMS",
      "deconvolution.log"
    )
  },
  error = function(e) {
    message("Error setting deconvolution parameter: ", e$message)
    stop("Error setting deconvolution parameter.")
  }
)

# Load DB packages (sourced module uses box::use so these aren't globally attached)
library(DBI)
library(RSQLite)

# Initialise SQLite DB (WAL mode; write run_info, metadata, status tables upfront)
# When extending an existing DB, existing records for OTHER samples are preserved.
# Only the samples being processed in THIS run are reset so progress tracking
# starts clean for them (handles both skip-others and overwrite cases).
message("Initialising SQLite database ...")
tryCatch(
  {
    sample_bases <- gsub("\\.raw$", "", basename(conf$dirs), ignore.case = TRUE)

    con_init <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    DBI::dbExecute(con_init, "PRAGMA journal_mode=WAL")
    DBI::dbExecute(con_init, "PRAGMA busy_timeout=5000")
    # Checkpoint + truncate any stale WAL left by a prior aborted run.
    # Safe no-op when no WAL exists or when readers hold a shared lock.
    DBI::dbExecute(con_init, "PRAGMA wal_checkpoint(TRUNCATE)")

    # One transaction for the whole reset: an interrupted init can then only
    # leave the DB fully reset or fully untouched, never half-way.
    #
    # con_init is closed explicitly at the end of this block and in the error
    # handler: on.exit() is a no-op at script top level, and leaving the
    # connection open kept the database locked for the rest of the run, which
    # made the closing WAL checkpoint fail.
    DBI::dbExecute(con_init, "BEGIN IMMEDIATE")

    # run_info: always overwrite (records this run's start time)
    DBI::dbWriteTable(
      con_init,
      "run_info",
      data.frame(
        started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        n_samples = length(conf$dirs),
        stringsAsFactors = FALSE
      ),
      overwrite = TRUE
    )

    # metadata: ensure table exists, then upsert only the samples being
    # processed (delete + insert).  Samples from previous runs that are NOT
    # being processed in this run are left untouched (extend case).
    # Using delete+insert rather than INSERT OR IGNORE avoids the need for a
    # UNIQUE constraint, which dbWriteTable does not create.
    if (!DBI::dbExistsTable(con_init, "metadata")) {
      DBI::dbExecute(con_init, "CREATE TABLE metadata (sample TEXT)")
    }
    for (s in sample_bases) {
      DBI::dbExecute(
        con_init,
        "DELETE FROM metadata WHERE sample = ?",
        params = list(s)
      )
      DBI::dbExecute(
        con_init,
        "INSERT INTO metadata(sample) VALUES (?)",
        params = list(s)
      )
    }

    # status: create if new; otherwise delete only the samples being processed
    # so prior done/failed records for skipped samples are untouched.
    if (!DBI::dbExistsTable(con_init, "status")) {
      DBI::dbWriteTable(
        con_init,
        "status",
        data.frame(
          sample = character(0),
          state = character(0),
          reason = character(0),
          error_msg = character(0),
          timestamp = character(0),
          stringsAsFactors = FALSE
        )
      )
    } else {
      for (s in sample_bases) {
        DBI::dbExecute(
          con_init,
          "DELETE FROM status WHERE sample = ?",
          params = list(s)
        )
      }
    }

    # A UNIQUE index on status(sample) is what makes the INSERT OR REPLACE
    # writes further down the pipeline behave as upserts; dbWriteTable creates
    # no constraints, so without it a second write for the same sample would
    # add a row and inflate the progress count.  Databases written by earlier
    # versions may already hold duplicates, so collapse them to the newest row
    # first.  Non-fatal: a run must not be blocked by an odd legacy database.
    tryCatch(
      {
        DBI::dbExecute(
          con_init,
          "DELETE FROM status WHERE rowid NOT IN
             (SELECT MAX(rowid) FROM status GROUP BY sample)"
        )
        DBI::dbExecute(
          con_init,
          "CREATE UNIQUE INDEX IF NOT EXISTS idx_status_sample
             ON status(sample)"
        )
      },
      error = function(e) {
        message("Note: could not enforce unique status rows: ", e$message)
      }
    )

    # For samples being (re)processed: clear any existing per-sample data rows
    # so a clean overwrite is written (avoids duplicate rows in peaks/mass_data/etc.)
    per_sample_tbls <- c(
      "peaks",
      "mass_data",
      "error",
      "config",
      "rawdata",
      "input_dat"
    )
    for (tbl in per_sample_tbls) {
      if (DBI::dbExistsTable(con_init, tbl)) {
        for (s in sample_bases) {
          DBI::dbExecute(
            con_init,
            sprintf("DELETE FROM %s WHERE sample = ?", tbl),
            params = list(s)
          )
        }
      }
    }

    # Drop stale completed sentinel so the Shiny observer does not fire
    # immediately when a run is added to an already-finished DB.
    if (DBI::dbExistsTable(con_init, "completed")) {
      DBI::dbExecute(con_init, "DROP TABLE completed")
    }

    DBI::dbExecute(con_init, "COMMIT")
    DBI::dbDisconnect(con_init)
    invisible(NULL)
  },
  error = function(e) {
    # Release the connection so the rest of the run (and the closing WAL
    # checkpoint) is not blocked by a half-finished initialisation.
    if (exists("con_init") && DBI::dbIsValid(con_init)) {
      tryCatch(DBI::dbExecute(con_init, "ROLLBACK"), error = function(e2) NULL)
      tryCatch(DBI::dbDisconnect(con_init), error = function(e2) NULL)
    }
    message("Error initialising SQLite database: ", e$message)
    stop("DB initialisation failed.")
  }
)

# Start deconvolution
tryCatch(
  {
    deconvolute(
      raw_dirs = conf$dirs,
      result_dir = result_dir,
      db_path = db_path,
      keep_raw_output = keep_raw_output,
      startz = conf$params$startz,
      endz = conf$params$endz,
      minmz = conf$params$minmz,
      maxmz = conf$params$maxmz,
      masslb = conf$params$masslb,
      massub = conf$params$massub,
      massbins = conf$params$massbins,
      peakthresh = conf$params$peakthresh,
      peakwindow = conf$params$peakwindow,
      peaknorm = conf$params$peaknorm,
      time_start = conf$params$time_start,
      time_end = conf$params$time_end
    )
  },
  error = function(e) {
    py_err <- reticulate::py_last_error()

    # Print the main error and the Python stack trace if it exists
    message("Error in deconvolution processing: ", e$message)
    if (!is.null(py_err)) {
      message(py_err)
    }

    stop("Deconvolution failed.")
  }
)

# If test run dont write result file
if (commandArgs(trailingOnly = TRUE)[5] != "testing") {
  # Read log and output
  tryCatch(
    {
      log <- if (file.exists(logfile)) {
        readLines(logfile, warn = FALSE)
      } else {
        "No log"
      }

      output <- if (file.exists(output_path)) {
        readLines(output_path, warn = FALSE)
      } else {
        "No output available"
      }
    },
    error = function(e) {
      message("Error reading log and output: ", e$message)
      stop("Error reading log and output")
    }
  )

  # Finalise SQLite DB (write session/output_log + completed sentinel)
  tryCatch(
    {
      generate_decon_rslt(
        log = log,
        output = output,
        db_path = db_path
      )
    },
    error = function(e) {
      message("Error finalising SQLite database: ", e$message)
      stop("Error finalising SQLite database.")
    }
  )
}

# Force a full WAL checkpoint and explicitly remove WAL/SHM files.
# stopCluster() returns as soon as the worker sockets close, so a worker R
# process can still hold a shared lock for a moment afterwards.  Retry for a few
# seconds instead of leaving -wal/-shm sidecars next to the result database.
wal_deadline <- Sys.time() + 15
repeat {
  wal_done <- tryCatch(
    {
      con_wal <- DBI::dbConnect(RSQLite::SQLite(), db_path)
      DBI::dbExecute(con_wal, "PRAGMA busy_timeout=5000")
      DBI::dbExecute(con_wal, "PRAGMA wal_checkpoint(TRUNCATE)")
      DBI::dbExecute(con_wal, "PRAGMA journal_mode=DELETE")
      DBI::dbDisconnect(con_wal)
      for (f in paste0(db_path, c("-wal", "-shm"))) {
        if (file.exists(f)) file.remove(f)
      }
      !any(file.exists(paste0(db_path, c("-wal", "-shm"))))
    },
    error = function(e) {
      tryCatch(
        if (DBI::dbIsValid(con_wal)) DBI::dbDisconnect(con_wal),
        error = function(e2) NULL
      )
      message("Note: WAL cleanup attempt failed: ", e$message)
      FALSE
    }
  )
  if (isTRUE(wal_done)) {
    message("WAL cleanup complete.")
    break
  }
  if (Sys.time() >= wal_deadline) {
    message("Note: WAL cleanup incomplete; sidecar files left in place.")
    break
  }
  Sys.sleep(0.5)
}
