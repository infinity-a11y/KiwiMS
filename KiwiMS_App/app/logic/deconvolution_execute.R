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

    DBI::dbDisconnect(con_init)
  },
  error = function(e) {
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
