# app/logic/deconvolution_execute.R

# Enable conda DLL search
Sys.setenv(CONDA_DLL_SEARCH_MODIFICATION_ENABLE = "1")

# Checking library Paths
message(paste("Current library paths: \n", paste(.libPaths(), collapse = "\n")))

# In dev mode manually add library paths
if (commandArgs(trailingOnly = TRUE)[5] == "TRUE") {
  .libPaths(c(
    normalizePath(file.path(
      Sys.getenv("LOCALAPPDATA"),
      "R",
      "win-library",
      "4.5"
    )),
    .libPaths()
  ))

  message(paste(
    "Modified library paths: \n",
    paste(.libPaths(), collapse = "\n")
  ))
}

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

# Start deconvolution
tryCatch(
  {
    deconvolute(
      raw_dirs = conf$dirs,
      result_dir = result_dir,
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

  # Summarizing results in rds file
  tryCatch(
    {
      result <- generate_decon_rslt(
        paths = conf$dirs,
        log = log,
        output = output,
        result_dir = result_dir,
        temp_dir = temp
      )

      result_id <- gsub(
        ".log",
        "_RESULT.rds",
        basename(logfile)
      )

      saveRDS(result, file.path(result_dir, result_id), compress = FALSE)
    },
    error = function(e) {
      message("Error in result file generation: ", e$message)
      stop("Error in result file generation.")
    }
  )
}
