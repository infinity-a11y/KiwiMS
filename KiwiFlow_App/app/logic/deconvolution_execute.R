# app/logic/deconvolution_execute.R

message("Initiating deconvolution ...")

# Sourcing deconvolution functions
tryCatch(
  {
    source_file <- file.path(
      commandArgs(trailingOnly = TRUE)[3],
      "app/logic/deconvolution_functions.R"
    )
    message(paste("Sourcing", source_file))
    source(source_file)
  },
  error = function(e) {
    message("Error sourcing deconvolution functions: ", e$message)
    stop("Deconvolution failed.")
  }
)

# Setting deconvolution parameters
tryCatch(
  {
    temp <- commandArgs(trailingOnly = TRUE)[1]
    conf <- readRDS(file.path(temp, "config.rds"))
    logfile <- commandArgs(trailingOnly = TRUE)[2]
    result_dir <- commandArgs(trailingOnly = TRUE)[4]
  },
  error = function(e) {
    message("Error setting deconvolution parameters: ", e$message)
    stop("Error setting deconvolution parameters.")
  }
)

# Start deconvolution
tryCatch(
  {
    deconvolute(
      conf$dirs,
      result_dir,
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
    message("Error in deconvolution processing: ", e$message)
    stop("Error in deconvolution processing")
  }
)

# Read log and output
tryCatch(
  {
    log <- if (file.exists(logfile)) {
      readLines(logfile, warn = FALSE)
    } else {
      c("No log")
    }

    output <- if (file.exists(file.path(temp, "output.txt"))) {
      readLines(file.path(temp, "output.txt"), warn = FALSE)
    } else {
      "No output"
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
