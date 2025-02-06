# app/logic/deconvolution_functions.R

box::use(
  utils[read.table],
  parallel[detectCores, makeCluster, parLapply, stopCluster]
)

#' @export
deconvolute <- function(parent_dir, py_script,
                        num_cores = detectCores() - 1,
                        config_startz = 1, config_endz = 50,
                        config_minmz = '', config_maxmz = '',
                        config_masslb = 5000, config_massub = 500000,
                        config_massbins = 10, config_peakthresh = 0.1,
                        config_peakwindow = 500, config_peaknorm = 1,
                        config_time_start = '', config_time_end = '') {

  # Find all .raw directories
  raw_dirs <- list.dirs(parent_dir, full.names = TRUE, recursive = FALSE)
  raw_dirs <- raw_dirs[grep("\\.raw$", raw_dirs)]

  if (length(raw_dirs) == 0) {
    stop("No .raw directories found in ", parent_dir)
  }

  message(sprintf("Found %d .raw directories to process", length(raw_dirs)))

  # Define the processing function for each directory
  process_single_dir <- function(waters_dir, py_script) {
    cmd <- paste("python", shQuote(py_script), shQuote(waters_dir),
                 shQuote(config_startz), shQuote(config_endz),
                 shQuote(config_minmz), shQuote(config_maxmz),
                 shQuote(config_masslb), shQuote(config_massub),
                 shQuote(config_massbins), shQuote(config_peakthresh),
                 shQuote(config_peakwindow), shQuote(config_peaknorm),
                 shQuote(config_time_start), shQuote(config_time_end))

    message(sprintf("Processing: %s", waters_dir))
    message(sprintf("Command: %s", cmd))

    tryCatch({
      output <- base::system(cmd, intern = TRUE, ignore.stderr = FALSE)
      list(status = "success",
           dir = waters_dir,
           output = output)
    }, error = function(e) {
      list(status = "error",
           dir = waters_dir,
           error = as.character(e))
    })
  }

  # Process directories in parallel
  if(num_cores > 1) {
    cl <- makeCluster(num_cores)
    on.exit(stopCluster(cl))

    message(paste0(num_cores, " cores detected. Parallel processing started."))

    results <- parLapply(cl, raw_dirs, process_single_dir,
                                   py_script = py_script)
  } else {
    message(paste0(num_cores, " core(s) detected. Slowed processing started."))
    
    results <- lapply(raw_dirs, process_single_dir, py_script = py_script)
  }

  # Summarize results
  successful <- sum(sapply(results, function(x) x$status == "success"))
  failed <- sum(sapply(results, function(x) x$status == "error"))

  message(
    sprintf(
      "\nProcessing complete:\n- Successfully processed: %d\n- Failed: %d",
      successful, failed))
  
  return(results)
}

#' @export
plot_ms_spec <- function(waters_dir) {
  
  # Get results directories
  unidecfiles <- list.files(waters_dir, full.names = TRUE)
  
  # Get file
  mass_intensity <- grep("_mass\\.txt$", unidecfiles, value = TRUE)
  mass_data <- read.table(mass_intensity, sep = " ", header = TRUE)
  colnames(mass_data) <- c("mz", "intensity")
  
  plot(mass_data$mz, mass_data$intensity, type = "h",
       xlab = "Mass (Da)", ylab = "Intensity",
       main = "Deconvoluted Mass Spectrum",
       col = "blue", lwd = 3)
}