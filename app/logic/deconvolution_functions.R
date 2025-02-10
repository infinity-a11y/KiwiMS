# app/logic/deconvolution_functions.R

box::use(
  utils[read.table],
  shiny[showNotification],
  parallel[detectCores, makeCluster, parLapply, stopCluster],
  reticulate[use_python, py_config, py_run_string],
)

#' @export
deconvolute <- function(raw_dirs, num_cores = detectCores() - 1,
                        config_startz = 1, config_endz = 50,
                        config_minmz = '', config_maxmz = '',
                        config_masslb = 5000, config_massub = 500000,
                        config_massbins = 10, config_peakthresh = 0.1,
                        config_peakwindow = 500, config_peaknorm = 1,
                        config_time_start = '', config_time_end = '') {
  
  # ensure python path and packages availability
  py_outcome <- tryCatch({
    use_python(py_config()$python, required = TRUE)
    TRUE
  }, error = function(e) {
    # showNotification(
    #   "Python modules could not be loaded. Aborting.",
    #   type = "error",
    #   duration = NULL
    # )
    FALSE
  })
  
  if (!py_outcome) {
    return()
  }
  
  # Define the processing function without default arguments
  process_single_dir <- function(waters_dir, 
                                 startz, endz, minmz, maxmz,
                                 masslb, massub, massbins, peakthresh,
                                 peakwindow, peaknorm, time_start, time_end) {
    
    input_path <- gsub("\\\\", "/", waters_dir)
    
    # Function to properly format parameters for Python
    format_param <- function(x) {
      if (is.character(x) && x == "") {
        return("''")  # Empty string becomes quoted empty string
      } else {
        return(as.character(x))  # Numbers remain as-is
      }
    }
    
    # Create parameters string for Python
    params_string <- sprintf(
      '"startz": %s, "endz": %s, "minmz": %s, "maxmz": %s, "masslb": %s, "massub": %s, "massbins": %s, "peakthresh": %s, "peakwindow": %s, "peaknorm": %s, "time_start": %s, "time_end": %s',
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
    
    reticulate::py_run_string(sprintf('
import sys
import unidec
import re

# Initialize UniDec engine
engine = unidec.UniDec()

# Convert Waters .raw to txt
input_file = r"%s"
engine.raw_process(input_file)
txt_file = re.sub(r"\\.raw$", "_rawdata.txt", input_file)
engine.open_file(txt_file)

# Parameters passed from R
params = {%s}

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
', input_path, params_string))
  }
  
  # showNotification(paste0("Deconvolution initiated"),
  #                  type = "message", duration = NULL)
  
  # Process directories in parallel
  if(num_cores > 1) {
    cl <- makeCluster(num_cores)
    on.exit(stopCluster(cl))
    
    message(paste0(num_cores, " cores detected. Parallel processing started."))
    
    # Create wrapper function that includes all parameters
    process_wrapper <- function(dir) {
      process_single_dir(dir, 
                         config_startz, config_endz,
                         config_minmz, config_maxmz,
                         config_masslb, config_massub,
                         config_massbins, config_peakthresh,
                         config_peakwindow, config_peaknorm,
                         config_time_start, config_time_end)
    }
    
    results <- parLapply(cl, raw_dirs, process_wrapper)
    
  } else {
    # message(paste0(num_cores, " core(s) detected. Sequential processing started."))
    
    results <- lapply(raw_dirs, function(dir) {
      process_single_dir(dir, 
                         config_startz, config_endz,
                         config_minmz, config_maxmz,
                         config_masslb, config_massub,
                         config_massbins, config_peakthresh,
                         config_peakwindow, config_peaknorm,
                         config_time_start, config_time_end)
    })
  }
  
  # Summarize results
  successful <- sum(sapply(results, function(x) !is.null(x)))
  failed <- length(results) - successful

  # showNotification("Deconvolution finalized", type = "message", duration = NULL)
  # message(sprintf(
  #   "\nProcessing complete:\n- Successfully processed: %d\n- Failed: %d",
  #   successful, failed))
  
  # return(results)
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