# app/logic/deconvolution_execute.R

box::use(
  app /
    logic /
    deconvolution_functions[
      deconvolute, generate_decon_rslt, spectrum_plot, process_single_dir
    ],
)

# Get parameters
temp <- commandArgs(trailingOnly = TRUE)[1]
conf <- readRDS(file.path(temp, "config.rds"))

# Start deconvolution
deconvolute(
  conf$dirs,
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

logfile <- commandArgs(trailingOnly = TRUE)[2]
log <- if (file.exists(logfile)) readLines(logfile, warn = FALSE) else
  c("No log")

output <- if (file.exists(file.path(temp, "output.txt"))) {
  readLines(file.path(temp, "output.txt"), warn = FALSE)
} else {
  "No output"
}

result <- generate_decon_rslt(
  paths = conf$dirs,
  log = log,
  output = output
)

results_dir <- file.path(Sys.getenv("USERPROFILE"), 
                         "Documents", "KiwiFlow", "results")
saveRDS(result, file.path(results_dir, "result.rds"), compress = FALSE)
