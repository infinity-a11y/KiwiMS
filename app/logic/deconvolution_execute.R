# app/logic/deconvolution_execute.R

box::use(
  app /
    logic /
    deconvolution_functions[
      deconvolute,
    ],
  app / logic / logging[get_log],
  app / logic / report_functions[generate_decon_rslt],
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

log <- if (file.exists(get_log())) readLines(get_log(), warn = FALSE) else
  c("No logs yet.")

output <- if (file.exists(file.path(temp, "output.txt"))) {
  readLines(file.path(temp, "output.txt"), warn = FALSE)
} else {
  "Log file not found."
}

result <- generate_decon_rslt(
  paths = conf$dirs,
  log = log,
  output = output
)

saveRDS(result, file.path(dirname(dirname(get_log())), "result.rds"))
