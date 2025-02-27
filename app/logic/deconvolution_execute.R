# app/logic/deconvolution_execute.R

box::use(
  app /
    logic /
    deconvolution_functions[
      deconvolute,
      create_384_plate_heatmap,
      spectrum_plot
    ],
)

# Get parameters
tmp <- commandArgs(trailingOnly = TRUE)[1]
conf <- readRDS(tmp)

# Prepare results files
# if (conf$overwrite != FALSE) {
#   conf$dirs[conf$overwrite]
# }

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
