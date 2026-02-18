config <- list(
  params = data.frame(
    startz = 1,
    endz = 50,
    minmz = 710,
    maxmz = 1100,
    masslb = 10000,
    massub = 60000,
    massbins = 0.5,
    peakthresh = 0.07,
    peakwindow = 40,
    peaknorm = 1,
    time_start = 0.5,
    time_end = 1.5
  ),
  dirs = paste0(
    commandArgs(trailingOnly = TRUE)[1],
    "\\resources\\2025-06-19_KARL-light+Ada_10_30min_01.raw"
  ),
  selected = "file"
)

saveRDS(
  config,
  file.path(commandArgs(trailingOnly = TRUE)[1], "resources/config.rds")
)
