# app/logic/deconvolution_functions.R

box::use(
  utils[read.table],
)

#' @export
deconvolute <- function(waters_dir, py_script, config_startz = 1, 
                        config_endz = 50, config_minmz = '', 
                        config_maxmz = '', config_masslb = 5000, 
                        config_massub = 500000, config_massbins = 10, 
                        config_peakthresh = 0.1, config_peakwindow = 500,
                        config_peaknorm = 1, config_time_start = '',
                        config_time_end = '') {
  
  cmd <- paste("python", shQuote(py_script), shQuote(waters_dir), 
               shQuote(config_startz), shQuote(config_endz), 
               shQuote(config_minmz), shQuote(config_maxmz),
               shQuote(config_masslb), shQuote(config_massub), 
               shQuote(config_massbins), shQuote(config_peakthresh), 
               shQuote(config_peakwindow), shQuote(config_peaknorm), 
               shQuote(config_time_start), shQuote(config_time_end))
  
  print(cmd)
  
  output <- base::system(cmd, intern = TRUE, ignore.stderr = FALSE)
  print(output)
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