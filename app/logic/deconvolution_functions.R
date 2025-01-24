# app/logic/deconvolution_functions.R

box::use(
  utils[read.table],
)

#' @export
deconvolute <- function(waters_dir, py_script) {
  cmd <- base::paste("python", py_script, waters_dir)
  base::system(cmd, intern = T)
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