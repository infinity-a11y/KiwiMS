startShiny <- function() {
  # Garbage collection
  gc()

  Sys.setenv(KIWIMS_DEV_MODE = "TRUE")

  paths <- c(
    "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe",
    "C:/Users/marian/AppData/Local/BraveSoftware/Brave-Browser/Application/brave.exe"
  )
  options(
    browser = paths[which(file.exists(paths))]
  )

  if (basename(getwd()) != "KiwiMS_App") {
    setwd("KiwiMS_App")
  }

  # Mirror the env vars that launch.ps1 sets in production.
  # KIWIMS_RSCRIPT forces the deconvolution subprocess to use R-Portable, whose
  # R version matches the compiled packages in renv/library — system R does not.
  Sys.setenv(
    PYTHONHOME        = normalizePath("env_kiwims", mustWork = FALSE),
    RETICULATE_PYTHON = normalizePath("env_kiwims/python.exe", mustWork = FALSE),
    KIWIMS_RSCRIPT    = normalizePath("R-Portable/bin/Rscript.exe", mustWork = FALSE)
  )

  rhino::build_sass()

  shiny::runApp("app.R", launch.browser = T)
}
