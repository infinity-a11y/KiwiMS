startShiny <- function() {
  paths <- c(
    "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe",
    "C:/Users/marian/AppData/Local/BraveSoftware/Brave-Browser/Application/brave.exe"
  )

  Sys.setenv(KIWIMS_DEV_MODE = "TRUE")
  options(
    browser = paths[which(file.exists(paths))]
  )

  if (basename(getwd()) != "KiwiMS_App") {
    setwd("KiwiMS_App")
  }

  rhino::build_sass()

  shiny::runApp("app.R", launch.browser = T)
}
