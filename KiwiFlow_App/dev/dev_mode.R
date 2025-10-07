startShiny <- function() {
  paths <- c(
    "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe",
    "C:/Users/marian/AppData/Local/BraveSoftware/Brave-Browser/Application/brave.exe"
  )

  options(
    browser = paths[which(file.exists(paths))]
  )
  if (basename(getwd()) != "KiwiFlow_App") {
    setwd("KiwiFlow_App")
  }
  rhino::build_sass()

  shiny::runApp("app.R", launch.browser = T)
}
