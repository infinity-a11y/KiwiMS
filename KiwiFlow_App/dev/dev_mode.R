startShiny <- function() {
  options(
    browser = "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe"
  )
  if (basename(getwd()) != "KiwiFlow_App") {
    setwd("KiwiFlow_App")
  }
  rhino::build_sass()

  shiny::runApp("app.R", launch.browser = T)
}
