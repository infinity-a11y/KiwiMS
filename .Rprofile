# A session started at the repo root (Positron) runs on the app's renv
# library, the one the installer ships, not on the user's global library:
# packages that differed between the two gave bugs only the exe showed.
# KiwiMS_App/.Rprofile, which activates renv there, never runs from here, as
# startShiny() only changes into KiwiMS_App after startup.
# KIWIMS_GLOBAL_LIB=TRUE keeps the global library.
if (!identical(Sys.getenv("KIWIMS_GLOBAL_LIB"), "TRUE")) {
  local({
    owd <- setwd("KiwiMS_App")
    on.exit(setwd(owd))
    source("renv/activate.R")
  })
}

source("KiwiMS_App/dev/dev_mode.R")
