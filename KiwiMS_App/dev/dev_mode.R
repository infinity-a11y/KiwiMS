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
  #
  # NOTE: In dev we use the live conda env directly, NOT env_kiwims/.
  # env_kiwims/ is a conda-pack artifact (paths still have placeholders until
  # conda-unpack.exe runs post-install). Using it unpacked here would break
  # reticulate's config probe and leave env_kiwims/ unusable for installer builds.
  dev_python <- "C:/Users/marian/AppData/Local/miniconda3/envs/kiwims/python.exe"
  if (!file.exists(dev_python)) {
    stop(
      "Dev Python not found at: ", dev_python,
      "\nUpdate dev_python in dev/dev_mode.R to point at your kiwims conda env."
    )
  }
  # Determine which Rscript to use for the deconvolution subprocess.
  # R-Portable has no registry entry so it needs R_HOME set explicitly to find
  # itself. Run a quick pre-flight to confirm it works; fall back to system R
  # if it fails (e.g. DLL not found, bad build), and clear R_HOME so the
  # system R subprocess uses its own registered home.
  r_portable     <- normalizePath("R-Portable",                  mustWork = FALSE)
  r_portable_exe <- normalizePath("R-Portable/bin/Rscript.exe",  mustWork = FALSE)
  old_r_home <- Sys.getenv("R_HOME", unset = "")
  Sys.setenv(R_HOME = r_portable)  # set before test so the child inherits it

  r_portable_ok <- tryCatch({
    res <- processx::run(
      r_portable_exe,
      args            = c("--vanilla", "--no-save", "--no-restore", "-e", "cat('OK')"),
      timeout         = 15,
      error_on_status = FALSE
    )
    trimws(res$stdout) == "OK"
  }, error = function(e) FALSE)

  if (r_portable_ok) {
    message("[dev_mode] R-Portable pre-flight: OK  -> using R-Portable for subprocess")
    dev_rscript <- r_portable_exe
  } else {
    message("[dev_mode] R-Portable pre-flight: FAIL -> falling back to system R")
    # Restore R_HOME so system R subprocess uses the correct (registered) home.
    if (nzchar(old_r_home)) Sys.setenv(R_HOME = old_r_home) else Sys.unsetenv("R_HOME")
    dev_rscript <- file.path(R.home("bin"), "Rscript.exe")
  }

  # PYTHONHOME must NOT be set for conda env Python — conda envs are self-contained
  # and Python finds its own stdlib from the executable location. Any wrong value
  # (even one level too high) causes "No module named 'encodings'" in every child
  # process. Explicitly unset it to clear any stale OS/session value.
  Sys.unsetenv("PYTHONHOME")

  Sys.setenv(
    RETICULATE_PYTHON = dev_python,
    PYTHONNOUSERSITE  = "1",  # prevent roaming user site-packages leaking in
    KIWIMS_RSCRIPT    = dev_rscript
  )

  rhino::build_sass()

  shiny::runApp("app.R", launch.browser = T)
}
