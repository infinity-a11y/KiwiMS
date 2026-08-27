# Shared setup and helpers for the KiwiMS test suite.
#
# testthat runs with the working directory set to tests/testthat, while the box
# modules under app/ resolve against the application root.  rhino::test_r()
# configures neither, so both are done here.

kiwims_app_root <- local({
  root <- normalizePath(
    file.path("..", ".."),
    winslash = "/",
    mustWork = FALSE
  )
  if (!dir.exists(file.path(root, "app", "logic"))) {
    # Fallback for runs started from the application root itself.
    root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  }
  function() root
})

options(box.path = kiwims_app_root())

# kiwims_test_data_root(): Root directory holding the .raw test corpora ----
# Override with KIWIMS_TEST_DATA to point the suite at another copy.
kiwims_test_data_root <- function() {
  Sys.getenv("KIWIMS_TEST_DATA", unset = "E:/KF_Testing/Test-Data")
}

# kiwims_raw_dirs(): Waters .raw sample directories inside `dir` ----
# Sorted so a run is reproducible; `n` takes the first n samples.
kiwims_raw_dirs <- function(dir, n = Inf) {
  if (!dir.exists(dir)) {
    return(character(0))
  }
  dirs <- list.dirs(dir, recursive = FALSE, full.names = TRUE)
  dirs <- sort(dirs[grepl("\\.raw$", dirs, ignore.case = TRUE)])
  head(dirs, n)
}

# kiwims_python(): Locate a Python interpreter that can import UniDec ----
# Checked once per session and cached; returns "" when nothing usable is found
# so callers can skip instead of failing.
kiwims_python <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) {
      return(cached)
    }
    candidates <- c(
      Sys.getenv("KIWIMS_TEST_PYTHON"),
      Sys.getenv("RETICULATE_PYTHON"),
      file.path(
        Sys.getenv("LOCALAPPDATA"),
        "miniconda3/envs/kiwims/python.exe"
      ),
      "C:/ProgramData/miniconda3/envs/kiwims/python.exe",
      file.path(kiwims_app_root(), "env_kiwims/python.exe")
    )
    candidates <- unique(candidates[nzchar(candidates)])

    for (py in candidates) {
      if (!file.exists(py)) {
        next
      }
      # Importing UniDec is the actual requirement, and a conda-pack artifact
      # whose paths have not been rewritten by conda-unpack fails here.
      #
      # Launched bare, the interpreter cannot find the env's native DLLs, so
      # the probe puts them on PATH.  reticulate does the equivalent itself,
      # which is why the app never needs this.
      env <- c(
        "current",
        PATH = paste(
          dirname(py),
          file.path(dirname(py), "Library", "bin"),
          Sys.getenv("PATH"),
          sep = ";"
        )
      )
      ok <- tryCatch(
        {
          res <- processx::run(
            py,
            c("-c", "import unidec"),
            env = env,
            error_on_status = FALSE,
            timeout = 300
          )
          res$status == 0
        },
        error = function(e) FALSE
      )
      if (ok) {
        cached <<- normalizePath(py, winslash = "/")
        return(cached)
      }
    }
    cached <<- ""
    cached
  }
})

# kiwims_rscript(): Rscript used for the deconvolution subprocess ----
# Mirrors what launch.ps1 (installed runtime) and dev/dev_mode.R (dev runtime)
# hand to the app: R-Portable when present, otherwise the running R.
kiwims_rscript <- function() {
  from_env <- Sys.getenv("KIWIMS_RSCRIPT")
  if (nzchar(from_env) && file.exists(from_env)) {
    return(normalizePath(from_env, winslash = "/"))
  }
  portable <- file.path(kiwims_app_root(), "R-Portable/bin/Rscript.exe")
  if (file.exists(portable)) {
    return(normalizePath(portable, winslash = "/"))
  }
  normalizePath(
    file.path(R.home("bin"), "Rscript.exe"),
    winslash = "/",
    mustWork = FALSE
  )
}

# kiwims_default_params(): The parameter frame the UI writes into config.rds ----
kiwims_default_params <- function(...) {
  params <- data.frame(
    startz = 1,
    endz = 50,
    minmz = "",
    maxmz = "",
    masslb = 5000,
    massub = 500000,
    massbins = 10,
    peakthresh = 0.1,
    peakwindow = 500,
    peaknorm = 1,
    time_start = "",
    time_end = "",
    stringsAsFactors = FALSE
  )
  overrides <- list(...)
  for (nm in names(overrides)) {
    params[[nm]] <- overrides[[nm]]
  }
  params
}

# kiwims_run_deconvolution(): Drive the real deconvolution subprocess ----
# Reproduces exactly what app/view/deconvolution_main.R launches, so the test
# exercises the whole path: config.rds, DB initialisation, the worker pool, the
# per-sample writes and the WAL cleanup.  Returns the DB path, the process
# result and the wall-clock duration.
kiwims_run_deconvolution <- function(
  raw_dirs,
  work_dir,
  analysis_name = "test_analysis",
  params = kiwims_default_params(),
  keep_raw_output = FALSE,
  timeout = 3600,
  env = character(0)
) {
  app_root <- kiwims_app_root()
  python <- kiwims_python()
  stopifnot(nzchar(python), length(raw_dirs) > 0)

  temp_dir <- file.path(work_dir, "temp")
  result_dir <- file.path(work_dir, "results")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(result_dir, showWarnings = FALSE, recursive = TRUE)

  saveRDS(
    list(params = params, dirs = raw_dirs, selected = "folder"),
    file.path(temp_dir, "config.rds")
  )

  log_path <- file.path(work_dir, "kiwims.log")
  writeLines(character(0), log_path)
  out_path <- file.path(work_dir, "process_out.log")
  db_path <- file.path(result_dir, paste0(analysis_name, ".db"))

  rscript <- kiwims_rscript()
  child_env <- c(
    "current",
    RETICULATE_PYTHON = python,
    PYTHONNOUSERSITE = "1",
    env
  )
  if (startsWith(rscript, normalizePath(app_root, winslash = "/"))) {
    # R-Portable has no registry entry, so it needs R_HOME to find itself.
    child_env <- c(
      child_env,
      R_HOME = normalizePath(
        file.path(app_root, "R-Portable"),
        winslash = "/"
      )
    )
  }

  started <- Sys.time()
  res <- processx::run(
    rscript,
    args = c(
      "app/logic/deconvolution_execute.R",
      temp_dir,
      log_path,
      app_root,
      result_dir,
      "FALSE", # KIWIMS_DEV_MODE; anything but "testing" runs the full path
      db_path,
      as.character(isTRUE(keep_raw_output))
    ),
    wd = app_root,
    env = child_env,
    error_on_status = FALSE,
    timeout = timeout,
    stderr_to_stdout = TRUE
  )
  writeLines(res$stdout, out_path)

  list(
    db_path = db_path,
    result_dir = result_dir,
    status = res$status,
    stdout = res$stdout,
    stdout_path = out_path,
    elapsed = as.numeric(difftime(Sys.time(), started, units = "secs"))
  )
}

# kiwims_cluster_log(): Where worker-side messages land ----
# Worker output cannot reach the parent's stdout, so deconvolute() points the
# pool at this file.  Each run truncates it.
kiwims_cluster_log <- function() {
  file.path(Sys.getenv("LOCALAPPDATA"), "KiwiMS", "last_cluster_log.txt")
}

# kiwims_db_query(): Read-only query against a run's SQLite database ----
kiwims_db_query <- function(db_path, sql, ...) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, sql, ...)
}

# kiwims_sample_bases(): Sample keys as the pipeline derives them ----
kiwims_sample_bases <- function(raw_dirs) {
  gsub("\\.raw$", "", basename(raw_dirs), ignore.case = TRUE)
}
