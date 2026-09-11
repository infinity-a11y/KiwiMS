# app/logic/ms_suggest.R
#
# Ask app/logic/ms_profile.py what an acquisition looks like, and turn that into
# deconvolution parameters the operator can accept or ignore.
#
# The profiler runs as a subprocess rather than through reticulate. Two reasons:
# the Shiny process never has to host a Python interpreter (importing UniDec
# costs several seconds and would block the session the first time a user
# pressed the button), and a reader that segfaults on a damaged file takes down
# a child process instead of the app.
#
# python.exe needs the conda DLL directories on PATH or compiled extensions fail
# to load with 0xC06D007E, "the specified module could not be found" -- the same
# set-up decon_prepare_python_home() performs for its own subprocess call.

box::use(
  jsonlite[fromJSON],
  processx[run],
)

box::use(
  app / logic / ms_formats[ms_sample_base],
)

# Profiling reads a handful of scan windows per sample; a minute is generous
# even for a large acquisition on a slow share, and bounds the wait if a vendor
# DLL hangs.
default_timeout_s <- 60

python_env_path <- function(python_exe) {
  env_root <- dirname(python_exe)
  paste(
    env_root,
    file.path(env_root, "Library", "bin"),
    Sys.getenv("PATH"),
    sep = ";"
  )
}

#' @export
ms_profiler_script <- function() {
  candidate <- tryCatch(box::file("ms_profile.py"), error = function(e) "")
  if (length(candidate) == 1L && nzchar(candidate) && file.exists(candidate)) {
    return(candidate)
  }
  file.path("app", "logic", "ms_profile.py")
}

# profile_ms_samples(): profile one or more acquisitions ----
#
# Returns list(ok, samples, errors, error). `samples` is a list of one entry per
# readable sample, each carrying both the measurements and the suggested
# parameters. Never signals: a failure is reported in the return value so a
# button handler can show it without a tryCatch of its own.
#' @export
profile_ms_samples <- function(
  paths,
  python_exe = Sys.getenv("RETICULATE_PYTHON"),
  timeout_s = default_timeout_s
) {
  fail <- function(msg) {
    list(ok = FALSE, samples = list(), errors = list(), error = msg)
  }

  paths <- paths[nzchar(paths)]
  if (!length(paths)) {
    return(fail("No sample selected."))
  }
  if (!nzchar(python_exe) || !file.exists(python_exe)) {
    return(fail(
      "Python interpreter not found; parameter suggestions are unavailable."
    ))
  }
  script <- ms_profiler_script()
  if (!file.exists(script)) {
    return(fail(paste0("Profiler script not found at '", script, "'.")))
  }

  res <- tryCatch(
    run(
      python_exe,
      # mustWork = FALSE: a path that no longer exists is the profiler's to
      # report per sample, not something to warn about here.
      c(
        "-W",
        "ignore",
        normalizePath(script, mustWork = FALSE),
        normalizePath(paths, mustWork = FALSE)
      ),
      env = c(
        "current",
        PATH = python_env_path(python_exe),
        PYTHONNOUSERSITE = "1"
      ),
      timeout = timeout_s,
      error_on_status = FALSE
    ),
    error = function(e) {
      list(status = -1L, stdout = "", stderr = conditionMessage(e))
    }
  )

  if (!nzchar(res$stdout)) {
    detail <- utils::tail(strsplit(res$stderr %||% "", "\n")[[1]], 3)
    return(fail(paste0(
      "The profiler produced no output. ",
      paste(trimws(detail), collapse = " ")
    )))
  }

  parsed <- tryCatch(
    fromJSON(res$stdout, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed)) {
    return(fail("Could not read the profiler response."))
  }
  parsed$samples <- parsed$samples %||% list()
  parsed$errors <- parsed$errors %||% list()
  parsed
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# suggestion_fields(): profile entry -> the numeric inputs it should fill ----
# Only the parameters the data actually speaks to. Peak threshold, peak window
# and normalisation are operator preferences about how results are reported, not
# properties of the acquisition, so they are deliberately left alone.
#' @export
suggestion_fields <- function(sample) {
  list(
    time_start = sample$time_start,
    time_end = sample$time_end,
    minmz = sample$minmz,
    maxmz = sample$maxmz,
    masslb = sample$masslb,
    massub = sample$massub,
    startz = sample$startz,
    endz = sample$endz,
    massbins = sample$massbins
  )
}

# aggregate_suggestions(): one parameter set that suits every profiled sample ----
#
# Deconvolution parameters apply to the whole run, not per sample, so a
# suggestion taken from one file can be wrong for the batch -- the reference
# Thermo pair alone spans 19.3 kDa and 70.2 kDa. Every bound is therefore
# widened to cover all of them: the narrowest setting that still contains every
# sample's own suggestion.
#' @export
aggregate_suggestions <- function(samples) {
  if (!length(samples)) {
    return(NULL)
  }
  pick <- function(from, field, fn) {
    vals <- vapply(
      from,
      function(s) as.numeric(s[[field]] %||% NA_real_),
      numeric(1L)
    )
    vals <- vals[is.finite(vals)]
    if (!length(vals)) NA_real_ else fn(vals)
  }

  # Mass and charge bounds come only from samples where a charge ladder was
  # actually resolved. A sample that produced none falls back to 5-100 kDa over
  # charges 1-50, and folding that into the batch would drag every bound back to
  # the factory-wide defaults on the strength of the one file we learned nothing
  # from -- a worse suggestion than the confident samples alone support.
  # Elution window and m/z range are kept from every sample: both are read off
  # the chromatogram and the signal region, neither needs an envelope.
  resolved <- Filter(
    function(s) !identical(s$confidence %||% "low", "low"),
    samples
  )
  if (!length(resolved)) {
    resolved <- samples
  }

  list(
    time_start = round(pick(samples, "time_start", min), 3),
    time_end = round(pick(samples, "time_end", max), 3),
    minmz = round(pick(samples, "minmz", min), 1),
    maxmz = round(pick(samples, "maxmz", max), 1),
    masslb = pick(resolved, "masslb", min),
    massub = pick(resolved, "massub", max),
    startz = pick(resolved, "startz", min),
    endz = pick(resolved, "endz", max),
    massbins = pick(samples, "massbins", max)
  )
}

# suggestion_summary(): one human sentence about what was found ----
#' @export
suggestion_summary <- function(sample) {
  conf <- sample$confidence %||% "low"
  win <- sprintf("%.2f-%.2f min", sample$time_start, sample$time_end)
  if (identical(conf, "low")) {
    return(paste0(
      "No charge-state envelope resolved in ",
      ms_sample_base(sample$name),
      ". Elution window set to ",
      win,
      " and mass range left wide - check the parameters before running."
    ))
  }
  paste0(
    ms_sample_base(sample$name),
    ": ",
    format(round(sample$predicted_mass, 1), big.mark = ",", trim = TRUE),
    " Da from ",
    sample$n_pairs,
    " charge-state pairs (",
    conf,
    " confidence), eluting ",
    win,
    "."
  )
}

# window_covers_run(): does an elution window actually sit inside the run? ----
# Guards the case that makes the now-live elution window dangerous: a window
# tuned for a 90-second Waters run applied to a 10-minute gradient. Returns
# list(ok, msg); msg is NULL when there is nothing to say.
#' @export
window_covers_run <- function(sample, time_start, time_end) {
  run_start <- sample$run_start
  run_end <- sample$run_end
  if (
    is.null(run_start) ||
      is.null(run_end) ||
      !is.finite(time_start) ||
      !is.finite(time_end)
  ) {
    return(list(ok = TRUE, msg = NULL))
  }
  overlap <- min(time_end, run_end) - max(time_start, run_start)
  span <- run_end - run_start
  label <- sprintf("%.2f-%.2f min", run_start, run_end)

  if (overlap <= 0) {
    return(list(
      ok = FALSE,
      msg = paste0(
        "The elution window ",
        sprintf("%.2f-%.2f min", time_start, time_end),
        " lies outside ",
        ms_sample_base(sample$name),
        ", which was acquired over ",
        label,
        "."
      )
    ))
  }
  if (span > 0 && overlap / span < 0.02) {
    return(list(
      ok = TRUE,
      msg = paste0(
        "The elution window covers under 2% of ",
        ms_sample_base(sample$name),
        " (acquired over ",
        label,
        ")."
      )
    ))
  }
  list(ok = TRUE, msg = NULL)
}
