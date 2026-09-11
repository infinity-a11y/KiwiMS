# End-to-end tests for the vendor-agnostic reader and the parameter profiler.
#
# They launch the real deconvolution subprocess and the real Python profiler, so
# they skip unless both a UniDec-capable interpreter and the test corpus are
# present.  Skip them entirely with KIWIMS_SKIP_DECON_RUN=1.

box::use(
  app/logic/ms_suggest[
    aggregate_suggestions,
    profile_ms_samples,
    suggestion_summary,
    window_covers_run
  ],
)

skip_unless_runnable <- function() {
  if (nzchar(Sys.getenv("KIWIMS_SKIP_DECON_RUN"))) {
    skip("KIWIMS_SKIP_DECON_RUN is set")
  }
  if (!dir.exists(kiwims_test_data_root())) {
    skip(paste("No test data at", kiwims_test_data_root()))
  }
  if (!nzchar(kiwims_python())) {
    skip("No Python interpreter that can import UniDec")
  }
}

thermo_samples <- function(n = 1) {
  corpus <- kiwims_thermo_corpus(n)
  if (is.null(corpus)) {
    skip("No Thermo .raw samples in the test corpus")
  }
  kiwims_thermo_files(corpus, n)
}

waters_samples <- function(n = 1) {
  roots <- sort(list.dirs(kiwims_test_data_root(), recursive = FALSE,
                          full.names = TRUE))
  roots <- roots[!grepl("\\.raw$", roots, ignore.case = TRUE)]
  for (r in roots) {
    got <- kiwims_raw_dirs(r, n)
    if (length(got) >= n) {
      return(got)
    }
  }
  skip("No Waters .raw samples in the test corpus")
}

# ---- the reader ------------------------------------------------------------

test_that("a Thermo .raw file deconvolutes end to end", {
  skip_unless_runnable()
  if (!file.exists(kiwims_rscript())) {
    skip("No Rscript available for the deconvolution subprocess")
  }
  samples <- thermo_samples(1)

  work <- withr::local_tempdir()
  res <- kiwims_run_deconvolution(
    raw_dirs = samples,
    work_dir = work,
    # Thermo intact-LC-MS runs here are ten-minute gradients, so leave the
    # window open rather than assume the Waters-era default fits.
    params = kiwims_default_params(masslb = 5000, massub = 100000)
  )

  expect_equal(res$status, 0L, info = paste("output in", res$stdout_path))

  status <- kiwims_db_query(res$db_path, "SELECT * FROM status")
  expect_setequal(status$sample, kiwims_sample_bases(samples))
  expect_equal(
    status$state,
    rep("done", nrow(status)),
    info = paste(status$error_msg, collapse = " | ")
  )

  peaks <- kiwims_db_query(res$db_path, "SELECT * FROM peaks")
  expect_gt(nrow(peaks), 0L)
  expect_true(all(peaks$mass > 0))
})

test_that("the elution window reaches the reader", {
  # Regression: time_start/time_end were written into the run config and into
  # _conf.dat but never applied -- raw_process() called waters_convert2()
  # without a time range, and the plain UniDec engine never reads
  # config.time_start (only ChromEng does).  Every run silently deconvoluted the
  # whole acquisition.  A window outside the acquisition must now fail the
  # sample rather than quietly produce a full-run result.
  skip_unless_runnable()
  if (!file.exists(kiwims_rscript())) {
    skip("No Rscript available for the deconvolution subprocess")
  }
  samples <- waters_samples(1)

  work <- withr::local_tempdir()
  res <- kiwims_run_deconvolution(
    raw_dirs = samples,
    work_dir = work,
    params = kiwims_default_params(time_start = 900, time_end = 1000)
  )

  status <- kiwims_db_query(res$db_path, "SELECT * FROM status")
  expect_equal(status$state, rep("failed", nrow(status)))
  expect_match(
    paste(status$error_msg, collapse = " "),
    "elution window",
    ignore.case = TRUE
  )
})

test_that("fitting the peak width reaches UniDec and changes the fit", {
  # config.mzsig was the one parameter the interface never set, so it stayed at
  # the library default whatever the instrument produced.  This checks the new
  # setting actually arrives: the recorded peak width has to differ, and on this
  # corpus the fit error improves.
  skip_unless_runnable()
  if (!file.exists(kiwims_rscript())) {
    skip("No Rscript available for the deconvolution subprocess")
  }
  samples <- waters_samples(1)

  fit_stats <- function(auto) {
    work <- withr::local_tempdir(.local_envir = parent.frame(2))
    res <- kiwims_run_deconvolution(
      raw_dirs = samples,
      work_dir = work,
      params = kiwims_default_params(auto_peak_width = auto)
    )
    expect_equal(res$status, 0L, info = paste("output in", res$stdout_path))
    err <- kiwims_db_query(res$db_path, "SELECT * FROM error")
    stats::setNames(err$Value, err$Key)
  }

  off <- fit_stats(FALSE)
  on <- fit_stats(TRUE)

  expect_true("mzsig" %in% names(off))
  expect_true("mzsig" %in% names(on))
  # The fitted width is data-derived, so it must not equal the library default.
  expect_false(isTRUE(all.equal(unname(off[["mzsig"]]), unname(on[["mzsig"]]))))
  expect_lt(on[["error"]], off[["error"]])
})

# ---- the profiler ----------------------------------------------------------

test_that("profiling recovers the mass the deconvolution then finds", {
  skip_unless_runnable()
  if (!file.exists(kiwims_rscript())) {
    skip("No Rscript available for the deconvolution subprocess")
  }
  # Not every acquisition resolves -- a sample whose compound out-ionises the
  # protein may legitimately yield no ladder -- so the accuracy claim is checked
  # on whichever sample the profiler was confident about, and the suite skips if
  # this corpus has none.
  samples <- waters_samples(6)
  prof <- profile_ms_samples(samples, python_exe = kiwims_python())
  expect_true(prof$ok, info = prof$error)

  confident <- Filter(
    function(s) identical(s$confidence, "high"),
    prof$samples
  )
  if (!length(confident)) {
    skip("No sample in this corpus resolved a charge-state envelope")
  }
  s <- confident[[1]]
  expect_gt(s$predicted_mass, 0)
  expect_lt(s$time_start, s$time_end)
  expect_gte(s$n_pairs, 6L)

  target <- samples[basename(samples) == s$name]
  expect_length(target, 1L)

  # Deconvolve using only what the profiler proposed for that sample, then check
  # the estimate against the mass UniDec actually reports.  The charge-envelope
  # arithmetic is exact when the ladder is real, so the tolerance is tight.
  fields <- aggregate_suggestions(list(s))
  work <- withr::local_tempdir()
  res <- kiwims_run_deconvolution(
    raw_dirs = target,
    work_dir = work,
    params = kiwims_default_params(
      startz = fields$startz,
      endz = fields$endz,
      minmz = fields$minmz,
      maxmz = fields$maxmz,
      masslb = fields$masslb,
      massub = fields$massub,
      massbins = fields$massbins,
      time_start = fields$time_start,
      time_end = fields$time_end
    )
  )
  expect_equal(res$status, 0L, info = paste("output in", res$stdout_path))

  peaks <- kiwims_db_query(res$db_path, "SELECT * FROM peaks")
  expect_gt(nrow(peaks), 0L)
  expect_lt(min(abs(peaks$mass - s$predicted_mass)), 5)
})

test_that("an unresolved sample does not widen the batch bounds", {
  # A sample with no envelope falls back to 5-100 kDa over charges 1-50; folding
  # that into the aggregate would discard what the confident samples established.
  resolved <- list(time_start = 1, time_end = 2, minmz = 600, maxmz = 1400,
                   masslb = 20000, massub = 26000, startz = 15, endz = 35,
                   massbins = 1, confidence = "high")
  unresolved <- list(time_start = 0.4, time_end = 3, minmz = 480, maxmz = 1600,
                     masslb = 5000, massub = 100000, startz = 1, endz = 50,
                     massbins = 1, confidence = "low")
  agg <- aggregate_suggestions(list(resolved, unresolved))

  expect_equal(agg$masslb, 20000)
  expect_equal(agg$massub, 26000)
  expect_equal(agg$startz, 15)
  expect_equal(agg$endz, 35)
  # The window and m/z range still cover both: neither needs an envelope.
  expect_equal(agg$time_start, 0.4)
  expect_equal(agg$time_end, 3)
  expect_equal(agg$minmz, 480)
  expect_equal(agg$maxmz, 1600)

  # With nothing resolved anywhere, the wide fallback is all there is.
  only_low <- aggregate_suggestions(list(unresolved))
  expect_equal(only_low$masslb, 5000)
  expect_equal(only_low$massub, 100000)
})

test_that("profiling reports the acquisition span and rejects bad input", {
  skip_unless_runnable()
  samples <- waters_samples(1)

  prof <- profile_ms_samples(samples, python_exe = kiwims_python())
  expect_true(prof$ok)
  s <- prof$samples[[1]]
  expect_lt(s$run_start, s$run_end)
  # The suggested window has to sit inside the run it came from.
  expect_gte(s$time_start, s$run_start)
  expect_lte(s$time_end, s$run_end)

  # A window nowhere near the run is reported as such; this is what the UI uses
  # to tell an operator their carried-over default no longer fits.
  check <- window_covers_run(s, s$run_end + 100, s$run_end + 200)
  expect_false(check$ok)
  expect_match(check$msg, "outside")

  # ...and the sample's own window obviously does fit.
  expect_true(window_covers_run(s, s$time_start, s$time_end)$ok)

  expect_match(suggestion_summary(s), "Da|envelope")
})

test_that("Suggest works before the target picker has reported a selection", {
  # Regression: the button read run_target_files(), which is driven by the
  # target picker.  That reactive is empty until the picker's UI has rendered
  # and Shiny has echoed the selection back, so pressing Suggest first -- the
  # common case, since it sits by the parameters rather than the file list --
  # refused with "select at least one sample" on a folder full of samples.
  skip_unless_runnable()
  corpus <- kiwims_thermo_corpus(1)
  if (is.null(corpus)) {
    corpus <- dirname(waters_samples(1)[1])
  }
  withr::local_envvar(RETICULATE_PYTHON = kiwims_python())

  box::use(app / view / deconvolution_main)

  sidebar <- list(
    dir = shiny::reactive(corpus),
    targetpath = shiny::reactive(tempdir()),
    selected = shiny::reactive("folder"),
    use_config = shiny::reactive(FALSE)
  )

  shiny::testServer(
    deconvolution_main$server,
    args = list(
      deconvolution_sidebar_vars = sidebar,
      conversion_main_vars = list(cancel_continuation = shiny::reactive(0)),
      reset_button = shiny::reactive(0),
      config_file = shiny::reactive(data.frame())
    ),
    {
      # Deliberately do NOT set input$target_selector: this is the state the
      # bug was reported in.
      session$setInputs(time_start = 0.5, time_end = 1.5)
      expect_length(run_target_files(), 0L)

      session$setInputs(suggest_params = 1)

      msg <- suggest_msg()
      expect_false(is.null(msg))
      expect_equal(msg$type, "info")
      expect_match(msg$text, "Da|envelope")
    }
  )
})

test_that("an unreadable path is reported, not thrown", {
  skip_unless_runnable()
  prof <- profile_ms_samples(
    file.path(tempdir(), "definitely_absent.raw"),
    python_exe = kiwims_python()
  )
  expect_false(prof$ok)
  expect_true(nzchar(prof$error))
})

test_that("suggestions are aggregated to cover every sample in the batch", {
  # Parameters apply to the whole run, so the batch bounds must contain each
  # sample's own suggestion -- the Thermo pair alone spans 19 kDa and 70 kDa.
  a <- list(time_start = 1, time_end = 2, minmz = 500, maxmz = 1500,
            masslb = 14000, massub = 24000, startz = 8, endz = 34,
            massbins = 1)
  b <- list(time_start = 5, time_end = 6, minmz = 400, maxmz = 3500,
            masslb = 52000, massub = 88000, startz = 64, endz = 96,
            massbins = 0.5)
  agg <- aggregate_suggestions(list(a, b))
  expect_equal(agg$time_start, 1)
  expect_equal(agg$time_end, 6)
  expect_equal(agg$minmz, 400)
  expect_equal(agg$maxmz, 3500)
  expect_equal(agg$masslb, 14000)
  expect_equal(agg$massub, 88000)
  expect_equal(agg$startz, 8)
  expect_equal(agg$endz, 96)
  expect_equal(agg$massbins, 1)
  expect_null(aggregate_suggestions(list()))
})
