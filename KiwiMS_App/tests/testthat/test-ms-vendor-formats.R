# End-to-end tests for the elution window of the vendor-agnostic reader, on a
# Waters sample of the reference dataset kinact_MLKL_3 (tests/reference_data).
# The Thermo reader is checked against its reference peaks in
# test-reference-data.R.
#
# They launch the real deconvolution subprocess, so they skip unless the
# dataset and a UniDec-capable interpreter are present.  Skip them entirely
# with KIWIMS_SKIP_DECON_RUN=1.

# ---- the reader ------------------------------------------------------------

test_that("the elution window reaches the reader", {
  # Regression: time_start/time_end were written into the run config and into
  # _conf.dat but never applied -- raw_process() called waters_convert2()
  # without a time range, and the plain UniDec engine never reads
  # config.time_start (only ChromEng does).  Every run silently deconvoluted the
  # whole acquisition.  A window outside the acquisition must now fail the
  # sample rather than quietly produce a full-run result.
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

test_that("a blank elution bound is open, not an error", {
  # The UI hands a blank numeric input over as NA. It must reach Python as an
  # open bound (here: to the last scan), so a start past the end of the
  # acquisition still selects nothing -- rather than crashing on a bare NA or
  # falling back to the whole run.
  samples <- waters_samples(1)

  work <- withr::local_tempdir()
  res <- kiwims_run_deconvolution(
    raw_dirs = samples,
    work_dir = work,
    params = kiwims_default_params(time_start = 900, time_end = NA_real_)
  )

  status <- kiwims_db_query(res$db_path, "SELECT * FROM status")
  expect_equal(status$state, rep("failed", nrow(status)))
  expect_match(
    paste(status$error_msg, collapse = " "),
    "elution window",
    ignore.case = TRUE
  )
})
