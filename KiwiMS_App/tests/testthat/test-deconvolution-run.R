# End-to-end tests: they launch the real deconvolution subprocess exactly the
# way app/view/deconvolution_main.R does, against real Waters .raw samples of
# the reference dataset kinact_MLKL_3 (tests/reference_data).
#
# They skip unless the dataset is at $KIWIMS_REFERENCE_DATA and a
# UniDec-capable Python interpreter is present, and fail when the dataset
# differs from its manifest. Skip them entirely with KIWIMS_SKIP_DECON_RUN=1.
# What the deconvolution finds is compared in test-reference-data.R; these
# tests are about the pipeline around it.

expect_run_succeeded <- function(res, raw_dirs) {
  expect_equal(
    res$status,
    0L,
    info = paste("subprocess output in", res$stdout_path)
  )

  tables <- kiwims_db_query(
    res$db_path,
    "SELECT name FROM sqlite_master WHERE type='table'"
  )$name
  expect_true(all(c("status", "metadata", "peaks", "mass_data") %in% tables))

  # The completion sentinel is what the Shiny progress observer waits for.
  expect_true("completed" %in% tables)

  status <- kiwims_db_query(res$db_path, "SELECT * FROM status")
  expect_setequal(status$sample, kiwims_sample_bases(raw_dirs))
  expect_equal(status$state, rep("done", nrow(status)))
}


test_that("a parallel run deconvolutes every sample and finalises the database", {
  raw_dirs <- waters_samples(4)

  work <- withr::local_tempdir("kiwims-par")
  res <- kiwims_run_deconvolution(raw_dirs, work)

  expect_run_succeeded(res, raw_dirs)

  # Worker bring-up is the step this suite guards against regressing: it used
  # to serialise Python initialisation across the pool and take minutes.
  ready <- grep(
    "worker\\(s\\) ready after",
    strsplit(res$stdout, "\r?\n")[[1]],
    value = TRUE
  )
  expect_length(ready, 1L)
  seconds <- as.numeric(sub(
    ".*ready after ([0-9.]+) s.*",
    "\\1",
    ready
  ))
  expect_lt(seconds, 90)

  # Every sample must carry usable spectra, not just a done marker.
  per_sample <- kiwims_db_query(
    res$db_path,
    "SELECT sample, COUNT(*) AS n FROM mass_data GROUP BY sample"
  )
  expect_setequal(per_sample$sample, kiwims_sample_bases(raw_dirs))
  expect_true(all(per_sample$n > 0))
  expect_true(nrow(kiwims_db_query(res$db_path, "SELECT * FROM peaks")) > 0)

  # keep_raw_output = FALSE must leave the destination free of UniDec scratch.
  expect_length(
    list.files(res$result_dir, pattern = "_unidecfiles$"),
    0L
  )

  # A finished run must not leave WAL sidecars beside the result database.
  expect_false(file.exists(paste0(res$db_path, "-wal")))
  expect_false(file.exists(paste0(res$db_path, "-shm")))
})

test_that("a long sample name does not hit Windows' path length limit", {
  # A sample with this name reproduced the bug deterministically, every run:
  # UniDec derives every intermediate filename it writes from the name of the
  # file it opens, nested three levels deep under a scratch directory
  # (<tmp>/<name>/<name>_rawdata_unidecfiles/<name>_rawdata_conf.dat, etc.),
  # and this ~65-character name was enough to cross Windows' 260-character
  # MAX_PATH there.  UniDec enforces that limit itself and, having nothing to
  # report on, fails with no R or Python exception: the pipeline just found no
  # output, with no clue in the DB as to why.  The name is what matters, so a
  # reference sample is copied to it.
  source_dir <- waters_samples(1)
  work <- withr::local_tempdir("kiwims-longname")
  long_dir <- kiwims_long_name_fixture(source_dir, file.path(work, "input"))

  res <- kiwims_run_deconvolution(long_dir, work)
  expect_run_succeeded(res, long_dir)

  reason <- kiwims_db_query(res$db_path, "SELECT reason FROM status")$reason
  expect_true(is.na(reason[1]))
  expect_gt(nrow(kiwims_db_query(res$db_path, "SELECT * FROM peaks")), 0)
})

test_that("a broken sample is recorded as failed without taking the run down", {
  samples <- waters_samples(2)

  work <- withr::local_tempdir("kiwims-mixed")
  # An empty .raw directory reaches UniDec and fails there, which is the
  # closest stand-in for a truncated or still-copying acquisition.
  broken <- file.path(work, "definitely_broken.raw")
  dir.create(broken, recursive = TRUE)

  raw_dirs <- c(samples, broken)
  res <- kiwims_run_deconvolution(raw_dirs, work)

  expect_equal(res$status, 0L, info = paste("output in", res$stdout_path))

  # UniDec crashes transiently under load, so a sample is retried before it is
  # written off; the retry must actually be attempted.  Worker messages go to
  # the cluster log rather than the parent's stdout.
  cluster_log <- readLines(kiwims_cluster_log(), warn = FALSE)
  expect_true(any(grepl("Retrying definitely_broken", cluster_log, fixed = TRUE)))

  status <- kiwims_db_query(
    res$db_path,
    "SELECT sample, state FROM status ORDER BY sample"
  )
  expect_setequal(status$sample, kiwims_sample_bases(raw_dirs))
  expect_equal(status$state[status$sample == "definitely_broken"], "failed")
  expect_equal(
    sort(status$sample[status$state == "done"]),
    sort(kiwims_sample_bases(samples))
  )
  expect_true("completed" %in% kiwims_db_query(
    res$db_path,
    "SELECT name FROM sqlite_master WHERE type='table'"
  )$name)
})

test_that("a single-worker run takes the sequential path and still completes", {
  raw_dirs <- waters_samples(2)

  work <- withr::local_tempdir("kiwims-seq")
  res <- kiwims_run_deconvolution(
    raw_dirs,
    work,
    env = c(KIWIMS_DECON_WORKERS = "1")
  )

  expect_match(res$stdout, "Sequential processing started")
  expect_run_succeeded(res, raw_dirs)
})

test_that("re-running extends an existing database instead of discarding it", {
  all_dirs <- waters_samples(4)
  first_batch <- all_dirs[1:2]
  second_batch <- all_dirs[3:4]

  work <- withr::local_tempdir("kiwims-extend")
  first <- kiwims_run_deconvolution(first_batch, work)
  expect_run_succeeded(first, first_batch)

  first_peaks <- kiwims_db_query(
    first$db_path,
    "SELECT sample, COUNT(*) AS n FROM peaks GROUP BY sample ORDER BY sample"
  )

  second <- kiwims_run_deconvolution(second_batch, work)
  expect_equal(second$status, 0L)

  status <- kiwims_db_query(second$db_path, "SELECT * FROM status")
  expect_setequal(status$sample, kiwims_sample_bases(all_dirs))
  expect_equal(status$state, rep("done", nrow(status)))

  # The first batch's rows must survive untouched, and none may be duplicated.
  after <- kiwims_db_query(
    second$db_path,
    "SELECT sample, COUNT(*) AS n FROM peaks GROUP BY sample ORDER BY sample"
  )
  expect_equal(
    after[after$sample %in% first_peaks$sample, ],
    first_peaks,
    ignore_attr = TRUE
  )
  expect_setequal(after$sample, kiwims_sample_bases(all_dirs))
})
