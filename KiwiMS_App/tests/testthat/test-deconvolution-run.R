# End-to-end tests: they launch the real deconvolution subprocess exactly the
# way app/view/deconvolution_main.R does, against real Waters .raw samples.
#
# They skip unless both a UniDec-capable Python interpreter and the .raw corpus
# are present.  Point the suite elsewhere with KIWIMS_TEST_DATA; skip them
# entirely with KIWIMS_SKIP_DECON_RUN=1.
#
# The sweep over every corpus in the data root is slow (each sample is tens of
# seconds of UniDec time), so it only runs with KIWIMS_TEST_DECON_ALL=1.

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
  if (!file.exists(kiwims_rscript())) {
    skip("No Rscript available for the deconvolution subprocess")
  }
}

# first_corpus_with(): First test-data directory holding at least n samples ----
first_corpus_with <- function(n) {
  roots <- sort(list.dirs(
    kiwims_test_data_root(),
    recursive = FALSE,
    full.names = TRUE
  ))
  roots <- roots[!grepl("\\.raw$", roots, ignore.case = TRUE)]
  for (r in roots) {
    if (length(kiwims_raw_dirs(r)) >= n) {
      return(r)
    }
  }
  NULL
}

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
  skip_unless_runnable()

  corpus <- first_corpus_with(4)
  skip_if(is.null(corpus), "No corpus with at least 4 samples")
  raw_dirs <- kiwims_raw_dirs(corpus, 4)

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

test_that("a broken sample is recorded as failed without taking the run down", {
  skip_unless_runnable()

  corpus <- first_corpus_with(2)
  skip_if(is.null(corpus), "No corpus with at least 2 samples")

  work <- withr::local_tempdir("kiwims-mixed")
  # An empty .raw directory reaches UniDec and fails there, which is the
  # closest stand-in for a truncated or still-copying acquisition.
  broken <- file.path(work, "definitely_broken.raw")
  dir.create(broken, recursive = TRUE)

  raw_dirs <- c(kiwims_raw_dirs(corpus, 2), broken)
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
    sort(kiwims_sample_bases(kiwims_raw_dirs(corpus, 2)))
  )
  expect_true("completed" %in% kiwims_db_query(
    res$db_path,
    "SELECT name FROM sqlite_master WHERE type='table'"
  )$name)
})

test_that("a single-worker run takes the sequential path and still completes", {
  skip_unless_runnable()

  corpus <- first_corpus_with(2)
  skip_if(is.null(corpus), "No corpus with at least 2 samples")
  raw_dirs <- kiwims_raw_dirs(corpus, 2)

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
  skip_unless_runnable()

  corpus <- first_corpus_with(4)
  skip_if(is.null(corpus), "No corpus with at least 4 samples")
  all_dirs <- kiwims_raw_dirs(corpus, 4)
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

test_that("every test-data corpus deconvolutes", {
  skip_unless_runnable()
  if (!nzchar(Sys.getenv("KIWIMS_TEST_DECON_ALL"))) {
    skip("Set KIWIMS_TEST_DECON_ALL=1 to sweep every corpus (slow)")
  }

  corpora <- sort(list.dirs(
    kiwims_test_data_root(),
    recursive = FALSE,
    full.names = TRUE
  ))
  corpora <- corpora[!grepl("\\.raw$", corpora, ignore.case = TRUE)]
  corpora <- Filter(function(d) length(kiwims_raw_dirs(d)) > 0, corpora)
  skip_if(length(corpora) == 0, "No corpora with .raw samples")

  n_per <- as.integer(Sys.getenv("KIWIMS_TEST_DECON_N", "2"))

  for (corpus in corpora) {
    raw_dirs <- kiwims_raw_dirs(corpus, n_per)
    work <- withr::local_tempdir("kiwims-sweep")
    res <- kiwims_run_deconvolution(raw_dirs, work)

    # expect_setequal() takes no `info`, so report the corpus through expect_true
    # rather than losing which one failed.
    expect_true(
      res$status == 0L,
      info = paste(basename(corpus), "->", res$stdout_path)
    )
    status <- kiwims_db_query(res$db_path, "SELECT * FROM status")
    expect_true(
      setequal(status$sample, kiwims_sample_bases(raw_dirs)),
      info = basename(corpus)
    )
    expect_true(
      all(status$state == "done"),
      info = paste(
        basename(corpus),
        "->",
        paste(status$sample[status$state != "done"], collapse = ", ")
      )
    )
  }
})
