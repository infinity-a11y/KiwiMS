# Unit tests for the deconvolution run scaffolding.  These never start Python
# or a worker pool, so they run everywhere in a few seconds.

box::use(
  app/logic/deconvolution_functions[
    cleanup_wal,
    db_with_retry,
    decon_failed_samples,
    decon_failure_detail,
    decon_is_complete,
    decon_mark_unprocessed,
    decon_progress_count,
    decon_samples_with_state,
    decon_worker_count,
    deconvolute,
    add_metrics_table,
    generate_decon_rslt,
    plate_heatmap,
    plate_highlight,
    plate_layout,
    plate_sample_at,
    read_decon_metrics,
    read_decon_peak_counts
  ],
)

# make_status_db(): Minimal run database with the tables the pipeline writes ----
make_status_db <- function(path, status = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (is.null(status)) {
    status <- data.frame(
      sample = character(0),
      state = character(0),
      reason = character(0),
      error_msg = character(0),
      timestamp = character(0),
      stringsAsFactors = FALSE
    )
  }
  DBI::dbWriteTable(con, "status", status)
  path
}

status_row <- function(
  sample,
  state,
  reason = NA_character_,
  error_msg = NA_character_
) {
  data.frame(
    sample = sample,
    state = state,
    reason = reason,
    error_msg = error_msg,
    timestamp = "2026-01-01 00:00:00",
    stringsAsFactors = FALSE
  )
}

test_that("worker count never exceeds the number of samples", {
  withr::local_envvar(KIWIMS_DECON_WORKERS = NA)

  expect_equal(decon_worker_count(3, 8), 3L)
  expect_equal(decon_worker_count(100, 4), 4L)
  expect_equal(decon_worker_count(1, 16), 1L)
})

test_that("worker count stays at least one for degenerate inputs", {
  withr::local_envvar(KIWIMS_DECON_WORKERS = NA)

  expect_equal(decon_worker_count(0, 8), 1L)
  expect_equal(decon_worker_count(5, 0), 1L)
  expect_equal(decon_worker_count(5, -4), 1L)
  expect_equal(decon_worker_count(5, NA), 1L)
})

test_that("worker count falls back to the machine size and honours the override", {
  withr::local_envvar(KIWIMS_DECON_WORKERS = NA)
  expected <- max(1L, min(parallel::detectCores() - 2L, 1000L))
  expect_equal(decon_worker_count(1000, NULL), as.integer(expected))

  withr::local_envvar(KIWIMS_DECON_WORKERS = "2")
  expect_equal(decon_worker_count(1000, NULL), 2L)
  expect_equal(decon_worker_count(1000, 16), 2L)

  # Junk in the override must not take the run down.
  withr::local_envvar(KIWIMS_DECON_WORKERS = "not-a-number")
  expect_equal(decon_worker_count(6, 4), 4L)
})

test_that("unprocessed samples are marked failed without touching known rows", {
  db <- make_status_db(
    withr::local_tempfile(fileext = ".db"),
    rbind(
      status_row("a", "done"),
      status_row("b", "failed", "error")
    )
  )

  n <- decon_mark_unprocessed(db, c("a", "b", "c", "d"))
  expect_equal(n, 2L)

  status <- kiwims_db_query(db, "SELECT * FROM status ORDER BY sample")
  expect_equal(status$sample, c("a", "b", "c", "d"))
  expect_equal(status$state, c("done", "failed", "failed", "failed"))
  expect_equal(status$reason, c(NA, "error", "not_processed", "not_processed"))

  # Re-running must be a no-op rather than duplicating rows.
  expect_equal(decon_mark_unprocessed(db, c("a", "b", "c", "d")), 0L)
  expect_equal(nrow(kiwims_db_query(db, "SELECT * FROM status")), 4L)
})

test_that("marking unprocessed samples tolerates a missing or empty database", {
  expect_equal(decon_mark_unprocessed(tempfile(fileext = ".db"), "a"), 0L)

  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbDisconnect(con)
  expect_equal(decon_mark_unprocessed(db, "a"), 0L)

  expect_equal(decon_mark_unprocessed(make_status_db(db), character(0)), 0L)
})

test_that("progress helpers report per-run counts and completion", {
  db <- make_status_db(
    withr::local_tempfile(fileext = ".db"),
    rbind(
      status_row("old_run", "done"),
      status_row("a", "done"),
      status_row("b", "failed", "error")
    )
  )

  expect_equal(decon_progress_count(db), 2L)
  expect_equal(decon_progress_count(db, c("a", "b")), 1L)
  expect_equal(decon_failed_samples(db), "b")
  expect_false(decon_is_complete(db))

  generate_decon_rslt(log = "log line", output = "output line", db_path = db)
  expect_true(decon_is_complete(db))
  expect_equal(kiwims_db_query(db, "SELECT line FROM session")$line, "log line")
})

test_that("decon_samples_with_state scopes to the requested samples and state", {
  db <- make_status_db(
    withr::local_tempfile(fileext = ".db"),
    rbind(
      status_row("old_run", "failed", "error"), # not in sample_bases: ignored
      status_row("a", "done"),
      status_row("b", "failed", "error"),
      status_row("c", "failed", "no_output_dir")
    )
  )

  expect_setequal(
    decon_samples_with_state(db, c("a", "b", "c"), "failed"),
    c("b", "c")
  )
  expect_equal(decon_samples_with_state(db, c("a", "b", "c"), "done"), "a")
  expect_equal(
    decon_samples_with_state(db, c("a", "b", "c"), "not_processed"),
    character(0)
  )
  expect_equal(decon_samples_with_state(db, character(0), "failed"), character(0))
  expect_equal(
    decon_samples_with_state(tempfile(fileext = ".db"), "a", "failed"),
    character(0)
  )
})

test_that("decon_failure_detail turns a failure code into a readable cause", {
  db <- make_status_db(
    withr::local_tempfile(fileext = ".db"),
    rbind(
      status_row("a", "done"),
      status_row("b", "failed", "no_output_dir"),
      status_row("c", "failed", "error", "boom: something broke\nsecond line"),
      status_row("d", "failed", "not_processed"),
      status_row("e", "failed", NA_character_),
      status_row("f", "failed", "path_too_long", "Working path is 287 characters, over Windows' 260-character limit: X")
    )
  )

  expect_null(decon_failure_detail(db, "a")) # not failed
  expect_null(decon_failure_detail(db, "does_not_exist"))

  no_output <- decon_failure_detail(db, "b")
  expect_match(no_output$cause, "no output")
  expect_null(no_output$detail)

  with_detail <- decon_failure_detail(db, "c")
  expect_match(with_detail$cause, "error")
  expect_equal(with_detail$detail, "boom: something broke\nsecond line")

  not_processed <- decon_failure_detail(db, "d")
  expect_match(not_processed$cause, "never picked up")

  # A NULL/NA reason must still produce something rather than erroring.
  unknown <- decon_failure_detail(db, "e")
  expect_true(nzchar(unknown$cause))

  path_too_long <- decon_failure_detail(db, "f")
  expect_match(path_too_long$cause, "260-character")
  expect_match(path_too_long$detail, "287 characters")
})

test_that("a long failure detail is capped rather than laid out in full", {
  db <- make_status_db(
    withr::local_tempfile(fileext = ".db"),
    status_row("a", "failed", "error", strrep("x", 5000))
  )
  info <- decon_failure_detail(db, "a")
  expect_lt(nchar(info$detail), 5000)
  expect_match(info$detail, "truncated")
})

test_that("a missing Python interpreter fails fast with a clear message", {
  withr::local_envvar(RETICULATE_PYTHON = "")
  expect_error(
    deconvolute(
      raw_dirs = "nowhere.raw",
      result_dir = tempdir(),
      db_path = tempfile(fileext = ".db")
    ),
    "RETICULATE_PYTHON"
  )

  withr::local_envvar(
    RETICULATE_PYTHON = file.path(tempdir(), "definitely-not-here.exe")
  )
  expect_error(
    deconvolute(
      raw_dirs = "nowhere.raw",
      result_dir = tempdir(),
      db_path = tempfile(fileext = ".db")
    ),
    "RETICULATE_PYTHON"
  )
})

test_that("a retried transaction re-runs its body instead of committing nothing", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE t (v INTEGER)")

  # Happy path: the body runs once and its writes are committed.
  runs <- 0L
  db_with_retry(con, {
    runs <- runs + 1L
    DBI::dbExecute(con, "INSERT INTO t(v) VALUES (1)")
  })
  expect_equal(runs, 1L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM t")$n, 1L)

  # A lock error must send the whole transaction round again, body included.
  # A promise-based body would be evaluated only once, so the retry would
  # commit an empty transaction and drop the write.
  runs <- 0L
  db_with_retry(
    con,
    {
      runs <- runs + 1L
      if (runs == 1L) {
        stop("database is locked")
      }
      DBI::dbExecute(con, "INSERT INTO t(v) VALUES (2)")
    },
    max_wait_s = 30
  )
  expect_equal(runs, 2L)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT v FROM t ORDER BY v")$v,
    c(1L, 2L)
  )

  # Anything that is not a lock/busy condition must surface immediately.
  expect_error(
    db_with_retry(con, stop("constraint violation"), max_wait_s = 1),
    "constraint violation"
  )
})

test_that("WAL cleanup leaves the database readable and the sidecars gone", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL")
  DBI::dbWriteTable(con, "peaks", data.frame(sample = "a", mass = 1))
  DBI::dbDisconnect(con)

  cleanup_wal(db)

  expect_false(file.exists(paste0(db, "-wal")))
  expect_false(file.exists(paste0(db, "-shm")))
  expect_equal(nrow(kiwims_db_query(db, "SELECT * FROM peaks")), 1L)
})

test_that("plate_layout uses the config wells when every sample has one", {
  plate <- plate_layout(c("s1", "s2", "s3"), c("B2", "c03", "B4"))

  expect_true(plate$by_well)
  expect_equal(plate$tiles$well_id, c("B2", "C3", "B4"))
  expect_equal(plate$tiles$row, c(2L, 3L, 2L))
  expect_equal(plate$tiles$col, c(2L, 3L, 4L))
  # Cropped to the rectangle the wells span
  expect_equal(plate$rows, 2:3)
  expect_equal(plate$cols, 2:4)
})

test_that("plate_layout falls back to queue order without usable wells", {
  samples <- paste0("s", 1:14)
  for (wells in list(
    NULL,
    rep(NA_character_, 14),
    c(rep("A1", 13), "A2"), # duplicate wells
    c(paste0("A", 1:13), "Z9"), # a well off the plate
    c(paste0("A", 1:13), NA) # a sample without a well
  )) {
    plate <- plate_layout(samples, wells)
    expect_false(plate$by_well)
    # Left to right, then the next row, on 12 columns
    expect_equal(plate$tiles$row, c(rep(1L, 12), 2L, 2L))
    expect_equal(plate$tiles$col, c(1:12, 1:2))
    expect_equal(plate$rows, 1:2)
    expect_equal(plate$cols, 1:12)
  }

  # 24 columns once the samples no longer fit a 96-well plate
  big <- plate_layout(paste0("s", 1:100))
  expect_equal(big$cols, 1:24)
  expect_equal(big$rows, 1:5)

  # No samples yet still makes a (blank) plate
  empty <- plate_layout(character(0))
  expect_equal(nrow(empty$tiles), 0L)
  expect_equal(empty$rows, 1L)
})

test_that("plate_sample_at and plate_highlight address tiles by position", {
  plate <- plate_layout(c("s1", "s2"), c("B2", "B3"))

  expect_equal(plate_sample_at(plate, 3, 2), "s2")
  expect_equal(plate_sample_at(plate, 3.2, 1.9), "s2")
  expect_length(plate_sample_at(plate, 4, 2), 0)
  expect_length(plate_sample_at(NULL, 3, 2), 0)

  frame <- plate_highlight(plate, "s2")
  expect_length(frame, 1)
  expect_equal(c(frame[[1]]$x0, frame[[1]]$x1), c(2.5, 3.5))
  expect_equal(c(frame[[1]]$y0, frame[[1]]$y1), c(1.5, 2.5))
  # Nothing to frame clears an earlier frame
  expect_equal(plate_highlight(plate, NULL), list())
  expect_equal(plate_highlight(plate, "__show_all__"), list())
})

test_that("plate_heatmap colours done tiles by peak count and crosses failures", {
  plate <- plate_layout(c("s1", "s2", "s3", "s4"), c("A1", "A2", "A3", "A4"))
  hm <- plate_heatmap(
    plate,
    peak_counts = data.frame(sample = c("s1", "s2"), n_peaks = c(3L, 0L)),
    failed = "s3"
  )
  built <- plotly::plotly_build(hm)
  traces <- built$x$data
  peaks <- traces[[2]]
  z <- matrix(unlist(peaks$z), nrow = 1)

  expect_equal(as.numeric(z[1, 1:2]), c(3, 0)) # done, 0 peaks still coloured
  expect_true(all(is.na(z[1, 3:4]))) # failed and pending stay uncoloured
  text <- unlist(peaks$text)
  expect_match(text[1], "Sample: s1.*Peaks: 3")
  expect_match(text[3], "Sample: s3.*Failed")
  expect_match(text[4], "Sample: s4.*Pending")

  # One cross, on the failed tile
  crosses <- Filter(function(t) identical(t$type, "scatter"), traces)
  expect_length(crosses, 1)
  expect_equal(as.numeric(crosses[[1]]$x), 3)
  expect_equal(as.numeric(crosses[[1]]$y), 1)
  expect_match(crosses[[1]]$marker$symbol, "^x")
})

test_that("plate_heatmap draws a plate before anything is done", {
  plate <- plate_layout(c("s1", "s2"))
  for (counts in list(NULL, data.frame())) {
    built <- plotly::plotly_build(plate_heatmap(plate, peak_counts = counts))
    z <- unlist(built$x$data[[2]]$z)
    expect_true(all(is.na(z)))
    # No failures, no cross trace
    expect_false(any(vapply(
      built$x$data,
      function(t) identical(t$type, "scatter"),
      logical(1)
    )))
  }
})

test_that("read_decon_peak_counts counts peaks and reports 0 for none", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbWriteTable(
    con,
    "peaks",
    data.frame(
      sample = c("s1", "s1", "s1", "s2"),
      mass = c(1, 2, 3, 4),
      intensity = c(10, 20, 30, 40)
    )
  )
  DBI::dbDisconnect(con)

  counts <- read_decon_peak_counts(db, c("s2", "s1", "s3"))
  expect_equal(counts$sample, c("s2", "s1", "s3"))
  expect_equal(counts$n_peaks, c(1L, 3L, 0L))
  expect_equal(nrow(read_decon_peak_counts(db, character(0))), 0L)
})

test_that("read_decon_metrics labels the error table and add_metrics_table shows it", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbWriteTable(
    con,
    "error",
    data.frame(
      sample = "s1",
      Key = c("error", "time", "iterations", "uniscore"),
      Value = c(123.4567891, 2.5, 100, 0.8765)
    )
  )
  DBI::dbDisconnect(con)

  metrics <- read_decon_metrics(db, "s1")
  expect_equal(
    metrics$Parameter,
    c(
      "Fitting Error",
      "Computation Time [s]",
      "Iteration Count",
      "UniScore (Quality)"
    )
  )
  expect_equal(metrics$Value[1], 123.456789)
  expect_equal(nrow(read_decon_metrics(db, "missing")), 0L)

  base <- plotly::plot_ly(
    data.frame(mass = 1:3, intensity = c(1, 5, 2)),
    x = ~mass,
    y = ~intensity,
    type = "scatter",
    mode = "lines"
  )
  built <- plotly::plotly_build(add_metrics_table(base, metrics))
  table <- Filter(function(t) identical(t$type, "table"), built$x$data)
  expect_length(table, 1)
  expect_equal(unlist(table[[1]]$cells$values[[1]]), metrics$Parameter)
  # The spectrum makes room for the table instead of running under it
  expect_lt(built$x$layout$xaxis$domain[2], table[[1]]$domain$x[1])

  # No metrics, no table
  expect_identical(add_metrics_table(base, metrics[0, ]), base)
})
