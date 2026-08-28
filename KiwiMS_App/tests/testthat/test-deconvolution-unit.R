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
    generate_decon_rslt,
    plate_heatmap
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

test_that("plate_heatmap marks failed wells distinctly from empty ones", {
  hm <- plate_heatmap(
    data.frame(sample = "s1", well_id = "A1", value = 100),
    all_wells = c("A1", "A2", "A3"),
    failed_wells = data.frame(sample = "s2", well_id = "A2")
  )
  built <- plotly::plotly_build(hm)
  trace <- built$x$data[[1]]
  rows <- trace$y
  cols <- as.character(trace$x)

  at <- function(well, mat) {
    row <- sub("[0-9]+$", "", well)
    col <- sub("^[A-Za-z]+", "", well)
    mat[match(row, rows), match(col, cols)]
  }

  expect_equal(at("A1", trace$z), 1) # done
  expect_equal(at("A2", trace$z), 2) # failed
  expect_equal(at("A3", trace$z), 0) # empty
  expect_match(at("A1", trace$text), "Sample: s1")
  # A failed well must carry its sample name too, not just "Failed" -- that's
  # what lets a viewer identify which sample it was, and what the click
  # handler needs to jump the selection there.
  expect_match(at("A2", trace$text), "Sample: s2")
  expect_match(at("A2", trace$text), "Failed")
  expect_match(at("A3", trace$text), "Empty")

  # A well can't be both: a done result takes precedence in the unlikely case
  # a well is listed in both `data` and `failed_wells`.
  hm2 <- plate_heatmap(
    data.frame(sample = "s1", well_id = "A1", value = 100),
    all_wells = "A1",
    failed_wells = data.frame(sample = "s1", well_id = "A1")
  )
  trace2 <- plotly::plotly_build(hm2)$x$data[[1]]
  expect_equal(as.numeric(trace2$z)[1], 1)
})

test_that("plate_heatmap tolerates a malformed or empty failed_wells", {
  for (bad in list(NULL, data.frame(), "A1", data.frame(well_id = "A1"))) {
    hm <- plate_heatmap(
      data.frame(sample = "s1", well_id = "A1", value = 100),
      all_wells = c("A1", "A2"),
      failed_wells = bad
    )
    trace <- plotly::plotly_build(hm)$x$data[[1]]
    expect_true(all(trace$z %in% c(0, 1)))
  }
})

test_that("plate_heatmap tolerates no failed wells (default behaviour unchanged)", {
  hm <- plate_heatmap(
    data.frame(sample = "s1", well_id = "A1", value = 100),
    all_wells = c("A1", "A2")
  )
  trace <- plotly::plotly_build(hm)$x$data[[1]]
  expect_true(all(trace$z %in% c(0, 1)))
})

test_that("plate_heatmap accepts a columnless data.frame (nothing done yet)", {
  # This is exactly what reactVars$rslt_df is before any sample completes
  # (reset_progress() sets it to data.frame()) -- output$heatmap can reach
  # plate_heatmap() with it as soon as there's a failed sample to show, even
  # if nothing has succeeded yet.
  for (empty_data in list(data.frame(), NULL)) {
    hm <- plate_heatmap(
      empty_data,
      all_wells = c("A1", "A2"),
      failed_wells = data.frame(sample = "s1", well_id = "A1")
    )
    trace <- plotly::plotly_build(hm)$x$data[[1]]
    rows <- trace$y
    cols <- as.character(trace$x)
    z_a1 <- trace$z[match("A", rows), match("1", cols)]
    z_a2 <- trace$z[match("A", rows), match("2", cols)]
    expect_equal(z_a1, 2) # failed
    expect_equal(z_a2, 0) # empty
  }
})
