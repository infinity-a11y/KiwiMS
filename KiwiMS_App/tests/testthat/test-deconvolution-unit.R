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
    decon_omp_threads,
    decon_progress_count,
    decon_samples_with_state,
    decon_worker_count,
    deconvolute,
    add_side_tables,
    bin_spectrum,
    decon_param_rows,
    downsample_spectrum,
    generate_decon_rslt,
    plate_heatmap,
    process_plot_data,
    plate_highlight,
    plate_layout,
    plate_format,
    plate_sample_at,
    parse_wells,
    read_decon_metrics,
    read_decon_peak_counts,
    spectrum_plot,
    summarise_decon_metrics
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
  # Half the physical cores, at most four: UniDec is memory-bound beyond that.
  expected <- max(1L, min(4L, parallel::detectCores(logical = FALSE) %/% 2L))
  expect_equal(decon_worker_count(1000, NULL), as.integer(expected))
  expect_lte(decon_worker_count(1000, NULL), 4L)

  withr::local_envvar(KIWIMS_DECON_WORKERS = "2")
  expect_equal(decon_worker_count(1000, NULL), 2L)
  expect_equal(decon_worker_count(1000, 16), 2L)

  # Junk in the override must not take the run down.
  withr::local_envvar(KIWIMS_DECON_WORKERS = "not-a-number")
  expect_equal(decon_worker_count(6, 4), 4L)
})

test_that("each worker's UniDec gets an even share of the threads", {
  logical <- parallel::detectCores()
  expect_equal(decon_omp_threads(1), as.integer(logical))
  expect_equal(decon_omp_threads(4), max(1L, as.integer(logical %/% 4L)))
  # More workers than threads still leaves every UniDec one thread.
  expect_equal(decon_omp_threads(logical * 2L), 1L)
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

test_that("parse_wells normalises well IDs and drops anything off the plate", {
  pos <- parse_wells(c("A1", "a01", " B012 ", "P24", "Q1", "A25", "X", NA))
  expect_equal(pos$well_id[1:4], c("A1", "A1", "B12", "P24"))
  expect_equal(pos$row, c(1L, 1L, 2L, 16L, NA, NA, NA, NA))
  expect_equal(pos$col, c(1L, 1L, 12L, 24L, NA, NA, NA, NA))
})

test_that("plate_format picks 96 or 384 wells, never a size in between", {
  expect_equal(plate_format(c("A1", "A2"))$wells, 96L)
  expect_equal(plate_format("H12")$wells, 96L)
  expect_equal(plate_format(c("A1", "I1"))$wells, 384L)
  expect_equal(plate_format(c("A1", "A13"))$wells, 384L)
  # Off the plate does not count
  expect_equal(plate_format(c("A1", "Z99"))$wells, 96L)
  big <- plate_format("P24")
  expect_equal(big$rows, 1:16)
  expect_equal(big$cols, 1:24)
  small <- plate_format(character(0))
  expect_equal(small$rows, 1:8)
  expect_equal(small$cols, 1:12)
  # Without wells, the number of samples decides
  expect_equal(plate_format(n_samples = 96)$wells, 96L)
  expect_equal(plate_format(n_samples = 97)$wells, 384L)
})

test_that("plate_layout puts samples on their config wells, on the whole plate", {
  plate <- plate_layout(c("s1", "s2", "s3"), c("B2", "c03", "B4"))

  expect_true(plate$by_well)
  expect_equal(plate$tiles$well_id, c("B2", "C3", "B4"))
  expect_equal(plate$tiles$row, c(2L, 3L, 2L))
  expect_equal(plate$tiles$col, c(2L, 3L, 4L))
  # The whole 96-well plate, not just the wells in use
  expect_equal(plate$rows, 1:8)
  expect_equal(plate$cols, 1:12)

  # Any well beyond H12 makes it a 384-well plate
  for (wells in list(c("A1", "I1"), c("A1", "A13"))) {
    big <- plate_layout(c("s1", "s2"), wells)
    expect_true(big$by_well)
    expect_equal(big$rows, 1:16)
    expect_equal(big$cols, 1:24)
  }
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
    # Left to right, then the next row, on a 96-well plate
    expect_equal(plate$tiles$row, c(rep(1L, 12), 2L, 2L))
    expect_equal(plate$tiles$col, c(1:12, 1:2))
    expect_equal(plate$tiles$well_id[13], "B1")
    expect_equal(plate$rows, 1:8)
    expect_equal(plate$cols, 1:12)
  }

  # A 384-well plate once the samples no longer fit 96 wells
  exactly_96 <- plate_layout(paste0("s", 1:96))
  expect_equal(exactly_96$cols, 1:12)
  big <- plate_layout(paste0("s", 1:97))
  expect_equal(big$rows, 1:16)
  expect_equal(big$cols, 1:24)
  expect_equal(big$tiles$well_id[25], "B1")

  # No samples yet still makes a (blank) plate
  empty <- plate_layout(character(0))
  expect_equal(nrow(empty$tiles), 0L)
  expect_equal(empty$rows, 1:8)
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
  as_rows <- function(m) if (is.list(m)) do.call(rbind, m) else as.matrix(m)
  z <- as_rows(peaks$z)
  text <- as_rows(peaks$text)

  # The whole 96-well plate is drawn
  expect_equal(dim(z), c(8L, 12L))
  expect_equal(as.numeric(z[1, 1:2]), c(3, 0)) # done, 0 peaks still coloured
  expect_true(all(is.na(z[1, 3:4]))) # failed and pending stay uncoloured
  expect_match(text[1, 1], "Well: A1.*Sample: s1.*Peaks: 3")
  expect_match(text[1, 3], "Sample: s3.*Failed")
  expect_match(text[1, 4], "Sample: s4.*Pending")
  expect_match(text[8, 12], "Well: H12.*Empty")

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
  ) |>
    plotly::layout(annotations = list(list(text = "peak label", x = 2, y = 5)))
  built <- plotly::plotly_build(
    add_side_tables(base, list(list(rows = metrics)))
  )
  texts <- vapply(built$x$layout$annotations, function(a) a$text, "")
  # The spectrum's own annotations stay, the table's cells are added
  expect_true("peak label" %in% texts)
  expect_true(all(metrics$Parameter %in% texts))
  expect_true("123.46" %in% texts) # always two decimals
  # No header row: nothing but the statistics and their values
  expect_length(texts, 1 + 2 * nrow(metrics))
  # Inner borders only: lines between the rows and one between the columns
  expect_length(built$x$layout$shapes, nrow(metrics))
  # The table sits in the right margin, widened to hold it, so the spectrum
  # never runs under it
  cells <- built$x$layout$annotations[-1]
  expect_true(all(vapply(cells, function(a) a$x == 1 && a$xshift > 0, NA)))
  widest <- max(vapply(cells, function(a) a$xshift, 0))
  expect_gte(built$x$layout$margin$r, widest)

  # A second block stacks below the first, each with its caption
  params <- decon_param_rows(data.frame(startz = 1, endz = 100, peaknorm = 2))
  two <- plotly::plotly_build(add_side_tables(
    base,
    list(
      list(rows = metrics, caption = "Metrics"),
      list(rows = params, caption = "Parameters")
    )
  ))
  ann <- two$x$layout$annotations
  depth <- function(text) -ann[[match(text, vapply(ann, `[[`, "", "text"))]]$yshift
  expect_lt(depth(metrics$Parameter[nrow(metrics)]), depth("<i>Parameters</i>"))
  expect_lt(depth("<i>Parameters</i>"), depth("Charge State [z]"))

  # No rows, no table
  expect_identical(add_side_tables(base, list(list(rows = metrics[0, ]))), base)
  expect_identical(add_side_tables(base, list()), base)
})

test_that("decon_param_rows labels the run's settings like the start form", {
  params <- data.frame(
    startz = 1,
    endz = 100,
    minmz = 710,
    maxmz = 1100,
    masslb = 10000,
    massub = 60000,
    massbins = 0.5,
    peakthresh = 0.05,
    peakwindow = 40,
    peaknorm = 2,
    time_start = NA_real_,
    time_end = 1.5
  )
  rows <- decon_param_rows(params)
  value <- function(name) rows$Value[rows$Parameter == name]

  expect_equal(nrow(rows), 8L)
  expect_match(value("Charge State [z]"), "^1 . 100$")
  expect_match(value("Mass Range [Da]"), "^10000 . 60000$")
  expect_equal(value("Sample Rate [Da]"), "0.5")
  expect_equal(value("Peak Normalization"), "Normalization to Sum")
  expect_equal(value("Retention Time [min]"), "up to 1.5")

  params$time_end <- NA_real_
  expect_equal(
    decon_param_rows(params)$Value[4],
    "Whole run"
  )
  expect_equal(nrow(decon_param_rows(NULL)), 0L)
})

test_that("spectrum_plot draws at display resolution: 1 Da, or 0.1 m/z raw", {
  points <- function(plot) {
    length(plotly::plotly_build(plot)$x$data[[1]]$x)
  }

  # Deconvoluted, sampled every 0.5 Da: one point per Da
  mass <- data.frame(mass = seq(1000, 1999.5, by = 0.5), intensity = 1)
  mass$intensity[701] <- 100
  deconv <- spectrum_plot(plot_data = list(mass = mass, highlight_peaks = NULL))
  expect_equal(points(deconv), 1000L)

  # Raw, read in 0.01 m/z bins: one point per 0.1 m/z, apex kept
  raw <- data.frame(mass = seq(500, 599.99, by = 0.01), intensity = 1)
  raw$intensity[5003] <- 100
  raw_plot <- spectrum_plot(plot_data = list(mass = raw), raw = TRUE)
  built <- plotly::plotly_build(raw_plot)$x$data[[1]]
  expect_equal(length(built$x), 1000L)
  expect_true(raw$mass[5003] %in% built$x)
})

test_that("spectrum_plot can build its peak markers hidden", {
  mass <- data.frame(mass = 1000 + 0:200, intensity = c(0:100, 99:0))
  plot_data <- list(mass = mass, highlight_peaks = mass[101, ])
  markers <- function(show) {
    built <- plotly::plotly_build(
      spectrum_plot(plot_data = plot_data, show_peak_symbols = show)
    )
    Filter(function(t) identical(t$mode, "markers"), built$x$data)
  }

  expect_length(markers(TRUE), 1)
  # Still built, so the figure on screen can show them again
  hidden <- markers(FALSE)
  expect_length(hidden, 1)
  expect_false(isTRUE(hidden[[1]]$visible))
})

test_that("summarise_decon_metrics gives mean and SD per statistic", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbWriteTable(
    con,
    "error",
    data.frame(
      sample = rep(c("s1", "s2", "s3"), each = 2),
      Key = rep(c("error", "time"), 3),
      Value = c(10, 1, 20, 1, 99, 5)
    )
  )
  DBI::dbDisconnect(con)

  summary <- summarise_decon_metrics(db, c("s1", "s2"))
  expect_equal(summary$Parameter, c("Fitting Error", "Computation Time [s]"))
  expect_equal(
    summary$Value,
    c("15.00 \u00b1 7.07", "1.00 \u00b1 0.00")
  )
  expect_equal(nrow(summarise_decon_metrics(db, character(0))), 0L)
  expect_equal(nrow(summarise_decon_metrics(db, "missing")), 0L)
})

test_that("bin_spectrum keeps the apex of every 1 Da bin", {
  df <- data.frame(
    mass = c(100.1, 100.4, 100.9, 101.2, 101.7, 102.5),
    intensity = c(1, 7, 3, 2, 9, 4)
  )
  binned <- bin_spectrum(df)
  expect_equal(binned$mass, c(100.4, 101.7, 102.5))
  expect_equal(binned$intensity, c(7, 9, 4))
  expect_identical(bin_spectrum(df[1, ]), df[1, ])
  expect_null(bin_spectrum(NULL))
})

test_that("downsample_spectrum caps the points and keeps every apex", {
  set.seed(1)
  df <- data.frame(mass = seq_len(10000), intensity = stats::runif(10000))
  df$intensity[c(1234, 8765)] <- c(50, 80)
  small <- downsample_spectrum(df, max_points = 500)
  expect_lte(nrow(small), 502)
  expect_true(all(c(1L, 1234L, 8765L, 10000L) %in% small$mass))
  expect_false(is.unsorted(small$mass))
  expect_identical(downsample_spectrum(df[1:10, ], max_points = 500), df[1:10, ])
})

test_that("bin_spectrum and downsample_spectrum keep every detected peak", {
  # A small peak (1002, 40 %) two Da from a tall one (1000, 100 %)
  df <- data.frame(mass = seq(900, 1100, by = 0.5), intensity = 1)
  df$intensity[df$mass == 1000] <- 100
  df$intensity[df$mass == 1002] <- 40
  peaks <- c(1000, 1002)

  # Squeezed to 20 points, the small apex shares a bucket with the tall one
  lost <- downsample_spectrum(df, max_points = 20)
  expect_false(1002 %in% lost$mass)
  kept <- downsample_spectrum(df, max_points = 20, peaks = peaks)
  expect_true(all(peaks %in% kept$mass))
  expect_equal(kept$intensity[kept$mass == 1002], 40)

  # In 5 Da bins they share a bin too
  expect_false(1002 %in% bin_spectrum(df, width = 5)$mass)
  binned <- bin_spectrum(df, width = 5, peaks = peaks)
  expect_true(all(peaks %in% binned$mass))
  expect_false(is.unsorted(binned$mass))
})

test_that("a peak's hover names every interpretation, the preferred one marked", {
  hit <- function(peak, cmp, mw, stoich, preferred) {
    data.frame(
      Protein = "P", `Mw Protein [Da]` = 1000, `Measured Mw Protein [Da]` = 1000,
      `Peak [Da]` = peak, Compound = cmp, `Compound Mw [Da]` = mw,
      `Binding Stoichiometry` = stoich, Preferred = preferred,
      check.names = FALSE
    )
  }
  sample <- list(
    mass = data.frame(mass = seq(900, 1400, by = 1), intensity = 1),
    peaks = data.frame(mass = c(1000, 1100, 1200), intensity = c(1, 1, 1)),
    hits = rbind(
      hit(1100, "A", 100, 1, TRUE),
      hit(1100, "A", 99, 1, FALSE),
      hit(1100, "B", 50, 2, FALSE),
      hit(1200, "A", 200, 1, TRUE)
    )
  )
  peaks <- process_plot_data(sample)$highlight_peaks
  label <- function(m) peaks$mass_stoich_label[peaks$mass == m]

  expect_equal(
    label(1100),
    "[100] x1 (preferred)\nAlternatives: [99] x1, [50] x2 (B)"
  )
  # One interpretation: no marking needed
  expect_equal(label(1200), "[200] x1")
})
