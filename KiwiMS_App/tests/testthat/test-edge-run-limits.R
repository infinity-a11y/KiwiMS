# Edge-case kit, run limits (tests/edge_cases/run_limits/README.md): the cap
# on samples per run, at and over the limit. The caps on replicate series and
# replicates per condition are tested in test-edge-replicates.R.

box::use(
  app/logic/conversion_constants[run_limits],
)

L <- function(name) kit_path("run_limits", name)

test_that("the run limit files are written and load like uploads", {
  expect_kit_loads("run_limits")
})

test_that("the caps are one 384-well plate and the four marker fills", {
  expect_equal(run_limits, list(max_series = 4, max_replicates = 4, max_samples = 384))
})

test_that("LM1: a config of 384 samples is accepted, 385 are refused", {
  expect_length(kit_config_issues(L("config_384_samples")), 0)
  expect_equal(
    kit_config_issues(L("config_385_samples")),
    "At most 384 samples per config (385 rows)."
  )
})

test_that("LM1: a Samples table of 384 samples passes, 385 are refused", {
  pt <- kit_load("baseline", "proteins_baseline")
  ct <- kit_load("baseline", "compounds_baseline")

  d <- kit_declare(pt, ct, kit_read_config(L("config_384_samples")))
  expect_equal(d$status, "PASS")

  over <- kit_read_config(L("config_385_samples"))
  d <- kit_declare(pt, ct, over)
  expect_equal(d$status, "BLOCKED")
  expect_equal(d$message, "At most 384 samples per run (385 present)")
  expect_match(d$note, "^Split the samples over several deconvolution databases")
  # The cap holds with kinact/KI off as well
  expect_equal(kit_declare(pt, ct, over, kinetics = FALSE)$message, d$message)
})

test_that("LM1: the sample cap is checked before anything else", {
  # Even with no proteins or compounds declared, the cap is what is reported
  st <- kit_sample_table(kit_read_config(L("config_385_samples")))
  expect_equal(
    as.character(check_sample_table(st, NULL, NULL)),
    "At most 384 samples per run (385 present)"
  )
})

# ---- The cap at the start of a deconvolution ---------------------------------

box::use(
  app/logic/deconvolution_functions[decon_planned_samples, decon_sample_cap_message],
)

# status_db(): An analysis database holding the given samples as done ----
status_db <- function(done, failed = character(0)) {
  db <- withr::local_tempfile(fileext = ".db", .local_envir = parent.frame())
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbWriteTable(con, "status", data.frame(
    sample = c(done, failed),
    state = rep(c("done", "failed"), c(length(done), length(failed)))
  ))
  DBI::dbDisconnect(con)
  db
}

test_that("a deconvolution of more than 384 inputs is refused", {
  inputs <- sprintf("S%03d.raw", 1:385)
  at_cap <- decon_planned_samples(inputs[1:384])
  expect_equal(at_cap, list(new = 384L, total = 384L))
  expect_null(decon_sample_cap_message(at_cap))

  over <- decon_planned_samples(inputs)
  expect_equal(
    decon_sample_cap_message(over),
    paste0(
      "At most <b>384</b> samples per analysis database. This run would hold ",
      "<b>385</b>. Select fewer samples or start a new analysis."
    )
  )
})

test_that("samples already in the database count towards the cap", {
  db <- status_db(sprintf("S%03d", 1:300), failed = sprintf("F%03d", 1:50))
  planned <- decon_planned_samples(sprintf("N%03d.raw", 1:85), db)
  # Failed samples are not in the database's result
  expect_equal(planned, list(new = 85L, total = 385L))
  expect_match(
    decon_sample_cap_message(planned),
    "<b>385</b> (85 queried, the rest already in the database)",
    fixed = TRUE
  )
})

test_that("a sample queried again counts once", {
  db <- status_db(sprintf("S%03d", 1:384))
  # Queried with and without extension, and twice
  queried <- c(sprintf("S%03d.raw", 1:384), "S001", "S001.raw")
  planned <- decon_planned_samples(queried, db)
  expect_equal(planned, list(new = 384L, total = 384L))
  expect_null(decon_sample_cap_message(planned))
})

test_that("a missing or unreadable database holds no samples", {
  expect_equal(decon_planned_samples("a.raw", tempfile(fileext = ".db"))$total, 1L)
  broken <- withr::local_tempfile(fileext = ".db")
  writeLines("not a database", broken)
  # RSQLite warns while connecting; the count must still come back
  expect_equal(suppressWarnings(decon_planned_samples("a.raw", broken))$total, 1L)
  no_status <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), no_status)
  DBI::dbWriteTable(con, "peaks", data.frame(sample = "x"))
  DBI::dbDisconnect(con)
  expect_equal(decon_planned_samples("a.raw", no_status)$total, 1L)
})
