# Unit tests for leaving samples out of a conversion: the Include column of
# the Samples table, the checks and tables built from it, and the result the
# conversion reads. Synthetic tables and a temporary result database only.

box::use(
  app/logic/conversion_constants[sample_include_js],
  app/logic/conversion_functions[
    add_hits,
    check_sample_table,
    clean_sample_table,
    confirm_sample_table,
    conversion_input_result,
    fill_sample_table,
    included_sample_rows,
    new_sample_table,
    order_sample_cols,
    restore_conc_time,
    restore_inclusion,
    sample_handsontable,
    sample_inclusion,
    summarize_hits
  ],
)

proteins <- data.frame(Protein = "P", `Mass 1` = 20000, check.names = FALSE)
compounds <- data.frame(
  Compound = c("A", "B"),
  `Mass 1` = c(300, 500),
  check.names = FALSE
)

# declared(): A Samples table as the declaration interface holds it ----
# `compound` and `include` are recycled over the samples
declared <- function(
  samples = c("S1", "S2", "S3"),
  compound = "A",
  include = TRUE,
  kinact = FALSE
) {
  tbl <- new_sample_table(
    list(samples = samples),
    proteins,
    compounds,
    kinact_ki = kinact
  )
  tbl$Protein <- "P"
  tbl$`Compound 1` <- rep_len(compound, nrow(tbl))
  tbl$Include <- rep_len(include, nrow(tbl))
  tbl$Replicate <- ""
  order_sample_cols(tbl)
}

# check(): check_sample_table() with the tables above ----
check <- function(tbl, ...) {
  check_sample_table(
    clean_sample_table(tbl),
    proteins$Protein,
    compounds$Compound,
    ...
  )
}

# result_db(): A result database holding the given samples as done ----
# Each sample gets one protein peak and one peak of compound A bound
result_db <- function(done, failed = character(0), env = parent.frame()) {
  db <- withr::local_tempfile(fileext = ".db", .local_envir = env)
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(
    con,
    "status",
    data.frame(
      sample = c(done, failed),
      state = rep(c("done", "failed"), c(length(done), length(failed)))
    )
  )
  all <- c(done, failed)
  DBI::dbWriteTable(
    con,
    "peaks",
    data.frame(
      sample = rep(all, each = 2),
      mass = rep(c(20000, 20300), length(all)),
      intensity = rep(c(60, 40), length(all))
    )
  )
  db
}

# ---- The Include column -----------------------------------------------------

test_that("a new Samples table includes every sample, Include leading", {
  tbl <- new_sample_table(list(samples = c("S2", "S1")), proteins, compounds)
  expect_equal(names(tbl)[1:2], c("Include", "Sample"))
  expect_identical(tbl$Include, c(TRUE, TRUE))
  expect_equal(tbl$Sample, c("S1", "S2"))

  kin <- new_sample_table(
    list(samples = "S1"),
    proteins,
    compounds,
    kinact_ki = TRUE
  )
  expect_equal(names(kin)[1], "Include")
  expect_equal(utils::tail(names(kin), 2), c("Concentration", "Time"))
})

test_that("only an explicit FALSE leaves a sample out", {
  tbl <- data.frame(Sample = paste0("S", 1:4))
  expect_identical(sample_inclusion(tbl), rep(TRUE, 4))

  tbl$Include <- c(TRUE, FALSE, NA, TRUE)
  expect_identical(sample_inclusion(tbl), c(TRUE, FALSE, TRUE, TRUE))

  # As text, e.g. from a table read back without its column types
  tbl$Include <- c("TRUE", "FALSE", "", "true")
  expect_identical(sample_inclusion(tbl), c(TRUE, FALSE, TRUE, TRUE))

  expect_identical(sample_inclusion(tbl[0, ]), logical(0))
})

test_that("the included rows lose the Include column", {
  tbl <- declared(include = c(TRUE, FALSE, TRUE))
  out <- included_sample_rows(tbl)
  expect_false("Include" %in% names(out))
  expect_equal(out$Sample, c("S1", "S3"))
  expect_equal(rownames(out), c("1", "2"))
  # A table without the column keeps every row
  expect_equal(nrow(included_sample_rows(out)), 2)
})

test_that("columns are ordered Include, Sample, Replicate, then the rest", {
  tbl <- data.frame(Protein = "P", Replicate = "R1", Sample = "S1", Include = TRUE)
  expect_equal(
    names(order_sample_cols(tbl)),
    c("Include", "Sample", "Replicate", "Protein")
  )
  expect_equal(
    names(order_sample_cols(tbl[, c("Protein", "Sample")])),
    c("Sample", "Protein")
  )
})

test_that("the Include state survives a rebuild of the table", {
  old <- declared(include = c(TRUE, FALSE, TRUE), kinact = TRUE)
  old$Concentration <- c(1, 2, 3)

  # A saved protein or compound table rebuilds the Samples table; a sample
  # new to it is included
  rebuilt <- new_sample_table(
    list(samples = c("S1", "S2", "S4")),
    proteins,
    compounds,
    kinact_ki = TRUE
  )
  rebuilt <- restore_inclusion(restore_conc_time(rebuilt, old), old)
  expect_identical(rebuilt$Include, c(TRUE, FALSE, TRUE))
  expect_equal(rebuilt$Concentration, c(1, 2, NA))

  expect_identical(restore_inclusion(rebuilt, NULL), rebuilt)
  no_col <- old[, names(old) != "Include"]
  expect_identical(restore_inclusion(rebuilt, no_col), rebuilt)
})

test_that("filling and cleaning keep Include and Replicate in place", {
  tbl <- declared(include = c(TRUE, FALSE, TRUE))
  # The left-out sample lists a second compound; it still counts for the
  # columns of the table shown
  tbl$`Compound 2` <- c("", "B", "")

  clean <- clean_sample_table(tbl)
  expect_equal(
    names(clean),
    c("Include", "Sample", "Replicate", "Protein", "Compound 1", "Compound 2")
  )
  expect_identical(clean$Include, c(TRUE, FALSE, TRUE))

  filled <- fill_sample_table(clean, kinact_ki = FALSE)
  expect_equal(
    names(filled),
    c("Include", "Sample", "Replicate", "Protein", paste("Compound", 1:5))
  )
  expect_identical(filled$Include, c(TRUE, FALSE, TRUE))

  kin <- declared(include = c(FALSE, TRUE, TRUE), kinact = TRUE)
  kin$Concentration <- c(1, 2, 3)
  kin$Time <- c(5, 5, 5)
  filled <- fill_sample_table(clean_sample_table(kin), kinact_ki = TRUE)
  expect_equal(names(filled)[1:3], c("Include", "Sample", "Replicate"))
  expect_equal(utils::tail(names(filled), 2), c("Concentration", "Time"))
  expect_identical(filled$Include, c(FALSE, TRUE, TRUE))
  expect_equal(filled$Concentration, c(1, 2, 3))
})

# ---- The checks ---------------------------------------------------------------

test_that("a left-out sample is not checked", {
  # An undeclared compound, as in a sample of a compound missing from the
  # Compounds table
  tbl <- declared(compound = c("A", "UNKNOWN", "B"))
  expect_equal(as.character(check(tbl)), "Compound name not declared")
  tbl$Include[2] <- FALSE
  expect_true(isTRUE(check(tbl)))

  # Nor its protein, compounds or their duplicates
  tbl <- declared()
  tbl$Protein[3] <- ""
  expect_equal(as.character(check(tbl)), "Assign proteins")
  tbl$Include[3] <- FALSE
  expect_true(isTRUE(check(tbl)))

  tbl <- declared()
  tbl$`Compound 2` <- c("", "", "A")
  expect_equal(as.character(check(tbl)), "Duplicated compounds")
  tbl$Include[3] <- FALSE
  expect_true(isTRUE(check(tbl)))
})

test_that("a table without included samples is refused", {
  res <- check(declared(include = FALSE))
  expect_equal(as.character(res), "No sample included")
  expect_match(attr(res, "note"), "Tick the samples")
})

test_that("the sample cap counts the included samples", {
  samples <- sprintf("S%03d", 1:385)
  tbl <- declared(samples)
  expect_equal(
    as.character(check(tbl)),
    "At most 384 samples per run (385 present)"
  )
  tbl$Include[385] <- FALSE
  expect_true(isTRUE(check(tbl)))
})

test_that("the kinetics design rules count the included samples", {
  design <- expand.grid(time = 1:3, conc = 1:3)
  tbl <- declared(sprintf("S%d", seq_len(nrow(design))), kinact = TRUE)
  tbl$Concentration <- design$conc
  tbl$Time <- design$time
  expect_true(isTRUE(check(tbl)))

  # Leaving out a time point of concentration 3 breaks its time series
  tbl$Include[9] <- FALSE
  expect_equal(
    as.character(check(tbl)),
    paste0(
      "At least 3 different non-zero time points required per concentration ",
      "(concentration 3 has only 2)"
    )
  )

  # A broken sample left out no longer blocks the table
  tbl <- rbind(tbl, tbl[1, ])
  tbl$Sample[10] <- "S10"
  tbl$Include <- TRUE
  tbl$Concentration[10] <- NA
  expect_equal(as.character(check(tbl)), "Fill Concentrations")
  tbl$Include[10] <- FALSE
  expect_true(isTRUE(check(tbl)))
})

test_that("replicate warnings compare the included samples only", {
  tbl <- declared(c("X_R1", "X_R2"), compound = c("A", "B"))
  res <- check(tbl)
  expect_true(isTRUE(res))
  expect_match(attr(res, "warning"), "Replicates declared differently")

  tbl$Include[2] <- FALSE
  res <- check(tbl)
  expect_true(isTRUE(res))
  expect_null(attr(res, "warning"))
})

# ---- Confirming and converting ------------------------------------------------

test_that("a confirmed table shows every sample and converts the included", {
  tbl <- declared(include = c(TRUE, FALSE, TRUE), kinact = TRUE)
  tbl$`Compound 2` <- c("", "B", "")
  tbl$Concentration <- c(1, 2, 3)
  tbl$Time <- c(5, 5, 5)

  confirmed <- confirm_sample_table(tbl, units = list(conc = "µM", time = "min"))

  expect_equal(confirmed$shown$Sample, c("S1", "S2", "S3"))
  expect_identical(confirmed$shown$Include, c(TRUE, FALSE, TRUE))
  expect_true("Compound 2" %in% names(confirmed$shown))

  # The converted table: the included samples only, without Include and
  # without the compound column only the left-out sample used
  expect_equal(
    names(confirmed$converted),
    c(
      "Sample",
      "Replicate",
      "Protein",
      "Compound 1",
      "Concentration [µM]",
      "Time [min]"
    )
  )
  expect_equal(confirmed$converted$Sample, c("S1", "S3"))
  expect_equal(confirmed$converted$`Concentration [µM]`, c(1, 3))
  expect_equal(confirmed$excluded, "S2")

  none_out <- confirm_sample_table(declared())
  expect_equal(none_out$excluded, character(0))
  expect_equal(nrow(none_out$converted), 3)
})

test_that("the conversion reads the included samples only", {
  db <- result_db(c("S1", "S2", "S3"), failed = "S4")

  all <- conversion_input_result(list(.db_path = db))
  expect_equal(sort(names(all$deconvolution)), c("S1", "S2", "S3"))

  picked <- conversion_input_result(list(.db_path = db), samples = c("S1", "S3"))
  expect_equal(sort(names(picked$deconvolution)), c("S1", "S3"))

  # A result held in memory is cut alike
  mem <- conversion_input_result(all, samples = "S2")
  expect_equal(names(mem$deconvolution), "S2")
  expect_identical(conversion_input_result(all), all)
})

test_that("the screening runs over the included samples only", {
  db <- result_db(c("S1", "S2", "S3"))
  # S2 declares a compound missing from the Compounds table: left out, it
  # neither blocks the table nor reaches the screening
  tbl <- declared(compound = c("A", "UNKNOWN", "A"), include = c(TRUE, FALSE, TRUE))
  expect_true(isTRUE(check(tbl)))

  converted <- confirm_sample_table(tbl)$converted
  result <- conversion_input_result(list(.db_path = db), samples = converted$Sample)
  screened <- suppressMessages(add_hits(
    result,
    sample_table = converted,
    protein_table = proteins,
    compound_table = compounds,
    peak_tolerance = 3,
    max_multiples = 2,
    session = list(sendCustomMessage = function(...) invisible(NULL)),
    ns = identity
  ))
  hits <- suppressMessages(summarize_hits(screened, sample_table = converted))

  expect_setequal(unique(hits$Sample), c("S1", "S3"))
  expect_true(all(hits$Compound[!is.na(hits$Compound)] == "A"))
})

# ---- The table widget ---------------------------------------------------------

test_that("the Include column renders as checkboxes", {
  tbl <- declared()
  hot <- sample_handsontable(tbl, proteins$Protein, compounds$Compound)
  cols <- hot$x$columns
  idx <- function(name) match(name, unlist(hot$x$rColnames))

  expect_equal(cols[[idx("Include")]]$type, "checkbox")
  expect_equal(cols[[idx("Include")]]$className, "htCenter")
  # Include, Sample, Replicate and Protein stay in view
  expect_equal(hot$x$fixedColumnsLeft, idx("Protein"))

  # The checking renderer starts after Sample and knows the Include column
  # and the 0-based protein position
  renderer <- function(name) as.character(cols[[idx(name)]]$renderer)
  expect_false(grepl("includeCol", renderer("Sample")))
  expect_match(renderer("Protein"), "var includeCol = 0;", fixed = TRUE)
  expect_match(
    renderer("Compound 1"),
    sprintf("if (col === %d)", idx("Protein") - 1L),
    fixed = TRUE
  )

  hooks <- vapply(hot$jsHooks$render, function(h) as.character(h$code), "")
  expect_true(sample_include_js %in% hooks)

  # Read-only once the table is saved
  saved <- sample_handsontable(tbl, proteins$Protein, compounds$Compound, disabled = TRUE)
  expect_true(saved$x$columns[[idx("Include")]]$readOnly)
})

test_that("a Samples table without Include renders as before", {
  tbl <- declared()[, -1]
  hot <- sample_handsontable(tbl, proteins$Protein, compounds$Compound)
  types <- vapply(hot$x$columns, `[[`, "", "type")
  expect_false("checkbox" %in% types)
  # Sample, Replicate, Protein
  expect_equal(hot$x$fixedColumnsLeft, 3L)
  expect_match(
    as.character(hot$x$columns[[3]]$renderer),
    "var includeCol = -1;",
    fixed = TRUE
  )
  hooks <- vapply(hot$jsHooks$render, function(h) as.character(h$code), "")
  expect_false(sample_include_js %in% hooks)
})
