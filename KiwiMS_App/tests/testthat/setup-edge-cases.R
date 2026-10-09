# Setup for the edge-case tests (test-edge-*.R): the kit's shared code, a
# fresh set of its test files and a cache of the runs several tests read.
#
# A setup file runs after the helpers, so box.path (helper-kiwims.R) is set
# by the time tests/edge_cases/_shared.R loads the app modules.

source(
  file.path(kiwims_app_root(), "tests", "edge_cases", "_shared.R"),
  local = TRUE
)

# The test files are git-ignored, so the tests write their own set, from the
# generators as they are now, to a directory of their own. Files generated
# for manual testing in the category folders are neither read nor touched.
kit_test_dir <- tempfile("kiwims-edge-kit")
withr::local_options(list(kiwims.kit_dir = kit_test_dir), .local_envir = teardown_env())
withr::defer(unlink(kit_test_dir, recursive = TRUE), teardown_env())

# kit_cached(): Evaluate `expr` once per test run ----
# The runs take a few seconds each with kinetics, and the baseline is read by
# every category.
kit_cache <- new.env()
kit_cached <- function(key, expr) {
  if (!exists(key, envir = kit_cache, inherits = FALSE)) {
    assign(key, expr, envir = kit_cache)
  }
  get(key, envir = kit_cache, inherits = FALSE)
}

kit_ctx <- function() kit_cached("ctx", kit_context())

for (category in kit_categories) {
  kit_generate(category, kit_test_dir, kit_ctx())
}

# kit_case(): A run of the kit's test files, cached by its arguments ----
# Files are given as "category/name"; tol, maxm and kinetics as in kit_run().
kit_case <- function(proteins, compounds, config, ...) {
  key <- paste(proteins, compounds, config, deparse(list(...)), sep = "|")
  kit_cached(key, {
    f <- function(x) kit_load(dirname(x), basename(x))
    kit_run(kit_ctx(), f(proteins), f(compounds), f(config), ...)
  })
}

# kit_check(): The Samples table check of the kit's test files ----
kit_check <- function(proteins, compounds, config, ...) {
  f <- function(x) kit_load(dirname(x), basename(x))
  kit_declare(f(proteins), f(compounds), f(config), ...)
}

# expect_rounded(): `x` shows as `value` at the precision `value` is given in ----
# The READMEs quote rounded numbers; this compares at that precision without
# tripping over how a value exactly on .5 rounds.
expect_rounded <- function(x, value, digits = 1) {
  expect_true(
    is.numeric(x) && length(x) == 1 && !is.na(x) &&
      abs(x - value) <= 0.5 * 10^-digits + 1e-9,
    info = sprintf("got %.6f, expected %s", as.numeric(x)[1], value)
  )
}

# expect_kit_loads(): Every file of a category loads like an upload ----
# The files the manual tests upload, generated the same way generate.R does.
expect_kit_loads <- function(category) {
  files <- list.files(file.path(kit_test_dir, category), pattern = "\\.csv$")
  expect_gt(length(files), 0)
  for (name in sub("\\.csv$", "", files)) {
    expect_true(is.data.frame(kit_load(category, name)), label = name)
  }
}

# kit_synthetic_check(): check_sample_table() on a hand-written declaration ----
# `samples` is a data frame with Sample, Protein, Compound 1 (.. n) and,
# optionally, Concentration, Time and Replicate columns.
kit_synthetic_check <- function(
  samples,
  proteins = data.frame(Protein = "P", `Mass 1` = 1000, check.names = FALSE),
  compounds = data.frame(Compound = "A", `Mass 1` = 100, check.names = FALSE),
  tolerance = 3,
  max_multiples = 1
) {
  check_sample_table(
    samples,
    proteins$Protein,
    compounds$Compound,
    protein_table = proteins,
    compound_table = compounds,
    tolerance = tolerance,
    max_multiples = max_multiples
  )
}

# kit_design(): A valid kinact/KI declaration of one complex ----
# Three concentrations x three time points plus a 0 µM control, one sample
# each, so a test changes only what it is about.
kit_design <- function(
  conc = c(1, 2, 4),
  time = c(5, 10, 20),
  protein = "P",
  compound = "A",
  prefix = "S"
) {
  grid <- expand.grid(time = time, conc = conc)
  grid <- rbind(data.frame(time = 0, conc = 0), grid)
  data.frame(
    Sample = sprintf("%s%02d", prefix, seq_len(nrow(grid))),
    Protein = protein,
    `Compound 1` = compound,
    `Concentration [μM]` = grid$conc,
    `Time [min]` = grid$time,
    check.names = FALSE
  )
}
