# The two reference datasets (tests/reference_data/README.md), kept outside
# the repository and verified against their MD5 manifests:
#   kinact_MLKL_3  122 Waters samples, the series the edge-case kit runs on
#   thermo_intact  2 Thermo .raw files
# Each is deconvolved with its reference parameters and compared with its
# committed peak list.
#
# Datasets are looked for at $KIWIMS_REFERENCE_DATA/<dataset> (default
# E:/KF_Testing/Test-Data). Missing: the tests skip. Present but different
# from the manifest: they fail. The whole MLKL series (about 4 min) runs only
# with KIWIMS_TEST_REFERENCE_FULL=1; KIWIMS_SKIP_DECON_RUN=1 skips every
# deconvolution.

box::use(
  app/logic/conversion_functions[read_decon_result, validate_decon_db],
)

source(
  file.path(kiwims_app_root(), "tests", "edge_cases", "fixture", "fixture.R"),
  local = TRUE
)

datasets <- c("kinact_MLKL_3", "thermo_intact")

# deconvolute(): Run samples of a dataset with its reference parameters ----
deconvolute <- function(dataset, inputs, label) {
  work <- withr::local_tempdir(label, .local_envir = teardown_env())
  res <- kiwims_run_deconvolution(
    inputs,
    work,
    params = do.call(kiwims_default_params, ref_params(dataset))
  )
  expect_equal(res$status, 0L, info = paste("subprocess output in", res$stdout_path))
  res
}

# expect_reference_peaks(): Peaks of a run equal the committed list ----
expect_reference_peaks <- function(dataset, db_path, samples) {
  expected <- ref_peaks(dataset)
  got <- kiwims_db_query(db_path, "SELECT sample, mass, intensity FROM peaks")
  version <- kiwims_db_query(db_path, "SELECT value FROM config WHERE key = 'version' LIMIT 1")$value
  expect_setequal(unique(got$sample), samples)
  for (s in samples) {
    a <- got[got$sample == s, ]
    b <- expected[expected$sample == s, ]
    a <- a[order(a$mass), ]
    b <- b[order(b$mass), ]
    info <- sprintf("%s (UniDec %s; the reference was made with 7.0.3)", s, version)
    expect_equal(a$mass, b$mass, info = info)
    expect_equal(a$intensity, b$intensity, tolerance = 1e-9, info = info)
  }
}

# ---- Without the data -------------------------------------------------------

test_that("each manifest and peak list describe the same samples", {
  for (dataset in datasets) {
    manifest <- ref_manifest(dataset)
    expected <- ref_peaks(dataset)
    # A Waters sample is a .raw directory, a Thermo sample a .raw file
    raw <- unique(sub("^([^/]*\\.raw)(/.*)?$", "\\1", manifest$file[grepl("\\.raw(/|$)", manifest$file)]))
    expect_setequal(sub("\\.raw$", "", raw), unique(expected$sample))
    expect_true(all(names(kiwims_default_params()) %in% names(ref_params(dataset))), label = dataset)
  }

  mlkl <- ref_manifest("kinact_MLKL_3")
  # Every Waters sample is complete: both functions with data and index
  for (f in c("_FUNC001.DAT", "_FUNC001.IDX", "_FUNC002.DAT", "_FUNC002.IDX")) {
    expect_equal(sum(basename(mlkl$file) == f), 122, label = f)
  }
  expect_equal(nrow(ref_peaks("kinact_MLKL_3")), 466)
  expect_equal(nrow(ref_manifest("thermo_intact")), 2)
})

test_that("a dataset that differs from its manifest is reported, not used", {
  dir <- withr::local_tempdir()
  data <- file.path(dir, "data", "toy")
  dir.create(file.path(data, "a.raw"), recursive = TRUE)
  writeLines("one", file.path(data, "a.raw", "_FUNC001.DAT"))
  writeLines("two", file.path(data, "b.txt"))
  dir.create(file.path(dir, "manifest", "toy"), recursive = TRUE)
  utils::write.table(
    ref_hash(data),
    file.path(dir, "manifest", "toy", "manifest.tsv"),
    sep = "\t", quote = FALSE, row.names = FALSE
  )
  check <- function() {
    ref_verify("toy", data, dir = file.path(dir, "manifest"), rehash = TRUE)
  }

  expect_equal(check()$status, "ok")
  expect_equal(ref_verify("toy", file.path(dir, "nowhere"), dir = file.path(dir, "manifest"))$status, "missing")
  # Same size, other content: only the MD5 tells
  writeLines("owt", file.path(data, "b.txt"))
  expect_equal(check()$problems, "md5 differs: b.txt")
  writeLines("longer", file.path(data, "b.txt"))
  expect_match(check()$problems, "^size b.txt")
  writeLines("x", file.path(data, "extra.txt"))
  unlink(file.path(data, "a.raw", "_FUNC001.DAT"))
  expect_setequal(
    check()$problems[1:2],
    c("missing: a.raw/_FUNC001.DAT", "not in the manifest: extra.txt")
  )
})

# ---- With the data ------------------------------------------------------------

test_that("kinact_MLKL_3 matches its manifest", {
  path <- skip_unless_reference("kinact_MLKL_3")
  expect_equal(ref_verify("kinact_MLKL_3", path)$status, "ok")
})

test_that("thermo_intact matches its manifest", {
  path <- skip_unless_reference("thermo_intact")
  expect_equal(ref_verify("thermo_intact", path)$status, "ok")
})

# The two samples the edge-case fixture needs, deconvolved once for both tests
# below (worker start-up makes up most of the time, so few samples is cheap)
key_run <- local({
  run <- NULL
  function() {
    path <- skip_unless_deconvolution("kinact_MLKL_3")
    if (is.null(run)) {
      run <<- deconvolute(
        "kinact_MLKL_3",
        file.path(path, paste0(kit_spectrum_samples, ".raw")),
        "kiwims-ref-key"
      )
    }
    run
  }
})

test_that("deconvolution of key MLKL samples reproduces their reference peaks", {
  res <- key_run()
  expect_reference_peaks("kinact_MLKL_3", res$db_path, kit_spectrum_samples)
})

test_that("the edge-case fixture builds from that run and opens in the app", {
  res <- key_run()
  out <- withr::local_tempfile(fileext = ".db")
  kit_build_fixture(res$db_path, out)

  expect_null(validate_decon_db(out))
  r <- read_decon_result(out)
  expect_length(r$deconvolution, 122)
  expect_equal(sum(vapply(r$deconvolution, function(s) nrow(s$peaks), 1L)), 466)
  with_spectrum <- names(Filter(function(s) nrow(s$mass) > 0, r$deconvolution))
  expect_setequal(with_spectrum, kit_spectrum_samples)

  # Nothing of the machine that made it
  con <- DBI::dbConnect(RSQLite::SQLite(), out, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con))
  values <- DBI::dbGetQuery(con, "SELECT value FROM config")$value
  expect_false(any(grepl("[A-Za-z]:[/\\\\]|/Users/|/home/", values)))
  expect_false(DBI::dbExistsTable(con, "session"))
  expect_false(DBI::dbExistsTable(con, "output_log"))

  # A result of other samples is refused
  expect_error(kit_build_fixture(res$db_path, out, samples = "nope"), "does not match")
})

test_that("deconvolution of the Thermo files reproduces their reference peaks", {
  files <- thermo_samples(Inf)
  res <- deconvolute("thermo_intact", files, "kiwims-ref-thermo")
  status <- kiwims_db_query(res$db_path, "SELECT sample, state, error_msg FROM status")
  expect_equal(status$state, rep("done", nrow(status)), info = paste(status$error_msg, collapse = " | "))
  expect_reference_peaks("thermo_intact", res$db_path, kiwims_sample_bases(files))
})

test_that("deconvolution of the whole MLKL series reproduces its reference peaks", {
  path <- skip_unless_deconvolution("kinact_MLKL_3")
  if (!nzchar(Sys.getenv("KIWIMS_TEST_REFERENCE_FULL"))) {
    skip("Set KIWIMS_TEST_REFERENCE_FULL=1 to deconvolve all 122 samples (about 4 min)")
  }
  samples <- unique(ref_peaks("kinact_MLKL_3")$sample)
  res <- deconvolute("kinact_MLKL_3", file.path(path, paste0(samples, ".raw")), "kiwims-ref-full")
  expect_reference_peaks("kinact_MLKL_3", res$db_path, samples)
})
