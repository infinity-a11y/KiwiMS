# Unit tests for the input-format registry.  No Python, no test corpus: these
# build their fixtures in a temp directory and run everywhere in a second.

box::use(
  app/logic/ms_formats[
    describe_duplicate_samples,
    describe_ms_inputs,
    has_ms_extension,
    is_ms_input,
    list_ms_inputs,
    ms_duplicate_samples,
    ms_extensions,
    ms_format_label,
    ms_result_dirname,
    ms_sample_base,
    ms_sample_in
  ],
)

# make_corpus(): a directory holding one sample of each supported shape ----
make_corpus <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(root, "waters_sample.raw"))
  # MassLynx puts its function files inside the directory; one is enough to
  # make the fixture recognisable.
  writeLines("x", file.path(root, "waters_sample.raw", "_FUNC001.DAT"))
  writeLines("x", file.path(root, "thermo_sample.raw"))
  writeLines("x", file.path(root, "open_sample.mzML"))
  writeLines("x", file.path(root, "old_sample.mzXML"))
  writeLines("x", file.path(root, "zipped_sample.mzML.gz"))
  writeLines("x", file.path(root, "notes.txt"))
  dir.create(file.path(root, "unrelated_folder"))
  root
}

test_that("ms_sample_base strips every supported extension, any case", {
  expect_equal(ms_sample_base("sample.raw"), "sample")
  expect_equal(ms_sample_base("SAMPLE.RAW"), "SAMPLE")
  expect_equal(ms_sample_base("sample.mzML"), "sample")
  expect_equal(ms_sample_base("sample.mzml"), "sample")
  expect_equal(ms_sample_base("sample.mzXML"), "sample")
  expect_equal(ms_sample_base("sample.mzML.gz"), "sample")
  expect_equal(ms_sample_base("C:/data/run/sample.raw"), "sample")
  expect_equal(ms_sample_base(character(0)), character(0))
})

test_that("ms_sample_base only strips a trailing extension", {
  # The old gsub("\\.raw$", ...) was anchored and got this right; the point is
  # that generalising to more formats did not lose the anchor.
  expect_equal(ms_sample_base("2024_raw_redo.raw"), "2024_raw_redo")
  expect_equal(ms_sample_base("drawing.raw"), "drawing")
  expect_equal(ms_sample_base("mzML_archive.raw"), "mzML_archive")
})

test_that("ms_result_dirname is anchored where the old substitution was not", {
  # Regression: result folders were built with
  #   gsub(".raw", "_rawdata_unidecfiles", x)
  # where "." is a regex wildcard and the match is neither anchored nor limited
  # to the first hit, so "drawing.raw" became
  # "_rawdata_unidecfilesing_rawdata_unidecfiles" and the run looked for results
  # in a folder that was never created.
  expect_equal(ms_result_dirname("plain.raw"), "plain_rawdata_unidecfiles")
  expect_equal(ms_result_dirname("drawing.raw"), "drawing_rawdata_unidecfiles")
  expect_equal(
    ms_result_dirname("2024_raw_redo.raw"),
    "2024_raw_redo_rawdata_unidecfiles"
  )
  expect_equal(ms_result_dirname("sample.mzML"), "sample_rawdata_unidecfiles")
  # A bare sample name is already the base and must pass through untouched.
  expect_equal(ms_result_dirname("plain"), "plain_rawdata_unidecfiles")
})

test_that("has_ms_extension accepts the registry and nothing else", {
  expect_true(all(has_ms_extension(paste0("s", ms_extensions()))))
  expect_true(has_ms_extension("s.RAW"))
  expect_false(has_ms_extension("s.txt"))
  expect_false(has_ms_extension("s.raw.bak"))
  expect_equal(has_ms_extension(character(0)), logical(0))
})

test_that("is_ms_input separates Thermo files from Waters directories", {
  root <- make_corpus()
  expect_true(is_ms_input(file.path(root, "waters_sample.raw")))
  expect_true(is_ms_input(file.path(root, "thermo_sample.raw")))
  expect_true(is_ms_input(file.path(root, "open_sample.mzML")))
  expect_true(is_ms_input(file.path(root, "zipped_sample.mzML.gz")))

  expect_false(is_ms_input(file.path(root, "notes.txt")))
  expect_false(is_ms_input(file.path(root, "unrelated_folder")))
  expect_false(is_ms_input(file.path(root, "absent.raw")))

  # The open formats are files; a directory that merely ends in .mzML is not a
  # sample, even though the extension matches.
  dir.create(file.path(root, "decoy.mzML"))
  expect_false(is_ms_input(file.path(root, "decoy.mzML")))
})

test_that("ms_format_label names the vendor a .raw path actually belongs to", {
  root <- make_corpus()
  expect_equal(ms_format_label(file.path(root, "waters_sample.raw")), "Waters")
  expect_equal(ms_format_label(file.path(root, "thermo_sample.raw")), "Thermo")
  expect_equal(ms_format_label(file.path(root, "open_sample.mzML")), "mzML")
  expect_equal(ms_format_label(file.path(root, "old_sample.mzXML")), "mzXML")
  expect_equal(
    ms_format_label(file.path(root, "zipped_sample.mzML.gz")),
    "mzML (gz)"
  )
})

test_that("list_ms_inputs finds files and directories, and ignores the rest", {
  root <- make_corpus()
  found <- list_ms_inputs(root)
  expect_setequal(
    basename(found),
    c(
      "waters_sample.raw",
      "thermo_sample.raw",
      "open_sample.mzML",
      "old_sample.mzXML",
      "zipped_sample.mzML.gz"
    )
  )

  # The regression this replaces: list.dirs() saw only the Waters directory, so
  # a folder of Thermo files read as empty.
  dir_only <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  expect_length(dir_only[grepl("\\.raw$", dir_only)], 1L)
  expect_gt(length(found), 1L)

  expect_equal(list_ms_inputs(file.path(root, "nowhere")), character(0))
  expect_equal(list_ms_inputs(""), character(0))

  # full_names = FALSE returns the same set, as names only.
  expect_setequal(list_ms_inputs(root, full_names = FALSE), basename(found))
})

test_that("list_ms_inputs does not recurse into a Waters directory", {
  # Recursion would walk MassLynx internals -- hundreds of network round trips
  # on a mapped share -- and would not find more samples anyway.
  root <- make_corpus()
  nested <- file.path(root, "waters_sample.raw", "inner.mzML")
  writeLines("x", nested)
  expect_false(any(basename(list_ms_inputs(root)) == "inner.mzML"))
})

test_that("describe_ms_inputs counts by vendor", {
  root <- make_corpus()
  desc <- describe_ms_inputs(list_ms_inputs(root))
  expect_match(desc, "1 Waters")
  expect_match(desc, "1 Thermo")
  expect_equal(describe_ms_inputs(character(0)), "")
})

test_that("ms_sample_in matches config samples with or without an extension", {
  # A config written as "A1" must find A1.raw, and one written as "A1.raw"
  # must still find it -- the extension does not identify the sample.
  inputs <- c("C:/data/A1.raw", "C:/data/A2.mzML", "C:/data/A3.raw")
  expect_equal(
    ms_sample_in(c("A1", "A2.raw", "A4"), inputs),
    c(TRUE, TRUE, FALSE)
  )
  expect_equal(
    ms_sample_in(inputs, c("A1", "A3.RAW")),
    c(TRUE, FALSE, TRUE)
  )
  expect_equal(ms_sample_in(character(0), inputs), logical(0))
  # Config columns read via read.csv can arrive as factors.
  expect_equal(ms_sample_base(factor("A1.raw")), "A1")
})

test_that("ms_duplicate_samples finds inputs that share a sample name", {
  # X.raw and X.mzML both run as sample "X"; on Windows x.mzML would also share
  # X's result folder, so case does not separate them either.
  paths <- c("C:/d/X.raw", "C:/d/x.mzML", "C:/d/Y.mzML", "C:/d/Z.raw", "C:/d/Z.mzXML")
  dups <- ms_duplicate_samples(paths)
  expect_named(dups, c("X", "Z"))
  expect_equal(dups$X, c("X.raw", "x.mzML"))
  expect_equal(dups$Z, c("Z.raw", "Z.mzXML"))
  expect_equal(
    describe_duplicate_samples(dups),
    "X (X.raw, x.mzML); Z (Z.raw, Z.mzXML)"
  )

  expect_length(ms_duplicate_samples(c("C:/d/A.raw", "C:/d/B.mzML")), 0L)
  expect_length(ms_duplicate_samples("C:/d/A.raw"), 0L)
  expect_length(ms_duplicate_samples(character(0)), 0L)
})

test_that("the start dialog refuses inputs that share a sample name", {
  # Drives the real module: duplicates block the run until one of the pair is
  # deselected, and with a config (no picker) they block outright.
  root <- withr::local_tempdir()
  for (f in c("X.raw", "X.mzML", "Y.mzML")) writeLines("x", file.path(root, f))

  box::use(app / view / deconvolution_main)
  use_config <- shiny::reactiveVal(FALSE)
  config <- shiny::reactiveVal(data.frame())

  shiny::testServer(
    deconvolution_main$server,
    args = list(
      deconvolution_sidebar_vars = list(
        dir = shiny::reactive(root),
        targetpath = shiny::reactive(root),
        selected = shiny::reactive("folder"),
        use_config = use_config
      ),
      conversion_main_vars = list(cancel_continuation = shiny::reactive(0)),
      reset_button = shiny::reactive(0),
      config_file = config
    ),
    {
      # Picker not rendered yet: everything is queued, so X clashes
      expect_length(planned_inputs(), 3L)
      msg <- duplicate_sample_message(planned_inputs())
      expect_match(msg, "X (X.mzML, X.raw)", fixed = TRUE)
      expect_match(msg, "or deselect it", fixed = TRUE)

      # Deselecting one of the pair clears it
      session$setInputs(target_selector = c("X.raw", "Y.mzML"))
      expect_null(duplicate_sample_message(planned_inputs()))

      # A config naming "X" matches both files -- no picker to fix it with
      use_config(TRUE)
      config(data.frame(Sample = c("X", "Y")))
      session$flushReact()
      expect_setequal(basename(planned_inputs()), c("X.mzML", "X.raw", "Y.mzML"))
      msg <- duplicate_sample_message(planned_inputs())
      expect_match(msg, "rename or move one of the files.", fixed = TRUE)
      expect_false(grepl("deselect", msg))
    }
  )
})
