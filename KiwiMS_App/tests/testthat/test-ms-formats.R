# Unit tests for the input-format registry.  No Python, no test corpus: these
# build their fixtures in a temp directory and run everywhere in a second.

box::use(
  app/logic/ms_formats[
    describe_ms_inputs,
    has_ms_extension,
    is_ms_input,
    list_ms_inputs,
    ms_extensions,
    ms_format_label,
    ms_result_dirname,
    ms_sample_base
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
