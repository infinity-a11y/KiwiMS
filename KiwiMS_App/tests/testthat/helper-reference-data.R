# Reference datasets (tests/reference_data): real acquisitions kept outside
# the repository, verified against their committed MD5 manifest before use.
# Runs after helper-kiwims.R, which sets box.path.

source(
  file.path(kiwims_app_root(), "tests", "reference_data", "reference_data.R"),
  local = TRUE
)

# skip_unless_reference(): Path of a verified reference dataset ----
# Skips when the dataset is not at $KIWIMS_REFERENCE_DATA/<dataset>; fails
# when it is there but differs from its manifest, since a test on other data
# would compare against the wrong expectations. Checked once per test run.
skip_unless_reference <- local({
  checked <- list()
  function(dataset) {
    if (is.null(checked[[dataset]])) {
      checked[[dataset]] <<- ref_verify(dataset)
    }
    result <- checked[[dataset]]
    if (result$status == "missing") {
      skip(paste0(
        "Reference dataset ", dataset, " not at ", result$path,
        " (set KIWIMS_REFERENCE_DATA)"
      ))
    }
    if (result$status == "mismatch") {
      stop(
        "Reference dataset ", dataset, " at ", result$path,
        " differs from tests/reference_data/", dataset, "/manifest.tsv:\n  ",
        paste(utils::head(result$problems, 10), collapse = "\n  "),
        call. = FALSE
      )
    }
    result$path
  }
})

# skip_unless_deconvolution(): Path of a verified dataset that can be run ----
# Everything an end-to-end test needs: the dataset, a Python interpreter that
# imports UniDec and an Rscript for the subprocess. KIWIMS_SKIP_DECON_RUN=1
# skips them all.
skip_unless_deconvolution <- function(dataset) {
  if (nzchar(Sys.getenv("KIWIMS_SKIP_DECON_RUN"))) {
    skip("KIWIMS_SKIP_DECON_RUN is set")
  }
  path <- skip_unless_reference(dataset)
  if (!nzchar(kiwims_python())) {
    skip("No Python interpreter that can import UniDec")
  }
  if (!file.exists(kiwims_rscript())) {
    skip("No Rscript available for the deconvolution subprocess")
  }
  path
}

# waters_samples(): The first n Waters samples of kinact_MLKL_3 ----
# In name order, so every run takes the same ones.
waters_samples <- function(n) {
  kiwims_raw_dirs(skip_unless_deconvolution("kinact_MLKL_3"), n)
}

# thermo_samples(): The first n Thermo files of thermo_intact ----
thermo_samples <- function(n) {
  kiwims_thermo_files(skip_unless_deconvolution("thermo_intact"), n)
}
