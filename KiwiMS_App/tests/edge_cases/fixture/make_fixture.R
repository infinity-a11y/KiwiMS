# Builds the edge-case fixture mlkl_bi8925.db (see fixture.R), the result
# database the manual tests load in the app. The file is not committed.
#
# Usage (from KiwiMS_App):
#   Rscript tests/edge_cases/fixture/make_fixture.R [--from=<result_db>] [out_db]
#
# --from takes the spectra from an existing deconvolution result of
# kinact_MLKL_3 made with the reference parameters
# (tests/reference_data/kinact_MLKL_3/params.tsv), e.g. one made in the app.
# Without it the two spectrum samples are deconvolved from the reference
# dataset, which must be present and match its manifest (about 30 s; needs
# the kiwims Python environment).

here <- dirname(normalizePath(sub(
  "^--file=",
  "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)
)))
app_root <- normalizePath(file.path(here, "..", "..", ".."), winslash = "/")
options(box.path = app_root)
source(file.path(app_root, "tests", "reference_data", "reference_data.R"))
source(file.path(here, "fixture.R"))

args <- commandArgs(trailingOnly = TRUE)
from <- sub("^--from=", "", grep("^--from=", args, value = TRUE))
out <- grep("^--", args, value = TRUE, invert = TRUE)
out <- if (length(out)) out[1] else file.path(here, "mlkl_bi8925.db")

if (!length(from)) {
  check <- ref_verify("kinact_MLKL_3")
  if (check$status != "ok") {
    stop(
      "Reference dataset kinact_MLKL_3 ", check$status, " at ", check$path,
      if (length(check$problems)) paste0(": ", check$problems[1]),
      ". Set KIWIMS_REFERENCE_DATA or pass --from=<result_db>."
    )
  }
  # The test helpers know how to launch the deconvolution the way the app does
  owd <- setwd(file.path(app_root, "tests", "testthat"))
  source("helper-kiwims.R")
  setwd(owd)
  if (!nzchar(kiwims_python())) {
    stop("No Python interpreter that can import UniDec; pass --from=<result_db>.")
  }
  work <- tempfile("kiwims-fixture")
  dir.create(work)
  res <- kiwims_run_deconvolution(
    file.path(check$path, paste0(kit_spectrum_samples, ".raw")),
    work,
    params = do.call(kiwims_default_params, ref_params("kinact_MLKL_3"))
  )
  if (res$status != 0) {
    stop("Deconvolution failed, see ", res$stdout_path)
  }
  from <- res$db_path
}

kit_build_fixture(from, out)
cat(sprintf("Wrote %s (%.0f KB)\n", out, file.size(out) / 1024))
