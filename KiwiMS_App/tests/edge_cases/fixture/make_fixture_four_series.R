# Builds fixture/mlkl_bi8925_4series.db: the edge-case fixture with four
# replicate series, R3 and R4 derived from R1 and R2 (kit_four_series_peaks()).
# For the manual test RP6. Needs the fixture mlkl_bi8925.db first (it takes
# the spectrum samples from there). The file is not committed.
#
# Usage (from KiwiMS_App):
#   Rscript tests/edge_cases/fixture/make_fixture_four_series.R [out_db]

here <- dirname(normalizePath(sub(
  "^--file=",
  "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)
)))
app_root <- normalizePath(file.path(here, "..", "..", ".."), winslash = "/")
options(box.path = app_root)
source(file.path(app_root, "tests", "reference_data", "reference_data.R"))
source(file.path(here, "fixture.R"))

base <- file.path(here, "mlkl_bi8925.db")
if (!file.exists(base)) {
  stop(base, " does not exist. Build it first: Rscript tests/edge_cases/fixture/make_fixture.R")
}
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else file.path(here, "mlkl_bi8925_4series.db")

kit_build_fixture(base, out, peaks = kit_four_series_peaks())
cat(sprintf("Wrote %s (%.0f KB)\n", out, file.size(out) / 1024))
