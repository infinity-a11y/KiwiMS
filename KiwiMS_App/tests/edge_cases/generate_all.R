# Writes the files of every category of the edge-case kit, which are
# git-ignored and only exist once generated. The automated tests don't need
# them: they write their own set to a temporary directory.
#
# Usage (from KiwiMS_App):
#   Rscript tests/edge_cases/generate_all.R [--report] [--fixture[=<result_db>]]
#
# --report     prints what the app shows for every test
# --fixture    also builds fixture/mlkl_bi8925.db, the database the manual
#              tests load; =<result_db> takes its spectra from an existing
#              deconvolution of kinact_MLKL_3 (see fixture/make_fixture.R)

here <- dirname(normalizePath(sub(
  "^--file=",
  "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)
)))
rscript <- file.path(R.home("bin"), "Rscript")
args <- commandArgs(trailingOnly = TRUE)
run <- function(script, script_args = character(0)) {
  status <- system2(rscript, c(shQuote(script), script_args))
  if (status != 0) {
    stop(script, " failed with status ", status)
  }
}

categories <- c("baseline", "mass_ambiguity", "complexes", "replicates", "run_limits")
for (category in categories) {
  run(file.path(here, category, "generate.R"), intersect(args, "--report"))
}

fixture <- grep("^--fixture", args, value = TRUE)
if (length(fixture)) {
  from <- sub("^--fixture=?", "", fixture[1])
  run(
    file.path(here, "fixture", "make_fixture.R"),
    if (nzchar(from)) paste0("--from=", shQuote(from))
  )
}
