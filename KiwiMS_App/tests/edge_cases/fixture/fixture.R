# Builder of the edge-case fixture mlkl_bi8925.db, a result database the app
# opens: the peaks of all 122 samples of kinact_MLKL_3 from the committed
# peak list, and the spectrum, UniDec parameters and error metrics of the two
# samples the manual tests open in the Spectrum view, taken from a
# deconvolution result of the same samples.
#
# Plain R; needs tests/reference_data/reference_data.R sourced first. Used by
# make_fixture.R and the reference tests.

# Samples whose spectrum the manual tests open
kit_spectrum_samples <- c(
  "2026-09-18_MULI+BI-8925_2o5_3min_R1", # reference sample of every category
  "2026-09-18_MULI+BI-8925_2o5_1min_R1" # complex 4.34 Da off (MA5a)
)

# kit_build_fixture(): Write the fixture from a deconvolution result ----
# `source_db` must hold the spectrum samples, deconvolved with the reference
# parameters: their peaks are checked against the peak list, so a result of
# other data or other settings is refused. Spectra are kept from 21,000 to
# 22,600 Da, where every peak of the series lies; the input/output keys of the
# UniDec parameters (temp paths of the machine that ran it) are blanked.
kit_build_fixture <- function(
  source_db,
  out,
  peaks = ref_peaks("kinact_MLKL_3"),
  samples = kit_spectrum_samples
) {
  src <- DBI::dbConnect(RSQLite::SQLite(), source_db, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(src), add = TRUE)
  in_samples <- sprintf(
    "WHERE sample IN (%s)",
    paste(rep("?", length(samples)), collapse = ", ")
  )
  rows <- function(tbl, where = in_samples) {
    DBI::dbGetQuery(
      src,
      sprintf("SELECT * FROM \"%s\" %s", tbl, where),
      params = as.list(samples)
    )
  }

  got <- rows("peaks")
  for (s in samples) {
    a <- got[got$sample == s, ]
    b <- peaks[peaks$sample == s, ]
    a <- a[order(a$mass), ]
    b <- b[order(b$mass), ]
    if (!nrow(a) || !identical(a$mass, b$mass) || any(abs(a$intensity - b$intensity) > 1e-6)) {
      stop(s, " in ", source_db, " does not match the reference peak list")
    }
  }

  config <- rows("config")
  config$value[config$key %in% c("input", "output")] <- ""
  mass <- rows("mass_data", paste(in_samples, "AND mass BETWEEN 21000 AND 22600"))
  error <- rows("error")

  all_samples <- unique(peaks$sample)
  unlink(out)
  con <- DBI::dbConnect(RSQLite::SQLite(), out)
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  write <- function(name, value) DBI::dbWriteTable(con, name, value)
  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  write("metadata", data.frame(sample = all_samples))
  write("status", data.frame(
    sample = all_samples,
    state = "done",
    reason = NA_character_,
    error_msg = NA_character_,
    timestamp = stamp
  ))
  write("peaks", peaks[, c("mass", "intensity", "sample")])
  write("mass_data", mass)
  write("error", error)
  write("config", config)
  write("run_info", data.frame(started_at = stamp, n_samples = length(all_samples)))
  write("completed", data.frame(finished_at = stamp))
  for (tbl in c("peaks", "mass_data", "error", "config")) {
    DBI::dbExecute(con, sprintf("CREATE INDEX idx_%s_sample ON %s(sample)", tbl, tbl))
  }
  DBI::dbExecute(con, "CREATE UNIQUE INDEX idx_status_sample ON status(sample)")
  DBI::dbExecute(con, "VACUUM")
  invisible(out)
}
