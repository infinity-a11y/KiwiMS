# Run limits: the sample cap of a run, at and over the limit (LM1). The caps
# on replicate series and replicates per condition are tested in
# ../replicates. See README.md for what each file tests.
#
# Usage (from KiwiMS_App):
#   Rscript tests/edge_cases/run_limits/generate.R [--report]

# kit_files(): The files of this category, by name ----
# Made-up samples: the cap is checked on the config and the Samples table,
# before any spectrum is read.
kit_files <- function(ctx) {
  list(
    config_384_samples = kit_config_synthetic(384),
    config_385_samples = kit_config_synthetic(385)
  )
}

# kit_report(): What the app shows for each test of this category ----
kit_report <- function(ctx) {
  P <- function(n) kit_load("baseline", n)
  for (n in c(384, 385)) {
    name <- sprintf("config_%d_samples", n)
    path <- kit_path("run_limits", name)
    issues <- kit_config_issues(path)
    cat("\n===== LM1", name, "=====\n")
    cat("Config upload", if (length(issues)) "REFUSED" else "ACCEPTED", "\n")
    if (length(issues)) cat(paste0("   ", issues), sep = "\n")
    kit_show(
      sprintf("LM1 Samples table with %d rows", n),
      kit_declare(P("proteins_baseline"), P("compounds_baseline"), kit_read_config(path))
    )
  }
}

if (sys.nframe() == 0L) {
  here <- dirname(normalizePath(sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(FALSE), value = TRUE)
  )))
  options(box.path = normalizePath(file.path(here, "..", "..", "..")))
  source(file.path(here, "..", "_shared.R"))
  kit_main(here, kit_report)
}
