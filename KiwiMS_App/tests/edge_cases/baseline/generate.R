# Baseline of the edge-case kit: the plain declaration of the MLKL + BI-8925
# series every other category is compared against (B0).
#
# Usage (from KiwiMS_App):
#   Rscript tests/edge_cases/baseline/generate.R [--report]

# kit_files(): The files of this category, by name ----
kit_files <- function(ctx) {
  list(
    proteins_baseline = kit_proteins(21638.84, 21816.84),
    compounds_baseline = kit_compounds("BI-8925", 266),
    config_baseline = kit_config(ctx, "BI-8925")
  )
}

# kit_report(): What the app shows for each test of this category ----
kit_report <- function(ctx) {
  pt <- kit_load("baseline", "proteins_baseline")
  ct <- kit_load("baseline", "compounds_baseline")
  cfg <- kit_load("baseline", "config_baseline")
  kit_show("B0 baseline", kit_declare(pt, ct, cfg), kit_run(ctx, pt, ct, cfg))
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
