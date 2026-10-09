# Protein-compound complexes: which compounds a sample is screened for, and
# the design rules k_obs and kinact/KI apply per complex (CX1-CX4). See
# README.md for what each file tests.
#
# Usage (from KiwiMS_App):
#   Rscript tests/edge_cases/complexes/generate.R [--report]

# kit_files(): The files of this category, by name ----
kit_files <- function(ctx) {
  # CX2: BI-8926 on a few 10 µM samples only, one replicate pair split
  mixed <- ctx$conc == 10 &
    (ctx$time %in% c(1, 10, 15) | (ctx$time == 20 & ctx$rep == "R1"))
  # CX3a/b: BI-8926 owns 2.5, 5 and 10 µM
  by_conc <- ctx$conc %in% c(2.5, 5, 10)
  # CX3c: ... but at 10 µM only the 1 and 3 min samples
  short <- ctx$conc %in% c(2.5, 5) | (ctx$conc == 10 & ctx$time %in% c(1, 3))

  list(
    # CX1, CX4: a second compound far from BI-8925
    compounds_decoy_first = kit_compounds(c("BI-8925", "DECOY"), 266, 500),
    # CX2, CX3a, CX3c: one molecule under two names
    compounds_two_names = kit_compounds(c("BI-8925", "BI-8926"), 266, 266),
    # CX3b: BI-8926 at a mass that gives no hit
    compounds_8926_no_hits = kit_compounds(c("BI-8925", "BI-8926"), 266, 500),
    # CX1: DECOY as Compound 1, BI-8925 as Compound 2
    config_decoy_first = kit_config(ctx, "DECOY", "BI-8925"),
    config_mixed_10uM = kit_config(ctx, ifelse(mixed, "BI-8926", "BI-8925")),
    config_split_by_conc = kit_config(ctx, ifelse(by_conc, "BI-8926", "BI-8925")),
    config_split_short_times = kit_config(ctx, ifelse(short, "BI-8926", "BI-8925")),
    # CX4: only Compound 2 filled
    config_compound_2_only = kit_config(ctx, "", "BI-8925")
  )
}

# kit_report(): What the app shows for each test of this category ----
kit_report <- function(ctx) {
  P <- function(n) kit_load("baseline", n)
  X <- function(n) kit_load("complexes", n)
  pt <- P("proteins_baseline")

  cx1 <- X("config_decoy_first")
  kit_show(
    "CX1 DECOY listed first, kinact/KI off",
    kit_declare(pt, X("compounds_decoy_first"), cx1, kinetics = FALSE),
    kit_run(ctx, pt, X("compounds_decoy_first"), cx1, kinetics = FALSE)
  )
  two <- X("compounds_two_names")
  kit_show("CX2 BI-8926 on a few 10 uM samples", kit_declare(pt, two, X("config_mixed_10uM")))
  by_conc <- X("config_split_by_conc")
  kit_show("CX3a BI-8926 owns 2.5/5/10 uM", kit_declare(pt, two, by_conc), kit_run(ctx, pt, two, by_conc))
  none <- X("compounds_8926_no_hits")
  kit_show("CX3b BI-8926 at 500 Da (no hits)", kit_declare(pt, none, by_conc), kit_run(ctx, pt, none, by_conc))
  kit_show("CX3c BI-8926 at 10 uM only at 1/3 min", kit_declare(pt, two, X("config_split_short_times")))
  decoy <- kit_load("mass_ambiguity", "config_with_decoy")
  kit_show("CX4 two compounds per sample, kinact/KI on", kit_declare(pt, X("compounds_decoy_first"), decoy))
  kit_show("CX4 two compounds per sample, kinact/KI off", kit_declare(pt, X("compounds_decoy_first"), decoy, kinetics = FALSE))
  kit_show("CX4 Compound 2 only", kit_declare(pt, P("compounds_baseline"), X("config_compound_2_only")))
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
