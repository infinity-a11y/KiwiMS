# Replicates and replicate series: how they are named, checked and capped
# (RP1-RP4). See README.md for what each file tests.
#
# Usage (from KiwiMS_App):
#   Rscript tests/edge_cases/replicates/generate.R [--report]

# kit_files(): The files of this category, by name ----
kit_files <- function(ctx) {
  # RP1: R2 of 10 µM / 20 min declared at 25 min
  typo <- ctx$time
  typo[ctx$conc == 10 & ctx$time == 20 & ctx$rep == "R2"] <- 25
  # RP3: the 10 µM samples at 30 and 40 min declared at 20 min
  six <- ctx$time
  six[ctx$conc == 10 & ctx$time %in% c(30, 40)] <- 20
  # RP4: Replicate R1 and R2 swapped for the 10 µM samples
  swapped <- ctx$rep
  at10 <- ctx$conc == 10
  swapped[at10] <- ifelse(ctx$rep[at10] == "R1", "R2", "R1")

  list(
    config_rep_mismatch = kit_config(ctx, "BI-8925", time = typo),
    # RP2: Replicate R1-R5 in turn, five series
    config_five_series = kit_config(
      ctx,
      "BI-8925",
      replicate = paste0("R", (seq_along(ctx$samples) - 1) %% 5 + 1)
    ),
    config_six_replicates = kit_config(ctx, "BI-8925", time = six),
    config_rep_swapped = kit_config(ctx, "BI-8925", replicate = swapped),
    # RP5: no Replicate values, the _R<n> endings name the series
    config_no_replicate = kit_config(ctx, "BI-8925", replicate = "")
  )
}

# kit_report(): What the app shows for each test of this category ----
kit_report <- function(ctx) {
  P <- function(n) kit_load("baseline", n)
  R <- function(n) kit_load("replicates", n)
  pt <- P("proteins_baseline")
  ct <- P("compounds_baseline")
  upload <- function(name) {
    issues <- kit_config_issues(kit_path("replicates", name))
    cat("Config upload", name, if (length(issues)) "REFUSED" else "ACCEPTED", "\n")
    if (length(issues)) cat(paste0("   ", issues), sep = "\n")
  }

  mismatch <- R("config_rep_mismatch")
  kit_show("RP1 R2 of 10 uM / 20 min declared at 25 min", kit_declare(pt, ct, mismatch), kit_run(ctx, pt, ct, mismatch))
  kit_show(
    "RP1b same with proteins_two_unbound",
    kit_declare(kit_load("mass_ambiguity", "proteins_two_unbound"), ct, mismatch)
  )
  cat("\n===== RP2 five replicate series =====\n")
  upload("config_five_series")
  kit_show("RP2 same table declared anyway", kit_declare(pt, ct, R("config_five_series")))
  kit_show("RP3 six replicates at 10 uM / 20 min", kit_declare(pt, ct, R("config_six_replicates")))
  swapped <- R("config_rep_swapped")
  kit_show("RP4 Replicate swapped for the 10 uM samples", kit_declare(pt, ct, swapped), kit_run(ctx, pt, ct, swapped))
  plain <- R("config_no_replicate")
  kit_show("RP5 Replicate column empty", kit_declare(pt, ct, plain), kit_run(ctx, pt, ct, plain))
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
