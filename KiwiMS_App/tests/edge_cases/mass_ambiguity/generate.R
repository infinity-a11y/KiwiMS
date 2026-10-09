# Mass ambiguities: two interpretations of the declaration that one peak
# cannot tell apart (MA1-MA6). See README.md for what each file tests.
#
# Usage (from KiwiMS_App):
#   Rscript tests/edge_cases/mass_ambiguity/generate.R [--report]

# kit_files(): The files of this category, by name ----
kit_files <- function(ctx) {
  list(
    # MA3, MA6: second unbound mass 5 Da from the main form
    proteins_two_unbound = kit_proteins(21638.84, 21816.84, 21643.84),
    # MA2: third mass = 21638.84 + 266, the main form's complex
    proteins_unbound_is_complex = kit_proteins(21638.84, 21816.84, 21904.84),
    # MA4: 21654.84 + 250 = 21638.84 + 266
    proteins_shared_complex = kit_proteins(21638.84, 21816.84, 21654.84),
    # MA1a, MA1e: a second compound 2 Da off
    compounds_decoy_268 = kit_compounds(c("BI-8925", "DECOY"), 266, 268),
    # MA1b: 2 x 133 = 266
    compounds_decoy_133 = kit_compounds(c("BI-8925", "DECOY"), 266, 133),
    # MA1c, MA1d: 6 Da off, exactly 2 x the 3 Da tolerance
    compounds_decoy_272 = kit_compounds(c("BI-8925", "DECOY"), 266, 272),
    # MA6: 5 Da off, in another row
    compounds_decoy_271 = kit_compounds(c("BI-8925", "DECOY"), 266, 271),
    # MA4: a second mass shift of BI-8925 reaching the shared complex
    compounds_shift_250 = kit_compounds("BI-8925", c(266, 250)),
    # MA5a: two mass shifts of one compound 2 Da apart
    compounds_close_shifts = kit_compounds("BI-8925", c(266, 264)),
    # MA5b: one mass shift close to half the other (2 x 133.5 = 267)
    compounds_near_half_shift = kit_compounds("BI-8925", c(266, 133.5)),
    # MA5c: one mass shift exactly half the other, refused
    compounds_half_shift = kit_compounds("BI-8925", c(266, 133)),
    # MA1a-d: both compounds in every sample
    config_with_decoy = kit_config(ctx, "BI-8925", "DECOY"),
    # MA1e: BI-8925 in the R1 samples, DECOY in the R2 samples
    config_split_by_rep = kit_config(
      ctx,
      ifelse(ctx$rep == "R1", "BI-8925", "DECOY")
    )
  )
}

# kit_report(): What the app shows for each test of this category ----
kit_report <- function(ctx) {
  P <- function(n) kit_load("baseline", n)
  M <- function(n) kit_load("mass_ambiguity", n)
  pt <- P("proteins_baseline")
  ct <- P("compounds_baseline")
  cfg <- P("config_baseline")
  off <- function(...) kit_declare(..., kinetics = FALSE)
  decoy <- M("config_with_decoy")

  kit_show("MA1a decoy 268, kinact/KI off", off(pt, M("compounds_decoy_268"), decoy))
  kit_show("MA1a decoy 268, kinact/KI on", kit_declare(pt, M("compounds_decoy_268"), decoy))
  kit_show("MA1b decoy 133, max. stoichiometry 4", off(pt, M("compounds_decoy_133"), decoy))
  kit_show("MA1b decoy 133, max. stoichiometry 1", off(pt, M("compounds_decoy_133"), decoy, maxm = 1))
  kit_show("MA1c decoy 272, tolerance 3", off(pt, M("compounds_decoy_272"), decoy))
  kit_show("MA1c decoy 272, tolerance 2.9", off(pt, M("compounds_decoy_272"), decoy, tol = 2.9))
  split <- M("config_split_by_rep")
  kit_show(
    "MA1e decoy 268 in the R2 samples",
    kit_declare(pt, M("compounds_decoy_268"), split),
    kit_run(ctx, pt, M("compounds_decoy_268"), split)
  )

  pt2 <- M("proteins_unbound_is_complex")
  kit_show("MA2 unbound 21904.84 = complex", kit_declare(pt2, ct, cfg), kit_run(ctx, pt2, ct, cfg))
  pt3 <- M("proteins_two_unbound")
  kit_show("MA3 two unbound 5 Da apart", kit_declare(pt3, ct, cfg), kit_run(ctx, pt3, ct, cfg))

  c250 <- M("compounds_shift_250")
  kit_show("MA4 control: baseline proteins, 266/250", kit_declare(pt, c250, cfg), kit_run(ctx, pt, c250, cfg))
  pt4 <- M("proteins_shared_complex")
  kit_show("MA4 21654.84 + 250 = 21638.84 + 266", kit_declare(pt4, c250, cfg), kit_run(ctx, pt4, c250, cfg))

  c5a <- M("compounds_close_shifts")
  kit_show("MA5a shifts 266/264", kit_declare(pt, c5a, cfg), kit_run(ctx, pt, c5a, cfg))
  c5b <- M("compounds_near_half_shift")
  kit_show("MA5b shifts 266/133.5", kit_declare(pt, c5b, cfg), kit_run(ctx, pt, c5b, cfg))
  c5c <- M("compounds_half_shift")
  kit_show("MA5c shifts 266/133 (Compounds table)", list(
    status = if (isTRUE(check_table(c5c, 3, "compounds"))) "PASS" else "BLOCKED",
    message = as.character(check_table(c5c, 3, "compounds"))
  ))

  kit_show("MA6 colouring files (declaration)", kit_declare(pt3, M("compounds_decoy_271"), cfg))
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
