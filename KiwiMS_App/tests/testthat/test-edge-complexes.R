# Edge-case kit, protein-compound complexes
# (tests/edge_cases/complexes/README.md): which compounds a sample is screened
# for, the one-compound rule of kinact/KI, and the design rules applied per
# complex. The CX tests run the kit's files on the fixture; the others pin the
# rules on synthetic declarations.

box::use(
  app/logic/conversion_functions[is_complex_row, run_complexes],
)

B <- function(name) paste0("baseline/", name)
X <- function(name) paste0("complexes/", name)

test_that("the complex files are written and load like uploads", {
  expect_kit_loads("complexes")
})

# ---- Fixture -----------------------------------------------------------------

test_that("CX1: every compound of a sample is screened, whatever its column", {
  d <- kit_check(B("proteins_baseline"), X("compounds_decoy_first"), X("config_decoy_first"), kinetics = FALSE)
  expect_equal(d$status, "PASS")

  base <- kit_case(B("proteins_baseline"), B("compounds_baseline"), B("config_baseline"))
  r <- kit_case(B("proteins_baseline"), X("compounds_decoy_first"), X("config_decoy_first"), kinetics = FALSE)
  # Before the fix no sample got a BI-8925 hit: a compound was only screened
  # when its column matched its row of the compound table
  expect_equal(kit_card(r, "BI-8925")[["samples"]], 120)
  expect_rounded(kit_card(r, "BI-8925")[["mean"]], 70.24, 2)
  expect_equal(r$total, base$total[names(r$total)])
  # DECOY is declared, so its complex is listed although it has no hit
  expect_equal(r$complexes, c("MLKL + BI-8925", "MLKL + DECOY"))
})

test_that("CX2: a compound on a few samples fails the design of its complex", {
  d <- kit_check(B("proteins_baseline"), X("compounds_two_names"), X("config_mixed_10uM"))
  expect_equal(d$status, "BLOCKED")
  expect_equal(d$message, "MLKL + BI-8926: At least 3 different non-zero concentrations required (1 present)")
})

test_that("CX3a: two complexes, one per concentration range, are fitted apart", {
  d <- kit_check(B("proteins_baseline"), X("compounds_two_names"), X("config_split_by_conc"))
  expect_equal(d$status, "PASS")
  expect_null(d$message)

  base <- kit_case(B("proteins_baseline"), B("compounds_baseline"), B("config_baseline"))
  r <- kit_case(B("proteins_baseline"), X("compounds_two_names"), X("config_split_by_conc"))
  # Every sample reads as in the baseline, under its own compound's name
  expect_equal(r$total, base$total[names(r$total)])
  expect_equal(kit_sample_hits(r)$Compound[1], "BI-8926")

  a <- kit_card(r, "BI-8925")
  b <- kit_card(r, "BI-8926")
  expect_rounded(a[["min"]], 0, 2)
  expect_rounded(a[["mean"]], 81.63, 2)
  expect_rounded(a[["sd"]], 21.97, 2)
  expect_rounded(b[["min"]], 0, 2)
  expect_rounded(b[["max"]], 92.78, 2)
  expect_rounded(b[["mean"]], 58.48, 2)
  expect_rounded(b[["sd"]], 26.54, 2)
  # Mass Shifts card: samples with a hit of each
  expect_equal(c(a[["samples"]], b[["samples"]]), c(61, 59))

  expect_equal(r$complexes, c("MLKL + BI-8925", "MLKL + BI-8926"))
  expect_equal(r$default, "MLKL + BI-8925")
  k1 <- r$kinetics[["MLKL + BI-8925"]]
  k2 <- r$kinetics[["MLKL + BI-8926"]]
  # BI-8925 keeps the 0 µM controls, BI-8926 has none
  expect_equal(c(k1$n, k2$n), c(62, 60))
  expect_rounded(k1$ratio, 224.3)
  expect_rounded(k1$ci[[1]], 199.9)
  expect_rounded(k1$ci[[2]], 251.0)
  expect_equal(k1$status, "linear")
  expect_equal(k1$warnings, "Saturation not reached")
  expect_rounded(k1$series[["R1"]], 220.2)
  expect_rounded(k1$series[["R2"]], 228.6)
  expect_rounded(k2$ratio, 330.4)
  expect_rounded(k2$ci[[1]], 306.9)
  expect_rounded(k2$ci[[2]], 357.2)
  expect_equal(k2$status, "linear")
})

test_that("CX3b: a declared complex without hits stays in the picker", {
  expect_equal(kit_check(B("proteins_baseline"), X("compounds_8926_no_hits"), X("config_split_by_conc"))$status, "PASS")

  r <- kit_case(B("proteins_baseline"), X("compounds_8926_no_hits"), X("config_split_by_conc"))
  # Its samples list it as a pair without an adduct, never as a complex
  expect_true("BI-8926" %in% r$hits$Compound)
  expect_false("BI-8926" %in% r$hits$Compound[is_complex_row(r$hits)])
  expect_rounded(kit_card(r, "BI-8925")[["mean"]], 81.63, 2)
  expect_rounded(kit_all_samples(r), 41.48, 2)

  expect_equal(r$complexes, c("MLKL + BI-8925", "MLKL + BI-8926"))
  none <- r$kinetics[["MLKL + BI-8926"]]
  expect_equal(none$reason, "No hits of BI-8926 in its samples")
  expect_true(is.na(none$ratio))
  expect_true(any(grepl("MLKL + BI-8926 (60 samples)", r$log, fixed = TRUE)))
  expect_true(any(grepl(
    "No hits of BI-8926 in its samples. Skipping binding kinetics analysis.",
    r$log,
    fixed = TRUE
  )))
  expect_rounded(r$kinetics[["MLKL + BI-8925"]]$ratio, 224.3)
})

test_that("CX3c: the time points are counted per complex", {
  d <- kit_check(B("proteins_baseline"), X("compounds_two_names"), X("config_split_short_times"))
  expect_equal(d$status, "BLOCKED")
  expect_equal(
    d$message,
    "MLKL + BI-8926: At least 3 different non-zero time points required per concentration (concentration 10 has only 2)"
  )
})

test_that("CX4: kinact/KI takes one compound per sample", {
  decoy <- "mass_ambiguity/config_with_decoy"
  on <- kit_check(B("proteins_baseline"), X("compounds_decoy_first"), decoy)
  expect_equal(on$status, "BLOCKED")
  expect_equal(on$message, "One compound per sample for kinact/KI (122 samples list several)")
  expect_length(on$details, 13)
  expect_equal(on$details[1], "2026-09-18_MULI+BI-8925_0_0min_R2: BI-8925, DECOY")
  expect_equal(on$details[13], "and 110 more")
  expect_match(on$note, "Screen compound mixtures with kinact/KI switched off.$")

  expect_equal(kit_check(B("proteins_baseline"), X("compounds_decoy_first"), decoy, kinetics = FALSE)$status, "PASS")
  # Compound 2 alone is one compound
  expect_equal(kit_check(B("proteins_baseline"), B("compounds_baseline"), X("config_compound_2_only"))$status, "PASS")
})

# ---- Synthetic: the design rules per complex ----------------------------------

test_that("the design rules hold at their boundaries", {
  expect_true(isTRUE(kit_synthetic_check(kit_design())))

  expect_equal(
    kit_synthetic_check(kit_design(conc = c(1, 2))),
    "At least 3 different non-zero concentrations required (2 present)"
  )
  expect_equal(
    kit_synthetic_check(kit_design(time = c(5, 10))),
    "At least 3 different non-zero time points required per concentration (concentration 1 has only 2)"
  )
  # Time 0 is no time point
  expect_match(kit_synthetic_check(kit_design(time = c(0, 5, 10))), "concentration 1 has only 2")
  # Ten concentrations pass, eleven do not
  expect_true(isTRUE(kit_synthetic_check(kit_design(conc = 1:10))))
  expect_equal(
    kit_synthetic_check(kit_design(conc = 1:11)),
    "At most 10 different non-zero concentrations allowed (11 present)"
  )
})

test_that("the untreated control needs no time course", {
  # A single 0 µM sample, already part of kit_design(), and several at time 0
  st <- kit_design()
  extra <- st[1, ]
  extra$Sample <- "S99"
  extra$`Time [min]` <- 30
  expect_true(isTRUE(kit_synthetic_check(rbind(st, extra))))
})

test_that("each complex must meet the design on its own", {
  st <- rbind(kit_design(), kit_design(conc = c(1, 2), compound = "B", prefix = "T"))
  ct <- data.frame(Compound = c("A", "B"), `Mass 1` = c(100, 300), check.names = FALSE)
  expect_equal(
    kit_synthetic_check(st, compounds = ct),
    "P + B: At least 3 different non-zero concentrations required (2 present)"
  )
  # Over the whole table the same samples count three concentrations
  st$`Compound 1` <- "A"
  expect_true(isTRUE(kit_synthetic_check(st, compounds = ct)))
})

test_that("each protein forms complexes of its own", {
  st <- rbind(kit_design(), kit_design(protein = "Q", prefix = "T"))
  pt <- data.frame(Protein = c("P", "Q"), `Mass 1` = c(1000, 5000), check.names = FALSE)
  expect_true(isTRUE(kit_synthetic_check(st, proteins = pt)))
  # Q with too few concentrations is named in the message
  st <- rbind(kit_design(), kit_design(conc = c(1, 2), protein = "Q", prefix = "T"))
  expect_match(kit_synthetic_check(st, proteins = pt), "^Q \\+ A: At least 3")
})

test_that("missing concentrations or times are asked for", {
  st <- kit_design()
  st$`Concentration [μM]`[3] <- NA
  expect_equal(kit_synthetic_check(st), "Fill Concentrations")
  st <- kit_design()
  st$`Time [min]`[3] <- NA
  expect_equal(kit_synthetic_check(st), "Fill Time")
})

test_that("names are checked against the declared proteins and compounds", {
  st <- kit_design()[, c("Sample", "Protein", "Compound 1")]
  check <- function(st) kit_synthetic_check(st)

  bad <- st
  bad$Protein[2] <- "X"
  expect_equal(check(bad), "Protein name not declared")
  bad <- st
  bad$`Compound 1`[2] <- "X"
  expect_equal(check(bad), "Compound name not declared")
  # Names are matched as typed: a trailing space is another name
  bad$`Compound 1`[2] <- "A "
  expect_equal(check(bad), "Compound name not declared")
  bad <- st
  bad$Protein[2] <- ""
  expect_equal(check(bad), "Assign proteins")
  bad <- st
  bad$`Compound 1`[2] <- ""
  expect_equal(check(bad), "Assign compounds")
  bad <- st
  bad$`Compound 2` <- "A"
  expect_equal(check(bad), "Duplicated compounds")
  expect_equal(check_sample_table(st, NULL, "A"), "Declare Proteins and Compounds")
})

test_that("without kinact/KI neither the design nor the one-compound rule applies", {
  st <- data.frame(Sample = "S1", Protein = "P", `Compound 1` = "A", `Compound 2` = "B", check.names = FALSE)
  ct <- data.frame(Compound = c("A", "B"), `Mass 1` = c(100, 300), check.names = FALSE)
  expect_true(isTRUE(kit_synthetic_check(st, compounds = ct)))
})

test_that("the complex picker lists hits and declared complexes", {
  hits <- data.frame(
    Sample = c("S1", "S2"),
    Protein = c("P", "P"),
    Compound = c("A", NA)
  )
  st <- data.frame(
    Sample = c("S1", "S2", "S3"),
    Protein = c("P", "P", "Q"),
    `Compound 1` = c("A", "B", "A"),
    Replicate = "R1",
    check.names = FALSE
  )
  expect_equal(run_complexes(hits)$key, "P + A")
  expect_equal(run_complexes(hits, st)$key, c("P + A", "P + B", "Q + A"))
})
