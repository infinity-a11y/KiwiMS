# Edge-case kit, replicates (tests/edge_cases/replicates/README.md): how
# replicate series are named, the checks on replicate groups and the caps on
# series and replicates per condition. The RP tests run the kit's files on the
# fixture; the others pin the rules on synthetic declarations.

box::use(
  app/logic/conversion_functions[compute_replicate_labels, sort_series],
)

B <- function(name) paste0("baseline/", name)
R <- function(name) paste0("replicates/", name)

test_that("the replicate files are written and load like uploads", {
  expect_kit_loads("replicates")
})

# ---- Fixture -----------------------------------------------------------------

test_that("RP1: a replicate declared at another time warns", {
  d <- kit_check(B("proteins_baseline"), B("compounds_baseline"), R("config_rep_mismatch"))
  expect_equal(d$status, "PASS + WARNING")
  expect_equal(d$message, "Replicates declared differently: 1 group")
  expect_equal(d$details, "2026-09-18_MULI+BI-8925_10_20min: Time 20 ↔ 25")
  expect_match(d$note, "^Samples named alike up to _R<n> are read as replicates")

  # One typo moves a sample to another time point
  r <- kit_case(B("proteins_baseline"), B("compounds_baseline"), R("config_rep_mismatch"))
  k <- r$kinetics[["MLKL + BI-8925"]]
  expect_rounded(k$ratio, 335.5)
  expect_rounded(k$series[["R2"]], 343.8)
  expect_rounded(k$proteoforms[["21816.84"]]$ratio, 195.6)
  expect_equal(k$proteoforms[["21816.84"]]$status, "linear")
})

test_that("RP1b: replicate and mass warnings share one hint", {
  d <- kit_check("mass_ambiguity/proteins_two_unbound", B("compounds_baseline"), R("config_rep_mismatch"))
  expect_equal(
    d$message,
    "Ambiguous masses within 2 × peak tolerance (6 Da): 5 pairs · Replicates declared differently: 1 group"
  )
  # Mass pairs first, then the replicate group
  expect_length(d$details, 6)
  expect_equal(d$details[6], "2026-09-18_MULI+BI-8925_10_20min: Time 20 ↔ 25")
})

test_that("RP2: five replicate series are refused", {
  issues <- kit_config_issues(kit_path("replicates", "config_five_series"))
  expect_equal(issues, "'Replicate': at most 4 replicate series (5 found: R1, R2, R3, R4, R5).")

  d <- kit_check(B("proteins_baseline"), B("compounds_baseline"), R("config_five_series"))
  expect_equal(d$status, "BLOCKED")
  expect_equal(d$message, "At most 4 replicate series (5 present)")
  expect_equal(unname(d$details), c("R1: 25 samples", "R2: 25 samples", "R3: 24 samples", "R4: 24 samples", "R5: 24 samples"))
})

test_that("RP3: six replicates of one condition are refused", {
  d <- kit_check(B("proteins_baseline"), B("compounds_baseline"), R("config_six_replicates"))
  expect_equal(d$status, "BLOCKED")
  expect_equal(d$message, "At most 4 replicates per condition (1 condition has up to 6)")
  expect_equal(d$details, "MLKL + BI-8925, concentration 10, time 20: 6 samples")
  expect_match(d$note, "Check for a mistyped concentration or time.$")
})

test_that("RP4: a Replicate value contradicting the file name warns and wins", {
  d <- kit_check(B("proteins_baseline"), B("compounds_baseline"), R("config_rep_swapped"))
  expect_equal(d$status, "PASS + WARNING")
  expect_equal(d$message, "Replicate differs from the file name: 20 samples")
  expect_true("2026-09-18_MULI+BI-8925_10_20min_R1: Replicate R2" %in% d$details)
  expect_match(d$note, "^The Replicate value from the config is used.")

  r <- kit_case(B("proteins_baseline"), B("compounds_baseline"), R("config_rep_swapped"))
  # Every sample enters the global fit on its own: only the series change
  k <- r$kinetics[["MLKL + BI-8925"]]
  expect_rounded(k$ratio, 334.1)
  expect_rounded(k$series[["R1"]], 325.4)
  expect_rounded(k$series[["R2"]], 343.6)
})

test_that("RP5: without Replicate values the file names name the series", {
  d <- kit_check(B("proteins_baseline"), B("compounds_baseline"), R("config_no_replicate"))
  expect_equal(d$status, "PASS")

  r <- kit_case(B("proteins_baseline"), B("compounds_baseline"), R("config_no_replicate"))
  k <- r$kinetics[["MLKL + BI-8925"]]
  expect_rounded(k$series[["R1"]], 327.9)
  expect_rounded(k$series[["R2"]], 341.0)
})

# ---- Synthetic: naming ---------------------------------------------------------

test_that("the series comes from the config, else the _R<n> ending, else nothing", {
  samples <- c("A_R1.raw", "A_R02", "b_r3", "Plain", "A-R1", "A_rep1")
  expect_equal(compute_replicate_labels(samples), c("R1", "R2", "R3", "", "", ""))

  # Partial config: matched per sample, extension ignored, empty values fall
  # back to the name
  config <- data.frame(
    Sample = c("A_R1", "Plain.raw", "b_r3"),
    Replicate = c("Day1", "Day2", "")
  )
  expect_equal(
    compute_replicate_labels(samples, config),
    c("Day1", "R2", "R3", "Day2", "", "")
  )
})

test_that("series sort naturally", {
  expect_equal(sort_series(c("R10", "R2", "R1", "R2")), c("R1", "R2", "R10"))
  # A label without a number comes after the numbered ones of its prefix
  expect_equal(sort_series(c("A", "A10", "A2")), c("A2", "A10", "A"))
})

# kit_replicates(): kit_design() with Replicate values and sample names ----
kit_replicates <- function(replicate, names = NULL) {
  st <- kit_design()
  if (!is.null(names)) st$Sample <- names
  st$Replicate <- rep_len(replicate, nrow(st))
  st
}

test_that("the series cap is 4, with kinact/KI only", {
  expect_true(isTRUE(kit_synthetic_check(kit_replicates(paste0("R", 1:4)))))
  five <- kit_replicates(paste0("R", 1:5))
  expect_equal(as.character(kit_synthetic_check(five)), "At most 4 replicate series (5 present)")
  # Without kinact/KI no per-series fit is made, so nothing is capped
  expect_true(isTRUE(kit_synthetic_check(five[, c("Sample", "Protein", "Compound 1", "Replicate")])))
})

test_that("a Replicate column holding condition names reads as empty", {
  # Tables saved before 0.7.5 held the sample name without _R<n>; the ending
  # names the series instead
  names <- sprintf("C%02d_R%d", seq_len(10), rep_len(1:5, 10))
  legacy <- kit_replicates(sub("_R[0-9]$", "", names), names)
  expect_equal(as.character(kit_synthetic_check(legacy)), "At most 4 replicate series (5 present)")
  legacy$Sample <- sprintf("C%02d_R%d", seq_len(10), rep_len(1:2, 10))
  legacy$Replicate <- sub("_R[0-9]$", "", legacy$Sample)
  expect_true(isTRUE(kit_synthetic_check(legacy)))
})

test_that("the config caps the series it names at 4", {
  cfg <- normalize_config_units(kit_config_synthetic(10))
  cfg$Replicate <- c(paste0("R", 1:4), rep("", 6))
  expect_length(validate_config(cfg), 0)
  cfg$Replicate[5] <- "R5"
  expect_equal(validate_config(cfg), "'Replicate': at most 4 replicate series (5 found: R1, R2, R3, R4, R5).")
})

# ---- Synthetic: replicates per condition ---------------------------------------

test_that("a condition holds at most 4 replicates", {
  with_copies <- function(n) {
    st <- kit_design()
    copies <- st[rep(2, n), ]
    copies$Sample <- sprintf("X%02d", seq_len(n))
    rbind(st, copies)
  }
  expect_true(isTRUE(kit_synthetic_check(with_copies(3))))
  chk <- kit_synthetic_check(with_copies(4))
  expect_equal(as.character(chk), "At most 4 replicates per condition (1 condition has up to 5)")
  expect_equal(attr(chk, "details"), "P + A, concentration 1, time 5: 5 samples")
})

test_that("untreated controls are not capped", {
  st <- kit_design()
  controls <- st[rep(1, 6), ]
  controls$Sample <- sprintf("C%02d", 1:6)
  expect_true(isTRUE(kit_synthetic_check(rbind(st, controls))))
})

test_that("without kinact/KI replicates are the samples named alike", {
  st <- function(n) {
    data.frame(Sample = sprintf("X_R%d", seq_len(n)), Protein = "P", `Compound 1` = "A", check.names = FALSE)
  }
  expect_true(isTRUE(kit_synthetic_check(st(4))))
  chk <- kit_synthetic_check(st(5))
  expect_equal(as.character(chk), "At most 4 replicates per condition (1 condition has up to 5)")
  expect_equal(attr(chk, "note"), "Replicates are samples named alike up to their _R<n> ending.")
})

# ---- Synthetic: replicate groups declared differently -------------------------

pt2 <- data.frame(Protein = c("P", "Q"), `Mass 1` = c(1000, 5000), check.names = FALSE)
ct2 <- data.frame(Compound = c("A", "B"), `Mass 1` = c(100, 300), check.names = FALSE)
group <- function(protein, c1, c2 = c("", "")) {
  data.frame(
    Sample = c("X_R1", "X_R2"),
    Protein = protein,
    `Compound 1` = c1,
    `Compound 2` = c2,
    check.names = FALSE
  )
}

test_that("a replicate group names the first column that differs", {
  chk <- kit_synthetic_check(group("P", c("A", "B")), pt2, ct2)
  expect_equal(attr(chk, "warning"), "Replicates declared differently: 1 group")
  expect_equal(attr(chk, "details"), "X: Compound A ↔ B")
  # Protein comes before Compound
  chk <- kit_synthetic_check(group(c("P", "Q"), c("A", "B")), pt2, ct2)
  expect_equal(attr(chk, "details"), "X: Protein P ↔ Q")
})

test_that("the compounds of a replicate are compared as a set", {
  chk <- kit_synthetic_check(group("P", c("A", "B"), c("B", "A")), pt2, ct2)
  expect_null(attr(chk, "warning"))
})

test_that("samples without an _R<n> ending form no replicate group", {
  st <- group("P", c("A", "B"))
  st$Sample <- c("X1", "X2")
  expect_null(attr(kit_synthetic_check(st, pt2, ct2), "warning"))
  # Nor does a group of one
  st$Sample <- c("X_R1", "Y_R1")
  expect_null(attr(kit_synthetic_check(st, pt2, ct2), "warning"))
})

test_that("with kinact/KI concentration and time are compared too", {
  st <- kit_design()
  st$Sample <- sprintf("C%02d", seq_len(nrow(st)))
  st$Sample[2:3] <- c("Y_R1", "Y_R2")
  expect_equal(attr(kit_synthetic_check(st), "details"), "Y: Time 5 ↔ 10")
})

# ---- Synthetic: Replicate value against the file name --------------------------

test_that("a Replicate value contradicts the name only by its number", {
  st <- data.frame(
    Sample = c("X_R1", "Y_R1", "Z_R1", "W_R01", "V"),
    Protein = "P",
    `Compound 1` = "A",
    Replicate = c("Rep2", "Day", "R1", "R1", "R3"),
    check.names = FALSE
  )
  chk <- kit_synthetic_check(st)
  expect_equal(attr(chk, "warning"), "Replicate differs from the file name: 1 sample")
  expect_equal(attr(chk, "details"), "X_R1: Replicate Rep2")
})
