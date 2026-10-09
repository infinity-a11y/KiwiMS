# Edge-case kit, mass ambiguities (tests/edge_cases/mass_ambiguity/README.md):
# interpretations of the declaration that one peak cannot tell apart. The MA
# tests run the kit's files on the fixture; the others pin the rules on
# synthetic peak lists.

box::use(
  app/logic/conversion_functions[
    add_hits,
    check_table,
    declaration_ambiguities,
    hit_preference_rule,
    is_complex_row,
    log_mass_ambiguities,
    mass_ambiguities,
    prefer_hits,
    prot_comp_handsontable,
    shift_multiples,
    shift_multiples_message,
    show_preferred_column,
    summarize_hits,
    unbound_species
  ],
  app/logic/conversion_constants[hit_preference_rules, shift_multiple_limits],
)

B <- function(name) paste0("baseline/", name)
M <- function(name) paste0("mass_ambiguity/", name)

# synth_hits(): Hits of one synthetic sample ----
# `compounds` maps each compound to its mass shifts; the sample lists all of
# them unless `sample_compounds` says otherwise. `preference` is the rule for
# the preferred assignment. With `log = TRUE` the messages of the run are
# returned instead of the hits.
synth_hits <- function(
  mass,
  intensity,
  protein_masses,
  compounds = list(A = 100),
  sample_compounds = names(compounds),
  tolerance = 3,
  max_multiples = 1,
  preference = "stoichiometry",
  log = FALSE
) {
  pt <- data.frame(Protein = "P")
  for (i in seq_along(protein_masses)) pt[[paste("Mass", i)]] <- protein_masses[i]
  ct <- data.frame(Compound = names(compounds))
  for (i in seq_len(max(lengths(compounds)))) {
    ct[[paste("Mass", i)]] <- vapply(
      compounds,
      function(x) if (i <= length(x)) x[i] else NA_real_,
      numeric(1)
    )
  }
  st <- data.frame(Sample = "S1", Protein = "P", check.names = FALSE)
  for (i in seq_along(sample_compounds)) st[[paste("Compound", i)]] <- sample_compounds[i]

  result <- list(deconvolution = list(
    S1 = list(peaks = data.frame(mass = mass, intensity = intensity))
  ))
  session <- list(sendCustomMessage = function(...) invisible(NULL))
  run <- function() {
    r <- add_hits(
      result, st, pt, ct, tolerance, max_multiples, session, identity,
      preference = preference
    )
    summarize_hits(r, st)
  }
  if (log) {
    msgs <- character(0)
    withCallingHandlers(run(), message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
    return(msgs)
  }
  suppressMessages(run())
}

# preferred_reading(): Mass shift and stoichiometry of the preferred hit ----
preferred_reading <- function(hits) {
  pref <- hits[hits$Preferred %in% TRUE & !is.na(hits$Compound), ]
  c(pref$`Compound Mw [Da]`, pref$`Binding Stoichiometry`)
}

compound_table <- function(...) {
  x <- list(...)
  d <- data.frame(Compound = names(x))
  for (i in seq_len(max(lengths(x)))) {
    d[[paste("Mass", i)]] <- vapply(x, function(m) if (i <= length(m)) m[i] else NA_real_, 1)
  }
  d
}

test_that("the mass ambiguity files are written and load like uploads", {
  expect_kit_loads("mass_ambiguity")
})

# ---- MA1: two compounds of one sample --------------------------------------

test_that("MA1a: compounds of one sample 2 Da apart are refused", {
  d <- kit_check(B("proteins_baseline"), M("compounds_decoy_268"), M("config_with_decoy"), kinetics = FALSE)
  expect_equal(d$status, "BLOCKED")
  expect_equal(
    d$message,
    "Compounds of one sample are not distinguishable within 2 \u00d7 peak tolerance (6 Da): BI-8925 \u2194 DECOY"
  )
  # One pair per stoichiometry the window still covers: x1 (2 Da), x2 (4 Da)
  # and x3 (6 Da), on each of the two forms
  expect_length(d$details, 6)
  expect_equal(
    d$details[1],
    "21,638.8 Da + BI-8925 (266.0 Da \u00d71) \u2194 21,638.8 Da + DECOY (268.0 Da \u00d71) (\u0394 2.0 Da)"
  )
  expect_true(any(grepl("\u00d73\\) \\(\u0394 6.0 Da\\)$", d$details)))
  expect_equal(d$note, "Assign them to separate samples or revise the mass shifts.")

  # With kinact/KI on the one-compound rule refuses the table first
  on <- kit_check(B("proteins_baseline"), M("compounds_decoy_268"), M("config_with_decoy"))
  expect_match(on$message, "^One compound per sample for kinact/KI")
})

test_that("MA1b: a compound at half the mass collides through stoichiometry", {
  check <- function(maxm) {
    kit_check(B("proteins_baseline"), M("compounds_decoy_133"), M("config_with_decoy"), kinetics = FALSE, maxm = maxm)
  }
  blocked <- check(4)
  expect_equal(blocked$status, "BLOCKED")
  expect_match(blocked$details[1], "DECOY \\(133.0 Da \u00d72\\) \\(\u0394 0.0 Da\\)$")
  expect_equal(check(1)$status, "PASS")
  expect_equal(check(2)$status, "BLOCKED")
})

test_that("MA1c: the window includes twice the tolerance", {
  check <- function(tol) {
    kit_check(B("proteins_baseline"), M("compounds_decoy_272"), M("config_with_decoy"), kinetics = FALSE, tol = tol)
  }
  at <- check(3)
  expect_equal(at$status, "BLOCKED")
  expect_match(at$details[1], "\\(\u0394 6.0 Da\\)$")
  expect_equal(check(2.9)$status, "PASS")
})

test_that("MA1d: the run is refused when the tolerance grew after the check", {
  # The run-start guard re-reads the declaration with the run's settings
  st <- kit_sample_table(kit_load("mass_ambiguity", "config_with_decoy"), kinetics = FALSE)
  pt <- kit_load("baseline", "proteins_baseline")
  ct <- kit_load("mass_ambiguity", "compounds_decoy_272")
  guard <- function(tol) {
    any(declaration_ambiguities(st, pt, ct, tolerance = tol, max_multiples = 4)$kind == "compounds")
  }
  expect_false(guard(2.9))
  expect_true(guard(3))
})

test_that("MA1e: compounds in separate samples are not checked against each other", {
  d <- kit_check(B("proteins_baseline"), M("compounds_decoy_268"), M("config_split_by_rep"))
  expect_equal(d$status, "PASS + WARNING")
  # The only warning is the replicate one: R1 and R2 now name other compounds
  expect_equal(d$message, "Replicates declared differently: 61 groups")
  expect_true("2026-09-18_MULI+BI-8925_10_30min: Compound DECOY \u2194 BI-8925" %in% d$details)

  r <- kit_case(B("proteins_baseline"), M("compounds_decoy_268"), M("config_split_by_rep"))
  a <- kit_card(r, "BI-8925")
  b <- kit_card(r, "DECOY")
  expect_rounded(a[["min"]], 0, 2)
  expect_rounded(a[["mean"]], 69.76, 2)
  expect_rounded(a[["sd"]], 27.61, 2)
  expect_rounded(b[["min"]], 0, 2)
  expect_rounded(b[["mean"]], 61.49, 2)
  expect_rounded(b[["sd"]], 34.33, 2)
  expect_rounded(kit_all_samples(r), 65.69, 2)
  expect_length(kit_no_hits(r), 13)
  # Two of them are not measured: fully converted at 80 µM, their complex
  # more than 3 Da off DECOY and no unbound peak left - binding is NA, not 0,
  # and they stay out of DECOY's kinetics
  expect_equal(
    names(r$total)[is.na(r$total)],
    c(
      "2026-09-18_MULI+BI-8925_80_40min_R2",
      "2026-09-18_MULI+BI-8925_80_50min_R2"
    )
  )

  expect_equal(r$complexes, c("MLKL + BI-8925", "MLKL + DECOY"))
  expect_equal(r$default, "MLKL + BI-8925")
  k <- r$kinetics[["MLKL + BI-8925"]]
  expect_equal(k$n, 61)
  expect_rounded(k$ratio, 327.9)
  expect_rounded(k$ci[[1]], 281.2)
  expect_rounded(k$ci[[2]], 387.5)
  expect_setequal(k$warnings, c("Plateaus differ", "Early 0 % readings"))
  # The DECOY mass picks up the BI-8925 complex 2.8-3.3 Da off: a plausible
  # kinact/KI for a compound that is not there, flagged only by its wide CI
  # and the missing saturation
  decoy <- r$kinetics[["MLKL + DECOY"]]
  expect_equal(decoy$n, 61)
  expect_rounded(decoy$ratio, 277.0)
  expect_rounded(decoy$ci[[1]], 177.7)
  expect_rounded(decoy$ci[[2]], 438.3)
  expect_equal(decoy$status, "linear")
  expect_true("Saturation not reached" %in% decoy$warnings)
  # Each complex holds one series only: no per-series fits
  expect_null(k$series)
  expect_null(decoy$series)
})

# ---- MA2-MA4: proteoforms ----------------------------------------------------

test_that("MA2: an unbound mass equal to a complex is read as unbound", {
  d <- kit_check(M("proteins_unbound_is_complex"), B("compounds_baseline"), B("config_baseline"))
  expect_equal(d$status, "PASS + WARNING")
  expect_equal(d$message, "Ambiguous masses within 2 \u00d7 peak tolerance (6 Da): 4 pairs")
  expect_equal(
    d$details[1],
    "21,904.8 Da unbound \u2194 21,638.8 Da + BI-8925 (266.0 Da \u00d71) (\u0394 0.0 Da)"
  )

  # The run starts with the rule applied to each pair, narrow enough for the
  # log panel and closed by a blank line before the first sample
  st <- kit_sample_table(kit_load("baseline", "config_baseline"))
  amb <- declaration_ambiguities(
    st,
    kit_load("mass_ambiguity", "proteins_unbound_is_complex"),
    kit_load("baseline", "compounds_baseline"),
    tolerance = 3,
    max_multiples = 4
  )
  block <- character(0)
  withCallingHandlers(
    log_mass_ambiguities(amb, 3),
    message = function(m) {
      block <<- c(block, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  block <- strsplit(gsub("\033\\[[0-9;]*m", "", paste(block, collapse = "")), "\n")[[1]]
  expect_equal(block[1], "AMBIGUOUS MASS ASSIGNMENTS (within 2 \u00d7 3 Da)")
  expect_equal(sum(grepl("read as unbound \\(\u0394 0.0 Da\\)$", block)), 1)
  expect_equal(sum(grepl("split evenly \\(\u0394 0.0 Da\\)$", block)), 3)
  expect_lte(max(nchar(block)), 45)
  expect_equal(block[length(block)], "")

  r <- kit_case(M("proteins_unbound_is_complex"), B("compounds_baseline"), B("config_baseline"))
  # Every sample with the complex peak logs the reading
  expect_equal(sum(grepl("read as unbound 21,904.8 Da, also fits", r$log)), 120)

  card <- kit_card(r, "BI-8925")
  expect_rounded(card[["min"]], 0, 2)
  expect_rounded(card[["max"]], 21.33, 2)
  expect_rounded(card[["mean"]], 14.47, 2)
  expect_rounded(card[["sd"]], 5.99, 2)
  expect_rounded(kit_all_samples(r), 14.47, 2)
  expect_equal(r$total[[kit_reference_sample]], 0)

  k <- r$kinetics[["MLKL + BI-8925"]]
  expect_rounded(k$ratio, 290.3)
  expect_rounded(k$ci[[1]], 257.5)
  expect_rounded(k$ci[[2]], 342.6)
  # The species carrying the most signal never binds, so it cannot be the
  # reference of the Proteoforms tab
  expect_equal(k$reference, 21816.84)
  for (p in c("21638.84", "21904.84")) {
    expect_true(is.na(k$proteoforms[[p]]$ratio))
    expect_equal(k$proteoforms[[p]]$reason, "No binding at any concentration")
    expect_true(k$proteoforms[[p]]$flagged)
  }
  expect_equal(c(k$proteoforms[["21638.84"]]$n_limit, k$proteoforms[["21638.84"]]$n), c(107, 107))
  expect_equal(c(k$proteoforms[["21904.84"]]$n_limit, k$proteoforms[["21904.84"]]$n), c(119, 119))
  expect_rounded(k$proteoforms[["21816.84"]]$ratio, 231.2)
})

test_that("MA3: two unbound masses 5 Da apart warn but change nothing", {
  d <- kit_check(M("proteins_two_unbound"), B("compounds_baseline"), B("config_baseline"))
  expect_equal(d$message, "Ambiguous masses within 2 \u00d7 peak tolerance (6 Da): 5 pairs")
  expect_equal(d$details[1], "21,638.8 Da unbound \u2194 21,643.8 Da unbound (\u0394 5.0 Da)")

  base <- kit_case(B("proteins_baseline"), B("compounds_baseline"), B("config_baseline"))
  r <- kit_case(M("proteins_two_unbound"), B("compounds_baseline"), B("config_baseline"), kinetics = FALSE)
  expect_equal(r$total, base$total[names(r$total)])
})

test_that("MA4: a complex shared by two proteoforms is split between them", {
  ctrl_d <- kit_check(B("proteins_baseline"), M("compounds_shift_250"), B("config_baseline"))
  expect_equal(ctrl_d$status, "PASS")
  base <- kit_case(B("proteins_baseline"), B("compounds_baseline"), B("config_baseline"))
  ctrl <- kit_case(B("proteins_baseline"), M("compounds_shift_250"), B("config_baseline"), kinetics = FALSE)
  expect_equal(ctrl$total, base$total[names(ctrl$total)])

  d <- kit_check(M("proteins_shared_complex"), M("compounds_shift_250"), B("config_baseline"))
  expect_equal(d$message, "Ambiguous masses within 2 \u00d7 peak tolerance (6 Da): 1 pair")
  expect_equal(
    d$details,
    "21,638.8 Da + BI-8925 (266.0 Da \u00d71) \u2194 21,654.8 Da + BI-8925 (250.0 Da \u00d71) (\u0394 0.0 Da)"
  )

  r <- kit_case(M("proteins_shared_complex"), M("compounds_shift_250"), B("config_baseline"))
  ref <- kit_sample_hits(r)
  shared <- ref[ref$`Peak [Da]` == 21903.5, ]
  expect_equal(shared$`Mw Protein [Da]`, c(21638.84, 21654.84))
  expect_equal(shared$`Compound Mw [Da]`, c(266, 250))
  expect_rounded(100 * shared$`% Binding`[1], 6.02, 2)
  expect_equal(shared$`% Binding`[1], shared$`% Binding`[2])
  expect_rounded(r$total[[kit_reference_sample]], 12.04, 2)
  # The split never counts the peak twice
  expect_equal(r$total, base$total[names(r$total)])

  card <- kit_card(r, "BI-8925")
  expect_rounded(card[["mean"]], 70.24, 2)
  expect_rounded(card[["sd"]], 26.86, 2)
  k <- r$kinetics[["MLKL + BI-8925"]]
  expect_rounded(k$ratio, 334.1)
  expect_rounded(k$proteoforms[["21638.84"]]$ratio, 210.4)
  third <- k$proteoforms[["21654.84"]]
  expect_true(is.na(third$ratio))
  expect_equal(third$reason, "Binding at 2 concentrations only, at least 3 are needed")
  expect_equal(c(third$n_limit, third$n), c(119, 119))
  expect_true(third$flagged)
})

# ---- MA5: shifts of one compound on one form --------------------------------

test_that("MA5a: two close shifts of one compound resolve to the closer one", {
  d <- kit_check(B("proteins_baseline"), M("compounds_close_shifts"), B("config_baseline"))
  expect_equal(d$status, "PASS")
  expect_null(d$message)

  r <- kit_case(B("proteins_baseline"), M("compounds_close_shifts"), B("config_baseline"))
  ref <- kit_sample_hits(r)
  peak <- ref[ref$`Peak [Da]` %in% 21903.5, ]
  expect_equal(peak$`Compound Mw [Da]`, c(266, 264))
  # Both are x1; 264 is 0.66 Da off the peak, 266 1.34 Da
  expect_equal(peak$Preferred, c(FALSE, TRUE))
  expect_rounded(r$total[[kit_reference_sample]], 12.04, 2)

  # 2o5_1min_R1's complex, 4.34 Da off 266, is caught by 264
  expect_rounded(r$total[["2026-09-18_MULI+BI-8925_2o5_1min_R1"]], 6.62, 2)
  expect_equal(kit_no_hits(r), "2026-09-18_MULI+BI-8925_0_0min_R1")
  card <- kit_card(r, "BI-8925")
  expect_rounded(card[["min"]], 0, 2)
  expect_rounded(card[["mean"]], 70.30, 2)
  expect_rounded(card[["sd"]], 26.73, 2)
  expect_rounded(kit_all_samples(r), 70.30, 2)
  k <- r$kinetics[["MLKL + BI-8925"]]
  expect_rounded(k$ratio, 334.9)
  expect_rounded(k$ci[[1]], 301.6)
  expect_rounded(k$ci[[2]], 379.1)
  expect_rounded(k$proteoforms[["21638.84"]]$ratio, 358.8)
})

test_that("MA5b: a shift close to twice another prefers the lower stoichiometry", {
  ct <- kit_load("mass_ambiguity", "compounds_near_half_shift")
  # 2 x 133.5 = 267 is 1 Da off 266: not an exact multiple, the table passes
  expect_true(isTRUE(check_table(ct, 3, "compounds")))
  expect_equal(kit_check(B("proteins_baseline"), M("compounds_near_half_shift"), B("config_baseline"))$status, "PASS")

  base <- kit_case(B("proteins_baseline"), B("compounds_baseline"), B("config_baseline"))
  r <- kit_case(B("proteins_baseline"), M("compounds_near_half_shift"), B("config_baseline"))
  peak <- kit_sample_hits(r)
  peak <- peak[peak$`Peak [Da]` %in% 21903.5, ]
  expect_equal(peak$`Compound Mw [Da]`, c(266, 133.5))
  expect_equal(peak$`Binding Stoichiometry`, c(1, 2))
  expect_equal(peak$Preferred, c(TRUE, FALSE))
  expect_equal(r$total, base$total[names(r$total)])
  expect_rounded(r$kinetics[["MLKL + BI-8925"]]$ratio, 334.1)
})

test_that("MA5c: a shift exactly twice another is refused", {
  ct <- kit_load("mass_ambiguity", "compounds_half_shift")
  chk <- check_table(ct, 3, "compounds")
  expect_false(isTRUE(chk))
  expect_equal(
    as.character(chk),
    "Mass shifts that are multiples of each other: BI-8925 (266 = 2 × 133)"
  )
  expect_equal(attr(chk, "details"), "BI-8925: Mass 1 (266 Da) = 2 × Mass 2 (133 Da)")
  expect_match(attr(chk, "note"), "Remove the multiple", fixed = TRUE)
  # Whatever the tolerance; the Proteins table and an untyped check pass
  expect_false(isTRUE(check_table(ct, 0.1, "compounds")))
  expect_true(isTRUE(check_table(ct, 3, "proteins")))
  expect_true(isTRUE(check_table(ct, 3)))
  # The run-start backstop finds the same pair
  expect_equal(nrow(shift_multiples(ct)), 1)
})

# ---- MA6: table colouring ----------------------------------------------------

test_that("MA6: the tables colour masses within the declaration check's window", {
  # The renderer runs in the browser; what R controls is the tolerance it is
  # given and the window it derives from it
  pt <- kit_load("mass_ambiguity", "proteins_two_unbound")
  js <- function(tol) {
    as.character(prot_comp_handsontable(pt, tolerance = tol)$x$columns[[1]]$renderer)
  }
  expect_match(js(3), "var GLOBAL_TOLERANCE = 3;", fixed = TRUE)
  expect_match(js(3), "2 * GLOBAL_TOLERANCE + 1e-9", fixed = TRUE)
  expect_match(js(2.5), "var GLOBAL_TOLERANCE = 2.5;", fixed = TRUE)
  expect_match(js(NULL), "var GLOBAL_TOLERANCE = null;", fixed = TRUE)

  # The colouring compares the numbers typed in a table only; the same pair
  # (Δ 5) is what the Samples-table hint reports
  d <- kit_check(M("proteins_two_unbound"), M("compounds_decoy_271"), B("config_baseline"))
  expect_equal(d$details[1], "21,638.8 Da unbound \u2194 21,643.8 Da unbound (\u0394 5.0 Da)")
  amb <- mass_ambiguities(c(21638.84, 21643.84), NULL, 1, tolerance = 2.4)
  expect_equal(nrow(amb), 0)
  amb <- mass_ambiguities(c(21638.84, 21643.84), NULL, 1, tolerance = 2.5)
  expect_equal(amb$kind, "species")
})

# ---- Synthetic edge cases ------------------------------------------------------

test_that("the window holds its boundary against floating-point sums", {
  # (1000 + 100.2) - (1000 + 100) is 0.2000000000000455, a hair over 2 x 0.1
  ct <- compound_table(A = 100, B = 100.2)
  amb <- mass_ambiguities(1000, ct, max_multiples = 1, tolerance = 0.1)
  expect_equal(amb$kind, "compounds")
  # ... but the margin does not widen the window
  ct <- compound_table(A = 100, B = 100.21)
  expect_equal(nrow(mass_ambiguities(1000, ct, max_multiples = 1, tolerance = 0.1)), 0)
})

test_that("an unusable tolerance finds no ambiguity instead of failing", {
  ct <- compound_table(A = 100, B = 100)
  for (tol in list(NA, -1, "", "abc", NULL, numeric(0))) {
    expect_equal(nrow(mass_ambiguities(1000, ct, 1, tol)), 0)
  }
  # Without the protein and compound tables the declaration check skips the
  # mass comparison altogether
  st <- data.frame(Sample = "S1", Protein = "P", `Compound 1` = "A", `Compound 2` = "B", check.names = FALSE)
  expect_true(isTRUE(check_sample_table(st, "P", c("A", "B"))))
  expect_match(kit_synthetic_check(st, compounds = ct), "not distinguishable .*: A \u2194 B$")
})

test_that("a declared mass listed twice is one species", {
  ct <- compound_table(A = 100)
  expect_equal(nrow(mass_ambiguities(c(1000, 1000), ct, 1, 3)), 0)
  once <- synth_hits(c(1000, 1100), c(60, 40), 1000)
  twice <- synth_hits(c(1000, 1100), c(60, 40), c(1000, 1000))
  expect_equal(unique(twice$`Total % Binding`), unique(once$`Total % Binding`))
})

test_that("a mass shift inside its own species' window reads the peak as unbound", {
  ct <- compound_table(A = 4)
  amb <- mass_ambiguities(1000, ct, 1, 3)
  expect_equal(amb$kind, "species")
  expect_equal(amb$delta, 4)

  # One peak 2 Da above the protein: unbound, no binding
  alone <- synth_hits(1002, 100, 1000, list(A = 4))
  expect_false(any(is_complex_row(alone)))
  expect_equal(unique(alone$`Total % Binding`), 0)
  # Next to a closer unbound peak, the same peak is the complex
  pair <- synth_hits(c(1000, 1003.5), c(70, 30), 1000, list(A = 4))
  expect_equal(unique(pair$`Total % Binding`), 0.3)
})

test_that("a peak within the tolerance of two species counts once", {
  # 1002 is the closest peak of both 1000 and 1004, 1102 a complex of both
  hits <- synth_hits(c(1002, 1102), c(60, 40), c(1000, 1004))
  # Intensities are normalised to the largest peak: 1002 is 100, counted for
  # the first species only (not 200)
  expect_equal(sum(unbound_species(hits)$prot_intensity), 100)
  expect_equal(unique(hits$`Total % Binding`), 0.4)
  complexes <- hits[hits$Preferred %in% TRUE, ]
  expect_equal(nrow(complexes), 2)
  expect_equal(sum(complexes$`% Binding`), 0.4)
})

test_that("a complex shared by three proteoforms is split in thirds", {
  # 1300 = 1000 + 300 = 1050 + 250 = 1100 + 200
  hits <- synth_hits(
    c(1000, 1050, 1100, 1300),
    c(30, 20, 10, 40),
    c(1000, 1050, 1100),
    list(A = c(300, 250, 200))
  )
  shared <- hits[hits$`Peak [Da]` %in% 1300 & hits$Preferred %in% TRUE, ]
  expect_equal(shared$`Mw Protein [Da]`, c(1000, 1050, 1100))
  expect_equal(shared$`% Binding`, rep(0.4 / 3, 3))
  expect_equal(unique(hits$`Total % Binding`), 0.4)
})

test_that("the default prefers the lowest stoichiometry, then the closest mass, then the first shift", {
  # 1266.2 is 0.2 Da off 1000 + 133 x 2 and 0.3 Da off 1000 + 266.5 x 1: x1
  # wins although 133 x 2 is closer and declared first
  hits <- synth_hits(c(1000, 1266.2), c(50, 50), 1000, list(A = c(133, 266.5)), max_multiples = 2)
  expect_equal(preferred_reading(hits), c(266.5, 1))

  # Same stoichiometry: the closer shift (0.5 Da off against 1.5 Da)
  hits <- synth_hits(c(1000, 1265.5), c(50, 50), 1000, list(A = c(264, 266)))
  expect_equal(preferred_reading(hits), c(266, 1))

  # Same stoichiometry, equally close: the shift declared first
  hits <- synth_hits(c(1000, 1265), c(50, 50), 1000, list(A = c(264, 266)))
  expect_equal(preferred_reading(hits), c(264, 1))
  # Both rows show the peak, its binding counts once
  expect_equal(nrow(hits[hits$`Peak [Da]` %in% 1265, ]), 2)
  expect_equal(unique(hits$`Total % Binding`), 0.5)
})

test_that("each rule picks its own reading where the criteria disagree", {
  # 1266.2 fits 133 x 2 (0.2 Da off) and 267 x 1 (0.8 Da off)
  pick <- function(rule, peak = 1266.2) {
    preferred_reading(synth_hits(
      c(1000, peak), c(50, 50), 1000, list(A = c(133, 267)),
      max_multiples = 2, preference = rule
    ))
  }
  expect_equal(pick("stoichiometry"), c(267, 1))
  expect_equal(pick("mass_error"), c(133, 2))
  expect_equal(pick("declared"), c(133, 2))
  # 1266.5 is 0.5 Da off both: the closest mass falls back to the lowest
  # stoichiometry, the declared order still takes Mass 1
  expect_equal(pick("mass_error", 1266.5), c(267, 1))
  expect_equal(pick("declared", 1266.5), c(133, 2))
  # A rule the release does not know is the default
  expect_equal(pick("removed_rule"), pick("stoichiometry"))

  # The rule only names the peak: binding is the same under every rule
  totals <- vapply(
    names(hit_preference_rules),
    function(rule) {
      hits <- synth_hits(
        c(1000, 1266.2), c(50, 50), 1000, list(A = c(133, 267)),
        max_multiples = 2, preference = rule
      )
      unique(hits$`Total % Binding`)
    },
    numeric(1)
  )
  expect_equal(unname(totals), rep(0.5, 3))
})

test_that("floating-point noise in the mass sums does not break a tie", {
  # 80.2 x 3 = 1240.6 and 241.6 x 1 = 1241.6 lie 0.5 Da either side of
  # 1241.1. The computed errors differ in the 14th decimal, 80.2 x 3 the
  # smaller; the tie still falls through to the lowest stoichiometry.
  expect_lt(abs(80.2 * 3 - 241.1), abs(241.6 - 241.1))
  hits <- synth_hits(
    c(1000, 1241.1), c(50, 50), 1000, list(A = c(80.2, 241.6)),
    max_multiples = 3, preference = "mass_error"
  )
  expect_equal(preferred_reading(hits), c(241.6, 1))
})

test_that("exact multiples among the shifts of one compound are found", {
  ct <- compound_table(
    A = c(133, 266),          # 2 x
    B = c(266, 88.67),        # 3 x 88.67 = 266.01, within 0.01 Da
    C = c(266, 88.6),         # 3 x 88.6 = 265.8: only close
    D = c(-18, -36, 18),      # same sign only
    E = c(10, 200, 210),      # 20 x is the last factor; 21 x is not checked
    F = c(266, 266),          # the same shift twice
    G = c(0, 0, 5),           # zero pairs with zero only
    H = c(100, NA, 300)       # empty cells are skipped
  )
  m <- shift_multiples(ct)
  key <- paste(m$compound, m$factor, m$smaller_mass, m$larger_mass)
  expect_setequal(key, c(
    "A 2 133 266", "B 3 88.67 266", "D 2 -18 -36", "E 20 10 200",
    "F 1 266 266", "G 1 0 0", "H 3 100 300"
  ))
  expect_equal(m$smaller[m$compound == "A"], "Mass 1")
  expect_equal(m$larger[m$compound == "B"], "Mass 1")

  # The precision is the limit: 0.0101 Da off is no longer exact
  expect_equal(nrow(shift_multiples(compound_table(A = c(100, 200.0101)))), 0)
  expect_equal(nrow(shift_multiples(compound_table(A = c(100, 200.01)))), 1)
  expect_equal(shift_multiple_limits$max_factor, 20)

  # Max. Stoichiometry does not enter: 133 and 399 are refused even when
  # complexes of more than two compounds are not screened
  expect_equal(nrow(shift_multiples(compound_table(A = c(133, 399)))), 1)
  # Nothing to compare
  expect_equal(nrow(shift_multiples(compound_table(A = 100, B = 200))), 0)
  expect_equal(nrow(shift_multiples(NULL)), 0)
})

test_that("the multiples refusal lists two pairs and folds the rest", {
  ct <- compound_table(A = c(133, 266), B = c(100, 300), C = c(50, 50))
  msg <- shift_multiples_message(shift_multiples(ct))
  expect_equal(
    as.character(msg),
    "Mass shifts that are multiples of each other: A (266 = 2 × 133), B (300 = 3 × 100) and 1 more"
  )
  expect_equal(attr(msg, "details")[3], "C: Mass 2 (50 Da) = Mass 1 (50 Da)")
})

test_that("the Preferred column is shown when a reading was not preferred", {
  # 1265.5 fits 264 and 266 x 1: one of them is not preferred
  ambiguous <- synth_hits(c(1000, 1265.5), c(50, 50), 1000, list(A = c(264, 266)))
  expect_true(show_preferred_column(ambiguous))
  plain <- synth_hits(c(1000, 1100), c(50, 50), 1000)
  expect_false(show_preferred_column(plain))
  # Only unbound rows (Preferred NA), or no Preferred column at all
  expect_false(show_preferred_column(synth_hits(1000, 100, 1000)))
  expect_false(show_preferred_column(data.frame(x = 1)))
})

test_that("the rules fall through their criteria in order", {
  # Five readings of one compound on one species, built so that every rule
  # needs a fallback and each lands on a different reading
  hits <- data.frame(
    theor_prot = 1000,
    compound = "A",
    multiple = c(2, 1, 1, 2, 3),
    delta_cmp = c(0.2, 0.8, 0.8, 0.2, 0.9)
  )
  shift <- c(2, 3, 2, 3, 1)
  # x1 (rows 2, 3), tied on the error, Mass 2 before Mass 3
  expect_equal(which(prefer_hits(hits, shift, "stoichiometry")), 3)
  # 0.2 Da (rows 1, 4), tied on stoichiometry, Mass 2 before Mass 3
  expect_equal(which(prefer_hits(hits, shift, "mass_error")), 1)
  # Mass 1
  expect_equal(which(prefer_hits(hits, shift, "declared")), 5)

  # Readings alike in every criterion: the first row
  twins <- hits[c(1, 1), ]
  for (rule in names(hit_preference_rules)) {
    expect_equal(prefer_hits(twins, c(2, 2), rule), c(TRUE, FALSE))
  }
  # Unknown or missing rules are the default
  for (rule in list("removed_rule", NA, NULL, character(0))) {
    expect_equal(hit_preference_rule(rule), "stoichiometry")
  }
  expect_equal(prefer_hits(hits[0, ], numeric(0)), logical(0))
})

test_that("every rule prefers exactly one reading per species and compound", {
  set.seed(20261009)
  for (rule in c(names(hit_preference_rules), "removed_rule")) {
    counts <- unlist(lapply(seq_len(300), function(k) {
      n <- sample(1:8, 1)
      hits <- data.frame(
        theor_prot = sample(c(1000, 1050.5), n, TRUE),
        compound = sample(c("A", "B"), n, TRUE),
        multiple = sample(1:3, n, TRUE),
        # Ties, near-ties within floating-point noise and missing errors
        delta_cmp = sample(c(0, 0.5, 0.5 + 1e-12, 1, NA), n, TRUE)
      )
      shift <- sample(c(1:3, NA), n, TRUE)
      pref <- prefer_hits(hits, shift, rule)
      as.vector(tapply(pref, paste(hits$theor_prot, hits$compound), sum))
    }))
    expect_true(all(counts == 1), info = rule)
  }
})

test_that("an ambiguous peak is logged with its rule and the preferred reading", {
  msgs <- synth_hits(
    c(1000, 1266.2), c(50, 50), 1000, list(A = c(133, 267)),
    max_multiples = 2, preference = "mass_error", log = TRUE
  )
  header <- grep("Ambiguous assignment", msgs, value = TRUE)
  expect_length(header, 1)
  expect_match(header, "Ambiguous assignment at 1266.20 Da (closest mass preferred)", fixed = TRUE)
  expect_true(any(grepl("✓ 1,000 Da + A (133.0 Da ×2), Δ 0.20 Da", msgs, fixed = TRUE)))
  expect_true(any(grepl("· 1,000 Da + A (267.0 Da ×1), Δ 0.80 Da", msgs, fixed = TRUE)))

  # A peak shared by two proteoforms is split, not resolved by the rule
  msgs <- synth_hits(c(1002, 1102), c(60, 40), c(1000, 1004), log = TRUE)
  expect_match(
    grep("Ambiguous assignment", msgs, value = TRUE),
    "(split between 2 proteoforms)",
    fixed = TRUE
  )
})

test_that("ambiguities are checked once per protein and compound set", {
  pt <- data.frame(Protein = "P", `Mass 1` = 1000, check.names = FALSE)
  ct <- compound_table(A = 100, B = 101)
  st <- data.frame(
    Sample = c("S1", "S2", "S3", "S4"),
    Protein = c("P", "P", "P", "Q"),
    `Compound 1` = c("A", "B", "A", "A"),
    `Compound 2` = c("B", "A", "", "B"),
    check.names = FALSE
  )
  amb <- declaration_ambiguities(st, pt, ct, tolerance = 3, max_multiples = 1)
  # S1 and S2 list the same set in another order; S3 has one compound; Q has
  # no protein table row
  expect_equal(nrow(amb), 1)
  expect_equal(amb$samples, 2)
  expect_equal(amb$protein, "P")
})

test_that("long ambiguity lists are cut in the hint and its tooltip", {
  # Four compounds of one mass: six compound pairs on each of 9 x 2 complexes
  pt <- data.frame(Protein = "P")
  for (i in 1:9) pt[[paste("Mass", i)]] <- 1000 * i
  ct <- compound_table(A = 100, B = 100, C = 100, D = 100)
  st <- data.frame(
    Sample = "S1", Protein = "P",
    `Compound 1` = "A", `Compound 2` = "B", `Compound 3` = "C", `Compound 4` = "D",
    check.names = FALSE
  )
  chk <- kit_synthetic_check(st, pt, ct, max_multiples = 2)
  expect_match(chk, ": A \u2194 B, A \u2194 C and 4 more$")
  details <- attr(chk, "details")
  expect_length(details, 13)
  expect_equal(details[13], sprintf("and %d more", 6 * 18 - 12))
})

test_that("a negative shift is screened below the lightest species", {
  # A complex lighter than its protein (the binding loses more than it adds)
  hits <- synth_hits(c(982, 1000), c(30, 70), 1000, list(A = -18))
  expect_equal(hits$Compound[hits$`Peak [Da]` %in% 982], "A")
  expect_equal(unique(hits$`Total % Binding`), 0.3)
  # At stoichiometry 2 as well
  hits <- synth_hits(c(964, 1000), c(30, 70), 1000, list(A = -18), max_multiples = 2)
  expect_equal(hits$`Binding Stoichiometry`[hits$`Peak [Da]` %in% 964], 2)
  # Peaks below every predicted mass stay out, as do those of no declaration
  hits <- synth_hits(c(900, 982, 1000), c(10, 30, 60), 1000, list(A = -18))
  expect_false(900 %in% hits$`Peak [Da]`)
  expect_equal(unique(hits$`Total % Binding`), 30 / 90)
})
