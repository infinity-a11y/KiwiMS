# Unit tests for the hit screening of the conversion analysis.  Everything runs
# on synthetic peak lists, so no deconvolution results or test corpora are
# needed.

box::use(
  app/logic/conversion_functions[
    add_hits,
    add_proteoform_binding,
    check_sample_table,
    check_table,
    clean_prot_comp_table,
    close_log_block,
    complex_hits,
    format_scientific,
    make_kobs_plot,
    mass_ambiguities,
    mass_shift_label,
    nest_log_block,
    predict_peaks,
    proteoform_binding,
    render_hits_table,
    select_complex_kinetics,
    summarize_hits,
    transform_hits,
    unbound_label,
    unbound_species
  ],
  app/logic/conversion_ui[hits_results_ui],
  app/logic/plot_download[prepare_hits_export],
)

# fake_session(): Stand-in for the Shiny session the progress bar writes to ----
fake_session <- function() {
  list(sendCustomMessage = function(...) invisible(NULL))
}

# make_result(): Result list holding one sample with the given peak list ----
make_result <- function(sample, mass, intensity) {
  list(
    deconvolution = stats::setNames(
      list(list(peaks = data.frame(mass = mass, intensity = intensity))),
      sample
    )
  )
}

# screen(): Run the hit screening over a single synthetic sample ----
screen <- function(
  mass,
  intensity,
  protein_masses,
  compound_masses = 100,
  peak_tolerance = 3,
  max_multiples = 2
) {
  sample <- "S1"

  protein_table <- data.frame(Protein = "P", stringsAsFactors = FALSE)
  for (i in seq_along(protein_masses)) {
    protein_table[[paste("Mass", i)]] <- protein_masses[i]
  }

  compound_table <- data.frame(Compound = "C", stringsAsFactors = FALSE)
  for (i in seq_along(compound_masses)) {
    compound_table[[paste("Mass", i)]] <- compound_masses[i]
  }

  sample_table <- data.frame(
    Sample = sample,
    Protein = "P",
    `Compound 1` = "C",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- suppressMessages(add_hits(
    make_result(sample, mass, intensity),
    sample_table = sample_table,
    protein_table = protein_table,
    compound_table = compound_table,
    peak_tolerance = peak_tolerance,
    max_multiples = max_multiples,
    session = fake_session(),
    ns = identity
  ))

  suppressMessages(summarize_hits(result, sample_table = sample_table))
}

test_that("a single declared protein mass yields one species per hit", {
  hits <- screen(
    mass = c(1000, 1100, 1200),
    intensity = c(50, 30, 20),
    protein_masses = 1000
  )

  expect_equal(nrow(hits), 2)
  expect_equal(unique(hits$`Mw Protein [Da]`), 1000)
  expect_equal(hits$`Binding Stoichiometry`, c(1, 2))
  expect_equal(unique(hits$`% Unmatched`), 0)
})

test_that("a second declared protein mass gets its own complexes assigned", {
  hits <- screen(
    mass = c(1000, 1010, 1100, 1110),
    intensity = c(50, 30, 12, 8),
    protein_masses = c(1000, 1010)
  )

  complexes <- hits[!is.na(hits$Compound), ]

  expect_equal(nrow(complexes), 2)
  expect_equal(complexes$`Mw Protein [Da]`, c(1000, 1010))
  expect_equal(complexes$`Peak [Da]`, c(1100, 1110))
  expect_equal(unique(hits$`% Unmatched`), 0)
})

test_that("the peak of a second species is not a complex of the first", {
  # 1100 is both the declared mass of the second species and protein 1 plus one
  # compound - the species assignment has to win
  hits <- screen(
    mass = c(1000, 1100),
    intensity = c(50, 30),
    protein_masses = c(1000, 1100)
  )

  expect_true(all(is.na(hits$Compound)))
  expect_equal(sort(hits$`Measured Mw Protein [Da]`), c(1000, 1100))
  expect_equal(unique(hits$`Total % Binding`), 0)
})

test_that("an unbound second species counts as signal, not as unmatched", {
  # Second species present but never bound; without it the 1010 peak would be
  # unexplained and would be missing from the intensity the binding refers to
  hits <- screen(
    mass = c(1000, 1010, 1100),
    intensity = c(50, 30, 20),
    protein_masses = c(1000, 1010)
  )

  species <- unbound_species(hits)

  expect_equal(unique(hits$`% Unmatched`), 0)
  # Reported per proteoform, so the two apo peaks stay separable
  expect_equal(species$theor_prot, c(1000, 1010))
  expect_equal(species$prot_intensity, c(100, 60))
  expect_equal(unique(hits$`Total % Binding`), 40 / (100 + 60 + 40))
})

test_that("binding and unbound fractions of every species add up to one", {
  hits <- screen(
    mass = c(1000, 1010, 1100, 1110, 1210),
    intensity = c(50, 30, 12, 8, 4),
    protein_masses = c(1000, 1010)
  )

  complexes <- hits[!is.na(hits$Compound), ]
  unbound <- sum(unbound_species(hits)$prot_intensity)
  total <- sum(complexes$Intensity) + unbound

  expect_equal(unique(hits$`Total % Binding`) + unbound / total, 1)
  expect_equal(sum(complexes$`% Binding`), unique(hits$`Total % Binding`))
})

test_that("unbound species are listed once regardless of how often they bind", {
  # First species binds twice, second species not at all
  hits <- screen(
    mass = c(1000, 1010, 1100, 1200),
    intensity = c(50, 30, 12, 8),
    protein_masses = c(1000, 1010)
  )

  species <- unbound_species(hits)

  expect_equal(nrow(species), 2)
  expect_equal(species$theor_prot, c(1000, 1010))
  expect_equal(species$prot_intensity, c(100, 60))
})

test_that("every proteoform gets its own binding next to the pooled one", {
  # Intensities are scaled to the base peak: apo 100 / 60, complexes 24 / 16
  hits <- screen(
    mass = c(1000, 1010, 1100, 1110),
    intensity = c(50, 30, 12, 8),
    protein_masses = c(1000, 1010)
  )

  binding <- proteoform_binding(hits)

  expect_equal(binding$species, c(1000, 1010))
  expect_equal(binding$binding, c(100 * 24 / 124, 100 * 16 / 76))
  expect_true(all(is.na(binding$limit)))
  # Pooled over the species, the split gives back the total binding
  expect_equal(
    sum(binding$complex) / sum(binding$unbound + binding$complex),
    unique(hits$`Total % Binding`)
  )
})

test_that("a proteoform binding pinned by an undetected peak is flagged", {
  # The second species shows its complex but no unbound peak
  hits <- screen(
    mass = c(1000, 1100, 1110),
    intensity = c(50, 12, 8),
    protein_masses = c(1000, 1010)
  )

  binding <- proteoform_binding(hits)
  second <- binding[binding$species == 1010, ]

  expect_equal(second$binding, 100)
  expect_equal(second$limit, "unbound")
  expect_true(is.na(binding$limit[binding$species == 1000]))
})

test_that("the proteoform binding column is only added for several masses", {
  single <- screen(
    mass = c(1000, 1100),
    intensity = c(50, 30),
    protein_masses = 1000
  )
  multi <- screen(
    mass = c(1000, 1010, 1100, 1110),
    intensity = c(50, 30, 12, 8),
    protein_masses = c(1000, 1010)
  )

  single_view <- add_proteoform_binding(transform_hits(single))
  multi_view <- add_proteoform_binding(transform_hits(multi))

  expect_false("Prot. Binding [%]" %in% names(single_view))
  expect_true("Prot. Binding [%]" %in% names(multi_view))
  complexes <- multi_view[multi_view$`Cmp Name` != "N/A", ]
  expect_equal(
    complexes$`Prot. Binding [%]`,
    c(100 * 24 / 124, 100 * 16 / 76)
  )
})

test_that("the proteoform binding column is displayed like the other percentages", {
  multi <- screen(
    mass = c(1000, 1010, 1100, 1110),
    intensity = c(50, 30, 12, 8),
    protein_masses = c(1000, 1010)
  )
  view <- add_proteoform_binding(transform_hits(multi))

  # Offered as a % bar only when the column exists
  bar_choices <- function(hits) {
    html <- as.character(hits_results_ui(identity, hits, units = character()))
    picker <- regmatches(
      html,
      regexpr("hits_binding_chart.*?</select>", html)
    )
    grepl("Prot. Binding [%]", picker, fixed = TRUE)
  }
  expect_true(bar_choices(view))
  expect_false(bar_choices(view[, names(view) != "Prot. Binding [%]"]))

  # Rounded for display, both as plain number and as bar
  render_defs <- function(bar_chart) {
    dt <- render_hits_table(
      view,
      concentration_colors = NULL,
      bar_chart = bar_chart,
      units = character()
    )
    # DT turns the target names into 0-based column indices
    col <- which(names(view) == "Prot. Binding [%]") - 1
    defs <- dt$x$options$columnDefs
    defs[vapply(
      defs,
      function(d) !is.null(d$render) && col %in% d$targets,
      logical(1)
    )]
  }
  plain <- render_defs(character())
  bar <- render_defs("Prot. Binding [%]")
  expect_length(plain, 1)
  expect_match(as.character(plain[[1]]$render), "toFixed(2)", fixed = TRUE)
  expect_length(bar, 1)
  expect_equal(bar[[1]]$className, "bar-chart-col")

  # Exported unrounded and numeric
  export <- prepare_hits_export(view)
  expect_type(export$`Prot. Binding [%]`, "double")
  expect_equal(
    export$`Prot. Binding [%]`[export$`Cmp Name` != "N/A"],
    c(100 * 24 / 124, 100 * 16 / 76)
  )
})

test_that("a proteoform is named in the labels only when there are several", {
  single <- mass_shift_label("266.0", "1", species_mass = 1000)
  multi <- mass_shift_label(
    "266.0",
    "1",
    species_mass = 1000,
    multi_species = TRUE
  )

  expect_false(grepl("1,000", single))
  expect_true(grepl("1,000 Da", multi))
  expect_true(grepl("266.0", multi))
  expect_equal(unbound_label(c(1000, 1010)), rep("Unbound Protein", 2))
  expect_equal(
    unbound_label(c(1000, 1010), multi_species = TRUE),
    c("Unbound 1,000 Da", "Unbound 1,010 Da")
  )
})

test_that("a declared species without signal adds no rows", {
  hits <- screen(
    mass = c(1000, 1100),
    intensity = c(50, 30),
    protein_masses = c(1000, 2000)
  )

  expect_equal(nrow(hits), 1)
  expect_equal(hits$`Mw Protein [Da]`, 1000)
  expect_equal(unique(hits$`% Unmatched`), 0)
})

test_that("the declaration table keeps every mass a protein was given", {
  tab <- cbind(
    Protein = c("P", rep(NA_character_, 8)),
    as.data.frame(matrix(
      NA_real_,
      nrow = 9,
      ncol = 9,
      dimnames = list(NULL, paste("Mass", 1:9))
    ))
  )
  tab[1, "Mass 1"] <- 1000
  tab[1, "Mass 2"] <- 1010

  cleaned <- clean_prot_comp_table("Protein", tab, full = FALSE)

  expect_equal(names(cleaned), c("Protein", "Mass 1", "Mass 2"))
  expect_equal(as.numeric(cleaned[1, -1]), c(1000, 1010))
})

test_that("the declaration tables refuse masses of zero or below", {
  compounds <- data.frame(
    Compound = c("BI-8925", "C2"),
    `Mass 1` = c(-266, 150),
    `Mass 2` = c(100, 0),
    `Mass 3` = c(-1, NA),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  chk <- check_table(compounds, 3, "compounds")
  expect_equal(as.character(chk), "Mass values must be greater than 0")
  expect_equal(
    attr(chk, "details"),
    c(
      "BI-8925: Mass 1 (-266 Da)",
      "BI-8925: Mass 3 (-1 Da)",
      "C2: Mass 2 (0 Da)"
    )
  )
  expect_match(attr(chk, "note"), "positive values", fixed = TRUE)

  proteins <- data.frame(
    Protein = "MLKL",
    `Mass 1` = -21638.84,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expect_false(isTRUE(check_table(proteins, 3, "proteins")))

  proteins$`Mass 1` <- 21638.84
  expect_true(isTRUE(check_table(proteins, 3, "proteins")))
})

# screen_compounds(): Hit screening of one sample against several compounds ----
screen_compounds <- function(
  mass,
  intensity,
  protein_masses,
  compounds,
  sample_compounds = names(compounds),
  peak_tolerance = 3,
  max_multiples = 2
) {
  protein_table <- data.frame(Protein = "P", stringsAsFactors = FALSE)
  for (i in seq_along(protein_masses)) {
    protein_table[[paste("Mass", i)]] <- protein_masses[i]
  }

  n_masses <- max(lengths(compounds))
  compound_table <- data.frame(
    Compound = names(compounds),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(n_masses)) {
    compound_table[[paste("Mass", i)]] <- vapply(
      compounds,
      function(m) if (i <= length(m)) m[i] else NA_real_,
      numeric(1)
    )
  }

  sample_table <- data.frame(Sample = "S1", Protein = "P")
  for (i in seq_along(sample_compounds)) {
    sample_table[[paste("Compound", i)]] <- sample_compounds[i]
  }

  result <- suppressMessages(add_hits(
    make_result("S1", mass, intensity),
    sample_table = sample_table,
    protein_table = protein_table,
    compound_table = compound_table,
    peak_tolerance = peak_tolerance,
    max_multiples = max_multiples,
    session = fake_session(),
    ns = identity
  ))

  suppressMessages(summarize_hits(result, sample_table = sample_table))
}

test_that("every compound of a multi-compound sample is screened", {
  # The sample lists B and C of a table holding A, B and C, in another order
  hits <- screen_compounds(
    mass = c(1000, 1200, 1300),
    intensity = c(50, 30, 20),
    protein_masses = 1000,
    compounds = list(A = 100, B = 200, C = 300),
    sample_compounds = c("C", "B")
  )

  expect_setequal(stats::na.omit(hits$Compound), c("B", "C"))
})

test_that("the predicted peaks are ordered species, stoichiometry, shift, compound", {
  compound_mw <- data.frame(
    Compound = c("A", "B"),
    `Mass 1` = c(100, 200),
    `Mass 2` = c(50, NA),
    check.names = FALSE
  )

  predicted <- predict_peaks(c(1000, 1010), compound_mw, max_multiples = 2)
  complexes <- predicted[predicted$type == "complex", ]

  expect_equal(predicted$mass[predicted$type == "unbound"], c(1000, 1010))
  # Species, then stoichiometry, then mass shift, then compound
  expect_equal(
    complexes$mass[complexes$species == 1000],
    c(1100, 1200, 1050, 1200, 1400, 1100)
  )
  expect_equal(nrow(complexes), 2 * 6)
})

test_that("mass ambiguities are found within twice the tolerance", {
  compound_mw <- data.frame(
    Compound = c("A", "B"),
    `Mass 1` = c(100, 105),
    check.names = FALSE
  )

  # Two compounds 5 Da apart: indistinguishable at 3 Da (window 6), not at 2
  amb <- mass_ambiguities(1000, compound_mw, max_multiples = 1, tolerance = 3)
  expect_equal(amb$kind, "compounds")
  expect_equal(amb$delta, 5)
  expect_equal(
    nrow(mass_ambiguities(1000, compound_mw, max_multiples = 1, tolerance = 2)),
    0
  )

  # A proteoform at +100 is the complex of the main form with A
  amb <- mass_ambiguities(
    c(1000, 1100),
    compound_mw[1, ],
    max_multiples = 1,
    tolerance = 3
  )
  expect_true("species" %in% amb$kind)

  # Complexes of two proteoforms coinciding (1000 + 2 x 100 = 1100 + 100)
  amb <- mass_ambiguities(
    c(1000, 1100),
    compound_mw[1, ],
    max_multiples = 2,
    tolerance = 3
  )
  expect_true("proteoform" %in% amb$kind)

  # Two mass shifts of one compound are left to the preferred assignment
  one_compound <- data.frame(
    Compound = "A",
    `Mass 1` = 100,
    `Mass 2` = 101,
    check.names = FALSE
  )
  expect_equal(
    nrow(mass_ambiguities(1000, one_compound, max_multiples = 1, tolerance = 3)),
    0
  )
})

test_that("the sample check refuses compounds a peak cannot tell apart", {
  protein_table <- data.frame(Protein = "P", `Mass 1` = 1000, check.names = FALSE)
  compound_table <- data.frame(
    Compound = c("A", "B", "C"),
    `Mass 1` = c(100, 104, 300),
    check.names = FALSE
  )
  sample_table <- function(cmp) {
    data.frame(
      Sample = "S1",
      Protein = "P",
      `Compound 1` = cmp[1],
      `Compound 2` = cmp[2],
      check.names = FALSE
    )
  }
  check <- function(cmp, tolerance = 3) {
    check_sample_table(
      sample_table(cmp),
      proteins = protein_table$Protein,
      compounds = compound_table$Compound,
      protein_table = protein_table,
      compound_table = compound_table,
      tolerance = tolerance,
      max_multiples = 1
    )
  }

  expect_match(check(c("A", "B")), "not distinguishable")
  # One short line naming the compounds; the pairs go to the tooltip
  expect_match(check(c("A", "B")), "A ↔ B$")
  expect_match(
    attr(check(c("A", "B")), "details"),
    "A \\(100.0 Da ×1\\) ↔ .* B \\(104.0 Da ×1\\) \\(Δ 4.0 Da\\)"
  )
  expect_match(attr(check(c("A", "B")), "note"), "separate samples")
  expect_true(isTRUE(check(c("A", "C"))))
  expect_null(attr(check(c("A", "C")), "warning"))
  # A tighter tolerance separates A and B again
  expect_true(isTRUE(check(c("A", "B"), tolerance = 1.5)))
})

test_that("the sample check passes proteoform ambiguities with a warning", {
  protein_table <- data.frame(
    Protein = "P",
    `Mass 1` = 1000,
    `Mass 2` = 1100,
    check.names = FALSE
  )
  compound_table <- data.frame(Compound = "A", `Mass 1` = 100, check.names = FALSE)

  result <- check_sample_table(
    data.frame(Sample = "S1", Protein = "P", `Compound 1` = "A", check.names = FALSE),
    proteins = "P",
    compounds = "A",
    protein_table = protein_table,
    compound_table = compound_table,
    tolerance = 3,
    max_multiples = 1
  )

  expect_true(isTRUE(result))
  expect_match(attr(result, "warning"), "Ambiguous masses")
})

test_that("the kinetics of a complex count the binding of its compound only", {
  hits <- screen_compounds(
    mass = c(1000, 1100, 1200),
    intensity = c(50, 30, 20),
    protein_masses = 1000,
    compounds = list(A = 100, B = 200),
    max_multiples = 1
  )
  hits$binding <- hits$`Total % Binding` * 100
  sample_table <- data.frame(
    Sample = "S1",
    Protein = "P",
    `Compound 1` = "A",
    `Compound 2` = "B",
    check.names = FALSE
  )

  a <- complex_hits(hits, sample_table, "P", "A")
  b <- complex_hits(hits, sample_table, "P", "B")

  expect_false("B" %in% a$Compound)
  expect_equal(unique(a$binding), 30)
  expect_equal(unique(b$binding), 20)
  # The shares of the compounds add up to the sample's total
  expect_equal(unique(a$binding) + unique(b$binding), unique(hits$binding))
  # Samples of another protein are not part of the complex
  expect_equal(nrow(complex_hits(hits, sample_table, "Q", "A")), 0)

  # A sample declaring one compound keeps its total binding
  single <- sample_table[, c("Sample", "Protein", "Compound 1")]
  expect_equal(
    unique(complex_hits(hits, single, "P", "A")$binding),
    unique(hits$binding)
  )
})

test_that("the kinetics shown are those of the picked complex", {
  entry <- function(compound, fitted) {
    list(
      protein = "P",
      compound = compound,
      hits = paste("hits", compound),
      binding_kobs_result = if (fitted) paste("kobs", compound),
      kinact_ki_result = if (fitted) paste("fit", compound),
      reason = if (!fitted) "no fit"
    )
  }
  result_list <- list(
    kinetics = list("P + A" = entry("A", FALSE), "P + B" = entry("B", TRUE))
  )

  # Without a pick, the first complex with a k_obs fit
  default <- select_complex_kinetics(result_list)
  expect_equal(default$kinetics_complex$key, "P + B")
  expect_equal(default$binding_kobs_result, "kobs B")
  expect_equal(
    select_complex_kinetics(result_list, "gone")$kinetics_complex$key,
    "P + B"
  )

  picked <- select_complex_kinetics(result_list, "P + A")
  expect_null(picked$binding_kobs_result)
  expect_null(picked$kinact_ki_result)
  expect_equal(picked$kinetics_hits, "hits A")
  expect_equal(picked$kinetics_complex$reason, "no fit")
})

test_that("the kinetics log of a complex closes its branches", {
  # Prerequisites failed: the k_obs step is the last one of the block
  skipped <- close_log_block(c(
    "  ├─ Infer observed first-order rate constant k_obs",
    "  │  │",
    "  │  ├─ At least 3 different non-zero concentrations are required.",
    "  │  └─ Skipping binding kinetics analysis.",
    "  │"
  ))
  expect_equal(skipped, c(
    "  └─ Infer observed first-order rate constant k_obs",
    "     │",
    "     ├─ At least 3 different non-zero concentrations are required.",
    "     └─ Skipping binding kinetics analysis."
  ))

  # Failed fit: its warning is the last line of the kinact/KI step
  failed <- close_log_block(c(
    "  ├─ Infer observed first-order rate constant k_obs",
    "  │  └─ Computing k_obs for 1 μM",
    "  │",
    "  └─ Infer second-order rate constant",
    "     ├─ ⚠ Solver: no convergence",
    "     ├─ ⚠ Fit failed"
  ))
  expect_equal(failed[6], "     └─ ⚠ Fit failed")
  expect_equal(failed[1], "  ├─ Infer observed first-order rate constant k_obs")

  # With several complexes each block hangs under its own branch
  nested <- nest_log_block(failed[4:6], "P + A", last = TRUE)
  expect_equal(nested, c(
    "  │",
    "  └─ P + A",
    "     │",
    "     └─ Infer second-order rate constant",
    "        ├─ ⚠ Solver: no convergence",
    "        └─ ⚠ Fit failed"
  ))
  expect_equal(
    nest_log_block("  └─ step", "P + B")[2:4],
    c("  ├─ P + B", "  │  │", "  │  └─ step")
  )
})

test_that("a failed kinact/KI fit is shown without values or curve", {
  expect_equal(format_scientific(NULL), "N/A")
  expect_equal(format_scientific(numeric(0)), "N/A")
  expect_equal(format_scientific(NA_real_), "N/A")

  kobs_table <- data.frame(
    kobs = c(0.1, 0.2, 0.4),
    kobs_se = c(0.01, 0.02, 0.04),
    v = 1,
    plateau = 80,
    row.names = c("1", "2", "4")
  )
  plot <- make_kobs_plot(
    NULL,
    colors = c(`1` = "red", `2` = "green", `4` = "blue"),
    units = c(Concentration = "Conc. [μM]", Time = "Time [min]"),
    kobs_table = kobs_table
  )
  built <- plotly::plotly_build(plot)$x$data
  expect_length(built, 3)
  expect_true(all(vapply(built, `[[`, character(1), "mode") == "markers"))
})

test_that("a peak claimed by two compounds is split, not counted twice", {
  hits <- screen_compounds(
    mass = c(1000, 1101),
    intensity = c(60, 40),
    protein_masses = 1000,
    compounds = list(A = 100, B = 102)
  )

  complexes <- hits[!is.na(hits$Compound), ]
  expect_equal(nrow(complexes), 2)
  expect_equal(sum(complexes$`% Binding`), unique(hits$`Total % Binding`))
})

test_that("a complex shared by two proteoforms is split between them", {
  # 1200 is 1000 + 2 x 100 and 1100 + 100
  hits <- screen_compounds(
    mass = c(1000, 1100, 1200),
    intensity = c(50, 30, 20),
    protein_masses = c(1000, 1100),
    compounds = list(A = 100)
  )

  binding <- proteoform_binding(hits)
  pooled <- sum(binding$complex) / sum(binding$unbound + binding$complex)

  expect_equal(pooled, unique(hits$`Total % Binding`))
  # Split evenly: half of the shared peak for each proteoform
  expect_equal(binding$complex, c(20, 20))
  preferred <- hits[hits$Preferred %in% TRUE, ]
  expect_equal(sum(preferred$`% Binding`), unique(hits$`Total % Binding`))
})
