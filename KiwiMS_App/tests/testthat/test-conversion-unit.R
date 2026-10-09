# Unit tests for the hit screening of the conversion analysis.  Everything runs
# on synthetic peak lists, so no deconvolution results or test corpora are
# needed.

box::use(
  app/logic/conversion_functions[
    add_hits,
    add_proteoform_binding,
    check_sample_table,
    check_table,
    color_key,
    clean_prot_comp_table,
    close_log_block,
    complex_hits,
    compound_mass_entries,
    format_scientific,
    is_complex_row,
    key_colors,
    make_kobs_plot,
    mass_ambiguities,
    mass_shift_label,
    multiple_spectra,
    nest_log_block,
    overview_choices,
    overview_colors,
    overview_compound_distribution,
    overview_distribution_note,
    overview_selection,
    overview_status_note,
    overview_subset,
    predict_peaks,
    proteoform_binding,
    render_hits_table,
    select_complex_kinetics,
    summarize_hits,
    transform_hits,
    transform_per_adduct,
    unbound_label,
    unbound_species
  ],
  app/logic/conversion_ui[binding_results_ui, hits_results_ui],
  app/logic/deconvolution_functions[spectrum_plot],
  app/logic/plot_download[prepare_hits_export],
  app/logic/palette[status_colors],
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

# screen_declared(): Display hits of one sample declaring several compounds ----
# `compounds` maps each compound name to its single mass shift; with none the
# sample is a protein-only control. Returns the hits as the results interface
# holds them, together with the tables they were screened against.
screen_declared <- function(mass, intensity, protein_masses, compounds) {
  sample <- "S1"

  protein_table <- data.frame(Protein = "P", stringsAsFactors = FALSE)
  for (i in seq_along(protein_masses)) {
    protein_table[[paste("Mass", i)]] <- protein_masses[i]
  }

  # "C" is declared by no sample and keeps the table filled for a control; the
  # second mass slot stays unused, as declaration tables have a few to spare
  compound_table <- data.frame(
    Compound = c("C", names(compounds)),
    `Mass 1` = c(100, unname(compounds)),
    `Mass 2` = NA_real_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # A control sample leaves its compound column empty
  declared <- if (length(compounds)) names(compounds) else NA_character_
  sample_table <- data.frame(Sample = sample, Protein = "P")
  for (i in seq_along(declared)) {
    sample_table[[paste("Compound", i)]] <- declared[i]
  }

  result <- suppressMessages(add_hits(
    make_result(sample, mass, intensity),
    sample_table = sample_table,
    protein_table = protein_table,
    compound_table = compound_table,
    peak_tolerance = 3,
    max_multiples = 2,
    session = fake_session(),
    ns = identity
  ))
  hits <- suppressMessages(summarize_hits(result, sample_table = sample_table))

  list(
    hits = hits,
    view = transform_hits(hits),
    protein_table = protein_table,
    compound_table = compound_table,
    sample_table = sample_table
  )
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

  # Both rows are unbound, naming the declared compound without an adduct
  expect_false(any(is_complex_row(hits)))
  expect_equal(hits$Compound, c("C", "C"))
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

test_that("a row without an adduct names the compound declared for its sample", {
  # The second species shows no complex
  screened <- screen_declared(
    mass = c(1000, 1010, 1100),
    intensity = c(50, 30, 12),
    protein_masses = c(1000, 1010),
    compounds = c(A = 100)
  )
  view <- screened$view

  expect_equal(nrow(view), 2)
  expect_equal(view$`Cmp Name`, c("A", "A"))
  complex <- is_complex_row(view)
  expect_equal(sum(complex), 1)
  expect_equal(view$`Theor. Prot. [Da]`[!complex], 1010)
  expect_equal(view$`Binding [%]`[!complex], 0)
})

test_that("every species shows each declared compound once", {
  # A binds the second species only; B is found nowhere
  screened <- screen_declared(
    mass = c(1000, 1010, 1110),
    intensity = c(50, 30, 10),
    protein_masses = c(1000, 1010),
    compounds = c(A = 100, B = 250)
  )
  view <- screened$view
  complex <- is_complex_row(view)
  pairs <- paste(view$`Theor. Prot. [Da]`, view$`Cmp Name`)

  expect_setequal(pairs, c("1000 A", "1000 B", "1010 A", "1010 B"))
  expect_equal(pairs[complex], "1010 A")
  expect_false(anyDuplicated(pairs) > 0)

  # The species bound by A carries B as a row without an adduct, with its own
  # signal as the peak and the sample's total binding
  b_row <- view[pairs == "1010 B", ]
  expect_equal(b_row$`Binding [%]`, 0)
  expect_equal(b_row$`Theor. Cmp [Da]`, "N/A")
  expect_equal(b_row$`Bind. Stoich.`, "N/A")
  expect_equal(b_row$`Peak Signal [Da]`, "1010.0")
  expect_equal(
    b_row$`Tot. Binding [%]`,
    view$`Tot. Binding [%]`[pairs == "1010 A"]
  )
})

test_that("rows without an adduct leave the binding values untouched", {
  screened <- screen_declared(
    mass = c(1000, 1010, 1110),
    intensity = c(50, 30, 10),
    protein_masses = c(1000, 1010),
    compounds = c(A = 100, B = 250)
  )
  hits <- screened$hits

  # The raw hits carry the same rows as the display
  expect_equal(nrow(hits), 4)
  expect_equal(sum(is_complex_row(hits)), 1)
  # Unbound signal counted once per species, the one complex peak once
  expect_equal(unique(hits$`Total % Binding`), 10 / 90)
  expect_equal(sum(hits$`% Binding`, na.rm = TRUE), 10 / 90)
})

test_that("a sample with nothing detected lists every declared compound", {
  screened <- screen_declared(
    mass = 500,
    intensity = 50,
    protein_masses = c(1000, 1010),
    compounds = c(A = 100, B = 250)
  )
  view <- screened$view

  expect_equal(view$`Cmp Name`, c("A", "B"))
  expect_false(any(is_complex_row(view)))
  expect_equal(view$`Meas. Prot. [Da]`, c("N/A", "N/A"))
  # Neither unbound nor complex signal: binding is 0 / 0, not measured
  expect_true(all(is.na(screened$hits$`Total % Binding`)))
  expect_true(all(is.na(view$`Binding [%]`)))
  expect_true(all(is.na(view$`Tot. Binding [%]`)))
})

test_that("a detected protein without an adduct reads a measured 0 %", {
  screened <- screen_declared(
    mass = 1000,
    intensity = 50,
    protein_masses = 1000,
    compounds = c(A = 100)
  )

  expect_equal(screened$hits$`Total % Binding`, 0)
  expect_equal(screened$view$`Tot. Binding [%]`, 0)
})

test_that("a sample without any protein peak stays unmeasured in the kinetics", {
  # S1 and S2 both declare A and B; S2 showed no protein at all
  hits <- data.frame(
    Sample = c("S1", "S1", "S2", "S2"),
    Protein = "P",
    Compound = c("A", "B", "A", "B"),
    `Compound Mw [Da]` = c(100, NA, NA, NA),
    Preferred = c(TRUE, NA, NA, NA),
    `% Binding` = c(0.2, 0, NA, NA),
    binding = c(30, 30, NA, NA),
    check.names = FALSE
  )
  st <- data.frame(
    Sample = c("S1", "S2"),
    Protein = "P",
    `Compound 1` = "A",
    `Compound 2` = "B",
    check.names = FALSE
  )

  rows <- complex_hits(hits, st, "P", "A")
  # S1 counts A's own share, S2 stays NA instead of a 0 % share
  expect_equal(rows$binding[rows$Sample == "S1"], 20)
  expect_true(is.na(rows$binding[rows$Sample == "S2"]))
})

test_that("the sample picker marks samples without a hit instead of grouping", {
  view <- screen_declared(
    mass = c(1000, 1100),
    intensity = c(50, 30),
    protein_masses = 1000,
    compounds = c(A = 100)
  )$view
  unbound <- screen_declared(
    mass = 1000,
    intensity = 50,
    protein_masses = 1000,
    compounds = c(A = 100)
  )$view
  unbound$`Sample ID` <- "S2"
  silent <- screen_declared(
    mass = 500,
    intensity = 50,
    protein_masses = 1000,
    compounds = c(A = 100)
  )$view
  silent$`Sample ID` <- "S3"
  hits <- rbind(view, unbound, silent)
  hits$truncSample_ID <- hits$`Sample ID`

  html <- as.character(binding_results_ui(identity, hits))
  picker <- regmatches(
    html,
    regexpr("conversion_sample_picker.*?</select>", html)
  )

  expect_false(grepl("<optgroup", picker, fixed = TRUE))
  expect_match(picker, "value=\"S1\" data-subtext=\"\">", fixed = TRUE)
  expect_match(picker, "value=\"S2\" data-subtext=\"No hits\">", fixed = TRUE)
  expect_match(picker, "value=\"S3\" data-subtext=\"Not measured\">", fixed = TRUE)
})

test_that("a sample declaring no compound keeps its rows unnamed", {
  screened <- screen_declared(
    mass = c(1000, 1010),
    intensity = c(50, 30),
    protein_masses = c(1000, 1010),
    compounds = numeric(0)
  )

  expect_equal(nrow(screened$view), 2)
  expect_true(all(is.na(screened$view$`Cmp Name`)))
  expect_false(any(is_complex_row(screened$view)))
})

test_that("the sample view gives a compound without an adduct no mass columns", {
  screened <- screen_declared(
    mass = c(1000, 1010, 1110),
    intensity = c(50, 30, 10),
    protein_masses = c(1000, 1010),
    compounds = c(A = 100, B = 250)
  )
  per_adduct <- transform_per_adduct(
    screened$view,
    proteins_table = screened$protein_table,
    compounds_table = screened$compound_table,
    samples_table = screened$sample_table
  )

  # One entry per species and compound
  expect_equal(nrow(per_adduct), 4)
  # Only A's single mass shift on its one complex - the empty mass of a row
  # without an adduct is not read as the unused second slot
  expect_false(any(grepl("Mass 2", names(per_adduct))))
  bound <- per_adduct$`Cmp Name` == "A" & per_adduct$`Theor. Prot. [Da]` == 1010
  expect_equal(per_adduct$`Binding (Mass 1)x1 [%]`[bound], 10 / 90 * 100)
  expect_true(all(is.na(per_adduct$`Binding (Mass 1)x1 [%]`[!bound])))
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

# ---- Overview tab of the Relative Binding interface ----------------------------

# overview_run(): A small run over three proteins and two compounds ----
# P (1000 Da) carries compound A (100 Da) in S1, S5 and S7, and the declared
# compound B (200 Da) without a hit in S1 and S2. Q (2000 Da) is declared with
# A in S3 and S6, but its mass is not in their spectra. R (3000 Da) is a
# control without compound. S7 is fully converted: only its complex peak is
# left. Every spectrum is a grid with the peaks on it, so the plots can be
# built too.
overview_run <- function() {
  peaks <- list(
    S1 = c(`1000` = 60, `1100` = 40),
    S2 = c(`1000` = 100),
    S3 = c(`1000` = 100),
    S4 = c(`1000` = 100),
    S5 = c(`1000` = 30, `1100` = 70),
    S6 = c(`1000` = 100),
    S7 = c(`1100` = 100)
  )
  grid <- seq(900, 1300, by = 1)
  result <- list(deconvolution = lapply(peaks, function(p) {
    pm <- as.numeric(names(p))
    spectrum <- vapply(
      grid,
      function(m) sum(p * exp(-((m - pm)^2) / 8)),
      numeric(1)
    )
    list(
      peaks = data.frame(mass = pm, intensity = unname(p)),
      mass = data.frame(mass = grid, intensity = spectrum + 0.01)
    )
  }))

  protein_table <- data.frame(
    Protein = c("P", "Q", "R"),
    `Mass 1` = c(1000, 2000, 3000),
    check.names = FALSE
  )
  compound_table <- data.frame(
    Compound = c("A", "B"),
    `Mass 1` = c(100, 200),
    check.names = FALSE
  )
  sample_table <- data.frame(
    Sample = names(peaks),
    Protein = c("P", "P", "Q", "R", "P", "Q", "P"),
    `Compound 1` = c("A", "B", "A", NA, "A", "A", "A"),
    `Compound 2` = c("B", NA, NA, NA, NA, NA, NA),
    check.names = FALSE
  )

  result <- suppressMessages(add_hits(
    result,
    sample_table = sample_table,
    protein_table = protein_table,
    compound_table = compound_table,
    peak_tolerance = 3,
    max_multiples = 2,
    session = fake_session(),
    ns = identity
  ))
  result$hits_summary <- suppressMessages(
    summarize_hits(result, sample_table = sample_table)
  )

  view <- add_proteoform_binding(transform_hits(result$hits_summary))
  view$truncSample_ID <- view$`Sample ID`

  list(
    result = result,
    view = view,
    protein_table = protein_table,
    compound_table = compound_table
  )
}

test_that("the Overview lists proteins with hits first, controls last", {
  run <- overview_run()
  choices <- overview_choices(run$view)

  expect_equal(choices$proteins$protein, c("P", "Q", "R"))
  expect_equal(choices$proteins$status, c("hits", "no_hits", "none"))
  expect_equal(
    overview_status_note(choices$proteins$status),
    c("", "No hits", "No compounds")
  )
  # A compound with a hit on the protein comes first
  expect_equal(choices$compounds$P$compound, c("A", "B"))
  expect_equal(choices$compounds$P$hit, c(TRUE, FALSE))
  expect_equal(choices$compounds$Q$compound, "A")
  expect_false(choices$compounds$Q$hit)
  expect_equal(nrow(choices$compounds$R), 0)
})

test_that("an Overview selection is always one the pickers offer", {
  choices <- overview_choices(overview_run()$view)

  # By default the first protein with all of its compounds
  expect_equal(overview_selection(choices), list(protein = "P", compounds = c("A", "B")))
  # An unknown protein falls back to the default
  expect_equal(overview_selection(choices, "X")$protein, "P")
  # Compounds not declared with the protein are dropped, the order kept
  expect_equal(overview_selection(choices, "P", c("B", "Z"))$compounds, "B")
  expect_equal(overview_selection(choices, "Q", "B")$compounds, character(0))
  expect_equal(overview_selection(choices, "R"), list(protein = "R", compounds = character(0)))
})

test_that("the Overview subset holds the picked complexes only", {
  view <- overview_run()$view

  sub <- overview_subset(view, "P", "A")
  expect_setequal(unique(sub$`Sample ID`), c("S1", "S5", "S7"))
  expect_true(all(sub$Protein == "P" & sub$`Cmp Name` == "A"))
  # B is declared in S1 and S2 without an adduct
  expect_setequal(unique(overview_subset(view, "P", "B")$`Sample ID`), c("S1", "S2"))
  # A control belongs to no complex
  expect_equal(nrow(overview_subset(view, "R", character(0))), 0)
  expect_equal(nrow(overview_subset(view, NULL, "A")), 0)
})

test_that("the Overview colours are the result's fixed colours", {
  view <- overview_run()$view
  key <- color_key(view)

  both <- overview_colors(view, "P", c("A", "B"), "Compounds", key)
  one <- overview_colors(view, "P", "B", "Compounds", key)
  expect_equal(names(both), c("A", "B"))
  # A compound keeps its colour whatever else is picked
  expect_equal(unname(one[["B"]]), unname(both[["B"]]))
  expect_equal(both, key_colors(key, "Compounds", c("A", "B")))
  # A compound left out is muted
  expect_equal(unname(one[["A"]]), status_colors()$muted)

  samples <- overview_colors(view, "P", "A", "Samples", key)
  expect_setequal(names(samples), c("S1", "S5", "S7"))
  expect_equal(samples, key_colors(key, "Samples", names(samples)))
  expect_length(overview_colors(view, "R", character(0), "Samples", key), 0)
  # Without a key, one is made from the table handed in
  expect_equal(overview_colors(view, "P", c("A", "B"), "Compounds"), both)
})

test_that("the compound mass shift card names the compound once there are several", {
  run <- overview_run()
  sub <- overview_subset(run$view, "P", c("A", "B"))

  single <- compound_mass_entries(sub, "A", run$compound_table)
  expect_equal(single$label, "100.0 Da")
  expect_equal(single$count, 3L)

  several <- compound_mass_entries(sub, c("B", "A"), run$compound_table)
  # The mass found comes first, the declared one without a peak after it
  expect_equal(several$label, c("A &middot; 100.0 Da", "B &middot; 200.0 Da"))
  expect_equal(several$present, c(TRUE, FALSE))
  expect_equal(several$count, c(3L, 0L))

  expect_equal(nrow(compound_mass_entries(sub, character(0), run$compound_table)), 0)
})

test_that("the Overview distribution plots the picked complexes with binding", {
  view <- overview_run()$view
  plot <- function(protein, compounds, variable = "Compounds") {
    overview_compound_distribution(
      view, protein, compounds,
      color_variable = variable,
      truncate_names = FALSE,
      distribution_scale = "Maximum"
    )
  }

  # A single compound: one bar per sample it was declared in
  built <- plotly::plotly_build(plot("P", "A"))$x$data
  bars <- built[vapply(built, `[[`, character(1), "type") == "bar"]
  expect_setequal(unlist(lapply(bars, `[[`, "x")), c("S1", "S5", "S7"))

  # B adds nothing to plot, so A is shown on its own
  expect_s3_class(plot("P", c("A", "B"), "Samples"), "plotly")

  # Nothing to plot: no binding, or not measured at all
  expect_null(plot("P", "B"))
  expect_null(plot("Q", "A"))
  expect_equal(overview_distribution_note(view, "P", "B"), "No binding events")
  expect_equal(overview_distribution_note(view, "Q", "A"), "No protein or complex peak found")
  expect_equal(overview_distribution_note(view, "P", character(0)), "No compound selected")
})

test_that("spectra without any assigned peak are drawn", {
  run <- overview_run()
  colors <- overview_colors(run$view, "Q", "A", "Compounds")

  # Q is in neither spectrum, so no peak is assigned
  spectra <- multiple_spectra(
    results_list = run$result,
    samples = c("S3", "S6"),
    color_cmp = colors,
    color_variable = "Compounds",
    hits_summary = run$view
  )
  expect_s3_class(plotly::plotly_build(spectra), "plotly")
})

test_that("a fully converted sample keeps its compound colour in the spectrum", {
  run <- overview_run()
  colors <- c(A = "#FF0000", B = "#00FF00")

  # S7 has no unbound peak left, only the complex
  built <- plotly::plotly_build(spectrum_plot(
    sample = run$result$deconvolution$S7,
    color_cmp = colors,
    color_variable = "Compounds",
    show_peak_labels = TRUE,
    show_mass_diff = FALSE
  ))$x$data
  marker <- Filter(function(t) identical(t$mode, "markers"), built)
  marker_colors <- unlist(lapply(marker, function(t) t$marker$color))
  expect_true("#FF0000" %in% toupper(marker_colors))
})

test_that("the binding interface opens on the Overview", {
  view <- overview_run()$view
  html <- as.character(binding_results_ui(identity, view))

  tabs <- regmatches(html, gregexpr("data-value=\"[^\"]+\"", html))[[1]]
  expect_equal(unique(tabs)[1:2], c("data-value=\"Overview\"", "data-value=\"Sample View\""))
  expect_false(grepl("Compound View|Protein View|Tot. Binding \\[%\\]</", html))

  protein_picker <- regmatches(
    html,
    regexpr("overview_protein_picker.*?</select>", html)
  )
  expect_match(protein_picker, "value=\"P\" data-subtext=\"\" selected=\"selected\">", fixed = TRUE)
  expect_match(protein_picker, "value=\"Q\" data-subtext=\"No hits\">", fixed = TRUE)
  expect_match(protein_picker, "value=\"R\" data-subtext=\"No compounds\">", fixed = TRUE)

  compound_picker <- regmatches(
    html,
    regexpr("overview_compound_picker.*?</select>", html)
  )
  expect_match(compound_picker, "multiple", fixed = TRUE)
  expect_match(compound_picker, "value=\"A\" data-subtext=\"\" selected=\"selected\">", fixed = TRUE)
  expect_match(compound_picker, "value=\"B\" data-subtext=\"No hits\" selected=\"selected\">", fixed = TRUE)

  # Opened on a selection, e.g. from a click in the Hits table
  html <- as.character(binding_results_ui(
    identity,
    view,
    overview = list(protein = "Q", compounds = "A")
  ))
  protein_picker <- regmatches(
    html,
    regexpr("overview_protein_picker.*?</select>", html)
  )
  expect_match(protein_picker, "value=\"Q\" data-subtext=\"No hits\" selected=\"selected\">", fixed = TRUE)
})
