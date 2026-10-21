# Unit tests for the hit screening of the conversion analysis.  Everything runs
# on synthetic peak lists, so no deconvolution results or test corpora are
# needed.

box::use(
  app/logic/conversion_functions[
    add_hits,
    check_sample_table,
    check_table,
    color_key,
    clean_prot_comp_table,
    close_log_block,
    complex_hits,
    compound_mass_entries,
    ensure_trace,
    format_scientific,
    hits_bar_cols,
    hits_base_col,
    hits_col_choices,
    hits_col_tooltip,
    hits_view_table,
    sample_view_col_kind,
    is_complex_row,
    key_colors,
    make_kobs_plot,
    mass_ambiguities,
    mass_shift_card,
    mass_shift_label,
    multiple_spectra,
    nest_log_block,
    overview_choices,
    overview_colors,
    overview_compound_distribution,
    overview_distribution_note,
    overview_has_distribution,
    overview_selection,
    overview_status_note,
    overview_subset,
    predict_peaks,
    proteoform_binding,
    proteoform_limit_note,
    classic_two_step,
    conc_display,
    global_binding_curves,
    legend_key_group,
    make_binding_plot,
    read_uploaded_file,
    render_hits_table,
    render_hits_view,
    select_complex_kinetics,
    stats_boxplot,
    stats_histogram,
    stats_scatter,
    stats_violin,
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

test_that("the hits table pools the proteoforms in its binding columns", {
  multi <- screen(
    mass = c(1000, 1010, 1100, 1110),
    intensity = c(50, 30, 12, 8),
    protein_masses = c(1000, 1010)
  )
  view <- transform_hits(multi)

  # No binding of a proteoform on its own; the total pools them
  expect_false(any(grepl("Prot. Binding", names(view), fixed = TRUE)))
  expect_false("Prot. Binding [%]" %in% hits_col_choices(view, character())$choices)
  expect_equal(unique(view$`Tot. Binding [%]`), 100 * 20 / 100)
})

test_that("every percentage of the hits table is drawn as a bar", {
  multi <- screen(
    mass = c(1000, 1010, 1100, 1110),
    intensity = c(50, 30, 12, 8),
    protein_masses = c(1000, 1010)
  )
  view <- transform_hits(multi)

  # No picker for the bars: the table draws them all
  html <- as.character(hits_results_ui(identity, view, units = character()))
  expect_false(grepl("hits_binding_chart", html, fixed = TRUE))

  pct <- grep("\\[%\\]$", names(view), value = TRUE)
  expect_setequal(hits_bar_cols(view), pct)
  dt <- render_hits_table(
    view,
    concentration_colors = NULL,
    bar_chart = hits_bar_cols(view),
    units = character()
  )
  bar_def <- Filter(
    function(d) identical(d$className, "bar-chart-col"),
    dt$x$options$columnDefs
  )[[1]]
  # DT turns the target names into 0-based column indices
  expect_setequal(unlist(bar_def$targets), match(pct, names(view)) - 1)

  # Exported unrounded and numeric
  export <- prepare_hits_export(view)
  expect_type(export$`Binding [%]`, "double")
  expect_equal(
    export$`Binding [%]`[export$`Cmp Name` != "N/A"],
    c(100 * 12 / 100, 100 * 8 / 100)
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

  # One row for the sample, its compounds listed in the declared order
  expect_equal(nrow(per_adduct), 1)
  expect_equal(per_adduct$`Cmp Name`, "A, B")
  # Only A's single mass shift on its one complex - the empty mass of a row
  # without an adduct is not read as the unused second slot - and nothing for
  # B, which has no complex
  expect_false(any(grepl("Mass 2", names(per_adduct))))
  expect_false(any(grepl("\\bB\\b", grep("Mass", names(per_adduct), value = TRUE))))
  expect_equal(per_adduct$`A·Mass 1`, 100)
  # 1,010 Da is Mass 2 of the protein: proteoform 2
  expect_equal(per_adduct$`PF2 [Da]`, 1010)
  expect_equal(
    per_adduct$`Binding PF2 + 1× A·Mass 1 [%]`,
    10 / 90 * 100
  )
  expect_equal(per_adduct$`Total Binding A·Mass 1 [%]`, 10 / 90 * 100)
  # The binding of each complex, the value its kinetics are fitted to, next
  # to the sample's total
  expect_equal(
    names(per_adduct)[grepl("^Tot\\. Binding", names(per_adduct))],
    c("Tot. Binding [%]", "Tot. Binding (A) [%]", "Tot. Binding (B) [%]")
  )
  expect_equal(per_adduct$`Tot. Binding (A) [%]`, 10 / 90 * 100)
  expect_equal(per_adduct$`Tot. Binding (B) [%]`, 0)
})

test_that("the sample view orders shared columns by mass shift and stoichiometry", {
  # One compound per sample, so the compounds share their columns. A, listed
  # first, binds twice only; B once.
  a <- screen_declared(
    mass = c(1000, 1200),
    intensity = c(50, 50),
    protein_masses = 1000,
    compounds = c(A = 100)
  )
  b <- screen_declared(
    mass = c(1000, 1150),
    intensity = c(50, 50),
    protein_masses = 1000,
    compounds = c(B = 150)
  )
  b$view$`Sample ID` <- "S2"
  compound_table <- rbind(
    a$compound_table,
    b$compound_table[b$compound_table$Compound == "B", ]
  )
  per_adduct <- transform_per_adduct(
    rbind(a$view, b$view),
    proteins_table = a$protein_table,
    compounds_table = compound_table,
    samples_table = rbind(a$sample_table, data.frame(Sample = "S2", Protein = "P", `Compound 1` = "B", check.names = FALSE))
  )

  expect_equal(nrow(per_adduct), 2)
  expect_equal(
    names(per_adduct)[grepl("^(Mass|Binding|Total)", names(per_adduct))],
    c(
      "Mass 1",
      "Binding P + 1× Mass 1 [%]",
      "Binding P + 2× Mass 1 [%]",
      "Total Binding Mass 1 [%]"
    )
  )
  # A sample reads 0 % where it has no complex, also under the columns of
  # the other compound: they are shared
  expect_equal(per_adduct$`Binding P + 1× Mass 1 [%]`, c(0, 50))
  expect_equal(per_adduct$`Binding P + 2× Mass 1 [%]`, c(50, 0))
  expect_equal(per_adduct$`Mass 1`, c(100, 150))
})

test_that("the sample view has a column for every stoichiometry searched", {
  screened <- screen_declared(
    mass = c(1000, 1100),
    intensity = c(50, 50),
    protein_masses = 1000,
    compounds = c(A = 100)
  )
  per_adduct <- function(max_multiples) {
    transform_per_adduct(
      screened$view,
      proteins_table = screened$protein_table,
      compounds_table = screened$compound_table,
      samples_table = screened$sample_table,
      max_multiples = max_multiples
    )
  }

  # Without the Max. Stoichiometry of the run, the ones found
  expect_equal(
    grep("^Binding", names(per_adduct(NULL)), value = TRUE),
    "Binding P + 1× Mass 1 [%]"
  )
  # With it, every one up to it, read as 0 % where nothing was found
  table <- per_adduct(7)
  expect_equal(
    grep("^Binding", names(table), value = TRUE),
    sprintf("Binding P + %d× Mass 1 [%%]", 1:7)
  )
  expect_equal(table$`Binding P + 1× Mass 1 [%]`, 50)
  expect_equal(unname(unlist(table[sprintf("Binding P + %d× Mass 1 [%%]", 2:7)])), rep(0, 6))
  expect_equal(table$`Total Binding Mass 1 [%]`, 50)
})

test_that("the sample view reads 0 % for a measured sample without a complex", {
  bound <- screen_declared(
    mass = c(1000, 1100),
    intensity = c(50, 50),
    protein_masses = 1000,
    compounds = c(A = 100)
  )
  unbound <- screen_declared(
    mass = 1000,
    intensity = 50,
    protein_masses = 1000,
    compounds = c(A = 100)
  )$view
  unbound$`Sample ID` <- "S2"
  # No protein peak at all: nothing was measured
  silent <- screen_declared(
    mass = 500,
    intensity = 50,
    protein_masses = 1000,
    compounds = c(A = 100)
  )$view
  silent$`Sample ID` <- "S3"

  per_adduct <- transform_per_adduct(
    rbind(bound$view, unbound, silent),
    proteins_table = bound$protein_table,
    compounds_table = bound$compound_table,
    samples_table = bound$sample_table,
    max_multiples = 2
  )

  expect_equal(per_adduct$`Sample ID`, c("S1", "S2", "S3"))
  # The compound is named whatever the binding
  expect_equal(per_adduct$`Cmp Name`, c("A", "A", "A"))
  expect_equal(per_adduct$`Binding P + 1× Mass 1 [%]`, c(50, 0, NA))
  expect_equal(per_adduct$`Binding P + 2× Mass 1 [%]`, c(0, 0, NA))
  expect_equal(per_adduct$`Total Binding Mass 1 [%]`, c(50, 0, NA))
  expect_equal(per_adduct$`Mass 1`, c(100, 100, NA))
})

test_that("the sample view gives every proteoform its columns, numbered as declared", {
  screened <- screen_declared(
    mass = c(1000, 1010, 1100, 1110),
    intensity = c(40, 20, 30, 10),
    protein_masses = c(1000, 1010),
    compounds = c(A = 100)
  )
  per_adduct <- transform_per_adduct(
    screened$view,
    proteins_table = screened$protein_table,
    compounds_table = screened$compound_table,
    samples_table = screened$sample_table
  )

  expect_equal(nrow(per_adduct), 1)
  # PF1 and PF2 (Mass 1 and 2 of the protein) hold the masses in place of
  # Theor. Prot. [Da]
  expect_false("Theor. Prot. [Da]" %in% names(per_adduct))
  expect_equal(per_adduct$`PF1 [Da]`, 1000)
  expect_equal(per_adduct$`PF2 [Da]`, 1010)
  expect_match(hits_col_tooltip("PF2 [Da]"), "^Proteoform 2")
  expect_match(hits_col_tooltip("Int. PF2 [%]"), "proteoform 2$")
  expect_equal(
    hits_col_tooltip("Binding PF2 + 2× A·Mass 1* [%]"),
    paste0(
      "Binding [%] of proteoform 2 carrying 2 × mass shift 1 of A. *: not ",
      "the preferred reading of its peak, not counted in the totals"
    )
  )
  expect_equal(
    hits_col_tooltip(c("Total Binding Mass 1 [%]", "A·Mass 1")),
    c(
      paste0(
        "Total binding of mass shift 1: its preferred readings over all ",
        "proteoforms and stoichiometries"
      ),
      "Declared mass shift 1 of A [Da]"
    )
  )
  expect_true(all(
    c("Int. PF1 [%]", "Int. PF2 [%]") %in%
      names(per_adduct)
  ))
  expect_false(any(grepl("Prot. Binding", names(per_adduct), fixed = TRUE)))
  # A reading per proteoform; the total of the shift pools them, as Tot.
  # Binding [%] does
  expect_equal(
    names(per_adduct)[grepl("^(Mass|Binding|Total)", names(per_adduct))],
    c(
      "Mass 1",
      "Binding PF1 + 1× Mass 1 [%]",
      "Binding PF2 + 1× Mass 1 [%]",
      "Total Binding Mass 1 [%]"
    )
  )
  expect_equal(per_adduct$`Binding PF2 + 1× Mass 1 [%]`, 10)
  expect_equal(per_adduct$`Total Binding Mass 1 [%]`, 40)
  expect_equal(
    per_adduct$`Total Binding Mass 1 [%]`,
    per_adduct$`Tot. Binding [%]`
  )
  # One compound per sample: no total per complex next to the sample's
  expect_false(any(grepl("^Tot\\. Binding \\(", names(per_adduct))))
  # Pickers, bars and number formats read the plain column
  expect_equal(
    hits_base_col(c("Int. PF2 [%]", "PF2 [Da]", "Binding PF2 + 1× Mass 1 [%]")),
    c("Int. Prot. [%]", "Theor. Prot. [Da]", "Binding PF2 + 1× Mass 1 [%]")
  )
  expect_equal(
    sample_view_col_kind(c(
      "Binding PF2 + 1× Mass 1 [%]", "Binding P + 3× A·Mass 2* [%]",
      "Total Binding A·Mass 1 [%]", "Tot. Binding (A) [%]", "Mass 1",
      "A·Mass 2", "Binding [%]", "Tot. Binding [%]", "PF1 [Da]"
    )),
    c("binding", "binding", "total", "complex", "mass", "mass", NA, NA, NA)
  )
  dt <- render_hits_table(
    per_adduct,
    concentration_colors = NULL,
    bar_chart = "Int. Prot. [%]",
    units = character(0)
  )
  bar_def <- Filter(
    function(d) identical(d$className, "bar-chart-col"),
    dt$x$options$columnDefs
  )[[1]]
  # DT turns the names into 0-based positions
  expect_setequal(
    unlist(bar_def$targets),
    match(
      c("Int. PF1 [%]", "Int. PF2 [%]"),
      names(per_adduct)
    ) -
      1
  )
  # The Hits interface passes the columns by their full names
  all_bars <- Filter(
    function(d) identical(d$className, "bar-chart-col"),
    render_hits_table(
      per_adduct,
      concentration_colors = NULL,
      bar_chart = hits_bar_cols(per_adduct),
      units = character(0)
    )$x$options$columnDefs
  )[[1]]
  expect_setequal(
    unlist(all_bars$targets),
    match(hits_bar_cols(per_adduct), names(per_adduct)) - 1
  )
  expect_true("Int. PF2 [%]" %in% hits_bar_cols(per_adduct))
})

test_that("the sample view stars a reading that is not preferred and leaves it out of the totals", {
  protein_table <- data.frame(Protein = "P", `Mass 1` = 1000, check.names = FALSE)
  # 1100 is A's first shift once and, 1 Da off, its second shift twice
  compound_table <- data.frame(
    Compound = "A",
    `Mass 1` = 100,
    `Mass 2` = 50.5,
    check.names = FALSE
  )
  sample_table <- data.frame(Sample = "S1", Protein = "P", `Compound 1` = "A", check.names = FALSE)
  result <- suppressMessages(add_hits(
    make_result("S1", c(1000, 1100), c(60, 40)),
    sample_table = sample_table,
    protein_table = protein_table,
    compound_table = compound_table,
    peak_tolerance = 3,
    max_multiples = 2,
    session = fake_session(),
    ns = identity
  ))
  view <- transform_hits(suppressMessages(
    summarize_hits(result, sample_table = sample_table)
  ))
  per_adduct <- transform_per_adduct(view, protein_table, compound_table, sample_table)

  expect_equal(nrow(per_adduct), 1)
  expect_equal(per_adduct$`Binding P + 1× Mass 1 [%]`, 40)
  expect_equal(per_adduct$`Binding P + 2× Mass 2* [%]`, 40)
  expect_equal(per_adduct$`Total Binding Mass 1 [%]`, 40)
  expect_equal(per_adduct$`Total Binding Mass 2 [%]`, 0)
  # A shift read only as the alternative gets no 0 % columns of its own
  expect_false(any(grepl("^Binding P \\+ [0-9]+× Mass 2 ", names(per_adduct))))
  expect_match(
    hits_col_tooltip("Binding P + 2× Mass 2* [%]"),
    "not the preferred reading"
  )

  # Without the declaration the shifts are numbered by size
  undeclared <- transform_per_adduct(view, protein_table, compound_table[0, ], sample_table)
  expect_equal(undeclared$`Mass 1`, 50.5)
  expect_equal(undeclared$`Binding P + 2× Mass 1* [%]`, 40)
  expect_equal(undeclared$`Total Binding Mass 2 [%]`, 40)
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

test_that("a complex shared by two proteoforms is not resolvable per proteoform", {
  # 1200 is 1000 + 2 x 100 and 1100 + 100
  hits <- screen_compounds(
    mass = c(1000, 1100, 1200),
    intensity = c(50, 30, 20),
    protein_masses = c(1000, 1100),
    compounds = list(A = 100)
  )

  # The sample's binding counts the peak once: 40 of 200 (intensities scaled
  # to the base peak), and the row values still add up to it
  expect_equal(unique(hits$`Total % Binding`), 0.2)
  preferred <- hits[hits$Preferred %in% TRUE, ]
  expect_equal(sum(preferred$`% Binding`), unique(hits$`Total % Binding`))

  # Nothing tells how much of the peak belongs to which form: neither gets a
  # binding of its own, and the peak is not counted for either
  binding <- proteoform_binding(hits)
  expect_true(all(is.na(binding$binding)))
  expect_equal(binding$limit, c("shared", "shared"))
  expect_true(all(binding$shared))
  expect_equal(binding$complex, c(0, 0))
  expect_match(proteoform_limit_note("shared"), "not resolvable")
})

test_that("the classic two-step cross-check recovers an exact hyperbola", {
  conc <- c(1, 2, 5, 10, 20, 50)
  kobs_result <- list(kobs_result_table = data.frame(
    kobs = 0.3 * conc / (8 + conc),
    kobs_se = 0.01,
    row.names = as.character(conc)
  ))
  ts <- classic_two_step(kobs_result)
  expect_equal(ts$kinact, 0.3, tolerance = 1e-5)
  expect_equal(ts$KI, 8, tolerance = 1e-5)
  expect_equal(ts$ratio, 0.3 / 8, tolerance = 1e-5)
  expect_equal(ts$n_concentrations, 6)

  # Too few concentrations for two parameters
  short <- list(kobs_result_table = kobs_result$kobs_result_table[1:2, ])
  expect_null(classic_two_step(short))
})

test_that("the binding curve shows the global fit, single fits on request", {
  bt <- data.frame(
    time = rep(c(0, 10, 20), 3),
    predicted_binding = c(0, 0, 0, 0, 40, 60, 0, 70, 80),
    binding = c(NA, 0, 0, NA, 41, 59, NA, 71, 79),
    binding_sd = NA_real_,
    n = 1,
    concentration = factor(rep(c("0", "2", "8"), each = 3), levels = c("8", "2", "0")),
    kobs = rep(c(0, 0.05, 0.12), each = 3),
    kobs_se = NA_real_,
    skip_reason = NA_character_
  )
  kin <- list(
    Model = "linear",
    Ratio = c(Estimate = 0.01),
    Fit = list(KI_hyperbolic = NA_real_),
    Plateaus = data.frame(conc = c(2, 8), plateau = c(80, 90), own = TRUE)
  )

  # One curve per fitted concentration from the global model; the control is
  # not part of the fit
  curves <- global_binding_curves(kin, bt)
  expect_setequal(unique(as.character(curves$concentration)), c("2", "8"))
  at <- curves[curves$concentration == "8" & curves$time == 20, ]
  expect_equal(at$predicted_binding, 90 * (1 - exp(-0.08 * 20)))

  units <- c(Concentration = "Conc. [µM]", Time = "Time [min]")
  labels <- function(plot) {
    built <- plotly::plotly_build(plot)
    unique(unlist(lapply(built$x$data, `[[`, "text")))
  }
  plain <- labels(make_binding_plot(list(binding_table = bt), units = units, points = "mean", kinact_ki_result = kin))
  expect_true("Global fit" %in% plain)
  expect_true("Untreated control" %in% plain)
  expect_false("Fitted alone (for comparison)" %in% plain)
  both <- labels(make_binding_plot(list(binding_table = bt), units = units, points = "mean", kinact_ki_result = kin, show_single = TRUE))
  expect_true("Fitted alone (for comparison)" %in% both)
})

test_that("the binding curve tells four replicate series apart by fill", {
  pts <- expand.grid(time = c(5, 10, 20), series = paste0("R", 1:4), stringsAsFactors = FALSE)
  pts$Sample <- paste0("S_", pts$time, "_", pts$series)
  pts$binding <- 50
  pts$concentration <- factor("8", levels = "8")
  bt <- data.frame(
    time = c(0, 5, 10, 20), predicted_binding = c(0, 30, 45, 55), binding = NA_real_,
    binding_sd = NA_real_, n = 1, concentration = factor("8", levels = "8"),
    kobs = 0.1, kobs_se = NA_real_, skip_reason = NA_character_
  )
  units <- c(Concentration = "Conc. [µM]", Time = "Time [min]")
  built <- plotly::plotly_build(make_binding_plot(
    list(binding_table = bt, binding_points = pts),
    colors = c(`8` = "red"), symbol_map = c(`8` = "square"), units = units, points = "samples"
  ))$x$data
  data_traces <- Filter(function(t) !identical(t$legendgroup, legend_key_group("series")), built)
  symbols <- unlist(lapply(data_traces, function(t) if (identical(t$mode, "markers")) t$marker$symbol))
  expect_setequal(unique(symbols), c("square", "square-open", "square-dot", "square-open-dot"))
  # One legend group names the series
  series_entries <- Filter(function(t) identical(t$legendgroup, legend_key_group("series")), built)
  expect_equal(vapply(series_entries, `[[`, character(1), "name"), paste0("R", 1:4))
})

test_that("the k_obs curve marks kinact, KI and the initial slope on request", {
  conc <- c(2, 5, 10, 20, 40)
  grid <- seq(0, 40, length.out = 51)
  kin <- list(
    Model = "hyperbolic",
    Status = "saturated",
    Ratio = c(Estimate = 0.02),
    Fit = list(KI_hyperbolic = 30),
    Params = matrix(
      c(0.6, 0.1, 30, 8),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("kinact", "KI"), c("Estimate", "Std. Error"))
    ),
    Kobs_Data = rbind(
      data.frame(conc = grid, predicted_kobs = 0.6 * grid / (30 + grid), kobs = NA, kobs_se = NA),
      data.frame(conc = conc, predicted_kobs = NA, kobs = 0.6 * conc / (30 + conc), kobs_se = 0.01)
    )
  )
  units <- c(Concentration = "Conc. [μM]", Time = "Time [min]")
  colors <- stats::setNames(c("red", "orange", "green", "blue", "purple"), conc)
  layout_of <- function(...) plotly::plotly_build(make_kobs_plot(kin, colors, units, ...))$x$layout

  expect_length(layout_of()$shapes, 0)
  marked <- layout_of(show_constants = TRUE)
  # kinact level, KI drop line, half-maximum line, initial slope
  expect_length(marked$shapes, 4)
  texts <- vapply(marked$annotations, `[[`, character(1), "text")
  expect_true(any(grepl("K<sub>I</sub> = 30", texts, fixed = TRUE)))
  expect_true(any(grepl("k<sub>inact</sub> = 0.6", texts, fixed = TRUE)))

  # Without saturation only the slope, and a note
  kin$Status <- "ki_undetermined"
  curved <- layout_of(show_constants = TRUE)
  expect_length(curved$shapes, 1)
  # A linear fit is the slope itself: no guide line on top of it
  kin$Status <- "linear"
  kin$Model <- "linear"
  linear <- layout_of(show_constants = TRUE)
  expect_length(linear$shapes, 0)
  expect_true(any(grepl("not determinable", vapply(linear$annotations, `[[`, character(1), "text"))))

  # Log concentration axis: ticks at the measured concentrations, and the
  # guides in log10 positions with the slope drawn as a line instead
  kin$Status <- "saturated"
  kin$Model <- "hyperbolic"
  logged <- layout_of(show_constants = TRUE, log_conc = TRUE)
  expect_identical(logged$xaxis$type, "log")
  expect_equal(unlist(logged$xaxis$tickvals), conc)
  expect_length(logged$shapes, 3)
  ki_drop <- Filter(function(s) identical(s$x0, s$x1), logged$shapes)[[1]]
  expect_equal(ki_drop$x0, log10(30))
})

test_that("the k_obs curve draws the two-step fit only when switched on", {
  conc <- c(2, 5, 10, 20, 40)
  kin <- list(
    Model = "hyperbolic",
    Status = "saturated",
    Ratio = c(Estimate = 0.02),
    Fit = list(KI_hyperbolic = 30),
    Two_Step = list(kinact = 0.5, KI = 25),
    Kobs_Data = data.frame(conc = conc, predicted_kobs = 0.6 * conc / (30 + conc),
                           kobs = 0.6 * conc / (30 + conc), kobs_se = 0.01)
  )
  units <- c(Concentration = "Conc. [μM]", Time = "Time [min]")
  colors <- stats::setNames(c("red", "orange", "green", "blue", "purple"), conc)
  build <- function(...) plotly::plotly_build(make_kobs_plot(kin, colors, units, ...))$x
  names_of <- function(b) unlist(lapply(b$data, `[[`, "name"))

  off <- build()
  expect_false("Two-step (classic)" %in% names_of(off))
  expect_false(off$layout$legend$itemclick)
  expect_false(off$layout$legend$itemdoubleclick)
  on <- build(show_two_step = TRUE)
  two_step <- Filter(function(t) identical(t$name, "Two-step (classic)"), on$data)
  expect_length(two_step, 1)
  expect_null(two_step[[1]]$visible)
  # Not dotted like the guides to the constants, not dashed like the
  # extrapolation, and listed with the global fit under one title
  expect_equal(two_step[[1]]$line$dash, "dashdot")
  global <- Filter(function(t) identical(t$name, "Global fit"), on$data)[[1]]
  expect_equal(two_step[[1]]$legendgroup, global$legendgroup)
  expect_equal(global$legendgrouptitle$text, "Fit")
  points <- Filter(function(t) t$name %in% as.character(conc), on$data)
  expect_length(unique(vapply(points, `[[`, character(1), "legendgroup")), 1)
  titles <- unlist(lapply(points, function(t) t$legendgrouptitle$text))
  expect_equal(titles, "Concentration [μM]")
})

test_that("a shared peak leaves the other samples of a proteoform measured", {
  hits <- rbind(
    screen_compounds(
      mass = c(1000, 1100, 1200),
      intensity = c(50, 30, 20),
      protein_masses = c(1000, 1100),
      compounds = list(A = 100)
    ),
    # No shared peak in S2: 1000 + 100 only
    within(
      screen_compounds(
        mass = c(1000, 1100, 1300),
        intensity = c(50, 30, 20),
        protein_masses = c(1000, 1100),
        compounds = list(A = 100)
      ),
      Sample <- "S2"
    )
  )
  binding <- proteoform_binding(hits)
  s2 <- binding[binding$sample == "S2", ]
  expect_false(any(s2$shared))
  expect_true(all(!is.na(s2$binding)))
  expect_true(all(binding$shared[binding$sample != "S2"]))
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

  view <- transform_hits(result$hits_summary)
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

test_that("the mass shift hover list stops after its lines and counts the rest", {
  entries <- data.frame(
    label = sprintf("%d.0 Da", 1:40),
    present = rep(c(TRUE, FALSE), each = 20),
    count = rep(c(2L, 0L), each = 20)
  )
  html <- as.character(mass_shift_card(entries, max_tooltip_lines = 10))

  expect_match(html, "37 more", fixed = TRUE)
  expect_match(html, "(20 without peak)", fixed = TRUE)
  expect_match(html, "13.0 Da", fixed = TRUE)
  expect_false(grepl("14.0 Da", html, fixed = TRUE))
  expect_match(html, "27 more not listed", fixed = TRUE)
  # The legend for greyed lines only shows with one of them listed
  expect_false(grepl("Grey:", html, fixed = TRUE))
  expect_false(grepl("overflow-y", html, fixed = TRUE))

  all_listed <- as.character(mass_shift_card(entries, max_tooltip_lines = 50))
  expect_false(grepl("not listed", all_listed, fixed = TRUE))
  expect_match(all_listed, "Grey:", fixed = TRUE)
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

  expect_s3_class(plot("P", c("A", "B"), "Samples"), "plotly")

  # Nothing to plot: no compound, or no sample
  expect_null(plot("P", character(0)))
  expect_null(plot("R", "A"))
  expect_false(overview_has_distribution(view, "R", "A"))
  expect_equal(overview_distribution_note(view, "P", character(0)), "No compound selected")
  expect_equal(overview_distribution_note(view, "R", "A"), "No samples to show")
})

test_that("the Overview distribution keeps the samples and compounds without a hit", {
  view <- overview_run()$view
  built <- function(protein, compounds, show_unmatched = TRUE) {
    plotly::plotly_build(overview_compound_distribution(
      view, protein, compounds,
      color_variable = "Compounds",
      truncate_names = FALSE,
      distribution_scale = "Maximum",
      show_unmatched = show_unmatched
    ))$x
  }
  unmatched_trace <- function(fig) {
    Filter(function(t) identical(t$name, "Unmatched [%]"), fig$data)
  }

  # B is found in no sample: its samples S1 and S2 stay in the plot at 0 %,
  # with A's three, each with its Unmatched [%]
  both <- built("P", c("A", "B"))
  expect_equal(both$layout$xaxis$ticktext, c("A", "B"))
  bars <- Filter(function(t) identical(t$type, "bar"), both$data)
  expect_equal(sum(vapply(bars, function(t) t$x[[1]] == 1, logical(1))), 2)
  expect_true(all(vapply(
    Filter(function(t) t$x[[1]] == 1, bars),
    function(t) t$y[[1]] == 0,
    logical(1)
  )))
  markers <- unmatched_trace(both)
  expect_length(markers, 1)
  expect_length(markers[[1]]$y, 5)
  # A marker sits over its own bar, not at the compound's centre
  expect_setequal(round(unlist(markers[[1]]$x), 6), round(vapply(
    Filter(function(t) !is.na(t$y[[1]]), bars),
    function(t) t$x[[1]] + t$offset[[1]] + t$width[[1]] / 2,
    numeric(1)
  ), 6))

  # A compound found nowhere is plotted on its own as well
  only_b <- built("P", "B")
  expect_setequal(
    unlist(lapply(Filter(function(t) identical(t$type, "bar"), only_b$data), `[[`, "x")),
    c("S1", "S2")
  )

  # Without any protein peak (Q is not in the spectra): no bar, but the
  # samples are there with all of their spectrum unmatched
  unmeasured <- built("Q", "A")
  expect_true(overview_has_distribution(view, "Q", "A"))
  expect_true(all(is.na(unlist(lapply(
    Filter(function(t) identical(t$type, "bar"), unmeasured$data),
    `[[`,
    "y"
  )))))
  q_markers <- unmatched_trace(unmeasured)[[1]]
  expect_setequal(unlist(q_markers$x), c("S3", "S6"))
  expect_equal(as.numeric(unlist(q_markers$y)), c(100, 100))

  # The markers can be switched off
  expect_length(unmatched_trace(built("P", c("A", "B"), FALSE)), 0)
})

test_that("the Overview distribution follows a given sample order", {
  view <- overview_run()$view
  bars_x <- function(order) {
    built <- plotly::plotly_build(overview_compound_distribution(
      view, "P", "A",
      color_variable = "Samples",
      truncate_names = FALSE,
      distribution_scale = "Maximum",
      sample_order = order
    ))
    axis <- built$x$layout$xaxis$categoryarray
    if (length(axis)) as.character(axis) else unique(unlist(lapply(
      Filter(function(t) identical(t$type, "bar"), built$x$data),
      function(t) as.character(t$x)
    )))
  }
  # By binding without an order, as given with one
  expect_equal(bars_x(NULL), c("S1", "S5", "S7"))
  expect_equal(bars_x(c("S7", "S1", "S5")), c("S7", "S1", "S5"))
  # Samples the order does not name keep their place after it
  expect_equal(bars_x(c("S5")), c("S5", "S1", "S7"))
})

test_that("the Hits table offers every colour variable", {
  hits <- overview_run()$view
  html <- as.character(hits_results_ui(identity, hits, units = character()))
  for (v in c("Protein", "Compound", "Sample", "None")) {
    expect_match(html, paste0(">", v, "<"), fixed = TRUE)
  }
})

test_that("the time view takes times from the sample table and keeps replicates apart", {
  # Names say nothing about the time; the declared table does
  samples <- c("A", "B", "C", "D")
  spectrum <- function(shift) {
    mass <- data.frame(mass = seq(1000, 1200, by = 1), intensity = 1)
    mass$intensity[mass$mass == 1100 + shift] <- 100
    list(mass = mass, highlight_peaks = mass[mass$mass == 1100 + shift, ])
  }
  plot_data <- stats::setNames(lapply(c(0, 2, 4, 6), spectrum), samples)
  hits <- data.frame(
    `Sample ID` = samples,
    Replicate = c("R1", "R2", "R1", "R2"),
    `Time [h]` = c(1, 1, 4, 4),
    `Meas. Prot. [Da]` = NA_real_,
    check.names = FALSE
  )
  units <- c(Concentration = "Conc. [µM]", Time = "Time [h]")

  for (cubic in c(TRUE, FALSE)) {
    built <- plotly::plotly_build(multiple_spectra(
      NULL,
      samples,
      cubic = cubic,
      time = TRUE,
      hits_summary = hits,
      units = units,
      plot_data = plot_data
    ))$x$data
    lines <- Filter(function(t) identical(t$mode, "lines"), built)

    # Every replicate its own spectrum, named by its time point
    expect_length(lines, 4)
    expect_setequal(vapply(lines, `[[`, "", "name"), c("1", "4"))
    # One legend row per time point
    expect_equal(sum(vapply(lines, function(t) isTRUE(t$showlegend), NA)), 2)
    hovers <- vapply(lines, `[[`, "", "hovertemplate")
    expect_true(any(grepl("Time: %{fullData.name} h (R2)", hovers, fixed = TRUE)))
  }

  # Cubic: the replicates of a time side by side, closer than the next time
  built <- plotly::plotly_build(multiple_spectra(
    NULL, samples, cubic = TRUE, time = TRUE,
    hits_summary = hits, units = units, plot_data = plot_data
  ))$x$data
  lines <- Filter(function(t) identical(t$mode, "lines"), built)
  z <- vapply(lines, function(t) as.numeric(t$z)[1], 0)
  names(z) <- paste(vapply(lines, `[[`, "", "name"), seq_along(z))
  within <- abs(diff(z[1:2]))
  between <- abs(mean(z[3:4]) - mean(z[1:2]))
  expect_gt(within, 0)
  expect_lt(within, between / 2)
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

test_that("uploaded tables without a trailing newline read without warnings", {
  txt <- tempfile(fileext = ".txt")
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(c(txt, csv)))
  writeChar("Protein\tMass\nP1\t25000", txt, eos = NULL)
  writeChar("Compound;Mass\nC1;350.2", csv, eos = NULL)

  expect_no_warning(df <- read_uploaded_file(txt, "txt"))
  expect_equal(dim(df), c(2L, 2L))
  expect_equal(df[2, 2], "25000")

  expect_no_warning(df <- read_uploaded_file(csv, "csv"))
  expect_equal(dim(df), c(2L, 2L))
  expect_equal(df[2, 1], "C1")
})

test_that("plots without any trace build without plotly's trace type warning", {
  message_plot <- plotly::plot_ly() |>
    plotly::layout(annotations = list(list(text = "Nothing to show")))
  built <- expect_no_warning(plotly::plotly_build(ensure_trace(message_plot)))
  expect_length(built$x$data, 1)
  expect_equal(built$x$data[[1]]$type, "scatter")
  expect_false(built$x$data[[1]]$showlegend)

  # Plots that already carry traces are left alone
  drawn <- plotly::plot_ly() |>
    plotly::add_markers(x = 1:3, y = 1:3, name = "a") |>
    plotly::add_markers(x = 1:3, y = 3:1, name = "b")
  expect_identical(ensure_trace(drawn), drawn)
  typed <- plotly::plot_ly(x = 1, y = 1, type = "bar")
  expect_identical(ensure_trace(typed), typed)
})

test_that("the Table Views are subsets of the hits table", {
  view <- overview_run()$view
  units <- character()
  cols <- hits_col_choices(view, units)

  # The identifying columns and the total binding are always shown
  fixed <- c("Sample ID", "Protein", "Cmp Name", "truncSample_ID")
  expect_false(any(c(fixed, "Tot. Binding [%]") %in% cols$choices))
  expect_true(all(cols$selected %in% cols$choices))
  expect_false(any(c("Well", "Meas. Prot. [Da]") %in% cols$selected))

  full <- hits_view_table(view, cols$selected, units)
  rows <- function(keep) {
    out <- full[keep, ]
    rownames(out) <- NULL
    out
  }
  expect_equal(
    match("Tot. Binding [%]", names(full)),
    match("Binding [%]", names(full)) + 1
  )
  expect_named(
    hits_view_table(view, NULL, units),
    c(fixed, "Tot. Binding [%]")
  )

  # Overview: the rows of the protein with the picked compounds
  overview <- hits_view_table(
    overview_subset(view, "P", "A"),
    cols$selected,
    units
  )
  expect_gt(nrow(overview), 0)
  expect_equal(
    overview,
    rows(full$Protein == "P" & full$`Cmp Name` %in% "A")
  )

  # Sample View: the rows of the sample
  sample <- hits_view_table(view, cols$selected, units, samples = "S1")
  expect_equal(sample, rows(full$`Sample ID` == "S1"))
})

test_that("the Table Views draw every percentage as a bar", {
  view <- overview_run()$view
  tbl <- hits_view_table(
    view,
    hits_col_choices(view, character())$choices,
    character()
  )
  dt <- render_hits_view(
    tbl,
    colors = key_colors(color_key(view), "Compounds", c("A", "B")),
    color_variable = "Compounds",
    units = character()
  )

  expect_equal(dt$x$options$dom, "t")
  expect_null(dt$x$options$language)
  bars <- Filter(
    function(d) identical(d$className, "bar-chart-col"),
    dt$x$options$columnDefs
  )
  expect_length(bars, 1)
  # DT turns the target names into 0-based column indices
  expect_setequal(
    names(tbl)[bars[[1]]$targets + 1],
    intersect(hits_bar_cols(tbl), names(tbl))
  )

  # Short sample IDs on display only
  tbl$truncSample_ID <- paste0(tbl$`Sample ID`, "_short")
  short <- render_hits_view(
    tbl,
    colors = NULL,
    color_variable = "Samples",
    truncate_names = TRUE,
    units = character()
  )
  expect_true(all(endsWith(short$x$data$`Sample ID`, "_short")))

  empty <- render_hits_view(
    tbl[0, ],
    colors = NULL,
    color_variable = "Compounds",
    units = character(),
    empty_message = "No compound selected"
  )
  expect_equal(empty$x$options$language$emptyTable, "No compound selected")
})

test_that("an empty colour map leaves the rows uncoloured", {
  # Nothing picked in the Overview: the colours come as an empty map, which
  # DT::styleEqual() would turn into a callback the browser cannot parse
  tbl <- data.frame(`Sample ID` = character(0), `Cmp Name` = character(0), check.names = FALSE)
  for (cv in c("Compounds", "Samples")) {
    dt <- render_hits_view(
      tbl,
      colors = stats::setNames(character(0), character(0)),
      color_variable = cv,
      units = character(),
      empty_message = "No compound selected"
    )
    expect_false(grepl("''", dt$x$options$rowCallback, fixed = TRUE))
  }
})

test_that("the statistics plots leave out a sample without a value, without a warning", {
  # A sample without any protein or complex peak has NA percentages; plotly
  # warns about each NA it is handed ("Ignoring 1 observations")
  hs <- data.frame(
    Sample = paste0("S", 1:6),
    Protein = c("P", "P", "P", "Q", "Q", "Q"),
    `% Correct` = c(90, 80, NA, 70, 65, 60),
    `% Unmatched` = c(10, 20, NA, 30, 35, 40),
    `Total % Binding` = c(0.5, 0.4, NA, 0.2, 0.2, 0.1),
    check.names = FALSE
  )
  drawn <- function(trace) sum(!is.na(trace$y))

  for (show in c("Correct", "Unmatched")) {
    expect_no_warning(built <- plotly::plotly_build(stats_histogram(hs, show = show)))
    expect_equal(sum(built$x$data[[1]]$x >= 0), 5)

    expect_no_warning(built <- plotly::plotly_build(stats_boxplot(hs, show = show)))
    expect_equal(drawn(built$x$data[[1]]), 5)
    expect_false(anyNA(unlist(lapply(built$x$layout$shapes, `[`, c("y0", "y1")))))

    expect_no_warning(built <- plotly::plotly_build(stats_scatter(hs, show = show)))
    expect_equal(sum(vapply(built$x$data, drawn, numeric(1))), 5)

    for (inner in c("Box", "Points")) {
      expect_no_warning(built <- plotly::plotly_build(
        stats_violin(hs, inner = inner, show = show)
      ))
      expect_false(anyNA(unlist(lapply(built$x$data, `[[`, "y"))))
    }
  }
})

test_that("the hit rate scatter draws a sample without a protein peak at 0 %, open", {
  # S3 has no protein or complex peak: no binding, but all of its spectrum
  # unmatched - an unexpected mass shift
  hs <- data.frame(
    Sample = paste0("S", 1:3),
    Protein = "P",
    `% Correct` = c(90, 80, 0),
    `% Unmatched` = c(10, 20, 100),
    `Total % Binding` = c(0.5, 0, NA),
    check.names = FALSE
  )
  expect_no_warning(built <- plotly::plotly_build(
    stats_scatter(hs, show = "Unmatched")
  )$x$data)
  points <- Filter(function(t) identical(t$name, "P"), built)[[1]]
  expect_equal(as.character(unlist(points$customdata)), c("S1", "S2", "S3"))
  # Jittered by up to 0.6 points
  expect_lt(abs(unlist(points$x)[3]), 1)
  expect_lt(abs(unlist(points$y)[3] - 100), 1)
  expect_equal(
    as.character(unlist(points$marker$symbol)),
    c("circle", "circle", "circle-open")
  )
  expect_match(unlist(points$text)[3], "N/A", fixed = TRUE)
  # The open marker is keyed in the legend, only when there is one
  expect_true(any(vapply(built, function(t) identical(t$name, "No protein or complex peak"), logical(1))))
  hs$`Total % Binding`[3] <- 0.1
  expect_false(any(vapply(
    plotly::plotly_build(stats_scatter(hs))$x$data,
    function(t) identical(t$name, "No protein or complex peak"),
    logical(1)
  )))
})

test_that("the Overview distribution carries the full sample ID for a click", {
  view <- overview_run()$view
  view$truncSample_ID <- paste0("short-", view$`Sample ID`)
  ids <- function(compounds) {
    built <- plotly::plotly_build(overview_compound_distribution(
      view, "P", compounds,
      color_variable = "Compounds",
      truncate_names = TRUE,
      distribution_scale = "Maximum"
    ))$x$data
    clickable <- Filter(function(t) identical(t$type, "bar") || identical(t$name, "Unmatched [%]"), built)
    unique(unlist(lapply(clickable, `[[`, "customdata")))
  }
  # Truncated names on the axis, the full IDs to open
  expect_setequal(ids("A"), c("S1", "S5", "S7"))
  expect_setequal(ids(c("A", "B")), c("S1", "S2", "S5", "S7"))
})

test_that("concentrations in molar units read as powers of ten on screen", {
  expect_equal(
    conc_display(c("8e-05", "2.5e-06", "1e-04", "2.5", "937.5", NA)),
    c("8×10⁻⁵", "2.5×10⁻⁶", "1×10⁻⁴", "2.5", "937.5", NA)
  )
  expect_equal(conc_display(c(8e-05, 40)), c("8×10⁻⁵", "40"))
})
