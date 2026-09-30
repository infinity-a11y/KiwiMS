# Unit tests for the hit screening of the conversion analysis.  Everything runs
# on synthetic peak lists, so no deconvolution results or test corpora are
# needed.

box::use(
  app/logic/conversion_functions[
    add_hits,
    clean_prot_comp_table,
    mass_shift_label,
    summarize_hits,
    unbound_label,
    unbound_species
  ],
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
