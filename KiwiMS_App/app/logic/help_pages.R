# app/logic/help_pages.R
#
# Content of the Help modals. Each list maps the input id of a Help button to a
# function building its modal; bind_help() in the module server wires them up.
# Buttons explaining the same thing share one page. The layout comes from
# help_modal.R, so the pages only say what to show.

box::use(
  app /
    logic /
    help_modal[
      conc_c,
      frac,
      help_def,
      help_defs,
      help_formula,
      help_link,
      help_modal,
      help_note,
      help_section,
      K_i,
      k_inact,
      k_obs,
      math,
      mvar,
      op,
      rxn,
    ],
  app / logic / conversion_constants[kinetics_settings, run_limits],
)

html_sub <- htmltools::tags$sub
html_sup <- htmltools::tags$sup

unidec_wiki <- "https://github.com/michaelmarty/UniDec/wiki/"

# Shared formulas ----

binding_formula <- function() {
  help_formula(
    "Binding [%]",
    op("="),
    frac(
      shiny::tagList("Σ", mvar("I", "complex")),
      shiny::tagList(
        "Σ",
        mvar("I", "complex"),
        op("+"),
        "Σ",
        mvar("I", "unbound")
      )
    ),
    op("×"),
    "100"
  )
}

exponential_formula <- function() {
  help_formula(
    "Binding(",
    mvar("t"),
    ")",
    op("="),
    mvar("P"),
    op("·"),
    "(1",
    op("−"),
    "e",
    html_sup("−", k_obs(), "·", mvar("t")),
    ")"
  )
}

hyperbolic_formula <- function() {
  help_formula(
    k_obs(),
    op("="),
    frac(
      shiny::tagList(k_inact(), op("·"), conc_c()),
      shiny::tagList(K_i(), op("+"), conc_c())
    )
  )
}

linear_formula <- function() {
  help_formula(k_obs(), op("="), frac(k_inact(), K_i()), op("·"), conc_c())
}

scheme_formula <- function() {
  help_formula(
    "E + C",
    rxn("⇌", K_i()),
    "E·C",
    rxn("⟶", k_inact()),
    "E–C"
  )
}

correct_formula <- function() {
  help_formula(
    "Correct [%]",
    op("="),
    frac(
      shiny::tagList("Σ", mvar("I", "assigned peaks")),
      shiny::tagList("Σ", mvar("I", "all peaks"))
    ),
    op("×"),
    "100"
  )
}

ratio_title <- function() {
  shiny::span(k_inact(), " / ", K_i())
}

# Shared paragraphs ----

# The ± and the bracketed interval on the kinact, KI and kinact/KI cards
global_error_note <- function(symbol) {
  shiny::p(
    shiny::strong("± on the card"),
    " is the standard error of ",
    symbol,
    " in the global fit: how precisely the fit determines the parameter, ",
    "not the spread of the measurements. The bracketed ",
    shiny::strong("95 % CI"),
    " is a bootstrap interval from ",
    kinetics_settings$bootstrap_n,
    " refits of the same fit with resampled residuals. It is the more ",
    "reliable range of the two, because it does not assume the estimate is ",
    "normally distributed."
  )
}

replicate_note <- function() {
  help_note(
    "Both ranges assume that every sample is a separate incubation. ",
    "Repeated injections of one incubation declared as replicates make them ",
    "too narrow; the Samples Declaration help explains how to declare ",
    "replicates."
  )
}

# Columns of the hit tables (Hits Table of a concentration, Table View)
hit_columns <- function() {
  help_defs(
    help_def(
      "Well / Sample ID / Replicate",
      "Plate well (from the experiment configuration), sample name and ",
      "replicate series."
    ),
    help_def(
      "Conc. / Time",
      "Compound concentration and incubation time (kinetics runs only)."
    ),
    help_def(
      "Theor. Prot. [Da]",
      "Declared mass of the protein species the row belongs to. A protein ",
      "declared with several masses has one species per mass."
    ),
    help_def(
      "Meas. Prot. [Da]",
      "Mass of the unbound peak assigned to that species; N/A when no ",
      "unbound peak was found within the Peak Tolerance (for example a fully ",
      "converted protein)."
    ),
    help_def("Δ Prot. [Da]", "Difference between declared and measured mass."),
    help_def("Int. Prot. [%]", "Intensity of the unbound peak of the species."),
    help_def("Peak Signal [Da]", "Mass of the complex peak."),
    help_def("Int. Cmp [%]", "Intensity of the complex peak."),
    help_def("Cmp Name", "Compound the peak was assigned to."),
    help_def(
      "Theor. Cmp [Da]",
      "Declared mass shift of the compound, for one bound molecule."
    ),
    help_def(
      "Δ Cmp [Da]",
      "Deviation of the observed mass shift from the predicted one: ",
      math(
        "|",
        mvar("n"),
        op("·"),
        "shift",
        op("−"),
        "(peak",
        op("−"),
        "species mass)|"
      ),
      "."
    ),
    help_def(
      "Bind. Stoich.",
      "Number n of compound molecules bound in the complex."
    ),
    help_def(
      "Preferred",
      "Whether this reading names the peak. Only shown when a peak fitted ",
      "more than one compound mass shift or stoichiometry (see Preferred ",
      "Assignment in the sidebar)."
    ),
    help_def(
      "Binding [%]",
      "Share of this complex peak in the protein signal of the sample. A ",
      "peak claimed by several proteoforms or compounds is split evenly ",
      "between them, so the rows still add up to the total."
    ),
    help_def(
      "Tot. Binding [%]",
      "Binding of the sample: all complex peaks together (see formula above)."
    ),
    help_def(
      "Prot. Binding [%]",
      "Binding of this proteoform on its own: its complexes over its own ",
      "unbound plus complex signal. Only present when a protein was declared ",
      "with several masses."
    ),
    help_def(
      "Unmatched [%] / Correct [%]",
      "Share of the spectrum's peak intensity the assignments do not / do ",
      "explain; see the Quality help."
    )
  )
}

intensity_note <- function() {
  shiny::p(
    "Intensities are the peak heights reported by the deconvolution, in % ",
    "(with the default Peak Normalization, of the summed height of all ",
    "peaks). A declared protein-compound pair without a complex is listed ",
    "with N/A in the compound columns, so every pair of a sample appears."
  )
}

# Deconvolution module ----

charge_range_page <- function() {
  help_modal(
    "Charge Range",
    shiny::p(
      "The charge states UniDec may assign to the peaks of the m/z ",
      "spectrum. With Low = 10 and High = 25, every ion is read as 10+ to ",
      "25+; signal of other charge states is forced onto wrong masses or ",
      "left unexplained."
    ),
    help_section(
      "Choosing the range",
      shiny::p(
        "In positive mode, an ion of mass ",
        mvar("M"),
        " carrying ",
        mvar("z"),
        " protons appears at"
      ),
      help_formula(
        mvar("m"),
        "/",
        mvar("z"),
        op("="),
        frac(
          shiny::tagList(mvar("M"), op("+"), mvar("z"), op("·"), "1.007 Da"),
          mvar("z")
        )
      ),
      shiny::p(
        "The range has to include every charge state that falls into the ",
        "m/z range for the masses of the mass range. The highest charge ",
        "needed is about"
      ),
      help_formula(
        mvar("z", "max"),
        op("≈"),
        frac(
          mvar("M", "max"),
          shiny::tagList("(", mvar("m"), "/", mvar("z"), ")", html_sub("min"))
        )
      ),
      shiny::p(
        "for example 60,000 Da / 710 m/z ≈ 85. Below that, the high charge ",
        "states of a large protein are read as wrong masses, which shows up ",
        "as extra peaks a few hundred Da apart or at half the protein mass."
      )
    ),
    help_note(
      "Start with a wide range and narrow it to remove artefacts, such as ",
      "peaks at half or twice the protein mass."
    ),
    help_link(
      paste0(unidec_wiki, "Deconvolution-Parameters#charge-range"),
      "UniDec Wiki – Charge Range"
    )
  )
}

mz_range_page <- function() {
  help_modal(
    "Deconvolution Range [m/z]",
    shiny::p(
      "The m/z window of the measured spectrum that is deconvoluted. ",
      "Signal outside it is ignored."
    ),
    shiny::p(
      "It should enclose the whole charge-state envelope of the protein and ",
      "its complexes, and leave out regions holding only noise, solvent ",
      "clusters or small molecules (usually the low m/z end). Cutting into ",
      "the envelope loses charge states, which weakens the deconvolution and ",
      "can shift the intensities of protein and complexes against each other."
    ),
    shiny::p(
      "Together with the charge range it decides which masses can be found: ",
      "a mass is only seen if some charge state of the charge range places ",
      "it inside this window."
    ),
    help_note(
      "To check the window, open a deconvoluted sample and switch its ",
      "Spectrum card to Raw m/z (Settings)."
    )
  )
}

mass_range_page <- function() {
  help_modal(
    "Mass Range",
    shiny::p(
      "The masses, in Da, the deconvoluted (zero-charge) spectrum may ",
      "contain. UniDec assigns m/z peaks only to masses inside this range."
    ),
    shiny::p(
      "Unlike the m/z range, which selects the data, the mass range limits ",
      "the result. It has to reach from the lightest declared protein mass ",
      "to the heaviest complex looked for: the protein plus Max. ",
      "Stoichiometry times the largest compound mass shift. Too narrow, and ",
      "complexes are cut off; much too wide, and m/z peaks can be explained by ",
      "spurious masses, such as artefacts at half or twice the true mass."
    ),
    help_section(
      "Example",
      shiny::p(
        "Protein 33,000 Da, compound mass shift 450 Da, Max. Stoichiometry ",
        "3: the range must cover 33,000 – 34,350 Da, for example ",
        "30,000 – 40,000 Da."
      )
    ),
    help_link(
      paste0(unidec_wiki, "Deconvolution-Parameters#mass-range"),
      "UniDec Wiki – Mass Range"
    )
  )
}

retention_time_page <- function() {
  help_modal(
    "Retention Time",
    shiny::p(
      "The scans acquired within this window of the run are summed into one ",
      "spectrum, which is then deconvoluted."
    ),
    shiny::p(
      "Set it to the elution window of the protein. This keeps out salts and ",
      "buffer components eluting early and the wash at the end, which only ",
      "add noise. Leave Start blank to read from the first scan and End blank ",
      "to read to the last; both blank uses the whole acquisition."
    ),
    help_note(
      "A window that selects no scan of a file, for example a start time ",
      "after the end of the run, makes that sample fail with “The elution ",
      "window selected no scans”."
    )
  )
}

peak_parameters_page <- function() {
  help_modal(
    "Peak Parameters",
    shiny::p(
      "After the deconvolution, UniDec picks peaks from the zero-charge mass ",
      "spectrum. These peaks, mass and intensity, are all the binding ",
      "analysis sees: every unbound protein, complex and unmatched signal is ",
      "one of them."
    ),
    help_section(
      "Detection Window [Da]",
      shiny::p(
        "A point counts as a peak only if it is the highest point within ± ",
        "this window. Of two peaks closer than the window, only the taller one ",
        "is reported."
      ),
      help_note(
        "Keep the window smaller than the smallest mass difference that has ",
        "to be resolved: the smallest compound mass shift and the spacing of ",
        "declared protein masses. A complex peak within the window of a taller ",
        "unbound peak is not reported and reads as 0 % binding."
      )
    ),
    help_section(
      "Peak Normalization",
      shiny::p(
        "How the intensities of the reported peaks are scaled: ",
        shiny::strong("No normalization"),
        " keeps the scale of the deconvoluted spectrum, ",
        shiny::strong("Max Normalization"),
        " sets the tallest peak to 100, ",
        shiny::strong("Normalization to Sum"),
        " makes all peaks add up to 100."
      ),
      shiny::p(
        "Binding, Correct and Unmatched are ratios of peak intensities and do ",
        "not change with the normalization; it only changes the intensity ",
        "values shown in tables and spectra."
      )
    ),
    help_section(
      "Threshold",
      shiny::p(
        "Minimum peak height as a fraction (0 – 1) of the tallest point of ",
        "the deconvoluted spectrum. A threshold of 0.05 ignores everything ",
        "below 5 % of the tallest peak; 0 accepts every local maximum."
      ),
      shiny::p(
        "A peak below the threshold counts as zero in the binding ",
        "calculation: a missed complex peak reads 0 % binding, a missed ",
        "unbound peak 100 %. Lower it when small species matter, such as ",
        "early time points, near-complete conversion or a minor proteoform; ",
        "raise it when noisy spectra produce spurious low peaks, which add to ",
        "Unmatched [%]."
      )
    ),
    help_link(
      paste0(unidec_wiki, "Peak-Selection-and-Plotting#picking-peaks"),
      "UniDec Wiki – Picking Peaks"
    )
  )
}

sample_rate_page <- function() {
  help_modal(
    "Sample Rate (Resolution)",
    shiny::p(
      "The spacing of the mass axis of the deconvoluted spectrum. With ",
      "0.5 Da, the spectrum has a point every 0.5 Da, and every peak mass is ",
      "a multiple of 0.5 Da."
    ),
    shiny::p(
      "It limits how precisely a peak mass is reported: a peak at ",
      "33,417.3 Da reads 33,417.5 Da with 0.5 Da, and 33,420 Da with 10 Da. ",
      "Keep it well below the Peak Tolerance of the binding analysis, so the ",
      "rounding cannot push a peak out of tolerance. Smaller values give ",
      "finer masses but make the deconvolution slower."
    ),
    help_link(
      paste0(unidec_wiki, "Deconvolution-Parameters#sample-rate"),
      "UniDec Wiki – Sample Rate"
    )
  )
}

decon_spectrum_page <- function() {
  help_modal(
    "Spectrum",
    shiny::p(
      "The spectrum of the sample selected above (or clicked in the Well ",
      "Plate). The Settings switch between two views:"
    ),
    help_defs(
      help_def(
        "Deconvoluted",
        "The zero-charge mass spectrum computed by UniDec: intensity against ",
        "mass [Da]. Its peaks are what the binding analysis works with."
      ),
      help_def(
        "Raw m/z",
        "The measured spectrum the deconvolution started from: intensity ",
        "against m/z, with the charge-state envelope of the protein."
      ),
      help_def("Annotate Mass", "Labels the peaks with their mass."),
      help_def(
        "Show Metrics",
        "Lists the statistics UniDec reports for the sample in a table ",
        "beside the spectrum (see below)."
      )
    ),
    shiny::p(
      "Check that the unbound protein and its expected complexes appear as ",
      "separate peaks. If they merge, revisit the Peak Parameters; if peaks ",
      "appear at half or twice the protein mass, the charge or mass range."
    ),
    shiny::p(
      "Once the run is finished and more than one sample was deconvoluted, ",
      "Show All (selected at the end of the run) draws the deconvoluted ",
      "spectra of all of them together, failed samples left out. The ",
      "Settings then switch between Cubic (one spectrum behind the other) ",
      "and Planar (all on the same axes)."
    ),
    shiny::p(
      "A sample that failed shows the reason instead of a spectrum; Copy ",
      "puts the error message on the clipboard."
    ),
    shiny::p("The metrics of a sample:"),
    help_defs(
      help_def(
        "Fitting Error",
        "Sum of squared differences between the measured m/z spectrum and ",
        "the spectrum rebuilt from the deconvolution result. Lower is better. ",
        "It depends on the intensity scale, so compare it between samples of ",
        "one run rather than against a fixed limit."
      ),
      help_def("Computation Time [s]", "Time UniDec spent on the sample."),
      help_def(
        "Iteration Count",
        "Iterations the deconvolution ran until it converged or reached its ",
        "limit."
      ),
      help_def(
        "UniScore (Quality)",
        "Overall quality from 0 to 1: the R² of the fit times the average ",
        "score of the peaks, weighted towards the most intense ones. A peak ",
        "scores high when it has a clean shape and plausible width, a smooth ",
        "charge-state distribution, and its charge states explain it uniquely. ",
        "Values close to 1 mean a clean, unambiguous deconvolution."
      ),
      help_def(
        "Sigma [m/z]",
        "Peak width (FWHM) assumed for the m/z peaks."
      ),
      help_def(
        "Charge Sigma [z]",
        "Width of the charge-state smoothing: neighbouring charge states are ",
        "pushed towards similar intensities."
      ),
      help_def(
        "Beta (Suppression)",
        "Strength of the softmax applied to the charge-state assignments, ",
        "which suppresses ambiguous assignments (artefacts); 0 switches it off."
      ),
      help_def(
        "Point Sigma",
        "Width of the point smoothing: neighbouring data points are pushed ",
        "towards the same charge state."
      )
    ),
    help_note(
      "The last four are deconvolution settings, not results. They are ",
      "listed so the run can be reproduced."
    )
  )
}

well_plate_page <- function() {
  help_modal(
    "Well Plate",
    shiny::p(
      "One tile per sample of the run. When the experiment configuration ",
      "gives every sample a well, the tiles are those wells. Otherwise the ",
      "samples fill the tiles in the order they are processed, left to right ",
      "and top to bottom."
    ),
    help_defs(
      help_def(
        "Coloured",
        "Deconvoluted. The colour and the number on the tile give how many ",
        "peaks UniDec detected in the sample."
      ),
      help_def(
        "Cross",
        "Deconvolution failed; hover for the sample and select it to see why."
      ),
      help_def("Grey", "Not deconvoluted yet, or no sample in this well.")
    ),
    shiny::p(
      "Click a tile to show its sample in the Spectrum card."
    )
  )
}

#' @export
deconvolution_help <- list(
  charge_range_tooltip_bttn = charge_range_page,
  mz_range_tooltip_bttn = mz_range_page,
  mass_range_tooltip_bttn = mass_range_page,
  retention_time_tooltip_bttn = retention_time_page,
  peak_parameter_tooltip_bttn = peak_parameters_page,
  sample_rate_tooltip_bttn = sample_rate_page,
  mass_spectra_tooltip_bttn = decon_spectrum_page,
  well_plate_tooltip_bttn = well_plate_page
)

# Conversion sidebar ----

peak_tolerance_page <- function() {
  help_modal(
    "Peak Tolerance",
    shiny::p(
      "The largest deviation, in Da, allowed between a detected peak and a ",
      "mass the declaration predicts. Predicted are every declared protein ",
      "mass (the unbound species) and every complex: a protein mass plus a ",
      "compound mass shift times the stoichiometry ",
      mvar("n"),
      ". A peak is assigned when"
    ),
    help_formula(
      "|",
      mvar("m", "peak"),
      op("−"),
      "(",
      mvar("M", "protein"),
      op("+"),
      mvar("n"),
      op("·"),
      mvar("Δm", "compound"),
      ")|",
      op("≤"),
      "tolerance"
    ),
    help_section(
      "Example",
      shiny::p("Protein 20,000 Da, compound mass shift 300 Da, tolerance 3 Da:"),
      help_defs(
        help_def("20,001.2 Da", "Unbound protein (Δ 1.2 Da) ✓"),
        help_def("20,301.8 Da", "Complex, 1 × 300 Da (Δ 1.8 Da) ✓"),
        help_def("20,598.0 Da", "Complex, 2 × 300 Da (Δ 2.0 Da) ✓"),
        help_def(
          "20,305.5 Da",
          "Not assigned (Δ 5.5 Da) ✗ – counted as unmatched"
        )
      )
    ),
    help_section(
      "Choosing the value",
      shiny::p(
        "Base it on the mass accuracy of the deconvoluted spectra: the Δ ",
        "Prot. column of the hits shows how far the unbound peaks lie from ",
        "their declared masses. Keep it above the Sample Rate of the ",
        "deconvolution."
      ),
      shiny::p(
        "Too tight, and real complexes are missed and count as unmatched. ",
        "Too wide, and one peak can match several predictions: masses closer ",
        "than twice the tolerance compete for the same peak, and the ",
        "Preferred Assignment rule decides between them."
      )
    )
  )
}

max_stoichiometry_page <- function() {
  help_modal(
    "Max. Stoichiometry",
    shiny::p(
      "The highest number of compound molecules per protein molecule the ",
      "hit search looks for. With 3, complexes carrying 1, 2 or 3 molecules ",
      "of a compound are predicted, for each of its declared mass shifts; the ",
      "peak of a 4:1 complex stays unassigned and counts as unmatched."
    ),
    shiny::p(
      "Higher values capture multiple labelling, for example of several ",
      "reactive residues, but add predicted masses and with them the chance ",
      "that an unrelated peak matches by coincidence. Choose the highest ",
      "stoichiometry that is chemically plausible, and make sure the mass ",
      "range of the deconvolution reaches the heaviest complex."
    )
  )
}

hit_preference_page <- function() {
  help_modal(
    "Preferred Assignment",
    shiny::p(
      "A peak can fit the same compound on the same protein in more than one ",
      "way: two declared ",
      shiny::strong("mass shifts"),
      " within the Peak Tolerance of each other, or one mass shift at a ",
      shiny::strong("stoichiometry"),
      " that matches another one at a different stoichiometry."
    ),
    shiny::p(
      "Exact multiples, such as 266 Da declared next to 133 Da, are refused ",
      "in the Compounds table: the stoichiometry search already covers them. ",
      "The rule resolves the remaining cases, where shifts are only close to ",
      "each other or to a multiple."
    ),
    shiny::p(
      "Exactly one of these readings becomes the ",
      shiny::strong("preferred assignment", .noWS = "after"),
      ". It names the peak in the hits table, the spectra and the mass shift ",
      "statistics. The other readings stay listed with the peak. The peak's ",
      "intensity counts once towards the binding, whichever reading is ",
      "preferred."
    ),
    help_section(
      "Rules",
      shiny::p(
        "Each rule compares the readings by its first criterion; readings ",
        "tied on it are compared by the next one. The declared order of the ",
        "mass shifts always comes last, so exactly one reading is preferred."
      ),
      help_defs(
        help_def(
          "Lowest stoichiometry (default)",
          "Fewest bound compound molecules, then closest mass, then declared ",
          "order."
        ),
        help_def(
          "Closest mass",
          "Smallest deviation of the peak from the predicted mass, then lowest ",
          "stoichiometry, then declared order. UniDec's peak matching assigns ",
          "peaks by closest mass as well."
        ),
        help_def(
          "Declared shift order",
          "The mass shift listed first in the Compounds table (Mass 1 before ",
          "Mass 2), then lowest stoichiometry."
        )
      )
    ),
    help_section(
      "Example",
      shiny::p(
        "Protein 20,000 Da; compound mass shifts Mass 1 = 133 Da and Mass 2 = ",
        "267 Da; peak at 20,266.2 Da, fitting 133 Da × 2 (Δ 0.2 Da) and ",
        "267 Da × 1 (Δ 0.8 Da):"
      ),
      help_defs(
        help_def("Lowest stoichiometry", shiny::strong("267 Da × 1")),
        help_def("Closest mass", shiny::strong("133 Da × 2")),
        help_def("Declared shift order", shiny::strong("133 Da × 2 (Mass 1)"))
      ),
      shiny::p(
        "A peak at 20,266.5 Da is 0.5 Da off both readings. Closest mass then ",
        "falls back to the lowest stoichiometry and prefers 267 Da × 1."
      )
    ),
    help_note(
      "Ambiguous peaks are listed in the conversion log and counted under ",
      "Warnings in the Protocol tab."
    )
  )
}

#' @export
conversion_sidebar_help <- list(
  peak_tol_tooltip_bttn = peak_tolerance_page,
  max_mult_tooltip_bttn = max_stoichiometry_page,
  hit_pref_tooltip_bttn = hit_preference_page
)

# Conversion declaration ----

samples_declaration_page <- function() {
  help_modal(
    "Samples Declaration",
    shiny::p(
      "Continue from a deconvolution or upload a deconvolution result ",
      "(.db). Then tell KiwiMS what each sample contains: its protein, its ",
      "compound(s) and, for a kinetics analysis, the compound concentration ",
      "and incubation time."
    ),
    help_section(
      "Columns",
      help_defs(
        help_def("Sample", "Sample name from the deconvolution (read-only)."),
        help_def("Protein", "A protein of the Proteins table."),
        help_def(
          "Compound 1 – 5",
          "Compounds of the Compounds table present in the sample. Leave them ",
          "empty for a control without compound."
        ),
        help_def(
          "Concentration / Time",
          "Compound concentration and incubation time, only with Run Kinetics ",
          "Analysis. Numbers only; the units are chosen above the table. A ",
          "sample at concentration 0 is an untreated control: it is drawn as ",
          "the baseline of the Binding Curve and not fitted."
        ),
        help_def(
          "Replicate",
          "The replicate series of the sample (read-only, see below)."
        )
      ),
      shiny::p(
        "Fill the table by typing, by pasting from a spreadsheet, or from an ",
        "experiment configuration (Configuration in the sidebar), which maps ",
        "the samples to their protein, compounds, concentration, time, well ",
        "and replicate."
      )
    ),
    help_section(
      "Replicates",
      shiny::p(
        shiny::strong("Replicate"),
        " names the replicate series a sample belongs to: one complete repeat ",
        "of the experiment, e.g. R1 for all samples of the first repeat and R2 ",
        "for those of the second. It comes from the Replicate column of the ",
        "experiment configuration or, without one, from an _R<n> ending of the ",
        "file name (sample_10uM_5min_R2.raw → R2). Without either it stays ",
        "empty."
      ),
      shiny::p(
        "Replicates of a condition are the samples with the same protein, ",
        "compound(s), concentration and time, one per series. Every sample ",
        "enters the fits on its own; the series only decide the per-series ",
        "fits (",
        k_inact(),
        "/",
        K_i(),
        " by Replicate Series) and the marker fills in the Fit plots."
      ),
      shiny::p(
        shiny::strong("What counts as a replicate: "),
        "a separate incubation, i.e. its own reaction mixed, incubated and ",
        "stopped on its own, then measured. Injecting the same incubation ",
        "twice is ",
        shiny::strong("not"),
        " a replicate: it repeats the measurement, not the experiment, and the ",
        "two readings agree more closely than two real incubations would. ",
        "KiwiMS counts every sample as an independent incubation, so ",
        "re-injections declared as replicates make the standard errors and ",
        "confidence intervals of ",
        k_obs(),
        " and ",
        k_inact(),
        "/",
        K_i(),
        " too narrow."
      )
    ),
    help_section(
      "Declaring your samples",
      help_defs(
        help_def(
          "Each sample its own incubation, experiment repeated",
          "Declare all samples. Give the samples of each repeat their own ",
          "series (R1, R2, …), the same for all samples of that repeat (same ",
          "day, plate or stock dilution)."
        ),
        help_def(
          "Each sample its own incubation, no repeat",
          "Declare all samples and leave Replicate empty (or all R1). The fits ",
          "work as usual; there are no per-series fits."
        ),
        help_def(
          "Aliquots taken from one reaction over time",
          "The usual time-course design. Each aliquot is a sample at its time ",
          "point; all aliquots of one reaction belong to the same series."
        ),
        help_def(
          "The same incubation injected more than once",
          "Include one injection per incubation only, by leaving the others ",
          "out of the deconvolution or the configuration. Declared as ",
          "replicates or as a second series, they make the confidence ",
          "intervals too narrow and the series agree by construction."
        )
      )
    ),
    help_note(
      shiny::strong("Limits: "),
      "at most ",
      run_limits$max_samples,
      " samples per run, ",
      run_limits$max_series,
      " replicate series and ",
      run_limits$max_replicates,
      " replicates per condition (untreated controls at concentration 0 ",
      "excepted). A kinetics analysis needs at least three non-zero ",
      "concentrations, each with at least three samples at two or more ",
      "non-zero time points."
    )
  )
}

proteins_declaration_page <- function() {
  help_modal(
    "Protein Declaration",
    shiny::p(
      "The proteins screened for: one row per protein with its name in the ",
      "first column and its mass in Da in the next. Fill the table by typing, ",
      "by pasting from a spreadsheet, or by uploading a table (.csv, .tsv, ",
      ".txt, .xlsx, .xls); headers are optional."
    ),
    shiny::p(
      "A protein can carry up to nine masses (Mass 1 – Mass 9), one per ",
      "proteoform, for example the unmodified protein next to a form with a ",
      "known modification. Each mass is screened as a species of its own: it ",
      "has its own unbound peak and binds compounds itself, and all of them ",
      "together make up the protein's binding."
    ),
    help_note(
      "Declare average masses, as the deconvolution of intact proteins ",
      "reports them. Masses must be positive and names unique. Masses closer ",
      "to each other than twice the Peak Tolerance are highlighted: a peak ",
      "between them cannot be told apart."
    ),
    shiny::div(
      class = "help-figure",
      shiny::tags$img(
        src = "static/protein_table.png",
        alt = "Example protein table"
      )
    )
  )
}

compounds_declaration_page <- function() {
  help_modal(
    "Compound Declaration",
    shiny::p(
      "The compounds screened for: one row per compound with its name in the ",
      "first column and up to nine mass shifts in Da (Mass 1 – Mass 9). Fill ",
      "the table by typing, by pasting from a spreadsheet, or by uploading a ",
      "table (.csv, .tsv, .txt, .xlsx, .xls); headers are optional."
    ),
    shiny::p(
      "A mass shift is the mass the protein gains when one molecule binds: ",
      "the compound's mass for an addition, or that mass minus the leaving ",
      "group when binding releases one. Declare several shifts when the ",
      "adduct can form in different ways, for example with and without loss ",
      "of a leaving group."
    ),
    help_note(
      "Shifts must be positive. Do not declare multiples of a shift (266 Da ",
      "next to 133 Da): the stoichiometry search already covers them, and ",
      "the table refuses them. Shifts closer to each other than twice the ",
      "Peak Tolerance are highlighted; the Preferred Assignment rule decides ",
      "between them."
    ),
    shiny::div(
      class = "help-figure",
      shiny::tags$img(
        src = "static/compound_table.png",
        alt = "Example compound table"
      )
    )
  )
}

# Relative binding ----

relative_binding_page <- function() {
  help_modal(
    "Relative Binding",
    shiny::p(
      "The binding of every sample, computed from the peak intensities of ",
      "its deconvoluted spectrum:"
    ),
    binding_formula(),
    shiny::p(
      "Σ",
      mvar("I", "complex"),
      " sums the intensities of all peaks assigned to a complex, Σ",
      mvar("I", "unbound"),
      " those of the unbound peaks of the protein, all its declared masses ",
      "together. The ratio reads as the fraction of modified protein on the ",
      "assumption that the protein and its complexes ionise alike."
    ),
    help_section(
      "Tabs",
      help_defs(
        help_def(
          "Overview",
          "One protein with any of the compounds declared with it, over all ",
          "its samples."
        ),
        help_def("Sample View", "One sample in detail.")
      )
    ),
    help_section(
      "Display",
      help_defs(
        help_def(
          "Short Sample IDs",
          "Shortens long sample names in plots and tables."
        ),
        help_def(
          "Color Variable",
          "Colours plots and tables by compound or by sample."
        ),
        help_def(
          "Colours",
          paste0(
            "Fixed for the whole result: a compound, sample or concentration ",
            "has the same colour in every plot, table and export. Compounds ",
            "set aside in the Overview are grey."
          )
        )
      )
    )
  )
}

sample_protein_page <- function() {
  help_modal(
    "Protein",
    shiny::p("The protein of the selected sample."),
    help_defs(
      help_def("Name", "Protein name from the Samples table."),
      help_def("Mw", "Declared mass(es) of the protein."),
      help_def(
        "Signal",
        "Measured mass of the unbound peak of each declared mass; “No signal” ",
        "when none was found within the Peak Tolerance, because the species ",
        "is absent or fully converted."
      ),
      help_def(
        "Binding",
        "Binding of the sample. With several declared masses, one value per ",
        "proteoform: its complexes over its own unbound plus complex signal."
      )
    ),
    help_note(
      "A value in orange is pinned to 0 or 100 % because the complex or the ",
      "unbound peak of that proteoform was not detected; hover over it for ",
      "details. Beyond two masses, the rest are listed on hover over the ",
      "ellipsis."
    )
  )
}

quality_page <- function() {
  help_modal(
    "Quality",
    shiny::p(
      "How much of the deconvoluted spectrum the hit search explains, ",
      "weighted by peak intensity:"
    ),
    correct_formula(),
    help_defs(
      help_def(
        "Correct [%]",
        "Share of the total peak intensity in peaks assigned to a declared ",
        "protein species (unbound) or one of its complexes."
      ),
      help_def(
        "Unmatched [%]",
        "The rest (100 − Correct), carried by peaks no assignment explains."
      ),
      help_def(
        "Peaks",
        "Number of detected peaks and how many of them were assigned."
      )
    ),
    shiny::p(
      "Values turn orange below 50 % Correct (above 50 % Unmatched) and red ",
      "below 10 % (above 90 %)."
    ),
    help_note(
      "A high Unmatched [%] points at undeclared species, such as a further ",
      "proteoform or adduct, at a Peak Tolerance that is too tight, or at ",
      "noise picked as peaks. Show Unmatched in the Annotated Spectrum marks ",
      "these peaks."
    )
  )
}

table_view_page <- function() {
  help_modal(
    "Table View",
    shiny::p(
      "The hits behind the plots: in the Sample View those of the selected ",
      "sample, in the Overview those of the selected protein and compounds ",
      "over all samples. One row per assignment of a peak."
    ),
    help_section("Columns", hit_columns()),
    intensity_note(),
    shiny::p(
      "Settings → Binding [%] Bar and Tot. Binding [%] Bar draw these values ",
      "as bars in their cells."
    )
  )
}

compound_distribution_page <- function() {
  help_modal(
    "Compound Distribution",
    help_section(
      "Sample View",
      shiny::p(
        "How the protein signal of the sample divides between the unbound ",
        "protein (dark slice) and each complex peak, labelled with its mass ",
        "shift × stoichiometry. The slices add up to 100 %; the complex ",
        "slices together are the sample's Tot. Binding [%]. With several ",
        "declared protein masses, each proteoform has slices of its own."
      )
    ),
    help_section(
      "Overview",
      shiny::p(
        "The binding of the selected compounds on the selected protein, one ",
        "bar per sample, stacked from its complexes (mass shift × ",
        "stoichiometry) with the sample's total written above. With one ",
        "compound the bars stand side by side; with several they are grouped ",
        "by compound. Samples run from least to most bound; a sample declared ",
        "with the compound but without a hit of it stays at 0 %."
      ),
      help_defs(
        help_def(
          "Range",
          "Maximum scales the axis to the highest binding, 100 to the full ",
          "0 – 100 %."
        ),
        help_def("Show Labels", "Writes the values onto the bars.")
      )
    ),
    shiny::p(
      "A peak read in several ways (see Preferred Assignment) is one slice or ",
      "segment, named after all its readings."
    )
  )
}

annotated_spectrum_page <- function() {
  help_modal(
    "Annotated Spectrum",
    shiny::p(
      "The deconvoluted spectrum with the assigned peaks marked: diamonds for ",
      "unbound protein species, circles for complexes, coloured like the ",
      "other plots."
    ),
    help_section(
      "Sample View",
      help_defs(
        help_def(
          "Show Distance",
          "Connects the unbound peak with each complex peak of the same ",
          "species and gives their mass difference, to compare with the ",
          "declared mass shift."
        ),
        help_def("Annotate Mass", "Labels the peaks with their mass.")
      )
    ),
    help_section(
      "Overview",
      shiny::p("The spectra of all samples of the selection together."),
      help_defs(
        help_def(
          "Cubic / Planar",
          "Spectra staggered one behind the other in 3D, or overlaid in one ",
          "plane."
        ),
        help_def(
          "Sort by Binding",
          "Orders the spectra by binding, like the Compound Distribution and ",
          "the Table View. Off, they are grouped by concentration."
        ),
        help_def(
          "Show Labels / Symbols / Legend",
          "Sample labels, peak symbols and legend; they start off for large ",
          "selections, where they get crowded."
        )
      )
    ),
    shiny::p(
      shiny::strong("Show Unmatched"),
      " marks detected peaks no assignment explains with a grey ×; they make ",
      "up the Unmatched [%] of a sample."
    )
  )
}

mass_shifts_page <- function() {
  help_modal(
    "Mass Shifts",
    shiny::p(
      "The Overview shows one protein with the compounds it was declared ",
      "with. Pick the protein above the left card and any of its compounds ",
      "above the right one; the plots and the table below show the picked ",
      "complexes only."
    ),
    help_defs(
      help_def(
        "Protein Mass Shifts",
        "The declared masses of the protein (its proteoforms), over all of its ",
        "samples."
      ),
      help_def(
        "Compound Mass Shifts",
        "The declared mass shifts of the picked compounds, counted on the ",
        "picked protein only. With several compounds picked, each mass names ",
        "its compound."
      ),
      help_def(
        "×N",
        "The number of samples a mass was found in. A compound mass counts for ",
        "the peaks it is the preferred assignment of. Greyed-out masses were ",
        "declared but not assigned to any peak; masses beyond the first three ",
        "are listed on hover."
      )
    ),
    help_note(
      "In the pickers, proteins whose complexes have no hit and compounds ",
      "without a hit on the picked protein are marked “No hits”; proteins ",
      "declared without a compound are marked “No compounds”."
    )
  )
}

# Kinetics ----

binding_curve_page <- function() {
  help_modal(
    "Binding Curve",
    shiny::p(
      "Binding over incubation time, one curve per concentration. Each ",
      "concentration is fitted on its own, all of its samples as individual ",
      "points, to a single exponential:"
    ),
    exponential_formula(),
    shiny::p(
      mvar("P"),
      " is the plateau, the binding approached at long incubation (bounded ",
      "to 0 – 100 %), and ",
      k_obs(),
      " the observed rate constant. The curve starts at 0 % at ",
      math(mvar("t"), " = 0"),
      " by construction."
    ),
    help_defs(
      help_def(
        "Untreated control",
        "Concentration 0 is drawn as a flat baseline and not fitted. A control ",
        "that does not stay at 0 % points at background adducts or carry-over."
      ),
      help_def(
        "No response",
        "A concentration without binding at any time point gets ",
        k_obs(),
        " = 0."
      ),
      help_def(
        "Not fitted",
        "With fewer than two time points, a flat response or a fit that does ",
        "not converge, a concentration is drawn as points without a curve."
      )
    ),
    shiny::p(
      shiny::strong("Settings → Data Points"),
      " shows either the mean ± SD per time point or the individual samples ",
      "the curves were fitted to. The export uses the same choice."
    )
  )
}

binding_curve_single_page <- function() {
  help_modal(
    "Binding Curve (Single Concentration)",
    shiny::p(
      "Binding over incubation time at the selected concentration, fitted to ",
      "a single exponential with all samples as individual points:"
    ),
    exponential_formula(),
    shiny::p(
      "The fit gives ",
      k_obs(),
      ", the plateau ",
      mvar("P"),
      " (bounded to 0 – 100 %) and the initial velocity ",
      mvar("v"),
      " shown in the cards beside. The curve starts at 0 % at ",
      math(mvar("t"), " = 0"),
      " by construction."
    ),
    shiny::p(
      shiny::strong("Settings → Data Points"),
      " shows either the mean ± SD per time point or the individual samples."
    )
  )
}

kobs_curve_page <- function() {
  help_modal(
    shiny::span(k_obs(), " Curve"),
    shiny::p(
      "Observed rate constants against compound concentration, described by ",
      "the two-step model of covalent inhibition: the compound first binds ",
      "reversibly, then reacts to the covalent complex."
    ),
    scheme_formula(),
    shiny::p(
      k_obs(),
      " rises with ",
      conc_c(),
      " and levels off at ",
      k_inact(),
      " once the protein is saturated:"
    ),
    hyperbolic_formula(),
    help_defs(
      help_def(
        "Points",
        k_obs(),
        " ± SE of each concentration fitted on its own (Binding Curve)."
      ),
      help_def(
        "Line",
        k_obs(),
        "(",
        conc_c(),
        ") of the global fit, which fits all samples of all included ",
        "concentrations at once rather than these points. It is hyperbolic ",
        "when ",
        k_obs(),
        " levels off significantly, otherwise straight with the slope ",
        k_inact(),
        "/",
        K_i(),
        "."
      ),
      help_def(
        "Show Extrapolation",
        "(Settings) Shades the measured concentration range and continues the ",
        "fitted line beyond it, dashed, for the proteoforms too when they are ",
        "shown."
      )
    )
  )
}

binding_analysis_page <- function() {
  help_modal(
    "Binding Analysis",
    shiny::p("The fit of each concentration on its own (Binding Curve)."),
    help_defs(
      help_def("Conc.", "Compound concentration."),
      help_def(k_obs(), "Observed rate constant."),
      help_def(
        "SE",
        "Standard error of ",
        k_obs(),
        " from that fit; N/A when it cannot be estimated, typically with too ",
        "few usable time points."
      ),
      help_def(
        "Velocity",
        mvar("v"),
        ", the initial rate of binding as a fraction of the protein per time ",
        "unit (see the Velocity help of the Concentrations tab)."
      ),
      help_def("Plateau [%]", "The fitted plateau ", mvar("P"), "."),
      help_def(
        "Included",
        "Whether the concentration enters the global fit. Unchecking it ",
        "refits ",
        k_inact(),
        ", ",
        K_i(),
        " and ",
        k_inact(),
        "/",
        K_i(),
        " without it."
      )
    ),
    help_note(
      "A concentration without any binding (",
      k_obs(),
      " = 0) carries no information on the rate and stays out of the global ",
      "fit regardless. Exclude other concentrations only for an experimental ",
      "reason, such as a pipetting error or precipitated compound, not to ",
      "improve the fit."
    )
  )
}

kinact_page <- function() {
  help_modal(
    k_inact(),
    shiny::p(
      k_inact(),
      " is the first-order rate constant of covalent bond formation within ",
      "the reversible complex (E·C ⟶ E–C): the highest ",
      k_obs(),
      ", reached when the protein is saturated with compound. It reflects ",
      "how reactive the warhead is once the compound is in place, independent ",
      "of how tightly the compound binds."
    ),
    global_error_note(k_inact()),
    help_section(
      "When it shows n.d. (not determinable)",
      shiny::p(
        k_inact(),
        " and ",
        K_i(),
        " can only be told apart when ",
        k_obs(),
        " visibly levels off within the measured concentrations. KiwiMS tests ",
        "this in the global fit: the curved (hyperbolic) model must describe ",
        "the data significantly better than a straight line (F-test, p < ",
        kinetics_settings$curvature_alpha,
        "), and ",
        K_i(),
        " must be determined to within ±",
        kinetics_settings$ki_max_rel_se * 100,
        " % and lie no higher than ",
        kinetics_settings$ki_max_over_conc,
        " times the highest concentration. Otherwise only ",
        k_inact(),
        "/",
        K_i(),
        " is reported; higher concentrations are needed to separate them."
      )
    ),
    replicate_note()
  )
}

ki_page <- function() {
  help_modal(
    K_i(),
    shiny::p(
      K_i(),
      " is the compound concentration at which ",
      k_obs(),
      " reaches half of ",
      k_inact(),
      ". It describes the affinity of the reversible complex: the lower ",
      K_i(),
      ", the lower the concentration at which the compound occupies the ",
      "protein. In terms of the rate constants of the two-step model,"
    ),
    help_formula(
      K_i(),
      op("="),
      frac(
        shiny::tagList(mvar("k", "off"), op("+"), k_inact()),
        mvar("k", "on")
      )
    ),
    shiny::p(
      "so it equals the dissociation constant of the reversible complex only ",
      "when the covalent step is much slower than dissociation (",
      k_inact(),
      " ≪ ",
      mvar("k", "off"),
      "); otherwise it is larger."
    ),
    global_error_note(K_i()),
    shiny::p(
      K_i(),
      " is only reported when the measured concentrations reach it. If all ",
      "of them lie far below ",
      K_i(),
      ", ",
      k_obs(),
      " rises linearly, and any ",
      K_i(),
      " above the measured range fits equally well; the card then shows n.d. ",
      "and the reason."
    ),
    replicate_note()
  )
}

ratio_page <- function() {
  help_modal(
    ratio_title(),
    shiny::p(
      "The second-order rate constant of covalent inactivation, the key ",
      "potency measure of a covalent inhibitor:"
    ),
    help_formula(frac(k_inact(), K_i())),
    shiny::p(
      "Well below ",
      K_i(),
      ", the model reduces to"
    ),
    linear_formula(),
    shiny::p(
      "so the ratio is the rate of labelling per unit of compound ",
      "concentration. Higher values mean more efficient labelling at low ",
      "concentration. Its unit is concentration⁻¹ · time⁻¹ in the units of ",
      "the Unit View; M⁻¹ s⁻¹ is customary for reporting."
    ),
    help_section(
      "How it is determined",
      shiny::p(
        "All samples of all included concentrations are fitted at once ",
        "(global fit), each sample as its own point. Concentrations whose ",
        "curve gets close to its plateau have a plateau of their own; the ",
        "others share one (see Plateau per Concentration in the Fit tab). ",
        k_inact(),
        "/",
        K_i(),
        " is a parameter of that fit, so it has a standard error even when ",
        k_inact(),
        " and ",
        K_i(),
        " cannot be separated."
      ),
      global_error_note(shiny::span(k_inact(), "/", K_i())),
      shiny::p(
        "The per-series error bars in the Fit tab are standard errors of the ",
        "same kind."
      )
    ),
    replicate_note(),
    help_defs(
      help_def(
        "Model",
        "Hyperbolic when ",
        k_obs(),
        " levels off significantly, otherwise linear (slope = ",
        k_inact(),
        "/",
        K_i(),
        ")."
      ),
      help_def(
        "Series",
        "The same fit on each replicate series alone, showing how much a ",
        "repeat of the experiment varies."
      ),
      help_def(
        "Warnings",
        "Hover for details. They flag results that need care, for example ",
        "missing saturation, plateaus that disagree, or early samples reading ",
        "0 % because small complex peaks fell below the deconvolution peak ",
        "threshold."
      )
    )
  )
}

diag_residuals_page <- function() {
  help_modal(
    "Global Fit Residuals",
    shiny::p(
      "Each point is one sample: measured binding minus the value of the ",
      "global fit at its concentration and time, plotted against time. ",
      "Colour and shape give the concentration; the fill gives the replicate ",
      "series (R1 filled, R2 open, R3 dotted, R4 open with dot), and a thin ",
      "line joins the points of one series over time."
    ),
    shiny::p(
      "Points scattering evenly around zero, mostly within the dotted ±2 SD ",
      "lines, mean the model describes the time courses. A run of points on ",
      "one side, for example early samples all below zero or one ",
      "concentration drifting away, shows something the model does not ",
      "capture, such as small complex peaks read as 0 %, a lag phase or an ",
      "unstable compound."
    )
  )
}

diag_plateaus_page <- function() {
  help_modal(
    "Plateau per Concentration",
    shiny::p(
      "All values in % binding. The symbol (same colour and shape as in the ",
      "other plots) is the mean binding observed at the concentration's last ",
      "time point. The short horizontal tick is the plateau of that ",
      "concentration fitted on its own (the Plateau column of the Binding ",
      "Analysis table). The wide grey bar is the plateau used in the global ",
      "fit; a bar spanning several concentrations means they share one ",
      "plateau."
    ),
    shiny::p(
      "The dotted stem from the symbol up to the tick is the rise the fit ",
      "expects after the last measurement. A long stem means the curve was ",
      "still rising when measurement stopped, so that plateau is an ",
      "extrapolation. The hover on a symbol also gives the model's estimate of ",
      "how far the curve got towards its plateau."
    ),
    shiny::p(
      "A plateau can only be measured when the curve gets close to it. ",
      "Concentrations reaching at least ",
      kinetics_settings$plateau_min_reached * 100,
      " % of it get a plateau of their own in the global fit; the others ",
      "share one. Well-reached plateaus should be similar, as the kinetic ",
      "model assumes one maximum occupancy. Plateaus differing by more than ",
      kinetics_settings$plateau_max_spread,
      " percentage points raise a warning; they can point at compound ",
      "depletion or instability."
    )
  )
}

diag_series_page <- function() {
  help_modal(
    shiny::span(k_inact(), "/", K_i(), " by Replicate Series"),
    shiny::p(
      "The global fit repeated on each replicate series alone (e.g. all R1 ",
      "samples, all R2 samples), with its standard error, next to the result ",
      "of all samples and its 95 % bootstrap confidence interval."
    ),
    shiny::p(
      "Series that agree within their error bars mean a repeat of the whole ",
      "experiment gives the same answer. A clear gap between series shows ",
      "variability the per-sample scatter does not capture, such as a ",
      "different stock dilution, plate or day."
    ),
    help_note(
      "This only holds when each series is an independent repeat with its ",
      "own incubations. A second series made of re-injections of the first ",
      "series' samples agrees with it by construction and says nothing about ",
      "reproducibility."
    )
  )
}

diag_saturation_page <- function() {
  help_modal(
    "Saturation Coverage",
    shiny::p(
      "The fraction of the maximal inactivation rate reached at each ",
      "measured concentration,"
    ),
    help_formula(
      frac(k_obs(), k_inact()),
      op("="),
      frac(conc_c(), shiny::tagList(K_i(), op("+"), conc_c()))
    ),
    shiny::p(
      "using the ",
      K_i(),
      " of the curved (hyperbolic) global fit. The shade marks the measured ",
      "range; the dotted lines mark ",
      K_i(),
      ", where half the maximal rate is reached."
    ),
    shiny::p(
      k_inact(),
      " and ",
      K_i(),
      " can only be separated when the measured range reaches well into the ",
      "bend of this curve. When the highest concentration stays at a small ",
      "fraction, the data only determine ",
      k_inact(),
      "/",
      K_i(),
      ". If ",
      K_i(),
      " is not determinable, its value here is an estimate and shown as such."
    )
  )
}

proteoforms_page <- function() {
  help_modal(
    "Proteoforms",
    shiny::p(
      "The kinetics use the ",
      shiny::strong("pooled"),
      " binding: the complexes of all declared masses of the protein over its ",
      "total signal. Here every proteoform is also fitted on its own binding, ",
      "its complexes over its own unbound plus complex signal, over the ",
      "concentrations the pooled fit includes."
    ),
    help_defs(
      help_def(
        "Reference",
        "The proteoform carrying the most signal among those whose binding is ",
        "measured in at least one sample. It is listed first, under the pooled ",
        "fit."
      ),
      help_def(
        "Paired Binding",
        "Each sample's binding of a proteoform against that of the reference. ",
        "Proteoforms that react alike lie on the diagonal."
      ),
      help_def(
        "Δ Binding vs Reference",
        "Mean paired difference in percentage points over the samples where ",
        "both values were measured; negative means the proteoform binds less."
      ),
      help_def(
        "Limit Values",
        "Samples whose value is pinned to 0 or 100 % because the complex or ",
        "the unbound peak of the proteoform was not detected. Paired Binding ",
        "leaves them out; Settings → Show Limit Values draws them as open ",
        "symbols."
      )
    ),
    help_note(
      "A minor proteoform's peaks fall below the deconvolution peak ",
      "threshold much earlier than those of the main species, which distorts ",
      "its own fit. Lowering the peak threshold of the deconvolution reduces ",
      "limit values."
    )
  )
}

kobs_value_page <- function() {
  help_modal(
    k_obs(),
    shiny::p(
      k_obs(),
      " is the observed pseudo-first-order rate constant of complex ",
      "formation at the selected concentration, from the single-exponential ",
      "fit of its binding curve. Its unit is the inverse time unit of the ",
      "Unit View."
    ),
    shiny::p(
      shiny::strong("± on the card"),
      " is the standard error of ",
      k_obs(),
      " in that fit: how precisely the curve determines the rate constant. It ",
      "is not the standard deviation of the replicates; their scatter is ",
      "visible in the binding curve (Settings → Data Points). It reads n.a. ",
      "when the fit cannot estimate it, typically with too few usable time ",
      "points."
    ),
    shiny::p("Under the two-step model,"),
    hyperbolic_formula(),
    shiny::p(
      "so ",
      k_obs(),
      " grows in proportion to ",
      conc_c(),
      " at low concentration and approaches ",
      k_inact(),
      " at high concentration."
    )
  )
}

plateau_page <- function() {
  help_modal(
    "Binding Plateau",
    shiny::p(
      "The plateau ",
      mvar("P"),
      " is the binding the curve approaches at long incubation, fitted ",
      "together with ",
      k_obs(),
      " and bounded to 0 – 100 %."
    ),
    shiny::p(
      "For an irreversible inhibitor in excess, every protein molecule is ",
      "eventually modified, so the plateau should approach 100 % at every ",
      "concentration. A plateau clearly below 100 % points at:"
    ),
    htmltools::tags$ul(
      htmltools::tags$li(
        "compound running out before the protein is fully labelled, because ",
        "it is not in excess or is consumed or degraded in the buffer;"
      ),
      htmltools::tags$li(
        "a fraction of the protein that cannot react, such as misfolded ",
        "protein, an oxidised target residue or a proteoform lacking it;"
      ),
      htmltools::tags$li("reversible covalent binding.")
    ),
    help_note(
      "A curve that has not levelled off within the measured time gives a ",
      "poorly determined plateau, which is an extrapolation. The Plateau per ",
      "Concentration plot in the Fit tab shows how far each curve got."
    )
  )
}

velocity_page <- function() {
  help_modal(
    shiny::span("Velocity ", mvar("v")),
    shiny::p(
      mvar("v"),
      " is the initial rate of binding: the slope of the binding curve at ",
      math(mvar("t"), " = 0"),
      ", as a fraction of the protein per time unit. It follows from the ",
      "plateau and ",
      k_obs(),
      ":"
    ),
    help_formula(
      mvar("v"),
      op("="),
      frac(shiny::tagList(mvar("P"), op("·"), k_obs()), "100")
    ),
    shiny::p(
      "Multiplied by 100 it is the initial slope in % per time unit. When the ",
      "plateau is 100 %, ",
      mvar("v"),
      " equals ",
      k_obs(),
      ". Only ",
      k_obs(),
      " enters the global ",
      k_inact(),
      "/",
      K_i(),
      " analysis."
    )
  )
}

hits_table_page <- function() {
  help_modal(
    "Hits Table",
    shiny::p(
      "The hits of all samples at the selected concentration, one row per ",
      "assignment of a peak. The binding of a sample is"
    ),
    binding_formula(),
    help_section("Columns", hit_columns()),
    intensity_note(),
    shiny::p(
      "Tot. Binding [%] of the samples is what the binding curve and ",
      k_obs(),
      " are fitted to. Settings → Binding [%] Bar and Tot. Binding [%] Bar ",
      "draw these values as bars."
    )
  )
}

hits_page <- function() {
  help_modal(
    "Hits",
    shiny::p(
      "All hits of the run in one table. The binding of a sample is"
    ),
    binding_formula(),
    help_section(
      "Controls",
      help_defs(
        help_def(
          "Display",
          shiny::strong("Adduct View"),
          ": one row per assignment of a peak, with the columns below. ",
          shiny::strong("Sample View"),
          ": one row per sample and protein–compound pair, with a binding ",
          "column per declared mass shift and stoichiometry."
        ),
        help_def(
          "Color Variable",
          paste0(
            "Colour the table by concentration, compound or sample, or not ",
            "at all. Rows take a light tint of the colour the plots use."
          )
        ),
        help_def(
          "Select Samples / Compounds / Columns",
          "Narrow the table; the export follows the selection."
        ),
        help_def("Show % Bar", "Percentage columns drawn as bars in their cells.")
      ),
      shiny::p(
        "Click a sample to open it in the Sample View, a protein or compound ",
        "to open it in the Overview, and a concentration to open its kinetics."
      )
    ),
    help_section("Columns", hit_columns()),
    intensity_note()
  )
}

conc_spectra_page <- function() {
  help_modal(
    "Mass Spectra",
    shiny::p(
      "The deconvoluted spectra of the samples at the selected ",
      "concentration, one per time point. Over the incubation the unbound ",
      "protein peak shrinks and the complex peaks (protein plus ",
      mvar("n"),
      " × the compound mass shift) grow."
    ),
    shiny::p(
      "Diamonds mark unbound protein species and circles complexes. The ",
      "binding read from these peaks is what the binding curve is fitted to."
    ),
    help_defs(
      help_def(
        "Cubic / Planar",
        "(Settings) Spectra staggered one behind the other in 3D, or overlaid ",
        "in one plane."
      )
    )
  )
}

# Summary ----

summary_page <- function() {
  help_modal(
    "Summary",
    shiny::p("An overview of the whole binding analysis run."),
    help_defs(
      help_def(
        "Protocol",
        "The conversion log with its alerts and warnings, the counts of ",
        "samples, hits, proteins and compounds, the overall hit rate and the ",
        "settings of the run."
      ),
      help_def(
        "Statistics",
        "How well the assignments explain the spectra (hit rate) across all ",
        "samples, and how this relates to binding."
      ),
      help_def(
        "Batch Control",
        "Results mapped onto the plate layout. Only shown when the experiment ",
        "configuration names the wells."
      )
    ),
    help_note(
      "Hit rate here means Correct [%] or Unmatched [%] of a sample; see the ",
      "Correct [%] help."
    )
  )
}

protocol_log_page <- function() {
  help_modal(
    "Conversion Log",
    shiny::p(
      "The record of the binding analysis of this run: for every sample the ",
      "peaks found, the hits, the intensities and the binding, then the ",
      "kinetic fits."
    ),
    shiny::p(
      "Orange lines are warnings and red lines alerts; both are counted in ",
      "the cards beside, where hovering lists them. The arrows jump to the top ",
      "or bottom of the log. Export → Clip copies the log to the clipboard, ",
      "Save writes it to a text file."
    )
  )
}

alerts_page <- function() {
  help_modal(
    "Alerts",
    shiny::p(
      "The number of alerts (red lines of the conversion log): steps that ",
      "could not be completed. Examples are a sample whose hit table is ",
      "incomplete, a binding that does not add up to 100 %, or no hits in any ",
      "sample, which skips the kinetics."
    ),
    shiny::p(
      "Alerts point at a problem with the input data; the affected sample or ",
      "step has no result. Hover over the count to see each alert with its ",
      "explanation and how often it occurred."
    )
  )
}

warnings_page <- function() {
  help_modal(
    "Warnings",
    shiny::p(
      "The number of warnings (orange lines of the conversion log): results ",
      "that were computed but need attention. Examples:"
    ),
    htmltools::tags$ul(
      htmltools::tags$li(
        "a sample without any protein or complex peak, whose binding cannot ",
        "be measured;"
      ),
      htmltools::tags$li(
        "a peak that fits several readings, resolved by the Preferred ",
        "Assignment rule, or that could be an unbound species as well as a ",
        "complex;"
      ),
      htmltools::tags$li(
        "samples or concentrations left out of the kinetics, and ",
        "concentrations that could not be fitted;"
      ),
      htmltools::tags$li(
        "warnings of the ",
        k_inact(),
        "/",
        K_i(),
        " fit, such as missing saturation."
      )
    ),
    shiny::p(
      "Hover over the count to see each warning with its explanation and how ",
      "often it occurred."
    )
  )
}

n_samples_page <- function() {
  help_modal(
    "Screened Samples",
    shiny::p(
      "The number of samples in this binding analysis: the samples of the ",
      "Samples table, each with its deconvoluted spectrum."
    )
  )
}

n_hits_page <- function() {
  help_modal(
    "Hits Detected",
    shiny::p(
      "The number of complex assignments over all samples: one per sample, ",
      "protein species, compound and reading of a peak. A sample with two ",
      "complexes counts twice; a peak read in two ways (see Preferred ",
      "Assignment) counts once per reading."
    )
  )
}

n_proteins_page <- function() {
  help_modal(
    "Proteins Detected",
    shiny::p(
      "The number of proteins with results in this run, out of the proteins ",
      "declared in the Proteins table. It turns orange when a declared ",
      "protein has none, for example because no sample was declared with it."
    )
  )
}

n_compounds_page <- function() {
  help_modal(
    "Compounds Detected",
    shiny::p(
      "The number of compounds found as a complex in at least one sample, out ",
      "of the compounds declared in the Compounds table. It turns orange when ",
      "a declared compound was not found in any sample."
    )
  )
}

run_quality_page <- function() {
  help_modal(
    "Correct [%] / Unmatched [%]",
    shiny::p(
      "How much of the deconvoluted spectra the hit search explains, weighted ",
      "by peak intensity. Per sample,"
    ),
    correct_formula(),
    shiny::p(
      "and Unmatched [%] is the rest (100 − Correct). The cards give the mean ",
      "± SD over all samples, one value per sample. Values turn orange below ",
      "50 % Correct (above 50 % Unmatched) and red below 10 % (above 90 %)."
    ),
    shiny::p(
      "In the Statistics tab, Include only hits → Hits only leaves out the ",
      "samples without any assigned peak (Correct 0 %, Unmatched 100 %)."
    ),
    help_note(
      "A high Unmatched [%] points at undeclared species, such as a further ",
      "proteoform or adduct, at a Peak Tolerance that is too tight, or at ",
      "noise picked as peaks."
    )
  )
}

stats_controls <- function() {
  help_defs(
    help_def(
      "Hit Rate Parameter",
      "Shows Correct [%] or Unmatched [%] in all plots of the tab."
    ),
    help_def(
      "Include only hits",
      "Hits only leaves out the samples without any assigned peak (Correct ",
      "0 %, Unmatched 100 %), such as failed acquisitions."
    )
  )
}

stats_histogram_page <- function() {
  help_modal(
    "Hit Rate Distribution",
    shiny::p(
      "How the hit rate spreads over the samples: a histogram with 1 % bins, ",
      "one count per sample. Well-assigned runs pile up at high Correct [%]; a ",
      "separate group at low values points at samples with undeclared species ",
      "or failed acquisitions."
    ),
    stats_controls()
  )
}

stats_boxplot_page <- function() {
  help_modal(
    "Hit Rate Summary Statistics",
    shiny::p(
      "The hit rate of all samples as a box plot: the box spans the middle ",
      "half of the samples (25th to 75th percentile) with the median as its ",
      "line, and the whiskers reach the most extreme samples within 1.5 times ",
      "the box height. Quartiles, median and mean are written beside it."
    ),
    help_defs(
      help_def("Show Points", "(Settings) Draws every sample as a point."),
      help_def(
        "Full Scale (0–100%)",
        "(Settings) Shows the whole percentage range instead of the range of ",
        "the data."
      )
    ),
    stats_controls()
  )
}

stats_scatter_page <- function() {
  help_modal(
    "Binding vs. Hit Rate",
    shiny::p(
      "Every sample as a point: its Tot. Binding [%] against its hit rate, ",
      "coloured by protein, compound or concentration (Settings → Color By). ",
      "Past twelve groups the colours repeat with another marker shape. The ",
      "dashed line ",
      "marks the mean hit rate, the dotted lines ± 1 SD."
    ),
    shiny::p(
      "Low Correct [%] together with low binding can mean that complexes ",
      "hide among the unmatched peaks, for example with an undeclared mass ",
      "shift, so the binding of these samples may be underestimated."
    ),
    help_note(
      "Points are shifted by up to ±0.6 percentage points so that samples ",
      "with equal values stay visible; the hover shows the exact values."
    ),
    stats_controls()
  )
}

stats_violin_page <- function() {
  help_modal(
    "Hit Rate Distribution by Group",
    shiny::p(
      "The hit rate of the samples of each protein or compound (Settings → ",
      "Group By). The width of a violin shows how many samples have a value; ",
      "Inner draws a box plot (Box) or every sample (Points) inside it. Groups ",
      "that stand out point at a protein or compound whose declaration misses ",
      "a species."
    ),
    stats_controls()
  )
}

batch_heatmap_page <- function() {
  help_modal(
    "Batch Control",
    shiny::p(
      "Each card maps one value of every sample onto the plate layout, taken ",
      "from the Well column of the experiment configuration:"
    ),
    help_defs(
      help_def("Total % Binding", "Binding of the sample."),
      help_def(
        "Hit Rate",
        "Correct [%] or Unmatched [%] of the sample (switch in Settings)."
      ),
      help_def(
        "Compound / Protein / Concentration / Time",
        "The declaration of the wells, to check the plate layout."
      )
    ),
    shiny::p(
      "Patterns that follow the plate rather than the experiment, such as low ",
      "hit rates along an edge or in one column, point at a problem with the ",
      "plate or the injection."
    ),
    shiny::p(
      "Compound, protein, concentration and time take the colours they have ",
      "in every other view. For the percentages, Settings → Full Scale ",
      "(0–100%) fixes the colour scale to the whole percentage range."
    )
  )
}

#' @export
conversion_help <- list(
  # Declaration
  resultinput_tooltip_bttn = samples_declaration_page,
  proteins_tooltip_bttn = proteins_declaration_page,
  compounds_tooltip_bttn = compounds_declaration_page,
  # Relative binding
  conversion_tooltip_bttn = relative_binding_page,
  conversion_samples_protein_tooltip_bttn = sample_protein_page,
  samples_quality_tooltip_bttn = quality_page,
  table_view_tooltip_bttn = table_view_page,
  cmp_distribution_tooltip_bttn = compound_distribution_page,
  annotated_spectrum_tooltip_bttn = annotated_spectrum_page,
  overview_mass_shifts_tooltip_bttn = mass_shifts_page,
  # Kinetics
  binding_curve_tooltip_bttn = binding_curve_page,
  kobs_curve_tooltip_bttn = kobs_curve_page,
  binding_analysis_tooltip_bttn = binding_analysis_page,
  kinact_tooltip_bttn = kinact_page,
  Ki_tooltip_bttn = ki_page,
  Kinact_Ki_tooltip_bttn = ratio_page,
  diag_residuals_tooltip_bttn = diag_residuals_page,
  diag_plateaus_tooltip_bttn = diag_plateaus_page,
  diag_series_tooltip_bttn = diag_series_page,
  diag_saturation_tooltip_bttn = diag_saturation_page,
  proteoform_tooltip_bttn = proteoforms_page,
  kobs_value_tooltip_bttn = kobs_value_page,
  binding_plateau_tooltip_bttn = plateau_page,
  v_value_tooltip_bttn = velocity_page,
  hits_table_tooltip_bttn = hits_table_page,
  hits_tooltip_bttn = hits_page,
  binding_curve_single_tooltip_bttn = binding_curve_single_page,
  mass_spectra_tooltip_bttn = conc_spectra_page,
  # Summary
  summary_tooltip_bttn = summary_page,
  protocol_log_help_bttn = protocol_log_page,
  pstat_alerts_help = alerts_page,
  pstat_warnings_help = warnings_page,
  pstat_n_samples_help = n_samples_page,
  pstat_n_hits_help = n_hits_page,
  pstat_n_proteins_help = n_proteins_page,
  pstat_n_compounds_help = n_compounds_page,
  pstat_correct_help = run_quality_page,
  pstat_unmatched_help = run_quality_page,
  pstat_correct_stat_help = run_quality_page,
  pstat_unmatched_stat_help = run_quality_page,
  pstat_peak_tol_help = peak_tolerance_page,
  pstat_max_stoich_help = max_stoichiometry_page,
  stats_histogram_help_bttn = stats_histogram_page,
  stats_boxplot_help_bttn = stats_boxplot_page,
  stats_scatter_help_bttn = stats_scatter_page,
  stats_violin_help_bttn = stats_violin_page,
  batch_heatmap_total_pct_help = batch_heatmap_page,
  batch_heatmap_pct_cmp_help = batch_heatmap_page,
  batch_heatmap_compound_help = batch_heatmap_page,
  batch_heatmap_protein_help = batch_heatmap_page,
  batch_heatmap_concentration_help = batch_heatmap_page,
  batch_heatmap_time_help = batch_heatmap_page
)
