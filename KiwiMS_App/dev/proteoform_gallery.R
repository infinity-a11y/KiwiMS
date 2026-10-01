# dev/proteoform_gallery.R
#
# Visual check of every conversion plot that deals with several proteoforms.
# Builds synthetic deconvolution results, runs them through the app's own hit
# screening, binding and kinetics code, and renders the plots, cards and tables
# of the Samples, Compounds, Proteins, Binding and Proteoforms views into one
# HTML page.
#
# Run from the KiwiMS_App folder:
#   Rscript dev/proteoform_gallery.R [output_dir]
#
# Two datasets:
#   Nine masses - a protein at the declaration cap of 9 masses, from a 45 %
#     main form down to 3 % minor forms, one of them reacting slower, one sitting
#     7 Da from the main complex (outside twice the tolerance, but merged by the
#     peak picking), one exactly on the main form's double complex, plus peaks
#     no declaration explains.
#   Collisions - one sample per ambiguity the declaration check reports, and a
#     fully converted sample without any unbound peak.

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) args[1] else file.path(tempdir(), "proteoform_gallery")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

options(box.path = getwd())
box::use(
  app /
    logic /
    conversion_functions[
      add_hits,
      summarize_hits,
      transform_hits,
      add_proteoform_binding,
      check_filter_hits,
      add_kobs_binding_result,
      add_kinact_ki_result,
      make_unit_view,
      convert_result_list_units,
      convert_kobs_result_units,
      convert_kinact_ki_units,
      convert_kinact_ki_params,
      convert_conc_keys,
      convert_hits_units,
      proteoform_binding,
      proteoform_kinetics,
      proteoform_comparison_table,
      proteoform_kobs_plot,
      proteoform_paired_plot,
      proteoform_colors,
      proteoform_limit_note,
      make_kobs_plot,
      make_binding_plot,
      multiple_spectra,
      smpl_compound_distribution,
      filter_table_view,
      render_table_view,
      get_cmp_colorScale,
      resolve_color_scale,
      label_smart_clean,
      collapse_species,
      species_mw_lines,
      declaration_ambiguities,
      check_sample_table,
      fmt_species_mass,
    ],
  app / logic / deconvolution_functions[spectrum_plot, ],
)

set.seed(42)

# Synthetic deconvolution ----------------------------------------------------

# Spectrum as a sum of Gaussian peaks on a 0.25 Da grid, with a little noise
make_spectrum <- function(centres, heights, range, sigma = 1.5) {
  grid <- seq(range[1], range[2], by = 0.25)
  y <- numeric(length(grid))
  for (k in seq_along(centres)) {
    if (heights[k] <= 0) next
    y <- y + heights[k] * exp(-(grid - centres[k])^2 / (2 * sigma^2))
  }
  y <- y + abs(stats::rnorm(length(grid), 0, 0.002 * max(y)))
  data.frame(mass = grid, intensity = y)
}

# Peak picking the way UniDec does it: local maxima within a window, above a
# threshold relative to the base peak, intensities normalised to their sum.
# 2 % rather than UniDec's 7 % default, which would hide most of the minor
# proteoforms from the start; the 4-5 % forms still reach it late in the run.
peak_threshold <- 0.02
pick_peaks <- function(spectrum, window = 10, threshold = peak_threshold) {
  y <- spectrum$intensity
  n <- length(y)
  half <- round(window / 0.25)
  is_max <- vapply(
    seq_len(n),
    function(i) {
      lo <- max(1, i - half)
      hi <- min(n, i + half)
      y[i] == max(y[lo:hi]) && y[i] >= threshold * max(y)
    },
    logical(1)
  )
  peaks <- spectrum[is_max, , drop = FALSE]
  peaks$intensity <- 100 * peaks$intensity / sum(peaks$intensity)
  rownames(peaks) <- NULL
  peaks
}

as_sample <- function(spectrum, peaks) {
  list(
    config = data.frame(),
    peaks = peaks,
    error = data.frame(),
    rawdata = data.frame(),
    mass = spectrum,
    input = data.frame()
  )
}

# Dataset "Nine masses" --------------------------------------------------------

cmp_mass <- 312.15
main <- 20000
nine <- data.frame(
  label = c(
    "main",
    "oxidation",
    "acetylation",
    "phosphorylation",
    "gluconoylation",
    "phosphogluconoylation",
    "near-miss",
    "Met loss",
    "on double complex"
  ),
  offset = c(0, 15.99, 42.01, 79.97, 178.05, 258.02, 305.00, -131.04, 2 * cmp_mass),
  share = c(0.40, 0.10, 0.12, 0.08, 0.10, 0.06, 0.05, 0.05, 0.04),
  # Relative reactivity: the phosphorylated form reacts markedly slower
  reactivity = c(1, 1, 0.95, 0.6, 0.9, 1, 1, 1.05, 1)
)
nine$mass <- main + nine$offset

kinact <- 0.005 # 1/s
KI <- 50 # µM
concs <- c(2.5, 5, 10, 20, 40, 80)
times <- c(1, 3, 5, 10, 15, 20, 30, 40, 50, 60)
design <- rbind(
  data.frame(conc = 0, time = 0, rep = c("R1", "R2")),
  expand.grid(conc = concs, time = times, rep = c("R1", "R2"), stringsAsFactors = FALSE)
)
design$sample <- sprintf(
  "PX+CMP-1_%s_%smin_%s",
  # Per value: format() on the vector pads every value to one decimal format
  sub(".", "o", as.character(design$conc), fixed = TRUE),
  design$time,
  design$rep
)

nine_sample <- function(conc, time) {
  kobs <- kinact * conc / (KI + conc)
  centres <- numeric(0)
  heights <- numeric(0)
  for (k in seq_len(nrow(nine))) {
    f <- 1 - exp(-nine$reactivity[k] * kobs * time * 60)
    # Double labelling of the main form at high concentration
    double <- if (k == 1) 0.12 * f^2 * conc / 80 else 0
    noise <- exp(stats::rnorm(3, 0, 0.04))
    centres <- c(centres, nine$mass[k], nine$mass[k] + cmp_mass, nine$mass[k] + 2 * cmp_mass)
    heights <- c(
      heights,
      nine$share[k] * (1 - f) * noise[1],
      nine$share[k] * (f - double) * noise[2],
      nine$share[k] * double * noise[3]
    )
  }
  # Peaks no declaration explains: a phosphate adduct of the main form and a
  # sodium adduct of its complex
  centres <- c(centres, main + 97.98, main + cmp_mass + 21.98)
  heights <- c(heights, 0.035, 0.03 * (1 - exp(-kobs * time * 60)))
  spectrum <- make_spectrum(centres, heights, c(19750, 21400))
  as_sample(spectrum, pick_peaks(spectrum))
}

nine_results <- list(
  deconvolution = stats::setNames(
    lapply(seq_len(nrow(design)), function(i) nine_sample(design$conc[i], design$time[i])),
    design$sample
  )
)

nine_proteins <- data.frame(Protein = "PX")
for (k in seq_len(nrow(nine))) {
  nine_proteins[[paste("Mass", k)]] <- round(nine$mass[k], 2)
}
nine_compounds <- data.frame(Compound = "CMP-1", `Mass 1` = cmp_mass, check.names = FALSE)
nine_samples <- data.frame(
  Sample = design$sample,
  Protein = "PX",
  `Compound 1` = "CMP-1",
  `Concentration [μM]` = design$conc,
  `Time [min]` = design$time,
  Replicate = design$rep,
  check.names = FALSE
)

# Dataset "Collisions" ---------------------------------------------------------

# PY 30,000 Da and a proteoform at +125 Da; compound CMP-A adds 125 Da. The
# proteoform's unbound peak is the main form's single complex, and the main
# form's double complex is the proteoform's single complex.
coll_proteins <- data.frame(Protein = "PY", `Mass 1` = 30000, `Mass 2` = 30125, check.names = FALSE)
coll_compounds <- data.frame(
  Compound = c("CMP-A", "CMP-B", "CMP-C"),
  `Mass 1` = c(125, 129, 400),
  check.names = FALSE
)
coll_spec <- list(
  "PY+CMP-A_collision" = list(c(30000, 30125, 30250, 30375), c(0.4, 0.3, 0.2, 0.1)),
  "PY+CMP-C_fully_converted" = list(c(30400, 30525, 30800), c(0.6, 0.3, 0.1)),
  "PY+CMP-C_many_unmatched" = list(
    c(30000, 30125, 30400, 30060, 30210, 30650, 30980),
    c(0.3, 0.1, 0.2, 0.12, 0.1, 0.1, 0.08)
  )
)
coll_results <- list(
  deconvolution = lapply(coll_spec, function(s) {
    spectrum <- make_spectrum(s[[1]], s[[2]], c(29800, 31200))
    as_sample(spectrum, pick_peaks(spectrum))
  })
)
coll_samples <- data.frame(
  Sample = names(coll_spec),
  Protein = "PY",
  `Compound 1` = c("CMP-A", "CMP-C", "CMP-C"),
  check.names = FALSE
)

# Pipeline -----------------------------------------------------------------

fake_session <- list(sendCustomMessage = function(...) invisible(NULL))
tolerance <- 3
max_multiples <- 4

run_screening <- function(results, samples, proteins, compounds) {
  log <- character(0)
  r <- withCallingHandlers(
    add_hits(
      results,
      samples,
      proteins,
      compounds,
      tolerance,
      max_multiples,
      fake_session,
      identity
    ),
    message = function(m) {
      log <<- c(log, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  r$hits_summary <- suppressMessages(summarize_hits(r, samples))
  list(result = r, log = gsub("\033\\[[0-9;]*m", "", log))
}

# Display table as the view builds it
view_hits <- function(hits_summary) {
  hs <- add_proteoform_binding(transform_hits(hits_summary))
  hs$truncSample_ID <- label_smart_clean(hs$`Sample ID`)
  hs
}

nine_run <- run_screening(nine_results, nine_samples, nine_proteins, nine_compounds)
r <- nine_run$result
units_disp <- c(Concentration = "Conc. [μM]", Time = "Time [min]")
units <- c(Concentration = "μM", Time = "min")
filtered <- suppressMessages(check_filter_hits(r))
r$binding_kobs_result <- suppressMessages(add_kobs_binding_result(
  filtered,
  conc_time = c(Concentration = "Concentration [μM]", Time = "Time [min]"),
  units = units
))
r$kinact_ki_result <- suppressMessages(add_kinact_ki_result(r, units))
hs <- view_hits(r$hits_summary)

view <- make_unit_view(units_disp, conc_unit = "M", time_unit = "s")
pooled <- convert_result_list_units(r, view)
kinetics <- proteoform_kinetics(r$hits_summary, units = units, conc_time = units_disp)
kinetics_view <- lapply(kinetics, function(k) {
  k$binding_kobs_result <- convert_kobs_result_units(k$binding_kobs_result, view)
  k$kinact_ki_result <- convert_kinact_ki_units(k$kinact_ki_result, view)
  k
})
entries <- lapply(kinetics_view, function(k) {
  list(kobs = k$binding_kobs_result$kobs_result_table, kinact_ki = k$kinact_ki_result)
})
binding <- proteoform_binding(r$hits_summary)
palette <- proteoform_colors(binding$species)

conc_levels <- unique(hs[[units_disp[["Concentration"]]]])
conc_colors <- get_cmp_colorScale(
  filtered_table = hs,
  scale = resolve_color_scale("Set3", length(conc_levels)),
  variable = "Concentration",
  trunc = TRUE,
  conc_col = units_disp[["Concentration"]]
)
conc_colors <- stats::setNames(conc_colors, unname(convert_conc_keys(names(conc_colors), view)))

coll_run <- run_screening(coll_results, coll_samples, coll_proteins, coll_compounds)
hs_coll <- view_hits(coll_run$result$hits_summary)

# Page pieces --------------------------------------------------------------

sized <- function(widget, height = 440) {
  widget$height <- height
  widget$sizingPolicy$defaultHeight <- height
  widget$sizingPolicy$browser$fill <- FALSE
  widget
}

card <- function(title, ..., note = NULL, wide = FALSE) {
  htmltools::div(
    class = paste("g-card", if (wide) "g-wide"),
    htmltools::div(class = "g-title", title),
    if (!is.null(note)) htmltools::div(class = "g-note", note),
    ...
  )
}

spectrum_colors <- function(tbl) {
  get_cmp_colorScale(filtered_table = tbl, scale = "plasma", variable = "Compounds", trunc = TRUE)
}

sample_spectrum <- function(result, hits, sample, labels = FALSE, unmatched = FALSE, theme = "dark") {
  tbl <- hits[hits$`Sample ID` == sample, ]
  sized(spectrum_plot(
    sample = result$deconvolution[[sample]],
    color_cmp = spectrum_colors(tbl),
    color_variable = "Compounds",
    show_peak_labels = labels,
    show_unmatched = unmatched,
    theme = theme
  ))
}

sample_cards <- function(result, hits, sample) {
  rows <- hits[hits$`Sample ID` == sample, ]
  theor <- suppressWarnings(as.numeric(rows$`Theor. Prot. [Da]`))
  species <- rows[!duplicated(theor), c("Theor. Prot. [Da]", "Meas. Prot. [Da]")]
  mw <- suppressWarnings(as.numeric(species$`Theor. Prot. [Da]`))
  meas <- suppressWarnings(as.numeric(species$`Meas. Prot. [Da]`))
  o <- order(mw)
  mw <- mw[o]
  meas <- meas[o]
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE)
  sb <- proteoform_binding(rows)
  sb <- sb[match(mw, sb$species), ]
  binding_text <- collapse_species(ifelse(
    is.na(sb$binding),
    "N/A",
    ifelse(
      is.na(sb$limit),
      sprintf("%.2f%%", sb$binding),
      sprintf(
        "<span class=\"g-warn\" title=\"%s\">%.2f%%</span>",
        proteoform_limit_note(sb$limit),
        sb$binding
      )
    )
  ))
  signal <- if (all(is.na(meas))) {
    "No signal"
  } else {
    collapse_species(ifelse(is.na(meas), "No signal", paste(fmt(round(meas, 2)), "Da")))
  }
  s <- result$deconvolution[[sample]]
  assigned <- c(s$hits$`Measured Mw Protein [Da]`, s$hits$`Peak [Da]`)
  quality <- sprintf(
    "%.2f%%<br>%.2f%%<br>%d / %d matched",
    as.numeric(rows$`Correct [%]`[1]),
    as.numeric(rows$`Unmatched [%]`[1]),
    sum(s$peaks$mass %in% assigned),
    nrow(s$peaks)
  )
  htmltools::div(
    class = "g-row",
    card(
      "Protein (Samples View)",
      htmltools::div(
        class = "g-kv",
        htmltools::HTML("<div class='g-k'>Name<br>Mw<br>Signal<br>Binding</div>"),
        htmltools::HTML(paste0(
          "<div class='g-v'>",
          rows$Protein[1],
          "<br>",
          collapse_species(paste(fmt(mw), "Da")),
          "<br>",
          signal,
          "<br>",
          binding_text,
          "</div>"
        ))
      )
    ),
    card(
      "Quality",
      htmltools::div(
        class = "g-kv",
        htmltools::HTML("<div class='g-k'>Correct<br>Unmatched<br>Peaks</div>"),
        htmltools::HTML(paste0("<div class='g-v'>", quality, "</div>"))
      )
    )
  )
}

proteins_card <- function(hits, protein) {
  rows <- hits[hits$Protein == protein, ]
  lines <- species_mw_lines(rows$`Theor. Prot. [Da]`, rows$`Meas. Prot. [Da]`)
  card(
    "Protein (Proteins View)",
    htmltools::div(
      class = "g-kv",
      htmltools::HTML(paste0("<div class='g-k'>", lines$labels, "</div>")),
      htmltools::HTML(paste0("<div class='g-v'>", lines$html, "</div>"))
    ),
    note = "Hover the ellipsis for the masses not shown."
  )
}

table_view <- function(hits, sample) {
  tbl <- hits[hits$`Sample ID` == sample & hits$`Cmp Name` != "N/A", ]
  inputs <- list(truncate_names = TRUE, color_variable = "Compounds", binding_bar = TRUE, tot_binding_bar = TRUE)
  colors <- spectrum_colors(tbl)
  units <- if (units_disp[["Concentration"]] %in% names(tbl)) units_disp else NULL
  sized(render_table_view(
    filter_table_view(tbl, colors = colors, inputs = inputs, units = units),
    colors = colors,
    tab = "Samples",
    inputs = inputs,
    units = units
  ), 260)
}

donut <- function(hits, sample, theme = "dark") {
  sized(smpl_compound_distribution(
    hits_summary = hits,
    sample = sample,
    color_variable = "Compounds",
    truncate_names = TRUE,
    color_scale = "plasma",
    theme = theme
  ))
}

many_spectra <- function(samples, cubic, theme = "dark") {
  mapping <- data.frame(original = unique(hs$`Sample ID`), truncated = label_smart_clean(unique(hs$`Sample ID`)))
  sized(multiple_spectra(
    results_list = r,
    samples = samples,
    cubic = cubic,
    color_cmp = spectrum_colors(hs[hs$`Sample ID` %in% samples, ]),
    truncated = mapping,
    color_variable = "Compounds",
    hits_summary = hs,
    unmatched_show = TRUE,
    theme = theme
  ), 520)
}

log_block <- function(lines, pattern) {
  keep <- grepl(pattern, lines) | grepl("also fits|└─ .* \\+ ", lines)
  htmltools::tags$pre(class = "g-log", paste(lines[keep], collapse = "\n"))
}

# Samples to look at
mid_sample <- "PX+CMP-1_10_15min_R1"
late_sample <- "PX+CMP-1_80_60min_R2"
series <- design$sample[design$conc == 10 & design$rep == "R1"]

# Declaration check of the collision dataset: its tables pass with warnings;
# adding CMP-B (4 Da from CMP-A) to the same sample is refused
coll_check <- check_sample_table(
  coll_samples,
  proteins = coll_proteins$Protein,
  compounds = coll_compounds$Compound,
  protein_table = coll_proteins,
  compound_table = coll_compounds,
  tolerance = tolerance,
  max_multiples = max_multiples
)
blocked_samples <- coll_samples[1, ]
blocked_samples$`Compound 2` <- "CMP-B"
blocked_check <- check_sample_table(
  blocked_samples,
  proteins = coll_proteins$Protein,
  compounds = coll_compounds$Compound,
  protein_table = coll_proteins,
  compound_table = coll_compounds,
  tolerance = tolerance,
  max_multiples = max_multiples
)
nine_amb <- declaration_ambiguities(nine_samples, nine_proteins, nine_compounds, tolerance, max_multiples)

comparison <- proteoform_comparison_table(
  kinetics_view,
  pooled = convert_kinact_ki_params(r$kinact_ki_result$Params, view),
  binding = binding,
  view = view
)
comparison_dt <- sized(
  DT::datatable(
    comparison,
    escape = FALSE,
    rownames = FALSE,
    selection = "none",
    options = list(dom = "t", paging = FALSE, ordering = FALSE)
  ) |>
    DT::formatStyle(
      columns = "Proteoform",
      target = "row",
      color = "black",
      backgroundColor = DT::styleEqual("Pooled", "#d4d4d4", default = "#f2f2f2"),
      fontWeight = DT::styleEqual("Pooled", "bold")
    ),
  380
)

pooled_entry <- list(
  Pooled = list(kobs = pooled$binding_kobs_result$kobs_result_table, kinact_ki = pooled$kinact_ki_result)
)

css <- "
body { background: #3a3a3f; color: #eee; font-family: 'Segoe UI', sans-serif; margin: 16px; }
h1 { font-weight: 500; } h2 { font-weight: 400; border-bottom: 1px solid #666; padding-bottom: 4px; margin-top: 40px; }
.g-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(520px, 1fr)); gap: 16px; }
.g-row { display: flex; gap: 16px; flex-wrap: wrap; }
.g-card { background: #2b2b30; border-radius: 6px; padding: 10px 12px; min-width: 300px; }
.g-wide { grid-column: 1 / -1; }
.g-title { background: #cfe0d4; color: #111; margin: -10px -12px 10px; padding: 6px 12px; border-radius: 6px 6px 0 0; }
.g-note { font-size: 0.8em; opacity: 0.7; margin-bottom: 6px; }
.g-kv { display: flex; gap: 3em; justify-content: center; font-size: 0.95em; line-height: 1.6; }
.g-k { font-weight: 200; } .g-v { font-weight: 400; }
.conversion-sample-protein-names { font-weight: 200; }
.g-warn, .protocol-stat-warn { color: darkorange; }
.g-log { background: #1e1e22; padding: 10px; font-size: 0.8em; white-space: pre-wrap; }
.g-msg { padding: 8px 10px; border-radius: 4px; margin: 6px 0; }
.g-msg-ok { background: #fff4d6; color: #5a4200; } .g-msg-err { background: #fde2e1; color: #7a1411; }
.g-light { background: #ffffff; color: #111; }
table.dataTable { background: #fff; color: #111; }
"

page <- htmltools::tagList(
  htmltools::tags$head(htmltools::tags$style(css), htmltools::tags$title("Proteoform Gallery")),
  htmltools::h1("Multiple-proteoform plots"),
  htmltools::p(sprintf(
    "Synthetic data run through the app's conversion code. Nine masses: %s. Compound CMP-1 %.2f Da, peak tolerance %s Da, max. stoichiometry %s, peak picking at %d %% with a 10 Da window.",
    paste(sprintf("%s Da (%s, %d %%)", fmt_species_mass(nine$mass), nine$label, round(100 * nine$share)), collapse = ", "),
    cmp_mass,
    tolerance,
    max_multiples,
    as.integer(round(100 * peak_threshold))
  )),

  htmltools::h2("Samples View"),
  htmltools::p("Mid time course: ", mid_sample),
  sample_cards(r, hs, mid_sample),
  htmltools::div(
    class = "g-grid",
    card("Table View", table_view(hs, mid_sample), wide = TRUE),
    card("Compound Distribution", donut(hs, mid_sample)),
    card("Annotated Spectrum", sample_spectrum(r, hs, mid_sample)),
    card("Annotated Spectrum - Annotate Hits", sample_spectrum(r, hs, mid_sample, labels = TRUE)),
    card("Annotated Spectrum - Show Unmatched", sample_spectrum(r, hs, mid_sample, unmatched = TRUE))
  ),
  htmltools::p("Late, near full conversion (unbound peaks below the threshold): ", late_sample),
  sample_cards(r, hs, late_sample),
  htmltools::div(
    class = "g-grid",
    card("Compound Distribution", donut(hs, late_sample)),
    card("Annotated Spectrum - Annotate Hits + Unmatched", sample_spectrum(r, hs, late_sample, labels = TRUE, unmatched = TRUE))
  ),

  htmltools::h2("Compounds / Proteins View"),
  htmltools::div(class = "g-row", proteins_card(hs, "PX")),
  htmltools::div(
    class = "g-grid",
    card("Annotated Spectrum - planar, Show Unmatched", many_spectra(series, cubic = FALSE)),
    card("Annotated Spectrum - cubic, Show Unmatched", many_spectra(series, cubic = TRUE))
  ),

  htmltools::h2("Binding tab"),
  htmltools::div(
    class = "g-grid",
    card("Binding Curve", sized(make_binding_plot(kobs_result = pooled$binding_kobs_result, colors = conc_colors, units = view$units))),
    card(
      "k_obs Curve - Show Proteoforms",
      sized(make_kobs_plot(pooled$kinact_ki_result, colors = conc_colors, units = view$units, proteoforms = entries, proteoform_palette = palette))
    ),
    card("k_obs Curve - proteoforms off", sized(make_kobs_plot(pooled$kinact_ki_result, colors = conc_colors, units = view$units)))
  ),

  htmltools::h2("Proteoforms tab"),
  htmltools::div(
    class = "g-grid",
    card("k_obs per Proteoform", sized(proteoform_kobs_plot(c(pooled_entry, entries), palette, view$units))),
    card("Paired Binding", sized(proteoform_paired_plot(binding, palette))),
    card("Paired Binding - Show Limit Values", sized(proteoform_paired_plot(binding, palette, show_limits = TRUE))),
    card("Proteoform Kinetics", comparison_dt, wide = TRUE)
  ),

  htmltools::h2("Ambiguous masses"),
  card(
    "Declaration check - Nine masses",
    htmltools::div(
      class = "g-msg g-msg-ok",
      if (nrow(nine_amb)) {
        paste(sprintf("%s ↔ %s (Δ %.2f Da, %s)", nine_amb$first, nine_amb$second, nine_amb$delta, nine_amb$kind), collapse = "; ")
      } else {
        "No ambiguities"
      }
    ),
    wide = TRUE
  ),
  card(
    "Declaration check - Collisions",
    htmltools::div(class = "g-msg g-msg-ok", attr(coll_check, "warning") %||% "passed"),
    htmltools::div(class = "g-note", "Same sample with CMP-B (129 Da, 4 Da from CMP-A) added:"),
    htmltools::div(class = "g-msg g-msg-err", as.character(blocked_check)),
    wide = TRUE
  ),
  card("Conversion log - Collisions", log_block(coll_run$log, "Hit Screening|Hit duplicates|read as unbound"), wide = TRUE),
  htmltools::div(
    class = "g-grid",
    lapply(names(coll_spec), function(s) {
      htmltools::tagList(
        card(paste("Annotated Spectrum -", s), sample_spectrum(coll_run$result, hs_coll, s, labels = TRUE, unmatched = TRUE)),
        if (any(hs_coll$`Sample ID` == s & hs_coll$`Cmp Name` != "N/A")) {
          card(paste("Compound Distribution -", s), donut(hs_coll, s))
        }
      )
    }),
    card("Table View - PY+CMP-A_collision", table_view(hs_coll, "PY+CMP-A_collision"), wide = TRUE)
  ),

  htmltools::h2("Light theme (exports)"),
  htmltools::div(
    class = "g-grid",
    card("k_obs per Proteoform", htmltools::div(class = "g-light", sized(proteoform_kobs_plot(c(pooled_entry, entries), palette, view$units, theme = "light")))),
    card("Paired Binding", htmltools::div(class = "g-light", sized(proteoform_paired_plot(binding, palette, theme = "light")))),
    card("Annotated Spectrum", htmltools::div(class = "g-light", sample_spectrum(r, hs, mid_sample, labels = TRUE, unmatched = TRUE, theme = "light"))),
    card("Annotated Spectrum - planar", htmltools::div(class = "g-light", many_spectra(series, cubic = FALSE, theme = "light")))
  )
)

out_file <- file.path(out_dir, "proteoform_gallery.html")
htmltools::save_html(page, out_file, libdir = "lib")
message("Written: ", normalizePath(out_file))
