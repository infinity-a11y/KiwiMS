# Shared code of the edge-case kit: the series the cases run on, the builders
# of the test files, the readers that load them the way the app does, and the
# declaration and conversion runs the expected values are computed with.
#
# Sourced by every <category>/generate.R and by
# tests/testthat/setup-edge-cases.R. Expects options(box.path) to point at
# the application root (KiwiMS_App).

box::use(
  app / logic / conversion_functions[
    add_hits,
    check_sample_table,
    check_table,
    complex_kinetics,
    is_complex_row,
    main_proteoform,
    process_uploaded_table,
    proteoform_binding,
    proteoform_kinetics,
    read_uploaded_file,
    run_complexes,
    select_complex_kinetics,
    summarize_hits
  ],
  app / logic / helper_functions[
    normalize_colnames,
    normalize_config_units,
    read_config_file,
    validate_config
  ],
)

source(file.path(
  getOption("box.path")[1],
  "tests",
  "reference_data",
  "reference_data.R"
))

# kit_root(): The tests/edge_cases directory ----
kit_root <- function() {
  file.path(getOption("box.path")[1], "tests", "edge_cases")
}

# kit_files_dir(): Where the test files are read from and written to ----
# The category folders by default; the automated tests write a fresh set to a
# temporary directory instead (option kiwims.kit_dir).
kit_files_dir <- function() {
  getOption("kiwims.kit_dir", kit_root())
}

# The categories of the kit, in the order they build on each other
kit_categories <- c("baseline", "mass_ambiguity", "complexes", "replicates", "run_limits")

# The sample the manual tests read the hits of: unbound 21,638 Da, second form
# 21,816 Da, complex 21,903.5 Da at 17.43 % intensity, 12.04 % binding
kit_reference_sample <- "2026-09-18_MULI+BI-8925_2o5_3min_R1"

# kit_context(): The series and the design read off its sample names ----
# The peak list is the committed deconvolution of the reference dataset
# kinact_MLKL_3 (tests/reference_data), the only part of a result the
# conversion reads. The names encode the design:
# <date>_MULI+BI-8925_<conc>_<time>min_R<n>, with "2o5" for 2.5 µM.
kit_context <- function(peaks = ref_peaks("kinact_MLKL_3")) {
  samples <- unique(peaks$sample)
  result <- list(deconvolution = lapply(
    stats::setNames(samples, samples),
    function(s) {
      p <- peaks[peaks$sample == s, c("mass", "intensity")]
      rownames(p) <- NULL
      list(peaks = p)
    }
  ))
  m <- regmatches(samples, regexec("_([0-9o]+)_([0-9]+)min_(R[12])$", samples))
  list(
    result = result,
    samples = samples,
    conc = as.numeric(sub("o", ".", vapply(m, `[`, "", 2))),
    time = as.numeric(vapply(m, `[`, "", 3)),
    rep = vapply(m, `[`, "", 4)
  )
}

# ---- Builders of the test files ---------------------------------------------

# kit_proteins(): Protein table of MLKL with the given masses ----
kit_proteins <- function(...) {
  masses <- c(...)
  d <- data.frame(Protein = "MLKL")
  for (i in seq_along(masses)) {
    d[[paste("Mass", i)]] <- masses[i]
  }
  d
}

# kit_compounds(): Compound table, one row per name, masses per row ----
# kit_compounds(c("A", "B"), 266, 268) gives A at 266 and B at 268;
# kit_compounds("A", c(266, 264)) gives A with two mass shifts.
kit_compounds <- function(names, ...) {
  rows <- list(...)
  d <- data.frame(Compound = names)
  for (i in seq_len(max(lengths(rows)))) {
    d[[paste("Mass", i)]] <- vapply(
      rows,
      function(r) if (i <= length(r)) r[i] else NA_real_,
      numeric(1)
    )
  }
  d
}

# kit_config(): Experiment config of the fixture's samples ----
# c1/c2 are Compound_1/Compound_2 (recycled), time and replicate default to
# the values encoded in the sample names.
kit_config <- function(
  ctx,
  c1,
  c2 = NULL,
  time = ctx$time,
  replicate = ctx$rep
) {
  d <- data.frame(
    Sample = ctx$samples,
    Replicate = replicate,
    Protein = "MLKL",
    Compound_1 = c1
  )
  if (!is.null(c2)) {
    d$Compound_2 <- c2
  }
  d$Compound_Concentration <- ctx$conc
  d$Concentration_Unit <- "µM"
  d$Incubation_Time <- time
  d$Time_Unit <- "min"
  d
}

# kit_config_synthetic(): Config of n made-up samples (no fixture needed) ----
# Every sample is a condition of its own (four concentrations, time 1 .. n
# min), so the sample cap is the only rule a large n can break.
kit_config_synthetic <- function(n) {
  data.frame(
    Sample = sprintf("Sample_%03d.raw", seq_len(n)),
    Replicate = "",
    Protein = "MLKL",
    Compound_1 = "BI-8925",
    Compound_Concentration = rep_len(c(2.5, 5, 10, 20), n),
    Concentration_Unit = "µM",
    Incubation_Time = seq_len(n),
    Time_Unit = "min"
  )
}

# kit_write(): Write one test file as the kit stores it ----
# Configs are written in UTF-8 (they carry the µ of the unit); protein and
# compound tables are plain ASCII.
kit_write <- function(df, path) {
  if (startsWith(basename(path), "config_")) {
    utils::write.csv(df, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  } else {
    utils::write.csv(df, path, row.names = FALSE, na = "")
  }
  invisible(path)
}

# ---- Readers: the files loaded the way the app loads them -------------------

# kit_read_table(): Protein or compound table as the upload dialog reads it ----
# Mass columns the file does not fill are dropped again.
kit_read_table <- function(path) {
  raw <- read_uploaded_file(path, "csv")
  type <- if (identical(raw[1, 1], "Protein")) "Protein" else "compound"
  tab <- suppressMessages(process_uploaded_table(raw, type))
  if (!is.data.frame(tab)) {
    stop(basename(path), ": ", tab)
  }
  tab <- tab[, colSums(!is.na(tab)) > 0, drop = FALSE]
  rownames(tab) <- NULL
  tab
}

# kit_read_config(): Experiment config as the config dialog reads it ----
kit_read_config <- function(path) {
  df <- read_config_file(path, "csv")
  normalize_config_units(normalize_colnames(df))
}

# kit_config_issues(): What the config dialog says about a config file ----
# character(0) when the upload is accepted.
kit_config_issues <- function(path) {
  validate_config(kit_read_config(path))
}

# kit_path(): A test file of a category ----
kit_path <- function(category, name) {
  file.path(kit_files_dir(), category, paste0(name, ".csv"))
}

# kit_load(): Read a test file of a category ----
kit_load <- function(category, name) {
  path <- kit_path(category, name)
  if (!file.exists(path)) {
    stop(
      path, " does not exist. Run Rscript tests/edge_cases/generate_all.R ",
      "(or the category's generate.R) first."
    )
  }
  if (startsWith(name, "config_")) kit_read_config(path) else kit_read_table(path)
}

# kit_generate(): Write the files of one category ----
# Sources <category>/generate.R for its kit_files(), writes every file into
# `dir`/<category> and reads it back through the app's readers, which proves
# it loads. Returns the paths.
kit_generate <- function(category, dir = kit_files_dir(), ctx = kit_context()) {
  gen <- new.env(parent = parent.frame())
  sys.source(file.path(kit_root(), category, "generate.R"), envir = gen)
  files <- gen$kit_files(ctx)
  out <- file.path(dir, category)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(out, paste0(names(files), ".csv"))
  for (i in seq_along(files)) {
    kit_write(files[[i]], paths[i])
    if (startsWith(names(files)[i], "config_")) {
      kit_read_config(paths[i])
    } else {
      kit_read_table(paths[i])
    }
  }
  invisible(paths)
}

# ---- Declaration and conversion ----------------------------------------------

# kit_sample_table(): The Samples table "Use Experiment Config" fills ----
# With kinetics = FALSE the concentration and time columns are left out, as
# with kinact/KI switched off.
kit_sample_table <- function(cfg, kinetics = TRUE) {
  st <- data.frame(
    Sample = cfg$Sample,
    Protein = cfg$Protein,
    `Compound 1` = cfg$Compound_1,
    check.names = FALSE
  )
  if (!is.null(cfg$Compound_2)) {
    st$`Compound 2` <- cfg$Compound_2
  }
  if (kinetics) {
    st[["Concentration [μM]"]] <- cfg$Compound_Concentration
    st[["Time [min]"]] <- cfg$Incubation_Time
  }
  st$Replicate <- cfg$Replicate
  st
}

# kit_declare(): The Samples table check ----
# status is "PASS", "PASS + WARNING" or "BLOCKED"; message the one-line hint,
# details/note its tooltip.
kit_declare <- function(
  pt,
  ct,
  cfg,
  tol = 3,
  maxm = 4,
  kinetics = TRUE
) {
  chk <- check_sample_table(
    kit_sample_table(cfg, kinetics),
    pt$Protein,
    ct$Compound,
    protein_table = pt,
    compound_table = ct,
    tolerance = tol,
    max_multiples = maxm
  )
  passed <- isTRUE(chk)
  list(
    status = if (!passed) {
      "BLOCKED"
    } else if (is.null(attr(chk, "warning"))) {
      "PASS"
    } else {
      "PASS + WARNING"
    },
    message = if (passed) attr(chk, "warning") else as.character(chk),
    details = attr(chk, "details"),
    note = attr(chk, "note")
  )
}

kit_units <- c(Concentration = "μM", Time = "min")
kit_conc_time <- c("Concentration [μM]", "Time [min]")

# kit_per_M_s(): µM⁻¹ min⁻¹ (the fixture's units) to M⁻¹ s⁻¹ ----
kit_per_M_s <- function(x) x * 1e6 / 60

# kit_run(): Hit screening and, with kinetics, k_obs and kinact/KI ----
# Follows the app's path: add_hits() and summarize_hits() over every sample,
# then complex_kinetics() per protein-compound complex; the Proteoforms tab
# fits each species on the hits of its complex. Returns the hits, the log,
# Total % binding per sample and a summary of every complex.
kit_run <- function(
  ctx,
  pt,
  ct,
  cfg,
  tol = 3,
  maxm = 4,
  kinetics = TRUE
) {
  st <- kit_sample_table(cfg, kinetics)
  session <- list(sendCustomMessage = function(...) invisible(NULL))
  log <- character(0)
  r <- withCallingHandlers(
    {
      r <- add_hits(ctx$result, st, pt, ct, tol, maxm, session, identity)
      r$hits_summary <- summarize_hits(r, st)
      if (kinetics) {
        r$kinetics <- complex_kinetics(
          r$hits_summary,
          st,
          kit_conc_time,
          kit_units
        )
      }
      r
    },
    message = function(m) {
      log <<- c(log, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  h <- r$hits_summary
  out <- list(
    hits = h,
    log = gsub("\033\\[[0-9;]*m", "", log),
    total = tapply(h$`Total % Binding`, h$Sample, `[`, 1) * 100,
    complexes = run_complexes(h, st)$key
  )
  if (kinetics) {
    out$kinetics <- lapply(r$kinetics, kit_complex_summary)
    out$default <- select_complex_kinetics(r)$kinetics_complex$key
  }
  out
}

# kit_complex_summary(): What the Kinetics and Proteoforms tabs show ----
kit_complex_summary <- function(entry) {
  k <- entry$kinact_ki_result
  ratio <- function(fit) {
    if (is.null(fit)) NA_real_ else kit_per_M_s(fit$Ratio[["Estimate"]])
  }
  # Each species fitted on the hits of the complex, over the fitted
  # concentrations (the 0 µM controls are not among them)
  prot <- if (!is.null(entry$binding_kobs_result)) {
    suppressMessages(proteoform_kinetics(
      entry$hits,
      units = kit_units,
      conc_time = c(Concentration = "Conc. [μM]", Time = "Time [min]"),
      concentrations_select = rownames(
        entry$binding_kobs_result$kobs_result_table
      )
    ))
  }
  main <- if (length(prot)) main_proteoform(proteoform_binding(entry$hits))

  list(
    n = length(unique(entry$hits$Sample)),
    reason = entry$reason,
    ratio = ratio(k),
    ci = if (!is.null(k)) kit_per_M_s(unlist(k$Ratio[c("CI 2.5%", "CI 97.5%")])),
    status = k$Status,
    warnings = vapply(k$Warnings, `[[`, character(1), "title"),
    series = if (!is.null(k$Series)) {
      stats::setNames(kit_per_M_s(k$Series$ratio), k$Series$series)
    },
    proteoforms = lapply(prot, function(p) {
      list(
        ratio = ratio(p$kinact_ki_result),
        status = p$kinact_ki_result$Status,
        reason = p$reason,
        n = p$n_samples,
        n_limit = p$n_limit,
        # More than half of the values at a detection limit: orange
        flagged = p$n_samples > 0 && p$n_limit / p$n_samples > 0.5
      )
    }),
    reference = if (!is.null(main)) main$species[main$main]
  )
}

# kit_card(): The former Tot. Binding card of a compound ----
# No longer shown in the app (the Overview tab replaced the Compound and
# Protein View); kept as the number the READMEs compare tests by.
# Per compound, over the samples declaring it: one value per sample, a sample
# the compound was not found in counting with its 0 %, one without any protein
# or complex peak (not measured, NA) left out. `samples` is the count of the
# Mass Shifts card, the samples with a hit of the compound.
kit_card <- function(run, compound) {
  rows <- run$hits[run$hits$Compound %in% compound, , drop = FALSE]
  x <- rows$`Total % Binding`[!duplicated(rows$Sample)] * 100
  x <- x[!is.na(x)]
  c(
    min = min(x),
    max = max(x),
    mean = mean(x),
    sd = stats::sd(x),
    samples = length(unique(rows$Sample[is_complex_row(rows)]))
  )
}

# kit_all_samples(): Mean Total % binding over all samples, 0 without a hit ----
# A sample without any protein or complex peak was not measured and is left out.
kit_all_samples <- function(run) mean(run$total, na.rm = TRUE)

# kit_no_hits(): Samples the Sample View marks "No hits" ----
kit_no_hits <- function(run) {
  hit <- unique(run$hits$Sample[is_complex_row(run$hits)])
  setdiff(unique(run$hits$Sample), hit)
}

# kit_sample_hits(): The hits table of one sample ----
kit_sample_hits <- function(run, sample = kit_reference_sample) {
  run$hits[
    run$hits$Sample == sample,
    c(
      "Mw Protein [Da]",
      "Peak [Da]",
      "Intensity",
      "Compound",
      "Compound Mw [Da]",
      "Binding Stoichiometry",
      "Preferred",
      "% Binding"
    )
  ]
}

# ---- Report --------------------------------------------------------------

kit_fmt <- function(x, digits = 1) {
  if (is.null(x) || !length(x) || is.na(x)) {
    "N/A"
  } else {
    formatC(x, format = "f", digits = digits)
  }
}

# kit_show(): Print what the app shows for one test ----
# The numbers the READMEs quote; generate.R --report prints them for every
# test of its category.
kit_show <- function(title, d, r = NULL) {
  cat("\n=====", title, "=====\n")
  cat("Declaration:", d$status, "\n")
  if (length(d$message)) cat("  ", d$message, "\n")
  if (length(d$details)) cat(paste0("     ", utils::head(d$details, 3)), sep = "\n")
  if (is.null(r)) {
    return(invisible())
  }

  cat(sprintf(
    "All samples: %.2f   reference sample: %.2f\n",
    kit_all_samples(r),
    r$total[[kit_reference_sample]]
  ))
  for (cn in sort(unique(stats::na.omit(r$hits$Compound)))) {
    card <- kit_card(r, cn)
    cat(sprintf(
      "  Tot. Binding card %s: %.2f - %.2f %%, %.2f ± %.2f (%d samples)\n",
      cn, card[["min"]], card[["max"]], card[["mean"]], card[["sd"]],
      as.integer(card[["samples"]])
    ))
  }
  cat("No Hits:", paste(kit_no_hits(r), collapse = ", "), "\n")
  cat("Hits of the reference sample:\n")
  print(kit_sample_hits(r), row.names = FALSE)
  amb <- grep("AMBIG|also fits|read as unbound", r$log, value = TRUE)
  if (length(amb)) {
    cat("Log:", length(amb), "ambiguity lines, first:", amb[1], "\n")
  }
  cat("Complex picker:", paste(r$complexes, collapse = " | "), "\n")
  if (!is.null(r$default)) cat("Selected by default:", r$default, "\n")

  for (key in names(r$kinetics)) {
    k <- r$kinetics[[key]]
    cat(sprintf("Kinetics %s (%d samples): kinact/KI %s", key, k$n, kit_fmt(k$ratio)))
    if (!is.null(k$ci)) cat(sprintf(" [95%% CI %s - %s]", kit_fmt(k$ci[[1]]), kit_fmt(k$ci[[2]])))
    if (!is.null(k$status)) cat("  status", k$status)
    if (!is.null(k$reason)) cat("  reason:", k$reason)
    cat("\n")
    if (length(k$warnings)) cat("    warnings:", paste(k$warnings, collapse = "; "), "\n")
    if (length(k$series)) {
      cat("    series:", paste(names(k$series), vapply(k$series, kit_fmt, ""), collapse = ", "), "\n")
    }
    if (length(k$reference)) cat("    Proteoforms tab reference:", k$reference, "\n")
    for (p in names(k$proteoforms)) {
      x <- k$proteoforms[[p]]
      cat(sprintf(
        "    %s: kinact/KI %s  status %s  limit %d/%d%s%s\n",
        p, kit_fmt(x$ratio), if (is.null(x$status)) "-" else x$status,
        x$n_limit, x$n, if (x$flagged) "  (orange)" else "",
        if (is.null(x$reason)) "" else paste0("  hover: ", x$reason)
      ))
    }
  }
}

# kit_main(): Entry point of a generate.R run as a script ----
# Writes the category's files next to its generate.R (they are git-ignored);
# with --report also prints what the app shows for each of its tests. The
# report reads the files of other categories too, so generate those first
# (generate_all.R does).
kit_main <- function(dir, report = NULL) {
  ctx <- kit_context()
  for (path in kit_generate(basename(dir), dirname(dir), ctx)) {
    cat("Wrote", file.path(basename(dir), basename(path)), "\n")
  }
  if ("--report" %in% commandArgs(trailingOnly = TRUE) && !is.null(report)) {
    options(width = 200)
    report(ctx)
  }
  invisible(ctx)
}
