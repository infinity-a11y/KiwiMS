# Writes the edge-case test files and prints the results the app should show
# for each of them, computed with the app's own functions on the MLKL DB.
# Kinetics follow the app's path: k_obs and kinact/KI per protein-compound
# complex (complex_kinetics()), proteoforms on the hits of the shown complex.
# Usage (from KiwiMS_App): Rscript dev/edge_cases/verify_edge_cases.R dev/edge_cases [db_path]
args <- commandArgs(trailingOnly = TRUE)
kit <- args[1]
dir.create(kit, showWarnings = FALSE, recursive = TRUE)
options(box.path = getwd(), width = 200)
box::use(app / logic / conversion_functions[
  read_decon_result, add_hits, summarize_hits, check_sample_table,
  proteoform_binding, main_proteoform, complex_kinetics, run_complexes, select_complex_kinetics,
  proteoform_kinetics, read_uploaded_file, process_uploaded_table
])

db <- if (length(args) > 1) args[2] else "E:/KF_Testing/Results/KiwiMS_2026-09-28_id6556.db"
res <- read_decon_result(db)
ss <- names(res$deconvolution)
m <- regmatches(ss, regexec("_([0-9o]+)_([0-9]+)min_(R[12])$", ss))
conc <- as.numeric(sub("o", ".", sapply(m, `[`, 2)))
time <- as.numeric(sapply(m, `[`, 3))
rep <- sapply(m, `[`, 4)

# ---- files ------------------------------------------------------------------
write_tab <- function(df, name) {
  utils::write.csv(df, file.path(kit, name), row.names = FALSE, na = "")
  # Round trip through the app's reader to prove the file loads
  back <- suppressMessages(process_uploaded_table(
    read_uploaded_file(file.path(kit, name), "csv"),
    if (names(df)[1] == "Protein") "Protein" else "compound"
  ))
  if (!is.data.frame(back)) stop(name, ": ", back)
  back[, colSums(!is.na(back)) > 0, drop = FALSE]
}
prot <- function(...) { v <- c(...); d <- data.frame(Protein = "MLKL"); for (i in seq_along(v)) d[[paste("Mass", i)]] <- v[i]; d }
cmp <- function(names, ...) { rows <- list(...); k <- max(lengths(rows))
  d <- data.frame(Compound = names); for (i in seq_len(k)) d[[paste("Mass", i)]] <- sapply(rows, function(r) if (i <= length(r)) r[i] else NA); d }

config <- function(c1, c2 = NULL, t = time, r = rep) {
  d <- data.frame(Sample = ss, Replicate = r, Protein = "MLKL", Compound_1 = c1)
  if (!is.null(c2)) d$Compound_2 <- c2
  d$Compound_Concentration <- conc; d$Concentration_Unit <- "\u00b5M"
  d$Incubation_Time <- t; d$Time_Unit <- "min"
  d
}
write_cfg <- function(d, name) utils::write.csv(d, file.path(kit, name), row.names = FALSE, na = "", fileEncoding = "UTF-8")

files <- list(
  proteins_baseline      = prot(21638.84, 21816.84),
  proteins_two_unbound   = prot(21638.84, 21816.84, 21643.84),
  proteins_unbound_is_complex = prot(21638.84, 21816.84, 21904.84),
  proteins_shared_complex = prot(21638.84, 21816.84, 21654.84),
  compounds_baseline     = cmp("BI-8925", 266),
  compounds_decoy_268    = cmp(c("BI-8925", "DECOY"), 266, 268),
  compounds_decoy_133    = cmp(c("BI-8925", "DECOY"), 266, 133),
  compounds_decoy_272    = cmp(c("BI-8925", "DECOY"), 266, 272),
  compounds_decoy_271    = cmp(c("BI-8925", "DECOY"), 266, 271),
  compounds_decoy_first  = cmp(c("BI-8925", "DECOY"), 266, 500),
  compounds_shift_250    = cmp("BI-8925", c(266, 250)),
  compounds_close_shifts = cmp("BI-8925", c(266, 264)),
  compounds_half_shift   = cmp("BI-8925", c(266, 133)),
  compounds_two_names    = cmp(c("BI-8925", "BI-8926"), 266, 266),
  compounds_8926_no_hits = cmp(c("BI-8925", "BI-8926"), 266, 500)
)
tabs <- lapply(names(files), function(n) write_tab(files[[n]], paste0(n, ".csv")))
names(tabs) <- names(files)

# BI-8926 on a few 10 uM samples only, one replicate pair split (screenshot case)
mixed <- conc == 10 & (time %in% c(1, 10, 15) | (time == 20 & rep == "R1"))
# BI-8926 owns 2.5, 5 and 10 uM
by_conc <- conc %in% c(2.5, 5, 10)
# ... but at 10 uM only the 1 and 3 min samples
short <- conc %in% c(2.5, 5) | (conc == 10 & time %in% c(1, 3))
# R2 of 10 uM / 20 min declared at 25 min
t_typo <- time; t_typo[conc == 10 & time == 20 & rep == "R2"] <- 25
six_reps <- time; six_reps[conc == 10 & time %in% c(30, 40)] <- 20
swapped <- rep; swapped[conc == 10] <- ifelse(rep[conc == 10] == "R1", "R2", "R1")

cfgs <- list(
  config_baseline     = config("BI-8925"),
  config_with_decoy   = config("BI-8925", "DECOY"),
  config_decoy_first  = config("DECOY", "BI-8925"),
  config_split_by_rep = config(ifelse(rep == "R1", "BI-8925", "DECOY")),
  config_mixed_10uM   = config(ifelse(mixed, "BI-8926", "BI-8925")),
  config_split_by_conc = config(ifelse(by_conc, "BI-8926", "BI-8925")),
  config_split_short_times = config(ifelse(short, "BI-8926", "BI-8925")),
  config_rep_mismatch = config("BI-8925", t = t_typo),
  # Replicate R1-R5 in turn: five series
  config_five_series = config("BI-8925", r = paste0("R", (seq_along(ss) - 1) %% 5 + 1)),
  # 10 uM samples at 30 and 40 min declared at 20 min: six replicates there
  config_six_replicates = config("BI-8925", t = six_reps),
  # Replicate R1 and R2 swapped for the 10 uM samples
  config_rep_swapped = config("BI-8925", r = swapped)
)
for (n in names(cfgs)) write_cfg(cfgs[[n]], paste0(n, ".csv"))

# 385 made-up samples: one over the sample cap (config upload check only)
write_cfg(
  data.frame(
    Sample = sprintf("Sample_%03d.raw", 1:385), Replicate = "", Protein = "MLKL",
    Compound_1 = "BI-8925", Compound_Concentration = rep(c(0, 2.5, 5, 10, 20), 77),
    Concentration_Unit = "µM", Incubation_Time = rep(c(0, 1, 3, 5, 10, 15, 20), 55),
    Time_Unit = "min"
  ),
  "config_385_samples.csv"
)

# ---- runs --------------------------------------------------------------------
sample_table <- function(cfg) {
  st <- data.frame(Sample = cfg$Sample, Protein = cfg$Protein, `Compound 1` = cfg$Compound_1, check.names = FALSE)
  if (!is.null(cfg$Compound_2)) st$`Compound 2` <- cfg$Compound_2
  st[["Concentration [\u03bcM]"]] <- cfg$Compound_Concentration
  st$`Time [min]` <- cfg$Incubation_Time
  st$Replicate <- cfg$Replicate
  st
}

# kinetics = FALSE checks the table as with kinact/KI switched off (no
# concentration and time columns)
declare <- function(pt, ct, cfg, tol = 3, maxm = 4, kinetics = TRUE) {
  st <- sample_table(cfg)
  if (!kinetics) st <- st[, !grepl("^Conc|^Time", names(st))]
  chk <- check_sample_table(st, pt$Protein, ct$Compound,
    protein_table = pt, compound_table = ct, tolerance = tol, max_multiples = maxm)
  status <- if (isTRUE(chk)) (if (is.null(attr(chk, "warning"))) "PASS" else "PASS + WARNING") else "BLOCKED"
  msg <- if (isTRUE(chk)) attr(chk, "warning") else chk
  list(status = status, message = msg, details = attr(chk, "details"))
}

fake_session <- list(sendCustomMessage = function(...) invisible(NULL))
units <- c(Concentration = "\u03bcM", Time = "min")
conc_time <- c("Concentration [\u03bcM]", "Time [min]")
per_M_s <- function(x) x * 1e6 / 60
ratio_of <- function(k) if (is.null(k)) NA_real_ else k$Ratio[["Estimate"]]

run <- function(pt, ct, cfg, tol = 3, maxm = 4, kinetics = TRUE) {
  st <- sample_table(cfg)
  if (!kinetics) st <- st[, !grepl("^Conc|^Time", names(st))]
  log <- character(0)
  r <- withCallingHandlers({
    r <- add_hits(res, st, pt, ct, tol, maxm, fake_session, identity)
    r$hits_summary <- summarize_hits(r, st)
    if (kinetics) r$kinetics <- complex_kinetics(r$hits_summary, st, conc_time, units)
    r
  }, message = function(m) { log <<- c(log, conditionMessage(m)); invokeRestart("muffleMessage") })
  h <- r$hits_summary
  log <- gsub("\033\\[[0-9;]*m", "", log)
  tot <- tapply(h$`Total % Binding`, h$Sample, `[`, 1) * 100
  pb <- proteoform_binding(h)
  per <- if (nrow(pb)) tapply(pb$binding, pb$species, function(x) mean(x, na.rm = TRUE)) else NULL
  lim <- if (nrow(pb)) tapply(!is.na(pb$limit), pb$species, sum) else NULL
  out <- list(h = h, log = log, total = tot, per = per, lim = lim,
    complexes = run_complexes(h, st)$key)
  if (kinetics) {
    out$complex <- lapply(r$kinetics, function(e) {
      k <- e$kinact_ki_result
      # The Proteoforms tab fits each species on the hits of the shown complex,
      # over the fitted concentrations (the 0 \u00b5M controls are not among them)
      prot <- if (!is.null(e$binding_kobs_result)) suppressMessages(proteoform_kinetics(
        e$hits, units = units,
        conc_time = c(Concentration = "Conc. [\u03bcM]", Time = "Time [min]"),
        concentrations_select = rownames(e$binding_kobs_result$kobs_result_table)))
      pb <- proteoform_binding(e$hits)
      main <- if (length(prot)) main_proteoform(pb)
      list(
        n = length(unique(e$hits$Sample)),
        reason = e$reason,
        ratio = per_M_s(ratio_of(k)),
        ci = if (!is.null(k)) per_M_s(k$Ratio[c("CI 2.5%", "CI 97.5%")]),
        status = k$Status,
        warnings = vapply(k$Warnings, `[[`, character(1), "title"),
        series = if (!is.null(k$Series)) setNames(per_M_s(k$Series$ratio), k$Series$series),
        skipped = e$binding_kobs_result$skipped,
        prot = lapply(prot, function(p) list(ratio = per_M_s(ratio_of(p$kinact_ki_result)),
          status = p$kinact_ki_result$Status, why = p$reason,
          n = p$n_samples, n_limit = p$n_limit)),
        reference = if (!is.null(main)) main$species[main$main]
      )
    })
    out$default <- select_complex_kinetics(r)$kinetics_complex$key
  }
  out
}

fmt <- function(x, d = 1) if (is.null(x) || !length(x) || is.na(x)) "N/A" else formatC(x, format = "f", digits = d)

show <- function(title, d, r = NULL, sample = "2026-09-18_MULI+BI-8925_2o5_3min_R1") {
  cat("\n=====", title, "=====\n")
  cat("Declaration:", d$status, "\n")
  if (!is.null(d$message)) cat("  ", d$message, "\n")
  if (length(d$details)) cat(paste0("     ", utils::head(d$details, 3)), sep = "\n")
  if (is.null(r)) return(invisible())
  cat(sprintf("Mean Total %% binding over all samples: %.3f   sample %s: %.3f\n", mean(r$total), sample, r$total[sample]))
  # The Tot. Binding card of the Compound/Protein View: per compound, over its
  # hit rows (a sample counts once per proteoform carrying the compound)
  for (cn in sort(unique(stats::na.omit(r$h$Compound)))) {
    x <- r$h$`Total % Binding`[r$h$Compound %in% cn] * 100
    cat(sprintf("  Tot. Binding card %s: %.2f%% - %.2f%%, %.2f%% ± %.2f (%d rows, %d samples)\n",
      cn, min(x), max(x), mean(x), stats::sd(x), length(x),
      length(unique(r$h$Sample[r$h$Compound %in% cn]))))
  }
  if (!is.null(r$per)) { cat("Per-proteoform mean binding:\n"); print(round(r$per, 3)); cat("Limit values per species:\n"); print(r$lim) }
  s <- r$h[r$h$Sample == sample, c("Mw Protein [Da]", "Peak [Da]", "Intensity", "Compound", "Compound Mw [Da]", "Binding Stoichiometry", "Preferred", "% Binding")]
  cat("Hits of", sample, ":\n"); print(s, row.names = FALSE)
  amb <- grep("AMBIG|within 2|\u0394|\u2194", r$log, value = TRUE)
  if (length(amb)) { cat("Log (ambiguity block):\n"); cat(head(amb, 8), sep = "\n") }
  fits <- grep("also fits", r$log)
  cat("Per-sample 'also fits' log lines:", length(fits), "\n")
  if (length(fits)) cat(r$log[fits[1] + 0:2], sep = "\n")
  cat("Complex picker:", paste(r$complexes, collapse = " | "), "\n")
  if (!is.null(r$default)) cat("Selected by default:", r$default, "\n")
  for (k in names(r$complex)) with(r$complex[[k]], {
    cat(sprintf("Kinetics %s (%d samples): kinact/KI %s M-1 s-1", k, n, fmt(ratio)))
    if (!is.null(ci)) cat(sprintf(" [95%% CI %s - %s]", fmt(ci[[1]]), fmt(ci[[2]])))
    if (!is.null(status)) cat("  status", status)
    if (!is.null(reason)) cat("  reason:", reason)
    cat("\n")
    if (length(warnings)) cat("    warnings:", paste(warnings, collapse = "; "), "\n")
    if (length(series)) cat("    series:", paste(names(series), vapply(series, fmt, ""), collapse = ", "), "\n")
    if (!is.null(skipped) && nrow(skipped)) cat("    skipped:", paste(skipped$concentration, skipped$reason, sep = " ", collapse = "; "), "\n")
    if (length(reference)) cat("    Proteoforms tab reference:", reference, "\n")
    for (p in names(prot)) with(prot[[p]], cat(sprintf("    %s: kinact/KI %s  status %s  limit %d/%d%s%s\n",
      p, fmt(ratio), if (is.null(status)) "-" else status, n_limit, n,
      if (n > 0 && n_limit / n > 0.5) "  -> orange flag" else "",
      if (is.null(why)) "" else paste0("  N/A hover: ", why))))
  })
}

T <- tabs; C <- cfgs
off <- function(...) declare(..., kinetics = FALSE)

base <- run(T$proteins_baseline, T$compounds_baseline, C$config_baseline)
show("T0 baseline", declare(T$proteins_baseline, T$compounds_baseline, C$config_baseline), base)

# T1: two compounds per sample; screened with kinact/KI off
show("T1a decoy 268, same samples, kinact/KI off", off(T$proteins_baseline, T$compounds_decoy_268, C$config_with_decoy))
show("T1a' same, kinact/KI on", declare(T$proteins_baseline, T$compounds_decoy_268, C$config_with_decoy))
show("T1b decoy 133 (max 4), kinact/KI off", off(T$proteins_baseline, T$compounds_decoy_133, C$config_with_decoy))
show("T1b decoy 133, max stoichiometry 1", off(T$proteins_baseline, T$compounds_decoy_133, C$config_with_decoy, maxm = 1))
show("T1c decoy 272, tol 3", off(T$proteins_baseline, T$compounds_decoy_272, C$config_with_decoy, tol = 3))
show("T1c decoy 272, tol 2.9", off(T$proteins_baseline, T$compounds_decoy_272, C$config_with_decoy, tol = 2.9))
show("T1e decoy 268 in the R2 samples", declare(T$proteins_baseline, T$compounds_decoy_268, C$config_split_by_rep),
  run(T$proteins_baseline, T$compounds_decoy_268, C$config_split_by_rep))

show("T2 unbound 21904.84 = complex", declare(T$proteins_unbound_is_complex, T$compounds_baseline, C$config_baseline),
  run(T$proteins_unbound_is_complex, T$compounds_baseline, C$config_baseline))
t3 <- run(T$proteins_two_unbound, T$compounds_baseline, C$config_baseline)
show("T3 two unbound 5 Da apart", declare(T$proteins_two_unbound, T$compounds_baseline, C$config_baseline), t3)
cat("T3 vs T0, max |Total % diff|:", max(abs(t3$total - base$total[names(t3$total)])), "\n")

ctrl <- run(T$proteins_baseline, T$compounds_shift_250, C$config_baseline)
show("T4 control: baseline proteins, BI-8925 266/250", declare(T$proteins_baseline, T$compounds_shift_250, C$config_baseline), ctrl)
t4 <- run(T$proteins_shared_complex, T$compounds_shift_250, C$config_baseline)
show("T4 21654.84 + 250 = 21638.84 + 266", declare(T$proteins_shared_complex, T$compounds_shift_250, C$config_baseline), t4)
cat("T4 vs control, max |Total % diff|:", max(abs(t4$total - ctrl$total[names(t4$total)])), "\n")

t5a <- run(T$proteins_baseline, T$compounds_close_shifts, C$config_baseline)
show("T5a shifts 266/264", declare(T$proteins_baseline, T$compounds_close_shifts, C$config_baseline), t5a)
cat("T5a vs T0, max |Total % diff|:", max(abs(t5a$total - base$total[names(t5a$total)])), "\n")
t5b <- run(T$proteins_baseline, T$compounds_half_shift, C$config_baseline)
show("T5b shifts 266/133", declare(T$proteins_baseline, T$compounds_half_shift, C$config_baseline), t5b)
cat("T5b vs T0, max |Total % diff|:", max(abs(t5b$total - base$total[names(t5b$total)])), "\n")

t6 <- run(T$proteins_baseline, T$compounds_decoy_first, C$config_decoy_first, kinetics = FALSE)
show("T6 DECOY listed first, kinact/KI off", off(T$proteins_baseline, T$compounds_decoy_first, C$config_decoy_first), t6)
cat("T6 vs T0, max |Total % diff|:", max(abs(t6$total - base$total[names(t6$total)])), "\n")

show("T7 colouring files (declaration)", declare(T$proteins_two_unbound, T$compounds_decoy_271, C$config_baseline))

# T8-T11: protein-compound complexes and replicates
show("T8 BI-8926 on a few 10 uM samples", declare(T$proteins_baseline, T$compounds_two_names, C$config_mixed_10uM))
show("T9a BI-8926 owns 2.5/5/10 uM", declare(T$proteins_baseline, T$compounds_two_names, C$config_split_by_conc),
  run(T$proteins_baseline, T$compounds_two_names, C$config_split_by_conc))
show("T9b same, BI-8926 at 500 Da (no hits)", declare(T$proteins_baseline, T$compounds_8926_no_hits, C$config_split_by_conc),
  run(T$proteins_baseline, T$compounds_8926_no_hits, C$config_split_by_conc))
show("T9c BI-8926 at 10 uM only at 1/3 min", declare(T$proteins_baseline, T$compounds_two_names, C$config_split_short_times))
show("T10 decoy 500 in every sample, kinact/KI on", declare(T$proteins_baseline, T$compounds_decoy_first, C$config_with_decoy))
t11 <- run(T$proteins_baseline, T$compounds_baseline, C$config_rep_mismatch)
show("T11 R2 of 10 uM / 20 min declared at 25 min", declare(T$proteins_baseline, T$compounds_baseline, C$config_rep_mismatch), t11)
show("T11b same with proteins_two_unbound (two warnings)", declare(T$proteins_two_unbound, T$compounds_baseline, C$config_rep_mismatch))

# T12-T15: replicate series, replicates per condition and the sample cap
box::use(app / logic / helper_functions[validate_config, normalize_config_units])
config_upload <- function(name) {
  d <- utils::read.csv(file.path(kit, name), check.names = FALSE, encoding = "UTF-8")
  issues <- validate_config(normalize_config_units(d))
  cat("Config upload", name, if (length(issues)) "REFUSED" else "ACCEPTED", "\n")
  if (length(issues)) cat(paste0("   ", issues), sep = "\n")
}
cat("\n===== T12 five replicate series =====\n"); config_upload("config_five_series.csv")
show("T12 same table declared anyway", declare(T$proteins_baseline, T$compounds_baseline, C$config_five_series))
show("T13 six replicates at 10 uM / 20 min", declare(T$proteins_baseline, T$compounds_baseline, C$config_six_replicates))
t14 <- run(T$proteins_baseline, T$compounds_baseline, C$config_rep_swapped)
show("T14 Replicate swapped for the 10 uM samples", declare(T$proteins_baseline, T$compounds_baseline, C$config_rep_swapped), t14)
cat("\n===== T15 sample cap =====\n"); config_upload("config_385_samples.csv")
big <- utils::read.csv(file.path(kit, "config_385_samples.csv"), check.names = FALSE, encoding = "UTF-8")
show("T15 Samples table with 385 rows", declare(T$proteins_baseline, T$compounds_baseline, big))
