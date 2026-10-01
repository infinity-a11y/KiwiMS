# Writes the edge-case test files and prints the results the app should show
# for each of them, computed with the app's own functions on the MLKL DB.
# Usage (from KiwiMS_App): Rscript dev/edge_cases/verify_edge_cases.R dev/edge_cases [db_path]
args <- commandArgs(trailingOnly = TRUE)
kit <- args[1]
dir.create(kit, showWarnings = FALSE, recursive = TRUE)
options(box.path = getwd(), width = 200)
box::use(app / logic / conversion_functions[
  read_decon_result, add_hits, summarize_hits, check_sample_table,
  declaration_ambiguities, proteoform_binding,
  check_filter_hits, add_kobs_binding_result, add_kinact_ki_result,
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
  back <- process_uploaded_table(read_uploaded_file(file.path(kit, name), "csv"),
    if (names(df)[1] == "Protein") "Protein" else "compound")
  if (!is.data.frame(back)) stop(name, ": ", back)
  back[, colSums(!is.na(back)) > 0, drop = FALSE]
}
prot <- function(...) { v <- c(...); d <- data.frame(Protein = "MLKL"); for (i in seq_along(v)) d[[paste("Mass", i)]] <- v[i]; d }
cmp <- function(names, ...) { rows <- list(...); k <- max(lengths(rows))
  d <- data.frame(Compound = names); for (i in seq_len(k)) d[[paste("Mass", i)]] <- sapply(rows, function(r) if (i <= length(r)) r[i] else NA); d }

config <- function(c1, c2 = NULL) {
  d <- data.frame(Sample = ss, Replicate = rep, Protein = "MLKL", Compound_1 = c1)
  if (!is.null(c2)) d$Compound_2 <- c2
  d$Compound_Concentration <- conc; d$Concentration_Unit <- "\u00b5M"
  d$Incubation_Time <- time; d$Time_Unit <- "min"
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
  compounds_half_shift   = cmp("BI-8925", c(266, 133))
)
tabs <- lapply(names(files), function(n) write_tab(files[[n]], paste0(n, ".csv")))
names(tabs) <- names(files)

cfgs <- list(
  config_baseline     = config("BI-8925"),
  config_with_decoy   = config("BI-8925", "DECOY"),
  config_decoy_first  = config("DECOY", "BI-8925"),
  config_split_by_rep = config(ifelse(rep == "R1", "BI-8925", "DECOY"))
)
for (n in names(cfgs)) write_cfg(cfgs[[n]], paste0(n, ".csv"))

# ---- runs --------------------------------------------------------------------
sample_table <- function(cfg) {
  st <- data.frame(Sample = cfg$Sample, Protein = cfg$Protein, `Compound 1` = cfg$Compound_1, check.names = FALSE)
  if (!is.null(cfg$Compound_2)) st$`Compound 2` <- cfg$Compound_2
  st[["Concentration [\u03bcM]"]] <- cfg$Compound_Concentration
  st$`Time [min]` <- cfg$Incubation_Time
  st$Replicate <- cfg$Replicate
  st
}

declare <- function(pt, ct, cfg, tol = 3, maxm = 4) {
  st <- sample_table(cfg)
  chk <- check_sample_table(st[, !grepl("^Conc|^Time|^Repl", names(st))], pt$Protein, ct$Compound,
    protein_table = pt, compound_table = ct, tolerance = tol, max_multiples = maxm)
  status <- if (isTRUE(chk)) (if (is.null(attr(chk, "warning"))) "PASS" else "PASS + WARNING") else "BLOCKED"
  msg <- if (isTRUE(chk)) attr(chk, "warning") else chk
  list(status = status, message = msg)
}

fake_session <- list(sendCustomMessage = function(...) invisible(NULL))
units <- c(Concentration = "\u03bcM", Time = "min")
run <- function(pt, ct, cfg, tol = 3, maxm = 4, kinetics = TRUE) {
  st <- sample_table(cfg)
  log <- character(0)
  r <- withCallingHandlers(add_hits(res, st, pt, ct, tol, maxm, fake_session, identity),
    message = function(m) { log <<- c(log, conditionMessage(m)); invokeRestart("muffleMessage") })
  r$hits_summary <- suppressMessages(summarize_hits(r, st))
  h <- r$hits_summary
  log <- gsub("\033\\[[0-9;]*m", "", log)
  tot <- tapply(h$`Total % Binding`, h$Sample, `[`, 1) * 100
  pb <- proteoform_binding(h)
  per <- if (nrow(pb)) tapply(pb$binding, pb$species, function(x) mean(x, na.rm = TRUE)) else NULL
  lim <- if (nrow(pb)) tapply(!is.na(pb$limit), pb$species, sum) else NULL
  out <- list(h = h, log = log, total = tot, per = per, lim = lim)
  if (kinetics) {
    ok <- try({
      f <- suppressMessages(check_filter_hits(r))
      r$binding_kobs_result <- suppressMessages(add_kobs_binding_result(f,
        conc_time = c(Concentration = "Concentration [\u03bcM]", Time = "Time [min]"), units = units))
      r$kinact_ki_result <- suppressMessages(add_kinact_ki_result(r, units))
      P <- r$kinact_ki_result$Params
      out$pooled <- P[1, 1] / P[2, 1] * 1e6 / 60
      kin <- suppressMessages(proteoform_kinetics(h, units = units,
        conc_time = c(Concentration = "Conc. [\u03bcM]", Time = "Time [min]")))
      out$kin <- lapply(kin, function(k) list(
        ratio = if (!is.null(k$kinact_ki_result)) k$kinact_ki_result$Params[1, 1] / k$kinact_ki_result$Params[2, 1] * 1e6 / 60 else NA,
        nonphysical = k$nonphysical, n = k$n_samples, n_limit = k$n_limit))
    }, silent = TRUE)
    if (inherits(ok, "try-error")) out$kin_error <- as.character(ok)
  }
  out
}

show <- function(title, d, r = NULL, sample = "2026-09-18_MULI+BI-8925_2o5_3min_R1") {
  cat("\n=====", title, "=====\n")
  cat("Declaration:", d$status, "\n")
  if (!is.null(d$message)) cat("  ", d$message, "\n")
  if (is.null(r)) return(invisible())
  cat(sprintf("Mean Total %% binding: %.3f   sample %s: %.3f\n", mean(r$total), sample, r$total[sample]))
  if (!is.null(r$per)) { cat("Per-proteoform mean binding:\n"); print(round(r$per, 3)); cat("Limit values per species:\n"); print(r$lim) }
  s <- r$h[r$h$Sample == sample, c("Mw Protein [Da]", "Peak [Da]", "Intensity", "Compound", "Compound Mw [Da]", "Binding Stoichiometry", "Preferred", "% Binding")]
  cat("Hits of", sample, ":\n"); print(s, row.names = FALSE)
  amb <- grep("AMBIG|within 2|\u0394|\u2194", r$log, value = TRUE)
  if (length(amb)) { cat("Log (ambiguity block):\n"); cat(head(amb, 8), sep = "\n") }
  fits <- grep("also fits", r$log)
  cat("Per-sample 'also fits' log lines:", length(fits), "\n")
  if (length(fits)) cat(r$log[fits[1] + 0:2], sep = "\n")
  if (!is.null(r$pooled)) cat(sprintf("Pooled kinact/KI: %.1f M-1 s-1\n", r$pooled))
  if (!is.null(r$kin)) for (k in names(r$kin)) with(r$kin[[k]], cat(sprintf("  %s: kinact/KI %s  nonphysical=%s  limit %d/%d%s\n",
    k, if (is.na(ratio)) "N/A" else sprintf("%.1f", ratio), nonphysical, n_limit, n, if (n > 0 && n_limit / n > 0.5) "  -> orange flag" else "")))
  if (!is.null(r$kin_error)) cat("kinetics error:", r$kin_error, "\n")
}

T <- tabs; C <- cfgs
base <- run(T$proteins_baseline, T$compounds_baseline, C$config_baseline)
show("S0 baseline", declare(T$proteins_baseline, T$compounds_baseline, C$config_baseline), base)

show("S1 case 2: DECOY 268 in same samples", declare(T$proteins_baseline, T$compounds_decoy_268, C$config_with_decoy))
show("S2 case 2 via stoichiometry: DECOY 133 (max 4)", declare(T$proteins_baseline, T$compounds_decoy_133, C$config_with_decoy))
show("S2b DECOY 133, max stoichiometry 1", declare(T$proteins_baseline, T$compounds_decoy_133, C$config_with_decoy, maxm = 1))
show("S3 boundary DECOY 272, tol 3", declare(T$proteins_baseline, T$compounds_decoy_272, C$config_with_decoy, tol = 3))
show("S3b boundary DECOY 272, tol 2.9", declare(T$proteins_baseline, T$compounds_decoy_272, C$config_with_decoy, tol = 2.9))
show("S4 DECOY 268 in other samples", declare(T$proteins_baseline, T$compounds_decoy_268, C$config_split_by_rep))

show("S5 case 4: unbound 21904.84 = complex", declare(T$proteins_unbound_is_complex, T$compounds_baseline, C$config_baseline),
  run(T$proteins_unbound_is_complex, T$compounds_baseline, C$config_baseline))
show("S6 case 4: two unbound 5 Da apart", declare(T$proteins_two_unbound, T$compounds_baseline, C$config_baseline),
  run(T$proteins_two_unbound, T$compounds_baseline, C$config_baseline))

ctrl <- run(T$proteins_baseline, T$compounds_shift_250, C$config_baseline)
show("S7-control: baseline proteins, BI-8925 266/250", declare(T$proteins_baseline, T$compounds_shift_250, C$config_baseline), ctrl)
s7 <- run(T$proteins_shared_complex, T$compounds_shift_250, C$config_baseline)
show("S7 case 5: 21654.84 + 250 = 21638.84 + 266", declare(T$proteins_shared_complex, T$compounds_shift_250, C$config_baseline), s7)
cat("S7 vs control, max |Total % diff|:", max(abs(s7$total - ctrl$total[names(s7$total)])), "\n")

s8 <- run(T$proteins_baseline, T$compounds_close_shifts, C$config_baseline)
show("S8 case 1: shifts 266/264", declare(T$proteins_baseline, T$compounds_close_shifts, C$config_baseline), s8)
cat("S8 vs baseline, max |Total % diff|:", max(abs(s8$total - base$total[names(s8$total)])), "\n")
s8b <- run(T$proteins_baseline, T$compounds_half_shift, C$config_baseline)
show("S8b case 3: shifts 266/133", declare(T$proteins_baseline, T$compounds_half_shift, C$config_baseline), s8b)
cat("S8b vs baseline, max |Total % diff|:", max(abs(s8b$total - base$total[names(s8b$total)])), "\n")

s9 <- run(T$proteins_baseline, T$compounds_decoy_first, C$config_decoy_first)
show("S9 multi-compound: DECOY listed first", declare(T$proteins_baseline, T$compounds_decoy_first, C$config_decoy_first), s9)
cat("S9 vs baseline, max |Total % diff|:", max(abs(s9$total - base$total[names(s9$total)])), "\n")

show("S10 colouring files (declaration)", declare(T$proteins_two_unbound, T$compounds_decoy_271, C$config_baseline))
