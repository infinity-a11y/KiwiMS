# Replicates

How replicates and replicate series are named, checked and capped. Setup and
conventions are in the [kit README](../README.md); B0 is in
[baseline](../baseline/README.md).

## What they mean

- **Condition:** one combination of protein, compound(s), concentration and
  time, e.g. MLKL + BI-8925 at 10 µM for 20 min.
- **Replicate:** one sample of a condition. `…_10_20min_R1` and
  `…_10_20min_R2` are the two replicates of the 10 µM / 20 min condition.
- **Replicate series:** all samples with the same replicate number, i.e. one
  complete repeat of the whole experiment. Series R1 holds the R1 sample of
  every condition. A series is what a repeat on another day, plate or stock
  dilution produces.

Replicates are counted per condition, series across the whole experiment.

## Implementation

`app/logic/conversion_functions.R`, unless noted.

- **Naming (`compute_replicate_labels()`):** the Replicate column of the
  Samples table holds the series, resolved per sample:

  | Source | Example | Replicate |
  |---|---|---|
  | Config, Replicate column | `R2`, `Rep2`, `Day2` | the value as given |
  | No config value, name ends in `_R<n>` (any case, extension ignored) | `…_10_20min_R2.raw`, `…_r02` | `R2` (leading zeros dropped) |
  | Neither | `Plate_A1.raw`, `…-R1`, `…_rep1` | empty: no series |

  `kinetic_series_labels()` reads a Replicate value equal to the sample name
  without `_R<n>` as empty. Tables saved before 0.7.5 held that condition name
  in the column. `sort_series()` orders series naturally (R2 before R10).
- **What uses them:** every sample enters the fits on its own; replicates are
  never averaged before fitting (means ± SD in the Binding Curve are for
  display). The series drive the per-series fits ("kinact/Ki by Replicate
  Series") and the marker fill of the Fit plots (R1 filled, R2 open, R3
  dotted, R4 open with dot).
- **Checks in `check_sample_table()`:**

  | Check | Rule | Result |
  |---|---|---|
  | Replicate series | at most `run_limits$max_series` = 4, kinact/KI on only | blocked |
  | Replicates per condition | at most `run_limits$max_replicates` = 4; with kinact/KI a condition is protein + compounds + concentration + time, 0 µM exempt; without, the samples named alike up to `_R<n>` | blocked |
  | `replicate_mismatches()` | samples named alike up to `_R<n>` must share protein, compounds (as a set) and, with kinact/KI, concentration and time; the first differing column is named | warning |
  | `replicate_conflicts()` | a Replicate value whose number differs from the `_R<n>` ending (`Rep2` on `…_R1`); values without a number never conflict | warning, config wins |

  All warnings of a table share one orange hint, separated by " · ", the mass
  ambiguities first.
- **Config upload (`validate_config()`, `app/logic/helper_functions.R`):**
  more than 4 distinct non-empty Replicate values refuse the file.

## Edge cases

| # | Edge case | Behaviour | Tests |
|---|---|---|---|
| 1 | One replicate declared at another time | warning, analysed as declared | RP1, *concentration and time* |
| 2 | Replicate and mass warnings at once | one hint, mass pairs first | RP1b |
| 3 | Replicates naming different compounds | warning (MA1e) | *first column* |
| 4 | Protein and compound both differ | Protein is named | *first column* |
| 5 | Compounds of a replicate in another order | no warning | *as a set* |
| 6 | Names without `_R<n>`, or a group of one | no group, no warning | *no group* |
| 7 | 5 series in the config | upload refused | RP2, *config caps* |
| 8 | 5 series in the Samples table | blocked with kinact/KI, passes without | RP2, *series cap* |
| 9 | Exactly 4 series | passes | *series cap*, *config caps* |
| 10 | Replicate column holding condition names (pre-0.7.5) | read as empty, name decides | *condition names* |
| 11 | 5–6 replicates of one condition | blocked | RP3, *at most 4* |
| 12 | Exactly 4 replicates | passes | *at most 4* |
| 13 | Many 0 µM controls | exempt | *untreated controls* |
| 14 | Without kinact/KI, 5 samples named alike | blocked | *named alike* |
| 15 | Replicate contradicting the file name | warning, config wins | RP4, *by its number* |
| 16 | `R01` on `_R1`, `Day` on `_R1`, a value on a name without `_R<n>` | no conflict | *by its number* |
| 17 | No Replicate values at all | names decide (`_R1` → R1) | RP5, *config, ending or nothing* |
| 18 | Partial config, `_R02`, `_r3`, `-R1`, `_rep1` | per-sample fallback; only `_R<n>` is recognised | *config, ending or nothing* |
| 19 | Series labels R1, R2, R10 | natural order | *sort naturally* |

Tests in *italics* are synthetic, in the automated suite only.

## Manual tests

### RP1 Replicates declared differently (warning)

`baseline/proteins_baseline` · `baseline/compounds_baseline` · `replicates/config_rep_mismatch` (R2 of 10 µM / 20 min declared at 25 min)

- Orange hint: "Replicates declared differently: 1 group". The tooltip reads
  "2026-09-18_MULI+BI-8925_10_20min: Time 20 ↔ 25", then a note that each
  sample is still analysed as declared.
- The table can be confirmed. The run shows what a single typo does:
  - kinact/KI 335.5 against 334.1, series R2 343.8 against 341.0;
  - the 21,816.84 proteoform drops from 231.2 (saturated) to 195.6 (linear).
- **RP1b:** use `mass_ambiguity/proteins_two_unbound` instead. Both warnings
  share one hint: "Ambiguous masses … : 5 pairs · Replicates declared
  differently: 1 group". The tooltip lists the mass pairs first, then the
  replicate group.

### RP2 Five replicate series (blocked)

`baseline/proteins_baseline` · `baseline/compounds_baseline` · `replicates/config_five_series` (Replicate R1, R2, R3, R4, R5 in turn)

- **Config upload:** refused, with "'Replicate': at most 4 replicate series
  (5 found: R1, R2, R3, R4, R5)."
- The Samples table would be blocked as well: "At most 4 replicate series (5
  present)", tooltip "R1: 25 samples", … Only the automated test reaches it;
  in the app the config never gets that far.

### RP3 Too many replicates of one condition (blocked)

`baseline/proteins_baseline` · `baseline/compounds_baseline` · `replicates/config_six_replicates` (the 10 µM samples at 30 and 40 min declared at 20 min)

- Red hint: "At most 4 replicates per condition (1 condition has up to 6)".
- Tooltip: "MLKL + BI-8925, concentration 10, time 20: 6 samples", and the
  note "Replicates are samples with the same protein, compounds,
  concentration and time. Check for a mistyped concentration or time."

### RP4 Replicate value contradicts the file name (warning)

`baseline/proteins_baseline` · `baseline/compounds_baseline` · `replicates/config_rep_swapped` (Replicate R1 and R2 swapped for the 20 samples at 10 µM)

- Orange hint: "Replicate differs from the file name: 20 samples". Tooltip
  lines such as "2026-09-18_MULI+BI-8925_10_20min_R1: Replicate R2", then
  "The Replicate value from the config is used."
- The Replicate column shows the config values (R2 for those `_R1` files).
- **Kinetics:** kinact/KI 334.1, as in B0: every sample still enters the
  global fit on its own. Only the per-series fits change: R1 325.4, R2 343.6
  (B0: 327.9 and 341.0).

### RP5 No Replicate values (names decide)

`baseline/proteins_baseline` · `baseline/compounds_baseline` · `replicates/config_no_replicate` (Replicate column empty)

- Passes, with no hint. The Replicate column shows R1 and R2 from the
  `_R<n>` endings.
- Results equal B0, series included (R1 327.9, R2 341.0).

## Automated tests

`tests/testthat/test-edge-replicates.R`:

| Test | Edge cases |
|---|---|
| the replicate files are written and load like uploads | – |
| RP1: a replicate declared at another time warns | 1 |
| RP1b: replicate and mass warnings share one hint | 2 |
| RP2: five replicate series are refused | 7, 8 |
| RP3: six replicates of one condition are refused | 11 |
| RP4: a Replicate value contradicting the file name warns and wins | 15 |
| RP5: without Replicate values the file names name the series | 17 |
| the series comes from the config, else the _R<n> ending, else nothing | 17, 18 |
| series sort naturally | 19 |
| the series cap is 4, with kinact/KI only | 8, 9 |
| a Replicate column holding condition names reads as empty | 10 |
| the config caps the series it names at 4 | 7, 9 |
| a condition holds at most 4 replicates | 11, 12 |
| untreated controls are not capped | 13 |
| without kinact/KI replicates are the samples named alike | 14 |
| a replicate group names the first column that differs | 3, 4 |
| the compounds of a replicate are compared as a set | 5 |
| samples without an _R<n> ending form no replicate group | 6 |
| with kinact/KI concentration and time are compared too | 1 |
| a Replicate value contradicts the name only by its number | 15, 16 |

## What can still go wrong

- **Re-injections declared as replicates:** if `_R1` and `_R2` are two
  injections of the same incubation, the fit counts them as independent and
  the standard errors and confidence intervals come out too narrow. The data
  can't reveal this, so the app explains it where the user decides or reads
  the result: the Samples Declaration help ("?" next to Browse), the kinact/KI
  help (the ± and the 95 % CI assume separate incubations) and the kinact/Ki
  by Replicate Series help (series made of re-injections agree by
  construction).
- **Other naming:** only `_R<n>` at the end of a name is recognised; `-R1`,
  `_rep1` or `_1` leave the Replicate column empty unless a config fills it.
- **Unequal repeats:** a series spanning fewer than 3 concentrations gets no
  per-series fit and is left out of the Fit tab's series plot.
- **A replicate group split across complexes** (MA1e, the 20 min pair of CX2)
  corrupts no number. Each complex just loses part of a time course or a whole
  series. That's why the replicate checks warn instead of blocking.
- **Five series without kinact/KI** pass the Samples table, since no
  per-series fit is made. The config upload still refuses them.
