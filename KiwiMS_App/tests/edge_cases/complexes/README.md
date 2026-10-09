# Protein-compound complexes

Which compounds a sample is screened for, and how k_obs and kinact/KI split a
run into protein-compound complexes. Setup and conventions are in the
[kit README](../README.md); B0 is in [baseline](../baseline/README.md).

## Implementation

All in `app/logic/conversion_functions.R`.

- **Screening (`add_hits()`):** a sample is screened for every compound in its
  Compound 1 … n columns, matched with `%in%` against the compound table.
  Column order and table order don't matter (CX1). The bug this fixed
  compared the columns with `==` against a one-row frame, so a compound was
  only found when its column matched its row of the compound table.
- **Complex:** one protein with one compound, keyed "Protein + Compound"
  (`complex_key()`). `run_complexes()` lists every pair with a hit and, given
  the Samples table, every declared pair, so a complex without hits still
  shows up in the picker (CX3b).
- **Kinetics per complex (`complex_kinetics()` → `fit_complex()`):** k_obs and
  kinact/KI are fitted on the complex's own samples. `complex_hits()` gives a
  single-compound sample its total binding, and a multi-compound sample only
  the preferred share of the complex's compound. A complex is skipped, with a
  reason the kinetics view shows, when its compound has no hit, when too few
  concentrations, hits or time points remain, or when no concentration could
  be fitted. `select_complex_kinetics()` opens the first complex with a k_obs
  fit by default.
- **Declaration rules with kinact/KI (`check_sample_table()`),** in this order:
  1. Concentration and Time filled ("Fill Concentrations", "Fill Time");
  2. 3–10 distinct non-zero concentrations over the whole table;
  3. **one compound per sample.** The concentration column holds one value
     per sample, and a second compound competing for the site changes k_obs
     (CX4). The tooltip lists the first 12 samples, then "and n more";
  4. **per complex:** at least 3 distinct non-zero concentrations, and for
     each at least 3 distinct non-zero time points. Concentration 0 is the
     untreated control and is exempt; time 0 doesn't count as a time point.
     With more than one complex the message starts with "Protein + Compound:".
- **Declaration rules always applied,** before the ones above: proteins and
  compounds declared ("Declare Proteins and Compounds"), names known
  ("Protein name not declared", "Compound name not declared"), every sample
  with a protein and a compound ("Assign proteins", "Assign compounds"), no
  compound twice in a sample ("Duplicated compounds").

## Edge cases

| # | Edge case | Behaviour | Tests |
|---|---|---|---|
| 1 | A sample lists its compounds in another order than the compound table | all screened | CX1 |
| 2 | A declared compound never hits | listed in the picker, "No hits of … in its samples" | CX1, CX3b, *picker* |
| 3 | A compound on a few samples only (one concentration) | blocked by its complex's design | CX2 |
| 4 | Two names for one molecule, one per concentration range | two complexes, fitted apart | CX3a |
| 5 | Too few time points at one concentration of one complex, enough over the table | blocked | CX3c, *each complex on its own* |
| 6 | Several compounds per sample with kinact/KI | blocked, listed in the tooltip | CX4 |
| 7 | Only Compound 2 filled | one compound, passes | CX4 |
| 8 | Several compounds per sample without kinact/KI | passes; no design rules | CX4, *without kinact/KI* |
| 9 | Exactly 3 concentrations × 3 time points | passes | *boundaries* |
| 10 | 2 concentrations, 2 time points, or 11 concentrations | blocked | *boundaries* |
| 11 | Time 0 at a non-zero concentration | not a time point | *boundaries* |
| 12 | Untreated control (0 µM) at any time, any number | exempt | *untreated control* |
| 13 | Two proteins | one complex each; the message names the failing one | *each protein* |
| 14 | Concentration or time missing | "Fill Concentrations" / "Fill Time" | *missing values* |
| 15 | Unknown protein or compound name, a name with a trailing space | blocked | *names* |
| 16 | A sample without protein or compound, a compound twice | blocked | *names* |

Tests in *italics* are synthetic, in the automated suite only.

## Manual tests

### CX1 Multi-compound screening (kinact/KI off)

`baseline/proteins_baseline` · `complexes/compounds_decoy_first` (BI-8925 266, DECOY 500) · `complexes/config_decoy_first` (Compound 1 = DECOY, Compound 2 = BI-8925)

**Switch kinact/KI off first.** With it on, the table is blocked with "One
compound per sample for kinact/KI (122 samples list several)" (CX4).

- Results are identical to B0: Tot. Binding card (BI-8925) 73.19 ± 23.61, all
  samples 70.24.
- Before the fix, **0 of 122** samples got a BI-8925 hit.

### CX2 A compound on a few samples only (blocked)

`baseline/proteins_baseline` · `complexes/compounds_two_names` (BI-8925 266, BI-8926 266) · `complexes/config_mixed_10uM` (BI-8926 on the 10 µM samples at 1, 10 and 15 min and on 20 min R1; BI-8925 everywhere else)

- Red hint: "MLKL + BI-8926: At least 3 different non-zero concentrations
  required (1 present)".
- Before, the design rules were checked over the whole table, which passed.
  BI-8926 then went into the run with 7 samples at one concentration. With a
  mass that gives hits it showed "Too few concentrations, hits or time points";
  with one that gives none, its samples left the kinetics without a trace. The
  10 µM curve of BI-8925 lost those 7 samples as well.

### CX3 Two complexes, one per concentration range

**CX3a:** `baseline/proteins_baseline` · `complexes/compounds_two_names` · `complexes/config_split_by_conc` (BI-8926 at 2.5, 5 and 10 µM; BI-8925 at 0, 20, 40 and 80 µM)

- Passes, with no hint.
- Every sample reads as in B0 (all samples 70.24); only the compound names
  differ, and the reference sample's hit now reads BI-8926.
- **Tot. Binding card**, per compound:
  - BI-8925: 9.20–100 %, **83.57 ± 18.22** (the high concentrations);
  - BI-8926: 8.29–92.78 %, **61.97 ± 23.70** (2.5, 5 and 10 µM).
- **Mass Shifts card** (Compound View): BI-8925 [266.0 Da] ×61, BI-8926 ×59.
- **Complex picker:** BI-8925 and BI-8926 under MLKL; BI-8925 selected.
  - **BI-8925:** 62 samples (the 0 µM controls included). kinact/KI
    **224.3** (CI 199.9–251.0), status linear, "Saturation not reached".
    Series R1 220.2, R2 228.6.
  - **BI-8926:** 60 samples, kinact/KI **330.4** (CI 306.9–357.2), linear.
  - These are the same molecule. The two values differ because each complex is
    fitted on its own concentrations only.
- The BI-8926 Binding Curve has no 0 µM baseline: the controls are declared
  with BI-8925.

**CX3b, declared complex without hits:** `baseline/proteins_baseline` · `complexes/compounds_8926_no_hits` (BI-8926 at 500 Da) · `complexes/config_split_by_conc`

- Passes. Only BI-8925 has a Tot. Binding card: 9.20–100 %, **83.57 ±
  18.22**, as in CX3a. All samples fall to 41.48.
- The complex picker still lists **BI-8926**. Pick it: the kinetics view shows
  the "no kinetics" card with "No hits of BI-8926 in its samples".
- The log has, under "MLKL + BI-8926 (60 samples)": "⚠ No hits of BI-8926 in
  its samples. Skipping binding kinetics analysis."
- BI-8925 is unchanged from CX3a (224.3).

**CX3c, time points per complex:** `baseline/proteins_baseline` · `complexes/compounds_two_names` · `complexes/config_split_short_times` (as CX3a, but BI-8926 has 10 µM only at 1 and 3 min)

- Red hint: "MLKL + BI-8926: At least 3 different non-zero time points
  required per concentration (concentration 10 has only 2)".
- Over the whole table 10 µM still has 10 time points, so the old check
  passed.

### CX4 One compound per sample with kinact/KI (blocked)

`baseline/proteins_baseline` · `complexes/compounds_decoy_first` (BI-8925 266, DECOY 500) · `mass_ambiguity/config_with_decoy` · **kinact/KI on**

- Red hint: "One compound per sample for kinact/KI (122 samples list
  several)".
- The tooltip lists the first 12 samples, e.g.
  "2026-09-18_MULI+BI-8925_0_0min_R2: BI-8925, DECOY", then "and 110 more",
  and ends with "Screen compound mixtures with kinact/KI switched off."
- Switch kinact/KI off: the table passes (DECOY 500 is far from 266).
- `baseline/compounds_baseline` with `complexes/config_compound_2_only`
  (Compound 1 empty, BI-8925 as Compound 2) counts as one compound and
  passes.

## Automated tests

`tests/testthat/test-edge-complexes.R`:

| Test | Edge cases |
|---|---|
| the complex files are written and load like uploads | – |
| CX1: every compound of a sample is screened, whatever its column | 1, 2 |
| CX2: a compound on a few samples fails the design of its complex | 3 |
| CX3a: two complexes, one per concentration range, are fitted apart | 4 |
| CX3b: a declared complex without hits stays in the picker | 2 |
| CX3c: the time points are counted per complex | 5 |
| CX4: kinact/KI takes one compound per sample | 6, 7, 8 |
| the design rules hold at their boundaries | 9, 10, 11 |
| the untreated control needs no time course | 12 |
| each complex must meet the design on its own | 5 |
| each protein forms complexes of its own | 13 |
| missing concentrations or times are asked for | 14 |
| names are checked against the declared proteins and compounds | 15, 16 |
| without kinact/KI neither the design nor the one-compound rule applies | 8 |
| the complex picker lists hits and declared complexes | 2 |

`test-conversion-unit.R` covers the binding share of a complex in a
multi-compound sample (`complex_hits()`) and the default complex
(`select_complex_kinetics()`).

## What can still go wrong

- **One molecule under two names** (CX3a) gives two kinact/KI values, each
  from part of the concentration range. Nothing tells the app they are the
  same compound.
- **A name with stray spaces** in the Samples table doesn't match the compound
  table and blocks the declaration ("Compound name not declared"). The
  one-compound rule and the replicate checks trim names; the name check
  doesn't. That's safe (it blocks) but the message doesn't say why.
- **The control belongs to one complex.** With several complexes over one
  protein, the 0 µM samples only count for the compound they are declared
  with (CX3a).
