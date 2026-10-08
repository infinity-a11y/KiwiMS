# Edge-case test kit: mass ambiguities, proteoforms and complexes

Test declarations for the MLKL + BI-8925 run
(`E:\KF_Testing\Results\KiwiMS_2026-09-28_id6556.db`, 122 samples). Each one
triggers one guard. The expected values below were computed with the app's own
functions on that DB. `verify_edge_cases.R` recreates the files and prints them
again:

```
Rscript dev/edge_cases/verify_edge_cases.R dev/edge_cases [db_path]
```

For synthetic data with up to nine proteoforms (the declaration cap), near
misses, unassigned peaks and the plots, see `dev/proteoform_gallery.R`.

## Ambiguity cases

The "Case" in a test title names the kind of mass ambiguity it covers. It is
not a sub-test: each test is complete as written.

| Case | Ambiguity | Tests |
|---|---|---|
| 1 | Two mass shifts of one compound close together on the same form | T5a |
| 2 | Two compounds of one sample give the same peak | T1 |
| 3 | One mass shift is a multiple of another | T5b |
| 4 | An unbound mass collides with a complex or another unbound mass | T2, T3 |
| 5 | A complex shared by two proteoforms | T4 |

T6–T15 test other guards: the multi-compound fix, table colouring, the
complex and replicate checks, and the limits on series, replicates and
samples.

## What changed with the 0.7.5-2 merge

- **Hits and binding are unchanged.** Every Total %, hit row and limit count
  matches the pre-merge values.
- **All kinetics numbers changed.** k_obs is now fitted with a free plateau,
  and kinact/KI comes from one global fit with a bootstrap CI. The T0 kinact/KI
  went from 227.6 to 334.1 M⁻¹s⁻¹ because the model changed, not because of a
  bug. kinact/KI is read from the result's `Ratio`, which also exists when the
  data don't saturate and kinact and KI are N/A.
- **Kinetics run per protein-compound complex.** The new guards in T8–T11 come
  from that: the declaration now checks one compound per sample, the design per
  complex, and replicate consistency. Declared complexes without hits now show
  up in the picker.

## Setup for every test

1. **Conversion:** load the DB. Use **Peak Tolerance 3 Da**, **Max. Stoichiometry 4**
   and **kinact/KI on**, unless a test says otherwise.
2. **Proteins tab:** upload the listed `proteins_*.csv` and confirm.
3. **Compounds tab:** upload the listed `compounds_*.csv` and confirm.
4. **Experiment Configuration dialog:** upload the listed `config_*.csv`.
   Then, in the Samples tab, press **Use Experiment Config** (the wand button).
   This fills Protein, Compound 1/2, concentration (µM) and time (min) for all
   122 samples.

Tests that put two compounds in one sample (T1a–d, T6) need **kinact/KI off**.
With it on, the table is blocked (T10).

The reference sample is `2026-09-18_MULI+BI-8925_2o5_3min_R1`:
- unbound 21,638 Da;
- second form 21,816 Da;
- complex peak 21,903.5 Da at 17.43 % intensity.

Two masses can claim the same peak once they are **≤ 2 × tolerance** apart,
which is 6 Da at a tolerance of 3.

kinact/KI values are given in M⁻¹s⁻¹: set the Unit View to M and s to read
them directly (334.1 M⁻¹s⁻¹ = 0.0200 µM⁻¹min⁻¹).

Total % binding comes as two numbers below:
- **Card:** what the Tot. Binding card of the Compound and Protein View
  shows. It is per compound and averages the compound's hit rows, so a sample
  counts once per proteoform and mass shift carrying the compound, and samples
  without a hit of it are left out.
- **All samples:** the mean over all 122 samples, one value each, 0 % for
  samples without a hit. The app doesn't show it; it's here to compare tests.

## T0 Baseline

`proteins_baseline` (21638.84, 21816.84) · `compounds_baseline` (266) · `config_baseline`

- Declaration passes, with no hint.
- Tot. Binding card (BI-8925): 8.29–100 %, **73.19 ± 23.61**. All samples:
  70.25. The reference sample is at 12.04 %.
- **Kinetics** (complex picker: MLKL › BI-8925 only):
  - kinact/KI **334.1** M⁻¹s⁻¹, 95 % CI 300.7–378.6;
  - status saturated, so kinact and KI are shown;
  - one warning: "Plateaus differ";
  - kinact/KI by replicate series: R1 327.9, R2 341.0.
- **Proteoforms tab:**
  - 21,638.84: 357.6.
  - 21,816.84: 231.2. Before the merge this fit didn't converge. 59 of 120
    values are limit values, under half, so nothing is orange.
  - The tab counts only the fitted concentrations, so the two 0 µM controls
    are left out: 120 samples, not 122.
  - Δ Binding vs Reference: 21,638.84 is the Reference, listed first under
    Pooled.
- Sample View picker: only `2o5_1min_R1` is under "No Hits" (it was 9 before
  the fix). See T5 for why even that one isn't truly hit-free.

## T1 Case 2: two compounds of one sample give the same peak (blocked)

All T1 tests run with **kinact/KI off**, except T1e. With it on, every T1
table is blocked earlier by the one-compound rule (T10).

**T1a:** `proteins_baseline` · `compounds_decoy_268` (BI-8925 266, DECOY 268) · `config_with_decoy` (both compounds in every sample)

**Switch kinact/KI off first.** With it on you get the one-compound-per-sample
hint from T10 instead, and never reach the mass check.

- The Samples table fails with a one-line red hint: "Compounds of one sample
  are not distinguishable within 2 × peak tolerance (6 Da): BI-8925 ↔ DECOY".
- Hover the info icon at the end of the hint. The tooltip lists the six
  colliding pairs, from "21,638.8 Da + BI-8925 (266.0 Da ×1) ↔ 21,638.8 Da +
  DECOY (268.0 Da ×1) (Δ 2.0 Da)" to ×3 (Δ 6.0 Da), then "Assign them to
  separate samples or revise the mass shifts."
- The table can't be confirmed.

**T1b, stoichiometry:** `proteins_baseline` · `compounds_decoy_133` (DECOY 133, and 2 × 133 = 266) · `config_with_decoy`

- Blocked at Max. Stoichiometry 4 (Δ 0.0 Da, DECOY ×2).
- Set Max. Stoichiometry to **1** without touching the table. After about a
  second the hint clears and the table passes.
- Set it back to 2 and it's blocked again.

**T1c, tolerance boundary:** `proteins_baseline` · `compounds_decoy_272` (Δ 6 Da) · `config_with_decoy`

- **Tolerance 3:** blocked, because Δ 6.0 is exactly 2 × 3 and still counts.
- **Tolerance 2.9:** passes.
- Switch between the two values. The table is re-checked each time.

**T1d, run-start guard:** same files as T1c.

1. At tolerance 2.9, confirm the Samples table.
2. Raise the tolerance to 3. A confirmed table isn't re-checked.
3. Press **Start**. You should get an error toast "Compounds not
   distinguishable" and a log entry, and the run doesn't start.
4. Set the tolerance to 2.9 and Start works.

**T1e, separate samples (kinact/KI on):** `proteins_baseline` · `compounds_decoy_268` · `config_split_by_rep` (BI-8925 in R1 samples, DECOY in R2)

- **Declaration:** passes with an orange hint "Replicates declared
  differently: 61 groups". Its tooltip lists lines such as
  "2026-09-18_MULI+BI-8925_10_30min: Compound DECOY ↔ BI-8925". That's
  correct: R1 and R2 of one condition now name different compounds (see T11).
  The compound guard stays silent, since compounds in different samples never
  compete for a peak.
- The R2 samples are only screened for DECOY. The Tot. Binding card is per
  compound (compound picker): BI-8925 12.01–100 %, **73.65 ± 23.11**; DECOY
  12.50–100 %, **73.76 ± 22.56**. The mean over all 122 samples, 13 without a
  hit included, is 64.62; the card doesn't show that.
- **Kinetics:** the complex picker under the Results Menu lists BI-8925 and
  DECOY under MLKL. It is active while the kinact/KI view is open.
  - **BI-8925** (selected by default): fitted on its 61 R1 samples only.
    kinact/KI **327.9** M⁻¹s⁻¹ (CI 281.2–387.5). Warnings: "Plateaus differ",
    "Early 0 % readings".
  - **DECOY:** 61 R2 samples. kinact/KI **298.5** (CI 180.6–541.5), status
    linear, warning "Saturation not reached".
  - **Caveat:** the 268 Da mass picks up the BI-8925 complex peak (Δ 2.8–3.3
    Da), so a compound that isn't there gets a plausible kinact/KI. Before the
    merge this fit failed. Now the only clues are the wide CI and the missing
    saturation. Nothing in the app can catch this, because the masses only
    collide across samples.
  - **kinact/KI by Replicate Series:** no per-series fits for either complex.
    Each complex holds one series only.
  - Switching between the two rebuilds the kinetics view.

## T2 Case 4: a proteoform's unbound mass equals a complex (warning, unbound wins)

`proteins_unbound_is_complex` (third mass 21904.84 = 21638.84 + 266) · `compounds_baseline` · `config_baseline`

- **Declaration:** passes with an orange hint: "Ambiguous masses within 2 ×
  peak tolerance (6 Da): 4 pairs". Its tooltip starts with "21,904.8 Da unbound
  ↔ 21,638.8 Da + BI-8925 (266.0 Da ×1) (Δ 0.0 Da)".
- **Log:**
  - a block "AMBIGUOUS MASS ASSIGNMENTS (within 2 × 3 Da)", then "rule
    applied when a peak matches both", then one branch per pair: "⚠ read as
    unbound" once and "⚠ split evenly" three times, each with Δ 0.0 Da and the
    two readings below it. No line is wider than about 45 characters, and a
    blank line comes before the first "Hit Screening";
  - the three "split evenly" pairs never act in this run: no compound peak
    above 22,100 Da is detected, so none of them is ever split;
  - in each of the 120 samples, a line "⚠ Peak 21903 Da read as unbound
    21,904.8 Da, also fits: 21,638.8 Da + BI-8925 (266.0 Da ×1)".
- **Result:** the complex peak is read as the third proteoform, so BI-8925
  binding of the main form disappears:
  - Tot. Binding card (BI-8925) falls to 4.43–21.33 %, **15.63 ± 4.53**
    (all samples: 14.47); the reference sample is at 0 %;
  - pooled kinact/KI **290.3** (CI 257.5–342.6). It is carried by the
    21,816.84 form alone. Before the merge there was no pooled fit.

  That's the rule working as intended. The hint is how you notice.
- **Proteoforms tab, table:**
  - 21,638.84 and 21,904.84 show **N/A** with the hover "No binding at any
    concentration";
  - their Limit Values (107 / 107 and 119 / 119) are orange, hover "More than
    half of the values sit at a detection limit";
  - 21,816.84: 231.2, as in T0, 59 / 120 limit values, not orange;
  - **21,816.84 is the Reference** and the first row under Pooled. 21,904.84
    carries the most signal (56.7 %, because the complex peak is read as its
    unbound form), but its binding is
    never measured, so it can't be the reference. Before the fix it was, and
    every Δ and the whole Paired Binding plot collapsed onto 0 %;
  - Δ Binding vs Reference is N/A for the other two, hover "No sample where both
    species are measured".
- **k_obs per Proteoform** (and the proteoform overlay in the Kinetics tab's
  k_obs Curve):
  - legend rows "21,638.8 Da · no binding" and "21,904.8 Da · no binding",
    greyed out; their zeros aren't drawn, since they would hide each other at
    k_obs = 0;
  - a click on such a row shows its zero points (hover "No binding at …");
  - 21,816.8 Da is drawn with its points and curve.
- **Paired Binding:**
  - x axis "Binding of 21,816.8 Da (reference) [%]", y axis "Binding of other
    proteoform [%]"; hovering the diagonal explains it (same binding);
  - with Show Limit Values (settings gear) off, there are no points. The plot
    says "No sample where another proteoform and the reference are both
    measured" and points to Show Limit Values; the legend lists "21,638.8 Da ·
    limits only" and "21,904.8 Da · limits only";
  - with it on, both show open circles on the 0 % line. Their points coincide
    sample by sample, so the later one hides the other.

## T3 Case 4: two unbound masses within 2 × tolerance (warning, no effect)

`proteins_two_unbound` (extra mass 21643.84, 5 Da from the main form) · `compounds_baseline` · `config_baseline`

- Orange hint "Ambiguous masses …"; its tooltip lists "21,638.8 Da unbound ↔
  21,643.8 Da unbound (Δ 5.0 Da)".
- Results are identical to T0, kinetics included. No detected peak lies within
  3 Da of 21,643.84: the main peak at 21,638 is 5.8 Da away.

## T4 Case 5: a complex shared by two proteoforms (warning, intensity split)

`proteins_shared_complex` (third mass 21654.84) · `compounds_shift_250` (BI-8925 266, 250) · `config_baseline`

Here 21,654.84 + 250 = 21,638.84 + 266 = 21,904.84 Da. Run `proteins_baseline`
with `compounds_shift_250` as a control: its results are identical to T0.

- **Declaration:** orange hint "Ambiguous masses …"; its tooltip lists
  "21,638.8 Da + BI-8925 (266.0 Da ×1) ↔ 21,654.8 Da + BI-8925 (250.0 Da ×1)
  (Δ 0.0 Da)".
- **Hits table, reference sample:** the 21,903.5 Da peak appears twice, once for
  21,638.84/266 and once for 21,654.84/250, each at **6.02 %** (12.04 / 2).
  Total stays **12.04 %**.
- **Pooled results:** identical to the control and to T0 (all samples 70.25,
  kinact/KI 334.1). The split never double counts.
- **Tot. Binding card** (BI-8925): 8.29–100 %, **72.59 ± 24.24**, slightly
  below T0's 73.19. The value per sample is the same; the shared peak only
  adds hit rows for the third proteoform, and the card weights its samples
  more.
- **Proteoforms tab:**
  - 21,638.84 drops to kinact/KI **210.4**, since half its complex went to the
    other form;
  - 21,654.84 sits at 100 % in every sample (its unbound peak is never
    detected), 119 / 119 limit values (orange). kinact/KI is N/A, hover
    "Binding at 2 concentrations only, at least 3 are needed";
  - **k_obs per Proteoform** and the Kinetics tab's k_obs Curve: 21,654.84
    is listed as "21,654.8 Da · limit values" and starts hidden. Its two k_obs
    (about 23 min⁻¹ = 0.38 s⁻¹) only say that it reads 100 % from the first
    time point; drawn, they stretched the axis about 30-fold and flattened
    every real curve. A click on the legend row shows them;
  - 21,816.84: 231.2.
- **Spectrum:** 21,654.84 has no unbound peak. There should be no stray line
  and no "NA Da" label.
- **Compound Distribution donut:** the slices add up to the total.

## T5 Cases 1 and 3: shifts of the same compound on the same form (silent, preferred hit)

**T5a:** `proteins_baseline` · `compounds_close_shifts` (BI-8925 266, 264) · `config_baseline`

- No hint. This case is resolved by design, not reported.
- **Compounds table:** 266 and 264 are striped (same row, within 6 Da).
- **Hits, reference sample:**
  - the 21,903.5 Da peak appears twice: 266 as *Preferred* TRUE, 264 as FALSE;
  - its binding is counted once (12.04 %).
- Tot. Binding card (BI-8925): 6.62–100 %, **73.05 ± 23.76**. All samples:
  70.30 against 70.25 at T0, changed by one sample only:
  - in `2o5_1min_R1` the complex sits at **21,900.5 Da**, 4.34 Da from
    21,638.84 + 266, so the 3 Da tolerance misses it;
  - 264 catches it: 6.62 % binding, and the sample leaves "No Hits".
- kinact/KI 334.9 (CI 301.6–379.1). The Proteoforms tab gives 358.8 for
  21,638.84.

**T5b:** `proteins_baseline` · `compounds_half_shift` (BI-8925 266, 133) · `config_baseline`

- 2 × 133 = 266. The hits show 266 ×1 as preferred and 133 ×2 as not.
- Results are identical to T0.

## T6 Multi-compound screening fix (kinact/KI off)

`proteins_baseline` · `compounds_decoy_first` (BI-8925 266, DECOY 500) · `config_decoy_first` (Compound 1 = DECOY, Compound 2 = BI-8925)

**Switch kinact/KI off first.** With it on, the table is blocked with "One
compound per sample for kinact/KI (122 samples list several)", as in T10.

- Results are identical to T0: Tot. Binding card (BI-8925) 73.19 ± 23.61,
  all samples 70.25.
- Before the fix, **0 of 122** samples got a BI-8925 hit: the compound was
  only screened when its column order matched the compound table.

## T7 Table colouring at 2 × tolerance

`proteins_two_unbound` (Proteins tab) and `compounds_decoy_271` (Compounds tab: 266 and 271, different rows, Δ 5) · no config: only the Proteins and Compounds tables are checked

- **Tolerance 3 (window 6):**
  - Proteins: 21,638.84 and 21,643.84 are striped (same row);
  - Compounds: 266 and 271 have a pale fill (different rows).
  - The old rule (< 1 × tolerance) coloured neither.
- **Tolerance 2.5 (window 5):** still coloured. The boundary counts.
- **Tolerance 2.4:** the colouring goes.

Table colouring only compares the numbers typed in the table. Collisions
through stoichiometry (T1b) or across proteins and compounds (T2, T4) only
show in the Samples-table hint.

## T8 A compound on a few samples only (blocked)

`proteins_baseline` · `compounds_two_names` (BI-8925 266, BI-8926 266) · `config_mixed_10uM` (BI-8926 on the 10 µM samples at 1, 10 and 15 min and on 20 min R1; BI-8925 everywhere else)

This is the case from the screenshot.
- Red hint: "MLKL + BI-8926: At least 3 different non-zero concentrations
  required (1 present)".
- The design rules (≥ 3 non-zero concentrations, ≥ 3 non-zero time points per
  concentration) now apply to every protein-compound complex. Before, they were
  checked over the whole table, which passed. BI-8926 then went into the run
  with 7 samples at one concentration:
  - with a mass that gives hits, it showed "Too few concentrations, hits or
    time points" in the kinetics view;
  - with a mass that gives none, it was missing from the complex picker, and
    its samples left the kinetics without a trace.
- The 10 µM curve of BI-8925 lost those 7 samples as well.

## T9 Two complexes, one per concentration range

**T9a:** `proteins_baseline` · `compounds_two_names` · `config_split_by_conc` (BI-8926 at 2.5, 5 and 10 µM; BI-8925 at 0, 20, 40 and 80 µM)

- Passes, with no hint.
- Every sample reads as in T0 (all samples 70.25); only the compound names
  differ, and the reference sample's hit now reads BI-8926.
- **Tot. Binding card**, per compound:
  - BI-8925: 9.20–100 %, **83.57 ± 18.22** (the high concentrations);
  - BI-8926: 8.29–92.78 %, **61.97 ± 23.70** (2.5, 5 and 10 µM).
- **Mass Shifts card** (Compound View): BI-8925 [266.0 Da] ×61, BI-8926
  ×59.
- **Complex picker:** BI-8925 and BI-8926 under MLKL. BI-8925 is selected by
  default.
  - **BI-8925:** 62 samples (the 0 µM controls included). kinact/KI **224.3**
    (CI 199.9–251.0), status linear, "Saturation not reached". Series R1
    220.2, R2 228.6.
  - **BI-8926:** 60 samples, kinact/KI **330.4** (CI 306.9–357.2), linear.
  - These are the same molecule. The two values differ because each complex is
    fitted only on its own concentrations.
- The BI-8926 kinetics have no 0 µM baseline in the Binding Curve, since the
  controls are declared with BI-8925.

**T9b, declared complex without hits:** `proteins_baseline` · `compounds_8926_no_hits` (BI-8926 at 500 Da) · `config_split_by_conc`

- Passes. The BI-8926 samples have no hits, so only BI-8925 has a Tot.
  Binding card: 9.20–100 %, **83.57 ± 18.22**, as in T9a. All samples fall
  to 41.48.
- The complex picker still lists **BI-8926**. Pick it: the kinetics view shows
  the "no kinetics" card with "No hits of BI-8926 in its samples".
- The log has, under "MLKL + BI-8926 (60 samples)": "⚠ No hits of BI-8926 in
  its samples. Skipping binding kinetics analysis."
- BI-8925 is unchanged from T9a (224.3).

**T9c, time points per complex:** `proteins_baseline` · `compounds_two_names` · `config_split_short_times` (as T9a, but BI-8926 has 10 µM only at 1 and 3 min)

- Red hint: "MLKL + BI-8926: At least 3 different non-zero time points
  required per concentration (concentration 10 has only 2)".
- Over the whole table, 10 µM still has 10 time points, so the old check
  passed.

## T10 One compound per sample with kinact/KI (blocked)

`proteins_baseline` · `compounds_decoy_first` (BI-8925 266, DECOY 500) · `config_with_decoy` · **kinact/KI on**

- Red hint: "One compound per sample for kinact/KI (122 samples list
  several)".
- The tooltip lists the first 12 samples, e.g.
  "2026-09-18_MULI+BI-8925_0_0min_R2: BI-8925, DECOY", then "and 110 more".
  The note ends with
  "Screen compound mixtures with kinact/KI switched off."
- The rule exists because the Concentration column holds one value per
  sample, and a second compound competing for the site changes k_obs.
- Switch kinact/KI off: the table passes (DECOY 500 is far from 266).
- A table that only uses Compound 2 (Compound 1 empty) counts as one compound
  and passes.

## T11 Replicates declared differently (warning)

`proteins_baseline` · `compounds_baseline` · `config_rep_mismatch` (R2 of 10 µM / 20 min declared at 25 min)

- Orange hint: "Replicates declared differently: 1 group". The tooltip reads
  "2026-09-18_MULI+BI-8925_10_20min: Time 20 ↔ 25", then a note explaining
  that each sample is still analysed as declared.
- The table can be confirmed. The run shows what a single typo does:
  - kinact/KI 335.5 against 334.1, series R2 343.8 against 341.0;
  - the 21,816.84 proteoform drops from 231.2 (saturated) to 195.6 (linear).
- **T11b:** also load `proteins_two_unbound`. Both warnings share one hint:
  "Ambiguous masses … : 5 pairs · Replicates declared differently: 1 group".
  The tooltip lists the mass pairs first, then the replicate group.

## T12 Five replicate series (blocked)

`proteins_baseline` · `compounds_baseline` · `config_five_series` (Replicate R1, R2, R3, R4, R5 in turn)

- **Config upload:** refused, with "'Replicate': at most 4 replicate series
  (5 found: R1, R2, R3, R4, R5)."
- The Samples table would be blocked as well: "At most 4 replicate series (5
  present)", tooltip "R1: 25 samples", … The script checks this; in the app
  the config never gets that far.

## T13 Too many replicates of one condition (blocked)

`proteins_baseline` · `compounds_baseline` · `config_six_replicates` (the 10 µM samples at 30 and 40 min declared at 20 min)

- Red hint: "At most 4 replicates per condition (1 condition has up to 6)".
- Tooltip: "MLKL + BI-8925, concentration 10, time 20: 6 samples", and the
  note "Replicates are samples with the same protein, compounds, concentration
  and time. Check for a mistyped concentration or time."
- The two 0 µM controls are exempt; a design with more controls passes.

## T14 Replicate value contradicts the file name (warning)

`proteins_baseline` · `compounds_baseline` · `config_rep_swapped` (Replicate R1 and R2 swapped for the 20 samples at 10 µM)

- Orange hint: "Replicate differs from the file name: 20 samples". Tooltip
  lines such as "2026-09-18_MULI+BI-8925_10_20min_R1: Replicate R2", then
  "The Replicate value from the config is used."
- The Replicate column shows the config values (R2 for those `_R1` files).
- **Kinetics:** kinact/KI 334.1, as in T0, because every sample still enters
  the global fit on its own. Only the per-series fits change: R1 325.4, R2
  343.6 (T0: 327.9 and 341.0).

## T15 Sample cap (blocked)

`config_385_samples` (385 made-up sample names)

- **Config upload:** refused, with "At most 384 samples per config (385
  rows)."
- **Samples table:** with more than 384 samples the table is blocked with "At
  most 384 samples per run (n present)". This DB has 122 samples, so only the
  script checks it.
- **Deconvolution:** point the deconvolution at more than 384 `.raw` files, or
  at fewer but with an analysis name whose database already holds samples.
  The start dialog shows a red line "At most 384 samples per analysis
  database. This run would hold …", and **Continue** is disabled. Deselect
  files in the picker until at most 384 remain: the red line goes and
  Continue is enabled again. Samples already in the database count once, also
  when they are queried again.

## How replicates and series work

### What they mean in the experiment

- **Condition:** one combination of protein, compound(s), concentration and
  time, e.g. MLKL + BI-8925 at 10 µM for 20 min.
- **Replicate:** one sample of a condition. Measuring a condition twice gives
  two replicates. In this kit, `…_10_20min_R1` and `…_10_20min_R2` are the two
  replicates of the 10 µM / 20 min condition.
- **Replicate series:** all samples with the same replicate number, i.e. one
  complete repeat of the whole experiment. Series R1 holds the R1 sample of
  every condition, series R2 the R2 samples. A series is what a repeat on
  another day, plate or stock dilution produces.

So the two words look at the same samples from two sides: replicates are
counted per condition, series across the whole experiment. With one sample per
condition and series, a condition has as many replicates as there are series.

### How KiwiMS uses them

- **The fits use every sample on its own.** Replicates are never averaged or
  merged before fitting. Means ± SD per time point in the Binding Curve are
  for display only.
- **The series** drive two things only:
  - the per-series fits, "kinact/Ki by Replicate Series" in the Fit tab: the
    whole global fit repeated on each series alone. Agreement between series
    means a repeat of the experiment gives the same answer;
  - the marker fill in the Fit plots: R1 filled, R2 open, R3 dotted, R4 open
    with dot.
- **The replicate groups** (samples named alike up to `_R<n>`) drive the
  "Replicates declared differently" warning (T11).

### How they are declared

The **Replicate** column of the Samples table always holds the series:

| Source | Example | Replicate column |
|---|---|---|
| Config, Replicate column | `R2`, `Rep2`, `Day2` | the value as given |
| No config value, file name ends in `_R<n>` | `…_10_20min_R2.raw` | `R2` (`_R02` → `R2`) |
| Neither | `Plate_A1.raw` | empty: no series, no per-series fit |

A config value wins over the file name; a contradicting number (config `R2`
for a file ending in `_R1`) raises the T14 warning. Series are ordered
naturally (R2 before R10).

Before this change the column showed the condition name (`…_10_20min`) for
`_R<n>` files and a meaningless counter (R1, R2, R3, … one per sample) for
other names. Tables saved with the old condition names still work: such a
value is read as missing and the `_R<n>` ending names the series.

### Limits

| | Cap | Checked |
|---|---|---|
| Replicate series | 4 | config upload, Samples table (kinact/KI on) |
| Replicates per condition | 4 | Samples table; untreated controls (concentration 0) exempt |
| Samples per run | 384 | config upload, Samples table, deconvolution start |

The series cap matches the four marker fills of the Fit plots. The sample cap
is one 384-well plate, the largest format the config's Well column accepts. The
values are `run_limits` in `app/logic/conversion_constants.R`.

### What can still go wrong

- **Re-injections declared as replicates:** if `_R1` and `_R2` are two
  injections of the same incubation, the fit counts them as independent and
  the standard errors and confidence intervals come out too narrow. The data
  can't reveal this, so the app explains it where the user decides or reads
  the result:
  - **Samples Declaration help** ("?" next to Browse in the Samples tab):
    what counts as a replicate (a separate incubation), then how to declare
    the samples for each design: separate incubations with or without a
    repeat of the experiment, aliquots taken from one reaction over time, and
    the same incubation injected more than once (include one injection only);
  - **kinact/KI help** (Kinetics tab, kinact/KI card): the ± and the 95 % CI
    assume separate incubations;
  - **kinact/Ki by Replicate Series help** (Fit tab): series made of
    re-injections agree by construction.
- **Other naming:** only `_R<n>` at the end of a name is recognised; `-R1`,
  `_rep1` or `_1` leave the Replicate column empty unless a config fills it.
- **Unequal repeats:** a series spanning fewer than 3 concentrations gets no
  per-series fit and is left out of the Fit tab's series plot.
- **A replicate group split across complexes** (T1e, or the 20 min pair in T8)
  corrupts no number. Each complex just loses part of a time course or a whole
  series. That's why the replicate checks warn instead of blocking.
