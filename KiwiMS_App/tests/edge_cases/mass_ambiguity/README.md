# Mass ambiguities

Two readings of the declaration that one peak cannot tell apart. Setup and
conventions are in the [kit README](../README.md); B0 is in
[baseline](../baseline/README.md).

## Implementation

All in `app/logic/conversion_functions.R`.

- **`predict_peaks()`** lists every peak the declaration predicts: each
  declared mass of the protein unbound, and with every mass shift of every
  compound at stoichiometry 1 … Max. Stoichiometry, ordered species, then
  stoichiometry, then mass shift, then compound. Each complex carries the
  declared position of its mass shift (Mass 1, Mass 2 …).
- **`check_hits()`** screens one sample:
  1. each declared mass takes the closest peak within the tolerance as its
     unbound signal. A peak claimed by two masses counts for the first one
     only;
  2. peaks below the lightest predicted mass minus the tolerance are dropped.
     That's the lightest protein mass, unless a negative mass shift makes a
     complex lighter still;
  3. a peak taken as unbound is never also a complex. If it also fits a
     complex of another species, the run logs "Peak … read as unbound …, also
     fits";
  4. every other peak is matched against all predicted complexes within the
     tolerance. Per species and compound, `prefer_hits()` marks exactly one
     match *Preferred*; the others are listed but don't count. The rule is
     the **Preferred Assignment** setting of the conversion sidebar
     (`hit_preference_rules` in `conversion_constants.R`):

     | Rule | Criteria, in turn |
     |---|---|
     | Lowest stoichiometry (default) | stoichiometry, mass error, declared shift order |
     | Closest mass | mass error, stoichiometry, declared shift order |
     | Declared shift order | declared shift order, stoichiometry |

     Mass errors are compared rounded to 1e-6 Da, so floating-point noise
     can't break a tie. No two matches of one compound on one species share
     shift and stoichiometry, and the row order settles anything left, so
     one match always wins. A peak with several matches is logged as
     "Ambiguous assignment at … Da (*rule* preferred)", every match below it
     with the preferred one ticked; the Warnings card counts these.
- **`conversion()`** turns intensities into binding. A peak counts once
  towards the total. A peak claimed by n preferred readings (different
  proteoforms, or different compounds) gives each 1/n of its intensity.
- **`mass_ambiguities()`** finds every pair of predicted peaks **≤ 2 ×
  tolerance** apart (+1e-9 for floating-point sums) and sorts it:

  | Kind | Pair | What happens | Declaration |
  |---|---|---|---|
  | `compounds` | one species, two different compounds | binding can't be attributed | **blocked** |
  | `species` | unbound and a complex, or two unbound | unbound reading wins | warning |
  | `proteoform` | complexes of two different species | intensity split evenly | warning |
  | (none) | one species, one compound, two shifts or stoichiometries | preferred assignment rule | no hint; logged per peak |

- **`declaration_ambiguities()`** runs that once per protein and set of
  compounds of the Samples table, and counts the samples per pair.
- **`check_sample_table()`** blocks on `compounds` pairs and collects the
  others into the orange "Ambiguous masses" hint. It lists at most 2 compound
  pairs in the hint and 12 pairs in the tooltip, then "and n more". It only
  runs this check when it is given the protein and compound tables, the
  tolerance and the maximum stoichiometry.
- **Run-start guard** (`app/view/conversion_sidebar.R`): **Start** runs
  `declaration_ambiguities()` again with the current tolerance and
  stoichiometry, because a confirmed table isn't re-checked. It refuses the
  run on `compounds` pairs, and logs the rest via `log_mass_ambiguities()`.
  It also refuses exact shift multiples (`shift_multiples()`, below) in case a
  compound table reached the run without passing its own check.
- **Exact shift multiples** (`shift_multiples()`, limits in
  `shift_multiple_limits`): `check_table()` refuses a Compounds table where
  one mass shift of a compound is a whole multiple (×1 to ×20) of another
  within 0.01 Da, e.g. 266 = 2 × 133. Max. Stoichiometry and the tolerance
  don't enter, so no later setting change lets one through. The info panel
  says "Fix table issues", the red hint names up to 2 pairs
  (`shift_multiples_message()`). Opposite signs are never multiples; a zero
  shift pairs with another zero only.
- **Table colouring** (`prot_comp_handsontable()`): JavaScript in the
  Proteins and Compounds tables marks numbers within 2 × tolerance of another
  number of the same table: striped in the same row, pale in another row.
  `check_table()` doesn't refuse such masses; the colour and the Samples-table
  hint are the only signals.

## Edge cases

| # | Edge case | Behaviour | Tests |
|---|---|---|---|
| 1 | Two compounds of one sample within the window | blocked | MA1a, *window boundary* |
| 2 | … only through stoichiometry (133 × 2 = 266) | blocked; Max. Stoichiometry 1 lifts it | MA1b |
| 3 | Distance exactly 2 × tolerance | counts as ambiguous | MA1c, *floating-point boundary* |
| 4 | Tolerance or stoichiometry changed after the table was confirmed | Start refuses the run | MA1d |
| 5 | The same compounds in separate samples | not compared; can fake a hit (caveat) | MA1e |
| 6 | An unbound mass equal to a complex of another mass | warning; unbound wins, logged per sample | MA2 |
| 7 | Two unbound masses within the window | warning; one peak counts for the first only | MA3, *peak of two species* |
| 8 | A complex shared by two (or more) proteoforms | warning; split evenly, total unchanged | MA4, *three proteoforms* |
| 9 | Two shifts of one compound close together | no hint; closer shift preferred, first declared one on a tie | MA5a, *default rule* |
| 10 | One shift close to a multiple of another | no hint; lowest stoichiometry preferred | MA5b, *default rule* |
| 11 | Masses typed close together in the Proteins/Compounds table | coloured at ≤ 2 × tolerance | MA6 |
| 12 | A mass shift smaller than the window, on its own species | `species` warning; a lone peak reads as unbound | *own species' window* |
| 13 | A declared mass listed twice | one species, no ambiguity, no double count | *mass listed twice* |
| 14 | Tolerance missing, negative or not a number | no ambiguity reported, no error | *unusable tolerance* |
| 15 | No protein/compound tables passed to the check | mass check skipped | *unusable tolerance* |
| 16 | Many pairs | hint: 2 compound pairs + "and n more"; tooltip: 12 pairs + "and n more" | *long lists* |
| 17 | Samples with the same compounds in another column order, or an undeclared protein | checked once per protein and compound set; undeclared skipped | *once per set* |
| 18 | A negative shift (complex lighter than its protein), also at stoichiometry 2 | screened like any other; peaks below every predicted mass stay out | *negative shift* |
| 19 | Preferred Assignment set to Closest mass or Declared shift order | that rule names the peak; binding unchanged | *rules disagree*, *fall through* |
| 20 | Two matches equally close, their errors differing only by floating-point noise | a tie: the next criterion decides | *floating-point noise* |
| 21 | Any mix of ties, missing errors and unknown rule names | exactly one preferred match per species and compound | *exactly one*, *fall through* |
| 22 | One shift exactly a multiple of another, or listed twice, within 0.01 Da | Compounds table refused; Start refuses too | MA5c, *exact multiples*, *multiples refusal* |
| 23 | Hits where a reading was not preferred | Preferred column shown in the hits table by default | *Preferred column* |

Tests in *italics* are synthetic, in the automated suite only.

## Manual tests

### MA1 Two compounds of one sample give the same peak (blocked)

All MA1 tests except MA1e run with **kinact/KI off**. With it on, every table
is blocked earlier by the one-compound rule (CX4).

**MA1a:** `baseline/proteins_baseline` · `mass_ambiguity/compounds_decoy_268` (BI-8925 266, DECOY 268) · `mass_ambiguity/config_with_decoy` (both compounds in every sample)

- The Samples table fails with a one-line red hint: "Compounds of one sample
  are not distinguishable within 2 × peak tolerance (6 Da): BI-8925 ↔ DECOY".
- Hover the info icon at the end of the hint. The tooltip lists six pairs,
  from "21,638.8 Da + BI-8925 (266.0 Da ×1) ↔ 21,638.8 Da + DECOY (268.0 Da
  ×1) (Δ 2.0 Da)" to ×3 (Δ 6.0 Da), then "Assign them to separate samples or
  revise the mass shifts."
- The table can't be confirmed.

**MA1b, stoichiometry:** `baseline/proteins_baseline` · `mass_ambiguity/compounds_decoy_133` (DECOY 133, and 2 × 133 = 266) · `mass_ambiguity/config_with_decoy`

- Blocked at Max. Stoichiometry 4 (Δ 0.0 Da, DECOY ×2).
- Set Max. Stoichiometry to **1** without touching the table. After about a
  second the hint clears and the table passes.
- Set it back to 2 and it's blocked again.

**MA1c, tolerance boundary:** `baseline/proteins_baseline` · `mass_ambiguity/compounds_decoy_272` (Δ 6 Da) · `mass_ambiguity/config_with_decoy`

- **Tolerance 3:** blocked, because Δ 6.0 is exactly 2 × 3 and still counts.
- **Tolerance 2.9:** passes.
- Switch between the two values. The table is re-checked each time.

**MA1d, run-start guard:** same files as MA1c.

1. At tolerance 2.9, confirm the Samples table.
2. Raise the tolerance to 3. A confirmed table isn't re-checked.
3. Press **Start**. You should get an error toast "Compounds not
   distinguishable" and a log entry, and the run doesn't start.
4. Set the tolerance to 2.9 and Start works.

**MA1e, separate samples (kinact/KI on):** `baseline/proteins_baseline` · `mass_ambiguity/compounds_decoy_268` · `mass_ambiguity/config_split_by_rep` (BI-8925 in R1 samples, DECOY in R2)

- **Declaration:** passes with an orange hint "Replicates declared
  differently: 61 groups". Its tooltip lists lines such as
  "2026-09-18_MULI+BI-8925_10_30min: Compound DECOY ↔ BI-8925". That's
  correct: R1 and R2 of one condition now name different compounds (RP1). The
  compound guard stays silent, since compounds in different samples never
  compete for a peak.
- The R2 samples are only screened for DECOY. Two of them, `80_40min_R2` and
  `80_50min_R2`, are **not measured**: fully converted, no unbound peak left,
  and their complex more than 3 Da off DECOY. Their binding is N/A, not 0 %;
  the log warns "No protein or complex peak found, binding not measurable" and
  the Sample View picker marks them "Not measured".
- Tot. Binding card, per compound (a sample without a hit at 0 %, one not
  measured left out): BI-8925 0–100 %, **69.76 ± 27.61**; DECOY 0–100 %,
  **61.49 ± 34.33**. All samples, 11 without a hit included and the 2 not
  measured left out: 65.69.
- **Kinetics:** the complex picker lists BI-8925 and DECOY under MLKL.
  - **BI-8925** (selected by default): its 61 R1 samples. kinact/KI
    **327.9** M⁻¹s⁻¹ (CI 281.2–387.5). Warnings "Plateaus differ", "Early 0 %
    readings".
  - **DECOY:** 61 R2 samples, the 2 not measured left out of the fit.
    kinact/KI **277.0** (CI 177.7–438.3), status linear, warning "Saturation
    not reached". Counted as 0 %, the two would pull it to 298.5 (CI
    180.6–541.5).
  - **Caveat:** the 268 Da mass picks up the BI-8925 complex peak (Δ 2.8–3.3
    Da), so a compound that isn't there gets a plausible kinact/KI. The only
    clues are the wide CI and the missing saturation. Nothing in the app can
    catch this, because the masses only collide across samples.
  - **kinact/KI by Replicate Series:** no per-series fits; each complex holds
    one series only.
  - Switching between the two rebuilds the kinetics view.

### MA2 A proteoform's unbound mass equals a complex (warning, unbound wins)

`mass_ambiguity/proteins_unbound_is_complex` (third mass 21904.84 = 21638.84 + 266) · `baseline/compounds_baseline` · `baseline/config_baseline`

- **Declaration:** passes with an orange hint "Ambiguous masses within 2 ×
  peak tolerance (6 Da): 4 pairs". The tooltip starts with "21,904.8 Da
  unbound ↔ 21,638.8 Da + BI-8925 (266.0 Da ×1) (Δ 0.0 Da)".
- **Log:**
  - a block "AMBIGUOUS MASS ASSIGNMENTS (within 2 × 3 Da)", "rule applied when
    a peak matches both", then one branch per pair: "⚠ read as unbound" once
    and "⚠ split evenly" three times, each with Δ 0.0 Da and the two readings
    below it. No line is wider than 45 characters, and a blank line comes
    before the first "Hit Screening";
  - the three "split evenly" pairs never act here: no compound peak above
    22,100 Da is detected;
  - in each of 120 samples, "⚠ Peak 21903 Da read as unbound 21,904.8 Da,
    also fits: 21,638.8 Da + BI-8925 (266.0 Da ×1)".
- **Result:** the complex peak is read as the third proteoform, so the
  BI-8925 binding of the main form disappears:
  - Tot. Binding card (BI-8925) falls to 0–21.33 %, **14.47 ± 5.99** (all
    samples: 14.47); the reference sample is at 0 %;
  - pooled kinact/KI **290.3** (CI 257.5–342.6), carried by 21,816.84 alone.

  That's the rule working as intended. The hint is how you notice.
- **Proteoforms tab:**
  - 21,638.84 and 21,904.84 show **N/A**, hover "No binding at any
    concentration"; their Limit Values (107 / 107 and 119 / 119) are orange;
  - 21,816.84: 231.2, as in B0, 59 / 120 limit values, not orange;
  - **21,816.84 is the Reference**. 21,904.84 carries the most signal, but its
    binding is never measured, so it can't be the reference (before the fix it
    was, and every Δ and the Paired Binding plot collapsed onto 0 %);
  - Δ Binding vs Reference is N/A for the other two, hover "No sample where
    both species are measured".
- **k_obs per Proteoform** (and the proteoform overlay of the k_obs Curve):
  "21,638.8 Da · no binding" and "21,904.8 Da · no binding" are greyed out and
  their zeros aren't drawn; a click on such a row shows them. 21,816.8 Da is
  drawn with its points and curve.
- **Paired Binding:** x axis "Binding of 21,816.8 Da (reference) [%]". With
  Show Limit Values off there are no points and the plot says "No sample where
  another proteoform and the reference are both measured"; the legend lists
  both others as "· limits only". With it on, both show open circles on the
  0 % line.

### MA3 Two unbound masses within the window (warning, no effect)

`mass_ambiguity/proteins_two_unbound` (extra mass 21643.84, 5 Da from the main form) · `baseline/compounds_baseline` · `baseline/config_baseline`

- Orange hint "Ambiguous masses …: 5 pairs"; the tooltip starts with
  "21,638.8 Da unbound ↔ 21,643.8 Da unbound (Δ 5.0 Da)".
- Results are identical to B0, kinetics included: no peak lies within 3 Da of
  21,643.84 (the main peak at 21,638 is 5.8 Da away).

### MA4 A complex shared by two proteoforms (warning, intensity split)

`mass_ambiguity/proteins_shared_complex` (third mass 21654.84) · `mass_ambiguity/compounds_shift_250` (BI-8925 266, 250) · `baseline/config_baseline`

Here 21,654.84 + 250 = 21,638.84 + 266 = 21,904.84 Da. As a control, run
`baseline/proteins_baseline` with `compounds_shift_250`: it passes without a
hint and every sample reads as in B0.

- **Declaration:** orange hint "Ambiguous masses …: 1 pair", tooltip
  "21,638.8 Da + BI-8925 (266.0 Da ×1) ↔ 21,654.8 Da + BI-8925 (250.0 Da ×1)
  (Δ 0.0 Da)".
- **Hits, reference sample:** the 21,903.5 Da peak appears twice, for
  21,638.84/266 and 21,654.84/250, each at **6.02 %**. Total stays
  **12.04 %**.
- **Pooled results:** identical to B0 (all samples 70.24, kinact/KI 334.1).
  The split never double counts.
- **Tot. Binding card:** 0–100 %, **70.24 ± 26.86**, as in B0. Every sample
  has the same value; the shared peak only adds hit rows for the third
  proteoform, and the card counts each sample once.
- **Proteoforms tab:**
  - 21,638.84 drops to **210.4**, since half its complex went to the other form;
  - 21,654.84 sits at 100 % in every sample (its unbound peak is never
    detected), 119 / 119 limit values (orange). kinact/KI N/A, hover "Binding
    at 2 concentrations only, at least 3 are needed";
  - in k_obs per Proteoform and the k_obs Curve it is "21,654.8 Da · limit
    values" and starts hidden: its two k_obs (about 0.38 s⁻¹) would stretch the
    axis 30-fold. A click on the legend row shows them;
  - 21,816.84: 231.2.
- **Spectrum** of the reference sample: 21,654.84 has no unbound peak. There
  is no stray line and no "NA Da" label.
- **Compound Distribution donut:** the slices add up to the total.

### MA5 Shifts of one compound on one form (no hint, preferred assignment)

**MA5a:** `baseline/proteins_baseline` · `mass_ambiguity/compounds_close_shifts` (BI-8925 266, 264) · `baseline/config_baseline`

- No hint above the Samples table: the Preferred Assignment rule resolves it.
- **Compounds table:** 266 and 264 are striped (same row, within 6 Da).
- **Hits, reference sample:** the 21,903.5 Da peak appears twice. With the
  default rule both are ×1, so the closer one wins: 264 (0.66 Da off) is
  *Preferred* TRUE and 266 (1.34 Da off) FALSE. Its binding counts once
  (12.04 %). The conversion log shows "Ambiguous assignment at 21903.50 Da
  (lowest stoichiometry preferred)".
- One sample changes. In `2o5_1min_R1` the complex sits at **21,900.5 Da**,
  missed by 266 (4.34 Da) but caught by 264: 6.62 % binding. It loses its
  "No hits" mark; only `0_0min_R1` keeps it. Open its spectrum to see the peak.
- Tot. Binding card: 0–100 %, **70.30 ± 26.73**. All samples: 70.30.
- kinact/KI 334.9 (CI 301.6–379.1). Proteoforms tab: 358.8 for 21,638.84.

**MA5b:** `baseline/proteins_baseline` · `mass_ambiguity/compounds_near_half_shift` (BI-8925 266, 133.5) · `baseline/config_baseline`

- 2 × 133.5 = 267, 1 Da from 266: not an exact multiple, the table passes.
  The hits show 266 ×1 as preferred and 133.5 ×2 as not; the hits table
  shows the Preferred column by default.
- Results are identical to B0.

**MA5c:** `mass_ambiguity/compounds_half_shift` (BI-8925 266, 133), Compounds tab

- 2 × 133 = 266 exactly. The info panel reads "Fix table issues", the red
  hint "Mass shifts that are multiples of each other: BI-8925 (266 = 2 ×
  133)", and the table can't be saved, at any tolerance.

### MA6 Table colouring at 2 × tolerance

`mass_ambiguity/proteins_two_unbound` (Proteins tab) and `mass_ambiguity/compounds_decoy_271` (Compounds tab: 266 and 271, different rows, Δ 5). No config: only the two tables are checked.

- **Tolerance 3 (window 6):** Proteins: 21,638.84 and 21,643.84 are striped
  (same row). Compounds: 266 and 271 have a pale fill (different rows).
- **Tolerance 2.5 (window 5):** still coloured. The boundary counts.
- **Tolerance 2.4:** the colouring goes.

The colouring only compares the numbers typed in one table. Collisions through
stoichiometry (MA1b) or across proteins and compounds (MA2, MA4) only show in
the Samples-table hint.

## Automated tests

`tests/testthat/test-edge-mass-ambiguity.R`. The MA tests run the files above
on the MLKL series; the synthetic ones pin the rules on made-up peak lists.

| Test | Edge cases |
|---|---|
| the mass ambiguity files are written and load like uploads | – |
| MA1a: compounds of one sample 2 Da apart are refused | 1 |
| MA1b: a compound at half the mass collides through stoichiometry | 2 |
| MA1c: the window includes twice the tolerance | 3 |
| MA1d: the run is refused when the tolerance grew after the check | 4 |
| MA1e: compounds in separate samples are not checked against each other | 5 |
| MA2: an unbound mass equal to a complex is read as unbound | 6, log block |
| MA3: two unbound masses 5 Da apart warn but change nothing | 7 |
| MA4: a complex shared by two proteoforms is split between them | 8 |
| MA5a: two close shifts of one compound resolve to the closer one | 9 |
| MA5b: a shift close to twice another prefers the lower stoichiometry | 10 |
| MA6: the tables colour masses within the declaration check's window | 11 |
| the window holds its boundary against floating-point sums | 3 |
| an unusable tolerance finds no ambiguity instead of failing | 14, 15 |
| a declared mass listed twice is one species | 13 |
| a mass shift inside its own species' window reads the peak as unbound | 12 |
| a peak within the tolerance of two species counts once | 7 |
| a complex shared by three proteoforms is split in thirds | 8 |
| the default prefers the lowest stoichiometry, then the closest mass, then the first shift | 9, 10 |
| each rule picks its own reading where the criteria disagree | 19 |
| floating-point noise in the mass sums does not break a tie | 20 |
| the rules fall through their criteria in order | 19, 21 |
| every rule prefers exactly one reading per species and compound | 21 |
| an ambiguous peak is logged with its rule and the preferred reading | 9, 8 |
| ambiguities are checked once per protein and compound set | 17 |
| long ambiguity lists are cut in the hint and its tooltip | 16 |
| a negative shift is screened below the lightest species | 18 |
| MA5c: a shift exactly twice another is refused | 22 |
| exact multiples among the shifts of one compound are found | 22 |
| the multiples refusal lists two pairs and folds the rest | 22 |
| the Preferred column is shown when a reading was not preferred | 23 |

`test-conversion-unit.R` covers the same rules at unit level (the
`mass_ambiguities()` kinds, the split between two compounds and between two
proteoforms).

MA6 can only check what R hands the browser (the tolerance and the 2 ×
tolerance window in the renderer); the colours themselves need the manual
test.

## What can still go wrong

- **Collisions across samples** (MA1e) are never checked, since no single
  sample is ambiguous. A compound declared in samples that don't contain it
  can pick up another compound's complex and get a plausible kinact/KI.
- **The unbound reading always wins** (MA2). If a declared proteoform happens
  to sit at a complex mass, binding of the main form vanishes. Only the hint
  and the per-sample log line say so.
- **A mass loss smaller than the window** reads its peak as unbound (edge
  case 12): a shift of −2 Da at a tolerance of 3 can't be told from the
  protein itself.
- **The colouring and the hint don't agree on everything:** the colouring
  compares the numbers inside one table, the hint compares predicted peaks.
  A pair can be coloured without a hint (two masses of different compounds
  never declared in one sample) and hinted without colour (MA2, MA4).
