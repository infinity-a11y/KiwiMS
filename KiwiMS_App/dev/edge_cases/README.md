# Edge-case test kit: mass ambiguities and proteoforms

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

## Setup for every test

1. **Conversion:** load the DB. Use **Peak Tolerance 3 Da**, **Max. Stoichiometry 4**
   and **kinact/KI on**, unless a test says otherwise.
2. **Proteins tab:** upload the listed `proteins_*.csv` and confirm.
3. **Compounds tab:** upload the listed `compounds_*.csv` and confirm.
4. **Experiment Configuration dialog:** upload the listed `config_*.csv`.
   Then, in the Samples tab, press **Use Experiment Config** (the wand button).
   This fills Protein, Compound 1/2, concentration (µM) and time (min) for all
   122 samples.

The reference sample is `2026-09-18_MULI+BI-8925_2o5_3min_R1`:
- unbound 21,638 Da;
- second form 21,816 Da;
- complex peak 21,903.5 Da at 17.43 % intensity.

Two masses can claim the same peak once they are **≤ 2 × tolerance** apart,
which is 6 Da at a tolerance of 3.

## T0 Baseline

`proteins_baseline` (21638.84, 21816.84) · `compounds_baseline` (266) · `config_baseline`

- Declaration passes, with no hint.
- Mean Total % binding is 70.25. The reference sample is at 12.04 %.
- Pooled kinact/KI is 227.6 M⁻¹s⁻¹.
- Proteoforms tab:
  - 21,638.84: 250.1.
  - 21,816.84: N/A. The fit doesn't converge; 61 of 122 values are limit
    values, exactly half, so not orange.
- Samples View picker: only `2o5_1min_R1` is under "No Hits" (it was 9 before
  the fix). See T5 for why even that one isn't truly hit-free.

## T1 Case 2: two compounds of one sample give the same peak (blocked)

**T1a:** `compounds_decoy_268` (BI-8925 266, DECOY 268) · `config_with_decoy` (both compounds in every sample)

- The Samples table fails with a one-line red hint: "Compounds of one sample
  are not distinguishable within 2 × peak tolerance (6 Da): BI-8925 ↔ DECOY".
- Hover the info icon at the end of the hint. The tooltip lists the six
  colliding pairs, from "21,638.8 Da + BI-8925 (266.0 Da ×1) ↔ 21,638.8 Da +
  DECOY (268.0 Da ×1) (Δ 2.0 Da)" to ×3 (Δ 6.0 Da), then "Assign them to
  separate samples or revise the mass shifts."
- The table can't be confirmed.

**T1b, stoichiometry:** `compounds_decoy_133` (DECOY 133, and 2 × 133 = 266) · `config_with_decoy`

- Blocked at Max. Stoichiometry 4 (Δ 0.0 Da, DECOY ×2).
- Set Max. Stoichiometry to **1** without touching the table. After about a
  second the hint clears and the table passes.
- Set it back to 2 and it's blocked again.

**T1c, tolerance boundary:** `compounds_decoy_272` (Δ 6 Da) · `config_with_decoy`

- **Tolerance 3:** blocked, because Δ 6.0 is exactly 2 × 3 and still counts.
- **Tolerance 2.9:** passes.
- Switch between the two values. The table is re-checked each time.

**T1d, run-start guard:** same files as T1c.

1. At tolerance 2.9, confirm the Samples table.
2. Raise the tolerance to 3. A confirmed table isn't re-checked.
3. Press **Start**. You should get an error toast "Compounds not
   distinguishable" and a log entry, and the run doesn't start.
4. Set the tolerance to 2.9 and Start works.

**T1e, separate samples:** `compounds_decoy_268` · `config_split_by_rep` (BI-8925 in R1 samples, DECOY in R2)

- Passes with no hint. Compounds in different samples never compete for a peak.
- **Kinetics:** fitted per protein-compound complex. The complex picker under
  the Results Menu lists BI-8925 and DECOY under MLKL; it is active while the
  kinact/KI view is open.
  - **BI-8925** (selected by default): fitted on its 61 R1 samples only,
    kinact/KI **224.8** M⁻¹s⁻¹.
  - **DECOY:** the 268 Da mass picks up part of the BI-8925 complex peak
    (Δ 2.8–3.3 Da), so its k_obs show no saturation and the kinact/KI fit
    fails. The cards show N/A with "fit failed". The k_obs plot shows the
    points without a curve, and the protocol log has the solver message.
  - Switching between the two rebuilds the kinetics view.

## T2 Case 4: a proteoform's unbound mass equals a complex (warning, unbound wins)

`proteins_unbound_is_complex` (third mass 21904.84 = 21638.84 + 266) · `compounds_baseline` · `config_baseline`

- **Declaration:** passes with an orange hint: "Ambiguous masses within 2 ×
  peak tolerance (6 Da): 4 pairs". Its tooltip starts with "21,904.8 Da unbound
  ↔ 21,638.8 Da + BI-8925 (266.0 Da ×1) (Δ 0.0 Da)".
- **Log:**
  - a block "AMBIGUOUS MASS ASSIGNMENTS (within 2 × tol Da)";
  - in each of the 120 samples, a line "⚠ Peak 21903 Da read as unbound
    21,904.8 Da, also fits: 21,638.8 Da + BI-8925 (266.0 Da ×1)".
- **Result:** the complex peak is read as the third proteoform, so BI-8925
  binding of the main form disappears:
  - mean Total % falls to 14.47; the reference sample is at 0 %;
  - no pooled kinact/KI fit.

  That's the rule working as intended. The hint is how you notice.
- **Proteoforms tab:**
  - 21,638.84 and 21,904.84 show **N/A**, with the hover "The fit gave a
    negative parameter";
  - their rows sit at the limit (109/109 and 120/120), so the Δ column is orange.

## T3 Case 4: two unbound masses within 2 × tolerance (warning, no effect)

`proteins_two_unbound` (extra mass 21643.84, 5 Da from the main form) · `compounds_baseline` · `config_baseline`

- Orange hint "Ambiguous masses …"; its tooltip lists "21,638.8 Da unbound ↔
  21,643.8 Da unbound (Δ 5.0 Da)".
- Results are identical to T0. No detected peak lies within 3 Da of 21,643.84:
  the main peak at 21,638 is 5.8 Da away.

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
- **Pooled results:** identical to the control and to T0 (mean Total 70.25,
  kinact/KI 227.6). The split never double counts.
- **Proteoforms tab:**
  - 21,638.84 drops to kinact/KI **149.9**, since half its complex went to the
    other form;
  - 21,654.84 sits at 100 % in every sample (its unbound peak is never
    detected), 120/120 limit values, so its row is orange.
- **Spectrum:** 21,654.84 has no unbound peak. There should be no stray line
  and no "NA Da" label.
- **Compound Distribution donut:** the slices add up to the total.

## T5 Cases 1 and 3: shifts of the same compound on the same form (silent, preferred hit)

**T5a:** `compounds_close_shifts` (BI-8925 266, 264) · `config_baseline`

- No hint. This case is resolved by design, not reported.
- **Compounds table:** 266 and 264 are striped (same row, within 6 Da).
- **Hits, reference sample:**
  - the 21,903.5 Da peak appears twice: 266 as *Preferred* TRUE, 264 as FALSE;
  - its binding is counted once (12.04 %).
- Mean Total 70.30 against 70.25 at T0, from one sample only:
  - in `2o5_1min_R1` the complex sits at **21,900.5 Da**, 4.34 Da from
    21,638.84 + 266, so the 3 Da tolerance misses it;
  - 264 catches it: 6.62 % binding, and the sample leaves "No Hits".

**T5b:** `compounds_half_shift` (BI-8925 266, 133) · `config_baseline`

- 2 × 133 = 266. The hits show 266 ×1 as preferred and 133 ×2 as not.
- Results are identical to T0.

## T6 Multi-compound screening fix

`compounds_decoy_first` (BI-8925 266, DECOY 500) · `config_decoy_first` (Compound 1 = DECOY, Compound 2 = BI-8925)

- Results are identical to T0 (mean Total 70.25).
- With the committed code, **0 of 122** samples got a BI-8925 hit: the
  compound was only screened when its column order matched the compound table.

## T7 Table colouring at 2 × tolerance

`proteins_two_unbound` (Proteins tab) and `compounds_decoy_271` (Compounds tab: 266 and 271, different rows, Δ 5)

- **Tolerance 3 (window 6):**
  - Proteins: 21,638.84 and 21,643.84 are striped (same row);
  - Compounds: 266 and 271 have a pale fill (different rows).
  - The old rule (< 1 × tolerance) coloured neither.
- **Tolerance 2.5 (window 5):** still coloured. The boundary counts.
- **Tolerance 2.4:** the colouring goes.

Table colouring only compares the numbers typed in the table. Collisions
through stoichiometry (T1b) or across proteins and compounds (T2, T4) only
show in the Samples-table hint.
