# Baseline

The plain declaration of the MLKL + BI-8925 series: one protein with its two
forms, one compound, one complex. Every other category compares against it.
Setup and conventions are in the [kit README](../README.md).

| File | Content |
|---|---|
| `proteins_baseline` | MLKL 21638.84, 21816.84 |
| `compounds_baseline` | BI-8925 266 |
| `config_baseline` | every sample with BI-8925, concentration, time and Replicate as named |

## B0 Baseline

`baseline/proteins_baseline` · `baseline/compounds_baseline` · `baseline/config_baseline`

- **Declaration** passes, with no hint.
- **Tot. Binding card** (BI-8925): 8.29–100 %, **73.19 ± 23.61**, 120 samples.
  All samples: 70.24. The reference sample is at 12.04 %.
- **Sample View picker**, "No Hits": two samples.
  - `0_0min_R1`, the R1 control. Its third peak sits at 21,895.5 Da, 9.3 Da
    below the complex (21,904.84 Da).
  - `2o5_1min_R1`: its complex sits at 21,900.5 Da, 4.34 Da from
    21,638.84 + 266, so the 3 Da tolerance misses it (see MA5a). The old
    README listed only this sample.
- **The R2 control reads binding:** `0_0min_R2` reads **9.20 %** although no
  compound was added. See [Background under the complex peak](#background-under-the-complex-peak).
- **Kinetics** (complex picker: MLKL › BI-8925 only):
  - kinact/KI **334.1** M⁻¹s⁻¹, 95 % CI 300.7–378.6;
  - status saturated, so kinact and KI are shown;
  - one warning: "Plateaus differ";
  - kinact/KI by replicate series: R1 327.9, R2 341.0.
- **Proteoforms tab:**
  - 21,638.84: 357.6, the Reference, listed first under Pooled;
  - 21,816.84: 231.2. 59 of 120 values are limit values, under half, so
    nothing is orange;
  - the tab counts only the fitted concentrations, so the two 0 µM controls
    are left out: 120 samples, not 122.

Why the +178 form behaves the way it does: UniDec's `peakthresh = 0.07` drops
its small apo and complex peaks below the 7 % floor. Its occupancy then snaps
to 0 % early and 100 % late, hence the many limit values.

## Background under the complex peak

Both untreated controls show a third peak 6–9 % high just below the complex
mass (21,904.84 Da): 21,895.5 Da in R1, 21,903 Da in R2. What it is, the data
can't say: the +258 Da (phosphogluconoylated) form of MLKL, the companion of
the +178 Da form, would sit at 21,896.8 Da; carry-over of the complex from an
earlier injection would sit at the complex mass.

The deconvolution can't keep it apart from the complex. Every one of the 122
samples has exactly one peak between 21,850 and 21,960 Da, and narrowing the
peak window from 40 to 10 or 5 Da doesn't change that: the deconvolved
spectrum has a single maximum there. (The narrow windows do reveal +35 Da
adduct peaks of both forms, 5–12 % high, which the 40 Da window absorbs.) So
the "complex" peak always carries the background as well:

- **Untreated and early samples:** the merged peak sits wherever the larger
  share pulls it. In R2 (21,903 Da) it falls within the 3 Da tolerance and
  reads 9.20 % binding; in R1 (21,895.5 Da) and in `2o5_1min_R1` (21,900.5
  Da) it falls outside and reads 0 %. That's why these two are the samples
  without a hit.
- **The controls don't enter the fits:** kinact/KI is the same with or without
  the R2 control's peak (they only appear in the Binding Curve).
- **The background does:** every treated sample reads the complex plus the
  background. Taking a uniform 7 % off every complex peak lowers kinact/KI
  from 334.1 to 269.4 M⁻¹s⁻¹. That's a rough estimate, not a correction (the
  background shrinks as the protein converts), but it shows the size of the
  effect.

Neither the conversion nor the peak picking can separate it, since no peak
list holds the background as a peak of its own. Two ways out remain: find its
origin (a blank injection between samples shows carry-over), or correct the
binding by what the untreated controls read, which KiwiMS doesn't do today.
The edge cases keep the series as it is, since they test the conversion on
what it is given.

## Automated tests

`tests/testthat/test-edge-baseline.R`:

| Test | Checks |
|---|---|
| the peak list holds the whole MLKL series | 122 samples, 466 peaks, the design read off the names |
| the baseline files are written and load like uploads | every file through the app's readers |
| B0: the baseline declaration passes without a hint | declaration |
| B0: binding of the baseline | card, all samples, reference sample, No Hits, R2 control |
| B0: kinetics of the baseline | kinact/KI, CI, status, warning, series, Proteoforms tab |
