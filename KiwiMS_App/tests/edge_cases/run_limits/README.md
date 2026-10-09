# Run limits

The hard caps of a run. Setup and conventions are in the
[kit README](../README.md).

## Implementation

`run_limits` in `app/logic/conversion_constants.R`:

| Cap | Value | Why | Checked | Tests |
|---|---|---|---|---|
| `max_samples` | 384 | one 384-well plate, the largest format the config's Well column accepts | config upload, Samples table, deconvolution start | here |
| `max_series` | 4 | the four marker fills of the Fit plots | config upload, Samples table (kinact/KI on) | [replicates](../replicates/README.md) |
| `max_replicates` | 4 | replicates per condition | Samples table (0 µM exempt) | [replicates](../replicates/README.md) |

The sample cap is checked in three places:

- **Config upload** (`validate_config()`, `app/logic/helper_functions.R`):
  more than 384 rows refuse the file.
- **Samples table** (`check_sample_table()`): the very first check, before
  the protein and compound declarations, with kinact/KI on or off.
- **Deconvolution start** (`decon_planned_samples()` and
  `decon_sample_cap_message()` in `app/logic/deconvolution_functions.R`,
  called by `app/view/deconvolution_main.R` with the picker's selection and
  the database the analysis name points at): the samples the analysis
  database would hold, the queried ones plus those already done there, each
  counted once. Failed samples don't count. A missing or unreadable database
  holds none. Above 384 the start dialog shows a red line and **Continue** is
  disabled.

## Edge cases

| # | Edge case | Behaviour | Tests |
|---|---|---|---|
| 1 | Config of exactly 384 samples | accepted | LM1 |
| 2 | Config of 385 samples | refused | LM1 |
| 3 | Samples table of 384 / 385 samples | passes / blocked | LM1 |
| 4 | 385 samples, kinact/KI off | blocked | LM1 |
| 5 | 385 samples with nothing else declared | the cap is reported first | LM1 |
| 6 | Deconvolution of 384 / 385 inputs | Continue enabled / disabled | *deconvolution of more than 384* |
| 7 | Deconvolution adding to a database that already holds samples | queried + done; failed ones don't count | *already in the database* |
| 8 | Samples queried again, with or without extension | counted once | *queried again* |
| 9 | Analysis database missing, not a database, or without a status table | holds no samples | *missing or unreadable* |

Tests in *italics* are synthetic. Whether the dialog disables **Continue**
for the message is part of the view and needs the manual test.

## Manual tests

### LM1 Sample cap (blocked)

`run_limits/config_384_samples`, `run_limits/config_385_samples` (made-up
sample names, each a condition of its own, so the cap is the only rule they
can break)

- **Config upload:** 384 is accepted; 385 is refused with "At most 384
  samples per config (385 rows)."
- **Samples table:** with more than 384 samples the table is blocked with "At
  most 384 samples per run (n present)". The series has 122 samples, so only
  the automated test reaches it.
- **Deconvolution:** point the deconvolution at more than 384 inputs, or at
  fewer but with an analysis name whose database already holds samples. The
  start dialog shows a red line "At most 384 samples per analysis database.
  This run would hold …", and **Continue** is disabled. Deselect files in the
  picker until at most 384 remain: the red line goes and Continue is enabled
  again. Samples already in the database count once, also when they are
  queried again.

## Automated tests

`tests/testthat/test-edge-run-limits.R`:

| Test | Edge cases |
|---|---|
| the run limit files are written and load like uploads | – |
| the caps are one 384-well plate and the four marker fills | the values |
| LM1: a config of 384 samples is accepted, 385 are refused | 1, 2 |
| LM1: a Samples table of 384 samples passes, 385 are refused | 3, 4 |
| LM1: the sample cap is checked before anything else | 5 |
| a deconvolution of more than 384 inputs is refused | 6 |
| samples already in the database count towards the cap | 7 |
| a sample queried again counts once | 8 |
| a missing or unreadable database holds no samples | 9 |

## What can still go wrong

- **Several databases of one experiment** are each capped on their own; a run
  split over two databases is converted as two runs.
