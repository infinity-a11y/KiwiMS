# Tests

```
tests/
  testthat/        the automated tests (testthat)
  edge_cases/      the edge-case kit: test files for the conversion's guards,
                   generated on request, with manual and automated tests
  reference_data/  manifests, parameters and expected results of the two
                   reference datasets, which live outside the repository
```

## Running

From `KiwiMS_App`:

```
# Everything
Rscript -e "rhino::test_r()"

# Without the deconvolution runs (unit, edge-case and digest tests only)
KIWIMS_SKIP_DECON_RUN=1 Rscript -e "rhino::test_r()"

# One file
Rscript -e "testthat::test_file('tests/testthat/test-edge-replicates.R')"
```

In PowerShell, set the variable first: `$env:KIWIMS_SKIP_DECON_RUN = "1"`.

## What runs where

| Layer | Files | Needs | Time |
|---|---|---|---|
| Unit | `test-conversion-unit.R`, `test-deconvolution-unit.R`, `test-ms-formats.R`, `test-user-settings.R` | nothing | < 1 min |
| Edge cases | `test-edge-*.R` ([edge_cases](edge_cases/README.md)) | nothing: they run on the committed MLKL peak list | < 1 min |
| Reference datasets | `test-reference-data.R` ([reference_data](reference_data/README.md)) | the datasets, Python with UniDec | ~1 min |
| Deconvolution pipeline | `test-deconvolution-run.R`, `test-ms-vendor-formats.R` | kinact_MLKL_3, Python with UniDec | ~3.5 min |

Without the reference datasets or without Python the last two layers skip, so
the suite passes on any machine and tests what it can. A dataset that is
present but differs from its manifest fails the tests that need it.

## Test data

Two datasets, both outside the repository and both pinned by an MD5 manifest
of every file: **kinact_MLKL_3** (122 Waters samples, 5.8 GB) and
**thermo_intact** (2 Thermo files, 55 MB). Where to put them, how a copy is
checked and what each test takes from them:
[reference_data/README.md](reference_data/README.md).

Nothing else is read from outside the repository. The files the edge-case kit
uploads in the app are generated (`edge_cases/generate_all.R`) and
git-ignored; the automated tests write their own copy to a temporary folder.

## Environment variables

| Variable | Default | Effect |
|---|---|---|
| `KIWIMS_REFERENCE_DATA` | `E:/KF_Testing/Test-Data` | directory holding the reference datasets |
| `KIWIMS_REFERENCE_REHASH` | unset | `1`: hash every dataset file again instead of trusting a remembered check |
| `KIWIMS_SKIP_DECON_RUN` | unset | `1`: skip every test that deconvolves |
| `KIWIMS_TEST_REFERENCE_FULL` | unset | `1`: also deconvolve all 122 MLKL samples (~4 min) |
| `KIWIMS_TEST_PYTHON` | conda env `kiwims`, then `env_kiwims/` | Python interpreter with UniDec for the deconvolution |
| `KIWIMS_RSCRIPT` | `R-Portable`, then the running R | Rscript the deconvolution subprocess runs on |

`KIWIMS_DECON_WORKERS` is the app's own setting for the number of
deconvolution workers; one test sets it to force the sequential path.

## Writing tests

- Helpers shared by every file are in `testthat/helper-*.R`. The edge-case
  kit's are in `testthat/setup-edge-cases.R` and `edge_cases/_shared.R`.
- A test that needs real data takes it from a reference dataset,
  `waters_samples(n)` or `thermo_samples(n)`. These skip or fail the test as
  described above. Don't read other folders on the test drive.
- A test that only needs peak lists runs on synthetic peaks or on the MLKL
  peak list, not on a deconvolution.
