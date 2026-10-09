# Reference datasets

The two real acquisitions every test that deconvolves runs on. They are too
large for the repository (kinact_MLKL_3 is 5.8 GB of Waters data that barely
compresses), so they live outside it. What is committed here pins them down:

| File | Content |
|---|---|
| `<dataset>/manifest.tsv` | every file of the dataset with its size and MD5 |
| `<dataset>/params.tsv` | the deconvolution parameters of the expected result |
| `<dataset>/peaks.tsv` | the expected result: every peak of every sample |

| Dataset | What | Used by |
|---|---|---|
| [kinact_MLKL_3](#kinact_mlkl_3) | 122 Waters samples, 5.79 GB | reference peaks, the edge-case kit, every Waters pipeline test |
| [thermo_intact](#thermo_intact) | 2 Thermo files, 55 MB | reference peaks of the Thermo reader |

## kinact_MLKL_3

MLKL + BI-8925 kinact/KI series: 122 Waters samples
`2026-09-18_MULI+BI-8925_<conc>_<time>min_R<n>.raw` (1,229 files), plus the
`config.csv` and `mw.txt` the acquisition came with. The expected peaks (466)
are the deconvolution made by KiwiMS 0.7.5 with UniDec 7.0.3 on 2026-09-28
(`KiwiMS_2026-09-28_id6556.db`), with:

| Parameter | Value |
|---|---|
| Charge | 1–50 |
| m/z | 710–1100 |
| Mass | 20,500–23,500 Da, bins 0.5 Da |
| Peak threshold, window, normalisation | 0.07, 40 Da, 2 |
| Elution window | whole acquisition |

The run logged an elution window of 0.5–1.5 min, but releases before 0.7.5
never applied it (see `test-ms-vendor-formats.R`), so the result covers the
whole acquisition. Deconvolving with the window moves intensities by a few
thousandths (70.878 → 70.884); without it, the result is identical to the
digit, for all 122 samples.

The expected peaks are also what the [edge-case kit](../edge_cases/README.md)
runs on, so the kit's numbers trace back to a deconvolution any machine with
the dataset can repeat.

Charge 1–50 and threshold 0.07 were the app's defaults at the time; the
defaults are now charge 1–100 and threshold 0.05. The expected peaks stay at
the parameters above, which the tests read from `params.tsv`: they pin a
reproducible result, not the defaults. Charge 100 gives the same peaks for
this ~22 kDa protein; threshold 0.05 adds the small peaks between 5 and 7 %
of the tallest (see the [baseline](../edge_cases/baseline/README.md#peak-threshold)).

## thermo_intact

Two Thermo intact-LC-MS files: `KARL_Thermo-file_intactLCMS.raw` (KRAS, peaks
at 19,330 Da and its dimer at 38,650 Da) and `WEEX_Thermo-file.raw` (23 peaks
between 5 and 50 kDa). A copy of `HiDrive-Thermo files intact LC-MS`, renamed
so the dataset has a name without spaces. The expected peaks were made on
2026-10-09 with UniDec 7.0.3 and UniDec's own default parameters (charge
1–50, bins 10 Da, threshold 0.1, window 500 Da, as in `params.tsv`), with a
mass range of 5,000–100,000 Da: these are ten-minute gradients, wider than the
Waters-era defaults assume. Two runs gave identical peaks.

## Where the data goes

Put each dataset in its own folder under one directory and point
`KIWIMS_REFERENCE_DATA` at that directory (default `E:/KF_Testing/Test-Data`):

```
<KIWIMS_REFERENCE_DATA>/
  kinact_MLKL_3/
    2026-09-18_MULI+BI-8925_0_0min_R1.raw/
    ...
    config.csv
    mw.txt
  thermo_intact/
    KARL_Thermo-file_intactLCMS.raw
    WEEX_Thermo-file.raw
```

Set it for R, e.g. in `~/.Renviron`:

```
KIWIMS_REFERENCE_DATA=D:/KiwiMS-reference
```

## How a copy is checked

The digests are taken of the files as they lie on disk, one MD5 per file of
its raw bytes; nothing is compressed or archived. A Waters sample is a
directory, so every file in it (`_FUNC001.DAT`, `_FUNC001.IDX`,
`_HEADER.TXT`, …) has its own manifest row; a Thermo sample is one file. A
copy is identical when it holds exactly the files of the manifest (none
missing, none extra), each of the same size and MD5.

Check a copy before use (a new disk, a download, a colleague's drive):

```
Rscript tests/reference_data/verify.R <dataset> [path] [--rehash]
```

It prints `OK`, `MISSING` or `MISMATCH` with the files that differ, and exits
with 0 only for `OK`. Hashing kinact_MLKL_3 takes about 15 s from a warm disk,
a few minutes from a cold one, so a passed check is remembered (in
`tools::R_user_dir("KiwiMS", "cache")`), keyed by the size and modification
time of every file and by the manifest. It is repeated when one of those
changes, or with `--rehash` (`KIWIMS_REFERENCE_REHASH=1` in the tests).

The tests run the same check through `skip_unless_reference()`:
- **dataset missing:** they skip;
- **dataset differs from the manifest:** they fail and list what differs;
- **dataset identical:** they run.

## Tests

`tests/testthat/test-reference-data.R`:

| Test | Needs | Time |
|---|---|---|
| each manifest and peak list describe the same samples | – | – |
| a dataset that differs from its manifest is reported, not used | – | – |
| kinact_MLKL_3 / thermo_intact matches its manifest | the dataset | 1 s (cached) |
| deconvolution of key MLKL samples reproduces their reference peaks | MLKL + Python | 0.5–1.5 min |
| the edge-case fixture builds from that run and opens in the app | MLKL + Python | 5 s |
| deconvolution of the Thermo files reproduces their reference peaks | Thermo + Python | 0.5 min |
| deconvolution of the whole MLKL series reproduces its reference peaks | MLKL + Python, `KIWIMS_TEST_REFERENCE_FULL=1` | ~4 min |

The key samples are the two the edge-case fixture needs; most of their time
is worker start-up, not UniDec. A failing comparison names the UniDec
version of the run: a different version can move peaks without any KiwiMS
change, so regenerate `peaks.tsv` only once that is understood.

The pipeline tests take their samples from the same datasets, through
`waters_samples(n)` and `thermo_samples(n)` (the first n in name order):

| File | Tests | Data |
|---|---|---|
| `test-deconvolution-run.R` | parallel run, sequential run, re-run extends the database, broken sample | 2–4 MLKL samples |
| | long sample name | an MLKL sample copied to the name that reproduced the bug |
| `test-ms-vendor-formats.R` | elution window, blank elution bound | 1 MLKL sample |

They check that the pipeline completes, not what it finds.

## Adding or replacing a dataset

The infrastructure is meant to stay at these two; add one only for a format
or an instrument neither covers.

1. Put it under `KIWIMS_REFERENCE_DATA/<dataset>` (a name without spaces).
2. `Rscript tests/reference_data/make_manifest.R <dataset>` writes
   `<dataset>/manifest.tsv`.
3. Write `<dataset>/params.tsv` and `<dataset>/peaks.tsv`
   (`ref_write_peaks()` in `reference_data.R` writes the peaks of a result
   without losing digits), and a section above.
4. Commit the three files. Replacing the files of an existing dataset changes
   every expectation built on it, the edge-case kit included.
