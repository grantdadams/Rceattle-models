# EBS pollock

Eastern Bering Sea walleye pollock in Rceattle, bridged against the 2024 ADMB
assessment (`pm` / AMAK). Single-sex, single-species; 1964–2024, ages 1–15.

Fleets: one fishery, the BTS bottom-trawl survey, the ATS acoustic-trawl survey,
the AVO acoustic index (shares ATS selectivity), the BTS and ATS age-1 abundance
indices, and the 1965–76 Japanese CPUE (shares fishery selectivity).

**Open `EBS pollock.Rproj` and run the scripts from this folder** — the `Data/`
paths in `2024/` are relative to the project root, not to the script.

## Pipeline

Scripts run in numeric order, chained through files in `Data/`. No master run
script; each header states its own prerequisites.

| Script | Does | Reads | Writes |
|---|---|---|---|
| `2024/00-fit-admb.R` | Builds and runs the ADMB reference | `ADMB/m23_rceattle_full/` | `pm.rep`, `pm.par` |
| `2024/01-build-data.R` | Builds the Rceattle data list | the skeleton workbook + BTS covariance | the bridge workbook |
| `2024/02-bridge.R` | Forward pass: dynamics fixed to the ADMB MLEs | skeleton workbook, `pm.par`, `pm.rep` | console validation |
| `2024/03-model-comparison.R` | Production fit + ADMB comparison. **Carries the reconciliation log.** | bridge workbook, `pm.rep` | console + plots |
| `2024/04-fit-and-diagnostics.R` | Standard diagnostic suite | bridge workbook | console + plots |
| `2024/05-update-data.R` | Rolls the data forward one year | bridge workbook | `EBS_25_...xlsx` |

Off-pipeline (un-numbered, research runs — not part of the assessment sequence):

- `2024/dsem.R` — environment-linked recruitment. **Not an assessment result:** the
  environmental columns are random placeholders and must be replaced with
  cohort-aligned ESP indicators before fitting. Needs `Rceattle@dev-DSEM` + `dsem` 3.0.0.
- `2024/mse.R` — management strategy evaluation under the NPFMC Tier 3 HCR. See the
  TODO in its header: it still uses a flat selectivity start, where the numbered
  scripts use a two-stage start because the fishery selectivity likelihood is multimodal.

`00-` only re-derives the ADMB outputs; those are committed, so the rest of the
pipeline runs without an ADMB toolchain. **It needs `admb` on PATH to do anything** —
without it the script reports that and leaves the committed `pm.rep`/`pm.par`
alone. Note the committed `pm` binary is macOS/arm64, so on Windows a rebuild is
the only route to a runnable executable.

## Reconciliation log

The catalogue of every difference between the ADMB assessment and this Rceattle
configuration lives in **one place**: the header of `2024/03-model-comparison.R`.

- **S1–S4** — structural alignment, applied in ADMB (`ADMB/m23_rceattle/`)
- **L1–L7** — likelihood/data/parameter alignment, applied in ADMB (`ADMB/m23_rceattle_full/`)
- **D1–D8** — Rceattle-side data and configuration conversions (no ADMB edits),
  applied in `2024/01-build-data.R` and baked into the workbook

The other scripts and the ADMB run READMEs cite those codes rather than repeating
the detail. ADMB-side edits are also flagged `MODIFIED (m23_rceattle...)` in `pm.tpl`.

## Data

The workbook is the canonical data source — no `.Rdata` copy of the `data_list` is
written, so the xlsx is what travels with the repo.

| File | Role |
|---|---|
| `Data/EBS_24_pollock_single_species_1964-2024.xlsx` | hand-assembled skeleton; input to `01-`, and what `02-` reads |
| `Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx` | the bridge build; output of `01-`, input to `03/04/05/dsem/mse` |
| `Data/BTS_survey_covariance_2024.dat` | BTS index covariance, injected as `index_cov` by `01-` |
| `Data/2024_ADMB_estimate.xlsx` | ADMB SSB/R for comparison; read only by `RTMB/` scratch scripts |

Naming follows the repo convention `<REGION>_<YY>_<stock>_<config>_<startyr>-<endyr>.xlsx`,
where `YY` is the assessment year and the range is the data years.

**Alias chain** — the bridge workbook has three names in circulation, all the same
build:

1. `EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx` — current, this repo
2. `2024_EBS_pollock_m23_rceattle_full.xlsx` — this repo before the `EBS_24_`
   convention, and Ianelli's earlier report draft
3. `2024_EBS_pollock_canonical_pm.xlsx` — his Rceattle companion report

Catch and the ADMB `obs_catch` are both in **thousand tonnes**; `02-` compares them
unscaled. Index `Log_sd` in the workbook is already a CV / log-sd — do not rescale
it by the observation.

**Use canonical `fleet_control` column names.** `read_data()` auto-upgrades the older
spellings (`Q_index` → `Catchability_index`, `Index_loglike` → `Index_distribution`,
`proj_F_prop` → `Proj_F_proportion`, `Sel_norm_bin1` → `Sel_norm_bin`, …), so
assigning to a deprecated name creates a dead column `fit_mod()` ignores — a
mis-built model rather than an error. `R/0-column_schema.R` in Rceattle is
authoritative.

## ADMB reference

| Directory | Role |
|---|---|
| `ADMB/m23` | the 2024 SAFE assessment (`DoCovBTS = 1`) |
| `ADMB/m23 no covar` | the same with `DoCovBTS = 0` |
| `ADMB/m23_rceattle` | stage 1: structural alignment (S1–S4) |
| `ADMB/m23_rceattle_full` | **stage 2: full alignment (L1–L7) — what the R scripts read** |
| `ADMB/m23_rceattle_nobnd` | staged, never built or run; its own README says the hypothesis was refuted |
| `ADMB/data` | shared `.dat` inputs; `pm.dat` is a starter file with relative paths |

## RTMB

`RTMB/` is a vendored copy of [`jimianelli/rtmb_ebswp`](https://github.com/jimianelli/rtmb_ebswp)
(docs at <https://jimianelli.github.io/rtmb_ebswp/>), the RTMB port of the same
assessment. Not part of this pipeline, not maintained here.

Two deliberate divergences from upstream:

- Three byte-identical duplicate HTML files removed (`index.html` and two
  `reporting/` copies of files already in `docs/`), ~13 MB. `docs/` untouched.
- `RTMB/.pollock_root/admb/runs/{data,for_rtmb}` are git symlinks that do not
  materialize on a Windows checkout, and the `_*.R` scratch scripts hardcode macOS
  paths — `RTMB/` does not run as-checked-out on Windows.

## Known issues

- `mse.R` has never been validated end-to-end against the bridge workbook; see its header TODO.
- `dsem.R` uses placeholder environmental covariates.
- The base fit converges with a **WARN**: the Hessian condition number is high and
  its least-identified direction loads almost entirely on the annual fishery
  selectivity deviations (`sel_coff_dev`).
- The 2D age-by-year AR1 selectivity sensitivity in `03-` settles in its own
  optimum (SSB correlation ~0.97 with the base, terminal ~3% lower) but trips the
  same estimability / non-positive-definite-Hessian checks as the base fit. The
  script attributes that to weak identification from the analytical survey q
  rather than to the 2D AR1 form itself. Ianelli's companion report reaches a
  harder verdict on his own run of this form — parameters at bounds and a FAIL
  convergence status — so the two are not describing the same fit. Research
  option, not a candidate configuration.
