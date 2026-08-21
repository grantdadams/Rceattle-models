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
| `2024/02-bridge.R` | Forward pass (dynamics) + likelihood check, both at the ADMB MLEs | both workbooks, `pm.par`, `pm.rep` | console validation |
| `2024/03-model-comparison.R` | Production fit + ADMB comparison. **Carries the reconciliation log.** | bridge workbook, `pm.rep` | console + plots |
| `2024/04-fit-and-diagnostics.R` | Standard diagnostic suite | bridge workbook | console + plots |
| `2024/05-update-data.R` | Rolls the data forward one year; builds before writing | bridge workbook | `EBS_25_...xlsx` |

Off-pipeline (un-numbered, research runs — not part of the assessment sequence):

- `2024/dsem.R` — environment-linked recruitment. **Not an assessment result:** the
  environmental columns are random placeholders and must be replaced with
  cohort-aligned ESP indicators before fitting. Needs `Rceattle@dev-DSEM` + `dsem` 3.0.0.
- `2024/mse.R` — management strategy evaluation under the NPFMC Tier 3 HCR. See the
  TODO in its header: it still uses a flat selectivity start, where the numbered
  scripts use a two-stage start because the fishery selectivity likelihood is multimodal.

### Rolling the data forward

`05-` builds the rolled data list before writing it, so a broken roll fails now
rather than next year mid-fit. What moves:

- `catch_data` already runs to `projyr`, so the new terminal year is **filled**,
  not appended — appending duplicates `(Fleet_code, Year)`.
- `comp_data` appended with `Sample_size = 0`, so a premature fit does not refit
  last year's proportions as though they were observed again.
- `weight` appended, terminal year carried forward. It feeds SSB, so a stale
  terminal row is read straight into the Tier 3 control rule.
- `index_data` appended with a **negative year** — predicted, not fitted. A
  positive-year placeholder would be fitted, and BTS and ATS_1 solve q
  analytically, so 99999 enters the mean and shifts q by +31% across the whole
  1982–2024 series while the model still builds.
- `index_cov$BTS` **unchanged**. The placeholder is not a fitted row, so Sigma
  still matches at 42×42. It only needs growing when the real observation goes
  in, and that is the new VAST matrix, not an extrapolation.

**Filling in the new year:** paste `Observation` and `Log_sd` on each fleet's own
scale, then flip the year positive. For BTS that also requires the new VAST
covariance — the build fails until it is supplied, which is what stops a real
observation being fitted against a stale Sigma.

Placeholders are 99999 (catch, survey observations) and 0 (comp sample size).
**Weight-at-age and the carried-forward covariance are not marked** — nothing in
the workbook distinguishes them from real values, so check them explicitly.

`00-` only re-derives the ADMB outputs; those are committed, so the rest of the
pipeline runs without an ADMB toolchain. **It needs `admb` on PATH to do anything** —
without it the script reports that and leaves the committed `pm.rep`/`pm.par`
alone. Note the committed `pm` binary is macOS/arm64, so on Windows a rebuild is
the only route to a runnable executable.

### Bridge fidelity

`02-` checks two things at ADMB's MLE, and both hold:

- **Dynamics** — N ratio 0.999993–1.000006, SSB mean |%diff| 0.00012%, catch 0.00014%.
- **Likelihood** — every component to ~1e-5: index by fleet (BTS, ATS, ATS_1, AVO,
  CPUE), composition by fleet (fishery, BTS, ATS), and the recruitment and
  initial-age deviation penalties.

Two adjustments make the comparison like for like, and both are in the script:

1. **Age-1.** ADMB's ATS biomass index and AVO exclude age 1 (L4/L5). Rceattle
   does that with `Bin_first_selected = 2`, but `Selectivity = "Fixed"` reads
   `emp_sel` verbatim, so age 1 is zeroed in the injected ATS/AVO selectivity.
   Without it the ATS index runs 3.9% high and its composition reads 393.2
   against ADMB's 30.8.
2. **Normalizing constants.** ADMB reports only the quadratic part of each index
   term; the full negative log-density adds `log(sd) + ½log(2π)` per observation.
   The check adds `n·½log(2π) + Σlog(sd)` back. BTS is exempt — `MVN` in Rceattle
   is deliberately the bare quadratic form so it matches ADMB directly.

The cleaner fix for (2) is to expand the likelihood statements in `pm.tpl` to
full form so no correction is needed on either side. Not yet done — it needs an
ADMB rebuild, and the committed `pm` binary is macOS/arm64. The five statements
to change, the constant each is missing, and how to verify afterwards are in
**`HANDOFF_pm_full_likelihood.md`**.

Not covered: ADMB's `sel_like` (17.48) and `sel_like_dev` (172.07) are
selectivity penalties that the `emp_sel` bypass skips, and `pm.rep` carries no
`catch_like` block.

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
- **Base fit converges WARN**: Hessian condition number 1.2e6, least-identified
  direction loading 100% on `sel_coff_dev`. It costs standard errors, not
  optimum stability — 50 jitters at sd 0.2 (2167 parameters perturbed) return
  the same objective to 1e-8, SSB to 3e-4 %, and `sel_coff_dev` itself to 7e-5.
- **2D age-by-year AR1 selectivity (`03-`) does not converge.** Max gradient
  30.5 on `rec_pars`, `sdreport` failed, 55 `log_F` at bounds, objective 1274.3
  vs the base 713.7. Its ADMB comparison statistics are not a fitted result.
  Ianelli reports 56 `log_F` at bounds and a FAIL, so the dev-line parameter-
  bounds fix did not resolve it. Research option, not a candidate.
- **Retrospective bias grows with forecast horizon.** Mohn's rho, 5
  peels, forecast year 0/1/2/3 — SSB 0.221/0.476/0.709/0.821; biomass
  0.331/0.472/0.696/0.875; recruitment −0.072/0.716/0.858/0.759; F
  +0.074/−0.179/−0.260/−0.189. Quote the horizon with the number. Not yet
  compared against ADMB, so it is unclear how much is the bridge.
- **Composition OSA residuals underdispersed.** SDNR 0.639 fishery, 0.802 BTS,
  0.517 ATS, 0.704 overall (1864 residuals); 6 of 10 groups fail `sdnr_ok`.
  Matches Ianelli's 0.71/0.84/0.70. His zero-dispersion result for the
  analytical-q and MVN index groups does **not** reproduce (AVO 1.015, BTS
  1.199, CPUE 0.519).
- **Age-1 M is not identified** — monotonic to either endpoint, ~2 nll units
  across 0.2–1.3. `04-` profiles age-3+ M instead: minimum at 0.35 against the
  assumed 0.30.
