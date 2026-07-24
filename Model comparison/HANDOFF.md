# HANDOFF — running the RCEATTLE estimation model on another computer

This folder adds **RCEATTLE** as an estimation model (EM) to the ASSAMC
operating-model / estimation-model comparison (the FIMS "model comparison
manuscript 2.0"). It mirrors `run_fims()` / `run_wham()`: it reads an ASSAMC
operating-model (OM) replicate, fits three recruitment scenarios, and writes the
same set of per-scenario result files the manuscript's post-processing consumes.

This document is everything a fresh machine needs to reproduce it. See
`README.md` for the design/contract details; this file is the operational
run-book.

---

## 1. What's in this folder

| File | Role |
|---|---|
| `run_rceattle.R` | The EM runner `run_rceattle(maindir, subdir, om_sim_num, casedir, em_bias_cor)` + helpers (`fit_rceattle_scenario`, `seed_rceattle_inits`, `rceattle_estimates`, `save_rceattle_outputs`, `count_na_standard_errors`, `get_condition_number`). Written to drop into ASSAMC's `R/` unchanged. |
| `om_to_rceattle.R` | Translates an OM replicate (`om_input`, `om_output`, `em_input`) into an Rceattle single-species `data_list`. Analogue of FIMS' `prepare_data_fims()`. |
| `demo_run_rceattle.R` | Local smoke-test driver — runs the EM against one bundled OM case and self-tests recovery vs OM truth. **Not** part of ASSAMC. |
| `run_em_patch.R` | The one dispatch line to add to ASSAMC's `R/run_em.R`. |
| `README.md` | Contract, scenario table, modelling-choice notes, coordination items. |

---

## 2. Prerequisites

### 2a. R + a C++ toolchain (for TMB)

RCEATTLE is a TMB/C++ model, so installing it **compiles C++** — the machine
needs a working compiler toolchain:

- **macOS:** Xcode command-line tools (`xcode-select --install`) + gfortran.
  Use the *system* toolchain, not Homebrew clang/gfortran, or the TMB build can
  break. Every R command below is prefixed with `export PATH=/usr/bin:$PATH` for
  exactly this reason.
- **Linux:** `build-essential` + `gfortran` (usually already present).
- **Windows:** install **Rtools** matching your R version.

R ≥ 4.2 recommended.

### 2b. R packages

```r
install.packages(c("remotes", "TMB", "dplyr", "foreach", "doParallel"))
# TMBhelper (RCEATTLE optimiser/convergence helper), from GitHub:
remotes::install_github("kaskr/TMB_contrib_R/TMBhelper")
```

`parallel`, `stats`, `utils` are base R. **FIMS is NOT required** to run
RCEATTLE (it only appears in comparison notes).

### 2c. RCEATTLE (the package under test)

Install the **released / main-branch** API — these scripts deliberately avoid
dev-branch-only functions (`build_data()` / `data_requirements()`), so any
recent release works:

```r
remotes::install_github("grantdadams/Rceattle")   # default (main) branch
```

Confirm it loaded and matches the main-branch surface (no `build_data`):

```r
library(Rceattle)
exists("fit_mod"); exists("build_srr"); exists("clean_data"); exists("switch_check")
exists("build_data", where = asNamespace("Rceattle"))   # should be FALSE
```

> If you are developing an Rceattle branch on the same machine, install the
> released package into the default library and load it with `library(Rceattle)`
> — do **not** `pkgload::load_all()` your dev checkout for these runs, or you may
> pick up unreleased behaviour.

---

## 3. Get the ASSAMC repo + operating-model data

The EM reads `casedir/output/OM/OM{i}.RData`. You need those OM files, either
pre-generated (bundled in the repo) or produced by ASSAMC's `run_om`.

```bash
git clone https://github.com/NOAA-FIMS/Age_Structured_Stock_Assessment_Model_Comparison.git
cd Age_Structured_Stock_Assessment_Model_Comparison
git checkout update-ems
```

**Bundled OM cases** (enough to smoke-test, no OM run needed) live under
`FIMS_integration_test_data/`:

- `FIMS_C1`, `FIMS_C2` — `logR_sd = 0.4`, **stochastic** recruitment. All three
  scenarios (including the random-effects ones) are well-posed. Use these.
- `FIMS_C0`, `C0noPhiF` — `logR_sd = 0`, **deterministic**. The random-effects
  scenarios are ill-posed (true σ_R = 0) and may time out (recorded as
  non-converged). Good only for testing the fixed-effects scenario + the guard.

Each `OM{i}.RData` `load()`s three objects: `om_input` (truth/spec),
`om_output` (true dynamics), `em_input` (observed-with-error data the EM fits).

**To generate fresh OM data** instead, run ASSAMC's `run_om` for your case
(see that repo's `example/`); it writes `OM{i}.RData` into `casedir/output/OM/`.

---

## 4. Quick smoke test (the demo)

1. Open `demo_run_rceattle.R` and set `assamc_dir` to **your** clone path
   (it is currently an absolute path on the original machine):

   ```r
   assamc_dir <- "/path/to/Age_Structured_Stock_Assessment_Model_Comparison"
   case       <- "FIMS_C1"   # stochastic; recommended
   ```

2. Run it:

   ```bash
   export PATH=/usr/bin:$PATH        # macOS system toolchain; omit on Linux/Windows
   Rscript demo_run_rceattle.R
   ```

3. Expected: ~2–3 min wall-clock for one replicate (two ~65 s random-effects
   fits + one ~2 s fixed-effects fit + build/sdreport). It prints a self-test
   like:

   ```
   random_effects   : conv=0  SSB cor=1.000  F cor=0.999  R cor=0.985
   ...
   ```

   `SSB cor ≈ 1.0`, `F cor ≈ 0.999`, `R cor ≈ 0.985` means the translator + fit
   recover OM truth — the end-to-end pipeline is healthy. (A ~3 % SSB level
   offset is expected: the EM uses mean recruitment vs the OM's Beverton–Holt,
   and a numbers-based survey.) Result files are written under
   `casedir/output/RCEATTLE/s1/`.

---

## 5. Running it as the estimation model (production path)

Two ways to invoke `run_rceattle()`:

**A. Standalone** (what the demo does):

```r
source("om_to_rceattle.R"); source("run_rceattle.R")
run_rceattle(
  maindir     = "<unused-but-kept-for-signature-parity>",
  subdir      = "RCEATTLE",
  om_sim_num  = 100,                 # number of OM replicates to fit
  casedir     = "<path>/<case>",     # must contain output/OM/OM{i}.RData
  em_bias_cor = FALSE
)
```

It cleans `casedir/output/RCEATTLE`, creates `s1..s{om_sim_num}`, and fits every
replicate **in parallel** (`detectCores() - 2` workers), three scenarios each.

**B. Inside the ASSAMC driver** — copy `run_rceattle.R` and `om_to_rceattle.R`
into the ASSAMC package's `R/`, then add the dispatch line from
`run_em_patch.R` to `R/run_em.R`, and include `"RCEATTLE"` in `em_names`. See
`run_em_patch.R` for the exact one-liner.

### The three scenarios (per replicate)

| suffix | recruitment deviations | σ_R |
|---|---|---|
| `random_effects` | random effect | estimated |
| `random_effects_sigmaR_constant` | random effect | fixed at OM value |
| `fixed_effects` | penalized fixed effect | fixed |

### Output files (per scenario, in `.../RCEATTLE/s{i}/`)

`fit_rceattle_<scn>.RDS` (tidy estimates: SSB/biomass/recruitment/F + SE),
`full_fit_rceattle_<scn>.RDS` (whole fit object, fallback),
`run_time_rceattle_<scn>.RDS` (seconds), `optimizer_convergence_rceattle_<scn>.RDS`
(0 = converged), `max_gradient_rceattle_<scn>.RDS`, `hessian_rceattle_<scn>.RDS`
(pdHess), `na_count_<scn>.RDS`, `condition_number_<scn>.RDS`.

---

## 6. Resource / timing notes

- A random-effects fit is ~60–75 s; the fixed-effects fit ~2 s. So ~2.5 min of
  compute per replicate, parallelised across replicates.
- `detectCores() - 2` workers; each worker limits TMB to 1 internal thread
  (`TMB::openmp(1)`) so the outer parallelism dominates. On an N-core box, budget
  roughly `ceil(om_sim_num / (N-2)) * ~2.5 min`.
- Memory: each worker holds one fitted model; peak is modest (single-species,
  ~30 yr × ~12 ages). `full_fit_*.RDS` files are the largest outputs.

---

## 7. Gotchas / things that bite

- **Toolchain PATH (macOS):** prefix R commands with `export PATH=/usr/bin:$PATH`
  so the system compiler shadows any Homebrew clang/gfortran. Omit on Linux/Windows.
- **Main-branch API only:** do not depend on `build_data()`/`data_requirements()`
  — they are dev-branch-only. The scripts build the `data_list` via the public
  `clean_data()` + `switch_check()` path instead.
- **Deterministic cases (σ_R = 0):** the random-effects scenarios are ill-posed;
  a `time_limit` guard abandons them gracefully (recorded non-converged) rather
  than hanging. Use stochastic cases (C1/C2) for real comparisons.
- **Survey index units:** the translator prefers `em_input$surveyB.obs` (biomass,
  matching FIMS) and falls back to `survey.obs` (numbers) when the former is
  absent — as it is in the bundled C0/C1 test data. Confirm your production OM
  populates `surveyB.obs`, or RCEATTLE fits a numbers survey while FIMS fits
  biomass (not apples-to-apples).
- **Condition number:** read from RCEATTLE's own convergence check; the code
  deliberately does **not** re-invoke `obj$env$spHess()` on the returned object
  (that can segfault uncatchably for random-effects models).
- **Do not commit generated output** into the ASSAMC clone —
  `casedir/output/RCEATTLE/` is run artefacts.

---

## 8. Verifying a fresh setup end-to-end

1. `library(Rceattle)` loads and `build_data` is absent (§2c).
2. `Rscript demo_run_rceattle.R` on `FIMS_C1` prints `conv=0` and
   `SSB cor ≈ 1.0`, `F cor ≈ 0.999`, `R cor ≈ 0.985` for the fixed and both
   random-effects scenarios.
3. `casedir/output/RCEATTLE/s1/` contains 8 files per scenario (24 total).
4. Max gradients (`max_gradient_rceattle_*.RDS`) are ≲ 1e-6 (with
   `newtonsteps = 1`), well under ASSAMC's 0.1 convergence threshold.

If all four hold, the machine reproduces the reference run.

---

## 9. Not yet done / coordinate with the FIMS–ASSAMC team

- **Downstream glue:** `check_convergence.R` and `read_plot_data.R` in ASSAMC
  have no RCEATTLE branch yet (they key off model-specific files). A branch that
  reads the `*_rceattle_*.RDS` files above is part of the upstream contribution.
- **Estimates-table schema:** reconcile `rceattle_estimates()`'s columns with
  exactly what `read_plot_data.R` expects for a new EM.
- **Reference points:** current runs are hindcast-only with mean recruitment
  (`estimateMode = 1`). MSY / SPR reference points (Beverton–Holt +
  `estimateMode = 0` + `build_hcr()`) are a documented follow-up if the
  manuscript compares reference points.
