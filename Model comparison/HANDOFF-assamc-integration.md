# Integrate Rceattle as an EM in the ASSAMC model-comparison package

**Status:** planned, not started. Written 2026-08-06. Self-contained — a fresh session should be
able to execute this by reading only this file.

---

## Cold start: environment facts a fresh session needs

**Repos**
- ASSAMC (target): `C:\Users\grant.adams\GitHub\FIMS ecosystem\Age_Structured_Stock_Assessment_Model_Comparison`
  — on branch **`update-ems-w-Rceattle`**, which branched off `update-ems` and has **zero commits**
  on it (`git diff update-ems..update-ems-w-Rceattle` is empty). Local-only, no remote tracking branch.
  PR base is `update-ems`.
- Source scripts (this folder): `C:\Users\grant.adams\GitHub\Rceattle ecosystem\Rceattle-models\Model comparison\`
  — `om_to_rceattle.R` (302 L), `run_rceattle.R` (333 L), `demo_run_rceattle.R`,
  `run_em_patch.R` (100% commented out), `README.md`, `HANDOFF.md`.
  The on-disk `HANDOFF.md` is **truncated** (82 of 244 lines; it cross-references a §6 and §8 that
  are no longer in the file). Recover the full version with
  `git show 0f7a527:"Model comparison/HANDOFF.md"` in this repo.
- Rceattle package: `C:\Users\grant.adams\GitHub\Rceattle ecosystem\Rceattle` — checked out on `dev`.

**R toolchain**
- **R is not on PATH.** Use `& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe"` from PowerShell
  (R-4.3.3 is also installed).
- Installed: `Rceattle 5.4.0`, `FIMS 0.8.0`, `foreach 1.5.2`, `doParallel 1.0.17`, `TMB 1.9.21`,
  `dplyr 1.2.1`. **ASSAMC is NOT installed** — work with `devtools::load_all()` on the repo,
  which is what `example/FIMS_comparison.R:98` does.
- The ported scripts avoid dev-only Rceattle API (`build_data()` / `data_requirements()`) on
  purpose. Everything they use — `clean_data`, `switch_check`, `build_params`, `build_srr`,
  `fit_control`, `fit_mod` — is exported on **both** Rceattle `main` (4.8.0) and the installed
  5.4.0, so the port is version-safe either way.

**Test data**
- `FIMS_integration_test_data\{FIMS_C0, FIMS_C1, FIMS_C2, C0noPhiF}\output\OM\OM1.RData` —
  **one replicate each** (~127 KB), so `om_sim_num` is effectively 1 without running `run_om()`.
- `FIMS_C1`: `logR_sd = 0.4`, `nyr = 30`, `nages = 12`; `em_input` has
  `L.obs, survey.obs, L.age.obs, survey.age.obs, n.L, n.survey, survey_q, cv.L, cv.survey` —
  **no `surveyB.obs`**. `FIMS_C0` / `C0noPhiF` have `logR_sd = 0` (deterministic).
- Each `OM<i>.RData` `load()`s three lists into the calling frame: `om_input` (truth/spec),
  `om_output` (true dynamics), `em_input` (observed-with-error — what the EM actually fits).

**Style notes for this codebase**
- `R/` is flat, one file per EM, no subfolders. Heavy use of `<<-` and globals
  (`om_input`, `om_output`, `em_input`, `om_list`, `keep_sim_id`, `keep_sim_num`) — new code must
  follow this or the plot functions break. `setwd()` side effects are everywhere.
- `readRep()` comes from `PBSadmb`, attached by the driver scripts, not the package.

---

## Context

The NOAA **Age Structured Stock Assessment Model Comparison** package (ASSAMC) simulates stock
dynamics with an operating model (OM) and runs a panel of estimation models (EMs) — AMAK, ASAP,
BAM, SS3, MAS, WHAM, FIMS — against the same simulated data, then plots their relative error
against truth.

Working Rceattle versions of the two required pieces already exist, out-of-tree, in this folder.
They were written deliberately to ASSAMC's `run_fims()` contract (same signature, same parallel
`foreach`, same RDS output stems) but have never been moved into the package.

Goal: land Rceattle in ASSAMC as a first-class EM so `ASSAMC::run_em(em_names = "Rceattle")`
and `ASSAMC::generate_plot(em_names = "Rceattle", ...)` work end-to-end, as a PR from
`update-ems-w-Rceattle`.

**Confirmed premise (matches what Bai said):** on this branch the downstream aggregation layer
is already stale — `read_plot_data.R` has no WHAM block at all, and its FIMS block still
`load()`s an `s<n>.RData` `report` object that the current `run_fims.R` no longer writes.
This plan therefore adds Rceattle's aggregation **additively** and does not repair FIMS/WHAM.
Rceattle will be the one EM that runs end-to-end.

### Decisions already made
- EM name string = **`"Rceattle"`** → output dir `output/Rceattle/s{i}/`, legend label `Rceattle`.
- Scope = runner **+** additive `check_convergence.R` / `read_plot_data.R` blocks. FIMS/WHAM untouched.
- Scenario feeding the comparison figures = **`random_effects`** (rec devs as random effects,
  sigmaR estimated), overridable per-case.

### Verified facts the design depends on
- Installed Rceattle is **5.4.0**; Rceattle `main` is 4.8.0. Every function the scripts use
  (`clean_data`, `switch_check`, `build_params`, `build_srr`, `fit_control`, `fit_mod`) is
  exported on **both**, so the port is version-safe. ASSAMC itself is not installed — it is
  used via `devtools::load_all()`.
- Bundled `FIMS_integration_test_data/FIMS_C1/.../OM1.RData`: `logR_sd = 0.4`, `nyr = 30`,
  `nages = 12`, and `em_input` has **only `survey.obs`**, no `surveyB.obs`.
- `check_performance.R:16` resolves each EM's aggregate by name —
  `eval(as.name(tolower(paste(em_names, "_list", sep=""))[i]))` — so the object must be
  `rceattle_list`, assigned with `<<-`.
- `check_convergence.R:19-20` sets `subdir <- em_names[em_id]`, so the output directory name
  must equal the EM name string byte-for-byte.

---

## Blocking issue to fix first: three helper-name collisions

`R/run_fims.R` already defines at package scope:

| Name | `run_fims.R` | ported `run_rceattle.R` |
|---|---|---|
| `cv_2_sd` | line 481 | identical body |
| `count_na_standard_errors` | line 486 (`sdreport`) | different body (`tryCatch`/`suppressWarnings`) |
| `get_condition_number` | line 511, **`(obj, opt, sdreport)` — 3 args** | **`(fit)` — 1 arg** |

`DESCRIPTION` has no `Collate:` field, so R collates alphabetically and `run_rceattle.R` wins.
`run_fims()` would then call a 1-arg function with 3 arguments and error — **a naive copy-paste
port silently breaks FIMS.** Resolution:

- **Drop** the duplicate `cv_2_sd` and reuse the package-scope one (identical body; sharing it
  is the point — it guarantees observation error is specified identically across EMs). Keep
  `"cv_2_sd"` in the `foreach` `.export` vector.
- **Rename** `count_na_standard_errors` → `rceattle_count_na_standard_errors`.
- **Rename** `get_condition_number` → `rceattle_condition_number`.

Second portability defect: `om_to_rceattle.R:286` uses the native pipe `|>`, but `DESCRIPTION`
declares `Depends: R (>= 3.5.0)` — a **parse error at install time** on R < 4.1. Replace with
`dplyr::select(simData$weight, Species, Sex, Year, dplyr::contains("Age"))`.

---

## Implementation

### Step 1 — `R/run_rceattle.R` (new; merge both source scripts into one file)

One file per EM matches the repo (`run_fims.R` is 518 lines and holds `prepare_data_fims()` +
helpers + `run_fims()`). Port `om_to_rceattle.R` + `run_rceattle.R` verbatim, then apply:

1. The three collision fixes and the `|>` fix above.
2. `subdir = "Rceattle"` as the `run_rceattle()` default.
3. **`survey_units` argument** on `om_to_rceattle()` and `run_rceattle()` — see *Latent bug* below.
4. New helper **`rceattle_plot_quantities(fit, om_input)`** — see *Aggregation payload* below.
5. `save_rceattle_outputs(..., om_input = NULL, save_full_fit = FALSE)`: write
   `plot_rceattle_<scn>.RDS` in **both** the NULL and success branches; gate the existing
   `full_fit_rceattle_*.RDS` write on `save_full_fit` (at 160 sims × 3 scenarios the full TMB
   objects are multi-GB, and `obj$env$ADFun` is an `externalptr` that `saveRDS` silently nulls).
6. `.export` vector: add `"rceattle_plot_quantities"`, update the two renamed helpers, keep `"cv_2_sd"`.
7. `match.arg(survey_units)` **before** the `foreach`, so a plain character is exported to workers.
8. Roxygen: `@export` on `run_rceattle()` only; `@keywords internal` **and** `@noRd` on every
   helper, so a future `devtools::document()` neither generates `.Rd` nor tries to export them.

### Step 2 — `R/run_em.R`, one line after the FIMS dispatch (line 24)

```r
  if("Rceattle" %in% em_names) run_rceattle(maindir=maindir, om_sim_num=om_sim_num, casedir=casedir, em_bias_cor=em_bias_cor)
```

`run_em.R:14-16` already `dir.create`s `output/<em_name>` for every name, so `output/Rceattle`
comes for free.

### Step 3 — `R/check_convergence.R`, new `else if` between the FIMS branch (ends line 40) and the ADMB `else` (line 41)

Without this, `subdir == "Rceattle"` falls into the ADMB branch and looks for
`admodel.cov` / `*.par`, which a TMB model never writes. The branch must supply
`positive_hessian` (0/1) and `gradient` per sim, reading the per-scenario RDS the runner
already writes:

- `hessian_rceattle_<scn>.RDS` — **absent** when the fit errored or hit the time-out guard
  (it is gated on `!is.null(fit$sdrep)`), so guard with `file.exists()`; absent **or**
  non-positive-definite ⇒ `positive_hessian = 0`, which is what drops the replicate.
- `max_gradient_rceattle_<scn>.RDS` — `NA` when the fit is NULL.
- Scenario selected via `getOption("ASSAMC.rceattle_scenario", "random_effects")`.

Use absolute paths; do not rely on the `setwd()` at line 20.

### Step 4 — `R/read_plot_data.R`, new block appended after the FIMS block (after line 582)

Build the **19-element `rceattle_list`** in the exact order and shapes of `om_list`
(`read_plot_data.R:61-76`): `biomass, abundance, ssb, recruit, Ftot, landing, survey, msy,
fmsy, ssbmsy, fratio, ssbratio, geomR0, arimR0, geomS0, arimS0, geomDf, arimDf, agecomp`.
Elements 1-7, 11, 12 are `nyr × keep_sim_num`; 8-10 and 13-18 are `1 × keep_sim_num`;
`agecomp` is a list of **nages × nyr** matrices (note the `apply(., 1, .)` transpose).
Index OM replicates with `keep_sim_id[om_sim]`, as every other block does.

Mapping (`rce <- readRDS(plot_rceattle_<scn>.RDS)`):

| Element | Source | Units |
|---|---|---|
| `biomass`, `ssb` | `rce$biomass`, `rce$ssb` | mt |
| `abundance` | `apply(rce$naa, 1, sum)/1000` | **thousands** |
| `recruit` | `rce$naa[,1]/1000` | **thousands** |
| `Ftot` | `rce$Ftot` (max F-at-age) | apical F — the analogue of the OM's `apply(FAA,1,max)` |
| `landing` | `rce$landing` (`catch_hat`) | mt |
| `survey` | `rce$survey` (`index_hat`) | q-scaled numbers, mean ≈ 1 |
| `msy`/`fmsy`/`ssbmsy` | borrowed from `om_msy`/`om_fmsy`/`om_ssbmsy` | see below |
| `fratio`/`ssbratio` | **EM's own** `Ftot`/`ssb` ÷ borrowed reference points | |
| `geomR0` | `rce$R0/1000` | thousands (median: `srr_fun=0` ⇒ `R = R0·exp(rec_dev)`) |
| `arimR0` | `rce$R0 * exp(0.5*rce$sigmaR^2)/1000` | EM's **own** estimated sigmaR |
| `geomS0`/`arimS0` | `R0 * rce$SPR0` (and bias-corrected) | mt; `SPR0` is Rceattle's `Phi.0` |
| `geomDf`/`arimDf` | terminal `ssb / S0` | |
| `agecomp` | `apply(rce$naa/1000, 1, function(x) x/sum(x))` | nages × nyr |

**Reference points are borrowed, not estimated — and the comment must say so loudly.**
Rceattle is fit at `estimateMode = 1` (hindcast, no HCR projection) with
`build_srr(srr_fun = 0)` (mean recruitment). With no stock-recruit relationship, equilibrium
yield is monotone in F and MSY is undefined; there is no honest value to compute. Two
deliberate departures from the FIMS block:
- **Drop the `*1.01` fudge** (`read_plot_data.R:548-550`) — copying the OM value verbatim makes
  the relative-error panel identically 0, which reads unambiguously as "not estimated", instead
  of a fabricated 1% bias.
- **Use EM-derived numerators for `fratio`/`ssbratio`.** The FIMS block sets
  `fims_fratio <- om_Ftot/om_fmsy` — OM on both sides, so its Kobe panel carries no information.
  Using Rceattle's own `Ftot`/`ssb` over the common borrowed denominators makes the status
  panels measure real estimation error. Matches the AMAK/ASAP/MAS structure
  (`read_plot_data.R:144-145, 244-245, 480-481`).

Source `msy`/`fmsy`/`ssbmsy` from the in-scope `om_msy`/`om_fmsy`/`om_ssbmsy` matrices, **not**
a re-`load()` — the FIMS block reads a stale `om_output` left over from the OM loop.

Finish with `rceattle_list <<- rceattle_list` and
`save(rceattle_list, file = file.path(casedir, "output", "rceattle_output.RData"))`.

### Step 5 — `NAMESPACE` + `man/run_rceattle.Rd`

**Do not run `devtools::document()`.** `man/` holds 14 `.Rd` against 34 files in `R/` (no
`run_wham`, `run_asap`, `run_ss`, `run_bam`, `run_amak`, `run_em`, or any `plot_*`); a
`document()` run would create ~20 files and rewrite every header, burying the actual change.
Instead:
- Insert exactly one line in `NAMESPACE`, `export(run_rceattle)`, between `export(run_om)` and
  `export(run_ss)` (alphabetical, so a future `document()` reproduces it verbatim). Follow FIMS,
  which is exported; `run_wham` not being exported is the anomaly.
- Hand-write `man/run_rceattle.Rd` copying the structure of `man/run_fims.Rd`.
- `DESCRIPTION`: optionally add `Suggests: Rceattle, foreach, doParallel, dplyr, TMB`. The
  package declares no dependencies at all today (drivers `library()` everything), so this is
  documentation, not enforcement — flag it, let the maintainer decide.

### Step 6 — Drivers

- **`example/Rceattle_comparison.R`** (new) — modelled on `example/FIMS_comparison.R`; replaces
  the source demo's hardcoded macOS path with `maindir <- file.path(here::here(), "example")`.
  Small case (`om_sim_num = 3`), `run_om()` → `run_em(em_names = "Rceattle")` →
  `generate_plot()`, plus a joint `c("FIMS", "Rceattle")` block and a commented
  `options(ASSAMC.rceattle_scenario = "fixed_effects")` next to a `logR_sd = 0` case.
- **`FIMS_integration_test_data/Rceattle_integration_test.R`** (new) — port of
  `demo_run_rceattle.R`, the fastest end-to-end signal: runs against the bundled
  `FIMS_C1/output/OM/OM1.RData` with `om_sim_num = 1`, so it needs no `run_om()`. Keeps the
  OM-truth correlation table and 3-panel PDF; mirrors the existing `FIMS_integration_test.R`.
  (Expected self-test numbers from the original HANDOFF §6: `SSB cor ≈ 1.000`, `F cor ≈ 0.999`,
  `R cor ≈ 0.985`; ~2–3 min per replicate, RE fit ~60–75 s, FE ~2 s.)

Do not carry `README.md` / `HANDOFF.md` into ASSAMC — fold their load-bearing content (scenario
table, the analytical-q-vs-estimated-q coordination note, the survey-units decision) into the
`run_rceattle()` roxygen block and inline comments.

---

## Two substantive fixes to the ported code

### Latent bug: the `surveyB.obs` auto-preference

`om_to_rceattle.R:85-92` prefers `em_input$surveyB.obs` (biomass) when present, else
`survey.obs` (numbers). The bundled OMs have only `survey.obs`, so it takes the numbers branch
today — but the current `R/OM_ObservationModel.R` **does** return `surveyB.obs`. The moment
anyone regenerates an OM, Rceattle silently switches to the biomass index. The two series
differ in **scale and shape** (`run_om.R:212-218`): `survey.obs` is selectivity-weighted
numbers mean-normalised to 1; `surveyB.obs` is additionally weight-at-age weighted, so it
reweights toward old heavy ages and its trajectory diverges whenever age structure shifts.
`read_plot_data.R:43` compares every EM's survey panel to `om_output$survey_index` — the
*numbers* series — and FIMS/AMAK/ASAP/BAM/SS all fit numbers.

Fix: replace auto-detection with an explicit `survey_units = c("numbers", "biomass")` argument
(`match.arg`), defaulting to `"numbers"`, `stop()`ing with a clear message if `"biomass"` is
requested but `surveyB.obs` is absent. Keep `attr(simData, "survey_units")`. Thread it through
`run_rceattle()`.

### `random_effects` on deterministic cases (C0 / C0noPhiF)

The chosen plot scenario estimates sigmaR, which is ill-posed when the OM's true
`logR_sd == 0` — the Laplace inner problem for a zero-variance random effect is singular. The
runner's 300 s `setTimeLimit` guard returns `NULL` and records non-convergence.

`check_convergence.R:77` has a short-circuit branch that can **retain** such a replicate (a NULL
fit gives `gradient = NA`, and `NA > 0.001` drops out of `which()`, so if every other replicate
is clean the branch fires and ignores `positive_hessian == 0`). So the `read_plot_data` guard is
load-bearing, not defensive padding:

- `if (is.null(rce)) { rceattle_missing <- c(rceattle_missing, keep_sim_id[om_sim]); next }`,
  leaving that column NA across all 19 elements. NA propagates harmlessly —
  `check_performance.R:28` yields NA RE, `plot_msy_re.R:20` already filters on `is.finite`,
  boxplots drop NA.
- Emit **one** aggregated `warning()` after the loop naming the missing sim ids and pointing at
  `options(ASSAMC.rceattle_scenario = "fixed_effects")`.
- Document in `?run_rceattle` that `random_effects` is expected to time out when `logR_sd == 0`.

---

## Aggregation payload: extend the runner, don't reload the full fit

Add `rceattle_plot_quantities(fit, om_input)` writing a **plain named list** (no TMB pointers,
no `Rceattle` class) to `plot_rceattle_<scn>.RDS`, so `read_plot_data()` needs neither Rceattle
installed nor multi-GB deserialization. Leave `fit_rceattle_<scn>.RDS` byte-identical — the demo
parses it by `label`.

Fields: `years, naa` (nyr × nages), `biomass, ssb, R, Ftot, Fmul, landing, survey, R0, SB0,
SPR0, sigmaR`.

Dimension traps this helper must centralise (all confirmed against Rceattle source):

- **Every population array is dimensioned `styr:projyr`**, and the converter sets
  `projyr = endyr + 1` — so `biomass`, `ssb`, `R`, `F_spp`, `N_at_age` all carry **one trailing
  projection column that must be dropped** (`yi <- seq_len(nyr)`).
- `N_at_age`, `F_at_age` are `[spp, sex, age, year]`; `biomass`, `ssb`, `R`, `R0`, `SB0` are
  `[spp, year]`.
- `index_hat` / `catch_hat` are **vectors** aligned row-for-row to `index_data` / `catch_data`.
  `clean_data()` appends `Catch = NA` rows for the projection years, so
  `length(catch_hat) == nyr + 1` while `length(index_hat) == nyr`. **Match on `Fleet_code` +
  `Year`, never on position.**
- `R_sd` is ADREPORT-only and never REPORTed, so it is absent from `fit$quantities`; read
  sigmaR from `exp(fit$estimated_params$R_log_sd[1])`.
- Prefer `apply(F_at_age[1,1,,yi], 2, max)` over `F_spp` for `Ftot` — `F_spp` is
  `Σ_flt exp(log_F)`, equal to apical F only when selectivity maxes at 1.
- **Do not** re-invoke `obj$env$spHess()` on a returned fit to get a condition number — that can
  segfault uncatchably for random-effects models. Read the value Rceattle already computed at
  `fit$convergence$checks$hessian_conditioning$data$condition_number`, as the ported
  `get_condition_number()` does.

---

## Verification

1. **Load + collision check** (seconds): `devtools::load_all()`, then
   `stopifnot(exists("run_rceattle"), length(formals(get_condition_number)) == 3L)` — proves
   FIMS's helper is un-shadowed. Grep the new file for `|>` and confirm zero hits.
2. **Dimension assertions on one fit** (~1 min, `fixed_effects` scenario — fastest, no Laplace)
   against the bundled `FIMS_C1/.../OM1.RData`:
   `ncol(q$biomass) == nyr + 1`, `length(q$catch_hat) == nyr + 1`,
   `length(q$index_hat) == nyr`, and `all.equal(q$N_at_age[1,1,1,1:nyr], q$R[1,1:nyr])`.
3. **Payload cross-checks**: `all.equal(rce$SB0, rce$R0 * rce$SPR0, tolerance = 1e-2)`;
   `cor(rce$landing, om_output$L.mt$fleet1) > 0.99`; `cor(rce$ssb, om_output$SSB) > 0.90`.
   **Survey-units regression test**: `cor(rce$survey, om_output$survey_index$survey1) > 0.90`
   and `abs(mean(rce$survey) - 1) < 0.2`; then rebuild with `survey_units = "biomass"` and
   confirm the mean is now far from 1 — making the latent bug visible.
4. **Smoke test**: run `FIMS_integration_test_data/Rceattle_integration_test.R`. Expect the
   RDS stems (now including `plot_rceattle_*.RDS`) in `output/Rceattle/s1/`, a correlation
   table, and the PDF.
5. **Full pipeline** on a tiny case via `example/Rceattle_comparison.R` (`om_sim_num = 3`):
   `identical(names(rceattle_list), names(om_list))`,
   `identical(lapply(rceattle_list[1:18], dim), lapply(om_list[1:18], dim))`,
   `dim(rceattle_list$agecomp[[1]])` is nages × nyr, `convergence_measures$gradient` not all NA,
   `figure/` non-empty, `generate_plot()` returns without error.
6. **FIMS non-regression (mandatory)**: run `generate_plot(em_names = c("FIMS", "Rceattle"), ...)`
   on the same case and `all.equal(fims_list, fims_list_baseline)` against a pre-change run on
   the same seed. This is the only test that proves the helper renames protected `run_fims()`.
7. **Deterministic case**: run against `FIMS_C0` (`logR_sd = 0`). Expect `random_effects` to
   time out with `optimizer_convergence = 1` and no `hessian_*.RDS`; confirm exactly one
   aggregated warning, and that `options(ASSAMC.rceattle_scenario = "fixed_effects")` produces a
   fully populated `rceattle_list`.

## Files touched

| File | Change |
|---|---|
| `R/run_rceattle.R` | **new** — merged port of both source scripts + the fixes above |
| `R/run_em.R` | +1 dispatch line after line 24 |
| `R/check_convergence.R` | +1 `else if (subdir == "Rceattle")` branch between lines 40 and 41 |
| `R/read_plot_data.R` | + `rceattle_list` block after line 582 |
| `NAMESPACE` | +1 line `export(run_rceattle)` |
| `man/run_rceattle.Rd` | **new**, hand-written |
| `example/Rceattle_comparison.R` | **new** driver |
| `FIMS_integration_test_data/Rceattle_integration_test.R` | **new** smoke test |
| `DESCRIPTION` | optional `Suggests:` line |

PR base: `update-ems` (where `update-ems-w-Rceattle` branched from). `R/run_fims.R` is
read-only reference only — the source of the three name collisions.

## Open coordination items to raise in the PR

- Rceattle uses **analytical (closed-form MLE) catchability**; FIMS estimates `log_q` as a
  free parameter. Deliberate, but not identical treatment.
- Rceattle uses **mean recruitment** (`srr_fun = 0`), while the OM generates recruitment from
  Beverton–Holt with h = 0.75. Consequence: no MSY (see above).
- Reference points are borrowed from the OM. A future opt-in could hang an ad-hoc `msy_calcs()`
  off the EM's estimated M / selectivity / weight / maturity with `steep` pinned to
  `om_input$median_h`, reusing the existing `adhoc_bias_cor` flag — no signature change needed.
