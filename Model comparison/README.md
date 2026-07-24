# Model comparison — RCEATTLE estimation model for the ASSAMC OM–EM project

This folder adds **RCEATTLE** as an estimation model (EM) to the
Age-Structured Stock Assessment Model Comparison ("model comparison manuscript
2.0"), which compares ASAP, BAM, SS3, WHAM, FIMS, and now RCEATTLE under a
shared Operating Model / Estimation Model (OM–EM) framework in the
[ASSAMC package](https://github.com/NOAA-FIMS/Age_Structured_Stock_Assessment_Model_Comparison).

The deliverable is a `run_rceattle()` function that mirrors that package's
`run_fims()` / `run_wham()` EM adapters: it consumes an OM-simulated replicate,
configures several RCEATTLE scenarios, fits them, and writes comparison outputs
(SSB, F, recruitment, biomass, convergence, uncertainty, run time) in the
framework's file layout.

## Files

| File | Purpose |
|------|---------|
| `run_rceattle.R` | The EM runner + helpers. Written to copy verbatim into ASSAMC's `R/`. Defines `run_rceattle()`, `fit_rceattle_scenario()`, `seed_rceattle_inits()`, `rceattle_estimates()`, and the FIMS-parity diagnostics (`count_na_standard_errors`, `get_condition_number`, `save_rceattle_outputs`). |
| `om_to_rceattle.R` | The OM → RCEATTLE `data_list` translator (analogue of FIMS' `prepare_data_fims()`), plus `cv_2_sd()`. |
| `demo_run_rceattle.R` | Local driver: runs `run_rceattle()` against a real OM case in the local ASSAMC clone and self-tests recovery vs OM truth. Not part of ASSAMC. |
| `run_em_patch.R` | The one dispatch line to add to ASSAMC's `R/run_em.R`. |

## How it plugs into ASSAMC

`run_em()` dispatches to each model as
`run_<model>(maindir, subdir, om_sim_num, casedir, em_bias_cor)`. Each runner
loops `1..om_sim_num`, `load()`s `casedir/output/OM/OM{i}.RData` (which injects
`om_input`, `om_output`, `em_input`), fits, and `saveRDS()`s a fixed set of
result files into `casedir/output/<subdir>/s{i}/`. `run_rceattle()` follows this
contract exactly and returns nothing (side effects only). Per scenario it writes:

```
fit_rceattle_<scenario>.RDS                  # tidy estimates table (see below)
full_fit_rceattle_<scenario>.RDS             # full Rceattle fit (fallback)
run_time_rceattle_<scenario>.RDS             # c(fit_optimization, fit_sdreport, fit_total, total)
optimizer_convergence_rceattle_<scenario>.RDS
max_gradient_rceattle_<scenario>.RDS
hessian_rceattle_<scenario>.RDS              # (when sdreport available)
na_count_<scenario>.RDS
condition_number_<scenario>.RDS
```

## The three scenarios

Mirroring FIMS/WHAM, all three use RCEATTLE **mean recruitment**
(`build_srr(srr_fun = 0)`: `R_y = R0 * exp(rec_dev)`, effective steepness ≈ 0.99)
— the numerically robust canonical RCEATTLE form. They differ only in how the
recruitment deviations are treated, which is the axis the manuscript compares:

| Scenario suffix | FIMS/WHAM meaning | RCEATTLE expression |
|-----------------|-------------------|---------------------|
| `random_effects` | rec devs random-effect, σ_R estimated | `random_rec = TRUE`; `R_log_sd` estimated |
| `random_effects_sigmaR_constant` | rec devs random-effect, σ_R fixed | `random_rec = TRUE`; `R_log_sd` mapped off at the OM value |
| `fixed_effects` | rec devs fixed-effect, σ_R fixed | `random_rec = FALSE` (penalised rec devs); `R_log_sd` mapped off |

## Estimates table schema

`fit_rceattle_<scenario>.RDS` is a long `data.frame`:

```
label        year   age   estimate   uncertainty
SSB / biomass / recruitment / F   (age = NA for these time series)
```

`uncertainty` is the sdreport standard error for `SSB`, `biomass`, and
`recruitment` (the ADREPORTed quantities); `F` carries `NA` because `F_spp` is
not ADREPORTed in the production TMB template.

> **Coordination item:** the exact column schema the manuscript's
> `read_plot_data.R` expects for a new EM should be reconciled with the FIMS
> team; adjust `rceattle_estimates()` to match. The full fit is saved alongside
> as a fallback.

## Convergence & run time

- `optimizer_convergence_rceattle_<scenario>.RDS` is `0` when the maximum
  marginal gradient is `< 0.1` **and** the Hessian is positive-definite,
  matching ASSAMC's `check_convergence.R` (which keeps replicates with max
  gradient `< 0.1`). RCEATTLE's `fit$opt` has no nlminb `convergence` field, so
  the code is derived; `max_gradient_rceattle_<scenario>.RDS` carries the raw
  value (from `fit$opt$max_gradient`) for the manuscript's own filtering.
- The fits use `newtonsteps = 1` (with `loopnum = 1`, `getJointPrecision =
  FALSE`) so the final gradient is tightened well below 0.1 without the 5×
  cost of the `fit_control()` defaults.
- `run_time_rceattle_<scenario>.RDS` is `c(fit_optimization, fit_sdreport,
  fit_total, total)` **in seconds**. RCEATTLE does not separate optimisation
  from sdreport, so those two entries are `NA`; `fit_total` is RCEATTLE's own
  model time and `total` is wall-clock (incl. sdreport). A random-effects fit
  is ~75 s; the fixed-effects fit ~2 s.

> **Coordination item:** ASSAMC's `check_convergence.R` and `read_plot_data.R`
> have no RCEATTLE branch yet (they key off model-specific files / a stale FIMS
> `s{i}_gradient.RData` layout). Adding a RCEATTLE branch that reads the
> `*_rceattle_*.RDS` files above is part of the upstream contribution.

## Modelling choices worth reviewing

- **Mean recruitment, not Beverton–Holt.** The OM uses Beverton–Holt
  (`SRmodel = 1`, h = 0.75). RCEATTLE fits mean recruitment with deviations; the
  three scenarios vary the *deviation treatment*, not the SRR form. Matching the
  OM's B–H steepness for **MSY-based reference points** needs α/β seeded from
  steepness + SPR0 and is a follow-up (see `srr_pred_fun` for an AMAK/Ianelli
  B–H penalty in projection). The current runs are hindcast-only
  (`estimateMode = 1`), so no reference points are produced yet.
- **Parameters seeded at OM truth.** Like FIMS/WHAM, `seed_rceattle_inits()`
  starts R0 and the recruitment deviations at the OM values; the RCEATTLE
  default R0 is orders of magnitude off this stock's scale and otherwise
  collapses the population.
- **Analytical survey catchability** (closed-form MLE q) is used for robustness;
  FIMS estimates `log_q` as a free parameter. Switch the survey
  `Catchability` to `"Estimated"` in `om_to_rceattle()` to match FIMS more
  literally (needs a sensible q starting value).
- **Survey observation field.** `om_to_rceattle()` prefers a non-empty
  `surveyB.obs` (biomass index) and otherwise uses `survey.obs` (numbers index),
  setting `Weight1_Numbers2` accordingly. Confirm which the manuscript's OM
  cases populate and in which units.
- **Deterministic OM cases (C0, C0noPhiF; σ_R = 0).** The random-effects
  scenarios are ill-posed there (σ_R → 0 boundary); `fit_rceattle_scenario()`
  has a `time_limit` guard so such fits abandon gracefully and are recorded as
  non-converged rather than hanging. Use the stochastic cases (C1, C2;
  σ_R = 0.4) to exercise the random-effects scenarios.

## Running the demo

```sh
export PATH=/usr/bin:$PATH      # system toolchain (see Rceattle CLAUDE.md)
cd "Model comparison"
Rscript demo_run_rceattle.R     # defaults to the stochastic C1 case
```

It runs the EM on one replicate, reloads the saved RDS files, prints the
estimated-vs-truth correlations, and draws the cross-scenario `plot_ssb` /
`plot_biomass` / `plot_recruitment` comparison. Edit `case` at the top to switch
OM cases.

## Dependencies

Released (main-branch) **Rceattle** — the scripts use only the public API
(`fit_mod`, `build_srr`, `build_params`, `build_map`, `fit_control`,
`clean_data`, `switch_check`, `plot_*`) and deliberately avoid dev-branch-only
constructors (`build_data()` / `data_requirements()`). Also `foreach`,
`doParallel`, `parallel`, `dplyr`.
