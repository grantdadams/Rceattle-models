# Generalizing the SS3 → Rceattle bridge to other stock synthesis models

The GOA Pcod 2024 work produced two artifacts: a generic converter
([R/ss3_to_rceattle.R](R/ss3_to_rceattle.R)) and a Pcod-specific test
driver ([ss3_to_ceattle_test.R](ss3_to_ceattle_test.R)). The converter
is reusable; the driver is the template that needs adapting per stock.

This document is the recipe for applying the bridge to a new SS3
model. It assumes you've read
[HANDOFF_estimation_parity.md](HANDOFF_estimation_parity.md) and
[Estimation_Differences.md](Estimation_Differences.md).

---

## Step 1 — Verify the converter handles your SS3 dat/ctl

The converter is at [R/ss3_to_rceattle.R](R/ss3_to_rceattle.R). Drop
the new SS3 model's `.dat`, `.ctl`, `.par`, and `Report.sso` into a
folder and run:

```r
source("R/ss3_to_rceattle.R")
cod_NEW <- ss3_to_rceattle(
  ss3_dir = "Data/<your_model>",
  minage  = 0,                 # or whatever your model uses
  verbose = TRUE
)
str(cod_NEW, max.level = 1)
```

You'll get back a `data_list` formatted for `Rceattle::fit_mod`. Run
through `fit_mod(estimateMode = 3)` (forward-pass, no estimation) to
see if it builds:

```r
mod0 <- Rceattle::fit_mod(
  data_list = cod_NEW,
  estimateMode = 3,
  growthFun = Rceattle::build_growth(fun = "vonBertalanffy"),
  fit_control = fit_control(verbose = 1)
)
```

If this errors, the converter is missing something — most likely a
new SS3 dat-file feature not covered (see "Known converter
limitations" below).

---

## Step 2 — Identify what's stock-specific in the test driver

These blocks in [ss3_to_ceattle_test.R](ss3_to_ceattle_test.R) need to
be re-fit for the new stock:

### 2.1 Active fleets list (line ~204)
```r
active_sel_fleets    <- c("FshTrawl", "FshLL", "FshPot", "Srv", "LLSrv")
fleet_block_pattern  <- c(FshTrawl = 2L, FshLL = 2L, FshPot = 3L,
                          Srv = 1L, LLSrv = NA_integer_)
```
Replace with your fleet names + the SS3 block_pattern index each fleet
uses for time-varying sel (look in your `.ctl` `# Block_Design` and
the selectivity parameter blocks).

### 2.2 Selectivity dim + pattern (line ~277)
For each active fleet:
```r
cod_pcod$fleet_control$Selectivity[fi]           <- "DoubleNormal"
cod_pcod$fleet_control$Selectivity_dimension[fi] <- "Length"
```
If your stock uses Logistic or DoubleLogistic in SS3, change to
`"Logistic"` / `"DoubleLogistic"` (Rce sel patterns are in
`Rceattle:::sel_map`). If sel is age-based, use `"Age"`.

### 2.3 M block setup (line ~99)
```r
m_block_yrs <- ctllist$Block_Design[[4]]   # SS3 block pattern 4 = M
cod_pcod$env_data$post2014 <-
  as.integer(cod_pcod$env_data$Year >= m_block_yrs[1] & ...)
```
Find the M block pattern index in your SS3 ctl (search for the
`NatM` parameter's `Block` column — it points to a row in
`Block_Design`). Adjust the env_data covariate name + years.

If your stock has NO M block, remove the M linkage entirely from
`growthFun_est_spec` and use `build_M1(M1_use_prior = TRUE, M_prior =
..., M_prior_sd = ...)`.

### 2.4 Catchability env-q linkage (line ~120)
The Pcod LLSrv has `env_var&link = 101` in its SS3 ctl row, meaning
"env covariate 1, exponential link". The test script bypasses this by
setting `Catchability = "Estimated"` + `Time_varying_q = "IID"` and
injecting per-year q-deviates. If your stock has env-q on any fleet,
do similar — or use Rce's `Catchability = "Environmental"` if SS3
uses the additive link (which is what Rce's Environmental mode does).

### 2.5 Growth + SS3 priors (line ~349)
```r
growthFun_est_spec <- build_growth(
  fun = "vonBertalanffy",
  linkages = list(
    K    = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 0.1966),
                        priors = list("(Intercept)" = normal(0.1966, 0.03)),
                        bounds = list("(Intercept)" = c(0.05, 1))),
    ...
  )
)
```
Replace `0.1966`, `0.03`, etc. with YOUR stock's SS3 values:
- `K` from ctl row labeled `VonBert_K` (PRIOR, PR_SD columns)
- `L1` from `L_at_Amin` row
- `Linf` from `L_at_Amax` row
- `sd_L1` from `CV_young` row
- `sd_Linf` from `CV_old` row
- Match SS3's `PR_type` to the Rce prior family
  ([Estimation_Differences.md §1.6](Estimation_Differences.md)).
- Use LOWER bounds > 0 (the build_bounds push gates on `lower > 0`).

### 2.6 M prior (line ~155)
The script extracts SS3's NatM prior:
```r
m_prior_ss3 <- extract_ss3_prior(ctllist$MG_parms, "NatM_p_1_Fem_GP_1$")
M_prior_rce <- exp(m_prior_ss3$PRIOR - 0.5 * m_prior_ss3$PR_SD^2)
```
The regex matches Pcod's NatM row name; verify your stock's row uses
the same name (`NatM_p_1_<sex>_GP_<gp_num>$`).

### 2.7 Sel injection (line ~605, `init_doublenormal_from_ss3`)
This function loops over `active_sel_fleets` and reads
`ss3_rep$SelSizeAdj` to get per-year effective sel params. If your
stock has a different fleet structure or block pattern, the function
should still work — it's parameterized by `fleet_meta` and reads
SS3's reported effective values directly.

If your stock uses pattern 1 (Logistic) instead of 24 (DoubleNormal),
the parameter mapping (P1=peak, P3=asc, etc.) needs to be adapted to
the Logistic params (alpha, inflection). Or use a different
injection helper.

---

## Step 3 — Run forward-pass and check parity

```r
cod_NEW_fixed <- Rceattle::fit_mod(
  data_list    = cod_NEW,
  inits        = inits,
  estimateMode = 3,
  growthFun    = growthFun_spec,    # no linkages for forward-pass
  M1Fun        = M1_block,
  fit_control  = fit_control(phase = FALSE, verbose = 1)
)
```

Inspect the same parity tables the Pcod script produces:
- `Grouped NLL comparison (SS3 vs Rceattle)`
- `Per-fleet LenComp + CAAL NLL`
- `Sel-at-length` matches

Expected baseline gaps (without further config alignment):
- Survey index: +0.5·log(2π)·n_obs
- Catch: ~10-50 (depending on Pope's vs Baranov mismatch)
- Length comp: <5 NLL if SS3Robust correctly applied
- CAAL: <500 NLL after off-by-one fix (verify alignment per
  [Estimation_Differences.md #14](Estimation_Differences.md))
- Recruitment: ~10-100 (depends on Methot-Taylor settings)

If a gap is dramatically larger than expected, work through
[Estimation_Differences.md](Estimation_Differences.md) to identify
the source.

---

## Step 4 — Run estimation with the SS3-faithful map override

See [HANDOFF_estimation_parity.md](HANDOFF_estimation_parity.md) for
the current work order. The infrastructure is built and reusable:
- `apply_ss3_sel_phase_fixes()` (PHASE map fixes from `ctllist$size_selex_parms$PHASE`)
- `build_ss3_age_error()` (ageing-error matrix from `datlist$ageerror`)
- `populate_selectivity_block()` (per-(fleet, year) sub-block ID from `ctllist$Block_Design`)
- `build_blockdev_arrays()` (cpp `*_dev_prior_weight` + factor-shared map from sub-blocks)
- Direct `inits$sel_dev_log_sd` / `inits$index_q_dev_log_sd` override (not via fleet_control)
- `inits` populated by `init_from_ss3()` / `init_log_F_from_ss3()` / `init_doublenormal_from_ss3()` / `init_state_from_ss3_natage_mode4()`

If a new stock surfaces a new SS3 feature (e.g. age selectivity, RW
sel time-varying, fishery CPUE survey), add a sibling helper alongside
the existing ones rather than baking the logic into the converter.

---

## Known converter limitations

The [R/ss3_to_rceattle.R](R/ss3_to_rceattle.R) converter handles
single-area, single-season SS3 models. Things it does NOT handle yet:

| SS3 feature | Status | Workaround |
|---|---|---|
| Multi-area | Untested | Will likely fail at `datlist$Nareas > 1` stop |
| Multi-season | Untested | Per-fleet `flt_month` may be wrong |
| Tag-recapture data | Not converted | Drop from data_list |
| Discards | Not converted | Treat as retained |
| Two-sex | Partially supported (`nsex_rce = 2`) | Verify per-sex sel + growth work |
| Generalized size composition | Not converted | Drop from data_list |
| Forecasting | Not converted | Rce projects via `estimateMode = 0` |
| Survey timing per-obs | Uses `flt_month` from fleet_control | Override `index_data$Month` if needed |
| Aging error matrix | Read from SS3 dat | Verify shape matches `nages` |

If you hit any of these, add a check at the top of `ss3_to_rceattle`
that explicitly errors with a clear message until support is added.

---

## Step 5 — Apply the Pcod test script as a template

Copy [ss3_to_ceattle_test.R](ss3_to_ceattle_test.R) to a new file
named after your stock (e.g., `ss3_to_ceattle_BSAI_pollock.R`). Walk
through the script top-to-bottom, replacing all Pcod-specific
constants from Step 2 above.

Key sections to inspect:
- `# 1. Read SS3` (line ~30) — `ss3_dir` path
- `# 2. Pop wt-at-age + maturity` (line ~75) — F mat, weight defaults
- `# 3. M1 linkage` (line ~99) — M block setup
- `# 4. Sel switch` (line ~270) — fleet config
- `# 7. init_doublenormal_from_ss3` (line ~605) — sel injection
- `# 9. Forward-pass fit` (line ~763)
- `# 10. Full MLE estimation` (line ~2070)

Don't forget the diagnostic blocks at the bottom — they're stock-
agnostic and useful as-is.

---

## Step 6 — Add a regression test for the new stock

In `tests/test-<stock>-conversion.R`, write a smoke test that:
1. Loads the SS3 model
2. Runs `ss3_to_rceattle()`
3. Runs `fit_mod(estimateMode = 3)` (forward-pass only)
4. Checks that key NLL components are within expected gaps of SS3

Example template:
```r
testthat::test_that("ss3_to_rceattle bridge works for <stock>", {
  testthat::skip_if_not_installed("r4ss")
  testthat::skip_if_not_installed("Rceattle")
  source("R/ss3_to_rceattle.R")
  ss3_dir <- "Data/<stock>"
  dat <- ss3_to_rceattle(ss3_dir, minage = 0, verbose = FALSE)
  mod <- Rceattle::fit_mod(
    data_list    = dat,
    estimateMode = 3,
    fit_control  = fit_control(verbose = 0))
  ss3_rep <- r4ss::SS_output(ss3_dir, verbose = FALSE, printstats = FALSE)
  rce_total <- sum(mod$quantities$jnll_comp)
  ss3_total <- ss3_rep$likelihoods_used["TOTAL", "values"]
  testthat::expect_lt(abs(rce_total - ss3_total), 500)  # tolerance for residual structural diffs
})
```

---

## Recommended order of stocks to validate

If you want to stress-test the converter against multiple SS3 stocks,
ordered by increasing complexity:

1. **Single-fleet, single-area, single-sex, VBGF** (e.g. AFSC sablefish
   simple variant) — verifies the basic pipeline
2. **Multi-fleet, single-season, VBGF + block sel** (e.g. BSAI Pollock
   or GOA Pollock) — verifies block_repl + dev_seq alignment
3. **Two-sex, sex-specific growth** (e.g. ATF) — verifies the sex
   stratification path
4. **Multi-area / multi-season** (e.g. complex Pacific Hake setup) —
   exercises area/season generalizations that don't exist yet

After each stock, update the "Known converter limitations" table above
with what worked, what failed, and what was patched.

---

## Quick checklist for applying to a new stock

- [ ] Drop SS3 dat/ctl/par/Report.sso into `Data/<stock>/`
- [ ] Run `ss3_to_rceattle()` → verify no errors
- [ ] Run `fit_mod(estimateMode = 3)` with `growthFun = build_growth("vonBertalanffy")` → verify no errors
- [ ] Inspect the forward-pass NLL comparison; flag any unexpectedly large gaps
- [ ] Copy `ss3_to_ceattle_test.R` to new file; replace Pcod-specific constants per Step 2
- [ ] Run forward-pass section; verify per-component NLL gaps within expected bounds
- [ ] Apply Phase A from HANDOFF_estimation_parity for estimation
- [ ] Write a smoke test for CI
- [ ] Document any new structural differences in
      [Estimation_Differences.md](Estimation_Differences.md) (which is
      currently Pcod-centric but the entries generalize)

---

## What to do if a new stock surfaces a converter bug

The Pcod work already fixed several converter bugs that would have
affected any stock. The remaining places to watch are:

- **Sel pattern coverage**: only patterns 1, 3, 8, 24 tested. If your
  stock uses 2, 5, 6, 7, 9, 11, 17, 24, 25, 27, etc., the
  injection helpers may need extension.
- **Growth functions**: only VBGF tested in production. Richards
  (model 2) has code paths in `growth.hpp` but isn't validated.
- **Empirical growth (`growth_model = 0`)**: incompatible with CAAL
  per existing data_check; works for length-comp-only stocks.
- **Per-fleet sel timing (`flt_month`)**: assumed scalar mid-year for
  most fleets; if your stock has season-varying surveys, the timing
  override in section 4 may need a per-obs Month override.

For each bug, add a test in the new stock's test file and reference
[Estimation_Differences.md](Estimation_Differences.md) if it
illuminates a new structural diff.
