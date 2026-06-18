# Handoff: Match SS3 model exactly — forward pass + estimation (GOA Pcod 2024)

Last updated: 2026-05-31 (evening).

**Goal**: get Rceattle to reproduce SS3's reported quantities (R, Bio, SSB,
per-component NLL, MLE parameter values) **exactly** for the modified GOA
Pcod 2024 model at `Data/goa_pcod-no init and ramp/`, both forward-pass
(parameters injected from SS3) and estimation (MLE from scratch). Modified
SS3 = original Pcod ctl with `max_bias_adj_in_MPD = -1` (ramp off),
lambda 18 = 0 (InitEQ_Regime off), and `F_Method = 2` (the latter so SS3
emits per-year per-fleet F as parameters we can read).

The reference catalogue of every known structural difference between the
two models is **[Estimation_Differences.md](Estimation_Differences.md)**
(now 1300+ lines, 20 numbered entries, kept current). This handoff is the
**work order** that uses that catalogue to drive next steps.

---

## Current state — one paragraph

Forward pass: **R machine-precision (1e-6), Bio 0.4%, SSB 54% (Jensen gap,
documented), TOTAL NLL +539 vs SS3** (down from a starting point of
+816,000+ via many iterations during May 2026). Every wing of the
parity architecture is now SS3-faithful: kernels (#1/#2/#3/#4),
ageing-error matrix (#19), linkage system (Phase A2 unification),
cpp BlockDev prior-weight + factor-shared map for time-varying sel/q
(#20), PHASE-honoring map override for SS3 PHASE<0 params (#18),
`.fit_tmb` resilience (nlminb errors no longer halt R). Estimation
**runs end-to-end** but converges with `discontinuous likelihood`
warning to TOTAL **+1952**. Param count 1672 → 315 after factor-shared
BlockDev map. The +1952 gap is dominated by **one open structural
difference**: SS3's `F_Method=2` Baranov continuous F vs Rceattle's
Pope's hybrid (#4). Once that is closed (either path), most downstream
residuals (LenComp, log_F gradient, Hessian NaNs) should collapse.

---

## TL;DR table

| Component | SS3 | Rce FP | Rce EST | Diff EST | Status / pointer |
|---|---|---|---|---|---|
| Survey index | −1.79 | −1.44 | −31.06 | −29.3 | partial q-dev overfit (LLSrv env_q); minor |
| **Catch** | **1.75** | **331** | **452** | **+450** | **#4 Pope's vs Baranov — THE blocker** |
| Length comp | 1336 | 1371 | 2389 | +1052 | downstream of #4 |
| CAAL | 721 | 854 | 1039 | +318 | downstream of #4 + N drift |
| Recruitment | −16.3 | −12.8 | 8.9 | +25.2 | rec_dev pulled by N drift |
| Init eq | 0 | 5.30 | 5.72 | +5.7 | cosmetic (mode 4 init_dev prior) |
| Parm priors | 0.79 | −4.47 | 43.3 | +42.5 | block-replacement priors |
| **Parm devs** | **6.09** | 44.3 | **94** | +88 | **BlockDev success (was +491 free IID)** |
| **TOTAL** | **2048** | **2587** | **4000** | **+1952** | gap dominated by Catch + LenComp |

R/Bio/SSB rel err (post-estimation): R 28% mean, Bio 24% mean, SSB 71% mean.

---

## What's already wired (inventory — don't rebuild)

### Cpp ([Rceattle/src/TMB/ceattle_v01_11.cpp](../../Rceattle/src/TMB/ceattle_v01_11.cpp))

- Survey index lognormal kernel: `log(σ) + 0.5·z²` (no `0.5·log(2π)`) — closed #3
- Catch kernel: SS3 robustified `0.5·(log(1.1·obs)−log(hat+0.1·obs)/σ)²` — closed #4 kernel form (still Baranov, not Pope's)
- Recruitment kernel: SS3-form per-year `0.5·z² + log(σ)`
- CAAL `pred_CAAL` integrates over the data bin (Lbin_method=2) — closed #17
- BlockDev — proper enum (`Time_varying_sel = "BlockDev"` = 6, `Time_varying_q = "BlockDev"` = 5):
  - Cpp prior_weight tensors: `DATA_ARRAY(sel_inf_dev_prior_weight)` `[3, n_flt, max_sex, nyrs_hind]`, `DATA_ARRAY(log_sel_slp_dev_prior_weight)` same shape, `DATA_MATRIX(index_q_dev_prior_weight)` `[n_flt, nyrs_hind]`
  - IID prior contributions multiplied by per-cell weight (1.0 default preserves old behavior bit-identically)
  - R: `build_map_selectivity` DoubleNormal + `build_map_catchability` BlockDev branches NA-lock all dev cells, then factor-share one estimable label per sub-block (read from `Selectivity_block` in catch/index_data)
  - R: `rearrange_data` auto-populates `*_dev_prior_weight = 1/N` from `Selectivity_block` for BlockDev fleets
  - R: `build_map_f_and_data_weights` NA-locks sel/q cells for `Fleet_type == "Off"` fleets (otherwise the default sequential map leaks them as estimable)
  - Result on Pcod: 1672 → **291 estimable params**, within 39 of SS3's 330 (gap = missing #10 dev-seq layer + 1 fixed `log_Finit`)
- Init mode 4 ("NonEquilibriumScaled") + init mode 5 ("EquilibriumScaled") — closes #6
- Linkage system: `growth_linkage_offset` / `M_linkage_offset` / `recruitment_linkage_offset` with log + identity link variants
- `Catchability = "EnvExp"` (= 7): SS3 case-1 exponential env-link `q = exp(LnQ · exp(β · env))` — closes #21. q[LLSrv] matches SS3 Calc_Q to 3.5e-6 per year.

### R ([Rceattle/R/](../../Rceattle/R/))

- [`build_params.R`](../../Rceattle/R/1-build_params.R): linkage init push for intercept rows; sentinel translation for `Time_varying_sel_sd_prior ≤ 0 → sel_dev_log_sd = -999` (skip prior)
- [`build_bounds.R`](../../Rceattle/R/3-build_parameter_bounds.R): push intercept-linkage bounds to base params (`log_growth_pars`, etc.) with `log()` conversion
- [`build_map.R`](../../Rceattle/R/2-build_map.R): `build_map_linkages` maps `beta_linkage[(Intercept)]` to NA at 0; `map_linkage_adjuster` keeps base param estimable for intercept-bearing formulas
- [`fit_mod.R`](../../Rceattle/R/6-fit_mod.R): when `inits` supplied, accepts user `map`; **defensive intercept-zero on `start_par$beta_linkage`** so name-vs-positional collisions can't bleed across linkage rows
- [`tmb_helpers.R`](../../Rceattle/R/0-tmb_helpers.R): `.fit_tmb` wraps `TMBhelper::fit_tmb` in `tryCatch` and falls through to in-package nlminb fallback when the TMBhelper path errors
- [`rearrange_data.R`](../../Rceattle/R/5-rearrange_data.R): defaults `*_dev_prior_weight = 1.0`; allocates 3D `age_error[sp, true_age, obs_age]` from `data_list$age_error` data.frame

### Test driver ([ss3_to_ceattle_forward_pass.R](ss3_to_ceattle_forward_pass.R))

- r4ss `SS_output` monkey-patch (skips Pstar/OFL sigma calc that errors on corrupt Report.sso)
- Jensen-gap closure: `Mat_F_wtatage` → SSB_WAA when applicable
- M post-2014 block via `M1_block` linkage spec
- LLSrv environmental q linkage
- Selectivity switched to parametric Length-DoubleNormal for active fleets
- `build_ss3_age_error(def_idx = 2)` writes SS3 ageing-error matrix (unbiased mean = true age, def-1 SDs) to `cod_pcod$age_error` — closes #19
- `populate_selectivity_block()` writes per-(fleet, year) sub-block ID from SS3 `ctllist$Block_Design` into `index_data$Selectivity_block` / `catch_data$Selectivity_block`
- `init_from_ss3()`, `init_log_F_from_ss3()`, `init_doublenormal_from_ss3()`, `init_state_from_ss3_natage_mode4()` — full SS3-MLE injection pipeline
- Bounds audit (§8b), Sel-at-length / Sel-at-age comparison (§9a), Growth-output comparison (§9b)
- Grouped-NLL table comparing every component vs SS3 `likelihoods_used`

### Estimation driver ([ss3_to_ceattle_estimate.R](ss3_to_ceattle_estimate.R))

- Sources the test script (FP setup) then runs estimation
- `apply_ss3_sel_phase_fixes()` — walks `ctllist$size_selex_parms$PHASE`, sets NA on Rce sel base slots whose SS3 PHASE<0 — closes #18
- Direct `inits$sel_dev_log_sd` / `inits$index_q_dev_log_sd` override (the cpp reads from these params, not from `data_list$fleet_control`)
- `build_blockdev_arrays()` — for each SS3 (fleet, sel-param) with `Block > 0`:
  - per-sub-block: writes `1/N` into the matching cells of the cpp prior_weight tensors
  - emits map patches that factor-share dev cells within each sub-block
- Pre-flight gradient check via `estimateMode = 3` (build obj, skip nlminb) so NaN gradients at the start point are caught before optimization
- Phasing OFF (`phase = FALSE`) — the package's phase loop rewrites the map at each step which conflicts with our user-supplied map; can be added back later
- Post-estimation: R/Bio/SSB rel err vs SS3, grouped NLL comparison, Hessian eigen analysis, sel-at-age per fleet

---

## The one open blocker — #4 Pope's vs Baranov

**SS3 (with `F_Method = 2` or default `F_Method = 1`)**:
- Continuous F per fleet per year (estimable parameter or hybrid solver)
- Catch fits observed catch nearly exactly by construction
- Catch NLL ≈ 0 for fully estimated models
- F_Method=2 emits per-year F as ADMB parameters (which we read via `parlist$F_fleet_*_YR_*_s_1`)

**Rceattle (current)**: continuous-F Baranov with `log_F` estimable
parameter per fleet per year, but the realized catch_hat goes through
SS3's robustified kernel only — the F-finding solver SS3 uses is not
implemented. When we inject SS3's MLE F values for the forward pass,
catch_hat matches to within 1% but the per-obs robustified residual sums
to +330 NLL (vs SS3's ~0). When the optimizer is allowed to estimate F,
it pushes log_F to extreme values (e.g. log_F = 4.25 → F = 70) trying to
close the catch gap further, hits a discontinuity, and stops with
`warning(8) discontinuous likelihood`.

**This single thing is responsible for**:
- log_F idx 196 with grad=517 at "convergence"
- All 315 Hessian rows being NaN (log_F's column blows up the AD tape)
- LenComp +1052 EST (sel devs constrained by BlockDev can't compensate)
- CAAL +318 EST (downstream of N drift caused by wrong F)
- Catch +450 EST

Without closing this, no path to exact match exists. With it closed, we
expect: log_F gradient ~0, Hessian becomes positive-definite, LenComp
drops to ~FP level, CAAL drops to ~FP level, and the only remaining
gaps are the small documented residuals (#13 plus-group, #15 Jensen SSB,
#6 init_dev prior cosmetic).

---

## Two paths to closing #4

### Path 1 — Implement SS3's Pope's hybrid F in Rceattle cpp

**What SS3 does** (default `F_Method = 1` "Hybrid F"):
1. For each year, given current N at start of year, sel, and obs catch:
2. Use Pope's discrete approximation `harvest_rate = catch_obs / midyear_biomass` as a starting guess (where midyear_biomass = N · exp(-M/2) · weight · sel)
3. Iterate 2-5 Newton steps converting that harvest rate to a continuous F that gives Baranov-equivalent catch (or hybrid-Pope's catch)
4. Set realized F[year, fleet] from the iterated value
5. Catch NLL becomes ~0 because catch_hat is constructed to equal catch_obs

**Required cpp changes**:
- New `F_method` enum on `data_list` (0 = current free log_F estimation, 1 = hybrid Pope's iteration)
- New cpp block that, when `F_method == 1`, computes F per (year, fleet) from observed catch + state instead of reading from `log_F` parameter
- `log_F` becomes mapped-out (NA) under `F_method == 1` so optimizer doesn't try to estimate it
- Catch likelihood becomes a residual on log(catch_hat) − log(catch_obs) which is ~0 by construction

**Reference implementation**: SS3 source [SS_global.tpl:1420-1490](ss3-source-code-main/SS_global.tpl#L1420) (the `if (F_Method == 1)` hybrid block). The math is small but the loop structure is non-trivial.

**Effort**: 1-2 days. Worth it long-term because every SS3 model worth bridging uses F_Method 1 or 2.

**Risk**: changes how N-at-age propagates year-over-year; needs careful regression testing on existing Rce non-SS3 models.

### Path 2 — Run SS3 with `F_Method = 3` (continuous Baranov)

**What SS3 does with `F_Method = 3`**: identical to Rceattle — continuous
F per fleet per year, estimated as a free parameter, with Baranov catch
equation. No hybrid solver. Catch NLL is the standard lognormal residual,
positive and non-trivial.

**Required SS3 ctl edits**:
```diff
- F_Method = 2  # (current — F per-year params)
+ F_Method = 3  # (continuous, free param per year per fleet)
```
Plus the `F_setup` table needs to be set to phase=1 for each F parameter
so SS3 actually estimates them. Documented in
[SS_to_match_Rceattle.md](SS3_to_match_Rceattle.md) (if it exists; if
not, create section there).

**Effort**: 30 min SS3 ctl edit + re-run SS3 + re-export. Smaller change,
cleaner comparison.

**Risk**: SS3's F_Method=3 estimates can differ from F_Method=1 or 2 by a
few % per year; the comparison target model becomes the F_Method=3
version, not the existing one. So the "exact match" target changes too.
That may be fine — the goal is parity between two methodologically
equivalent models, not to match a specific historical SS3 output.

### Recommendation

**Do Path 2 first** (30 min). If F_Method=3 SS3 vs current Rceattle
estimation matches to 🟡 (per-component < 1 NLL), Path 1 is unnecessary
for parity work — keep Rce's continuous F. If they still diverge, do
Path 1 to also close the SS3-side methodology gap.

---

## After #4 is closed — small residuals to chase

Listed in expected order of NLL impact. Each is decomposed in
[Estimation_Differences.md](Estimation_Differences.md) under its
numbered section.

1. **#13 plus-group LAA convention** — Rce uses static `exp(−0.2·a)` weighting; SS3 uses dynamic N-weighted plus-group. Affects LAA / WAA at the plus age (~2e-3 rel err). Downstream into LenComp / CAAL via shifted mass at L=99+ cm. **Fix path**: enable the commented dynamic plus-group block in [growth.hpp:230-260](../../Rceattle/src/TMB/growth.hpp#L230).
2. **#15 Jensen gap on SSB (length-based maturity)** — Rce computes SSB as `N · weight_hat · maturity · sex_ratio`; SS3 integrates `mat(L) · wt(L)` over the size distribution per age. For length-based maturity (Pcod uses `Mat50%`/`Mat_slope` on length), Jensen's inequality makes the integral ~ 8% lower than the point estimate. **Fix path 1a (sex_ratio)** is a one-line converter change; **path 1b (Jensen)** needs cpp gating on an injected `mat_wt_at_age` slot.
3. **#6 InitEQ init_dev prior** — under mode 4 with non-zero init_dev (which we use to pin styr N), the lognormal prior on init_dev fires ~+5 NLL. Cosmetic — doesn't affect estimates. **Fix path**: implement SS3's `regime_like` formula in cpp or gate the prior under a per-species flag.
4. **#19 per-row ageing-error reference** — Pcod has 2 ageing-error defs; 75% of CAAL rows use def 2, 25% use def 1. We currently use def 2 for all rows. **Fix path**: add per-comp-row ageing-error index to `caal_data` / `comp_data`; rearrange to a 4D `age_error[sp, def_id, true, obs]`; cpp picks `def_id` per row.
5. **#7 M-block prior structure** — SS3 has independent priors on `M_base` and `M_block_2014_replacement`; Rce has prior on the offset `M_block - M_base`. Small NLL difference.
6. **#20 q-dev BlockDev** — ~~LLSrv has SS3 env-q linkage (not block), so q-devs are still free IID with σ=0.3 prior. Survey NLL is partially overfit (−31 vs SS3 −1.8). **Fix path**: model the SS3 env-q exponential link `q[yr] = exp(LnQ_base · exp(env_add · env[yr]))` directly in cpp (currently approximated via per-year q_devs).~~ **Closed by #21 EnvExp.**
7. **Phase A3 N drift** — even with all of the above closed, mid-series (1982-2010) shows 2-3% Bio rel err that hasn't been localized. Suspected: a M-block boundary discontinuity or a sub-block sel transition introducing a one-year jump. Hunt down by dumping per-year N-at-age comparison and looking for the year where Rce/SS3 ratio diverges.

---

## Decision points for the user

Before the next session executes, decide:

### D1. Path 1 vs Path 2 for #4
- **Path 2 (SS3 F_Method=3, 30 min)** is what I'd recommend. Fast feedback, no Rce package change.
- **Path 1 (Rce Pope's, 1-2 days)** is more thorough — closes the methodology gap for ALL future SS3 bridges, not just this one.

### D2. Scope of "exact match"
- **Tier 1 (NLL within 1)**: requires closing #4 + the cosmetic init_dev prior + Jensen sex_ratio. ~1 week.
- **Tier 2 (NLL within 0.01)**: also requires plus-group LAA dynamic, Jensen on length-based maturity, per-row ageing error, M-block prior structure. ~3-4 weeks.
- **Tier 3 (bitwise param match)**: also requires #9 `dev_link` scaling, #10 three-tier sel base + block_repl + dev_seq. ~6-8 weeks (significant cpp + converter work).

### D3. Phasing vs single-shot estimation
- Currently `phase = FALSE` in estimate script because the package's phase loop rewrites the map at each step. To use phasing with our user-supplied map, need to either:
  - (a) Patch `Rceattle::TMBphase` to accept and respect an external map skeleton (the user map's NAs survive, additional NAs added per phase step), OR
  - (b) Implement phasing manually in the estimate script by wrapping fit_mod calls.
- Phasing helps with conditioning. Worth doing once Path 2 closes the catch issue.

---

## How to reproduce / extend

### Run forward-pass only (~3-5 min)
```bash
cd "Rceattle-models/GOA cod"
Rscript ss3_to_ceattle_forward_pass.R > /tmp/fp.log 2>&1
# Inspect:
grep -A4 "Forward-pass relative" /tmp/fp.log
grep -A12 "Grouped NLL components" /tmp/fp.log
```
Expect (current): R 1e-6, Bio 3.8e-3 mean, SSB 54% (Jensen, expected), Total +539.

### Run estimation (~15-30 min)
```bash
cd "Rceattle-models/GOA cod"
Rscript ss3_to_ceattle_estimate.R > /tmp/est.log 2>&1
# Inspect:
grep -A12 "Estimation: grouped NLL" /tmp/est.log
grep -A12 "Top 10 worst gradients" /tmp/est.log
grep "Hessian rows with NaN" /tmp/est.log
```
Expect (current, pre-#4 fix): warning 8 "discontinuous likelihood",
TOTAL ~4000, log_F idx 196 grad ~500, all rows NaN.

### Rebuild Rceattle after cpp / R changes
```bash
cd "Rceattle ecosystem"
R CMD INSTALL --preclean --no-multiarch --with-keep.source Rceattle
# 1-2 min if cpp; ~20 s if R-only
```

### Re-run SS3 (if you change ctl)
```bash
cd "Rceattle-models/GOA cod/Data/goa_pcod-no init and ramp"
# (need the SS3 binary, e.g. ss3 or ss_osx, in PATH)
ss3
# Outputs: Report.sso, ss3.par, etc.
# Then re-run R driver — it reads parlist + datlist + ctllist fresh.
```

---

## Files to edit for each path

### For Path 2 (SS3 F_Method = 3)
1. `Data/goa_pcod-no init and ramp/Model19_1e.ctl` — change F_Method line and update F_setup table; see [SS_to_match_Rceattle.md] (write or update this section)
2. Re-run SS3; re-export Report.sso + ss3.par
3. No R or cpp changes needed — Rceattle estimation already does continuous F
4. Update [Estimation_Differences.md](Estimation_Differences.md) §4 to record that F_Method now matches

### For Path 1 (cpp Pope's hybrid)
1. [Rceattle/src/TMB/ceattle_v01_11.cpp](../../Rceattle/src/TMB/ceattle_v01_11.cpp) — add `F_method` enum read; add hybrid F iteration block; gate `log_F` parameter path
2. [Rceattle/R/2-build_map.R](../../Rceattle/R/2-build_map.R) — `build_map_f_and_data_weights` maps `log_F` to NA when `F_method == 1`
3. [Rceattle/R/0-build_fleet_control.R](../../Rceattle/R/0-build_fleet_control.R) (or wherever) — add `F_method` to data_list schema
4. New regression tests in `Rceattle/tests/testthat/` — Pope's vs Baranov on a simple 1-fleet 10-yr toy model
5. Update [Estimation_Differences.md](Estimation_Differences.md) §4 status to ✅

---

## Memory the next session should know

- The modified SS3 model is at `Data/goa_pcod-no init and ramp/`. The original (with ramp + InitEQ) is at `Data/goa_pcod/`. Test script points at the modified one.
- r4ss `SS_output` errors on corrupt Report.sso when parameters are at bounds; the test script applies a `body()` monkey-patch to skip the Pstar/OFL sigma block. This is needed for every fresh R session that uses `r4ss::SS_output` on these dirs.
- Phase A2 unification bug was: `inits$beta_linkage[1] <- log(M_blk/M_base)` collided with the K-intercept row when growth linkages were enabled. Fixed via name-based lookup in the script + defensive intercept-zero in `fit_mod`. **If you see Bio rel err = 165% after any future linkage change, this is the first thing to check.**
- σ for sel_dev / q_dev priors is baked into `inits$sel_dev_log_sd` and `inits$index_q_dev_log_sd` at `build_params` time from `data_list$fleet_control`. Post-source overrides to `fleet_control` are NO-OPs. Override `inits$*_log_sd` directly.
- `Selectivity_block` column in `index_data` / `catch_data` is populated per (fleet, year) by `populate_selectivity_block()` in the test script. Uses SS3 ctl Block_Design. One block design per fleet (the dominant one across PHASE>=0 sel params).
- `.fit_tmb` swallows nlminb errors and falls through to the in-package fallback. So a partial result is always recoverable even when the optimizer hits NaN gradient mid-run.

---

## Related documents

- **[Estimation_Differences.md](Estimation_Differences.md)** — the authoritative catalogue of every structural diff (20 numbered entries); current per-component NLL snapshot for FP and EST.
- [SS3_to_match_Rceattle.md](SS3_to_match_Rceattle.md) — compact recipe of SS3 ctl/starter edits applying Path 2 for each difference (includes F_Method = 3 path for #4).
- [Generalizing_to_other_SS3_models.md](Generalizing_to_other_SS3_models.md) — applying this bridge to other stocks.
- [HANDOFF_growth_matrix_empirical.md](HANDOFF_growth_matrix_empirical.md) — separate empirical-growth + CAAL package bug (parallel track).

---

## One-line summary for a fresh Claude session

> The full SS3-faithful estimation pipeline (PHASE map fixes, ageing
> error, BlockDev prior_weight + factor-shared map, sigma-aware dev
> priors, nlminb resilience, mode-4 init_dev pin) is built and runs end
> to end. The TOTAL +1952 EST gap vs SS3 collapses to small residuals
> once #4 (Pope's vs Baranov on log_F) is closed — either by running SS3
> with `F_Method = 3` (30 min, recommended first) or by implementing
> Pope's hybrid in Rce cpp (1-2 days). See [Estimation_Differences.md](Estimation_Differences.md)
> entries #4 (Catch), #18 (PHASE), #19 (ageing error), #20 (BlockDev)
> for the catalogued structural picture.
