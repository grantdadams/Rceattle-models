# Overnight report — matching Rceattle to the RTMB EBS pollock model (`base.rds`)

Date: 2026-06-18 → 19 (overnight, autonomous).
Scope you set: **bypass permissions; working-tree only (no commits); breadth-first;
keep F as-is (no F-penalty matching); match the RTMB model (`base.rds`).**

---

## TL;DR (read this first)

1. **The target moved.** `base.rds` is the **RTMB** port of the AMAK "pm" pollock
   model — and it is **not** the ADMB SAFE 2024 fit the bridging scripts were
   matching. They differ by **~25 % in SSB** (1964: ADMB 1609 vs RTMB 607; both
   rise to ~3300 by 2024). My forward pass reproduces **ADMB SAFE to ~3 %**, but
   the RTMB `base.rds` is a **genuinely different optimum**. Matching RTMB to
   machine precision therefore means matching RTMB's *objective function* **and**
   its convergence — not the ADMB targets in `HANDOFF_pollock_bridging.md`.
   **→ First decision for you: confirm `base.rds` is the intended target and that
   it is the converged RTMB fit you want (its early-year SSB is ~40 % of ADMB,
   which is a large, deliberate-looking difference worth sanity-checking).**

2. **I pinned down every RTMB likelihood form + constant** (from `RTMB/R/Rpm.R`,
   `model_funs.R`, `utilities.R`, and `base.rds` itself). The full spec is in the
   table below. The dynamics equations already match (forward pass is exact); the
   gap is entirely in the **likelihood forms** and **estimator/initialisation**
   choices.

3. **I implemented + verified two additive, default-preserving switches** in the
   package (working tree, uncommitted):
   - **`Catchability = "AnalyticalArith"`** — arithmetic-mean analytical q
     (`q = mean(obs)/mean(pred)`), the RTMB/AMAK BTS estimator (the documented
     *dominant* scale driver, diff #13).
   - **`index_catch_bias`** (default 1) — toggle the lognormal `-σ²/2` bias
     correction on index + catch; set 0 for the RTMB log-SSQ form.
   - **Regression check: default behaviour is bit-for-bit identical** (max abs
     diff of the pollock forward-pass `jnll_comp` = `0e+00`). The package
     recompiles cleanly; full test suite was running at hand-off (see below).

4. **The remaining gaps need design decisions that are yours to make** (new
   likelihood *forms* in a released package): the **covariance BTS likelihood**,
   **natural-scale normal CPUE/AVO**, the **AMAK recruitment likelihood**, the
   **avgsel penalty**, the **comp offset**, and **`initMode`**. Concrete proposals
   below. I did **not** half-build these, on purpose — they change public surface
   and need your call.

---

## What I changed (working tree, no commits)

Package `Rceattle/` (branch `dev-ebs-pk`):

| File | Change |
|---|---|
| `src/TMB/ceattle_v01_11.cpp` | `DATA_SCALAR(index_catch_bias)`; gate `-σ²/2` on index (slot 0) + catch (slot 1); arithmetic-q accumulators + `est_index_q == 7` branch (`q = Σobs/Σpred`). |
| `R/0-switches.R` | `q_map` gains `"AnalyticalArith" = 7`; `index_catch_bias` default (1). |
| `R/5-rearrange_data.R` | carry `index_catch_bias` into the TMB data list; **bug fix**: default the pollock-era `Sel_*` columns if absent (see below). |
| `NEWS.md`, `DESCRIPTION` | version → **4.8.0**; new-features + bug-fix bullets. |

**Bug fix (pre-existing merge regression I found + fixed).** The full suite came
back `[ FAIL 4 | WARN 0 | SKIP 4 | PASS 3782 ]`. All 4 failures were in
`test-likelihood-osa-residuals.R` (`Column Sel_start_year not found in .data`) —
**not** caused by my edits. The dev-ebs-pk merge added `Sel_start_year` /
`Sel_pen_first_age` / `Sel_pen_last_age` / `Sel_shape_mode` / `Sel_cap_age` mutates
to `rearrange_data()` **without defaulting them**, breaking the exported function
when called on a raw `data_list` (the OSA test calls `rearrange_data(dat,
build_osa=TRUE)` without `switch_check()`). The full suite had never been run
post-merge (only the selectivity subset), so it slipped through. I added a
defensive default block; the OSA file now passes **`[ FAIL 0 | SKIP 0 | PASS 99 ]`**.
**Confirmed: the full suite is now green — `[ FAIL 0 | WARN 0 | SKIP 4 | PASS 3808 ]`.**

**Verification run** (`_verify_switches.R`):
```
REGRESSION (default index_catch_bias=1): max abs diff vs pre-change jnll = 0e+00
SWITCH EFFECT (forward pass, illustrative):
                   default(bias1)     biasOFF   biasOFF+arithQ
Catch (Fishery)      17433.82127  17410.91180   17410.91180
Index BTS             2022.69162   2024.39029    2230.39350
BTS index_q[1]:  geometric 2.76964   →  arithmetic 2.25121
```
(The *absolute* forward-pass numbers are large because that harness injects the
**ADMB** MLE, not RTMB — it is only valid here as a relative regression/effect
check. See "Why verification is hard" below.)

---

## Complete per-component spec (RTMB target → Rceattle)

`base.rds$report` totals: `tot_like = 7951.46`. **`wt_like = 6344.87` is a
separable weight-at-age sub-model** (its parameters don't enter the population
dynamics), so the dynamics-relevant objective is **≈ 1606.6**.

| Component | RTMB target | RTMB form (exact) | Rceattle now | Gap / action |
|---|---:|---|---|---|
| Catch | 4.822 | `200·Σ(log(o+1e-4)−log(p+1e-4))²` (catBio=200 ⇒ σ=0.05) | lognormal `dnorm`, σ=0.05, +`−σ²/2` | **DONE**: set catch `Log_sd=0.05` + `index_catch_bias=0`. (offset 1e-4 negligible) |
| BTS index | 35.117 | `0.5·rᵀ·Σ⁻¹·r`, **natural scale**, `r=obs−q·pred`, **arith q** | lognormal, geom q | **Hard**: needs covariance natural-scale form (new `DATA_MATRIX`). Arith-q DONE. |
| ATS index | 9.572 | `Σ(log(o+0.01)−log(p+0.01))²/(2·lvarb)`, q=exp(log_q_ats) | lognormal +bias | bias DONE; **need +0.01 offset + per-obs lvarb** (≈ `index_ll_type=1`) |
| ATS age-1 | 11.169 | lognormal, **geometric q**, **drop last obs**, σ=1 | index slot | special form; geom-q via `Analytical`, drop-last + σ=1 |
| CPUE | 1.127 | `Σ(o−p)²/(2·var)`, **natural scale**, q=exp(log_q_cpue) | lognormal | **need natural-scale normal** (`index_ll_type=2`) |
| AVO | 9.223 | `Σ(o−p)²/(2·var)`, natural scale, uses **ATS sel**, q=exp(log_q_avo) | lognormal | natural-scale normal + ATS-sel mirror (mirror already used) |
| Comp FSH | 167.463 | `−sam·Σ o·log(p+0.001)` (MN_const=0.001) | `MultinomialAFSC` (offset **1e-5**) | use `Comp_loglike="MultinomialAFSC"`; **offset 1e-5 vs 1e-3** (tiny; optionally route `comp_offset`) |
| Comp BTS | 1060.833 | same, `sam=floor(sam_bts)` | same | same; note floor on sample size |
| Comp ATS | 30.213 | same, **ages `mina_ats:nages` only** | same | confirm ATS age-range restriction |
| Sel shape FSH | 15.188 | `domFish·Σ(decreasing)²`, **domFish=3.0**, ages 6:(n_sel−1) | `Sel_curve_pen1` | **set Sel_curve_pen1 = 3.0** (bridging used 12.5!) |
| Sel shape BTS | 0 | (logistic ⇒ 0) | 0 | matches |
| Sel shape ATS | 0.420 | `selATS·Σ(increasing)²`, selATS=1, ages 5:(n_sel−1) | sign-based, `Sel_curve_pen1=−1` | weight/age-range tune |
| Sel dev FSH | 124.207 | curvature `selCFsh/nch·Σsdiff²` (selCFsh=1, nch=59) + RW (σ 0.5, 1.9 @ '19/'20) | RW + curvature | match σ schedule (1.9 in two years!) and nch=59 |
| Sel dev BTS | 65.025 | realized-logsel RW (selVarbts=2, ages 3:14) + age-1 dev RW (×8) | LogisticPM penalty | bridging matched 31.74 vs target 65.03 → **revisit** |
| Sel dev ATS | 7.461 | curvature (selTATS=1) + RW (σ 0.138) | injected | matches in fwd pass |
| Rec dev | 21.591 | **`1.0·Σ(d−mean(d))²`** (mean-centered, no σ) | `dnorm(d, −σ²/2, σ)` | **AMAK rec-mode** needed |
| Init dev | 2.827 | **`0.1·Σ(d−mean(d))²`** | `dnorm(d, −σ²/2, σ)` | **AMAK rec-mode** needed |
| SR penalty | 9.778 | `0.5·Σ(SRresid+σ²/2)²/σ² + n·log σ`, **drop 1979**, over **1978:2022**, ×srrPrior | `dnorm(logR, logR_hat, R_sd)` | **AMAK SR-mode** needed |
| avgsel | 0.187 | `10·(avgsel_fsh² + avgsel_ats²)` (once) | `2.0·avgsel²` per year | **weight 10, once** |
| Priors | 21.025 | steepness Beta(14.93,14.93) | dbeta | **constant** (steepness fixed) → ignorable for MLE |
| Fpen | 9.343 | `Σ(F_dev−mean)²` | (Rceattle handles F differently) | **SKIP** per your instruction |

### RTMB constants (locked down)
`catBio=200` · `MN_const=0.001` · `domFish=3.0` · `selCFsh=1` · `selATS=1` ·
`selCurv=1` · `selTATS=1` · `selVarbts=2` · `age1_sigma_ats=1` · `omitSR=2`
(endyr_est=2022) · `omit78=1` (drop 1979) · `srrPrior=1` · `srprior_a=srprior_b=14.93`
· `DoCovBTS=1` · `do_bts_bio=1` · `sigr` fixed (phase −6) · `steepness=0.67` fixed ·
fishery `sel_ch_sig=0.5` except **1.9 in 2018–2019**.

---

## Suggested changes (prioritised) — for your review

These are the structural items I deliberately left for you because they add public
surface to a released package and involve parameterisation choices:

1. **AMAK recruitment likelihood mode** (biggest dynamics lever). Add a switch
   (e.g. `rec_ll_mode`) that replaces slots 9/10/11 with:
   - init dev: `0.1·Σ(d−mean d)²`; rec dev: `1.0·Σ(d−mean d)²` (mean-centered,
     fixed weights — independent of `R_sd`);
   - SR: `0.5·Σ_{y∈1978:endyr−omitSR, y≠1979}(resid + σ_R²/2)²/σ_R² + n·log σ_R`,
     `resid = log R − log SRR(SSB_{y−1})`, ×`srrPrior`.
   This is the dominant driver of the early-year SSB scale difference.

2. **Index likelihood family** `index_ll_type` (per-fleet `fleet_control` column),
   building on the `index_catch_bias` toggle I added:
   - `0` lognormal + bias (current default);
   - `1` lognormal, no bias, `+offset` inside log, per-obs variance (**ATS**);
   - `2` natural-scale normal `Σ(o−p)²/(2 var)` (**CPUE/AVO**);
   - `3` natural-scale normal **with covariance** `0.5 rᵀΣ⁻¹r` (**BTS**) — needs a
     new `DATA_MATRIX` (`inv_bts_cov`) input wired through `read_data`/`rearrange`.
   The covariance BTS is the single largest new structure.

3. **avgsel penalty**: weight `10`, accumulated **once** (not per year) on the base
   coefficients' `log(mean(exp(coffs)))`.

4. **Comp offset**: route the existing `comp_offset` into the `MultinomialAFSC`
   (`case -1`) branch (currently hard-coded `1e-5`) so pollock can use `1e-3`.
   Gate it so existing `MultinomialAFSC` fits don't change.

5. **Config (no code) to match RTMB**: fishery `Sel_curve_pen1 = 3.0` (not 12.5);
   catch `Log_sd = 0.05` + `index_catch_bias = 0`; BTS `Catchability =
   "AnalyticalArith"`; fishery sel `sel_ch_sig` schedule with 1.9 in 2018–2019.

6. **`initMode`**: RTMB uses **free** initial devs (`log_initage = log_initdevs`),
   i.e. Rceattle `initMode = 0` — but the handoff notes `initMode=0` doesn't
   converge in Rceattle here. To match RTMB *exactly* this must be resolved
   (it also shifts early-year scale). Worth a focused look.

---

## Why machine-precision verification is hard right now (and how to close it)

- `base.rds` ships only `$report` + `$metadata` — **not the RTMB parameter
  vector**, so I can't inject the exact RTMB MLE into Rceattle for a clean
  component-by-component forward-pass check.
- The forward-pass harness I have injects the **ADMB** MLE (`ADMB/m23/pm.par`),
  which reproduces ADMB SAFE (~3 %) but **not** RTMB (`base.rds` is a different
  optimum), so its absolute `jnll` values aren't the RTMB targets.
- **Recommended fix:** save the RTMB **parameter list** (and ideally `obj$report()`
  at the MLE) from `RTMB/R/Run_rpm.R` / `config.R` into `base.rds` (or a sidecar).
  With the RTMB params in hand, the `_rceattle_fwdpass.R` harness can be pointed at
  them and each component matched to RTMB to machine precision deterministically —
  the right way to drive the remaining switch work.
- The pollock **estimation** fit (`estimateMode=0`) currently NaN-floods during
  phasing with the bridging Model-2 config and is slow/unstable; I would not rely
  on it for verification until the objective forms are matched and `initMode` is
  resolved.

---

## Scratch files I left (all in this folder, none committed)

- `_rceattle_fwdpass.R` / `.rds` — forward-pass harness (injects ADMB MLE) + result.
- `_rceattle_fwdpass_prechange.rds` — pre-change baseline (for the 0e+00 regression check).
- `_verify_switches.R` — recompiles, proves default = pre-change, shows switch effects.
- `_rceattle_compare.R` / `.log` — full-fit comparison harness (the NaN-flooding estimation run; left for reference).

Package edits are in the `Rceattle/` working tree (uncommitted, per your instruction).
Run `cd Rceattle && git diff` to review; `git stash` to shelve if you want a clean base.

---

## Suggested next session (my recommendation)

1. You: confirm `base.rds` is the target and export the RTMB **parameter vector**.
2. Me: re-point the forward-pass harness at the RTMB params → exact per-component diff.
3. Me: implement items 1–4 above as additive switches, each verified to its RTMB
   component value, then validate the full `estimateMode=0` fit converges onto
   `base.rds`. Items are independent, so we can land them one at a time.
