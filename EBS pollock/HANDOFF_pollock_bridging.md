> ## ⚠️ UPDATED 2026-07-15 — read `HANDOFF_admb_match_session2.md` first
>
> **The forward-pass claim below is WRONG and must not be relied on.** This document
> says the forward pass "matches the population trajectory ... exactly". It does not
> test the dynamics at all: `_rceattle_fwdpass.R` sets **`estDynamics = 1`, which FIXES
> N from `NByageFixed` instead of computing it**. N was *injected*, so the ~6-sig-fig
> agreement only ever showed that the likelihood is right *given* N — never that the
> dynamics reproduce. Any conclusion resting on "the dynamics are proven identical" is
> void. A valid forward pass requires `NByageFixed` refreshed from
> `ADMB/m23_rceattle/pm.rep`; it remains the cleanest model-vs-optimizer split and is
> **still to be done**.
>
> Also superseded: the ADMB target is now `ADMB/m23_rceattle` (structurally aligned to
> Rceattle), **not** `ADMB/m23`. Reference: **1224 active params, objective
> 740.525106862990, max grad 3.77e-04**.

# Handoff: 2024 EBS pollock ADMB ("pm"/AMAK) → Rceattle bridging

Last updated: 2026-06-02.

**Goal**: reproduce the 2024 EBS pollock ADMB assessment (`ADMB/m23/pm.tpl`,
Ianelli's AMAK "pm" model) in Rceattle — (1) a **forward pass** with parameters
fixed to the ADMB MLEs that matches the population trajectory and every
selectivity-penalty likelihood **exactly**, and (2) an **estimation model** that
uses the same selectivity structure and is compared to ADMB. Mirrors the GOA
Northern rockfish / BSAI Alaska plaice bridging scripts.

Scripts in `EBS pollock/`:
- `2024 EBS pollock bridging.R` — Model 1 (forward pass) + Model 2/3 (estimation).
- `2024 EBS pollock.R` — production estimation run + ADMB SAFE comparison.

The structural-difference catalogue lives in the **header of the bridging
script** (numbered diffs #1–#13). This handoff is the work-order summary.

---

## Current state — one paragraph

**Forward pass is exact**: SSB / R / Catch match ADMB to ~0.0001 % (6 sig figs),
and **all three time-varying selectivity penalties now match ADMB to the reported
digits** (see table). BTS/ATS index predictions match 0 %, AVO is finite and on
scale (~20 %). This required three new Rceattle selectivity capabilities
(`LogisticPM`, sign-based non-parametric shape penalty, and per-fleet
penalty-window controls) plus two AVO fixes. **The estimation model (Model 2)**
is configured with the same selectivity forms (Fishery/ATS `NonParametricPM`-RW,
BTS `LogisticPM`-RW, AVO mirrors ATS) and **builds + starts phasing cleanly**,
but a full converged run + ADMB comparison is **still pending** (it is the slow
step and was stopped mid-phasing).

---

## TL;DR — selectivity likelihood parity (forward pass)

| Component (ADMB)        | ADMB value | Rceattle | Match |
|-------------------------|-----------:|---------:|:-----:|
| Fishery `sel_like(1)` (shape)     | 12.606   | 12.606   | ✅ |
| Fishery `sel_like_dev(1)` (curv+RW)| 120.762  | 120.76   | ✅ |
| BTS `sel_like(2)`                 | 0        | 0        | ✅ |
| BTS `sel_like_dev(2)`             | 31.7415  | 31.741   | ✅ |
| ATS `sel_like(3)` (ascending)     | 2.35936  | 2.359    | ✅ |
| ATS `sel_like_dev(3)`             | 6.70429  | 6.704    | ✅ |
| SSB / R / Catch trajectory        | —        | ~0.0001 %| ✅ |
| BTS / ATS index prediction        | —        | 0 %      | ✅ |

Data likelihoods (survey index, catch, age comps, recruitment) are **not**
expected to match — lognormal-vs-normal index likelihood, multinomial
constants/Francis weights, ±σ²/2 rec bias correction (diffs #7/#9/#10/#13).
`wt_like` = 6345 (ADMB's empirical weight-at-age sub-model) has no Rceattle
counterpart (Rceattle takes weight-at-age as data).

---

## Selectivity mapping (the heart of the work)

| Fleet | ADMB form | Rceattle | Key config |
|-------|-----------|----------|-----------|
| Fishery | non-par coffs + RW devs | `NonParametricPM` (9) RW | pen 12.5 / (1/60) / 1, σ=0.5 |
| BTS | logistic + free age-1, mid-age, mult. devs | `LogisticPM` (11) RW | realized-logsel RW (w=2, ages 3–14) + age-1 dev RW (w=8); `Sel_start_year`=1982 |
| ATS | non-par, ascending-constrained, per-year renorm | `NonParametricPM` (9) RW, **inject realized log-sel** | `Sel_curve_pen1=-1` (penalize increasing), pen2=1, pen3=0, σ=0.138, `Sel_start_year`=1994, `Sel_pen_first_age`=2, `Bin_first_selected`=1 |
| AVO | uses ATS sel × estimated q | empirical = ATS sel copy (fwd) / mirror ATS (est) | analytical q in fwd pass |

---

## New Rceattle package features (compiled in; all tests pass)

Added in `Rceattle/` (recompile = `rm -f src/TMB/ceattle_v01_11.{o,so} && touch
src/TMB/ceattle_v01_11.cpp && R CMD INSTALL`):

1. **`LogisticPM` (sel_type 11)** — AMAK BTS form: logistic at mid-age
   `age_vector(j)=j+0.5` (x=`bin+1.5`), multiplicative slope/inflection devs, free
   age-1 log-selectivity (in `sel_inf[2]`/`sel_inf_dev[2]`). Never normalizes →
   `Sel_norm_bin1/2` repurposed as the penalty age-range. (`0-constants.R`,
   `selectivity.hpp` case 11, `1-build_params`/`2-build_map`/`4-data_check`.)
2. **Sign-based non-parametric shape penalty** — `Sel_curve_pen1 ≥ 0` penalizes
   DECREASING selectivity-at-age, `< 0` penalizes INCREASING (ATS ascending),
   `|weight|` = strength. Applied to type 9 (and 11's age-1 form).
3. **`Sel_start_year`** (`flt_sel_start_yr`) — per-fleet selectivity start year;
   penalties start the year after, excluding pre-survey years + the boundary jump.
4. **`Sel_pen_first_age`** (`flt_sel_pen_first_age`) — first age of the shape
   penalty, decoupled from `Bin_first_selected` (ATS: age-1 selected for the index
   but shape constraint starts at mina_ats=2).
5. **`flt_sel_lead`** — selectivity penalty accumulated once for mirrored fleets
   (skip if an earlier fleet shares `Selectivity_index` AND type). Keeps the
   forward pass right (AVO empirical/type-0) and the estimation right (AVO/ATS
   both type-9, penalty counted once).

Tests: `tests-Selectivity/test-logisticpm-selectivity.R` (3), updated
`test-nonparametricpm-selectivity.R` (4), `test-nonparametric-selectivity.R`
(348) — all pass.

---

## AVO fix (forward pass)

`pred_avo` uses ATS selectivity (`log_sel_ats`, pm.tpl L2834), but `emp_sel`
ships **no AVO rows** → AVO sel = 0 → predicted = 0 → **Inf** index likelihood.
Fix: (a) copy the ATS `emp_sel` rows onto AVO; (b) give AVO **analytical q** in
the forward pass — Rceattle survey biomass differs from ADMB by a ~1e3 unit scale
that analytical q absorbs for BTS/ATS but a fixed `log_q_avo` exposes (1000×-off
prediction). AVO is now finite, ~20 % (acoustic index, more scatter).

---

## Estimation model (Model 2) — status

Configured (`est_data`): Fishery `NonParametricPM`-RW (σ=0.5), BTS `LogisticPM`-RW,
**ATS `NonParametricPM`-RW** (ascending, estimated not injected), **AVO mirrors
ATS** (`Selectivity_index`=2, both type 9 so the build_map type-check passes;
shared penalty counted once via `flt_sel_lead`). `initMode`=2, `sigma_rec_prior`
=0.707, M fixed at the ADMB age schedule.

- **Builds and starts phasing with no errors / no type-mismatch warnings.**
- **Not yet converged** — stopped mid-phasing (the new ATS RW adds ~8 coffs × ~31
  dev years; phasing is slow and silent).
- Prior baseline (before this session's ATS/AVO work, with BTS logistic + ATS
  time-invariant): SSB corr ~0.86, R ~0.97, Hessian not positive-definite
  (weakly-identified fishery sel devs), ~1.4× early-year SSB scale offset.

---

## Next steps (in priority order)

1. **Let Model 2 finish.** Run `2024 EBS pollock bridging.R` to completion;
   record convergence (max gradient), Hessian status, SSB/R correlation vs ADMB,
   and whether the new ATS non-par form changes the prior ~0.86 SSB corr.
2. **Verify the mirrored ATS/AVO penalty is counted once** in the estimation jnll
   (confirm `flt_sel_lead` zeroes the mirror fleet's `jnll_comp` rows 5/6).
3. **Model 3** (estimate age/time-invariant M) once Model 2 is healthy.
4. **Document the residual estimation gap** in the script header — the early-year
   SSB scale (rec/init ±σ²/2 bias correction vs ADMB centered-at-0) and the
   survey-q estimator (geometric vs arithmetic mean obs/pred) are the known
   drivers; both were chosen to document-not-implement.
5. (Optional) Port the analogous **`compare_params.R`** harness (pm.std/pm.tpl vs
   Rceattle `build_map`/`build_bounds`) to diff parameter counts/bounds — noting
   the ADMB weight-at-age sub-model (`coh_eff`/`yr_eff`/`d_scale`, ~148 params +
   `wt_like`) and projection module have no Rceattle counterpart.

---

## Reproduce / resume quickly

```r
# from EBS pollock/
source("2024 EBS pollock bridging.R")   # Model 1 prints the parity table; Model 2 estimates
```

Forward-pass spot check (selectivity penalties): the script's Model-1 block; ADMB
targets are in `ADMB/m23/pm.rep` (`sel_like`, `sel_like_dev`) and `control.dat`
(`ctrl_flag`: 15/22 = ATS shape/curvature, 19/26 = BTS penalty branch).
