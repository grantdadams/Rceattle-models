# m23_rceattle_full — ADMB m23 fully aligned to Rceattle (stage 2)

**This is the stage-2 reference, and the one the R scripts read**
(`AD <- "ADMB/m23_rceattle_full"` in `../../2024/02-bridge.R` and
`../../2024/03-model-comparison.R`). It carries the stage-1 structural alignment
described below **plus** the likelihood, data, and parameter alignment. The full
catalogue is the reconciliation log at the top of
`../../2024/03-model-comparison.R`: **S1-S4** structural (stage 1, `../m23_rceattle`),
**L1-L7** likelihood (stage 2, here), **D1-D8** Rceattle-side data conversions.

A copy of `../m23` (the 2024 EBS pollock ADMB "pm"/AMAK assessment, `DoCovBTS = 1`)
with the **structural differences vs Rceattle removed**, so the two codebases can be
driven to agreement on estimated **SSB and recruitment**.

Rationale: the forward pass already proves the two models' *dynamics* are identical
(SSB/R/catch reproduce to ~6 sig figs with parameters fixed to the ADMB MLEs). The
remaining disagreement in *free estimation* comes from a handful of structural /
parameterization differences. Rather than force Rceattle to imitate a more flexible
ADMB, this run strips ADMB to the common core both models share.

Shared data lives in `../data/` (paths in `pm.dat` are relative and unchanged).

---

## Modifications

### 1. Weight-at-age submodel OFF  (`control.dat`)
| key | m23 | m23_rceattle |
|---|---:|---:|
| `phase_coheff` | 1 | **-1** |
| `phase_yreff`  | 1 | **-1** |

ADMB estimates weight-at-age via a cohort/year-effect submodel (`coh_eff`, `yr_eff`,
`d_scale`, ~148 parameters) contributing `wt_like = 6344.87` — **89% of m23's
`tot_like` (7093.9)**. Rceattle has no such submodel: it takes weight-at-age as fixed
data. Setting both phases negative fixes the weight parameters at their
`INITIALIZATION_SECTION` values (`L1`, `L2`, `log_alpha`, `log_K = -0.1356`), so
weight-at-age is effectively data, as in Rceattle. `phase_d_scale` follows
`phase_coheff` automatically (`pm.tpl` ~L868).

Note: the submodel is largely *separable* from the hindcast (`wt_ssb`/`wt_fsh` are
`init_matrix` **inputs**; the submodel drives `wt_like` and the *projection* weights
`wt_fut`), so this mainly makes `tot_like` comparable rather than moving SSB/R.

### 2. BTS selectivity deviations: dev_vector -> plain vector, first year fixed  (`pm.tpl`)
```
- init_bounded_dev_vector sel_slp_bts_dev(styr_bts,endyr_r,-5,5,phase_logist_bts_dev+1)
- init_bounded_dev_vector sel_a50_bts_dev(styr_bts,endyr_r,-5,5,phase_logist_bts_dev)
- init_bounded_dev_vector sel_age_one_bts_dev(styr_bts,endyr_r,-5,5,phase_age1devs_bts)
+ init_vector sel_slp_bts_dev_est(styr_bts+1,endyr_r,phase_logist_bts_dev+1)
+ init_vector sel_a50_bts_dev_est(styr_bts+1,endyr_r,phase_logist_bts_dev)
+ init_vector sel_age_one_bts_dev_est(styr_bts+1,endyr_r,phase_age1devs_bts)
+ vector sel_slp_bts_dev(styr_bts,endyr_r)      // full range, yr 1 = 0
+ vector sel_a50_bts_dev(styr_bts,endyr_r)
+ vector sel_age_one_bts_dev(styr_bts,endyr_r)
```
ADMB's `init_bounded_dev_vector` carries a **sum-to-zero constraint** (plus ±5 bounds)
that has no TMB/Rceattle equivalent; Rceattle instead estimates lightly-penalized
deviations and **fixes the first one** (`build_map`, LogisticPM + RandomWalk) to pin
the level. This change reproduces Rceattle exactly: no bounds, no sum-to-zero, first
year excluded from estimation and held at 0.

The full-range `*_dev` vectors are rebuilt each iteration in `PROCEDURE_SECTION`
(`initialize()` then copy the estimated years in), so **all downstream code is
unchanged** — `compute_selectivity()`, the age-1 loop, and the random-walk penalties
still see `styr_bts..endyr_r`. The `active()` checks now test the `*_est` parameters
(`active()` only works on `init_` objects).

These are the only **active** selectivity dev_vectors: `phase_logist_bts = 2` (BTS is
logistic), while `phase_logist_fsh = -1` makes the fishery-logistic dev_vectors
(`sel_dif1_fsh_dev`, `sel_a501_fsh_dev`, `sel_trm2_fsh_dev`) inactive — the fishery
uses non-parametric `sel_devs_fsh`, an `init_bounded_matrix` (no sum-to-zero), so it
needs no change. `log_F_devs` remains a dev_vector (fishing mortality, not selectivity).

### 3. Recruitment -> mean recruitment  (`control.dat`)
| key | m23 | m23_rceattle |
|---|---:|---:|
| `SrType` | 1 (Ricker) | **3 (avg / mean recruitment)** |

`pm.tpl:77` — `SRR 1=ricker, 2 bholt, 3 avg`. m23 uses **Ricker**; Rceattle as
configured uses mean recruitment (`srr_fun = 0`), so `SrType = 3` aligns them. (The
alternative is to keep the SR curve and set Rceattle's
`build_srr(srr_fun = 0, srr_pred_fun = ...)`, which the Rceattle docs note is the
AMAK/Ianelli construction.)

### 4. Fishing mortality: freely estimated per year, no penalty  (`pm.tpl` + `control.dat`)
| what | m23 | m23_rceattle |
|---|---|---|
| `log_F_devs` | `init_bounded_dev_vector` (**sum-to-zero**, ±15) | **`init_bounded_vector`** (plain, ±15) |
| `log_avg_F` | estimated (phase 1) | **fixed (phase -1)** at -1.6 |
| `ctrl_flag(4)` (F-penalty weight, `control.dat` line 133) | 1 | **0** |

Rceattle estimates F **freely per year** (`PARAMETER_MATRIX log_F`;
`F = sel * exp(log_F(flt,yr))`) with **no** mean+deviation decomposition and **no F
penalty**. ADMB used `Fmort = mfexp(log_avg_F + log_F_devs)` with a sum-to-zero dev
vector *and* a regularizer `F_pen = norm2(log_F_devs)` weighted by `ctrl_flag(4) = 1`
(implied sigma = 1/sqrt(2) = 0.707) — that penalty is a genuine difference in the
objective, not a reparameterization, and it pulled early-year F toward the mean.

All three changes are required **together**: removing the sum-to-zero *and* the
penalty while leaving `log_avg_F` estimated would give n+1 parameters for n annual
F's — the level is perfectly confounded (add c to `log_avg_F`, subtract c from every
dev) and the Hessian is singular. Fixing `log_avg_F` leaves exactly one free
parameter per year; `log_F_devs` is then simply the per-year log-F as an offset from
the fixed mean, which is Rceattle's parameterization exactly.

Effect: `tot_like` 1041.68 -> **740.53** (the F penalty alone was worth ~301), and
SSB(1964) 1701.9 -> **1988.4**, R(1964) 6540.9 -> **7020.2** — i.e. it moves the early
years, which is precisely where ADMB and Rceattle disagreed.

The `if (current_phase()<3) fff += 10.*square(log(mean(Fmort)/.2))` term (~line 3522)
is left alone: it is an early-phase conditioner only ("Removed at the end"), so it
does not enter the final objective.

### 5. Supporting changes  (`pm.tpl`)
- `Est_Fixed_Effects_wts()` is now called **unconditionally**. With the weight phases
  negative, `active(coh_eff)/active(yr_eff)` are false, so the original guarded call
  would never run and `wt_pre` would stay uninitialized — breaking `wt_fut` (used by
  the MSY/F40 calculation every iteration). It is deterministic now that the weight
  parameters are fixed.
- `fff += wt_like` is now guarded by `if (active(coh_eff)||active(yr_eff))`. With the
  submodel off, `wt_like` is a **constant** (zero gradient — it cannot change the MLE);
  excluding it keeps `tot_like` directly comparable with Rceattle.

---

## Unchanged
Data, `DoCovBTS = 1` (covariance BTS survey likelihood), M schedule, the ctrl_flag
penalty weights (BTS realized-log-sel RW `ctrl_flag(26) = 2`, age-1 RW `8`), q forms,
composition likelihoods, and the initial-age cascade (already matches Rceattle:
`anchor·exp(-ΣM + init_dev)` + geometric plus-group + init-dev penalty σ=0.707).

## Build / run
```sh
export PATH=/usr/local/bin:$PATH
admb pm            # builds ./pm
./pm -nox -iprint 150
```
**`pm.par` from m23 is NOT compatible** — the parameter vector changed shape (the BTS
dev vectors lost one element each). Start from the `INITIALIZATION_SECTION` (no
`-ainp`/`-binp`), as above.

## Comparing
- vs `../m23`: `pm.rep` blocks `SSB`, `R`, `surv_like`, `tot_like`. Expect `tot_like`
  to drop by roughly `wt_like` (~6345) since the weight submodel no longer contributes.
- vs Rceattle: pair with the Rceattle config using `Index_loglike = "MVN"` +
  `Catchability = "AnalyticalArith"` + `index_cov = list(BTS = cov_2024.dat)`,
  mean recruitment, `initMode = "NonEquilibrium"`, M fixed. That configuration is
  built by `../../2024/01-build-data.R`; see codes D1-D8 in
  `../../2024/03-model-comparison.R`.

---

## Known quirks / follow-ups (added 2026-07-15)

- **`steepness` remains ACTIVE under `SrType = 3`.** Mean recruitment never uses it, so
  it is an inert parameter carrying (near) zero gradient, yet it is in `pm.std` and
  counts toward the 1224. ADMB still inverted its Hessian, so it must be pinned by a
  prior or bound (`init_bounded_number steepness(0.2, Steepness_UB)`). Consider setting
  `phase_steepness` negative in a future run for a cleaner comparison.
- **`rec_dev_future` (5)** is projection-only and has no Rceattle counterpart in a
  hindcast-only fit.
- Hence the comparable count is **1224 − 6 = 1218 = Rceattle exactly.**

## Verified reference output (this build)

```
pm.par header: Number of parameters = 1224
               Objective = 740.525106862990
               Max gradient = 0.000376778799600388
SSB(1964) = 1988.4  R(1964) = 7020.2  SSB(1978) = 1044.9  R(1978) = 24256
SSB(2024) = 3411.5  R(2024) = 18325.2
obs_catch(1978) = 979.431 -> pred_catch(1978) = 983.776   (F ~ 0.5)
```
`pm.std` lists only the **active** parameter set (sums to 1224). `pm.par` lists all
**2119 declared** including inactive — do not count it.
