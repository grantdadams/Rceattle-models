> ## ⚠️ UPDATED 2026-07-15 — read `HANDOFF_admb_match_session2.md` first
>
> **Correction:** this document calls `HANDOFF_pollock_bridging.md` "(the ADMB→Rceattle
> forward-pass bridge, still valid)". **It is not valid.** That bridge sets
> `estDynamics = 1`, which fixes N from `NByageFixed` rather than computing it — so its
> "exact" trajectory match tested the likelihood *given* N, never the dynamics. See the
> banner on that file.
>
> Superseded since: the ADMB target is now `ADMB/m23_rceattle` (structurally aligned to
> Rceattle — weight submodel off, BTS dev_vectors → plain vectors, mean recruitment,
> F freely estimated + F penalty removed), **not** `ADMB/m23`. Reference: **1224 active
> params, objective 740.525106862990, max grad 3.77e-04**. Parameters, variances and
> penalties are now reconciled with Rceattle **exactly** (1218 = 1224 − 6); the fit
> still fails. Two real Rceattle bugs were found and fixed along the way.

# Handoff: RTMB vs ADMB EBS pollock, and what Rceattle should match

Last updated: 2026-06-19. Supersedes the BTS conclusion in `OVERNIGHT_REPORT.md`
(see "Correction" below). Companion to `HANDOFF_pollock_bridging.md` (the
ADMB→Rceattle forward-pass bridge, still valid).

---

## TL;DR (the decisions that matter)

1. **Target the ADMB SAFE assessment, not `base.rds`.** The ADMB run RTMB ports
   (`ADMB/m23/pm.rep`, "for_rtmb") is **byte-identical to the 2024 SAFE SSB**
   (`Data/2024_ADMB_estimate.xlsx`): mean |%diff| = **0.00%**. `base.rds` (the
   RTMB fit) sits **~18% off** that (early SSB 607 vs 1609), and is only **one of
   several RTMB local optima** — a poor machine-precision target (see §3).

2. **ADMB and RTMB are the same model for every data likelihood.** Applying the
   RTMB likelihood formulas to ADMB's own predicted state reproduces ADMB's
   reported components to ~1e-4 for **7 of 8** components (§2). The lone formula
   difference is the **BTS survey-biomass likelihood** (covariance MVN vs IID).

3. **But the BTS likelihood is NOT what drives the trajectory gap** (correction of
   the overnight report). Switching RTMB BTS from MVN→IID barely moves the fit
   (§3). The ~18–25% SSB divergence is an **estimation / early-year initialization
   artifact plus RTMB local-optimum behaviour**, not a structural ADMB-vs-RTMB
   difference.

4. **Consequence for Rceattle:** you do **not** need the hard covariance-BTS
   likelihood (a `DATA_MATRIX inv_bts_cov` + `0.5 rᵀΣ⁻¹r`). ADMB uses IID normal
   (`control.dat: DoCovBTS = 0`), which Rceattle's standard normal/lognormal index
   likelihood already covers. That removes the biggest blocker from the overnight
   plan. The remaining work is (a) match ADMB SAFE's data likelihoods (mostly
   done in the bridge) and (b) resolve the early-year recruitment/initialization
   scale (§5, the real open question).

---

## 1. What `base.rds` is, and how to run the RTMB model locally

`base.rds` = the RTMB port of the AMAK "pm" pollock model (`RTMB/`, Jim Ianelli).
It stores only `$report` + `$metadata` — **not the parameter vector.**

**The parameter object** (`RTMB/R/config.R`):
```r
parms <- read_pars(pm.par)                          # ADMB MLE = starting values
parms$steepness <- 0.67                             # fixed
parms <- add_fishery_selectivity_parameters(parms, data)
map_obj <- create_map_from_par(parms, parms, exact_names = fixed_params, ...)
obj <- MakeADFun(rpm, parms, map = map_obj)         # 1350 estimated pars
```
Fit = `nlminb(obj$par, obj$fn, obj$gr)` (`analysis/Run_rpm.R`). `base.rds` is
written by `R/write_output.R` as `list(report = rpm(parms), metadata = ...)` — so
its exact params are unrecoverable from the file; only `$report` survives.

**It runs locally.** The env is wired: `RTMB/.pollock_root/admb/runs/{for_rtmb,data}`
symlink to `ADMB/m23` and `ADMB/data`; `ebswp`/`RTMB`/`TMBhelper`/`tidyverse` are
installed (R 4.5.1, RTMB 1.9). To run: set env vars and source config, stopping
before `MakeADFun` if you want to intervene:
```r
Sys.setenv(RTMB_EBSWP_ROOT = "<RTMB>", POLLOCK_ROOT = "<RTMB>/.pollock_root")
setwd("<RTMB>")
cfg <- readLines("R/config.R"); eval(parse(text = paste(
  cfg[seq_len(grep("MakeADFun", cfg)[1]-1)], collapse="\n")), envir=globalenv())
```
Recover the MLE param list after a fit with `obj$env$parList(obj$env$last.par.best)`.

**Two gotchas (both handled in the scratch scripts):**
- `Rpm.R` has leftover developer diagnostics (`NLL - pm$NLL`, `age_like/pm$age_like`
  near lines 429/476) that compare to the ADMB rep `pm`. The local `pm.rep` has no
  `NLL` section, so `pm$NLL`/`pm$age_like` are `NULL`; harmless in plain R but under
  AD taping `advector(NULL)` **crashes MakeADFun** ("type=NULL; target=double").
  Fix: inject `pm$NLL <- rep(1,20); pm$age_like <- rep(1,3)` before `MakeADFun`.
- `config.R` calls `rm(list = ls())` — set paths via `Sys.setenv`, not variables.

---

## 2. Pure structural gap: RTMB formula on ADMB's state vs ADMB's reported value

Same state (ADMB's predicted `pm$...`), two formula sets. This isolates *formula*
from *fit*. (Script: `RTMB/_structural_gap.R`.)

| Component | RTMB formula | ADMB reported | verdict |
|---|---:|---:|:--|
| catch | 0.9519986 | 0.9520790 | identical |
| age_like[fsh] | 145.6621 | 145.6650 | identical |
| age_like[bts] | 166.5458 | 166.5460 | identical |
| age_like[ats] | 30.42835 | 30.42830 | identical |
| CPUE | 1.754284 | 1.754280 | identical |
| AVO | 9.974975 | 9.975040 | identical |
| ATS | 9.407026 | 9.407030 | identical |
| **BTS (biomass)** | **1097.58** (MVN) | **2.4e-5** | **only difference** |

**7/8 data likelihoods are the same model to ~1e-4.** Method validated: the BTS
formula on RTMB's *own* state reproduces `base.rds`'s `bts_like` exactly
(35.117 vs 35.117).

**RTMB constants (locked down, from `RTMB/R/utilities.R` + `base.rds`):**
`catBio = 200` (⇒ catch σ = 0.05) · `MN_const = 0.001` · `domFish = 3.0` (not 12.5!) ·
`selCFsh = 1` · `selATS = selCurv = selTATS = 1` · `selVarbts = 2` · `age1_sigma_ats = 1` ·
`omitSR = 2` (endyr_est = 2022) · `omit78 = 1` (drop 1979) · `srrPrior = 1` ·
`srprior_a = srprior_b = 14.93` · `steepness = 0.67` fixed · `sigr` fixed (phase −6) ·
fishery `sel_ch_sig = 0.5` except **1.9 in 2018–2019**.

---

## 3. Correction: the BTS likelihood does NOT drive the divergence

`control.dat` has **`DoCovBTS = 0`** (IID normal) and `do_bts_bio = 1`. But **the
RTMB code ignores it** — `Rpm.R:335` and `utilities.R:532` both **hardcode
`DoCovBTS = 1`** (covariance MVN). So `base.rds` used the MVN regardless of the
control flag. (I changed `Rpm.R:335` to `DoCovBTS = DoCovBTS` so it honours the
flag — revert if unwanted.)

Re-fitting RTMB both ways (`RTMB/_test_iid_bts.R`, `TMBhelper::fit_tmb`,
Newton-polished):

| | total nll | bts_like | SSB 1964 | SSB 2024 | vs ADMB SAFE |
|---|---:|---:|---:|---:|---:|
| **ADMB SAFE** (= for_rtmb) | — | ≈0 | **1609** | **2951** | — |
| RTMB IID (DoCov 0, your control) | 8249 | 164 | 658 | 4135 | 31% |
| RTMB MVN (DoCov 1, base.rds) | 8064 | 90 | 660 | 3849 | 25% |
| RTMB `base.rds` | 7951 | 35 | 607 | 3347 | 18% |

All three RTMB fits land at SSB₁₉₆₄ ≈ 610–660 vs ADMB **1609** — the BTS form
barely matters. Note the **three different RTMB optima** (7951 / 8064 / 8249): RTMB
is optimizer-sensitive here and does **not** reproduce `base.rds` on re-fit even
with the same code+data. That is why `base.rds` is a bad machine-precision target.

---

## 4. Current Rceattle package state (branch `dev-ebs-pk`)

The overnight working-tree edits (`index_catch_bias`, `AnalyticalArith` q, an OSA
`rearrange_data` fix) were **superseded/reset** — the tree is clean. Equivalent
functionality now lives in the package proper:
- **Bias correction:** `fit_control(bias_adjust_obs=, bias_adjust_proc=)` toggles
  the lognormal `-σ²/2` on the observation (index/catch) and process
  (recruitment/init/steepness-prior) likelihoods. This is the clean version of the
  overnight `index_catch_bias` switch — set `bias_adjust_obs = FALSE` for the
  AMAK/RTMB log-SSQ index & catch form.
- **Comp offset:** `fit_control(comp_offset=)` (default 1e-5; set 1e-3 to match
  RTMB `MN_const`; use `Comp_loglike = "MultinomialAFSC"`).
- **Still absent** (needed only if a future target requires them): an
  arithmetic-mean analytical q (`q = mean(obs)/mean(pred)`; RTMB/AMAK BTS uses it
  but note §3 — BTS is IID and not the driver), and any covariance BTS likelihood.
- **Verify:** run the full suite on the current tree
  (`NOT_CRAN=true Rscript -e 'devtools::test()'`). Overnight I found+fixed 4
  `test-likelihood-osa-residuals.R` failures caused by `rearrange_data()` erroring
  on a raw `data_list` missing the pollock-era `Sel_*` columns; confirm whether
  that regression is present on the current tree.

---

## 5. The real open question (next session)

Why does the RTMB fit land ~2.6× below ADMB SAFE in early years (607 vs 1609),
converging late? The data likelihoods are identical (§2) and BTS is ruled out
(§3), so the driver is in the **penalty / initialization block**, which the §2
test can't probe (those terms need parameters, absent from `pm.rep`/`base.rds`):
- **Initialization:** RTMB sets `N[styr, 2:nages] = exp(log_initdevs)` (free init
  devs) with a `0.1·Σ(initdev − mean)²` penalty; ADMB's init parameterization /
  penalty differs. This is the prime suspect for the early-year scale.
- **Recruitment penalty:** RTMB `1.0·Σ(recdev − mean)²` (mean-centred) + the
  AMAK BH-SR term over 1978:2022 dropping 1979; check the `log_avgrec` handling
  and the last-year `N[endyr,1] = exp(log_rec_devs[i+1])` quirk (no `log_avgrec`).
- **Local optima:** even isolating the above, expect optimizer sensitivity;
  multi-start or Newton polishing may be required to land a stable RTMB solution.

**Recommended path:** stop chasing `base.rds`. Match Rceattle → **ADMB SAFE**
(the official run; `HANDOFF_pollock_bridging.md`'s forward pass already hits it to
~3%). Use `bias_adjust_obs = FALSE` + `comp_offset = 1e-3` +
`Comp_loglike = "MultinomialAFSC"` + the selectivity forms from the bridge, and
BTS as a standard IID normal index (matching `control.dat: DoCovBTS = 0`). Then
the only remaining Rceattle↔ADMB gap should be the survey-q estimator (geometric
vs arithmetic mean; `HANDOFF_pollock_bridging.md` diff #13) and the init/rec
parameterization — both well-characterised.

---

## 6. Scratch scripts left in the workspace (none committed to `RTMB/`)

In `EBS pollock/RTMB/` (Jim's repo — delete before any PR there):
- `_extract_rtmb_mle.R` — run RTMB, extract full `parList` MLE + report (→ `../rtmb_mle.rds`).
- `_structural_gap.R` — §2 table (RTMB formula on ADMB state vs ADMB report).
- `_test_iid_bts.R` — §3 IID-vs-MVN re-fit comparison.
- `_compare_admb_rtmb.R`, `_debug_rtmb.R`, `_debug_makeadfun.R`, `_debug_tb.R` — diagnostics.
- **`R/Rpm.R:335` edited** (`DoCovBTS = 1` → `DoCovBTS = DoCovBTS`); revert if unwanted.

In `EBS pollock/`:
- `OVERNIGHT_REPORT.md` — the earlier report (its BTS-as-driver conclusion is
  **corrected here**; the per-component spec + RTMB constants remain valid).
- `_rceattle_*.R/.rds` — Rceattle forward-pass / comparison harnesses.
- `rtmb_mle.rds` — an RTMB MLE (nll 8064, MVN) — NOT `base.rds`'s optimum.
- `base.rds` — restored from the canonical `RTMB/base.rds` (identical, tot_like
  7951.457) after it went missing mid-session.
