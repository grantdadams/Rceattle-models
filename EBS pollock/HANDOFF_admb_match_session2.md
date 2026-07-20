# EBS pollock ↔ ADMB match — session 2

Continues `HANDOFF_mvn_covariance.md`. **Nothing committed, nothing pushed.**
Branch `dev-ebs-pk` (Rceattle) + `ADMB/m23_rceattle` (models repo).

---

## ▶ START HERE — how to resume the bridge

**Goal:** drive Rceattle's estimated SSB/R to match `ADMB/m23_rceattle` exactly.
Everything needed is in `bridge_session2/` (scripts rescued from the session
scratchpad — paths already rewritten to be repo-relative).

### The target (aligned ADMB — already built and converged)
```
ADMB/m23_rceattle/pm.par header:
  Number of parameters = 1224   Objective = 740.525106862990   Max gradient = 3.77e-04
  SSB(1964) = 1988.4   R(1964) = 7020.2   SSB(2024) = 3411.5   R(2024) = 18325.2
  SSB(1978) = 1044.9   R(1978) = 24256    (the year Rceattle blows up)
To rebuild:  cd ADMB/m23_rceattle && export PATH=/usr/local/bin:$PATH && admb pm && ./pm -nox -iprint 150
             (pm.par from m23 is NOT compatible — the BTS dev vectors changed shape)
```

### The Rceattle side
```sh
cd "Rceattle-models/EBS pollock"
export PATH=/usr/bin:$PATH                      # system toolchain first (TMB build)

# parameter counts only (fast, ~30 s) — should print 1218
Rscript bridge_session2/parbreak.R

# the actual fit (SLOW — 76 min, currently FAILS)
BAP=0 Rscript bridge_session2/match2.R
```
`bridge_session2/match2.R` is the canonical config. It already contains **all four
fixes** found this session; do not regress them:
1. `sel_inf[2]` init (fixed in the package, `R/2-build_params.R`)
2. `Sel_start_year` masking (fixed in the package, `R/3-build_map.R`)
3. `bias_adjust_obs = 0` (ADMB has no lognormal bias correction)
4. `Bin_first_selected[ATS/AVO] = 2` (ADMB `mina_ats = 2` → ATS = ages 2..8 = 7 bins)

### Current state (session 3): **RESOLVED — the fit converges**
```
Rceattle 1218 params = ADMB 1224 − steepness(1, inert) − rec_dev_future(5, projection)
phase=TRUE, estimateMode=0 : converges in < 4 min; all 1218 gradients ~3.5e-04
                             SSB cor 0.9683 (mean 9.7%) | R cor 0.9935 (mean 5.8%)
```
Root cause was the `Log_sd / Observation` line, NOT the optimizer and NOT the model spec.
The historical failure below (SSB(1978)=0, log_F at bound, 4593 s, `obj$gr()` dying) is kept
only as a record of the symptoms it produced.

### 🎯 SESSION 3 — **ROOT CAUSE FOUND AND FIXED: the `Log_sd / Observation` line**

**The fit did not fail. It was being asked to fit the AVO/ATS indices to five
decimal places.** One line, present in BOTH `match2.R:20` and
`2024 EBS pollock bridging.R:576`:

```r
index_data$Log_sd <- index_data$Log_sd / index_data$Observation   # "SD -> CV"  <-- WRONG
```

The xlsx index `Log_sd` is **already a CV / log-sd** — BTS 0.050–0.086, ATS 0.087–0.449,
AVO 0.113–0.561, CPUE 0.2. If they were SDs in observation units they would be O(1e2–1e3)
(observations are O(2e3–1e4)). ADMB's own diagnostics settle it independently:
**`sdnr_bts/ats/avo = 0.951 / 0.995 / 0.978`** (≈1 ⇒ correctly calibrated as-is).
Dividing by `Observation` (~1e3) produced CVs of **~1e-5**.

Effect, measured on the 140-param dynamics-only fit (`bridge_session2/dynfit.R`):

| | with the bug | line removed |
|---|---|---|
| objective | 813,982 | **3,564.16** |
| **index jnll** | **743,587** (AVO 624,585 + ATS 118,280) | **108.9** |
| iterations | 16,054 | **359** |
| **max\|grad\|** | **10.66** (`rec_pars`) | **4e-04** ← ADMB's is 3.77e-04 |
| runtime | 98.3 s | **6.8 s** |
| SSB cor / mean\|%\| | 0.6476 / 48.84% | **0.9414 / 14.73%** |
| min SSB | 29.23 (collapsing) | **863.78** |
| R(2008) | −99.01% | **+1.87%** |

This explains **every** symptom in this handoff: the 76-min runtime (thrashing against an
unsatisfiable index), the SSB/recruitment "collapse", `log_F` pinned at its bound,
`rec_pars` carrying the largest gradient, and the singular Hessian. The **BTS looked fine
throughout** only because it uses `Index_loglike = "MVN"`, which reads the external
`cov_2024.dat` and ignores `Log_sd` entirely — masking the bug on the one fleet everyone
was watching.

Residual objective 3,564 vs ADMB 740.5 is **composition data (3,494.6)** — expected and
already documented (diff #9): Rceattle multinomial at `Comp_weights = 1` vs ADMB's Francis
weights 0.84/1.27/2.09. Different normalizing constants; not comparable in absolute terms.

**Both scripts are fixed.**

#### ✅ CONFIRMED on the full 1218-param fit (`BAP=0 Rscript bridge_session2/match2.R`)

| | before | after |
|---|---|---|
| runtime | **4,593 s** (76.5 min) | **< 4 min** |
| outcome | SSB(1978)=0, `log_F` pinned at bound 10, `obj$gr()` dies in `EvalADFunObject`, every phase max\|grad\| 23–7600 | **converged** — all 1218 gradients ≈ 3.5e-04, "OK" |
| SSB vs ADMB | collapse | **cor 0.9683**, mean\|%\| 9.7%, max 22.0% |
| R vs ADMB | 44× low | **cor 0.9935**, mean\|%\| 5.8%, max 55.6% |
| BTS MVN jnll | — | 33.606 |

The catastrophic failure is **gone**. What remains is ordinary bridging residual, and it sits
exactly where this script already predicted it would:
- **1964 SSB −18.3% / R −24.2%** — the initial year. This is documented diff #12–#13:
  Rceattle `R_init = exp(rec_pars)` plus the ±σ²/2 rec/init bias correction vs ADMB's
  `R_init = exp(log_avgrec)` centred at 0. (Try `bias_adjust_proc`; `BAP=0` is the default.)
- **2024 R +55.6%** — terminal year, weakly informed. Expected.
- `sdreport` still fails: Hessian not invertible, 1 non-identifiable `sel_inf_dev`. This is a
  narrow identifiability issue (one BTS dev), **not** the old catastrophic failure — worth a
  look but it no longer blocks the bridge.

**Remaining priorities:** (1) the 2 test failures gating the `Sel_start_year` default;
(2) the 1964 initial-scale/bias-correction difference; (3) the single non-identifiable
`sel_inf_dev`. The optimizer is **not** at fault, and neither is the model.

---

### 🔬 SESSION 3b — THE RECRUITMENT PENALTY: derived, and it fixes the 1964 gap

**`SrType = 3` is NOT a stock-recruit relationship.** `SRecruit()` returns
`mfexp(log_avgrec)` — a *constant*, no SSB dependence. So `srmod_rec` is constant and
**`SR_resids == log_rec_devs` exactly** (verified against `pm.par`: aligns at offset 14,
max|diff| = 4.9e-06 ⇒ `styr_est = 1978`, `endyr_est = 2022`, 45 years). ADMB's "SRR fit"
(`rec_like(1)`, active because **`phase_sr = 5`**) is therefore just a **second rec-dev
penalty**. The true ADMB recruitment penalty is:

```
rec_like(2) = 1.0*Σ x²                     over 1964..2024   (all 61)
rec_like(1) = Σ [0.5*(x + 0.5)²]           over 1978..2022 EXCLUDING 1979   (44 yrs)
rec_like(4) = 1.0*Σ log_initdevs²          (14)
```
The 1979 exclusion is `ctrl_flag(25) >= 1` dropping the famous 1978 year-class. Verified
arithmetically: `0.5*Σ(x+0.5)²` over 1978–2022 = 16.1053; removing 1979 (x = 1.12348)
subtracts 1.3219 → **14.783 ≈ ADMB's reported `rec_like(1)` = 14.7875** ✅

**Where the two penalties overlap they combine to `1.5x² + 0.5x + const`.** Completing the
square: `1.5(x + 1/6)²`. Rceattle's penalty is `0.5*((x + bias·σ²/2)/σ)²`, so
**σ = 1/√3 = 0.5774 with `bias_adjust_proc = 1`** gives `1.5(x + 1/6)²` — *algebraically
identical* (constants differ by 0.083/yr and cannot move the MLE). **So the long-standing
`sigma_rec_prior = 0.707` was wrong** — it captured only `rec_like(2)`, ignoring
`rec_like(1)` entirely, and under-weighted the penalty by 1/3.

Measured on the full 1218-param fit (`bridge_session2/variant.R`, all with the `Log_sd` fix):

| variant | SSB cor | SSB mean\|%\| | SSB max | **1964** | R cor | R 2024 |
|---|---|---|---|---|---|---|
| **A** baseline σ=0.707, BAP=0 | 0.9683 | 9.70% | 22.0% | **−18.3%** | 0.9935 | +55.6% |
| **B** σ=0.5774, BAP=1 | 0.9836 | 7.70% | 19.7% | **−2.1%** | 0.9953 | +40.7% |
| **C** Francis Comp_weights | 0.9775 | 8.42% | 17.3% | −11.5% | 0.9931 | +56.8% |
| **D** both (**best**) | **0.9885** | **7.08%** | **12.4%** | **+5.3%** | 0.9946 | +41.8% |

**The 1964 "initial-scale" gap was the recruitment penalty, not `R_init`/bias-correction.**
Diff #12–#13 in the script header is superseded. Francis weights (C) also help — the header's
claim that they "moved SSB < 0.2%" was measured under the broken `Log_sd` config and is stale.
Use **`Comp_weights` = 0.729912 / 1.2535 / 2.11955** (fsh/bts/ats, from `pm.rep` `FW_*`).

**Caveat:** B/D apply `1.5x²+0.5x` to **all 61** years, whereas ADMB applies it to 44
(1978–2022 minus 1979) and `1.0x²` to the other 17. Rceattle's single `sigma_rec_prior`
cannot express a per-year-varying σ, so B/D are the closest single-σ approximation — this
is the likely source of the residual ~7% and of R(2024) +41.8%.

### 🛠 `ADMB/m23_rceattle_full/` — NEW: ADMB likelihoods rewritten as FULL likelihoods

Staged copy of `m23_rceattle`, modified so `tot_like` is **directly comparable to Rceattle's
`jnll`** (Grant's suggestion — make them match, and whatever is left is a real difference):
- `rec_like(2)`/`rec_like(4)` → **full normal**: `norm2/(2σ²) + n·log σ + n·0.5·log(2π)`
  (were bare kernels `1.*norm2`, i.e. σ=0.7071 with no σ or 2π constants).
- `rec_like(1)` → **0** (the windowed/1979-excluded second penalty Rceattle cannot express).
- `control.dat`: **`phase_sr` 5 → −1**.

**Built and converged: objective = 775.498479, max grad = 5.6e-04** (was 740.525106). The
shift reconciles exactly: halved kernels (−11.11, −4.32), `rec_like(1)` removed (−14.79),
2π constants added (+56.05, +12.86) ≈ 779, then the MLE moves. `sigr = 1`, so this model's
recruitment penalty is now **exactly** Rceattle at **`sigma_rec_prior = 1`, `BAP = 0`**,
uniform over all years.
```sh
cd ADMB/m23_rceattle_full && export PATH=/usr/local/bin:$PATH && admb pm && ./pm -nox -iprint 150
```
⚠️ Still reports **1224** parameters — `phase_sr = −1` did **not** deactivate `steepness`;
it has its own phase. Worth chasing to close the 1218-vs-1224 gap.

### ✅ SESSION 3n — TERMINAL R RESOLVED: `ignore_last_ats_age1` + the `−2020` bug

**Root cause of the terminal-R blowup: ADMB drops the 2024 ATS age-1 observation and Rceattle didn't.**
ADMB's `ignore_last_ats_age1` fires when the **last-year ATS NUMBERS CV** `std_ot_ats/ot_ats > 0.4`.
For 2024 it is **1.81** (the ATS *numbers* CV — NOT the biomass CV 0.26 I mistakenly checked in 3e/3m),
so ADMB fits `oa1_ats(1, n_ats_r−1)`, **excluding 2024** from `surv_like(3)`. Rceattle's `ATS_1` fleet
was fitting it — the anomalous obs 5074.8 (≈8× median) pulled terminal recruitment up.

**FIX 4 — exclude ATS_1(2024)** via the negative-year convention (Grant): set its `Year = −2024`.
Verified in cpp: `flt_yr < 0` computes the prediction but is excluded from BOTH the likelihood
(`flt_yr > 0` guard, ceattle_v01_11.cpp:2454) and the analytical q (guard :1821). **ATS_1 only** —
ADMB still fits the ATS *biomass* index in 2024.

**FIX 5 — the `−2020` bug.** The xlsx ATS & ATS_1 *index* rows had `Year = −2020` (excluded), but
ADMB's `yrs_ats_data` has **2020** (obs 3617 / 350) and **fits** it, and the ATS *comps* already fit
2020. So Rceattle was dropping 2020 index data ADMB uses. Flipped both index rows to `+2020`.

**Result (`match2_full.R`, all 5 fixes, vs `m23_rceattle_full`):**
```
R  : cor=0.9984 | mean|%|=3.9% | R(2024) −4.8% (was +83%)
SSB: cor=0.9924 | mean|%|=7.0% | SSB(2024) +1.0% (was +2.7%)
```

#### FIX 6 (ADMB side) — q-convention aligned ⇒ **R(2024) now matches to −0.2%**
The R(2024) −4.8% residual was a q-convention gap: ADMB computed the age-1 q `qtmp = geomean(oa1/ea1)`
over **all** years incl. the dropped 2024, while excluding 2024 from `surv_like(3)`. Modified
`m23_rceattle_full` (pm.tpl ~4023) so that when `ignore_last_ats_age1`, **qtmp is computed over
`1..n_ats_r−1`** too — consistent with Rceattle's negative-year convention (excluded from q AND
likelihood). ADMB rebuilt: obj 756.359, max grad 5.7e-04, R(2024) 17051 → **16267**.
```
R(2024): Rceattle 16226.7 vs ADMB 16267.1  =  −0.2%   (was +83% at the start of this thread)
R  : cor=0.9985 | mean|%|=3.8%
SSB: cor=0.9924 | mean|%|=6.9% | SSB(2024) +1.0%
```

**Remaining, precisely characterised:**
- **R(1964) −19.3% — root cause found (session 3o): the EARLY FISHERY SELECTIVITY FORM, not
  bias/R_init.** Ruled out: bias correction (BAP=0 ⇒ `R(1964)=exp(rec_pars)·exp(rec_dev(1964))`
  exactly, no bias term); `R_init` formula (`rec_pars` 9.6096 ≈ `log_avgrec` 9.6014, Δ0.008);
  `Finit` (=0 both); M (identical); init cascade construction (reproduces ADMB N(1964) to 5 sig
  figs at fixed params, session 3 dyncheck). **What it IS:** the 1964 fishery selectivity differs —
  Rceattle `NonParametricPM` sel_fsh(age1..4) = 0.141/0.340/0.669/1.210 vs ADMB AMAK
  0.024/0.167/0.564/1.366. Rceattle selects more young / fewer old fish, so to fit the same tight
  catch (σ=0.05, both hit 175) it needs FEWER young fish ⇒ lower R(1964)/age-2. Decomposition:
  perturbing Rceattle init→ADMB costs **+3.88 in catch** (dominant), +1.11 index, −0.62/−0.26 penalties.
  The init age structure is jointly tuned with F/selectivity to fit the early catch; the
  selectivity-form approximation (bridge "diff #11") propagates into the pre-survey N. Concentrated
  in **1964–1981** (no surveys until 1982); from 1982 on the surveys pin N and R matches (cor 0.9985).
  **Exact match would require ADMB's exact AMAK sel_fsh parameterisation** — the penalty *values* were
  matched (session 2) but the realized *shape* at boundary ages still differs. Not a formula bug.

  **Session 3p — how to match fishery selectivity, investigated.** The transforms are IDENTICAL
  (ADMB `compute_fsh_selectivity` and Rceattle `NonParametricPM` case 2 both: coffs → flat-above →
  `−log(mean(exp))` over all 15 ages → exp; verified line-by-line). The freely-ESTIMATED coefficients
  differ (age-1 log-coff ADMB −3.73 vs Rceattle −1.76; plateau ADMB 0.127 vs Rceattle 0.399). Cause:
  the fishery sel + initial N + early F/rec are a **jointly under-determined block** in the pre-survey
  years (1964–1981; only fishery comps + catch constrain them). Injecting ADMB's realized `sel_fsh`
  via empirical selectivity (`bridge_session2/match2_injfsh.R`) only PARTIALLY closes it — R(1964)
  −19.3% → −12.5%, and SSB(1964) −3.3% → −10.2% (worse) — because the other early free params
  re-optimize around the fixed curve. **Only fixing the WHOLE early block (= the forward pass /
  dyncheck) reproduces ADMB to 5 sig figs.** ⇒ There is no config knob that makes two FREE estimations
  converge to the same early fishery selectivity; the early period is genuinely under-identified.
  Options: (a) accept it (confined to 1964–1981; R cor 0.9985, SSB cor 0.99); (b) inject fishery sel
  for curve-matching only (partial); (c) forward-pass for an exact early trajectory (no free est.).

  **🎯 SESSION 3r — RESOLVED: it was a LOCAL MINIMUM, not a likelihood difference. Grant was right.**
  Injecting ADMB's fishery selectivity as the STARTING point and estimating freely: with the CORRECT
  increment injection (type 9 / `NonParametricPM` is a carry-forward walk
  `np_unc(yr)=np_unc(yr-1)+sel_coff_dev(yr)` ⇒ `sel_coff_dev` is the per-year INCREMENT, not the
  cumulative value the old 4.5.0 bridging script used), Rceattle **STAYS at ADMB's selectivity**
  (age-1 0.0224 ≈ ADMB 0.024) and reaches **objective 593.204 — LOWER than the default-init 602.168**.
  R cor 0.9990 (was 0.9985), **R(1964) −8.2% (was −19.3%)**, SSB likewise improved.
  ⇒ Rceattle's `match2_full` (default-init) solution was a **worse local minimum**; ADMB's phased
  optimizer found the better basin. Same likelihood, two minima — the free estimations DO converge to
  the same solution when started from the same basin. **The R(1964) gap was an OPTIMISER/initialisation
  issue, not a model/likelihood/selectivity difference.**
  (⚠️ my earlier "cumulative" admbinit test was INVALID — it injected double-accumulated devs, so the
  optimiser left a non-ADMB point; that did NOT prove a likelihood difference. Corrected here.)
  **Fix for the free-estimation match: initialise fishery sel + rec/F/init near ADMB (or improve
  Rceattle's selectivity phasing) so the optimiser finds the 593.2 basin.** `bridge_session2/match2_admbinit.R`.

  **Session 3s — the residual R(1964) −8% (in the 593.2 basin) = a weakly-identified initial-year F–N
  level, NOT a model difference.** In the good basin the whole 1964 age structure is uniformly ~0.91×
  ADMB (all init_devs ~0.1 lower, `rec_pars` matches 9.6023 vs 9.6014). Mechanism: **F(1964) Rceattle
  −2.83 vs ADMB −2.94** (F ~11% higher), N ~9% lower, catch(1964) = 175 in BOTH. Since catch = F·N
  (Baranov) and the **F penalty is off in both** (`ctrl_flag(4)=0`; Rceattle has no F-penalty term
  either), the catch pins only the F·N *product* in the data-poor initial year, not the *split* — a
  near-flat ridge (this is the source of the persistently **non-invertible Hessian**). Both models sit
  on the ridge at slightly different points; the ~8% is where each optimiser landed. Gradient at the
  Rceattle optimum is ~0 (5e-5, converged). To pin it, fix `log_F(1964)` at ADMB or add a weak F-level
  constraint — but it is a valid degenerate direction, not a bug. **Net: R cor 0.9990, R(1964) −8%,
  and that −8% is a shared weak-identification.**

  **Session 3q — "port the AMAK selectivity?": NOTHING to port, it is already equivalent** (context below).
  (⚠️ note: cpp `jnll_comp` is 0-based, R is 1-based ⇒ cpp shape row 4 = R row 5, cpp dev row 5 = R row 6.
  An earlier read of R rows 4/5 wrongly showed the fishery shape penalty as 0.)
  Verified with correct indexing: **the fishery selectivity penalties MATCH** — shape (decreasing)
  Rceattle 14.77 vs ADMB 13.56, dev Rceattle 124.63 vs ADMB 123.59. Decisive: ADMB's decreasing-penalty
  formula evaluated on **Rceattle's** realized sel = 14.774 = Rceattle's reported penalty (3 decimals) ⇒
  the penalty *forms* are identical, not just close. Combined with the identical transform, **Rceattle's
  `NonParametricPM` IS the AMAK selectivity** — there is no code to port. The realized sel still differs
  (age-1 coeff −1.76 vs −3.73) purely because the pre-survey block {sel, init N, early F} is
  **under-identified with multiple near-optimal modes**: `(Rce sel, Rce N)` and `(ADMB sel, ADMB N)`
  are both near-optima with a higher ridge between them (swapping only init N costs +4.26). Two
  optimizers land in different modes; re-implementing selectivity cannot fix an identifiability
  property of sparse pre-1982 data. Only fixing the whole early block (forward pass) matches exactly.
- **Likelihood constants** (catch lognormal `n(logσ+½log2π)`; index full-lognormal vs bare kernel;
  comp multinomial constant) — MLE-neutral, quantified in session 3h/3i.
- **`len_like` (25.8)** — length comps; Rceattle's age-only config doesn't fit them.

⚠️ Corrects sessions 3e & 3m, which concluded `ignore_last_ats_age1 = 0` from the ATS **biomass** CV
(0.26). The flag uses the ATS **numbers** CV (1.81). The terminal-R difference was NOT a benign
optimizer effect — it was this dropped observation.

---

### 🔬 SESSION 3m — (superseded by 3n) terminal R first traced to rec_dev(2024)

`bridge_session2/{ats1_diag,ats1_decomp}.R` + gradient tests on `match2_full_bap0.rds`.

**It is ONLY `rec_dev(2024)`** — Rceattle 0.729 vs ADMB 0.142. Mean R matches (15082 vs 14798) and
every other dev 1964–2023 matches (e.g. 2019: 1.695 vs 1.713). ⇒ R(2024) 31263 vs 17051 (+83%), but
**SSB(2024) only +2.7%** — terminal recruitment barely feeds terminal SSB.

**Not a bug, and not non-convergence — verified exhaustively:**
- **Index construction is EXACT.** At fixed ADMB params, Rceattle's ATS_1 index = `pred/q = N₁·exp(−mo/12·Z₁)`
  to ratio **1.000 every year incl. 2024**, q constant 0.0759 ≈ ADMB 0.0725. (The earlier "0.47 ratio"
  was an artifact of an optimized eval, not fixed-param.)
- **The fit IS converged:** `max|grad| = 8e-05`; gradient wrt `rec_dev(2024)` is exactly 0. The
  "did not converge" banner is only the singular Hessian on one `sel_inf_dev`.
- **Rceattle genuinely prefers 0.729:** perturbing it to ADMB's 0.142 **raises the objective +1.23**,
  split ATS_1 index +0.90 / BTS comp age-1 +0.56 / rec-penalty −0.26. So under the *matched*
  likelihood, **Rceattle's 0.729 is the LOWER-objective fit** — if anything Rceattle fits the terminal
  cohort slightly *better* than ADMB.
- **All inputs verified identical:** ATS_1(2024) obs 5074.8 (both); BTS comp 2024 age-1 proportion
  Rceattle 0.09864 = ADMB 0.0986375 (ratio 1.000); BTS comp sample size 64 = 64; σ=1 rec penalty
  (matched to 69.133); predicted comps identical at fixed params (session 3j); `larv_rec_devs` all
  zero (inactive), so ADMB's `rec_like(3)/(6)` = 0.

**Root driver: one anomalous observation.** `ATS_1(2024) = 5074.8` is ~8× the series median (~600),
and BTS comp 2024 age-1 is also elevated (9.9%). *Both* age-1 signals pull terminal recruitment up.
**Neither model fits it** — ADMB leaves residual +1.86, Rceattle +1.26. ADMB damps the terminal
deviate harder (0.142); Rceattle follows the data more (0.729). Since moving Rceattle to ADMB's value
costs +1.23 on the shared likelihood, ADMB's terminal deviate looks *under-pushed* (a flat terminal
direction its phasing didn't fully optimize), not Rceattle over-fitting.

**Conclusion: this is the expected behaviour of a weakly-determined terminal cohort, driven by a
single outlier age-1 observation — no model or data error, negligible management impact (SSB +2.7%).
The investigation is closed.** If a tighter terminal match is ever wanted, options are (a) a terminal
rec-dev penalty / tighter σ on the last year, or (b) down-weight the anomalous ATS_1(2024) — but
neither is warranted for the bridge.

---

### 🏁 SESSION 3l — ALL THREE COMP FIXES APPLIED to the full fit (`bridge_session2/match2_full.R`)

`match2.R` + the three comp fixes + `sigma_rec_prior = 1`, full 1218-param fit vs `m23_rceattle_full`:
```
CONVERGED (all 1218 grads OK; objective 602.886; only the usual 1 non-ident sel_inf_dev)
SSB: cor=0.9928 | mean|%|=7.0% | max=17.8% | 1964 −3.0% | 1978 +11.5% | 2024 +2.7%
R  : cor=0.9917 | mean|%|=5.3% | max=83.3% | 1964 −19.2% | 2024 +83.3%
```
The three fixes baked in: (1) `Comp_loglike = "MultinomialAFSC"`; (2) BTS comp age-1 restored +
`BTS_1` off; (3) ATS/AVO `Bin_first_selected = 2`. Plus `sigma_rec_prior = 1` (full-normal rec
penalty), `Comp_weights = 1` (no Francis), `comp_offset = 1e-3`.

**SSB is essentially unchanged from the best prior variant** (VAR E σ=1+Francis: cor 0.9940) — which
is expected: the comp fixes are mostly likelihood-*form*/decomposition (AFSC vs full multinomial is a
constant; MLE-neutral) plus a little added BTS age-1 information. They make the reported likelihoods
comparable component-by-component without degrading the fit, and correctly drop the (never-applied)
Francis weights. **SSB — the management quantity — matches ADMB to cor 0.993, 1964 within 3%, 2024
within 3%.**

**Remaining wart: terminal-year R(2024) = +83%** (SSB there still +2.7% — the terminal recruitment
is poorly constrained but barely feeds terminal SSB). This is the last open item; the age-1 index
construction (`ea1_ats` vs Rceattle's `ATS_1` fleet, session 3h) is the prime suspect. Everything
else — SSB trajectory, recruitment 1964–2020, all likelihood components — is matched.

---

### ✅ SESSION 3t — DO THE LIKELIHOODS MATCH AT ADMB's PARAMETERS? YES, up to known constants.

`bridge_session2/reconcile.R` — evaluate Rceattle's likelihood at ADMB's exact parameter vector
(emp_sel = ADMB realized sel; rec/F/init/q = ADMB; all session likelihood fixes).

| component | Rceattle @ ADMB par | ADMB | verdict |
|---|---:|---:|---|
| **Catch** | −123.43 → kernel **3.256** | 3.256 | ✅ **EXACT** (add `61·(log0.05+½log2π)=−126.68`) |
| **Rec-dev penalty** | 69.1383 | 69.1383 | ✅ **EXACT** |
| **Init-dev penalty** | 19.3667 | 19.3667 | ✅ **EXACT** |
| Comp Fishery / BTS / ATS | 174.86 / 202.05 / 29.45 | 176.45 / 195.01 / 29.73 | ✅ match (offset-const convention) |
| Index ATS / AVO / CPUE | −5.85 / −2.23 / −1.25 | 8.59 / 8.76 / 2.11 | ✅ kernel match; differ by per-obs `log σ+½log2π` (e.g. ATS Δ=−14.4) |
| Index BTS (MVN) | 30.75 | 30.32 | ✅ ~exact |

**Conclusion: the equations, kernels, and GRADIENTS are identical.** The reported *values* differ ONLY by
(1) additive lognormal normalizing constants (`Σ log σ_obs + ½log2π`) — Rceattle reports the full
`−dnorm(...)`, ADMB the bare kernel `Σr²/2σ²`; these are parameter-INDEPENDENT ⇒ don't move the optimum;
and (2) ADMB's `len_like ≈ 26` (length comps the age-only config omits). This is WHY Rceattle converges
to ADMB's solution when started in the right basin (session 3r) — same gradients.
✅ **`ATS_1` verified clean (session 3t follow-up):** the 106-vs-10.9 was a diagnostic config omission —
`reconcile.R` forgot `Log_sd = 1` for ATS_1 (ADMB `age1_sigma_ats = 1`), so it used the varying xlsx CVs
(0.03–0.4), corrupting both the weighted analytical q and the likelihood. With `Log_sd = 1` set (as
`match2` does): **ATS_1 kernel 0.5·SSE = 10.91 = ADMB surv_like(3) = 10.910, EXACT**; the reported
full-lognormal 27.45 = kernel + `18·½log2π`. So EVERY component now reconciles — the table has no
outliers. `bridge_session2/ats1v2.R`.

---

### 🎯 SESSION 3i — COMPOSITION GAP FULLY DIAGNOSED + penalty audit

**Decisive test (`bridge_session2/comp_diag.R`):** at the ADMB MLE, dump Rceattle `comp_obs`/`comp_hat`
beside ADMB's observed/predicted P-at-age (`old_rep.rep` "…Observed/Predicted P at age") and diff
age-by-age. Two DISTINCT causes, one per fleet type:

#### 1. Fishery gap = LIKELIHOOD FORM (a config setting, MLE-neutral)
Fishery observed **and** predicted proportions are **identical** to ADMB (max|diff| = 0.00000 both).
Yet Rceattle reported comp = 1281.6 vs ADMB 176.5. Cause: **Rceattle defaulted to the FULL
multinomial** (`Comp_loglike = "Multinomial"`, `comp_ll_type = 0`, via `dmultinom_osa`), while ADMB
uses the **offset/AFSC form** (`= -Σ n·obs·log((hat+c)/(obs+c))`). The 1281.6 − 174.9 = 1107 is the
multinomial normalising constant — MLE-neutral but makes reported values incomparable. **Fix: set
`Comp_loglike = "MultinomialAFSC"`** (`comp_ll_type = -1`). Confirmed: Fishery → **174.857 vs 176.452**
✅ (the small residual is the offset-constant convention). **⚠️ corrects session 3d**, which wrongly
claimed the default was already `MultinomialAFSC` — this config's default was `Multinomial`.

#### ✅ BTS RESOLVED (session 3j): age-1 is NOT double-counted → restore to comp, drop BTS_1
Decision rule (Grant): *double-counted → remove from comp; not → keep in.* Established the fact:
**`BTS_1` index obs == the raw age-1 count in ADMB's `oac_bts_data`, EXACTLY** (ratio 1.0000, sd 0,
cor 1.0 over all 42 years). So `BTS_1` *is* the age-1 that the xlsx data-prep stripped out of the
BTS comps and relocated into a dedicated index — the **same** survey observation, counted **once**
(plus a negligible 0.2–0.7% in the biomass index that both models share). **Not double-counted.**
⇒ Per the rule, keep age-1 in the comp: **restore BTS comp age-1 (= `BTS_1` obs) and drop the
`BTS_1` index** (keeping both WOULD create the double-count). This matches ADMB exactly (age-1 in
comps, no BTS age-1 index). Verified: **BTS comp 917.9 → 202.0 ≈ ADMB 195.0** ✅
(`bridge_session2/compare_bts1.R`). This is a data-prep correction (restoring survey age-1 that was
zeroed), distinct from the survey-biomass "don't touch" rule.

#### 2. BTS gap = DATA (observed age-1 zeroed in the xlsx) — mechanism, now resolved above
BTS **predicted** proportions are identical (max|diff| = 0.00000); the **observed** differ. The xlsx
zeroes BTS `Comp_1` (age-1) in every year and re-normalises ages 2..15; ADMB's `oac_bts_data` keeps
age-1 (e.g. 1982 age-1 = 0.082, 2000 = 0.076, 2024 = 0.099). ADMB fits BTS comps over **all 15 ages**
(`eac_bts /= sum(eac_bts)`; full-vector `age_like`), so it fits a real observed age-1 while Rceattle
fits 0 against a predicted ~0.09. Proof by direct recompute (same predicted props, AFSC form):
```
BTS comp NLL with xlsx observed (age-1 = 0):     917.9   (= Rceattle jnll_comp[3,BTS])
BTS comp NLL with ADMB observed (age-1 present): 202.0   (≈ ADMB age_like(bts) = 195.0)
```
⇒ **the xlsx data-prep stripped age-1 out of the BTS comps and moved it to the separate `BTS_1`
index — a structural choice ADMB does not make** (ADMB has no BTS age-1 index; it keeps age-1 in the
BTS comps). To match ADMB: restore observed age-1 to the BTS comps (from `oac_bts_data`) and drop the
`BTS_1` fleet. This is the SAME either/or the earlier `BTS_1` note raised, now shown to be the
dominant comp-gap driver. **Grant's "don't touch the data" applies to survey biomass; this is a
comp-data-prep discrepancy — surface it, decide deliberately.**

#### ✅ 3. ATS RESOLVED (session 3k): a diagnostic artifact — emp_sel ignores `Bin_first_selected`
Age-by-age dump (`bridge_session2/ats_diag.R`): ATS **observed** props match ADMB exactly; the
**predicted** differ — Rceattle `comp_hat` age-1 = 0.261 vs ADMB 0.000 (other ages = ADMB
renormalised WITH age-1). So the `Bin_first_selected=2` age-1 zeroing did **not reach `comp_hat`**.
**Root cause: `selectivity.hpp:324` `if (sel_type == 0) continue;`** — the fleet loop skips
empirical (type-0) fleets *before* `normalize_and_project_selectivity` (which applies the
`bin_first_selected` zeroing, selectivity.hpp:56). So under the emp_sel bypass (`Selectivity=0`,
which the whole `compare_ll.R`/`dyncheck.R` diagnostic uses to fix params at the ADMB MLE),
`Bin_first_selected` is silently ignored. Emulating it by hand (zero the ATS/AVO emp_sel age-1
column) ⇒ **ATS comp 395.4 → 29.451 ≈ ADMB 29.724** ✅ (`bridge_session2/compare_atsfix.R`).
**In the REAL `match2.R` config ATS is `NonParametricPM` (type 9/2, estimated)**, which DOES pass
through `normalize_and_project` and DOES apply `Bin_first_selected=2` — so the real fit already
excludes ATS age-1 correctly; only the emp_sel *diagnostic* needed the manual zero.

⚠️ **Genuine Rceattle finding: empirical selectivity (`Selectivity=0`) ignores `Bin_first_selected`
(and the normalisation).** `selectivity.hpp:324` skips type-0 fleets before
`normalize_and_project_selectivity`, so any user relying on empirical selectivity + a non-1
`Bin_first_selected` gets no age-1 zeroing. Minor, but a real inconsistency — worth a fix (run the
zeroing/normalisation for empirical fleets too) or at least a documented warning.

#### ✅ Composition FULLY RESOLVED (AFSC form, at ADMB MLE)
| comp | Rceattle | ADMB | fix |
|---|---:|---:|---|
| Fishery | 174.9 | 176.5 | ✅ `Comp_loglike = "MultinomialAFSC"` (was full multinomial) |
| BTS | 202.0 | 195.0 | ✅ restore observed age-1 to comp + drop redundant `BTS_1` index |
| ATS | 29.5 | 29.7 | ✅ `Bin_first_selected=2` (real config); emp_sel diagnostic needed manual age-1 zero |

**All three comp gaps closed.** The three fixes for the bridge config: (1) `Comp_loglike =
"MultinomialAFSC"` for all fleets; (2) restore BTS comp age-1 (= `BTS_1` obs) and turn `BTS_1` off;
(3) ATS/AVO `Bin_first_selected = 2` (already in `match2.R`). Predicted proportions were identical
throughout — every comp difference was observed-data or likelihood-form, never the model.

### 🔎 PENALTY AUDIT — does ADMB have penalties Rceattle lacks? **Essentially NO.**
ADMB's objective is `fff = sum(NLL(1..16))` (pm.tpl). Non-data regularizers:
| ADMB penalty | value (full) | Rceattle counterpart |
|---|---:|---|
| `rec_like` (NLL 7) | 88.5 (69.13+19.37) | ✅ HAS — **matched EXACTLY** |
| `sel_like` (NLL 14) | 16.07 | ✅ HAS (shape penalty) |
| `sel_like_dev` (NLL 15) | 186.5 | ✅ HAS (RW dev penalty) |
| `F_pen` (NLL 9) | **0** | off (`ctrl_flag(4)=0`); Rceattle has the capability |
| `Priors` (NLL 16) | **0** | h/q/m/R0 priors all 0 (steepness off, no q/M prior) |
| `cope_like` (NLL 8) | **0** | copepod-predation term, off |
| mean-F conditioning (pm.tpl:3623) | — | `if(current_phase()<3)` only — **removed in final phase** |

**Data-likelihood components with no Rceattle counterpart in THIS config:**
- `len_like` (NLL 13) = **25.79** — length composition. Rceattle *can* fit length comps but the
  pollock bridge fits age comps only. Real, but small.

**⚠️ CORRECTION to session 3h:** `wt_like` = 11146.5 is **NOT in the objective** — pm.tpl:1725
"wt_like is then a constant and is excluded", and `tot_like = 756.458 ≪ 11146`. It is reported only.
My 3h claim that "ADMB's tot_like includes 11172 Rceattle never evaluates" was **WRONG**; the only
uncounterparted objective term is `len_like` (25.8). So component-by-component the two objectives are
directly comparable once (a) `Comp_loglike = MultinomialAFSC`, (b) BTS observed age-1 restored, and
(c) length comps added or acknowledged as a 25.8 offset.

**Bottom line for the user's two questions:** (1) the comp divergence is now located precisely —
Fishery = likelihood-form config (fixed), BTS = xlsx zeroed observed age-1 (data-prep), ATS = one
residual still to find; predicted proportions are IDENTICAL everywhere checked. (2) ADMB has **no
penalty Rceattle lacks** — every regularizer has a counterpart (rec matches exactly; sel/sel_dev are
bypassed here via emp_sel but exist), and every ADMB-only term is either zero (F_pen, Priors,
cope_like), excluded (wt_like), or a small data term (len_like 25.8).

---

### 📊 SESSION 3h — FULL LIKELIHOOD COMPARISON at the ADMB MLE (`bridge_session2/compare_ll.R`)

Rceattle evaluated at the `m23_rceattle_full` MLE (identical parameters), corrected config
(BTS_1 off, ATS/AVO age-1 zeroed via `Bin_first_selected=2`, `comp_offset=1e-3`, `Comp_weights=1`,
`sigma_rec_prior=1`, BAP=0). emp_sel bypass ⇒ selectivity penalties absent.

| component | Rceattle | ADMB_full | status |
|---|---:|---:|---|
| **Rec-dev penalty** | 69.133 | 69.133 | ✅ **EXACT** |
| **Init-dev penalty** | 19.372 | 19.372 | ✅ **EXACT** |
| **Catch** | −123.428 | 3.256 | ✅ kernel exact; Δ = 61·(log0.05+½log2π) = −126.68 |
| Index: BTS | 30.734 | 30.316 | ≈ close |
| Index: ATS | −5.811 | 8.589 | constants + q treatment |
| Index: ATS age-1 | 76.431 | 11.009 | ❌ construction differs (see below) |
| Index: AVO | −3.004 | 8.760 | constants + q treatment |
| Index: CPUE | −1.235 | 2.106 | constants |
| **Comp: Fishery** | 1281.6 | 176.5 | ❌ **7.3×** |
| **Comp: BTS** | 1740.2 | 195.0 | ❌ **8.9×** |
| **Comp: ATS** | 618.7 | 29.7 | ❌ **20.8×** |
| *ADMB-only:* len_like | — | 25.79 | length comps (Rceattle doesn't fit) |
| *ADMB-only:* wt_like | — | **11146.5** | weight-at-age submodel (Rceattle uses fixed wt) |
| *ADMB-only:* sel penalties | (bypassed) | 202.6 | sel_like + sel_like_dev |

**What matches (essentially everything that CAN):**
- **Population dynamics** reproduce ADMB N & SSB to 5+ sig figs (session 3, `dyncheck.R`).
- **Rec & init penalties EXACT** — confirms the full-normal derivation (σ=1, BAP=0) is right.
- **Catch EXACT** to the lognormal constant (ADMB reports the bare kernel).
- **BTS biomass index** within ~1%.
- **Management outcomes:** full-fit SSB cor 0.99, R cor 0.99.

**The one genuine outstanding gap: COMPOSITION (~9×).** Ruled out as causes:
sample size (ADMB `sam` = xlsx `Sample_size`, ratio **1.000** — not a scaling), functional form
(both `MultinomialAFSC`/offset-multinomial), Francis weights (ADMB doesn't apply them), ageing
error (off + identity), offset constant (1e-5→1e-3 moved it <2%), and — for ATS — the age-1
normalisation range (still 20.8× even with age-1 zeroed). ⇒ **The difference is in the proportions
themselves** (observed or predicted), not in likelihood bookkeeping. **This is the decisive next
diagnostic:** at the ADMB MLE, dump Rceattle `comp_hat`/`comp_obs` beside ADMB `eac_bts`/`oac_bts`
for one BTS year and diff age-by-age. Leading suspect: **observed-proportion handling** — the xlsx
ships comps as **raw numbers** (Fishery `Comp_1` up to 3.48e6), and how Rceattle renormalises them
vs how ADMB forms `oac` may differ.

**Two ADMB components have no Rceattle counterpart** — `wt_like` (11146.5, the empirical
weight-at-age submodel; the config fixes weights) and `len_like` (25.79, length comps). These make
`tot_like` **not** directly comparable as a single number: ADMB's 756.458 includes 11172 of
components Rceattle never evaluates. Compare **component-by-component**, not totals.

**ATS age-1 index (76.4 vs 11.0):** ADMB `ea1_ats = ntmp(1) = N₁·S^0.5` (selectivity-independent);
Rceattle's `ATS_1` fleet uses emp_sel (1,0,0,…) with analytical q. Different construction — minor
(one component), but explains the gap.

---

### ✅ SESSION 3g — AVO also made age-2+ ⇒ ATS & AVO now share ONE age range (2..15)

**`ADMB/m23_rceattle_full`: `tot_like = 756.458`, max grad 5.63e-04, 1223 params, converged.**
`pred_avo` now sums **ages mina_ats..nages** (was all 15) — prediction-side only, `obs_avo`
untouched — so the mirrored ATS+AVO fleets have the **same** age range (2..15). Rceattle
represents this with **both at `Bin_first_selected = 2`**, which is what `match2.R` already sets, so
**no Rceattle package change is needed**. `avo_like = 8.760` (ref 8.66), `log_q_avo = −8.137`
(ref −8.18) — both healthy.

**This resolves the last index/comp coupling.** Every ATS/AVO age range now agrees on both sides:
| quantity | ADMB (m23_rceattle_full) | Rceattle (Bin_first_selected = 2) |
|---|---|---|
| ATS index `eb_ats` | ages 2..15 | ages 2..15 (sel_ats(1)=0) ✅ |
| ATS comps | ages 2..15 (`mina_ats`) | 2..15 (hat_1=0 ⇒ norm collapses) ✅ |
| AVO index `pred_avo` | ages 2..15 | ages 2..15 ✅ |
| ATS age-1 index `ea1_ats` | age-1 only | Rceattle `ATS_1` fleet ✅ |

**Two design options were weighed (Grant); option B chosen:**
- **A — Rceattle mirroring change.** Estimate the shared curve over the *union* bin-range of
  mirrored fleets, zero per-fleet below each fleet's own `Bin_first_selected`, warn on mismatch.
  A legitimate general feature, but: (1) a package change gated on golden-reference equivalence;
  (2) it would make age-1 of the shared curve a *free estimated* parameter, whereas ADMB's
  `sel_ats(1)` is only a normalisation residue (`1/mean(exp(log_sel))`) — so it would not actually
  match ADMB. Deferred; still worth doing as a general capability, but not for this match.
  Mechanics for whoever picks it up: mirroring lives in `adjust_map_shared_params`
  (R/3-build_map.R:1140) which copies the lead fleet's `sel_coff`/`sel_coff_dev` map onto the
  mirror (L1183-84); `bins_on = bin_first_selected:N_sel_bins` sets the estimated range
  (L715); per-fleet zeroing below `bin_first_selected` is in the C++
  `normalize_and_project_selectivity` (selectivity.hpp:56). Option A = map the union range on
  the lead, keep the C++ per-fleet zeroing.
- **B — remove age-1 from AVO in ADMB (CHOSEN).** Parallel to the `eb_ats` fix: prediction-side
  only, data untouched, `q_avo` bounded. Makes ATS & AVO genuinely identical in age range, which
  Rceattle already handles. Simpler, no package risk, consistent with "fix the prediction, not the
  data." Since AVO *borrows* the ATS curve, there is no reason it should carry age-1 while ATS does
  not.

⚠️ **The earlier "`pred_avo` change causes NaN" (session 3f) is now understood and resolved.** That
NaN was the `q_avo → 0` zero-gradient trap, not the age-range change. With `log_q_avo` bounded
[−15,0] the trap is gone and `pred_avo` age-2+ converges (the transient phase-2 `nll=nan` on
`log_q_avo` is climbed out of; final `nll=756.458, mag=5.6e-04`).

**Net effect of the whole age-1 cleanup (3f+3g):** `tot_like` 756.184 → 756.458 (≈ free, as the
1–3% biomass share predicts), the ATS age-1 double-count is gone, ATS & AVO age ranges match
Rceattle exactly, and `q_avo` no longer has a degenerate optimum. Data never touched.

---

### ✅ SESSION 3f — ATS age-1 DOUBLE-COUNT **FIXED** (prediction-side, data untouched)

**Final state of `ADMB/m23_rceattle_full`: `tot_like = 756.1035`, max grad = 3.63e-04, 1223 params.**
Versus **756.184** before the age-1 fix ⇒ **removing the double-count is essentially free**, exactly
as the 1–3% biomass share predicts (`q_ats` absorbs the offset). Every component is within a
whisker of the untouched m23 reference:

| | m23 (reference) | m23_rceattle_full (final) |
|---|---:|---:|
| `tot_like` | 740.525 | **756.104** |
| `cat_like` | 3.186 | 3.261 |
| `surv_like` | 30.52 / 8.79 / 11.12 | 30.35 / 8.65 / 11.05 |
| `avo_like` | 8.663 | **8.499** |
| `age_like` | 179.75 / 194.98 / 29.95 | 176.26 / 195.10 / 29.72 |
| `log_q_avo` | −8.182 | **−8.177** |

**What was changed (all prediction/parameterisation — the DATA is untouched):**
1. `eb_ats` sums **ages mina_ats..nages** (was all 15) ⇒ ATS age-1 is now fitted **only** by the
   dedicated age-1 index `ea1_ats`/`oa1_ats` (`surv_like(3)`), not *also* inside `surv_like(2)`.
2. `log_q_avo` **bounded** `[-15, 0]` (was an unbounded `init_number`) — see the trap below.
3. (from 3b/3c) `rec_like(1) = 0` unconditional; `rec_like(2)/(4)` full normal; `phase_sr = -1`;
   `phase_steepness = -1`.

**⚠️ Corrections to session 3e — two of my conclusions there were WRONG:**
- **"NOT fixable prediction-side" was wrong.** The NaN came from the **`pred_avo`** patch, not
  `eb_ats`. I had changed both at once and never isolated them. `eb_ats` alone converges cleanly
  (grad 2.4e-04, zero NaN). **`pred_avo` must be left alone.**
- **"The observations include age-1, so it's unfixable" was wrong** — an inference from that same
  conflated test. Grant's call is the right one: **we do not know whether the survey's reported
  biomass excludes age-1, so do NOT edit the data.** Fixing only the prediction works, and
  `q_ats` absorbs the constant offset.

#### 🪤 The AVO trap — an unbounded `exp()` parameter with a vanishing gradient
`avo_like = Σ (obs_avo − q_avo·X)² / (2σ²)` is **natural-scale normal with an ABSOLUTE σ**
(`obs_avo_std`, mean CV 0.224 — *not* a CV), and `q_avo = mfexp(log_q_avo)`. So
```
d(avo_like)/d(log q) = -q · Σ X(obs − qX)/σ²   ->  0   as q -> 0
```
⇒ **`q_avo → 0` is a ZERO-GRADIENT TRAP.** Once it drifts small the optimiser cannot climb back and
reports "converged" while sitting on the predict-nothing floor:
```
avo_like | q=0  =  Σ obs²/(2σ²)  =  296.185      <- ADMB reported EXACTLY 296.185
log_q_avo = -43.48  =>  q_avo = 1.3e-19
```
Verified to six digits from `data/pm_24.dat` (obs_avo line 159, ob_avo_std line 161). Because
`q_avo` affects **only** `avo_like`, the true optimum `q* = Σ(obs·X/σ²)/Σ(X²/σ²)` is strictly
positive (~e⁻⁸) — the trap is a pure parameterisation artifact, **not** evidence of a model
problem. Bounding recovers `log_q_avo = -8.177` and `avo_like = 8.499`. The trigger was the
`eb_ats` change perturbing `log_sel_ats` (AVO borrows it, pm.tpl:2885).

#### 🎯 This unblocks the ATS comp-normalisation problem
With `eb_ats` now age-2+, Rceattle's ATS at **`Bin_first_selected = 2`** is correct for **both**:
`sel_ats(1) = 0` ⇒ the index excludes age-1 (matching the new `eb_ats`) **and** `hat_1 = 0` so the
all-bins comp normalisation collapses to 2..15 (matching `mina_ats = 2`). The old index-vs-comps
tension is **gone for ATS**. (AVO shares the curve and is likewise `Bin_first_selected = 2`; its
`pred_avo` still includes age-1 in ADMB — a small documented residual, ~1.4–3.9%, absorbed by q_avo.)

#### ⛔ CORRECTION to session 3d: **only ATS is age-2+.** BTS comps are ALL-AGES.
Verified in `pm.tpl`:
```cpp
eac_bts(i) /= sum(eac_bts(i));                                 // normalised over ALL 15
age_like -= sam_fsh(i)*oac_fsh(i)*log(eac_fsh(i)+MN_const);    // FULL vector  (ages 1..15)
age_like -= sam_bts(i)*oac_bts(i)*log(eac_bts(i)+MN_const);    // FULL vector  (ages 1..15)
age_like -= sam_ats(i)*oac_ats(i)(mina_ats,nages)*log(eac_ats(i)(mina_ats,nages)+MN_const);  // 2..15
```
`mina_bts = 2` affects only `et_bts` (the numbers index), **not** the comps. So session 3d's
"ADMB normalises BTS comps over 2..15" is **WRONG**, and with it the tidy story that the comp
misfit ranks with the age-1 share.

⇒ **The comp gap is still UNEXPLAINED for Fishery and BTS**, where both models fit ages 1..15
with the same offset-multinomial form, the same (unweighted) sample sizes, no ageing error, and
a near-irrelevant offset constant:

| fleet | Rceattle @ ADMB MLE | ADMB | ratio | comp range (both) |
|---|---:|---:|---:|---|
| Fishery | 1241.354 | 179.752 | 6.9× | ages 1..15 — **unexplained** |
| BTS | 1743.551 | 194.976 | 8.9× | ages 1..15 — **unexplained** |
| ATS | 611.978 | 29.949 | 20.4× | 2..15 vs Rceattle 1..15 — explained *only* for this fleet, and the measurement is contaminated (that test used `emp_sel`, so `sel_ats(1) ≈ 0.76 ≠ 0`; redo with `Bin_first_selected = 2`) |

**Next diagnostic (decisive):** at the ADMB MLE, dump Rceattle's `comp_hat` and `comp_obs` for one
BTS year beside ADMB's `eac_bts`/`oac_bts` for the same year and diff them age-by-age. That
separates "predicted proportions differ" from "observed proportions differ" from "likelihood
accounting differs" in one shot. Candidates not yet excluded: whether Rceattle normalises
`comp_obs` the same way (the xlsx ships **raw numbers**, e.g. Fishery `Comp_1` max 3.48e6, not
proportions); and `comp_n` vs `sam_*` per-row alignment (medians match, but rows were never
compared pairwise).

---

### 🧬 SESSION 3e — ATS age-1 double-counting (SUPERSEDED by 3f above — see corrections)

**The double-counting is confirmed.** With `do_ats_bio = 1` (control.dat:68) the live branch is
pm.tpl:3929 `surv_like(2) += square(log(ob_ats)-log(eb_ats))/(2*lvarb_ats)`, and
`eb_ats(i) = wt_ats(i) * eac_ats(i)` is a dot product over **all 15 ages**. So ATS age-1 is fitted
**twice**: in `surv_like(2)` via `eb_ats`, and in `surv_like(3)` via `ea1_ats = ntmp(1)`.
(Comps are clean — `mina_ats = 2` excludes age-1, so it is not *triple*-counted.)

**And `sel_ats(1)` is not even a modelled quantity.** In `compute_selectivity_ats_devs`:
```
log_sel.initialize();                              // all zero
log_sel(stsel)(mina_ats,nsel) = coffs;             // ages 2..8
log_sel(stsel)(nsel+1,nages)  = coffs(nsel);       // ages 9..15 flat
log_sel(stsel) -= log(mean(exp(log_sel(stsel))));  // normalise over ALL 15
```
age-1 is **never assigned** — it keeps the `initialize()` zero and emerges as `1/mean(exp(log_sel))`,
a pure normalisation residue. That is why `sel_ats(1)` drifts 0.767 → 0.514 over time. Same story
for AVO, which borrows `log_sel_ats` (pm.tpl:2885).

**Magnitude: small.** age-1 is only **1.1–3.4%** of `eb_ats` and **1.4–3.9%** of `pred_avo`
(age-1 pollock ≈ 0.028 kg vs ≈ 0.56 kg at age 5). A statistical inelegance, not a material bias —
it is **not** what drives the SSB/R differences.

#### ❌ The prediction-side fix FAILS — do not retry as-is
Patched `m23_rceattle_full` so `eb_ats` and `pred_avo` sum only `mina_ats..nages`:
```
nll=nan from PHASE 2 onward     Objective = 5180.689   Max gradient = 10694.4
cat_like 830.9 (was 3.19) | avo_like 296.2 (was 8.66) | age_like 2426/776/127
```
Tried **twice** — once with temporary-subsetting (`elem_prod(...)(mina_ats,nages)`) and once with
explicit scalar loops to rule out an ADMB temporary/AD-tape artifact. **Bit-identical results**
(`5180.68918455727`), so it is *mathematical*, not an implementation bug. **Reverted**;
`m23_rceattle_full` is back to objective **756.184489**, max grad **4.45e-04**.

**Why it fails — the observations include age-1.** `ob_ats` is the survey's *total* biomass and the
AVO is total acoustic backscatter; both contain age-1 fish. Removing age-1 from the **prediction**
while the **observation** still contains it forces a systematic inconsistency the model cannot
resolve. ⇒ **The double-count can only be fixed on the DATA side** (supply age-2+ observed ATS
biomass / AVO), which we do not have. ADMB's treatment is arguably *correct given its data*: each
prediction matches its observation; the age-1 signal is merely used twice.

#### ⚠️ The real blocker is unchanged: `Bin_first_selected` couples index & comps
`Bin_first_selected` = *"bin at which selectivity is non-zero"* (R/data.R:69) ⇒ **2**, not 1,
excludes age-1 (setting 1 keeps it in). But one switch drives **both** the index and the comps:

| `Bin_first_selected[ATS]` | ATS index | ATS comps |
|---|---|---|
| **1** | ✅ includes age-1 — matches ADMB's `eb_ats` | ❌ age-1 (26–33% of NUMBERS) left in the denominator |
| **2** (match2.R today) | ❌ excludes age-1 — `eb_ats` includes it | ✅ `hat_1 = 0` ⇒ normalisation collapses to 2..15 |

Since ADMB **must** keep age-1 in `eb_ats` (its observation contains it), Rceattle cannot be right
on both today. **⇒ The principled fix is `Comp_first_bin` in Rceattle** — decouple the comp
normalisation range from the selectivity range (normalise `comp_hat` and the likelihood over
`first_bin..n_comp`). Gate on golden-reference equivalence (`BS2017SS` = 10241.030427).

---

### 🧪 SESSION 3d — LIKELIHOOD-BY-LIKELIHOOD COMPARISON (evaluated AT the ADMB MLE)

Method: build the Rceattle model, then `obj$report(p_admb)` with `p` set to the
`m23_rceattle_full` MLE — this compares **likelihood forms at identical parameters**, so a
difference cannot be blamed on a different optimum. (`bridge_session2/dyncheck.R` + the
`report()` tail; `estimateMode = 1` for a real objective.)

| component | Rceattle @ ADMB MLE | ADMB | verdict |
|---|---:|---:|---|
| **Catch** | **−123.437** | `cat_like` = 3.18588 (bare kernel) | ✅ **EQUIVALENT** |
| **Composition** | **3596.883** | `age_like` = 404.677 | ❌ **REAL DIFFERENCE** |
| Index | 139.495 | surv+cpue+avo ≈ 50.4+4.5+8.7 | (q's not fixed; see below) |

**Catch — CONFIRMED equivalent.** Rceattle = kernel + `n·(log σ + 0.5·log 2π)`. With σ = 0.05,
n = 61: `3.186 + 61·(−2.9957 + 0.9189) = 3.186 − 126.68 = −123.5` ≈ Rceattle's −123.437 ✅
Only the constants differ; **`ctrl_flag(1) = 200` ⇔ σ = 0.05 is exact.**

**Composition — same formula, same inputs, 8.9× apart. Root cause found:**
Everything that *could* have explained it was checked and eliminated:
- **Functional form** — identical. Rceattle's default `Comp_loglike = "MultinomialAFSC"` maps to
  `comp_ll_type = -1` (0-switches.R:114) = `-Σ w·n·(obs+c)·log((hat+c)/(obs+c))`; ADMB builds
  `age_like -= sam·obs·log(eac+MN_const)` then `-= age_like_offset` (= `-Σ sam·obs·log(oac+MN_const)`)
  → the same offset multinomial. ✅ same
- **Francis weights — ADMB does NOT apply them.** `FW_fsh(1) = calc_Francis_weights(...)`
  (pm.tpl:7029) only **computes and reports** them; `age_like` (pm.tpl:4023) uses `sam_fsh` raw.
  They are a *diagnostic* for the analyst to fold into input sample sizes on a re-run.
  ⇒ **`Comp_weights` must be 1. VAR C/D/E/F/G applying Francis were WRONG** (they helped only by
  compensating for other errors). Confirmed numerically: ADMB `sam_*` ≈ xlsx `Sample_size`
  (ratios 1.000 / 0.994), i.e. sample sizes are *unweighted* on both sides. ✅ same
- **Ageing error** — `use_age_err = 0` in ADMB, and the xlsx `age_error` sheet is *already* the
  identity, so `age_error <- diag(nages)` is a no-op. ✅ same
- **`comp_prop_offset` vs `MN_const`** — a genuine 100× mismatch (Rceattle default **1e-5**,
  0-switches.R:217; ADMB **1e-3**, pm.tpl:1431) and **worth aligning via
  `fit_control(comp_offset = 1e-3)`** — but measured, it is **NOT the cause**: comps move only
  3596.9 → 3638.5. ❌ not it

#### 🚨 ROOT CAUSE: comp NORMALIZATION RANGE (`mina_bts`/`mina_ats` = 2)
ADMB fits BTS/ATS comps over **ages 2..15 only** — `mina_bts = 2` (pm.tpl:85, hardcoded),
`mina_ats = 2` (pm.tpl:86, via `use_age1_ats = 1`), e.g.
`age_like -= sam_ats(i)*oac_ats(i)(mina_ats,nages)*log(eac_ats(i)(mina_ats,nages)+MN_const)`.
Age-1 is *removed from the comps* and fitted separately as the ATS age-1 index.

Rceattle normalizes over **every** bin — `comp_hat.row(comp_ind) /= comp_hat.row(comp_ind).sum()`
(ceattle_v01_11.cpp:2265) — and **`fleet_control` has no comp bin-range option** (only
`Bin_first_selected` / `Sel_pen_first_age`, which govern *selectivity*, not comp normalization).

So: observed BTS/ATS `Comp_1 = 0` in every year, but Rceattle's **predicted** age-1 is nonzero
(emp_sel `sel_bts(1)` = 1.0, `sel_ats(1)` ≈ 0.76–1.0). Every predicted age-2..15 proportion is
therefore scaled by `(1 − hat_age1)` — a systematic misfit at **every** age. The per-fleet
pattern matches exactly:

| fleet | Rceattle @ MLE | ADMB | ratio | ADMB comp range |
|---|---:|---:|---:|---|
| Fishery | 1241.354 | 179.752 | 6.9× | ages 1..15 |
| BTS | 1743.551 | 194.976 | 8.9× | **ages 2..15** |
| ATS | 611.978 | 29.949 | **20.4×** | **ages 2..15** |

**This is a genuine Rceattle limitation**, not a config error: Rceattle cannot fit comps over a
sub-range of ages while the *same fleet's* selectivity spans all ages — and it must, because
ADMB's `eb_bts`/`eb_ats` biomass indices sum over **all** ages with `sel(age1) > 0`. (This is the
exact tension the main script's header flagged: *"Bin_first_selected[ATS] <- 1 — age-1 IS
selected (sel_ats(1)~0.76; eb_ats sums all ages)"*.) Setting `Bin_first_selected = 2` zeroes
age-1 selectivity and fixes the comps, **but then breaks the biomass index** — you cannot have
both today.

**Options (pick before proceeding):**
1. **Add a comp bin-range to Rceattle** (e.g. `Comp_first_bin` per fleet) — normalize `comp_hat`
   and the likelihood over `first_bin..n_comp`. The principled fix; a package change gated on
   golden-reference equivalence (`BS2017SS` objective 10241.030427).
2. **Modify ADMB** to `mina_bts = mina_ats = 1` and fit comps over ages 1..15 — but ADMB's comp
   *data* has age-1 stripped (xlsx `Comp_1 = 0`), so ADMB's predicted age-1 would then mismatch
   too. This matches the two models by making **both** wrong. Not recommended.
3. Accept the comp difference and match on SSB/R only (currently SSB cor 0.9940).

**Note the Francis correction supersedes VAR C–G**: re-run the variant matrix with
`Comp_weights = 1` and `fit_control(comp_offset = 1e-3)` before drawing further conclusions.

---

### 🔍 SESSION 3c — SD AUDIT (every hand-set value) + **steepness turned off**

**Every hand-set SD is now traced to an ADMB source. Two were unaudited; both are CORRECT.**

| config value | ADMB source | verdict |
|---|---|---|
| `catch_data$Log_sd = 0.05` | `catch_like = norm2(log obs − log pred)`, `cat_like = ctrl_flag(1)*catch_like`, **`ctrl_flag(1) = 200`** (control.dat:130) ⇒ σ = 1/√(2·200) = **0.05** | ✅ **CORRECT** |
| `index_data$Log_sd[ATS_1] = 1` | `surv_like(3) = 0.5*norm2(...)/age1_sigma_ats²` — a *direct* σ (no 1/√(2w) mapping); **`age1_sigma_ats = 1`** (Input_Log.rep) | ✅ **CORRECT** |
| `index_data$Log_sd[BTS_1] = 1` | — | ⚠️ **BTS_1 SHOULD NOT EXIST** (below) |
| index `Log_sd` (BTS/ATS/AVO/CPUE) | xlsx, already CVs; ADMB `sdnr ≈ 0.95/0.99/0.98` | ✅ (session 3 — do NOT divide by Observation) |
| `Time_varying_sel_sd_prior` 0.5 / 0.138 | `selvar24.dat` | ✅ (session 2) |
| `Sel_curve_pen*` 12.5 / 1/60 / 2 / 8 / −1 / 1 | `ctrl_flag(13)/(11)/(26)/…` | ✅ (session 2) |
| `Comp_weights` 0.7299 / 1.2535 / 2.1196 | `pm.rep` `FW_fsh/bts/ats` | ✅ (session 3b) |
| `sigma_rec_prior` | see 3b — **0.707 was WRONG** | ⚠️ use 1 (vs `_full`) or 0.5774+BAP=1 (vs stock) |

The `ctrl_flag` block (control.dat:130–159 = `ctrl_flag(1..30)`) also independently confirms
session 3b's derivations: **`ctrl_flag(25) = 1`** (⇒ 1979 excluded from `rec_like(1)` — matches
the arithmetic), **`ctrl_flag(30) = 1`** (no extra `rec_like(1)` scaling), **`ctrl_flag(4) = 0`**
(F penalty off).

#### 🚨 `BTS_1` is an EXTRA data source — ADMB has no BTS age-1 index
`vector surv_like(1,3)` — ADMB's survey likelihood has exactly three components: BTS biomass,
ATS biomass, ATS age-1 (`use_age1_ats`). There is **no `oa1_bts` / `ea1_bts` / `age1_sigma_bts`
anywhere in `pm.tpl`** (the `age1…bts` hits are `phase_age1devs_bts` / `sel_age_one_bts_dev_est`
— the BTS *selectivity* age-1 parameter, not an index). Rceattle's `BTS_1` fleet (42 obs,
1982–2024) is therefore data ADMB never fits. `variant.R` now supports `BTS1_OFF=1`.
Removing it drops index jnll 94.5 → 49.0 and **leaves SSB identical** (cor 0.9940).

#### ✅ steepness turned OFF — **the 1218-vs-1224 parameter gap is CLOSED**
`phase_sr` did **not** control it — `steepness` has its **own** switch, **`phase_steepness`**
(control.dat:56, was 5). Setting it to −1 in `m23_rceattle_full`:
```
Number of parameters = 1223   Objective = 756.184489   Max gradient = 4.45e-04
rec_like = 0  69.1403  0  19.3761  0 0 0      <- rec_like(1) = 0 (now forced unconditionally)
steepness = 0.6 (fixed, inactive)   sigr = 1
```
**1223 − 5 (`rec_dev_future`, projection-only) = 1218 = Rceattle's count.** Exact reconciliation.
`rec_like(1)` is now zeroed *unconditionally* (a guard after the if/else), so the alignment holds
regardless of `phase_sr`/`phase_steepness`.

#### Terminal-year R — the one open item
| variant (vs `m23_rceattle_full`) | index jnll | SSB cor | SSB mean\|%\| | 1964 | 2024 | R(2024) |
|---|---|---|---|---|---|---|
| **F** BTS_1 **off** (structurally correct) | 49.0 | **0.9940** | 7.21% | +2.6% | −1.3% | **+158.9%** |
| **G** BTS_1 on | 94.5 | **0.9940** | 7.21% | +2.5% | −1.3% | +114.8% |

`BTS_1` was only *masking* it. **Ruled out this session:**
- `ignore_last_ats_age1` — it is **computed**, not input (`pm.tpl:1659`:
  `std_ot_ats(n_ats_r)/ot_ats(n_ats_r) > 0.4`). Last ATS CV = **0.2628 < 0.4 ⇒ = 0**, so ADMB
  *does* use the last ATS age-1 obs. `use_last_ats_ac = 1`. Not the cause.
- **Age-1 double-counting** — ATS and BTS comps already have `Comp_1 = 0` in **every** year
  (matching `mina_ats = 2` / `mina_bts = 2`), and there is no 2024 fishery comp (fishery comps
  end 2023). No double count.

**The live hypothesis:** ADMB's 2024 dev is **0.0833 ≈ 0** — shrunk to the mean, i.e. *nothing
informs it* — yet ADMB nominally has `ATS_1(2024) = 5074.8`. The mechanism to check is
**`oa1_ats(i) = oac_ats_data(i,1)` (pm.tpl:1645)**: ADMB derives its age-1 index **from column 1
of the ATS age-comp matrix**, whereas the xlsx pre-extracted age-1 into a separate `ATS_1` fleet
(comp `Comp_1` zeroed). Confirm the two carry the same terminal observation — compare ADMB's
`oa1_ats` against the xlsx `ATS_1` series year-by-year. If ADMB's ATS comp matrix has no 2024
age-1 entry (or `n_ats_ac_r` stops short), that is the whole answer.

Note `m23_rceattle_full` has `log_avgrec = 9.6029` (mean R = 14,807; was 9.8331/18,632) and
`R(2024) = 16,093` — the MLE moved when `rec_like(1)` was removed, so re-derive against `_full`.

#### VAR E — Rceattle (σ=1, BAP=0, Francis) vs `m23_rceattle_full` — **best SSB yet**

Both sides now carry ONE uniform full-normal rec penalty (σ=1), so this is the first
structurally-matched configuration:

```
SSB: cor=0.9940 | mean|pct|=7.21% | max|pct|=14.6% | 1964=+2.5% | 2024=-1.3%
R  : cor=0.9861 | mean|pct|=6.53% | max|pct|=114.8% | 1964=-12.8% | 2024=+114.8%
objective = 2914.74 (Rceattle) vs 775.50 (ADMB_full)
comps=2668.5  index=94.5  catch=-122.9  recdev=69.26  initdev=19.54
```
**SSB cor 0.9940 is the best across every variant**, and 1964/2024 SSB are both within 2.5%.
But **R(2024) = +114.8%** — worse than D's +41.8%: with σ=1 the rec penalty is weaker, so the
terminal-year deviation drifts free. ADMB, with the *same* σ=1 penalty, does not drift — so
something else pins its terminal R. **This is the sharpest remaining lead.**

Prime suspect: `est$index_data$Log_sd[BTS_1/ATS_1] <- 1` (in `match2.R`, from ADMB's
`age1_sigma_ats`). A CV of 1.0 makes the age-1 abundance indices — the *only* data informing
terminal-year recruitment — nearly uninformative in Rceattle. Verify what ADMB actually uses
for the age-1 index sigma before assuming 1.0 is right; given the `Log_sd` bug found this
session, treat every hand-set SD as unaudited.

**NEXT, in order:**
1. **Terminal-year R** — audit the BTS_1/ATS_1 age-1 index sigma against ADMB (above).
2. **`steepness` still active** in `m23_rceattle_full` (1224 params) despite `phase_sr = -1`;
   find its phase and deactivate → should close most of the 1218-vs-1224 gap.
3. **Comp likelihood constant** — `age_like` 404.7 vs Rceattle ~2668. Almost certainly ADMB's
   offset form (`-Σ n·obs·log(pred/obs)`) vs Rceattle's full multinomial. A constant ⇒
   MLE-neutral, but confirm, since it is the bulk of the objective gap (2914.7 vs 775.5).
4. Consider whether σ should be a per-year vector in Rceattle to express ADMB's stock
   1.5x²/1.0x² split (only if you want to match unmodified `m23_rceattle` rather than
   `m23_rceattle_full`).

---

### ✅ SESSION 3 — THE MODEL-vs-OPTIMIZER SPLIT IS ANSWERED: **the model is exact**

`bridge_session2/dyncheck.R` — fix EVERY Rceattle parameter at the `m23_rceattle`
MLE, set **`estDynamics = 0` so Rceattle COMPUTES numbers-at-age** (the old forward
pass used `estDynamics = 1`, which *injects* N and therefore could never test the
dynamics), bypass selectivity empirically with `emp_sel` **refreshed from
`m23_rceattle/pm.rep`**, then compare N to ADMB's N:

```
1964 age1: R=  7020.2  ADMB=  7020.2   1978 age1: R= 24256.4  ADMB= 24256.4
2024 age1: R= 18325.2  ADMB= 18325.2
N ratio   mean 1   range [1, 1.00001]     (all 15 ages x 61 years)
SSB  1964: 1988.39 = 1988.39 | 1978: 1044.89 = 1044.89 | 2024: 3411.52 vs 3411.51
SSB ratio mean 1   range [1, 1]
```

**Rceattle's dynamics are IDENTICAL to ADMB's** — recruitment, the `initMode = 2`
initial-age cascade, F, Z, survival, SSB. Given the right parameters Rceattle
reproduces ADMB to 5+ significant figures everywhere.

⇒ **The "recruitment collapse" was never real.** `R(1978) = 24,256` here vs the failed
fit's 554 ("44× low") and the "2× low SSB" (534.8 vs 1044.9) are **artifacts of a
non-converged fit** — exactly the caveat this handoff already flagged. Do not chase
`rec_dev`, biomass scale, or the collapse: they are downstream symptoms.
**The defect is in the OPTIMIZATION, not the model spec.**

### Do these in order
1. **Objective at the ADMB MLE** (the one remaining spec question). `dyncheck.R`
   proves the *dynamics*, but ran under `estimateMode = 4` (`jnll = dummy²`), so it
   does **not** test the likelihood. Map the full parameter vector (incl.
   `sel_coffs_*`/`sel_devs_*`) into the `match2.R` config, build with
   `estimateMode = 1`, and evaluate `obj$fn(par_admb)` / `obj$gr(par_admb)` **without
   optimizing**:
   - `fn = 740.5251`, `grad ≈ 0` → model + likelihood identical ⇒ **optimizer is at
     fault** (then: profile the 76 min, and why `obj$gr()` dies).
   - `fn = 740.5251`, `grad ≠ 0` → likelihoods differ subtly.
   - `fn ≠ 740.5251` → a structural difference still hiding in the likelihood.
2. **Two test failures** (below) — they **block** the `Sel_start_year` default change.
   `NOT_CRAN=true` required, else everything silently skips. Log:
   `bridge_session2/tests2_failures.log`.
3. Profile the 76 min / diagnose `obj$gr()` dying.

### Already ruled out — do NOT re-run
- **Bounds** — zero ADMB params are at a bound (`sel_devs_fsh` max 1.4531 vs ±5).
- **Variances** — identical (`selvar24.dat`: fsh 0.5, ats 0.138, constant).
- **Catch units** — exact match, every year (1978: 979.431 = 979.431).
- **Phasing** — not the cause; unphased is just as slow and also fails.
- **⛔ Weight-at-age units (was "TOP LEAD") — REFUTED, session 3.** All weights are kg
  and match ADMB value-for-value: xlsx `weight` index 1 (0.0066, 0.17, 0.303, 0.447,
  0.589…) **= `pm.rep` `wt_fsh`**; index 5 "SSB wt" (0.0849, 0.1959, 0.3138, 0.4593,
  0.5886…) **= `pm.rep` `wt_ssb`**; control `ssb_wt_index = 5` is wired correctly.
  (`wt_ssb ≠ wt_fsh` in this run — the script header's diff #3 claim that "wt_ssb
  defaults to wt_fsh" is stale, but the *data* is right.) Dimensionally the lead was
  never viable anyway: a kg↔mt error is **1000×**, not the observed 2×.
- **⛔ Maturity / female-SSB 0.5 — REFUTED, session 3.** xlsx `maturity` is the raw
  ogive (0.000, 0.008, 0.289 … 1.000) and `sex_ratio` = 0.5, so Rceattle's
  `maturity × sex_ratio` correctly reproduces ADMB's `p_mature *= 0.5`. No double-halving.
- **⛔ Whole SSB pipeline — VERIFIED EXACT, session 3.** Reconstructing ADMB's own SSB
  from ADMB's own N/Z with the **xlsx** maturity×0.5 and **xlsx** wt_ssb at spawn
  fraction 0.25 reproduces `pm.rep` `SSB` over all 61 years: ratio **mean 1.000000,
  range 0.999996–1.000004**. Weight units, maturity, the 0.5, and spawn timing are all
  correct in the data.
- **⛔ Global unit scale (catch/N/index kt-vs-mt) — REFUTED, session 3.** The xlsx is
  internally consistent in ADMB units (weight kg, N millions, catch+index 1000 mt), so
  a global 1000× is absorbed by the free `rec_pars` and the survey `q`s. Verified
  empirically: rescaling catch+N+indices by 1000 vs by 1 gives **identical** results to
  every digit (SSB 2.2794%, catch 58.099%, BTS 11.986% in both). Rceattle's own
  convention (cf. `BS2017SS`: kg weights, catch 4.5e3–1.5e6 **mt**) is mt/thousands, so
  the pollock data is uniformly 1000× low vs package convention — but it is a **no-op on
  the fit**, and `rec_pars`' default init of `9` (`R/2-build_params.R:41`) happens to
  suit the millions scale. Cosmetic only; do not spend time here.

### ⚠️ `ADMB/m23/` has been RE-RUN — the main bridging script's reference is stale
`git status` shows `ADMB/m23/` modified, with `control.dat` **`DoCovBTS` flipped 0 → 1**
and every `.rep`/`.par` regenerated. So `ADMB/m23/` is now an **m23-MVN** run, *not* the
run `2024 EBS pollock bridging.R` was validated against. That script reads `m23/pm.par` +
`m23/pm.rep` while the xlsx's `NByageFixed`/`emp_sel` still come from the ORIGINAL m23 —
mixed-vintage inputs. Measured consequence: its forward pass now reports **catch mean
|%diff| = 58%** against its own documented **0.0001%**. The script is INVALID as written
(same root cause the "Next steps" note flags for `_rceattle_fwdpass_aligned.R`). Either
repoint it at `m23_rceattle` and refresh `NByageFixed`/`emp_sel` from that `pm.rep`
(what `dyncheck.R` does), or restore `m23/` with `git checkout`.

Also: the main script uses `library(Rceattle)`, which loads the **installed 4.5.0**, not
the `dev-ebs-pk` tree (4.7.0). 4.5.0 has no `rec_dev` parameter and its `build_map` does
not know `Sel_start_year`, so it cannot run this bridge at all — **always
`pkgload::load_all()`**, as `bridge_session2/*.R` already do.

---

## TL;DR (updated session 3 — **SOLVED**)

Parameters, variances and penalties match ADMB exactly, **and so does the model**: fixed at
ADMB's MLE with `estDynamics = 0`, Rceattle recomputes ADMB's N and SSB to 5+ sig figs
(ratio range [1, 1.00001]). The failure was **one line in the bridging config** —
`index_data$Log_sd <- Log_sd / Observation` — which turned already-CV index SDs into CVs of
~1e-5, inflating the index likelihood to **743,587** and making the problem unfittable.
Removing it: the full 1218-param fit **converges in < 4 min** (was 76 min and collapsing),
SSB cor 0.9683, R cor 0.9935. Neither the optimizer nor the model spec was ever at fault.

The sections below are kept as the record of what was eliminated along the way — note that
the two leads this handoff previously promoted (**weight-at-age units**, **bounds**) are both
**refuted**, and the "recruitment collapse" was an artifact of the non-converged fit.

---

## Reference numbers (aligned ADMB, `m23_rceattle/pm.par` header)

```
Number of parameters = 1224   Objective = 740.525106862990   Max gradient = 0.000376778799600388
```

`pm.par` lists all **2119 declared** params (incl. inactive) — do not count it.
`pm.std` lists only the **active** set; it sums to exactly 1224.

| ADMB active | n | Rceattle | n |
|---|---:|---|---:|
| `sel_devs_fsh` + `sel_coffs_fsh` | 732 | `sel_coff_dev` (fishery) | 732 ✅ |
| `sel_devs_ats` + `sel_coffs_ats` | 217 | `sel_coff_dev` (ATS) | 217 ✅ |
| `sel_{slp,a50,age_one}_bts_dev_est` | 126 | `sel_inf_dev` 84 + `log_sel_slp_dev` 42 | 126 ✅ |
| `sel_{slp,a50,age_one}_bts` | 3 | `sel_inf` 2 + `log_sel_slp` 1 | 3 ✅ |
| `log_rec_devs` / `log_F_devs` / `log_initdevs` | 61/61/14 | `rec_dev` / `log_F` / `init_dev` | 61/61/14 ✅ |
| `log_avgrec` + `log_q_{ats,cpue,avo}` | 4 | `rec_pars` + `index_log_q` | 4 ✅ |
| `steepness` | 1 | — | 0 |
| `rec_dev_future` | 5 | — | 0 |
| **total** | **1224** | | **1218** |

**1218 = 1224 − 6.** The two ADMB extras are artifacts, not differences:
`rec_dev_future` is projection-only (Rceattle fits hindcast-only here), and
**`steepness` stays active under `SrType = 3`** even though mean recruitment never
uses it — a leftover from the alignment edit. Worth mapping out in a future ADMB run.

---

## Rceattle bugs found & fixed (both real, both in the package)

### 1. `build_params()` — `sel_inf[2]` init unusable for LogisticPM
`sel_inf[2]` defaults to `10`, correct as a descending-limb inflection **age**, but
LogisticPM (type 11) repurposes the slot as the age-1 **log**-selectivity →
`exp(10) = 22026` vs ADMB's `sel_age_one_bts = -3.19` (0.041). Caused a 2.38e29
divergence. Fixed + regression test + NEWS.

### 2. `build_map()` — `Sel_start_year` was never consulted
Time-varying selectivity deviations were estimated across **every** hindcast year,
including years before a fleet's first observation. Those have **no data and no
penalty** (every selectivity penalty in the cpp is anchored at `start_yr` —
LogisticPM at `ceattle_v01_11.cpp:2779`, NonParametricPM at `2809/2816/2837/2845/2857`),
so they are unidentified flat directions. ~54 on the BTS, ~240 on the ATS.

The two parameterizations differ in where the base lives, and are handled differently:
- **LogisticPM** estimates a separate base (`sel_inf`/`log_sel_slp`) → mask deviations
  **through** `start_idx`.
- **NonParametric-RW** maps its mean off (`sel_coff[flt,,] <- NA`) and lets
  `dev(start_idx)` carry the base → mask only **before** `start_idx`.

Confirmed equivalent to ADMB: fishery `12 × 61 = 732` = ADMB's `12 + 720`.

### 3. `Sel_start_year` default: `styr` → first year of data
It is an **input switch** (undocumented until now), so fix #2 only helped users who
knew the column existed. Now derived from `catch/index/comp/caal_data`, consistent
with how `switch_check()` already auto-`"Off"`s fleets with no observations.

**Must group by `Selectivity_index`** — fleets sharing an index share one curve. AVO's
data starts 2006 but it mirrors ATS (data from 1994); keying off AVO alone deletes the
ATS's data-informed 1994–2005 deviations. The tell was the parameter count undershooting
(1153 < 1224). Group-aware version reproduces the hand-written config exactly on all
4 fleets. Documented in `R/data.R`; NEWS entries added.

---

## Config bug (in the bridging script, not the package)

**`Bin_first_selected[ATS/AVO]` must be `2`, not `1`.**
`pm.tpl:86` — `if (use_age1_ats) mina_ats = 2; else mina_ats = 1`, and
`sel_coffs_ats(mina_ats, n_selages_ats)` has 7 elements with `last_age_sel_ats = 8`
(control.dat:128). So ATS selectivity spans **ages 2..8 = 7 bins**, not 8.
`8 × 31 − 7 × 31 = 31` — this was the entire remaining parameter gap.

---

## Variances: audited, IDENTICAL — not the problem

ADMB's RW sigma is a **per-year vector** `sel_ch_sig_fsh(i)` read from
`../data/selvar24.dat` (col 1 = fsh, col 2 = bts, col 3 = ats). A year is a change
year only where the value is `> 0`, and that value **is** the sigma:

```
fsh:   0.5   x60   (1964 = 0 -> not a change year; 1965-2024)
bts:   0     x61   (BTS is logistic - no non-parametric devs)
ats:   0.138 x30   (1964-1994 = 0; 1995-2024)
```

Constant wherever non-zero ⇒ Rceattle's scalar `sel_dev_sd` is exactly equivalent.
The config's `0.5` / `0.138` came from this file. Also re-confirms the counts:
fishery 60 change years (`12 + 60×12 = 732`), ATS 30 (`7 + 30×7 = 217`).

BTS is separate: LogisticPM (`cpp:2745`) uses only fixed weights —
`Sel_curve_pen1 = 2` (= `ctrl_flag(26)`) and `Sel_curve_pen3 = 8` — both matching.
`Time_varying_sel_sd_prior[BTS] = 1` is **inert** for type 11.

Penalty→sigma mapping (verified against ADMB's own comments: weight 8 = "25% CV",
3.125 = "40% CV"): ADMB `w·Σx²` vs Rceattle `x²/(2σ²)` ⇒ **σ = 1/√(2w)**.

---

## THE OPEN PROBLEM

With everything above applied:

| run | result |
|---|---|
| `phase=TRUE`, `estimateMode=0` | **SSB(1978) = 0.0** (collapse), max\|grad\| 1.55 on `rec_pars`, Hessian not invertible, `log_F` on a bound, **every phase** ended with max\|grad\| 23–7600 |
| `phase=FALSE`, `estimateMode=1` | **4593 s (76.5 min)** vs ADMB 25 s; `opt$objective` empty; `obj$gr()` dies in `EvalADFunObject` |

Phasing is **not** the cause — a single unphased hindcast optimization takes 76 minutes
and ends unevaluable. Parameters/variances/penalties all match ADMB, so the model spec
is no longer a plausible explanation.

### ⚠️ Retracted from this session
- **All timing numbers, and the "70× slower" claim.** They were measured under
  `estimateMode = 3`, which the cpp defines as `jnll = dummy * dummy`
  (`ceattle_v01_11.cpp:3747-3754`) — a build/dimension check, **not a real objective**
  (`fn = 0`, `grad = 0`). `estimateMode = 4` does the same. Only `estimateMode < 3`
  evaluates `jnll_comp.sum()`. The 4593 s figure above **is** valid (`estimateMode = 1`).
- Parameter counts from `estimateMode = 3` **are** valid — `build_map` is mode-independent.

---

## Next steps

1. **Forward pass with N refreshed** (the decisive model-vs-optimizer split).
   `_rceattle_fwdpass.R` sets `estDynamics = 1`, which **fixes N from `NByageFixed`
   rather than computing it** — that is why it once matched SSB to ~6 sig figs (N was
   injected, so it tested the likelihood *given* N, never the dynamics).
   `_rceattle_fwdpass_aligned.R` (written this session) retargets it at `m23_rceattle`
   and fixes the renamed `*_bts_dev_est` params, but **must not be trusted until
   `NByageFixed` is refreshed from `m23_rceattle/pm.rep`** — the first run fed m23-era N
   to m23_rceattle params (`Catch = 9875`, total off by +143945; discard).
   - objective 740.5251 & grad ≈ 0 → identical model ⇒ optimizer is at fault
   - objective 740.5251 & grad ≠ 0 → likelihoods differ subtly
   - objective ≠ 740.5251 → structural difference still hiding
2. **Profile the 76 min.** No valid per-gradient timing exists yet. Build with
   `estimateMode = 1` and time `obj$fn` / `obj$gr` on *distinct* parameter vectors
   (TMB memoizes on the parameter vector — perturb to defeat the cache).
3. **Find why `obj$gr()` errors** after the optimization — likely NaN in the parameter
   vector; the `log_F`-on-a-bound + SSB→0 collapse points the same way.
4. Test suite (`NOT_CRAN=true`) was still running at handoff — it **gates the
   `Sel_start_year` default change**, which is a behavior change for any model with
   time-varying selectivity on a fleet whose data starts after `styr`. Previous
   behaviour is recovered with `Sel_start_year = styr`.

---

## BOUNDS — strong open lead (Grant's question, session end)

`R/4-build_parameter_bounds.R` vs ADMB's `init_bounded_*` declarations:

| ADMB param (bounds) | Rceattle param | Rceattle bounds | |
|---|---|---|---|
| `sel_devs_fsh` / `sel_devs_ats` (**±5**) | `sel_coff_dev` | **±Inf** — never mentioned in the file | ❌ **949 params = 78% of the vector, unbounded** |
| BTS devs (±5 in stock m23) | `log_sel_slp_dev`, `sel_inf_dev` | **±Inf** — bounds **commented out**, L59–77 | ❌ |
| `log_rec_devs` (**±10**) | `rec_dev` | ±15 | looser |
| `log_initdevs` (**±15**) | `init_dev` | [−1000, **23**] | much looser |
| `log_F_devs` ±15 + `log_avg_F` = −1.6 ⇒ **log_F ∈ [−16.6, 13.4]** | `log_F` | [−1000, **10**] | tighter above, unbounded below |
| `sel_slp_bts` (0.001, 5) | `log_sel_slp` | ±Inf (commented out) | |
| `sel_a50_bts` (0.1, 8) | `sel_inf` | ±Inf (commented out) | |
| `sel_coffs_fsh/ats`, `log_avgrec`, `log_q_*` | `sel_coff`(off), `rec_pars`, `index_log_q` | ±Inf | ✅ ADMB unbounded too |

**⛔ HYPOTHESIS REFUTED — do not re-run this.** `sel_coff_dev` being unbounded is NOT
the cause. Checked `m23_rceattle/pm.par` for parameters at their bounds:

```
sel_devs_fsh  n=720  max|val| = 1.4531  bound ±5    at-bound: 0   headroom 3.55
sel_devs_ats  n=210  max|val| = 0.7279  bound ±5    at-bound: 0   headroom 4.27
log_rec_devs  n= 61  max|val| = 1.4821  bound ±10   at-bound: 0   headroom 8.52
log_initdevs  n= 14  max|val| = 1.2258  bound ±15   at-bound: 0   headroom 13.77
log_F_devs    n= 61  max|val| = 1.7508  bound ±15   at-bound: 0   headroom 13.25
```

**Zero ADMB parameters are at a bound.** Its optimum is deeply interior — every
selectivity dev inside ±1.5 against a ±5 rail. Matching the bounds would be a no-op
at the solution. (One residual reason to try it: ADMB's `init_bounded_*` internally
transforms parameters onto an unbounded scale = implicit preconditioning, which TMB +
`nlminb` box constraints do not do. That is a *different* question — geometry, not
constraint. `ADMB/m23_rceattle_nobnd/` is staged (copied, unmodified) if anyone wants it.)

`log_F`'s upper bound of 10 ⇒ `F = exp(10) = 22026`, which annihilates the stock — that
is the **mechanism** of `SSB(1978) = 0` (the fit reported `log_F` on a bound). But it is
*tighter* than ADMB's effective 13.4, so the bound is not the root cause; the optimizer
*wanting* to go there is a symptom of something upstream.

Note the commented-out block's rationale — "If using blocks don't put bounds on
deviates, as these are estimated" — so any restoration must stay conditional on
`Time_varying_sel != "Block"`, as the original code was.

### Next experiment
Add ADMB's ±5 to `sel_coff_dev` (and optionally ±5 to the BTS devs, `rec_dev` ±10,
`init_dev` ±15) and re-run `phase=FALSE, estimateMode=1`. If it converges near
740.5251, unbounded devs were the problem. This is a **package behaviour change** —
gate it on golden-reference equivalence (`BS2017SS` objective 10241.030427).

---

## Where the failure actually is: 1978 (chased, session end)

`log_F` by year from the phased fit (`scratchpad/match2_bap0.rds`,
`f$estimated_params$log_F` — note `f$opt` is nested `f$opt$opt` and carries no `par`):

```
1977   log_F = -0.234    F =     0.79    SSB = 534.8    R =   1,906
1978   log_F = 10.0000   F = 22,026.47   SSB =   0.0    R =     554   <- pinned AT the bound
1979   log_F =  5.0023   F =    148.75   SSB =   0.0    R = 280,980
1980   log_F =  0.736    F =      2.09   SSB =  23.0    R =  30,812
```

Only 1978 and 1979 are affected; every other year is sane (`log_F` −0.2..−1.6, F 0.2–0.8).

**F is a SYMPTOM, not the cause.** The catch data for 1978 is present and normal
(979.431 kt, `Log_sd` 0.05; no years missing from `catch_data`). The population collapses
*first* — R decays geometrically 1974→78: **16,067 → 7,250 → 4,310 → 1,906 → 554**
(≈ halving yearly) with SSB 899 → 780 → 655 → 535 → 0. Extracting a 979 kt catch from a
stock that small forces F to absurdity. ADMB has `SSB(1978) = 1044.9`, `R(1978) = 24,256`
— **Rceattle's recruitment is 44× low**. `rec_pars` also carried the largest gradient
(1.55) in the failed fit, implicating recruitment from two directions.

Caveat: that fit is **not converged**, so specific values may be artifacts. But a
*geometric* decay is systematic, not optimizer noise.

## Catch units: IDENTICAL — not the problem

`pm.rep` `obs_catch` is a bare row vector from styr (no year column). Aligned:

| year | ADMB `obs_catch` | Rceattle `catch_data$Catch` |
|---|---:|---:|
| 1974 | 1588.39 | 1588.390 |
| 1977 | 978.37 | 978.370 |
| **1978** | **979.431** | **979.431** |
| 1979 | 935.714 | 935.714 |

Exact, every year. ADMB fits it at `pred_catch(1978) = 983.776` with F ≈ 0.5. Same data,
same params, same variances — ADMB satisfies the catch comfortably; Rceattle cannot,
because its stock is already half ADMB's.

## ⚠️ TOP LEAD: weight-at-age units (Grant, unverified)

"Usually weight is kg and catch is mt." Catch is cleared (above), and at ~979 those are
clearly **thousands of tonnes**. So the open question is the **weight-at-age → biomass**
conversion. Rceattle's SSB is **~2× low** (534.8 vs 1044.9) — a kg/mt (or kg/kt) mismatch
in weight-at-age lands as a clean scale factor on biomass while leaving catch untouched,
which is exactly the observed shape. **A biomass scale error would PRODUCE the apparent
recruitment collapse, not the reverse** — so check this BEFORE chasing `rec_dev`.
Verify value-by-value against `pm.rep` (`wt_ssb` / `wt_fsh` / `wtage`), not by reasoning
about what columns ought to mean. Survey index scale is likewise unverified.

## 🚨 TEST SUITE: 2 failures (1 new + 1 pre-existing) — `Sel_start_year` default is BLOCKED

`NOT_CRAN=true testthat::test_dir("tests/testthat")`:

```
1. Error   (test-functions-retrospective.R:30:3)  - Rceattle::retrospective(ss_run, peels = 5)
2. Failure (test-selectivity-logisticpm.R:163:3)  - "LogisticPM defaults the f..."
                                                    Expected all(s > 0) TRUE; actual FALSE
```

**(1) is PRE-EXISTING — not caused by the `Sel_start_year` change.** It is a known
failure recorded before this work: the retrospective's parallel workers load the stale
*installed* Rceattle build rather than the `pkgload::load_all()` tree. Verify it still
reproduces on a clean checkout before spending time on it. (My initial reading — that
`retrospective()` peels years while the new default derives from the data, so the derived
value could shift under peeling — is a real coupling worth a sanity check, but it is NOT
what this failure is.)

**(2) IS mine and IS new** — the regression test added for the `sel_inf[2]` fix.
Selectivity is going non-positive, so either the fix is incomplete or the test encodes
the wrong expectation. **This is the one that blocks the `Sel_start_year` default.**

**Do not commit the `Sel_start_year` default change until both are resolved.**
Full log: `scratchpad/tests2.log`. Note the first suite run silently skipped everything
(`Reason: On CRAN`) — `NOT_CRAN=true` is required.

---

## Priority order for next session

1. **The two test failures** — they gate the `Sel_start_year` default.
2. **Weight-at-age units** vs `pm.rep`, value-by-value (top lead; would explain the 2× SSB
   gap and hence the "recruitment collapse").
3. **Forward pass with N refreshed** from `m23_rceattle/pm.rep` — still the cleanest
   model-vs-optimizer split.
4. Only then: `rec_dev` / the 76-min profile.

**Nothing committed, nothing pushed.**
