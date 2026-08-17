# 2024 Aleutian Islands Pacific cod: Stock Synthesis → Rceattle bridge

This folder bridges the 2024 AI Pcod assessment from Stock Synthesis 3.30
(`../SS3/run`) into Rceattle, following the same two-stage pattern as the GOA
cod (`../../GOA cod/Bridging`) and BSAI Alaska plaice bridges:

1. **Forward pass** ([ss3_to_ceattle_forward_pass.R](ss3_to_ceattle_forward_pass.R))
   — inject the SS3 MLE parameter estimates into Rceattle and run with
   `estimateMode = 3` (no estimation). Verifies Rceattle reproduces SS3's
   population trajectory and characterises every likelihood component.
2. **Estimation** ([ss3_to_ceattle_estimate.R](ss3_to_ceattle_estimate.R))
   — start from the SS3 values and let Rceattle's TMB optimiser estimate
   (`estimateMode = 1`), then compare the converged fit to SS3.

Both reuse the generic converter [../R/ss3_to_rceattle.R](../R/ss3_to_rceattle.R)
(copied from the GOA cod work) to turn the SS3 `.dat`/`.ctl`/`.par`/`Report.sso`
into an Rceattle `data_list`.

## How to run

```r
setwd(".../AI cod")
source("Bridging/ss3_to_ceattle_forward_pass.R")   # forward-pass parity
source("Bridging/ss3_to_ceattle_estimate.R")       # estimation + SS3 comparison
```

`ss3_to_ceattle_estimate.R` sources the forward-pass script first (it reuses the
configured data list, the SS3-injected `inits`, the Richards growth spec, and
the M-block spec), so running the estimate script alone runs the whole pipeline.
It writes `AIcod_estimate_vs_SS3.pdf` (biomass/SSB/recruitment overlays).

## Model structure (AI Pcod vs GOA Pcod)

| Feature | AI Pcod 2024 | Note |
|---|---|---|
| Fleets | 2 (FshComb fishery + Srv survey) | simpler than GOA cod (5) |
| Sexes | 1 (Ngenders = 1, FracFemale = 0.5) | SSB **not** halved (see below) |
| Years / ages | 1991–2024 / ages 0–13 | |
| Growth | **Richards (GrowthModel = 2)** | not von Bertalanffy |
| Selectivity | length DoubleNormal (pattern 24), both asymptotic | no time blocks |
| M | 0.417 base, 0.579 for 2016–2024 (block) | block design 2 |
| Survey q | simple (link 1), no env-q | simpler than GOA cod |
| SR | Beverton-Holt, log(R0)=11.37, sigmaR=0.832 | steepness fixed = 1 |
| Initial state | InitF = 0.0595 (fished non-equilibrium) | |

## Forward-pass parity (estimateMode = 3, SS3 MLEs injected)

Population trajectory reproduces SS3:

| Quantity | Max rel. err vs SS3 |
|---|---|
| Recruitment | ~5e-6 (exact) |
| Total biomass | ~1.7% |
| Spawning biomass | ~6.1% (Jensen gap, below) |

Likelihood components (Rceattle vs SS3), with the cause of each gap:

| Component | Rceattle | SS3 | Cause of difference |
|---|---|---|---|
| Survey/Index | 4.6 | −8.3 | +0.5·log(2π)·n_obs index normalising constant (expected) |
| Catch | 23.7 | ~0 | selectivity-form (4-param DN vs 6-param SS3) → sel-at-age |
| Length comp | 204 | 127 | residual ALK/sel shape; predicted comps correlate 0.98 w/ SS3 |
| CAAL (age) | 1083 | 356 | after injecting the SS3 ageing-error matrix (was 2262 with the converter's identity matrix); residual ≈ the `dmultinom` lgamma constant SS3 omits (deviance-form ≈ 423) |
| Recruitment | 35 | −1.1 | Methot-Taylor bias ramp not implemented (by design) |

## Estimation parity (estimateMode = 1, started from SS3)

Converges with a positive-definite Hessian (max |gradient| ≈ 1e-3). The
Rceattle MLE tracks SS3 within ~10–20%:

| Quantity | Max rel. err vs SS3 |
|---|---|
| Spawning biomass | ~20% (Rceattle runs ~7–20% above SS3) |
| Total biomass | ~22% |
| Recruitment | main years <25%; terminal 2019–2021 ~5–17% (after fixing forecast recdevs) |

| Parameter | SS3 | Rceattle MLE |
|---|---|---|
| log(R0) | 11.37 | 10.99 |
| M base | 0.417 | 0.417 (fixed) |
| M block (2016+) | 0.579 | 0.576 |
| ln q (Srv) | −0.138 | −0.292 |

The biomass offset is largely a q ↔ scale trade-off (Rceattle estimates a lower
q and a higher biomass than SS3, with a lower R0), driven by the selectivity and
likelihood-form differences below.

### Estimable-parameter count

| Class | Rceattle | SS3 | |
|---|---|---|---|
| R0, M-block, q, InitF | 4 | 4 | matched |
| Selectivity (peak P1 + ascending P3, both fleets) | 4 | 4 | matched |
| Recruitment (rec_dev 1991–2021 = 31 + init_dev = 13) | 44 | 44 | matched (init_dev ↔ SS3 early recdevs 1978–1990; forecast 2022–24 fixed) |
| Growth (K, L1, Linf, Richards `m`) | 0 | 4 | **−4** (held fixed; degenerate — see below) |
| Fishing mortality | 34 (`log_F`) | 0 | **+34** (SS3 hybrid F method 3 solves F internally; Rceattle estimates annual `log_F` pinned by catch) |
| **Total** | **86** | **56** | |

Every comparable parameter class matches SS3 exactly. The two count differences
are structural: SS3's **hybrid F** (method 3) estimates no F parameters, and we
hold growth fixed. The hybrid-F difference is not reconcilable without a
hybrid-F mode in Rceattle (F is always an estimated parameter there, determined
by the catch likelihood).

---

## Structural differences that limit exact replication

These are the SS3 features that Rceattle's current formulation cannot reproduce
to machine precision. They are intrinsic to the two platforms, not bugs in the
bridge.

1. **Selectivity functional form.** SS3 uses the 6-parameter pattern-24 double
   normal (P1 peak, P2 plateau width, P3/P4 ascending/descending widths, P5/P6
   end floors). Rceattle's `DoubleNormal` (case 8) is a 4-parameter
   simplification (peak, σ_asc, σ_desc, right-tail floor). For AI cod both
   fleets are **asymptotic**, so the 4-param form reproduces the realised
   selectivity-at-length to SSE ≈ 1e-4 — but the derived selectivity-**at-age**
   (length-sel convolved with the ALK) differs slightly from SS3's reported
   `Asel2`, which propagates into the catch fit. *A faithful 6-param SS3
   double normal on the Rceattle dev branch would remove this (worthwhile for
   dome-shaped stocks; not the limiting factor for AI cod).*

2. **Empirical selectivity is age-only.** Rceattle's empirical-selectivity path
   (`Selectivity = "Fixed"`) populates `sel_at_age` but not `sel_at_length`, so
   length-comp / CAAL predictions collapse to 0. We therefore must use a
   parametric **length** selectivity for any model with length composition data
   (hence point 1). Empirical growth (`growth_model = 0`) is also unusable here:
   in this build the C++ `growth_matrix` is left unpopulated in the empirical
   branch, so both length-comp and CAAL predictions collapse.

3. **Richards growth — reproduces, but held fixed in estimation.** Despite being
   flagged as unvalidated, Rceattle's Richards path reproduces SS3's
   length-at-age to 2 decimals for ages 0–12 (only the age-13 plus group differs,
   111.3 vs 114.5). SS3 estimates K, L_at_Amin, L_at_Amax **and the Richards
   shape `m`** (all phase ≥ 2, no priors — PR_type = 0). We hold all of growth,
   including `m`, **fixed at the SS3 MLEs** during estimation. See the dedicated
   section below for the empirical justification.

4. **Growth SD parameterisation.** SS3 stores a constant CV (CV_Growth_Pattern
   = 0 → SD = CV·L). Rceattle's `growth_log_sd` is an absolute SD in cm,
   linearly interpolated by length between SD(L1) and SD(Linf). We inject
   SD(L1) = CV_young·L1 and SD(Linf) = CV_old·Linf so the endpoints match; the
   interior differs slightly, broadening/narrowing the ALK at mid-ages.

5. **Single-sex SSB scaling.** SS3 (Ngenders = 1, *not* −1) reports SpawnBio =
   Σ N·Mat_F_wtatage with **no** FracFemale multiplier (verified to 5 sig-figs),
   so `sex_ratio` is set to 1.0. A residual ~5% **Jensen gap** remains because
   Rceattle multiplies separately age-integrated maturity and weight, whereas
   SS3's `Mat_F_wtatage` integrates maturity(L)·weight(L) jointly over the
   length distribution. (Same gap documented for GOA cod.)

6. **Mid-season catch weight.** SS3's catch equation uses mid-season body
   weight (`SelWt` ≈ Wt_Mid), ~15–20% heavier than begin-year weight at mid
   ages. Setting `fleet_control$Month = 6` for the fishery makes Rceattle
   compute the catch weight at mid-year, which cut the catch NLL from 567 → 24
   and improved total biomass parity from ~4.5% → ~1.7%.

7. **Composition likelihood form.** SS3 reports the multinomial **deviance**
   kernel. We use `MultinomialAFSC` (= Martin's deviance form, −N·(obs+ε)·
   log((hat+ε)/(obs+ε))) for length comps so the absolute NLL is comparable to
   SS3's `Length_comp`. CAAL only supports the full `Multinomial` (with the
   `dmultinom` lgamma constant SS3 omits) or Dirichlet-multinomial in this
   build, so the CAAL absolute NLL carries a constant offset.

8. **Variance adjustment (data weighting).** SS3's ctl applies Francis/TA1.8
   multipliers to comp effective N (Factor 4 lengths ×0.04/0.07, Factor 5 ages
   ×0.14). The converter copies raw input Nsamp; the bridge re-applies these
   multipliers, without which the comp NLL is ~25× too large.

9. **CAAL length-axis padding.** SS3's length comps span all 143 data bins but
   CAAL is tabulated only over 103 bins (12.5–115.5 cm). Rceattle (parametric
   growth) requires `unique(caal Length) == nlengths`. We keep all 143 bins
   (length comps unchanged) and pad `caal_data` with 40 ghost rows
   (`Year = −styr`, `Sample_size = 0`) so the count matches; ghosts are excluded
   from the likelihood by the `yr > 0` guard.

9b. **Ageing-error matrix.** The converter installs an identity (no-error)
    ageing matrix; SS3 has real ageing error (`age_error_sd` grows ~0.03 → 0.6+
    by age 5). The forward pass rebuilds the SS3 matrix (integrating
    N(mean_a, sd_a) over the observed-age bins) and injects it — this cut the
    CAAL NLL from 2262 → 1083 (the predicted age-at-length was too sharp
    without it). The residual CAAL gap to SS3 is then mostly the `dmultinom`
    constant (point 7).

10. **Recruitment bias ramp.** The Methot-Taylor bias-adjustment ramp is not
    implemented in Rceattle (per project scope). Recruitment deviations are
    injected with the ramp applied so realised recruitment matches SS3, but the
    recruitment *likelihood* diverges by design.

11. **Selectivity right-floor non-identifiability.** Both fleets are asymptotic,
    so the DoubleNormal right-tail floor logit sits on a flat likelihood ridge
    (`check_estimability` flags it). It is fixed during estimation; without this
    the Hessian is not invertible.

12. **Phase / optimiser differences.** Per project scope, ADMB phasing and the
    SS3 optimiser algorithm are not matched to Rceattle's TMB optimiser; small
    differences in the converged solution are expected and acceptable.

---

## Why growth is held fixed in estimation

SS3 estimates growth (K, L_at_Amin, L_at_Amax, Richards shape `m`); we tried to
do the same in Rceattle and found it degenerate for this stock. The findings:

- **The growth prior mechanism works.** The rebuilt Rceattle re-targets a prior
  on a growth `(Intercept)` onto the base `log_growth_pars` (back-transformed
  through the log link), verified by the new
  `Rceattle/tests/testthat/tests-Linkage/test-intercept-prior-base-parameter.R`
  (growth K/L1/Linf and M, normal + lognormal families, 11 assertions). The
  prior contributes the expected `−Σ dnorm(param, μ, σ)` to jnll row 20. (The
  *installed* 4.5.0 predated this wiring, so the prior was silently dropped
  until the rebuild — worth knowing if results differ across machines.)

- **Estimating growth collapses the terminal trajectory.** Freeing K/L1/Linf
  (even with tight priors and a warm start from the converged growth-fixed fit)
  drives terminal SSB to ~360 (vs SS3 49,350), with a log_F gradient ~200 and a
  non-invertible Hessian. The trigger is the **unconstrained terminal
  recruitment**: SS3's Methot-Taylor bias-adjustment ramp (out of scope here)
  is what holds recent recdevs down; without it, freeing growth lets them crash.
  Pinning M_block at the SS3 value does **not** fix it.

- **It is not the comp down-weighting.** A/B test (free growth):
  | weighting | terminal SSB | Linf |
  |---|---|---|
  | SS3 Francis down-weights (matches SS3) | 362 | 113.6 |
  | full Nsamp (no down-weights) | 121 | 107.9 |
  Removing the down-weights makes it **worse** and pulls Linf further from SS3.
  Rceattle's CAAL intrinsically prefers Linf ≈ 108–113 (< SS3's 123) regardless
  of weighting, so SS3's Linf is not recoverable from the CAAL alone here — it
  is held up by SS3's growth-fitting machinery (phasing + bias-ramp-constrained
  recruitment) that we deliberately do not replicate. Keeping the SS3 Francis
  weights is therefore correct (they are part of SS3's likelihood).

- **Conclusion.** Growth (incl. `m`) is fixed at the SS3 MLEs. The forward pass
  proved this reproduces SS3 length-at-age, and with growth fixed the estimation
  converges cleanly (PD Hessian, max |grad| ≈ 8e-4) and tracks SS3 within ~15%
  (SSB) / ~20% (Bio). Estimating growth would require implementing a terminal-
  recruitment constraint (bias-ramp analog) and/or parameter phasing.
