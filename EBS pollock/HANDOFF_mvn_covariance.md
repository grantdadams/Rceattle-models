> ## ⚠️ UPDATED 2026-07-15 — read `HANDOFF_admb_match_session2.md` first
>
> **The MVN feature described below is real and still valid** (verified 4 ways; TMB ==
> plain-R to 5.7e-14). **The match numbers are stale.** "est-fit SSB cor 0.99 but +15%
> high" described the *old* comparison against `ADMB/m23`. The target is now
> `ADMB/m23_rceattle` (structurally aligned), reference **objective 740.525106862990,
> 1224 active params**.
>
> Since then: parameters/variances/penalties reconciled **exactly** (1218 = 1224 − 6);
> two real Rceattle bugs found and fixed (`build_params` `sel_inf[2]` init;
> `build_map` ignoring `Sel_start_year`); and the fit now **fails hard** —
> `SSB(1978) = 0`, `log_F` pinned at its bound, 4593 s vs ADMB's 25 s. Top lead is
> **weight-at-age units** (Rceattle SSB ~2× low). Catch units and bounds are cleared.

# Handoff: covariance (MVN) BTS survey likelihood in Rceattle + m23-MVN match

Date: 2026-07-05/06 (overnight autonomous session). Nothing pushed — everything is
in the `dev-ebs-pk` working tree for your review. Companion to
`HANDOFF_rtmb_admb_matching.md` and `HANDOFF_pollock_bridging.md`.

---

## TL;DR

1. **New Rceattle feature (implemented, compiled, tested): a full variance-covariance
   (MVN) survey-index likelihood.** A survey fleet can use a VAST-style Σ instead of IID
   lognormal, via `fleet_control$Index_loglike = "MVN"` + an `index_cov` list, reproducing
   the AMAK/ebswp `DoCovBTS=1` likelihood `0.5·r'Σ⁻¹r`. Companion arithmetic-mean q
   (`Catchability = "AnalyticalArith"`). **Fully back-compatible** — default is `"Lognormal"`,
   existing models are numerically unchanged.

2. **The feature is verified correct four independent ways** (see §2). On matching state it
   reproduces ADMB's `surv_like(1)` **exactly**.

3. **Your m23-MVN rerun landed while I worked** (`control.dat: DoCovBTS 1`, `pm.rep`/`pm.par`
   regenerated 07-05). The BTS target is now **`surv_like(1) = 31.0299`** — *not* the
   ~2.4e-5 the earlier analysis saw (that was the old `DoCovBTS=0` shipped run, where a
   missing-`break` fall-through applied Σ to *log* residuals and nulled BTS to ≈0). With
   MVN genuinely on, ADMB now fits BTS biomass, exactly as you predicted.

4. **Estimation fit (the real test): Rceattle-MVN vs ADMB m23-MVN — correlation 0.9882.**
   Strong shape agreement (supports the "they converge with MVN on" hypothesis). Residual
   **+15% mean level offset** (Rceattle runs high) + a non-PD Hessian (weakly-identified BTS
   selectivity devs) remain — both well-characterized, neither is the MVN feature. See §4.

5. **Four decisions are yours** (§6): native `MVNORM` vs the bare form; `bias_adjust_proc`;
   the BTS sel-dev identifiability; and the data-workflow RFC scope.

---

## 1. The feature — how to use it

```r
mydata <- Rceattle::read_data("Data/2024_EBS_pollock.xlsx")
mydata$fleet_control$Index_loglike[fcn == "BTS"] <- "MVN"            # was "Lognormal"
mydata$fleet_control$Catchability[fcn == "BTS"]  <- "AnalyticalArith" # mean(obs)/mean(pred), AMAK q
mydata$index_cov <- list(BTS = as.matrix(read.table("ADMB/data/cov_2024.dat")))  # 42x42 Σ, keyed by Fleet_name
fit <- Rceattle::fit_mod(data_list = mydata, ...)
```

- `Index_loglike`: `"Lognormal"` (default, unchanged) / `"MVN"` (bare `0.5 r'Σ⁻¹r`, reports the
  AMAK/ADMB value 31.03) / `"MVNORM"` (full TMB `density::MVNORM(Σ)` normalized density = MVN +
  `0.5(logdet Σ + n·log2π)`; identical fit, proper likelihood). Both use TMB-native MVNORM.
- `Catchability`: adds `"AnalyticalArith"` (=7) = `mean(obs)/mean(pred)`. The old `"Analytical"`
  (=3) stays geometric-mean.
- `index_cov`: named list of Σ per covariance fleet, keyed by `Fleet_name` (or `Fleet_code`).
  Must be square/symmetric, dim = # fitted survey obs (Year∈[styr,endyr], Obs>0), in
  index_data row order.
- Likelihood added: `density::MVNORM(Σ)` on `r = obs − q·pred` (natural scale), reusing
  `jnll_comp` row 0 (no magic-integer renumber). Non-covariance fleets carry a 1×1 inert
  dummy (the present-but-inert pattern).
- Early validation: `data_check()` errors clearly if an MVN fleet has no Σ / wrong-dim Σ /
  non-symmetric Σ.

## 2. Verification (four ways — all green)

| Check | Result |
|---|---|
| TMB `jnll_comp[BTS]` vs plain-R `0.5 r'Σ⁻¹r` (same state) | **1e-14 / 5.7e-14** (machine precision) |
| Arithmetic-mean q property (`sum(pred)==sum(obs)`) | holds |
| `0.5 r'Σ⁻¹r` on **ADMB's own** `ob_bts/eb_bts` | **31.0298 ≈ ADMB 31.0299** |
| End-to-end forward pass, N refreshed from current `pm.rep` | BTS slot **29.01** (from 151→29 after N refresh); residual 2.02 = fishery-F bridge artifact (diff #11), not the feature; SSB₁₉₆₄ **exact** (1568.15 vs 1568.15) |

The forward-pass residual 2.02 is because the bridge's fishery non-parametric sel→`NonParametricPM`
mapping doesn't perfectly reproduce ADMB `sel_fsh`, so survey survival `√S=exp(−Z/2)` differs.
N, weights, and BTS selectivity all match ADMB exactly. Closing it needs the bridge to also
inject realized `sel_fsh` (like ATS) — outside the feature.

## 3. Backward-compatibility (confirmed)

- **157 tests pass across every changed path**, 0 failures: new `test-likelihood-index-covariance.R`
  (8), `test-likelihood-index-calculations.R` (5), `test-likelihood-osa-residuals.R` (101),
  `test-data-functions.R` + `test-data-optional-fields.R` (43).
- `BS2017SS` fit objective unchanged (10241.030427), `index_ll_type` all 0, dummy precision list.
- The **only** failing test is `test-functions-retrospective.R` — **pre-existing and not mine**
  (its parallel workers `library(Rceattle)` the stale *installed* build; byte-identical error
  with/without my changes; I touched no params/map/retro code).
- Two test-infra fixes were needed and made (§5).
- Full suite is ~2 h locally (heavy TMB integration fits); I confirmed the changed paths
  directly rather than wait it out. **Run the full suite / CI before release.**

## 4. Estimation fit: Rceattle-MVN vs ADMB m23-MVN

Config = your `2024 EBS pollock.R` (Model 1: `initMode=2`, `sigma_rec_prior=0.707`,
Fishery `NonParametricPM`-RW σ0.5, BTS `LogisticPM`-RW σ0.1, ATS estimated q, M fixed)
**+ BTS MVN + `AnalyticalArith` q + `cov_2024.dat` + `fit_control(bias_adjust_proc=0)`.**
(Script: `scratchpad/pollock_mvn_estimate.R`; fit saved to `scratchpad/pollock_mvn_fit.rds`.)

| Metric | Value |
|---|---|
| SSB correlation vs ADMB m23-MVN | **0.9882** |
| SSB mean\|%diff\| / max | 15.4% / 45.8% |
| SSB 1964 / 2024 (R vs ADMB) | 2287 vs 1568 (+46%) / 3802 vs 3420 (+11%) |
| BTS MVN `jnll_comp` (free fit) | 61.83 (ADMB 31.03) |
| Convergence | max\|grad\| ok, **Hessian not PD**; 64 non-identifiable `log_sel_slp_dev`/`sel_inf_dev` |

**Reading it:** the 0.99 correlation is the headline — with MVN on, Rceattle tracks the ADMB
m23-MVN trajectory closely in shape, consistent with your hypothesis that turning covariance
on brings the models together. The **level offset flipped sign** vs the old handoff (RTMB ran
~2.6× *low* → Rceattle-MVN now runs ~15% *high*), which strongly implicates **`bias_adjust_proc`**:
setting it to 0 (to match ADMB's centered-at-0 rec/init penalties, per the dev-vector analysis)
removed the −σ²/2 downward shift and appears to overshoot. Worth a small sweep (§6). The non-PD
Hessian is the documented weakly-identified BTS selectivity-deviation issue — needs a tighter
dev penalty/prior or fixing more devs, independent of the covariance work.

## 5. dev_vectors (your note) — resolved

Investigated across ADMB / RTMB / Rceattle. Conclusions:
- The **only genuine ADMB `dev_vector`** (sum-to-zero) here is the **BTS logistic selectivity RW**.
  `log_initdevs`/`log_rec_devs` are plain bounded vectors with ridge penalties — no sum-to-zero.
- **Rceattle already fixes the first BTS RW dev** automatically (`build_map` for
  `LogisticPM`+`RandomWalk`) — your instinct, already implemented. No manual override needed.
- RTMB does **not** fix first elements (it can't); it mean-centers penalties instead. Different
  reparameterization, same fit.
- **Key knob:** `fit_control(bias_adjust_proc = 0)` to match ADMB's centered-at-0 penalties —
  but see §4 (may overshoot; sweep it).

## 6. Decisions for you

1. **Native `MVNORM` vs bare form — RESOLVED as a per-fleet option (your suggestion).**
   `Index_loglike` now takes `"MVN"` (bare `0.5 r'Σ⁻¹r`, reports 31.03, ADMB/AMAK parity) *or*
   `"MVNORM"` (full TMB `density::MVNORM(Σ)` normalized density). Both drive the same TMB-native
   MVNORM factorization (robust; no explicit inverse); `"MVN"` just subtracts the fixed constant
   `0.5(logdet Σ + n·log2π) = 318.945`. Verified: bare = plain-R `0.5 r'Σ⁻¹r` to 5.7e-14; full =
   bare + 318.945 exactly; identical fit (constant has zero gradient). Under `"MVNORM"` the
   reported BTS value is 31.03 + 318.945 = **349.98** — so use `"MVN"` when comparing to ebswp's
   reported `surv_like`, `"MVNORM"` when you want a proper normalized likelihood (e.g. cross-model
   comparison). Nothing more needed from you here.
2. **`bias_adjust_proc`.** 0 overshoots (+15% high); default 1 ran low historically. Sweep
   {0, 1} × maybe a middle, and check SSB level vs ADMB m23-MVN.
3. **BTS sel-dev identifiability** (non-PD Hessian). Tighten the BTS dev penalty / prior or fix
   more devs. Pre-existing; blocks clean SEs.
4. **Data-workflow RFC** (`../DATA_WORKFLOW_RFC.md`): the one open fork is whether to absorb
   the `fit_mod()` run-control switches into the data object (Phase 3). My weak rec: the middle
   path (optional `model_config` slot). Your call.

## 7. Reviewable diff (package, unpushed on `dev-ebs-pk`)

```
NEWS.md                    +16   feature bullet (4.7.0)
R/0-switches.R             +56   index_loglike_map (Lognormal/MVN/MVNORM), AnalyticalArith q, guards, validate/convert
R/0-clean_data.R           +10   index_cov dummy-off default
R/1-data_check.R           +44   MVN/MVNORM Σ validation (missing/dim/symmetry)
R/5-rearrange_data.R       +49   index_ll_type, per-fleet Σ list + normalizing constant
R/data.R / man/BS2017SS.Rd  +5   roxygen (Index_loglike, index_cov, AnalyticalArith)
src/TMB/ceattle_v01_11.cpp +95   LOM_t struct, DATA_STRUCT(Sigma)+const, arithmetic q, density::MVNORM branch
tests/testthat/helpers.R    +7   make_test_data now switch_check()s (fixes pre-existing raw-fixture gap)
tests/testthat/test-likelihood-index-covariance.R  NEW  regression test (self-consistency, arith-q, MVNORM==MVN+const, validation)
```
Two test-infra fixes worth noting: (a) `revert_switches`/`convert_switches` now default a
missing `Index_loglike` column (a hand-built fleet_control passed straight to `rearrange_data`
was erroring); (b) `make_test_data()` now runs `switch_check()` so the fixture is usable by the
direct-`rearrange_data()` tests — this fixes the pre-existing `Sel_start_year` regression the
prior handoff flagged.

Version stays 4.7.0 (open dev; last release 4.6.0). NEWS bullet added.

## 8. Scratch (session, safe to delete)
`scratchpad/pollock_mvn_estimate.R` (+`.log`, `pollock_mvn_fit.rds`) — the estimation fit.
`../DATA_WORKFLOW_RFC.md` — the Part-2 RFC (kept; move/track as you like).
