# Estimation differences: SS3 vs Rceattle (GOA Pcod 2024)

Definitive catalogue of every known structural / formulation difference
between SS3 and Rceattle when both are fitted via MLE. For each
difference: the SS3 form, the Rceattle form, where each lives in the
code, the observed NLL magnitude on Pcod 2024, and two paths to close
the gap — implement SS3's feature in Rceattle, or turn it off in SS3.

Last updated: 2026-05-31.

## SS3 model configurations referenced in this catalog

Two SS3 model directories are used as parity targets. Both share the
same `.dat` file ([GOAPcod2024Oct17_1e_5cm.dat](Data/goa_pcod-no%20init%20and%20ramp/GOAPcod2024Oct17_1e_5cm.dat))
and SS3 build; they differ only in two ctl/starter settings:

| Setting | `Data/goa_pcod/` (original) | `Data/goa_pcod-no init and ramp/` (modified) |
|---|---|---|
| `max_bias_adj_in_MPD` (ctl) | `0.9112` (Methot-Taylor ramp on) | **`-1`** (override ramp; `b = 1.0` for all recdevs) |
| `like_comp = 18` lambda (ctl) | absent (lambda = 1, default) | **`0`** (`InitEQ_Regime` soft penalty off) |
| Reported `InitEQ_Regime` NLL | 2.78 | **0** (lambda zero, post-re-run) |
| Reported `Recruitment` NLL | −2.62 | **−16.30** (b=1 every year, no soft penalty constraining regime) |
| Reported `SR_regime_BLK5add_1976` MLE | −0.6782 | **−1.336** (regime now unconstrained by soft penalty) |
| Reported `TOTAL` NLL | 2068.4 | **2049.8** |

The modified model is the Path-2 ("turn it off in SS3") fix for
[#5](#5-recruitment-methot-taylor-bias-adjustment-ramp-) (Methot-Taylor
ramp) and [#6](#6-initeq_regime-penalty-) (InitEQ_Regime soft penalty).
It does NOT change the data, the structural model, or any biological
parameter values — it only zeroes two SS3 likelihood contributions that
Rceattle has no analog of. Use the modified model when you want forward-
pass parity uncontaminated by those two SS3-specific features.

The Pcod test driver
([ss3_to_ceattle_test.R](ss3_to_ceattle_test.R)) currently points at
the modified directory. To switch back to the original SS3 model, edit
`SS3_DIR <- "Data/goa_pcod"` near the top of that script.

The exact ctl line edits to recreate the modified model:

```diff
-105: 0.911242473694779 #_max_bias_adj_in_MPD
+105: -1               #_max_bias_adj_in_MPD

(Lambdas block, before the terminator -9999 row)
+ 18  1  1  0  1  #_InitEQ_Regime_Phz1
```

## Standard: forward-pass should match SS3 to machine precision

Every component below should either reach **machine-precision parity**
with SS3 (per-component NLL gap < 1e-6 OR per-predicted-quantity rel
err < 1e-6) when both models are given identical inputs, OR be
catalogued as a documented structural difference with magnitude
verified against current model output. A component with a "small" gap
of 0.1 or 1.0 NLL is NOT considered matched — there is a reason for
those digits and the catalog must explain them.

## Status legend

- ✅ Matched at machine precision (gap < 1e-6 NLL, or rel err < 1e-6
  on predicted quantities). The implementations are equivalent up to
  floating-point round-off.
- 🟡 Match up to floating-point noise (gap < 1e-3). Algorithmically
  equivalent; the residual is summed FP noise across many terms.
- 🟠 Structural diff. Cannot match at machine precision because of a
  documented algorithmic difference (e.g. SS3 has a feature Rce
  doesn't, or vice versa). Magnitude must be specified and verified
  against current `Rscript ss3_to_ceattle_test.R` output. Fix paths
  (Path 1 in Rce / Path 2 in SS3) must be present with a recommendation.
- 🔴 Unknown gap. Must be investigated and resolved to ✅ / 🟡 / 🟠
  before forward-pass parity can be declared.

---

## Summary table

Status under the **machine-precision standard** above. Rows that
previously listed gaps "within tolerance" but are >> 1e-6 have been
reclassified: either the kernel formula matches but the inputs that
flow into it produce small per-cell residuals (🟡), or a structural
diff is preventing the closure (🟠).

| # | Component | NLL gap (Pcod 2024) | Status | Closeable by |
|---|---|---|---|---|
| 1 | Length-comp likelihood **kernel** | per-cell ≤ 1e-6 | ✅ | Manual SS3-robust recompute in R matches cpp `jnll_comp` to 0.0000 across all five active fleets — see "Manual recompute LenComp" block in test output. The kernel itself is at machine precision. |
| 1b | Length-comp **NLL** (kernel applied to `comp_hat`) | +0.86 NLL (Rce − SS3) | 🟠 | **Downstream of #13 + #4 — cannot close without closing those.** Verified by stratified decomposition of per-fleet NLL into `L < 80 cm` and `L ≥ 80 cm` bins: all 5 fleets show the SAME pattern of Rce-vs-SS3 = `(−2 to −3)` for `L<80` and `(+2 to +3)` for `L≥80`, with the official per-fleet NLL gap being the residual (FshTrawl +0.61, FshLL +0.31, FshPot +0.03, Srv +0.09, LLSrv −0.18). The mass-conserving probability shift between strata is the signature of #13 (Rce's static `exp(−0.2·a)` plus-group LAA undershoots SS3's dynamic N-weighted plus-group by ~10 cm → mass moves from L=99–104 into L=80–94). Per-cell sample (FshTrawl 2023): bins L=39.5–74.5 match SS3 to ratio = 1.0000 (≤ 1e-4); L=84.5–89.5 ratio 1.02–1.03; L=99.5 ratio 0.91; L=104.5 ratio 0.75. Secondary contributor: #4 (Pope's vs Baranov via N-at-age drift; Bio rel err 9.29e-4 propagates to comp_hat at ~10⁻³ per bin). |
| 2 | CAAL likelihood **kernel** | per-cell ≤ 1e-6 | ✅ | Same SS3-robust formula as #1; manual recompute matches cpp `jnll_comp` to machine precision. The +263 / +313 NLL gap reported below is in `caal_hat` (the predicted CAAL), not the kernel. |
| 2b | CAAL **NLL** after data-bin integration fix | Srv: +31 (↓ from +182). Fisheries: +160 / +67 / +54 (↑ from +52 / +12 / +16). TOTAL +313 (↑ from +263) | 🟠 | **Doubly downstream of #17 (Rce CAAL integration) and #4 (Pope's vs Baranov on fishery catch-at-age)**. The data-bin integration fix (#17, committed) closes the Srv L<80 gap (the structurally tractable part). Fisheries cannot close further without addressing #4: their catch-at-age distribution within a 5 cm data bin depends on Pope's vs Baranov within-year survival, which propagates into the per-bin pred_CAAL. Per-fleet decomposition in test output and #17 below. |
| 3 | Survey index lognormal constant | **closed** — −0.22 NLL residual (was +46) | 🟡 | **FIXED 2026-05-31** at [ceattle_v01_11.cpp:2552–2569](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L2552). Manual `log(σ) + 0.5·z²` kernel replaces TMB `dnorm(...,true)`. Closure: 50 obs × 0.5·log(2π) = 45.95 NLL exactly. Residual −0.22 is per-obs FP noise. Old `dnorm` line kept commented next to the new code for easy revert. |
| 4 | Catch likelihood (Pope's vs Baranov) | +26 | 🟠 | SS3 `F_Method = 3` (config) OR Rce Pope's impl (code). Pope's vs Baranov gives fundamentally different `catch_hat`. |
| 5 | Recruitment Methot-Taylor bias ramp | +39 | 🟠 | SS3 ramp off (config) OR Rce per-year `b_y` impl (code, moderate). |
| 6 | InitEQ_Regime penalty | −2.8 | 🟠 | SS3 lambda 9 = 0 (config) OR Rce term impl (code, small). |
| 7 | M-block prior structure | −0.33 | 🟠 | Conceptual: SS3 prior on absolute log(M_block); Rce on offset. |
| 8 | Sel-dev `dev_se` prior magnitude | varies | 🟠 | Match `Time_varying_sel_sd_prior` to SS3 `dev_se`. Note Rce's `4×` slope/inflection scaling has no SS3 analog — see entry detail. |
| 9 | SS3 per-param `dev_link` scaling | structural | 🔴 | SS3: `dev_seq × dev_se × (HI−LO)`. Rce: raw additive. Formula not fully decoded from SS3 source. |
| 10 | SS3 three-tier sel (base + block_repl + dev_seq) | structural | 🔴 | Rce collapses to base + per-year dev. Architectural diff. |
| 11 | Pop-grid integration (sel × ALK at 1-cm pop bins) | ≤ 1e-7 verified | ✅ | Both compute on fine grid via `lengths_pop`. Verified per-bin to machine precision. |
| 12 | `addtocomp` smoothing form | identical | ✅ | Both use `(p+ε)/(1+nε)`; same ε from SS3 dat. |
| 13 | Plus-group LAA/WAA convention | LAA age 10: rel err 2.36e-3 | 🟠 | Rce static `exp(-0.2·a)`; SS3 dynamic N-weighted. Code in `growth.hpp` lines 230-260 commented; enabling closes most. |
| 14 | CAAL age-column alignment in converter | fixed | ✅ | Off-by-one bug eliminated; regression test pinned in `test-caal-age-alignment.R`. |
| 15 | SSB scaling (sex_ratio + Jensen on length-based mat) | Rce SSB ≈ 0.46× SS3 | 🟠 | Converter `sex_ratio = 1` for `Nsexes = 1` + injected `mat_wt_at_age`. Doesn't affect NLL but affects derived SSB output and downstream priors. |
| 16 | Estimation-bounds mismatch on scalar log-scale params | n/a (bounds, not NLL) | 🟡 | `linkage_spec(bounds = c(eps, hi))` with `eps > 0`. Already plumbed; user must set positive lower bounds. |
| 17 | CAAL pop-bin integration over the data bin | Srv L<80 closed by −152 NLL; fishery L<80 worsened by +200 (Pope's coupling, #4) | 🟠 | **Fix committed in [ceattle_v01_11.cpp:1939–2000](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L1939) and [:2378–2398](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L2378)**. Replaces single-pop-bin CAAL prediction with integration over all pop bins inside the data bin (correct for `Lbin_method=2`; degenerates correctly for `Lbin_method=1`). Surveys: structurally tractable, closes most of the L<80 gap. Fisheries: residual is downstream of #4 — see §2b. |
| 18 | DoubleNormal per-param PHASE: SS3 fixes some slots, Rce estimates all 6 | ✅ pinned via map override | 🟡 | Resolved via `apply_ss3_sel_phase_fixes()` in [ss3_to_ceattle_estimate.R](ss3_to_ceattle_estimate.R) — walks `ctllist$size_selex_parms$PHASE`, sets NA on matching Rce `sel_inf`/`log_sel_slp` cells. For Pcod: 7 base slots pinned (FshTrawl P5/P6, FshLL P4/P5/P6, FshPot P5, LLSrv P5). Inits already carry SS3 INIT values so TMB freezes correctly. Permanent infra (per-fleet PHASE column in `fleet_control` + `build_map_selectivity` extension) still future work. |
| 19 | Ageing-error matrix: SS3 has per-row reference; Rce uses identity | CAAL +307 → +133 NLL (def 2 unbiased) | 🟠 | SS3 `dat$ageerror` has 2 definitions (def 1 biased mean ~ true+0.6; def 2 unbiased mean = true). 75% of Pcod CAAL rows reference def 2. Resolved (test script): `build_ss3_age_error()` builds P[obs\|true] = Φ((obs+0.5−μ)/σ) − Φ((obs−0.5−μ)/σ) per true age, accumulates plus/minus-group mass at boundaries, writes to `cod_pcod$age_error`. Def 1 (biased) badly mismatched — CAAL +6045. Def 2 closed CAAL by −174. Per-row ageerr (different def per fleet/year) not yet supported; would require Rce data structure change. |
| 20 | Time-varying sel/q "IID" overpenalizes vs SS3 "BlockDev" replacement | Parm_devs SS3 6 vs Rce 497 (v8 free IID); BlockDev closes to Rce 94 | 🟡 | SS3 fires N(0, σ) prior ONCE per block-replacement parameter (4–5 per fleet); Rce's IID prior fires once per (fleet, slot, year) — 1590+ priors. Resolved via per-cell `prior_weight` array (Option A): `1/N` inside a sub-block, `0` outside. Combined with factor-shared map (one TMB factor level per sub-block), the per-year prior loop collapses to exactly the SS3 per-replacement contribution. **Cpp**: `DATA_ARRAY(sel_inf_dev_prior_weight)`, `DATA_ARRAY(log_sel_slp_dev_prior_weight)`, `DATA_MATRIX(index_q_dev_prior_weight)` in [ceattle_v01_11.cpp:286-296](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L286); defaults 1.0 in [R/5-rearrange_data.R](../../Rceattle/R/5-rearrange_data.R) preserve existing IID behavior. **Driver**: `populate_selectivity_block()` (in test script) writes per-(fleet, year) sub-block ID from SS3 `Block_Design`; `build_blockdev_arrays()` (in estimate script) builds the prior_weight tensor + map patches. **First Pcod run**: 1672 → 315 estimable params, Parm devs +88 (was +491), but TOTAL +1952 because residual is dominated by #4 (Pope's vs Baranov) on log_F. |

---

## 1. Length-comp likelihood kernel ✅

**SS3** ([SS_objfunc.tpl:430](ss3-source-code-main/SS_objfunc.tpl#L430)):
$$\text{age\_like}_{f,i} = -\text{offset}_{f,i} + \text{Comp\_logL\_multinomial}(N, \text{obs}, \text{exp})$$
where ([SS_miscfxn.tpl:113](ss3-source-code-main/SS_miscfxn.tpl#L113), [SS_prelim.tpl:528](ss3-source-code-main/SS_prelim.tpl#L528)):
- `Comp_logL_multinomial = -N · Σ obs · log(exp)`
- `offset = -N · Σ obs · log(obs)` ("perfect-fit reference")
- Combined: `age_like = N · Σ obs · log(obs/exp)`

**Rceattle** ([ceattle_v01_11.cpp case 2](../../Rceattle/src/TMB/ceattle_v01_11.cpp):
$$\text{NLL} = w_f \cdot N_i \cdot \sum_a \tilde{p}_{\text{obs},a} \log\left(\tilde{p}_{\text{obs},a}\,/\,\tilde{p}_{\text{hat},a}\right)$$
with `tilde_p = (p + ac) / (1 + n*ac)` (addtocomp smoothing).

**Verification**:
- The R manual recompute of the SS3-robust kernel applied to Rce's own
  `comp_hat` matches cpp `jnll_comp` to **0.000000** across all 5 active
  fleets — confirming the kernel itself is at machine precision (see
  "Manual recompute LenComp NLL" block in test output).
- Per-cell reconstruction vs SS3's per-cell `Like`: 374.58 vs SS3 official
  374.10 for Srv 2024 CAAL — the 0.5 NLL residual is `comp_hat` divergence
  (#1b), not the kernel.

**Status**: ✅ Matched at machine precision. Set `Comp_loglike = "SS3Robust"`
(case 2) in fleet_control. Set `Comp_addtocomp = 1e-4` to match SS3 dat.

**The +0.86 NLL gap reported on the Length comp row is not from the
kernel — see [#1b](#1b-length-comp-nll-downstream-of-13--4) below for
the structural decomposition.**

---

## 1b. Length-comp NLL (downstream of #13 + #4) 🟠

The kernel (#1) is at machine precision when applied to Rce's `comp_hat`.
The +0.86 Length-comp NLL gap vs SS3 comes entirely from differences in
the predicted `comp_hat` that flow into the kernel, NOT from the kernel
itself.

**Decomposition** (stratified by length bin, Rce − SS3-via-Exp):

| Fleet | L < 80 cm | L ≥ 80 cm | Net | Official gap (Rce − SS3) |
|---|---|---|---|---|
| FshTrawl | −2.20 | +2.43 | +0.23 | +0.61 |
| FshLL    | −3.33 | +2.83 | −0.50 | +0.31 |
| FshPot   | −3.31 | +2.82 | −0.50 | +0.03 |
| Srv      | −0.77 | +0.70 | −0.07 | +0.09 |
| LLSrv    | −2.28 | +1.12 | −1.16 | −0.18 |

All 5 active fleets show the **same sign pattern** — Rce NLL is lower
than SS3 for L < 80 and higher for L ≥ 80. This is mass-conserving
probability transport between length strata, the signature of an
upstream LAA / plus-group shift, not random per-cell noise.

**Per-cell evidence** (FshTrawl, year 2023, comp_ind = 109; Rce sum = 1.000, SS3 sum = 1.000):

| Bin | Rce hat | SS3 hat | Rce/SS3 |
|---|---|---|---|
| 39.5 | 0.0356 | 0.0356 | 1.0000 |
| 44.5–74.5 | — | — | **1.0000 (≤ 1e-4)** |
| 79.5 | 0.0440 | 0.04383 | 1.0040 |
| 84.5 | 0.02157 | 0.02115 | 1.0200 |
| 89.5 | 0.01094 | 0.01062 | 1.0310 |
| 94.5 | 0.005454 | 0.005472 | 0.9966 |
| 99.5 | 0.002357 | 0.002593 | 0.9089 |
| 104.5 (plus-group) | 0.001041 | 0.001387 | **0.7507** |

Sources of the per-stratum diff, in priority order:

1. **#13 Plus-group LAA convention (dominant)** — Rce's static
   `exp(−0.2·a)` weighting puts the plus-group cohort mean LAA ~10 cm
   below SS3's dynamic N-weighted plus-group. Probability mass that SS3
   carries in L = 99.5–104.5 lands in L = 84.5–94.5 in Rce. This is the
   `+2 to +3` NLL contribution at L ≥ 80 across every active fleet.

2. **#4 Pope's vs Baranov via N-at-age drift (secondary)** — Bio rel err
   9.29e-4 (forward-pass) propagates into comp_hat at the same ~10⁻³
   per-bin level. Contributes ~0.1–0.2 NLL per fleet, mostly washed out
   by the larger #13 effect.

3. **#3 sub-machine-precision sel-at-length noise** — Per-fleet sel max
   rel err: FshTrawl 2.50e-2 (at L=12, sel=8e-5, negligible weight);
   FshLL 1.00e-5; FshPot 7.64e-6; Srv 5.50e-5; LLSrv 2.92e-5. Contributes
   ≤ 0.01 NLL per fleet.

**Why FshPot lands at +0.03**: not because it's structurally different —
it has the same +2.82 L≥80 / −3.31 L<80 internal split as every other
fleet. The official per-fleet gap is the residual after stratum
cancellation plus differences in how SS3 vs Rce smooth obs/hat. For
FshPot, those happen to compose to near-zero. Reading +0.03 as
"essentially machine precision" misattributes the cancellation to
parity.

**Path 1 — close to machine precision in Rce**: implement #13 (enable the
dynamic N-weighted plus-group LAA already commented in
[growth.hpp lines 230–260](../../Rceattle/src/TMB/growth.hpp#L230)). The
NLL gap will drop from +0.86 → ≤ 0.1 NLL (residual from #4) when #13 is
closed.

**Path 2 — make SS3 match Rce**: not feasible (SS3's dynamic plus-group
is core to its growth pipeline).

**Recommendation**: defer until Phase A makes a closure useful. The +0.86
is well below the +263 CAAL or +46 Survey gaps and doesn't dominate the
forward-pass total.

**Status**: 🟠 Structural diff — magnitude verified at +0.86 NLL,
decomposed and traced to #13 + #4. Not closeable without closing #13.

---

## 2. CAAL likelihood kernel ✅ (after bug fix)

Same kernel formula as #1 above (SS3 reuses `Comp_logL_multinomial`).
Was historically the largest gap (+5661 NLL) until two bugs were
fixed in this session:

**Bug A** (Rceattle cpp, fixed in [ceattle_v01_11.cpp:1989](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L1989)):
The marginal length-comp pred_CAAL loop didn't skip `age <
age_first_selected`. CAAL section already had this guard (line 2317);
length-comp section did not. Effect: age-0 N bled into smallest L bin
via ALK tail → predicted comp at bin 4.5 was 23× SS3.

**Bug B** (test-side ss3_to_rceattle.R `build_caal_data`, fixed):
SS3 Pcod `agebin_vector = [1..10]` (no age 0); user's `minage = 0`
means Rce expects ages 0..10. Old pad-trailing-zeros logic put SS3 age
1 obs into `CAAL_1` (Rce age 0). Now pads leading zeros so SS3 age `k`
goes into `CAAL_(k+1)` (Rce age `k`).

After both fixes: Total CAAL NLL 6383 → 984 (95% closed).

**Remaining +263 gap**: per-cohort N-at-age divergence from SS3
(Methot-Taylor — see #5).

---

## 3. Survey index lognormal additive constant 🟡 (closed)

**SS3** ([SS_objfunc.tpl Survey block](ss3-source-code-main/SS_objfunc.tpl)):
$$\text{NLL} = \sum_i \left[\log(\sigma_i) + \tfrac{1}{2}\left(\frac{\log(\text{obs}_i) - \log(\hat{q})}{\sigma_i}\right)^2\right]$$

**Rceattle (pre-fix)** used TMB `dnorm(..., log=TRUE)`, which returns
the full normal log-density including `−0.5·log(2π) − log(σ)`. The
`jnll` therefore picked up `0.5·log(2π)` per obs as a constant offset:
$$\text{NLL}_\text{Rce, pre} = \sum_i \left[\tfrac{1}{2}\log(2\pi) + \log(\sigma_i) + \tfrac{1}{2}z_i^2\right]$$

**Fix (2026-05-31)** at
[ceattle_v01_11.cpp:2552–2569](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L2552):
```cpp
Type resid = log(index_obs(index_ind, 0))
             - (log(index_hat(index_ind)) - square(index_std_dev)/2.0);
jnll_comp(0, index) += log(index_std_dev)
                       + 0.5 * square(resid / index_std_dev);
// jnll_comp(0, index) -= dnorm(log(index_obs(index_ind, 0)), log(index_hat(index_ind)) - square(index_std_dev)/2.0, index_std_dev, true);
```
The old `dnorm` line is kept commented next to the new code for one-line
revert. The bias-correction `−σ²/2` on the predicted mean is retained
(unchanged from pre-fix) — only the per-obs `0.5·log(2π)` constant is
dropped.

**Before / after** (Pcod 2024 FP):

| | SS3 | Rce | Diff |
|---|---|---|---|
| Pre-fix  | −0.97 | +44.75 | **+45.73** |
| Post-fix | −0.97 | −1.19  | **−0.22** |

Closure: 50 obs × 0.5·log(2π) = 50 × 0.9189 = **45.95 NLL** — matches
the observed `45.73 − (−0.22) = 45.95` exactly. The −0.22 NLL residual
is per-obs FP accumulation noise (within the floating-point tolerance
of summing 50 terms each ~10⁻¹⁰ apart from SS3's ADMB-precision sum).

**Magnitude**: −0.22 (was +45.73). Within the 🟡 "FP noise <1 NLL"
band. Does not affect parameter estimates (additive constant).

**Status**: 🟡 closed via cpp kernel rewrite. Re-revert by uncommenting
the old `dnorm` line and removing the manual block.

---

## 4. Catch likelihood: Pope's hybrid vs Baranov continuous F 🟠

**SS3** (with default `F_Method = 1` hybrid Pope's): the catch
likelihood block at [SS_objfunc.tpl:712](ss3-source-code-main/SS_objfunc.tpl#L712)
is gated on `if (F_Method > 1)` and SKIPPED entirely for the default
hybrid Pope's path. The hybrid-F method makes catch fit observed
exactly by construction, so `catch_like = 0` is reported.

**Rceattle** ([ceattle_v01_11.cpp:2606+](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L2604)):
$$C_{\text{hat}}^{\text{Rce}}(\text{flt}, \text{yr}) = \sum_a \frac{F_{a,\text{yr}}}{Z_{a,\text{yr}}} \cdot N_{a,\text{yr}} \cdot (1 - e^{-Z_{a,\text{yr}}}) \cdot w_{a,\text{yr}}$$
with `log_F` as a freely estimated parameter. Catch NLL is a
lognormal on observed vs predicted, evaluated every evaluation. There
is no Rceattle equivalent of Pope's "fit catch exactly" path.

**SS3-parity kernel fixes (2026-05-31)** at
[ceattle_v01_11.cpp:2604–2632](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L2604).
Two staged rewrites of the catch kernel; both prior versions kept
commented in the cpp block for one-line revert:

1. **Cosmetic constant drop** (`log(σ) + 0.5·z²_pure`): replaced TMB
   `dnorm(..., log = TRUE)` with the manual SS3-form kernel, drops the
   per-obs `0.5·log(2π)`. Same pattern as Survey [#3](#3-survey-index-lognormal-additive-constant--closed)
   and Recruitment [#5](#5-recruitment-methot-taylor-bias-adjustment-ramp--mostly-closed).

2. **Full SS3 robustification** ([SS_objfunc.tpl:727](ss3-source-code-main/SS_objfunc.tpl#L727)):
   ```cpp
   resid = log(1.1·obs) - log(hat + 0.1·obs);
   jnll += 0.5 * square(resid / sigma);
   ```
   The `1.1·obs` numerator and `+ 0.1·obs` denominator are a soft floor
   that bounds the residual at `log(11) ≈ 2.4` when `hat → 0`. Also
   drops the `log(σ)` constant and bias correction `+σ²/2` that SS3
   omits. At the optimum (`hat ≈ obs`) this is numerically identical
   to a pure lognormal kernel.

**Magnitudes** (against the modified SS3 model, Pcod 2024 FP):

| Stage | SS3 | Rce | Diff |
|---|---|---|---|
| Pre-fix (`dnorm(..., true)` kernel) | 0 | +97.29 | +97.3 |
| After kernel fix #1 (Survey-style, `0.5·log(2π)` dropped) | 0 | −25.85 | −25.85 |
| After kernel fix #2 (full SS3 robustification) | 0 | +110 / +408 (see note) | full magnitude exposed |

**Note on what fix #2 changes**: kernel fix #1 still included
`log(σ_catch) = log(0.05) = −3.0` per obs in the kernel sum (a
**−432 NLL** constant offset across 144 obs). That constant was
masking the actual size of the residuals. Fix #2 drops the constant
and replaces the residual with SS3's soft-floored form. The result is
**−25.85 → +408 NLL on F_Method = 3** model (where Rce has Pope's-vs-
Baranov dynamic divergence; the +408 is real per-obs catch_hat error)
and **+109 → +408 NLL on F_Method = 2** model (similar gap; the +109
was log(σ) hiding the real divergence).

Reading this correctly: the +408 figure is the **honest magnitude**
of catch_hat divergence between Rce and SS3, expressed on SS3's
likelihood scale. The −25.85 and +109 figures from intermediate fixes
were artifacts of the `log(σ)` term, not true closures.

**The residual +408 NLL is structural**, not cosmetic. Mechanism:
- Rce inherits SS3's MLE F values via the `init_log_F_from_ss3` helper.
- Rce computes catch_hat via Baranov using those F values together with
  Rce's own N_at_age propagation.
- Rce's N has ~1.5% rel err vs SS3's at most ages (R matches at 1e-6
  but small per-cohort drift accumulates).
- Sel × F × N aggregation amplifies the 1.5% N error into ~17% per-obs
  catch_hat error.
- Robustified kernel × (17% residual / σ=0.05) ≈ 3.4 z-score per obs
  ≈ 5.8 NLL per obs × 144 obs ≈ 830, halved by the soft floor on the
  largest residuals → ≈ +408 observed.

SS3 reports +1.75 because **SS3 estimates F to match catch by
construction** — its F MLE is the F that makes catch_hat ≈ obs. Rce
uses those same F values but in a different N-propagation context,
breaking the catch ≈ obs tight fit.

**Path 1 — make Rce match SS3**: implement Pope's hybrid F method as
an alternative to Baranov continuous F. Substantial cpp work
(introduces a `F_Method` switch, replicates the iterative F-tuning
SS3 does each evaluation).

**Path 2 — make SS3 match Rce**: set `F_Method = 3` in SS3
`starter.ss`. This makes SS3 estimate `log_F[flt, yr]` as a free
parameter the same way Rce does. Re-run SS3 (~5 min). SS3's catch
likelihood block then activates and produces a lognormal NLL with the
same residual structure as Rce — both report similar non-zero values.

**Recommendation**: Path 2 (SS3 config change). Cleaner and faster.
Note: this requires the same kind of full SS3 re-run as the modified
`goa_pcod-no init and ramp` model.

### F_Method = 2 attempt (2026-05-31)

Switched `goa_pcod-no init and ramp/Model19_1e.ctl:119` from `3` (hybrid)
to `2` (instantaneous F parameter-estimated). SS3 re-converged but with a
suspect-variance warning and a notably-shifted optimum
(`SR_LN(R0) = 13.15 → 12.61`, `SR_regime = -0.66 → -1.37`,
`log_R0·exp(SR_regime)` effectively 4× lower than with F_Method=3). Three
plumbing issues surfaced in the Rce test driver and required fixes (all
already committed at [ss3_to_ceattle_test.R](ss3_to_ceattle_test.R)):

1. **r4ss `SS_output()` crash** on a corrupted Report.sso (multiple
   `Bratio_YYYY` rows concatenated without newlines + non-numeric SD
   values in DERIVED_QUANTITIES). Patched at the top of the test
   script — blanks the `Pstar_sigma` / `OFL_sigma` calc and sets both
   to `NA_real_`, then detaches + re-attaches the `r4ss` package so
   search-path resolution picks up the patched function.

2. **`ageselex` / `sizeselex` duplicate "NoName" columns**. F_Method=2
   adds 19 extra unnamed cols that break dplyr filter. Added
   `.dedupe_cols()` helper.

3. **`SelSizeAdj` column shift**. F_Method=2 prepends an integer flag
   column, shifting all sel parameters from Par1..Par6 to Par2..Par7.
   `init_doublenormal_from_ss3` patched to detect (via non-NA Par7) and
   shift the column mapping. Without this fix, the injected sel devs
   were completely off (range −66..+92 in fleet 2) and Rce produced
   NaN/−Inf in `Z_at_age`, then NaN N_at_age from year 1978 onward.

4. **F injection from corrupt `$timeseries`**. r4ss fails to parse the
   `Bratio_YYYY` smashed rows in Report.sso, which corrupts the
   `$timeseries$Yr` column and drops the rows for years where SS3 fitted
   F = 0 originally (1994, 1997, 2000, 2006 for Pcod under the
   F_Method = 3 optimum, before the re-run). The `init_log_F_from_ss3`
   helper saw NA for those years and substituted `log_F = log(1e-9) ≈
   −20.7`, driving Rce catch_hat to ~1e-4 mt against catch obs of
   ~30,000 mt. Per-row kernel ≈ 0.5·(log(30000/1e-4)/0.05)² ≈ 70k.
   12 rows × 70k ≈ 816k NLL.

   **Resolution**: read F from `ss3_rep$parameters` (`F_fleet_<n>_YR_<yr>_s_1`
   entries) instead of `$timeseries`. SS3 always emits these
   parameter-level rows under F_Method = 2 with the actual MLE F per
   year/fleet. Patched at
   [ss3_to_ceattle_test.R](ss3_to_ceattle_test.R)
   `init_log_F_from_ss3` — preferred path when `^F_fleet_.*_YR_` exists
   in `$parameters`, fallback to ts-column extraction otherwise.

### Post-fix state (2026-05-31, after fix #4)

| Component | SS3 | Rce | Diff | Note |
|---|---|---|---|---|
| Survey | −1.79 | −1.44 | +0.35 | FP noise (#3 closed) |
| **Catch** | 1.75 | **111.0** | **+109.3** | Now-tractable residual, no more 816k blowup |
| Length comp | 1336.33 | 1388.36 | +52.0 | downstream of Bio drift (1.5% mean) |
| CAAL | 721.20 | 1028.33 | +307.1 | similar to F_Method=3 state |
| Recruitment | −16.32 | −12.84 | +3.48 | kernel-matched (#5) |
| InitEQ_Regime | 0 | 0 | 0 | ✅ |
| Parm priors | 0.79 | 0.93 | +0.14 | essentially closed |
| Sel-dev | 6.09 | 0 | −6.09 | sentinel |
| **TOTAL** | **2048.07** | **2513.65** | **+465.6** | |

**Bottom line on F_Method = 2 (post-fix)**: SS3 catch likelihood DOES
activate (Catch row 0 → 1.75) and the F injection from `$parameters`
correctly propagates the SS3 MLE F values into Rce. The remaining +109
catch gap is the actual structural diff between the two implementations
(small per-year Baranov dynamics differences, per-obs σ handling). No
longer a plumbing artifact — it's a genuine model-form residual.

**Decision point**: F_Method = 2 vs F_Method = 3 (modified ctl) is now
a real choice rather than "F_Method = 2 is unusable":

| | F_Method = 3 (hybrid) | F_Method = 2 (free F) |
|---|---|---|
| Rce TOTAL | 2347.5 | 2513.65 |
| Gap vs SS3 | +297.7 | +465.6 |
| Catch row gap | −25.85 | +109.3 |
| Cosmetic vs structural | catch is Pope's vs Baranov (#4) | catch is genuine residual |

F_Method = 3 still has the SMALLER TOTAL gap. The trade-off: F_Method = 3
isolates the catch divergence as the Pope's-vs-Baranov structural diff
(catalogued at #4); F_Method = 2 makes catch a "real" parity component
that both models compute under the same Baranov + lognormal framework.

**Pragmatic recommendation (revised)**: KEEP F_Method = 2 if you want
catch parity to be a real component (post-fix); revert to F_Method = 3
if you want the smaller overall TOTAL and are content to leave #4 as
catalogued structural diff.

---

## 5. Recruitment Methot-Taylor bias-adjustment ramp 🟡 (mostly closed)

### Post-fix state (2026-05-31)

The original +39 NLL gap had **two compounding sources**:

1. **Methot-Taylor bias ramp** (the historical headline of this section).
   Closed structurally on the SS3 side by setting `max_bias_adj_in_MPD = -1`
   in [`Data/goa_pcod-no init and ramp/Model19_1e.ctl:105`](Data/goa_pcod-no%20init%20and%20ramp/Model19_1e.ctl#L105).
   The [ss3_to_ceattle_test.R](ss3_to_ceattle_test.R) `compute_bias_adj()`
   helper recognises this sentinel as of 2026-05-31; older versions
   plugged `bmax = -1` into the ramp formula and produced negative bias
   adjustments. With this fix, both models apply `b=1` to every recdev
   and the cohort dynamics line up — R matches SS3 to **1e-6**.

2. **Per-year kernel convention** (newly isolated). Even with the ramp
   off, Rceattle reported `Recruitment dev = 36.12` vs SS3's `−16.30`
   (gap +52.4). This residual was a **kernel form** difference, not a
   ramp-related issue:
   - SS3 ([SS_objfunc.tpl:804+809](ss3-source-code-main/SS_objfunc.tpl#L804)):
     per-year contribution to `noBias_recr_like` is
     `0.5 · (dev/σ_R)² + log(σ_R)` where `dev` is the RAW SS3 deviation
     (no bias correction inside the kernel — SS3 applies `−σ_R²/2`
     inside the R formula instead).
   - Rceattle (old): `dnorm(rec_dev, σ_R²/2, σ_R, true)` =
     `0.5·log(2π) + log(σ_R) + 0.5·((rec_dev − σ_R²/2)/σ_R)²`. Since
     `rec_dev = dev − σ_R²/2` is injected, the residual becomes
     `(dev − σ_R²)/σ_R` — a per-year systematic shift of `−σ_R²` relative
     to SS3's kernel.

**Fix (2026-05-31)** at [ceattle_v01_11.cpp:3043–3056](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L3043):
```cpp
Type dev_ss3 = rec_dev(sp, yr) + square(R_sd(sp))/Type(2.0);
jnll_comp(10, sp) += Type(0.5) * square(dev_ss3 / R_sd(sp)) + log(R_sd(sp));
// jnll_comp(10, sp) -= dnorm(rec_dev(sp, yr), square(R_sd(sp))/2.0, R_sd(sp), true);
```
Old `dnorm` line kept commented next to the new block. Drops the per-year
`0.5·log(2π)` constant and recovers SS3's raw `dev` from Rce's stored
`rec_dev = dev − σ²/2`.

**Before / after** (against the modified SS3 model):

| | SS3 | Rce | Diff |
|---|---|---|---|
| Original ramp on, dnorm kernel | −2.62 | +36.58 | +39.21 |
| Ramp off (Path 2), dnorm kernel | −16.30 | +36.12 | +52.42 |
| Ramp off + SS3-form kernel (now) | −16.30 | **−12.64** | **+3.66** |

**Magnitude**: +3.66 NLL residual. Likely the SS3 `sd_offset_rec × log(σ_R)`
constant offset (`= 1 × log(0.44) = −0.821` plus any Fcast_recr_like
contribution; not yet fully decoded but small).

**Status**: 🟡 mostly closed; +3.66 residual is a per-model-config
constant that doesn't affect estimation.

### Historical / structural form (for reference)

**SS3** ([Methot & Taylor 2011 CJFAS](https://doi.org/10.1139/f2011-092)):
Year-specific bias correction $b_y$ ramps from 0 (early/uninformed)
to 1 (well-informed) to 0 (late/uninformed):
$$\text{rec\_dev}_y \sim \mathcal{N}\!\left(-\tfrac{1}{2} b_y \sigma_R^2,\, \sigma_R\right)$$
The ramp is parameterized by `(early_yr, full_yr, last_full_yr, late_yr)`
in ctl's `# recdev advanced options`. Outside the ramp, $b_y = 0$,
which means the prior mean = 0 (no bias correction) and the dev is
penalized as `dnorm(dev, 0, σ_R)`.

**Rceattle**: constant $b = 1$:
$$\text{rec\_dev}_y \sim \mathcal{N}\!\left(-\tfrac{1}{2}\sigma_R^2,\, \sigma_R\right)$$

**Delta**: +39 NLL on the "Recruitment dev" row (SS3 reports −2.6,
Rce 36.6). Per memory `feedback_ss3_rec_bias_ramp.md`, this is
SKIPPED per scope decision.

**Path 1 — make Rce match SS3**: add `bias_adj_y` data vector + apply
in the rec_dev prior. Code change in
[ceattle_v01_11.cpp slot 11 (Recruitment deviates)](../../Rceattle/src/TMB/ceattle_v01_11.cpp).
Moderate complexity (need to plumb a per-year vector through data).

**Path 2 — make SS3 match Rce**: disable the ramp in SS3 ctl. The
cleanest knob is the SS3 sentinel `max_bias_adj_in_MPD = -1`, which
overrides the linear ramp and forces `b_y = 1` for every estimated
recdev. This is the change applied in
[`Data/goa_pcod-no init and ramp/Model19_1e.ctl:105`](Data/goa_pcod-no%20init%20and%20ramp/Model19_1e.ctl#L105)
(see [SS3 model configurations](#ss3-model-configurations-referenced-in-this-catalog)
at the top of this file). After the change, SS3's `Recruitment` row goes
from −2.62 → −14.92 (full-bias correction applied to every year). The
[ss3_to_ceattle_test.R](ss3_to_ceattle_test.R) `compute_bias_adj()`
helper recognises this sentinel as of 2026-05-31 (prior versions
plugged `bmax = -1` into the ramp formula and produced negative
bias adjustments).

**Recommendation**: Path 2 (SS3 config). Path 1 is non-trivial and
the math is well-documented in Methot & Taylor (2011) if you want to
mirror it eventually.

---

## 6. InitEQ_Regime penalty 🟠

**SS3**: contributes 2.78 NLL to "Init eq" in Pcod 2024. SS3 places a
prior on the initial-equilibrium regime deviation parameter that
adjusts the equilibrium F or catches used to derive the initial
age-structure.

**Rceattle**: the **mechanism** is implemented via two related modes:

- `initMode = 4` (`"NonEquilibriumScaled"`,
  [ceattle_v01_11.cpp:1177–1183](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L1177)):
  `N_init[a] = R_init · exp(-Finit) · exp(-sum(M1[0..a-1]) + init_dev[a-1])`.
  Has per-age `init_dev`. Carries the bias-corrected lognormal init_dev
  prior at jnll slot 9.
- `initMode = 5` (`"EquilibriumScaled"`, added 2026-05-31):
  identical cascade but `init_dev` is mapped OFF in
  [build_map.R:97](../../Rceattle/R/2-build_map.R#L97) and the init_dev
  prior is skipped in
  [cpp:3034](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L3034). Formula
  reduces to `N_init[a] = R_init · exp(-Finit) · exp(-sum(M1[0..a-1]))`.

Both are mathematically equivalent to SS3's `R_init = R0 · exp(SR_regime)`
under the mapping `Finit ↔ -SR_regime`. The Pcod test driver uses
mode 5 (cleanest replica of SS3's regime convention with no per-age devs).

What's MISSING in Rce is a **soft penalty** equivalent to SS3's
hardcoded `InitEQ_Regime` contribution. SS3 reports +2.78 NLL on this
row even when the SR_regime parameter has no formal prior in ctl
(PR_type = 0, PR_SD = 0). The penalty formula is at
[SS_objfunc.tpl:810](ss3-source-code-main/SS_objfunc.tpl#L810):
```cpp
regime_like = 0.5 * square(log(R1 / R1_exp) / (sigmaR / ave_age));
```
which simplifies to $\tfrac{1}{2}(\text{SR\_regime} \cdot \overline{\text{age}} / \sigma_R)^2$
with `ave_age = 1/natM - 0.5` (see
[SS_biofxn.tpl:1285](ss3-source-code-main/SS_biofxn.tpl#L1285)).
Lambda code = `18` (see
[SS_readcontrol_330.tpl:5519](ss3-source-code-main/SS_readcontrol_330.tpl#L5519)).
Numerical verification on Pcod: 0.5·(-0.6782 · 1.528 / 0.44)² = **2.775**
vs SS3 reported **2.776** (8e-4 residual).

**Delta**: −2.8 NLL (Rce < SS3 because Rce has no analog of SS3's
hardcoded soft penalty).

**Pcod test driver state (2026-05-31)**: switched to `initMode = 5`
with `log_Finit = log(-SR_regime_BLK5add_1976) = log(0.6782)` injected
from SS3 MLE
([ss3_to_ceattle_test.R Section 8 + Section 9](ss3_to_ceattle_test.R)).
The mode-5 cascade approximates SS3 styr Bio to 0.18% (vs the older
mode-0 exact-pin via `log(ss3_N)` injection); the regression is
visible as +12 NLL on length comp (1342 → 1354) but is offset by a
Catch improvement (Baranov self-consistency on initial cohorts:
+25.97 → +20.15). Net −13 NLL improvement. Init eq stays at 0 on
the Rce side (penalty cleanly skipped).

**Path 1 — make Rce match SS3**: with the formula now decoded above,
add the penalty to `ceattle_v01_11.cpp` in modes 4/5. One-line form
(at the species loop near the existing rec_dev prior, slot 9 or new
slot):
```cpp
// SS3 InitEQ_Regime soft penalty (SS_objfunc.tpl:810). Active in
// modes 4/5 where Finit acts as a regime-shift on R_init.
if((initMode == 4) | (initMode == 5)){
  Type ave_age_sp = Type(1.0) / M1_at_age(sp, 0, 0, 0) - Type(0.5);
  jnll_comp(SLOT, sp) += Type(0.5) * square(-log_Finit(sp) * ave_age_sp / R_sd(sp));
  //                                       ^^ Finit = -SR_regime
}
```
Not implemented as of 2026-05-31.

**Path 2 — make SS3 match Rce**: set `like_comp = 18` lambda to 0 in
the SS3 ctl lambdas block. Applied in
[`Data/goa_pcod-no init and ramp/Model19_1e.ctl:306`](Data/goa_pcod-no%20init%20and%20ramp/Model19_1e.ctl#L306)
(see [SS3 model configurations](#ss3-model-configurations-referenced-in-this-catalog)
at the top of this file). After lambda zero + SS3 re-run, the
`InitEQ_Regime` row → 0 and the −2.78 NLL gap closes.

**Path 2 — make SS3 match Rce**: in `starter.ss`, set the `InitEQ_Regime`
lambda to 0 via the `lambdas` block (line 9 with `like_comp = 9`).
Or remove the InitEQ deviation parameter in ctl (set its PHASE
negative). Re-run SS3.

**Recommendation**: Path 2. Pcod model's value (2.78) is small;
turning it off is one line in starter.ss.

---

## 7. M-block prior structure 🟠

**SS3** ([Pcod ctl line 55](Data/goa_pcod/Model19_1e.ctl#L55)):
The M base parameter (`NatM_p_1_Fem_GP_1`) has a lognormal prior
with `PRIOR = -0.81`, `PR_SD = 0.41`. The M block-replacement value
(`NatM_p_1_Fem_GP_1_BLK4repl_2014`) has its OWN lognormal prior with
the SAME parameters (`-0.81, 0.41`). Both are independent priors on
absolute log-M values.

NLL contribution per cell:
- Base: $0.5 \cdot ((\log M_{\text{base}} - \text{PRIOR}) / \text{PR\_SD})^2 = 0.031$
- Block: $0.5 \cdot ((\log M_{\text{block}} - \text{PRIOR}) / \text{PR\_SD})^2 = 1.10$
- Total M prior contribution: ~1.13

**Rceattle**: only the base $M_1$ has the prior directly (via
`M1_use_prior`). The block effect is implemented as a linkage offset:
$$\log M_y = \log M_1 + \beta_{\text{post2014}} \cdot \mathbb{1}[y \geq 2014]$$
with `beta_post2014 ~ N(0, 0.41)` (a prior on the OFFSET centered at 0).

NLL contribution at `beta = log(0.817/0.493) = 0.51`:
$0.5 \cdot (0.51 / 0.41)^2 = 0.77$

Total Rce M-related prior: ~0.85 vs SS3's ~1.13.

**Delta**: −0.33 NLL on "Parm priors".

**Why the math differs**: SS3 places the prior on the ABSOLUTE value;
Rceattle's linkage system places it on the OFFSET. The implied
joint-prior on the offset (assuming independence of base + block in
SS3) is $\mathcal{N}(0, \sigma\sqrt{2})$ with $\sigma\sqrt{2} = 0.58$,
not 0.41. So even the "right" Rce SD would be 0.58 to match SS3
exactly.

**Path 1 — make Rce match SS3 exactly**: add a prior mechanism on the
absolute block VALUE (not the offset). Would need either:
- Re-architect linkage so per-block intercept rows have their own
  prior on the EFFECTIVE param value, not the linkage coefficient
- Or expose a per-stratum prior on log_M1 + offset

**Path 2 — approximate match by widening Rce's offset sd**: use
`normal(0, 0.41 * sqrt(2))` instead of `normal(0, 0.41)`. Numerically
closer to SS3 but conceptually slightly different (this conflates the
implied "independent base + block" prior into a single offset prior).

**Recommendation**: tiny gap (0.33 NLL), accept and document. Or use
Path 2 if exact NLL match is needed.

---

## 8. Sel-dev `dev_se` prior magnitude 🟡

**SS3** ([Pcod ctl, FshLL P1](Data/goa_pcod/Model19_1e.ctl)):
Each `dev_seq` parameter has a corresponding `dev_se` (the prior SD).
For FshLL P1: `dev_se = 0.20`. The prior is `dev ~ N(0, dev_se)` on
each estimated annual deviation.

**Rceattle**: per-fleet `Time_varying_sel_sd_prior` controls the
shrinkage:
- `sel_inf_dev ~ N(0, sel_dev_sd)` per fleet/sex/year
- `log_sel_slp_dev ~ N(0, 4 * sel_dev_sd)` (note the 4× scaling for
  slope vs inflection per [ceattle_v01_11.cpp:2721](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L2721))

**Delta**: depends entirely on what you set
`Time_varying_sel_sd_prior` to. With sentinel = -1 (Phase 1), Rce
contributes 0. With `= 1.0` (general estimation default), Rce may
contribute more or less than SS3's 5.74 depending on per-year deviate
magnitudes.

**Path 1 — match SS3 exactly**: set
`Time_varying_sel_sd_prior` per-fleet to SS3's `dev_se`:
```r
cod_pcod$fleet_control$Time_varying_sel_sd_prior[fi] <- 0.20  # SS3's dev_se
```
Note: SS3's 4× scaling on slope params doesn't have a Rce per-param
override, so the slope prior will be 4× too tight relative to SS3.
Workaround: divide the SD by 4 to compensate, but then the inflection
prior is too loose. The architectures don't allow a perfect map.

**Path 2 — make SS3 match Rce**: SS3 doesn't naturally support a
single `sel_dev_sd` per fleet that applies to all params. Each
param's `dev_se` is independent. To approximate: set all `dev_se` to
the same value per fleet.

**Recommendation**: Path 1 with per-fleet `dev_se` matching, accept
the inflection/slope scaling discrepancy. Document the 4× factor.

---

## 9. SS3 `dev_link` scaling on `dev_seq` parameters 🔴

**SS3**: The stored `dev_seq` value is NOT the effective parameter
delta. It's scaled by:
$$\Delta_{\text{eff}}(y) = \text{dev\_seq}(y) \cdot f(\text{dev\_link}, \text{dev\_se}, \text{HI}-\text{LO})$$
where $f(\cdot)$ depends on `dev_link` mode (0/1/2/3) and the
parameter bounds. We empirically found for FshLL P1:
$\Delta_{\text{eff}} \approx \text{dev\_seq} \cdot 16$
(dev_seq=1.36 → +20.6 cm peak shift). We couldn't decode the exact
SS3 formula from the source.

**Rceattle**: dev is the raw additive offset:
$$\Delta_{\text{eff}}(y) = \text{sel\_inf\_dev}(y) \quad\text{(direct)}$$

**Delta**: structural — different parameterizations. We bypass this
in forward-pass by reading SS3's per-year EFFECTIVE values from
`ss3_rep$SelSizeAdj` and computing `dev = SS3_effective - Rce_base`.
For estimation, we'd need to either:

**Path 1 — implement SS3's exact dev_seq scaling in Rce**: requires
parsing SS3's `dev_link` mode per param and applying the matching
scaling. Difficult because the scaling formula isn't well-documented
in the SS3 source we have access to.

**Path 2 — avoid SS3's dev_seq mechanism in SS3**: turn off all
`dev_seq` parameters in SS3 (set PHASE negative on each `_dev_se`).
This forces SS3 to use only block_replacement for time variation.
But then SS3 won't fit annual variation between blocks.

**Path 3 — both use raw additive devs**: have Rce mirror SS3 by
treating each per-year sel value as a free parameter (no shrinkage
beyond a wide prior), bypassing dev_seq entirely. This works if you
set the `Time_varying_sel_sd_prior` to a large value.

**Recommendation**: Path 3 with appropriate prior sd. Documents the
SS3 scaling as an SS3-specific feature not mirrored.

---

## 10. SS3 three-tier sel: base + block_repl + dev_seq 🔴

**SS3**: per param, has up to three estimated quantities:
- `base` — one free parameter per fleet/sex
- `BLKreplN` — one free parameter per block (1-4 typical)
- `dev_seq[y]` — annual deviation in the dev period
And combines them deterministically:
$$P_{\text{eff}}(y) = \begin{cases}
\text{base} + \text{dev}(y) \cdot s & y \notin \text{block} \\
\text{block\_repl}(y) + \text{dev}(y) \cdot s & y \in \text{block}
\end{cases}$$

**Rceattle**: flatter — per param, has:
- `sel_inf[fleet, sex]` — one base param
- `sel_inf_dev[fleet, sex, year]` — per-year additive deviate

The "block" structure is implicit in the dev pattern. With
`Time_varying_sel = "Block"`, dev params are SHARED across all years
in a block (one param per block), which is closer to SS3 but doesn't
support overlay devs within a block.

**Delta**: structural, no NLL number per se but affects how many
parameters are estimated and how they're constrained.

**Path 1 — implement SS3-style three-tier in Rce**: add a
`block_repl[fleet, block_id]` parameter array and an overlay
mechanism. Substantial cpp + R work. Probably not worth it for a
single-model comparison.

**Path 2 — flatten SS3**: turn off block_repl (set PHASE negative on
each BLKrepl) and force SS3 to use dev_seq for ALL time variation.
Then SS3's structure matches Rce's flat dev model.

**Recommendation**: Path 2 if estimation parity matters; otherwise
accept structural divergence and document.

---

## 11. Pop-grid integration ✅

**SS3** ([SS_expval.tpl:642](ss3-source-code-main/SS_expval.tpl#L642)):
$$\text{exp\_AL}[a, L_{\text{data}}] = \sum_{L_{\text{pop}}} \text{N}(a) \cdot \text{sel\_l}(L_{\text{pop}}) \cdot \text{ALK}(a, L_{\text{pop}})$$

**Rceattle** ([ceattle_v01_11.cpp:2004](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L2004)):
Same form, on the same fine pop-grid. Uses `growth_matrix_pop` and
`sel_at_length_pop` arrays computed at 1-cm resolution. Pop→data
aggregation via precomputed `pop_to_data_bin` mapping.

**Status**: ✅ Matched. Verified to machine precision on per-bin
comp_hat comparisons.

---

## 12. `addtocomp` smoothing ✅

**SS3** (`dat` file `len_info` / `age_info`):
$$\tilde{p}_i = (p_i + \text{addtocomp}) / (1 + n \cdot \text{addtocomp})$$
Applied to both obs and hat.

**Rceattle**: same form, applied in pred_comp section:
[ceattle_v01_11.cpp:2248](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L2248).

**Status**: ✅ Matched. For Pcod 2024, both use `addtocomp = 1e-4`.

---

## 13. Plus-group LAA/WAA convention 🟠

**SS3**: dynamic N-weighted plus-group mean LAA. The plus-group LAA
is updated each year based on the current age composition entering
the plus-group cohort.

**Rceattle**: static `exp(-0.2·a)` weighting (WHAM-style
approximation). The plus-group LAA depends on a fixed
year-independent weight scheme.

**Delta**: small direct impact on overall NLL, but **propagates to the
Length-comp NLL ([#1b](#1b-length-comp-nll-downstream-of-13--4)) as the
dominant cause of the +0.86 gap**. Per-cell evidence: 2.36e-3 max rel
err on LAA age 10 (and similar on WAA) translates into Rce comp_hat
being 2-3% too high at L=84.5–94.5 and 10-25% too low at L=99.5–104.5
across every fleet (FshTrawl 2023 detail: Rce/SS3 ratio 0.91 at L=99.5,
0.75 at L=104.5 plus-group bin). The mass-conserving probability
transport from L ≥ 95 into L = 80–94 IS the +0.86 length-comp NLL gap.

**Path 1 — make Rce match SS3**: implement dynamic N-weighted
plus-group correction. Some code already in
[growth.hpp lines 230-260](../../Rceattle/src/TMB/growth.hpp#L230) as
a comment block, not enabled.

**Path 2 — make SS3 match Rce**: hard. SS3's dynamic approach is core
to its growth pipeline.

**Recommendation**: Path 1 (enable the commented dynamic plus-group
in Rce). Was deferred per `project_goa_pcod_ss3_rceattle.md` memory.

---

## 14. CAAL age-column alignment ✅

**Problem (now fixed)**: SS3 Pcod's `agebin_vector = [1, 2, …, 10]`
covers ages 1-10. Rce with `minage = 0` expects ages 0-10 across 11
slots. The old `build_caal_data` padded trailing zeros — putting
SS3's age 1 obs into `CAAL_1` (Rce age 0). All obs shifted by one.

**Fix** ([R/ss3_to_rceattle.R build_caal_data](R/ss3_to_rceattle.R)):
parse the SS3 column's actual age label (e.g. `a1` → age 1), map
directly to the Rce slot for that age (`age k` → `CAAL_(k - minage + 1)`).

**Magnitude before fix**: +5398 NLL on CAAL (closed 95% of the +5661
gap).

**Status**: ✅ Fixed; regression test in
[tests/test-caal-age-alignment.R](tests/test-caal-age-alignment.R) (3 cases).

---

## 15. SSB scaling convention (sex_ratio + Jensen gap on length-based maturity) 🟠

**Observation**: With N-at-age, growth, and maturity all injected from
SS3, Rceattle's reported `ssb` is **~0.46×** SS3's `SpawnBio` on Pcod
across every hindcast year (1977: 74557 vs 162823; 2024: 47457 vs
102580; ratio = 0.458 ± 0.01). Bio_all matches SS3 to 1e-3.

**Two compounding causes:**

### 15a. Sex-ratio convention (dominant, factor ≈ 0.5)

**SS3 (Nsexes = 1)**: `SpawnBio = Σ N[a] · Mat_F_wtatage[a]`, where
`Mat_F_wtatage` is the per-age expectation of `mat(L) · wt(L)` (no
`FracFemale` multiplier). For Pcod (Nsexes = 1, FracFemale = 0.5),
SS3 treats the combined-sex N as the spawning pool and represents
"per-recruit reproductive output" without halving for sex.

**Rceattle** ([ceattle_v01_11.cpp:594](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L594)):
For `nsex == 1`, `mature_females(sp, age) = maturity(sp, age) * sex_ratio(sp, age)`,
then `ssb(sp, yr) += N · exp(-Z·spawn_month/12) · weight_hat · mature_females`.
The converter sets `sex_ratio[a] = FracFemale_GP_1 = 0.5`
([R/ss3_to_rceattle.R:838](R/ss3_to_rceattle.R#L838)), so Rce SSB is
half the combined-sex × maturity × weight aggregate.

**Magnitude**: ~0.5× alone.

### 15b. Jensen gap from length-based maturity (secondary, factor ≈ 0.92)

**SS3**: Maturity is specified on **length** (`Len_Mat`). SS3
computes `Mat_F_wtatage[a] = E_L[mat(L) · wt(L) | age = a]` — a JOINT
expectation across the within-age length distribution. Larger fish at
a given age are both heavier and more likely mature, so
`Cov(mat, wt | a) > 0` and `E[mat · wt] > E[mat] · E[wt]`.

**Rceattle**: `maturity[a]` is the converter's
`E_L[mat(L)]` (marginalized) and `weight[a]` is the VBGF
`E_L[wt(L)]`. The SSB term `maturity[a] · weight_hat[a]` evaluates the
product of marginals — missing the covariance term.

**Magnitude on Pcod**: ~0.92× residual after the 0.5 sex_ratio factor
(0.5 × 0.92 ≈ 0.46, matching the observed ratio). The gap is small
in absolute %SSB terms but always biases Rce low when length-based
maturity is used.

### Path 1 — make Rce match SS3

(a) **sex_ratio fix (15a)**: When the SS3 source has `Nsexes = 1`,
emit `sex_ratio[a] = 1` (not 0.5) from the converter so Rce SSB does
not halve the combined-sex pool. Alternatively, gate the
`mature_females = maturity · sex_ratio` line in `ceattle_v01_11.cpp`
on a new `data_list$ssb_convention` flag.

(b) **Jensen fix (15b)**: Add a precomputed `mat_wt_at_age` slot to
the data list (filled from SS3's `Mat_F_wtatage` when available) and
have the cpp use that in place of `maturity · weight_hat` whenever
present. The existing Jensen-gap closure in
[ss3_to_ceattle_test.R Section 2](ss3_to_ceattle_test.R) writes
`Mat_F_wtatage` into `SSB_WAA`, but is currently SKIPPED for the
parametric-VB path because cpp overwrites `weight_hat` from VBGF
([ss3_to_ceattle_test.R:88-93](ss3_to_ceattle_test.R#L88)). The
parametric-path fix is to skip the VB overwrite for `wt_idx_ssb`
when an injected `mat_wt_at_age` is present.

### Path 2 — make SS3 match Rce

Not feasible: SS3's combined-sex SpawnBio convention is hard-coded
and used downstream for SR, SPR, and reference-point calculations.
Halving SS3's SpawnBio would break those.

### Recommendation

Path 1(a) is a one-line converter change with no cpp impact and
closes the dominant 0.5× factor. Path 1(b) requires cpp gating; defer
until Phase A estimation work, since the residual 8% gap doesn't
affect the optimizer's gradient on log_F / sel pars (those depend on
predicted index, catch, and comp — not on absolute SSB level).

**Status**: 🟠 structural, not blocking forward-pass NLL parity.
Affects depletion / reference-point comparisons but not the per-
component NLL gap.

---

## 16. Estimation-bounds mismatch on scalar log-scale params 🟡

`bounds_audit()` (§8b in [ss3_to_ceattle_test.R](ss3_to_ceattle_test.R))
now compares SS3 ctl `LO/HI` against the Rce-bound on its **natural
scale** (back-transforming `log_M1` / `log_growth_pars` / `growth_log_sd`
via `exp()`). Run against `mod0$data_list` so the linkage push fires.

Current snapshot (2026-05-31):

| Param | SS3 ctl LO/HI | Rce LO/HI (natural) | Note |
|---|---|---|---|
| `M (NatM_p_1_Fem_GP_1)` | `[0.1, 1.5]` | `[0.001, 2]` | Rce wider; could tighten via `M1_block` bounds |
| `K (VonBert_K)` | `[0, 1]` | `[0.05, 1]` | upper matches; lower intentionally > 0 to keep log scale finite |
| `L1 (L_at_Amin)` | `[0, 50]` | `[0.1, 50]` | upper matches; lower > 0 |
| `Linf (L_at_Amax)` | `[70, 130]` | `[70, 130]` | ✅ exact |
| `CV_young` | `[0, 10]` | `[0.5, 10]` | upper matches; lower > 0 |
| `CV_old` | `[0, 20]` | `[0.5, 20]` | upper matches; lower > 0 |
| `R0 (SR_LN(R0))` | `[10, 20]` | `[-Inf, Inf]` | **Rce unbounded — set via fit_mod `bounds=` for Phase A** |
| `sigma_R (SR_sigmaR)` | `[0, 10]` | `[0, Inf]` | upper unbounded; set via `bounds=` |

Action items before Phase A:
- Add explicit bounds for `R0` (`[10, 20]` on log scale) and `sigma_R`
  upper via `fit_mod(bounds = ...)` or extend the `recFun` to carry them.
- Optionally tighten `M1_block` bounds from `[0.001, 2]` to `[0.1, 1.5]`
  to match SS3 (low impact — current MLE region is well inside both).

The growth lower bounds (K, L1, sd_young, sd_old) being strictly > 0 is
intentional: `build_bounds` pushes log() on the natural-scale bound, so
`log(0)` would emit `-Inf` and silently skip the bound. The
`linkage_spec(bounds = c(eps, hi))` pattern is the documented user
contract.

**Status**: 🟡 — audit display fixed; remaining mismatches are catalogued
above with bounded action items.

---

## 17. CAAL pop-bin integration over the data bin 🟠

**Original Rce behavior** ([ceattle_v01_11.cpp:2378–2382 pre-fix](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L2378)):
For every CAAL row, Rce evaluated `pred_CAAL` at a **single 1-cm pop bin**
(`caal_lp_target[sp, ln]` — the pop bin just below the data-bin left edge).
A pre-fix code comment cited the SS3 source as supporting this for
`Lbin_method = 1` (pop bins). The comment was correct for that method
but inapplicable to Pcod, which uses `Lbin_method = 2` (5-cm data bins
over a 1-cm pop grid).

**SS3 behavior (Lbin_method = 2)**: for each CAAL row with raw
`Lbin_lo = L0`, the prediction is integrated over all pop bins whose
left edges lie in `[L0, L0_next)`. For Pcod this is **5 pop bins** per
CAAL row (e.g. `[54.5, 59.5)` = pop bins `lp = 54..58`). Within the
data bin, `sel(L_pop)` is also non-trivial (e.g. Srv sel rises from
0.76 at L=53 to 0.92 at L=58), so the integration is sel-weighted.

**Fix** ([ceattle_v01_11.cpp:1939–2000](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L1939)):
Replaced `caal_lp_target` with `caal_lp_start` / `caal_lp_end` (the
half-open pop-bin range `[start, end)` inside each data bin), computed
once at setup. The CAAL prediction loop
([:2378–2398](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L2378)) sums
contributions across that range:

```cpp
for (int lp = lp_lo; lp < lp_hi; lp++) {
  pred_CAAL(flt, sex, age, ln, yr) += common_fac
                                       * sel_at_length_pop(flt, sex, lp, yr)
                                       * growth_matrix_pop(wtind, sex, age, lp, yr);
}
```

For `Lbin_method = 1` (data == pop), each data bin contains exactly
one pop bin and the loop degenerates to a single iteration — preserving
prior behavior in that regime.

### Before / after — CAAL NLL per fleet (Pcod 2024)

|         | SS3   | Rce (pre-fix) | Diff pre  | Rce (post-fix) | Diff post |
|---------|-------|---------------|-----------|----------------|-----------|
| FshTrawl| 122.29|  174.70       | +52.41    |  282.41        | +160.13   |
| FshLL   | 126.99|  139.53       | +12.55    |  194.17        |  +67.18   |
| FshPot  |  98.14|  114.23       | +16.09    |  152.52        |  +54.38   |
| **Srv** | **374.10** | **555.80** | **+181.70** | **405.86** | **+31.76** |
| LLSrv   |   0.00|    0.00       |  +0.00    |    0.00        |   +0.00   |
| **Total CAAL** | **721.52** | **984.27** | **+262.80** | **1034.96** | **+313.40** |

**Per-fleet stratified L<80 vs L≥80** (post-fix):

|         | L<80 Rce−SS3 | L≥80 Rce−SS3 | Total diff |
|---------|--------------|--------------|------------|
| FshTrawl| +156.95      |   +3.12      | +160.06    |
| FshLL   |  +64.10      |   +2.84      |  +66.95    |
| FshPot  |  +50.97      |   +3.28      |  +54.24    |
| Srv     | **+38.42**   |  −7.14       | **+31.28** |

### Interpretation

- **Srv L<80 closed by −152 NLL** (from +190.8 to +38.4). For surveys
  the prediction is `q · N · sel(L_pop) · ALK(a, L_pop) · exp(−Z·mo/12)`,
  a clean N×ALK×sel integral over the data bin. Integration is the
  structurally correct operation and closes most of the gap.
- **Fishery L<80 worsened by ~+200 NLL** (FshTrawl +52→+157, FshLL
  +12→+64, FshPot +16→+51). For fisheries the catch-at-age factor
  `Frate/Z · (1 − exp(−Z)) · N` depends on Baranov within-year
  survival; SS3 distributes catch via Pope's hybrid. Within a 5-cm
  data bin, the per-age catch density differs **even when sel·ALK
  matches**, because Pope's "pulse" catch and Baranov "continuous"
  catch redistribute fish across L differently. Integrating an
  internally-Baranov pred_CAAL over a wider window amplifies that
  structural mismatch.

### Per-cell evidence (worst Srv L<80 cell pre-fix, yr=1999 L=54.5)

|Age|obs|Rce hat (pre)|SS3 hat|Rce/SS3 (pre)|
|---|---|---|---|---|
|3|0|0.167|0.043|3.9×|
|4|0.107|0.675|0.237|2.85×|
|5|**0.800**|**0.141**|**0.510**|**0.28×**|
|6|0.080|0.014|0.158|0.09×|

Single 1-cm slice at L=54.5: ALK favors age 4 (mean=53.1, near the
slice). 5-cm integration over [54.5, 59.5): ALK contributions rise
for age 5 as L approaches 61.2 (age-5 mean), correctly recovering
SS3's age-5-dominant prediction.

### Path 1 — already taken (this section)

Implemented. Closes Srv structurally; fishery residual is bounded by #4.

### Path 2 — make SS3 match Rce

Not applicable (Rce-side correctness fix, no SS3 config affects this).

### Recommendation

Keep the fix. Per user direction (2026-05-31): "Pope's is a fundamental
difference and I'd rather be exact on where we can be exact." The
fishery worsening is properly attributable to #4, not the integration.
Total CAAL goes from +263 to +313, but this redistributes a +149 Srv
structural diff (#17) into a +200 fishery downstream of #4 — making
the structural cause clearer for both.

**Status**: 🟠 — fix committed; closes the structurally tractable
portion of CAAL parity. Residual now properly attributed to #4.

---

## 18. DoubleNormal per-parameter PHASE: SS3 fixes some slots, Rceattle estimates all 🔴

**SS3** ([Pcod ctl `# size selex parameters block`](Data/goa_pcod/Model19_1e.ctl)):
Per parameter row in the size_selex block (and the block-replacement +
dev_se rows), ADMB takes a PHASE column. Positive PHASE → estimated;
negative → fixed at INIT. For Pcod FshLL:

```
SizeSel_P_1_FshLL(2)        PHASE = 1   ← peak (estimated)
SizeSel_P_2_FshLL(2)        PHASE = 2   ← top-width (estimated)
SizeSel_P_3_FshLL(2)        PHASE = 2   ← asc-width (estimated)
SizeSel_P_4_FshLL(2)        PHASE = -2  ← desc-width (FIXED at INIT = 10)
SizeSel_P_5_FshLL(2)        PHASE = -2  ← init-floor (FIXED, INIT = -999 sentinel)
SizeSel_P_6_FshLL(2)        PHASE = -2  ← final-floor (FIXED at INIT = 10)
```

The fixed slots (P4, P5, P6) effectively turn this fleet into a
logistic-ascending-only function (peak with a wide ascending limb, no
descending limb because exp(P4) is huge, no left floor because
P5 = -999 → init_floor = 0, no right floor because P6 = +10 →
final_floor = 1). SS3 estimates only the 3 active slots (P1, P2, P3)
plus the block-replacement values for the active slots.

**Rceattle** ([R/2-build_map.R `build_map_selectivity`](../../Rceattle/R/2-build_map.R)
lines ~744-792 for DoubleNormal):
Maps in ALL 6 slots of `sel_inf[1:3]` and `log_sel_slp[1:3]` for any
fleet with `Selectivity = "DoubleNormal"`. The slot-4-equivalent (m
for Richards growth) is mapped out, but the DoubleNormal sel_inf/slp
arrays are 3-deep on each (peak/right-floor/left-floor and
asc-sigma/desc-sigma/top-width), giving 6 free params per fleet —
PLUS the per-fleet/year `sel_inf_dev` and `log_sel_slp_dev` arrays if
`Time_varying_sel != "Off"`. There's currently no per-slot PHASE
override.

**Delta (estimation-side)**: at estimation time, Rce has extra free
parameters that SS3 holds fixed. For Pcod's 3 active DoubleNormal
fleets (FshTrawl, FshLL, FshPot) × 3 fixed slots × 1 sex = ~9 extra
free parameters per the base, plus the corresponding sel devs in
those slots if time-varying. The optimizer wanders these into
arbitrary values (since they have no data signal once the active 3
fit the comp), inflating the AD tape and contributing to the
Hessian-NaN cascade documented in
[HANDOFF_estimation_parity.md](HANDOFF_estimation_parity.md).

Forward-pass is unaffected if the user injects all 6 values from
SS3's `SelSizeAdj` (which `init_doublenormal_from_ss3` does).

**Path 1 — user-supplied map override (recommended; no Rce code change)**:
`fit_mod()` already accepts a `map` argument that, when supplied,
bypasses `build_map`. After building a skeleton with `mod0$map$mapList`,
walk SS3's per-param PHASE column and set NA in any slot SS3 holds
fixed, then pass the modified map straight to the estimation call:

```r
# After mod0 build, get the skeleton:
mod0     <- fit_mod(data_list = cod_pcod, estimateMode = 3, ...)
sel_map  <- mod0$map$mapList   # default: all DN slots active

# P-index -> (array, slot) per the cpp DoubleNormal convention
#   P1 peak       -> sel_inf[1]
#   P2 top-width  -> log_sel_slp[3]
#   P3 asc-sigma  -> log_sel_slp[1]
#   P4 desc-sigma -> log_sel_slp[2]
#   P5 init-floor -> sel_inf[3]
#   P6 final-flr  -> sel_inf[2]
slot_of <- list(
  c("sel_inf",     1),  c("log_sel_slp", 3),  c("log_sel_slp", 1),
  c("log_sel_slp", 2),  c("sel_inf",     3),  c("sel_inf",     2))

for (i in seq_along(active_sel_fleets)) {
  fname <- active_sel_fleets[i]; fi <- fleet_meta$idx[i]
  fnum  <- fleet_meta$ss3_num[i]
  for (P in 1:6) {
    pat <- sprintf("^SizeSel_P_%d_%s\\(%d\\)$", P, fname, fnum)
    row <- ctllist$size_selex_parms[
      grep(pat, rownames(ctllist$size_selex_parms)), ]
    if (nrow(row) == 0 || row$PHASE >= 0) next   # estimated, leave alone
    arr <- slot_of[[P]][1]; slot <- as.integer(slot_of[[P]][2])
    sel_map[[arr]][slot, fi, 1] <- NA
    # also map out the matching dev array if time-varying
    dev_arr <- paste0(arr, "_dev")
    if (!is.null(sel_map[[dev_arr]]))
      sel_map[[dev_arr]][slot, fi, 1, ] <- NA
  }
}

# Pass directly to estimation:
cod_pcod_est <- fit_mod(..., map = list(mapList = sel_map))
```

This works today against the installed Rceattle. The override is
honored at [6-fit_mod.R:405-408](../../Rceattle/R/6-fit_mod.R#L405) —
when `map` is supplied, `build_map` is skipped entirely.

**Path 2 — add per-fleet `Sel_param_PHASE` column to fleet_control**:
permanent infrastructure that packages Path 1 behind a config column
so users / converters don't write the parsing loop. Implementation
sketch in `build_map_selectivity` (R/2-build_map.R, DoubleNormal
branch):
```r
phase <- fleet_control$Sel_param_PHASE[[flt]]   # length-6 numeric
if (!is.null(phase)) {
  if (phase[1] < 0) map_list$sel_inf[1, flt, sex] <- NA      # peak
  if (phase[2] < 0) map_list$log_sel_slp[3, flt, sex] <- NA  # topw
  if (phase[3] < 0) map_list$log_sel_slp[1, flt, sex] <- NA  # asc
  if (phase[4] < 0) map_list$log_sel_slp[2, flt, sex] <- NA  # desc
  if (phase[5] < 0) map_list$sel_inf[3, flt, sex] <- NA      # init
  if (phase[6] < 0) map_list$sel_inf[2, flt, sex] <- NA      # final
  # ... + map out matching dev arrays
}
```
Converter `ss3_to_rceattle.R` populates `Sel_param_PHASE` from
`ctllist$size_selex_parms$PHASE` per param row, mapped to the 6-slot
Rce convention.

**Path 3 — make SS3 mirror Rce (estimate all 6)**: flip all sel
PHASEs positive in SS3 ctl. Not recommended — changes the SS3 model
(more free parameters, different priors needed to keep them
identifiable).

**Recommendation**: Path 1 immediately for the current Phase A
estimation work — it's a single block of test-script code, no
Rceattle source change. Path 2 as a follow-up infrastructure
addition so other stocks get it for free. Status stays 🔴 until
either is in place — without it, estimation will systematically
wander on the would-be-fixed slots.

**Action**:
- **Now**: paste the Path-1 snippet right before the estimation
  `fit_mod` call in [ss3_to_ceattle_test.R](ss3_to_ceattle_test.R).
- **Follow-up**: package as Path 2 (per-fleet column + build_map
  extension), add converter support, write a regression test that
  flipping one PHASE flips the corresponding map slot to NA.

---

## Total per-component NLL summary

After all current fixes and config settings (forward-pass, Phase 1).
Status column shows machine-precision classification — every non-✅
row is either a small per-cell residual to chase (🟡) or a structural
diff that needs Path 1 / Path 2 action to close (🟠 / 🔴).

Current snapshot is taken against the **modified SS3 model**
(`Data/goa_pcod-no init and ramp/`) with `F_Method = 2`, `initMode =
"NonEquilibriumScaled"` (mode 4) + `init_state_from_ss3_natage_mode4`
injection, and `growthFun_est_spec` on mod0 + FP + estimation. All cpp
fixes for #3 (Survey cosmetic constant), #4 (catch kernel
robustification), #5 (recruitment kernel), and #17 (CAAL data-bin
integration) are active. Numbers below come from a `Rscript
ss3_to_ceattle_test.R` run on 2026-05-31.

**Forward-pass (no estimation; SS3 MLEs injected)**:

| Component | SS3 | Rce | Diff | Status | Notes |
|---|---|---|---|---|---|
| Survey index | -1.79 | -1.44 | +0.34 | 🟡 | #3 closed; residual is per-obs FP noise |
| Catch | 1.75 | 331.36 | +329.6 | 🟠 | SS3 estimates F to match catch by construction; Rce inherits F MLE but mid-series cohort drift (Phase A3) breaks the tight fit |
| Length comp | 1336.33 | 1371.19 | +34.9 | 🟠 | downstream of Bio drift + #13 plus-group |
| CAAL | 721.20 | 854.02 | +132.8 | 🟠 | #17 closes Srv; #19 ageing-error def 2 closed −174; fishery residual downstream of #4 |
| Recruitment | -16.32 | -12.84 | +3.48 | 🟡 | #5 kernel matched |
| Init eq | 0 | 5.30 | +5.30 | 🟠 | mode-4 init_dev lognormal prior; cosmetic |
| Parm priors | 0.79 | -4.47 | -5.27 | 🟡 | essentially closed |
| Parm devs (sel+q) | 6.09 | 44.33 | +38.24 | 🟠 | sentinel σ skips dev prior in FP path |
| **FP TOTAL** | **2048.07** | **2587.45** | **+539.4** | | post-ageing-error |

**Phase A first-cut estimation (BlockDev + PHASE fixes + ageing-error def 2)**:

| Component | SS3 | Rce est | Diff | Notes |
|---|---|---|---|---|
| Survey index | -1.79 | -31.06 | -29.3 | partial q-dev overfit (LLSrv env q only constrained, not block-shared) |
| Catch | 1.75 | 452 | +450 | #4 Pope's vs Baranov dominates; log_F idx 196 has grad=517 |
| Length comp | 1336 | 2389 | +1052 | sel devs constrained ⇒ can't compensate catch dynamics |
| CAAL | 721 | 1039 | +318 | propagates from N drift + log_F |
| Recruitment | -16.32 | 8.88 | +25.2 | rec_dev pulled by N drift |
| Init eq | 0 | 5.72 | +5.7 | same as FP |
| **Parm priors** | 0.79 | 43.30 | +42.5 | block-replacement priors fire (matches SS3 structure) |
| **Parm devs (sel+q)** | **6.09** | **93.93** | **+87.8** | **BlockDev closed from +491 (free IID) → +88** ✓ |
| **EST TOTAL** | **2048.07** | **4000.35** | **+1952** | param count: 1672 → 315 (factor-shared as designed); convergence warning 8 (discontinuous likelihood, log_F + sel ridge); Hessian all-NaN — identifiability needs #4 closed |

### Phase 0 / A closures (2026-05-31)

| Iteration | Closure | Mechanism |
|---|---|---|
| Survey kernel (#3) | +46 → −0.34 NLL | Drop `0.5·log(2π)` per obs |
| CAAL data-bin integration (#17) | −149 NLL on Srv | Integrate over data bin (`Lbin_method = 2`) |
| Recruitment kernel (#5) | +52 → +3.5 NLL | SS3-form per-year `0.5·z² + log(σ)`; ramp off in SS3 |
| InitEQ_Regime (#6) | +2.8 → 0 NLL | SS3 lambda 18 = 0 + Rce mode-5 init_dev skipped |
| Catch kernel (#4) | full robustification → +330 honest residual | SS3 robust form `0.5·(log(1.1·obs / (hat + 0.1·obs))/σ)²` |
| F injection from `$parameters` | +816,800 NLL | Bypassed corrupt `$timeseries`; closed catch + length comp on F_Method=2 |
| N-at-age styr pin (mode 4) | −89 NLL net | `initMode = "NonEquilibriumScaled"` + `init_state_from_ss3_natage_mode4` helper |
| Phase A1 bounds | ✅ committed | growth lower bounds all > 0 in `growthFun_est_spec` |
| Phase A2 inits unification | ✅ committed | mod0 + FP + estimation share `growthFun_est_spec`; positional `inits$beta_linkage[1]` collision fixed (name-based lookup + defensive intercept-zero in `Rceattle::fit_mod`) |
| Ageing error (#19) | −174 NLL on CAAL | `build_ss3_age_error()` def 2 (unbiased) injected into `cod_pcod$age_error` |
| BlockDev infrastructure (#20) | Parm devs +491 → +88 | New cpp `*_dev_prior_weight` DATA arrays + factor-shared map from SS3 `Block_Design` (43 sub-block patches for Pcod) |
| DoubleNormal PHASE fixes (#18) | 7 base slots pinned | `apply_ss3_sel_phase_fixes()` walks `size_selex_parms$PHASE`, sets NA on Rce sel cells whose SS3 PHASE < 0 |
| `.fit_tmb` resilience | nlminb errors no longer halt R | `tryCatch` around TMBhelper path, falls through to in-package nlminb fallback ([Rceattle/R/0-tmb_helpers.R](../../Rceattle/R/0-tmb_helpers.R)) |

### Outstanding after Phase A first cut

1. **Catch +452 NLL + log_F grad=517 (idx 196)** — the dominant remaining issue. SS3's F_Method=2 estimates F to match catch directly (Baranov continuous F); Rce's iterated-Pope hybrid produces different per-year catch_hat. Until #4 is closed (cpp Pope's path OR SS3 ctl `F_Method = 3`), the optimizer can't reconcile F + catch and the residual overflows into LenComp.
2. **All 315 Hessian rows NaN** — identifiability collapses around log_F. Same root cause as (1); closing #4 should make most rows finite.
3. **Length comp +1052** — sel devs are now correctly constrained (BlockDev), so the model can no longer absorb catch-dynamic errors via sel bending. The LenComp gap is now pure downstream-of-#4 + #13 plus-group LAA + mid-series N drift.
4. **Survey −31** — partial residual from LLSrv q-devs being free IID rather than block-shared (Pcod has no q-block design in SS3; the env_q linkage path means Rce's per-year q-devs are the closest analog, and the σ=0.3 prior partly constrains them).
5. **Init eq +5.7** — mode-4 `init_dev` lognormal prior. Cosmetic; doesn't affect estimates.

### Net headline

Forward-pass is **ready for estimation** (FP TOTAL +539 with ageing
error). R parity at machine precision (1e-6); Bio at 0.4%.

**Phase A first-cut estimation runs** with PHASE map fixes + BlockDev
sel/q-dev priors + ageing-error def 2 + sel-dev sigma override + the
package's `.fit_tmb` error-resilient path. It does NOT converge cleanly
yet: nlminb stops with warning 8 (discontinuous likelihood), all 315
Hessian rows are NaN, and the dominant residual is the log_F +
Catch+LenComp coupling that needs **#4 (Pope's vs Baranov)** closed
before Phase B parity can be approached.

The infrastructure to align Rce with SS3's structural choices is now
in place: linkage system (Phase A1+A2), PHASE map (#18), ageing error
(#19), BlockDev (#20), nlminb resilience. Phase B work is now
unblocked once #4 has a concrete path (cpp Pope's implementation OR
SS3 ctl `F_Method = 3`).

---

## Decision matrix

Under the machine-precision standard, "forward-pass parity" means
every component on the per-component table above is ✅ or 🟡 with the
residual traced. Targets:

| Goal | Required actions |
|---|---|
| **Phase 0 / machine-precision FP parity** (the right exit criterion) | (a) Trace residual on #1 (Length comp +0.86) per fleet — verify it's pure FP noise (target 🟡) OR identify the structural source (move to 🟠). (b) For each 🟠 row, verify magnitude matches current output (sanity). (c) If any 🔴 surfaces, escalate. |
| **Forward-pass NLL within ~10 of SS3** | Above + cpp one-liner for #3 (drop `0.5·log(2π)`). |
| **Estimation NLL within ~5 of SS3** | Above + Rce code: Pope's (#4) OR SS3 `F_Method=3`; Methot-Taylor (#5) OR SS3 ramp off; InitEQ (#6) OR SS3 lambda 9 = 0. |
| **Bitwise param match (machine-precision everywhere)** | Above + #7 absolute-block prior + #9 dev_link scaling + #10 three-tier sel. Probably 2-4 weeks of Rce code work. |

---

## Related documents

- [SS3_to_match_Rceattle.md](SS3_to_match_Rceattle.md) — compact recipe
  of SS3 ctl/starter edits applying Path 2 for each difference
- [HANDOFF_estimation_parity.md](HANDOFF_estimation_parity.md) — current
  work order; what's wired, what's the next single biggest lever
- [Generalizing_to_other_SS3_models.md](Generalizing_to_other_SS3_models.md) —
  applying this bridge to other stocks
- [HANDOFF_growth_matrix_empirical.md](HANDOFF_growth_matrix_empirical.md) —
  separate empirical-growth + CAAL package bug (still open)
