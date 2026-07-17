# Handoff — Bridging the 2021 GOA pollock WHAM model into Rceattle

Status as of 2026-07-14. Everything below marked **VERIFIED** was confirmed by running code, not inferred.

> ## ⚠️ 2026-07-15 session — READ THIS FIRST
>
> This session moved to **macOS** (the Windows scratch paths below are dead) and overturned
> three claims in the original handoff. Corrections are in **§ 2026-07-15 corrections** at the
> bottom. Summary of what changed:
>
> 1. **The reference fit does NOT reproduce.** `opt$objective` = **474.0967 is not reachable
>    here.** Best properly-converged optimum found: **489.9975** (max gradient 8e-06). The
>    surface is multimodal. Everything downstream that compares against 474.0967 is suspect.
> 2. **The N1 age-structure description is WRONG.** WHAM does *not* apply the year-1
>    recruitment deviate to the initial age structure. Ages 2+ sit at equilibrium at
>    `R0 = exp(mean_rec_pars)`, independent of `log_N1_pars`.
> 3. **`initMode = 1` alone does NOT match WHAM.** Rceattle and WHAM index M differently in
>    the equilibrium (departing vs arriving age). Harmless when M is constant; a **2× error at
>    age 2** for pollock. Fix verified — see corrections.
>
> New/changed files: `2021 pollock update data.R` (**new**, builds the Rceattle data list),
> `2021 pollock bridging.R` (**new**, Phase-1 forward-pass check — **PASSES exactly**),
> `Data/pkwham/akwham_input_2021.RDS` (**re-downloaded** — was missing),
> `Data/2021pollock_wham.Rdata` (**now holds the ~489.67 fit, not 474.0967**).
>
> **✅ Phase 1 (forward-pass validation) is DONE.** With WHAM's converged selectivity fed as
> `emp_sel` and N/F/q mapped from `parList`, Rceattle reproduces WHAM's year-1 N-at-age, SSB,
> F, catch, and all six index predictions **to ~5e-16 (machine precision)**. The data build is
> correct.
>
> **✅ Phase 2 native parameterization — FIXED validation DONE, estimation OPEN (2026-07-16).**
> The data build now carries the full native selectivity config, and the fixed-parameter check
> reproduces WHAM **exactly** (all derived quantities + all 7 selectivity blocks to machine
> precision; Shelikof to 6.5e-07). This proves every selectivity/q parameterization mapping.
> **But FREE estimation diverges — Rceattle lands SSB up to ~112% off WHAM.** Root cause is
> diagnosed (differently-weighted penalized objective); see **§ Phase 2 estimation diagnosis**.
>
> **✅ Phase 3 OSA cross-check — DONE, EXACT (2026-07-17).** Rceattle's one-step-ahead (OSA)
> residuals match WHAM's to **~1e-13 (machine precision)** across **all 1031 observations** —
> every aggregate index/catch, fishery age-comp, and survey age-comp, `r = 1.00000000`. Because
> both models are fixed-effects (random NULL), OSA residuals are conditioning-order-invariant, so
> an *exact* match is attainable (not merely the growth-template's tight correlation). Lives in
> `2021 pollock bridging.R` **Phase 3**, mirroring `tests/comparison/WHAM-OSA-comparison.R`.
> Three conditions make it exact + one C++ finding + one data-build fix — see **§ 2026-07-17**.
> **This session also DISPROVED the Phase-2 "root cause" (lead #1 below): the `4*sel_dev_sd`
> slope penalty contributes only 13.7 of the 761 selectivity penalty; the real driver is an
> age-scale vs logit-scale deviate mismatch. See the correction in § Phase 2 diagnosis.**
>
> **✅ DELIVERABLE MET (scope confirmed 2026-07-17).** The requirement is *"show the OSAs are the
> same when the models are at the same solution"* — parameters may be fixed. Phase 3 does exactly
> this and is EXACT. **Phase 2 FREE estimation is DESCOPED — not required.** (For the record it
> does NOT freely converge; the dominant obstacle is a structural initial-recruitment / R0
> difference — free Rceattle R0 lands 64% high — see § Phase 2 diagnosis. Left documented, not
> pursued.)

## Goal

Bridge the 2021 GOA pollock model implemented in WHAM ("pkwham") into Rceattle, following the
house style of the sibling year scripts: a short final model script plus a bridging script.
The two final models should converge to the same solution.

Deliverables (target):

- `2021 pollock model.R` — final Rceattle model (mirrors `2023 pollock model.R` / `2024 pollock model.R`)
- `2021 pollock bridging.R` — bridging script (mirrors `2023 pollock bridging.R` / `2024 pollock bridging.R`)
- possibly `2021 pollock update data.R` (mirrors `2024 pollock update data.R`)

Source of the WHAM model: <https://github.com/afsc-assessments/GOApollock/tree/main/alt_models/pkwham>
(`functions.R::match_input`, `run_wham.R`, `pkwham_bridging.R`).

## What is DONE

### 1. pkwham sources vendored — `Data/pkwham/`

Downloaded from the GOApollock repo: `akwham_input_2021.RDS` (the archived, prepared WHAM input),
`goa_pk_asap3.txt`, `functions.R`, `run_wham.R`, `pkwham_bridging.R`.

### 2. WHAM reference model reproduced and fitted — `Data/2021 Pollock WHAM.R` **VERIFIED, RUNS**

Run it from the project root. It saves `Data/2021pollock_wham.Rdata` containing `fit`, a list with
`$opt`, `$rep`, `$parList`, `$input`.

**Expected result: `opt$objective` = 474.0967, `opt$convergence` = 0.** If a future run disagrees, stop
and investigate before trusting anything downstream.

Two non-obvious things that script had to solve — its header comments explain both:

- **The archived RDS no longer builds.** It was written under wham 1.0.6.9000; the installed wham is
  1.0.7.9000, which added the length/growth data fields, so loading it fails with
  `Error when reading the variable: 'n_lengths'`. Rather than pin an old wham, the script rebuilds the
  input skeleton with the installed wham via `prepare_wham_input()` and transplants the archived values.
  This is safe because **every** quantity `match_input()` derives from the ADMB report is an *initial
  value for an estimated parameter*; the only things held fixed are explicit constants, which the script
  sets directly. **VERIFIED:** the asap3-derived data is identical between wham versions — the only five
  differing fields are exactly `match_input()`'s intended overrides (`mature`, `waa`, `fracyr_indices`,
  `bias_correct_oe`, `bias_correct_pe`).
- **Current wham drops 56 observations** (1190 → 1134): the Shelikof (index 1) age-1/age-2 composition
  observations, because that selectivity block is fixed at 0 for those ages. **VERIFIED benign** — those
  observed proportions are *exactly 0* in the data, so their multinomial contribution is zero either way
  and the ages 3–10 proportions already sum to 1 (the rescale is a no-op).
- `fit_wham(do.fit = TRUE)` **crashes** on this model. Cause: the Shelikof age-specific selectivity
  saturates at exactly 1 for ages 5–7, so the Hessian is singular and the estimability check dies. The
  script optimizes the TMB object directly with `nlminb` + restarts instead. This is expected, not a bug
  to fix.

### 3. Rceattle dev branch compiled — **VERIFIED, loads as v4.6.0**

`pkgload::load_all()` **fails on Windows**: pkgbuild's debug build (`DEBUG=true` → `-O0 -g`) blows past
the object-file section limit —
`as: ceattle_v01_11.o: too many sections (152659) ... Fatal error: ... 'file too big'`.
Workaround that works: install with normal optimization via `R CMD INSTALL`. To avoid clobbering your
own Rceattle 4.5.0 (master), it was installed into a **scratch library**:

```
C:\Users\GRANT~1.ADA\AppData\Local\Temp\claude\c--Users-grant-adams-GitHub-Rceattle-ecosystem-Rceattle-models-GOA-pollock\c037bd60-6758-4167-ac00-831e69beac94\scratchpad\devlib
```

Prepend that to `.libPaths()` before `library(Rceattle)`. **This is a scratch/temp path and will not
survive** — for real work, either install dev into your normal library or re-run:

```
R CMD INSTALL --no-multiarch --no-docs --no-byte-compile --library=<lib> "Rceattle"
```

(from `c:\Users\grant.adams\GitHub\Rceattle ecosystem`, with the `Rceattle` checkout on `dev`).

Note: the `Rceattle` checkout was switched from `dev-DSEM` to `dev`, and a dirty `.Rhistory` was
stashed (`git stash`) — **you have a stash to restore or drop.**

## The WHAM model being bridged — all VERIFIED empirically

1 species, ages 1–10, years 1970–2021 (52 yrs), 1 fishery + 6 indices.

- **M is FIXED**, age-varying, time-invariant: `1.39, 0.69, 0.48, 0.37, 0.34, 0.30, 0.30, 0.29, 0.28, 0.29`
  (`exp(fit$input$par$M_a)`, map all NA).
- **sigmaR FIXED at 1** (`log_NAA_sigma` = 0, mapped off). Recruitment deviates are penalized-likelihood,
  **not** random effects (`input$random <- NULL`).
- **Maturity is time-invariant** (checked: all 52 rows identical) and already halved for female-only SSB:
  `0, 0.006805, 0.028435, 0.136955, 0.305085, 0.430320, 0.470200, 0.489310, 0.494740, 0.496830`.
  This clears the one structural mismatch I was worried about, since Rceattle maturity is time-invariant.
- Spawning: `fracyr_SSB` = 0.21 → Rceattle `spawn_month` = 2.52.
- 4 weight-at-age matrices (`fit$input$data$waa`, 4×52×10, year-varying). Pointers: fishery + totcatch →
  waa[1]; SSB → waa[2]; Jan-1 population → waa[3]; indices → waa[c(2,3,3,2,2,4)].
  → Rceattle `ssb_wt_index = 2`, `pop_wt_index = 3`, fleet `Weight_index` = c(2,3,3,2,2,4) for indices
  1–6 and 1 for the fishery.
- Index months (`fracyr_indices[1,] * 12`): `2.508, 6.516, 7.319, 0, 0, 6.228`.
- `units_indices` = `1,1,1,2,2,1` (1 = biomass, 2 = numbers) → Rceattle `Weight1_Numbers2`.
- Catch SD = 0.05. Index SDs in `agg_index_sigma` (52×6); indices 2 and 3 are year-varying, others constant.
- Comps: multinomial (`age_comp_model_*` == 1). Neff constant per index (index1 7.1, index2 7.2,
  index3 14, index6 39.8); `catch_Neff` year-varying 0.37–74.2. **Indices 4 and 5 have no comps.**
- Years used: catch comps 1975–2020 (n=46); index obs n = 28, 15, 34, 13, 13, 5; index comps
  n = 28, 14, 11, 0, 0, 4. Select via `use_indices` / `use_index_paa` / `use_catch_paa`.
  `-999` is the missing-value code in `agg_indices`.
- Initial N-at-age: `N1_model = 1`, `log_N1_pars` = c(estimated, FIXED 0) → **unfished equilibrium with
  F = 0 in year 1** → Rceattle `initMode = 1`. WHAM applies the year-1 recruitment deviate to the whole
  initial age structure.
- F: `log_F1` + a random walk of 51 `F_devs`. Rceattle parameterizes `log_F` per year, so set
  `log_F[fishery, y] = log_F1 + cumsum(F_devs)[1:(y-1)]` (same idiom as `WHAM-growth-comparison.R`).

### Selectivity

WHAM `selblock_models`: 1 = age-specific, 3 = double-logistic. `logit_selpars` cols 1–10 = age-specific,
11–12 = logistic, 13–16 = double-logistic. Bounds for cols 13:16 are set to lower = −10, upper = 20;
transform is `sel(x) = lower + (upper − lower)/(1 + exp(−x))`.

| Block | Fleet | Form | Notes |
|---|---|---|---|
| 1 | Fishery | double-logistic | all 4 pars estimated; **time-varying on the ascending limb only** |
| 2 | Index 1 Shelikof | age-specific | ages 1–2 fixed at 0; ages 3–10 free |
| 3 | Index 2 NMFS BT | double-logistic | pars 13,14 estimated; 15,16 fixed (a2=20, b2=0.367879441171442) |
| 4 | Index 3 ADF&G | double-logistic | same structure as index 2 |
| 5 | Index 4 | age-specific | all fixed = `c(1,0,0,0,0,0,0,0,0,0)` (age-1 index) |
| 6 | Index 5 | age-specific | all fixed = `c(0,1,0,0,0,0,0,0,0,0)` (age-2 index) |
| 7 | Index 6 summer acoustic | — | all pars fixed; selectivity == 1.0 at every age |

**The most important structural finding.** `selpars_re[1:104]` = 52 iid deviates on par **13** (a1,
inflection) followed by 52 on par **14** (b1, slope) — i.e. the fishery selectivity is time-varying on
the **ascending limb only**. **VERIFIED:** `fit$rep$selAA[[1]]` varies across years by ~0.94 at age 3
while age 10 is pinned at 0.3804 in every year. This is *not* obvious from the code — `match_input()`
fills `selpars_re[1:104]` and the map turns cols 11:12 off, which naively reads as deviates on the
unused *logistic* params. The observed time-variation pattern settles it. The deviate SD is FIXED at
`exp(sel_repars)` = 0.1 (mapped off) → a penalty, not a random effect.

Converged selectivities (year 1):

- Index 1: `0, 0, 0.5521, 0.5001, 1, 1, 1, 0.7676, 0.5396, 0.2885` (ages 5–7 saturate at 1 → singular Hessian)
- Index 2: `0.1152 0.2005 0.3256 0.4818 0.6416 0.7751 0.8690 0.9274 0.9609 0.9793`
- Index 3: `0.0051 0.0189 0.0683 0.2175 0.5132 0.8000 0.9382 0.9829 0.9954 0.9988`
- Index 6: all 1.0

### Catchability

`q = 0 + 1000/(1 + exp(−logit_q))` (`q_lower` = 0, `q_upper` = 1000).

- q1 (Shelikof) and q3 (ADF&G) are **AR1 time-varying** (`q_re` cols 1 and 3, 52 each = 104 estimated).
  SDs FIXED at 0.038 and 0.05, AR1 `rho` par = 10 (`q_repars` mapped off) → effectively a
  near-random-walk penalty.
- q2, q4, q5, q6 constant. Converged: q1 0.871→1.223, q2 = 1.00704, q3 0.3103→0.2126, q4 = 0.246,
  q5 = 0.30167, q6 = 0.75.
- **No catchability prior is active.** `match_input()` on GitHub tries to set a N(0.85, 0.1) prior on the
  bottom-trawl q, but the archived input has `use_q_prior = 0` and `q_prior_re` mapped off, and the
  source comment says *"tried this to get prior on BT to match but didn't work"*. `nll_q_prior` == 0 in
  the fit. **Do not add a q prior to the Rceattle model** — this is a real, documented difference from
  the ADMB assessment and deserves a comment in the bridging script. (Contrast: the 2023/2024 bridging
  scripts do carry a BT q prior N(0.85, 0.1).)

### Converged likelihood components (`fit$rep`, total `nll` = 474.0967)

```
nll_agg_catch   -107.7700     nll_catch_acomp   612.3624
nll_agg_indices  100.7106     nll_index_acomp   382.8711
nll_NAA           88.6665     nll_sel          -414.6496
nll_q           -188.0944
```

`nll_sel` and `nll_q` are **negative** → WHAM includes the Gaussian normalizing constants in these
penalties; Rceattle likely omits them. Expect a constant offset in those components. Compare
*differences* between two parameter vectors rather than absolute values, exactly as `pkwham_bridging.R`
does in its "Phase 2" section.

## Rceattle dev conventions — VERIFIED

- **dev RENAMED parameters vs master.** The 2023/2024 scripts use master names; the 2021 scripts must use
  dev names: `log_sel_slp` (not `ln_sel_slp`), `log_F` (not `ln_F`), `index_log_q` (not `index_ln_q`),
  `R_log_sd` (not `R_ln_sd`). Confirm the full list with `names(Rceattle::build_params(dat))`.
- `fleet_control$Selectivity` (`R/0-switches.R`, `sel_map`): Fixed=0, Logistic=1, NonParametric=2,
  DoubleLogistic=3, DescendingLogistic=4, Hake=5, 2DAR1=6, 3DAR1=7, DoubleNormal=8, NonParametricPM=9,
  LogisticPM=11. String names are accepted.
- `fleet_control$Time_varying_sel` (`tv_sel_map`): Off=0, IID=1, AR1=2, Block=3, RandomWalk=4,
  RandomWalkAscending=5.
- `fleet_control$Catchability` (`q_map`): Fixed=0, Estimated=1, Estimated-with-prior=2, Analytical=3,
  PowerEquation=4, Environmental=5, AR1=6. (The 2024 script's `Catchability[1] <- 6` is AR1.)
- `fit_mod(estimateMode=)`: 0 = hindcast + projection, 1 = hindcast only, 2 = projection only,
  3 = build without optimizing, 4 = optimize all mapped out. **Trap:** for `estimateMode >= 3` the TMB
  template returns a placeholder objective (`jnll = dummy*dummy`), so `obj$fn()`/gradients are
  meaningless — read the REPORTed `jnll_comp` instead. That is exactly how the sibling bridging scripts
  check fixed-parameter agreement.
- `fleet_control` columns (dev): Fleet_name, Fleet_code, Fleet_type, Species, Month, Selectivity_index,
  Selectivity, N_sel_bins, Sel_curve_pen1, Sel_curve_pen2, Time_varying_sel, Time_varying_sel_sd_prior,
  Bin_first_selected, Sel_norm_bin1, Sel_norm_bin2, Comp_loglike, Comp_weights, CAAL_loglike,
  Weight1_Numbers2, Weight_index, Age_transition_index, Q_index, Catchability, Q_prior, Q_sd_prior,
  Time_varying_q, Time_varying_q_sd_prior, Estimate_index_sd, Index_sd_prior, Estimate_catch_sd,
  Catch_sd_prior, proj_F_prop. (`Fleet_type`: 1 = fishery, 2 = survey/index.)

## What is LEFT

1. **Build the Rceattle data list** from `fit$input$data` — *not* from the 2023/2024 pollock `.xlsx`
   (different data vintage **and** master-branch format). Building from the WHAM input guarantees the two
   models see identical data, which is required for them to converge to the same solution.
   **Best template: `Rceattle/tests/comparison/WHAM-growth-comparison.R`** (on `dev`). It starts from a
   bundled dataset (`data("GOAcod")`) and overwrites every element — `fleet_control`, `catch_data`,
   `index_data`, `comp_data`, `weight`, `maturity`, `M1_base`, `sex_ratio`, `age_error`,
   `age_trans_matrix`, `emp_sel`, `NByageFixed`, `env_data`, `ration_data` — then maps WHAM parameters
   onto `inits` and compares derived quantities with a `plot(x, y); abline(0,1)` idiom. Mirror it for both
   the data build and the comparisons.
   Gotchas: a **negative `Year`** in `comp_data`/`index_data` turns an observation off; indices 4/5 are in
   numbers; check how `emp_sel` is consumed in `R/` for the fixed selectivity blocks (indices 4, 5, 6).
2. **Write `2021 pollock bridging.R`** — load data + the WHAM fit; fit a base Rceattle model; map WHAM
   `parList` → Rceattle `inits`; fit with `estimateMode = 3` and verify derived quantities
   (selectivity-at-age, F, q, N-at-age, SSB, catch, index) against `fit$rep`; compare likelihood
   components; then estimate freely and compare to WHAM.
3. **Write `2021 pollock model.R`** — short and standalone, so prefer a separate data script
   (`2021 pollock update data.R`) that saves an `.Rdata`/xlsx the model script loads.
4. **Verify the two converge to the same solution.**

## Anticipated sticking points

- **Shelikof age-specific selectivity.** Rceattle's `NonParametric` (2) applies Ianelli-style penalties
  and typically normalizes to max = 1; WHAM's `age-specific` is a free 0–1 parameter per age with no
  normalization. This is the most likely structural mismatch — check
  `src/TMB/selectivity.hpp` and `Sel_curve_pen1`/`Sel_curve_pen2` / `Sel_norm_bin1`/`Sel_norm_bin2`
  before assuming they align. Ages 5–7 sit exactly at 1, so a normalizing constraint may be harmless
  here, but confirm rather than assume.
- **Double-logistic parameterization.** WHAM: `sel = 1/(1+exp(-(a-a1)/b1)) * (1 - 1/(1+exp(-(a-a2)/b2)))`.
  Rceattle: `1/(1+exp(-exp(log_sel_slp1)*(a - sel_inf1))) * (1 - 1/(1+exp(-exp(log_sel_slp2)*(a - sel_inf2))))`.
  So `sel_inf = a`, `log_sel_slp = -log(b)`. `WHAM-growth-comparison.R` uses exactly this
  (`inits$log_sel_slp[1,,1] <- rev(log(1/selpars[,24]))`). Check whether Rceattle normalizes the
  double-logistic to max 1 — WHAM does not.
- **The AR1 q with rho par = 10.** Check what Rceattle's `Catchability = "AR1"` (6) actually implements
  and whether the fixed SD / rho can be matched (`Time_varying_q_sd_prior`, `index_q_rho`).
- **Likelihood constants** in `nll_sel` / `nll_q` (see above) — compare differences, not absolutes.

## Reference: sibling-script conventions to match

`# Section ----` headers; terse trailing comments on `fit_mod()` args (`# Don't save`,
`# Single species mode`, `# No random recruitment`); a fixed-parameter model compared
component-by-component; then a free estimation; then `plot_biomass` / `plot_ssb` / `plot_index` at the
end driven by a `mod_list` / `model_names` pair. The 2024 bridging script's `# Uses "master" branch`
header line should become `# Uses "dev" branch` for 2021.

## § 2026-07-15 corrections — all VERIFIED by running code on macOS

### Environment (the Windows notes above are dead)

- Now on **macOS**, R 4.5.1, wham **1.0.7.9000**, `Rceattle` checkout on branch **`dev-ebs-pk`**
  (v4.7.0), with uncommitted EBS-pollock edits in the working tree.
- **You do NOT need to switch to `dev`.** `dev-ebs-pk` already *contains* all of `origin/dev`
  (it is ahead; only 4 doc-only commits behind) and has the dev parameter names
  (`index_log_q`, `log_sel_slp`, …). Switching branches would disturb the in-flight EBS work.
- `pkgload::load_all()` was not needed; `R CMD INSTALL` into a scratch library works fine on
  macOS (the Windows "too many sections" problem does not occur). Prepend the scratch
  `devlib` to `.libPaths()`. **Scratch lib is temporary** — reinstall with:
  `R CMD INSTALL --no-multiarch --no-docs --no-byte-compile --library=<lib> Rceattle`
- `Data/pkwham/akwham_input_2021.RDS` was **missing** and was re-downloaded from the
  GOApollock repo. It loads fine.

### 1. The reference fit does not reproduce (objective 474.0967) — **UNRESOLVED**

Running `Data/2021 Pollock WHAM.R` verbatim gives **628.93, convergence = 1**, not
474.0967 / convergence 0. This is not a transcription error; it reproduces deterministically.

What was tried and what it showed:

| Start / method | Objective | Max gradient |
|---|---|---|
| archived inits + 4× nlminb (the script as written) | 628.93 | 2200 | 
| archived inits + 6× nlminb + BFGS polish | 504.76 | 43.6 (stalled) |
| 8 random jitters (sd 0.15) | 578.8 – 12454 | jitter 1 **converged** at 578.8 |
| basin hopping from 504.76 (50 restarts, sd 0.30→0.02) | **489.9975** | **8.1e-06 (converged)** |

**Conclusion: the model specification is right; the optimizer is the problem.** At 489.9975
every likelihood component is close to the handoff's reported values — `nll_agg_catch`
−107.84 vs −107.77, `nll_sel` −414.75 vs −414.65, `nll_agg_indices` 101.18 vs 100.71,
`nll_q` −187.68 vs −188.09. The remaining 15.9 gap is concentrated in `nll_catch_acomp`
(620.53 vs 612.36) and `nll_NAA` (94.14 vs 88.67). So 474.0967 is a *better basin* that the
Windows run happened to land in; nlminb's path is BLAS/platform-dependent and this surface is
badly multimodal (as expected — the Shelikof selectivity saturates at exactly 1, which is
already documented above as making the Hessian singular).

`Data/2021pollock_wham.Rdata` now contains the **489.9975** fit. Options for a future session:
(a) accept 489.9975 and bridge against it — **fine for the fixed-parameter (Phase 1) work,
which only needs both models at the *same* parameters**; (b) keep hunting for 474 (try
`optimx`, TMB `newton`, or many more jitters); (c) obtain the original Windows `parList`.

### 2. `log_N1_pars` is a placeholder, not an ADMB-matched value

`old$par$log_N1_pars` = `c(13.5, -Inf)`, i.e. exactly the `exp(13.5)` the script itself passes
via `NAA_re$N1_pars`. `match_input()` never set it from the ADMB report. Consequence: the
objective at the "ADMB-matched" inits is **2875**, with `nll_agg_catch` = 2200 concentrated in
1970–1976 (1970 predicted catch is 5.1× observed, decaying to a near-perfect fit by 1979).
**This is expected, not a bug** — N1 is simply a free parameter starting at a poor guess.
Don't waste time chasing it as the handoff's earlier framing might suggest.

### 3. WHAM's N1 age structure — the handoff above is WRONG

The claim *"WHAM applies the year-1 recruitment deviate to the whole initial age structure"*
is **false**. Tested four candidate constructions against `fit$rep$NAA[1,]`; exactly one
matches, to **0.0 relative error**:

```
NAA[1,1]     = exp(log_N1_pars[1])                     # age 1 only
NAA[1,a]     = R0 * prod(exp(-M[2..a]))    a = 2..10   # R0 = exp(mean_rec_pars)
NAA[1,10]   /= (1 - exp(-M[10]))                       # plus group
```

Note **M is indexed at the *arriving* age** (`N_a = N_{a-1} * exp(-M_a)`), which is unusual.
Ages 2+ are pinned to R0 and are *independent of* `log_N1_pars`. (Confirmed numerically:
`R0*exp(-M[2])` = 1,857,046 = reported `NAA[1,2]`, whereas the handoff's reading gives 181,680.)

The `selpars_re[1:104]` structural finding **is confirmed** — and now non-circularly: the
*archived* `old$map$selpars_re` is literally `1:104` then 208 NAs, matching what the script builds.

### 4. Rceattle `initMode = 1` does NOT match WHAM — verified fix

`src/TMB/ceattle_v01_11.cpp:1208-1232`: Rceattle's equilibrium is
`N_a = R_init * exp(-mort_sum(a) + init_dev(a-1))` where
`mort_sum(a) = sum(M[1..a-1])` — **M at the *departing* age**. WHAM uses the *arriving* age.

**This is invisible in the template** `tests/comparison/WHAM-growth-comparison.R`, whose test
case has constant M = 0.35 — the two conventions coincide there. For pollock (M 1.39 → 0.29)
it is a **2× error at age 2**. Do not trust that template's `init_dev[1,] <- 0` here.

Fix (**verified: reproduces WHAM's `NAA[1,]` to 2.6e-06, which is just R0 rounding**), keeping
`initMode = 1`:

```r
inits$rec_pars[1,1] <- fit$parList$mean_rec_pars                              # R_init = R0
inits$rec_dev[1,1]  <- fit$parList$log_N1_pars[1] - fit$parList$mean_rec_pars # age-1, yr 1
inits$rec_dev[1,2:nyrs] <- fit$parList$log_NAA[,1] - fit$parList$mean_rec_pars
inits$init_dev[1,]  <- M[1] - M[2:nages]   # <-- corrects departing- vs arriving-age M
inits$log_Finit[1]  <- -Inf                # WHAM log_N1_pars[2] = -Inf -> F = 0 in year 1
```

`init_dev` is **free of any penalty** when `initMode <= 1` (`ceattle_v01_11.cpp:3095`,
`if(initMode > 1)`), so this correction costs nothing in the likelihood. The plus-group
divisor already matches because `Finit = 0`.

**Known Phase-1 likelihood offset:** Rceattle penalizes `rec_dev` for **all 52** years
(slot 10), but WHAM's `log_N1_pars[1]` is an unpenalized fixed effect — WHAM only penalizes
the 51 `log_NAA[,1]` deviates. Expect one extra `dnorm` term in Rceattle's slot 10. Also set
`bias_adjust_proc = 0` to match WHAM's `bias_correct_pe = 0`.

**Phase 2 caveat:** even with the `init_dev` fix the two are *structurally* different under
free estimation — WHAM has 1 free N1 parameter with ages 2+ pinned at R0-equilibrium, whereas
using `init_dev` as a correction leaves 9 free parameters unless they are mapped off. To make
the two models genuinely identical you would need either to map `init_dev` to a constant
offset, or add a WHAM-style equilibrium option to the cpp. **Not decided — needs your call.**

### 5. `2021 pollock update data.R` — NEW, builds and runs, **NOT yet validated**

Builds the Rceattle data list from `fit$input$data` (not the .xlsx), per the plan above.
Saves `Data/2021pollock_rceattle.Rdata`. Confirmed to run, and the structural values match
this handoff (spawn_month 2.52, M, halved maturity, `Weight_index` = 2,3,3,2,2,4,1,
`Weight1_Numbers2` = 1,1,1,2,2,1). Fleets are **1-6 = indices, 7 = fishery**.

It has **not** been through `fit_mod()` yet — the derived-quantity comparison (step 2 of
"What is LEFT") is where the next session should start. Decisions baked in that still need
checking:

- `sex_ratio = 1` (not 0.5), because WHAM's `mature` is already halved — otherwise SSB is
  halved twice. **Verify against `fit$rep$SSB`.**
- Shelikof → `Selectivity = "NonParametric"` with `Sel_norm_bin1/2 = NA`. The
  normalization concern in "Anticipated sticking points" is **still unresolved**.
- Fishery → `Time_varying_sel = 5` (RandomWalkAscending). WHAM's deviates are **iid**, not a
  random walk — `Time_varying_sel = 1` (IID) may be the correct choice. **Unverified guess.**
- Indices 4/5/6 → `Selectivity = "Fixed"` + `emp_sel`; how `emp_sel` is consumed is unchecked.
- `-999` index rows are switched off via negative `Year` and their `Observation` is
  overwritten with 1 to survive data checks. **Confirm `data_check` honours this.**

## § 2026-07-16 — Phase 2 native parameterization (all VERIFIED by running code)

Goal for this session: make Rceattle estimate **all the same parameters** as WHAM and converge
to the **same solution**. Outcome: the parameterization is fully mapped and the fixed-parameter
check is exact, but free estimation does not yet reproduce WHAM's solution.

### Selectivity — all resolved and VERIFIED

The "anticipated sticking points" about Shelikof normalization are **solved**. Empirically:

- **Shelikof age-specific (ages 1-2 = 0, 3-10 free) → `NonParametric`** with:
  - **`Bin_first_selected = 3`** zeros ages 1-2. NOT 2 — the cpp zeroing test is
    `bin < bin_first_selected` (0-based) and the R layer subtracts 1
    (`3-build_map.R:89`), so R value 3 → C++ 2 → zeros C++ bins 0,1 = ages 1,2. VERIFIED:
    `2 → [0,1,1,…]`, `3 → [0,0,1,…]`.
  - **`Sel_norm_bin1 = -1`** (any negative → normalize by max). WHAM's age-specific block is a
    bounded 0-1 parameter that saturated at 1.0 (ages 6-7), i.e. it is *already* max-normalized,
    so max-normalization reproduces WHAM's scale **exactly** (VERIFIED to 6.5e-07 with
    `sel_coff[3:10] = log(WHAM_sel[3:10])`). Range-mean normalization (`Sel_norm_bin1/2 >= 0`)
    also works but gives a different scale (divides by the range *mean*), so it does NOT match.
  - **`Sel_curve_pen1 = Sel_curve_pen2 = 0`** turns off the Ianelli monotonicity/curvature
    penalties (WHAM's age-specific block has none). **Caveat:** the `avg_sel` normalization
    penalty (`ceattle_v01_11.cpp:2780`, hardcoded weight 2, `+= 2*avg_sel^2`) is ALWAYS on for
    NonParametric and WHAM has no equivalent. It pins the *scale* (mean(exp(sel))=1), which for a
    survey is confounded with q — so it moves q, not shape. Small (0.197 at the WHAM inits) but
    nonzero, and it is one of the objective-weighting differences (below).
- **BT / ADF&G / Fishery double-logistic:** `sel_inf = a`, `log_sel_slp = -log(b)` where WHAM
  `a,b = lo + (hi-lo)/(1+exp(-(logit_selpars[,13:16] + re)))`, `lo=-10, hi=20`. Decode VERIFIED
  to reproduce `fit$rep$selAA` exactly. BT/ADF&G fix the descending limb (`a2=20, b2=exp(-1)`,
  effectively no descending over ages 1-10) — map off `sel_inf[2,]` / `log_sel_slp[2,]`.
- **Fishery time-varying ascending limb only:** WHAM has IID deviates on par 13 (ascending
  inflection) + par 14 (ascending slope), 52 each. Rceattle `Time_varying_sel = "IID"` puts
  deviates on **all four** double-logistic params; `"RandomWalkAscending"` restricts to the
  ascending limb but forces a random walk (first deviate fixed). Neither matches, so: use
  **IID** and then **map off the descending-limb deviates** (`sel_inf_dev[2,FISH,,]`,
  `log_sel_slp_dev[2,FISH,,] <- NA`). This gives IID-on-ascending-only. VERIFIED: fishery
  selectivity matches WHAM in every year (yr 26 to 1.7e-16).

### Catchability — RandomWalk, not AR1

WHAM's q1/q3 are AR1 with **rho par = 10 → tanh(10) ≈ 1**, i.e. effectively a **random walk**.
Rceattle's native `Catchability = "AR1"` (est_index_q=6) is the *Rogers et al 2024 env-index-
driven* variant — it also fits the deviates to an environmental index (`ceattle_v01_11.cpp:3037`)
and `data_check` demands `Time_varying_q` be a valid `env_data` column. That is NOT WHAM's model.
Use **`Catchability = "Estimated"` + `Time_varying_q = "RandomWalk"`** (`dnorm(dev_y-dev_{y-1},0,
sd)`, cpp:3056) with the SDs fixed at 0.038 / 0.05 via `index_q_dev_log_sd`. The data build now
does this. (One residual: RandomWalk fixes the year-1 deviate, so 51 free per index vs WHAM's 52.)
Also note the **link differs** — WHAM applies q deviates on the *logit* scale, Rceattle on the
*log* scale (`index_q = exp(index_log_q + index_q_dev)`). Near-identical when q ≪ q_upper (it is),
but not exact.

### Custom map mechanism — VERIFIED

`fit_mod(map = ...)` accepts a prebuilt map. Do NOT call `build_map()` on the raw data list (it
fails on `growth_model` before the data is cleaned). Instead take **`null$map`** from a first
`fit_mod(estimateMode=3)` call (built on cleaned data), edit `map$mapList[[param]]` (NA = fix),
then `map$mapFactor <- lapply(map$mapList, factor)`, and pass `map =`. Used to: fix `init_dev`
(equilibrium), fix `R_log_sd` (sigmaR=1), drop fishery descending deviates, fix BT/ADF&G
descending base params. Free-parameter counts then match WHAM (rec 52, F 52, Shelikof sel_coff 8,
double-logistic base 8, fishery ascending devs 104, q devs ~102, index_log_q 6, mean rec 1).

### § Phase 2 estimation diagnosis — THE OPEN PROBLEM

Free estimation (`estimateMode=1`, `random_rec=FALSE`) converges but to a very different solution:
SSB max |rel diff| **1.12**, terminal SSB **+64%**, recruitment/F ~35% mean error. The starting
point (WHAM params) is exact in derived quantities, so this is an **objective-surface** problem,
not a mapping bug.

`jnll_comp` at the WHAM-mapped inits vs WHAM's components (this is the key clue):

| component | Rceattle @ inits | WHAM | note |
|---|---|---|---|
| Index (agg)         | 101.10 | 100.47 | ✓ close |
| Catch (agg)         | −107.81 | −107.83 | ✓ exact |
| Composition         | 1009.87 | 1005.35 (`catch_acomp`+`index_acomp`) | ✓ close (multinomial const) |
| Recruitment devs    | 109.31 | 94.21 (`nll_NAA`) | off ~15 |
| **Selectivity devs**| **760.95** | **−414.71** (`nll_sel`) | **off ~1176** |
| Catchability devs   | −202.02 | −187.81 (`nll_q`) | off ~14 |
| NonPar sel penalty  | 0.20 | 0 | Rceattle-only (`avg_sel`) |
| **TOTAL**           | **1671.6** | 979.3 (=489.67×2 conv.) | |

The dominant discrepancy is the **selectivity-deviate penalty**: Rceattle +761 vs WHAM −415, a
~1176 gap. WHAM's `nll_sel` is large-negative because it carries the Gaussian normalizing
constants; Rceattle's slot-5 penalty is structured/scaled differently. Because this term dominates
Rceattle's total objective, the minimizer trades away index/catch fit to shrink the selectivity
deviates, dragging the whole solution (and SSB scale) away from WHAM. The recruitment-dev (+15)
and q-dev (+14) offsets are the second-order contributors (year-1 rec penalty; logit-vs-log q).

**2026-07-17 — FREE ESTIMATION MEASURED (from WHAM's params, sel penalty relaxed). Two
independent drivers, now isolated. It does NOT converge to WHAM's solution:**

| fishery `sel_dev_log_sd` (exp) | SSB max\|rel\| | terminal SSB rel | R max\|rel\| |
|---|---|---|---|
| 0.1 (WHAM-scale nominal) | 0.934 | +0.438 | 0.538 |
| 0.744 (age-scale corrected) | 0.766 | +0.206 | 0.255 |
| 2.0 | 0.666 | +0.088 | 0.189 |
| ~unpenalized (1e4) | 0.670 | +0.087 | 0.184 |

- **Driver A (dominant, structural): initial recruitment scale / R0.** Even with the selectivity
  penalty ~off, early-year (1970–75) SSB is **~65% high** and terminal only ~9% — a uniform
  scale, not a shape error. VERIFIED: free Rceattle estimates `rec_pars` (R0) = **15.228** vs
  WHAM `mean_rec_pars` = **14.732**, ratio **1.642**, and the year-1 numbers at ages 2–10 are
  inflated by *exactly* 1.642×. Cause = the flagged N1 structural difference: WHAM pins ages 2+ at
  R0-equilibrium off a *single* free N1 (age-1) param, while Rceattle `initMode = 1` scales the
  whole initial age vector by a *freely estimated* R0. The sparse early data don't pin R0, so the
  two frameworks' recruitment penalties settle it at different values. **Not tunable** — needs the
  initial condition constrained to WHAM (map/fix `rec_pars` + early `rec_dev` to WHAM's, or add a
  WHAM-style single-parameter equilibrium-N1 option to the cpp). SSB shape correlation is 0.89.
- **Driver B (secondary, tunable): the selectivity-deviate penalty scale below.** Widening the
  fishery `sel_dev_log_sd` from 0.1 toward the age-scale-equivalent value cuts terminal SSB error
  44% → 9%, confirming the corrected diagnosis. NB the knob is the **parameter**
  `inits$sel_dev_log_sd[fishery]` (`= log SD`, already mapped off), NOT `Time_varying_sel_sd_prior`
  (which only seeds it at build time and has no effect when prebuilt `inits` are passed).

1. **Selectivity-deviate penalty (secondary) — 2026-07-14 diagnosis was WRONG; CORRECTED
   2026-07-17.** The earlier lead blamed `ceattle_v01_11.cpp:2875` (`dnorm(., 0, 4*sel_dev_sd)`
   on the ascending *slope*). **That is not the driver.** Decomposing the slot-5 = 760.95
   penalty at the WHAM-mapped inits (reproduces to 760.9478, exact):

   | slot-5 term | value |
   |---|---|
   | ascending **inflection** `dnorm(sel_inf_dev, 0, 0.1)` | **+819.05** |
   | ascending **slope** `dnorm(log_sel_slp_dev, 0, 4*0.1)` (the "4×" term) | +13.71 |
   | descending (deviates mapped to 0, constants only) | −71.81 |

   The `4×` slope term is only 13.7 of 761 — and patching it to `1×` would *raise* it to ~145,
   moving **away** from WHAM. The real cause is a **scale mismatch**: WHAM penalizes `selpars_re`
   on the **logit scale** of a bounded transform (lo=−10, hi=20) at SD 0.1; Rceattle penalizes
   `sel_inf_dev` on the **age scale** at SD 0.1. The Jacobian is `(hi−lo)·p·(1−p) ≈ 7.44`, so
   Rceattle's age-scale deviate (sd ≈ 0.591) is ~7.4× WHAM's logit-scale deviate (sd ≈ 0.0795) —
   penalized at the *same* 0.1 SD it is over-penalized ~55×. **WHAM's `nll_sel` is exactly
   `-sum(dnorm(selpars_re, 0, 0.1, log))` over all 312 terms (104 free + 208 mapped zeros) =
   −431.70 + 16.99 = −414.706** (verified to the digit). Fix options: (a) set the fishery
   `Time_varying_sel_sd_prior` to the age-scale-equivalent SD `0.1 * 7.44 ≈ 0.744` so the *free*
   optimum lands where WHAM's does (the constant offset in the objective is irrelevant to the
   argmin — only relative weighting matters); (b) reparameterize the cpp deviate onto the bounded
   logit scale to match WHAM structurally. **NOTE Phase 3 sidesteps this entirely** by pinning
   WHAM's parameters, so OSA agreement needs none of it.
2. Confirm `random_rec = FALSE` is the right choice (WHAM `input$random <- NULL` = penalized).
   Try `random_rec = TRUE` (Laplace) as a cross-check — with fixed SDs the mode should be similar.
3. The `avg_sel` NonParametric penalty (cpp:2780) has no WHAM analog. Small now, but under free
   estimation it actively pulls the Shelikof scale. Consider whether it can be down-weighted.
4. Only after the objective is reconciled should absolute agreement be judged — remember WHAM
   itself sits at 489.67 here, not 474 (multimodal), so aim for derived-quantity agreement.

### Files (native-parameterization work is in the repo, survives)

- `2021 pollock update data.R` — updated: native selectivity config (Shelikof NonPar+maxnorm
  +Bfs3, double-logistics, fishery IID, q RandomWalk) **+ survey comp Month = survey month**
  (2026-07-17 fix; see § below). Rebuild before running the bridge.
- `2021 pollock bridging.R` — Phase 1 (emp_sel, exact) + **Phase 2** (native params; fixed
  validation exact, estimation diverges) + **Phase 3** (OSA cross-check, EXACT). Runs top to bottom.
- `Data/2021pollock_wham.Rdata` — the 489.67 WHAM fit (reproduced by `Data/2021 Pollock WHAM.R`).

## § 2026-07-17 — Phase 3 OSA cross-check (all VERIFIED by running code)

Goal for this session: get an **exact** OSA-residual match to confirm Rceattle's one-step-ahead
residuals against WHAM's (the reference the comp decomposition was ported from). Outcome: **done,
machine precision** — n=1031 residuals, `r = 1.00000000`, max|diff| = 9.6e-13. Lives in
`2021 pollock bridging.R` **Phase 3**; mirrors `Rceattle/tests/comparison/WHAM-OSA-comparison.R`.

### Why exact is possible (and how)

Both models are **fixed-effects** (WHAM `input$random = NULL`; Rceattle `random_rec = FALSE`),
so observations are independent given the parameters and OSA residuals are **invariant to
conditioning order** — WHAM's `conditional=` sequencing and Rceattle's `subset` ordering give
the same answer. So the target is *exact equality*, not the growth template's "tight correlation".

**The pinning trick (no C++ change).** OSA needs a *real* objective (`estimateMode < 3`;
`osa_residuals()` rejects ≥3 because `jnll = dummy*dummy`). To hold every parameter at WHAM's
values while keeping a real objective: `estimateMode = 1` **and map every parameter to `NA`
except the inert `dummy`**. `dummy` never enters `jnll` when `estimateMode < 3`, so it has zero
gradient — the optimizer moves nothing and the fit sits exactly at WHAM's params. Verified: SSB /
catch / all indices reproduce WHAM to ~1e-16, `obj$fn()` = 1671.52 (a real number), 1 free param.

### The four conditions for exactness — each VERIFIED

1. **`bias_adjust_obs = 0`** (`fit_control`). Rceattle centers the lognormal index/catch at
   `log(hat) − bias_adjust_obs·σ²/2` (`cpp:2409,2445`); WHAM has `bias_correct_oe = 0`. With the
   default `1`, every aggregate OSA residual is offset by exactly `0.5·σ` (catch: 0.02498 =
   0.5×0.05). Setting `0` → aggregates match to 1e-14.
2. **`comp_offset = 0`** (`fit_control`). WHAM-style multinomial obsvec; the 1e-5 default would
   not reproduce WHAM's observation vector. Same as the growth template.
3. **Survey comp `Month` = survey month** (data-build fix). For empirical WAA (`growth_model = 0`)
   the survey age-comp reads its timing month from the **comp row** (`cpp:1948`,
   `mo = comp_n(comp_ind,0)`), NOT the fleet month. The old build set comp `Month = 0`, so the
   survey comp got **no** `exp(−mo/12·Z)` decay → age-1 (M=1.39, the steepest decay) was
   over-predicted (idx2 yr2001 age-1 0.4799 vs WHAM 0.3699). Setting comp `Month =
   fracyr_indices·12` → survey comps match to 1e-16. **The fishery comp is unaffected** (Baranov
   catch-at-age `F/Z·(1−e^−Z)·N` has no timing exponent), which is why it matched all along and
   isolated the bug to surveys.
4. **Shelikof q absorbs the max-normalization.** Rceattle max-normalizes the NonParametric block,
   but WHAM's age-specific Shelikof saturates at `max = 0.99999935`, not 1 — so normalizing
   inflates its selectivity by `1/max`. Scale is confounded with q, so set
   `index_q_dev[1,] = log(q_wham · max)` and index 1 matches to 1e-16 (was 6.5e-07). (This is
   also the source of the "Shelikof to 6.5e-07" residual in the Phase-2 fixed check.)

### WHAM side — rebuild, don't refit

The saved `fit` is a plain list (`opt/rep/parList/input`, **no `$obj`**), because
`Data/2021 Pollock WHAM.R` optimizes the TMB object directly (`fit_wham(do.fit=TRUE)` crashes).
So rebuild the obj from `fit$input` with `do.fit = FALSE`, `obj$fn(fit$opt$par)` to populate
`last.par`, set `obj$env$last.par.best <- obj$env$last.par`, and hand it to
`wham::make_osa_residuals()` with `model$is_sdrep <- TRUE` (sdreport is unnecessary for OSA and
fails here anyway — the saturating Shelikof gives a singular Hessian). No basin-hopping needed.

### Fleet / label / axis mapping (for the comparison merge)

- Rceattle fleet **7** = fishery = WHAM **`fleet_1`** (`catchpaa` / `logcatch`); Rceattle indices
  **1:6** = WHAM **`index_1:6`** (`indexpaa` / `logindex`).
- WHAM `year` is **1-indexed** → calendar = `year + (styr − 1)` = `year + 1969`.
- WHAM `bin` = age = Rceattle `age_length_bin`. WHAM keeps the last age (residual `NA`, sum-to-N);
  Rceattle drops it — so WHAM has 10 bins/comp-year vs Rceattle's 9. Merge on finite residuals.
- WHAM drops Shelikof age-1/age-2 comps (obs 1190→1134); its `index_1` comp is ages 3–10 only.

### What Phase 3 does NOT resolve

Phase 3 pins WHAM's parameters, so it needs neither the Phase-2 penalty reconciliation (lead #1,
now corrected above) nor a converged free fit. The final deliverable — the two models *converging
to the same solution from scratch* — still needs Phase 2's objective-weighting fix.

## Scratch artifacts (temp dir, will not survive)

Under `...\c037bd60-6758-4167-ac00-831e69beac94\scratchpad\`: `build_input.R` (the reusable
`build_pk_input()` builder), `fit_manual.R` (fit + checks), `explore_wham*.R`, `probe3.R`, `probe4.R`,
`check_data.R`, `check_dev.R`, `devlib/` (the compiled Rceattle dev). Everything needed has been folded
into `Data/2021 Pollock WHAM.R`; the rest is disposable.
