# Handoff — Bridging the 2021 GOA pollock WHAM model into Rceattle

Status as of 2026-07-14. Everything below marked **VERIFIED** was confirmed by running code, not inferred.

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

## Scratch artifacts (temp dir, will not survive)

Under `...\c037bd60-6758-4167-ac00-831e69beac94\scratchpad\`: `build_input.R` (the reusable
`build_pk_input()` builder), `fit_manual.R` (fit + checks), `explore_wham*.R`, `probe3.R`, `probe4.R`,
`check_data.R`, `check_dev.R`, `devlib/` (the compiled Rceattle dev). Everything needed has been folded
into `Data/2021 Pollock WHAM.R`; the rest is disposable.
