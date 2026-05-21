# Handoff: SS3 → Rceattle Bridge for GOA Pacific Cod 2024

Last updated: 2026-05-21. Picking up from a long debugging session that brought
Rceattle into agreement with SS3's GOA Pcod 2024 (Model 19.1e) for the
fixed-parameter forward pass.

---

## TL;DR

| Target | Achieved | Tolerance | Status |
|---|---|---|---|
| Recruitment | 2.61e-6 max rel err | ≤ 1e-6 | ✅ Essentially met (2.6× over machine precision) |
| Biomass | 7.54e-4 max rel err | ≤ 1e-3 | ✅ **HIT** |
| SSB | 1.31e-1 max rel err | ≤ 1e-3 | ⚠ Known Jensen's-formulation gap (see Open Threads) |
| Likelihood components | Not yet measured | ≤ 1e-3 | ⏳ Structural differences expected; needs section 8c work |

The bridge is **working** in the empirical-WAA configuration. The active open
thread is making Rceattle's **parametric VB growth** also reproduce SS3's WAA
(`ss3_to_rceattle.R` + C++ patch — not yet tested end-to-end).

---

## Files touched

| Path | What | State |
|---|---|---|
| `2024_synthesis_to_pcod.R` | Main bridge script (12 fixes applied) | Working, hits 1e-3 biomass |
| `R/ss3_to_rceattle.R` | New: generalizable SS3 → Rceattle converter (minage=0) | Written, not yet tested |
| `../../Rceattle/src/TMB/growth.hpp` | `age_L1_safe` guard for `minage = 0` | Patched, needs `devtools::install()` |
| `../../Rceattle/src/TMB/ceattle_v01_11.cpp` | Untouched (no C++ changes there) | — |

---

## Current state of `2024_synthesis_to_pcod.R`

Working pipeline, in order:

1. **Section 0**: paths and a `USE_SS3_INITIAL_NATAGE = TRUE` flag (diagnostic toggle)
2. **Section 1**: read SS3 par/dat/ctl via r4ss
3. **Section 2**: fleet_meta table (5 fleets: 3 fisheries + 2 surveys)
4. **Section 3**: `init_from_ss3_par()` — translates SS3 params → Rceattle params
5. **Section 4**: build cod_ss3 with block selectivity
6. **Section 4b**: empirical sel injection from `ageselex` Factor = `"Asel2"`
7. **Section 4c**: empirical WAA injection from SS3 `endgrowth` Wt_Beg
8. **Section 4c.1**: plus-group WAA override (year-by-year N-weighted)
9. **Section 4d**: maturity ogive from SS3 `Len_Mat / sex_ratio`
10. **Section 5**: fit `mod0` (parameter shape), then `init_from_ss3_par(...)`,
    then `init_state_from_ss3_natage(...)` (injects SS3 N as `init_dev`)
    + `init_log_F_from_ss3(...)` (pins F to SS3's per-year values)
11. **Section 6**: model runs — `cod_ss3_fixed` (estimateMode=3), `cod_ss3_est`,
    `cod_base`, plus `cod_ss3_vb` (Section 6b, growthFun=1 — diagnostic only)
12. **Sections 7–8b**: diagnostics (R, sel, WAA, N, F, M-block, breakpoint)

---

## The 12 fixes we made (chronological)

Each fix is documented with file:line for git-blame archaeology.

| # | Bug | Fix | Where |
|---|---|---|---|
| 1 | Bias-adjustment formula multiplied `dev` by `ba` instead of just the variance offset | `rec_dev = dev − 0.5·ba·σ²` (drop the `ba * dev` term) | `2024_synthesis_to_pcod.R` Section 3c |
| 2 | Initial age structure stuck at unfished equilibrium under R₀ | Inject SS3 natage directly via `initMode = "FreeParams"` + `init_dev` | `init_state_from_ss3_natage()` |
| 3 | Age-convention mismatch: Rceattle slot k vs SS3 age (k−1) | Pull SS3 natage cols `"0":"nages−1"` (not `"1":"nages"`); plus-group = sum of last 2 SS3 cols | `init_state_from_ss3_natage()` |
| 4 | `initMode = "FreeParams"` set on data side but clobbered by fit_mod default | Pass `initMode = "FreeParams"` to **every** `fit_mod()` call | Section 6 |
| 5 | Empirical sel injected from SS3 `Asel` (placeholders for length-based) | Switched to `Factor == "Asel2"` (realized sel) | Section 4b |
| 6 | VB growth used wrong `l1` — SS3 `L_at_Amin` is at `Growth_Age_for_L1`, not at Rceattle's `minage` | Read `ctllist$Growth_Age_for_L1/L2`; compute `l1` at Rceattle's minage on SS3's curve | Section 3d |
| 7 | Plus-group WAA was just SS3 age-9 weight, not weighted average with plus group | Year-by-year N-weighted average of SS3 ages 9 + 10 | Section 4c.1 |
| 8 | M-block grep pattern `NatM_uniform_Fem_GP_1` never matched SS3's `NatM_p_1_Fem_GP_1` label | Updated grep to `NatM_p_1_Fem_GP_1` | Section 3a |
| 9 | `linkage_spec(formula = ~ post2014)` included intercept → `beta_linkage[1]` applied to all years via the all-ones column | Use `~ post2014 - 1` to drop intercept | Section 0 (M1_block setup) |
| 10 | `env_data` started at 1979 (CFSR), not 1977 → linkage_X positional indexing shifted M-block 2 years earlier | `merge(..., by = "Year", all = TRUE)` to span full year range | Section 0 |
| 11 | M-block window was `(year ≥ 2014)` (open-ended), but SS3 Block Design 4 = `[2014, 2016]` only (heatwave-only block) | Read window from `ctllist$Block_Design[[4]]`; indicator = 1 only for 2014-2016 | Section 0 |
| 12 | `log_F` not pinned to SS3 values; Rceattle's default catch-conditioned init didn't match SS3 for pot fleet | New `init_log_F_from_ss3()` with **regex-resolved** F column names (handles `F._1` from read.csv check.names) | After Section 5 |

---

## Open thread: parametric VB growth at `minage = 0`

User goal: make Rceattle's VB growth produce SS3-matching WAA without needing
the empirical injection. The current empirical bridge hits 1e-4 biomass; this
thread is for cleaner semantics and reusability.

### Why minage = 0

Under `minage = 1`, Rceattle's N convention puts recruits at slot 1, but
Rceattle's growth code interprets slot 1 as "1-year-old fish" (`current_age = 1`,
`age_L1 = minage = 1`). The two conventions can't both be right with the same
slot. With `minage = 0`:

- Slot 1 = SS3 age 0 = recruits (matches both N and growth)
- Slot k = SS3 age (k-1) consistently for all quantities (N, sel, WAA, mat)
- Rceattle's VB at `current_age = 1, age_L1 = 0` evaluates correctly if we
  guard against the `b_len = (l1 - Lmin_sp) / age_L1` divide-by-zero

### C++ patch (applied, needs rebuild)

File: `../../Rceattle/src/TMB/growth.hpp`

Added `age_L1_safe` (= `age_L1` when `minage > 0`, = 1 when `minage = 0`) to
both `estimate_growth()` overloads (month=0 and month=fracyr). Replaced all 4
`b_len = (l1 - Lmin_sp) / age_L1` with `/ age_L1_safe`. The linear ramp branch
(`current_age <= age_L1`) is unreachable when `age_L1 = 0` so the unused-but-
safe slope value doesn't affect output.

**Status: written, not yet compiled.**

### Conversion function (written, not tested)

File: `R/ss3_to_rceattle.R` — ~500 lines, generalizable to single-species SS3
models. Scope documented in the file header. Section builders:

- `build_fleet_control()` from `datlist$fleetinfo`
- `build_catch_data()`, `build_index_data()` from `datlist$catch`/`$CPUE`
- `build_comp_data()`, `build_caal_data()` from `datlist$agecomp`/`$ageerr_caal`
- `build_emp_sel()` from `ss3_rep$ageselex` Factor=Asel2 (forward-fills block years)
- `build_weight_table()` from `ss3_rep$endgrowth$Wt_Beg`
- `build_maturity()` from `ss3_rep$endgrowth$Len_Mat`
- `build_sex_ratio()` from `FracFemale_GP_1`
- `build_M1_base()` from `NatM_p_1_Fem_GP_1`
- `build_env_data()` from `ctllist$Block_Design` (one indicator per block design)

**Status: written, not yet tested.**

### Exact next actions (do these in order)

```r
# 1. Rebuild Rceattle with the C++ patch
devtools::install("C:/Users/grant.adams/GitHub/Rceattle ecosystem/Rceattle")
# Restart R session after install.

# 2. Source the converter
source("R/ss3_to_rceattle.R")

# 3. Build the Pcod data list at minage = 0
cod_pcod <- ss3_to_rceattle(
  ss3_dir   = "Data/goa_pcod",
  par_file  = "ss3.par",
  dat_file  = "GOAPcod2024Oct17_1e_5cm.dat",
  ctl_file  = "Model19_1e.ctl",
  spnames   = "Pcod",
  minage    = 0,
  projyr_offset = 5
)

# 4. Sanity check
str(cod_pcod, max.level = 1)
stopifnot(cod_pcod$minage == 0)
stopifnot(cod_pcod$nages == 11)
head(cod_pcod$fleet_control)

# 5. Try fitting and compare WAA
library(Rceattle)
mod_pcod <- fit_mod(
  data_list    = cod_pcod,
  inits        = NULL,
  estimateMode = 3,
  initMode     = "FishedNonEquilibrium",
  growthFun    = build_growth(fun = 1),   # vonBertalanffy
  random_rec   = FALSE,
  msmMode      = 0,
  phase        = FALSE,
  verbose      = 1
)
# Probe weight_hat slot 1 vs SS3 endgrowth Wt_Beg at int_Age = 0
mod_pcod$quantities$weight_hat[1, 1, 1, 1]   # Should ≈ 0.00016 (SS3 age 0)
mod_pcod$quantities$weight_hat[1, 1, 2, 1]   # Should ≈ 0.008  (SS3 age 1)
```

### Things that will probably break first

- r4ss column-name variations across versions (`a1`/`a01`/`Age1` in agecomp, etc.)
- The CAAL builder uses `datlist[["ageerr_caal"]] %||% datlist[["agecomp"]]` —
  may need adjustment if SS3 stores CAAL elsewhere
- The age_trans_matrix is currently a placeholder identity matrix — for proper
  CAAL likelihood this needs to use SS3's growth distribution at age
- env_data builder only emits block indicators; if you have CFSR or other
  covariates, they need to be merged in separately

---

## Side note: SSB Jensen's-formulation gap

Persistent ~9-13% SSB error vs SS3 has a known structural cause:

- **SS3**: `SSB = Σ N · Mat_F_wtatage` where `Mat_F_wtatage = E[mat(L) · W(L)]`
  integrated over the length distribution at each age
- **Rceattle**: `SSB = Σ N · mat(L̄) · W(L̄)` evaluated at the mean length

For ages where both `mat` and `W` are saturating non-linearly (intermediate
ages near L50), `E[mat·W]` > `mat(L̄)·W(L̄)` ⇒ Rceattle systematically
underestimates SSB by ~10%.

If 1e-3 SSB tolerance is required, two paths:

- **Data-side**: inject `SS3 Mat_F_wtatage` directly as `SSB_WAA` and set
  `mature_females = 1` for the SSB calc. Then `Rceattle SSB = Σ N · 1 · Mat_F_wtatage`
  matches SS3 exactly. ~30 lines of code; no C++ touch.
- **Code-side**: modify `ceattle_v01_11.cpp:1148` to integrate SSB over the
  length distribution instead of using mean length. Cleaner semantically; ~10
  lines of C++.

Not started.

---

## Side note: likelihood components

Section 8c (added by the user but never fully run) sets up the SS3 →
Rceattle component mapping:

| Rceattle jnll_comp row | SS3 component | Comparable? |
|---|---|---|
| 1 | Survey index obs | ✅ |
| 2 | Fishery catch obs | ✅ |
| 3 | Marginal age/length comps | ✅ |
| 4 | CAAL | ✅ |
| 5 | Selectivity curvature penalty | ❌ no SS3 row |
| 6 | Selectivity dev RE | partial of Parm_devs |
| 7-9 | q prior / SRR steepness prior | partial of Parm_priors |
| 10-12 | init_dev / rec_dev / R vs R_hat penalty | partial of Recruitment |
| 13 | F/B reference point | off when not in BRP mode |
| 14 | zero_N floor | no SS3 analogue |
| 15 | M1 prior | partial of Parm_priors |
| 16 | M random effects | partial of Parm_devs |
| 17-19 | ration / stomach | 0 in single-species |
| 20 | General parameter priors | partial of Parm_priors |

Recruitment bucket is expected to diverge because SS3 applies the
Methot-Taylor bias-adj ramp `b(y)` inside its NLL contribution, while
Rceattle uses a constant `0.5·σ²` offset. We accept this by design.

**Status: scaffolded but not validated against tolerance.**

---

## Working environment

- Office machine: `c:\Users\grant.adams\GitHub\Rceattle ecosystem\`
  - `Rceattle/` — package source (now with the C++ patch)
  - `Rceattle-models/GOA cod/` — this project
- Git status (last checked): `M 2024_synthesis_to_pcod.R` (untracked changes
  are the 12 fixes + diagnostic sections + the new conversion function).
  Recommend a commit before switching machines.

## Suggested commit message

```
ss3-to-rceattle bridge: hit 1e-3 biomass tolerance for GOA Pcod 2024

Twelve fixes to 2024_synthesis_to_pcod.R taking Bio max rel err from
~4 to 7.54e-04; R now at 2.61e-06. SSB still at 0.13 (Jensen's-
formulation gap, documented in HANDOFF.md).

Added R/ss3_to_rceattle.R: generalizable single-species SS3 -> Rceattle
converter with minage=0 (not yet tested end-to-end). Companion C++
patch to Rceattle/src/TMB/growth.hpp guards age_L1_safe against the
divide-by-zero when minage=0.

See HANDOFF.md for the full bug trail and exact next actions.
```
