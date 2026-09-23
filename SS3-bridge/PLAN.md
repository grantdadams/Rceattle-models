# Plan: exact SS3 → Rceattle bridge for AI and GOA Pacific cod

## Context

AI cod (`Rceattle-models/AI cod - Dev`) and GOA cod (`Rceattle-models/GOA cod`) both have
partial SS3 bridges.

- **AI forward pass:** SSB is off by about 6% and the CAAL NLL by about 700.
- **AI estimate:** SSB is off by about 20%, with growth fixed.
- **GOA forward pass:** NLL is +370.
- **GOA estimate:** NLL is +1952, and all Hessian rows are NaN.

Both bridges depend on `origin/dev-cod-bridge`, which is 6 commits ahead of `dev` and 798
behind, and edits files `dev` has since renamed.

**Goal:** from a cold start, Rceattle converges to the *same solution* as an SS3 reference run
for each stock.

**Decisions made:**
- Targets are SS3 runs adjusted toward Rceattle for *estimation-method* choices: F method,
  bias ramp and initial-equilibrium penalty. Biology, selectivity and likelihood features are
  implemented in Rceattle.
- AI target: `AI cod - Dev/Data/M24_1`.
- GOA target: `GOA cod/Data/goa_pcod-no init and ramp`.
- Work on a new branch off `dev`. Port the useful pieces of `dev-cod-bridge` by hand through
  the schema and switch system; keep the old branch as a read-only reference.
- The plan covers the full scope in phases, growth first.

**Facts checked on `dev`:**
- Catch is Baranov with F as parameters (`ceattle.cpp:2720`). This is SS3 `F_Method 2`, so the
  GOA notes' "Pope's is the blocker" is stale. The notes also have SS3's F_Method numbering
  backwards.
- Richards (`growth.hpp:171`) is algebraically SS3's Richards with A2 = 999. Only the shape
  parameter is on a log scale, and M24_1's shape (0.398) is positive, so no change is needed.
- The SD of length-at-age is linear in length, constant below `age_L1` (`growth.hpp:219-232`).
  That is SS3 `CV_Growth_Pattern 2`, which GOA uses. AI uses pattern 0 (CV as a function of
  length-at-age), which is missing.
- The age-length key and weight-at-age are built on the *data* length bins (`growth.hpp:244-275`).
  SS3 builds them on the 1-cm population bins.
- Maturity is at age only (`ceattle.cpp:1917`). SS3 uses maturity at length, integrated over
  length, which causes the 5–8% SSB Jensen gap.
- The double normal has 4 parameters (`selectivity.hpp:548`). SS3 pattern 24 has 6.
- Rceattle's `rec_dev ~ N(-σ²/2, σ)` with R = R̂·e^dev is SS3 with `max_bias_adj = -1` under
  the shift dev_R = dev_SS3 − σ²/2. That is a parameter mapping, not a model change.

## Phase 0: set up (about 1 day)

1. Create branch `cod-bridge` off `dev`.
2. Build the adjusted AI run from `Data/M24_1`: `F_Method 2`, `max_bias_adj -1`, everything
   else unchanged. Run it with `GOA cod/Data/goa_pcod/ss3.exe` (3.30.22.1) after checking
   M24_1's SS3 version.
3. Re-confirm that the GOA adjusted run is `F_Method 2`, has `max_bias_adj -1`, and has
   InitEQ lambda 0.
4. Consolidate the two diverged copies of `ss3_to_rceattle.R` into one shared file in
   `Rceattle-models`. Fix the hard-coded Mac `setwd()` and the broken `source()` paths.
5. Write `parity_check.R` per stock. It runs three gates and is the acceptance test for every
   phase:
   - **G1, forward state:** load the SS3 MLE into Rceattle, then compare length-at-age,
     SD-at-age, the age-length key, weight-at-age, fecundity, selectivity, N-at-age, SSB and R
     elementwise.
   - **G2, likelihood at the SS3 MLE:** compare NLL components, allowing only documented
     constants, and check max|gradient|.
   - **G3, cold start:** fit from Rceattle defaults and compare objective, parameters, SSB and R.

## Phase 1: growth and biology (about 5–7 days)

All Phase 1 changes are new options, and the current defaults are unchanged.

| # | Change | Needed by | Where |
|---|---|---|---|
| 1a | Population length grid: `build_growth(minlength, lengthbin, maxlength)`. The age-length key and weight-at-length are computed on 1-cm population bins, then summed into the data bins. Port from `dev-cod-bridge` (`lengths_pop`, `growth_matrix_pop`). Use SS3's bin edges and its first/last-bin pnorm treatment. | both | `R/0-build_growth.R`, `growth.hpp`, `ceattle.cpp`, data rearrange |
| 1b | SD-of-length-at-age forms keyed to SS3 `CV_Growth_Pattern`: add pattern 0 (CV linear in length-at-age, SD = CV·L, including below A1). The existing behaviour is pattern 2. Extend `.GROWTH_SD_STYLE` through the schema. | AI | `R/0-build_growth.R:28`, `growth.hpp:215-239`, `R/0-column_schema.R` |
| 1c | Maturity at length: logistic L50 and slope per species. Fecundity-at-age = Σ_l ALK(l\|a, spawn time)·mat(l)·W(l). This is SS3's calculation and closes the Jensen gap. | both | new species inputs via `/new-column`, `growth.hpp`, SSB at `ceattle.cpp:1917,2005` and projection/SPR paths |
| 1d | Verify, and match if needed: SS3's length below A1 (the ramp from the first population bin), the plus-group mean length (`growth.hpp:202-213` against SS3), and the within-year timing of weight for catch, survey and spawning (SS3 month 7 = Rceattle Month 6; spawn month 1 = 0). | both | `growth.hpp`, converter |

**Gate:** G1 matches length-at-age, SD, the age-length key, weight-at-age, fecundity and SSB to
≤1e-6 relative for both stocks.

## Phase 2: selectivity (about 4–5 days)

| # | Change | Needed by |
|---|---|---|
| 2a | SS3 pattern-24 six-parameter double normal as a **new** selectivity code. The 4-parameter `DoubleNormal` stays unchanged. Support fixed slots through the map, replacing GOA's `apply_ss3_sel_phase_fixes()` patch. | both |
| 2b | Age × length combined selectivity (SS3 age pattern 10 / `age_first_selected`). Port from the branch. | GOA |
| 2c | SS3 three-tier time variation: base, then block *replacement*, then annual devs with `dev_link 1` (P·exp(dev·se), dev ~ N(0,1), fixed se). Check whether `dev`'s `Block`/`IID` `Time_varying_sel` covers the first two tiers before porting `BlockDev`. | GOA |

**Gate:** G1 matches selectivity-at-age for every fleet and year to ≤1e-6.

## Phase 3: data and likelihood (about 3–4 days)

| # | Change | Needed by |
|---|---|---|
| 3a | `Comp_addtocomp` / `CAAL_addtocomp` (SS3's add-to-comp). This changes the solution. Port through `/new-column`. | both |
| 3b | Multiple ageing-error definitions: a per-row key on comp and CAAL data (GOA Srv uses def 1). The converter builds each matrix from SS3's age-error mean and SD vectors exactly as SS3 does. | GOA, and cleaner for AI |
| 3c | SS3 multinomial NLL constant (deviance form) as a *reported* offset. It does not move the solution but is needed for G2 per component. Decide at implementation whether to port `SS3Robust` or only report the constant. | both |

**Gate:** G2 components match up to listed constants, and max|gradient| at the SS3 MLE is ≤1e-3.

## Phase 4: mortality, catchability, initial state (about 2–3 days)

| # | Change | Needed by |
|---|---|---|
| 4a | M block with its prior on the **absolute** block M (SS3), not on the offset. Check the natural-scale linkage prior (`fam 2`, `ceattle.cpp:4921`) first; add only if missing. | both (GOA prior) |
| 4b | LLSrv environmental q (`env_var&link 101`): confirm that a q linkage reproduces P·exp(β·env) exactly. | GOA |
| 4c | Initial age structure: map SS3's InitF equilibrium plus early recdevs (AI 1978–90, GOA from 1967) onto an Rceattle `initMode`. Add a mode only if none is exact. | both |

**Gate:** G3 cold start. Objective equal to ≤1e-3, parameters to ≤1e-4 relative, SSB and R to
≤1e-4, and a PD Hessian for both stocks.

## Phase 5: finish (about 2 days)

1. Run `/golden-check`; every new option defaults off, so the four reference models must be
   unchanged.
2. Run `/verify` for the simulation, refit and plotting harnesses affected.
3. Add a `test-growth-*` / `test-selectivity-*` test per new option. Run
   `TESTTHAT_PARALLEL=false` after `load_all`.
4. Update `NEWS.md`, the `DESCRIPTION` minor version, `vignettes/stock-synthesis-conversion.Rmd`
   (replacing the hake-only and empirical-growth caveats) and `growth-estimation.Rmd`. Run
   `/doc-sync` and `/pkgdown-check`.
5. Rewrite both bridging READMEs with the final parity tables. Delete `tmp_*.R` and the GOA
   leftovers in the AI folder. Add the cod stocks to `inst/dev/SIBLING-REPOS.md` and record
   `dev-cod-bridge` as superseded in `SESSION_HANDOFF.md`.

## Rules that apply throughout

- Every new switch code, default and unit gets confirmed with the user before it enters
  `R/0-column_schema.R` (hard rule 9). Proposed names follow SS3's own option names.
- New columns go in through `/new-column` and must round-trip `write_data()`/`read_data()`.
- Each C++ change is followed by `load_all`, the stock gate and `/golden-check` before the next
  change.
- Commits are plain, with the numbers that moved in the body.

## Known risks

- G3 can fail on optimizer behaviour even when G2 has zero gradient at the SS3 MLE. In that case,
  compare Hessians before changing model code.
- The GOA recdev start is 1967 against `styr` 1977. `initMode` may not have an exact analogue
  (4c); that is the likeliest place a new mode is needed.
