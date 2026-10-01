# Handoff: the SS3 → Rceattle bridge

State as of **2026-09-30**. Two Claude sessions are working this at once; see
*Editing this file* at the bottom before you change it.

## Where the truth lives

Measurements, not prose, settle questions here:

- `GOA-estimation-parity.md` — the current GOA cod accounting, per component and per
  fleet, and the `Still open` list. **Authoritative.**
- `GOA-remaining-gap.md` — the earlier split the parity doc reproduces.
- `CAAL-length-bin-defect.md` — the SS3 data-file defect (not an Rceattle bug).
- `PLAN.md` — phases 0–5.
- `../GOA cod/Bridging/HANDOFF_estimation_parity.md` — **superseded**, a May/June
  snapshot. It cites `ceattle_v01_11.cpp` (now `ceattle.cpp`), reports "TOTAL NLL
  +539 vs SS3", and names "Pope's vs Baranov" as the open blocker. Read it for
  history only.

## State

GOA cod forward pass at SS3's MLE: **1998.9663** with the LLSrv environmental link on,
**2008.9309** with it off. SS3 `TOTAL` is 2051.98.

**Do not compare per-fleet index likelihoods to SS3's directly.** They differ by a
lognormal constant — Srv is off by 15.78 where the parity doc records a residual of
−0.00006. Only on/off differences within Rceattle are interpretable.

The two dominant residuals in that accounting:

| residual | size | status |
|---|---|---|
| Index, LLSrv | +9.9645 | **closed** 2026-09-30, measured 9.96459 |
| Recruitment | +49.7485 | **attributed** to the `init_dev` penalty, not closed |

Everything else at the MLE is small (catch +0.080 at SS3's L1).

## In flight

### Package: PR #181, `link = "exponential"` — DONE, awaiting merge

Head `267a951d` on `feat/linkage-power-link`, 13 commits. Suite **10,057 / 0** with
golden **21 / 0** on a stable tree. This is SS3's environmental link type 1 and it is
what closed the index residual.

Left to do, in order:

1. Merge to `dev`.
2. **Golden on the MERGED tree, not once per side.** This branch changes the `index_q`
   computation (an AD-tape change); `feat/srr-init-level` changes `build_params`,
   `build_map` and `build_parameter_bounds`, which every model goes through. Neither
   suite proves the combination.
3. `NEWS.md` and the `DESCRIPTION` version line are the real merge conflict — both
   branches insert above `# Rceattle 5.46.0`. The code hunks are far apart
   (`linkage.hpp:72-78` vs `:40`; `LINKAGE_LINK_CODES` ~10 lines from
   `.is_init_linkage_row()`). Settle 5.47.0 / 5.48.0 before the rebase, not during.
4. `/pkgdown-check` has not been run; `pkgdown.yml` triggers on `main` only, so a PR
   to `dev` gets no pkgdown CI.

### Package: `feat/srr-init-level`, recruitment `init` — owned by the other session

<!-- OWNED BY feat/srr-init-level. Update this section in place. -->

5.48.0. Adds `init` as a fourth recruitment linkage parameter (code 3), a log-scale
multiplier on the initial age structure read at year 0 only. Uncommitted pending a full
suite. Two things that matter to anyone merging across it:

- `RCEATTLE_N_REC_PARAMS` goes 3 → 4 in `linkage.hpp`; it dimensions
  `recruitment_linkage_offset`.
- The shared-intercept machinery changed. `build_map`, `build_params` and
  `build_parameter_bounds` all assumed an intercept row has a base parameter to carry
  the level; `init` has none, so `~ 1` estimated and moved nothing while every builder
  reported success. All four sites now go through `.is_init_linkage_row()`
  (`R/0-linkage_encode.R`) — that predicate has to stay in the condition.

This is the mechanism for the Recruitment residual above.

**Measured** (`SS3-bridge/regime_in_finit.R`, forward pass, GOA cod 2024). Carrying the
level outside `init_dev` drops the objective **49.9133 nats**, all of it in
`Initial abundance deviates` (54.97530 → 5.06184). Index, catch, length composition and
CAAL move by at most 1.9e-04; the recruitment deviates not at all. The Recruitment
residual closes from **+49.7485 to −0.165**, so there is no difference in the initial
state between the two models — only a penalty SS3 does not charge.

That 54.97530 is the same number `GOA-estimation-parity.md`'s decomposition reports as
54.98 at SS3's MLE, reached from the other direction, which is the cross-check that the
two accountings agree.

**Why it is ranked first below.** The optimiser retires the `init_dev` penalty by
lowering R0 (0.37 log units, penalty 54.98 → 17.72), and SB0, B40% and depletion all
scale with R0. So the defect does not just cost nats, it biases the reference points
that set the quota. `init` removes the reason to lower R0 rather than correcting R0
afterwards.

**Refusals**, each closing a configuration that would otherwise be estimated and inert
or unidentified: a link other than `log`; more than one design column (only year 0 is
read, so two coefficients share one number and a per-year RE estimates deviates no year
but the first reads); `initMode = "FreeParams"` (never reads `R_init`);
`initMode = "OffsetEquilibrium"` (already scales the same ages by `rec_dev[, 1]`).

**Not yet done:** the GOA refit with `init` active. A forward pass cannot show it,
because the whole effect is on where the optimum sits. The prediction to test is that
fitted `log(R0)` moves from 12.3568 toward SS3's 12.7234 and the `init_dev` penalty
stops paying for the regime. Needs no special checkout — PR #178 put `cod-bridge` in
`dev`, so this branch already carries `DoubleNormalSS3`.

### Rceattle-models: uncommitted, for review

- `GOA cod/Bridging/ss3_to_ceattle_forward_pass.R` — the LLSrv environmental link,
  behind `RCE_Q_ENV` (default on) so before/after stays measurable.
- `SS3-bridge/GOA-estimation-parity.md` — the closure, with the numbers.

## Still open, ranked by whether it moves a quota number

1. **`log(R0)` is 31% low, and it is a BRIDGE artifact.** The bridge folds SS3's
   unpenalised `SR_regime_BLK5add_1976` into `init_dev` and then charges it the deviate
   penalty, so the optimiser retires it by lowering R0 (54.98 → 17.72). SB0, B40% and
   depletion all scale with R0. `init` targets exactly this.
2. **Rceattle has no equilibrium-catch concept** anywhere in `R/` or `src/TMB/`. SS3's
   `Equil_catch` is 0.0035 nats but reaches +6.6 on the growth gradient; it is what
   flattens the `Finit`/`init_dev` ridge. Structural, not a converter setting.
3. **Rceattle CAAL carries no month.** Every CAAL row is predicted at `flt_month(flt)`.
4. **The M-block prior does not map.** SS3 puts `Log_Norm(−0.81, 0.41)` on the block's M
   *value*; Rceattle's parameter is `log(M_block / M_base)`.
5. **The 866 unpenalised selectivity offsets.** A like-for-like free-selectivity
   comparison needs the devs as random effects with SS3's `dev_se`.
6. **`∂catch/∂L1` disagrees by up to 4.07 nats.** Too small to move L1 here (~0.001 cm),
   but a real structural difference that will matter where L1 is not ~1.3 cm.

Scope: this is GOA cod with AI cod alongside. See
`Generalizing_to_other_SS3_models.md` — bridged for cod is not bridged for SS3.

## How to run it

From `Rceattle-models/GOA cod`:

```sh
export RCEATTLE_PKG=<a worktree carrying the link>   # else ../../Rceattle
Rscript Bridging/ss3_to_ceattle_forward_pass.R        # link on
RCE_Q_ENV=false Rscript Bridging/ss3_to_ceattle_forward_pass.R   # baseline
```

`cod-bridge` is merged into `dev` (PR #178), so **any** branch off `dev` already carries
`DoubleNormalSS3` and `dn_peak` — no special checkout is needed. Confirm with
`git merge-base --is-ancestor a56881d9 HEAD`.

Three traps worth not rediscovering:

- `env-var 101` on `LnQ_base_LLSrv(5)` is link type 1 on environmental variable 1. The
  series is `datlist$envdat` (r4ss: `year` / `variable` / `value`), 1979–2024.
- **r4ss labels the coefficient `_ENV_add` while `ss_summary.sso` calls it `_ENV_mult`**,
  and SS3's type 1 is multiplicative — r4ss's label contradicts the direction. Match both
  spellings if you grep a label. The value is 0.517737 either way.
- `env_data` spans `styr:projyr`, so **seven** model years need the covariate filled
  (1977–78 and 2025–29), not two. The fill is mandatory — the linkage refuses an NA
  fixed-effect covariate — and 0 is harmless, because LLSrv has no observation before 1990.

## Editing this file

Two sessions update this. **Edit only your own section, additively.** On 2026-09-30 a
wholesale rewrite of a shared memory file dropped five hazards another session had
recorded while its summary still claimed to cover them. Re-read before writing, and do
not reflow sections you do not own.
