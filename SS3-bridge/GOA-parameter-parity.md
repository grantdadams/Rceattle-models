# GOA Pacific cod: estimating the same parameters SS3 does

**Status:** the target is enumerated and verified; the work to reach it is identified.
Read with `GOA-estimation-parity.md`, which measures how far the two optima sit apart, and
`HANDOFF.md` for branch state.

## Which SS3 model, because there are three and they disagree

This matters more than it looks, and an earlier version of this note got it wrong by mixing
them. `Active_count` from each `Report.sso`:

| directory | `Active_count` | `SR_regime_BLK5add_1976` |
|---|---|---|
| `Data/goa_pcod` (pristine assessment) | **196** | -0.678228 |
| `Data/goa_pcod_caal_bins_fixed` (**what the bridge reads**) | **330** | -1.3879 |
| `Data/goa_pcod_caal_bins_1cm` | 330 | -1.39409 |

**The bridge targets 330**, because `ss3_to_ceattle_forward_pass.R:80` defaults `RCE_SS3_DIR` to
`Data/goa_pcod_caal_bins_fixed`. The 134-parameter difference from the pristine model is exactly
its 134 annual `F_fleet_N_YR_...` parameters: the bridge variant runs `F_Method = 2`, which makes
F explicit, where the pristine model's `F_Method = 3` solves for it. 196 + 134 = 330.

So quote 330 for the bridge and 196 for the assessment, and never mix a value read from one
directory with a count read from another.

## SS3's 330, decomposed

From the `PARAMETERS` table of `goa_pcod_caal_bins_fixed/Report.sso`, rows with a numeric
`Active_Cnt` (387 rows, 330 active):

| kind | n |
|---|---|
| annual F (`F_fleet_N_YR_YYYY_s_1`) | **134** |
| selectivity `DEVmult` (penalised, fixed sd 0.2) | **63** |
| `Main_RecrDev_1978..2024` | **47** |
| selectivity block replacements | **40** |
| base singles (M, 5 growth, SR_LN(R0), 2 log q, 23 selectivity base) | **32** |
| `Early_InitAge_1..10` | **10** |
| `NatM..._BLK4repl_2014` and `SR_regime_BLK5add_1976` | **2** |
| `Early_RecrDev_1977` | **1** |
| `LnQ_base_LLSrv(5)_ENV_mult` | **1** |

134 + 63 + 47 + 40 + 32 + 10 + 2 + 1 + 1 = 330.

## Thirteen of the 330 are not identified in SS3 either

`Size_DblN_descend_se_FshTrawl(1)_DEVmult_1977..1989` are estimated at **~2e-07** with a reported
sd of **0**, and the realized `descend_se` shows no dev-year variation at all. They are held at
zero by the sd-0.2 penalty with no data pulling them. The other four dev groups are real:

| dev group | n | max abs | mean abs |
|---|---|---|---|
| `peak` FshLL(2) | 12 | 1.409 | 0.513 |
| `ascend_se` FshLL(2) | 12 | 0.967 | 0.389 |
| `peak` FshTrawl(1) | 13 | 0.718 | 0.302 |
| `ascend_se` FshTrawl(1) | 13 | 0.814 | 0.318 |
| **`descend_se` FshTrawl(1)** | **13** | **6.7e-07** | **2.0e-07** |

So matching the count means estimating 13 coefficients that do nothing. That reproduces SS3's
parameter set faithfully and reproduces its flat directions with it, which is a risk to
`pdHess`. Decide it deliberately and say which was done; do not let it be an accident.

## Block year ranges, from the control file

`Model19_1e.ctl` lines 19-26: 6 patterns, `2 4 1 1 1 1` blocks each.

| pattern | blocks | used by |
|---|---|---|
| 1 | 1996-2005, 2006-2024 | `Srv(4)` |
| 2 | 1990-2004, 2005-2006, 2007-2016, 2017-2024 | `FshTrawl(1)`, `FshLL(2)` |
| 3 | 2017-2024 | `FshPot(3)` |
| 4 | **2014-2016** | `NatM` -- a three-year window, not 2014 onwards |
| 5 | **1976-1976** | `SR_regime` -- one year, at `styr - 1` |
| 6 | 1976-2006 | |

Pattern 5 confirms the regime is a single year before the hindcast, which is why no linkage can
address it directly (`.trim_env_data()` drops pre-styr rows) and why it is expressed as an
initial-level parameter instead.

Not every block present is active: `SizeSel_P_1_FshTrawl(1)_BLK2repl_2017` is phase **-1**, so
`peak` FshTrawl has 3 active blocks where the other pattern-2 parameters have 4. The 40 is a
count of ACTIVE blocks.

Per fleet and parameter, the control file also sets `dev_se` at phase **-5** fixed at **0.2** and
`dev_autocorr` at phase **-6** fixed at **0**. So the deviations are penalised deviates with a
fixed sd and no autocorrelation: in Rceattle an identity-link `(1 | Year)` with
`integrate = FALSE` and the sd supplied, not a free offset and not an integrated random effect.

## What matches already, and why that was worth checking

Two counts look like mismatches and are not. Both were checked rather than assumed, and both
dissolved:

- **Recruitment deviations match at 48.** `Main_RecrDev` runs 1978-2024, which is 47, but
  `Early_RecrDev_1977` is a separate ACTIVE single. Rceattle estimates `rec_dev` across
  1977-2024 = 48. Equal. (Both counts are the same in all three directories.)
- **Initial age deviations match at 10.** `#_Nages = 10` and `Report.sso`'s `NUMBERS_AT_AGE`
  columns run 0-10, so the population has **11** ages and Rceattle's `nages = 11` under the
  converter's `minage = 0`. `init_dev` is estimated over columns `1:(nages - 1)` = 10, against
  `Early_InitAge_1..10`. Equal.

So the gap is confined to selectivity time-variation plus two single parameters.

## The gap: 106 parameters

| missing in Rceattle | n | how it is expressed | state |
|---|---|---|---|
| selectivity block replacements | 41 | identity-link linkage, **one column per block YEAR RANGE** | converter work |
| selectivity `DEVmult`s | 63 | identity-link per-year columns, **penalised**, fixed sd | converter work |
| `LnQ_base_LLSrv(5)_ENV_mult` | 1 | `link = "exponential"` on q | PR #181 |
| `SR_regime_BLK5add_1976` | 1 | recruitment `init` linkage | `feat/srr-init-level` |

90 + 106 = 196.

## Why neither current G3 variant is an equal-footing comparison

`run_g3_goa.R` has two modes and neither estimates SS3's set:

- **`fixsel`** holds every selectivity offset at SS3's value, so Rceattle estimates about **90** —
  104 fewer than SS3, and the question "given SS3's selectivity, does Rceattle find SS3's growth,
  M and recruitment" is a different question from parity.
- **`freesel`** frees **867** coefficients. That is not SS3's 104 either, and the 867 carry **no
  penalty** where SS3 penalises its 63 devs (`Parm_devs = 6.49` at its own MLE), so it fits a
  strictly looser model than SS3 ever did and a lower objective means nothing.

The 867 is the symptom of the representation, not of a choice. The bridge builds one linkage
design column **per varying year** (`ss3_to_ceattle_forward_pass.R`, the `PAR_LINK` loop), which
reproduces SS3's realized selectivity exactly -- that is why the forward pass matches -- but it
conflates SS3's two mechanisms. A block spanning ten years becomes ten identical columns instead
of one parameter.

## What SS3 actually does, from the control file

`Model19_1e.ctl`, `# timevary selex parameters` (line 207 on). Per fleet and per pattern-24
parameter:

- up to four **block replacements**, `BLK2repl_1990 / 2005 / 2007 / 2017`, each with **its own
  phase** -- and not all are estimated: `SizeSel_P_1_FshTrawl(1)_BLK2repl_2017` is phase **-1**.
  So the 41 is a count of ACTIVE blocks, not of blocks present.
- `dev_se` at phase **-5**, fixed at **0.2**, and `dev_autocorr` at phase **-6**, fixed at **0**.

So the deviations are penalised with a **fixed** sd of 0.2 and no autocorrelation. In Rceattle
that is an identity-link `(1 | Year)` term with `integrate = FALSE` and the sd supplied rather
than estimated -- a penalised deviate, not an integrated random effect, and not a free offset.

## How SS3's DEVmult actually works, and why it is a LOG link

Measured 2026-10-01, and it changes the design. SS3's base and its 1977 realized value differ on
the dev'd FshTrawl parameters:

| parameter | SS3 base | realized 1977 | `DEVmult_1977` |
|---|---|---|---|
| `Size_DblN_peak_FshTrawl(1)` | 57.75250 | 52.88110 | -0.44060 |
| `Size_DblN_ascend_se_FshTrawl(1)` | 5.12526 | 4.83955 | -0.28680 |

It is not additive -- `57.7525 - 0.4406` is not `52.8811`. It is

```
realized = base * exp(dev * dev_se)
```

with `dev_se` the control file's fixed **0.2**: `ln(52.8811 / 57.7525) / -0.44060 = 0.2000`, and
`ln(4.83955 / 5.12526) / 0.2 = -0.28685` against the reported -0.28680. So the `DEVmult`
parameter is a **standardised** deviate and the realized effect is multiplicative. That also
reconciles `Parm_devs = 6.4903`: it is `sum(dev^2)/2` over the 63 standardised deviates (mean
`|dev|` ~0.35 gives ~4.7, same order), not a 0.2-scaled quadratic.

**Consequences for the Rceattle design:**

- The **devs are a `log` link** with offset `dev * 0.2`, equivalently a `log`-link `(1 | Year)`
  term over the dev years with `integrate = FALSE` and sigma fixed at **0.2**. They are NOT
  identity offsets.
- The **blocks stay `identity`**, because `Blk_Fxn = 2` REPLACES the parameter, so the offset is
  `block_value - base`.
- The two compose correctly **only because they never overlap**: devs are 1977-1989 and blocks
  start 1990 (patterns 2, 3) or 1996 (pattern 1). Rceattle consumes them as
  `base * exp(log_offset) + nat_offset`, so a dev year gets `base * exp(dev * 0.2)` and a block
  year gets `base + (block - base)`. If a year ever carried both, this decomposition would be
  wrong.

The current bridge treats every selectivity offset as `identity`, derived from the realized
series, which reproduces the forward pass exactly and is why G2 matches. It is the wrong
parameterisation to ESTIMATE under: an identity offset on a dev year has no penalty and the wrong
scale.

**One more count subtlety.** The bridge sets Rceattle's base to the realized **1977** value
(`sel_base <- sel_eff[, , 1]`). For FshTrawl that absorbs `DEVmult_1977`, so a design keyed on
1978-1989 would give 12 dev coefficients where SS3 has 13 -- three short across FshTrawl's three
dev'd parameters. To match, set the base to SS3's **base** parameter and give 1977 its own dev
column. `descend_se_FshTrawl` happens to be unaffected because its `DEVmult_1977` is 0.00000 (it
is one of the 13 inert ones).

## Reached: 330 = 330. And the cold start does not converge, for a reason worth having

**Parameter parity is achieved** (`RCE_SEL_PARITY=true`, measured on the merged
`init` + exponential-q tree): Rceattle estimates **330** by distinct map level against SS3's
`Active_count` of **330**, and the design is value-preserving -- `sel_at_age` reproduces SS3 to
**4.49e-06** and the objective to **8e-06** before anything is freed. The dev penalty matches
`Parm_devs` to **0.01 nats** on a 43.5-nat normalising constant.

**CORRECTED 2026-10-01.** An earlier version of this section said the fit then fails warm and
cold, and attributed it to Rceattle inverting a Hessian where SS3's optimiser does not. **That was
wrong, and the cause was self-inflicted.** With `newtonsteps = 0` and NOTHING held, all 330
estimated:

| | objective | max abs gradient |
|---|---|---|
| warm (from SS3's MLE) | 1866.0803 | **0.0026** |
| cold | 1896.6892 | **0.00445** |

Both converge. The `dgesv: system is exactly singular` failures came from
`TMBhelper::fit_tmb`'s post-optimisation Newton refinement, which does `solve(H, g)`
(`R/0-tmb_helpers.R` shows the same step in the fallback path, there wrapped in `tryCatch`;
TMBhelper's own is not). `run_g3_goa.R` passes `newtonsteps = 3` and this script copied it.
**Rceattle's own default is 0** (`fit_control`), so the package was never the problem.

The Newton steps also degraded the fit rather than refining it: gradient 0.221 with
`newtonsteps = 3` plus 21 parameters held, against 0.0026 with 0 held and 0 steps.

**Why ADMB converges on the same model.** Its optimiser is quasi-Newton on gradients alone,
maintaining an approximate inverse Hessian, so a zero-curvature direction simply produces no
movement. It forms the Hessian once at the end for standard errors, where a NEAR-singular matrix
inverts numerically and yields SE = 507 instead of an error. Nothing in that path solves
`H x = g` at the optimum.

**What survives.** The ~22 unidentified parameters are real -- SS3's own standard errors say so,
and AI cod and EBS cod have none above 10 (see `GOA-identifiability.md` material in the Pete
write-up). They simply do not prevent Rceattle from fitting. The flat-direction count is **8**,
not the 24 reported earlier: that number came from labelling eigenvectors with map levels ordered
by `unique()`, where TMB orders by `levels(factor())`, which sorts lexicographically so "10"
precedes "2". With the ordering fixed the flat set is the five `top_logit` bases, FshTrawl's
`descend_se` base, FshPot's `top_logit` block against its own base, and FshLL's peak-block common
mode -- coherent, and exactly where SS3's standard errors point.

**Open:** the cold start converges to 1896.69 against the warm start's 1866.08, so it lands 30.6
nats above the better optimum. That is a phasing or multiple-optima question, not an
identifiability one.

## Block year ranges, from the control file

`Model19_1e.ctl` lines 19-26: 6 patterns, `2 4 1 1 1 1` blocks each.

| pattern | blocks | used by |
|---|---|---|
| 1 | 1996-2005, 2006-2024 | `Srv(4)` |
| 2 | 1990-2004, 2005-2006, 2007-2016, 2017-2024 | `FshTrawl(1)`, `FshLL(2)` |
| 3 | 2017-2024 | `FshPot(3)` |
| 4 | **2014-2016** | `NatM` -- a three-year window, not 2014 onwards |
| 5 | **1976-1976** | `SR_regime` -- one year, at `styr - 1` |
| 6 | 1976-2006 | |

Pattern 5 confirms the regime is a single year before the hindcast, which is why no linkage can
address it directly (`.trim_env_data()` drops pre-styr rows) and why it is expressed as an
initial-level parameter instead.

Not every block present is active: `SizeSel_P_1_FshTrawl(1)_BLK2repl_2017` is phase **-1**, so
`peak` FshTrawl has 3 active blocks where the other pattern-2 parameters have 4. The 40 is a
count of ACTIVE blocks.

Per fleet and parameter, the control file also sets `dev_se` at phase **-5** fixed at **0.2** and
`dev_autocorr` at phase **-6** fixed at **0**. So the deviations are penalised deviates with a
fixed sd and no autocorrelation: in Rceattle an identity-link `(1 | Year)` with
`integrate = FALSE` and the sd supplied, not a free offset and not an integrated random effect.

## What matches already, and why that was worth checking

Two counts look like mismatches and are not. Both were checked rather than assumed, and both
dissolved:

- **Recruitment deviations match at 48.** `Main_RecrDev` runs 1978-2024, which is 47, but
  `Early_RecrDev_1977` is a separate ACTIVE single. Rceattle estimates `rec_dev` across
  1977-2024 = 48. Equal. (Both counts are the same in all three directories.)
- **Initial age deviations match at 10.** `#_Nages = 10` and `Report.sso`'s `NUMBERS_AT_AGE`
  columns run 0-10, so the population has **11** ages and Rceattle's `nages = 11` under the
  converter's `minage = 0`. `init_dev` is estimated over columns `1:(nages - 1)` = 10, against
  `Early_InitAge_1..10`. Equal.

So the gap is confined to selectivity time-variation plus two single parameters.

## The gap: 106 parameters

| missing in Rceattle | n | how it is expressed | state |
|---|---|---|---|
| selectivity block replacements | 41 | identity-link linkage, **one column per block YEAR RANGE** | converter work |
| selectivity `DEVmult`s | 63 | identity-link per-year columns, **penalised**, fixed sd | converter work |
| `LnQ_base_LLSrv(5)_ENV_mult` | 1 | `link = "exponential"` on q | PR #181 |
| `SR_regime_BLK5add_1976` | 1 | recruitment `init` linkage | `feat/srr-init-level` |

90 + 106 = 196.

## Why neither current G3 variant is an equal-footing comparison

`run_g3_goa.R` has two modes and neither estimates SS3's set:

- **`fixsel`** holds every selectivity offset at SS3's value, so Rceattle estimates about **90** —
  104 fewer than SS3, and the question "given SS3's selectivity, does Rceattle find SS3's growth,
  M and recruitment" is a different question from parity.
- **`freesel`** frees **867** coefficients. That is not SS3's 104 either, and the 867 carry **no
  penalty** where SS3 penalises its 63 devs (`Parm_devs = 6.49` at its own MLE), so it fits a
  strictly looser model than SS3 ever did and a lower objective means nothing.

The 867 is the symptom of the representation, not of a choice. The bridge builds one linkage
design column **per varying year** (`ss3_to_ceattle_forward_pass.R`, the `PAR_LINK` loop), which
reproduces SS3's realized selectivity exactly -- that is why the forward pass matches -- but it
conflates SS3's two mechanisms. A block spanning ten years becomes ten identical columns instead
of one parameter.

## What SS3 actually does, from the control file

`Model19_1e.ctl`, `# timevary selex parameters` (line 207 on). Per fleet and per pattern-24
parameter:

- up to four **block replacements**, `BLK2repl_1990 / 2005 / 2007 / 2017`, each with **its own
  phase** -- and not all are estimated: `SizeSel_P_1_FshTrawl(1)_BLK2repl_2017` is phase **-1**.
  So the 41 is a count of ACTIVE blocks, not of blocks present.
- `dev_se` at phase **-5**, fixed at **0.2**, and `dev_autocorr` at phase **-6**, fixed at **0**.

So the deviations are penalised with a **fixed** sd of 0.2 and no autocorrelation. In Rceattle
that is an identity-link `(1 | Year)` term with `integrate = FALSE` and the sd supplied rather
than estimated -- a penalised deviate, not an integrated random effect, and not a free offset.

## How SS3's DEVmult actually works, and why it is a LOG link

Measured 2026-10-01, and it changes the design. SS3's base and its 1977 realized value differ on
the dev'd FshTrawl parameters:

| parameter | SS3 base | realized 1977 | `DEVmult_1977` |
|---|---|---|---|
| `Size_DblN_peak_FshTrawl(1)` | 57.75250 | 52.88110 | -0.44060 |
| `Size_DblN_ascend_se_FshTrawl(1)` | 5.12526 | 4.83955 | -0.28680 |

It is not additive -- `57.7525 - 0.4406` is not `52.8811`. It is

```
realized = base * exp(dev * dev_se)
```

with `dev_se` the control file's fixed **0.2**: `ln(52.8811 / 57.7525) / -0.44060 = 0.2000`, and
`ln(4.83955 / 5.12526) / 0.2 = -0.28685` against the reported -0.28680. So the `DEVmult`
parameter is a **standardised** deviate and the realized effect is multiplicative. That also
reconciles `Parm_devs = 6.4903`: it is `sum(dev^2)/2` over the 63 standardised deviates (mean
`|dev|` ~0.35 gives ~4.7, same order), not a 0.2-scaled quadratic.

**Consequences for the Rceattle design:**

- The **devs are a `log` link** with offset `dev * 0.2`, equivalently a `log`-link `(1 | Year)`
  term over the dev years with `integrate = FALSE` and sigma fixed at **0.2**. They are NOT
  identity offsets.
- The **blocks stay `identity`**, because `Blk_Fxn = 2` REPLACES the parameter, so the offset is
  `block_value - base`.
- The two compose correctly **only because they never overlap**: devs are 1977-1989 and blocks
  start 1990 (patterns 2, 3) or 1996 (pattern 1). Rceattle consumes them as
  `base * exp(log_offset) + nat_offset`, so a dev year gets `base * exp(dev * 0.2)` and a block
  year gets `base + (block - base)`. If a year ever carried both, this decomposition would be
  wrong.

The current bridge treats every selectivity offset as `identity`, derived from the realized
series, which reproduces the forward pass exactly and is why G2 matches. It is the wrong
parameterisation to ESTIMATE under: an identity offset on a dev year has no penalty and the wrong
scale.

**One more count subtlety.** The bridge sets Rceattle's base to the realized **1977** value
(`sel_base <- sel_eff[, , 1]`). For FshTrawl that absorbs `DEVmult_1977`, so a design keyed on
1978-1989 would give 12 dev coefficients where SS3 has 13 -- three short across FshTrawl's three
dev'd parameters. To match, set the base to SS3's **base** parameter and give 1977 its own dev
column. `descend_se_FshTrawl` happens to be unaffected because its `DEVmult_1977` is 0.00000 (it
is one of the 13 inert ones).

## Reached: 330 = 330. And the cold start does not converge, for a reason worth having

**Parameter parity is achieved** (`RCE_SEL_PARITY=true`, measured on the merged
`init` + exponential-q tree): Rceattle estimates **330** by distinct map level against SS3's
`Active_count` of **330**, and the design is value-preserving -- `sel_at_age` reproduces SS3 to
**4.49e-06** and the objective to **8e-06** before anything is freed. The dev penalty matches
`Parm_devs` to **0.01 nats** on a 43.5-nat normalising constant.

**The fit then fails**, warm and cold, with `dgesv: system is exactly singular` /
`reciprocal condition number = 1.8e-17`. That is not a bridge defect. Taking the Hessian at
SS3's own values and decomposing it: **24 directions with |eigenvalue| < 1e-2**, against a
maximum of 4.5e+07, and the smallest seven are NEGATIVE (-7.4e-05 to -2.5e-07), i.e. zero
curvature with numerical noise.

Named, the flat mass is concentrated in three places:

1. **`top_logit` block coefficients -- almost all of them.** `s1p2_blk1990/2005/2017`,
   `s2p2_blk1990/2005/2007/2017`, `s3p2_blk2017`, `s4p2_blk1996`, each loading against its own
   `sel_dn6` base. `top_logit` is the double normal's plateau WIDTH on a logit scale, and SS3's
   values put it at -4.5 to -12.2, i.e. a plateau of **0.0105 down to 4.9e-06**. A flat top
   0.006 wide and one 1e-05 wide are the same curve, so the coefficient has no curvature.
2. **The 13 `descend_se_FshTrawl` devs**, `s1p4_dev1977..1989`, which SS3 itself estimates at
   ~2e-07.
3. **Three `dn_peak` blocks** -- `s4p1_blk1996`, `s4p1_blk2006`, `s1p1_blk2007`.

**So GOA Pacific cod's control file asks SS3 to estimate roughly 24 parameters the data do not
inform.** SS3 returns values for them anyway and reports `Parm_StDev = 0`; nothing in its output
says they are unidentified. Rceattle inverts a Hessian where SS3's optimiser does not, so
faithful count parity surfaces the deficiency as a hard failure.

**What this means for the comparison.** "Estimate all the same parameters" is achievable as a
COUNT and is not achievable as a FIT. The equal-footing comparison is of the identified
subspace: estimate the ~306 directions the data inform and hold the ~24 they do not at SS3's
values. That is a stronger claim than a count match, because it compares the same estimable
model rather than the same parameter list.

Two traps found getting here, both from reading Rceattle's own output rather than assuming:

- **`Parm_StDev` is 0 for EVERY dev row**, not just the unidentified ones, so it cannot be used
  to find them. SS3 does not report standard errors for devs at all. The VALUE discriminates
  (~2e-07 against 0.3-1.4); the gradient does not (~1e-06 for both).
- **A relative eigenvalue threshold is useless here.** `1e-6 * max` with a maximum of 4.5e+07 is
  44.9, which flags well-determined directions as flat. Use an absolute cut.

## To do, in order

1. **Restructure the selectivity linkage design** so each estimated block is ONE coefficient
   over its year range, and each `DEVmult` year is one coefficient under a fixed-sd density. The
   design change must be value-preserving: verify the forward pass still reproduces SS3 before
   freeing anything, since a block is only one coefficient if SS3's offset really is constant
   across its range.
2. **Check the count**, not the shape: Rceattle's estimated-parameter total should read 196.
3. **Then** run the cold start. A lower objective is only meaningful once the counts agree AND
   the 63 deviations carry their penalty.

## Not in the 196, and still not expressible

SS3's **age** selectivity pattern 10 (ages 1..nages = 1, age 0 = 0) multiplies the
size-derived curve. It is not an estimated parameter, so it does not enter this count, but it is
worth **76.6 nats** on the forward pass and Rceattle has no per-fleet age multiplier to compose
with a length-based curve. See `GOA-remaining-gap.md` section 1.
