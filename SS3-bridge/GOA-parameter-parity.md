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

## MEASURED 2026-10-08: Rceattle estimates 1074, not 225 — the selectivity linkages are not pinned

`GOA-estimation-parity.md` records "selectivity blocks + devs | 103 | **0 — held at SS3's
values**" and a Rceattle total of **225**. That is false. Counted off `fp$obj$par` from the
forward pass (`RCE_SS3_DIR=Data/goa_pcod_caal_bins_fixed`):

| Rceattle block | free | SS3 counterpart | n |
|---|---|---|---|
| `beta_linkage` | **869** | sel `DEVmult` 63 + block replacements 40 | **103** |
| `log_F` | 134 | annual F | 134 |
| `rec_dev` | 48 | `Main_RecrDev` 47 + `Early_RecrDev_1977` 1 | 48 |
| `init_dev` | 10 | `Early_InitAge_1..10` | 10 |
| `sel_dn6` | 5 | part of the 23 selectivity base | |
| `log_growth_pars` 3, `growth_log_sd` 2, `index_log_q` 2, `rec_pars` 1 | 8 | part of the 32 base singles | |
| **total** | **1074** | | **330** |

`ss3_map$mapFactor$beta_linkage` is a factor of length 873 with **4 NA and 869 distinct
levels** -- i.e. four pinned, every other coefficient free and on its own level.
`ss3_to_ceattle_forward_pass.R` only ever *writes* `inits$beta_linkage` (lines 700, 932, 948);
there is no code anywhere that maps it out. So the pinning the accounting assumes has never
existed, and Rceattle carries **766 free parameters SS3 does not have**.

**What this does and does not invalidate.**

- **The forward pass is unaffected.** It runs at `estimateMode = 3`, which builds without
  optimising, so a free parameter sitting at SS3's injected value contributes exactly what SS3's
  value contributes. The likelihood parity (max residual 0.0013 nats) stands.
- **G2 is not interpretable as stated.** Max |gradient| 185 at SS3's MLE, on `beta_linkage`
  (-185, -112, -104) and the growth parameters, is 869 unintended free parameters wanting to
  move. SS3's MLE cannot be a stationary point for a model with 766 extra directions in it.
- **Every free fit is overfitting.** `run_goa_cod.R`'s free fit stops at **1225.23** against the
  **1940.10** this file's sibling records as "Rceattle's own optimum" -- 715 nats lower, which is
  what 766 surplus parameters buy. That number is not a better fit and not a bad stop; it is a
  different model.

**To close it**, map `beta_linkage` down to the 103 coefficients SS3 actually estimates, holding
the rest at the injected values: the 63 `DEVmult` as identity-link `(1 | Year)` with
`integrate = FALSE` and sd 0.2 supplied (the control file fixes `dev_se` at phase -5, value 0.2,
and `dev_autocorr` at phase -6, value 0), and the 40 active block replacements as their own
coefficients. Until then quote 1074, and do not quote 225.

### FIXED 2026-10-08: the linkage coefficients are now held

`ss3_to_ceattle_forward_pass.R` now pins every `beta_linkage` coefficient at the value it
injects, right after `ss3_fix_map()`. `RCE_FIX_LINKAGE=false` leaves them free. Grant's call.

Measured before and after, `Data/goa_pcod_caal_bins_fixed`:

| | free | forward pass at SS3's MLE | Rceattle's own optimum | max abs gradient there |
|---|---|---|---|---|
| before | 1074 | 1962.1657 | 1225.2296 | **2.08** (not converged) |
| after | **205** | **1962.1657** (unchanged) | **1925.9058** | **0.00478** (converged) |

**The forward pass does not move, and that is the check.** Pinning a free parameter at the
value it already holds cannot change an unoptimised evaluation, so an unchanged 1962.1657 is
what confirms the 766 surplus directions were never contributing to the forward-pass
likelihood -- only to any fit taken from it. The likelihood parity (max residual 0.0013 nats)
is therefore unaffected, and estimation parity is now meaningful for the first time.

Rceattle's optimum lands **14.2 nats below** the 1940.10 this file's sibling records, at a
better gradient (0.00478 against 0.0077). Both closures since that measurement moved the
forward pass by -36.8, so an optimum moving in the same direction is expected; it has not been
attributed term by term.

**G2 at SS3's MLE improves 185 -> 83** and `beta_linkage` leaves the top ten entirely. What
remains is `log_growth_pars` (-83, -66.7, 32.2), `growth_log_sd` (64.4, 10.4), `index_log_q`
(45), `rec_pars` (44.6) and `rec_dev` (10.4, 7.75, 6.54).

### The growth gradient and the age-0 selectivity difference are ONE issue

The growth half of that residual gradient is not a separate problem. L1 **is** injected
correctly (the log reads `Growth injected: K=0.2039 L1=1.3007 Linf=99.4611`), so the gradient
is Rceattle's likelihood genuinely preferring a different L1 at SS3's own MLE. This file's
sibling already measured why: the ORIGINAL model's L1 slice *rises* +14.70 at 0.5 and +38.51 at
0.001, where the age-0-SELECTED variant's *falls* -- "the direct evidence that the age-0 zeroing
is what holds SS3's L1 at 1.3007". Rceattle has no age-0 zeroing
(`../GOA cod/Bridging/Estimation_Differences.md` #19), so it behaves like the age-0-selected
variant and its composition likelihood pulls L1 down.

So one missing feature accounts for all of it: the five remaining G1 failures, the growth
gradient in G2, and the "L1 runs to a harness floor" behaviour. Zeroing age-0 selectivity on a
length-based fleet is the single thing left to close the GOA cod bridge.

### 2026-10-08, part 2: the selectivity parameterisation DOES match, and why it looked like it did not

`RCE_SEL_PARITY=true` already implements SS3's two mechanisms -- blocks as identity-link columns
(coefficient = block value - base, over the block's year range), devs as log-link columns
(coefficient = dev x 0.2, one year each). It is **off by default**. With it on:

```
[sel parity] blocks 40 (+7 held), devs 50 (+13 held unidentified in SS3) = 90 estimated
```

40 blocks is exactly SS3's active count, and 50 + 13 = SS3's 63 devs. So the structure matches;
it is not a subset of the per-year design and could never have been reached by pinning.

**The phase source was the bug.** A block is held when its SS3 phase is non-positive, read from
`SS3_DIR/Report.sso` column 5. A directory written with `last_estimation_phase 0` -- which every
forward-pass variant is -- reports `Active_Cnt = _` and **`Phase = -1` for every parameter**, so
nothing had a positive phase and all 47 blocks were held:

| directory | block phase | blocks with positive phase |
|---|---|---|
| `goa_pcod_caal_bins_fixed` (estimated) | 1 | **42** |
| `goa_pcod_caal_bins_fixed_age0` (estimation off) | **-1** | **0** |
| `goa_pcod_caal_bins_fixed_age0_est` (estimated) | 1 | **42** |

It failed quietly: a `NAs introduced by coercion` warning on the phase vector, then 47 held
blocks reported as a normal line of output. **`RCE_SS3_PHASE_DIR`** now points the phase read at
an estimated sibling while values and data still come from `SS3_DIR`; it reports how many labels
matched (387 of 387).

**Counts now, age-0 removed, `RCE_SEL_PARITY=true RCE_FIX_LINKAGE=false`:** Rceattle **302**
against SS3's **330**. The remaining 28 are the 13 devs SS3 estimates that are unidentified in
it (`descend_se_FshTrawl` at ~2e-07, already documented above) plus the selectivity base slots
SS3 estimates that the PHASE-honouring map pins (SS3 has 23 active selectivity base, Rceattle 5
free `sel_dn6`).

| | SS3 (330 free) | Rceattle (302 free) |
|---|---|---|
| objective | 2075.78 | 1876.93 (max grad 0.00114) |
| K | 0.207996 | 0.207575 |
| Linf | 99.460800 | 99.460613 |
| L1 | **0.000000** | 0.034862 |
| ln(R0) | 12.619400 | 12.671387 |
| M | 0.456845 | 0.467787 (**not estimated** in Rceattle) |

**`L1` is not representable.** SS3's patched MLE puts L1 at exactly its bound, 0. Rceattle
parameterises growth on the log scale, so injecting that MLE fails outright with "Initial
parameter values are not within bounds: log_growth_pars". That is what the "harness floor at
1e-3" in the estimation doc actually is -- not a harness choice but a parameterisation limit.
Freed, Rceattle lands at 0.0349 rather than at a floor.

**Still not the same MLE.** After the additive constants Rceattle sits ~33 nats BELOW SS3 with 28
FEWER parameters, which is the wrong direction for a parameter-count explanation and so is a
real difference in fit, not a degrees-of-freedom artifact. The unmatched prior rows (Rceattle M
prior + linkage priors against SS3 `Parm_priors` + `Parm_devs` + `Parm_softbounds`) and the
`predicted CAAL` 5.97e-02 cell error are the two unexplained terms left.

### 2026-10-08, part 3: the gap is 22, and exactly which 22

`ss3_fix_map()` reads `ss3_rep$parameters$Phase` and needed the same phase redirect as `.ss3_P`.
A forward-pass directory reports **117 negative and 0 positive** phases where its estimated
sibling reports **42 negative and exactly 330 positive** -- SS3's own count -- so the map was
pinning 117 parameters SS3 estimates. Fixed: the forward pass now rebuilds
`ss3_rep$parameters$Phase` from `RCE_SS3_PHASE_DIR` before calling the map, and says so. Free
count 302 -> **308**, forward pass unchanged at 1934.1311 (the check: a map cannot move an
unoptimised evaluation).

With `RCE_SEL_PARITY=true RCE_FIX_LINKAGE=false` against the age-0-removed model:

| Rceattle block | free | SS3 counterpart | n | |
|---|---|---|---|---|
| `log_F` | 134 | annual F | 134 | match |
| `beta_linkage` | **103** | sel `DEVmult` 63 + block replacements 40 | **103** | **match** |
| `rec_dev` | 48 | `Main_RecrDev` 47 + `Early_RecrDev_1977` 1 | 48 | match |
| `init_dev` | 10 | `Early_InitAge_1..10` | 10 | match |
| `sel_dn6` 5, growth 5, `index_log_q` 2, `rec_pars` 1 | 13 | base singles 32, `NatM`/`SR_regime` blocks 2, `LnQ ENV_mult` 1 | 35 | **-22** |
| **total** | **308** | | **330** | |

**The selectivity count now matches exactly**, which was the hard part. The whole remaining gap
is the last row, and it is three things:

1. **18 selectivity base.** SS3 has 23 active; Rceattle frees 5. `ss3_fix_map()` only maps out 7
   `sel_dn6` slots, so the rest are held by Rceattle's own `build_map()` -- which is the
   `RCE_SEL_PARITY` design: the base is held and the variation lives in the 103 linkage
   coefficients. A legitimate split of the same information, not a missing parameter, but it does
   mean the counts cannot both be 330 and mean the same thing.
2. **1 M.** `log_M1` is not in `obj$par` at all; base M is fixed at SS3's value, which is why a
   fitted M of exactly 0.467787 is SS3's own number handed back rather than an estimate.
3. **3 non-selectivity linkage coefficients** -- `NatM..._BLK4repl_2014`,
   `SR_regime_BLK5add_1976`, `LnQ_base_LLSrv(5)_ENV_mult`. They exist in Rceattle's linkage table
   and SS3 estimates all three.

**The forward-pass likelihood already matches to 0.0013 nats**, and that is the comparison the
parameter count does not affect: at `estimateMode = 3` nothing is optimised, so a held parameter
sitting at its injected value contributes exactly what SS3's value contributes. The count matters
only for the estimated comparison.

#### Correction: the 103 = 103 above is a COINCIDENCE, and the three extras are not missing

Measured from the map and the linkage table together, under `RCE_SEL_PARITY`: the table is 129
rows (122 `sel`, 7 non-`sel`) and `beta_linkage` has **103 free = 100 selectivity + 3
non-selectivity**, with 22 selectivity rows pinned. So it is not 63 + 40, and the match to SS3's
103 selectivity coefficients was arithmetic luck.

Row by row, the non-`sel` coefficients:

| row | state | SS3 counterpart |
|---|---|---|
| `growth K / L1 / Linf (Intercept)` | **pinned** | none -- they would duplicate `log_growth_pars` |
| `M M1 heatwave` | **free** | `NatM..._BLK4repl_2014` |
| `recruitment R_init init_lvl` | **free** | `SR_regime_BLK5add_1976` |
| `q (Intercept)` | **pinned** | none -- duplicates `index_log_q` |
| `q LLSrv_q_env` | **free** | `LnQ_base_LLSrv(5)_ENV_mult` |

**So all three of SS3's non-selectivity extras ARE free**, and the pinned intercepts are pinned
correctly: an intercept linkage on a process that already has a base parameter duplicates it, and
leaving both free is the masking trap. That also fits the two convergence flags on the free fit --
`index_log_q` reported non-identifiable and a non-invertible Hessian -- which would be worse, not
better, with the intercepts freed.

**Corrected gap, still 22:**

| | Rceattle | SS3 | |
|---|---|---|---|
| selectivity linkage | **100** | 63 `DEVmult` + 40 blocks = **103** | **-3** |
| non-selectivity linkage | 3 | 2 blocks + `ENV_mult` = 3 | match |
| base singles (`sel_dn6` 5, growth 5, q 2, `rec_pars` 1) | 13 | 32 | **-19** |
| `log_F` / `rec_dev` / `init_dev` | 134 / 48 / 10 | 134 / 48 / 10 | match |

The -19 is 18 selectivity base plus `log_M1`, as above. The **-3 on the selectivity linkage is
new and unexplained**: `[sel parity]` declares 90 estimated (40 blocks + 50 devs) while the map
leaves 100 free, and SS3 has 103. Those three numbers need reconciling against the 122 `sel`
rows and the 22 pinned before any count claim is made again.
