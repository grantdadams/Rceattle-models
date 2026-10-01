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
