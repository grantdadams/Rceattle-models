# GOA Pacific cod: estimating the same 196 parameters SS3 does

**Status:** the target is enumerated and verified; the work to reach it is identified.
Measured 2026-09-30 from `Data/goa_pcod/Report.sso` (`Model19_1e`, the pristine assessment
control file). Read with `GOA-estimation-parity.md`, which measures how far the two optima
sit apart, and `HANDOFF.md` for branch state.

SS3 reports **`Active_count: 196`**. Taking the `PARAMETERS` table rows with a numeric
`Active_Cnt` (lines 239-491 of that `Report.sso`) and grouping them:

| kind | n | what it is |
|---|---|---|
| `SizeSel_..._DEVmult_<yr>` | **63** | per-year selectivity deviations, **penalised** |
| `Main_RecrDev_1978..2024` | **47** | recruitment deviations |
| `SizeSel_..._BLK2repl_<yr>` | **41** | selectivity block replacements |
| `Early_InitAge_1..10` | **10** | initial age deviations |
| `LnQ_base_LLSrv(5)_ENV_mult` | **1** | environmental catchability |
| base singles | **34** | M, growth (5), SR_LN(R0), `SR_regime_BLK5add_1976`, `Early_RecrDev_1977`, 2 log q, 24 selectivity base |

63 + 47 + 41 + 10 + 1 + 34 = 196.

## What matches already, and why that was worth checking

Two counts look like mismatches and are not. Both were checked rather than assumed, and both
dissolved:

- **Recruitment deviations match at 48.** `Main_RecrDev` runs 1978-2024, which is 47, but
  `Early_RecrDev_1977` is a separate ACTIVE single. Rceattle estimates `rec_dev` across
  1977-2024 = 48. Equal.
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
