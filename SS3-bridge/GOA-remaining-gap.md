# What is left between Rceattle and SS3 on GOA Pacific cod

> **Updated 2026-09-26.** Both composition components now agree to 0.001 and the
> catch to 1e-04. Every remaining difference is accounted for; see
> "Full accounting" at the end, which was re-derived at the current head on
> 2026-09-26 and reproduces to 1e-04.
>
> Section 2's CAAL residual of +143.5 is **closed** -- that is what the
> ageing-error fleet split did -- and section 2 is kept for the measurement.
> Estimation parity is a separate note, `GOA-estimation-parity.md`.

**Status:** measured at SS3's MLE, 2026-09-25. Two SS3 features Rceattle cannot
express account for all of the remaining difference. Neither is a bridge defect.

Forward-pass objective, GOA Pcod Model 19.1e (CAAL-corrected copy):

| | objective |
|---|---|
| SS3, as the assessment runs it | 2051.98 |
| SS3, same parameters, age-0 selected | 2128.83 |
| Rceattle | **2008.93** |

The Rceattle figure was 2158.22 when this was first written, before the
ageing-error fleet split closed the CAAL residual of section 2. The Full
accounting at the end reproduces at the current head; it is the one to trust.

**Estimation parity is written up separately**, in `GOA-estimation-parity.md`.
Both places where Rceattle's own optimum differs from SS3's trace back here: the
L1 drift is item 1 below seen through estimation, and M is simply weakly
identified (0.90 nats at SS3's value).

## 1. Age selectivity: SS3 zeroes age 0, Rceattle cannot

All five active fleets carry SS3 **age** selectivity pattern 10, which sets ages
1..nages to 1 and leaves age 0 at zero (`SS_selex.tpl:997-1001`). SS3's realized
`Asel2` is the size-derived curve **times** that, so age 0 is zeroed. Rceattle
has no per-fleet age multiplier that composes with a length-based curve.

It surfaces on fleet 4 (Srv) alone, because it alone has a non-zero initial
floor -- `P5 = -2.79`, so selectivity is 0.0577 at the smallest lengths. The
other four fleets' size curves are already ~0 there.

Confirmed by switching SS3's age patterns from 10 to 0 ("constant age-specific
selex for ages 0 to nages", `SS_selex.tpl:990-993`) and rerunning SS3 as a pure
forward pass at the same MLE (330 estimated parameters down to 1). Against that
variant:

| | with SS3 zeroing age 0 | with age 0 selected |
|---|---|---|
| `sel_at_age` vs `Asel2`, fleet 4 | 3.57e-02 | **2.45e-06** |
| `sel_at_age`, all five fleets | up to 3.57e-02 | **<= 3.0e-06** |
| Composition likelihood | 1425.53 vs 1331.85 | **1407.639 vs 1407.640** |

So the age-0 zeroing was the entire fleet-4 selectivity discrepancy, and with it
removed the length compositions agree to **0.001**. It is worth 76.6 nats.

Both composition components have since been checked against the same variant
across a grid of L1 spanning 0.001 to 6.39 cm, not only at the MLE: max abs
difference **0.0049** nats on the length comps and **0.0009** on the CAAL
(`GOA-estimation-parity.md`). That exercises the age-length key over four orders
of magnitude of the parameter setting its young end.

`Bin_first_selected` is NOT a substitute. Rule 10: it is read on the fleet's own
`Selectivity_dimension`, which is Length here, so it zeroes population LENGTH
bin 0 rather than age 0. Setting it to 2 lowers the objective by 6.4 nats, but
only by breaking the length curve where SS3's sub-`startbin` ramp defines it.

## 2. Ageing error: SS3 picks a matrix per observation, Rceattle per species

GOA carries **two** ageing-error definitions, and fleet 4 uses **both**: 133
CAAL rows on definition 1 and 162 on definition 2, while fleets 1-3 use only
definition 2. Definition 1 is **biased** -- its mean ages run 0.60, 1.81, 3.02,
4.22 ... against the true age -- where definition 2 is unbiased (`-1`). The two
share the same SDs.

Rceattle takes one ageing-error matrix per species, so the converter applies
definition 2 to everything and says so. Splitting fleet 4's CAAL error by the
definition each row *should* use:

| rows needing | n | max abs | mean abs |
|---|---|---|---|
| definition 2 (what is applied) | 161 | **8.0e-06** | 6.3e-07 |
| definition 1 (biased, not applied) | 133 | **4.8e-01** | 2.9e-01 |

That is the whole of the remaining CAAL residual, +143.5. Nothing else in that
component is out: the rows that happen to get the right matrix are exact.

This is also why the length compositions can match to 0.001 while CAAL does not.
A length composition is the age-marginal of `pred_CAAL`, and an ageing-error
matrix redistributes probability **across ages within a length bin**, leaving
that marginal alone.

## Closing them

- A per-fleet age-selectivity multiplier that composes with a length-based
  curve. SS3's pattern 10 is the common case (no age-0 selection) and would
  cover every AFSC size-selective fleet met so far.
- An ageing-error matrix selectable per observation row -- SS3's `ageerr`
  column on each composition and CAAL record -- rather than one per species.

Until both exist, GOA cod cannot be bridged exactly, and the residual is
quantified above rather than unexplained.

## Reproducing

The age-0 variant is built by changing the five `10` entries under
`#_age_selex_patterns` to `0` in `Model19_1e.ctl`, setting starter's
`#_init_values_src` to 1 and `#_last_estimation_phase` to 0 (the GOA starter
uses those short labels, not the long ones the AI model has), and running an
SS3 v3.30.22.1 binary. Point the bridge at it with `RCE_SS3_DIR`.


## Full accounting (2026-09-25)

Against the age-0-selected SS3 variant, at SS3's MLE, after the density
constants SS3 drops:

| SS3 component | Rceattle | SS3 | constant | residual |
|---|---|---|---|---|
| Length_comp | 1407.6387 | 1407.6400 | — | **-0.0013** |
| Age_comp | 732.7701 | 732.7710 | — | **-0.0009** |
| Catch | -263.9869 | 1.5320 | -265.5188 | **-0.0001** |
| Survey | 52.3850 | -3.5264 | 45.9469 | +9.9645 |
| Recruitment | 85.9251 | -17.1218 | 53.2984 | +49.7485 |

and three rows SS3 has that Rceattle does not: `Parm_priors` 1.0285,
`Parm_devs` 6.4903, `Parm_softbounds` 0.0117.

**Survey +9.96 is LLSrv's environmental catchability**, the `EnvExp` gap,
re-confirmed 2026-09-26 at +9.9644 after a catchability defect in the fleet split
was fixed (`shared-block-parameter-injection.md`); for a while it read +16.20. The
predicted index splits cleanly: fleet 4 (Srv) agrees to **4.8e-06** over its 16
observations, fleet 5 (LLSrv) is out by up to 2.9e-01 over its 34. Both
catchabilities are injected correctly (1.4964 and 1.38505, matching SS3), and
the SDs the likelihood uses match SS3's `SE` exactly, so the whole of it is the
missing exponential link.

**Recruitment +49.75 is a bridge choice, not a model difference -- measured
2026-09-30.** SS3 carries its regime shift as a free parameter,
`SR_regime_BLK5add_1976` = -1.3879, which it does not penalise. The bridge folds
that into `init_dev` so the initial numbers pin exactly, and Rceattle then
charges the constant shift the recruitment deviate penalty.

Carrying the level somewhere unpenalised instead closes it. Under `srr_fun = 0`
`R_init = R0(sp, 0)` with no `SPRFinit` feedback, and under `initMode 4`
(`FishedNonEquilibriumScaled`) `Finit` enters the initial decay ONCE rather than
cumulatively, so `exp(-Finit)` is a constant multiplier on ages 1..nages-2 and
carries no penalty. Setting `Finit = -SR_regime` and re-pinning `init_dev`
(`+Finit` at every age, plus `log[(1-e^-Mp-Finit)/(1-e^-Mp)]` = 0.8145 on the
plus group, whose geometric series divides by `1 - exp(-M_plus - Finit)`):

| jnll row | level in `init_dev` | level in `Finit` | change |
|---|---|---|---|
| Index data | 52.38500518 | 52.38500512 | -5.8e-08 |
| Catch data | -263.98685665 | -263.98687065 | -1.4e-05 |
| Composition data | 1407.63853186 | 1407.63871718 | +1.9e-04 |
| CAAL data | 732.77013449 | 732.77013586 | +1.4e-06 |
| **Initial abundance deviates** | **54.97530425** | **5.06183553** | **-49.9133** |
| Recruitment deviates | 30.94981834 | 30.94981834 | 0 |
| M prior | 0.03485308 | 0.03485308 | 0 |

The whole of it is one row and no fitted component moves. Against SS3's
recruitment component (-17.1218 plus its 53.2984 constant = 36.1766), Rceattle
goes 85.9251 -> 36.0116, so **the residual closes from +49.7485 to -0.165**.
There is no difference in the initial state; there was only a penalty SS3 does
not charge.

`Finit` is the wrong home for it, and is used here only to measure. It reports a
recruitment regime as an initial fishing mortality of 1.39/yr with `SPRFinit`
evaluated there; it works only because GOA cod has no initial equilibrium catch,
so nothing else scores `Finit` (AI cod has one); it stops being a level under a
fitted curve, where `SPRFinit` feeds back into `R_init` in the opposing
direction; and the plus group's deviate absorbs +0.81 that a clean parameter
would not, so part of the remaining 5.06 is an artefact of the carrier.
Rceattle needs a free, unpenalised initial recruitment LEVEL. `init_log_scalar`
(`ceattle.cpp`, section 6.5) is already the slot -- it is hard-wired to
`rec_dev(sp, 0)` for `initMode 5` and zero otherwise.

Caveat on the run: styr numbers-at-age agree to 1.2e-04 rather than exactly,
because the plus-group correction uses `M_base` where the model's year-0
plus-group M differs slightly under the M block; that is also the 1.9e-04 on the
composition. Reproduce with `scratchpad/regime_in_finit.R` (forward pass only).

**Catch was a harness bug, now fixed.** `.ss3_constants()` charged a density
constant to all 144 hindcast catch rows, but both models score only positive
catches (`catch_ret_obs > 0` in SS3, `catch_obs > 0` in Rceattle). GOA has ten
zero-catch years, the years before its pot fishery existed, and counting them
put a spurious +19.81 on the residual. With the constant taken over fitted rows
the residual is -1e-04.

**Priors.** GOA has four, worth 1.0285 in total, which the bridge does not
inject: `NatM_uniform_Fem_GP_1` Log_Norm(-0.81, 0.41) = 0.0075,
`L_at_Amax_Fem_GP_1` Normal(99.46, 0.015) = 0.0027, `VonBert_K_Fem_GP_1`
Normal(0.1966, 0.030) = 0.0296, and `NatM_uniform_Fem_GP_1_BLK4repl_2014`
Log_Norm(-0.81, 0.41) = 0.9887. The first three map straight onto Rceattle
priors; the fourth does not, because SS3 puts it on the block's M VALUE while
Rceattle's parameter is the log-ratio `log(M_block / M_base)`. AI cod has no
priors at all, which is why its bridge removed them.

**The bias-adjustment ramp is not a factor.** `max_bias_adj = -1` is SS3's
shortcut for full bias adjustment everywhere, and the `recruit` table confirms
`biasadjuster` is 1 in every year, so the ramp years in the control file are
inert and there is nothing to switch off.
