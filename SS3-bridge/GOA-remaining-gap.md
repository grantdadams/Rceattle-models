# What is left between Rceattle and SS3 on GOA Pacific cod

**Status:** measured at SS3's MLE, 2026-09-25. Two SS3 features Rceattle cannot
express account for all of the remaining difference. Neither is a bridge defect.

Forward-pass objective, GOA Pcod Model 19.1e (CAAL-corrected copy):

| | objective |
|---|---|
| SS3, as the assessment runs it | 2051.98 |
| SS3, same parameters, age-0 selected | 2128.82 |
| Rceattle | 2158.22 |

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
