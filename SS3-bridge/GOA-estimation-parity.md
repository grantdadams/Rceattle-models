# GOA Pacific cod, G3: where Rceattle's own optimum sits relative to SS3's

**2026-10-09: forward-pass parity holds; ESTIMATION PARITY IS NOT ESTABLISHED.**
Everything from "The comparison is conditional" down is the SUPERSEDED earlier
state — a 225-parameter conditional fit whose premises no longer hold, kept for
the provenance of its numbers. Read this section instead.

> **An earlier version of this section claimed "estimation parity reached, the
> two optima agree to 0.048 nats". That claim was wrong and is retracted.** It
> was measured on a fit that holds 55 of 60 `sel_dn6` entries — including base
> selectivity parameters SS3 estimated — at SS3's injected values, and reported
> it as "Rceattle's own optimum". Free them and Rceattle lands 10.28 nats
> *below* SS3, not level with it. The two numbers and why neither is parity are
> below. Three adversarial reviews found this; two found it independently.

## 2026-10-15: matched at 330 free parameters, agreeing to 0.036 nats

Run `compare_cod_models.R` from the `GOA cod` folder; it sets its own
configuration. Both sides now estimate **330** parameters, matching category by
category (F 134; recruitment deviations 48 + 10 `init_dev`; growth 5; M 2; q 3;
stock-recruit 2; selectivity 126).

| component, SS3 kernel units | Rceattle | SS3 | diff |
|---|---|---|---|
| Length_comp | 1361.0842 | 1361.1100 | −0.026 |
| Age_comp (CAAL) | 728.0906 | 728.0110 | +0.080 |
| Survey | −5.6343 | −5.4940 | −0.140 |
| Recruitment | −16.9353 | −17.0211 | +0.086 |
| Catch | 1.6252 | 1.6470 | −0.022 |
| selectivity devs | 6.5298 | 6.5465 | −0.017 |
| expressible priors | 0.0771 | 0.0756 | +0.001 |
| **prior, M 2014-16 block** | **0.0000** | **0.8944** | **−0.894** |
| **`Parm_softbounds`** | **0.0000** | **0.0130** | **−0.013** |

Objectives: Rceattle 1859.221433 (max|grad| 9.4e-04), SS3 **2075.77885793** —
read from `ss3.par`, not from summing `Report.sso`, whose 6-significant-figure
components overstate the total by +0.0036. Net of the two penalties SS3 charges
that Rceattle cannot express, **the optima agree to −0.036**, with an uncertainty
of about ±0.004 that is SS3's report rounding rather than model disagreement.
`Length_comp` prints as `1361.11`, so that row alone is known only to ±0.005.

**What closed the last gap**, and the honest cost: the bridge derived its map
from a skeleton model built with `inits = NULL`, where `build_params()` defaults
both double-normal ends to the −999 sentinel, so `build_map()` held
`Size_DblN_end_logit` for FshPot and LLSrv — two parameters SS3 estimates at
phase 2. Building the map from the injected values instead frees them; they land
within 5.3e-4 and 2.2e-3 of SS3's values with real curvature, and the forward
pass is bit-identical. **But total agreement got worse, not better**: the
previous 328-parameter fit sat at −0.0012, and four components (Survey, Catch,
Recruitment, and SSB) degraded. That near-exact total was offsetting errors from
holding two parameters SS3 estimated, not parity; −0.036 at a matched parameter
count is the more honest number, and it is 1.7e-5 relative on a 2075-nat
objective.

**Still open.** Both fits report convergence `FAIL`: the Hessian is not
positive-definite (5 negative eigenvalues, condition number 7.1e14) and six
`beta_linkage` selectivity coefficients are genuinely rank-deficient. That
predates this change and is unaffected by it. SS3's own `covar.sso` in this run
carries no covariance rows, so there is no evidence its Hessian inverted either —
do not frame this as Rceattle failing where SS3 succeeds. And nothing has
simulated from Rceattle at SS3's parameters and refitted, so the two end
parameters agreeing with SS3 is agreement with one point estimate, not
demonstrated identifiability.

## Neither configuration compares the same parameter space

The obstacle is structural, and naming it first makes the two fits legible.
**SS3's time blocks REPLACE the base parameter; Rceattle's linkage ADDS to it.**
Every blocked selectivity parameter in `Model19_1e.ctl` carries `Blk_Fxn = 2`,
and SS3's own generated labels say so — `SizeSel_P_1_FshTrawl(1)_BLK2repl_1990`.
Under *replace*, the base governs only the years no block covers. Under
Rceattle's additive identity linkage the base enters every year, so the base and
the block coefficients are confounded wherever the blocks tile the rest of the
series — and block pattern 2 (FshTrawl, FshLL) covers 1990-2024 with no gap,
leaving the base's own years 1977-1989, which is exactly where their deviations
operate.

The bridge reproduces SS3's *realised* selectivity exactly by setting each block
coefficient to `SS3_value − base`, which is why the forward pass matches on all
24 G1 gates. It does not reproduce SS3's *parameterisation*, and estimation
compares parameterisations. So there are two fits and neither is parity:

| | held base (the earlier headline) | freed base |
|---|---|---|
| objective | 1859.3075 | **1849.8913** |
| free parameters | 322 | 336 |
| free `sel_dn6` of 60 | 5 | 19 |
| in SS3 units | 2074.9234 | **2065.5074** |
| **vs SS3's 2075.7825** | **−0.8591** | **−10.2751** |
| non-identifiable | 13 | 18 |
| convergence status | FAIL | FAIL |

Holding the bases makes the pin binding — freeing them gains **9.4162 nats**, so
the 0.859 "agreement" sat inside a pin 11× larger. Freeing them buys 18
non-identifiable directions instead, because base and block are confounded. The
honest reading is that **Rceattle fits these data better than SS3 does** — at the
freed-base optimum every data component improves (Length −4.83, CAAL −3.06,
Survey −2.22, Catch −0.19, Recruitment −0.26) — but it does so in a
parameterisation whose dimension is not comparable to SS3's, and in neither
configuration is the Hessian invertible.

### The held-base table, for the record

Scored in SS3's units — SS3 writes likelihood *kernels* where Rceattle evaluates
full densities, so each Rceattle row has its normalising constant removed. This
is the arithmetic the retracted claim rested on; the constants below are correct
and independently verified. What is wrong is calling the left column an optimum.

| component | Rceattle | SS3 | diff |
|---|---|---|---|
| Length_comp | 1361.6157 | 1361.1100 | +0.5057 |
| Age_comp (CAAL) | 727.4848 | 728.0110 | −0.5262 |
| Survey | −5.4204 | −5.4940 | +0.0736 |
| Catch | 1.6584 | 1.6470 | +0.0114 |
| Recruitment | −17.0141 | −17.0211 | +0.0070 |
| selectivity devs (`Parm_devs`) | 6.5246 | 6.5465 | −0.0219 |
| prior, VonBert K | 0.0695 | 0.0722 | −0.0027 |
| prior, L_at_Amax | 0.0013 | 0.0014 | −0.0001 |
| prior, base M | 0.0036 | 0.0021 | +0.0015 |
| prior, M 2014-16 block | **0.0000** | **0.8944** | **−0.8944** |
| `Parm_softbounds` | **0.0000** | **0.0130** | **−0.0130** |
| **total** | **2074.9234** | **2075.7825** | **−0.8591** |

The two bolded rows are penalties SS3 charges and Rceattle has no counterpart
for, and they are −0.9074 of the −0.8591, leaving +0.0483 across everything else.
**That +0.0483 is not evidence of agreement**: it is what a fit pinned at SS3's
own selectivity bases scores, and the pin is worth 9.4162 nats.

Reported objectives are Rceattle 1859.30748417 (max|grad| 9.3e-04) and SS3
2075.78246040 (its rows summed; the reported TOTAL rounds to 2075.78). The
215.615941913 between them is density constants:

```
data-row constants (parity_check.R:.ss3_constants())   -166.273435527
  Catch    134 scored rows   sum(log(Log_sd) + 0.5log2pi)   -265.5187971130
  Survey    50 scored rows   50 * 0.5log2pi                   45.9469266602
  Recruit   58 deviates      58 * 0.5log2pi                   53.2984349259
linkage-prior constants                                 -49.369846800
  63 sel devs at sd 0.2                                   -43.5014608915
  growth K    at sd 0.03                                   -2.5876193641
  growth Linf at sd 0.015                                  -3.2807665447
M prior   log(0.41) + 0.5log2pi                            0.027340414
TOTAL                                                  -215.615941913
```

**The constants are constants, not functions of the fit.**
`.ss3_recdev_offset()` has two fit- or configuration-dependent terms — the
deviates Rceattle penalises that SS3 leaves unestimated, charged at their current
values, and `(n_rec − sd_offset_rec) log sigmaR` — and subtracting a quantity
that moves with the fit would be circular. **Both are exactly zero here.** All 58
of SS3's deviates carry `biasadjuster = 1` (10 `Init_age` 1967-76, 1 `Early`
1977, 47 `Main` 1978-2024; the 15 `Fore` rows carry 0 and are not deviates), so
`sd_offset_rec` = 58 = `n_rec` and `main_recdev` spans every year, leaving no
unestimated deviate to charge. The Recruitment constant is therefore
`58 × 0.5 log(2 pi)` = 53.2984349259 and nothing else. Measured identical at both
the prior-on and headline fits. Catch and Survey read row counts and observation
sd's only, never a parameter. **This is a property of THIS model's SS3
configuration, not of the harness** — turn on SS3's bias-adjust ramp or shorten
`main_recdev` and the figure silently becomes a function of the refitted
`rec_dev`, so recompute it rather than reusing 53.2984.

### Two premises the comparison rests on, neither stated before

**1. SS3's catch likelihood is not a plain lognormal, and the bridge matches it
by inflating the sd 10%.** `SS_objfunc.tpl:733` writes
`0.5 ((log(1.1 obs) − log(pred + 0.1 obs)) / se)²`, where Rceattle writes a plain
`dnorm(log(obs), log(pred), sd)`. The bridge sets `Log_sd` = 0.055 against SS3's
`catch_se` = 0.05 on all three fleets, which matches the first derivative exactly
but not the second. Measured at identical predictions the two forms differ by
**2.4e-05** over 134 rows — immaterial against +0.0483, but it grows with the
residuals, so it is a premise and not an identity.

**2. `sd_offset = 1`** (`Model19_1e.ctl:302`). That is what puts
`sd_offset · log(se)` in SS3's survey term and makes `sd_offset_rec` = 58. At
`sd_offset = 0` the Survey constant would need `sum(log(SE))` added and the
Recruitment constant `+58 log(0.44)` — a 142-nat error, silently.

SS3 writing kernels only is confirmed from source for all five components
(`SS_objfunc.tpl` lines 733, 132-134, 759/766, 1546, 917-941). `Parm_devs` is
the subtle one: `parm_dev_like(i,2)` is hard-coded to 0.0 for
`parm_dev_type == 1`, so the `log(se)` term is dropped *even though*
`sd_offset = 1`, and the surviving comment in SS3's source no longer describes
that. Kernel-only is what the code does, which is how it is treated here.

### The one structural gap: SS3's prior on the heatwave M block

SS3 applies `NatM`'s lognormal(−0.81, 0.41) prior to the block *replacement*
value `NatM_uniform_Fem_GP_1_BLK4repl_2014`, i.e. to the block's absolute M.
Rceattle's heatwave coefficient is a multiplicative log offset from a freely
estimated base, `M(yr) = M_base · exp(beta · heatwave)`, so the equivalent prior
is on `log M_base + beta` — a linear combination of two estimated parameters,
which the linkage table's per-row priors cannot express. It is not a defect;
it is a missing prior form. Recorded as `Estimation_Differences.md` #22.

Its effect is visible and in the predicted direction: unpenalised, Rceattle's
block M runs 1.9% above SS3's, which is why Rceattle buys +0.51 on the length
comps while giving back −0.53 on CAAL.

### Parameters, each at its own optimum

| parameter | Rceattle | SS3 | diff |
|---|---|---|---|
| M base | 0.460606 | 0.456845 | +0.82% |
| M 2014-16 block | 0.784543 | 0.769794 | +1.92% |
| VonBert K | 0.207788 | 0.207996 | −0.10% |
| L_at_Amax (Linf) | 99.460771 | 99.460800 | −3e-7% |
| L_at_Amin (L1) | 0.001 | 4.93e-08 | **both at their lower bound** |
| ln(R0) | 12.648522 | 12.619400 | +0.23% |
| log q, Srv(4) | 0.457198 | 0.462348 | −0.0052 — **non-identifiable** |
| log q, LLSrv(5) | 0.374093 | 0.378185 | −0.0041 — **non-identifiable** |
| LLSrv q env coefficient | 0.459243 | 0.455372 | +0.85% |
| 63 selectivity deviations | — | — | max 0.0133, cor 0.999944 |
| 48 recruitment deviations | — | — | max 0.0275 after centring |
| SSB 1977-2024 | — | — | max 1.36%, mean +0.41%, cor 0.999976 |

Three qualifications the table needs:

* **L1 is on a bound in BOTH models.** SS3's `L_at_Amin` is 4.92948e-08 with
  status `LO`, the only parameter in its 330 that is not `OK`, and its
  `warning.sso` flags it. Rceattle's lower bound is 1e-3. Length at age 1 moves
  0.0009 cm between the two against a 5 cm length bin, so it is numerically
  benign — but both implementations are pressing length-at-youngest-age to zero,
  which is how a model with age-0 fish newly selected keeps them out of the
  length comps. Parity is being measured in a degenerate corner of that
  parameter.
* **Both q's are flagged non-identifiable** by `check_estimability` at this
  solution, so their ~1% agreement is agreement along a direction the Hessian
  cannot resolve.
* **The recruitment deviations are differently centred, by exactly σR²/2.**
  `mean(rce − ss3)` = −0.09680035 against `0.44²/2` = 0.0968, agreeing to
  3.5e-07 — Rceattle centres the deviations at `−bias_adjust_proc·σR²/2` where
  SS3 reports them at zero mean. Removing that convention offset takes
  max|diff| from 0.115390 to **0.027475**. Quote the centred number; the raw one
  is 84% convention.

The selectivity deviations compare on the **standardized** scale: SS3's reported
`DEVmult` is the deviate divided by its `_dev_se` (0.2 for all five blocks),
while Rceattle's `beta_linkage` is the log offset itself. The two penalties are
the same object — `0.5 Σ (beta/0.2)²` = 6.524599 against `0.5 Σ DEVmult²` =
6.546524, SS3's reported `Parm_devs` being 6.546530. Compared on the raw scale
instead, the largest deviation reads 1.14 and means nothing.

### How to reproduce

From the `GOA cod` folder:

```
RCE_SS3_DIR="Data/goa_pcod_caal_bins_fixed_age0" \
RCE_SS3_PHASE_DIR="Data/goa_pcod_caal_bins_fixed_age0_est" \
RCE_SEL_PARITY=true RCE_FIX_LINKAGE=false RCE_SEL_DEV_ALL=true \
RCE_INIT_PRIOR=false Rscript run_goa_cod.R
```

Add `RCE_SEL_FREE_BASE=true` for the freed-base fit. That flag keeps the
intercept on the free-block linkage specs, which stops
`map_linkage_adjuster()` masking the base `sel_dn6` and pins the `(Intercept)`
row at 0 instead — the same contract the M heatwave linkage uses. The forward
pass is identical either way (1912.2067 both), since a pinned intercept
contributes nothing; only the map changes.

`RCE_INIT_PRIOR=false` is not optional for this comparison, and it is a matched
configuration rather than a knob turned for agreement. `Model19_1e.ctl:303-310`
carries a 5th lambda row, `like_comp 18` (`initEQregime`) `value 0`, and
`Report.sso` reports `InitEQ_Regime 0 0`. The regime parameter is still
estimated: `SR_regime_BLK5add_1976` sits at phase 1, status OK, gradient 2e-07.
So SS3 estimates the initial-level shift and charges nothing for it. Leaving
Rceattle's normal(0, sigmaR/(1/M − 0.5)) prior on costs 4.13 nats SS3 does not
pay and moves the optimum 7.5866 nats: with the prior ON the held-base total in
SS3 units is **2082.9055, i.e. +7.12 against SS3**, not −0.86. (An earlier
version of this line said "−42.6", which is not derivable from a 7.59-nat move —
it was a transcription of the `Linkage-table priors` jnll row, −42.7744.)

That `lambda = 0` row is **not** in the operational assessment. `Data/goa_pcod/`
reads "4 changes to default Lambdas" with no `InitEQ_Regime` line, so lambda = 1
and `Report.sso` reports `InitEQ_Regime 2.77603`. The bridge added the row. The
consequence is large: the initial-level shift goes from −0.678228 (0.51·R0) in
the assessment to −1.46651 (0.23·R0) here, and at the compared optimum the
penalty SS3 would have paid is 15.84 nats. Rceattle tracks whichever
configuration it is pointed at — prior off gives −1.457849 against SS3's
−1.46651, prior on gives −0.771997 against the assessment's −0.678228 — which is
the evidence that the prior form is right. But the comparison is against a
variant of the assessment, not the assessment.

### What is still open

* **Parameter-space parity, which is now the whole problem.** The 322-vs-330 gap
  is not 8 missing directions; it is **−18 base selectivity parameters pinned,
  +10 redundant `Srv_ae1` block coefficients**. Neither configuration fixes it:
  hold the bases and the pin is worth 9.4162 nats, free them and 18 directions
  go non-identifiable. A comparable parameter space needs the additive-vs-replace
  mismatch resolved — either a replace-semantics link, or restricting each
  block's design column to the years no later block covers so the base keeps its
  own years.
* **#21's 10 duplicated coefficients are exactly flat**, measured: the held-base
  Hessian has 9 eigenvalues at machine zero (min −1.675e-05, condition number
  1.5e22), **all nine spanned purely by `beta_linkage`**, seven of them the
  `s4p*` columns on fleet 10 (`Srv_ae1`). `Srv`/`Srv_ae1` share
  `Selectivity_index = 4` so they share one `sel_dn6` block, but the linkage
  table keys on `fleet`, so the shared block's columns are emitted twice and only
  their sum enters the model. The individual offsets are not estimable
  separately, which is why their reported values diverge by up to 20.89.
* **Neither fit converges by Rceattle's own standard.** Both report
  `status: FAIL` — `sdreport` fails (Hessian not invertible), so there is no
  `sdrep` and no standard errors, plus `parameters_on_bounds` (L1) and
  `estimability` (13 non-identifiable held-base, 18 freed-base). SS3's own
  `max|grad|` is 1.5e-06 against Rceattle's 9.3e-04.
* **The held-base fit is also start-dependent**, so even as a restricted
  optimum it is not pinned: a 5% jitter of the same 322-parameter model reaches
  1855.53, 3.78 nats below it. *(Measured by review; not re-run here.)*
* **The comparison is against a variant of the assessment, twice over.**
  Removing SS3's age-0 selectivity zeroing (#19) costs SS3 **23.80 nats** against
  its own structure (2075.78 against `bins_fixed`'s 2051.98, at the same 330
  parameters) and drives `L_at_Amin` to its lower bound; the `lambda = 0` change
  is worth ~15.8 nats to SS3. Both are documented and defensible as *code
  verification*. Neither supports a claim about reproducing the 2024 GOA Pcod
  assessment.
* **G2 has never been run at a valid point.** Its 83 is the gradient at the
  original model's MLE, not this one's.

## The comparison is conditional, not a contest between two fits

**SUPERSEDED 2026-10-09 — see above.** Two of this section's premises are no
longer true: the selectivity linkage offsets now carry an N(0, 0.2) penalty
matching SS3's `Parm_devs`, and both `SR_regime_BLK5add_1976` and
`LnQ_base_LLSrv(5)_ENV_mult` are now expressible (as the `R_init` and `q`
linkages respectively).

Selectivity is held at SS3's fitted values, because Rceattle's 866 selectivity
linkage offsets carry no penalty while SS3 penalises its `DEVmult`s
(`Parm_devs` = 6.49 at its own MLE); freeing them fits a looser model than SS3
ever did. That makes the two parameter sets **strictly nested**, and the exact
accounting is:

| class | SS3 | Rceattle |
|---|---|---|
| F (`F_Method = 2`, so F's are parameters) | 134 | 134 (`log_F`) |
| recruitment deviations | 58 | 58 (`rec_dev` 48 + `init_dev` 10) |
| selectivity base | 23 | 23 (`sel_dn6`) |
| growth + M | 7 | 7 |
| catchability | 3 | 2 |
| stock-recruit | 2 | 1 |
| **selectivity blocks + devs** | **103** | **0 — held at SS3's values** |
| total estimated | **330** | **225** |

The 105-parameter difference is exactly the 103 held selectivity parameters plus
`SR_regime_BLK5add_1976` and `LnQ_base_LLSrv(5)_ENV_mult`, neither of which
Rceattle can express. Every other class matches one for one. So the honest
statement is **"Rceattle is handed SS3's fitted time-varying selectivity as data
and estimates everything else"**, not "Rceattle fits a smaller model". The two
objectives are not comparable in level either -- density constants, `Parm_devs`,
`Parm_softbounds`, the missing M-block prior and the `init_dev` charge below all
sit between them -- so only differences within one model mean anything here.

| | value |
|---|---|
| objective, forward pass at SS3's MLE | 2008.93 |
| objective, Rceattle's own optimum | 1940.10 |
| max abs gradient | 0.0077 |
| terminal SSB vs SS3 | 91,276 vs 92,522 (−1.35%) |

| parameter | SS3 | Rceattle |
|---|---|---|
| M (base) | 0.4678 | 0.3045 |
| M (2014-16 heatwave block) | 0.7918 | 0.5067 |
| heatwave / base ratio | 1.693 | **1.664** |
| K | 0.2039 | 0.2078 |
| Linf | 99.4611 | **99.4602** |
| L1 | 1.3007 | 0.001 (a harness floor -- see below) |
| log(R0) | 12.7234 | 12.3568 |

Linf is pinned by SS3's own Normal(99.46, 0.015) prior, injected from the control
file, and lands 9e-04 away. The M block **ratio** agrees to 1.7% though both
levels are ~35% low, so the block structure is right and only the level moves.

## Three parameters disagree, not two

### 1. M: the composition wants it lower, by 1.32 nats

Profiled with everything else re-optimised (`profile_g3_goa.R M`), holding M at
SS3's value costs **0.90 nats in total**. That total is not the disagreement,
because SS3's lognormal M prior is centred at M = 0.4449 and therefore pulls
**toward** SS3's value:

| | M prior | total | likelihood alone |
|---|---|---|---|
| M = 0.3045 (Rceattle) | 0.4549 | 1940.098 | 1939.643 |
| M = 0.4678 (SS3) | 0.0349 | 1941.000 | 1940.965 |
| difference | **−0.4200** | +0.9017 | **+1.3217** |

So moving to SS3's M *releases* 0.42 nats of prior and the data pay 1.32,
**z = 1.62** on one degree of freedom. Restoring SS3's own M-block prior, which
Rceattle has no home for, adds another 0.94 in SS3's disfavour (0.9887 at SS3's
block M of 0.7918 against ≈0.05 at Rceattle's 0.5067), taking the penalised gap
to **1.84 nats, z = 1.92**.

And there *is* a component that wants a different M. At SS3's value, relative to
Rceattle's:

| component | Δ |
|---|---|
| **Composition (length comps)** | **+2.108** |
| Recruitment deviates | −0.904 |
| M prior | −0.420 |
| Initial abundance deviates | +0.164 |
| Index | −0.086 |
| CAAL | +0.035 |
| Catch | +0.004 |

The length compositions prefer M = 0.30 by 2.1 nats and the recruitment deviates
and the prior between them give most of it back. This is a real, if modest,
disagreement about M driven by the length compositions -- not the flat likelihood
the first version of this note described.

### 2. log(R0) differs by 31%, and it is a bridge artifact

log(R0) is 12.3568 against SS3's 12.7234 -- **R0 down 31%**. Terminal SSB is only
−1.35% out, but SB0, B40% and depletion scale with R0, so this is the
quota-relevant disagreement and the first version of this note listed it in a
table and never mentioned it again.

Decomposing the 68.83 nats Rceattle gains between SS3's MLE and its own optimum:

| component | Δ |
|---|---|
| **Initial abundance deviates** | **−37.26** |
| Composition | −32.06 |
| CAAL | −3.38 |
| Recruitment deviates | +4.17 |
| Index | −0.82 |
| M prior | +0.42 |
| Catch, linkage priors | +0.10 |

The single largest term is the `init_dev` penalty. That is
`GOA-remaining-gap.md`'s own open item: SS3 carries its regime shift as a free,
unpenalised parameter `SR_regime_BLK5add_1976 = −1.3879`, and the bridge folds it
into `init_dev` so the initial numbers pin exactly. Rceattle then charges it the
recruitment-deviate penalty. Because `init_dev` is
`log N_target − log R_init + Σ M`, the optimiser can retire that penalty by
lowering R0 -- and does, 0.37 log units, taking the penalty from 54.98 to 17.72.

**So R0 and SB0 disagree because of how the bridge parameterises the regime
shift, not because the two models disagree about recruitment.** Giving the regime
its own unpenalised parameter would close this and the +49.75 recruitment
residual together.

### 3. L1 runs to a harness floor, and the composition drives it

Rceattle's L1 profile falls monotonically and is 8.27 nats better at 0.001 than
at SS3's 1.3007. Two corrections to the first version of this note:

**The 1e-3 lower bound is the harness's, not SS3's.** `Model19_1e.ctl` declares
`L_at_Amin_Fem_GP_1` on **(0, 50)** with no prior. The bridge floors it at 1e-3
only because the parameter is log-scaled
(`ss3_to_ceattle_forward_pass.R`, `max(LO, 1e-3)`), and the fit sits at
`log(1e-3)` exactly. So L1 is not at a boundary optimum of a declared plausible
range -- **it runs to zero and is stopped by an arbitrary numerical floor**,
which is a loss of identifiability, not a disagreement about a value.

**The 8.27 nats is the composition data.** Per component, at L1 = 1.3007 relative
to the floor: Composition +4.94, CAAL +2.67, Index +0.86, recruitment deviates
+0.54, M prior +0.17, catch −0.04, and **initial abundance deviates −0.83**. So
this is not the `init_dev` pool of item 2 -- that term pushes the other way. It
is the length and age compositions, which is where SS3's age-0 selectivity
zeroing bites.

Sliced against **the age-0-selected SS3 variant**, which is the model Rceattle
represents, with everything else at SS3's MLE:

| L1 | SS3 (age-0) | Rceattle | difference | of which LLSrv `EnvExp` | residual |
|---|---|---|---|---|---|
| 0.0010 | +10.41 | +11.96 | 1.55 | 1.46 | **0.08** |
| 0.1000 | +6.62 | +8.04 | 1.42 | 1.35 | **0.07** |
| 0.5000 | **−3.67** | **−2.74** | 0.93 | 0.91 | **0.03** |
| 1.3007 | 0 | 0 | — | — | — |
| 3.0000 | +113.03 | +110.91 | −2.12 | −1.96 | **−0.15** |
| 6.3923 | +730.60 | +720.49 | −10.11 | −6.05 | −4.06 |

**Both models minimise at L1 = 0.5** and both prefer it to SS3's MLE value. And
both composition components agree across the whole grid, not just at the MLE:
max abs difference **0.0049** nats on the length comps and **0.0009** on the
CAAL, over four orders of magnitude of the parameter that sets the young end of
the age-length key. Component by component from L1 = 1.3007 to 0.5, the length
comp falls −14.99 in *both* models.

And SS3's age-0 variant does the same thing on a **profile**, which is the
comparison that settles it. `ss3_L1_profile.R` pins `L_at_Amin` at phase −1 and
lets SS3 re-estimate the other 329 parameters at each point:

| L1 | SS3 profile Δ | Rceattle profile Δ |
|---|---|---|
| **0.001** | **0.00** | **0.00** |
| 0.100 | +0.06 | +0.20 |
| 0.500 | +0.70 | +1.77 |
| 1.3007 (SS3's MLE value) | +3.97 | +8.27 |
| 3.000 | +20.10 | +27.75 |

**Both minimise at the lowest grid point and rise monotonically away from it.**
So L1 is unidentified downward in the age-0-selected model *itself* -- SS3 runs
it to zero too, once its age-0 zeroing is removed. That is the attribution
established on a like-for-like construction, not inferred from a slice.

The magnitudes are not comparable: SS3 re-estimates 329 parameters at each point,
including the 103 selectivity blocks and devs, so it can absorb some of the
change in selectivity where Rceattle, holding selectivity fixed, cannot. That
also shows up in where each model books the gain from 1.3007 down to 0.001 --
SS3 length +0.33 / age +3.49, Rceattle length +4.94 / CAAL +2.67. The direction
and the location of the minimum are the like-for-like part; the split between
components is not.

Two limits on the slice table above, which stand:

- The survey subtraction is bookkeeping, not evidence: it removes
  `LnQ_base_LLSrv(5)_ENV_mult`, an estimated SS3 parameter whose coefficient
  would re-fit with L1. It is a much smaller correction than before the
  catchability defect was fixed (0.91 nats at L1 = 0.5, was 4.64), but it is
  still a correction.
- The **original** model's slice rises +14.70 at L1 = 0.5 and +38.51 at 0.001
  (`_ss3_L1_scan_goa_pcod_caal_bins_fixed.rds`) where the age-0 variant's falls,
  which is the direct evidence that the age-0 zeroing is what holds SS3's L1 at
  1.3007.

## The accounting at the current head

At SS3's MLE, against the age-0-selected variant, after the density constants
SS3 drops:

| component | Rceattle | SS3 | constant | residual |
|---|---|---|---|---|
| Composition (length) | 1407.6398 | 1407.6400 | — | **−0.0002** |
| CAAL | 732.7703 | 732.7710 | — | **−0.0007** |
| Catch | −263.9869 | 1.5319 | −265.5188 | **−0.0000** |
| Index | 52.3849 | −3.5264 | 45.9469 | **+9.9644** |
| deviates (init + rec) | 85.9251 | −17.1218 | 53.2984 | **+49.7485** |

### Re-measured 2026-10-08 — both open residuals are CLOSED

The table above is kept as the record of what was open. Re-run at
`merge/dsem-v5-main-5.49.1` (5.52.0.9001) and, identically, at plain `afsc/dev`
5.53.0, against the same age-0-selected SS3 variant:

| component | Rceattle | SS3 | constant | residual | was |
|---|---|---|---|---|---|
| Composition (length) | 1407.6387 | 1407.6400 | — | **−0.0013** | −0.0002 |
| CAAL | 732.7701 | 732.7710 | — | **−0.0009** | −0.0007 |
| Catch | −263.9869 | 1.5319 | −265.5188 | **−0.0000** | −0.0000 |
| Index | 42.4204 | −3.5264 | 45.9469 | **−0.0001** | **+9.9644** |
| deviates (init + rec) | 36.1766 | −17.1218 | 53.2984 | **0.0000** | **+49.7485** |

**Max residual 0.0013 nats.** The Index closed with `link = "exponential"`
(PR #181), which this doc already predicted. The deviates residual, recorded
above as *attributed but not closed*, is closed too and nothing recorded it.

Forward-pass total at SS3's MLE is now **1962.1657**, not the 1998.9663 in
`HANDOFF.md`. That is the two closures, not a regression: 1998.9663 was the total
with the deviates residual still open.

**Two traps that make a closed bridge look broken.** Both cost a session:

1. **Pair the SS3 run.** `ss3_to_ceattle_forward_pass.R` defaults to
   `RCE_SS3_DIR=Data/goa_pcod_caal_lambda_on` (SS3 `TOTAL` 2058.00), but this
   accounting is `Data/goa_pcod_caal_bins_fixed` (2051.98). Scored on
   `lambda_on` the same five residuals come back at a max of **6.57**, not
   0.0013.
2. **The SS3 column here is the age-0-SELECTED variant**, whose components are
   saved in `_ss3_L1_scan_goa_age0.rds` at `L1 = 1.3007` -- not the raw
   `Report.sso` rows. `bins_fixed` reports `Length_comp` 1331.85 where this
   column has 1407.64; comparing against the raw row gives a spurious +75.79.
   No SS3 binary is needed to re-derive it, the scan file has it.

### What is still open

- **`Estimation_Differences.md` #19, age-0 selectivity.** SS3's
  `age_selex_pattern = 10` zeroes age-0 selectivity on all five length-selected
  fleets; Rceattle has no analogue. `Srv` carries 0.035657 there against SS3's 0,
  and that one value is every remaining G1 failure: Srv length comp 9.05e-02,
  survey index 1.02e-01, CAAL 5.97e-02, FshTrawl 2.35e-05. Ages 1-10 agree to
  1e-6 on every fleet. This is why the accounting needs the patched variant.
- **G2: max |gradient| 185 at SS3's MLE**, on `beta_linkage` (−185, −112, −104),
  `log_growth_pars` (−83, −66.7) and `growth_log_sd` (64.4). SS3's MLE is not a
  stationary point for Rceattle.
- **Three prior rows still have no mapping**: Rceattle `Linkage-table priors`
  7.1118 and `M prior` 0.0349 against SS3 `Parm_priors` 1.02845, `Parm_devs`
  6.49032, `Parm_softbounds` 0.0117358. The 7.11-against-6.49 pair looks like one
  quantity under two names; unmeasured.
- **`parity_check.R`'s NLL table reads raw `Report.sso` rows**, so it prints
  `Length_comp` residual +75.79. It should read the age-0 column, as the table
  above does.

plus three SS3 rows Rceattle has no analogue for: `Parm_priors` 1.0285,
`Parm_devs` 6.4903, `Parm_softbounds` 0.0117.

The Index residual splits cleanly by fleet: **Srv −0.00006** over its 16
observations, **LLSrv +9.9645** over its 34 -- the whole of it is the missing
`EnvExp` link. That this reproduces `GOA-remaining-gap.md`'s figures exactly is
the point: those numbers were right, and the code had regressed under them.

## Still open

- **The catch component's response to L1 parts company by up to 4.07 nats.** The
  residual column above is entirely the catch likelihood: +0.080, +0.065, +0.024,
  0, −0.159, −4.066. It is monotone in L1 and changes sign at SS3's MLE, so the
  two models disagree about ∂catch/∂L1 *at* the MLE, where the levels agree to
  1e-4. It is too small to move the L1 optimum (local curvature ≈ 78 nats/cm²
  against a gradient bias of 0.03-0.09 nats/cm, so ~0.001 cm), but it is a real
  unexplained structural difference -- weight-at-age integration over the length
  distribution, or catch timing -- and it will matter for a stock whose L1 is not
  1.3 cm. Note L1 = 6.3923 is **SS3's own control-file starting value**, well
  inside its declared (0, 50); calling it implausible, as the first version of
  this note did, was wrong.
- The 866 unpenalised selectivity offsets. A like-for-like free-selectivity
  comparison needs the devs as random effects with SS3's `dev_se` and the block
  replacements left free.
- The M-block prior does not map: SS3 puts Log_Norm(−0.81, 0.41) on the block's M
  **value**, where Rceattle's parameter is `log(M_block / M_base)`.
- ~~`EnvExp`, SS3 environmental link type 1, for LLSrv catchability.~~
  **Closed 2026-09-30.** Rceattle 5.47.0 added `link = "exponential"`, which is
  SS3's type 1 (`SS_timevaryparm.tpl:206`, `case 1: // exponential env link`),
  and `ss3_to_ceattle_forward_pass.R` now carries it:
  `control.ss_new` gives `LnQ_base_LLSrv(5)` an `env-var` of `101` -- type 1 on
  environmental variable 1 -- and SS3's coefficient is `0.517737`
  (`LnQ_base_LLSrv(5)_ENV_add` in the `.par`; `ss_summary.sso` calls the same
  parameter `_ENV_mult`, and SS3's type 1 is multiplicative, so r4ss's label
  misleads).

  Measured with `RCE_Q_ENV` on and off, so the lognormal constant cancels:

  | | total jnll | Index, LLSrv |
  |---|---|---|
  | without the link | 2008.9309 | 44.74738 |
  | with the link | 1998.9663 | 34.78278 |
  | closed | **9.9646** | **9.96459** |

  This table predicted `+9.9645`. The whole of it lands in `Index data` and on
  LLSrv: no other `jnll_comp` row moves by more than 1e-6. LLSrv's q runs
  1.2242-1.9374 about its base of 1.3851.

  Two notes for whoever reads a fitted coefficient here. Variable 1 covers
  1979-2024 and the model starts in 1977, so seven model years are filled with 0;
  that is required (the linkage refuses an NA fixed-effect covariate) and
  harmless, because LLSrv has no index observation before 1990. And `beta`
  multiplies `log q`, so it is only interpretable alongside its base -- a fit that
  crossed `q = 1` would carry the opposite sign for the same curve.
- The regime shift needs its own unpenalised parameter rather than a home inside
  `init_dev` (item 2 above).
- `parity_check.R`'s G1 does not run on GOA: its `growth_matrix` read assumes
  AI's single length-bin grid.
- **L1 is unidentified in the age-0-selected model**, in both codes. That is not a
  bridge defect, but it does mean the bridge's 1e-3 floor is load-bearing: a
  GOA-cod fit that frees L1 with age 0 selected will sit on whatever floor it is
  given. Closing the age-0 gap is what restores its identifiability.

## Three defects found in this work, all in my own code

1. **A shared parameter block starts at the MEAN of its members' values.** The
   ageing-error fleet split gave the new fleet the parent's `Catchability_index`
   but injected q by fleet name, so the split fleet kept a default and dragged
   Srv's q to `sqrt(1.4964) = 1.2233`. Srv's predicted index came out a uniform
   18.25% low, worth 6.23 nats, with nothing raised. Fixed; full write-up in
   `shared-block-parameter-injection.md`.
2. **`newtonsteps > 0` can return a parameter outside its bounds.** L1 came back
   at 5.08e-05 against a 1e-3 floor, gradient 0.263, Hessian non-invertible; at
   `newtonsteps = 0` it sits at the floor with gradient 0.0077 and `sdreport`
   returns. `newtonsteps-leave-the-bounds.md`.
3. **A scan harness must delete SS3's shipped outputs before running.** The model
   folders contain `Report.sso`, so a binary that fails to launch (a Windows
   `ss3.exe` on macOS exits 126) leaves `SS_output()` reading the MLE report and
   every point of a scan comes back identical and plausible. The first version of
   the L1 scan had all six points silently equal to the MLE.

## What the review corrected

Recorded because the errors are instructive, not for completeness: the M prior's
sign (it pulls toward SS3, so the data gap is 1.32 not "milder than 0.90"); the
claim that no component wants a different M (the length comps want 2.1 nats of
it); the omission of log(R0); the L1 bound's provenance and therefore its
interpretation; "~96" selectivity parameters (103); dismissing L1 = 6.3923 as
implausible when it is SS3's own starting value; and one sentence that compared
SS3's slice to Rceattle's profile after the note had said not to. The harness
also had no convergence check on profile points -- it now reports any point above
a gradient of 0.01, and M = 0.35 is one.

The review's own competing hypothesis -- that the L1 profile is the `init_dev`
pool -- did not hold: `init_dev` moves −0.83 over that profile while the
compositions move +7.61.

## The age-0 zeroing is removed from the bridged model (2026-10-08)

`Data/goa_pcod_caal_bins_fixed_age0/` is now a persistent SS3 directory: the bridged model with
`age_selex_pattern` 10 -> 0 on the five length-selected fleets, estimation off
(`last_estimation_phase 0`), parameters read from the original `ss3.par`. Built by
`../GOA cod/Bridging/mk_age0.R` with `Data/_iso_v25_base/ss3` (ADMB 13.2, arm64), using
`ss3_age0_fp.R`'s own `age0_patch()` verbatim. It reproduces the saved variant
(`_ss3_L1_scan_goa_age0.rds` at `L1 = 1.3007`) on every component to 1e-4.

**One gotcha, and it bit:** SS3 REWRITES `ss3.par` on exit, with different labels
(`SR_parm[1]` -> `SRparm[1]`) and a "Number of parameters = 1" header. The forward pass injects
from that file, so reading SS3's own output instead of the original MLE par moved the
Recruitment residual to +4.4511 and shifted N-at-age, SSB, recruitment, index and catch to
~4e-05. The original par is restored and SS3's output kept as `ss3.par.ss3out`. **Any patched
directory must carry the original MLE par** -- the point is one structural switch changed, not
one switch and a re-serialized par.

### Result: 23 of 24 G1 checks pass, and the NLL table needs no override

Scored with `RCE_SS3_DIR=Data/goa_pcod_caal_bins_fixed_age0` and the linkage coefficients
pinned, straight off the raw `Report.sso` rows:

| component | Rceattle | SS3 | constant | residual |
|---|---|---|---|---|
| Composition (length) | 1407.6387 | 1407.6400 | — | **−0.0013** |
| CAAL / Age_comp | 732.7701 | 732.7710 | — | **−0.0009** |
| Catch | −263.9868 | 1.5320 | −265.5188 | **−0.0001** |
| Survey | 42.4204 | −3.5264 | 45.9469 | **−0.0001** |
| deviates (init + rec) | 36.1766 | −17.1218 | 53.2984 | **0.0000** |

G1 went 19/24 to **23/24**. Every selectivity gate passes -- `Srv` 3.57e-02 -> **1.17e-06** --
and every length comp -- `Srv` 9.05e-02 -> **7.96e-07**, FshTrawl 2.35e-05 -> 9.45e-07. The
survey index went 1.02e-01 -> **4.85e-06**. So the age-0 difference really was the whole of it,
as `Estimation_Differences.md` #19 predicted.

### What is left

**1. `predicted CAAL`, 5.97e-02 -- the sole remaining G1 failure.** Unchanged by the age-0 patch,
so it is a separate cause: see #17 (CAAL pop-bin integration over the data bin) and
`CAAL-length-bin-defect.md`. Note the CAAL *likelihood* matches to −0.0009 while the predicted
CAAL *cells* are off by 5.97e-02, so the error cancels in the sum.

**2. G2 is still 83, and a correction to what I said about it.** Pinning the linkages took it
185 -> 83 and removed `beta_linkage`; removing the age-0 zeroing did not move it. It is now
`log_growth_pars` (−83, −66.7, 32.2), `growth_log_sd` (64.4, 10.4), `rec_pars` (44.6) and
`rec_dev` (10.4). Earlier in this work the growth gradient was attributed to #19; that is only
half right. The age-0 zeroing is why **SS3's** MLE puts L1 at 1.3007, but the patched model's
own MLE is NOT at 1.3007 either -- its L1 slice falls, which is what the scan measured. So the
gradient at 1.3007 is expected for both Rceattle AND the patched SS3 model, because 1.3007 is
the ORIGINAL model's optimum. **To test gradient parity, run the patched model with estimation
ON and compare at its own MLE.** G2 at 1.3007 cannot be driven to zero and is not evidence of a
discrepancy.
