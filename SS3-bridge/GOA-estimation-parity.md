# GOA Pacific cod, G3: where Rceattle's own optimum sits relative to SS3's

**2026-09-26, second pass.** An adversarial review found the first version of this
note wrong in several places; the numbers below are the corrected ones, and the
review also led to a real defect in the ageing-error fleet split (see
`shared-block-parameter-injection.md`). What changed and why is at the end.

Run it with `Rscript Bridging/run_g3_goa.R fixsel` from the `GOA cod` folder.

## The comparison is conditional, not a contest between two fits

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
- `EnvExp`, SS3 environmental link type 1, for LLSrv catchability.
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
