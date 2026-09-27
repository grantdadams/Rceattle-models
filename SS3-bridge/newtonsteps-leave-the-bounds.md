# `newtonsteps > 0` can return a parameter outside its bounds

**Found 2026-09-26 on GOA Pacific cod, G3 estimation. Not a bridge defect — an
Rceattle fitting defect, reproducible on any bounded parameter that optimises to
its bound.**

## What happens

`fit_mod()` passes `build_bounds()`'s `L`/`U` to the optimiser
(`R/6-fit_mod.R:1442-1444`), so nlminb is bounded and respects them. The Newton
refinement that runs *after* nlminb is not: both code paths take a plain
unconstrained step,

```r
par <- par - solve(h, as.numeric(g))    # R/0-tmb_helpers.R:68, and TMBhelper's own
```

with no clamp to `lower`/`upper`. A parameter that nlminb parked on its bound is
therefore free to be pushed straight through it, and the value that comes back
is the one saved in the fit.

## Measured

GOA Pcod Model 19.1e, G3 `fixsel`, 225 free parameters, identical in every
respect but `newtonsteps`. L1 (length at the youngest age, `log_growth_pars`
slot 2) has a lower bound of 1e-3 cm, taken from SS3's own `L_at_Amin` range:

| | `newtonsteps = 3` | `newtonsteps = 0` |
|---|---|---|
| objective | 1940.0966 | 1940.0983 |
| L1 | **5.08e-05** (below the bound) | **1.00e-03** (at the bound) |
| max abs gradient | 0.263 | **0.00246** |
| `sdreport` | **failed, Hessian not positive definite** | returned |
| convergence status | **FAIL** | WARN |

The Newton steps bought 0.0017 nats and cost the feasible region, two orders of
magnitude of gradient, and the standard errors.

## Why it is worth a guard

The convergence battery reports this as `parameters_on_bounds`, whose message is
`"1 parameter(s) at a configured bound"` — a WARN, and the wrong statement: the
parameter is not at the bound, it is past it. `.check_bounds()` tests
`par <= lo + tol`, which a value below `lo` satisfies, so an out-of-bounds
result is silently folded into the at-the-bound case. Nothing in the output says
the fit left the range the model declared plausible.

Two things to fix, neither of which changes a converged fit's numbers:

1. **Clamp the Newton step**, or skip Newton refinement for parameters sitting on
   a bound. A step that leaves the feasible set is not a refinement.
2. **Separate the diagnostic.** `par < lo` and `par > hi` deserve their own
   record at FAIL, distinct from the legitimate at-the-boundary optimum that
   `parameters_on_bounds` is written for.

Also stale, and in a package where a comment must state current behaviour
(rule 8): `R/0-convergence.R:519` says *"Optimization is unbounded in
fit_mod()"*. It is bounded — `L` and `U` reach nlminb. That comment is what sent
this diagnosis down the wrong path for a while.

## Consequence for the bridge

Every G3 run and every profile here uses `newtonsteps = 0`
(`RCE_G3_NEWTON=0`, the default in `profile_g3_goa.R`). L1 genuinely wants to sit
at its lower bound — that part is a real disagreement with SS3, not an artifact,
and is profiled separately.
