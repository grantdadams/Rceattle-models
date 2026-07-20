# m23_rceattle_nobnd — STAGED, NOT BUILT, NOT RUN

Copy of `../m23_rceattle` (tpl/dat/control only), created 2026-07-15 to test matching
ADMB's parameter **bounds** to Rceattle's (i.e. removing them). **No edits applied.**

## Why it was staged, and why it was NOT run

The original rationale — "Rceattle leaves 949 selectivity devs unbounded while ADMB
holds them at ±5" — was **refuted before any edit was made**. Checking
`../m23_rceattle/pm.par` for parameters at their bounds:

```
sel_devs_fsh  n=720  max|val| = 1.4531  bound ±5    at-bound: 0   headroom 3.55
sel_devs_ats  n=210  max|val| = 0.7279  bound ±5    at-bound: 0   headroom 4.27
log_rec_devs  n= 61  max|val| = 1.4821  bound ±10   at-bound: 0   headroom 8.52
log_initdevs  n= 14  max|val| = 1.2258  bound ±15   at-bound: 0   headroom 13.77
log_F_devs    n= 61  max|val| = 1.7508  bound ±15   at-bound: 0   headroom 13.25
```

**Zero ADMB parameters are at a bound** — its optimum is deeply interior. Matching the
bounds would be a no-op at the solution, so this is NOT a candidate explanation for
Rceattle's failure.

## The one question it could still answer

ADMB's `init_bounded_*` internally **transforms** each parameter onto an unbounded
scale — implicit preconditioning. TMB + `nlminb` uses raw parameters with box
constraints and does no such rescaling. So removing ADMB's bounds tests whether that
*transform* (not the constraint) is why ADMB converges in 25 s where Rceattle takes
4593 s. That is a question about **optimizer geometry**, not model spec.

If pursued, the lines to change (`pm.tpl`) are:
```
1235  init_bounded_vector log_initdevs(2,nages,-15.,15.,3)                    -> Rceattle: [-1000, 23]
1236  init_bounded_vector log_rec_devs(styr,endyr_r,-10.,10.,phase_rec_devs)  -> Rceattle: ±15
1275  init_bounded_vector log_F_devs(styr,endyr_r,-15.,15.,2)                 -> Rceattle log_F: [-1000, 10]
                                                                                 (log_F = log_avg_F(-1.6) + dev)
1282  init_bounded_matrix sel_devs_fsh(...,-5.,5.,...)                        -> Rceattle: unbounded
1284  init_bounded_matrix sel_devs_ats(...,-5.,5.,...)                        -> Rceattle: unbounded
```
**Do NOT unbound `sel_slp_bts` (0.001,5) or `sel_a50_bts` (0.1,8).** Those are a
*parameterization* difference, not a bounds difference: ADMB estimates the slope on the
**natural** scale (the bound enforces positivity), Rceattle estimates `log_sel_slp` on
the **log** scale (exp() enforces positivity, so ±Inf there is equivalent to (0,∞)).
Making them plain `init_number` would admit a negative slope — nonsense.
`sel_age_one_bts` is already `init_number` (unbounded) and matches Rceattle.

Delete this folder if the geometry question is not pursued.
