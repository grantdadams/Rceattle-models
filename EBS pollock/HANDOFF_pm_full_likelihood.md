# Handoff: expand the `pm.tpl` index likelihoods to full form

**Goal.** Make `ADMB/m23_rceattle_full` and Rceattle report the *same* likelihood
at the same parameters, with no correction applied on either side.

**Status.** The two models already agree. `2024/02-bridge.R` checks every
component at ADMB's MLE and all ten match to ~1e-5. But five ADMB index terms
are written as bare quadratic forms, so the bridge has to add the omitted
normalizing constants back before comparing. This handoff removes that step.

This is a reporting change, not a model change: adding a constant to the
objective leaves every gradient untouched, so the MLE does not move. The one
thing it does change is `tot_like`, which is why it has to be done deliberately
and re-bridged afterwards.

---

## What the bridge measures now

At ADMB's MLE, `Rceattle - ADMB` per index fleet against the predicted constant
`n * 0.5 * log(2*pi) + sum(log(sd))`:

| fleet | distribution | diff | predicted constant | residual |
|---|---|---|---|---|
| BTS   | MVN        |   0.00001 | (none — see below) | — |
| ATS   | Lognormal  | −14.52049 | −14.52054 |  0.00004 |
| ATS_1 | Lognormal  |  16.54146 |  16.54089 |  0.00057 |
| AVO   | Normal     |   0.68452 |   0.68453 | −0.00001 |
| CPUE  | Normal     |  92.96581 |  92.96581 | −0.00000 |

The residual column is the point: the entire discrepancy is the constant. Nothing
about the model differs.

Composition (`age_like`) and the recruitment penalties (`rec_like(2)`, `(4)`)
already match without correction and need no edit.

### Rceattle >= 5.9.0 breaks this for AVO and CPUE

The table above was measured before Rceattle made the natural-scale normal index
likelihood **left-truncated at zero** (`Index_distribution = "Normal"`; see R1 in
the reconciliation log). An index cannot be negative and `data_check()` will not
accept one, so the density is renormalized over `(0, Inf)` and each fitted
observation gains `+ log(Phi(mu / sigma))`. ADMB's `avo_like` / `cpue_like` have
no such term and are not going to grow one.

That matters for this handoff specifically, because the plan rests on the whole
gap being a *constant*: add the omitted normalizing constants in ADMB and the two
models report the same number, with the MLE untouched. The truncation term is not
a constant. It depends on `mu = q * pred`, so it carries a gradient, it cannot be
added on the ADMB side by a reporting change, and it moves the Rceattle MLE
slightly rather than only the reported total.

Size, using the 18 `ob_avo_std` values against a prediction at the observed AVO
scale:

| AVO prediction | sum log Phi(mu/sigma) | worst single year |
|---|---|---|
| mu ~ 1.74 (observed scale) | -0.0209 | -0.0145 |
| mu = 1.2 | -0.1634 | -0.0680 |
| mu = 0.9 | -0.5087 | -0.1380 |

Against an AVO residual of -0.00001 today, so at the nominal scale the term is
some three orders of magnitude larger than the agreement this table reports, and
it grows quickly in any year where the prediction falls toward its sigma. CPUE is
the same family and needs the same treatment; its magnitude has not been measured
here.

So before re-bridging: re-measure the AVO and CPUE rows under the current
Rceattle, and expect their residual to be `sum(log Phi(mu/sigma))` rather than
zero. If an exact ADMB comparison is wanted for those two fleets, recompute the
untruncated part from the reported `index_hat` -- `-sum(dnorm(obs, index_hat,
sigma, log = TRUE))` -- rather than reading `jnll_comp` row 1. The BTS (MVN),
ATS and ATS_1 (lognormal) rows are unaffected.

---

## The pattern to follow

`rec_like` was already expanded under **L1** — use it as the template
(`pm.tpl` ~3468):

```cpp
  if (active(log_rec_devs))
    rec_like(2) = norm2(log_rec_devs)/(2.*sigmaRsq)
                + double(size_count(log_rec_devs))*log(sigr)
                + double(size_count(log_rec_devs))*0.5*log(2.*M_PI);
```

i.e. `kernel + n*log(sigma) + n*0.5*log(2*pi)`.

---

## Edits

Five statements. Each currently contributes only the kernel; add `log(sd)` and
`0.5*log(2*pi)` per observation. Flag each with `MODIFIED (m23_rceattle_full)`
as the existing edits are, and give it an **L8** code in the reconciliation log
at the top of `2024/03-model-comparison.R`.

### 1. `surv_like(2)` — ATS biomass index (lognormal)

`pm.tpl:4018` and `pm.tpl:4024`, two branches of the same term:

```cpp
surv_like(2) += square(log(ob_ats(i)+.01)-log(eb_ats(i)+.01))/ (2.*lvarb_ats(i));
```

`lvarb_ats = square(lseb_ats)`, so the sd is `lseb_ats(i)`. Add per observation:

```cpp
  + log(lseb_ats(i)) + 0.5*log(2.*M_PI)
```

Both branches need it — the second (`ot_ats`/`et_ats`, using `lvar_ats`) is the
total-numbers form and takes `lse_ats(i)` instead.

### 2. `surv_like(3)` — ATS age-1 index (lognormal)

`pm.tpl:4041` and `pm.tpl:4046`:

```cpp
surv_like(3) = 0.5*norm2(log(oa1_ats+.01)-log(ea1_ats*qtmp +.01))/(age1_sigma_ats*age1_sigma_ats);
```

Single scalar sigma, so add once, scaled by the count. Mind the two branches:
`ignore_last_ats_age1` fits `1,n_ats_r-1` (L7), so `n` differs between them.

```cpp
  + double(n)*log(age1_sigma_ats) + double(n)*0.5*log(2.*M_PI)
```

Verification target: ATS_1 is the cleanest case — `age1_sigma_ats = 1`, so
`sum(log(sd)) = 0` and the whole constant is `n*0.5*log(2*pi)`. With n = 18 that
is 16.5409, against a measured gap of 16.5415.

### 3. `avo_like` — AVO (natural-scale normal, absolute sd)

`pm.tpl:4059`:

```cpp
avo_like += square(avo_dev(i))/(2.*obs_avo_var(i));
```

`obs_avo_var = square(obs_avo_std)`, so add per observation:

```cpp
  + log(obs_avo_std(i)) + 0.5*log(2.*M_PI)
```

### 4. `cpue_like` — Japanese CPUE (natural-scale normal, absolute sd)

`pm.tpl:4055`, same shape, `obs_cpue_var = square(obs_cpue_std)`:

```cpp
  + log(obs_cpue_std(i)) + 0.5*log(2.*M_PI)
```

This is the largest single correction (92.97) because the CPUE sds are on the
natural scale of the index (563–1342), so `log(sd)` is ~6.5 per observation.

### 5. `surv_like(1)` — BTS (MVN) — **decide, do not default**

`pm.tpl:3973`:

```cpp
surv_like(1)  = .5 * srv_tmp * inv_bts_cov * srv_tmp;
```

This one **already matches** Rceattle, because `Index_distribution = "MVN"` is
deliberately the bare quadratic form for exactly this reason. Two options:

- **Leave it.** Then BTS is the one term still reported bare, and the asymmetry
  has to be documented.
- **Expand it** to `0.5*(r' Sigma^-1 r + logdet(Sigma) + n*log(2*pi))` and switch
  the Rceattle fleet to `Index_distribution = "MVNORM"`, which is that full form.
  Both sides then carry the constant and the whole objective is comparable.

The second is tidier and is why `MVNORM` exists. It is a one-word change in
`2024/01-build-data.R` plus the `pm.tpl` edit, and it changes the *reported*
BTS value by `0.5*(logdet(Sigma) + n*log(2*pi))` without moving the fit.

---

## Verify

1. Rebuild and refit:
   ```
   cd ADMB/m23_rceattle_full && admb pm && ./pm -nox -iprint 150
   ```
   or run `2024/00-fit-admb.R`, which wraps this.

2. Confirm the MLE did not move. `pm.par`'s objective will change by the sum of
   the added constants, but the **parameter estimates and maximum gradient must
   not**. Before: `Objective function value = 736.724477801363`,
   `Maximum gradient component = 0.000346749367320379`.

3. Re-run `2024/02-bridge.R`. Then delete the constant correction from its
   likelihood block — `konst` should become 0 for every fleet and every row
   should still read `OK`. That deletion is the deliverable: it is what proves
   the two likelihoods are stated identically rather than reconciled after the
   fact.

4. Update the L-codes in `2024/03-model-comparison.R` and the bridge-fidelity
   section of `README.md`.

---

## Blocker on Windows

The ADMB rebuild does not currently work on this machine:

```
Error: Unable to find ADMB library 'libadmb-mingw64-g++14.a'
```

ADMB 13.2 ships no library built against the g++14 that rtools45 provides, and
the committed `pm` binary is macOS/arm64, so it cannot be run here either. Do
this work on the Mac, or install an ADMB build matching the local compiler.
`00-fit-admb.R` checks for the executable after building rather than trusting
the exit status, because `admb` returns 0 even when the build fails.

---

## Not covered by the bridge either way

Two ADMB terms have no Rceattle counterpart in the check, because the forward
pass injects realized selectivity through `emp_sel` and so never evaluates the
selectivity penalties:

- `sel_like` — 17.48
- `sel_like_dev` — 172.07

Closing that gap is a separate job: it needs ADMB's `sel_coffs_*` / `sel_devs_*`
mapped into Rceattle's `sel_coff` / `sel_coff_dev`, respecting the shared
`Selectivity_index` blocks (Fishery+CPUE share block 1, AVO+ATS share block 2)
and the year-1 pinning. Worth doing before anyone claims the bridge is complete;
not needed for the likelihood-statement work above.
