# Handoff: Populate `growth_matrix` from `age_trans_matrix` in empirical growth branch

Discovered 2026-05-22 during the GOA Pcod SS3→Rceattle estimation bridge.
The current behavior makes empirical growth incompatible with any CAAL-based
likelihood, which blocks faithful reproduction of an SS3 model that fits
both CAAL and length comps when the user wants to keep weight-at-age fixed.

## TL;DR

`calculate_weight()` in [Rceattle/src/TMB/growth.hpp](../../Rceattle/src/TMB/growth.hpp)
populates `weight_hat` from `weight_obs` in the empirical branch
(`growth_model == 0`) but never touches `growth_matrix`. The user-provided
`age_trans_matrix` data is read in via `DATA_ARRAY` but is only consulted by
the marginal-length-comp likelihood path, not for CAAL. The CAAL predictor
multiplies by `growth_matrix(...)` which is still at its zero-init, so
`pred_CAAL = 0` everywhere and the multinomial NLL becomes uninformative.

A current `data_check` guard (added in commit `5ade38c`) errors out for this
combination per-species, telling the user to either switch to parametric
growth or drop CAAL data. That stops the silent-bad-fit failure mode but
doesn't enable the legitimate "fixed empirical WAA + fit CAAL" workflow.

## Symptoms (reproducible on Pcod 2024 bridge)

```r
source("ss3_to_ceattle_test.R")  # the GOA Pcod test pipeline
# growthFun = build_growth(fun = 0), CAAL data present, M1Fun w/ post-2014 block
# Forward-pass (estimateMode = 3, all params at SS3 MLE):
#   R   max rel err: 2.61e-06   <- machine precision
#   Bio max rel err: 2.68e-06   <- machine precision
#   SSB max rel err: 3.75e-06   <- machine precision  (after Jensen fix)
#   CAAL NLL:        ~32,000    <- structurally bad
#
# Probe:
gm <- mod$quantities$growth_matrix
all(gm == 0)                            # TRUE   <- the bug
range(rowSums(gm[1, 1, , , 1]))         # [0, 0]  (should be [1, 1])

# Estimation (estimateMode = 1) then wanders to spurious minima:
#   log(R0) drifts ~30% low
#   log_F 5-10x SS3
#   M block beta = 0.22 vs SS3 0.51
```

## The fix

Update `calculate_weight()`'s empirical branches (one for pop / SSB, one for
fleets) to seed `growth_matrix` from the user-provided `age_trans_matrix`.

### Where

[Rceattle/src/TMB/growth.hpp:518-535](../../Rceattle/src/TMB/growth.hpp#L518)
(population / SSB) and lines 591-601 (fleet). Both currently do only
`weight_hat = weight_obs`.

### Sketch

```cpp
// 1.1. Empirical weight-at-age (population + SSB)
if (growth_model(sp) == 0) {
  int atm_idx = pop_age_transition_index(sp);   // need to plumb in
  for (int sex = 0; sex < nsex(sp); sex++) {
    for (int age = 0; age < nages(sp); age++) {
      for (int yr = 0; yr < nyrs; yr++) {
        int yr_ind = (yr < nyrs_hind) ? yr : (nyrs_hind - 1);
        weight_hat(wt_idx_pop, sex, age, yr) =
          weight_obs(pop_wt_index(sp), sex, age, yr_ind);
        weight_hat(wt_idx_ssb, sex, age, yr) =
          weight_obs(ssb_wt_index(sp), sex, age, yr_ind);

        // NEW: seed growth_matrix from the user-provided ALK so CAAL
        // and length-comp prediction paths see a populated transition.
        for (int ln = 0; ln < nlengths(sp); ln++) {
          growth_matrix(wt_idx_pop, sex, age, ln, yr) =
            age_trans_matrix(atm_idx, sex, age, ln);
          growth_matrix(wt_idx_ssb, sex, age, ln, yr) =
            age_trans_matrix(atm_idx, sex, age, ln);
        }
      }
    }
  }
}
```

And the same for the fleet branch at line 591 using
`flt_age_transition_index(flt)`.

### Signature changes required

`calculate_weight()` currently doesn't receive `age_trans_matrix`,
`pop_age_transition_index`, or `flt_age_transition_index`. Three options
(in order of cleanliness):

1. **Add them as parameters to `calculate_weight()`.** Cleanest. Update the
   call site at [ceattle_v01_11.cpp:698](../../Rceattle/src/TMB/ceattle_v01_11.cpp#L698)
   to pass them through. Need to update the template-instantiated function
   signature; otherwise mechanically straightforward.
2. **Populate `growth_matrix` outside `calculate_weight()`,** in
   `ceattle_v01_11.cpp` directly before the `calculate_weight()` call.
   Avoids touching `growth.hpp` signatures.
3. **Pass `age_trans_matrix` via a struct/list of references.** Overkill.

Pick (1) or (2). (2) is the more surgical fix.

## Regression test

Add to [tests/testthat/tests-Growth/](../../Rceattle/tests/testthat/tests-Growth/):

```r
test_that("growth_matrix is populated from age_trans_matrix at empirical growth", {
  dat <- make_test_data(nyrs = 6, nages = 5, minage = 0, seed = 42)
  # Make age_trans_matrix a non-trivial ALK so the assertion is meaningful
  # (the default is identity; we want to detect a real population, not just
  # that something non-zero was written).
  dat$age_trans_matrix[, paste0("Length_", 1:5)] <-
    matrix(c(0.7, 0.2, 0.05, 0.03, 0.02,
             0.2, 0.5, 0.2,  0.07, 0.03,
             0.05,0.2, 0.5,  0.2,  0.05,
             0.03,0.07,0.2,  0.5,  0.2,
             0.02,0.03,0.05, 0.2,  0.7), nrow = 5, byrow = TRUE)

  res <- fit_mod(data_list = dat, estimateMode = 3,
                 growthFun = build_growth(fun = 0),
                 fit_control = fit_control(phase = FALSE, verbose = 0))

  gm <- res$quantities$growth_matrix
  expect_false(all(gm == 0))           # was: TRUE  (the bug)
  # Rows (across length bins) should sum to ~1 for each (wtind, sex, age, yr)
  row_sums <- apply(gm[1, 1, , , 1], 1, sum)
  expect_true(all(abs(row_sums - 1) < 1e-6))
})
```

Once the fix is in, also relax the per-species data_check error added in
commit `5ade38c` (R/4-data_check.R, around line 460) — it can be downgraded
to a warning, or removed entirely.

## Then unblock the Pcod estimation bridge

With `growth_matrix` populated, the GOA Pcod bridge can stay on empirical
growth (so the Section 4e Jensen's-gap SSB fix still applies and the
forward-pass at machine precision is preserved) AND fit CAAL meaningfully.
Re-run `ss3_to_ceattle_test.R` after re-reverting the test script to
empirical growth (currently set to vonBertalanffy as a workaround), and
re-check the estimation MLE against SS3.

Expected outcome with the fix:
- Forward-pass: R/Bio/SSB stay at machine precision (no change there)
- Estimation: optimizer should converge near SS3's MLE for log_R0, rec_devs,
  init_devs, log_M1, beta_linkage, index_log_q. log_F may still need
  attention (high dimensionality), but the q-N tradeoff that lets log_F
  drift will be tamed by the now-informative CAAL gradient.

## Related work in this commit chain

- `c327582` (Rceattle): growth.hpp minage=0 segfault fix; auto-Off inactive
  fleets; data_check selectivity-without-comp guard
- `5ade38c` (Rceattle): the per-species empirical+CAAL data_check error
  that this handoff document references for the relax/remove step
- `cfcacf3`, `1c153a6` (GOA cod): converter updates and test script
