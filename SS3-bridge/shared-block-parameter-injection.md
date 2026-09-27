# A shared parameter block starts at the MEAN of its members' values

**Found 2026-09-26 on GOA Pacific cod. It cost the Srv survey 18% of its
predicted index and 6.23 nats, and nothing in Rceattle or the bridge raised a
word.**

## The rule

Fleets sharing a `Selectivity_index` or a `Catchability_index` share ONE
parameter block (CLAUDE.md rule 10). TMB collapses a shared parameter to the
**mean of its members' starting values**, and these are held on the log scale, so
the block starts at the **geometric mean** of the members' values. No fleet keeps
the value in its own row.

Rceattle states this, and warns about it, for the deviation standard deviations
-- `.warn_shared_dev_sd()`, `R/3-build_map.R:110-136`. **Nothing warns for the
parameters themselves**: not `index_log_q`, not the selectivity base parameters.
Anything that writes starting values per fleet, rather than per block, is exposed.

## How it bit

`split_fleets_by_ageerr()` splits a fleet whose age rows span two SS3
ageing-error definitions. The new fleet inherits the parent's
`Selectivity_index` and `Catchability_index`, so the split costs no parameters --
which is the point. But the forward pass injected catchability by looking up the
fleet's own SS3 name:

```r
q <- gp(parlist$Q_parms, sprintf("LnQ_base_%s\\(%d\\)$", name[i], ss3_num[i]))
```

`Srv_ae1(10)` is a converter invention with no SS3 fleet, so the lookup returned
`NA`, its `index_log_q` stayed at the default 0, and the shared block started at

```
exp(mean(log(1.496398), log(1))) = 1.223270 = sqrt(1.496398)
```

Every one of Srv's 16 predicted index values came out a uniform factor 0.817477
low. The standard deviations were right to 5e-07, the length comps agreed to
0.0002 and the CAAL to 0.0007, so nothing else pointed at it.

| | before | after |
|---|---|---|
| `index_q`, Srv | 1.223270 | **1.496399** (SS3: 1.496398) |
| Srv index residual vs SS3 | +6.2346 | **−0.00006** |
| LLSrv index residual | +9.9645 | +9.9645 (the `EnvExp` gap, unchanged) |
| forward-pass objective | 2015.1665 | **2008.9309** |

Selectivity escaped only by accident: the converter already injected it by
`fleet_meta$ss3_src`, the source fleet's index, added earlier for an unrelated
reason. Had it not, the same averaging would have moved the selectivity curve.

## What was changed

- **Inject by block, not by fleet.** `fleet_meta` gained `q_src`
  (`Catchability_index`), and the q loop resolves the block's source fleet before
  the lookup.
- **A hard check, not a warning.** After injection, any catchability block whose
  Survey members disagree by more than 1e-8 in log q stops the run and names
  them. Averaging is silent, so the guard cannot be.
- **The invariant is stated where the split happens**, and
  `split_fleets_by_ageerr()` now says on creating a fleet that its selectivity
  and catchability must be injected by block.

## Worth fixing in Rceattle

`.warn_shared_dev_sd()` has the right idea and the wrong scope. The same warning
belongs on `index_log_q` and on the selectivity base parameters, for any group
whose estimated members arrive with different starting values. A user who gives
two fleets one `Catchability_index` and two different starting catchabilities
today gets the geometric mean and no message.

## How this was found

Not by the fit failing -- it did not. An adversarial review noticed that
`GOA-remaining-gap.md`'s accounting recorded Rceattle's Survey component as
52.3850 while the current head reported 58.6197, and asked which was right. The
answer was that the note was correct and the code had regressed under it. **An
accounting that no longer reproduces is a defect report, not a stale document.**
