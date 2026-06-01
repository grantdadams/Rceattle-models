# SS3 ctl/starter edits to match Rceattle

Compact recipe of SS3-side configuration changes that turn off SS3
features Rceattle doesn't replicate (or implements differently), so
the two models share a likelihood structure that estimation can
converge to compatibly. Each item is the **Path 2** action from the
correspondingly numbered entry in
[Estimation_Differences.md](Estimation_Differences.md) — see that doc
for the full per-component analysis, Rceattle-side alternative
(Path 1), and current NLL magnitudes.

Already applied in `Data/goa_pcod-no init and ramp/`:

```diff
# Model19_1e.ctl
- 0.9112 # max_bias_adj_in_MPD    (Methot-Taylor ramp; #5)
+ -1     # max_bias_adj_in_MPD    (override to constant b=1)

# Lambdas block (before the -9999 terminator)
+ 18 1 1 0 1 # InitEQ_Regime_Phz1 lambda = 0  (#6)

# starter.ss
- F_Method = 1   # (or 2)
+ F_Method = 2   # F per-year emitted as parameters (lets Rce read them)
```

---

## SS3 edits still pending (per-entry pointer into Estimation_Differences.md)

| Diff # | SS3 ctl/starter edit | Why |
|---|---|---|
| **#4** | `F_Method = 3` in starter.ss (continuous Baranov instead of hybrid Pope's) | Matches Rce's continuous-F formulation. **Currently the dominant remaining gap (Catch +330 FP, +452 EST).** Trying this first is recommended (30 min effort) — see [HANDOFF_estimation_parity.md](HANDOFF_estimation_parity.md) Path 2. |
| #3 | Already closed cpp-side (kept here for reference) | Rce dropped `0.5·log(2π)` per obs to match SS3. |
| #5 | Already closed via `max_bias_adj = -1` above | Rce uses constant b=1; SS3 ramp now off. |
| #6 | Already closed via `lambda 18 = 0` above | InitEQ_Regime penalty now off. |
| #7 | Set per-block PR_type = 0 OR set lambda 13 = 0 to drop the M-block absolute-value prior | Rce's linkage system puts the prior on the OFFSET (`beta_linkage`), not the absolute block-replacement. Magnitude ~0.3 NLL on Parm priors. |
| #8 | Set sel-dev `dev_se` PHASE to negative (no prior on devs) OR set Parm_devs lambda to 0 | Rce's IID dev prior strength differs; turning off in SS3 isolates the gap. With BlockDev (#20) now wired this matters less. |
| #9 | Set per-param `dev_link = 0` (turn off SS3's `dev_seq × dev_se × (HI−LO)` scaling) | Rce uses raw additive devs. Hard to invert SS3's scaling from raw `parm_devs` values, so easier to disable in SS3. |
| #10 | Constrain SS3 to base + dev_seq only (no block_repl tier) — set Block_Fxn = 0 for all sel params | Rce's sel model is base + per-year dev; SS3's three-tier (base + block_repl + dev_seq) has no Rce analog. |

---

## Verifying parity

The **Grouped NLL comparison** block in
[ss3_to_ceattle_test.R](ss3_to_ceattle_test.R) prints SS3 vs Rce per
component every run. Expected behavior after every SS3 edit above:

```
Component             SS3      Rce      Diff
Survey index         -1.79    -1.44    +0.34   <- FP-noise band (✅ #3 closed)
Catch                 1.75      ~       ~      <- closes via F_Method = 3 (#4)
Length comp        1336.33      ~       ~      <- closes downstream of #4
Age/CAAL comp       721.20      ~       ~      <- closes downstream of #4
Recruitment dev     -16.32      ~       ~      <- closed via max_bias = -1 (✅ #5)
Init eq            ~0          ~       ~      <- closed via lambda 18 = 0 (✅ #6)
Parm priors           0.79      ~       ~      <- closes via dropping #7
Parm devs (sel+q)     6.09      ~       ~      <- magnitude OK after BlockDev (✅ #20)
```

For per-fleet breakdown of mismatches use
`cod_pcod_est$quantities$jnll_comp[, fleet_idx]` vs SS3
`ss3_rep$likelihoods_by_fleet`. Per-cell deviations are in the
`[Predicted ...]` diagnostic blocks if you re-enable them (they were
moved out of the FP test script during condensation — see commit
history for the removed §9b CRITICAL-QUANTITY block).

---

## Related

- [Estimation_Differences.md](Estimation_Differences.md) — every numbered diff, Path 1 (Rce code) and Path 2 (SS3 config) per entry, current NLL magnitudes.
- [HANDOFF_estimation_parity.md](HANDOFF_estimation_parity.md) — work order; explicit recommendation to try Path 2 for #4 first.
