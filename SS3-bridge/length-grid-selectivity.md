# Length-based selectivity is built on the data grid, where SS3 uses the population grid

**Status:** verified against SS3 v3.30.22.1 output and the Rceattle source.
**Affects:** any SS3 model whose composition length bins are coarser than its
population length bins. GOA Pacific cod is one (21 data bins at 5 cm over a
105-bin 1 cm population grid). AI Pacific cod is not — both its grids are the
same 1 cm — which is why AI reproduces SS3's selectivity-at-age to 4.5e-07 and
GOA does not.

## What differs

`src/TMB/selectivity.hpp:376-378`:

```cpp
int nbins     = is_length_based ? nlengths(sp) : nages(sp);
Type binwidth = is_length_based ? (lengths(sp, 1) - lengths(sp, 0)) : Type(1.0);
```

so a length-based curve is evaluated at the **data** bins. `rearrange_data()`
aggregates the age-length key to those same bins, so the two are consistent
with each other:

| array | dimensions (GOA cod) |
|---|---|
| `sel_at_length` | 9 fleets x 1 sex x **21** bins x 53 years |
| `growth_matrix` | 11 ages x 1 sex x 11 x **21** bins x 53 years |

SS3 evaluates size selectivity on the **population** bins and carries the
age-length key there too, aggregating to data bins only when it forms the
expected composition. Its `sizeselex` table has **105** length columns.

Both schemes are internally consistent, and they agree exactly when the two
grids are the same. They differ when the data bins are coarser, because
applying selectivity at 5 cm bin midpoints to an already-aggregated key is not
the same as applying it at 1 cm and aggregating afterwards.

`x_last` shows it directly. Rceattle takes `lengths(nbins-1) + 0.5 * binwidth`
= 104.5 + 2.5 = **107**; SS3's population grid gives **105**. `x_last` sets
`peak2`, which sets the whole descending limb.

## What it costs on GOA cod

Predicted length compositions against SS3's, at SS3's MLE:

| fleet | max abs | mean abs |
|---|---|---|
| 1 FshTrawl | 5.8e-03 | 7.0e-04 |
| 2 FshLL | 5.9e-03 | 1.0e-03 |
| 3 FshPot | 2.3e-02 | 2.9e-03 |
| **4 Srv** | **2.6e-01** | **1.6e-02** |
| 5 LLSrv | 2.2e-02 | 4.9e-03 |

Fleet 4's selectivity-at-age error is a **constant 0.137 across the early
years** rather than varying year to year, so it is not the block or deviation
machinery — it is the base curve. It bites hardest there because fleet 4 is the
only fleet with a real final floor (base P6 = -1.337, floor 0.21) instead of
P6 = 10 (floor ~1), so its curve moves fastest inside a 5 cm bin.

This is where the remaining 389 nats of GOA's forward-pass gap sit: the
composition (1655.76 vs 1331.85) and CAAL (885.02 vs 732.79) rows carry no
density constant, so those differences are differences in fit.

## Closing it

Needs an Rceattle change, not a bridge change: evaluate a length-based
selectivity on `lengths_pop` and aggregate through `pop_to_data_bin`, the way
`fill_age_length_key()` already builds the key. Until then a model with coarse
data bins cannot be bridged to SS3 exactly.

Not urgent for correctness of Rceattle's own fits — its scheme is
self-consistent, and a model configured entirely on one grid is unaffected. It
matters for SS3 parity, and for anyone reading a length-based selectivity curve
as if it were resolved at the population scale.

## A trap met on the way

`fit$data_list` is the **pre-`rearrange_data()`** list, so `$lengths` there
still holds the population grid. Reading it as the fitted grid makes this look
like a converter bug writing the wrong bins. The converter is right:
`caal_data$Length` resolves the corrected `35 39` population-bin numbers to
4.5, 9.5 ... 104.5, and `rearrange_data()` builds `lengths` from those.
