# GOA Pacific cod: Lbin_method and the CAAL length columns

Five runs of GOA Pcod Model 19.1e on **SS3 v3.30.25.1** (the current release), as
forward passes at **one common set of parameters** -- same `Model19_1e.ctl`, same
`ss3.par`, same `forecast.ss`, `init_values_src = 1` and
`last_estimation_phase = 0`. Only the CAAL length columns and `Lbin_method` vary,
so every difference below is the data reading and nothing else.

The CAAL rows are labelled on the **5 cm data length bins** (21 bins,
`4.5 9.5 ... 104.5`). The **population** bins are **1 cm** (105 bins, `0.5` to
`104.5`). That mismatch is what makes this matter.

| run | `Lbin_method` | columns 7-8 | bins SS3 used | Age_comp | TOTAL |
|---|---|---|---|---|---|
| **D** | 1 | `34.5 34.5` | `3.5-3.5`, `8.5-8.5`, `13.5-13.5` -- **1 cm low** | 721.519 | 2068.39 |
| **A** | 3 | `34.5 34.5` | `4.5-4.5`, `9.5-9.5`, `14.5-14.5` | **741.112** | **2087.98** |
| **C** | 1 | `35 35` | `4.5-4.5`, `9.5-9.5`, `14.5-14.5` | **741.112** | **2087.98** |
| **B** | 1 | `35 39` | `0.5-8.5`, `9.5-13.5`, `14.5-18.5` | **916.192** | **2263.06** |
| **E** | 3 | `34.5 38.5` | `4.5-8.5`, `9.5-13.5`, `14.5-18.5` | **916.192** | **2263.06** |

B's first cell is `0.5-8.5` and E's is `4.5-8.5` -- that is point 5 below, the only
place the two differ.

Every other component is identical across all five -- Catch 1.21e-12, Survey
-0.972632, Length_comp 1340.79, Recruitment -2.62328, Parm_priors 1.15484 --
which is the check that nothing but the CAAL reading moved.

## What the runs show

**1. `Lbin_lo` and `Lbin_hi` are both LEFT EDGES.** They name the first and last
bin of a range; they do not delimit an interval. So the five 1 cm bins covering
the 34.5-39.5 data bin are written `34.5 38.5` as lengths, or `35 39` as bin
numbers. Writing `34.5 39.5` would name a **sixth** bin, because 39.5 is the left
edge of the next data bin.

**2. The two addressing methods are exactly equivalent.** A = C (single bin) and
B = E (five-bin span), each agreeing on every likelihood component. So
`Lbin_method = 3` with lengths and `Lbin_method = 1` with population bin numbers
are two spellings of the same thing. Either is fine.

**3. The truncation is still present in v3.30.25.1.** Run D is the file as the
assessment ships it: `Lbin_method = 1` with *lengths* in the columns. SS3 reports
the bins one cm below their labels, because under method 1 there is no
length-to-bin conversion -- the value is used directly as a bin index and the
decimal is dropped there. Worth 19.6 nats of `Age_comp` here. Method 3 is immune
because it converts first, by matching the value against the population bin
lower edges.

**4. "Corrected" is ambiguous, and the two versions are different models.**
A 5 cm data bin covers five 1 cm population bins.

- **C / A** put each row on the **single** bin at the data bin's lower edge.
- **B / E** put it on **all five** bins the data bin covers.

On the pristine 2024 control file, re-estimated, these give **+1.97%** and
**+5.11%** on the 2025 OFL respectively. B re-estimates to a likelihood of
2051.98; the 2263.06 above is B evaluated at the *original* model's MLE, not its
own, so it is not a fit comparison.

**5. B treats the extreme bins as accumulation bins; E does not.** B writes the
lowest data bin `1 9` -- a minus group reaching down to 0.5 cm -- and the top bin
`105 105`, a plus group clamped at the last population bin. E takes the strict
five-bin span, `4.5 8.5`. Every interior bin is a clean span of 5 in both. The
difference is worth nothing to six significant figures, because fish below
4.5 cm carry essentially no predicted abundance, but it is a choice worth being
explicit about.

## The CAAL grid does not have to match the length-comp grid

Worth knowing before choosing between C and B: `Lbin_filter` is dimensioned on
the **population** bins (`nlength2`), not the length-comp data bins
(`nlen_bin`), and the expectation is formed as `age_exp = exp_AL * Lbin_filter`.
So each CAAL row's length cell is specified per observation on the population
grid, independently of how the length compositions are binned. Rows may differ
in width from each other and from the length comps, and they need not tile --
they can overlap or leave gaps. The only constraints are that a row's bins form a
contiguous run, and that the **age** dimension is shared (`#_N_agebins`, 10
here).

`Lbin_method` only changes the spelling: 1 addresses population bins by number,
3 by lower edge (which must match exactly), and 2 by **data** bin number -- method
2 being the one that does tie CAAL to the length-comp grid.

So the 5 cm CAAL cells are a modelling choice, not something the format forces.
The same otoliths could be binned on the 1 cm population grid, which each aged
fish's measured length supports, at the cost of smaller per-bin sample sizes.

## Where to look in the output

- **`Report.sso`** -- search **`FIT_AGE_COMPS`**. One row per age observation;
  `Lbin_lo` and `Lbin_hi` are columns 12 and 13. Marginal age comps show the full
  range (`0.5` to `104.5`); CAAL rows carry specific bins.
- **`CompReport.sso`** -- search **`Lbin_lo`** for the header, then read the rows
  whose `Kind` column is `AGE`.

In both files the number SS3 prints is the **length of the bin it actually
used**, not the value in the data file. So comparing the two is the test: if the
reported bins sit below the data file's labels, the columns were truncated.

## Reproducing

Each `.dat` here goes in a directory with `Model19_1e.ctl`, `ss3.par`,
`forecast.ss` and a `starter.ss` with `init_values_src = 1` and
`last_estimation_phase = 0`, all taken from `Data/goa_pcod`. Then `ss3 -nohess`.
`result_*.txt` holds each run's method, columns, likelihood components and the
bins it used.
