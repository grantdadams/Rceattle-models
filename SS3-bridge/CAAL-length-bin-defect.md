# Conditional age-at-length rows address the wrong length bins in the AI and GOA Pacific cod SS3 models

**Status:** verified against the Stock Synthesis source and reproduced by rerunning each model.
**Affects:** `AI cod - Dev/Data/M24_1*` and `GOA cod/Data/goa_pcod*`. All five November 2024 EBS
Pacific cod models write their columns the same way but are **not** materially affected — see the section below, which
is worth reading as the shape of a negative result. In general it affects any SS3 model that
writes lengths in the `Lbin_lo` / `Lbin_hi` columns of a conditional age-at-length (CAAL) row
under `Lbin_method = 1` or `2`, on Stock Synthesis 3.30.24 or earlier.

## Summary

In both cod models every CAAL observation is fitted against a prediction for the **wrong length
bins**. The observations themselves are fine; it is the expected values SS3 compares them with
that are misplaced.

| model | length bins the row is labelled with | length bins SS3 actually uses |
|---|---|---|
| AI cod | one 1 cm bin, e.g. 24.5 | **two** bins, 23.5 and 24.5 — and neighbouring rows overlap |
| GOA cod | 34.5, on a 5 cm data bin grid | **one 1 cm** population bin, at 33.5 |

**The off-by-one is certain in both. How wide a GOA row is meant to be is NOT**, and it changes
the size of the fix by 5x. See "What a GOA row is meant to span" below before quoting any GOA
number. The AI case has no such ambiguity: its data bins are already 1 cm, and the Rceattle
bridge confirms the corrected reading independently.

Correcting the AI model moves mean length-at-age up 0.2–0.5 cm and the 2025 OFL down 1.25%.
Correcting GOA moves the 2025 OFL up **1.97%** under the one-bin reading or **9.66%** under the
5 cm reading. Corrected copies are `AI cod - Dev/Data/M24_1_caal_bins_fixed`,
`GOA cod/Data/goa_pcod_caal_bins_1cm` and `GOA cod/Data/goa_pcod_caal_bins_fixed`; in each, only
the two CAAL length columns differ from the original, and rerunning each original reproduces its
archived likelihood exactly.

## Why it happens

SS3 reads the two CAAL length columns into **integer** containers, so a value written as `24.5`
becomes `24` before anything else looks at it. All line numbers are Stock Synthesis v3.30.22.1,
the version both models were run with.

1. `SS_readdata_330.tpl:2448-2449` — the containers are integer matrices:

   ```
   imatrix  Lbin_lo(1,Nfleet,1,Nobs_a);
   imatrix  Lbin_hi(1,Nfleet,1,Nobs_a);
   ```

2. `SS_readdata_330.tpl:2586-2587` — the data file's columns 7 and 8 are assigned into them from
   a double vector, which truncates: `24.5` becomes `24`.

   ```
   Lbin_lo(f, j) = Age_Data[i](7);
   Lbin_hi(f, j) = Age_Data[i](8);
   ```

3. `SS_readdata_330.tpl:2589-2600` — under `Lbin_method = 1` the values are **population length
   bin numbers** and are used with no further conversion. (Both cod data files set
   `Lbin_method = 1` and then write lengths, not bin numbers.)

4. `SS_readdata_330.tpl:2681-2684` — the row's length filter is set over the **inclusive** bin
   index range:

   ```
   Lbin_filter(f, j) = 0.;
   Lbin_filter(f, j)(Lbin_lo(f, j), Lbin_hi(f, j)) = 1;
   ```

5. `SS_expval.tpl:631` — the expected age composition for the row is the joint age x length
   expectation summed over **every bin the filter marks**:

   ```
   age_exp = exp_AL * Lbin_filter(f, i);
   ```

With population bins at 0.5, 1.5, 2.5 ... the bin at index *k* has lower edge *k* − 0.5. So a row
written `Lbin_lo = 24.5` is truncated to index 24, which is the bin at **23.5** — one bin below
what the file says.

`Report.sso` confirms the truncation, because it writes the columns back out as
`len_bins(Lbin_lo(f,i))` (`SS_write_report.tpl:2398` and `:4105`), i.e. the truncated bin's
length. In the GOA run all 21 distinct CAAL `Lbin_lo` values in `Report.sso` sit exactly 1 cm
below the values in the data file; the AI run does the same.

## What each model ends up fitting

**AI cod.** Every one of the 1160 CAAL rows has `Lbin_hi = Lbin_lo + 1` (e.g. `24.5 25.5`),
apparently intended as the lower and upper edge of a single 1 cm bin. After truncation that is
index range 24–25, so the cell spans **two** bins, 23.5 and 24.5. Because consecutive rows step
by 1 cm, neighbouring cells also **overlap by one bin**. `Report.sso` shows it directly: the
as-written run has `Lbin_lo` 11.5–114.5 against `Lbin_hi` 12.5–115.5, a 1 cm span, where the data
file's own `Lbin_lo` runs 12.5–115.5.

**GOA cod.** Population bins are 1 cm (105 of them) but the **data** length bins are 5 cm (21 of
them), and the CAAL rows are on those 5 cm bins, with `Lbin_hi = Lbin_lo`. After truncation each
row addresses a single **1 cm** bin, one bin below its label. So a row holding the ages of fish
measured between 34.5 and 39.5 cm is compared with the predicted age composition of the 1 cm bin
at 33.5 cm. This is the larger of the two errors, and it applies to all 827 CAAL rows.

## Effect on the AI assessment

`Data/M24_1_caal_bins_fixed` is `M24_1_adjusted` with only the CAAL length columns changed:
`Lbin_lo` and `Lbin_hi` both set to the population bin **number** holding that length
(`bin = length + 0.5`), which is what `Lbin_method = 1` asks for. 1160 lines change and nothing
else does — the control file, the executable and every other data row are identical. The
13 turned-off marginal age-composition rows are untouched.

`Report.sso` confirms the fix took: the corrected run's CAAL cells are single bins
(`Lbin_lo` = `Lbin_hi` = 12.5–115.5) sitting on the data file's own labels.

| quantity | as written | corrected | change |
|---|---|---|---|
| total likelihood | 531.003 | 532.903 | +1.900 |
| age composition (CAAL) | 402.473 | 404.423 | +1.950 |
| length composition | 140.059 | 139.838 | −0.221 |
| mean length at age 2 (cm) | 25.78 | 26.11 | +0.33 (+1.26%) |
| mean length at age 4 (cm) | 50.65 | 51.16 | +0.50 (+0.99%) |
| von Bertalanffy K | 0.2190 | 0.2154 | −1.66% |
| Richards shape | 0.4059 | 0.4379 | +7.88% |
| survey catchability q | 0.8801 | 0.8919 | +1.34% |
| R0 | 84 516 | 81 977 | −3.00% |
| terminal SSB (2024, mt) | 50 085 | 49 384 | −1.40% |
| SSB unfished (mt) | 210 600 | 208 200 | −1.10% |
| B2024 / B0 | 0.2379 | 0.2371 | −0.30% |
| 2025 OFL (t) | 20 600 | 20 340 | −1.25% |
| 2025 ABC / forecast catch (t) | 12 900 | 12 720 | −1.43% |

Growth is the quantity the CAAL data mainly inform, and it is the one that moves: length at age
rises 0.2–0.5 cm across ages 1–13. Stock status is nearly unchanged (depletion −0.30%), so the
tier and the status determination do not turn on this. The catch advice moves about 1.3%.

### A note on the one month-1 row, which is *not* an error

One of the 1160 AI CAAL rows (2002, `Lbin_lo` 100.5) is recorded at month 1 while the other 1159
and the survey index are at month 7. It is **not** a transcription error: 2002 has exactly 100
rows at month 7 and SS3 refuses more than 100 age-composition observations per fleet × time
(`SS_readdata_330.tpl:2514`). Setting that row to month 7 makes the model fail to start. It is a
workaround for that limit and should be left alone. The only side effect is that SS3 evaluates
those 4 fish against the January age-length key rather than the July one.

## What a GOA row is meant to span, and why the data cannot say

The off-by-one is certain. The width is not, and it is worth 5x.

GOA's `Lbin_lo` values are exactly its 21 5 cm data length bin edges and nothing else, which is
what first suggested each row holds a whole 5 cm bin. But a single 1 cm bin sitting on each data
bin edge fits the file equally well, and the two need different corrections:

| reading | fix for the 34.5 row | population bins used |
|---|---|---|
| one 1 cm bin at the label | `35 35` | 34.5 only |
| the whole 5 cm data bin | `35 39` | 34.5 through 38.5 |

**The likelihood cannot choose between them.** Evaluating each cell definition at a fixed
parameter set is completely confounded, because whichever definition a run was fitted under wins
at its own MLE:

| cell definition | at the as-written MLE | at the 5 cm MLE |
|---|---|---|
| as written (bin 34, 33.5 cm) | **721.2** | 927.4 |
| one bin at the label (bin 35, 34.5 cm) | 740.4 | 815.6 |
| the 5 cm data bin (bins 35-39) | 913.4 | **732.8** |

Fitting each from scratch is a fair comparison — the observations are identical in all three and
only the predicted cell definition changes — and it mildly favours the one-bin reading:

| quantity | as written | 1 cm at label | whole 5 cm bin |
|---|---|---|---|
| total likelihood | 2048.07 | **2045.71** | 2051.98 |
| age composition | 721.20 | 723.68 | 732.79 |
| length composition | 1336.33 | 1332.92 | 1331.85 |
| mean length at age 1 (cm) | 9.28 | 9.85 | 10.81 |
| von Bertalanffy K | 0.1910 | 0.1947 | 0.2039 |
| natural mortality M | 0.4309 | 0.4412 | 0.4678 |
| terminal SSB (2024, mt) | 89 958 | 89 908 | 92 522 |
| B2024 / B0 | 0.233 | 0.235 | 0.242 |
| **2025 OFL (t)** | 35 141 | **35 833 (+1.97%)** | 38 536 (+9.66%) |
| 2025 ABC (t) | 24 124 | 24 724 | 27 308 |

The 1 cm reading gives the lowest total likelihood of the three, but that is a fit comparison and
not evidence of what the rows mean. Only the person who tabulated the ages can say. The
assessment's own prep script reads SS3 output rather than building the CAAL, so it does not
record the intent either.

Both corrected models are kept: `goa_pcod_caal_bins_1cm` (`35 35`, total 2045.71) and
`goa_pcod_caal_bins_fixed` (`35 39`, total 2051.98). In each, masking the two CAAL columns makes
the data file byte-identical to the original, and rerunning the unmodified model reproduces the
archived 2048.07 exactly. The 5 cm copy's partition is SS3's own, the one `make_len_bin` builds
for the length compositions (`SS_readdata_330.tpl:1700-1746`), which puts population bins 1-9 in
the first data bin as a minus group and gives the last data bin the plus bin.

`Report.sso` confirms each fix took: as written the cells sit at 3.5, 8.5 ... 103.5, one bin below
every data label; the 1 cm copy puts them on the labels (34.5-34.5) and the 5 cm copy spans
0.5-8.5, 9.5-13.5 ... 104.5-104.5.

A useful guard **while a file writes lengths in those columns**: compare `Report.sso`'s CAAL
`Lbin_lo` against the data file's. `Report.sso` prints the length of the bin SS3 used, so the two
should agree, and any difference is the truncation. Once the columns hold bin *numbers*, as they
should, the two legitimately differ and the test becomes whether `Report.sso`'s `Lbin_lo` and
`Lbin_hi` bracket the intended data bin.

## Every run has it, including the current assessments

It is in the data files, not in any one configuration, and **it is still there in this year's
models** — both were pulled from the AFSC repos and rerun here, not just inspected:

| current model | source | rows | data file | `Report.sso` |
|---|---|---|---|---|
| AI `M24_1_2025` | `afsc-assessments/AI_PCOD` | 1160, all active | 12.5–115.5 | 11.5–114.5 |
| GOA `M24.0`, `GOAPcod2025Dec08.dat` | `afsc-assessments/goapcod`, 2025_Assessment | 857, all active | 4.5–104.5 | 3.5–103.5 |

The GOA 2025 model's CAAL carries its whole age likelihood (733.2 of a total 2109.0). The AI 2025
model still has `Lbin_hi = Lbin_lo + 1`, so its cells are still two bins wide as well as low.

The earlier runs, checked the same way:

| run | CAAL rows | data file `Lbin_lo` | `Report.sso` `Lbin_lo` |
|---|---|---|---|
| `AI cod - Dev/SS3/run` | 1160 | 12.5–115.5 | 11.5–114.5 |
| `AI cod - Dev/Data/M24_1` | 1160 | 12.5–115.5 | 11.5–114.5 |
| `AI cod - Dev/Data/M24_1_baseline` | 1160 | 12.5–115.5 | 11.5–114.5 |
| `AI cod - Dev/Data/M24_1_adjusted` | 1160 | 12.5–115.5 | 11.5–114.5 |
| `GOA cod/Data/goa_pcod` | 827 | 4.5–104.5 | 3.5–103.5 |
| `GOA cod/Data/goa_pcod-no init and ramp` | 827 | 4.5–104.5 | 3.5–103.5 |

`SS3/run` is a different configuration from `M24_1_adjusted` (total likelihood 474.879 against
531.003) and carries the same CAAL rows and the same shift, so the corrected copies here are not
the origin of it.

## No EBS Pacific cod model is materially affected

Checked because they write their CAAL columns the same way (`Lbin_method = 1` with lengths), from
`afsc-assessments/EBS_PCOD`, `2024_ASSESSMENT/NOVEMBER_MODELS/APPENDIX_2.3_2024_MODELS.zip`.
All five November 2024 models were checked, not just 24.1, and they are identical in this
respect. They come out differently from AI and GOA for two reasons.

**None of them fits conditional age-at-length data at all.** Every one has the same 23 active age
rows — fleet 2, 2000–2023, `Lbin_lo 1.5 Lbin_hi 119.5`, i.e. the whole length range — and those
carry the entire `Age_comp` likelihood. `condbase` is empty in all five.

| model | data file | single-bin CAAL rows | of which active | full-range rows | active |
|---|---|---|---|---|---|
| 23.1.0.d | `BSPcod24_OCT_1cm.dat` | 0 | 0 | 30 | 23 |
| 24.0 | `BSPcod24_OCT_5cm.dat` | 960 | **0** | 30 | 23 |
| 24.1 | `BSPcod24_OCT_5cm_NB.dat` | 960 | **0** | 30 | 23 |
| 24.2 | `BSPcod24_OCT_5cm_NB.dat` | 960 | **0** | 30 | 23 |
| 24.3 | `BSPcod24_OCT_5cm_NB.dat` | 960 | **0** | 30 | 23 |

The 960 single-bin rows in 24.0–24.3 (`Lbin_lo` 4.5, 9.5, 14.5 …, `Lbin_hi = Lbin_lo`, exactly
GOA's shape) are all on **negative fleet numbers**, so they are switched off. 23.1.0.d carries no
single-bin rows at all, its data file being the 1 cm version. So the AI/GOA defect cannot bite in
any of them: the rows it would corrupt are not in the likelihood.

The numbers below are from 24.1; the other four share the same 23 active rows and the same
population grid, so the same reasoning applies to each.

**The truncation still touches those 23 rows, but negligibly.** They are written `1.5 119.5`,
meaning the whole length range. Truncated to bins 1 and 119, which on this model's population
vector are 0.001 cm and 117.5 cm, so the top two bins (118.5 and 119.5 cm) are left out of the
predicted composition. Writing `1 121` instead — which also makes SS3 drop the filter entirely —
changes essentially nothing, because EBS cod do not reach those lengths:

| quantity | as written | corrected |
|---|---|---|
| age composition | 55.6440 | 55.6452 |
| total likelihood | 243.416 | 243.417 |
| SSB 2024, B/B0, 2025 OFL, 2025 ABC | — | **0.000% change** |

**One thing to flag if those CAAL rows are ever switched back on.** These models read their
population bins from an explicit vector that starts `0.001, 0.5, 1.5, …`, an extra bin at the
bottom compared with AI and GOA. Bin *k* is therefore *k* − 1.5 cm rather than *k* − 0.5, so a row
labelled 34.5 would truncate to bin 34 = **32.5 cm — two bins low, not one**. The rows are already
written in the vulnerable style, so enabling them without also converting them to bin numbers
would reintroduce the defect at double the offset.

Caveat: the v3.30.21 macOS binary (the version this model was run with) fails to read the data
file with `Incompatible array bounds in dmatrix`, so both runs above used v3.30.22.1. The
truncation behaviour is identical across every release to 3.30.24, so the conclusion about which
bins are used holds; the likelihood and derived numbers are from these runs, not from the
assessment's own.

## Reproducing this

From `Rceattle-models`, with an SS3 v3.30.22.1 executable (`r4ss::get_ss3_exe(version =
"v3.30.22.1")`):

- `AI cod - Dev/Data/M24_1_adjusted` rerun as-is gives 531.003 and
  `GOA cod/Data/goa_pcod-no init and ramp` gives 2048.07, both matching their archived
  `Report.sso`, so the platform makes no difference.
- `M24_1_caal_bins_fixed` gives 532.903; `goa_pcod_caal_bins_fixed` gives 2051.98.
- `SS3-bridge/compare_caal_bin_fix.R <as-written dir> <corrected dir>` produces the tables above.

The defect was found while building an exact SS3 to Rceattle bridge for these two stocks: Rceattle
reproduced SS3's age-length key, N-at-age, selectivity, SSB and predicted length compositions to
within 1e-6, but its predicted CAAL differed by up to 0.118 in probability. Honouring SS3's
multi-bin cell brought that to 1.8e-6 on 1159 of the 1160 AI rows.
