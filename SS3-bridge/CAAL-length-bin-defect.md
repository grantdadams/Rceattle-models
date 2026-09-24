# Conditional age-at-length rows address the wrong length bins in the AI and GOA Pacific cod SS3 models

**Status:** verified against the Stock Synthesis source and reproduced by rerunning each model.
**Affects:** `AI cod - Dev/Data/M24_1*` and `GOA cod/Data/goa_pcod*`, and any other SS3 model that
writes lengths in the `Lbin_lo` / `Lbin_hi` columns of a conditional age-at-length (CAAL) row
under `Lbin_method = 1` or `2`, on Stock Synthesis 3.30.24 or earlier.

## Summary

In both cod models every CAAL observation is fitted against a prediction for the **wrong length
bins**. The observations themselves are fine; it is the expected values SS3 compares them with
that are misplaced.

| model | length bins the row is labelled with | length bins SS3 actually uses |
|---|---|---|
| AI cod | one 1 cm bin, e.g. 24.5 | **two** bins, 23.5 and 24.5 — and neighbouring rows overlap |
| GOA cod | one 5 cm data bin, e.g. 34.5 | **one 1 cm** population bin, at 33.5 |

Correcting the AI model moves mean length-at-age up 0.2–0.5 cm and the 2025 OFL down 1.25%.
Correcting GOA moves mean length at age 1 up 16.5% and the 2025 OFL up 9.7%. Corrected copies of
both models are `AI cod - Dev/Data/M24_1_caal_bins_fixed` and
`GOA cod/Data/goa_pcod_caal_bins_fixed`; in each, only the two CAAL length columns differ from
the original, and rerunning each original reproduces its archived likelihood exactly.

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

## Effect on the GOA assessment

`Data/goa_pcod_caal_bins_fixed` is `goa_pcod-no init and ramp` with only the CAAL `Lbin_lo` /
`Lbin_hi` columns changed, to the population bin **numbers** each 5 cm data bin covers. The
partition is SS3's own: `make_len_bin` (`SS_readdata_330.tpl:1700-1746`) puts population bins
1–9 in the first data bin (it is a minus group, so it collects everything below 4.5 cm), bins
10–14 in the second, and so on, with the last data bin taking the plus bin. Masking those two
columns makes the two data files byte-identical. Rerunning the unmodified model reproduces the
archived total likelihood of 2048.07 exactly.

`Report.sso` confirms both the defect and the fix. As written, the CAAL cells are single 1 cm
bins at 3.5, 8.5 ... 103.5 — one bin below every data label (4.5, 9.5 ... 104.5). Corrected, they
span 0.5–8.5, 9.5–13.5, ... 104.5–104.5. All 827 observations are fitted in both runs. (In the
corrected run `r4ss` files the single observation in the lowest bin under `agedbase` rather than
`condbase`, because it starts at the first population bin; SS3 still fits it as a conditional
cell, with `Lbin_lo 0.5 Lbin_hi 8.5` and a normal likelihood contribution.)

| quantity | as written | corrected | change |
|---|---|---|---|
| total likelihood | 2048.07 | 2051.98 | +3.91 |
| age composition (CAAL) | 721.20 | 732.79 | +11.59 |
| length composition | 1336.33 | 1331.85 | −4.48 |
| survey | −1.785 | −4.604 | −2.82 |
| mean length at age 1 (cm) | 9.28 | 10.81 | **+1.53 (+16.5%)** |
| mean length at age 4 (cm) | 48.62 | 51.38 | +2.76 (+5.7%) |
| von Bertalanffy K | 0.1910 | 0.2039 | +6.75% |
| natural mortality M | 0.4309 | 0.4678 | +8.57% |
| terminal SSB (2024, mt) | 89 958 | 92 522 | +2.85% |
| B2024 / B0 | 0.2330 | 0.2425 | +4.08% |
| **2025 OFL (t)** | **35 141** | **38 536** | **+9.66%** |
| 2025 ABC / forecast catch (t) | 24 124 | 27 308 | +13.2% |

The GOA effect is much the larger of the two, as expected from the larger misplacement: each
row's observation comes from a 5 cm bin but was being compared with the predicted ages of a
single 1 cm bin, one bin low. Length at age 1 moves 16.5% and the 2025 OFL moves +9.7%.

## How to fix it

Any of the following, in decreasing order of how little has to change:

The two numberings, since the difference is the whole point:

| stock | data length bins | population length bins | 34.5 cm is … |
|---|---|---|---|
| AI | 143, 1 cm, 0.5–142.5 | the same 143 | data bin 35, population bin 35 |
| GOA | **21, 5 cm**, 4.5–104.5 | 105, 1 cm, 0.5–104.5 | **data bin 7, population bin 35** |

1. **Write population bin numbers**, as `Lbin_method = 1` specifies. For 1 cm bins starting at
   0.5 cm that is `bin = length + 0.5`. For a single bin set `Lbin_hi = Lbin_lo`: AI's 18.5 cm
   row becomes `19 19`. This is what `M24_1_caal_bins_fixed` does, and it works on the SS3
   version already in use. GOA needs the **range** of population bins covered by each 5 cm data
   bin: its 4.5 cm bin is `1 9` and its 34.5 cm bin is `35 39`.
2. **Use `Lbin_method = 2`** (data length bin numbers — the position in the *data* vector, so
   GOA's 34.5 cm bin is number 7, not 35), with `Lbin_hi = Lbin_lo`. This is exact **only when
   the data and population length grids are the same**, as they are for AI, where 18.5 cm is
   data bin 19 and population bin 19 alike so the row is `19 19` under either method. It does
   not work for GOA: method 2 converts each endpoint to the population bin sitting at that data
   bin's *lower* edge (`SS_readdata_330.tpl:2604-2628`), so `7 7` gives one population bin and
   `7 8` gives six. Neither is the five the data bin covers, and there is no pair that is.
3. **Move to SS3 v3.30.25 or later and use `Lbin_method = 3`**, where the values are lengths —
   **and still fix `Lbin_hi`**. Under every method, `Lbin_hi` names the *last bin included*, not
   the upper edge of the length interval: a single 1 cm bin at 18.5 is `18.5 18.5`, and GOA's
   5 cm data bin at 9.5 is `9.5 13.5`, not `14.5`. Two things to know, both checked by running
   the v3.30.25 binary rather than reasoning about the source:

   - `Lbin_method = 3` **cannot work** before v3.30.25: `Lbin_lo` is an integer, so the length is
     truncated and then compared for exact equality against half-integer bin edges, which never
     matches, and SS3 stops with `L_bin_lo no match to poplenbins in age comp`. The containers
     were widened to `matrix` in commit `416bf89`, "convert lbin_lo to real for compare to
     len_bins", released in v3.30.25.
   - On v3.30.25, switching the method alone does **not** fix the AI file. Measured on the first
     CAAL row:

     | data file | SS3 | bins fitted (`Report.sso`) |
     |---|---|---|
     | `18.5 19.5`, method 1 | 3.30.22.1 | 17.5 and 18.5 — shifted low, two bins |
     | `18.5 19.5`, method 3 | 3.30.25 | 18.5 and 19.5 — right place, still two bins |
     | `18.5 18.5`, method 3 | 3.30.25 | 18.5 — correct |

     The middle row is the trap: the visible symptom (the shift) disappears while the cell stays
     twice as wide. Absolute likelihoods are not comparable across SS3 versions, so only the bins
     are read from those runs.

## The corrected files verified row by row

Read straight out of `FIT_AGE_COMPS` in each corrected run's `Report.sso` and compared against
the **original** file's labels, not through `r4ss`:

- **AI**: all **1160** CAAL rows have `Lbin_lo` = `Lbin_hi` = the single bin the original file
  labelled. 0 mismatches.
- **GOA**: all **827** rows span exactly the population bins of their 5 cm data bin — 0.5–8.5 for
  the minus group, 9.5–13.5, … , 104.5–104.5 for the plus bin. 0 mismatches.

`SS3-bridge/verify_caal_bins.R` does this; re-run it after any change to the corrected files.

A useful guard **while a file writes lengths in those columns**: compare `Report.sso`'s CAAL
`Lbin_lo` against the data file's. `Report.sso` prints the length of the bin SS3 used, so the two
should agree, and any difference is the truncation. Once the columns hold bin *numbers*, as they
should, the two legitimately differ and the test becomes whether `Report.sso`'s `Lbin_lo` and
`Lbin_hi` bracket the intended data bin.

## Every run in both folders has it

It is in the data files, not in any one configuration. Checked by that comparison:

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
