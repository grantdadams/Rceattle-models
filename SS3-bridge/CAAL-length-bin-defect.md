# Conditional age-at-length rows address the wrong length bins in the AI and GOA Pacific cod SS3 models

**Status:** verified against the Stock Synthesis source and reproduced by rerunning each model.
**Affects:** `AI cod - Dev/Data/M24_1*` and `GOA cod/Data/goa_pcod*`. All five November 2024 EBS
Pacific cod models write their columns the same way but are **not** materially affected — see the section below, which
is worth reading as the shape of a negative result. In general it affects any SS3 model that
writes lengths in the `Lbin_lo` / `Lbin_hi` columns of a conditional age-at-length (CAAL) row
under `Lbin_method = 1`. **Not fixed in the current release:** measured on v3.30.25.1 the bins
still come back 1 cm low, worth 19.6 nats of `Age_comp` on GOA cod (see the v3.30.25.1 section).
An earlier version of this line said "3.30.24 or earlier", which that measurement disproves, and
said `Lbin_method = 2` was affected as well — it is not, because method 2 converts a data bin
number to a population bin by exact match before use.

## Summary

In both cod models every CAAL observation is fitted against a prediction for the **wrong length
bins**. The observations themselves are fine; it is the expected values SS3 compares them with
that are misplaced.

| model | length bins the row is labelled with | length bins SS3 actually uses |
|---|---|---|
| AI cod | one 1 cm bin, e.g. 24.5 | **two** bins, 23.5 and 24.5 — and neighbouring rows overlap |
| GOA cod | 34.5, on a 5 cm data bin grid | **one 1 cm** population bin, at 33.5 |

**The off-by-one is certain in both.** How wide a GOA row is meant to be was open for a while and
is worth 5x; it is now settled at the whole 5 cm bin by the assessment's own prep code — see
"What a GOA row spans" below. The AI case never had that ambiguity: its data bins are already
1 cm, and the Rceattle bridge confirms the corrected reading independently.

Correcting the AI model moves mean length-at-age up 0.2–0.5 cm and the 2025 OFL down 1.25%.

**GOA, re-measured 2026-09-28 on a verified matched pair** — the pristine assessment control
file, 196 estimated parameters on both sides, the two `.dat` files differing only in the CAAL
block, same v3.30.22.1 binary:

| | original | corrected | change |
|---|---|---|---|
| SSB 2024 (terminal) | 102,580 | 103,624 | +1.02% |
| SSB mean 1977–2024 | | | +1.42% (up in 41 of 48 years) |
| M | 0.4929 | 0.5216 | +5.81% |
| K | 0.1905 | 0.2035 | +6.85% |
| Linf | 99.4609 | 99.4611 | +0.00% |
| unfished SSB, B40% | | | +0.28% |
| F reference point (`annF_SPR`) | 0.65 | 0.67 | +3.75% |
| **OFL 2025** | 45,522 | 47,847 | **+5.11%** |
| OFL 2028 (peak) | 61,887 | 68,168 | +10.15% |

The correction moves growth and mortality, not scale: unfished SSB and B40% shift by a quarter
of a percent, and the OFL rise is carried almost entirely by the F reference point following the
higher M.

> **This is not the +9.66% in the bin-width table further down, and both are right.** That table
> is built on the BRIDGE configuration -- its 5 cm column is total likelihood 2051.98, which is
> `goa_pcod_caal_bins_fixed`, carrying `F_Method` 3 → 2 (134 F parameters become estimated) and
> `max_bias_adj` → −1. The table above is the PRISTINE assessment control file, total likelihood
> 2068.39. Same data correction, two control configurations, and the OFL effect differs by a
> factor of nearly two: +9.66% under the bridge's estimated-F setup, +5.11% under the
> assessment's hybrid F.
>
> **Quote the pristine figures when the question is "what does this do to the assessment".** The
> bridge figures answer a different question, which is what the correction does to the model
> Rceattle is being matched against. Corrected copies are `AI cod - Dev/Data/M24_1_caal_bins_fixed`,
`GOA cod/Data/goa_pcod_caal_bins_1cm` and `GOA cod/Data/goa_pcod_caal_bins_fixed`; in each, only
the two CAAL length columns differ from the original, and rerunning each original reproduces its
archived likelihood exactly.

## Why it happens

Under `Lbin_method = 1` the two columns are **population length bin numbers**, used with no
length-to-bin conversion, and the decimal is dropped when the value is used as a bin index. Both
cod data files set `Lbin_method = 1` and then write lengths, not bin numbers.

> **Corrected 2026-09-30.** An earlier version of this note said the columns are read into
> `imatrix` containers, citing `SS_readdata_330.tpl:2448-2449` for v3.30.22.1, so that `24.5`
> became `24` at read time. **The source on disk declares `matrix` — an ADMB double — at that
> same line**, and I cannot check a v3.30.22.1 copy (both checkouts here report the unversioned
> `#V3.30.xx.yy`). The read-time claim is therefore unverified, and it is also the wrong
> explanation: if the truncation happened at read, `Lbin_method = 3` would truncate too and then
> fail its exact-match test, which it demonstrably does not. The exact line that drops the
> decimal in the method-1 path is **not pinned down**; everything below is measured from model
> output rather than read from source, and does not depend on it.

1. `SS_readdata_330.tpl:2586-2587` — the data file's columns 7 and 8 are assigned straight across:

   ```
   Lbin_lo(f, j) = Age_Data[i](7);
   Lbin_hi(f, j) = Age_Data[i](8);
   ```

2. `SS_readdata_330.tpl:2589-2600` — the `switch (Lbin_method)` converts all three spellings to
   population bin numbers. **Case 1 does no conversion at all**: the value is already supposed to
   be a bin number. Case 2 maps a data bin number and case 3 a length, each by an exact match
   against the population bin lower edges (`len_bins(k) == ...`), calling `write_message(FATAL, 0)`
   when nothing matches. That asymmetry is the whole story: a method-3 length is converted to an
   index before use, a method-1 length never is.

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

## What a GOA row spans: resolved, it is the whole 5 cm bin

**Settled by the assessment's own data-prep code** (`pete-hulson/goa_pcod`), after a long detour
through fit comparisons that could not settle it. Each CAAL row is the age composition of every
aged fish in a 5 cm length bin.

`dev/assessment/1_get_data.r` passes the 5 cm grid to the conditional age-at-length builder —
`len_bins = len_bins5` in the 2024 pipeline, under the comment `## new len comps at 5 cm bins`,
and in 2025 `len_bins` is that grid outright, the 1 cm alternative having been dropped.
`R/get_data/conditional_Length_AGE_cor.r` then assigns each fish to the largest grid value below
its length and sums every fish in the bin into one row:

```r
length$BIN[length$LENGTH < len_bins[((n-i)+1)]] <- len_bins[n-i]
...
Agecomp_obs[,8] <- Agecomp_obs[,7] <- as.numeric(substr(Agecomp_lengths,5,10))
```

Lengths are taken as integer cm (`as.integer(LENGTH / 10)`), so BIN 34.5 collects the 35, 36, 37,
38 and 39 cm fish — a 5 cm stratum. The last line writes the bin's label into **both** columns, so
`Lbin_hi = Lbin_lo` is the label written twice, not a 1 cm cell.

**So the fix for the 34.5 row is `35 39`.** On the bridge configuration that table uses, the
2025 OFL effect is +9.66% rather than the +1.97% the 1 cm reading gives; on the pristine
assessment control file it is +5.11% (see the measured table near the top).

The rest of this section records how the file alone could not decide it, which is worth keeping:
it is why the fit comparison below must not be read as evidence.

GOA's `Lbin_lo` values are exactly its 21 5 cm data length bin edges and nothing else, which is
what first suggested each row holds a whole 5 cm bin. A single 1 cm bin sitting on each data bin
edge fits the file equally well, and the two need different corrections:

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
| **2025 OFL (t)** | 35 141 | 35 833 (+1.97%) | **38 536 (+9.66%)** |   <!-- bridge config; pristine gives +5.11% -->
| 2025 ABC (t) | 24 124 | 24 724 | 27 308 |

The 1 cm reading gives the lowest total likelihood of the three, and for a while that was read as
mild evidence for it. **It is not evidence at all.** A fit comparison cannot say what a datum
means: the cell definition is a fact about how the fish were tabulated, not a parameter, and a
cell that is too narrow can fit better by bending growth to suit it — which is what the mean
length-at-age row shows happening. The prep code settles it, and it says 5 cm.

The lesson worth carrying: when two readings of a datum are confounded in the likelihood, the
answer is in the code or the person who wrote the file, never in the objective function.

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

## Still present in SS3 v3.30.25.1, and the two spellings are exactly equivalent

Measured 2026-09-30 on the current release (`ss3_osx_arm64`, v3.30.25.1), GOA Pcod Model 19.1e as
five forward passes at **one common parameter set** — the same `Model19_1e.ctl`, `ss3.par` and
`forecast.ss` from `Data/goa_pcod`, `init_values_src = 1`, `last_estimation_phase = 0` — so only
the CAAL reading varies. Files in `lbin-method-check/`.

| run | `Lbin_method` | columns 7-8 | bins SS3 used | `Age_comp` | `TOTAL` |
|---|---|---|---|---|---|
| D | 1 | `34.5 34.5` | `3.5-3.5`, `8.5-8.5`, `13.5-13.5` — **1 cm low** | 721.519 | 2068.39 |
| A | 3 | `34.5 34.5` | `4.5-4.5`, `9.5-9.5`, `14.5-14.5` | **741.112** | **2087.98** |
| C | 1 | `35 35` | `4.5-4.5`, `9.5-9.5`, `14.5-14.5` | **741.112** | **2087.98** |
| B | 1 | `35 39` | `0.5-8.5`, `9.5-13.5`, `14.5-18.5` | **916.192** | **2263.06** |
| E | 3 | `34.5 38.5` | `4.5-8.5`, `9.5-13.5`, `14.5-18.5` | **916.192** | **2263.06** |

Every other component is identical across all five (Catch 1.21e-12, Survey -0.972632,
Length_comp 1340.79, Recruitment -2.62328, Parm_priors 1.15484), which is the check that nothing
but the CAAL reading moved.

Three things follow.

**The defect is live in the current release.** D is the file as the assessment ships it, and its
bins still come back 1 cm below their labels — worth **19.6 nats** of `Age_comp` against the same
model read correctly. Anyone writing lengths under `Lbin_method = 1` is silently fitting the
wrong bins on v3.30.25.1.

**`Lbin_method = 3` with lengths and `Lbin_method = 1` with bin numbers are the same model.**
A and C agree on every component, and all 9160 CAAL cells match on bin, observation and
prediction. Likewise B and E. So either spelling is correct; it is mixing lengths with method 1
that is wrong.

**`Lbin_lo` and `Lbin_hi` are both LEFT EDGES** — the first and last bin of a range, not the ends
of an interval. The five 1 cm bins covering the 34.5-39.5 data bin are `34.5 38.5` as lengths or
`35 39` as bin numbers. Writing `34.5 39.5` names a **sixth** bin, since 39.5 is the left edge of
the next data bin. B and E differ only at the extremes: B makes the lowest data bin a minus group
(`0.5-8.5`) and the top a plus group (`105 105`), which is worth nothing to six significant
figures here but is a deliberate choice, not a slip.

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
