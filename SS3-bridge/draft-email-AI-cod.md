**Draft — AI Pacific cod (M24_1). Not sent.**

Subject: Possible issue with the conditional age-at-length bins in the AI Pacific cod model

Hi Ingrid,

Claude and I have been building the AI Pacific cod bridge over to Rceattle. While trying to get
the two models to agree I think I've found a problem in how M24_1's conditional age-at-length
(CAAL) data are being read. Stock Synthesis is super complex, so I may have misread something and
if so please ignore!

I think every CAAL row in the data is being compared against predictions from two 1 cm length
bins instead of one — the bin it was meant to be, plus the one below it. The observations are
fine; it's the expected values they're compared with that might be off. These rows are the whole
age likelihood, which is about three quarters of the model's total, so it isn't a corner of the
model.

The age composition section sets `Lbin_method = 1`, which tells SS3 that the `Lbin_lo` and
`Lbin_hi` columns hold population length bin numbers. The file has lengths there instead
(`18.5 19.5`, `19.5 20.5`, etc). Taking the first one as the example:

1. SS3 stores both columns as integers (`SS_readdata_330.tpl:2448`, assigned at `:2586`), so
   `18.5 19.5` becomes `18 19`.
2. It reads that pair as a range of bins and flags every bin from 18 through 19. So one bin
   becomes two bins.
3. The row's expected age composition is then summed over every flagged bin
   (`SS_expval.tpl:631`).

Population bins run 0.5, 1.5, 2.5 ... so bin 18 is 17.5 cm and bin 19 is 18.5 cm. The data is
compared against the predicted ages of fish 17.5–19.5 cm, when the row was meant to be the single
bin 18.5–19.5. And because consecutive rows step by 1 cm, the next row (`19.5 20.5`, so bins 19
and 20) flags bin 19 as well, so neighbouring cells overlap.

There's a quick way to check this. The `FIT_AGE_COMPS` section of `Report.sso` writes each row's
`Lbin_lo` and `Lbin_hi` back out as the lengths of the bins SS3 actually used. While the file
writes lengths in those columns, the two should read the same. The very first CAAL row lines up
like this:

```
data.ss                      1991 7 2 0 0 1  18.5 19.5  1 ...
Report.sso, FIT_AGE_COMPS    2 Srv 1 1991 1 2 7 1991.5 0 0 1  17.5 18.5  _ _ 1 ...
```

So the data's `Lbin_lo` runs 12.5–115.5 while `Report.sso` reports 11.5–114.5. One bin low, and
`Lbin_hi` a bin above `Lbin_lo` rather than equal to it, which is the two-bin width. I see the
same thing in every AI run I have — `SS3/run`, `M24_1`, `M24_1_baseline` and `M24_1_adjusted` all
report 11.5–114.5 — so it looks like it's in the data file rather than in any one configuration.

To fix it, you can set `Lbin_lo = Lbin_hi` and set both to the population bin number holding that
length (`bin = length + 0.5`, so 18.5 cm is bin 19 and the first row becomes `19 19`), matching
what `Lbin_method = 1` expects.

|                          | as written | corrected |
| ------------------------ | ---------- | --------- |
| total likelihood         | 531.003    | 532.903   |
| mean length at age 2     | 25.78 cm   | 26.11 cm  |
| mean length at age 4     | 50.65 cm   | 51.16 cm  |
| von Bertalanffy K        | 0.2190     | 0.2154    |
| survey q                 | 0.880      | 0.892     |
| terminal SSB (2024)      | 50 085 t   | 49 384 t  |
| B2024/B0                 | 0.238      | 0.237     |
| 2025 OFL                 | 20 600 t   | 20 340 t  |

Stock status is essentially unchanged, so the status determination doesn't change, and the catch
advice moves about 1.3%.

You can also use a newer SS3 (v3.30.25 and above) and set `Lbin_method = 3`, where the columns are
the actual lengths. You still need to set `Lbin_hi = Lbin_lo` — I checked, and switching the
method while leaving `18.5 19.5` as is still fits two bins, it just stops being shifted down.

I'm happy to send you the corrected data file and both run directories, or to walk through it. And
if I've misunderstood how you intended those columns to be read, I'd rather hear that now.

Thanks,
Grant
