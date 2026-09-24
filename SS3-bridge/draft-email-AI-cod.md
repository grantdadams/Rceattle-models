**Draft — AI Pacific cod (M24_1). Not sent.**

Subject: Possible issue with the conditional age-at-length bins in the AI Pacific cod model

Hi Ingrid,

I've been building a bridge between Stock Synthesis and Rceattle using AI Pacific cod as one of
the test cases, and in the course of getting the two models to agree I think I've found a problem
in how M24_1's conditional age-at-length (CAAL) data are being read. I wanted to send it to you
before doing anything else with it. I may have misread something, so please push back if so.

**The short version.** Every CAAL row in the data file is being fitted against the predicted age
composition of *two* 1 cm length bins rather than one, and both of them sit 1 cm below the length
the row is labelled with. The observations are fine; it's the expected values they're compared
with that are misplaced.

These rows are the whole age likelihood, so it isn't a corner of the model: the `Age_comp`
component is 402.473 and the CAAL rows' `Like` column sums to 402.4731, i.e. all of it. The
marginal age comps are switched off (fleet `-2`), so nothing else contributes. `Age_comp` is
about three quarters of M24_1's total likelihood of 531.

**Why it happens.** The age composition section sets `Lbin_method = 1`, which tells SS3 that the
`Lbin_lo` and `Lbin_hi` columns hold population length *bin numbers*. The file writes lengths
there instead — `18.5 19.5`, `19.5 20.5`, and so on. Three things then happen to each row.
Taking the first one as the example:

1. SS3 stores both columns as integers (`SS_readdata_330.tpl:2448`), so `18.5 19.5` becomes
   `18 19`. Nothing warns about it.
2. It reads that pair as a *range* of bins and flags every bin from 18 through 19 — so two
   bins, not one.
3. The row's expected age composition is then summed over every flagged bin
   (`SS_expval.tpl:631`).

Population bins run 0.5, 1.5, 2.5 ... so bin 18 is 17.5 cm and bin 19 is 18.5 cm. The row is
therefore compared against the predicted ages of fish 17.5–19.5 cm: twice as wide as the 18.5 cm
bin it was meant to be, and shifted a bin down. And because consecutive rows step by 1 cm, the
next row (`19.5 20.5`, so bins 19 and 20) flags bin 19 as well — neighbouring cells overlap.

There's a quick way to check this in your own output without taking my word for it. The
`FIT_AGE_COMPS` section of `Report.sso` writes each row's `Lbin_lo` and `Lbin_hi` back out as the
*lengths* of the bins SS3 actually used. So when a data file writes lengths in those columns — as
these do — the two should read the same, and any difference is the truncation. The very first
CAAL row lines up like this:

```
data.ss                      1991 7 2 0 0 1  18.5 19.5  1 ...
Report.sso, FIT_AGE_COMPS    2 Srv 1 1991 1 2 7 1991.5 0 0 1  17.5 18.5  _ _ 1 ...
```

and it holds all the way down: the data file's `Lbin_lo` runs 12.5–115.5 while `Report.sso`
reports 11.5–114.5. One bin low, and `Lbin_hi` a bin above `Lbin_lo` rather than equal to it,
which is the two-bin width.

(The check only reads that way while the file writes lengths. Once the columns hold bin *numbers*,
as they should, the two columns legitimately differ — `Report.sso` still prints lengths. Then the
test is that `Report.sso`'s `Lbin_lo` matches the length you meant, and `Lbin_hi` equals it for a
single bin.)

I see the same thing in every AI run in the folder I have — `SS3/run`, `M24_1`,
`M24_1_baseline` and `M24_1_adjusted` all carry the same 1160 CAAL rows and all report 11.5–114.5
— so this looks like it is in the data file rather than in any one configuration.

**What it does to the assessment.** I made a copy of `M24_1_adjusted` with only those two columns
changed — `Lbin_lo` and `Lbin_hi` both set to the population bin number holding that length
(`bin = length + 0.5`), which is what `Lbin_method = 1` asks for. 1160 lines change and nothing
else does: same control file, same executable, same everything. Rerunning the unmodified model
first reproduces its archived likelihood of 531.003 exactly, so the comparison is clean.

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

Growth is what the CAAL data mainly inform and it's what moves — length at age goes up 0.2–0.5 cm
across the age range. Stock status is essentially unchanged, so the tier and the status
determination don't turn on this, and the catch advice moves about 1.3%. So it's a real bias but
not a dramatic one for AI.

**Fixing it.** Any of these works:

1. Write population bin numbers, as `Lbin_method = 1` specifies, with `Lbin_hi = Lbin_lo` for a
   single bin. The population bins are 0.5, 1.5, 2.5 ... so 18.5 cm is bin number 19, and that
   first row becomes `19 19` instead of `18.5 19.5`. This is what I did and it runs on the SS3
   version you're already using.
2. Use `Lbin_method = 2` and write **data** length bin numbers — the position in the data length
   bin vector rather than the population one — again with `Lbin_hi = Lbin_lo`. M24_1's data bins
   are also 0.5, 1.5, 2.5 ... so 18.5 cm is data bin 19 as well, and the row is `19 19` either
   way. That equivalence is a coincidence of this model: the two grids happen to be the same 143
   1 cm bins. Where a model's data bins are coarser — GOA Pacific cod has 21 5 cm data bins over
   105 1 cm population bins, so its 34.5 cm data bin is number 7 but population bin 35 — method 2
   can't express a whole data bin at all, because it converts each endpoint to the population bin
   at that data bin's lower edge. Option 1 is the more portable habit.
3. Move to SS3 v3.30.25 or later and use `Lbin_method = 3`, where the columns are lengths —
   **and still set `Lbin_hi = Lbin_lo`**. Two cautions here, both of which I checked by running
   it rather than reasoning about it:

   - `Lbin_method = 3` *cannot* work before v3.30.25. The same integer truncation makes the
     length compare unequal to every half-integer bin edge, and SS3 stops with
     `L_bin_lo no match to poplenbins`. Fixed upstream in commit 416bf89, released in v3.30.25.
   - On v3.30.25, simply switching the method and leaving the values as they are does **not**
     fix the model. `Lbin_hi` is the lower edge of the *last bin included*, not the upper edge of
     the interval, so `18.5 19.5` still selects two bins — it just stops being shifted:

     ```
     data file        SS3         bins actually fitted
     18.5 19.5  m1    3.30.22.1   17.5 and 18.5   (shifted low, two bins)
     18.5 19.5  m3    3.30.25     18.5 and 19.5   (right place, still two bins)
     18.5 18.5  m3    3.30.25     18.5            (correct)
     ```

     I mention it because that middle case is the one to watch: the obvious symptom disappears
     while the cell is still twice as wide as intended.

**One thing that is not a problem.** The 2002 row at month 1, where every other CAAL row is at
month 7, looked like a typo to me at first. It isn't: 2002 has exactly 100 rows at month 7 and SS3
caps age composition observations at 100 per fleet × time, so setting that row to month 7 makes
the model refuse to start. I assume it was moved deliberately to get under the cap. I'd leave it;
the only side effect is that those 4 fish are compared with the January age-length key instead of
the July one.

I'm happy to send you the corrected data file and both run directories, or to walk through it. And
if I've misunderstood how you intended those columns to be read, I'd rather hear that now.

Thanks,
Grant
