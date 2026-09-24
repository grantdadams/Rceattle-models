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

There's a quick way to check this in your own output without taking my word for it. `Report.sso`
writes the CAAL `Lbin_lo` column back out as the length of the bin SS3 actually used. In M24_1 the
data file's `Lbin_lo` runs 12.5–115.5 but `Report.sso` reports 11.5–114.5, with `Lbin_hi` one bin
above — one bin low, two bins wide. If the two ever disagree, the bins being fitted aren't the
bins that were written.

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
   single bin. This is what I did and it runs on the SS3 version you're already using.
2. Use `Lbin_method = 2` and write data length bin numbers.
3. Move to SS3 v3.30.25 or later and use `Lbin_method = 3`, where the columns are lengths. Worth
   knowing that `Lbin_method = 3` *cannot* work before v3.30.25 — the same integer truncation
   makes the length compare unequal to every half-integer bin edge and SS3 stops with
   `L_bin_lo no match to poplenbins`. It was fixed upstream in commit 416bf89, released in
   v3.30.25.

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
