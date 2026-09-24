**Draft — GOA Pacific cod (Model 19.1e). Not sent.**

Subject: Conditional age-at-length bins in the GOA Pacific cod model — affects the OFL by about 10%

Hi Pete,

I've been building a bridge between Stock Synthesis and Rceattle with GOA and AI Pacific cod as
the test cases, and getting the two to agree turned up what I think is a real problem in how the
GOA model's conditional age-at-length (CAAL) data are read. The effect on GOA is larger than I
expected, so I wanted to get it to you quickly. I may have misread something — please push back
if so.

**The short version.** Each CAAL row holds the ages of fish measured in a 5 cm length bin, but
SS3 is comparing it with the predicted age composition of a single 1 cm bin, sitting 1 cm below
the row's label. So a row holding fish from 34.5–39.5 cm is fitted against the predicted ages of
the 1 cm bin at 33.5 cm. The observations are fine; the expected values they're compared with are
wrong. It applies to all 827 CAAL rows.

Those rows are the whole age likelihood: the `Age_comp` component is 721.195 and the CAAL rows'
`Like` column sums to 721.1952, i.e. all of it. The marginal age comps are switched off
(fleet `-1`), so nothing else contributes.

**Why it happens.** The age composition section sets `Lbin_method = 1`, which tells SS3 that
`Lbin_lo` and `Lbin_hi` hold population length *bin numbers*. The file writes lengths there
instead — 4.5, 9.5, 14.5 ... the data bin edges — with `Lbin_hi = Lbin_lo`. Two things follow.
Taking the `34.5 34.5` row as the example:

1. SS3 stores both columns as integers (`SS_readdata_330.tpl:2448`), so `34.5 34.5` becomes
   `34 34`. Nothing warns about it. Population bins run 0.5, 1.5, 2.5 ... so bin 34 is 33.5 cm
   — one bin below the label.
2. SS3 reads the pair as a *range* of bins and sums the row's expected age composition over
   every bin in it (`SS_expval.tpl:631`). With `Lbin_hi = Lbin_lo` that range is a single 1 cm
   bin. To cover a 5 cm data bin, the two columns have to span all five population bins inside
   it — here `35 39`, which is 34.5 through 38.5 cm.

So the row is compared against the predicted ages of a single 1 cm bin at 33.5 cm, when its
fish were measured over 34.5–39.5 cm.

You can check this in your own output. The `FIT_AGE_COMPS` section of `Report.sso` writes each
row's `Lbin_lo` and `Lbin_hi` back out as the *lengths* of the bins SS3 actually used. So when a
data file writes lengths in those columns — as this one does — the two should read the same, and
any difference is the truncation. The first CAAL row lines up like this:

```
GOAPcod2024Oct17_1e_5cm.dat   2007 1 1 0 0 2  34.5 34.5  0.14 ...
Report.sso, FIT_AGE_COMPS     1 FshTrawl 1 2007 1 2 7 2007.5 0 0 2  33.5 33.5  _ _ 0.14 ...
```

and it holds all the way down: the data file's values run 4.5–104.5 while `Report.sso` reports
3.5–103.5, with `Lbin_hi` equal to `Lbin_lo` throughout — every cell one bin low and 1 cm wide
instead of 5.

(The check only reads that way while the file writes lengths. Once the columns hold bin *numbers*,
as they should, the two legitimately differ — `Report.sso` still prints lengths. Then the test is
that `Report.sso`'s `Lbin_lo` and `Lbin_hi` bracket the data bin you meant.)

Both GOA runs I have — `goa_pcod` and `goa_pcod-no init and ramp` — carry the same 827 CAAL rows
and both report 3.5–103.5, so this looks like it is in the data file rather than in any one
configuration.

**What it does to the assessment.** I made a copy of `goa_pcod-no init and ramp` with only those
two columns changed, to the population bin numbers each 5 cm data bin covers. I used SS3's own
partition for this, the one `make_len_bin` builds for the length compositions
(`SS_readdata_330.tpl:1700-1746`): bins 1–9 for the first data bin (it's a minus group, so it
picks up everything below 4.5 cm), 10–14 for the second, and so on, with the last data bin taking
the plus bin. Mask those two columns and the two data files are byte-identical. Rerunning the
unmodified model first reproduces its archived likelihood of 2048.07 exactly.

|                          | as written | corrected |
| ------------------------ | ---------- | --------- |
| total likelihood         | 2048.07    | 2051.98   |
| mean length at age 1     | 9.28 cm    | 10.82 cm  |
| mean length at age 4     | 48.62 cm   | 51.38 cm  |
| von Bertalanffy K        | 0.1910     | 0.2039    |
| natural mortality M      | 0.4309     | 0.4678    |
| terminal SSB (2024)      | 89 958 t   | 92 522 t  |
| B2024/B0                 | 0.233      | 0.242     |
| **2025 OFL**             | **35 141 t** | **38 536 t** |
| 2025 ABC                 | 24 124 t   | 27 308 t  |

Growth is what the CAAL data mainly inform and it moves a lot: length at age 1 by 16.5% and age 4
by 5.7%. M is estimated in this model and rises 8.6% with it. The OFL goes up about 9.7% and the
ABC about 13%. Status is not far off (B/B0 0.233 to 0.242), so this doesn't look like a tier or
status question, but it is a material change to the catch advice.

All 827 observations are still fitted in the corrected run. One small reporting wrinkle: the
single observation in the lowest data bin now starts at population bin 1, and `r4ss` files that
one under `agedbase` rather than `condbase`. SS3 still fits it as a conditional cell — it shows up
in `CompReport.sso` with `Lbin_lo 0.5 Lbin_hi 8.5` and a normal likelihood contribution.

**Fixing it.** Any of these works:

1. Write population bin numbers spanning each data bin, as `Lbin_method = 1` specifies — e.g.
   `Lbin_lo 10, Lbin_hi 14` for the 9.5 cm data bin. This is what I did and it runs on the SS3
   version you're already using.
2. Move to SS3 v3.30.25 or later and use `Lbin_method = 3`, where the columns are lengths, again
   as a range — `9.5 13.5` for that same bin. Note that `Lbin_hi` is the lower edge of the *last
   population bin included*, not the upper edge of the length interval, so it is `13.5` and not
   `14.5`. Two cautions:

   - `Lbin_method = 3` *cannot* work before v3.30.25: the same integer truncation makes the
     length compare unequal to every half-integer bin edge and SS3 stops with
     `L_bin_lo no match to poplenbins`. Fixed upstream in commit 416bf89, released in v3.30.25.
   - Switching the method without also fixing the values does nothing useful. I checked this on
     AI cod, which has the same defect in a milder form: on v3.30.25 a row written `18.5 19.5`
     under method 3 fits bins 18.5 *and* 19.5 — correctly positioned but still two bins wide.
     For GOA, where `Lbin_hi = Lbin_lo` today, switching to method 3 alone would leave every cell
     a single 1 cm bin; it would only stop being shifted.

`Lbin_method = 2` is not an option here, which surprised me. It looks like the natural fit —
write the data bin number and let SS3 expand it — but it converts *each endpoint* to the
population bin at that data bin's lower edge (`SS_readdata_330.tpl:2604-2628`). So `7 7` gives a
single 1 cm bin at 34.5, and `7 8` gives six bins, 34.5 through 39.5. Neither is the five bins
the data bin covers. Method 2 only lands exactly when the data and population grids are the same,
which is not the case for this model.

The AI Pacific cod model has a version of the same problem, from the same cause — I've written to
Ingrid about that one separately. Its effect is much smaller (OFL −1.25%) because its CAAL is
already on 1 cm bins, so only the 1 cm offset and a one-bin overlap are in play.

I'm happy to send the corrected data file and both run directories, or to walk through it. And if
I've misunderstood how you intended those columns to be read, I'd rather hear that now.

Thanks,
Grant
