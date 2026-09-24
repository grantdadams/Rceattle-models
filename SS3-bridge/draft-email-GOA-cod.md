**Draft — GOA Pacific cod (Model 19.1e). Not sent.**

Subject: Conditional age-at-length bins in the GOA Pacific cod model — affects the OFL by about 10%

Hi Pete,

Claude and I have been building the GOA Pacific cod bridge over to Rceattle. While trying to get
the two models to agree I think I've found a problem in how the conditional age-at-length (CAAL)
data are being read. Stock Synthesis is super complex, so I may have misread something and if so
please ignore — but the effect looks large enough that I wanted to get it to you quickly.

I think each CAAL row holds the ages of fish measured in a 5 cm length bin, but is being compared
against predictions for a single 1 cm bin, sitting 1 cm below the row's label. So a row holding
fish from 34.5–39.5 cm is compared with the predicted ages of the 1 cm bin at 33.5 cm. The
observations are fine; it's the expected values they're compared with that might be off. It
applies to all 827 CAAL rows, and those rows are the whole age likelihood.

The age composition section sets `Lbin_method = 1`, which tells SS3 that `Lbin_lo` and `Lbin_hi`
hold population length bin numbers. The file has lengths there instead — 4.5, 9.5, 14.5 ... the
data bin edges — with `Lbin_hi = Lbin_lo`. Taking the `34.5 34.5` row as the example:

1. SS3 stores both columns as integers (`SS_readdata_330.tpl:2448`, assigned at `:2586`), so
   `34.5 34.5` becomes `34 34`. Population bins run 0.5, 1.5, 2.5 ... so bin 34 is 33.5 cm — one
   bin below the label.
2. It reads the pair as a range of bins and sums the row's expected age composition over every
   bin in it (`SS_expval.tpl:631`). With `Lbin_hi = Lbin_lo` that range is a single 1 cm bin,
   where the 5 cm data bin covers five of them.

There's a quick way to check this. The `FIT_AGE_COMPS` section of `Report.sso` writes each row's
`Lbin_lo` and `Lbin_hi` back out as the lengths of the bins SS3 actually used. While the file
writes lengths in those columns, the two should read the same. The first CAAL row lines up like
this:

```
GOAPcod2024Oct17_1e_5cm.dat   2007 1 1 0 0 2  34.5 34.5  0.14 ...
Report.sso, FIT_AGE_COMPS     1 FshTrawl 1 2007 1 2 7 2007.5 0 0 2  33.5 33.5  _ _ 0.14 ...
```

So the data's values run 4.5–104.5 while `Report.sso` reports 3.5–103.5, with `Lbin_hi` equal to
`Lbin_lo` throughout — every cell one bin low and 1 cm wide instead of 5. Both GOA runs I have
(`goa_pcod` and `goa_pcod-no init and ramp`) report the same, so it looks like it's in the data
file rather than in any one configuration.

To fix it, set `Lbin_lo` and `Lbin_hi` to the population bin numbers that span each data bin. The
9.5 cm data bin covers population bins 10 to 14 (9.5 through 13.5 cm), so its rows become
`10 14`; the 34.5 cm bin becomes `35 39`. I used SS3's own partition for this, the one
`make_len_bin` builds for the length compositions (`SS_readdata_330.tpl:1700-1746`), which puts
bins 1–9 in the first data bin — it's a minus group, so it picks up everything below 4.5 cm — and
gives the last data bin the plus bin.

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

Growth is what the CAAL data mainly inform and it moves a lot: length at age 1 by 16.5%. M is
estimated here and rises with it. Status isn't far off (B/B0 0.233 to 0.242), so this doesn't
look like a tier or status question, but the OFL goes up about 9.7% and the ABC about 13%.

You can also use a newer SS3 (v3.30.25 and above) and set `Lbin_method = 3`, where the columns are
the actual lengths — `9.5 13.5` for that bin. Note `Lbin_hi` is the lower edge of the *last bin
included*, not the upper edge of the interval, so it's 13.5 and not 14.5. What doesn't work is
`Lbin_method = 2`: you'd expect to write the data bin number (the 34.5 cm bin is number 7) and
have SS3 expand it, but it converts each endpoint to the population bin at that data bin's lower
edge, so `7 7` gives one bin and `7 8` gives six. Neither is the five you want.

The AI Pacific cod model has a version of the same problem from the same cause — I've written to
Ingrid about that one separately. Its effect is much smaller (OFL −1.25%) because its CAAL is
already on 1 cm bins, so only the 1 cm offset and a one-bin overlap are in play.

I'm happy to send the corrected data file and both run directories, or to walk through it. And if
I've misunderstood how you intended those columns to be read, I'd rather hear that now.

Thanks,
Grant
