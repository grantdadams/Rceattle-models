**Draft — GOA Pacific cod (Model 19.1e). Grant's register, matching the AI email. Not sent.**

Subject: Possible issue with the conditional age-at-length bins in the GOA Pacific cod model

Hi Pete,

Claude and I have been bridging the GOA Pacific cod model over to Rceattle. While trying to get
the two models to agree, I think I've found a problem in how the CAAL data are being read. Stock
Synthesis is super complex, so I may have misread something, or maybe this was intentional. If so
please ignore!

I think each CAAL row holds the ages of fish measured in a 5 cm length bin, but is being compared
against predictions for a single 1 cm bin, one bin below the label. So a row holding fish from
34.5–39.5 cm is compared with the predicted ages of the 1 cm bin at 33.5 cm. The observations are
fine; it's the expected values they're compared with that might be off.

The age composition section sets Lbin_method = 1, telling SS3 that the Lbin_lo and Lbin_hi columns
hold population length bin numbers. The file has lengths there instead (4.5, 9.5, 14.5 ... the
data bin edges), with Lbin_hi = Lbin_lo. Taking the 34.5 34.5 row, then

1. SS3 stores both columns as integers (SS_readdata_330.tpl:2448, assigned at :2586), so
   34.5 34.5 becomes 34 34. Population bins run 0.5, 1.5, 2.5 ... so bin 34 is 33.5 cm, one bin
   below the label.
2. It reads that pair as a range of bins and sums the row's expected conditional age composition
   over every bin in it (SS_expval.tpl:631). With Lbin_hi = Lbin_lo that range is a single 1 cm
   bin, where the 5 cm data bin covers five of them.

To verify this, I looked at the FIT_AGE_COMPS section of Report.sso, which writes each row's
Lbin_lo and Lbin_hi back out as the lengths of the bins SS3 actually used. The data writes lengths
in those columns and the two should read the same. The first CAAL row lines up like this:

GOAPcod2024Oct17_1e_5cm.dat   2007 1 1 0 0 2  34.5 34.5  0.14 ...
Report.sso, FIT_AGE_COMPS     1 FshTrawl 1 2007 1 2 7 2007.5 0 0 2  33.5 33.5  _ _ 0.14 ...

I see the same in both GOA runs I have (goa_pcod and goa_pcod-no init and ramp), so I don't think
it's something I introduced.

To fix it, you can set Lbin_lo and Lbin_hi to the population bin numbers that span each data bin.
The 9.5 cm data bin covers population bins 10 to 14 (9.5 through 13.5 cm), so its rows become
10 14, and the 34.5 cm bin becomes 35 39. I used SS3's own partition for this, the one
make_len_bin builds for the length comps (SS_readdata_330.tpl:1700-1746), which gives the first
data bin population bins 1 to 9 (it's a minus group, so it picks up everything under 4.5 cm) and
the last data bin the plus bin.

| Metric                   | As Written | Corrected |
| :----------------------- | :--------- | :-------- |
| **Total Likelihood**     | 2048.07    | 2051.98   |
| **Mean Length at Age 1** | 9.28 cm    | 10.81 cm  |
| **Mean Length at Age 4** | 48.62 cm   | 51.38 cm  |
| **von Bertalanffy K**    | 0.1910     | 0.2039    |
| **Natural Mortality M**  | 0.4309     | 0.4678    |
| **Terminal SSB (2024)**  | 89 958 t   | 92 522 t  |
| **B2024/B0**             | 0.233      | 0.242     |
| **2025 OFL**             | 35 141 t   | 38 536 t  |
| **2025 ABC**             | 24 124 t   | 27 308 t  |

Growth is what the CAAL mainly informs and it's what moves: length at age 1 by about 16%, and M
rises with it. Status isn't far off (B/B0 0.233 to 0.242), so I don't think this is a tier or
status question, but the OFL goes up about 10% and the ABC about 13%. Happy to send you the
corrected data file to compare.

You can also use a newer SS3 (v3.30.25 and above) and set Lbin_method = 3, where the columns are
the actual lengths, so 9.5 13.5 for that bin. Note Lbin_hi is the lower edge of the last bin
included rather than the top of the interval, so 13.5 and not 14.5. One that looks right but
isn't is Lbin_method = 2 with the data bin number: SS3 converts each end to the population bin at
that data bin's lower edge, so 7 7 gives one bin and 7 8 gives six, neither of which is the five
you want.

The AI Pacific cod model has a version of the same thing, which I've written to Ingrid about
separately. It's much smaller there (OFL about 1% lower) since that CAAL is already on 1 cm bins.

Sorry that was a lot and if this has already come up or intentional, sorry to rehash!

Cheers,
Grant
