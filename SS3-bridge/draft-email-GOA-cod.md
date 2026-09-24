**Draft — GOA Pacific cod (Model 19.1e). Grant's register, matching the AI email. Not sent.**

Subject: Possible issue with the conditional age-at-length bins in the GOA Pacific cod model

Hi Pete,

Claude and I have been bridging the GOA Pacific cod model over to Rceattle. While trying to get
the two models to agree, I think I've found a problem in how the CAAL data are being read. Stock
Synthesis is super complex, so I may have misread something, or maybe this was intentional. If so
please ignore!

I think every CAAL row is being compared against predictions for a length bin one bin *below* the
one it's labelled with. A row labelled 34.5 cm is compared with the predicted ages of the 1 cm
bin at 33.5 cm. The observations are fine; it's the expected values they're compared with that
might be off. It applies to all 827 CAAL rows.

The age composition section sets Lbin_method = 1, telling SS3 that the Lbin_lo and Lbin_hi columns
hold population length bin numbers. The file has lengths there instead — 4.5, 9.5, 14.5 ... the
data bin edges — with Lbin_hi = Lbin_lo. Taking the 34.5 34.5 row:

1. SS3 stores both columns as integers (SS_readdata_330.tpl:2448, assigned at :2586), so
   34.5 34.5 becomes 34 34. Population bins run 0.5, 1.5, 2.5 ... so bin 34 is 33.5 cm, one bin
   below the label.
2. It reads the pair as a range of bins and sums the row's expected conditional age composition
   over every bin in it (SS_expval.tpl:631). With Lbin_hi = Lbin_lo that range is one 1 cm bin.

To verify this, I looked at the FIT_AGE_COMPS section of Report.sso, which writes each row's
Lbin_lo and Lbin_hi back out as the lengths of the bins SS3 actually used. The data writes lengths
in those columns and the two should read the same. The first CAAL row lines up like this:

GOAPcod2024Oct17_1e_5cm.dat   2007 1 1 0 0 2  34.5 34.5  0.14 ...
Report.sso, FIT_AGE_COMPS     1 FshTrawl 1 2007 1 2 7 2007.5 0 0 2  33.5 33.5  _ _ 0.14 ...

The data's values run 4.5–104.5 while Report.sso reports 3.5–103.5, every cell one bin low. Both
GOA runs I have (goa_pcod and goa_pcod-no init and ramp) report the same, so I don't think it's
something I introduced.

**The part I can't work out from the files is how wide each row is meant to be**, and it changes
the answer a lot, so I'd rather ask than guess. The Lbin_lo values are exactly your 21 5 cm data
length bin edges and nothing else, which made me wonder whether each row holds the ages of fish
from a whole 5 cm bin. But it could equally be a single 1 cm bin that just happens to sit on each
data bin edge. Those need different fixes:

- **one 1 cm bin at the label** — set Lbin_lo = Lbin_hi to the population bin number holding that
  length (bin = length + 0.5), so the 34.5 row becomes `35 35`;
- **the whole 5 cm data bin** — set them to the population bins it spans, so `35 39` (34.5 through
  38.5 cm). Note SS3 needs the range written out; Lbin_hi = Lbin_lo gives one bin whatever
  Lbin_method says.

I fitted both. The observations are identical in all three runs — only the predicted cell
definition changes — so the likelihoods are comparable:

| Metric                   | As Written | 1 cm at label | whole 5 cm bin |
| :----------------------- | :--------- | :------------ | :------------- |
| **Total Likelihood**     | 2048.07    | **2045.71**   | 2051.98        |
| **Age comp**             | 721.20     | 723.68        | 732.79         |
| **Length comp**          | 1336.33    | 1332.92       | 1331.85        |
| **Mean Length at Age 1** | 9.28 cm    | 9.85 cm       | 10.81 cm       |
| **von Bertalanffy K**    | 0.1910     | 0.1947        | 0.2039         |
| **Natural Mortality M**  | 0.4309     | 0.4412        | 0.4678         |
| **Terminal SSB (2024)**  | 89 958 t   | 89 908 t      | 92 522 t       |
| **B2024/B0**             | 0.233      | 0.235         | 0.242          |
| **2025 OFL**             | 35 141 t   | 35 833 t      | 38 536 t       |
| **2025 ABC**             | 24 124 t   | 24 724 t      | 27 308 t       |

So the one-bin fix moves the OFL about 2% and the 5 cm fix about 10%. The 1 cm reading gives the
lowest total likelihood of the three, which is mild evidence for it, but that's a fit comparison
and not proof of what the data mean — you'd know from how the ages were tabulated.

Either way growth is what the CAAL informs and what moves, and M rises with it. Status is close
in all three (B/B0 0.233 to 0.242), so I don't think this is a tier or status question. Happy to
send you either corrected data file to compare.

You can also use a newer SS3 (v3.30.25 and above) and set Lbin_method = 3, where the columns are
the actual lengths. Note Lbin_hi is the lower edge of the last bin included rather than the top of
the interval, so a 5 cm bin at 9.5 is `9.5 13.5`, not `9.5 14.5`. What doesn't work is
Lbin_method = 2 with the data bin number: SS3 converts each end to the population bin at that data
bin's lower edge, so `7 7` gives one bin and `7 8` gives six.

The AI Pacific cod model has a version of the same thing, which I've written to Ingrid about
separately. There the data bins are already 1 cm so there's no ambiguity about width, and the
effect is smaller (OFL about 1% lower).

Sorry that was a lot and if this has already come up or intentional, sorry to rehash!

Cheers,
Grant
