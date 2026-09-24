**Draft — EBS Pacific cod (November 2024 models). Written for the case where Steve wants to turn
the CAAL rows back on and test them. Grant's register, matching the AI and GOA emails. Not sent.**

Subject: EBS Pacific cod — the CAAL rows need converting before they're switched on

Hi Steve,

Claude and I have been bridging the AI and GOA Pacific cod models over to Rceattle, and along the
way we found a problem in how SS3 reads the CAAL length-bin columns. It bites in both of those.
I checked EBS too — it doesn't bite there, but only because the CAAL rows aren't currently being
fitted. Since you're thinking about turning them on, here's what I'd change first, because as
written I think they'd come in wrong.

**Where things stand now.** In 24.0 through 24.3 there are 960 single-bin CAAL rows, all on
negative fleet numbers, so SS3 skips them and `condbase` comes out empty. 23.1.0.d has none at
all, reading the 1 cm data file. In all five models the only age data in the likelihood are the
same 23 marginal rows — fleet 2, 2000–2023, written `1.5 119.5` for the whole length range — and
those carry the entire `Age_comp` likelihood.

**What would happen if you flipped the fleet signs as-is.** The age composition section sets
`Lbin_method = 1`, which tells SS3 that `Lbin_lo` and `Lbin_hi` hold population bin *numbers*.
The columns contain lengths instead — 4.5, 9.5, 14.5 … your 5 cm data bin edges. SS3 stores both
columns as integers (`SS_readdata_330.tpl:2448`, assigned at `:2586`), so 34.5 becomes 34, and
then uses that as a bin index with no conversion.

EBS is worse off here than GOA, and it's worth knowing why. Your population bins come from an
explicit vector that starts `0.001, 0.5, 1.5, …` — an extra bin at the bottom compared with the
other two stocks. That makes bin *k* equal to *k* − 1.5 cm rather than *k* − 0.5, so the
truncation lands **two bins low** instead of one:

| row labelled | SS3 would use | correct bin number |
| :----------- | :------------ | :----------------- |
| 4.5 cm  | bin 4 = 2.5 cm   | bin 6 |
| 9.5 cm  | bin 9 = 7.5 cm   | bin 11 |
| 14.5 cm | bin 14 = 12.5 cm | bin 16 |
| 34.5 cm | bin 34 = 32.5 cm | bin 36 |
| 114.5 cm | bin 114 = 112.5 cm | bin 116 |

The rule on your grid is **bin = length + 1.5**.

**The other half of it is how wide each row is meant to be**, and it changes the fix. Your
`Lbin_lo` values are exactly 23 of your 24 5 cm data bin edges and nothing else, with
`Lbin_hi = Lbin_lo`. I think each row holds every aged fish in a whole 5 cm bin, for two reasons,
though you're much better placed to say than I am.

The first is `cond_length_age_cor.r` in the assessment functions. It bins every aged fish to
whatever grid it's handed and sums all the fish in a bin into one row:

```r
length$BIN[length$LENGTH < len_bins1[((n-i)+1)]] <- len_bins1[n-i]
...
Agecomp_obs[,8] <- Agecomp_obs[,7] <- as.numeric(substr(Agecomp_lengths,5,10))
```

That last line writes the bin's label into *both* columns, so `Lbin_hi = Lbin_lo` is the label
written twice rather than a statement that the cell is 1 cm wide. Since the rows in the 5 cm
files carry the 5 cm grid's values, the grid it was handed was the 5 cm one, and each row is a
5 cm aggregate. (The caveat is that `MAIN_BS_PCOD.r` sets `len_bins` to 1 cm, and that's the
pipeline behind `BSPcod24_OCT_1cm.dat` — which has no CAAL rows at all. I couldn't find the
script that built the 5 cm files, so I'm inferring from the function rather than reading the
call.) The second reason is just magnitude: the CAAL rows total 860–1470 fish a year, which
looks like a full annual ageing sample; if each row were a 1 cm slice, the real aged sample would
have to be five times that.

If it is the 5 cm reading, each row needs the *range* of population bins it spans, not one bin:

- the 34.5 row covers 34.5 to 39.5 cm, which is population bins 36 through 40, so `36 40`;
- `Lbin_hi = Lbin_lo` gives a single 1 cm bin whatever `Lbin_method` says, so the range has to be
  written out explicitly.

If each row really is a single 1 cm bin sitting on a data bin edge, it's just `36 36`.

This is exactly the question that came up for GOA, where it changes the OFL effect by about 5x,
so it's worth being sure before reading anything into the fitted result.

**One thing that is already in the likelihood, and doesn't matter.** The 23 active marginal rows
hit the same truncation: written `1.5 119.5`, they truncate to bins 1 and 119, which on your
vector are 0.001 cm and 117.5 cm, so the top two bins get left out of the predicted composition.
Writing `1 121` instead puts them back and changes essentially nothing, because EBS cod don't
reach 118 cm:

| quantity | as written | corrected |
| :------- | :--------- | :-------- |
| age composition | 55.6440 | 55.6452 |
| total likelihood | 243.416 | 243.417 |
| SSB 2024, B/B0, 2025 OFL, 2025 ABC | — | **0.000% change** |

I'd leave that alone; I'm only mentioning it so you know I checked rather than assumed.

Happy to send you a converted data file with the CAAL columns as bin numbers — either reading —
so you can switch the rows on and see what they do without doing the conversion yourself. Say
which and I'll put it together.

Two caveats on my end. The v3.30.21 macOS binary wouldn't read the data file for me
(`Incompatible array bounds in dmatrix`), so my runs are v3.30.22.1; the truncation behaviour is
identical across every release through 3.30.24, so which bins get used is unaffected, but those
likelihood numbers are from my runs rather than the assessment's own. And I only looked at the
November 2024 models from the `EBS_PCOD` repo (`APPENDIX_2.3_2024_MODELS.zip`) — I haven't
checked this year's.

Worth saying the fix is only needed on 3.30.24 and earlier. From v3.30.25 you can set
`Lbin_method = 3` and write actual lengths, though `Lbin_hi` is then the lower edge of the last
bin included rather than the top of the interval, so a 5 cm bin at 34.5 is `34.5 38.5`.

For what it's worth, the same underlying issue does bite in AI and GOA, where the CAAL rows are
active — I've written to Ingrid and Pete about those separately.

Sorry that was a lot!

Cheers,
Grant
