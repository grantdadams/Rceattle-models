**Draft — EBS Pacific cod (November 2024 models). Grant's register, matching the AI and GOA
emails. Not sent.**

Subject: EBS Pacific cod — are the conditional age-at-length rows meant to be switched off?

Hi Steve,

Claude and I have been bridging the AI and GOA Pacific cod models over to Rceattle, and along the
way we found a problem in how SS3 reads the CAAL length-bin columns in those two. I checked the
EBS models to see whether it affected them too. **It doesn't** — but the reason it doesn't is
something I wanted to ask you about, in case it's not intentional.

Short version: none of the November 2024 EBS models fits conditional age-at-length data at all.

In 24.0 through 24.3 there are 960 single-bin CAAL rows, and every one of them is on a negative
fleet number, so SS3 skips them — `condbase` comes out empty. 23.1.0.d has no single-bin rows
at all, since it reads the 1 cm data file. In all five models the only age data in the likelihood
are the same 23 marginal rows: fleet 2, 2000–2023, written `1.5 119.5`, i.e. the whole length
range. Those 23 rows carry the entire `Age_comp` likelihood.

| model | data file | single-bin CAAL rows | of which active | full-range rows | active |
|---|---|---|---|---|---|
| 23.1.0.d | `BSPcod24_OCT_1cm.dat` | 0 | 0 | 30 | 23 |
| 24.0 | `BSPcod24_OCT_5cm.dat` | 960 | **0** | 30 | 23 |
| 24.1 | `BSPcod24_OCT_5cm_NB.dat` | 960 | **0** | 30 | 23 |
| 24.2 | `BSPcod24_OCT_5cm_NB.dat` | 960 | **0** | 30 | 23 |
| 24.3 | `BSPcod24_OCT_5cm_NB.dat` | 960 | **0** | 30 | 23 |

If that's deliberate — you moved to marginal ages and left the CAAL rows in the file as a record —
then there's nothing to do and you can stop reading here. I mostly wanted to check, because it's
the kind of thing that can happen by accident when a data file gets rebuilt, and from the file
alone I can't tell which it is.

**The one thing worth knowing before you ever switch them back on.** Those 960 rows are written in
the same style that causes the problem in the AI and GOA models: `Lbin_method = 1`, which tells
SS3 the `Lbin_lo`/`Lbin_hi` columns hold population bin *numbers*, but the columns contain lengths
(4.5, 9.5, 14.5 …). SS3 stores those columns as integers, so 34.5 becomes 34 and the row is fitted
against the predicted ages of population bin 34 rather than the length it's labelled with.

EBS is worse than GOA here, because your population bins come from an explicit vector that starts
`0.001, 0.5, 1.5, …` — one extra bin at the bottom. That makes bin *k* equal to *k* − 1.5 cm
rather than *k* − 0.5, so a row labelled 34.5 would land on **32.5 cm, two bins low** instead of
GOA's one. So if those rows are ever enabled, they'd want converting to bin numbers at the same
time rather than just flipping the fleet sign.

**The 23 active rows do hit the same truncation, but it doesn't matter.** Written `1.5 119.5`,
they truncate to bins 1 and 119, which on your vector are 0.001 cm and 117.5 cm — so the top two
bins (118.5 and 119.5 cm) are dropped from the predicted composition. Writing `1 121` instead
puts them back:

| quantity | as written | corrected |
|---|---|---|
| age composition | 55.6440 | 55.6452 |
| total likelihood | 243.416 | 243.417 |
| SSB 2024, B/B0, 2025 OFL, 2025 ABC | — | **0.000% change** |

Nothing moves, because EBS cod don't reach 118 cm. I'd leave it alone; I'm only mentioning it so
you know I checked rather than assumed.

Two caveats on my end. The v3.30.21 macOS binary wouldn't read the data file for me
(`Incompatible array bounds in dmatrix`), so both runs above are v3.30.22.1 — the truncation
behaviour is the same in every release through 3.30.24, so which bins get used is unaffected, but
those likelihood numbers are from my runs, not from the assessment's own. And I only looked at the
November 2024 models from the `EBS_PCOD` repo (`APPENDIX_2.3_2024_MODELS.zip`) — I haven't checked
this year's.

For what it's worth, the underlying bin issue does bite in AI and GOA, where the CAAL rows are
active — I've written to Ingrid and Pete about those separately.

Sorry that was a lot, and if the switched-off rows are intentional, sorry to rehash!

Cheers,
Grant
