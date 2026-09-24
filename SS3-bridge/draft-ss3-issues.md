**Drafts — two GitHub issues for the CAAL length-bin problem. Not filed.**

Verdict on the manual: **not well documented.** The relevant passages are §7.16.3.2 ("Lbin Low
and Lbin High") and §7.17 ("Conditional Age-at-Length"). Between them they state the three bin
methods and that the reported values are converted back to length, but they never say the values
must be whole numbers under methods 1 and 2, never say what happens if they are not, and the one
worked CAAL example does not say which method it assumes. Searching the manual for
`Lbin_method` returns nothing — it is called "Bin method for age data" there, while the data file
calls it `Lbin_method_for_Age_Data`.

Nothing in `nmfs-ost/ss3-doc` issues mentions `Lbin` at all. Four closed issues in
`ss3-source-code` do (#247, #556, #557, #739), but all concern method 3 or the reporting of
`Lbin_lo` in `FIT_AGE_COMPS` vs `CompReport.sso` — none is this.

---

# Issue A — for `nmfs-ost/ss3-source-code`

**Title:** [Bug]: non-integer `Lbin_lo`/`Lbin_hi` are silently truncated under `Lbin_method = 1`,
while method 3 raises a fatal error for the equivalent mistake

### Describe the bug

Under `Lbin_method = 1` the `Lbin_lo`/`Lbin_hi` columns of an age composition observation are
population length bin **numbers**. If a file instead contains **lengths** — an easy mistake, and
one the manual does not warn against — SS3 accepts them silently and truncates them to integers,
so every conditional age-at-length row is fitted against the wrong length bins. There is no
warning in `warning.sso` and the run completes normally.

Method 3 already does the right thing for the same class of error: it searches `len_bins` for an
exact match and calls `write_message(FATAL, 0)` with `"L_bin_lo no match to poplenbins in age
comp"` if there is none. Method 1 has no equivalent check, only a clamp to `[1, nlength]`. That
asymmetry is the bug — the safest method is the one that is strict, and the default method is the
one that is silent.

Two consequences, depending on how the file is written:

- `Lbin_lo` and `Lbin_hi` one bin apart (e.g. `18.5 19.5`, intended as the edges of one 1 cm
  bin) becomes index range `18..19`, so the cell spans **two** population bins and consecutive
  rows **overlap**.
- `Lbin_hi = Lbin_lo` (e.g. `34.5 34.5`, a row on a 5 cm data bin grid) becomes bin 34, a single
  1 cm bin one below the label.

In both cases the observations are untouched; it is the expected values they are compared with
that are misplaced. Because growth is largely what conditional age-at-length data inform, the
result is a shifted growth curve rather than an obviously bad fit.

### To Reproduce

In any model with 1 cm population bins and `Lbin_method = 1`, write lengths rather than bin
numbers in columns 7 and 8 of a conditional age-at-length row:

```
#_Lbin_method_for_Age_Data: 1=poplenbins; 2=datalenbins; 3=lengths
1
#_yr month fleet sex part ageerr Lbin_lo Lbin_hi Nsamp <data vector>
 1991   7     2    0    0     1     18.5    19.5      1  ...
```

Run, then compare the input against the `FIT_AGE_COMPS` section of `Report.sso`, which reports
these columns converted back to actual length and so should read the same as the input:

```
data.ss                     1991 7 2 0 0 1  18.5 19.5  1 ...
Report.sso FIT_AGE_COMPS    ... 1991 1 2 7 1991.5 0 0 1  17.5 18.5  _ _ 1 ...
```

Tested on **v3.30.25.1** (macOS, current release at the time of writing) on the 2024 Aleutian
Islands Pacific cod assessment: all 1160 CAAL rows come back one bin low, the run exits 0, and
`warning.sso` contains six warnings, none of which mentions `Lbin`. v3.30.22.1 gives the
identical total likelihood (531.003), so the behaviour is unchanged across those releases.

### Expected behavior

A non-integer `Lbin_lo` or `Lbin_hi` under `Lbin_method = 1` or `2` should produce a warning, or
a fatal error, as method 3 already does for its own equivalent mistake. Anything that makes the
run stop or complain would have caught this immediately.

### Additional context

The containers changed from `imatrix` to `matrix` between v3.30.22.1 and current `main`
(`SS_readdata_330.tpl:2452-2453`), which moved where the truncation happens but did not remove
it: the values are still used as subrange indices at

```cpp
Lbin_filter(f, j)(Lbin_lo(f, j), Lbin_hi(f, j)) = 1;
```

and the adjacent two-sex branch casts explicitly with `int(Lbin_lo(f, j))`.

A minimal fix in the `case 1:` branch of the `switch (Lbin_method)` at
`SS_readdata_330.tpl:2594`, and the same for `case 2:`:

```cpp
if (Lbin_lo(f, j) != floor(Lbin_lo(f, j)) || Lbin_hi(f, j) != floor(Lbin_hi(f, j)))
{
  warnstream << "Lbin_lo/Lbin_hi must be whole bin numbers under Lbin_method " << Lbin_method
             << "; got non-integer values in age comp " << header_a(f, j)
             << ". Did you mean Lbin_method = 3 (actual lengths)?";
  write_message(FATAL, 0);
}
```

This is not hypothetical: the 2024 Aleutian Islands and Gulf of Alaska Pacific cod assessments
both write lengths under `Lbin_method = 1`, and both still do in their most recent files.
Correcting the AI model moves mean length-at-age up 0.2–0.5 cm and the 2025 OFL down 1.25%;
correcting GOA moves its 2025 OFL up about 10%. These are US federal catch limits, and the error
is invisible in the run output.

I am happy to open a PR with the check and a test if that would help.

---

# Issue B — for `nmfs-ost/ss3-doc`

**Title:** CAAL example in §7.17 does not say which bin method it assumes, and the manual never
states that `Lbin_lo`/`Lbin_hi` must be whole numbers under methods 1 and 2

### What is unclear

**1. §7.17's worked example does not state its bin method, and its narrative reads as lengths.**
The example is

```
Year Month Fleet Sex Partition AgeErr Lbin_lo Lbin_hi Nsamp Data Vector
1987   1     1    1      0       2      10      10     18   <data values>
1987   1     1    1      0       2      12      12     24   <data values>
1987   1     1    1      0       2      14      14     16   <data values>
1987   1     1    1      0       2      16      16     30   <data values>
```

described as "the age data is treated as on being conditional on the 2 cm length bins of
10–11.99, 12–13.99, 14–15.99, and 16–17.99 cm."

Read on its own, that says the numbers 10, 12, 14, 16 are lengths in cm and each row covers 2 cm
— which corresponds to `Lbin_method = 3`, and even then only if those lengths are population bin
boundaries and `Lbin_hi` is the lower edge of the last bin included. Under `Lbin_method = 1`,
which §7.16.3.2 lists first, the same numbers are population bin *indices* and the stated cm
ranges would not follow. Since this is the manual's only conditional age-at-length example, it
would help to state the method it assumes, and ideally to show the same four rows under each
method.

**2. The manual never says the values must be whole numbers under methods 1 and 2.** §7.16.3.2
says only that "Whether these are entered as population bin number, length data bin number, or
actual length is controlled by the value of the length bin range method above." It does not say
what happens to a value like `34.5` under method 1. In practice SS3 truncates it silently and
fits the row against the wrong bins (filed separately against `ss3-source-code`). A sentence
would prevent it:

> Under methods 1 and 2 these are bin numbers and must be whole numbers. A value such as 34.5 is
> not a bin number; it will be truncated to bin 34 and the observation will be fitted against the
> wrong length bins. Use method 3 if you want to enter actual lengths.

**3. The check that detects the mistake is not described as a check.** §7.16.3.2 notes that "In
reporting to the `comp_report.sso`, the reported `Lbin_lo` and `Lbin_hi` values are always
converted to actual length." That round-trip is the one way a user can confirm SS3 used the bins
they meant, but the manual presents it as a reporting detail. Worth adding that comparing those
reported values against the input is a quick way to verify the rows were interpreted as intended
— with the caveat that they only agree by construction when the input was written as lengths.

**4. Method 3's `Lbin_hi` convention deserves a sentence.** §7.16.3.2 says method 3 values "must
correspond to population length bin boundary", but not that `Lbin_hi` is the lower edge of the
**last bin included** rather than the top of the interval. A 5 cm cell starting at 9.5 cm on a
1 cm population grid is `9.5 13.5`, not `9.5 14.5`.

**5. Minor:** the switch is called "Bin method for age data" in the manual and
`Lbin_method_for_Age_Data` in the data file, so searching the manual for the name a user sees in
their file finds nothing. Worth naming it both ways once.

### Context

Found while bridging the Aleutian Islands and Gulf of Alaska Pacific cod assessments to another
model. Both write lengths under `Lbin_method = 1`; both are affected.
