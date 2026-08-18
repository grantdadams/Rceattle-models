# Rceattle models

Assessment models built with [Rceattle](https://github.com/grantdadams/Rceattle),
the R/TMB implementation of CEATTLE. One top-level folder per stock (or per group
of related experiments); each is a self-contained RStudio project.

`Rceattle install.R` installs the package and its dependencies.

## Stocks

| Folder | Stock | Notes |
|---|---|---|
| `EBS pollock` | Eastern Bering Sea walleye pollock | Scripts split by assessment year (`2024/`); bridged from the ADMB assessment (`ADMB/m23`); also carries the RTMB port, the DSEM run, and an MSE. See its README |
| `GOA pollock` | Gulf of Alaska walleye pollock | Scripts split by assessment year (`2021/`–`2025/`); bridged against `goa_pk` |
| `GOA cod` | GOA Pacific cod | Bridged from Stock Synthesis (`Bridging/`, `ss3-source-code-main/`) |
| `AI cod - Dev` | Aleutian Islands Pacific cod | In development; SS3 bridging |
| `GOA arrowtooth flounder` | GOA arrowtooth flounder | 2023 assessment, alternative model runs, and a report |
| `GOA sablefish` | Alaska sablefish | 2019 scripts plus the vendored `Sablefish-master` assessment |
| `GOA northern rockfish` | GOA northern rockfish | 2022 and 2024 bridging against the ADMB model |
| `GOA pop` | GOA Pacific ocean perch | 2023 bridging and comparison |
| `BSAI alaska plaice` | BSAI Alaska plaice | 2021 bridging |
| `BSAI atka mackerel` | BSAI Atka mackerel | 2022 bridging against AMAK |
| `BSAI northern rock sole` | BSAI northern rock sole | 2022 bridging against the ADMB model |
| `BSAI pop` | BSAI Pacific ocean perch | 2024 bridging |
| `BSAI yellowfin sole` | BSAI yellowfin sole | 2022 assessment and environmental-sensitivity runs |
| `Pacific hake` | Pacific hake with arrowtooth and sablefish predators | Multispecies; see its README |
| `Jack mackerel` | Jack mackerel | Bridged from the JJM model (`Data/JJM_2024.xlsx`) |
| `Adriatic - Dev` | Adriatic Sea multispecies | Teaching/course material, in development |

## Multispecies and cross-cutting

| Folder | Contents |
|---|---|
| `GOA CEATTLE` | The GOA multispecies model (pollock, Pacific cod, arrowtooth). `Model runs/GOA_18` … `GOA_25` are the successive assessment vintages |
| `Experiments` | Method work that is not a single stock: `BSAI CEATTLE` (Holsman 2022), `Selectivity experiments`, `SigmaR profiles` |
| `Model comparison` | Rceattle as an estimation model in the NOAA model-comparison project |
| `ADMB_models` | Reference ADMB/SS sources kept for bridging (`ceattle.tpl`, `SS-3.24U.tpl`, `mlmak`, …) |

## Conventions

`GOA pollock`, `EBS pollock`, and `Pacific hake` are the reference layout for a
stock folder:

```
<Stock>/
  <Stock>.Rproj      RStudio project, named after the folder
  README.md          what each script does, what it reads, known issues
  .gitignore         the stock's large regenerable ADMB/SS3 output
  Data/              input workbooks and assessment files
  <year>/            scripts for one assessment year, numbered in run order:
                     01-build-data.R, 02-bridge.R, 03-model.R, ...
```

Older folders instead keep scripts at the top level named
`<year> <stock> [bridging].R`, which run in the same build → bridge → fit order.

Large, regenerable run output (ADMB and SS3 directories, fitted `.Rdata`, plot
dumps) should be ignored per folder rather than committed — see
`GOA pollock/.gitignore` and `Pacific hake/.gitignore`, which ignore the run
directory and then un-ignore by name the few files a script actually reads.
The repo-wide `.gitignore` covers only session, OS, and build scratch.
