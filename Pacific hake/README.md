# Pacific hake — CEATTLE

Hake with arrowtooth flounder (ATF) and sablefish (SBF) as predators. Hake is
fully estimated; ATF and SBF enter with fixed numbers-at-age and impose predation
mortality on hake.

Run the scripts from this directory (paths are relative to it), in order.

| Script | What it does | Reads |
|---|---|---|
| `01-prepare-weight-data.R` | Pulls weight-at-age from the 2022 SS3 assessment via `r4ss::SS_output()` | `Data/hake-2022-model-files/` |
| `02-fit-multispecies.R` | Single-species and cannibalism fits, old vs new data | `Data/hake_intrasp_250207.xlsx`, `Data/2024_hake_ATF.xlsx` |
| `03-fit-suitability.R` | Estimated-suitability multispecies fit | `Data/102525_hake_sbf_atf.xlsx` |
| `04-mse.R` | The hake + ATF + SBF MSE. Four staged fits (single-species, single-species under a category-1 HCR, MSVPA, estimated suitability), then `run_mse()` | `Data/300426_SBF_ATF_Hake_Final.xlsx` |
| `05-analysis-dm.R` | Dirichlet-multinomial comparison against the multinomial | `Data/300426_SBF_ATF_Hake_Final.xlsx` |

`Dev/` is scratch work in progress; `Deprecated/` is kept for reference and is
not expected to run.

## Data

Workbooks live in `Data/`. The date prefixes are not in a single format — the
files came from different sources — so go by the table above rather than by the
name.

Three SS3 run directories are on disk but **not tracked** (see `.gitignore`):
`Data/SS32019/`, `Data/2024hakeassessmentfiles/`, and
`Data/hake-2022-model-files/plots/`. `Data/hake-2022-model-files/` itself *is*
tracked, because `01-prepare-weight-data.R` reads it.

## Known issues

- **`05-analysis-dm.R` does not run as written.** It reads
  `results/models_final/ms_LN_run_refit.Rdata` and
  `results/Models_July11/Comps_DM/...Rdata` from a `results/` directory that is
  not in the repository. It also still sets the deprecated `Comp_loglike`
  column (now `Comp_distribution`).
- **`02-fit-multispecies.R` fails on `Data/2024_hake_ATF.xlsx`.** That workbook
  is internally inconsistent: its control sheet says `nspp = 1, nsex = 1`, while
  `weight` and `M1_base` both carry species 1 and 2. `data_check()` rejects it.
  The `hake_intrasp_250207.xlsx` fits in the same script run fine. This is a
  data problem, not a package one — it fails identically on Rceattle 4.9.1 and
  5.6.1.
- `Data/2022 Model/Prepare hake weight data.R` is a byte-identical copy of
  `01-prepare-weight-data.R`.

## Reference values

`04-mse.R` reproduces, on Rceattle 5.6.1:

| Stage | −log L |
|---|---|
| single-species | 2133.82 |
| single-species + category-1 HCR | 2134.47 |
| MSVPA, estimated M | 2137.44 |

These match the values the script's header records as the "clean" numbers —
i.e. excluding the three `theta_diet` prior constants. From 5.6.x, Rceattle
drops a composition-weight prior placed on a weight it is not estimating and
says so, so the constants no longer need subtracting by hand.
