# HANDOFF — RCEATTLE estimation model for the ASSAMC comparison

This is the RCEATTLE estimation model (EM), written to slot into the ASSAMC
framework the same way `run_fims()` / `run_wham()` do. It reads an operating-model
(OM) replicate, fits three recruitment scenarios, and writes one set of
per-scenario result files per replicate. Everything below is what you need to
drop it into your pipeline and run; `README.md` has the deeper
design notes and `demo_run_rceattle.R` is a self-contained smoke test.

I've verified it end-to-end against your bundled OM cases.

---

## 1. The files

| File | What it is | Goes where |
|---|---|---|
| `run_rceattle.R` | EM runner `run_rceattle(maindir, subdir, om_sim_num, casedir, em_bias_cor)` + helpers. Same signature/side-effect contract as `run_fims`/`run_wham`. | copy into ASSAMC `R/` |
| `om_to_rceattle.R` | Translates `om_input`/`om_output`/`em_input` → an RCEATTLE `data_list`. The analogue of `prepare_data_fims()`. | copy into ASSAMC `R/` |
| `run_em_patch.R` | The one dispatch line to add to `R/run_em.R`. | apply to `run_em.R` |
| `demo_run_rceattle.R` | Local test + OM-vs-EM figure. Not part of the package. | keep here / your scratch |
| `README.md` | Contract, scenario table, modelling-choice rationale. | reference |

---

## 2. One prerequisite: install RCEATTLE

RCEATTLE is a TMB/C++ package, so installing it compiles C++ (needs Rtools on
Windows / Xcode CLT on macOS / build-essential on Linux — same story as WHAM).

```r
install.packages(c("TMB", "dplyr", "foreach", "doParallel"))
remotes::install_github("kaskr/TMB_contrib_R/TMBhelper")   # RCEATTLE optimiser helper
remotes::install_github("grantdadams/Rceattle")            # main branch = released API
```

The scripts use only the released public API (no dev-branch functions).

---

## 3. Wire it into the pipeline

1. Copy `run_rceattle.R` and `om_to_rceattle.R` into ASSAMC's `R/`.
2. Add the dispatch line from `run_em_patch.R` to `R/run_em.R` (right next to the
   `run_wham` / `run_fims` lines):

   ```r
   if("RCEATTLE" %in% em_names) run_rceattle(maindir=maindir, om_sim_num=om_sim_num, casedir=casedir, em_bias_cor=em_bias_cor)
   ```
3. Include `"RCEATTLE"` in `em_names`. That's it — it then behaves like any other
   EM: cleans `casedir/output/RCEATTLE`, makes `s1..s{om_sim_num}`, and fits every
   replicate in parallel (`detectCores()-2` workers, `TMB::openmp(1)` per worker).

No return value; it works entirely by writing result files (§4).

---

## 4. Scenarios and output-file schema

Three scenarios per replicate, mirroring the FIMS/WHAM recruitment axis:

| suffix | recruitment deviations | σ_R |
|---|---|---|
| `random_effects` | random effect | estimated |
| `random_effects_sigmaR_constant` | random effect | fixed at OM value |
| `fixed_effects` | penalized fixed effect | fixed |

Per scenario, written to `.../RCEATTLE/s{i}/` (names chosen to line up with the
FIMS/WHAM outputs your post-processing already reads):

- `fit_rceattle_<scn>.RDS` — tidy estimates: `label` (SSB / biomass /
  recruitment / F) × `year` × `estimate` × `uncertainty` (SE). **This is the one
  to reconcile with `read_plot_data.R` — see §8.**
- `full_fit_rceattle_<scn>.RDS` — the whole fitted object (fallback / debugging).
- `run_time_rceattle_<scn>.RDS` — `c(fit_optimization, fit_sdreport, fit_total,
  total)` in **seconds** (RCEATTLE doesn't split optimisation vs sdreport, so
  those two are NA; `total` is wall-clock).
- `optimizer_convergence_rceattle_<scn>.RDS` — 0/1 (see §6).
- `max_gradient_rceattle_<scn>.RDS`, `hessian_rceattle_<scn>.RDS` (pdHess),
  `na_count_<scn>.RDS`, `condition_number_<scn>.RDS`.

---
