# =============================================================================
# GOA pollock 2025 --  compares Rceattle against goa_pk SAFE model).
# Fits the production setup (DM comps, AR1/Ecov catchability
# on the Shelikof survey index, selectivity priors, initMode = "OffsetEquilibrium")
# and overlays the SAFE derived quantities.
#
# Run "2025 pollock build data.R" (-> GOA_25_pollock.Rdata) and, for the
# comparison, fitted goa_pk object in Data/2024pollock.Rdata.
#
# =============================================================================
#
# Rceattle conditional negative log-likelihood matches goa_pk's
# -sum(loglik) to +0.016 (1830.74 vs 1830.72); all dynamics (N-at-age,
# selectivity, F, q1-q6, recruitment) and every likelihood component match.
# The only residual (~0.013) is the Dirichlet-multinomial
# linear-parameterization offset ((1 + o*A), o = 1e-5), which is negligible.

# Bridging a bug-corrected goa_pk shows the two models share the same CONDITIONAL
# likelihood at goa_pk's MLEs (the forward pass matches to ~1e-11). Their MARGINAL
# (Laplace) objectives differ only because they integrate different random
# effects -- goa_pk only Ecov_exp; this Rceattle config also integrates the
# recruitment deviations and the q random walk -- a modelling choice, not a
# discrepancy (at the mode the random density equals the penalty).
#
# On UNCERTAINTY, the two sdreports are empirically nearly identical for the
# data-informed quantities: SSB SEs match goa_pk to ~1% in EVERY year and
# recruitment SEs match across the historical series (CV ratios ~1.00). The one
# systematic difference is the recruitment-process SD: Rceattle ESTIMATES it
# (R_sd ~= 1.02) whereas goa_pk FIXES sigmaR = 1.3 (a weakly-identified-
# hyperparameter AFSC convention). So only in the data-poor TERMINAL year, where
# the process SD dominates the recruitment SE, do they diverge -- and there
# Rceattle's is ~20% SMALLER (its tighter estimated SD), NOT larger. Neither is
# "wrong": goa_pk conditions on sigmaR = 1.3, Rceattle estimates it. The free-fit
# conditional difference (~-15 here) is multimodality (a slightly better optimum
# from the OffsetEquilibrium init), not this SD/random-effect difference.
#
# --- (A) Changes made to goa_pk model -----------------------------------
#   Implemented by "00-fit-goa_pk.R", which produces both fitted objects loaded
#   below. A1/A3/A4 are source-side and live in reference/goa_pk_2024_mfix.cpp
#   (a copy of Cole's 2024 source with every edit tagged "GRANT"); A2/A5/A6 are
#   data-side and are applied to input$dat in that script.
#   A1. Initial age-structure M-index bug fix. goa_pk's initN loop used
#       initN(j) = initN(j-1)*exp(-M(j+1)), which skips age-2 M and applies each
#       age's future M. Corrected to exp(-M(j)).
#   A2. Constant q3 random-walk SD. goa_pk used a per-year q3_rwlk_sd vector
#       (0.001 in most years, 0.05 in a few); set to a constant 0.05 for all
#       years (input$dat$q3_rwlk_sd[] <- 0.05) so Rceattle's single-per-fleet RW
#       SD reproduces it. Rceattle does not currently have a per-year RW SD.
#   A3. Removed the q1/q2 random-walk penalty lines. q1 uses the Ecov link and q2
#       is fixed, so their deviates are pinned to 0; their RW penalties were pure
#       dnorm(0,0,rwlk_sd) normalizing constants (~527 units) that do not affect
#       the fit. Rceattle does not define q1/q2 as random walks, so it omits
#       them; dropping them in goa_pk makes the TV-catchability component match.
#   A4. Added the lognormal bias correction (-sd^2/2) to the total-catch.
#       goa_pk bias-corrects the survey indices but not the total catch,
#       Rceattle does both via bias_adjust_obs = TRUE.
#   A5. Normalized the aging-error matrix rows to sum to 1. Rceattle re-normalizes
#       and goa_pk does not. Rows did not sum to 1.
#   A6. Normalized each composition observation to sum to 1. goa_pk fits the raw
#       pk24_12.txt proportions, which sum to ~1.00001;
#       Rceattle normalizes every comp row to 1 in rearrange_data() before
#       fitting.
#
# --- (B) Rceattle package features added ------------------------------------
#   B1. initMode = "OffsetEquilibrium": seeds the initial ages off the first-year
#       recruitment exp(rec_pars + rec_dev[year 1]) with init devs OFF and no
#       init-dev penalty -- goa_pk's convention (vs initMode 1, which seeds off
#       the mean-recruitment equilibrium R0).
#   B2. Selectivity base-parameter priors via the linkage grammar
#       (build_selectivity(... priors = ...)): lognormal() on the log-scale
#       slopes, normal() on the natural-scale inflections.
#   B3. Composition young/old-age accumulation (Comp_accum_young / Comp_accum_old
#       on fleet_control), applied per-sex-block: folds comp tails into the
#       boundary bins and restricts the DM to [young, old] -- goa_pk's ac_yng /
#       ac_old. Fishery folds age-1 into age-2; Shelikof folds ages 1-2 into
#       age-3. THIS FEATURE IS BEING COMMITTED SEPARATELY.
#
# --- (C) Model configuration / mapping in the bridge (see the bridging script)
#   C1. Dirichlet-multinomial age comps on the fishery + srv1/2/3/6
#       (Comp_distribution = "DirichletMultinomial"); length comps stay multinomial.
#   C2. QAR1 catchability on the Shelikof survey via the ar1() grammar with the
#       observed Ecov covariate and goa_pk's fixed measurement SD
#       (Rogers et al. 2024). Legacy Time_varying_q on q1 turned OFF.
#   C3. Selectivity priors mirror goa_pk EXACTLY, including BOTH limbs on srv2
#       (logistic) and srv6 (descending) -- goa_pk priors the inactive limb too
#       (a constant on the fixed parameter), which we reproduce.
#   C4. Dirichlet-multinomial parameter prior: lognormal(0, 2) on theta_comp
#       reproduces goa_pk's dnorm(log_DM_pars, 0, 2).
#   C5. Bottom-trawl (srv2) catchability prior N(log 0.85, 0.1)
#       (Catchability = "Estimated-with-prior").
#   C6. bias_adjust_proc = FALSE: turns off Rceattle's lognormal bias correction
#       on the recruitment-deviate penalty so it matches goa_pk's plain
#       dnorm(dev, 0, sigmaR).
#   C7. DM weights (log theta) set on fleet_control$Comp_weights, NOT inits --
#       fit_mod() sources comp_weights from that column and overrides inits.
#   C8. Survey timing: comp Month = yrfrct * 12. Rceattle applies exp(-(Month/12)
#       * Z), so goa_pk's fraction-of-year yrfrct is scaled to a month. Indexed
#       PER YEAR (srv2's yrfrct varies 0.543/0.584 across years).
#   C9. q3 keeps its legacy random-walk catchability (Cole's log_q3_dev), SD 0.05;
#       the fishery gets its own selectivity block (Selectivity_index = 8).
#   C10. Parameter mapping uses the dev-branch names (log_F, log_sel_slp,
#        index_log_q, R_log_sd, ...), and recruitment is scaled +log(1e6)
#        (goa_pk is in millions, Rceattle in absolute numbers).
#   C11. Source Cole's parameters from fit$opt$par -- the vector fit$rep is
#        reported at. fit$obj$env$parList() is NOT the MLE: it is bit-identical
#        to fit$parList, and both sit EXACTLY 1e-3 off opt$par on log_DM_pars[5]
#        (the srv6 D-M weight). fit_pk() calls obj$report() before sdreport() but
#        reads parList() after, leaving obj$env$last.par behind the optimizer.
#        Every other parameter is bit-identical, so this hides well.
#        Mapping parList() costs ~+2.9e-4 on the composition block and ~-2.9e-4
#        on the selectivity/D-M prior block. Those nearly cancel -- at the
#        optimum the total gradient vanishes, so a small parameter offset moves
#        the two in opposite directions -- leaving a total NLL diff of ~9e-7
#        that reads as agreement while two components are each ~3e-4 out.
#        With opt$par every component matches: composition 3e-11, priors 1e-14,
#        total forward-pass NLL diff ~1.2e-11.
#
# --- (D) Data conversions (see the build-data script) -----------------------
#   D1. Survey indices scaled x1e6 (millions -> absolute numbers).
#   D2. wt_pop = wt_srv2 (bottom trawl), wt_spawn = wt_srv1 (Shelikof).
#   D3. Age-specific M hard-coded (goa_pk vector), not estimated.
#   D4. Composition ESS x2 on srv1/3/6 (already applied in Cole's fit data).
#   D5. Age-1 / age-2 Shelikof indices (fleets 4/5) turned off.
#   D6. Aging-error matrix (age_error) rows normalized to sum to 1 (pairs with A5).
#   D7. Maturity-at-age set to goa_pk's `mat` vector (the 2024 ogive). The GOA_24
#       skeleton carried a slightly older ogive, leaving female spawning biomass
#       ~1-3% low (year-varying with the age structure) even though N-at-age
#       matched to 1e-8. SSB is SSB-independent of the hindcast likelihood here,
#       so this aligns the reported SSB to machine precision WITHOUT moving the
#       fit (forward-pass NLL unchanged at ~1e-11).
# =============================================================================

library(Rceattle)
library(dplyr)
library(ggplot2)
# Run from the "GOA pollock" project root, like the other scripts in this folder.

# The workbook is the canonical data source -- Data/*.Rdata is gitignored, so
# the xlsx is what travels with the repo and what Cole edits directly.
pollock25 <- read_data("Data/GOA_25_pollock_single_species_1970-2024.xlsx")
load("Data/2024pollock.Rdata")           # -> fit  (goa_pk ORIGINAL, published)
fit_orig <- fit
load("Data/2024pollock_mfix.Rdata")      # -> fit  (goa_pk CORRECTED, mfix)
# `fit` is now the corrected goa_pk (M-index fix + catch bias correction +
# aging-error / comp-obs normalization; see RECONCILIATION LOG A1-A6). It is the
# reconciliation target Rceattle reproduces exactly; fit_orig is kept only to
# show what the corrections moved.
# Source Cole's parameters from fit$opt$par -- the vector fit$rep was reported
# at. NOT fit$obj$env$parList(), which is bit-identical to fit$parList and sits
# 1e-3 off the MLE on log_DM_pars[5]; see C11.
pl <- fit$obj$env$parList()          # skeleton: shapes, and the mapped-off pars
.opt <- fit$opt$par                  # the MLE (fixed effects only)
for (.p in unique(names(.opt))) {
  .v <- .opt[names(.opt) == .p]
  if (!is.null(pl[[.p]]) && length(pl[[.p]]) == length(.v))
    pl[[.p]][] <- as.numeric(.v)
}
rm(.opt, .p, .v)
SHELIKOF <- 1L; FISHERY <- 8L
nyrs <- length(pollock25$styr:pollock25$endyr)

# ---- Production configuration (grammar features) ---------------------------
# Kept identical to "2025 pollock bridging.R" (see its comments and the
# RECONCILIATION LOG above) so the free fit lands on goa_pk's configuration.
pollock25$fleet_control$Selectivity_index[FISHERY] <- FISHERY
pollock25$fleet_control$Catchability[SHELIKOF]   <- "Estimated"
pollock25$fleet_control$Time_varying_q[SHELIKOF] <- "Off"
pollock25$fleet_control$Catchability[2] <- "Estimated-with-prior"
pollock25$fleet_control$Catchability_init[2] <- 0.85
pollock25$fleet_control$Catchability_prior_sd[2] <- 0.1
# Composition young-age accumulation (fishery age-1 -> 2; Shelikof ages 1-2 -> 3).
pollock25$fleet_control$Comp_accum_young <- 1L
pollock25$fleet_control$Comp_accum_old   <- 0L
pollock25$fleet_control$Comp_accum_young[FISHERY]  <- 2L
pollock25$fleet_control$Comp_accum_young[SHELIKOF] <- 3L
# DM weights (log theta) starting values from goa_pk (fit_mod sources these from
# fleet_control$Comp_weights); estimated freely from here.
pollock25$fleet_control$Comp_weights[c(FISHERY, 1, 2, 3, 6)] <- pl$log_DM_pars

# Linkages name their fleets rather than numbering them: fit_mod() checks each
# against fleet_control$Fleet_name and errors on a miss, whereas a Fleet_code
# that is wrong but in range attaches the prior to a different fleet and the
# model still fits. SHELIKOF / FISHERY stay above -- they index fleet_control
# rows, which is a position, not a fleet reference.
SHELIKOF_ACOUSTIC <- "Pollock_survey_1_shelikof_acoustic"
ASC_LIMB_PRIOR  <- c("Pollock_survey_2_bottom_trawl",
                     "Pollock_survey_3_adfg",
                     "Pollock_survey_6_summer_acoustic",
                     "GOA_pollock_fishery")
DESC_LIMB_PRIOR <- c("Pollock_survey_1_shelikof_acoustic",
                     "Pollock_survey_2_bottom_trawl",
                     "Pollock_survey_6_summer_acoustic",
                     "GOA_pollock_fishery")
DM_PRIOR        <- c("GOA_pollock_fishery",
                     "Pollock_survey_1_shelikof_acoustic",
                     "Pollock_survey_2_bottom_trawl",
                     "Pollock_survey_3_adfg",
                     "Pollock_survey_6_summer_acoustic")

q_spec <- build_catchability(linkages = list(
  q = linkage_spec(~ ar1(1 | Year), by = ~ fleet, fleet = SHELIKOF_ACOUSTIC,
                   observe = "QcovPol", obs_sd = exp(pl$log_Ecov_obs_sd))))
# Selectivity priors mirror goa_pk exactly (BOTH limbs on srv2 and srv6).
sel_spec <- build_selectivity(linkages = list(
  slp_asc  = linkage_spec(~ 1, by = ~ fleet, fleet = ASC_LIMB_PRIOR,
                          priors = list(`(Intercept)` = lognormal(-1, 1.5))),
  inf_asc  = linkage_spec(~ 1, by = ~ fleet, fleet = ASC_LIMB_PRIOR,
                          priors = list(`(Intercept)` = normal(0, 3))),
  slp_desc = linkage_spec(~ 1, by = ~ fleet, fleet = DESC_LIMB_PRIOR,
                          priors = list(`(Intercept)` = lognormal(-1, 1.5))),
  inf_desc = linkage_spec(~ 1, by = ~ fleet, fleet = DESC_LIMB_PRIOR,
                          priors = list(`(Intercept)` = normal(10, 3)))))
# Dirichlet-multinomial parameter prior (dnorm(log_DM_pars, 0, 2)).
comp_spec <- build_composition(linkages = list(
  theta_comp = linkage_spec(~ 1, by = ~ fleet, fleet = DM_PRIOR,
                            priors = list(`(Intercept)` = lognormal(0, 2)))))

# ---- Forward-pass EXACT equivalence (the replica proof) --------------------
# Fix goa_pk's MLEs in Rceattle (estimateMode = "DebugBuild", no optimization) and confirm
# the two models agree to machine precision -- the actual "exact replica" test
# (the free fit below only shows Rceattle lands at least as good an optimum).
# `pollock_fixed` (from the bridging script) holds the mapped goa_pk MLEs; we
# rebuild here rather than reading its stored jnll so the number reflects the
# currently installed Rceattle. NOTE: the exact match requires an Rceattle build
# that includes the linear Dirichlet-multinomial alpha (comp_obs_tmp.sum(), the
# N*(1+offset*nbins) effective sample size matching goa_pk's sum(otmp)); an older
# build using the raw comp_n leaves a ~4e-5 residual on this sum.
fwd_nll <- NA_real_; cole_nll <- -sum(fit$rep$loglik)
if (file.exists("Data/GOA_25_pollock_bridge_fits.Rdata")) {
  load("Data/GOA_25_pollock_bridge_fits.Rdata")   # -> pollock_fixed (Cole's MLEs)
  fwd <- fit_mod(data_list = pollock25, inits = pollock_fixed$estimated_params,
                 estimateMode = "DebugBuild", msmMode = "SingleSpecies", initMode = "OffsetEquilibrium",
                 random_rec = TRUE, random_q = TRUE,
                 qFun = q_spec, selFun = sel_spec, compFun = comp_spec,
                 fit_control = fit_control(getsd = FALSE, verbose = 0,
                                           bias_adjust_proc = FALSE))
  fwd_nll <- sum(fwd$quantities$jnll_comp)
}

# ---- Fit (free estimation) -------------------------------------------------
pollock_2025 <- fit_mod(
  data_list = pollock25, inits = NULL, estimateMode = "Hindcast", msmMode = "SingleSpecies",
  initMode = "OffsetEquilibrium", random_rec = TRUE, random_q = TRUE,
  qFun = q_spec, selFun = sel_spec, compFun = comp_spec,
  fit_control = fit_control(phase = TRUE, getsd = TRUE, verbose = 1,
                            bias_adjust_proc = FALSE))

# ---- Three-way overlay: goa_pk original, goa_pk corrected, Rceattle ---------
# Clone the Rceattle fit and graft in each goa_pk reported series (goa_pk carries
# biomass/recruitment in millions -> absolute numbers).
graft <- function(src) {
  s <- pollock_2025
  s$quantities$biomass[, 1:nyrs] <- src$rep$Etotalbio * 1e6
  s$quantities$ssb[, 1:nyrs]     <- src$rep$Espawnbio * 1e6
  s$quantities$R[, 1:nyrs]       <- src$rep$recruit   * 1e6
  s
}
safe_orig <- graft(fit_orig)   # goa_pk as published
safe_corr <- graft(fit)        # goa_pk corrected (reconciliation target)

mods  <- list(safe_orig, safe_corr, pollock_2025)
names <- c("goa_pk (original)", "goa_pk (corrected)", "Rceattle 2025")

plot_biomass(mods, model_names = names)
plot_ssb(mods, model_names = names)
plot_recruitment(mods, model_names = names)
plot_index(pollock_2025)

# ---- Three-way total-NLL comparison ----------------------------------------
# goa_pk$opt$objective is the MARGINAL (Laplace) NLL; Rceattle's jnll and
# -sum(goa_pk$rep$loglik) are CONDITIONAL (deviates held at their modes), so the
# conditional column is the like-for-like comparison. The corrections (A1-A6)
# move goa_pk from `original` to `corrected`; Rceattle reproduces `corrected`.
cond <- c(`goa_pk (original)`  = -sum(fit_orig$rep$loglik),
          `goa_pk (corrected)` = -sum(fit$rep$loglik),
          `Rceattle 2025 (free)` = pollock_2025$quantities$jnll)
cat("\n== Three-way conditional NLL ==\n")
print(round(cond, 4))
cat(sprintf("\nRceattle free fit - goa_pk corrected: %+.3f",
            cond[["Rceattle 2025 (free)"]] - cond[["goa_pk (corrected)"]]),
    "(multimodal: OffsetEquilibrium init can find a slightly better optimum)\n")

# ---- Forward-pass EXACT equivalence (computed above on a clean DLL) ---------
if (!is.na(fwd_nll)) {
  cat(sprintf("\nForward-pass (Cole's MLEs fixed): Rceattle %.9f vs goa_pk corrected %.9f  |diff| %.2e\n",
              fwd_nll, cole_nll, abs(fwd_nll - cole_nll)))
}

# ---- Component NLL table (Rceattle free fit) -------------------------------
cat("\n== Component NLL (Rceattle free fit) ==\n")
print(round(pollock_2025$quantities$jnll_comp, 3))

save(pollock_2025, file = "Data/GOA_25_pollock_final.Rdata")
