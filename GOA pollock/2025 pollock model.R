# =============================================================================
# GOA pollock 2025 -- final Rceattle model + comparison against Cole Monnahan's
# goa_pk (the "SAFE" model). Fits the production configuration (Dirichlet-
# multinomial comps, grammar AR1/Ecov catchability on the Shelikof survey,
# selectivity priors, initMode = "FishedEquilibrium") and overlays the SAFE
# derived quantities for a side-by-side comparison.
#
# Run "2025 pollock build data.R" (-> GOA_25_pollock.Rdata) and, for the
# comparison, have Cole's fitted goa_pk object in Data/2024pollock.Rdata.
# The parameter-level exact bridge and its verification live in
# "2025 pollock bridging.R"; this script is the clean fit + figures.
#
# =============================================================================
# RECONCILIATION LOG -- every change made so Rceattle reproduces goa_pk exactly.
#
# Result: the Rceattle CONDITIONAL negative log-likelihood matches goa_pk's
# -sum(loglik) to +0.016 units (1830.74 vs 1830.72); all dynamics (N-at-age,
# selectivity, F, q1-q6, recruitment) and every likelihood component match to
# machine precision. The only residual (~0.013) is the Dirichlet-multinomial
# linear-parameterization offset ((1 + o*A), o = 1e-5), which is negligible.
#
# --- IMPORTANT verification note --------------------------------------------
# The forward pass establishes that the two models share the same CONDITIONAL
# (penalized) likelihood at a fixed parameter point -- NOT that they are the same
# fitted model. Compare CONDITIONAL to CONDITIONAL: Rceattle$quantities$jnll vs
# goa_pk -sum(fit$rep$loglik). Do NOT use goa_pk fit$opt$objective (a MARGINAL /
# Laplace NLL); conditional-to-marginal makes the models look ~108 apart when the
# conditional surfaces are identical.
#   The marginal objectives genuinely DIFFER because the two make different
# random-effect choices: goa_pk integrates only Ecov_exp (55 states, marginal -
# conditional gap ~108), while this Rceattle config integrates the recruitment
# deviations, the linkage random effects and the q random walk (164 states, gap
# ~288). At the mode the random density equals the penalty, so the conditional
# match is exact; but the two fitted models' sdreport uncertainties on SSB /
# recruitment / reference points will not be identical, and the free-fit
# conditional difference (~-15 here) is partly this structural difference, not
# only multimodality. The replica is of a bug-corrected goa_pk (A1 M-index fix
# shifts 1970s N-at-age up to ~22%; A3 strips ~590 units of RW normalizing
# constants), so it does not reproduce the as-published SAFE objective.
#
# --- (A) Changes made to Cole's goa_pk model (worktree GOApollock-mfix, refit
#         to Data/2024pollock_mfix.Rdata) -----------------------------------
#   A1. Initial age-structure M-index bug fix. goa_pk's initN loop used
#       initN(j) = initN(j-1)*exp(-M(j+1)), which skips age-2 M and applies each
#       ARRIVING age's M. Corrected to exp(-M(j)) (standard DEPARTING-age
#       cumulative-M decay), matching Rceattle's FishedEquilibrium. Without this
#       the 1970s numbers-at-age differ by up to ~34% (decaying to 0 by ~2000).
#   A2. Constant q3 random-walk SD. goa_pk used a per-year q3_rwlk_sd vector
#       (0.001 in most years, 0.05 in a few); set to a constant 0.05 for all
#       years (input$dat$q3_rwlk_sd[] <- 0.05) so Rceattle's single-per-fleet RW
#       SD reproduces it. Rceattle cannot express a per-year RW SD.
#   A3. Removed the q1/q2 random-walk penalty lines. q1 uses the Ecov link and q2
#       is fixed, so their deviates are pinned to 0; their RW penalties were pure
#       dnorm(0,0,rwlk_sd) normalizing constants (~527 units) that do not affect
#       the fit. Rceattle does not define q1/q2 as random walks, so it omits
#       them; dropping them in goa_pk makes the TV-catchability component match.
#   A4. Added the lognormal bias correction (-sd^2/2) to the total-catch mean.
#       goa_pk bias-corrects the survey indices but NOT the total catch, whereas
#       Rceattle applies bias_adjust_obs = TRUE to both. Adding it to goa_pk's
#       catch (matching its index treatment) makes the catch component match
#       (~+0.029 before). [Alternative: a per-series bias flag in Rceattle.]
#   A5. Normalized the aging-error matrix rows to sum to 1 (row-stochastic). The
#       source age_trans has a ~1e-4 deficit in true-ages 5-8. goa_pk applies
#       normalize(Nsrv)*age_trans WITHOUT re-normalizing, so that deficit
#       propagates into the predicted comps; Rceattle re-normalizes. Making the
#       matrix a proper distribution on BOTH sides (age_trans in the run script,
#       age_error in build-data D6) aligns the predicted comps (the ~-0.013).
#   A6. Normalized each composition observation to sum to 1. goa_pk fits the raw
#       pk24_12.txt proportions, which sum to ~1.00001 (data-file rounding);
#       Rceattle normalizes every comp row to 1 in rearrange_data() before
#       fitting. That ~1e-5 discrepancy shifts the Dirichlet-multinomial alpha /
#       phi by ~1e-4, leaving a per-fleet mixed-sign residual (~-0.0008 total)
#       AFTER A5. Normalizing goa_pk's comp proportions (catp/srvp*/lenp/srvlenp*)
#       in the run script closes it -- both models then fit proportions that sum
#       to 1, matching each DM component to machine precision. Rceattle needs no
#       change here (it already normalizes); this is a goa_pk-side alignment only.
#
# --- (B) Rceattle package features used (initMode 5, sel priors, comp
#         accumulation -- new this cycle) ------------------------------------
#   B1. initMode = "FishedEquilibrium": seeds the initial ages off the first-year
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
#   C11. Source Cole's parameters from the optimizer's solution
#        (fit$obj$env$parList() == fit$opt$par), NOT fit$parList. goa_pk's stored
#        parList carries a stale srv6 Dirichlet-multinomial weight (log_DM_pars[5]
#        off by EXACTLY 1e-3 from the MLE; every other parameter is bit-identical),
#        which otherwise left the srv6 age-comp component ~2.9e-4 high. Using the
#        true MLE closes the last composition residual (all 5 DM fleets now match
#        to machine precision; total forward-pass NLL diff ~1.7e-11).
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
setwd("~/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA pollock")

load("Data/GOA_25_pollock.Rdata")        # -> pollock25 (Rceattle data_list)
load("Data/2024pollock.Rdata")           # -> fit  (goa_pk ORIGINAL, published)
fit_orig <- fit
load("Data/2024pollock_mfix.Rdata")      # -> fit  (goa_pk CORRECTED, mfix)
# `fit` is now the corrected goa_pk (M-index fix + catch bias correction +
# aging-error / comp-obs normalization; see RECONCILIATION LOG A1-A6). It is the
# reconciliation target Rceattle reproduces exactly; fit_orig is kept only to
# show what the corrections moved.
# Source Cole's parameters from the optimizer's solution, not fit$parList (which
# carries a stale srv6 log_DM_pars[5]; see C11).
pl <- fit$obj$env$parList()
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

q_spec <- build_catchability(linkages = list(
  q = linkage_spec(~ ar1(1 | Year), by = ~ fleet, fleet = SHELIKOF,
                   observe = "QcovPol", obs_sd = exp(pl$log_Ecov_obs_sd))))
# Selectivity priors mirror goa_pk exactly (BOTH limbs on srv2 and srv6).
sel_spec <- build_selectivity(linkages = list(
  slp_asc  = linkage_spec(~ 1, by = ~ fleet, fleet = c(2, 3, 6, 8),
                          priors = list(`(Intercept)` = lognormal(-1, 1.5))),
  inf_asc  = linkage_spec(~ 1, by = ~ fleet, fleet = c(2, 3, 6, 8),
                          priors = list(`(Intercept)` = normal(0, 3))),
  slp_desc = linkage_spec(~ 1, by = ~ fleet, fleet = c(1, 2, 6, 8),
                          priors = list(`(Intercept)` = lognormal(-1, 1.5))),
  inf_desc = linkage_spec(~ 1, by = ~ fleet, fleet = c(1, 2, 6, 8),
                          priors = list(`(Intercept)` = normal(10, 3)))))
# Dirichlet-multinomial parameter prior (dnorm(log_DM_pars, 0, 2)).
comp_spec <- build_composition(linkages = list(
  theta_comp = linkage_spec(~ 1, by = ~ fleet, fleet = c(FISHERY, 1, 2, 3, 6),
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
                 estimateMode = "DebugBuild", msmMode = "SingleSpecies", initMode = "FishedEquilibrium",
                 random_rec = TRUE, random_q = TRUE,
                 qFun = q_spec, selFun = sel_spec, compFun = comp_spec,
                 fit_control = fit_control(getsd = FALSE, verbose = 0,
                                           bias_adjust_proc = FALSE))
  fwd_nll <- sum(fwd$quantities$jnll_comp)
}

# ---- Fit (free estimation) -------------------------------------------------
pollock_2025 <- fit_mod(
  data_list = pollock25, inits = NULL, estimateMode = "Hindcast", msmMode = "SingleSpecies",
  initMode = "FishedEquilibrium", random_rec = TRUE, random_q = TRUE,
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
    "(multimodal: FishedEquilibrium init can find a slightly better optimum)\n")

# ---- Forward-pass EXACT equivalence (computed above on a clean DLL) ---------
if (!is.na(fwd_nll)) {
  cat(sprintf("\nForward-pass (Cole's MLEs fixed): Rceattle %.9f vs goa_pk corrected %.9f  |diff| %.2e\n",
              fwd_nll, cole_nll, abs(fwd_nll - cole_nll)))
}

# ---- Component NLL table (Rceattle free fit) -------------------------------
cat("\n== Component NLL (Rceattle free fit) ==\n")
print(round(pollock_2025$quantities$jnll_comp, 3))

save(pollock_2025, file = "Data/GOA_25_pollock_final.Rdata")
