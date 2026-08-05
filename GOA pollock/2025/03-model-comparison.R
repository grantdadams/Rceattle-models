# =============================================================================
# GOA pollock 2025 --  compares Rceattle against goa_pk SAFE model.
# Fits the production setup (DM comps, AR1/Ecov catchability
# on the Shelikof survey index, selectivity priors, initMode = "OffsetEquilibrium")
# and overlays the SAFE derived quantities.
#
# Run "01-build-data.R" and, for the
# comparison, fitted goa_pk object in Data/2024pollock.Rdata.
#
# =============================================================================
#

# Bridging a bug-corrected goa_pk shows the two models share the same conditional
# likelihood at goa_pk's MLEs.  Uncertainty matches too, with sigmaR estimated
# (00-fit-goa_pk.R -> Data/2024pollock_mfix_estSigR.Rdata): all CVs match to 3 decimals.
# Note, there isa separate multimodality from the "OffsetEquilibrium" initialization.
#
# --- (A) Changes made to goa_pk -----------------------------------
#   Implemented by "2025/00-fit-goa_pk.R" and "reference/goa_pk_2024_mfix.cpp"
#   (a copy of every edit tagged "GRANT"); A2/A5/A6 are data-related and are
#   applied to input$dat in that script.
#   A1. Initial age-structure M-index bug fix. goa_pk's initN loop used
#       initN(j) = initN(j-1)*exp(-M(j+1)), which skips age-2 M and applies each
#       age's future M. Corrected to exp(-M(j)).
#   A2. Constant q3 random-walk SD. goa_pk used a per-year q3_rwlk_sd vector
#       (0.001 in most years, 0.05 in a few). I set to a constant 0.05 for all
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
# --- (B) Rceattle features added ------------------------------------
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
#       age-3.
#
# --- (C) Data conversions (see the build-data script) -----------------------
#   C1. Survey indices scaled x1e6 (millions -> absolute numbers).
#   C2. wt_pop = wt_srv2 (bottom trawl), wt_spawn = wt_srv1 (Shelikof).
#   C3. Composition ESS x2 on srv1/3/6 (already applied in Cole's fit data).
#   C4. Age-1 / age-2 Shelikof indices (fleets 4/5) turned off.
#   C5. Aging-error matrix (age_error) rows normalized to sum to 1 (pairs with A5).
# =============================================================================

library(Rceattle)
library(dplyr)
library(ggplot2)
# Run from the "GOA pollock" project root, like the other scripts in this folder.

# Data ----
pollock25 <- read_data("Data/GOA_25_pollock_single_species_1970-2024.xlsx")
load("Data/2024pollock.Rdata")                 # Original goa_pk fit
fit_orig <- fit
load("Data/2024pollock_mfix_estSigR.Rdata")    # Corrected goa_pk fit sigmaR estimated
fit_estSigR <- fit
load("Data/2024pollock_mfix.Rdata")            # Corrected goa_pk fit sigmaR fixed = 1.3


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

# Model configuration ----
# identical to "02-bridge.R"
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

# DM weights (log theta) starting values from goa_pk (fit_mod gets these from
# fleet_control$Comp_weights); estimated freely from there.
pollock25$fleet_control$Comp_weights[c(FISHERY, 1, 2, 3, 6)] <- pl$log_DM_pars

# Linkages name their fleets (can also use Fleet_code):
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

# * QAR1 ----
q_spec <- build_catchability(linkages = list(
  q = linkage_spec(~ ar1(1 | Year),
                   by = ~ fleet,
                   fleet = SHELIKOF_ACOUSTIC,
                   observe = "QcovPol",
                   obs_sd = exp(pl$log_Ecov_obs_sd))))

# * Selectivity priors ----
# mirror goa_pk exactly (both limbs on srv2 and srv6).
sel_spec <- build_selectivity(linkages = list(
  slp_asc  = linkage_spec(~ 1,
                          fleet = ASC_LIMB_PRIOR,
                          priors = list(intercept = lognormal(-1, 1.5))),
  inf_asc  = linkage_spec(~ 1,
                          fleet = ASC_LIMB_PRIOR,
                          priors = list(intercept = normal(0, 3))),
  slp_desc = linkage_spec(~ 1,
                          fleet = DESC_LIMB_PRIOR,
                          priors = list(intercept = lognormal(-1, 1.5))),
  inf_desc = linkage_spec(~ 1,
                          fleet = DESC_LIMB_PRIOR,
                          priors = list(intercept = normal(10, 3)))))
# * Dirichlet-multinomial prior ----
# (dnorm(log_DM_pars, 0, 2)).
comp_spec <- build_composition(linkages = list(
  theta_comp = linkage_spec(~ 1,
                            fleet = DM_PRIOR,
                            priors = list(intercept = lognormal(0, 2)))))


# Model comparison ----
# * Forward-pass ----
# Fix goa_pk's MLEs in Rceattle (estimateMode = "DebugBuild", no optimization) and confirm
# the two models match.
fwd_nll <- NA_real_; cole_nll <- -sum(fit$rep$loglik)
if (file.exists("Data/GOA_25_pollock_bridge_fits.Rdata")) {
  load("Data/GOA_25_pollock_bridge_fits.Rdata")   # -> pollock_fixed (Cole's MLEs)
  fwd <- fit_mod(data_list = pollock25,
                 inits = pollock_fixed$estimated_params,
                 estimateMode = "DebugBuild",
                 msmMode = "SingleSpecies",
                 initMode = "OffsetEquilibrium",
                 random_rec = TRUE,
                 random_q = TRUE,
                 qFun = q_spec,
                 selFun = sel_spec,
                 compFun = comp_spec,
                 fit_control = fit_control(getsd = FALSE, verbose = 0,
                                           bias_adjust_proc = FALSE))
  fwd_nll <- sum(fwd$quantities$jnll_comp)
}

# * Fit ----
pollock_2025 <- fit_mod(
  data_list = pollock25,
  inits = NULL,
  estimateMode = "Hindcast",
  msmMode = "SingleSpecies",
  initMode = "OffsetEquilibrium",
  random_rec = TRUE,
  random_q = TRUE,
  qFun = q_spec,
  selFun = sel_spec,
  compFun = comp_spec,
  fit_control = fit_control(phase = TRUE, getsd = TRUE, verbose = 0,
                            bias_adjust_proc = FALSE))

# * Modified safe model ----
graft <- function(src) {
  s <- pollock_2025
  s$quantities$biomass[, 1:nyrs] <- src$rep$Etotalbio * 1e6
  s$quantities$ssb[, 1:nyrs]     <- src$rep$Espawnbio * 1e6
  s$quantities$R[, 1:nyrs]       <- src$rep$recruit   * 1e6
  s
}
safe_orig <- graft(fit_orig)   # goa_pk as published
safe_corr <- graft(fit)        # goa_pk corrected (reconciliation target)

mods  <- list(safe_orig, safe_corr, fwd, pollock_2025)
names <- c("goa_pk (original)", "goa_pk (corrected)", "Rceattle (forward-pass)","Rceattle 2025")

# * Plot ----
plot_biomass(mods, model_names = names)
plot_ssb(mods, model_names = names)
plot_recruitment(mods, model_names = names)
plot_index(pollock_2025)

# * Total-NLL comparison ----
cond <- c(`goa_pk (original)`  = -sum(fit_orig$rep$loglik),
          `goa_pk (corrected)` = -sum(fit$rep$loglik),
          `Rceattle 2025 (free)` = pollock_2025$quantities$jnll)
cat("\n== Three-way conditional NLL ==\n")
print(round(cond, 4))
cat(sprintf("\nRceattle free fit - goa_pk corrected: %+.3f",
            cond[["Rceattle 2025 (free)"]] - cond[["goa_pk (corrected)"]]),
    "(multimodal: OffsetEquilibrium init can find a slightly better optimum)\n")

# * Forward-pass equivalence ---------
if (!is.na(fwd_nll)) {
  cat(sprintf("\nForward-pass (Cole's MLEs fixed): Rceattle %.9f vs goa_pk corrected %.9f  |diff| %.2e\n",
              fwd_nll, cole_nll, abs(fwd_nll - cole_nll)))
}

# Component NLL (Rceattle fit) -------------------------------
cat("\n== Component NLL (Rceattle free fit) ==\n")
print(round(pollock_2025$quantities$jnll_comp, 3))

# Uncertainty (corrected goa_pk with sigmaR fixed and estimated) ----
sr <- pollock_2025$sdrep; vnm <- names(sr$value)
cat(sprintf("\n== Recruitment-process SD ==\n  goa_pk fixed 1.300 | goa_pk est %.4f | Rceattle R_sd %.4f\n",
            fit_estSigR$parList$sigmaR, sr$value[which(vnm == "R_sd")[1]]))
gcv <- function(f, nn) { d <- f$sd[f$sd$name == nn, ]; setNames(d$se / d$est, d$year) }
rcv <- function(tag) { i <- which(vnm == tag)
setNames(sr$sd[i] / sr$value[i], pollock25$styr + seq_along(i) - 1L) }
for (q in list(c("SSB", "Espawnbio", "ssb"), c("Recruitment", "recruit", "R"))) {
  fx <- gcv(fit, q[2]); es <- gcv(fit_estSigR, q[2]); rc <- rcv(q[3])
  cat(sprintf("== %s CV (goa_pk fixed | est | Rceattle) ==\n", q[1]))
  for (y in c("2000", "2015", "2020", "2024")) if (!is.na(es[y]))
    cat(sprintf("  %s:  %.3f | %.3f | %.3f\n", y, fx[y], es[y], rc[y]))
}

save(pollock_2025, file = "Data/GOA_25_pollock_final.Rdata")
