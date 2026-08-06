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

# NOTE: corrections A7-A9 change goa_pk and are NOT in the saved Data/*.Rdata
# fits. Re-run "00-fit-goa_pk.R" before trusting any comparison below.
#
# Bridging a bug-corrected goa_pk shows the two models share the same conditional
# likelihood at goa_pk's MLEs (forward pass, <= 1e-8).
#
# The free fits are compared against the sigmaR-ESTIMATED goa_pk, not the
# sigmaR = 1.3 one. Rceattle estimates the recruitment-process SD and integrates
# the deviates, so that is the variant posing the same estimation problem;
# comparing against the fixed-sigmaR fit instead moves SSB by ~9% and
# recruitment by ~17%. A constant still separates the two objectives -- see the
# decomposition printed at the end.
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
#   A7. Fishery ascending random walk: fix the same end Rceattle does. goa_pk
#       maps the mean off and estimates all 55 deviates; Rceattle estimates the
#       mean and fixes deviate 1. Same likelihood and same 55 parameters, but a
#       prior on a mapped-off mean is inert, so only this makes the two
#       estimation problems identical. Costs +0.089 in goa_pk's conditional NLL.
#   A8. Removed the descending fishery selectivity random-walk penalties.
#       slp2_fsh_dev / inf2_fsh_dev are mapped off at 0, so those terms were
#       normalizing constants (149.4338) on deviates the model never estimates --
#       the same case as A3. Rceattle has no way to charge them.
#   A9. Removed the selectivity priors on mapped-off limbs (srv2 descending,
#       srv6 ascending). goa_pk already omits these for srv1 and srv3; srv2 and
#       srv6 were inconsistent. Rceattle now rejects such a prior outright.
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
load("Data/2024pollock.Rdata")                 # goa_pk as published
fit_orig <- fit
load("Data/2024pollock_mfix.Rdata")            # corrected, sigmaR fixed at 1.3
fit_sigRfix <- fit
# The comparison model is the sigmaR-ESTIMATED fit: Rceattle estimates the
# recruitment-process SD and integrates the deviates, so this is the variant
# that poses the same estimation problem. `fit` refers to it from here on.
load("Data/2024pollock_mfix_estSigR.Rdata")    # corrected, sigmaR estimated
fit_estSigR <- fit


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
# As "02-bridge.R", but the fishery ascending random walk uses the linkage
# grammar here; 02-bridge.R injects goa_pk's deviates into the legacy arrays.
pollock25$fleet_control$Selectivity_index[FISHERY] <- FISHERY
pollock25$fleet_control$Catchability[SHELIKOF]   <- "Estimated"
pollock25$fleet_control$Time_varying_q[SHELIKOF] <- "Off"
pollock25$fleet_control$Catchability[2] <- "Estimated-with-prior"
pollock25$fleet_control$Catchability_init[2] <- 0.85
pollock25$fleet_control$Catchability_prior_sd[2] <- 0.1

# Survey-3 (adfg) time-varying q is expressed through the linkage grammar below;
# switch off the legacy fleet_control mode it replaces.
ADFG <- 3L
pollock25$fleet_control$Time_varying_q[ADFG] <- "Off"   # -> rw(1 | Year) on q, below

# Fishery ascending-limb random walk also moves to the grammar; switch off the
# legacy mode. goa_pk penalizes these deviates rather than integrating them,
# hence integrate = FALSE. The legacy penalty uses sel_dev_sd on the slope and
# 4x that on the inflection, so the two take different sigmas.
SEL_RW_SD <- pollock25$fleet_control$Time_varying_sel_sd[FISHERY]
pollock25$fleet_control$Time_varying_sel[FISHERY] <- "Off"  # -> rw(1 | Year), below

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
ADFG_SURVEY       <- "Pollock_survey_3_adfg"
# Prior fleets, restricted to the limb each curve actually has: srv2/srv3 are
# Logistic (ascending only), srv1/srv6 are DescendingLogistic (descending only),
# the fishery is DoubleLogistic (both). A prior on the other limb would only add
# a constant -- fit_mod() now rejects it.
ASC_LIMB_PRIOR <- c("Pollock_survey_2_bottom_trawl",
                     "Pollock_survey_3_adfg",
                     "GOA_pollock_fishery")
DESC_LIMB_PRIOR <- c("Pollock_survey_1_shelikof_acoustic",
                     "Pollock_survey_6_summer_acoustic",
                     "GOA_pollock_fishery")
DM_PRIOR        <- c("GOA_pollock_fishery",
                     "Pollock_survey_1_shelikof_acoustic",
                     "Pollock_survey_2_bottom_trawl",
                     "Pollock_survey_3_adfg",
                     "Pollock_survey_6_summer_acoustic")

# * Catchability: QAR1 (Shelikof) + random-walk q (adfg) ----
# Shelikof is the state-space QAR1; adfg is a random walk on q with the SD fixed
# at 0.05 (init = list(sigma = ...) with no sigma prior fixes it), matching the
# legacy Time_varying_q RW penalty (index_q_dev_sd).
q_spec <- build_catchability(linkages = list(
  q = list(
    linkage_spec(~ ar1(1 | Year),
                 by = ~ fleet,
                 fleet = SHELIKOF_ACOUSTIC,
                 observe = "QcovPol",
                 obs_sd = exp(pl$log_Ecov_obs_sd)),
    linkage_spec(~ rw(1 | Year),
                 by = ~ fleet,
                 fleet = ADFG_SURVEY,
                 init = list(sigma = 0.05)))))

# * Selectivity priors + fishery ascending random walk ----
# Priors mirror goa_pk exactly (both limbs on srv2 and srv6). The ascending limb
# takes a second spec: the ~ 1 spec keeps the shared prior, the walk adds
# fishery-only deviates around it. Ascending only -- goa_pk maps its descending
# deviates off and estimates just slp1/inf1. (02-bridge.R walks all four, because
# reproducing goa_pk's likelihood needs the constant penalty it still charges on
# those fixed-at-zero deviates.)
# The two models fix opposite ends of this walk; see A7.
sel_spec <- build_selectivity(linkages = list(
  slp_asc  = list(
    linkage_spec(~ 1,
                 fleet = ASC_LIMB_PRIOR,
                 priors = list(intercept = lognormal(-1, 1.5))),
    linkage_spec(~ rw(1 | Year),
                 fleet = "GOA_pollock_fishery",
                 init = list(sigma = SEL_RW_SD),
                 integrate = FALSE)),
  inf_asc  = list(
    linkage_spec(~ 1,
                 fleet = ASC_LIMB_PRIOR,
                 priors = list(intercept = normal(0, 3))),
    # 4x SD: the legacy penalty weights the inflection deviate at 4 * sel_dev_sd.
    linkage_spec(~ rw(1 | Year),
                 fleet = "GOA_pollock_fishery",
                 link = "identity",
                 init = list(sigma = 4 * SEL_RW_SD), integrate = FALSE)),
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
# The forward pass injects 02-bridge.R's parameters, which come from the
# sigmaR-FIXED fit, so compare it against that variant. The free fit below is
# compared against the sigmaR-estimated one, which poses the same estimation
# problem.
fwd_nll <- NA_real_; cole_nll <- -sum(fit_sigRfix$rep$loglik)
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
plot_biomass(list(safe_corr, fwd))
plot_index(pollock_2025)

# * Total-NLL comparison ----
cond <- c(`goa_pk (original)`  = -sum(fit_orig$rep$loglik),
          `goa_pk (corrected, sigmaR est)` = -sum(fit$rep$loglik),
          `Rceattle 2025 (free)` = pollock_2025$quantities$jnll)
cat("\n== Three-way conditional NLL ==\n")
print(round(cond, 4))
# The two free fits are not the same estimation problem, so decompose the gap
# instead of reporting one number. SEL_RW_CONST is the walk penalty goa_pk
# charges on its descending fishery deviates, which are mapped off at 0 -- a
# constant Rceattle cannot reproduce (the same thing A3 removed for q1/q2).
# PRIOR_CONST is the selectivity priors Rceattle puts on slots the fleet's own
# curve never uses (srv6 ascending, srv2 descending), frozen at build defaults
# rather than goa_pk's values. The remainder is the real difference: Rceattle
# integrates the recruitment deviates and estimates sigma_R, goa_pk penalizes
# them at sigmaR = 1.3.
SEL_RW_CONST <- (nyrs - 1) * (dnorm(0, 0, SEL_RW_SD, log = TRUE) +
                              dnorm(0, 0, 4 * SEL_RW_SD, log = TRUE))
PRIOR_CONST  <- 13.193917
gap <- cond[["Rceattle 2025 (free)"]] - cond[["goa_pk (corrected, sigmaR est)"]]
cat(sprintf(paste0("\nRceattle free fit - goa_pk (sigmaR est): %+.3f\n",
                   "  %+.3f  goa_pk's walk penalty on zero-pinned descending deviates\n",
                   "  %+.3f  priors on selectivity slots the fleet's curve does not use\n",
                   "  %+.3f  recruitment treatment (Laplace + free sigma_R vs fixed 1.3)\n"),
            gap, -SEL_RW_CONST, PRIOR_CONST,
            gap + SEL_RW_CONST - PRIOR_CONST))

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
  fx <- gcv(fit_sigRfix, q[2]); es <- gcv(fit_estSigR, q[2]); rc <- rcv(q[3])
  cat(sprintf("== %s CV (goa_pk fixed | est | Rceattle) ==\n", q[1]))
  for (y in c("2000", "2015", "2020", "2024")) if (!is.na(es[y]))
    cat(sprintf("  %s:  %.3f | %.3f | %.3f\n", y, fx[y], es[y], rc[y]))
}

save(pollock_2025, file = "Data/GOA_25_pollock_final.Rdata")
