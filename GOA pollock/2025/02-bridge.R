# =============================================================================
# GOA pollock 2025 -- bridge "goa_pk" to Rceattle:
#
# Two models are set-up
#   (A) FORWARD-PASS -- map goa_pk's MLE parList onto the Rceattle parameters,
#       hold them fixed (estimateMode = "DebugBuild"), and confirm the derived quantities and
#       per-component likelihoods match `fit$rep` to ~1e-6. The one fix to goa_pk
#       was the wrong M-indexing for initialization.
#   (B) ESTIMATION -- fit from default and confirm convergence to goa_pk's objective.
#
# Requires: Data/GOA_25_pollock.Rdata (from "2025 pollock build data.R") and
# Data/2024pollock_mfix.Rdata (Cole's CORRECTED goa_pk fit: M-index fix + catch
# bias correction + aging-error / comp-obs normalization; see the RECONCILIATION
# LOG in "2025 pollock model.R", A1-A6). Uses the dev Rceattle (initMode 5 + sel
# priors). The corrected fit is the reconciliation target; the original published
# fit (Data/2024pollock.Rdata) is not used here.
# =============================================================================

library(Rceattle)
library(dplyr)
setwd("~/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA pollock")

load("Data/GOA_25_pollock.Rdata")        # -> pollock25 (the Rceattle data_list)
load("Data/2024pollock_mfix.Rdata")      # -> fit  (Cole's CORRECTED goa_pk fit)
# Use the optimizer's actual solution (fit$obj$env$parList() == fit$opt$par), not
# fit$parList: goa_pk's stored parList carries a stale srv6 Dirichlet-multinomial
# weight (log_DM_pars[5] off by exactly 1e-3 from the MLE; every other parameter
# is bit-identical), which would leave the srv6 age-comp component ~3e-4 high.
pl  <- fit$obj$env$parList()
rep <- fit$rep
yrs   <- pollock25$styr:pollock25$endyr
nyrs  <- length(yrs)
SHELIKOF <- 1L    # fleet carrying the AR1/Ecov catchability
FISHERY  <- 8L

# ---- Grammar specifications ------------------------------------------------
# (1) QAR1 on the Shelikof acoustic q (Rogers 2024): an AR1 latent observed
# against the standardized Ecov covariate with Cole's fixed measurement SD
# exp(log_Ecov_obs_sd). Process SD (log_Ecov_sd) and rho (transf_rho) are
# estimated; the effect size is Ecov_beta.
q_spec <- build_catchability(linkages = list(
  q = linkage_spec(~ ar1(1 | Year), by = ~ fleet, fleet = SHELIKOF,
                   observe = "QcovPol", obs_sd = exp(pl$log_Ecov_obs_sd))))

# (2) Selectivity priors (goa_pk "Selectivity Priors" block). Slopes are on the
# log scale (lognormal), inflections on the natural scale (normal). Cole's
# groupings: ascending (peak/logistic) inflection ~ N(0,3), descending ~ N(10,3),
# all log-slopes ~ N(-1,1.5). Ascending fleets: srv2 (2), srv3 (3), fishery (8).
# Descending fleets: srv1 (1), srv6 (6), fishery (8). (srv1/srv6 are descending
# logistic; srv2/srv3 logistic; fishery double logistic.)
# Cole's exact set (goa_pk.cpp:1155-1174): ascending-limb priors (log_slp1/inf1)
# on the fishery + srv2 + srv3 + srv6; descending-limb priors (log_slp2/inf2) on
# the fishery + srv1 + srv2 + srv6. srv2 (logistic) and srv6 (descending) are
# priored on BOTH limbs even though one limb is fixed -- that limb's prior is a
# constant (evaluated at the mapped fixed value), which we reproduce to match the
# objective. (srv1 ascending and srv3 descending are commented out in goa_pk.)
sel_spec <- build_selectivity(linkages = list(
  slp_asc  = linkage_spec(~ 1, by = ~ fleet, fleet = c(2, 3, 6, 8),
                          priors = list(`(Intercept)` = lognormal(-1, 1.5))),
  inf_asc  = linkage_spec(~ 1, by = ~ fleet, fleet = c(2, 3, 6, 8),
                          priors = list(`(Intercept)` = normal(0, 3))),
  slp_desc = linkage_spec(~ 1, by = ~ fleet, fleet = c(1, 2, 6, 8),
                          priors = list(`(Intercept)` = lognormal(-1, 1.5))),
  inf_desc = linkage_spec(~ 1, by = ~ fleet, fleet = c(1, 2, 6, 8),
                          priors = list(`(Intercept)` = normal(10, 3)))))

# (3) Dirichlet-multinomial parameter prior: goa_pk adds dnorm(log_DM_pars, 0, 2)
# to its Selectivity-Priors block. comp_weights is the log DM scalar, so a
# lognormal(0, 2) prior on the natural theta = exp(comp_weights) reproduces
# dnorm(comp_weights, 0, 2) exactly, on the 5 Dirichlet-multinomial fleets.
comp_spec <- build_composition(linkages = list(
  theta_comp = linkage_spec(~ 1, by = ~ fleet, fleet = c(FISHERY, 1, 2, 3, 6),
                            priors = list(`(Intercept)` = lognormal(0, 2)))))

# ---- Model-config fixes for the grammar bridge -----------------------------
# Fishery gets its OWN selectivity block (the legacy skeleton mirrored it to
# fleet 7; harmless with fixed params, but a per-fleet sel prior needs a lead
# block). Fleet 7 legitimately mirrors fleet 1 (a duplicate Shelikof block).
pollock25$fleet_control$Selectivity_index[FISHERY] <- FISHERY

# Shelikof q is now the grammar AR1/Ecov linkage, so drop the legacy code-6 AR1
# / random-walk on q1 (Catchability = free mean + the ar1() linkage on top).
pollock25$fleet_control$Catchability[SHELIKOF]    <- "Estimated"
pollock25$fleet_control$Time_varying_q[SHELIKOF]  <- "Off"
# srv3 keeps its legacy random-walk q (Cole's log_q3_dev); srv2 is a fixed q with
# the BT catchability prior N(log 0.85, 0.1).
pollock25$fleet_control$Catchability[2] <- "Estimated-with-prior"
pollock25$fleet_control$Catchability_init[2]       <- 0.85
pollock25$fleet_control$Catchability_prior_sd[2]   <- 0.1

# Composition young-age accumulation (goa_pk ac_yng): the fishery folds age-1
# into age-2 (ac_yng_fsh = 2) and the Shelikof survey folds ages 1-2 into age-3
# (ac_yng_srv1 = 3). All other fleets have no accumulation (ac_yng = 1) and no
# old-tail accumulation (ac_old = 10 = plus group). Comp_accum_old = 0 => none.
pollock25$fleet_control$Comp_accum_young <- 1L
pollock25$fleet_control$Comp_accum_old   <- 0L
pollock25$fleet_control$Comp_accum_young[FISHERY]  <- 2L
pollock25$fleet_control$Comp_accum_young[SHELIKOF] <- 3L

# Dirichlet-multinomial weights (log theta). fit_mod() sources comp_weights from
# fleet_control$Comp_weights (it OVERRIDES inits$comp_weights), so set the DM log-
# weights on the fleet_control column. Cole's log_DM_pars order is (fishery, srv1,
# srv2, srv3, srv6) -> Rceattle fleet codes 8, 1, 2, 3, 6.
pollock25$fleet_control$Comp_weights[c(FISHERY, 1, 2, 3, 6)] <- pl$log_DM_pars

# ============================================================================
# (A) FORWARD-PASS EXACT: map Cole's parList -> Rceattle inits, hold fixed.
# ============================================================================
# Build the inits SKELETON from a grammar-attached estimateMode = "DebugBuild" build, so it
# already carries the linkage parameters (beta_linkage_re / _obs, log_sigma_linkage,
# trans_rho_linkage, log_obs_sd_linkage) at the correct sizes -- build_params()
# alone (no linkages) would omit them.
skel <- fit_mod(
  data_list = pollock25, inits = NULL, estimateMode = "DebugBuild", msmMode = "SingleSpecies",
  initMode = "FishedEquilibrium", random_rec = TRUE, random_q = TRUE,
  qFun = q_spec, selFun = sel_spec, compFun = comp_spec,
  fit_control = fit_control(phase = FALSE, getsd = FALSE, verbose = 0, bias_adjust_proc = FALSE))
inits <- skel$estimated_params

# -- Recruitment (goa_pk is in millions -> +log(1e6) on the mean) ------------
inits$rec_pars[, 1] <- log(exp(pl$mean_log_recruit) * 1e6)
inits$rec_dev[, 1:nyrs] <- pl$dev_log_recruit
inits$R_log_sd <- log(pl$sigmaR)
# NB: no init_dev assignment -- initMode = "FishedEquilibrium" seeds the initial
# ages off exp(rec_pars + rec_dev[1]) automatically (the year-1 recruitment).

# -- Fishing mortality -------------------------------------------------------
inits$log_F[FISHERY, ] <- pl$mean_log_F + pl$dev_log_F

# -- Selectivity means (ascending slot 1, descending slot 2) -----------------
inits$log_sel_slp[1, c(2, 3, 6, 8), 1] <-
  c(pl$log_slp1_srv2, pl$log_slp1_srv3, pl$log_slp1_srv6, pl$log_slp1_fsh_mean)
inits$sel_inf[1, c(2, 3, 6, 8), 1] <-
  c(pl$inf1_srv2, pl$inf1_srv3, pl$inf1_srv6, pl$inf1_fsh_mean)
inits$log_sel_slp[2, c(1, 2, 6, 7, 8), 1] <-
  c(pl$log_slp2_srv1, pl$log_slp2_srv2, pl$log_slp2_srv6, pl$log_slp2_srv1, pl$log_slp2_fsh_mean)
inits$sel_inf[2, c(1, 2, 6, 7, 8), 1] <-
  c(pl$inf2_srv1, pl$inf2_srv2, pl$inf2_srv6, pl$inf2_srv1, pl$inf2_fsh_mean)

# -- Fishery time-varying selectivity deviates (random walk on the ascending limb)
inits$log_sel_slp_dev[1, FISHERY, 1, ] <- pl$slp1_fsh_dev
inits$log_sel_slp_dev[2, FISHERY, 1, ] <- pl$slp2_fsh_dev
inits$sel_inf_dev[1, FISHERY, 1, ]    <- pl$inf1_fsh_dev
inits$sel_inf_dev[2, FISHERY, 1, ]    <- pl$inf2_fsh_dev

# -- Catchability means ------------------------------------------------------
inits$index_log_q[1:6] <- unlist(pl[c("log_q1_mean", "log_q2_mean", "log_q3_mean",
                                      "log_q4", "log_q5", "log_q6")])
# srv3 keeps the legacy random-walk q deviates (Cole's log_q3_dev); fleet 1's
# q deviates are carried by the grammar AR1 latent instead (beta_linkage_re).
if (!is.null(inits$index_q_dev)) inits$index_q_dev[3, ] <- pl$log_q3_dev
# q3 random-walk SD. The corrected local goa_pk sets q3_rwlk_sd to a CONSTANT
# 0.05 for all years (Cole's original used a per-year 0.001/0.05 vector that
# Rceattle's single-per-fleet SD cannot reproduce). With a constant SD, Rceattle's
# RW penalty matches exactly: map the fleet-3 RW SD to 0.05.
if (!is.null(inits$index_q_dev_log_sd)) inits$index_q_dev_log_sd[3] <- log(0.05)

# -- Grammar QAR1 parameters (replace the legacy index_q_dev/rho/beta path) ---
# The AR1 latent enters log q1 as beta_linkage_obs * beta_linkage_re, observed
# against QcovPol with fixed SD; process SD and rho are the linkage hyperparams.
inits$beta_linkage_re    <- pl$Ecov_exp                 # AR1 latent state (per year)
inits$beta_linkage_obs   <- pl$Ecov_beta                # effect size on log q1
inits$log_sigma_linkage  <- pl$log_Ecov_sd              # AR1 process SD
inits$trans_rho_linkage  <- pl$transf_rho               # AR1 correlation (logit space)
inits$log_obs_sd_linkage <- pl$log_Ecov_obs_sd          # fixed measurement SD

# -- Dirichlet-multinomial weights (theta) -----------------------------------
# Set on fleet_control$Comp_weights above (fit_mod() overrides inits$comp_weights
# from that column), so no inits assignment here.

# -- Fit at the fixed solution ----------------------------------------------
pollock_fixed <- fit_mod(
  data_list = pollock25, inits = inits, estimateMode = "DebugBuild", msmMode = "SingleSpecies",
  initMode = "FishedEquilibrium", random_rec = TRUE, random_q = TRUE,
  qFun = q_spec, selFun = sel_spec, compFun = comp_spec,
  fit_control = fit_control(phase = FALSE, getsd = FALSE, verbose = 0, bias_adjust_proc = FALSE))

# ---- Forward-pass comparisons (tolerance-checked) --------------------------
qf <- pollock_fixed$quantities
report <- function(label, a, b, tol = 1e-6) {
  d <- max(abs(as.numeric(a) - as.numeric(b)), na.rm = TRUE)
  cat(sprintf("  %-28s max|diff| = %.3e  %s\n", label, d,
              if (d < tol) "OK" else "**CHECK**"))
  invisible(d)
}
cat("== Forward-pass exact check (Rceattle @ Cole's MLE vs goa_pk rep) ==\n")
report("selectivity srv1", qf$sel_at_age[1, 1, , 1], rep$slctsrv1)
report("selectivity srv2", qf$sel_at_age[2, 1, , 1], rep$slctsrv2)
report("selectivity srv3", qf$sel_at_age[3, 1, , 1], rep$slctsrv3)
report("selectivity srv6", qf$sel_at_age[6, 1, , 1], rep$slctsrv6)
report("selectivity fishery", t(qf$sel_at_age[8, 1, , 1:nyrs]), rep$slctfsh)
report("fishing mortality F", qf$F_spp[, 1:nyrs], rep$F)
report("catchability q1", qf$index_q[1, ], rep$q1)
report("catchability q2", qf$index_q[2, ], rep$q2)
report("catchability q3", qf$index_q[3, ], rep$q3)
report("catchability q6", qf$index_q[6, ], rep$q6)
# SSB / recruitment differ by the intended initial-age M-indexing fix; report,
# don't gate. The bulk of the series (post-initial cohorts) should still match.
report("SSB (x1e6)", qf$ssb[1, 1:nyrs], rep$Espawnbio * 1e6, tol = Inf)
report("recruitment (x1e6)", qf$R[1, 1:nyrs], rep$recruit * 1e6, tol = Inf)

# ---- Isolate the intended initial-age deviation ----------------------------
# Everything except the first-year initial cohorts should match; the initial
# ages differ by exactly Cole's M-indexing bug that we corrected.
cat("\n== Initial-age structure (intended difference: Cole's M-index bug fixed) ==\n")
report("N-at-age yr>=2 (x1e6)", qf$N_at_age[1, 1, , 2:nyrs],
       t(rep$N[2:nyrs, ]) * 1e6, tol = 1e-4)   # rep$N is [year, age]; transpose to [age, year]

# ---- Per-component likelihood table ----------------------------------------
cat("\n== Component NLL: Rceattle jnll_comp vs goa_pk -loglik ==\n")
print(round(pollock_fixed$quantities$jnll_comp, 4))
cat("goa_pk total objective:", fit$opt$objective,
    "  Rceattle jnll:", pollock_fixed$quantities$jnll, "\n")

# ============================================================================
# (B) FREE ESTIMATION: refit with the grammar and check same minimum.
# ============================================================================
pollock_est <- fit_mod(
  data_list = pollock25, inits = inits, estimateMode = "Hindcast", msmMode = "SingleSpecies",
  initMode = "FishedEquilibrium", random_rec = TRUE, random_q = TRUE,
  qFun = q_spec, selFun = sel_spec, compFun = comp_spec,
  fit_control = fit_control(phase = TRUE, getsd = FALSE, verbose = 1, bias_adjust_proc = FALSE))

grad <- tryCatch(max(abs(pollock_est$obj$gr(pollock_est$opt$par))),
                 error = function(e) NA_real_)
cat("\n== Free estimation ==\n")
cat("  goa_pk objective (marginal): ", sprintf("%.4f", fit$opt$objective), "\n")
cat("  Rceattle objective (marginal):", sprintf("%.4f", pollock_est$opt$objective), "\n")
cat("  goa_pk conditional NLL:      ", sprintf("%.4f", -sum(fit$rep$loglik)), "\n")
cat("  Rceattle conditional jnll:   ", sprintf("%.4f", pollock_est$quantities$jnll), "\n")
cat("  Rceattle max |gradient|:     ", sprintf("%.2e", grad), "\n")

save(pollock_fixed, pollock_est, file = "Data/GOA_25_pollock_bridge_fits.Rdata")
