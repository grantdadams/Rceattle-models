# =============================================================================
# GOA pollock 2025 -- bridge "goa_pk" to Rceattle:
#
# Two models are set-up
#   (A) FORWARD-PASS -- map goa_pk's MLE parList onto the Rceattle parameters,
#       hold them fixed (estimateMode = "DebugBuild"), and confirm the derived
#       quantities and per-component likelihoods match `fit$rep` to <= 1e-8.
#       See 03's reconciliation log (A1-A7) for the goa_pk-side fixes.
#   (B) ESTIMATION -- fit from default inits and confirm convergence to goa_pk's objective.
#
# Requires: the data workbook (from "01-build-data.R") and
# Data/2024pollock_mfix.Rdata (corrected goa_pk fit: M-index fix + catch
# bias correction + aging-error / comp-obs normalization; see "03-model.R" for
# model differences, and "00-fit-goa_pk.R" to rebuild it).
# =============================================================================

library(Rceattle)
library(dplyr)

# The workbook is the canonical data source -- Data/*.Rdata is gitignored, so
# the xlsx is what travels with the repo and what Cole edits directly.
pollock25 <- read_data("Data/GOA_25_pollock_single_species_1970-2024.xlsx")
load("Data/2024pollock_mfix.Rdata")      # -> fit  (rebuild with 00-fit-goa_pk.R)
# Source the parameters from fit$opt$par -- the vector fit$rep was reported at.
#
# fit$obj$env$parList() is NOT the MLE: it is bit-identical to fit$parList, and
# BOTH sit exactly 1e-3 off fit$opt$par on log_DM_pars[5] (the srv6 D-M weight).
# fit_pk() calls obj$report() BEFORE sdreport(), then reads parList() AFTER, so
# obj$env$last.par is left behind the optimizer's solution. Every other
# parameter is identical, which is why this hides so well.
#
# Mapping parList() instead costs ~3e-4 in BOTH the composition and the
# selectivity/D-M prior blocks. They very nearly cancel -- goa_pk is at its
# optimum, so the total gradient vanishes and a small parameter offset moves the
# two blocks in opposite directions -- leaving a total NLL diff of ~9e-7 that
# looks like clean agreement. Using opt$par takes every component to <=3e-11.
pl  <- fit$obj$env$parList()          # skeleton: shapes, and the mapped-off pars
.opt <- fit$opt$par                   # the MLE (fixed effects only)
for (.p in unique(names(.opt))) {
  .v <- .opt[names(.opt) == .p]
  # Skip partially-mapped parameters, where opt$par is shorter than the slot;
  # those keep their parList values, as do random effects (not in opt$par).
  if (!is.null(pl[[.p]]) && length(pl[[.p]]) == length(.v))
    pl[[.p]][] <- as.numeric(.v)
}
rm(.opt, .p, .v)
rep <- fit$rep
yrs   <- pollock25$styr:pollock25$endyr
nyrs  <- length(yrs)
SHELIKOF <- 1L    # fleet with the AR1/Ecov catchability
FISHERY  <- 8L

# ---- Model specifications ------------------------------------------------
# Linkages name their fleets rather than numbering them: fit_mod() checks each
# against fleet_control$Fleet_name and errors on a miss, whereas a Fleet_code
# that is wrong but in range attaches the prior to a different fleet and the
# model still fits. SHELIKOF / FISHERY stay above -- they index fleet_control
# rows, which is a position, not a fleet reference.
SHELIKOF_ACOUSTIC <- "Pollock_survey_1_shelikof_acoustic"
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

# (1) QAR1 on the Shelikof acoustic q (Rogers 2024) with fixed measurement SD
# exp(log_Ecov_obs_sd). Process SD (log_Ecov_sd) and rho (transf_rho) are
# estimated; the effect size is Ecov_beta.
q_spec <- build_catchability(linkages = list(
  q = list(
    linkage_spec(~ ar1(1 | Year),
                 by = ~ fleet,
                 fleet = SHELIKOF_ACOUSTIC,
                 observe = "QcovPol",
                 obs_sd = exp(pl$log_Ecov_obs_sd)),
    # srv3 (adfg) random-walk q, SD fixed at 0.05 (was legacy Time_varying_q)
    linkage_spec(~ rw(1 | Year),
                 by = ~ fleet,
                 fleet = "Pollock_survey_3_adfg",
                 init = list(sigma = 0.05)))))

# (2) Selectivity priors. Slopes are on the log scale (lognormal),
# asymptote on the natural scale (normal).
# The ascending limb also carries the fishery random walk as a second spec.
# goa_pk penalizes these deviates rather than integrating them, hence
# integrate = FALSE, at sel_dev_sd on the slope and 4x that on the inflection.
# Ascending only: goa_pk maps its descending deviates off, and A8 removes the
# constant penalty it used to charge on them.
SEL_RW_SD <- pollock25$fleet_control$Time_varying_sel_sd[FISHERY]
sel_spec <- build_selectivity(linkages = list(
  slp_asc  = list(
    linkage_spec(~ 1, by = ~ fleet, fleet = ASC_LIMB_PRIOR,
                 priors = list(`(Intercept)` = lognormal(-1, 1.5))),
    linkage_spec(~ rw(1 | Year), by = ~ fleet, fleet = "GOA_pollock_fishery",
                 init = list(sigma = SEL_RW_SD), integrate = FALSE)),
  inf_asc  = list(
    linkage_spec(~ 1, by = ~ fleet, fleet = ASC_LIMB_PRIOR,
                 priors = list(`(Intercept)` = normal(0, 3))),
    linkage_spec(~ rw(1 | Year), by = ~ fleet, fleet = "GOA_pollock_fishery",
                 link = "identity",
                 init = list(sigma = 4 * SEL_RW_SD), integrate = FALSE)),
  slp_desc = linkage_spec(~ 1, by = ~ fleet, fleet = DESC_LIMB_PRIOR,
                          priors = list(`(Intercept)` = lognormal(-1, 1.5))),
  inf_desc = linkage_spec(~ 1, by = ~ fleet, fleet = DESC_LIMB_PRIOR,
                          priors = list(`(Intercept)` = normal(10, 3)))))

# (3) Dirichlet-multinomial prior: goa_pk adds dnorm(log_DM_pars, 0, 2)
comp_spec <- build_composition(linkages = list(
  theta_comp = linkage_spec(~ 1, by = ~ fleet, fleet = DM_PRIOR,
                            priors = list(`(Intercept)` = lognormal(0, 2)))))

# ---- Model fixes -----------------------------
pollock25$fleet_control$Selectivity_index[FISHERY] <- FISHERY

# Shelikof q uses the updated AR1/Ecov linkage, so drop the legacy code-6 AR1
# / random-walk on q1 (Catchability = free mean + the ar1() linkage on top).
pollock25$fleet_control$Catchability[SHELIKOF]    <- "Estimated"
pollock25$fleet_control$Time_varying_q[SHELIKOF]  <- "Off"
# srv3's legacy random-walk q is now a rw(1 | Year) linkage (above), so switch
# off its legacy mode; srv2 is an estimated q with the BT prior N(log 0.85, 0.1).
pollock25$fleet_control$Time_varying_q[3] <- "Off"
# All four fishery selectivity walks are rw(1 | Year) linkages now (above), so
# switch off the legacy mode they replace.
pollock25$fleet_control$Time_varying_sel[FISHERY] <- "Off"
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
# fleet_control$Comp_weights (it overrides inits$comp_weights), so set the DM log-
# weights on the fleet_control column. Cole's log_DM_pars order is (fishery, srv1,
# srv2, srv3, srv6) -> Rceattle fleet codes 8, 1, 2, 3, 6.
pollock25$fleet_control$Comp_weights[c(FISHERY, 1, 2, 3, 6)] <- pl$log_DM_pars

# ============================================================================
# (A) Forward pass ----
# ============================================================================
# Build the inits from a estimateMode = "DebugBuild" run and input values
skel <- fit_mod(
  data_list = pollock25,
  inits = NULL,
  estimateMode = "DebugBuild",
  msmMode = "SingleSpecies",
  initMode = "OffsetEquilibrium",
  random_rec = TRUE,
  random_q = TRUE,
  qFun = q_spec,
  selFun = sel_spec,
  compFun = comp_spec,
  fit_control = fit_control(phase = FALSE, getsd = FALSE, verbose = 0, bias_adjust_proc = FALSE))

inits <- skel$estimated_params

# -- Recruitment (goa_pk is in millions -> +log(1e6) on the mean) ------------
inits$rec_pars[, 1] <- log(exp(pl$mean_log_recruit) * 1e6)
inits$rec_dev[, 1:nyrs] <- pl$dev_log_recruit
inits$R_log_sd <- log(pl$sigmaR)
# NB: no init_dev assignment -- initMode = "OffsetEquilibrium" seeds the initial
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

# -- Fishery time-varying selectivity deviates -------------------------------
# All four walks are now rw() linkages, injected with the other linkage states
# below; nothing goes into the legacy log_sel_slp_dev / sel_inf_dev arrays.

# -- Catchability means ------------------------------------------------------
inits$index_log_q[1:6] <- unlist(pl[c("log_q1_mean", "log_q2_mean", "log_q3_mean",
                                      "log_q4", "log_q5", "log_q6")])

# -- Linkage random-effect states --------------------------------------------
# Three groups now: the Shelikof QAR1 and the srv3 random walk are integrated
# (beta_linkage_re); the two fishery selectivity walks are penalized
# (beta_linkage_re_pen). Index by re_pos, NOT re_index: re_index is the slot in
# the global numbering across both vectors, so it runs past the end of
# beta_linkage_re_pen once both treatments are present.
#
# The grammar pins each walk's first deviate, but a pinned parameter is held at
# whatever `inits` supplies -- not at 0 -- so goa_pk's deviates go in as they are
# and every base stays equal to goa_pk's mean. Do NOT fold the first deviate into
# the base: the fishery's base parameters carry the selectivity priors, and
# shifting the level into the base moves the point those priors are evaluated at
# (goa_pk evaluates them at the mean). The likelihood is invariant to the shift;
# the prior is not.
.lt  <- as.data.frame(skel$data_list$linkage_table)
.lt  <- .lt[!is.na(.lt$re_struct), ]
.grp <- function(proc, param, flt) {
  g <- .lt[.lt$process == proc & .lt$param == param & .lt$fleet == flt, ]
  g[order(g$re_time), ]
}
.she <- .grp("q", "q", SHELIKOF)
.adf <- .grp("q", "q", 3L)

inits$beta_linkage_re[.she$re_pos + 1L] <- pl$Ecov_exp        # Shelikof AR1 latent
inits$beta_linkage_re[.adf$re_pos + 1L] <- pl$log_q3_dev      # srv3 RW, first deviate included

# The four fishery selectivity walks. goa_pk's deviates go in unmodified and each
# base stays at goa_pk's mean -- which is where goa_pk evaluates the selectivity
# priors.
for (.w in list(list("slp_asc", pl$slp1_fsh_dev),
                list("inf_asc", pl$inf1_fsh_dev))) {
  .g <- .grp("sel", .w[[1]], FISHERY)
  inits$beta_linkage_re_pen[.g$re_pos + 1L] <- .w[[2]]
}
rm(.w, .g)

# Process SD: set only the Shelikof AR1 group's slot. The srv3 walk and both
# selectivity walks have their sigma fixed by the spec and must stay put.
inits$log_sigma_linkage[.she$sigma_index[1] + 1L] <- pl$log_Ecov_sd
rm(.lt, .grp, .she, .adf)

# -- QAR1 hyperparameters -----------------------------------------------------
# The AR1 latent enters log q1 as beta_linkage_obs * beta_linkage_re, observed
# against QcovPol with fixed SD; rho is the AR1 correlation.
inits$beta_linkage_obs   <- pl$Ecov_beta                # effect size on log q1
inits$trans_rho_linkage  <- pl$transf_rho               # AR1 correlation (logit space)
inits$log_obs_sd_linkage <- pl$log_Ecov_obs_sd          # fixed measurement SD

# -- Dirichlet-multinomial weights (theta) -----------------------------------
# Set on fleet_control$Comp_weights above (fit_mod() overrides inits$comp_weights)


# -- Fit at the fixed solution ----------------------------------------------
pollock_fixed <- fit_mod(
  data_list = pollock25,
  inits = inits,
  estimateMode = "DebugBuild",
  msmMode = "SingleSpecies",
  initMode = "OffsetEquilibrium",
  random_rec = TRUE,
  random_q = TRUE,
  qFun = q_spec,
  selFun = sel_spec,
  compFun = comp_spec,
  fit_control = fit_control(phase = FALSE, getsd = FALSE, verbose = 0, bias_adjust_proc = FALSE))

# ---- Comparison --------------------------
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
# SSB / recruitment are in millions in goa_pk. Absolute tolerances here, not the
# 1e-6 used for the dimensionless quantities above: these series run to ~1e9, so
# 1e-4 absolute is ~1e-13 relative -- float64 round-off, not a model difference.
report("SSB (x1e6)", qf$ssb[1, 1:nyrs], rep$Espawnbio * 1e6, tol = 1e-4)
report("recruitment (x1e6)", qf$R[1, 1:nyrs], rep$recruit * 1e6, tol = 1e-4)

# ---- Initial age structure -------------------------------------------------
# The target is the CORRECTED goa_pk (A1 applied in reference/goa_pk_2024_mfix.cpp),
# so the initial ages agree too and the check covers every year -- there is no
# longer an intended deviation to isolate. If year 1 breaks while years 2+ hold,
# the M-indexing correction has come undone.
cat("\n== Initial-age structure (M-index fix applied in both models) ==\n")
report("N-at-age all yrs (x1e6)", qf$N_at_age[1, 1, , 1:nyrs],
       t(rep$N) * 1e6, tol = 1e-4)   # rep$N is [year, age]; transpose to [age, year]

# ---- Per-component likelihood table ----------------------------------------
cat("\n== Component NLL: Rceattle jnll_comp vs goa_pk -loglik ==\n")
print(round(pollock_fixed$quantities$jnll_comp, 4))
cat("goa_pk total objective:", fit$opt$objective,
    "  Rceattle jnll:", pollock_fixed$quantities$jnll, "\n")

# ============================================================================
# (B) Estimation ----
# ============================================================================
pollock_est <- fit_mod(
  data_list = pollock25,
  inits = inits,
  estimateMode = "Hindcast",
  msmMode = "SingleSpecies",
  initMode = "OffsetEquilibrium",
  random_rec = TRUE,
  random_q = TRUE,
  qFun = q_spec,
  selFun = sel_spec,
  compFun = comp_spec,
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
