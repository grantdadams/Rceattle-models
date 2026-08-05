# Fit the GOA pollock model and run the standard diagnostic suite.
# https://grantdadams.github.io/Rceattle/articles/model-diagnostics.html

library(Rceattle)

# Data ----
pollock25 <- read_data("Data/GOA_25_pollock_single_species_1970-2024.xlsx")
load("Data/2024pollock_mfix_estSigR.Rdata")    # Corrected goa_pk fit sigmaR estimated
pl <- fit$obj$env$parList()          # skeleton: shapes, and the mapped-off pars

# Model configuration ----
SHELIKOF <- 1L; BOTTOM_TRAWL <- 2L; FISHERY <- 8L # Fleet indices

# identical to "02-bridge.R"
pollock25$fleet_control$Selectivity_index[FISHERY] <- FISHERY
pollock25$fleet_control$Catchability[SHELIKOF]   <- "Estimated"
pollock25$fleet_control$Time_varying_q[SHELIKOF] <- "Off"
pollock25$fleet_control$Catchability[BOTTOM_TRAWL] <- "Estimated-with-prior"
pollock25$fleet_control$Catchability_init[BOTTOM_TRAWL] <- 0.85
pollock25$fleet_control$Catchability_prior_sd[BOTTOM_TRAWL] <- 0.1

# Composition young-age accumulation (fishery age-1 -> 2; Shelikof ages 1-2 -> 3).
pollock25$fleet_control$Comp_accum_young <- 1L
pollock25$fleet_control$Comp_accum_old   <- 0L
pollock25$fleet_control$Comp_accum_young[FISHERY]  <- 2L
pollock25$fleet_control$Comp_accum_young[SHELIKOF] <- 3L

# DM weights (log theta) starting values from goa_pk (fit_mod gets these from
# fleet_control$Comp_weights); estimated freely from there.
pollock25$fleet_control$Comp_weights[c(FISHERY, 1, BOTTOM_TRAWL, 3, 6)] <- pl$log_DM_pars

# Survey-3 (adfg) time-varying q is expressed through the linkage grammar below,
# so switch off the legacy fleet_control mode it replaces.
ADFG <- 3L
pollock25$fleet_control$Time_varying_q[ADFG] <- "Off"   # -> rw(1 | Year) on q, below

# TODO: also express the fishery ascending-selectivity random walk
# (Time_varying_sel = "RandomWalkAscending") through the grammar. It is left on
# the legacy switch for now because it is a *penalized fixed effect* (goa_pk
# convention), whereas the grammar's rw() is a Laplace-integrated random effect
# -- integrating it shifts the fit (~14% SSB) and breaks exact goa_pk
# reproduction. Convert once linkage_spec() gains an `integrate = FALSE` switch
# (penalized fixed effect, permitted only with a fixed sigma).

# Linkages name their fleets (can also use Fleet_code):
SHELIKOF_ACOUSTIC <- "Pollock_survey_1_shelikof_acoustic"
ADFG_SURVEY       <- "Pollock_survey_3_adfg"
ASC_LIMB_PRIOR_FLEETS  <- c("Pollock_survey_2_bottom_trawl",
                            "Pollock_survey_3_adfg",
                            "Pollock_survey_6_summer_acoustic",
                            "GOA_pollock_fishery")
DESC_LIMB_PRIOR_FLEETS <- c("Pollock_survey_1_shelikof_acoustic",
                            "Pollock_survey_2_bottom_trawl",
                            "Pollock_survey_6_summer_acoustic",
                            "GOA_pollock_fishery")
DM_PRIOR_FLEETS        <- c("GOA_pollock_fishery",
                            "Pollock_survey_1_shelikof_acoustic",
                            "Pollock_survey_2_bottom_trawl",
                            "Pollock_survey_3_adfg",
                            "Pollock_survey_6_summer_acoustic")

# * Catchability: QAR1 (Shelikof) + random-walk q (adfg) ----
# Shelikof is the state-space QAR1; the adfg survey is a random walk on q. The
# random-walk SD is fixed at 0.05 (init = list(sigma = ...) with no sigma prior
# fixes it), matching the legacy Time_varying_q RW penalty (index_q_dev_sd).
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

# * Selectivity priors ----
# mirror goa_pk exactly (both limbs on srv2 and srv6). The fishery ascending-limb
# random walk stays on the legacy Time_varying_sel switch (see TODO above).
sel_spec <- build_selectivity(linkages = list(
  slp_asc  = linkage_spec(~ 1,
                          fleet = ASC_LIMB_PRIOR_FLEETS,
                          priors = list(intercept = lognormal(-1, 1.5))),
  inf_asc  = linkage_spec(~ 1,
                          fleet = ASC_LIMB_PRIOR_FLEETS,
                          priors = list(intercept = normal(0, 3))),
  slp_desc = linkage_spec(~ 1,
                          fleet = DESC_LIMB_PRIOR_FLEETS,
                          priors = list(intercept = lognormal(-1, 1.5))),
  inf_desc = linkage_spec(~ 1,
                          fleet = DESC_LIMB_PRIOR_FLEETS,
                          priors = list(intercept = normal(10, 3)))))
# * Dirichlet-multinomial prior ----
# (dnorm(log_DM_pars, 0, 2)).
comp_spec <- build_composition(linkages = list(
  theta_comp = linkage_spec(~ 1,
                            fleet = DM_PRIOR_FLEETS,
                            priors = list(intercept = lognormal(0, 2)))))

# Fit ----
mod_25 <- fit_mod(data_list = pollock25,
                  estimateMode = "Hindcast",            # hindcast (add an HCR + estimateMode = "Estimate" to project)
                  random_rec = TRUE, random_q = TRUE,
                  msmMode = "SingleSpecies",
                  initMode = "OffsetEquilibrium",
                  qFun = q_spec, selFun = sel_spec, compFun = comp_spec,
                  fit_control = fit_control(phase = TRUE, verbose = 1,
                                            bias_adjust_proc = FALSE))

# Diagnostics ----
# * Summaries ----
summary(mod_25)
convergence_diagnostics(mod_25)

# * Fit plots ----
plot_index(mod_25)
plot_logindex(mod_25)
plot_indexresidual(mod_25)
plot_comp(mod_25)
plot_catch(mod_25)

# * OSA residuals (Stewart & Monnahan 2025 SDNR / tail diagnostics) ----
osa <- osa_residuals(mod_25)
osa_diagnostics(osa)
plot(osa)                                      # Q-Q + residual-by-year

# * Retrospective (Mohn's rho) ----
mod_25_retro <- retrospective(Rceattle = mod_25, peels = 5)
mod_25_retro$mohns
plot_biomass(mod_25_retro$Rceattle_list)

# * Jitter (optimum stability) ----
mod_25_jitters <- jitter(Rceattle = mod_25, njitter = 100, phase = TRUE)
hist(log(mod_25_jitters$nll - min(mod_25_jitters$nll)),
     main = "Jitter NLL spread (log scale)", xlab = "log(NLL - min NLL)")
plot_biomass(mod_25_jitters$Rceattle_list)

# * Self-test (estimation bias) ----
mod_25_sims <- self_test(mod_25, nsim = 100)
length(mod_25_sims)                            # converged simulations
plot_biomass(c(list(mod_25), mod_25_sims),
             model_names = c("fit", names(mod_25_sims)))

# * Likelihood profile on sigmaR ----
prof_sigmaR <- profile(fitted = mod_25, param = "sigmaR", slots = list(1),
                       values = list(seq(0.1, 1.5, by = 0.05)))
plot(prof_sigmaR$grid$slot_1,
     prof_sigmaR$nll - min(prof_sigmaR$nll, na.rm = TRUE),
     type = "l", xlab = "sigmaR", ylab = "dNLL")
