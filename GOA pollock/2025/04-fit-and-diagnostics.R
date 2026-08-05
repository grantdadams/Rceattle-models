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

# Linkages name their fleets (can also use Fleet_code):
SHELIKOF_ACOUSTIC <- "Pollock_survey_1_shelikof_acoustic"
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
