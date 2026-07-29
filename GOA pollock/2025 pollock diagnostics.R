# Fit the GOA pollock model and run the standard diagnostic suite.
# https://grantdadams.github.io/Rceattle/articles/model-diagnostics.html

library(Rceattle)

# Data ----
# Assembled in "2025 pollock build data.R". Swap in GOA_26_pollock.Rdata once the
# terminal year is finalized ("2025 pollock update data.R").
load("Data/GOA_25_pollock.Rdata")            # -> pollock25

# Model configuration ----
# Dirichlet-multinomial age comps, Rogers et al. (2024) AR1 catchability on the
# Shelikof acoustic survey, and normal priors on the logistic selectivity.
SHELIKOF <- 1L; BOTTOM_TRAWL <- 2L; FISHERY <- 8L
fc <- pollock25$fleet_control
fc$Selectivity_index[FISHERY]          <- FISHERY               # fishery keeps its own block
fc$Catchability[SHELIKOF]              <- "Estimated"
fc$Time_varying_q[SHELIKOF]            <- "Off"                 # the AR1 link replaces it
fc$Catchability[BOTTOM_TRAWL]          <- "Estimated-with-prior"
fc$Catchability_init[BOTTOM_TRAWL]     <- 0.85
fc$Catchability_prior_sd[BOTTOM_TRAWL] <- 0.1
fc$Comp_accum_young                    <- 1L
fc$Comp_accum_old                      <- 0L
fc$Comp_accum_young[FISHERY]           <- 2L                    # fold fishery age-1 into age-2
fc$Comp_accum_young[SHELIKOF]          <- 3L                    # fold Shelikof ages 1-2 into age-3
fc$Comp_weights[c(FISHERY, 1, 2, 3, 6)] <- 0                    # DM log-theta starts (estimated)
pollock25$fleet_control <- fc

q_spec <- build_catchability(linkages = list(
  q = linkage_spec(~ ar1(1 | Year), by = ~ fleet, fleet = SHELIKOF,
                   observe = "QcovPol", obs_sd = 0.02)))        # fixed Ecov measurement SD

sel_spec <- build_selectivity(linkages = list(
  slp_asc  = linkage_spec(~ 1, by = ~ fleet, fleet = c(2, 3, 6, 8),
                          priors = list(`(Intercept)` = lognormal(-1, 1.5))),
  inf_asc  = linkage_spec(~ 1, by = ~ fleet, fleet = c(2, 3, 6, 8),
                          priors = list(`(Intercept)` = normal(0, 3))),
  slp_desc = linkage_spec(~ 1, by = ~ fleet, fleet = c(1, 2, 6, 8),
                          priors = list(`(Intercept)` = lognormal(-1, 1.5))),
  inf_desc = linkage_spec(~ 1, by = ~ fleet, fleet = c(1, 2, 6, 8),
                          priors = list(`(Intercept)` = normal(10, 3)))))

comp_spec <- build_composition(linkages = list(
  theta_comp = linkage_spec(~ 1, by = ~ fleet, fleet = c(FISHERY, 1, 2, 3, 6),
                            priors = list(`(Intercept)` = lognormal(0, 2)))))

# Fit ----
mod_25 <- fit_mod(data_list = pollock25,
                  estimateMode = "Hindcast",            # hindcast (add an HCR + estimateMode = "Estimate" to project)
                  random_rec = TRUE, random_q = TRUE,
                  msmMode = "SingleSpecies",
                  initMode = "FishedEquilibrium",
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
