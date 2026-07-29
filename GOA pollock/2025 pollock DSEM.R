# Environmentally-driven recruitment for GOA pollock via a dynamic SEM.
# Compares an IID recruitment baseline with a full environment -> recruitment SEM.

# Libraries ----
# remotes::install_version("dsem", version = "3.0.0")     # required version
# remotes::install_github("grantdadams/Rceattle@dev-DSEM")
library(Rceattle)                              # dev-DSEM
library(dplyr)

# Data ----
# Current assessment data (1970-2024), from "2025 pollock build data.R".
load("Data/GOA_25_pollock.Rdata")              # -> pollock25
pollock25$estDynamics <- "Estimated"

# Base fleet settings (the selectivity priors and Rogers-2024 AR1 catchability
# used in the production fit live in "2025 pollock diagnostics.R"; here we keep
# the fleet structure simple to isolate the recruitment SEM).
SHELIKOF <- 1L; BOTTOM_TRAWL <- 2L; FISHERY <- 8L
fc <- pollock25$fleet_control
fc$Catchability[c(SHELIKOF, BOTTOM_TRAWL)] <- "Estimated"
fc$Comp_accum_young                    <- 1L
fc$Comp_accum_old                      <- 0L
fc$Comp_accum_young[FISHERY]           <- 2L   # fold fishery age-1 into age-2
fc$Comp_accum_young[SHELIKOF]          <- 3L   # fold Shelikof ages 1-2 into age-3
fc$Comp_weights[c(FISHERY, 1, 2, 3, 6)] <- 0   # DM log-theta starts (estimated)
pollock25$fleet_control <- fc

# Environmental covariates ----
# @kalei replace the random placeholders with the real GOA pollock indices.
env_data <- data.frame(Year = pollock25$styr:pollock25$endyr,
                       EnvIndex1 = rnorm(length(pollock25$styr:pollock25$endyr)),
                       EnvIndex2 = rnorm(length(pollock25$styr:pollock25$endyr)),
                       EnvIndex3 = rnorm(length(pollock25$styr:pollock25$endyr)))
pollock25$env_data <- env_data
plot_data(pollock25)

# SEM specifications ----
# * IID baseline (recruitment variance only) ----
pk_iid_sem <- "
  # source        link  target         lag  param          start
  # --- AR1 process for each covariate ---
  EnvIndex1  ->  EnvIndex1,       1,   EnvIndex1_AR1,   0
  EnvIndex2  ->  EnvIndex2,       1,   EnvIndex2_AR2,   0
  EnvIndex3  ->  EnvIndex3,       1,   EnvIndex3_AR3,   0
  # --- recruitment variance ---
  recdevs1  <->  recdevs1,        0,   sigmaR1,         1
"

# * Full SEM (covariates -> recruitment) ----
pk_sem <- "
  # source        link  target         lag  param            start
  # --- AR1 process for each covariate ---
  EnvIndex1  ->  EnvIndex1,       1,   EnvIndex1_AR1,     0
  EnvIndex2  ->  EnvIndex2,       1,   EnvIndex2_AR2,     0
  EnvIndex3  ->  EnvIndex3,       1,   EnvIndex3_AR3,     0
  # --- covariates -> recruitment ---
  EnvIndex1  ->  recdevs1,        1,   EnvIndex1_to_R,    0
  EnvIndex2  ->  recdevs1,        1,   EnvIndex2_to_R,    0
  EnvIndex3  ->  recdevs1,        1,   EnvIndex3_to_R,    0
  # --- recruitment variance ---
  recdevs1  <->  recdevs1,        0,   sigmaR1,           1
"

# Fit ----
# * IID baseline ----
pk_iid_mod <- fit_mod(data_list = pollock25,
                      estimateMode = "Hindcast",        # hindcast (add an HCR + estimateMode = "Estimate" to project)
                      random_rec = TRUE,
                      dsem = build_DSEM(sem = pk_iid_sem, family = "fixed",
                                        sigmaR_prior_sd = 0.5),
                      msmMode = "SingleSpecies",
                      initMode = "FishedEquilibrium",
                      fit_control = fit_control(verbose = 1, phase = TRUE))
summary(pk_iid_mod)
AIC(pk_iid_mod)

# * Full SEM ----
pk_dsem_mod <- fit_mod(data_list = pollock25,
                       estimateMode = "Hindcast",
                       random_rec = TRUE,
                       dsem = build_DSEM(sem = pk_sem, family = "fixed",
                                         sigmaR_prior_sd = 0.5),
                       msmMode = "SingleSpecies",
                       initMode = "FishedEquilibrium",
                       fit_control = fit_control(verbose = 1, phase = TRUE))
summary(pk_dsem_mod)
AIC(pk_dsem_mod)
