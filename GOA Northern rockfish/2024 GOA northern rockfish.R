# =============================================================================
# 2024 GOA northern rockfish assessment in Rceattle (CEATTLE)
# =============================================================================
# Single-sex, single-species model.
#
# DATA  (Data/2024_GOA_northern_rockfish.xlsx, built in
#        "2024 GOA northern rockfish bridging.R" from the urm data object)
# - Fishery catch (1961-2024)
# - Fishery age and length composition
# - Survey biomass (NMFS GOA bottom trawl) and SD
# - Survey age composition
# - Empirical weight-at-age & maturity-at-age
# - Ageing-error and size-at-age (growth) transition matrices
#
# MODEL
# - Single sex
# - Survey selectivity  = logistic, with catchability q (prior)
# - Fishery selectivity = logistic
# - Empirical weight-at-age
# - M = fixed / estimated with lognormal prior (urm prior: mean 0.06, cv 0.05)
#
# The 2024 reference model is the RTMB "urm" model (BenWilliams-NOAA/urm); see
# "2024 GOA northern rockfish bridging.R" for the full reconciliation and for
# the structural differences between the urm and Rceattle codebases.
# =============================================================================

library(Rceattle)
library(dplyr)
library(readxl)

# Load data ----
# Built by the bridging script from ./2024 model/urm-main/data/dat.RDS.
# Input sample sizes and the lognormal survey Log_sd are already baked in, so
# no post-hoc sample-size rescaling is needed (unlike the 2022 SAFE CSV step).
mydata <- Rceattle::read_data(file = "Data/2024_GOA_northern_rockfish.xlsx")
yrs <- mydata$styr:mydata$endyr


# Model 1 ----
# - Estimate (M fixed at the data value, 0.06)
model1 <- Rceattle::fit_mod(data_list = mydata,
                            inits = NULL,        # Initial parameters = 0
                            file = NULL,         # Don't save
                            estimateMode = 0,    # Estimate
                            random_rec = FALSE,  # No random recruitment
                            msmMode = 0,         # Single species mode
                            verbose = 1,
                            phase = TRUE,
                            initMode = 1)        # Assume unfished equilibrium


# Model 2 ----
# - Estimate M with lognormal prior (urm: mean_M = 0.06, cv_M = 0.05).
#   urm ESTIMATES M (log_M is a free parameter with this prior), so this is the
#   configuration directly comparable to the urm reference model.
#   M_prior_sd is log-scale (= cv_M = 0.05), matching urm's dnorm(log(M), ...).
model2 <- Rceattle::fit_mod(data_list = mydata,
                            inits = NULL,
                            file = NULL,
                            estimateMode = 0,
                            random_rec = FALSE,
                            msmMode = 0,
                            verbose = 1,
                            phase = TRUE,
                            initMode = 1,
                            M1Fun = build_M1(updateM1 = TRUE,
                                             M1_model     = 1,
                                             M1_use_prior = TRUE,
                                             M_prior      = 0.06,
                                             M_prior_sd   = 0.05))


# - urm reference model -------------------------------------------------------
# Overlay the 2024 RTMB "urm" output (saved by the bridging script as
# urm_report_2024.Rdata). Stuffed into a copy of model1 for plotting, the same
# way the 2022 script overlaid the ADMB SAFE estimates.
if (file.exists("Data/urm_report_2024.Rdata")) {
  load("Data/urm_report_2024.Rdata")        # -> urm_report
  urm_mod <- model1
  urm_mod$quantities$biomass[1, 1:length(yrs)] <- urm_report$tot_bio
  urm_mod$quantities$ssb[1, 1:length(yrs)]     <- urm_report$spawn_bio
  urm_mod$quantities$R[1, 1:length(yrs)]       <- urm_report$recruits

  plot_biomass(list(model1, model2, urm_mod),
               model_names = c("CEATTLE fix M", "CEATTLE est M", "urm (RTMB)"))
  plot_ssb(list(model1, model2, urm_mod),
           model_names = c("CEATTLE fix M", "CEATTLE est M", "urm (RTMB)"))
} else {
  # urm output not available - just compare the two CEATTLE configurations
  plot_biomass(list(model1, model2), model_names = c("CEATTLE fix M", "CEATTLE est M"))
  plot_ssb(list(model1, model2), model_names = c("CEATTLE fix M", "CEATTLE est M"))
}
