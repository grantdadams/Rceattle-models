# Code to run a management strategy evaluation (MSE) for the bering sea pollock model in Rceattle

# The operating and estimation models (OMs & EMs) are the same and fit to survey and fishery data from 1964 to 2024.
# Both are single sex, single-species models with 3 surveys and one fishery. The EM uses the North Pacific Fishery Management Council's
# Tier-3 harvest control rule to provide recommended catch.


# The MSE does the following for each projection year from 2025 to 2050 following a 1-year assessment cycle:

# 1) Recommended catch from the EM for year "y+1" is aggregated to the OM's data-set
# 2) The OM's dynamics are updated from year "y" to "y+1". All parameters except those associated with fishing in year "y + 1" are held constant.
# 3) Expected survey and fishery data are extracted from the OM for year "y+1" and aggregated to the EM's data-set. This may include stochastic observation error!
# 4) The EM is fit to the new data until year "y+1". All parameters are estimated

# The loop repeats until 2050.


# Load libraries ----
library(Rceattle) # https://github.com/grantdadams/Rceattle/tree/dev-name-change

check = Sys.time()
# Load data ----
ebs_pollock <- Rceattle::read_data( file = "Data/bsp0.xlsx")
ebs_pollock$estDynamics = 0
ebs_pollock$index_data$Log_sd <- ebs_pollock$index_data$Log_sd/ebs_pollock$index_data$Observation
ebs_pollock$catch_data$Catch <- ebs_pollock$catch_data$Catch*1000
ebs_pollock$catch_data$Log_sd <- 0.05
ebs_pollock$fleet_control$Fleet_type[5:6] <- "Survey"   # Setting age-1 data as survey
ebs_pollock$fleet_control$proj_F_prop[1] <- 1


# Fit operating model (OM) ----
# - Fit to historical data
# - This model represents the true state of the environment
pollock_om <- Rceattle::fit_mod(data_list = ebs_pollock,
                                  inits = NULL,    # Initial parameters = 0
                                  file = NULL,     # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0,     # Single species mode
                                  verbose = 1,
                                  phase = TRUE,
                                  initMode = "NonEquilibrium")    # Unfished equilibrium with init_dev's turned on


# Fit estimation model (EM) ----
# - Fit to historical data
# - This represents are hypothesized state of the environment and is used to provide management advice
pollock_em <- Rceattle::fit_mod(data_list = ebs_pollock,
                                inits = NULL,    # Initial parameters = 0
                                file = NULL,     # Don't save
                                estimateMode = 0, # Estimate
                                random_rec = FALSE, # No random recruitment
                                msmMode = 0,     # Single species mode
                                verbose = 1,
                                phase = TRUE,
                                HCR = build_hcr(HCR = 5,          # NPFMC Tier3 HCR
                                                Ftarget = 0.4, # F40%
                                                Flimit = 0.35, # F35%
                                                Plimit = 0.2,     # No fishing when SB<SB20
                                                Alpha = 0.05),
                                initMode = "NonEquilibrium")    # Unfished equilibrium with init_dev's turned on


# Plot models ----
plot_biomass(list(pollock_em, pollock_om), incl_proj = TRUE, model_names = c("EM", "OM"))
plot_catch(list(pollock_em, pollock_om), incl_proj = TRUE, model_names = c("EM", "OM"))


# Run MSEs ----
# * MSE 1 ----
# - No stochasticity in future observations (observation error) and recruitment (number of new fish entering the population)
mse1 <- run_mse(om = pollock_om, em = pollock_em, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = FALSE, sample_rec = FALSE)


# * MSE 2 ----
# - No stochasticity in future observations (observation error), but stochastic recruitment (number of new fish entering the population)
# -- Recruitment stochasticity is treating variation in recruitment in the projection similar to historical estimates
mse2 <- run_mse(om = pollock_om, em = pollock_em, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = FALSE, sample_rec = TRUE)


# * MSE 3 ----
# - Add stochasticity in future observations (observation error) and recruitment (number of new fish entering the population)
# -- Recruitment stochasticity is treating variation in recruitment in the projection similar to historical estimates
# -- Observation error adds random variation to the data following the assumed probability distribution and historical variance
mse3 <- run_mse(om = pollock_om, em = pollock_em, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = TRUE, sample_rec = TRUE)


# Plot MSEs ----
# - Operating model SSB and SSB depletion
plot_ssb(list(mse1$Sim_1$OM,
                  mse2$Sim_1$OM,
                  mse3$Sim_1$OM
),
model_names = c("MSE1", "MSE2", "MSE3"))


plot_depletionSSB(list(mse1$Sim_1$OM,
              mse2$Sim_1$OM,
              mse3$Sim_1$OM
),
model_names = c("MSE1", "MSE2", "MSE3"))


# - Estimation models from MSE3
plot_ssb(mse3$Sim_1$EM, incl_proj = TRUE)
plot_catch(mse3$Sim_1$EM, incl_proj = TRUE)
plot_recruitment(mse3$Sim_1$EM, incl_proj = TRUE)


# Query data ----
# Extract data from terminal year of MSE that is used to fit the models
mse3$Sim_1$EM$`OM_Sim_1. EM_yr_2050`$data_list$index_data # Survey index time-series
mse3$Sim_1$EM$`OM_Sim_1. EM_yr_2050`$data_list$comp_data # Survey and fishery composition data
mse3$Sim_1$EM$`OM_Sim_1. EM_yr_2050`$data_list$catch_data # Fishery catch time-series


runtime = Sys.time() - check
