# =============================================================================
# EBS pollock 2024 -- management strategy evaluation (MSE)
# =============================================================================
# OFF-PIPELINE (un-numbered): a research run, not part of the assessment sequence.
#
# Run from the "EBS pollock" project root.
# Reads:  Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx
# Prereq: "01-build-data.R"
#
# The operating and estimation models (OMs & EMs) are the same and fit to survey and fishery data from 1964 to 2024.
# Both are single sex, single-species models with 3 surveys and one fishery. The EM uses the North Pacific Fishery Management Council's
# Tier-3 harvest control rule to provide recommended catch.
#
# The MSE does the following for each projection year from 2025 to 2050 following a 1-year assessment cycle:
#
# 1) Recommended catch from the EM for year "y+1" is aggregated to the OM's data-set
# 2) The OM's dynamics are updated from year "y" to "y+1". All parameters except those associated with fishing in year "y + 1" are held constant.
# 3) Expected survey and fishery data are extracted from the OM for year "y+1" and aggregated to the EM's data-set. This may include stochastic observation error!
# 4) The EM is fit to the new data until year "y+1". All parameters are estimated
#
# The loop repeats until 2050.
#
# TODO: the OM/EM below use the default flat selectivity start. "03-model-comparison.R"
# and "04-fit-and-diagnostics.R" both start fishery selectivity from the data and
# switch the time-varying deviations on only after a base fit, because the fishery
# selectivity likelihood is multimodal and the flat start finds an early-period
# local optimum. Port that two-stage start here before reading any MSE result.
# =============================================================================

# Load libraries ----
library(Rceattle)

check = Sys.time()

# Load data ----
# The workbook from "01-build-data.R" already carries the ADMB-bridge encoding:
# catch Log_sd = 0.05, the age-1 index Log_sd, ATS_1 as a "Survey", and BTS_1
# switched "Off" (its age-1 observations were folded back into the BTS age comps,
# so re-enabling it would double-count them). Do NOT re-apply any of that here,
# and do NOT rescale index Log_sd by Observation -- the xlsx Log_sd is already a
# CV / log-sd. Catch is in thousand tonnes, matching ADMB obs_catch;
# "02-bridge.R" compares the two unscaled.
ebs_pollock <- Rceattle::read_data(file = "Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx")
ebs_pollock$estDynamics = 0

# Look fleets up by name rather than hardcoding row numbers, so the script does
# not silently attach to the wrong fleet if the fleet order changes. Use the
# CANONICAL column name: read_data() upgrades the old proj_F_prop spelling, so
# assigning to the old name would create a dead column fit_mod() ignores.
#
# run_mse() sums Proj_F_proportion WITHOUT na.rm, so an NA anywhere in the column
# aborts the run. The workbook ships NA on the CPUE row, so set the whole column
# explicitly: all projected F goes to the single fishery.
fsh <- which(ebs_pollock$fleet_control$Fleet_name == "Fishery")
ebs_pollock$fleet_control$Proj_F_proportion      <- 0
ebs_pollock$fleet_control$Proj_F_proportion[fsh] <- 1

M1Fun <- build_M1(updateM1 = TRUE, M1_model = "fixed")
ctl   <- fit_control(verbose = 1, phase = TRUE,
                     bias_adjust_proc = 0, bias_adjust_obs = 0, comp_offset = 1e-3)


# Fit operating model (OM) ----
# - Fit to historical data
# - This model represents the true state of the environment
ebs_om <- Rceattle::fit_mod(data_list = ebs_pollock,
                                inits = NULL,          # Initial parameters = 0
                                file = NULL,           # Don't save
                                estimateMode = "Estimate",   # 0 = hindcast + HCR projection
                                random_rec = FALSE,    # No random recruitment
                                msmMode = "SingleSpecies",
                                initMode = "NonEquilibrium",  # equilibrium + init devs
                                M1Fun = M1Fun,
                                fit_control = ctl)


# Fit estimation model (EM) ----
# - Fit to historical data
# - This represents are hypothesized state of the environment and is used to provide management advice
ebs_em <- Rceattle::fit_mod(data_list = ebs_pollock,
                                inits = NULL,          # Initial parameters = 0
                                file = NULL,           # Don't save
                                estimateMode = "Estimate",   # 0 = hindcast + HCR projection
                                random_rec = FALSE,    # No random recruitment
                                msmMode = "SingleSpecies",
                                HCR = build_hcr(HCR = "NPFMC",   # NPFMC Tier 3 HCR
                                                Ftarget = 0.4,   # F40% = max FABC
                                                Flimit = 0.35,   # F35% = FOFL
                                                Plimit = 0.2,    # no fishing when SSB < B20
                                                Alpha = 0.05),
                                initMode = "NonEquilibrium",  # equilibrium + init devs
                                M1Fun = M1Fun,
                                fit_control = ctl)


# Plot models ----
plot_biomass(list(ebs_em, ebs_om), incl_proj = TRUE, model_names = c("EM", "OM"))
plot_catch(list(ebs_em, ebs_om), incl_proj = TRUE, model_names = c("EM", "OM"))


# Run MSEs ----
# * MSE 1 ----
# - No stochasticity in future observations (observation error) and recruitment (number of new fish entering the population)
mse1 <- run_mse(om = ebs_om, em = ebs_em, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = FALSE, sample_rec = FALSE)


# * MSE 2 ----
# - No stochasticity in future observations (observation error), but stochastic recruitment (number of new fish entering the population)
# -- Recruitment stochasticity is treating variation in recruitment in the projection similar to historical estimates
mse2 <- run_mse(om = ebs_om, em = ebs_em, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = FALSE, sample_rec = TRUE)


# * MSE 3 ----
# - Add stochasticity in future observations (observation error) and recruitment (number of new fish entering the population)
# -- Recruitment stochasticity is treating variation in recruitment in the projection similar to historical estimates
# -- Observation error adds random variation to the data following the assumed probability distribution and historical variance
mse3 <- run_mse(om = ebs_om, em = ebs_em, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = TRUE, sample_rec = TRUE)


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
