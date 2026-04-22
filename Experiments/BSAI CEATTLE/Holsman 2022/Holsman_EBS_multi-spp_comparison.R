library(Rceattle)

################################################
# Data
################################################
# Example
# To run the 2017 single species assessment for the Bering Sea, a data file must first be loaded:
data("BS2017SS") # ?BS2017SS for more information on the data
data("BS2017MS") # Note: the only difference is the residual mortality (M1_base) is lower


################################################
# Estimation
################################################
# - Single species
ss_run <- Rceattle::fit_mod(data_list = BS2017SS,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            phase = "default",
                            verbose = 1)


# - Multi species
# -- Intialized from single-species MLE parameter values
ms_run <- Rceattle::fit_mod(data_list = BS2017MS,
                            inits = ss_run$estimated_params, # Initial parameters from single species ests
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            niter = 3, # 3 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # MSVPA based
                            suitMode = 0, # empirical suitability
                            verbose = 1)

# -- Check M2
ms_run$data_list$M1_base # Check input M1
ms_run$data_list$est_M1 # Set to 0 to fix M1 at input M1
apply(ms_run$quantities$M2[1,1,1:12,1:39],1, mean) # check mean M2 - decreases with age
apply(ms_run$quantities$M[1,1,1:12,1:39],1, mean) # Check mean M - decreases with age for the most part
# - No consumption on age 10 either so M is set to M1 (I dont have the M1 = M1 + 0.0001 in TMB)


# -- Phased in from default initial parameter values
ms_run_phased <- Rceattle::fit_mod(data_list = BS2017MS,
                            inits = NULL, # Initial parameters from single species ests
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            niter = 3, # 3 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # MSVPA based
                            suitMode = 0, # empirical suitability
                            phase = "default",
                            verbose = 1)

# -- Check M2
ms_run_phased$data_list$M1_base # Check input M1
ms_run_phased$data_list$est_M1 # Set to 0 to fix M1 at input M1
apply(ms_run_phased$quantities$M2[1,1,1:12,1:39],1, mean) # check mean M2 - decreases with age
apply(ms_run_phased$quantities$M[1,1,1:12,1:39],1, mean) # Check mean M - decreases with age for the most part
# - No consumption on age 10 either so M is set to M1 (I dont have the M1 = M1 + 0.0001 in TMB)


# - Kirstin's multi-species model
mydata_ms <- Rceattle::read_data( file = "BS2017MS_KH.xlsx")
ms_run_kh    <- Rceattle::fit_mod(data_list = mydata_ms,
                               inits = ss_run$estimated_params, # Initialize from single species model
                               file = NULL, # Don't save
                               estimateMode = 0, # Estimate
                               niter = 3, # 3 iterations around population and predation dynamics
                               random_rec = FALSE, # No random recruitment
                               msmMode = 1, # MSVPA based
                               suitMode = 0, # empirical suitability
                               verbose = 1)

ms_run_kh$data_list$M1_base # Check input M1
ms_run_kh$data_list$est_M1 # Set to 0 to fix M1 at input M1
apply(ms_run_kh$quantities$M2[1,1,1:12,1:39],1, mean) # check mean M2 - decreases with age
round(apply(ms_run_kh$quantities$M[1,1,1:12,1:39],1, mean),4) # Check mean M - decreases with age for the most part
# - No consumption on age 10 either so M is set to M1 (I dont have the M1 = M1 + 0.0001 in TMB)


# We can plot both runs as well:
mod_list <- list(ss_run, ms_run, ms_run_phased, ms_run_kh)
mod_names <- c("Single-species", "Multi-species","Multi-species phased", "Multi-species KH")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)

plot_selectivity(Rceattle = mod_list, model_names = mod_names)
plot_mortality(Rceattle = mod_list[[3]], type = 3)
