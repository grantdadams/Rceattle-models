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
ms_run <- fit_mod(data_list = BS2017MS,
                            inits = ss_run$estimated_params, # Initial parameters from single species ests
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            niter = 3, # 3 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # MSVPA based
                            suitMode = 0, # empirical suitability
                            verbose = 1)

# - Check M1
ms_run$quantities$M1[1,1,]



# - Single-species update
# -- Update the single species M to the mean M-at-age from the multi-species model for pollock
BS2017SS_update <- BS2017SS
BS2017SS_update$M1_base[1, 3:(12+2)] <- apply(ms_run$quantities$M[1,1,1:12,1:39],1, mean) # Set M to mean m-at-age from multispecies model for pollock
ss_run_update <- Rceattle::fit_mod(data_list = BS2017SS_update,
                            inits = NULL, # Initial parameters = default. NOTE.
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            phase = "default",
                            verbose = 1)



# We can plot both runs as well:
mod_list <- list(ss_run, ms_run, ss_run_update)
mod_names <- c("Single-species", "Multi-species","Single-species updated")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)

plot_selectivity(Rceattle = mod_list, model_names = mod_names)
plot_mortality(Rceattle = mod_list[[3]], type = 3)
