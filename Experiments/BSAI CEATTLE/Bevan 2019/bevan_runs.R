
library(Rceattle)

################################################
# Data
################################################
# Example
# To run the 2017 single species assessment for the Bering Sea, a data file must first be loaded:
data(BS2017SS) # ?BS2017SS for more information on the data


################################################
# Estimation
################################################
# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
ss_run <- Rceattle::fit_mod(data_list = BS2017SS,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE)
# Type ?fit_mod for more details

file_name <- c("BSAI/Bevan 2019/Figures/ss_runs")

library(oce)
line_cols <- oceColorsViridis(3)

# The you can plot the model results using using
plot_biomass(Rceattle =  ss_run, model_names = "Single-species no fishing", file = file_name, incl_proj = TRUE, lwd = 5, line_col = line_cols[1])
plot_recruitment(Rceattle =  ss_run, incl_proj = T)


# For the a multispecies model starting from the single species parameters, the following can be specified to load the data:
data("BS2017MS") # Note: the only difference is the residual mortality is lower
# Or we can use the previous data set

ms_run <- Rceattle::fit_mod(data_list = BS2017MS,
                            inits = ss_run$estimated_params, # Initial parameters from single species ests
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            niter = 10, # 10 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # MSVPA based
                            suitMode = 0, # empirical suitability
                            silent = TRUE)


# We can plot both runs as well:
mod_list <- list(ss_run, ms_run)
mod_names <- c("Single-species no fishing", "Multi-species no fishing")

file_name <- c("BSAI/Bevan 2019/Figures/ms_runs_no_proj")
# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names, file = file_name, incl_proj = TRUE, lwd = 5, line_col = line_cols[1:2])


################################################
# Projection
################################################
# Rceattle automatically projects the population forward when estimating
# To check to see what the F rates are for each fishery we can check:
BS2017MS$fsh_control$proj_F
BS2017MS$projyr # Year the population is projected forward

# We can then change the F
BS2017MS$fsh_control$proj_F <- c(0.2342936, 0.513, 0.0774777)

# Re-run, without estimating
ms_run_proj <- Rceattle::fit_mod(data_list = BS2017MS,
                                 inits = ms_run$estimated_params, # Initial parameters from single species ests
                                 file = NULL, # Don't save
                                 debug =TRUE, # Do not estimate
                                 niter = 10, # 10 iterations around population and predation d  ynamics
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 1, # MSVPA based
                                 suitMode = 0, # empirical suitability
                                 silent = TRUE)


# plot
file_name <- c("BSAI/Bevan 2019/Figures/ms_runs")
mod_list <- list(ss_run, ms_run, ms_run_proj)
mod_names <- c("Single-species no fishing", "Multi-species no fishing", "Multi-species mean historical fishing rate")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names, incl_proj = TRUE, file = file_name, lwd = 5, line_col = line_cols[1:3])
