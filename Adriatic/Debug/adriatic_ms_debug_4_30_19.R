
library(Rceattle)

################################################
# Data
################################################
# Example
# To run the 2017 single species assessment for the Bering Sea, a data file must first be loaded:
# data(BS2017SS) # ?BS2017SS for more information on the data
# Write data to excel
# Rceattle::write_excel(data_list = BS2017SS, file = "BS2017SS.xlsx")


# Read the data in
adriatic_data <- Rceattle::read_excel(file = "Adriatic_v8.xlsx")


################################################
# Estimation
################################################
inits <- build_params(adriatic_data)
inits$ln_mn_rec <- c(9,9,9)

# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
ss_run <- Rceattle::fit_mod(data_list = adriatic_data,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE,
                            recompile = TRUE)
# Type ?fit_mod for more details



ms_run10 <- Rceattle::fit_mod(data_list = adriatic_data,
                            inits = ss_run$estimated_params, # Initial parameters from single species ests
                            file = NULL, # Don't save
                            debug = 0, # Do not estimate. Set to zero to estimate.
                            niter = 10, # 10 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # MSVPA based
                            suitMode = 0, # empirical suitability
                            silent = FALSE,
                            minNByage = 0.01)




ms_run4 <- Rceattle::fit_mod(data_list = adriatic_data,
                             inits = ss_run$estimated_params, # Initial parameters from single species ests
                             file = NULL, # Don't save
                             debug = 0, # Do not estimate. Set to zero to estimate.
                             niter = 4, # 10 iterations around population and predation dynamics
                             random_rec = FALSE, # No random recruitment
                             msmMode = 1, # MSVPA based
                             suitMode = 0, # empirical suitability
                             silent = FALSE)


 ms_run4bound <- Rceattle::fit_mod(data_list = adriatic_data,
                             inits = ss_run$estimated_params, # Initial parameters from single species ests
                             file = NULL, # Don't save
                             debug = 0, # Do not estimate. Set to zero to estimate.
                             niter = 4, # 10 iterations around population and predation dynamics
                             random_rec = FALSE, # No random recruitment
                             msmMode = 1, # MSVPA based
                             suitMode = 0, # empirical suitability
                             silent = FALSE,
                             minNByage = 0.01)


# plot
mod_list <- list(ss_run, ms_run4, ms_run4bound, ms_run10)
mod_names <- c("SS", "MS 4 iter", "MS 4 iter with bound", "MS 10 iter with bound")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names, file = "test")
plot_recruitment(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE, file = "test")
plot_index(mod_list, model_names = mod_names, file = "test")
plot_catch(mod_list, model_names = mod_names, file = "test")
plot_selectivity(mod_list, model_names = mod_names, file = "test")
