library(Rceattle)
library(readxl)

################################################
# Data
################################################
mydata_hake <- Rceattle::read_data( file = "Data/hake_intrasp_230324_GA.xlsx")

################################################
# Fit initial model
################################################
mydata_hake$est_M1 <- 0
ss_run <- Rceattle::fit_mod(data_list = mydata_hake,
                               inits = NULL, # Initial parameters = 0
                               file = NULL, # Don't save
                               estimateMode = 1,
                               msmMode = 0, # Single species mode
                               phase = "default")

mydata_hake$est_M1 <- 1
ss_run_M <- Rceattle::fit_mod(data_list = mydata_hake,
                                inits = NULL, # Initial parameters = 0
                                file = NULL, # Don't save
                                estimateMode = 1,
                                msmMode = 0, # Single species mode
                                phase = "default")

mydata_hake$est_M1 <- 1
ms_run <- Rceattle::fit_mod(data_list = mydata_hake,
                             inits = ss_run_M$estimated_params, # Initial parameters = 0
                             file = NULL, # Don't save
                             estimateMode = 1,
                             msmMode = 1, # Multi species mode
                             niter = 3,
                             phase = "default")

################################################
# Plotting
################################################
mod_list <- list(ss_run, ss_run_M, ms_run)
mod_names <- c("Single-species", "Single-species estimate M", "Multi-species")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)
plot_depletionSSB(Rceattle = mod_list, model_names = mod_names)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE)

# Plot mortality and predation
plot_b_eaten(Rceattle = mod_list, model_names = mod_names) # Biomass eaten as prey
plot_b_eaten_prop(Rceattle = mod_list, model_names = mod_names) # Biomass eaten as prey by each predator
plot_mort(Rceattle = ms_run, type = 3) # Mortality-at-age time series
