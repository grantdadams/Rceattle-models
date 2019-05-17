# Grant Adams, Kirstin Holsman, Andre Punt - May 2019
library(Rceattle)

################################################
# Bullet 1 - Estimation
################################################
# Read the data in
adriatic_data <- Rceattle::read_data(file = "Adriatic_v9.xlsx")


# Run the model in single species mode
ss_run <- Rceattle::fit_mod(data_list = adriatic_data,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single-species mode
                            silent = TRUE,
                            recompile = FALSE)
# Type ?fit_mod for more details

# Run the model in multi-species species mode 
# Initialize from single-species parameter estimates
inits <- ss_run$estimated_params

# Run
ms_run <- Rceattle::fit_mod(data_list = adriatic_data,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # Multi-species mode
                            niter = 3, # Number of iterations around predation/population dynamics
                            silent = TRUE,
                            recompile = FALSE)


# Plot both runs as well:
species_names <- c("Hake", "Sardine", "Anchovy")
mod_list <- list(ss_run, ms_run)
mod_names <- c("Single-species", "Multi-species")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names, species = species_names)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE, species = species_names)

plot_selectivity(Rceattle = mod_list, model_names = mod_names)
plot_mort(Rceattle = mod_list, model_names = mod_names, age = 2)


################################################
# Bullet 2 - Sensitivity to constant temperature
################################################

# Create copy of data and set temperature to constant temperature
adriatic_data_constant_T <- adriatic_data
adriatic_data_constant_T$BTempC <- replace(adriatic_data_constant_T$BTempC, 
                                           values = 5)
# WARNING: if you make "BTempC" a single value, it will not work.

# Run
ms_run_constant_T <- Rceattle::fit_mod(data_list = adriatic_data_constant_T,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # Multi-species mode
                            niter = 3, # Number of iterations around predation/population dynamics
                            silent = TRUE,
                            recompile = FALSE)


# Plot both runs as well:
species_names <- c("Hake", "Sardine", "Anchovy")
mod_list <- list(ms_run, ms_run_constant_T)
mod_names <- c("Multi-species", "Multi-species constant Temp")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names, species = species_names)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE, species = species_names)

plot_selectivity(Rceattle = mod_list, model_names = mod_names)
plot_mort(Rceattle = mod_list, model_names = mod_names, age = 4)



################################################
# Bullet 3 - Sensitivity to diet data - TBD
################################################



################################################
# Bullet 4 - Sensitivity to M1 - TBD
################################################
