# Grant Adams, Kirstin Holsman, Andre Punt - May 2019
library(Rceattle)

################################################
# Bullet 1 - Estimation
################################################
# Read the data in
data <- Rceattle::read_data(file = "Adriatic_base.xlsx")


# Run the model in single species mode to get pass the estimated parameters as initial values for the multi-species mode
ss_run <- Rceattle::fit_mod(data_list = data,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            msmMode = 0, # Single-species mode
                            silent = TRUE,
                            recompile = FALSE)
# Type ?fit_mod for more details

# Run the model in multi-species species mode 
# Initialize from single-species parameter estimates
inits <- ss_run$estimated_params

# Run the model in multi-species mode
ms_run <- Rceattle::fit_mod(data_list = data,
                            inits = inits, # Initial parameters  from single species model
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            msmMode = 1, # Multi-species mode
                            niter = 3, # Number of iterations around predation/population dynamics
                            silent = TRUE,
                            recompile = FALSE)


# Plot both runs as well:
species_names <- c("Hake", "Sardine", "Anchovy")
mod_list <- list(ss_run, ms_run)
mod_names <- c("Single-spp", "Multi-spp")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names, species = species_names)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE, species = species_names)

plot_selectivity(Rceattle = mod_list, model_names = mod_names)
plot_mort(Rceattle = mod_list, model_names = mod_names, age = 2)

# Store the results in a spreadsheet
write_results(ms_run, file = "Base_multi_species_results.xlsx") # Look at sheeps "SSB" and "Recruitment"

