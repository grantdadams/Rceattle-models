# Grant Adams, Kirstin Holsman, Andre Punt - May 2019
library(Rceattle)

################################################
# Bullet 3 - Estimation
################################################
# Read the data in
data <- Rceattle::read_data(file = "Data/Adriatic_base.xlsx")


# Run the model in single species mode
ss_run <- Rceattle::fit_mod(data_list = data,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            msmMode = 0, # Single species mode
                            silent = TRUE,
                            recompile = FALSE)
# Type ?fit_mod for more details


# Extract the spawning stock biomass and recruitment series
ssb <- ss_run$quantities$biomassSSB
recruitment <- ss_run$quantities$R


# Store the results in a spreadsheet
write_results(ss_run, file = "Base_single_species_results.xlsx") # Look at sheeps "SSB" and "Recruitment"
# you can also do
write.csv(ssb, file = "Base_single_species_ssb.csv")
write.csv(recruitment, file = "Base_single_species_recruitment.csv")


# View the diagnostic plots
# 1. Index and catch fits
plot_catch(Rceattle =  ss_run)
plot_index(Rceattle =  ss_run)

# 2. Comps
plot_srv_comp(Rceattle =  ss_run) # Black like is estimate, grey is observed
plot_fsh_comp(Rceattle =  ss_run) # Black like is estimate, grey is observed

# 3. Selectivity
plot_selectivity(ss_run)

# 4. Time series 
species_names <- c("Hake", "Sardine", "Anchovy")
plot_biomass(Rceattle =  ss_run, species = species_names)
plot_recruitment(Rceattle =  ss_run, species = species_names)


################################################
# Bullet 4 - Sensitivity to natural mortality
################################################

# 1. Halve natural mortality

# Create a new data set and adjust mortality
data_half_mort <- data
data_half_mort$M1_base <- data_half_mort$M1_base * 0.5


# Run the model in single species mode
ss_run_half_mort <- Rceattle::fit_mod(data_list = data_half_mort,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            msmMode = 0, # Single species mode
                            silent = TRUE,
                            recompile = FALSE)

# Compare spawning stock biomass and recruitment
species_names <- c("Hake", "Sardine", "Anchovy")
mod_list <- list(ss_run, ss_run_half_mort)
mod_names <- c("Base", "Half M1")

plot_biomass(Rceattle =  mod_list, species = species_names, model_names = mod_names)
plot_recruitment(Rceattle =  mod_list, species = species_names, model_names = mod_names)



# 2. Double natural mortality

# Create a new data set and adjust mortality
data_double_mort <- data
data_double_mort$M1_base <- data_double_mort$M1_base * 2


# Run the model in single species mode
ss_run_double_mort <- Rceattle::fit_mod(data_list = data_double_mort,
                                      inits = NULL, # Initial parameters = 0
                                      file = NULL, # Don't save
                                      debug = 0, # Estimate. Set to 1 to not estimate.
                                      msmMode = 0, # Single species mode
                                      silent = TRUE,
                                      recompile = FALSE)

# Compare spawning stock biomass and recruitment
species_names <- c("Hake", "Sardine", "Anchovy")
mod_list <- list(ss_run, ss_run_double_mort)
mod_names <- c("Base", "Double M1")

plot_biomass(Rceattle =  mod_list, species = species_names, model_names = mod_names)
plot_recruitment(Rceattle =  mod_list, species = species_names, model_names = mod_names)


# Compare all models
# Compare spawning stock biomass and recruitment
species_names <- c("Hake", "Sardine", "Anchovy")
mod_list <- list(ss_run, ss_run_half_mort, ss_run_double_mort)
mod_names <- c("Base", "Half M1", "Double M1")

plot_biomass(Rceattle =  mod_list, species = species_names, model_names = mod_names)
plot_recruitment(Rceattle =  mod_list, species = species_names, model_names = mod_names)


################################################
# Bullet 5 - Sensitivity to data weighting
################################################

# 1. Halve survey and fishery composition data weights

# Create a new data set and adjust composition data weights
data_half_weight <- data
data_half_weight$srv_comp$Sample_size <- data_half_weight$srv_comp$Sample_size * 0.5
data_half_weight$fsh_comp$Sample_size <- data_half_weight$fsh_comp$Sample_size * 0.5

# Run the model in single species mode
ss_run_half_weight <- Rceattle::fit_mod(data_list = data_half_weight,
                                      inits = NULL, # Initial parameters = 0
                                      file = NULL, # Don't save
                                      debug = 0, # Estimate. Set to 1 to not estimate.
                                      msmMode = 0, # Single species mode
                                      silent = TRUE,
                                      recompile = FALSE)

# Compare spawning stock biomass and recruitment
species_names <- c("Hake", "Sardine", "Anchovy")
mod_list <- list(ss_run, ss_run_half_weight)
mod_names <- c("Base", "Half weight")

plot_biomass(Rceattle =  mod_list, species = species_names, model_names = mod_names)
plot_recruitment(Rceattle =  mod_list, species = species_names, model_names = mod_names)



# 2. Double survey and fishery composition data weights

# Create a new data set and adjust composition data weights
data_double_weight <- data
data_double_weight$srv_comp$Sample_size <- data_double_weight$srv_comp$Sample_size * 2
data_double_weight$fsh_comp$Sample_size <- data_double_weight$fsh_comp$Sample_size * 2

# Run the model in single species mode
ss_run_double_weight <- Rceattle::fit_mod(data_list = data_double_weight,
                                        inits = NULL, # Initial parameters = 0
                                        file = NULL, # Don't save
                                        debug = 0, # Estimate. Set to 1 to not estimate.
                                        msmMode = 0, # Single species mode
                                        silent = TRUE,
                                        recompile = FALSE)

# Compare spawning stock biomass and recruitment
species_names <- c("Hake", "Sardine", "Anchovy")
mod_list <- list(ss_run, ss_run_double_weight)
mod_names <- c("Base", "Double weight")

plot_biomass(Rceattle =  mod_list, species = species_names, model_names = mod_names)
plot_recruitment(Rceattle =  mod_list, species = species_names, model_names = mod_names)


# Compare all models
# Compare spawning stock biomass and recruitment
species_names <- c("Hake", "Sardine", "Anchovy")
mod_list <- list(ss_run, ss_run_half_weight, ss_run_double_weight)
mod_names <- c("Base", "Half weight", "Double weight")

plot_biomass(Rceattle =  mod_list, species = species_names, model_names = mod_names)
plot_recruitment(Rceattle =  mod_list, species = species_names, model_names = mod_names)

