# Grant Adams, Kirstin Holsman, Andre Punt - May 2019
library(Rceattle)

################################################
# Bullet 1 - Estimation
################################################
# Read the data in
data <- Rceattle::read_data(file = "Data/Adriatic_base.xlsx")


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
                            inits = inits, # Initial parameters = 0
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


################################################
# Bullet 2 - Sensitivity to constant temperature
################################################

# Create copy of data and set temperature to constant temperature
data_constant_T <- data
data_constant_T$BTempC <- replace(data_constant_T$BTempC, 
                                           values = 5)
# WARNING: if you make "BTempC" a single value, it will not work.

# Run
ms_run_constant_T <- Rceattle::fit_mod(data_list = data_constant_T,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate. Set to 1 to not estimate.
                            msmMode = 1, # Multi-species mode
                            niter = 3, # Number of iterations around predation/population dynamics
                            silent = TRUE,
                            recompile = FALSE)


# Plot both runs as well:
mod_list <- list(ms_run, ms_run_constant_T)
mod_names <- c("Multi-spp", "Multi-spp constant Temp")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names, species = species_names)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, add_ci = TRUE, species = species_names)

plot_selectivity(Rceattle = mod_list, model_names = mod_names)
plot_mort(Rceattle = mod_list, model_names = mod_names, age = 4)



################################################
# Bullet 3 - Sensitivity to diet data
################################################
# Get data files and create objects to store models
file_list <- c("Adriatic_wA1.xlsx", # Adriatic 1
               "Adriatic_wA2.xlsx", # Adriatic 2
               "Adriatic_wS1.xlsx", # Strait of Sicily 1
               "Adriatic_wS2.xlsx", # Strait of Sicily 2
               "Adriatic_wS3.xlsx", # Strait of Sicily 3
               "Adriatic_wS4.xlsx", # Strait of Sicily 4
               "Adriatic_wT1.xlsx", # central Tyrrhenian 1
               "Adriatic_wT2.xlsx"  # central Tyrrhenian 2
               )
nmods <- length(file_list)
diet_mods <- list()

# Loop through diet sensitivity data set and estimate models
for(i in 1:nmods){
  sensitivitiy_data <- Rceattle::read_data(file = paste0("Data/", file_list[i]))
  
  
  # Run the model in single species mode to get pass the estimated parameters as initial values for the multi-species mode
  diet_mods[[i]] <- Rceattle::fit_mod(data_list = sensitivitiy_data,
                              inits = ss_run$estimated_params, # Initial parameters = 0
                              file = NULL, # Don't save
                              debug = 0, # Estimate. Set to 1 to not estimate.
                              msmMode = 1, # Multi-species mode
                              silent = TRUE,
                              recompile = FALSE,
                              niter = 3)
}


# Plot it 
mod_list <- c(list(ss_run, ms_run), diet_mods)
mod_names <- c("Single-spp", "Multi-spp Base" , "A1", "A2", "S1", "S2", "S3", "S4", "T1", "T2")

plot_biomass(Rceattle = mod_list, model_names = mod_names, species = species_names)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, add_ci = FALSE, species = species_names)


################################################
# Bullet 4 - Sensitivity to M1
################################################

# 1. Constant natural mortality

# Create a new data set and adjust mortality
data_constant_M1 <- data
data_constant_M1$M1_base <- replace(data_constant_M1$M1_base, 
                                    values = rep(0.2, length(data_constant_M1$M1_base))) # Change everything to 0.2


# Run the model in multi-species mode
ms_run_constant_mort <- Rceattle::fit_mod(data_list = data_constant_M1,
                                          inits = ss_run$estimated_params, # Initial parameters = 0
                                          file = NULL, # Don't save
                                          debug = 0, # Estimate. Set to 1 to not estimate.
                                          msmMode = 1, # Multi-species mode
                                          silent = TRUE,
                                          recompile = FALSE,
                                          niter = 3)

# Compare spawning stock biomass and recruitment
mod_list <- list(ms_run, ms_run_constant_mort)
mod_names <- c("Multi-spp base", "Multi-spp constant M1")

plot_biomass(Rceattle =  mod_list, species = species_names, model_names = mod_names)
plot_recruitment(Rceattle =  mod_list, species = species_names, model_names = mod_names)



# 2. Double natural mortality

# Create a new data set and adjust mortality
data_double_mort <- data
data_double_mort$M1_base <- data_double_mort$M1_base * 2


# Run the model in single species mode
ms_run_double_mort <- Rceattle::fit_mod(data_list = data_double_mort,
                                        inits = ss_run$estimated_params, # Initial parameters = 0
                                        file = NULL, # Don't save
                                        debug = 0, # Estimate. Set to 1 to not estimate.
                                        msmMode = 1, # Multi-species mode
                                        silent = TRUE,
                                        recompile = FALSE,
                                        niter = 3)

# Compare spawning stock biomass and recruitment
mod_list <- list(ms_run, ms_run_double_mort)
mod_names <- c("Multi-spp base", "Multi-spp double M1")

plot_biomass(Rceattle =  mod_list, species = species_names, model_names = mod_names)
plot_recruitment(Rceattle =  mod_list, species = species_names, model_names = mod_names)


########################################################
# Bullet 5 - Compare all single and multi-species models
########################################################

# Plot it 
mod_list <- list(ss_run, ms_run, ms_run_constant_mort, ms_run_double_mort, ms_run_constant_T)
mod_list <- c(mod_list, diet_mods) # Diet mods is already a list, so we need to combine lists with c
mod_names <- c("Single-spp", "Multi-spp base" , "Multi-spp constant M1", "Multi-spp double M1", "Multi-spp constant T", "A1", "A2", "S1", "S2", "S3", "S4", "T1", "T2")

plot_biomass(Rceattle = mod_list, model_names = mod_names, species = species_names)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, add_ci = FALSE, species = species_names)

plot_selectivity(Rceattle = mod_list, model_names = mod_names, species = species_names)
