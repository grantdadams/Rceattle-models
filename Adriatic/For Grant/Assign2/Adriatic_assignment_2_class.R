# Grant Adams, Kirstin Holsman, Andre Punt - May 2019
library(Rceattle)

################################################
# Bullet 3 - Estimation
################################################
# Read the data in
data <- Rceattle::read_data(file = "Adriatic_SS_base_Sol.xlsx")


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
write_results(ss_run, file = "Base_single_species_results.xlsx") # Look at sheets "SSB" and "Recruitment"

################################################
# Bullet 5 - Sensitivity to natural mortality
################################################


################################################
# Bullet 6 - Sensitivity to data weighting
################################################
