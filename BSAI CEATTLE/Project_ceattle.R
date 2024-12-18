# Install Rceattle
devtools::install_github("grantdadams/Rceattle", auth_token = "4925b42ac46f1e0aefd671e9dc0c1cf1b3157017")

# 2 options to project
# 1 edit estimated parameter list
# 2 edit initial parameter list using build_param


library(Rceattle)
data(BS2017SS)
data(BS2017MS)

############################
# Option 1
############################
# Run single species
ss_run <- Rceattle(data_list = BS2017SS,
                   inits = NULL, # Initial parameters = 0
                   file_name = NULL, # Don't save
                   debug = 0, # Estimate
                   random_rec = FALSE, # No random recruitment
                   msmMode = 0, # Single species mode
                   avgnMode = 0,
                   silent = TRUE)
# Run multi-species
ms_run <- Rceattle(data_list = BS2017MS,
                   inits = ss_run$estimated_params, # Initial parameters from SS mode
                   file_name = NULL, # Don't save
                   debug = 0, # Estimate
                   random_rec = FALSE, # No random recruitment
                   suitMode = 0, # Empirical suitability
                   msmMode = 1, # MSVPA based predation
                   avgnMode = 0,
                   silent = TRUE)

# Extract estimated parameter list
params <- ms_run$estimated_params

# Change what we want (For example, changing to an estimated Holling Type II with lognormal length-based suitability
sp = 1 # Pollock is species 1
params$phi = replace(params$phi, values = rep(log(0.5), length(params$phi))) # Phi is a nspp by nspp matrix of predator/prey species preference coefficients. Here I am assuming each predator likes everything equally. Predator species is the row, prey species is the column.
params$log_gam_a <- rep(log(0.5), length(params$log_gam_a)) # Change sd of lognormal suitability
params$log_gam_b <- rep(log(2), length(params$log_gam_a)) # Change ideal log predator/prey length ratio

# Project by setting debug = 1 to not estimate
ms_proj <- Rceattle(data_list = BS2017MS,
                   inits = params, # Use altered parameters
                   file_name = NULL, # Don't save
                   debug = 1, # Do not estimate
                   random_rec = FALSE, # No random recruitment
                   suitMode = 2, # Length based lognormal suitability
                   msmMode = 1, # MSVPA based predation
                   avgnMode = 0,
                   silent = TRUE)

plot_biomass(Rceattle = list(ms_run, ms_proj), model_names = c("MS", "Type 2"))

# Extract data
new_data <- sim_mod(ms_proj, expected = TRUE) # Get expected survey biomass, survey catch-at-age/length, fishery catch, fishery ctach-at-age/length, and observerved stomach proportion by weight
