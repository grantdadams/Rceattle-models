# Install Rceattle
devtools::install_github("grantdadams/Rceattle", auth_token = "4925b42ac46f1e0aefd671e9dc0c1cf1b3157017")

# Example
# To run the 2017 single species assessment for the Bering Sea, a data file must first be loaded:
library(Rceattle)
data(BS2017SS) # ?BS2017SS for more information on the data 

# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
ss_run <- Rceattle(data_list = BS2017SS,
                   inits = NULL, # Initial parameters = 0
                   file_name = NULL, # Don't save
                   debug = 0, # Estimate
                   random_rec = FALSE, # No random recruitment
                   msmMode = 0, # Single species mode
                   avgnMode = 0,
                   silent = TRUE)

# The you can plot the model results using using
plot_biomass(ceattle_list =  list(ss_run))
plot_recruitment(ceattle_list =  list(ss_run))



# For the 2017 multispecies model starting from the single species parameters, the following can be specified:
data(BS2017MS) # ?BS2017MS for more information on the data 

ms_run <- Rceattle(data_list = BS2017MS,
                   inits = ss_run$estimated_params, # Initial parameters  from single species
                   file_name = NULL, # Don't save
                   debug = 0, # Estimate
                   random_rec = FALSE, # No random recruitment
                   msmMode = 1, # Holsman et al empirical suitability
                   avgnMode = 0,
                   silent = TRUE)


# We can plot both runs as well:
plot_biomass(ceattle_list =  list(ms_run, ss_run), model_names = c("MS", "SS"))
plot_recruitment(ceattle_list =  list(ms_run, ss_run), model_names = c("MS", "SS"))


# Data can be simulated from the estimated quantities using `sim_mod`:
ss_sim <- sim_mod(ss_run)

ss_sim_run <- Rceattle(
  data_list = ss_sim,
  inits = NULL, # Initial parameters = 0
  file_name = NULL, # Don't save
  debug = 0, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  avgnMode = 0,
  silent = FALSE)

ms_sim <- sim_mod(ms_run)

ms_sim_run <- Rceattle(
  data_list = ms_sim,
  inits = NULL, # Initial parameters = 0
  file_name = NULL, # Don't save
  debug = 0, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # Holsman MS mode
  avgnMode = 0,
  silent = FALSE)



# For recruitment, the model assumes that recruitment $R$ of species $sp$ follows the following equation:
ss_re <- Rceattle(
  data_list = ss_sim,
  inits = NULL, # Initial parameters = 0
  file_name = NULL, # Don't save
  debug = 0, # Estimate
  random_rec = TRUE, # No random recruitment
  msmMode = 0, # Single species mode
  avgnMode = 0,
  silent = TRUE)
