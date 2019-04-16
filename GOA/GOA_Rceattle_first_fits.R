
library(Rceattle)

################################################
# Data
################################################
# Read the data in
mydata <- Rceattle::read_excel( file = "C:/Users/Grant Adams/Documents/GitHub/RceattleRuns/GOA/GOA2017SS_v3_from_1977_v2.xlsx")


################################################
# Estimation
################################################
# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
ss_run <- Rceattle::fit_mod(data_list = mydata,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 1, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE)
ss_run$quantities$jnll_comp

inits <- build_params(mydata)
inits$ln_mn_rec <- c(7,7,7)

ss_run <- Rceattle::fit_mod(data_list = mydata,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = FALSE)
ss_run$quantities$jnll_comp

library(TMBhelper)
identified <- suppressMessages(TMBhelper::Check_Identifiable(ss_run$obj))

# Make into list
identified_param_list <- obj$env$parList(as.numeric(identified$BadParams$Param_check))
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==0,"Not estimated",x), how = "replace")
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==1,"OK",x), how = "replace")
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==2,"BAD",x), how = "replace")

# The you can plot the model results using using
plot_biomass(Rceattle =  ss_run)
plot_recruitment(Rceattle =  ss_run)


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
mod_names <- c("SS", "MS")

# Plot biomass trajectory
plot_biomass(Rceattle = mod_list, model_names = mod_names, incl_proj = TRUE)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, incl_proj = TRUE, add_ci = TRUE)

plot_selectivity(Rceattle = mod_list, model_names = mod_names)
plot_mort(Rceattle = mod_list, model_names = mod_names, age = 2)


################################################
# Simulation
################################################
# Data can be simulated from the estimated quantities using `sim_mod`:
ss_sim <- sim_mod(ss_run)

ss_sim_run <- Rceattle::fit_mod(
  data_list = ss_sim,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  debug = 0, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  silent = TRUE)

# Simulate the multispecies model
ms_sim <- sim_mod(ms_run)

ms_sim_run <- Rceattle::fit_mod(
  data_list = ms_sim,
  inits = ss_run$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  debug = 0, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # Holsman MS mode
  silent = TRUE)

# We can plot the simulated runs
mod_list <- list(ss_sim_run, ms_sim_run)
mod_names <- c("SS sim", "MS sim")

plot_biomass(Rceattle = mod_list, model_names = mod_names)
plot_recruitment(Rceattle = mod_list, model_names = mod_names)



################################################
# Model variants
################################################

# For recruitment, the model can estimate recruitment deviates as random effects
ss_re <- Rceattle::fit_mod(
  data_list = mydata,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  debug = 0, # Estimate
  random_rec = TRUE, # Turn of recruitment deviations as random effects
  msmMode = 0, # Single species mode
  silent = TRUE)

# Diet estimation
ms_gamma <- Rceattle::fit_mod(
  data_list = mydata,
  inits = ss_run$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  debug = 0, # Estimate
  niter = 10, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 1, # Have a gamma function with time-independent length ratio for suitability. Includes diet in likelihood as multinomial
  silent = TRUE)
# Can try different functions. Look at ?fit_mod suitmode


################################################
# Included files
################################################
# If we want to extract the cpp file
cpp_directory <- system.file("executables",package="Rceattle")
TMBfilename <- "ceattle_v01_04"
cpp_file <- paste0(cpp_directory, "/", TMBfilename, ".cpp")
cpp_file <- file(cpp_file)
cpp_file <- readLines(cpp_file)

