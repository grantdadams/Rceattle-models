library(Rceattle)
library(readxl)

################################################
# Data
################################################
# Read the data in
mydata_atf <- Rceattle::read_data( file = "CEATTLE_arrowtooth_single_species_1961-2018.xlsx")
# Fishery - Non-parametric selectivity
# Survey 1 (Bottom Trawl) - Logistic selectivity, q = 1


#######################################
# Mod 1 - Estimate using CEATTLE
#######################################
mydata_atf_est <- mydata_atf
mydata_atf_est$estDynamics = 0 # Estimate dynamics

atf_ceattle <- Rceattle::fit_mod(
  data_list = mydata_atf_est,
  inits = NULL, # Initial parameters = 0/default
  file = NULL, # Don't save
  debug = FALSE, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  silent = TRUE,
  recompile = FALSE,
  phase = "default")


######################### 
# Mod 2 - SAFE Model
#########################
library(readxl)
safe2018biomass <- as.data.frame(read_xlsx("2018_SAFE_atf_estimates.xlsx", sheet = 1))
safe2018ssb <- as.data.frame(read_xlsx("2018_SAFE_atf_estimates.xlsx", sheet = 2))
safe2018rec <- as.data.frame(read_xlsx("2018_SAFE_atf_estimates.xlsx", sheet = 3))

atf_safe <- atf_ceattle
atf_safe$quantities$biomass[1,1:57] <- t(safe2018biomass[,2])
atf_safe$quantities$biomassSSB[1,1:57] <- t(safe2018ssb[,2])
atf_safe$quantities$R[1,1:57] <- t(safe2018rec[,2])


######################### 
# Plots
#########################
# - SAFE vs SS
mod_list <- c(list(atf_ceattle, atf_safe))
mod_names <- c( "CEATTLE est", "2017 SAFE (mt)")
for(i in 1:length(mod_list)){
  mod_list[[i]]$data_list$endyr = 2017
}


plot_biomass(mod_list, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
plot_ssb(mod_list, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
plot_recruitment(mod_list, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)