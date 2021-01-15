library(Rceattle)
library(readxl)
setwd("Model runs/GOA_18.5.1/")

################################################
# Data
################################################
# Read the data in
mydata_pcod_fixed <- Rceattle::read_data( file = "Data/GOA_18.5.1_pcod_single_species_1977-2018.xlsx")
mydata_pcod_fixed$pmature[1,2:13] <- 2

# Pcod_bt_survey - dome - timevarying (but logistic in recent years)
# Pcod_ll_survey - dome
# Pcod_trawl_fishery - logistic - timevarying
# Pcod_longline_fishery - logistic - timevarying
# Pcod_pot_fishery - dome - timevarying



#######################################
# Mod 0 - Estimate
#######################################
mydata_pcod_est <- mydata_pcod_fixed
mydata_pcod_est$msmMode = 0

dat <- rearrange_dat(mydata_pcod_est)
params <- build_params(mydata_pcod_est)
map <- build_map(data_list = mydata_pcod_est, params = params)

pcod_base <- Rceattle::fit_mod(
  # cpp_directory = "C:/Users/Grant Adams/Documents/GitHub/Rceattle/inst/executables",
  data_list = mydata_pcod_est,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  debug = FALSE, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  silent = FALSE,
  recompile = FALSE,
  phase = "default")

pcod_base <- Rceattle::fit_mod(
  # cpp_directory = "C:/Users/Grant Adams/Documents/GitHub/Rceattle/inst/executables",
  data_list = mydata_pcod_est,
  inits = pcod_base$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  debug = FALSE, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  silent = TRUE,
  recompile = FALSE,
  phase = "default")


####################
# Fix selectivity
####################
mydata_pcod_fix_sel <- mydata_pcod_est
mydata_pcod_fix_sel$fleet_control$Selectivity = 0
mydata_pcod_fix_sel$fleet_control$Nselages = 0
mydata_pcod_fix_sel$fleet_control$Time_varying_sel = 0
mydata_pcod_fix_sel$fleet_control$Sel_sd_prior = 0


pcod_fix_sel <- Rceattle::fit_mod(
  # cpp_directory = "C:/Users/Grant Adams/Documents/GitHub/Rceattle/inst/executables",
  data_list = mydata_pcod_fix_sel,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  debug = FALSE, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  silent = TRUE,
  recompile = FALSE,
  phase = "default")



####################
# Fix natage
####################
mydata_pcod_fix <- mydata_pcod_est
mydata_pcod_fix$estDynamics[1] <- 1



pcod_fix <- Rceattle::fit_mod(
  # cpp_directory = "C:/Users/Grant Adams/Documents/GitHub/Rceattle/inst/executables",
  data_list = mydata_pcod_fix,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  debug = TRUE, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  silent = TRUE,
  recompile = FALSE,
  phase = "default")


######################### 
# SAFE Models
#########################

library(readxl)
safe2018biomass <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 1))
safe2018ssb <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 2))
safe2018rec <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 3))

pcod_safe <- pcod_base
pcod_safe$quantities$biomass[1,1:42] <- t(safe2018biomass[1:42,3])
pcod_safe$quantities$biomassSSB[1,1:42] <- t(safe2018ssb[1:42,3])
pcod_safe$quantities$R[1,1:42] <- t(safe2018rec[1:42,3])


# 18.3.2
# load("C:/Users/Grant Adams/Documents/GitHub/RceattleRuns/GOA/Model runs/GOA_18.3.2/Models/18_3_12020-10-25.RData")
# pcod_18_3_2 <- pcod_base
# pcod_18_3_2$quantities$biomass[1,1:42] <- mod_list_all[[1]]$quantities$biomass[2,1:42]
# pcod_18_3_2$quantities$biomassSSB[1,1:42] <- mod_list_all[[1]]$quantities$biomassSSB[2,1:42]
# pcod_18_3_2$quantities$R[1,1:42] <- mod_list_all[[1]]$quantities$R[2,1:42]


######################### 
# Plots
#########################

# - SAFE vs SS
file_name <- "Data/Cod tests/18.5.1_SAFE_vs_ceattle_pcod"
mod_list <- list(pcod_base, pcod_fix, pcod_safe)
mod_names <- c( "CEATTLE est","CEATTLE fixed natage","2018 SAFE (mt)")

plot_biomass(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
# plot_ssb(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
