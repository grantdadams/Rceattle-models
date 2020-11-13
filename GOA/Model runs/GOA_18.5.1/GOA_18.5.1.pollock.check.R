library(Rceattle)
setwd("Model runs/GOA_18.5.1/")

################################################
# Data
################################################
# Read the data in
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_18.5.1_pollock_single_species_1970-2018.xlsx")

pollock_base <- Rceattle::fit_mod(data_list = mydata_pollock,
                  inits = NULL, # Initial parameters = 0
                  file = NULL, # Don't save
                  debug = 0, # Estimate
                  random_rec = FALSE, # No random recruitment
                  msmMode = 0, # Single species mode
                  silent = TRUE,
                  recompile = FALSE,
                  phase = "default")

# Random walk of q - adfg, shelikof, bt, random walk of fishery sel
mydata_pollock_rw <- mydata_pollock
mydata_pollock_rw$fleet_control$Time_varying_q[c(1,2,3,7)] <- 1
mydata_pollock_rw$fleet_control$Q_sd_prior[c(1,2,3,7)] <- 0.05

mydata_pollock_rw$fleet_control$Time_varying_sel[8] <- 1
mydata_pollock_rw$fleet_control$Sel_sd_prior[8] <- 0.05
mydata_pollock_rw$fleet_control$Selectivity[8] <- 3
pollock_rw <- Rceattle::fit_mod(data_list = mydata_pollock_rw,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  debug = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  silent = TRUE,
                                  recompile = FALSE,
                                  phase = "default")

# SAFE Models
library(readxl)
safe2018biomass <- as.data.frame(read_xlsx("Data/2018_SAFE_pollock_estimates.xlsx", sheet = 1))
safe2018ssb <- as.data.frame(read_xlsx("Data/2018_SAFE_pollock_estimates.xlsx", sheet = 2))
safe2018rec <- as.data.frame(read_xlsx("Data/2018_SAFE_pollock_estimates.xlsx", sheet = 3))
pollock_safe <- pollock_base
pollock_safe$quantities$biomass[1,1:49] <- t(safe2018biomass[,2]) * 1000000 * 0.453592
pollock_safe$quantities$biomassSSB[1,1:49] <- t(safe2018ssb[,2])  * 1000000 * 0.453592
pollock_safe$quantities$R[1,1:49] <- t(safe2018rec[,2]) * 1000000


pollock_base$quantities$biomass[1,1:49] <- colSums(pollock_base$quantities$biomassByage[1,3:10,1:49])
pollock_rw$quantities$biomass[1,1:49] <- colSums(pollock_rw$quantities$biomassByage[1,3:10,1:49])

######################### Plots
# - SAFE vs SS
file_name <- "Figures/18.5.1/18.5.1_SAFE_vs_ceattle_pollock"
mod_list <- list(pollock_base, pollock_rw, pollock_safe)
mod_names <- c("CEATTLE pollock", "CEATTLE pollock rw", "2018 SAFE (mt)")

plot_biomass(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
plot_ssb(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
plot_recruitment(mod_list, file = file_name, add_ci = FALSE, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
