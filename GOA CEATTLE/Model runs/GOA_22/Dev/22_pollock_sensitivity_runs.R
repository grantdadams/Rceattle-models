library(Rceattle)
library(readxl)
setwd("Model runs/GOA_22.1.1/")

################################################
# Data and base models
################################################

################################################
# Pollock
################################################
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_22.1.1_pollock_single_species_1970-2022.xlsx")
mydata_pollock$msmMode = 0
mydata_pollock$estDynamics = 0

# - Fit single-species models
pollock_base <- Rceattle::fit_mod(data_list = mydata_pollock,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  verbose = 1,
                                  phase = "default")

# - Fit single-species models descending logistic on survey 6
mydata_pollock_dl6 <- mydata_pollock
mydata_pollock_dl6$fleet_control$Selectivity[6] <- 4
mydata_pollock_dl6$emp_sel <- mydata_pollock_dl6$emp_sel[-3,]

pollock_dl6 <- Rceattle::fit_mod(data_list = mydata_pollock_dl6,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 estimateMode = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 verbose = 1,
                                 phase = "default")

# - Fit single-species models
pollock_base_re <- Rceattle::fit_mod(data_list = mydata_pollock,
                                     inits = pollock_base$estimated_params, # Initial parameters = 0
                                     file = NULL, # Don't save
                                     estimateMode = 0, # Estimate
                                     random_rec = TRUE, # No random recruitment
                                     msmMode = 0, # Single species mode
                                     verbose = 1,
                                     phase = NULL)

# - Fit single-species models
pollock_dl6_re <- Rceattle::fit_mod(data_list = mydata_pollock_dl6,
                                    inits = pollock_dl6$estimated_params, # Initial parameters = 0
                                    file = NULL, # Don't save
                                    estimateMode = 0, # Estimate
                                    random_rec = TRUE, # No random recruitment
                                    msmMode = 0, # Single species mode
                                    verbose = 1,
                                    phase = NULL)



# - Compare with SAFE Model
#########################
library(readxl)
safe2022biomass <- as.data.frame(read_xlsx("Data/2022_Pollock_Safe.xlsx", sheet = 1))

# Assign data to CEATTLE object
years <- 1970:2022
Mod_2022_SAFE <- pollock_base
Mod_2022_SAFE$quantities$biomass[1,1:length(years)] <- safe2022biomass$B * 1000000
Mod_2022_SAFE$quantities$biomassSSB[1,1:length(years)] <-  safe2022biomass$SSB * 1000000
Mod_2022_SAFE$quantities$R[1,1:length(years)] <-  safe2022biomass$R * 1000000

mod_list <- list(pollock_base, pollock_base_re, pollock_dl6, pollock_dl6_re, Mod_2022_SAFE)
model_names <- c("Base", "Base w/ random rec", "Srv6 DL", "Srv6 DL w/ random rec", "SAFE")

# -- Plot
plot_biomass(mod_list, model_names = model_names)
plot_ssb(mod_list, model_names = model_names)
plot_recruitment(mod_list, model_names = model_names)
plot_logindex(mod_list, model_names = model_names)


# - Retros
pollock_retro <- retrospective(pollock_base, 10)
plot_biomass(pollock_retro$Rceattle_list)

ss_retro <- retrospective(mod_list_all[[2]], 10)
plot_biomass(ss_retro$Rceattle_list)

ms_retro <- retrospective(mod_list_all[[3]], 10)
plot_biomass(ms_retro$Rceattle_list)