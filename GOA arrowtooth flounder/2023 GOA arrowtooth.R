# Code used to run the 2023 arrowtooth flounder assessment model in CEATTLE
# uses the "dev" version of Rceattle

library(Rceattle)
library(readxl)
library(dplyr)
library(TMB)

# Load data ----
mydata_atf <- Rceattle::read_data( file = "Data/GOA_23.1.1_arrowtooth_single_species_1977-2023.xlsx")
mydata_atf$estDynamics = 0
mydata_atf$index_data$Log_sd <- mydata_atf$index_data$Log_sd/mydata_atf$index_data$Observation
mydata_atf$fleet_control$proj_F_prop <- c(1,1,1)


# Single-species models ----
# - Fit single-species models and no fishing
# * Fix M ----
ceattle_ss <- Rceattle::fit_mod(data_list = mydata_atf,
                                inits = NULL, # Initial parameters = 0
                                file = NULL, # Don't save
                                estimateMode = 0, # Estimate
                                random_rec = FALSE, # No random recruitment
                                msmMode = 0, # Single species mode
                                verbose = 1,
                                phase = TRUE,
                                initMode = 1)



# Compare models ----
# - SAFE model
SAFE2023 <- read_excel("Data/2023_SAFE_biomass_estimate.xlsx", sheet = 1)

SAFE2023_mod <- ceattle_ss
SAFE2023_mod$quantities$biomass[1,1:length(1977:2023)] <- SAFE2023$Biomass
SAFE2023_mod$quantities$ssb[1,1:length(1977:2023)] <- SAFE2023$SSB
SAFE2023_mod$quantities$R[1,1:length(1977:2023)] <- SAFE2023$Recruitment/1000

# - Plot
model_list <- list(SAFE2023_mod, ceattle_ss)
model_names = c("ADMB", "TMB single-spp")

plot_biomass(model_list, model_names = model_names)
plot_ssb(model_list, model_names = model_names)
plot_recruitment(model_list, model_names = model_names)
