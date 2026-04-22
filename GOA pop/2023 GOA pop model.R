# Code to run the GOA pop assessment approximation in CEATTLE
# https://github.com/noaa-afsc/goa_pop/tree/a4538de11cacc7e1071558a2eb40286507a38c63/2024

# DATA
# - Fishery catch
# - Fishery age composition
# - Fishery weight-at-age
# - Survey biomass and standard error
# - Survey age composition
# - Survey weight-at-age
# - Age at maturity
# - Population weight-at-age
# - Ageing error

# MODEL
# - Single sex
# - Survey selectivity = sex-combined non-parametric
# - Survey q = analytical
# - Fishery selectivity = sex-combined non-parametric
# - Beverton recruitment (1977-2019) where steepness = 0.8
# - Empirical weight-at-age
# - M = 0.3

# Load data ----
library(Rceattle)
mydata_pop <- Rceattle::read_data( file = "Data/GOApop_single_species_2023.xlsx")
mydata_pop$estDynamics = 0
mydata_pop$index_data$Log_sd <- mydata_pop$index_data$Log_sd/mydata_pop$index_data$Observation
# mydata_pop$fsh_biom$Catch <- mydata_pop$fsh_biom$Catch*1000
mydata_pop$weight[,6:ncol(mydata_pop$weight)] <- mydata_pop$weight[,6:ncol(mydata_pop$weight)]/1000 # G to KG

# - Fix M
bridging_model_1 <- Rceattle::fit_mod(data_list = mydata_pop,
                                      inits = NULL, # Initial parameters = 0
                                      file = NULL, # Don't save
                                      estimateMode = 0, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      phase = TRUE,
                                      initMode = 1)



# - SAFE model
library(readxl)
SAFE2023_mod <- bridging_model_1
nyrs <- length(mydata_pop$styr:mydata_pop$endyr)
admb_ests <- read_excel("Data/2023_ADMB_estimate.xlsx", sheet = 1)
SAFE2023_mod$quantities$biomass[1,1:nyrs] <- admb_ests$Tot_biom * 1e6
SAFE2023_mod$quantities$ssb[1,1:nyrs] <- admb_ests$SpBiom
SAFE2023_mod$quantities$R[1,1:nyrs] <- admb_ests$Recruitment


plot_biomass(list(bridging_model_1, SAFE2023_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Biomass", line = 1.8)
plot_ssb(list(bridging_model_1, SAFE2023_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "SSB", line = 1.8)
plot_recruitment(list(bridging_model_1, SAFE2023_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Recruitment", line = 1.8)

