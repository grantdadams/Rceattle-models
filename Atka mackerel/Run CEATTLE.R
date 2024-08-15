# Code to run the atka mackerel assessment in CEATTLE
# model is a two sex, single-species model

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
mydata_atka <- Rceattle::read_data( file = "Data/atka_single_species_2022.xlsx")
mydata_atka$estDynamics = 0
mydata_atka$srv_biom$Log_sd <- mydata_atka$srv_biom$Log_sd/mydata_atka$srv_biom$Observation
# mydata_atka$fsh_biom$Catch <- mydata_atka$fsh_biom$Catch*1000

# - Fix M
bridging_model_1 <- Rceattle::fit_mod(data_list = mydata_atka,
                                      inits = NULL, # Initial parameters = 0
                                      file = NULL, # Don't save
                                      estimateMode = 0, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      phase = "default",
                                      initMode = 1,
                                      recFun = build_srr(srr_fun = 1,
                                                         proj_mean_rec = FALSE,
                                                         srr_est_mode = 2, # Prior on steepness
                                                         srr_prior_mean = 0.8,
                                                         srr_prior_sd = 0.2))



# - SAFE model
library(readxl)
SAFE2022_mod <- bridging_model_1
SAFE2022_mod$quantities$biomass[1,1:length(1977:2023)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 4)$Est
SAFE2022_mod$quantities$biomassSSB[1,1:length(1977:2023)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 3)$Est
SAFE2022_mod$quantities$R[1,1:length(1977:2022)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 2)$Est


plot_biomass(list(bridging_model_1, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Biomass", line = 1.8)
plot_ssb(list(bridging_model_1, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "SSB", line = 1.8)
plot_recruitment(list(bridging_model_1, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Recruitment", line = 1.8)

dev.off()
plot_selectivity(bridging_model_3)
