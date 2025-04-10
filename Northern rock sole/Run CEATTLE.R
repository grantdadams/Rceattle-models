# Code to run the bering northern rock sole assessment in CEATTLE
# https://github.com/afsc-assessments/BSAI_NRS/tree/main (2022 alldata)
# model is a two sex, single-species model
# uses dev_srr branch
# https://github.com/grantdadams/Rceattle/tree/dev_srr

# DATA
# - Fishery catch
# - Fishery age composition
# - Fishery weight-at-age
# - Survey biomass and standard error
# - Survey age composition
# - Survey weight-at-age
# - Age at maturity

# MODEL
# - Two sex
# - Survey selectivity = sex-specific logistic
# - Survey q
# - Fishery selectivity = sex-specific time-varying logistic
# - Ricker recruitment (1978-2017)
# - Empirical weight-at-age
# - M = 0.15 for females, estimated for males

# Load data ----
library(Rceattle)
mydata_nrs <- Rceattle::read_data(file = "Data/nrs_single_species_2022.xlsx")
mydata_nrs$estDynamics = 0
mydata_nrs$index_data$Log_sd <- mydata_nrs$index_data$Log_sd/mydata_nrs$index_data$Observation
mydata_nrs$catch_data$Catch <- mydata_nrs$catch_data$Catch*1000

# - Fix M
bridging_model_1 <- Rceattle::fit_mod(data_list = mydata_nrs,
                                          inits = NULL, # Initial parameters = 0
                                          file = NULL, # Don't save
                                          estimateMode = 0, # Estimate
                                          random_rec = FALSE, # No random recruitment
                                          msmMode = 0, # Single species mode
                                          verbose = 1,
                                          phase = TRUE,
                                          initMode = 1)

# - Est female and male M
bridging_model_2 <- Rceattle::fit_mod(data_list = mydata_nrs,
                                      inits = bridging_model_1$estimated_params,
                                      file = NULL, # Don't save
                                      estimateMode = 0, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      M1Fun = build_M1(M1_model = c(2),
                                                       M_prior = 0.15,
                                                       M_prior_sd = 0.2,
                                                       M1_use_prior = TRUE),
                                      phase = FALSE,
                                      initMode = 1)


# - Fix female M and estimate male M
inits <- bridging_model_1$estimated_params
map <- build_map(data_list = bridging_model_2$data_list, params = inits)
map$mapList$ln_M1[1,1,] <- NA
map$mapFactor$ln_M1 <- factor(map$mapList$ln_M1)

bridging_model_3 <- Rceattle::fit_mod(data_list = mydata_nrs,
                                      inits = inits,
                                      map = map,
                                      file = NULL, # Don't save
                                      estimateMode = 0, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      phase = FALSE,
                                      initMode = 1)


# - SAFE model
library(readxl)
SAFE2022_mod <- bridging_model_1
SAFE2022_mod$quantities$biomass[1,1:length(1975:2022)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 4)$Est * 1000
SAFE2022_mod$quantities$ssb[1,1:length(1975:2022)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 3)$Est * 1000
SAFE2022_mod$quantities$R[1,1:length(1975:2022)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 2)$Est * 1000

# Plots ----
plot_biomass(list(bridging_model_2, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Biomass", line = 1.8)
plot_ssb(list(bridging_model_3, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "SSB", line = 1.8)
plot_recruitment(list(bridging_model_3, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Recruitment", line = 1.8)

# dev.off()
plot_selectivity(bridging_model_3)
