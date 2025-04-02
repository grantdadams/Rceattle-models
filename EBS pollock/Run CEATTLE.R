# Code to run the bering sea pollock model in CEATTLE
# model is a single sex, single-species model

# DATA
# - Fishery catch
# - Fishery age composition
# - Fishery weight-at-age
# - Surveys
# -- Bottom trawl (random walk-logistic for age > 1, normal deviates for age = 1), additional penalty on selectivity
# -- AT (age-1 is an index, age > 1 have selectivity smoother)
# - Bottom temperature
# - Survey age composition
# - Catch-at-age methodology
# - Annual length-at-age and weight-at-age from surveys
# - Age at maturity

# MODEL
# - One sex
# - Ricker recruitment (1978-2017) w/ prior on steepness
# - Empirical weight-at-age
# - M = 0.3 for females, estimated for males

# Load data ----
library(Rceattle) # https://github.com/grantdadams/Rceattle/tree/dev
ebs_pollock <- Rceattle::read_data( file = "Data/EBS_2024_pollock_single_species.xlsx")
ebs_pollock$estDynamics = 0
ebs_pollock$index_data$Log_sd <- ebs_pollock$index_data$Log_sd/ebs_pollock$index_data$Observation
ebs_pollock$catch_data$Catch <- ebs_pollock$catch_data$Catch*1000
ebs_pollock$catch_data$Log_sd <- 0.05

# - Fix M
bridging_model_1 <- Rceattle::fit_mod(data_list = ebs_pollock,
                                          inits = NULL, # Initial parameters = 0
                                          file = NULL, # Don't save
                                          estimateMode = 0, # Estimate
                                          random_rec = FALSE, # No random recruitment
                                          msmMode = 0, # Single species mode
                                          verbose = 2,
                                          phase = TRUE,
                                          initMode = 2)



# - SAFE model
library(readxl)
SAFE2022_mod <- bridging_model_1
SAFE2022_mod$quantities$biomass[1,1:length(1954:2022)] <- read_excel("C:/Users/grant.adams/GitHub/yfs_ss3/Rceattle runs/Data/2022_ADMB_estimate.xlsx", sheet = 4)$Est * 1000
SAFE2022_mod$quantities$biomassSSB[1,1:length(1954:2022)] <- read_excel("C:/Users/grant.adams/GitHub/yfs_ss3/Rceattle runs/Data/2022_ADMB_estimate.xlsx", sheet = 3)$Est * 1000
SAFE2022_mod$quantities$R[1,1:length(1954:2022)] <- read_excel("C:/Users/grant.adams/GitHub/yfs_ss3/Rceattle runs/Data/2022_ADMB_estimate.xlsx", sheet = 2)$Est * 1000


plot_biomass(list(bridging_model_3, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Biomass", line = 1.8)
plot_ssb(list(bridging_model_3, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "SSB", line = 1.8)
plot_recruitment(list(bridging_model_3, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Recruitment", line = 1.8)

dev.off()
plot_selectivity(bridging_model_3)
