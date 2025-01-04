# Code to run the bering sea pollock model in CEATTLE
# model is a single sex, single-species model

# DATA
# - Fishery catch
# - Fishery age composition
# - Fishery weight-at-age
# - Survey biomass and standard error
# - Bottom temperature
# - Survey age composition
# - Catch-at-age methodology
# - Annual length-at-age and weight-at-age from surveys
# - Age at maturity

# MODEL
# - Two sex
# - Survey selectivity = sex-combined logistic
# - Survey q = q * e^(B*Env Indices)
# - Fishery selectivity = sex-specific time-varying logistic
# - Ricker recruitment (1978-2017)
# - Empirical weight-at-age
# - M = 0.12 for females, estimated for males

# Load data ----
library(Rceattle)
mydata_yfs <- Rceattle::read_data( file = "Data/EBS_2024_pollock_single_species.xlsx")
mydata_yfs$estDynamics = 0
mydata_yfs$srv_biom$Log_sd <- mydata_yfs$srv_biom$Log_sd/mydata_yfs$srv_biom$Observation
mydata_yfs$fsh_biom$Catch <- mydata_yfs$fsh_biom$Catch*1000

# - Fix M
bridging_model_1 <- Rceattle::fit_mod(data_list = mydata_yfs,
                                          inits = NULL, # Initial parameters = 0
                                          file = NULL, # Don't save
                                          estimateMode = 0, # Estimate
                                          random_rec = FALSE, # No random recruitment
                                          msmMode = 0, # Single species mode
                                          verbose = 1,
                                          phase = TRUE,
                                          initMode = 2)

# - Est female and male M
bridging_model_2 <- Rceattle::fit_mod(data_list = mydata_yfs,
                                      inits = bridging_model_1$estimated_params,
                                      file = NULL, # Don't save
                                      estimateMode = 0, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      M1Fun = build_M1(M1_model = c(2)),
                                      phase = NULL,
                                      initMode = 2)

# - Fix female M and estimate male M
inits <- bridging_model_1$estimated_params
map <- build_map(data_list = bridging_model_2$data_list, params = inits)
map$mapList$ln_M1[1,1,] <- NA
map$mapFactor$ln_M1 <- factor(map$mapList$ln_M1)

# -- Fix selectivity for survey to be sex-invariant
map$mapList$sel_inf[1,1,] <- 1
map$mapList$ln_sel_slp[1,1,] <- 1

map$mapFactor$sel_inf <- factor(map$mapList$sel_inf)
map$mapFactor$ln_sel_slp <- factor(map$mapList$ln_sel_slp)

bridging_model_3 <- Rceattle::fit_mod(data_list = mydata_yfs,
                                      inits = inits,
                                      map = map,
                                      file = NULL, # Don't save
                                      estimateMode = 0, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      phase = NULL,
                                      initMode = 2)

#
# bridging_model_re <- Rceattle::fit_mod(data_list = mydata_yfs,
#                                       inits = bridging_model_1$estimated_params, # Initial parameters = 0
#                                       file = NULL, # Don't save
#                                       estimateMode = 0, # Estimate
#                                       random_rec = TRUE, # No random recruitment
#                                       msmMode = 0, # Single species mode
#                                       verbose = 1,
#                                       phase = NULL,
#                                       initMode = 2)


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
