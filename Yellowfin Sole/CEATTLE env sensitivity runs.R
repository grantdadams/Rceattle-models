# Code to run the bering and aleutian island yellowfin sole assessment in CEATTLE
# - testing sensitivities to different environmental indices
# - https://github.com/afsc-gap-products/model-based-indices/tree/main/species_specific_code/BS/yellowfin_sole


# Model is a two sex, single-species model
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
mydata_yfs <- Rceattle::read_data( file = "Data/yfs_single_species_2022.xlsx")
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
                                          phase = "default",
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


# Fit q-link models ----
# * Base model ----
base_model <- Rceattle::fit_mod(data_list = mydata_yfs,
                                      inits = inits,
                                      map = map,
                                      file = NULL, # Don't save
                                      estimateMode = 0, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      phase = NULL,
                                      initMode = 2)

# * No interaction ----
map2 <- map
map2$mapList$srv_q_beta[1,3] <- NA
inits$srv_q_beta[1,3] <- 0
map2$mapFactor$srv_q_beta <- factor(map2$mapList$srv_q_beta)

no_interaction <- Rceattle::fit_mod(data_list = mydata_yfs,
                                inits = inits,
                                map = map2,
                                file = NULL, # Don't save
                                estimateMode = 0, # Estimate
                                random_rec = FALSE, # No random recruitment
                                msmMode = 0, # Single species mode
                                verbose = 1,
                                phase = NULL,
                                initMode = 2)

# * No start date ----
# - Map out start date and interaction
map3 <- map
map3$mapList$srv_q_beta[1,2:3] <- NA
inits$srv_q_beta[1,2:3] <- 0
map3$mapFactor$srv_q_beta <- factor(map3$mapList$srv_q_beta)

no_startdate <- Rceattle::fit_mod(data_list = mydata_yfs,
                                    inits = inits,
                                    map = map3,
                                    file = NULL, # Don't save
                                    estimateMode = 0, # Estimate
                                    random_rec = FALSE, # No random recruitment
                                    msmMode = 0, # Single species mode
                                    verbose = 1,
                                    phase = NULL,
                                    initMode = 2)

# * No sst ----
# - Map out sst and interaction
map4 <- map
map4$mapList$srv_q_beta[1,c(1,3)] <- NA
inits$srv_q_beta[1,c(1,3)] <- 0
map4$mapFactor$srv_q_beta <- factor(map4$mapList$srv_q_beta)

no_sstq <- Rceattle::fit_mod(data_list = mydata_yfs,
                                  inits = inits,
                                  map = map4,
                                  file = NULL, # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  verbose = 1,
                                  phase = NULL,
                                  initMode = 2)

# * No q indices ----
# - Map out all params
map5 <- map
map5$mapList$srv_q_beta[1,1:3] <- NA
inits$srv_q_beta[1,1:3] <- 0
map5$mapFactor$srv_q_beta <- factor(map5$mapList$srv_q_beta)

no_indices <- Rceattle::fit_mod(data_list = mydata_yfs,
                             inits = inits,
                             map = map5,
                             file = NULL, # Don't save
                             estimateMode = 0, # Estimate
                             random_rec = FALSE, # No random recruitment
                             msmMode = 0, # Single species mode
                             verbose = 1,
                             phase = NULL,
                             initMode = 2)



# Plot ----
model_list <- list(base_model, no_interaction, no_startdate, no_sstq, no_indices)
plot_biomass(model_list, model_names = 1:5)


# Retrospectives
retro_list <- lapply(model_list, function(x) retrospective(x, peels = 10))
