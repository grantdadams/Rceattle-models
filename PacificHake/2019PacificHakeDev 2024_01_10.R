library(Rceattle)
library(dplyr)
library(readxl)

################################################
# Data
################################################
mydata_pollock <- Rceattle::read_data(file = "data/hake_intrasp_240102.xlsx")
load("~/GitHub/RceattleRuns/PacificHake/ss_priorM1.Rdata")

################################################
# Fit initial model
################################################
# mydata_pollock$fleet_control$Selectivity[] <- 1
pk_base <- Rceattle::fit_mod(data_list = mydata_pollock,
                             inits = NULL, # Initial parameters = 0
                             file = NULL, # Don't save
                             estimateMode = 0,
                             M1Fun = Rceattle::build_M1(M1_model = 1,
                                                        M1_use_prior = TRUE,
                                                        M1_prior_mean = 0.2,
                                                        M1_prior_sd = .1),
                             msmMode = 0, # Single species mode
                             phase = "default")
# pk_base$quantities$jnll_comp

mydata_pollock$fleet_control$Age_first_selected <- c(1,2)
mydata_pollock$fleet_control$Nselages <- c(6, 6)
mydata_pollock$fleet_control$Time_varying_sel <- c(1,0)
pk_base_init <- Rceattle::fit_mod(data_list = mydata_pollock,
                             inits = NULL, # Initial parameters = 0
                             file = NULL, # Don't save
                             estimateMode = 0,
                             M1Fun = Rceattle::build_M1(M1_model = 1,
                                                        M1_use_prior = TRUE,
                                                        M1_prior_mean = 0.2,
                                                        M1_prior_sd = .1),
                             HCR = Rceattle::build_hcr(HCR = 6, # Cat 1 HCR
                                                       FsprLimit = 0.4, # F40%
                                                       Ptarget = 0.4, # Target is 40% B0
                                                       Plimit = 0.1, # No fishing when SB<SB10
                                                       Pstar = 0.5,
                                                       Sigma = 0.5),
                             projection_uncertainty = FALSE,
                             msmMode = 0, # Single species mode
                             phase = "default")

map <- pk_base_init$map
map$mapList$sel_coff[1,1,1] <- NA
map$mapList$sel_coff[2,1,2] <- NA

map$mapList$sel_coff_dev[1,1,,] <- NA
map$mapList$sel_coff_dev[2,1,2,] <- NA
map$mapFactor$sel_coff <- as.factor(map$mapList$sel_coff)

pk_base_init <- Rceattle::fit_mod(data_list = mydata_pollock,
                                  map = map,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0,
                                  M1Fun = Rceattle::build_M1(M1_model = 1,
                                                             M1_use_prior = TRUE,
                                                             M1_prior_mean = 0.2,
                                                             M1_prior_sd = .1),
                                  msmMode = 0, # Single species mode
                                  phase = "default")


pk_base_init$identified$param_list$sel_coff[,1,]
pk_base_init$quantities$sel[1,1,,1]
exp(cumsum(pk_base_init$estimated_params$sel_coff[1,1,]) - max(cumsum(pk_base_init$estimated_params$sel_coff[1,1,])))
plot_comp(pk_base_init)
plot_biomass(list(pk_base_init, ss_priorM1$model))
plot_recruitment(list(pk_base_init, ss_priorM1$model))
plot_ssb(list(pk_base_init, ss_priorM1$model))
plot_index(list(pk_base_init, ss_priorM1$model))
plot_catch(list(pk_base_init, ss_priorM1$model))
plot_selectivity(pk_base_init)
plot_selectivity(ss_priorM1$model)

pk_base_init$quantities$jnll_comp - ss_priorM1$model$quantities$jnll_comp

