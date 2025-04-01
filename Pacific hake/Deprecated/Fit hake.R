library(Rceattle)
library(dplyr)
library(readxl)

################################################
# Data
################################################
hakedata <- Rceattle::read_data(file = "Data/hake_yr24_241125.xlsx")
hakedata$fleet_control$Age_max_selected = NA
hakedata$fleet_control$Comp_loglike = 0
hakedata$styr <- 1966

################################################
# Fit initial model
################################################
hake_ss_eq <- Rceattle::fit_mod(data_list = hakedata,
                                inits = NULL, # Initial parameters = 0
                                file = NULL, # Don't save
                                estimateMode = 0,
                                verbose = 1,
                                msmMode = 0, # Single species mode
                                phase = TRUE,
                                initMode = 1) # Unfished equilibrium with no devs

hake_ss <- Rceattle::fit_mod(data_list = hakedata,
                             inits = NULL, # Initial parameters = 0
                             file = NULL, # Don't save
                             estimateMode = 0,
                             verbose = 1,
                             msmMode = 0, # Single species mode
                             phase = TRUE,
                             initMode = 2) # Unfished equilibrium with devs


hakedata$fleet_control$Comp_loglike = 1 # Dirichlet multinomial
hake_ss_dm <- Rceattle::fit_mod(data_list = hakedata,
                                inits = NULL, # Initial parameters = 0
                                file = NULL, # Don't save
                                estimateMode = 0,
                                msmMode = 0, # Single species mode
                                phase = TRUE,
                                initMode = 2)


# inits <- hake_ss_dm$estimated_params
# inits$rec_pars[,2] <- 0.2
hake_ss_ssr_dm <- fit_mod(data_list = hakedata,
                          inits = NULL, # Initial parameters = 0
                          file = NULL, # Don't save
                          estimateMode = 0,
                          verbose = 1,
                          msmMode = 0, # Single species mode
                          phase = TRUE,
                          recFun = build_srr(srr_fun = 2, # Beverton and holt with beta prior on steepness
                                             srr_est_mode = 3,
                                             srr_prior = 0.777,
                                             srr_prior_sd = 0.05
                          ),
                          initMode = 2)

hake_ss_dm_M <- Rceattle::fit_mod(data_list = hakedata,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0,
                                  verbose = 1,
                                  msmMode = 0, # Single species mode
                                  phase = TRUE,
                                  M1Fun = build_M1(M1_model = 1, # Estimate M
                                                   M1_use_prior = TRUE,
                                                   M_prior = exp(-1.61),
                                                   M_prior_sd = 0.1),
                                  initMode = 2)



hake_ss_dm_srr_M <- fit_mod(data_list = hakedata,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0,
                            verbose = 1,
                            random_rec = 0,
                            msmMode = 0, # Single species mode
                            phase = TRUE,
                            recFun = build_srr(srr_fun = 2,
                                               srr_est_mode = 3,
                                               srr_prior = 0.777,
                                               srr_prior_sd = 0.05
                            ),
                            M1Fun = build_M1(M1_model = 1,
                                             M1_use_prior = TRUE,
                                             M_prior = exp(-1.61),
                                             M_prior_sd = 0.1),
                            initMode = 2)

hake_ss_dm_srr_Mre <- fit_mod(data_list = hakedata,
                              inits = NULL, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 0,
                              verbose = 1,
                              random_rec = 1,
                              msmMode = 0, # Single species mode
                              phase = TRUE,
                              recFun = build_srr(srr_fun = 2,
                                                 srr_est_mode = 3,
                                                 srr_prior = 0.777,
                                                 srr_prior_sd = 0.05
                              ),
                              M1Fun = build_M1(M1_model = 1,
                                               M1_use_prior = TRUE,
                                               M_prior = exp(-1.61),
                                               M_prior_sd = 0.1),
                              initMode = 2)

hake_ss_dm_srr_M_init1 <- fit_mod(data_list = hakedata,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0,
                                  verbose = 1,
                                  msmMode = 0, # Single species mode
                                  phase = TRUE,
                                  recFun = build_srr(srr_fun = 2,
                                                     srr_est_mode = 3,
                                                     srr_prior = 0.777,
                                                     srr_prior_sd = 0.05
                                  ),
                                  M1Fun = build_M1(M1_model = 1,
                                                   M1_use_prior = TRUE,
                                                   M_prior = exp(-1.61),
                                                   M_prior_sd = 0.1),
                                  initMode = 2)


model_names = c("SS", "Multinomial No Idevs fixM", "Multinomial fixM", "DM estM", "DM Beverton estM", "DM Beverton estM No Idevs")
mod_list <- list(hake_ss_eq, hake_ss, hake_ss_dm_M, hake_ss_dm_srr_M, hake_ss_dm_srr_M_init1)

plot_biomass(mod_list, model_names = model_names)
plot_ssb(mod_list, model_names = model_names)
plot_stock_recruit(hake_ss_ssr_dm, model_names = model_names)



biomass <- as.data.frame(do.call("cbind", lapply(mod_list, function(x) x$quantities$biomass[1,1:58])))
ssb <- as.data.frame(do.call("cbind", lapply(mod_list, function(x) x$quantities$biomassSSB[1,1:58])))
R <- as.data.frame(do.call("cbind", lapply(mod_list, function(x) x$quantities$R[1,1:58])))
colnames(biomass) <- model_names

plot_catch(hake_ss2)
plot_recruitment(hake_ss2)
hake_ss$quantities$jnll
hake_ss$estimated_params$F_dev
