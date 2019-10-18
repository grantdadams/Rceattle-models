library(Rceattle)
setwd("Model runs/GOA_18.1.4 - Time Varying")

# Updated the ALK

################################################
# Data
################################################
# Read the data in
mydata <- Rceattle::read_data( file = "GOA_18.1.4.xlsx")

################################################
# Estimation
################################################
ss_run_base <- Rceattle::fit_mod(data_list = mydata,
                            inits = NULL, # Initial parameters = 0
                            file = "Models/ss_mod0", # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE,
                            recompile = FALSE)

file_name <- "Figures/Base/Base"
plot_index(ss_run_base, file = file_name)
# plot_catch(ss_run_base, file = file_name)
Rceattle::plot_srv_comp(ss_run_base, file = file_name)
Rceattle::plot_fsh_comp(ss_run_base, file = file_name)
plot_biomass(ss_run_base, file = file_name)
plot_ssb(ss_run_base, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_base, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_base, file = file_name)
write_results(ss_run_base, file = paste0(file_name, ".xlsx"))

################################################
# Multi-species estimation
################################################
mydata$M1_base <- mydata$M1_base * 0.1
ms_run_base <- Rceattle::fit_mod(data_list = mydata,
                                 inits = ss_run_base$estimated_params, # Initial parameters = 0
                                 file = "Models/ms_mod0", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 1, # Multi species mode
                                 niter = 3,
                                 silent = FALSE,
                                 recompile = FALSE)

ms_run_base$quantities$jnll_comp
ms_run_base$quantities$M2[,1,]
ms_run_base$quantities$suit_main[3,3,,,1]

file_name <- "Figures/MA_base/MS_base"
plot_index(ms_run_base, file = file_name)
# plot_catch(ms_run_base, file = file_name)
Rceattle::plot_srv_comp(ms_run_base, file = file_name)
Rceattle::plot_fsh_comp(ms_run_base, file = file_name)
plot_biomass(ms_run_base, file = file_name)
plot_ssb(ms_run_base, file = file_name, add_ci = TRUE)
plot_recruitment(ms_run_base, file = file_name, add_ci = TRUE)
plot_selectivity(ms_run_base, file = file_name)
write_results(ms_run_base, file = paste0(file_name, ".xlsx"))


################################################
# Model 1 - Add Pollock Random Walk to Selectivity
################################################
mydata$fsh_control$Time_varying_sel[1] = 1
mydata$fsh_control$Sel_sd_prior[1] = 0.05

ss_run_mod1 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = NULL, # Initial parameters = 0
                                 file = "Models/ss_mod1", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE)

file_name <- "Figures/RW1/RW1"
plot_index(ss_run_mod1, file = file_name)
# plot_catch(ss_run_mod1, file = file_name)
Rceattle::plot_srv_comp(ss_run_mod1, file = file_name)
Rceattle::plot_fsh_comp(ss_run_mod1, file = file_name)
plot_biomass(ss_run_mod1, file = file_name)
plot_ssb(ss_run_mod1, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_mod1, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_mod1, file = file_name)
write_results(ss_run_mod1, file = paste0(file_name, ".xlsx"))


################################################
# Model 2 - Add Pollock Random Walk to Acoustic Catchability
################################################
mydata$srv_control$Estimate_q[2] = 1
mydata$srv_control$Log_q_prior[2] = -13

ss_run_mod2 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = FALSE)

mydata$srv_control$Time_varying_q[2] = 1
mydata$srv_control$Q_sd_prior[2] = 0.05
ss_run_mod2$estimated_params$ln_sigma_srv_q[2] <- log(0.05)

ss_run_mod2 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = ss_run_mod2$estimated_params, # Initial parameters = 0
                                 file = "Models/ss_mod2", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = FALSE)

file_name <- "Figures/RW2/RW2"
plot_index(ss_run_mod2, file = file_name)
# plot_catch(ss_run_mod2, file = file_name)
Rceattle::plot_srv_comp(ss_run_mod2, file = file_name)
Rceattle::plot_fsh_comp(ss_run_mod2, file = file_name)
plot_biomass(ss_run_mod2, file = file_name)
plot_ssb(ss_run_mod2, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_mod2, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_mod2, file = file_name)
write_results(ss_run_mod2, file = paste0(file_name, ".xlsx"))


################################################
# Model 3 - Add Pollock Random Walk to BT Catchability
################################################
mydata$srv_control$Estimate_q[3] = 1
mydata$srv_control$Log_q_prior[3] = -13

ss_run_mod3 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = FALSE)

mydata$srv_control$Time_varying_q[3] = 1
mydata$srv_control$Q_sd_prior[3] = 0.01
ss_run_mod3$estimated_params$ln_sigma_srv_q[3] <- log(0.01)

ss_run_mod3 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = ss_run_mod3$estimated_params, # Initial parameters = 0
                                 file = "Models/ss_mod3", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = FALSE)

file_name <- "Figures/RW3/RW3"
plot_index(ss_run_mod3, file = file_name)
# plot_catch(ss_run_mod3, file = file_name)
Rceattle::plot_srv_comp(ss_run_mod3, file = file_name)
Rceattle::plot_fsh_comp(ss_run_mod3, file = file_name)
plot_biomass(ss_run_mod3, file = file_name)
plot_ssb(ss_run_mod3, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_mod3, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_mod3, file = file_name)
write_results(ss_run_mod3, file = paste0(file_name, ".xlsx"))


################################################
# Model 4 - Add Pollock Random Walk to ADFG Catchability
################################################
mydata$srv_control$Estimate_q[4] = 1
mydata$srv_control$Log_q_prior[4] = -13

ss_run_mod4 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE)

mydata$srv_control$Time_varying_q[4] = 1
mydata$srv_control$Q_sd_prior[4] = 0.05
ss_run_mod4$estimated_params$ln_sigma_srv_q[4] <- log(0.05)

ss_run_mod4 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = ss_run_mod4$estimated_params, # Initial parameters = 0
                                 file = "Models/ss_mod4", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE)

file_name <- "Figures/RW4/RW4"
plot_index(ss_run_mod4, file = file_name)
# plot_catch(ss_run_mod4, file = file_name)
Rceattle::plot_srv_comp(ss_run_mod4, file = file_name)
Rceattle::plot_fsh_comp(ss_run_mod4, file = file_name)
plot_biomass(ss_run_mod4, file = file_name)
plot_ssb(ss_run_mod4, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_mod4, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_mod4, file = file_name)
write_results(ss_run_mod4, file = paste0(file_name, ".xlsx"))


################################################
# Model 5 - Add multi-species
################################################
ms_run_mod1 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = ss_run_mod4$estimated_params, # Initial parameters = 0
                                 file = "FModels/ms_mod1", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 1, # Single species mode
                                 silent = FALSE,
                                 niter = 10)

file_name <- "Figures/MS1/MS1"
plot_index(ms_run_mod1, file = file_name)
# plot_catch(ms_run_mod1, file = file_name)
Rceattle::plot_srv_comp(ms_run_mod1, file = file_name)
Rceattle::plot_fsh_comp(ms_run_mod1, file = file_name)
plot_biomass(ms_run_mod1, file = file_name)
plot_ssb(ms_run_mod1, file = file_name, add_ci = TRUE)
plot_recruitment(ms_run_mod1, file = file_name, add_ci = TRUE)
plot_selectivity(ms_run_mod1, file = file_name)
write_results(ms_run_mod1, file = paste0(file_name, ".xlsx"))