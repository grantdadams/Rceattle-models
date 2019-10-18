library(Rceattle)
setwd("Model runs/GOA_18.2.2 - Time Varying - 2sex - ATFq")

# Updated the ALK

################################################
# Data
################################################
# Read the data in
mydata <- Rceattle::read_data( file = "GOA_18.2.2_small_pcod_removed.xlsx")

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
                                 recompile = FALSE,
                                 phase = "default")


file_name <- "Figures/Base/Base"
plot_index(ss_run_base, file = file_name)
# plot_catch(ss_run_base, file = file_name)
Rceattle::plot_comp(ss_run_base, file = file_name)
plot_biomass(ss_run_base, file = file_name)
plot_ssb(ss_run_base, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_base, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_base, file = file_name)
write_results(ss_run_base, file = paste0(file_name, ".xlsx"))




################################################
# Model 5 - Add multi-species
################################################
mydata_ms <- ss_run_base$data_list
mydata_ms$M1_base[1,3] <- 1.14 + 0.06169283
mydata_ms$M1_base[1,4:6] <- c(0.5, 0.4, 0.34)
mydata_ms$M1_base[2,3] <- 0.36
mydata_ms$M1_base[3,3] <- 0.01
mydata_ms$M1_base[4,3] <- 0.01
mydata_ms$BTempC <- mydata_ms$BTempC * 0 + 5.55042


ms_run_mod1 <- Rceattle::fit_mod(data_list = mydata_ms,
                                 inits = ss_run_base$estimated_params, # Initial parameters = 0
                                 file = "Models/ms0", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 1, # Single species mode
                                 silent = TRUE, phase = NULL,
                                 niter = 10)



file_name <- "Figures/MS1/MS0"
plot_index(ms_run_mod1, file = file_name)
# plot_catch(ms_run_mod1, file = file_name)
Rceattle::plot_srv_comp(ms_run_mod1, file = file_name)
Rceattle::plot_fsh_comp(ms_run_mod1, file = file_name)
plot_biomass(ms_run_mod1, file = file_name)
plot_ssb(ms_run_mod1, file = file_name, add_ci = TRUE)
plot_recruitment(ms_run_mod1, file = file_name, add_ci = TRUE)
plot_selectivity(ms_run_mod1, file = file_name)
write_results(ms_run_mod1, file = paste0(file_name, ".xlsx"))


################################################
# Model 1 - Add Pollock Random Walk to Selectivity
################################################
mydata$fleet_control$Time_varying_sel[10] = 1
mydata$fleet_control$Sel_sd_prior[10] = 0.05
inits <- ss_run_base$estimated_params
inits$ln_sigma_sel[10] <- log(0.05)

ss_run_mod1 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = inits, # Initial parameters = 0
                                 file = "Models/ss_mod1", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE,
                                 phase = "default")


################################################
# Model 2 - Add Pollock Random Walk to Acoustic Catchability
################################################
mydata$fleet_control$Estimate_q[1] = 1
mydata$fleet_control$Log_q_prior[1] = -13
inits <- ss_run_mod1$estimated_params
inits$ln_srv_q[1] <- log(ss_run_mod1$quantities$srv_q_analytical[1])

ss_run_mod2 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = inits, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE, phase = "default")

mydata$fleet_control$Time_varying_q[1] = 1
mydata$fleet_control$Q_sd_prior[1] = 0.05
inits <- ss_run_mod2$estimated_params
inits$ln_sigma_srv_q[1] <- log(0.05)

ss_run_mod2 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = inits, # Initial parameters = 0
                                 file = "Models/ss_mod2", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE, phase = "default")

################################################
# Model 3 - Add Pollock Random Walk to BT Catchability
################################################
mydata$fleet_control$Estimate_q[2] = 1
mydata$fleet_control$Log_q_prior[2] = -13
inits <- ss_run_mod2$estimated_params
inits$ln_srv_q[2] <- log(ss_run_mod1$quantities$srv_q_analytical[2])

ss_run_mod3 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = inits, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE, phase = "default")

mydata$fleet_control$Time_varying_q[2] = 1
mydata$fleet_control$Q_sd_prior[2] = 0.01
inits <- ss_run_mod3$estimated_params
inits$ln_sigma_srv_q[2] <- log(0.01)

ss_run_mod3 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = inits, # Initial parameters = 0
                                 file = "Models/ss_mod3", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE, phase = "default")


################################################
# Model 4 - Add Pollock Random Walk to ADFG Catchability
################################################
mydata$fleet_control$Estimate_q[3] = 1
mydata$fleet_control$Log_q_prior[3] = -13
inits <- ss_run_mod3$estimated_params
inits$ln_srv_q[3] <- log(ss_run_mod1$quantities$srv_q_analytical[3])

ss_run_mod4 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = inits, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE, phase = "default")

mydata$fleet_control$Time_varying_q[3] = 1
mydata$fleet_control$Q_sd_prior[3] = 0.05
inits <- ss_run_mod4$estimated_params
inits$ln_sigma_srv_q[3] <- log(0.05)

ss_run_mod4 <- Rceattle::fit_mod(data_list = mydata,
                                 inits = inits, # Initial parameters = 0
                                 file = "Models/ss_mod4", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE, phase = "default")

file_name <- "Figures/RW4/RW4"
plot_index(ss_run_mod4, file = file_name)
# plot_catch(ss_run_mod4, file = file_name)
Rceattle::plot_comp(ss_run_mod4, file = file_name)
plot_biomass(ss_run_mod4)
plot_ssb(ss_run_mod4, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_mod4, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_mod4, file = file_name)
write_results(ss_run_mod4, file = paste0(file_name, ".xlsx"))


################################################
# Model 5 - Add multi-species
################################################
mydata_ms <- ss_run_mod4$data_list
mydata_ms$M1_base[1,3] <- 1.14 + 0.06169283 - 0.09233365
mydata_ms$M1_base[1,4:6] <- c(0.5, 0.4, 0.34)
mydata_ms$M1_base[2,3] <- 0.36
mydata_ms$M1_base[3,3] <- 0.1 + 04458429
mydata_ms$M1_base[4,3] <- 0.1
mydata_ms$BTempC <- mydata_ms$BTempC * 0 + 5.55042


ms_run_mod1 <- Rceattle::fit_mod(data_list = mydata_ms,
                                 inits = ss_run_mod4$estimated_params, # Initial parameters = 0
                                 file = "Models/ms1", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 1, # Single species mode
                                 silent = TRUE, phase = NULL,
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

