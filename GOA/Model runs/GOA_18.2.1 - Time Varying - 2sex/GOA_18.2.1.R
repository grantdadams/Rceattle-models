library(Rceattle)
setwd("Model runs/GOA_18.2.1 - Time Varying - 2sex")

# Updated the ALK

################################################
# Data
################################################
# Read the data in
mydata <- Rceattle::read_data( file = "GOA_18.2.1.xlsx")

################################################
# Model 1 - Single-species
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
Rceattle::plot_srv_comp(ss_run_base, file = file_name)
Rceattle::plot_fsh_comp(ss_run_base, file = file_name)
plot_biomass(ss_run_base, file = file_name)
plot_ssb(ss_run_base, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_base, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_base, file = file_name)
write_results(ss_run_base, file = paste0(file_name, ".xlsx"))



################################################
# Model 2 - Add multi-species
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