# Code to run the bering sea pollock model in CEATTLE
# model is a single sex, single-species model


library(Rceattle) # https://github.com/grantdadams/Rceattle/tree/dev-name-change

# Load data ----
ebs_pollock <- Rceattle::read_data( file = "Data/bsp0.xlsx")
ebs_pollock$estDynamics = 0
ebs_pollock$index_data$Log_sd <- ebs_pollock$index_data$Log_sd/ebs_pollock$index_data$Observation
ebs_pollock$catch_data$Catch <- ebs_pollock$catch_data$Catch*1000
ebs_pollock$catch_data$Log_sd <- 0.05
ebs_pollock$fleet_control$Fleet_type[5:6] <- 2 # Setting age-1 data as survey

# - Fix M
pollock_base <- Rceattle::fit_mod(data_list = ebs_pollock,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  verbose = 1,
                                  phase = TRUE,
                                  initMode = 2) # Unfished equilibrium with init_dev's turned on

