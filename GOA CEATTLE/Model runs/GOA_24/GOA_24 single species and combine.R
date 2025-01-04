library(Rceattle)
library(readxl)
library(dplyr)
setwd("Model runs/GOA_24/")

################################################
# Pollock
################################################
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_24_pollock_single_species_1970-2024.xlsx")
mydata_pollock$msmMode = 0
mydata_pollock$estDynamics = 0


mydata_pollock$endyr <- 2024
# - Fit single-species models
pollock_base <- fit_mod(data_list = mydata_pollock,
                        inits = NULL, # Initial parameters = 0
                        file = NULL, # Don't save
                        estimateMode = 0, # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0, # Single species mode
                        verbose = 1,
                        initMode = 1,
                        phase = "default")


################################################
# Cod
################################################
mydata_pcod <- Rceattle::read_data( file = "Data/GOA_24_pcod_single_species_1977-2024.xlsx")
mydata_pcod$fleet_control <- mydata_pcod$fleet_control  %>%
  mutate(Comp_loglike = 0,
         Age_max_selected = NA)
mydata_pcod$pmature[1,2:13] <- 2
mydata_pcod$estDynamics[1] = 0
# - Using same length comp data as 2023 because marginals werent output in 2024

# - Fit single-species models
cod_base <- Rceattle::fit_mod(data_list = mydata_pcod,
                              inits = NULL, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 0, # Estimate
                              M1Fun = build_M1(M1_model = 1,
                                               M1_use_prior = FALSE,
                                               M2_use_prior = FALSE),
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              verbose = 1,
                              phase = "default")



################################################
# ATF
################################################
mydata_atf <- Rceattle::read_data( file = "Data/GOA_24_arrowtooth_single_species_1977-2024.xlsx")
mydata_atf$estDynamics = 0
mydata_atf$srv_biom$Log_sd <- mydata_atf$srv_biom$Log_sd/mydata_atf$srv_biom$Observation
mydata_atf$fleet_control$proj_F_prop <- c(1,1,1)

mydata_atf$fleet_control <- mydata_atf$fleet_control  %>%
  mutate(Comp_loglike = 0,
         Age_max_selected = NA)

# - Fit single-species models
atf_base <- Rceattle::fit_mod(data_list = mydata_atf,
                              inits = NULL, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 0, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              verbose = 1,
                              phase = "default")



################################################
# Combine data
################################################
mydata_pollock$pmature <- mydata_pollock$pmature[,1:11]
combined_data <-  combine_data(data_list1 = combine_data(data_list1 = mydata_pollock, data_list2 = mydata_atf), data_list2 =  mydata_pcod)
combined_data$msmMode <- 0
combined_data$styr <- 1977
write_data(combined_data, file = "Data/GOA_24_data_1977_2024.xlsx")



