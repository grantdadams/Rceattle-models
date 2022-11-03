library(Rceattle)
library(readxl)
setwd("Model runs/GOA_22.1.1/")

################################################
# Pollock
################################################
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_22.1.1_pollock_single_species_1970-2022.xlsx")
mydata_pollock$msmMode = 0
mydata_pollock$estDynamics = 0

# - Fit single-species models
pollock_base <- Rceattle::fit_mod(data_list = mydata_pollock,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  verbose = 1,
                                  phase = "default")


################################################
# Cod
################################################
mydata_pcod <- Rceattle::read_data( file = "Data/GOA_22.1.1_pcod_single_species_1977-2022.xlsx")
mydata_pcod$pmature[1,2:13] <- 2
mydata_pcod$estDynamics[1] = 0

# - Fit single-species models
cod_base <- Rceattle::fit_mod(data_list = mydata_pcod,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  verbose = 1,
                                  phase = "default")


################################################
# ATF
################################################
mydata_atf <- Rceattle::read_data( file = "Data/GOA_22.1.1_arrowtooth_single_species_1977-2022.xlsx")
mydata_atf$estDynamics = 0
mydata_atf$srv_biom$Log_sd <- mydata_atf$srv_biom$Log_sd/mydata_atf$srv_biom$Observation

# - Fit single-species models
atf_base <- Rceattle::fit_mod(data_list = mydata_atf,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  verbose = 1,
                                  phase = "default")

atf_base <- Rceattle::fit_mod(data_list = mydata_atf,
                              inits = atf_base$estimated_params, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 0, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              verbose = 1,
                              phase = "default")



################################################
# Combine data
################################################
GOA_22 <-  combine_data(data_list1 = combine_data(data_list1 = mydata_pollock, data_list2 = mydata_atf), data_list2 =  mydata_pcod)
GOA_22$msmMode <- 0
GOA_22$styr <- 1977

GOA_22_base <- Rceattle::fit_mod(data_list = GOA_22,
                              inits = NULL, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 0, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              verbose = 1,
                              phase = "default")

write_data(GOA_22, file = "Data/GOA_22_1_1_data_1977_2022.xlsx")

