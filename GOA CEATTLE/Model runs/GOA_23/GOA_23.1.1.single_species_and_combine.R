library(Rceattle)
library(readxl)
library(dplyr)
setwd("Model runs/GOA_23.1.1/")

################################################
# Pollock
################################################
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_23.1.1_pollock_single_species_1970-2023.xlsx")
mydata_pollock$msmMode = 0
mydata_pollock$srv_biom$Observation <- mydata_pollock$srv_biom$Observation * 1e6
mydata_pollock$estDynamics = 0


mydata_pollock$endyr <- 2023
# - Fit single-species models
pollock_base <- fit_mod(data_list = mydata_pollock,
                        inits = NULL, # Initial parameters = 0
                        file = NULL, # Don't save
                        estimateMode = 0, # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0, # Single species mode
                        verbose = 1,
                        initMode = 2,
                        phase = "default")


################################################
# Cod
################################################
mydata_pcod <- Rceattle::read_data( file = "Data/GOA_23.1.1_pcod_single_species_1977-2023.xlsx")
mydata_pcod$pmature[1,2:13] <- 2
mydata_pcod$estDynamics[1] = 0

cod_alk <- read_xlsx( "Data/SAFE data/cod_alk.xlsx", col_names = TRUE) %>%
  as.data.frame()
rownames(cod_alk) <- cod_alk$Length
cod_alk <- cod_alk %>%
  select(-Length)
cod_alk <- t(cod_alk)
cod_alk[2,] <- cod_alk[1,] + cod_alk[2,]
cod_alk <- cod_alk[-1,]
cod_alk <- cod_alk[,rev(1:ncol(cod_alk))]
write.csv(cod_alk, "Data/Cod_alk.csv")

# - Fit single-species models
library(dplyr)
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
mydata_atf <- Rceattle::read_data( file = "Data/GOA_23.1.1_arrowtooth_single_species_1977-2023.xlsx")
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



################################################
# Combine data
################################################
mydata_pollock$pmature <- mydata_pollock$pmature[,1:11]
combined_data <-  combine_data(data_list1 = combine_data(data_list1 = mydata_pollock, data_list2 = mydata_atf), data_list2 =  mydata_pcod)
combined_data$msmMode <- 0
combined_data$styr <- 1977
write_data(combined_data, file = "Data/GOA_23_1_1_data_1977_2023.xlsx")



