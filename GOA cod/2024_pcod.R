library(Rceattle)
library(readxl)
library(dplyr)

################################################
# Cod
################################################
mydata_pcod <- read_data( file = "Data/GOA_24_pcod_single_species_1977-2024.xlsx")
mydata_pcod$maturity[1,2:13] <- 2
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
                              phase = TRUE)

cod_caal <- read_data(file = "Data/GOA_24_pcod_single_species_1977-2024_w_CAAL.xlsx")


# - Fit single-species models
cod_base <- Rceattle::fit_mod(data_list = cod_caal,
                              inits = NULL, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 3, # Estimate
                              M1Fun = build_M1(M1_model = 1,
                                               M1_use_prior = FALSE,
                                               M2_use_prior = FALSE),
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              verbose = 1,
                              phase = TRUE)
