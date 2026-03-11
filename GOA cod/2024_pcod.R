library(Rceattle)
library(readxl)
library(dplyr)

################################################
# Cod
################################################
cod_caal <- read_data(file = "Data/GOA_24_pcod_single_species_1977-2024_w_CAAL.xlsx")


# - Fit empirical waa model
cod_base <- Rceattle::fit_mod(data_list = cod_caal,
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

# - Growth
cod_caal$fleet_control$Selectivity <- cod_caal$fleet_control$Selectivity + 5
cod_growth <- Rceattle::fit_mod(data_list = cod_caal,
                              inits = NULL, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 0, # Estimate
                              growthFun = build_growth(growth_model = 1),
                              M1Fun = build_M1(M1_model = 1,
                                               M1_use_prior = FALSE,
                                               M2_use_prior = FALSE),
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              verbose = 1,
                              phase = TRUE)


plot_biomass(list(cod_base, cod_growth))
