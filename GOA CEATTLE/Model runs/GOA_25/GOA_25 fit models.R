library(Rceattle)
library(readxl)
library(dplyr)
setwd("Model runs/GOA_25/")

# Manually add in diet data
combined_data <- read_data(file = "Data/GOA_25_data_1977_2025.xlsx")


# - Est single-species fixed M
ss_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            verbose = 1,
                            phase = TRUE)



# - Est single-species estimate M
ssm <- Rceattle::fit_mod(data_list = combined_data,
                         inits = ss_mod$estimated_params, 
                         file = NULL, # Don't save
                         estimateMode = 0, # Estimate
                         random_rec = FALSE, # No random recruitment
                         msmMode = 0, # Single species mode
                         verbose = 1,
                         phase = TRUE,
                         M1Fun = build_M1(M1_model = c(1,2,1),
                                          M1_use_prior = FALSE,
                                          M2_use_prior = FALSE))


# - Est multi-species
ms_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = ss_mod$estimated_params,
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # Multi species mode
                            verbose = 1,
                            suit_styr = 1990,
                            suit_endyr = 2015,
                            phase = FALSE,
                            M1Fun = build_M1(M1_model = c(1,2,1),
                                             M1_use_prior = FALSE,
                                             M2_use_prior = FALSE))




# - Plot
mod_list_all <- list(ss_mod, ssm, ms_mod)
plot_biomass(mod_list_all)
plot_b_eaten(mod_list_all)
plot_recruitment(mod_list_all)

# - Save
save(mod_list_all, file = "Models/GOA_25_mod_list.RData")
