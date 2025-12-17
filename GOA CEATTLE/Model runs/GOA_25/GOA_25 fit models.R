library(Rceattle)
library(readxl)
library(dplyr)
setwd("Model runs/GOA_25/")

# Manually add in diet data
combined_data <- read_data(file = "Data/GOA_25_data_1977_2025.xlsx")
combined_data$diet_data <- combined_data$diet_data %>%
  dplyr::filter(Prey != 4) %>%
  dplyr::filter(Pred != 4) %>%
  dplyr::filter(!(Pred = 3 & Pred_age > 10)) %>%
  dplyr::filter(!(Prey = 3 & Prey_age > 10)) %>%
  dplyr::mutate(
    Stomach_proportion_by_weight = as.numeric(Stomach_proportion_by_weight),
    Sample_size = as.numeric(Sample_size)
  ) %>%
  as.data.frame()

# Copy 2024 weights to 2025
terminal_weight <- combined_data$weight %>%
  dplyr::group_by(Wt_index, Wt_name, Sex) %>%
  dplyr::arrange(Year) %>%
  dplyr::slice(n()) %>%
  dplyr::mutate(Year = 2025)

combined_data$weight <- rbind(combined_data$weight, terminal_weight) %>%
  dplyr::arrange(Wt_index, Sex, Year)


# Copy 2024 Pyrs to 2025
terminal_pyrs <- combined_data$Pyrs %>%
  dplyr::group_by(Species, Sex) %>%
  dplyr::arrange(Year) %>%
  dplyr::slice(n()) %>%
  dplyr::mutate(Year = 2025)

combined_data$Pyrs <- rbind(combined_data$Pyrs, terminal_pyrs) %>%
  dplyr::arrange(Species, Sex, Year)


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
