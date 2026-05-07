# Code to run approximation of 2024 Gulf of Alaska cod model
# Uses "dev" version of Rceattle

library(Rceattle)
library(readxl)
library(dplyr)

################################################
# Cod
################################################
cod_caal <- read_data(file = "Data/GOA_24_pcod_single_species_1977-2024_w_CAAL.xlsx")
cod_caal$maturity[,-1] <- 1



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
cod_caal$fleet_control$Selectivity_dimension <- "Length"
cod_growth <- Rceattle::fit_mod(data_list = cod_caal,
                              inits = NULL, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 0, # Estimate
                              growthFun = build_growth(fun = "vonBertalanffy"),
                              M1Fun = build_M1(M1_model = 1,
                                               M1_use_prior = FALSE,
                                               M2_use_prior = FALSE),
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              verbose = 1,
                              phase = TRUE)


# - Compare
X2024pcod_time_series <- read.csv("Data/2024pcod_time_series.csv")
years <- cod_caal$styr:cod_caal$endyr
safe2024 <- cod_base
safe2024$quantities$biomass[,1:length(years)] <- X2024pcod_time_series %>% filter(Yr %in% years) %>% pull(Bio_all)
safe2024$quantities$ssb[,1:length(years)] <- X2024pcod_time_series %>% filter(Yr %in% years) %>% pull(SpawnBio)
safe2024$quantities$R[,1:length(years)] <- X2024pcod_time_series %>% filter(Yr %in% years) %>% pull(Recruit_0)

# - Plot
plot_biomass(list(cod_base, cod_growth, safe2024), model_names = c("Base", "Growth", "SAFE"))
plot_ssb(list(cod_base, cod_growth, safe2024), model_names = c("Base", "Growth", "SAFE"))
plot_recruitment(list(cod_base, cod_growth, safe2024), model_names = c("Base", "Growth", "SAFE"))
