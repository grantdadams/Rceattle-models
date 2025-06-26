# Code to run the atka mackerel assessment in CEATTLE
# model is a single sex, single-species model
# uses dev-name-change branch

# DATA
# - Fishery catch
# - Fishery age composition
# - Fishery weight-at-age
# - Survey biomass and standard error
# - Survey age composition
# - Survey weight-at-age
# - Age at maturity
# - Population weight-at-age
# - Ageing error

# MODEL
# - Single sex
# - Survey selectivity = sex-combined non-parametric
# - Survey q = estimated parameter
# - Fishery selectivity = sex-combined non-parametric (time varying)
# - Beverton recruitment (1977-2019) where steepness = 0.8
# - Empirical weight-at-age
# - M = 0.3

# Load data ----
library(Rceattle)
mydata_atka <- Rceattle::read_data( file = "Data/atka_single_species_2022.xlsx")
mydata_atka$estDynamics = 0
mydata_atka$index_data$Log_sd <- mydata_atka$index_data$Log_sd/mydata_atka$index_data$Observation
mydata_atka$catch_data$Catch <- mydata_atka$catch_data$Catch * 1000
mydata_atka$Pyrs <- mydata_atka$Pyrs %>%
  dplyr::filter(Sex == 1) %>%
  dplyr::mutate(Sex = 0 )

# Add in time-varying fishery sel
mydata_atka$fleet_control <- mydata_atka$fleet_control %>%
  dplyr::mutate(Sel_curve_pen1 = Time_varying_sel,
                Sel_curve_pen2 = Sel_sd_prior,
                Time_varying_sel = c(0,1),
                Sel_sd_prior = c(0, sqrt(0.35))) %>%
  dplyr::relocate(Sel_curve_pen1, .after = Nselages) %>%
  dplyr::relocate(Sel_curve_pen2, .after = Sel_curve_pen1)


# Model 1 ----
mydata_atka$sigma_rec_prior <- 0.4723773
bridging_model_1 <- Rceattle::fit_mod(
  data_list = mydata_atka,
  inits = NULL,     # Initial parameters at default
  file = "atka",      # Don't save
  estimateMode = 0, # Estimate
  random_rec = FALSE,# No random recruitment
  msmMode = 0,      # Single species mode
  verbose = 1,
  phase = TRUE,
  initMode = 2) # Tight prior to keep Steepness at 0.8
bridging_model_1$quantities$sel[2,1,,1:10]

# - SAFE model
library(readxl)
SAFE2022_mod <- bridging_model_1
SAFE2022_mod$quantities$biomass[1,1:length(1977:2023)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 4)$Est * 1e3
SAFE2022_mod$quantities$ssb[1,1:length(1977:2023)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 3)$Est * 1e3
SAFE2022_mod$quantities$R[1,1:length(1977:2022)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 2)$Est * 1e6


plot_biomass(list(bridging_model_1, SAFE2022_mod), model_names = c("CEATTLE", "SAFE"))
plot_ssb(list(bridging_model_1, SAFE2022_mod), model_names = c("CEATTLE", "SAFE"))
plot_recruitment(list(bridging_model_1, SAFE2022_mod), model_names = c("CEATTLE", "SAFE"))

# dev.off()
# plot_selectivity(bridging_model_3)

