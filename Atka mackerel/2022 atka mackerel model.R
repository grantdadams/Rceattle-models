# Code to run the atka mackerel assessment in CEATTLE
# model is a single sex, single-species model

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
library(TMB)
mydata_atka <- Rceattle::read_data( file = "Data/atka_single_species_2022.xlsx")
mydata_atka$estDynamics = 0
mydata_atka$index_data$Log_sd <- mydata_atka$index_data$Log_sd/mydata_atka$index_data$Observation
# mydata_atka$catch_data$Catch <- mydata_atka$catch_data$Catch * 1000

# Adjust months (AMAK does month - 1)
mydata_atka$spawn_month <- 7
mydata_atka$index_data$Month <- 6.5
mydata_atka$Pyrs <- mydata_atka$Pyrs %>%
  dplyr::filter(Sex == 1) %>%
  dplyr::mutate(Sex = 0 )

# Prior for q
mydata_atka$fleet_control$Estimate_q[1] <- 2   # Estimate with prior
mydata_atka$fleet_control$Q_prior[1] <- 1      # Prior mean
mydata_atka$fleet_control$Q_sd_prior[1] <- 0.2 # SD of prior

# Add in time-varying fishery sel
mydata_atka$fleet_control <- mydata_atka$fleet_control %>%
  dplyr::mutate(Sel_curve_pen1 = Time_varying_sel,
                Sel_curve_pen2 = Sel_sd_prior,
                Time_varying_sel = c(0,1),
                Sel_sd_prior = c(0, 0.35),
                Sel_curve_pen1 = 1/(2 * Sel_curve_pen1^2), # AMAK conversion (TODO, convert CEATTLE to variance)
                Sel_curve_pen2 = 1/Sel_curve_pen2^2)  %>%  # AMAK conversion
  dplyr::relocate(Sel_curve_pen1, .after = Nselages) %>%
  dplyr::relocate(Sel_curve_pen2, .after = Sel_curve_pen1)

# Selectivity normalization ages for survey
mydata_atka$fleet_control$Age_max_selected[1] <- 4
mydata_atka$fleet_control$Age_max_selected_upper <- NA
mydata_atka$fleet_control$Age_max_selected_upper[1] <- 10


# Fit model ----
mydata_atka$sigma_rec_prior <- 0.4723773
Rceattle_atka <- Rceattle::fit_mod(
  data_list = mydata_atka,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_rec = TRUE, # No random recruitment
  msmMode = 0, # Single species mode
  verbose = 1,
  phase = FALSE,
  initMode = 2,
  recFun = build_srr(srr_pred_fun = 2,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 2,    # Prior on steepness
                     srr_hat_styr = 1977, # Years for prior
                     srr_hat_endyr = 2019,
                     srr_prior = 0.8,
                     srr_prior_sd = 0.0001)
)


# SAFE model ----
library(readxl)
SAFE2022_mod <- Rceattle_atka
SAFE2022_mod$quantities$biomass[1,1:length(1977:2023)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 4)$Est
SAFE2022_mod$quantities$ssb[1,1:length(1977:2023)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 3)$Est
SAFE2022_mod$quantities$R[1,1:length(1977:2022)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 2)$Est * 1000


plot_biomass(list(Rceattle_atka, SAFE2022_mod), model_names = c("CEATTLE", "SAFE"))
plot_ssb(list(Rceattle_atka, SAFE2022_mod), model_names = c("CEATTLE", "SAFE"))
plot_recruitment(list(Rceattle_atka, SAFE2022_mod), model_names = c("CEATTLE", "SAFE"))

