# Code to run the atka mackerel assessment in CEATTLE
# model is a single sex, single-species mode
# uses "dev" branch

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
library(dplyr)
library(TMB)
mydata_atka <- Rceattle::read_data( file = "Data/atka_single_species_2022.xlsx")
mydata_atka$estDynamics = 0
mydata_atka$index_data$Log_sd <- mydata_atka$index_data$Log_sd/mydata_atka$index_data$Observation
# mydata_atka$catch_data$Catch <- mydata_atka$catch_data$Catch * 1000

# Adjust months (AMAK does month - 1)
mydata_atka$spawn_month <- 7
mydata_atka$index_data$Month <- 6.5
mydata_atka$ration_data <- mydata_atka$ration_data %>%
  dplyr::filter(Sex == 1) %>%
  dplyr::mutate(Sex = 0 )

# Prior for q
mydata_atka$fleet_control$Catchability[1] <- 2   # Estimate with prior
mydata_atka$fleet_control$Catchability_init[1] <- 1      # Prior mean
mydata_atka$fleet_control$Catchability_prior_sd[1] <- 0.2 # SD of prior

# Add in time-varying fishery sel
mydata_atka$fleet_control <- mydata_atka$fleet_control %>%
  dplyr::mutate(
    # The workbook's Time_varying_sel column holds the ADMB curvature sd and
    # Time_varying_sel_sd the decreasing-penalty sd, not switch values. Each
    # becomes the weight Rceattle reads, and they go to opposite columns from
    # the ones they are named after: Sel_curve_pen1 is the DECREASING weight
    # (amak.tpl 2531, 0.5*d^2/seldec_pen, over a seldec_pen squared on input at
    # line 615) and Sel_curve_pen2 the CURVATURE weight (amak.tpl 948).
    # Reproduces Data/mod23/input.log: Curv_pen 2 / 0.558712.
    Sel_curve_pen1 = 0.5 / (Time_varying_sel_sd^2)^2,
    Sel_curve_pen2 = 1 / (2 * Time_varying_sel^2),
    Time_varying_sel = c(0,1),
    Time_varying_sel_sd = c(0, 0.35)) %>%
  dplyr::relocate(Sel_curve_pen1, .after = N_sel_bins) %>%
  dplyr::relocate(Sel_curve_pen2, .after = Sel_curve_pen1)

# Selectivity normalization ages for survey
mydata_atka$fleet_control$Sel_norm_bin <- NA
mydata_atka$fleet_control$Sel_norm_bin[1] <- 4
mydata_atka$fleet_control$Sel_norm_bin_upper <- NA
mydata_atka$fleet_control$Sel_norm_bin_upper[1] <- 10


# Fit model ----
mydata_atka$sigma_rec <- 0.4723773
Rceattle_atka <- Rceattle::fit_mod(
  data_list = mydata_atka,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_rec = TRUE, # No random recruitment
  msmMode = 0, # Single species mode
  verbose = 1,
  phase = FALSE,
  initMode = 1,
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

