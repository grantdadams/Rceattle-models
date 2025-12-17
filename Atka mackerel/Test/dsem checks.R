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
  file = paste("dnc", Sys.time()), # Don't save
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


load("~/Documents/GitHub/Rceattle-models/Atka mackerel/DSEM 2025-08-30 10:21:42.514091.RData")
dsem <- mod_objects
load("~/Documents/GitHub/Rceattle-models/Atka mackerel/DSEM 2025-08-30 10:45:59.159801.RData")
dsem_newer <- mod_objects
load("~/Documents/GitHub/Rceattle-models/Atka mackerel/DNC 2025-08-30 10:18:45.823446.RData")
dnc <- mod_objects
plot_biomass(list(dsem, dsem_newer, dnc, Rceattle_atka), model_names = 1:4)

check <- data.frame(Param = names(Rceattle_atka$estimated_params), par = NA, dim = NA, length = NA, map = NA)

for(i in 1:nrow(check)){
  parname <- names(Rceattle_atka$estimated_params)[i]

  check$par[i] <- sum(Rceattle_atka$estimated_params[[parname]] !=  mod_objects$estimated_params[[parname]])
  check$map[i] <- sum(as.character(Rceattle_atka$map$mapFactor[[parname]]) !=  as.character(mod_objects$map$mapFactor[[parname]]), na.rm = TRUE)
  check$dim[i] <- sum(dim(Rceattle_atka$estimated_params[[parname]]) !=  dim(mod_objects$estimated_params[[parname]]))
  check$length[i] <- length(as.numeric(Rceattle_atka$estimated_params[[parname]])) ==  length(as.numeric(mod_objects$estimated_params[[parname]]))
}

