# Code to run the bering sea pollock model in CEATTLE
# model is a single sex, single-species model
library(Rceattle)
library(dplyr)

# DATA
# - Fishery catch
# - Fishery age composition
# - Fishery weight-at-age
# - Surveys
# -- Bottom trawl (random walk-logistic for age > 1, normal deviates for age = 1), additional penalty on selectivity
# -- AT (age-1 is an index, age > 1 have selectivity smoother)
# - Bottom temperature
# - Survey age composition
# - Catch-at-age methodology
# - Annual length-at-age and weight-at-age from surveys
# - Age at maturity

# MODEL
# - One sex
# - Ricker recruitment (1978-2017) w/ prior on steepness
# - Empirical weight-at-age
# - M = 0.3 for females, estimated for males

# Load data ----
ebs_pollock <- Rceattle::read_data( file = "Data/bsp0.xlsx")
ebs_pollock$estDynamics = 0
ebs_pollock$index_data$Log_sd <- ebs_pollock$index_data$Log_sd/ebs_pollock$index_data$Observation
ebs_pollock$index_data$Observation <- ebs_pollock$index_data$Observation
ebs_pollock$catch_data$Catch <- ebs_pollock$catch_data$Catch

ebs_pollock$catch_data$Log_sd <- 0.05
ebs_pollock$spawn_month = 3
ebs_pollock$fleet_control$Fleet_type[5:6] <- 2 # Setting ATS age-1 data as survey
# ebs_pollock$fleet_control$Estimate_q[3] <- 0 # Bottom trawl q = mean(ob_bts)/mean(eb_bts)
# ebs_pollock$fleet_control$Estimate_q[6] <- 3 # ATS_1 q = mfexp(mean(log(oa1_ats)-log(ea1_ats)));
yrs <- ebs_pollock$styr:ebs_pollock$endyr
ebs_pollock$age_error[1:15,3:17] <- diag(15) # Removing age error b/c turned off
ebs_pollock$fleet_control$Time_varying_sel[1] <- 0

# Adjust survey timing
ebs_pollock$index_data <- ebs_pollock$index_data %>%
  dplyr::mutate(Month = case_when(
    Fleet_name == "BTS" ~ 6,
    Fleet_name == "BTS_1" ~ 6,
    Fleet_name == "ATS" ~ 6,
    Fleet_name == "ATS_1" ~ 6,
    Fleet_name == "AVO" ~ 0,
    Fleet_name == "Fishery CPUE" ~ 0
  ))


ebs_pollock$comp_data <- ebs_pollock$comp_data %>%
  dplyr::mutate(Month = case_when(
    Fleet_name == "BTS" ~ 6,
    Fleet_name == "ATS" ~ 6
  ))

# - Look at the data
plot_data(ebs_pollock)

# - Fix M
pollock_base <- Rceattle::fit_mod(data_list = ebs_pollock,
                                          inits = NULL, # Initial parameters = 0
                                          file = NULL, # Don't save
                                          estimateMode = 0, # Estimate
                                          random_rec = FALSE, # No random recruitment
                                          msmMode = 0, # Single species mode
                                          verbose = 1,
                                          phase = TRUE,
                                          initMode = 2) # Unfished equilibrium with init_dev's turned on

# - Estimate age-invariant M
pollock_estM <- fit_mod(data_list = ebs_pollock,
                        inits = NULL,       # Initial parameters = 0
                        file = NULL,        # Don't save
                        estimateMode = 0,   # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0,        # Single species mode
                        verbose = 1,        # Minimal messages
                        M1Fun = build_M1(M1_model = 1), # Estimate age and time invariant M: see ?build_M1 for more details
                        initMode = 2,       # Unfished equilibrium with init_dev's turned on
                        phase = TRUE)       # Phase


# - Estimate age-invariant M and Ricker SRR
pollock_estM_ricker <- fit_mod(data_list = ebs_pollock,
                               inits = NULL,       # Initial parameters = 0
                               file = NULL,        # Don't save
                               estimateMode = 0,   # Estimate
                               random_rec = FALSE, # No random recruitment
                               msmMode = 0,        # Single species mode
                               verbose = 1,        # Minimal messages
                               M1Fun = build_M1(M1_model = 1), # Estimate age and time invariant M: see ?build_M1 for more details
                               recFun = build_srr(srr_fun = 0, # Default no-stock recruit curve
                                                  srr_pred_fun = 4, # Ricker curve as additional penalty (if srr_fun and srr_pred_fun are the same, no penalty is used)
                                                  srr_est_mode = 1, # Freely estimate alpha
                                                  srr_hat_styr = 1977, # Estimate starting 7 years after styr = 1970
                                                  srr_hat_endyr = 2020
                               ),
                               initMode = 2,       # Unfished equilibrium with init_dev's turned on
                               phase = TRUE)



# - SAFE model
#FIXME: NEED to get from Jim
library(readxl)
SAFE2024_mod <- pollock_base
SAFE2024_mod$quantities$ssb[1,1:length(1964:2024)] <- read_excel("Data/2024_ADMB_estimate.xlsx", sheet = 3)$Est
SAFE2024_mod$quantities$R[1,1:length(1964:2024)] <- read_excel("Data/2024_ADMB_estimate.xlsx", sheet = 2)$Est

plot_ssb(list(pollock_base, SAFE2024_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "SSB", line = 1.8)
plot_recruitment(list(pollock_base, SAFE2024_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Recruitment", line = 1.8)
