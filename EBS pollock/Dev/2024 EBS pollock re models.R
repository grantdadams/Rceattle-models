# Code to run the bering sea pollock model in CEATTLE
# model is a single sex, single-species model


library(Rceattle) # https://github.com/grantdadams/Rceattle/tree/dev-name-change

# Load data ----
bsp <- Rceattle::read_data( file = here::here("runs","ceattle", "bsp0.xlsx") )
bsp$estDynamics = 0

# - Rescale data
bsp$index_data$Log_sd <- bsp$index_data$Log_sd/bsp$index_data$Observation
bsp$index_data$Observation <- bsp$index_data$Observation * 1000
bsp$catch_data$Catch <- bsp$catch_data$Catch * 1000
bsp$catch_data$Log_sd <- 0.05

bsp$fleet_control$Fleet_type[5:6] <- 2       # Setting ATS age-1 data as survey
# bsp$fleet_control$Estimate_q[3] <- 0     # Bottom trawl q = mean(ob_bts)/mean(eb_bts)
# bsp$fleet_control$Estimate_q[6] <- 3     # ATS_1 q = mfexp(mean(log(oa1_ats)-log(ea1_ats)));
yrs <- bsp$styr:bsp$endyr
bsp$age_error[1:15,3:17] <- diag(15)       # Removing age error b/c turned off
bsp$fleet_control$Time_varying_sel[1] <- 1 # Adding time-varying sel

# Adjust survey timing
bsp$spawn_month = 3
bsp$index_data <- bsp$index_data %>%
  dplyr::mutate(Month = case_when(
    Fleet_name == "BTS" ~ 6,
    Fleet_name == "BTS_1" ~ 6,
    Fleet_name == "ATS" ~ 6,
    Fleet_name == "ATS_1" ~ 6,
    Fleet_name == "AVO" ~ 0,
    Fleet_name == "Fishery CPUE" ~ 0
  ))


bsp$comp_data <- bsp$comp_data %>%
  dplyr::mutate(Month = case_when(
    Fleet_name == "BTS" ~ 6,
    Fleet_name == "ATS" ~ 6
  ))

# Fit ----
pollock_init4 <- Rceattle::fit_mod(data_list = bsp,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = TRUE, # Laplace approximation and estimate sigR
                                  random_sel = TRUE, # Laplace approximation and estimate sigSel
                                  msmMode = 0, # Single species mode
                                  verbose = 1,
                                  phase = FALSE,
                                  initMode = 4) # Fished equilibrium with init_dev's turned on version 2 (cumsum(M_a) + Finit)


pollock_init3 <- Rceattle::fit_mod(data_list = bsp,
                                   inits = NULL, # Initial parameters from default
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = TRUE, # Laplace approximation and estimate sigR
                                   random_sel = TRUE, # Laplace approximation and estimate sigSel
                                   msmMode = 0, # Single species mode
                                   verbose = 1,
                                   phase = FALSE,
                                   initMode = 3) # Fished equilibrium with init_dev's turned on version 1 (cumsum(M_a+Finit))
