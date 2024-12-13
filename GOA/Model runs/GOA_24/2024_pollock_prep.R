library(Rceattle)
library(dplyr)
library(tidyr)
library(TMB)
# setwd("Model runs/GOA_24")

# Load data ----
pollock23 <- read_data("Data/GOA_23_pollock_single_species_1970-2023.xlsx")
pollock23$fleet_control <- pollock23$fleet_control %>%
  select(-c(Accumatation_age_upper, Accumatation_age_lower)) %>%
  mutate(Comp_loglike = 0,
         Age_max_selected = c(3, 10, 10, NA, NA, 1, NA, 7))
safe24 <- readRDS("Data/SAFE data/fit.RDS")


# Update data ----
# * Controls ----
pollock23$endyr <- 2024
pollock23$fleet_control$Fleet_type[4:5] <- 0


# * Catch ---- 
fsh_biom <- pollock23$fsh_biom
fsh_biom <- fsh_biom[1,]
fsh_biom$Year <- pollock23$endyr 
pollock23$fsh_biom <- rbind(pollock23$fsh_biom , fsh_biom)
pollock23$fsh_biom$Catch <- safe24$input$dat$cattot


# * Env data ----
pollock23$env_data <- data.frame(Year = safe24$input$dat$Ecov_obs_year,
                                 QcovPol = safe24$input$dat$Ecov_obs)

# * Comp ----
# (same number in CEATTLE)
colnames(safe24$input$dat$srvp1) <- paste0("Comp_",1:10) # 1
colnames(safe24$input$dat$srvp2) <- paste0("Comp_",1:10) # 2
colnames(safe24$input$dat$srvp3) <- paste0("Comp_",1:10) # 3
colnames(safe24$input$dat$srvp6) <- paste0("Comp_",1:10) # 6
colnames(safe24$input$dat$catp) <- paste0("Comp_",1:10) # 8

fsh_acomp <- cbind(data.frame(Year = safe24$input$dat$fshyrs, 
                              Sample_size = safe24$input$dat$multN_fsh, # * 2, 
                              Fleet_code = 8, Age0_Length1 = 0),
                   safe24$input$dat$catp
)

srv1_acomp <- cbind(data.frame(Year = safe24$input$dat$srv_acyrs1, 
                               Sample_size = safe24$input$dat$multN_srv1, # * 2,
                               Fleet_code = 1, Age0_Length1 = 0),
                    safe24$input$dat$srvp1
)

srv2_acomp <- cbind(data.frame(Year = safe24$input$dat$srv_acyrs2, 
                               Sample_size = safe24$input$dat$multN_srv2, # * 2,
                               Fleet_code = 2, Age0_Length1 = 0),
                    safe24$input$dat$srvp2
)

srv3_acomp <- cbind(data.frame(Year = safe24$input$dat$srv_acyrs3, 
                               Sample_size = safe24$input$dat$multN_srv3, # * 2,
                               Fleet_code = 3, Age0_Length1 = 0),
                    safe24$input$dat$srvp3
)

srv6_acomp <- cbind(data.frame(Year = safe24$input$dat$srv_acyrs6, 
                               Sample_size = safe24$input$dat$multN_srv6, # * 3,
                               Fleet_code = 6, Age0_Length1 = 0),
                    safe24$input$dat$srvp6
)

comp_data <- pollock23$comp_data %>%
  dplyr::filter(Fleet_code %in% c(1:3, 6, 8) & Age0_Length1 == 0) %>%
  group_by(Fleet_code) %>%
  slice(1)
comp_data <- comp_data %>%
  select(Fleet_name, Fleet_code, Species, Sex, Age0_Length1, Month) %>%
  full_join(do.call("rbind", list(srv1_acomp, srv2_acomp, srv3_acomp, srv6_acomp, fsh_acomp)))

comp_data <- comp_data %>%
  bind_rows(
    pollock23$comp_data %>%
      dplyr::filter(!Fleet_code %in% c(1:3, 6, 8)) %>%
      dplyr::select(Fleet_name, Fleet_code, Species, Sex, Age0_Length1, Month, Year, Sample_size, paste0("Comp_", 1:10))
  ) %>%
  arrange(Fleet_code, Year) %>%
  as.data.frame()

pollock23$comp_data <- comp_data


# * Index data ----
head(pollock23$srv_biom)
srv1 <- data.frame(Fleet_code = 1, Year = safe24$input$dat$srvyrs1, Observation = 
                     safe24$input$dat$indxsurv1 * 1e6, Log_sd = safe24$input$dat$indxsurv_log_sd1)


srv2 <- data.frame(Fleet_code = 2, Year = safe24$input$dat$srvyrs2, Observation = 
                     safe24$input$dat$indxsurv2 * 1e6, Log_sd = safe24$input$dat$indxsurv_log_sd2)

srv3 <- data.frame(Fleet_code = 3, Year = safe24$input$dat$srvyrs3, Observation = 
                     safe24$input$dat$indxsurv3 * 1e6, Log_sd = safe24$input$dat$indxsurv_log_sd3)

srv6 <- data.frame(Fleet_code = 6, Year = safe24$input$dat$srvyrs6, Observation = 
                     safe24$input$dat$indxsurv6 * 1e6, Log_sd = safe24$input$dat$indxsurv_log_sd6)

srv_biom <- pollock23$srv_biom %>%
  dplyr::filter(Fleet_code %in% c(1:3, 6)) %>%
  select(Fleet_name, Fleet_code, Species, Year, Month, Selectivity_block, Q_block) %>%
  left_join(do.call("rbind", list(srv1, srv2, srv3, srv6))) %>%
  bind_rows(pollock23$srv_biom %>%
              dplyr::filter(!Fleet_code %in% c(1:3, 6)))

pollock23$srv_biom <- srv_biom


# * WT ----
colnames(safe24$input$dat$wt_srv1) <- paste0("Age",1:10) # 2
colnames(safe24$input$dat$wt_srv2) <- paste0("Age",1:10) # 3
colnames(safe24$input$dat$wt_srv3) <- paste0("Age",1:10) # 4
colnames(safe24$input$dat$wt_srv6) <- paste0("Age",1:10) # 5
colnames(safe24$input$dat$wt_fsh) <- paste0("Age",1:10) # 1

fsh_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 1),
                    safe24$input$dat$wt_fsh
)

srv1_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 2),
                     safe24$input$dat$wt_srv1
)

srv2_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 3),
                     safe24$input$dat$wt_srv2
)

srv3_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 4),
                     safe24$input$dat$wt_srv3
)

srv6_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 5),
                     safe24$input$dat$wt_srv6
)


wt <- pollock23$wt %>%
  group_by(Wt_index) %>%
  slice(1) %>%
  select(Wt_name, Wt_index, Species, Sex) %>%
  full_join(do.call("rbind", list(fsh_weight, srv1_weight, srv2_weight, srv3_weight, srv6_weight))) %>%
  as.data.frame()

pollock23$wt <- wt


# * Pyrs ----
tail(pollock23$Pyrs)
pyrs_new <- data.frame(Species = 1, Sex = 0, Year = (max(pollock23$Pyrs$Year)+1):2024)
pyrs_new <- pyrs_new %>% cbind(pollock23$Pyrs %>% 
                                 dplyr::slice(n()) %>%
                                 dplyr::select(paste0("Age",1:10)))

pollock23$Pyrs <- rbind(pollock23$Pyrs %>%
                          dplyr::select(Species, Sex, Year, paste0("Age",1:10)), 
                        pyrs_new)



# Fit base model ----
pollock_base <- fit_mod(data_list = pollock23,
                        inits = NULL, # Initial parameters = 0
                        file = NULL, # Don't save
                        estimateMode = 0, # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0, # Single species mode
                        verbose = 1,
                        initMode = 1,
                        phase = "default")

# Fit dirichlet model ----
pollock23$fleet_control$Comp_loglike <- 0
pollock23$fleet_control$Estimate_q[1] <- 6
pollock23$fleet_control$Time_varying_q[1] <- 1
pollock_dm <- fit_mod(data_list = pollock23,
                      inits = NULL, # Initial parameters = 0
                      file = NULL, # Don't save
                      estimateMode = 3, # Estimate
                      random_rec = FALSE, # No random recruitment
                      msmMode = 0, # Single species mode
                      verbose = 1,
                      initMode = 1,
                      phase = "default")

# Fix parameters ----
pkinits <- build_params(pollock_dm$data_list)
yrs <- pollock_dm$data_list$styr:pollock_dm$data_list$endyr
nyrs <- length(yrs)

# - Recruitment
pkinits$rec_pars[,1] = log(exp(safe24$parList$mean_log_recruit)*1e6) # Mean rec
pkinits$rec_dev[,1:nyrs] <- safe24$parList$dev_log_recruit
pkinits$init_dev[1,] <- safe24$parList$dev_log_recruit[1]
pkinits$ln_rec_sigma <- log(safe24$parList$sigmaR)

# F
pkinits$ln_mean_F[8] <- safe24$parList$mean_log_F
pkinits$F_dev[8,] <- safe24$parList$dev_log_F

# Selectivity
#1-(1/(1+exp(-exp(safe24$parList$log_slp2_srv1) * (1:10 - safe24$parList$inf2_srv1))))
# -- Ascending log
pkinits$ln_sel_slp[1,c(2,3,6,8),1] <- c(safe24$parList$log_slp1_srv2, safe24$parList$log_slp1_srv3, safe24$parList$log_slp1_srv6, safe24$parList$log_slp1_fsh_mean)
pkinits$sel_inf[1,c(2,3,6,8),1] <- c(safe24$parList$inf1_srv2, safe24$parList$inf1_srv3, safe24$parList$inf1_srv6, safe24$parList$inf1_fsh_mean)

# -- Descending log
pkinits$ln_sel_slp[2,c(1,2,6,7,8),1] <- c(safe24$parList$log_slp2_srv1, safe24$parList$log_slp2_srv2, safe24$parList$log_slp2_srv6, safe24$parList$log_slp2_srv1, safe24$parList$log_slp2_fsh_mean)
pkinits$sel_inf[2,c(1,2,6,7,8),1] <- c(safe24$parList$inf2_srv1, safe24$parList$inf2_srv2, safe24$parList$inf2_srv6, safe24$parList$inf2_srv1, safe24$parList$inf2_fsh_mean) # Note: survey 7 is mapped with the same selectivity as 1, so seeting inits to the same


# -- Deviates
pkinits$ln_sel_slp_dev[1,8,1,] <- safe24$parList$slp1_fsh_dev
pkinits$ln_sel_slp_dev[2,8,1,] <- safe24$parList$slp2_fsh_dev

pkinits$sel_inf_dev[1,8,1,] <- safe24$parList$inf1_fsh_dev
pkinits$sel_inf_dev[2,8,1,] <- safe24$parList$inf2_fsh_dev

# Catchability
pkinits$ln_srv_q[1:6] <- unlist(safe24$parList[c("log_q1_mean", "log_q2_mean", "log_q3_mean", "log_q4", "log_q5", "log_q6")])

pkinits$ln_srv_q_dev[1,] <- safe24$parList$Ecov_exp
pkinits$ln_srv_q_dev[2,] <- safe24$parList$log_q2_dev
pkinits$ln_srv_q_dev[3,] <- safe24$parList$log_q3_dev

# - Rho
pkinits$srv_q_rho[1] <- safe24$parList$transf_rho
pkinits$srv_q_beta[1,1] <- safe24$parList$Ecov_beta
pkinits$ln_sigma_srv_q[1] <- safe24$parList$log_Ecov_obs_sd
pkinits$ln_sigma_time_varying_srv_q[1] <- safe24$parList$log_Ecov_sd

# DM
pkinits$comp_weights[c(1:3,6,8)] <- safe24$parList$log_DM_pars

# * Fit fixed parameters ----
pollock_fixed <- fit_mod(data_list = pollock23,
                         inits = pkinits, # Initial parameters = 0
                         file = NULL, # Don't save
                         estimateMode = 3, # Estimate
                         random_rec = TRUE, # No random recruitment
                         msmMode = 0, # Single species mode
                         verbose = 1,
                         initMode = 1,
                         random_q = 1,
                         phase = NULL)

library(TMB)
pollock23$fleet_control$Age_max_selected[8] <- 7
pollock_fixed_wrong <- fit_mod(
  data_list = pollock23,
  TMBfilename = "ceattle_v01_11_pk",
  inits = pkinits, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_q = TRUE,
  random_rec = TRUE, # No random recruitment
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 1,
  phase = "default"
)

pollock_fixed_wrong <- fit_mod(
  data_list = pollock23,
  TMBfilename = "ceattle_v01_11_pk",
  inits = pollock_fixed_wrong$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_q = TRUE,
  random_rec = TRUE, # No random recruitment
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 1,
  phase = NULL
)
pkinits$ln_sel_slp
pollock_fixed_wrong$quantities$ln_sel_slp

# -- Selectivity
pollock_fixed_wrong$quantities$sel[1,1,,1]
safe24$rep$slctsrv1 # Good

pollock_fixed_wrong$quantities$sel[2,1,,1]
safe24$rep$slctsrv2

pollock_fixed_wrong$quantities$sel[3,1,,1]
safe24$rep$slctsrv3

pollock_fixed_wrong$quantities$sel[6,1,,1]
safe24$rep$slctsrv6

ceattle_sel <- t(pollock_fixed_wrong$quantities$sel[8,1,,1:nyrs])
ceattle_sel - safe24$rep$slctfsh

# -- Mort
pollock_fixed_wrong$quantities$F_spp[,1:nyrs]
safe24$rep$F # Good

# -- Catchability
safe24$rep$q1 - pollock_fixed_wrong$quantities$srv_q[1,]

safe24$rep$q2 - pollock_fixed_wrong$quantities$srv_q[2,]

safe24$rep$q3 - pollock_fixed_wrong$quantities$srv_q[3,]

safe24$rep$q6 - pollock_fixed_wrong$quantities$srv_q[6,]


# Loglike:
# Fishery: 1 = catch, 2 = age-comp fishery, 3 = length-comp fishery, 
# Index 1 (Shelikof): 4 = index, 5 = age comp, 6 = length comp, 
# Index 2 (Bottom trawl): 7 = index, 8 = age comp, 9 = length comp, 
# Index 3 (ADFG): 10 = survey index, 11 = age, 12 = length comp, 
# Index 4-5: 14 = age1 index, 15 = age2 index, UNUSED
# Index 6 (Summer acoustic): 15 = index, 16 = age-comp, 17 = length-comp

# Population: 18 = recruitment deviates
# Penalties: 19 = Selectivity deviate, 20 = Catchability deviates, 21 = NA, 22 = BT q prior, 23 = Selectivity priors

# Random effects: 24 = Q-devs for env process (process error), 25 = fit to environmental index

safell <- -as.numeric(safe24$rep$loglik)

safe_jnll <- pollock_fixed_wrong$quantities$jnll_comp
safe_jnll <- safe_jnll[1:13,-c(4,5,7)]
safe_jnll[] <- 0
safe_jnll[1,1:4] <- safell[c(4, 7, 10, 15)]
safe_jnll[2,5] <- safell[1]
safe_jnll[3,1:5] <- safell[c(5, 8, 11, 16 , 2)] + safell[c(5, 8, 11, 16 , 2)+1]
safe_jnll[11,1] <- safell[18]
safe_jnll[6,5] <- safell[19] # Selectivity deviate
safe_jnll[9,3] <- safell[20]
safe_jnll[8,1] <- safell[22] # BT prio
safe_jnll[8,1] <- safell[24] 
safe_jnll[9,1] <- safell[25]

safe_jnll
pollock_fixed_wrong$quantities$jnll_comp[1:13,-c(4,5,7)]


# * Estimate
pollock_est_wrong <- fit_mod(
  data_list = pollock23,
  TMBfilename = "ceattle_v01_11_pk",
  inits = pkinits, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_q = TRUE,
  random_rec = TRUE, # No random recruitment
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 1,
  phase = "default"
)

# * Estimate
pollock_est <- fit_mod(
  data_list = pollock23,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_q = TRUE,
  random_rec = TRUE, # No random recruitment
  msmMode = 0, # Single species mode
  verbose = 2,
  initMode = 2,
  phase = "default"
)

# Plot ----
safe <- pollock_base
nyrs <- length(1970:2024)
safe$quantities$biomass[,1:nyrs] <- safe24$rep$Etotalbio * 1e6
safe$quantities$biomassSSB[,1:nyrs] <- safe24$rep$Espawnbio * 1e6
safe$quantities$srv_bio_hat <- safe24$rep$Eindxsurv1 

plot_biomass(list(safe, pollock_est_wrong), model_names = c("SAFE", "CEATTLE"))
plot_index(pollock_base, model_names = 1:2)

write_data(pollock23, "Data/GOA_24_pollock_single_species_1970-2024.xlsx")
