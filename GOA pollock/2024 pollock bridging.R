library(Rceattle)
library(dplyr)
library(tidyr)
library(TMB)

# Load data ----
pollock23 <- read_data("Data/Pollock_2023.xlsx")
load("Data/2024pollock.Rdata")


# Update data ----
# * Controls ----
pollock23$endyr <- 2024
pollock23$fleet_control$Fleet_type[4:5] <- 0 # Turn off age-1 indices


# * Catch ----
catch_data <- pollock23$catch_data
catch_data <- catch_data[1,]
catch_data$Year <- pollock23$endyr
pollock23$catch_data <- rbind(pollock23$catch_data , catch_data)
pollock23$catch_data$Catch <- fit$input$dat$cattot


# * Env data ----
pollock23$env_data <- data.frame(Year = fit$input$dat$Ecov_obs_year,
                                 QcovPol = fit$input$dat$Ecov_obs)

# * Comp ----
# (same number in CEATTLE)
# - Age
colnames(fit$obj$env$data$srvp1) <- paste0("Comp_",1:10) # 1
colnames(fit$obj$env$data$srvp2) <- paste0("Comp_",1:10) # 2
colnames(fit$obj$env$data$srvp3) <- paste0("Comp_",1:10) # 3
colnames(fit$obj$env$data$srvp6) <- paste0("Comp_",1:10) # 6
colnames(fit$obj$env$data$catp) <- paste0("Comp_",1:10) # 8

# - Length
colnames(fit$obj$env$data$srvlenp1) <- paste0("Comp_",1:7) # 1
colnames(fit$obj$env$data$srvlenp2) <- paste0("Comp_",1:7) # 2
colnames(fit$obj$env$data$srvlenp3) <- paste0("Comp_",1:7) # 3
colnames(fit$obj$env$data$srvlenp6) <- paste0("Comp_",1:7) # 6
colnames(fit$obj$env$data$lenp) <- paste0("Comp_",1:8) # 8

fsh_acomp <- cbind(data.frame(Year = fit$obj$env$data$fshyrs,
                              Sample_size = fit$obj$env$data$multN_fsh, # * 2,
                              Month = 0,
                              Fleet_code = 8, Age0_Length1 = 0),
                   fit$obj$env$data$catp
)

fsh_lcomp <- cbind(data.frame(Year = fit$obj$env$data$fshlenyrs,
                              Sample_size = fit$obj$env$data$multNlen_fsh, # * 2,
                              Month = 0,
                              Fleet_code = 8, Age0_Length1 = 1),
                   fit$obj$env$data$lenp
)

srv1_acomp <- cbind(data.frame(Year = fit$obj$env$data$srv_acyrs1,
                               Sample_size = fit$obj$env$data$multN_srv1, # * 2,
                               Month = fit$obj$env$data$yrfrct_srv1[1],
                               Fleet_code = 1, Age0_Length1 = 0),
                    fit$obj$env$data$srvp1
)

srv1_lcomp <- cbind(data.frame(Year = fit$obj$env$data$srv_lenyrs1,
                               Sample_size = fit$obj$env$data$multNlen_srv1, # * 3,
                               Month = fit$obj$env$data$yrfrct_srv1[1],
                               Fleet_code = 1, Age0_Length1 = 1),
                    fit$obj$env$data$srvlenp1
)

srv2_acomp <- cbind(data.frame(Year = fit$obj$env$data$srv_acyrs2,
                               Sample_size = fit$obj$env$data$multN_srv2, # * 2,
                               Month = fit$obj$env$data$yrfrct_srv2[1],
                               Fleet_code = 2, Age0_Length1 = 0),
                    fit$obj$env$data$srvp2
)

srv2_lcomp <- cbind(data.frame(Year = fit$obj$env$data$srv_lenyrs2,
                               Sample_size = fit$obj$env$data$multNlen_srv2, # * 3,
                               Month = fit$obj$env$data$yrfrct_srv2[1],
                               Fleet_code = 2, Age0_Length1 = 1),
                    fit$obj$env$data$srvlenp2
)

srv3_acomp <- cbind(data.frame(Year = fit$obj$env$data$srv_acyrs3,
                               Sample_size = fit$obj$env$data$multN_srv3, # * 2,
                               Month = fit$obj$env$data$yrfrct_srv3[1],
                               Fleet_code = 3, Age0_Length1 = 0),
                    fit$obj$env$data$srvp3
)

srv3_lcomp <- cbind(data.frame(Year = fit$obj$env$data$srv_lenyrs3,
                               Sample_size = fit$obj$env$data$multNlen_srv3, # * 3,
                               Month = fit$obj$env$data$yrfrct_srv3[1],
                               Fleet_code = 3, Age0_Length1 = 1),
                    fit$obj$env$data$srvlenp3
)

srv6_acomp <- cbind(data.frame(Year = fit$obj$env$data$srv_acyrs6,
                               Sample_size = fit$obj$env$data$multN_srv6, # * 3,
                               Month = fit$obj$env$data$yrfrct_srv6[1],
                               Fleet_code = 6, Age0_Length1 = 0),
                    fit$obj$env$data$srvp6
)

srv6_lcomp <- cbind(data.frame(Year = fit$obj$env$data$srv_lenyrs6,
                               Sample_size = fit$obj$env$data$multNlen_srv6, # * 3,
                               Month = fit$obj$env$data$yrfrct_srv6[1],
                               Fleet_code = 6, Age0_Length1 = 1),
                    fit$obj$env$data$srvlenp6
)

comp_info <- pollock23$comp_data %>%
  distinct(Fleet_code, Fleet_name, Species, Sex)

comp_data <- comp_info %>%
  full_join(do.call("bind_rows", list(srv1_acomp, srv2_acomp, srv3_acomp, srv6_acomp, fsh_acomp,
                                      srv1_lcomp, srv2_lcomp, srv3_lcomp, srv6_lcomp, fsh_lcomp))) %>%
  dplyr::select(Fleet_name, Fleet_code, Species, Sex, Age0_Length1, Month, Year, Sample_size, paste0("Comp_", 1:10)) %>%
  arrange(Fleet_code, Age0_Length1, Year)

pollock23$comp_data <- comp_data


# * Index data ----
head(pollock23$index_data)
srv1 <- data.frame(Fleet_code = 1, Year = fit$input$dat$srvyrs1, Observation =
                     fit$input$dat$indxsurv1 * 1e6, Log_sd = fit$input$dat$indxsurv_log_sd1)

srv2 <- data.frame(Fleet_code = 2, Year = fit$input$dat$srvyrs2, Observation =
                     fit$input$dat$indxsurv2 * 1e6, Log_sd = fit$input$dat$indxsurv_log_sd2)

srv3 <- data.frame(Fleet_code = 3, Year = fit$input$dat$srvyrs3, Observation =
                     fit$input$dat$indxsurv3 * 1e6, Log_sd = fit$input$dat$indxsurv_log_sd3)

srv4 <- data.frame(Fleet_code = 4, Year = -fit$input$dat$srvyrs4, Observation =
                     fit$input$dat$indxsurv4 * 1e6, Log_sd = fit$input$dat$indxsurv_log_sd4) # Turned off

srv5 <- data.frame(Fleet_code = 5, Year = -fit$input$dat$srvyrs5, Observation =
                     fit$input$dat$indxsurv5 * 1e6, Log_sd = fit$input$dat$indxsurv_log_sd5) # Turned off

srv6 <- data.frame(Fleet_code = 6, Year = fit$input$dat$srvyrs6, Observation =
                     fit$input$dat$indxsurv6 * 1e6, Log_sd = fit$input$dat$indxsurv_log_sd6)

index_data <- pollock23$index_data %>%
  distinct(Fleet_name, Fleet_code, Species, Selectivity_block, Q_block, Month) %>%
  left_join(do.call("rbind", list(srv1, srv2, srv3, srv4, srv5, srv6))) %>%
  dplyr::filter(Fleet_code %in% c(1:6))

pollock23$index_data <- index_data


# * WT ----
colnames(fit$input$dat$wt_srv1) <- paste0("Age",1:10) # 2
colnames(fit$input$dat$wt_srv2) <- paste0("Age",1:10) # 3
colnames(fit$input$dat$wt_srv3) <- paste0("Age",1:10) # 4
colnames(fit$input$dat$wt_srv6) <- paste0("Age",1:10) # 5
colnames(fit$input$dat$wt_fsh) <- paste0("Age",1:10) # 1

fsh_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 1),
                    fit$input$dat$wt_fsh
)

srv1_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 2),
                     fit$input$dat$wt_srv1
)

srv2_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 3),
                     fit$input$dat$wt_srv2
)

srv3_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 4),
                     fit$input$dat$wt_srv3
)

srv6_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 5),
                     fit$input$dat$wt_srv6
)


wt <- pollock23$weight %>%
  group_by(Wt_index) %>%
  slice(1) %>%
  select(Wt_name, Wt_index, Species, Sex) %>%
  full_join(do.call("rbind", list(fsh_weight, srv1_weight, srv2_weight, srv3_weight, srv6_weight))) %>%
  as.data.frame()

pollock23$weight <- wt


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
                        phase = TRUE)


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
                      phase = TRUE)

# Fix parameters ----
pkinits <- build_params(pollock_dm$data_list)
pkinits_old <- build_params(pollock_dm$data_list)
yrs <- pollock_dm$data_list$styr:pollock_dm$data_list$endyr
nyrs <- length(yrs)

# - Recruitment
pkinits$rec_pars[,1] = log(exp(fit$parList$mean_log_recruit)*1e6) # Mean rec
pkinits$rec_dev[,1:nyrs] <- fit$parList$dev_log_recruit
pkinits$init_dev[1,] <- fit$parList$dev_log_recruit[1]
pkinits$R_ln_sd <- log(fit$parList$sigmaR)

# F
pkinits$ln_mean_F[8] <- fit$parList$mean_log_F
pkinits$F_dev[8,] <- fit$parList$dev_log_F

# Selectivity
#1-(1/(1+exp(-exp(fit$parList$log_slp2_srv1) * (1:10 - fit$parList$inf2_srv1))))
# -- Ascending log
pkinits$ln_sel_slp[1,c(2,3,6,8),1] <- c(fit$parList$log_slp1_srv2, fit$parList$log_slp1_srv3, fit$parList$log_slp1_srv6, fit$parList$log_slp1_fsh_mean)
pkinits$sel_inf[1,c(2,3,6,8),1] <- c(fit$parList$inf1_srv2, fit$parList$inf1_srv3, fit$parList$inf1_srv6, fit$parList$inf1_fsh_mean)

# -- Descending log
pkinits$ln_sel_slp[2,c(1,2,6,7,8),1] <- c(fit$parList$log_slp2_srv1, fit$parList$log_slp2_srv2, fit$parList$log_slp2_srv6, fit$parList$log_slp2_srv1, fit$parList$log_slp2_fsh_mean)
pkinits$sel_inf[2,c(1,2,6,7,8),1] <- c(fit$parList$inf2_srv1, fit$parList$inf2_srv2, fit$parList$inf2_srv6, fit$parList$inf2_srv1, fit$parList$inf2_fsh_mean) # Note: survey 7 is mapped with the same selectivity as 1, so seeting inits to the same


# -- Deviates
pkinits$ln_sel_slp_dev[1,8,1,] <- fit$parList$slp1_fsh_dev
pkinits$ln_sel_slp_dev[2,8,1,] <- fit$parList$slp2_fsh_dev

pkinits$sel_inf_dev[1,8,1,] <- fit$parList$inf1_fsh_dev
pkinits$sel_inf_dev[2,8,1,] <- fit$parList$inf2_fsh_dev

# Catchability
pkinits$index_ln_q[1:6] <- unlist(fit$parList[c("log_q1_mean", "log_q2_mean", "log_q3_mean", "log_q4", "log_q5", "log_q6")])

pkinits$index_q_dev[1,] <- fit$parList$Ecov_exp
pkinits$index_q_dev[2,] <- fit$parList$log_q2_dev
pkinits$index_q_dev[3,] <- fit$parList$log_q3_dev

# - Rho
pkinits$index_q_rho[1] <- fit$parList$transf_rho
pkinits$index_q_beta[1,1] <- fit$parList$Ecov_beta
pkinits$index_q_ln_sd[1] <- fit$parList$log_Ecov_obs_sd
pkinits$index_q_dev_ln_sd[1] <- fit$parList$log_Ecov_sd

# DM
pkinits$comp_weights[c(1:3,6,8)] <- fit$parList$log_DM_pars

# * Fit fixed parameters ----
pollock23$fleet_control$Age_max_selected[7] <- 3
pollock23$fleet_control$Age_max_selected[8] <- 7
pollock_fixed <- fit_mod(data_list = pollock23,
                         inits = pkinits, # Initial parameters = 0
                         file = NULL, # Don't save
                         estimateMode = 3, # Estimate
                         random_rec = TRUE, # No random recruitment
                         msmMode = 0, # Single species mode
                         verbose = 2,
                         initMode = 1,
                         random_q = 1,
                         phase = TRUE)


# * Fit fixed parameters w/ pollock issues ----
library(TMB)
pollock_fixed_wrong <- fit_mod(
  data_list = pollock23,
  TMBfilename = "ceattle_v01_11_dev",
  inits = pkinits, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 3, # Estimate
  random_q = TRUE,
  random_rec = TRUE, # No random recruitment
  msmMode = 0, # Single species mode
  verbose = 2,
  initMode = 1,
  phase = TRUE
)
#
# pollock_fixed_wrong <- fit_mod(
#   data_list = pollock23,
#   TMBfilename = "ceattle_v01_11_pk",
#   inits = pollock_fixed_wrong$estimated_params, # Initial parameters = 0
#   file = NULL, # Don't save
#   estimateMode = 0, # Estimate
#   random_q = TRUE,
#   random_rec = TRUE, # No random recruitment
#   msmMode = 0, # Single species mode
#   verbose = 1,
#   initMode = 1,
#   phase = NULL
# )
pkinits$ln_sel_slp-pollock_fixed_wrong$quantities$ln_sel_slp
pkinits$ln_sel_slp_dev[1,8,1,]-pollock_fixed_wrong$quantities$ln_sel_slp_dev[1,8,1,]
pkinits$sel_inf_dev[1,8,1,]-pollock_fixed_wrong$quantities$sel_inf_dev[1,8,1,]

# -- Selectivity
pollock_fixed_wrong$quantities$sel[1,1,,1]-fit$rep$slctsrv1 # Good

pollock_fixed_wrong$quantities$sel[2,1,,1]-fit$rep$slctsrv2

pollock_fixed_wrong$quantities$sel[3,1,,1]-fit$rep$slctsrv3

pollock_fixed_wrong$quantities$sel[6,1,,1]-fit$rep$slctsrv6

ceattle_sel <- t(pollock_fixed_wrong$quantities$sel[8,1,,1:nyrs])
ceattle_sel - fit$rep$slctfsh

# -- Mort
pollock_fixed_wrong$quantities$F_spp[,1:nyrs]-fit$rep$F # Good
fit$obj$env$map$inf1_fsh_mean
fit$obj$env$map$log_slp1_fsh_mean
fit$obj$env$map$inf2_fsh_dev
fit$obj$env$map$slp2_fsh_dev

# -- Catchability
fit$rep$q1 - pollock_fixed_wrong$quantities$index_q[1,]

fit$rep$q2 - pollock_fixed_wrong$quantities$index_q[2,]

fit$rep$q3 - pollock_fixed_wrong$quantities$index_q[3,]

fit$rep$q6 - pollock_fixed_wrong$quantities$index_q[6,]


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

safell <- -as.numeric(fit$rep$loglik)

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
  TMBfilename = "ceattle_v01_11_dev",
  inits = pkinits, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_q = TRUE,
  random_rec = TRUE, # No random recruitment
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 1,
  phase = TRUE
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
  verbose = 1,
  initMode = 1,
  phase = FALSE
)

# Plot ----
safe <- pollock_base
nyrs <- length(1970:2024)
safe$quantities$biomass[,1:nyrs] <- fit$rep$Etotalbio * 1e6
safe$quantities$ssb[,1:nyrs] <- fit$rep$Espawnbio * 1e6
safe$quantities$srv_bio_hat <- fit$rep$Eindxsurv1

plot_biomass(list(safe, pollock_fixed_wrong, pollock_est_wrong), model_names = c("SAFE", "CEATTLE"))
plot_index(pollock_fixed, model_names = 1:2)

write_data(pollock23, "Data/GOA_24_pollock_single_species_1970-2024.xlsx")
