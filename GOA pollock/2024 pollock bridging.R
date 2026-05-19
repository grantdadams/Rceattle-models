# Uses "master" branch
library(Rceattle)
library(dplyr)
library(tidyr)
library(TMB)

# Load data ----
pollock24 <- read_data("Data/GOA_24_pollock_single_species_1970-2024.xlsx")
load("Data/2024pollock.Rdata")

# Fit base model ----
pollock_base <- fit_mod(data_list = pollock24,
                        inits = NULL, # Initial parameters = 0
                        file = NULL, # Don't save
                        estimateMode = 0, # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0, # Single species mode
                        verbose = 1,
                        initMode = 1,
                        phase = TRUE)


# Fit dirichlet model ----
pollock24$fleet_control$Comp_loglike <- 1
pollock24$fleet_control$Catchability[1] <- 6
pollock24$fleet_control$Time_varying_q[1] <- 1
pollock_dm <- fit_mod(data_list = pollock24,
                      inits = NULL, # Initial parameters = 0
                      file = NULL, # Don't save
                      estimateMode = 0, # Estimate
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
pkinits$ln_F[8,] <- fit$parList$mean_log_F + fit$parList$dev_log_F

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
pollock24$fleet_control$Age_max_selected <- NA
pollock24$fleet_control$Age_max_selected[7] <- 3
pollock24$fleet_control$Age_max_selected[8] <- 7
pollock_fixed <- fit_mod(data_list = pollock24,
                         inits = pkinits, # Initial parameters = 0
                         file = NULL, # Don't save
                         estimateMode = 3, # Estimate
                         random_rec = TRUE, # No random recruitment
                         msmMode = 0, # Single species mode
                         verbose = 2,
                         initMode = 1,
                         random_q = 1,
                         phase = TRUE)


pkinits$ln_sel_slp-pollock_fixed$estimated_params$ln_sel_slp
pkinits$ln_sel_slp_dev[1,8,1,]-pollock_fixed$estimated_params$ln_sel_slp_dev[1,8,1,]
pkinits$sel_inf_dev[1,8,1,]-pollock_fixed$estimated_params$sel_inf_dev[1,8,1,]

# -- Selectivity
pollock_fixed$quantities$sel_at_age[1,1,,1]-fit$rep$slctsrv1 # Good

pollock_fixed$quantities$sel_at_age[2,1,,1]-fit$rep$slctsrv2

pollock_fixed$quantities$sel_at_age[3,1,,1]-fit$rep$slctsrv3

pollock_fixed$quantities$sel_at_age[6,1,,1]-fit$rep$slctsrv6

ceattle_sel <- t(pollock_fixed$quantities$sel_at_age[8,1,,1:nyrs])
ceattle_sel - fit$rep$slctfsh


# -- Mort
pollock_fixed$quantities$F_spp[,1:nyrs]-fit$rep$F # Good


# -- Catchability
fit$rep$q1 - pollock_fixed$quantities$index_q[1,]

fit$rep$q2 - pollock_fixed$quantities$index_q[2,]

fit$rep$q3 - pollock_fixed$quantities$index_q[3,]

fit$rep$q6 - pollock_fixed$quantities$index_q[6,]


# Check loglike: ----
# Fishery: 1 = catch, 2 = age-comp fishery, 3 = length-comp fishery (NONE),
# Index 1 (Shelikof): 4 = index, 5 = age comp, 6 = length comp (NONE),
# Index 2 (Bottom trawl): 7 = index, 8 = age comp, 9 = length comp,
# Index 3 (ADFG): 11 = survey index, 12 = age, 13 = length comp (NONE),
# Index 4-5: 14 = age1 index, 15 = age2 index,
# Index 6 (Summer acoustic): 16 = index, 17 = age-comp, 17 = length-comp

# Population: 18 = recruitment deviates
# Penalties: 19 = Selectivity deviate, 21 = Catchability deviates, 19 & 22 = NA,
# Priors: 23 = BT q prior, 23 = Selectivity priors

safell <- -as.numeric(fit$rep$loglik)

nll_comp <- pollock_fixed$quantities$jnll_comp[-c(1,9),-7]
nll_comp <- apply(nll_comp, 2, as.numeric)
rownames(nll_comp) <- rownames(pollock_fixed$quantities$jnll_comp[-c(1,9),])
colnames(nll_comp) <- c(paste0("Index", 1:6), "Fishery")

safe_jnll <- nll_comp
safe_jnll[] <- 0

# Index data
safe_jnll[1,1:6] <- safell[c(4, 7, 11, 14:16)]

# Catch data
safe_jnll[2,7] <- safell[1]

# Comp data
safe_jnll[3,c(1:3,6:7)] <- safell[c(5, 8, 12, 17 , 2)] + safell[c(6, 9, 13, 17 , 3)] * c(1,1,1,0,1)


safe_jnll[10,1] <- safell[18]# Rec dev
safe_jnll[5,7] <- safell[19] # Fishery selectivity deviates
safe_jnll[7,1] <- safell[21] # Catchability deviates (Survey 1 and 3)
safe_jnll[6,2] <- safell[23] # BT q prior N(0.85, 0.1)
nll_comp[7,1] <- sum(nll_comp[7,])
nll_comp[7,2:7] <- 0


safe_jnll[-c(11:18),]
nll_comp[-c(11:18),]



# * Estimate ----
pollock_est <- fit_mod(
  data_list = pollock24,
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
nyrs <- length(pollock_base$data_list$styr:pollock_base$data_list$endyr)
safe$quantities$biomass[,1:nyrs] <- fit$rep$Etotalbio * 1e6
safe$quantities$ssb[,1:nyrs] <- fit$rep$Espawnbio * 1e6
safe$quantities$index_hat <- safe$quantities$index_hat / 1e6
safe$quantities$index_hat[1:(nyrs*6)] <- c(fit$rep$Eindxsurv1, fit$rep$Eindxsurv2, fit$rep$Eindxsurv3, fit$rep$Eindxsurv4, fit$rep$Eindxsurv5, fit$rep$Eindxsurv6) * 1e6

safe$quantities$catch_hat[1:54] - fit$rep$cattot

plot_biomass(list(safe, pollock_base), model_names = c("SAFE", "CEATTLE"))
plot_index(pollock_fixed, model_names = 1:2)
