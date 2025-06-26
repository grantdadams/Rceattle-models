library(Rceattle)
library(dplyr)
library(tidyr)
library(TMB)


# Fishery: double logistic selectivity
# Index 1 (Shelikof): Descending logistic, time-varying q (random walk)
# Index 2 (Bottom trawl): 7 = index, 8 = age comp, 9 = length comp,
# Index 3 (ADFG): 11 = survey index, 12 = age, 13 = length comp (NONE),
# Index 4-5: 14 = age1 index, 15 = age2 index,
# Index 6 (Summer acoustic): 16 = index, 17 = age-comp, 17 = length-comp

# Load 2023 model ----
load("Data/Pollock23.Rdata")
parList <- fit$obj$env$parList(fit$obj$env$last.par.best)
# HEAD "a506609"

# Load data ----
pollock23 <- read_data("Data/GOA_23_pollock_single_species_1970-2023.xlsx")
pollock23$index_data$Observation <- pollock23$index_data$Observation * 1e6
pollock23$fleet_control$Q_sd_prior[2] = 0.1
pollock23$fleet_control$Q_prior[2] <- 0.85

pollock23$fleet_control <- pollock23$fleet_control %>%
  select(-c(Accumatation_age_upper, Accumatation_age_lower)) %>%
  mutate(Comp_loglike = -1,
         Age_max_selected = c(3, 10, 10, NA, NA, 1, NA, 7))

pollock23$catch_data$Catch <- fit$obj$env$data$cattot

# * Expand survey data ----
index_expanded <- pollock23$index_data %>%
  dplyr::distinct(Fleet_name, Fleet_code, Species, Month) %>%
  dplyr::cross_join(data.frame(Year = pollock23$styr:pollock23$endyr))

index_data <- pollock23$index_data %>%
  dplyr::full_join(index_expanded) %>%
  dplyr::arrange(Fleet_code, Year) %>%
  dplyr::mutate(Selectivity_block = 1,
                Q_block = 1,
                Year = ifelse(is.na(Observation), - Year, Year),
                Observation = ifelse(is.na(Observation), NA, Observation),
                Log_sd = ifelse(is.na(Log_sd), NA, Log_sd))
pollock23$index_data <- index_data


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

write_data(pollock23, file = "Data/Pollock_2023.xlsx")


# Fit base model ----
pollock_base <- fit_mod(data_list = pollock23,
                        inits = NULL, # Initial parameters = 0
                        file = NULL, # Don't save
                        estimateMode = 0, # Estimate
                        random_q = FALSE,
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0, # Single species mode
                        verbose = 1,
                        initMode = 1,
                        loopnum = 4,
                        newtonsteps = 4,
                        control = list(eval.max=10000, iter.max=10000, trace=100),
                        phase = TRUE)

# SAFE inits ----
inits <- pollock_base$initial_params
inits$rec_pars[1,1] <- log(exp(1.1) * 1e6)
inits$ln_sel_slp[1:2,8,1] <- c(0.77, 0.93)
inits$sel_inf[1:2,8,1] <- c(3.74, 9.70)

inits$ln_sel_slp[2,1,1] <- 0.53 # log_slp2_srv1
inits$sel_inf[2,1,1] <- 9.80 # inf2_srv1

inits$ln_sel_slp[2,2,1] <- -0.46 # log_slp1_srv2
inits$sel_inf[2,2,1] <- 4.07 # inf1_srv2
inits$ln_sel_slp[2,2,1] <- 1 # log_slp2_srv2
inits$sel_inf[2,2,1] <- 20 # inf2_srv2
inits$ln_sel_slp[2,3,1] <- 0.46 # log_slp1_srv3
inits$sel_inf[2,3,1] <- 4.37 # inf1_srv3


inits$ln_sel_slp[2,2,1] <- 4.9 # log_slp1_srv6
inits$sel_inf[2,2,1] <- 0.5 # inf1_srv6
inits$ln_sel_slp[2,2,1] <- 0.24 # log_slp2_srv6
inits$sel_inf[2,2,1] <- 7.87 # inf2_srv6

inits$ln_F[8,] <- -1.97 # mean_log_F
inits$index_ln_q[1:6] <- c(-0.53, -0.20, -1.54, -1.20, -1.08, -0.26)

pollock_inits <- fit_mod(data_list = pollock23,
                        inits = inits, # Initial parameters = 0
                        file = NULL, # Don't save
                        estimateMode = 0, # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0, # Single species mode
                        verbose = 1,
                        initMode = 1,
                        phase = FALSE)
pollock_inits$opt$objective


# Fix parameters ----
pkest <- pollock_base$estimated_params
pkinits <- build_params(pollock_base$data_list)
yrs <- pollock_base$data_list$styr:pollock_base$data_list$endyr
nyrs <- length(yrs)

# - Recruitment
pkinits$rec_pars[,1] = log(exp(parList$mean_log_recruit)*1e6) # Mean rec
pkinits$rec_dev[,1:nyrs] <- parList$dev_log_recruit
pkinits$init_dev[1,] <- 0 # parList$dev_log_recruit[1]
pkinits$R_ln_sd <- log(parList$sigmaR)

# F
pkinits$ln_F[8,] <- parList$mean_log_F + parList$dev_log_F

# Selectivity
#1-(1/(1+exp(-exp(parList$log_slp2_srv1) * (1:10 - parList$inf2_srv1))))
# -- Ascending log
pkinits$ln_sel_slp[1,c(2,3,6,8),1] <- c(parList$log_slp1_srv2, parList$log_slp1_srv3, parList$log_slp1_srv6, parList$log_slp1_fsh_mean)
pkinits$sel_inf[1,c(2,3,6,8),1] <- c(parList$inf1_srv2, parList$inf1_srv3, parList$inf1_srv6, parList$inf1_fsh_mean)

# -- Descending log
pkinits$ln_sel_slp[2,c(1,2,6,7,8),1] <- c(parList$log_slp2_srv1, parList$log_slp2_srv2, parList$log_slp2_srv6, parList$log_slp2_srv1, parList$log_slp2_fsh_mean)
pkinits$sel_inf[2,c(1,2,6,7,8),1] <- c(parList$inf2_srv1, parList$inf2_srv2, parList$inf2_srv6, parList$inf2_srv1, parList$inf2_fsh_mean) # Note: survey 7 is mapped with the same selectivity as 1, so seeting inits to the same


# -- Deviates
pkinits$ln_sel_slp_dev[1,8,1,] <- parList$slp1_fsh_dev
pkinits$ln_sel_slp_dev[2,8,1,] <- parList$slp2_fsh_dev

pkinits$sel_inf_dev[1,8,1,] <- parList$inf1_fsh_dev
pkinits$sel_inf_dev[2,8,1,] <- parList$inf2_fsh_dev

# Catchability
pkinits$index_ln_q[1:6] <- unlist(parList[c("log_q1_mean", "log_q2_mean", "log_q3_mean", "log_q4", "log_q5", "log_q6")])
pkinits$index_q_dev[1,] <- parList$log_q1_dev
pkinits$index_q_dev[2,] <- parList$log_q2_dev
pkinits$index_q_dev[3,] <- parList$log_q3_dev

# * Fit fixed parameters ----
pollock_fixed <- fit_mod(data_list = pollock23,
                         inits = pkinits, # Initial parameters = 0
                         file = NULL, # Don't save
                         TMBfilename = "src/ceattle_v01_11_pk23", # Altered likelihoods
                         estimateMode = 3, # Estimate
                         random_rec = TRUE, # No random recruitment
                         msmMode = 0, # Single species mode
                         verbose = 2,
                         initMode = 1,
                         phase = TRUE)

# Year-1
fit$rep$N[1,] * 1e6
pollock_fixed$quantities$N_at_age[1,1,,1] * exp(parList$dev_log_recruit[1])


# Check quantities ----
# -- Selectivity
max(pollock_fixed$quantities$sel[1,1,,1]-fit$rep$slctsrv1) # Good

max(pollock_fixed$quantities$sel[2,1,,1]-fit$rep$slctsrv2)

max(pollock_fixed$quantities$sel[3,1,,1]-fit$rep$slctsrv3)

max(pollock_fixed$quantities$sel[6,1,,1]-fit$rep$slctsrv6)

ceattle_sel <- t(pollock_fixed$quantities$sel[8,1,,1:nyrs])
max(ceattle_sel - fit$rep$slctfsh)

# -- Mort
max(pollock_fixed$quantities$F_spp[,1:nyrs]-fit$rep$F) # Good
max(t(pollock_fixed$quantities$Z_at_age[1,1,,1:54])-fit$rep$Z) # Good
max(pollock_fixed$quantities$M_at_age[1,1,,1]-fit$rep$M) # Good

Fage = t(pollock_fixed$quantities$F_flt_age[8,1,,1:54])
Nage = t(pollock_fixed$quantities$N_at_age[1,1,,1:54])
Z = t(pollock_fixed$quantities$Z_at_age[1,1,,1:54])
rowSums(Nage * Fage/Z*(1 - exp(-Z)) * fit$obj$env$data$wt_fsh)

pollock_fixed$quantities$catch_hat[1:54] - fit$rep$Ecattot

sum(0.5 * ((log(pollock_fixed$quantities$catch_hat[1:54]) - log(fit$obj$env$data$cattot))/0.05)^2)

fit$obj$env$map$inf1_fsh_mean
fit$obj$env$map$log_slp1_fsh_mean
fit$obj$env$map$inf2_fsh_dev
fit$obj$env$map$slp2_fsh_dev

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


# Adjusted likelihood ----
pollock_adj <- fit_mod(
  data_list = pollock23,
  TMBfilename = "src/ceattle_v01_11_pk23",
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_q = FALSE,
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 1,
  loopnum = 4,
  newtonsteps = 4,
  control = list(eval.max=10000, iter.max=10000, trace=100),
  phase = TRUE
)
pollock_adj$opt$objective

# Pollock adjusted with bias correction ----
pollock_adj_ln <- fit_mod(
  data_list = pollock23,
  TMBfilename = "src/ceattle_v01_11_pk23_ln",
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_q = FALSE,
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 1,
  loopnum = 4,
  newtonsteps = 4,
  control = list(eval.max=10000, iter.max=10000, trace=100),
  phase = TRUE
)
pollock_adj$opt$objective




# SAFE model ----
safe <- pollock_base
nyrs <- length(pollock_base$data_list$styr:pollock_base$data_list$endyr)
safe$quantities$biomass[,1:nyrs] <- fit$rep$Etotalbio * 1e6
safe$quantities$ssb[,1:nyrs] <- fit$rep$Espawnbio * 1e6
safe$quantities$index_hat <- safe$quantities$index_hat / 1e6
safe$quantities$index_hat[1:(nyrs*6)] <- c(fit$rep$Eindxsurv1, fit$rep$Eindxsurv2, fit$rep$Eindxsurv3, fit$rep$Eindxsurv4, fit$rep$Eindxsurv5, fit$rep$Eindxsurv6) * 1e6

safe$quantities$catch_hat[1:54] - fit$rep$cattot

# Plot ----
mod_list <- list(pollock_base, pollock_adj, pollock_adj_ln, pollock_fixed, safe)
model_names <- c("CEATTLE", "CEATTLE w/ SAFE ll", "CEATTLE w/ SAFE ll and BC", "CEATTLE fixed params", "SAFE")


plot_biomass(mod_list, model_names = model_names)
plot_ssb(mod_list, model_names = model_names)
plot_index(mod_list, model_names = model_names)
