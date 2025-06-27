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
parList <- fit$obj$env$parList(fit$obj$env$last.par.best) # HEAD "a506609"
pollock23 <- Rceattle::read_data( file = "Data/Pollock_2023.xlsx")
pollock23$sigma_rec_prior <- 1.1



# Fit base model ----
pollock_base <- fit_mod(data_list = pollock23,
                        inits = NULL, # Initial parameters = 0
                        file = NULL, # Don't save
                        estimateMode = 0, # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0, # Single species mode
                        verbose = 1,
                        initMode = 1,
                        phase = TRUE
                        )



# SAFE init model ----
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

# * Fit inits ----
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


# Fix parameter model ----
pkest <- pollock_base$estimated_params
pkinits <- pollock_base$initial_params
yrs <- pollock_base$data_list$styr:pollock_base$data_list$endyr
nyrs <- length(yrs)

# - Recruitment
pkinits$rec_pars[,1] = log(exp(parList$mean_log_recruit)*1e6) # Mean rec
pkinits$x_tj[1:nyrs,1] <- parList$dev_log_recruit # DSEM
pkinits$init_dev[1,] <- 0 # parList$dev_log_recruit[1]

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
pollock_fixed <- fit_mod(
  data_list = pollock23,
  inits = pkinits, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 1,
  phase = FALSE
)
pollock_fixed$opt$objective
pollock_inits$opt$objective
pollock_base$opt$objective

pollock_fixed$quantities$jnll_gmrf_dsem
pollock_base$quantities$jnll_gmrf_dsem

pollock_fixed$quantities$R0
pollock_base$quantities$R0

apply(pollock_base$quantities$jnll_comp[-c(1,9),], 2, as.numeric) - apply(pollock_fixed$quantities$jnll_comp[-c(1,9),], 2, as.numeric)


# Year-1
fit$rep$N[1,] * 1e6
pollock_fixed$quantities$N_at_age[1,1,,1] * exp(parList$dev_log_recruit[1])


# * Check quantities ----
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

# -- Catchability
fit$rep$q1 - pollock_fixed$quantities$index_q[1,]

fit$rep$q2 - pollock_fixed$quantities$index_q[2,]

fit$rep$q3 - pollock_fixed$quantities$index_q[3,]

fit$rep$q6 - pollock_fixed$quantities$index_q[6,]






# Fit dev-name-change model ----
initsdsem <- pollock_base$estimated_params
load("~/Documents/GitHub/Rceattle-models/GOA pollock/devnamechange.RData")

check <- data.frame(Name = names(initsdsem), ZeroDsem = NA, ZeroDev = NA)
for(i in 1:nrow(check)){
  check$ZeroDsem[i] <- sum(initsdsem[[i]] != 0)
  if(check$Name[i] %in% names(mod_objects$estimated_params)){
    initsdsem[[i]] <- mod_objects$estimated_params[[check$Name[i]]]
    check$ZeroDev[i] <- sum(mod_objects$estimated_params[[check$Name[i]]] != 0)
  }
}
check$Diff = check$ZeroDsem != check$ZeroDev
check

initsdsem$x_tj[,1] <- mod_objects$estimated_params$rec_dev # DSEM



pollock_check <- fit_mod(data_list = pollock23,
                         inits = initsdsem, # Initial parameters = 0
                         file = NULL, # Don't save
                         estimateMode = 3, # Estimate
                         random_rec = FALSE, # No random recruitment
                         msmMode = 0, # Single species mode
                         verbose = 1,
                         initMode = 2,
                         phase = TRUE
)
apply(pollock_check$quantities$jnll_comp[-c(1,9),], 2, as.numeric)-apply(mod_objects$quantities$jnll_comp[-c(1,9),], 2, as.numeric)

pars <- pollock_base$estimated_params
dsem <- pollock_base$dsem
summary(dsem)

dsem$obj$env$parList(dsem$obj$par) <- pars[names(dsem$obj$env$parList())]



# SAFE model ----
safe <- pollock_base
nyrs <- length(pollock_base$data_list$styr:pollock_base$data_list$endyr)
safe$quantities$biomass[,1:nyrs] <- fit$rep$Etotalbio * 1e6
safe$quantities$ssb[,1:nyrs] <- fit$rep$Espawnbio * 1e6
safe$quantities$R[,1:nyrs] <- fit$rep$recruit * 1e6
safe$quantities$index_hat <- safe$quantities$index_hat / 1e6
safe$quantities$index_hat[1:(nyrs*6)] <- c(fit$rep$Eindxsurv1, fit$rep$Eindxsurv2, fit$rep$Eindxsurv3, fit$rep$Eindxsurv4, fit$rep$Eindxsurv5, fit$rep$Eindxsurv6) * 1e6

safe$quantities$catch_hat[1:54] - fit$rep$cattot

# Plot ----
mod_list <- list(pollock_base, pollock_fixed, safe)
model_names <- c("CEATTLE",  "CEATTLE fixed params", "SAFE")


plot_biomass(mod_list, model_names = model_names)
plot_ssb(mod_list, model_names = model_names)
plot_recruitment(mod_list, model_names = model_names)
plot_index(mod_list, model_names = model_names)

