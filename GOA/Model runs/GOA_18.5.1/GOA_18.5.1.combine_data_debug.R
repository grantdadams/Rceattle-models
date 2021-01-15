library(Rceattle)
library(readxl)
setwd("Model runs/GOA_18.5.1/")

################################################
# Data
################################################

# Pollock
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_18.5.1_pollock_single_species_1970-2018.xlsx")
# Scale n-at-age to vals
mydata_pollock$NByageFixed[,5:15] <- mydata_pollock$NByageFixed[,5:15] * 1000000
mydata_pollock$srv_biom$Observation <- mydata_pollock$srv_biom$Observation * 1000000
mydata_pollock$msmMode = 0
mydata_pollock$estDynamics = 0

# Cod and halibut
mydata_pcod_est <- Rceattle::read_data( file = "Data/GOA_18.5.1_pcod_single_species_1977-2018.xlsx")
mydata_pcod_est$pmature[1,2:13] <- 2
mydata_pcod_est$estDynamics[1] = 0

# ATF
mydata_atf_est <- Rceattle::read_data( file = "Data/GOA_18.5.1_arrowtooth_single_species_1961-2018.xlsx")
mydata_atf_est$estDynamics = 0
mydata_atf_est$styr <- 1977


# Pollock and ATF
data1 <- Rceattle::read_data( file = "Data/GOA_18.5.1_1961-2018_combined_by_hand.xlsx")
data1$NByageFixed[which(data1$NByageFixed$Species == 1),5:15] <- data1$NByageFixed[which(data1$NByageFixed$Species == 1),5:15] * 1000000
data1$srv_biom$Observation[which(data1$srv_biom$Species == 1)]  <- data1$srv_biom$Observation[which(data1$srv_biom$Species == 1)] * 1000000
data1$msmMode = 0
data1$estDynamics = c(0, 0)
data1$styr <- 1977

data <-  combine_data(data_list1 = combine_data(data_list1 = mydata_atf_est, data_list2 = mydata_atf_est), data_list2 =  mydata_pcod_est)
write_data(data, file = "GOA_Rceattle_model.xlsx")
data$styr <- 1977
data$estDynamics <- c(0, 0)



mydata_pollock$styr = 1977
check2 <- Rceattle::fit_mod(data_list = mydata_pollock,
                            inits = NULL, # Initial parameters = 0
                            file = "2018pollock", # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE,
                            recompile = FALSE,
                            phase = "default")


mydata_atf_est$styr = 1977
check3 <- Rceattle::fit_mod(data_list = mydata_atf_est,
                            inits = NULL, # Initial parameters = 0
                            file = "2018atf", # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE,
                            recompile = FALSE,
                            phase = "default")



inits <- build_params(data1)

##### Pollock inits
# Fishery selectivity
inits$ln_sel_slp[1:2,8,1] <- check2$estimated_params$ln_sel_slp[1:2,8,1]
inits$sel_inf[1:2,8,1] <- check2$estimated_params$sel_inf[1:2,8,1]
inits$ln_sel_slp_dev[1,8,1,] <- check2$estimated_params$ln_sel_slp_dev[1,8,1,]
inits$sel_inf_dev[1,8,1,] <- check2$estimated_params$sel_inf_dev[1,8,1,]

# Fishing mortality
inits$ln_mean_F[8] <- check2$estimated_params$ln_mean_F[8]
inits$F_dev[8,] <- check2$estimated_params$F_dev[8,]

# Recruitment
inits$ln_mean_rec[1] <- check2$estimated_params$ln_mean_rec[1]
inits$rec_dev[1,1:42] <- check2$estimated_params$rec_dev[1:42]
inits$init_dev[1,1:9] <- check2$estimated_params$init_dev[1:9]

# Survey 1 - Descending logistic, random walk q
inits$ln_sel_slp[2,c(1,7),1] <- check2$estimated_params$ln_sel_slp[2,c(1,7),1]
inits$sel_inf[2,c(1,7),1] <- check2$estimated_params$sel_inf[2,c(1,7),1]
inits$ln_srv_q[1:6] <- check2$estimated_params$ln_srv_q[1:6]
inits$ln_srv_q_dev[c(1,3),] <- check2$estimated_params$ln_srv_q_dev[c(1,3),]

# Survey 2 -  Logistic, prior on q
inits$ln_sel_slp[1,2:3,1] <- check2$estimated_params$ln_sel_slp[1,2:3,1]
inits$sel_inf[1,2:3,1] <- check2$estimated_params$sel_inf[1,2:3,1] 

##### ATF Inits
# Fishery selectivity - Non-parametric
inits$sel_coff[11,1:2,1:19] <- check3$estimated_params$sel_coff[3,1:2,1:19]

# Fishing mortality
inits$ln_mean_F[11] <- check3$estimated_params$ln_mean_F[3]
inits$F_dev[11,1:42] <- check3$estimated_params$F_dev[3,1:42]

# Recruitment
inits$ln_mean_rec[2] <- check3$estimated_params$ln_mean_rec[1]
inits$rec_dev[2,1:42] <- check3$estimated_params$rec_dev[1:42]
inits$init_dev[2,1:20] <- check3$estimated_params$init_dev[1:20]

# Survey BT - Logistic, q = 1
inits$ln_sel_slp[1:2,9:10,1:2] <- check3$estimated_params$ln_sel_slp[1:2,1:2,1:2]
inits$sel_inf[1:2,9:10,1:2] <- check3$estimated_params$sel_inf[1:2,1:2,1:2]


Mod_18_5_1 <- Rceattle::fit_mod(data_list = data,
                                inits = NULL, # Initial parameters = 0
                                file = NULL, # Don't save
                                debug = 0, # Estimate
                                random_rec = FALSE, # No random recruitment
                                msmMode = 0, # Single species mode
                                silent = TRUE,
                                recompile = FALSE,
                                phase = "default")

opt <- optim(par <- Mod_18_5_1$obj$par, fn = Mod_18_5_1$obj$fn, gr = Mod_18_5_1$obj$gr, control = list(maxit = 2e6))
opt

Mod_18_5_1$quantities$jnll_comp[,1:8] - check2$quantities$jnll_comp
Mod_18_5_1$quantities$jnll_comp[,9:11] - check3$quantities$jnll_comp

# somethings up with fsh_biom and srv_biom
check2$data_list$srv_biom$Hat <- check2$quantities$srv_bio_hat
Mod_18_5_1$data_list$srv_biom$Hat <- Mod_18_5_1$quantities$srv_bio_hat
head(check2$data_list$srv_biom) 
head(Mod_18_5_1$data_list$srv_biom)

# Input data?
data_check2 <- rearrange_dat(data1)
data_check_pollock <- rearrange_dat(mydata_pollock)
head(data_check2$srv_biom_obs)
head(data_check_pollock$srv_biom_obs)

# Not a problem

# Catchability?
check2$quantities$srv_q - Mod_18_5_1$quantities$srv_q[1:8,]
check3$quantities$srv_q - Mod_18_5_1$quantities$srv_q[9:11,]
# Not a problem

check3$estimated_params$srv_q_pow
# srv q power  work

# Total mort?
check2$quantities$Zed[1,1,1:10,1:42] - Mod_18_5_1$quantities$Zed[1,1,1:10,1:42] # Good for pollock
check3$quantities$Zed[1,1,1:21,1:42] - Mod_18_5_1$quantities$Zed[2,1,1:21,1:42] # Good ATF
check3$quantities$Zed[1,2,1:21,1:42] - Mod_18_5_1$quantities$Zed[2,2,1:21,1:42] # Good ATF
# Not right for ATF

# M2 is good
check2$quantities$M2[1,1,1:10,1:42] - Mod_18_5_1$quantities$M2[1,1,1:10,1:42] # Good for pollock
check3$quantities$M2[1,1,1:21,1:42] - Mod_18_5_1$quantities$M2[2,1,1:21,1:42]
check3$quantities$M2[1,2,1:21,1:10] - Mod_18_5_1$quantities$M2[2,2,1:21,1:10]

# F_tot 
check2$quantities$F_tot[1,1,1:10,1:42] - Mod_18_5_1$quantities$F_tot[1,1,1:10,1:42] # Good for pollock
check3$quantities$F_tot[1,1,1:21,1:42] - Mod_18_5_1$quantities$F_tot[2,1,1:21,1:42] # Not right for ATF
check3$quantities$F_tot[1,2,1:21,1:10] - Mod_18_5_1$quantities$F_tot[2,2,1:21,1:10] # Not right for ATF


# F
check3$quantities$F[1,1,1:21,1:10] - Mod_18_5_1$quantities$F[9,1,1:21,1:10] # Not right for ATF
check3$quantities$F[1,2,1:21,1:10] - Mod_18_5_1$quantities$F[9,2,1:21,1:10] # Not right for ATF
check3$quantities$F[2,1,1:21,1:10] - Mod_18_5_1$quantities$F[10,1,1:21,1:10] # Not right for ATF
check3$quantities$F[2,2,1:21,1:10] - Mod_18_5_1$quantities$F[10,2,1:21,1:10] # Not right for ATF
check3$quantities$F[3,1,1:21,1:10] - Mod_18_5_1$quantities$F[11,1,1:21,1:10] # Not right for ATF
check3$quantities$F[3,2,1:21,1:10] - Mod_18_5_1$quantities$F[11,2,1:21,1:10] # Not right for ATF
check3$estimated_params$F_dev



# nbyage
check2$quantities$NByage[1,1,1:10,1:42] - Mod_18_5_1$quantities$NByage[1,1,1:10,1:42]
check3$quantities$NByage[1,1,1:21,1:42] - Mod_18_5_1$quantities$NByage[2,1,1:21,1:42]
check3$quantities$NByage[1,2,1:21,1:10] - Mod_18_5_1$quantities$NByage[2,2,1:21,1:10]
# right


# Sel?
check2$quantities$sel[1,1,1:10,1:42] - Mod_18_5_1$quantities$sel[1,1,1:10,1:42] # Good for pollock
check2$quantities$sel[2,1,1:10,1:42] - Mod_18_5_1$quantities$sel[2,1,1:10,1:42] # Good for pollock
check2$quantities$sel[3,1,1:10,1:42] - Mod_18_5_1$quantities$sel[3,1,1:10,1:42] # Good for pollock
check2$quantities$sel[4,1,1:10,1:42] - Mod_18_5_1$quantities$sel[4,1,1:10,1:42] # Good for pollock
check2$quantities$sel[5,1,1:10,1:42] - Mod_18_5_1$quantities$sel[5,1,1:10,1:42] # Good for pollock
check2$quantities$sel[6,1,1:10,1:42] - Mod_18_5_1$quantities$sel[6,1,1:10,1:42] # Good for pollock
check2$quantities$sel[7,1,1:10,1:42] - Mod_18_5_1$quantities$sel[7,1,1:10,1:42] # Bad for fleet 7
check2$quantities$sel[8,1,1:10,1:42] - Mod_18_5_1$quantities$sel[8,1,1:10,1:42] # Good for pollock


check3$quantities$sel[1,1,1:21,1:42] - Mod_18_5_1$quantities$sel[9,1,1:21,1:42]
check3$quantities$sel[2,1,1:21,1:42] - Mod_18_5_1$quantities$sel[10,1,1:21,1:42]
check3$quantities$sel[3,1,1:21,1:42] - Mod_18_5_1$quantities$sel[11,1,1:21,1:42]

check3$quantities$sel[1,2,1:21,1:42] - Mod_18_5_1$quantities$sel[9,2,1:21,1:42]
check3$quantities$sel[2,2,1:21,1:42] - Mod_18_5_1$quantities$sel[10,2,1:21,1:42]
check3$quantities$sel[3,2,1:21,1:42] - Mod_18_5_1$quantities$sel[11,2,1:21,1:42]
# Sel is good


# nbyage
check2$quantities$NByage[1,1,1:10,1:42] - Mod_18_5_1$quantities$NByage[1,1,1:10,1:42]
check3$quantities$NByage[1,1,1:21,1:42] - Mod_18_5_1$quantities$NByage[2,1,1:21,1:42]
check3$quantities$NByage[1,2,1:21,1:10] - Mod_18_5_1$quantities$NByage[2,2,1:21,1:10]
# Not right


# Sex ratio is good
check2$quantities$R_sexr
check3$quantities$R_sexr
Mod_18_5_1$quantities$R_sexr

# Sex ratio is good
check2$quantities$pop_scalar
check3$quantities$pop_scalar
Mod_18_5_1$quantities$pop_scalar

# Recruitment?
check2$quantities$R[1,1:42] - Mod_18_5_1$quantities$R[1,1:42]
check3$quantities$R[1,1:42] - Mod_18_5_1$quantities$R[2,1:42]

# Comp data
comp_hat3 <- check3$data_list$comp_data
comp_hat3[,9:125] <- check3$quantities$comp_hat

mod_comp <- Mod_18_5_1$data_list$comp_data
mod_comp[,9:125] <- Mod_18_5_1$quantities$comp_hat
mod_comp <- mod_comp[which(mod_comp$Species == 2),]

rowSums(comp_hat3[,9:125] - mod_comp[,9:125])
# comp data looks good, like is bad...
