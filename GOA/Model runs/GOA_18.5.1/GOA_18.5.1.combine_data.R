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

data <-  combine_data( data_list1 = combine_data(data_list1 = mydata_pollock, data_list2 = mydata_atf_est), data_list2 =  mydata_pcod_est)
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
                                phase = "default", control = list(eval.max = 2e+09, iter.max =1e+8, trace = 0))

quantities <- Mod_18_5_1$obj$report(Mod_18_5_1$obj$env$last.par.best)
quantities$biomassSSB[,1:10]
check3$quantities$biomassSSB[,1:10]

Mod_18_5_1$quantities <- quantities
plot_biomass(list(Mod_18_5_1, check2), species = 1)


Mod_18_5_1$estimated_params$sel_inf[1,,]
Mod_18_5_1$identified$param_list$sel_inf[1,,]

parms <- suppressWarnings(Mod_18_5_1$obj$env$parList(Mod_18_5_1$obj$env$last.par.best))
identified_param_list <- Mod_18_5_1$obj$env$parList((Mod_18_5_1$identified$BadParams$Param_check)) 
