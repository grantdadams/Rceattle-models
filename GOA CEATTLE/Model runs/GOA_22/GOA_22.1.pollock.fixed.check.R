library(Rceattle)
library(readxl)
setwd("Model runs/GOA_22.1.1/")

################################################
# Data
################################################
# Read the data in
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_22.1.1_pollock_single_species_1970-2022.xlsx")
# mydata_pollock_fixed <- Rceattle::read_data( file = "Data/GOA_18.5.1_pollock_single_species_1970-2018.xlsx")
safe2022est <- as.data.frame(read_xlsx("Data/SAFE data/Pollock/2022_SAFE_pollock_parameters.xlsx", sheet = 1))
# Fishery - Double logistic, random walk ascending params
# Survey 1 (shelikof acoustic) - Descending logistic, random walk q
# Survey 2 (bottom trawl) - Logistic, prior on q
# Survey 3  (adfg) - Logistic, random walk q
# Survey 4 (age1 acoustic) - Selectivity = 1 for age 1, single q
# Survey 5 (age2 acoustic) - Selectivity  = 1 for age 2, single q
# Survey 6 (summer acoustic) - Descending logistic, single q
# Survey 7 (shelikof acoustic BS) - Selectivity  = 1 for all ages, single q: NOT USED


#######################################
# Model 1 - Fix parameters - Check likelihoods
#######################################
inits <- build_params(mydata_pollock)

# Fishery selectivity
inits$ln_sel_slp[1:2,8,1] <- c(0.780281092269, 0.907394287100 )
inits$sel_inf[1:2,8,1] <- c(3.73798237692, 9.70062112827)
inits$ln_sel_slp_dev[1,8,1,] <- safe2022est$sel_slp_dev1
inits$sel_inf_dev[1,8,1,] <- safe2022est$sel_inf_dev1

# Fishing mortality
inits$ln_mean_F[8] <- -1.96488589131
inits$F_dev[8,] <- safe2022est$F_dev

# Recruitment
inits$ln_mean_rec = 1.08729468697
inits$rec_dev[1:53] <- safe2022est$R_dev
inits$init_dev[] <- safe2022est$R_dev[1]

# Survey 1 - Descending logistic, random walk q
inits$ln_sel_slp[2,1,1] <- 0.524130949093 # log_slp1_srv1
inits$sel_inf[2,1,1] <- 9.74113679962 # inf1_srv1
inits$ln_srv_q[1] <- -0.519308482216 # log_q1_mean
inits$ln_srv_q_dev[1,] <- safe2022est$log_q1_dev

# Survey 2 -  Logistic, prior on q
inits$ln_sel_slp[1,2,1] <- -0.461812344789 # log_slp1_srv2
inits$sel_inf[1,2,1] <- 4.02713330587 # inf1_srv2
inits$ln_sel_slp[2,2,1] <- 1.00000000000 # log_slp2_srv2
inits$sel_inf[2,2,1] <- 20.0000000000 # inf2_srv2
inits$ln_srv_q[2] <- -0.205784258807 # log_q2_mean


# Survey 3 - Logistic, random walk q
inits$ln_sel_slp[1,3,1] <-0.336472034023 # log_slp1_srv3
inits$sel_inf[1,3,1] <- 4.71737950613 # inf1_srv3
inits$ln_srv_q[3] <- -1.46152154948 # log_q3_mean
inits$ln_srv_q_dev[3,] <- safe2022est$log_q3_dev

# Survey 4 - Selectivity = 1 for age 1, single q
inits$ln_srv_q[4] <- -1.16901414560 # log_q4

# Survey 5 - Selectivity  = 1 for age 2, single q
inits$ln_srv_q[5] <- -1.05139432235 # log_q5


# Survey 6 - Selectivity  = 1 for all ages, single q
inits$ln_sel_slp[1,6,1] <-4.90000000000 # log_slp1_srv6
inits$sel_inf[1,6,1] <- 0.500000000000 # inf1_srv6:
inits$ln_sel_slp[2,6,1] <-0.227402633304 # log_slp2_srv6
inits$sel_inf[2,6,1] <- 7.86580876241 # inf2_srv6:
inits$ln_srv_q[6] <- -0.256836375502 # log_q6

pollock_fixed_params <- Rceattle::fit_mod(data_list = mydata_pollock,
                                   inits = inits, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 4, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 0, # Single species mode
                                   verbose = 2,
                                   phase = "default")

# Check selectivity and catchability
selectivity <- as.data.frame(read_xlsx("Data/SAFE data/Pollock/2022_safe_expected_survey.xlsx", sheet = 2))
fish_selectivity <- as.data.frame(read_xlsx("Data/SAFE data/Pollock/2022_safe_expected_survey.xlsx", sheet = 3))
catchability <- as.data.frame(read_xlsx("Data/SAFE data/Pollock/2022_safe_expected_survey.xlsx", sheet = 4))

# Selectivity is good
round(pollock_fixed_params$quantities$sel[1:6,1,,1],6) - round(t(selectivity[,-1]),6) 

# Fishery selectivity
round(pollock_fixed_params$quantities$sel[8,1,,],6) - round(t(fish_selectivity[,-1]),6)

sel_check <-  sapply(1:10, function(x) 1 / (1 + exp( -exp((0.780281092269 + safe2022est$sel_slp_dev1)) * ((x) -( 3.73798237692 + safe2022est$sel_inf_dev1)))) * 
(1 - (1 / (1 + exp( -(exp(0.907394287100 )) * ((x) - (9.70062112827)))))))  # Downward slope;
sel_check = sel_check / (sel_check[,7])
round((fish_selectivity[,-1]),6) - round(sel_check, 6)
round(sel_check, 5)[4,3]
round((fish_selectivity[,-1]),5)[4,3]

# Catchability is good
round(pollock_fixed_params$quantities$srv_q[1:6,],6) - round(t(catchability[,-1]),6) 

# Nbyage
nbyage <- t(pollock_fixed_params$quantities$NByage[1,1,,1:53])
nbyage_fices <- pollock_fixed_params$data_list$NByageFixed[,5:14]

nbyage - nbyage_fices

# Look at index

# Check index
srv_biom <- pollock_fixed_params$data_list$srv_biom
srv_biom$est <- signif(pollock_fixed_params$quantities$srv_bio_hat,6) 

safe_2018_index <- as.data.frame(read_xlsx("Data/SAFE data/Pollock/2022_safe_expected_survey.xlsx", sheet = 1))
srv_biom$SAFE <- NA
for(i in 1:6){
  sub <- which(srv_biom$Fleet_code == i)
  yrs <- srv_biom$Year[sub]
  bio_hat <- safe_2018_index[which(safe_2018_index$Year %in% yrs), i + 1]
  srv_biom$SAFE[sub] <- bio_hat 
}
srv_biom
srv_biom$RE <- (srv_biom$est - srv_biom$SAFE)/srv_biom$SAFE
max(srv_biom$RE , na.rm = TRUE)

srv_biom[which(srv_biom$RE > 0.0001),]

# Look at comp
comp_hat <- pollock_fixed_params$comp_data[,1:8]
comp_hat <- cbind(comp_hat, pollock_fixed_params$quantities$comp_hat)
write.csv(comp_hat, file = "Data/SAFE data/Pollock/CEATTLE_comp_hat.csv")

age_hat <- pollock_fixed_params$comp_data[,1:8]
age_hat <- cbind(age_hat, pollock_fixed_params$quantities$age_hat)
write.csv(age_hat, file = "Data/SAFE data/Pollock/CEATTLE_age_hat.csv")


#######################################
# Mod 2 - Fix n-at-age and parameters
#######################################
mydata_pollock_fixed <- mydata_pollock
mydata_pollock_fixed$estDynamics = 1

# Scale n-at-age to vals
mydata_pollock_fixed$NByageFixed[,5:15] <- mydata_pollock_fixed$NByageFixed[,5:15] 
mydata_pollock_fixed$srv_biom$Observation <- mydata_pollock_fixed$srv_biom$Observation 

pollock_fixed <- Rceattle::fit_mod(data_list = mydata_pollock_fixed,
                                          inits = inits, # Initial parameters = 0
                                          file = NULL, # Don't save
                                          estimateMode = 4, # Estimate
                                          random_rec = FALSE, # No random recruitment
                                          msmMode = 0, # Single species mode
                                          verbose = 2,
                                          phase = "default")

# Check index
srv_biom <- pollock_fixed$data_list$srv_biom
srv_biom$est <- pollock_fixed$quantities$srv_bio_hat
srv_biom$est_ll <- pollock_fixed$quantities$check_srv_ll

# Look at index
safe_2018_index <- as.data.frame(read_xlsx("Data/SAFE data/Pollock/2022_safe_expected_survey.xlsx", sheet = 1))
srv_biom$SAFE <- NA
for(i in 1:6){
  sub <- which(srv_biom$Fleet_code == i)
  yrs <- srv_biom$Year[sub]
  bio_hat <- safe_2018_index[which(safe_2018_index$Year %in% yrs), i + 1]
  srv_biom$SAFE[sub] <- bio_hat
}
srv_biom
srv_biom$RE <- (srv_biom$est - srv_biom$SAFE)/srv_biom$SAFE
max(srv_biom$RE , na.rm = TRUE)

srv_biom[which(srv_biom$RE > 0.0001),]


# srv1_biom <- srv_biom[which(srv_biom$Fleet_code == 1),]
# -.5*norm2(elem_div(
#   (log(indxsurv1)-log(Eindxsurv1(srvyrs1))+square(indxsurv_log_sd1)/2.),indxsurv_log_sd1));

srv_log_sd_hat

sum(0.5 * ((log(srv1_biom$Observation) - log(srv1_biom$est)+srv1_biom$Log_sd^2/2)/srv1_biom$Log_sd)^2)

pollock_fixed$quantities$srv_log_sd_hat

pollock_fixed$quantities$jnll_comp

# Look at comp
comp_hat <- mydata_pollock_fixed$comp_data[,1:8]
comp_hat <- cbind(comp_hat, pollock_fixed$quantities$comp_hat)

age_hat <- mydata_pollock_fixed$comp_data[,1:8]
age_hat <- cbind(age_hat, pollock_fixed$quantities$age_hat)





#######################################
# Mod 3 - Fix n-at-age and input selectivity
#######################################
mydata_pollock_fixed <- mydata_pollock
mydata_pollock_fixed$estDynamics = 1
mydata_pollock_fixed$fleet_control$Selectivity[] <- 0

# Scale n-at-age to vals
mydata_pollock_fixed$NByageFixed[,5:15] <- mydata_pollock_fixed$NByageFixed[,5:15] * 1000000
mydata_pollock_fixed$srv_biom$Observation <- mydata_pollock_fixed$srv_biom$Observation * 1000000

pollock_fixed_sel <- Rceattle::fit_mod(data_list = mydata_pollock_fixed,
                                   inits = inits, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 4, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 0, # Single species mode
                                   verbose = 2,
                                   phase = "default")

pollock_fixed_sel$quantities$jnll_comp

# Check index
srv_biom <- pollock_fixed_sel$data_list$srv_biom
srv_biom$est <- pollock_fixed_sel$quantities$srv_bio_hat

# Look at index
safe_2018_index <- as.data.frame(read_xlsx("Data/SAFE data/Pollock/2022_safe_expected_survey.xlsx", sheet = 1))
srv_biom$SAFE <- NA
for(i in 1:6){
  sub <- which(srv_biom$Fleet_code == i)
  yrs <- srv_biom$Year[sub]
  bio_hat <- safe_2018_index[which(safe_2018_index$Year %in% yrs), i + 1]
  srv_biom$SAFE[sub] <- bio_hat * 1000000
}
srv_biom
srv_biom$RE <- (srv_biom$est - srv_biom$SAFE)/srv_biom$SAFE
max(srv_biom$RE , na.rm = TRUE)

srv_biom[which(srv_biom$RE > 0.0001),]

# double check
year = 2022 - 1973
fleet = 3

sum(pollock_fixed_sel$quantities$sel[3,1,,1] * mydata_pollock_fixed$NByageFixed[year,5:14] * mydata_pollock_fixed$wt[which(mydata_pollock_fixed$wt$Wt_index == 4)[year], 6:15]) * pollock_fixed_sel$quantities$srv_q[3,1]


# Look at comp
comp_hat <- mydata_pollock_fixed$comp_data[,1:18]
est_comp <- pollock_fixed_sel$quantities$comp_hat[,1:10]
colnames(est_comp) <- paste0("EstComp",1:10)
comp_hat <- cbind(comp_hat, est_comp)
write.csv(comp_hat, file = "Data/SAFE data/Pollock/CEATTLE_comp_hat.csv")

age_hat <- mydata_pollock_fixed$comp_data[,1:18]
est_age <- pollock_fixed_sel$quantities$age_hat[,1:10]
colnames(est_age) <- paste0("EstAge",1:10) 
age_hat <- cbind(age_hat, est_age)
write.csv(age_hat, file = "Data/SAFE data/Pollock/CEATTLE_age_hat.csv")




write.csv(pollock_fixed$quantities$jnll_comp, file = "jnll_pollock.csv")

################################################
# Model 4 - Estimate from data
################################################
mydata_pollock$srv_biom$Observation <- mydata_pollock$srv_biom$Observation  * 1000
pollock_est <- Rceattle::fit_mod(data_list = mydata_pollock,
                                   inits = NULL, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 0, # Single species mode
                                   phase = "default")



################################################
# Model 5 - SAFE model
################################################
library(readxl)
safe2022biomass <- as.data.frame(read_xlsx("Data/2022_Pollock_Safe.xlsx", sheet = 1))

# Assign data to CEATTLE object
years <- 1970:2022
Mod_2022_SAFE <- pollock_est
Mod_2022_SAFE$quantities$biomass[1,1:length(years)] <- safe2022biomass$B * 1000000
Mod_2022_SAFE$quantities$biomassSSB[1,1:length(years)] <-  safe2022biomass$SSB * 1000000
Mod_2022_SAFE$quantities$R[1,1:length(years)] <-  safe2022biomass$R * 1000000


################################################
# Plot
################################################
pollock_fixed_scaled <- pollock_fixed_params
pollock_fixed_scaled$quantities$biomass[1,1:length(years)] <- pollock_fixed_params$quantities$biomass[1,1:length(years)] * 1000000
pollock_fixed_scaled$quantities$biomassSSB[1,1:length(years)] <-  pollock_fixed_params$quantities$biomassSSB[1,1:length(years)] * 1000000
pollock_fixed_scaled$quantities$R[1,1:length(years)] <-  pollock_fixed_params$quantities$R[1,1:length(years)]  * 1000000


pollock_init <-readRDS("pollock.rds")

mod_list <- list(pollock_est, pollock_fixed_scaled, pollock_fixed_sel, Mod_2022_SAFE)
mod_names <- c( "CEATTLE est","CEATTLE fixed params", "fixed sel","2018 SAFE (mt)")

plot_biomass(mod_list, model_names = mod_names)
#plot_recruitment(mod_list, model_names = mod_names)
