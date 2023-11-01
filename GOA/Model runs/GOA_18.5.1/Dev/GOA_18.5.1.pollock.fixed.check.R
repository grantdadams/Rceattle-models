library(Rceattle)
library(readxl)
setwd("Model runs/GOA_18.5.1/")

################################################
# Data
################################################
# Read the data in
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_18.5.1_pollock_single_species_1970-2018.xlsx")
mydata_pollock_fixed <- Rceattle::read_data( file = "Data/GOA_18.5.1_pollock_single_species_1970-2018.xlsx")
safe2018est <- as.data.frame(read_xlsx("Data/Pollock tests/2018_SAFE_pollock_parameters.xlsx", sheet = 1))
safe2018estMain <- as.data.frame(read_xlsx("Data/Pollock tests/2018_SAFE_pollock_parameters.xlsx", sheet = 2))
# Fishery - Double logistic, random walk ascending params
# Survey 1 - Descending logistic, random walk q
# Survey 2 - Logistic, prior on q
# Survey 3 - Logistic, random walk q
# Survey 4 - Selectivity = 1 for age 1, single q
# Survey 5 - Selectivity  = 1 for age 2, single q
# Survey 6 - Selectivity  = 1 for all ages, single q



#######################################
# Mod 2 - Fix n-at-age and parameters - Check likelihoods
#######################################
mydata_pollock_fixed$srr_prior_mean <- 4
mydata_pollock_fixed$initMode  <- 1
mydata_pollock_fixed$estDynamics = 1

# Scale n-at-age to vals
# mydata_pollock_fixed$NByageFixed[,5:15] <- mydata_pollock_fixed$NByageFixed[,5:15] * 1000000
# mydata_pollock_fixed$srv_biom$Observation <- mydata_pollock_fixed$srv_biom$Observation * 1000000
mydata_pollock_fixed$msmMode = 0
inits <- build_params(mydata_pollock_fixed)

# Fishery selectivity
inits$ln_sel_slp[1:2,8,1] <- c(0.771142817517, 0.895652703661 )
inits$sel_inf[1:2,8,1] <- c(3.79539137331, 9.74118767032)
inits$ln_sel_slp_dev[1,8,1,] <- safe2018est$sel_slp_dev1
inits$sel_inf_dev[1,8,1,] <- safe2018est$sel_inf_dev1

# Fishing mortality
inits$ln_mean_F[8] <- -1.96496591515
inits$F_dev[8,] <- safe2018est$F_dev

# Recruitment
inits$rec_pars[1,1] = 1.14237068498
inits$rec_dev[1:49] <- safe2018est$R_dev

# Survey 1 - Descending logistic, random walk q
inits$ln_sel_slp[2,1,1] <- safe2018estMain$log_slp2_srv1
inits$sel_inf[2,1,1] <- safe2018estMain$inf2_srv1
inits$ln_srv_q[1] <- safe2018estMain$log_q1_mean
inits$ln_srv_q_dev[1,] <- safe2018est$log_q1_dev

# Survey 2 -  Logistic, prior on q
inits$ln_sel_slp[1,2,1] <- safe2018estMain$log_slp1_srv2
inits$sel_inf[1,2,1] <- safe2018estMain$inf1_srv2
inits$ln_srv_q[2] <- safe2018estMain$log_q2_mean


# Survey 3 - Logistic, random walk q
inits$ln_sel_slp[1,3,1] <- safe2018estMain$log_slp1_srv3
inits$sel_inf[1,3,1] <- safe2018estMain$inf1_srv3
inits$ln_srv_q[3] <- safe2018estMain$log_q3_mean
inits$ln_srv_q_dev[3,] <- safe2018est$log_q3_dev

# Survey 4 - Selectivity = 1 for age 1, single q
inits$ln_srv_q[4] <- safe2018estMain$log_q4

# Survey 5 - Selectivity  = 1 for age 2, single q
inits$ln_srv_q[5] <- safe2018estMain$log_q5


# Survey 6 - Selectivity  = 1 for all ages, single q
inits$ln_srv_q[6] <- safe2018estMain$log_q6

library(Rceattle)
pollock_fixed <- Rceattle::fit_mod(data_list = mydata_pollock_fixed,
                                   inits = inits, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 4, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 0, # Single species mode
                                   verbose = 1,
                                   phase = "default")

  
round(pollock_fixed$quantities$jnll_comp,4)[1:8,1:6]

safe_jnll <- read.csv( file = "Data/Pollock tests/2018_SAFE_nll_components.csv")
rownames(safe_jnll) <- safe_jnll[,1]
safe_jnll = safe_jnll[,-1]
round(safe_jnll,4)[1:8,1:6]

# Save bits
srv_biom <- pollock_fixed$data_list$srv_biom
srv_biom$est <- pollock_fixed$quantities$srv_bio_hat

# Look at index
library(readxl)
safe_2018_index <- as.data.frame(read_xlsx("Data/Pollock tests/2018_safe_expected_survey.xlsx", sheet = 1))
srv_biom$SAFE <- NA
index_cols <- data.frame(Index = c(7,1,2,3,4,5,6), Col = c(2,3,4,5,6,7,8))
for(i in 1:nrow(index_cols)){
  sub <- which(srv_biom$Fleet_code == index_cols$Index[i])
  yrs <- srv_biom$Year[sub]
  bio_hat <- safe_2018_index[which(safe_2018_index$Year %in% yrs),index_cols$Col[i]]
  srv_biom$SAFE[sub] <- bio_hat * 1e6
}
srv_biom
srv_biom$RE <- (srv_biom$est - srv_biom$SAFE)/srv_biom$SAFE
write.csv(srv_biom, file = "srv_biom.csv")


# Check selectivities
# Srv 1 is all good

# Srv 2
# - Srv selectivity for srv 2 is good
pollock_fixed$quantities$sel[2,1,,1] - c(0.130663, 0.21864, 0.343155, 0.495111, 0.6503, 0.782476, 0.878728, 0.941117, 0.978559, 1)

# - Catchability is good
pollock_fixed$quantities$srv_q[2,] - 0.847681

# Srv 3
# - Srv selectivity for srv 3 is good
pollock_fixed$quantities$sel[3,1,,1] - c( 0.00561133, 0.0229488, 0.0890586, 0.28926, 0.628943, 0.87613, 0.967484, 0.992343, 0.998508, 1)

# - Catchability is good
pollock_fixed$quantities$srv_q[3,] - c(0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.634447, 0.627027, 0.633928, 0.639367, 0.650636, 0.671494, 0.695191, 0.720276, 0.752022, 0.781426, 0.814788, 0.849029, 0.87149, 0.888081, 0.88689, 0.881087, 0.868459, 0.854833, 0.851308, 0.844487, 0.823881, 0.775554, 0.722421, 0.666528, 0.599173, 0.54263, 0.489501, 0.449416, 0.422951, 0.421468)


# Srv 4
# - Srv selectivity for srv 4 is good
pollock_fixed$quantities$sel[4,1,,1]

# - Catchability is good
pollock_fixed$quantities$srv_q[4,1] - 0.335939


# Srv 5
# - Srv selectivity for srv 5 is good
pollock_fixed$quantities$sel[5,1,,1]

# - Catchability is good
pollock_fixed$quantities$srv_q[5,1] - 0.418653


# Srv 6
# - Srv selectivity for srv 6 is good
pollock_fixed$quantities$sel[6,1,,1] # All 1s

# - Catchability is good
pollock_fixed$quantities$srv_q[6,] - 0.82806

# Selectivities are correct
# Catchabilities are correct


comp_hat <- mydata_pollock_fixed$comp_data[,1:8]
comp_hat <- cbind(comp_hat, pollock_fixed$quantities$comp_hat)

age_hat <- mydata_pollock_fixed$comp_data[,1:8]
age_hat <- cbind(age_hat, pollock_fixed$quantities$age_hat)

write.csv(comp_hat, file = "comp_hat.csv")
write.csv(t(pollock_fixed$quantities$NByage[1,1,,]), file = "n_hat.csv")
write.csv(t(pollock_fixed$quantities$Zed[1,1,,]), file = "zed_hat.csv")
write.csv(age_hat, file = "c_hat.csv")

# Last item, q3 dev like
sum(0.5*(((safe2018est$log_q3_dev[2:49] - safe2018est$log_q3_dev[1:48])/0.05)^2))


mydata_pollock_est <- mydata_pollock_fixed
mydata_pollock_est$estDynamics = 0
data("GOApollock")
pollock_est <- Rceattle::fit_mod(data_list = mydata_pollock_fixed,
                                   inits = NULL, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 0, # Single species mode
                                   verbose = 1,
                                   phase = "default")

pollock_fixed$quantities$biomass <- pollock_fixed$quantities$biomass/1e6
mod_list <- list(pollock_est, pollock_fixed)
mod_names <- c( "CEATTLE est","CEATTLE fixed natage")
plot_biomass(mod_list, model_names = mod_names)
