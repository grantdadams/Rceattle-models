library(Rceattle)
library(readxl)
setwd("Model runs/GOA_18.5.1/")

################################################
# Data
################################################
# Read the data in
mydata_pollock_fixed <- Rceattle::read_data( file = "Data/GOA_18.5.1_pollock_single_species_1970-2018.xlsx")
mydata_pollock <-  mydata_pollock_fixed
mydata_pollock$estDynamics = 0
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
# Mod 1 - Fix n-at-age and parameters - Check likelihoods
#######################################
mydata_pollock_fixed <- mydata_pollock_fixed
mydata_pollock_fixed$estDynamics = 1

# Scale n-at-age to vals
mydata_pollock_fixed$NByageFixed[,5:15] <- mydata_pollock_fixed$NByageFixed[,5:15] * 1000000
mydata_pollock_fixed$srv_biom$Observation <- mydata_pollock_fixed$srv_biom$Observation * 1000000
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
inits$ln_mn_rec = 1.14237068498
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
                                   debug = 1, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 0, # Single species mode
                                   silent = TRUE,
                                   recompile = FALSE,
                                   phase = "default")

#######################################
# Mod 2 - Base
#######################################

mydata_pollock_est <- mydata_pollock_fixed
mydata_pollock_est$estDynamics = 0
pollock_base <- Rceattle::fit_mod(data_list = mydata_pollock_est,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE,
                                 recompile = FALSE,
                                 phase = "default")


# Start year 1977
mydata_pollock_est$styr = 1977
pollock_base_short <- Rceattle::fit_mod(data_list = mydata_pollock_est,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  debug = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  silent = TRUE,
                                  recompile = FALSE,
                                  phase = "default")


######################### 
# SAFE Models
#########################

library(readxl)
safe2018biomass <- as.data.frame(read_xlsx("Data/Pollock tests/2018_SAFE_pollock_estimates.xlsx", sheet = 1))
safe_nage <- as.data.frame(read_xlsx("Data/2018_safe_n_at_age.xlsx", sheet = 1))
safe2018ssb <- as.data.frame(read_xlsx("Data/Pollock tests/2018_SAFE_pollock_estimates.xlsx", sheet = 2))
safe2018rec <- as.data.frame(read_xlsx("Data/Pollock tests/2018_SAFE_pollock_estimates.xlsx", sheet = 3))

pollock_safe_list <- list()
for(i in 1:7){
  pollock_safe_list[[i]] <- pollock_base
  pollock_safe_list[[i]]$quantities$biomass[1,1:49] <- t(safe2018biomass[,i+1]) * 1000000
  pollock_safe_list[[i]]$quantities$biomassSSB[1,1:49] <- t(safe2018ssb[,i+1])  * 1000000 
  pollock_safe_list[[i]]$quantities$R[1,1:49] <- t(safe2018rec[,i+1]) * 1000000
}

# # Look at index
# safe_2018_index <- as.data.frame(read_xlsx("Data/2018_safe_expected_survey.xlsx", sheet = 1))
# index_cols <- data.frame(Index = c(7,1,2,3,4,5,6), Col = c(2,3,4,5,6,7,8))
# srv_biom <- pollock_safe$data_list$srv_biom
# for(i in 1:nrow(index_cols)){
#   sub <- which(srv_biom$Fleet_code == index_cols$Index[i])
#   yrs <- srv_biom$Year[sub]
#   bio_hat <- safe_2018_index[which(safe_2018_index$Year %in% yrs),index_cols$Col[i]]
#   srv_biom$Observation[sub] <- bio_hat
# }
# pollock_safe$quantities$srv_bio_hat <- srv_biom$Observation



######################### 
# Plots
#########################
# - SAFE vs SS
file_name <- "Figures/18.5.1/18.5.1_SAFE_vs_ceattle_pollock"
mod_list <- c(list(pollock_base, pollock_fixed, pollock_safe_list[[1]]))
mod_names <- c( "CEATTLE est","CEATTLE fixed natage","2018 SAFE (mt)")

# Convert to age-3 biomass
for(i in c(1)){
  mod_list[[i]]$quantities$biomass[1,1:49] <- colSums(mod_list[[i]]$quantities$biomassByage[1,3:10,1:49])
}

plot_biomass(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
plot_ssb(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)

sapply(mod_list, function(x) sum(x$quantities$jnll_comp))



plot_recruitment(mod_list, file = file_name, add_ci = FALSE, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
plot_logindex(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL)
plot_catch(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL)

for(i in 1:6){
  mod_list <- c(list(pollock_fixed_sel, pollock_fixed, pollock_base, pollock_safe_list[[1]], pollock_base_list[[i]]))
  mod_names <- c("CEATTLE fixed sel", "CEATTLE fixed all", "CEATTLE est", "2018 SAFE (mt)", i)
  plot_ssb(mod_list, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
}

# 
# 
# 
# #######################
# # Test plots
# #######################
# 
# # rec plots
# jpeg(filename = "Figures/natage.compare.jpg", height = 10, width = 8, res = 300, units = "in")
# par(mfrow = c(5,2), mar = c(2.8,3.8,1,1))
# for(i in 1:10){
#   safe = safe_nage[[paste0("Age",i)]]
#   ceattle <- pollock_base$quantities$NByage[1,1,i,1:49]/1000000
#   plot(y = safe, x = 1970:2018, type = "l", xlab = "Year", ylab = "N (millions)", ylim = c(0, max(c(safe, ceattle))))
#   lines(y = ceattle, x = 1970:2018, lty = 2)
#   legend("topleft", c("SAFE", "CEATTLE"), lty = c(1,2), bty = "n")
#   legend("top", paste0("Age ",i), bty = "n")
# }
# dev.off()
# 
# 
# # Scale post 1990 mean
# pollock_base_scaled <- pollock_base
# pollock_rw_scaled <- pollock_fixed
# pollock_rw_scaled$quantities$biomassSSB <- pollock_rw_scaled$quantities$biomassSSB * 1000000
# 
# pollock_base_scaled$quantities$biomassSSB <- pollock_base_scaled$quantities$biomassSSB * mean(pollock_safe$quantities$biomassSSB[,21:49]/pollock_base_scaled$quantities$biomassSSB[,21:49])
# 
# pollock_rw_scaled$quantities$biomass <- pollock_rw_scaled$quantities$biomass * 1000000
# 
# pollock_base_scaled$quantities$biomass <- pollock_base_scaled$quantities$biomass * mean(pollock_safe$quantities$biomass[,21:49]/pollock_base_scaled$quantities$biomass[,21:49])
# 
# mod_list <- list(pollock_base, pollock_rw_scaled, pollock_base, pollock_safe)
# mod_names <- c("CEATTLE fixed selectivity", "CEATTLE fixed pop params", "CEATTLE estimated", "2018 SAFE (mt)")
# 
# plot_biomass(mod_list, file = "scaled", model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
# plot_ssb(mod_list, file = "scaled", model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
# 
# # Loglike 1 = catch, 2 = catch-age-comp, 3 = length-comp fishery, 4 = shelikof index, 5 = shelikof age comp, 6 = shelikof length comp, 7 = bt index, 8 = bt age comp, 9 = bt length comp, 10 = nothing, 11, = adfg surve index, 12 = adfg age, 13 = adfg length comp, 14 = age1 index, 15 = age2 index, age 6 = summer acoustic index,