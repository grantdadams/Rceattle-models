library(Rceattle)
library(readxl)
setwd("Model runs/GOA_18.5.1/")

################################################
# Data
################################################
# Read the data in
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_18.5.1_pollock_single_species_1970-2018.xlsx")
mydata_pollock$spawn_month = 0.21 * 12
mydata_pollock2 <- Rceattle::read_data( file = "Data/GOA_18.5.1_pollock_single_species_1970-2018_fixed_bt_sel.xlsx")
mydata_pollock2$spawn_month = 0.21 * 12
safe2018est <- as.data.frame(read_xlsx("Data/2018_SAFE_pollock_parameters.xlsx", sheet = 1))


mydata_pollock_rw <- mydata_pollock

#mydata_pollock_rw$fleet_control$Time_varying_sel[8] <- 5
#mydata_pollock_rw$fleet_control$Sel_sd_prior[8] <- 0.05
mydata_pollock_rw$fleet_control$Selectivity[c(8)] <- 3
pollock_base <- Rceattle::fit_mod(data_list = mydata_pollock_rw,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  debug = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  silent = TRUE,
                                  recompile = FALSE,
                                  phase = "default")

# Random walk of q - adfg, shelikof, bt, random walk of fishery sel
mydata_pollock_rw <- mydata_pollock

mydata_pollock_rw$fleet_control$Time_varying_sel[8] <- 5
mydata_pollock_rw$fleet_control$Sel_sd_prior[8] <- 0.05
mydata_pollock_rw$fleet_control$Selectivity[c(8)] <- 3
mydata_pollock_rw$styr = 1970
mydata_pollock_rw$msmMode = 0
inits <- build_params(mydata_pollock_rw)
inits$sel_slp[1:2,8,1] <- c(0.771142817517, 0.895652703661 )
inits$sel_inf[1:2,8,1] <- c(3.79539137331, 9.74118767032)
inits$ln_mean_F[8] <- -1.96496591515
inits$ln_mn_rec = 1.14237068498
inits$sel_slp_dev[1,8,1,] <- safe2018est$sel_slp_dev1
inits$sel_inf_dev[1,8,1,] <- safe2018est$sel_inf_dev1
inits$rec_dev[1:49] <- safe2018est$R_dev
#inits$init_dev[1,] <- safe2018est$R_dev[1]
inits$F_dev[8,] <- safe2018est$F_dev
map <- build_map(mydata_pollock_rw, inits)

# Map out F and selectivity for fishery
map[[1]]$sel_inf_dev <- as.factor(replace(map[[2]]$sel_inf_dev, values = rep( NA, length(map[[2]]$sel_inf_dev))))
map[[1]]$sel_slp_dev <- as.factor(replace(map[[2]]$sel_slp_dev, values = rep( NA, length(map[[2]]$sel_slp_dev))))
map[[1]]$F_dev <- as.factor(replace(map[[2]]$F_dev, values = rep( NA, length(map[[2]]$F_dev))))
map[[2]]$sel_slp[1:2,8,] <- NA
map[[2]]$sel_inf[1:2,8,] <- NA
map[[1]]$sel_slp <- as.factor(map[[2]]$sel_slp)
map[[1]]$sel_inf <- as.factor(map[[2]]$sel_inf)
#map[[1]]$init_dev <- as.factor(replace(map[[2]]$init_dev, values = rep( NA, length(map[[2]]$init_dev))))
map[[1]]$ln_mean_F <- as.factor(rep(NA, 8))

pollock_fixed <- Rceattle::fit_mod(data_list = mydata_pollock_rw,
                                   inits = inits, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   debug = 1, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 0, # Single species mode
                                   silent = TRUE,
                                   recompile = FALSE,
                                   phase = "default")




# Map out F and selectivity for fishery
map <- build_map(mydata_pollock_rw, inits)
map[[1]]$sel_inf_dev <- as.factor(replace(map[[2]]$sel_inf_dev, values = rep( NA, length(map[[2]]$sel_inf_dev))))
map[[1]]$sel_slp_dev <- as.factor(replace(map[[2]]$sel_slp_dev, values = rep( NA, length(map[[2]]$sel_slp_dev))))
map[[2]]$sel_slp[1:2,8,] <- NA
map[[2]]$sel_inf[1:2,8,] <- NA
map[[1]]$sel_slp <- as.factor(map[[2]]$sel_slp)
map[[1]]$sel_inf <- as.factor(map[[2]]$sel_inf)
#map[[1]]$init_dev <- as.factor(replace(map[[2]]$init_dev, values = rep( NA, length(map[[2]]$init_dev))))

pollock_fixed_sel <- Rceattle::fit_mod(data_list = mydata_pollock_rw,
                                   inits = inits, # Initial parameters = 0
                                   map = map,
                                   file = NULL, # Don't save
                                   debug = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 0, # Single species mode
                                   silent = TRUE,
                                   recompile = FALSE,
                                   phase = "default")

#selectivity the same?
t(pollock_fixed$quantities$sel[8,1,,]) - mydata_pollock2$emp_sel[which(mydata_pollock2$emp_sel$Fleet_code==8),6:15]
pollock_fixed$quantities$sigma_sel



######################### 
# SAFE Models
#########################

library(readxl)
safe2018biomass <- as.data.frame(read_xlsx("Data/2018_SAFE_pollock_estimates.xlsx", sheet = 1))
safe_nage <- as.data.frame(read_xlsx("Data/2018_safe_n_at_age.xlsx", sheet = 1))
safe2018ssb <- as.data.frame(read_xlsx("Data/2018_SAFE_pollock_estimates.xlsx", sheet = 2))
safe2018rec <- as.data.frame(read_xlsx("Data/2018_SAFE_pollock_estimates.xlsx", sheet = 3))

pollock_safe_list <- list()
for(i in 1:4){
  pollock_safe_list[[i]] <- pollock_base
  pollock_safe_list[[i]]$quantities$biomass[1,1:49] <- t(safe2018biomass[,i+1]) * 1000000
  pollock_safe_list[[i]]$quantities$biomassSSB[1,1:49] <- t(safe2018ssb[,i+1])  * 1000000 
  pollock_safe_list[[i]]$quantities$R[1,1:49] <- t(safe2018rec[,i+1]) * 1000000
}

# Look at index
safe_2018_index <- as.data.frame(read_xlsx("Data/2018_safe_expected_survey.xlsx", sheet = 1))
index_cols <- data.frame(Index = c(7,1,2,3,4,5,6), Col = c(2,3,4,5,6,7,8))
srv_biom <- pollock_safe$data_list$srv_biom
for(i in 1:nrow(index_cols)){
  sub <- which(srv_biom$Fleet_code == index_cols$Index[i])
  yrs <- srv_biom$Year[sub]
  bio_hat <- safe_2018_index[which(safe_2018_index$Year %in% yrs),index_cols$Col[i]]
  srv_biom$Observation[sub] <- bio_hat
}
pollock_safe$quantities$srv_bio_hat <- srv_biom$Observation


pollock_base$quantities$biomass[1,1:49] <- colSums(pollock_base$quantities$biomassByage[1,3:10,1:49])
pollock_fixed$quantities$biomass[1,1:49] <- colSums(pollock_fixed$quantities$biomassByage[1,3:10,1:49])*1000000
pollock_fixed$quantities$biomassSSB <- pollock_fixed$quantities$biomassSSB * 1000000
pollock_fixed$quantities$fsh_bio_hat <- pollock_fixed$quantities$fsh_bio_hat * 1000000
pollock_est$quantities$biomass[1,1:49] <- colSums(pollock_est$quantities$biomassByage[1,3:10,1:49])



######################### 
# Plots
#########################
# - SAFE vs SS
file_name <- "Figures/18.5.1/18.5.1_SAFE_vs_ceattle_pollock"
mod_list <- c(list(pollock_fixed_sel, pollock_fixed, pollock_est, pollock_safe_list[[1]]))
mod_names <- c("CEATTLE fixed sel", "CEATTLE fixed all", "CEATTLE est", "2018 SAFE (mt)")

plot_biomass(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
plot_ssb(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
plot_recruitment(mod_list, file = file_name, add_ci = FALSE, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
plot_logindex(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL)
plot_catch(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL)




#######################
# Test plots
#######################

# rec plots
jpeg(filename = "Figures/natage.compare.jpg", height = 10, width = 8, res = 300, units = "in")
par(mfrow = c(5,2), mar = c(2.8,3.8,1,1))
for(i in 1:10){
  safe = safe_nage[[paste0("Age",i)]]
  ceattle <- pollock_base$quantities$NByage[1,1,i,1:49]/1000000
  plot(y = safe, x = 1970:2018, type = "l", xlab = "Year", ylab = "N (millions)", ylim = c(0, max(c(safe, ceattle))))
  lines(y = ceattle, x = 1970:2018, lty = 2)
  legend("topleft", c("SAFE", "CEATTLE"), lty = c(1,2), bty = "n")
  legend("top", paste0("Age ",i), bty = "n")
}
dev.off()


# Scale post 1990 mean
pollock_base_scaled <- pollock_base
pollock_rw_scaled <- pollock_fixed
pollock_rw_scaled$quantities$biomassSSB <- pollock_rw_scaled$quantities$biomassSSB * 1000000

pollock_base_scaled$quantities$biomassSSB <- pollock_base_scaled$quantities$biomassSSB * mean(pollock_safe$quantities$biomassSSB[,21:49]/pollock_base_scaled$quantities$biomassSSB[,21:49])

pollock_rw_scaled$quantities$biomass <- pollock_rw_scaled$quantities$biomass * 1000000

pollock_base_scaled$quantities$biomass <- pollock_base_scaled$quantities$biomass * mean(pollock_safe$quantities$biomass[,21:49]/pollock_base_scaled$quantities$biomass[,21:49])

mod_list <- list(pollock_base, pollock_rw_scaled, pollock_est, pollock_safe)
mod_names <- c("CEATTLE fixed selectivity", "CEATTLE fixed pop params", "CEATTLE estimated", "2018 SAFE (mt)")

plot_biomass(mod_list, file = "scaled", model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
plot_ssb(mod_list, file = "scaled", model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)

# Loglike 1 = catch, 2 = catch-age-comp, 3 = length-comp fishery, 4 = shelikof index, 5 = shelikof age comp, 6 = shelikof length comp, 7 = bt index, 8 = bt age comp, 9 = bt length comp, 10 = nothing, 11, = adfg surve index, 12 = adfg age, 13 = adfg length comp, 14 = age1 index, 15 = age2 index, age 6 = summer acoustic index,