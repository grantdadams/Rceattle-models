library(Rceattle)
setwd("Model runs/GOA_18.5.1/")

################################################
# Data
################################################
# Read the data in
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_18.5.1_pollock_single_species_1970-2018.xlsx")
mydata_pollock2 <- Rceattle::read_data( file = "Data/GOA_18.5.1_pollock_single_species_1970-2018_fixed_bt_sel.xlsx")

mydata_pollock$sigma_rec_prior <- 4
pollock_base <- Rceattle::fit_mod(data_list = mydata_pollock,
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
mydata_pollock_rw$fleet_control$Time_varying_q[c(1,2,3,7)] <- 1
mydata_pollock_rw$fleet_control$Q_sd_prior[c(1,2,3,7)] <- 0.2

mydata_pollock_rw$fleet_control$Time_varying_sel[8] <- 1
mydata_pollock_rw$fleet_control$Sel_sd_prior[8] <- 0.05
mydata_pollock_rw$fleet_control$Selectivity[c(8)] <- 3

pollock_rw <- Rceattle::fit_mod(data_list = mydata_pollock_rw,
                                inits = NULL, # Initial parameters = 0
                                file = NULL, # Don't save
                                debug = 0, # Estimate
                                random_rec = FALSE, # No random recruitment
                                msmMode = 0, # Single species mode
                                silent = TRUE,
                                recompile = FALSE,
                                phase = "default")

mydata_pollock_fq <- mydata_pollock_rw
mydata_pollock_fq$fleet_control$Estimate_q[2] <- 0
mydata_pollock_fq$fleet_control$Log_q_prior <- log(0.85)
pollock_fix_q <- Rceattle::fit_mod(data_list = mydata_pollock_fq,
                                   inits = NULL, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   debug = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 0, # Single species mode
                                   silent = TRUE,
                                   recompile = FALSE,
                                   phase = "default")


# SAFE Models
library(readxl)
safe2018biomass <- as.data.frame(read_xlsx("Data/2018_SAFE_pollock_estimates.xlsx", sheet = 1))
safe_nage <- as.data.frame(read_xlsx("Data/2018_safe_n_at_age.xlsx", sheet = 1))
safe2018ssb <- as.data.frame(read_xlsx("Data/2018_SAFE_pollock_estimates.xlsx", sheet = 2))
safe2018rec <- as.data.frame(read_xlsx("Data/2018_SAFE_pollock_estimates.xlsx", sheet = 3))
pollock_safe <- pollock_base
pollock_safe$quantities$biomass[1,1:49] <- t(safe2018biomass[,2]) * 1000000
pollock_safe$quantities$biomassSSB[1,1:49] <- t(safe2018ssb[,2])  * 1000000 
pollock_safe$quantities$R[1,1:49] <- t(safe2018rec[,2]) * 1000000

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
pollock_rw$quantities$biomass[1,1:49] <- colSums(pollock_rw$quantities$biomassByage[1,3:10,1:49])
pollock_fix_q$quantities$biomass[1,1:49] <- colSums(pollock_fix_q$quantities$biomassByage[1,3:10,1:49])

######################### Plots
# - SAFE vs SS
file_name <- "Figures/18.5.1/18.5.1_SAFE_vs_ceattle_pollock"
mod_list <- list(pollock_base, pollock_rw, pollock_safe)
mod_names <- c("CEATTLE pollock", "CEATTLE pollock rw", "2018 SAFE (mt)")

plot_biomass(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
plot_ssb(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
plot_recruitment(mod_list, file = file_name, add_ci = FALSE, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)



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


# Loglike 1 = catch, 2 = catch-age-comp, 3 = length-comp fishery, 4 = shelikof index, 5 = shelikof age comp, 6 = shelikof length comp, 7 = bt index, 8 = bt age comp, 9 = bt length comp, 10 = nothing, 11, = adfg surve index, 12 = adfg age, 13 = adfg length comp, 14 = age1 index, 15 = age2 index, age 6 = summer acoustic index,