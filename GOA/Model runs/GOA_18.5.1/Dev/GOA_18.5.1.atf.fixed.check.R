library(Rceattle)
library(readxl)
setwd("Model runs/GOA_18.5.1/")

################################################
# Data
################################################
# Read the data in
mydata_atf_fixed <- Rceattle::read_data( file = "Data/GOA_18.5.1_arrowtooth_single_species_1961-2018.xlsx")
# Fishery - Non-parametric
# Survey 1 - Logistic, q = 1


#######################################
# Mod 0 - Estimate
#######################################
mydata_atf_est <- mydata_atf_fixed
mydata_atf_est$estDynamics = 0

dat <- rearrange_dat(mydata_atf_est)
dat$pop_wt_index
dat$ssb_wt_index

atf_base <- Rceattle::fit_mod(
  # cpp_directory = "C:/Users/Grant Adams/Documents/GitHub/Rceattle/inst/executables",
  data_list = mydata_atf_est,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  debug = FALSE, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  silent = TRUE,
  recompile = FALSE,
  phase = "default")

#######################################
# Mod 1 - Fix selectivity
#######################################
mydata_fix_sel <- mydata_atf_est
mydata_fix_sel$fleet_control$Selectivity = 0
atf_fix_sel <- Rceattle::fit_mod(
  data_list = mydata_fix_sel,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  debug = FALSE, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  silent = TRUE,
  recompile = FALSE,
  phase = "default")



#######################################
# Mod 2 - Fix n-at-age and parameters - Check likelihoods
#######################################
mydata_atf_fixed <- mydata_atf_fixed
mydata_atf_fixed$end = 2017
mydata_atf_fixed$estDynamics = 1
mydata_atf_fixed$NByageFixed[,5:25] <- mydata_atf_fixed$NByageFixed[,5:25] / 1000

# Scale n-at-age to vals
mydata_atf_fixed$msmMode = 0
inits <- build_params(mydata_atf_fixed)

# Fishery selectivity - Non-parametric
inits$sel_coff[3,1,1:19] <- c(-4.65087351891, -3.75901911880, -2.88721410018, -2.08094554736, -1.39011040645, -0.829995544198, -0.375593174815, -0.00125280471673, 0.276509770849, 0.425214562398, 0.451794548745, 0.444573389256, 0.433263388997, 0.420785121459, 0.408655239792, 0.397245031178, 0.387001735832, 0.377863662879, 0.369603901535)
inits$sel_coff[3,2,1:19] <- c(-4.35903772245, -3.51769871061, -2.68753068312, -1.89414463489, -1.17342708695, -0.562419140903, -0.0901839194654, 0.230049485795, 0.404394136214, 0.461851259951, 0.453394143228, 0.426415480051, 0.393511946438, 0.359302151175, 0.326174154244, 0.295704889235, 0.269115063399, 0.247647496296, 0.233459438294)

# Fishing mortality
inits$ln_mean_F[3] <- -4.50387519715
inits$F_dev[3,1:57] <- c(-2.10118573832, -2.08609460688, -2.07238231061, -2.06153201768, -2.05425215475, -0.478702455147, -0.554059609917, -0.842930032347, -1.09310650402, -0.725258859188, -1.18097262406, 0.161044900859, 0.987223711945, 0.286614090823, -0.280016927229, -0.205809302710, 0.898169196703, 0.739521803541, 0.573780621201, 0.533751335827, 0.397891658976, -0.158772136148, 0.0682657340178, -0.618072494943, -1.50607467815, -1.80756793220, -0.461027239290, -0.468819449538, -1.19352794105, -0.137203438670, 0.0892802018383, 0.515014341803, 0.450957213491, 0.835436982352, 0.567635305760, 0.754611773107, 0.420270563691, 0.185132760047, 0.407125221340, 0.817849584284, 0.632051274971, 0.693441583899, 1.02999120951, 0.331551923921, 0.547692253355, 0.846605872459, 0.736627174417, 0.858810526778, 0.695620271202, 0.669992593886, 0.940512939417, 0.553316796356, 0.630390169204, 1.19224912175, 0.598489973600, 0.685794462012, 0.754653304508)

# Recruitment
rec_devs <- c(-0.739710902648, -0.237138698571, -0.268353357901, -0.301867192388, -0.337505720529, -0.375031912909, -0.414088611502, -0.454340440749, -0.495357027284, -0.536600282121, -0.577367504712, -0.616819962668, -0.653608862842, -0.685885882083, -0.711623905743, -0.706079505905, -0.692292752822, -0.647376957152, -0.584069818184, -0.551065401423, -0.560477810220, -0.588151167004, -0.625849890608, -0.659791642825, -0.693804018794, -0.718442253025, -0.730745178336, -0.703450418372, -0.640373884111, -0.526387774500, -0.371823217485, -0.165854200884, 0.100405270028, 0.164069868753, 0.0363368301871, 0.210413173685, 0.306921959568, 0.446140456923, 0.543776378424, 0.446017997671, 0.268735102643, 0.191866395446, 0.215939022922, 0.453604639234, 0.734287225895, 0.564850056242, 0.599553011941, 0.741336430642, 0.605042509596, 0.475610843795, 0.622614043434, 0.519527794187, 0.289041744625, 0.353741926375, 0.389949546224, 0.382605126510, 0.651344120618, 0.837038020887, 0.873969353410, 1.14567938117, 0.692245495666, 0.622527730009, 0.513015167763, 0.590386638062, 0.600923019971, 0.566808345383, 0.218103299351, 0.210533753689, -0.109161997485, -0.125284188942, 0.105061535089, 0.293061518087, 0.269651324027, 0.127461439969, -0.264736348518, 0.0903210770001, 1.16145763022e-007)
inits$ln_mn_rec = 19.4535948326
inits$rec_dev[1:57] <- rec_devs[21:77]
inits$init_dev[1:20] <- rec_devs[1:20]

# Survey 1 - Logistic, q = 1
inits$ln_sel_slp[1,1,1] <- log(1.29423268707) # Female
inits$ln_sel_slp[1,1,2] <- log(0.499999956106) # Male
inits$sel_inf[1,1,1] <- 3.25807092424 # Female
inits$sel_inf[1,1,2] <- 4.48573166827 # Male

# Survey Males - Logistic, q = 1
inits$ln_sel_slp[1,2,1] <- log(1.29423268707) # Female
inits$ln_sel_slp[1,2,2] <- log(0.499999956106) # Male
inits$sel_inf[1,2,1] <- 3.25807092424 # Female
inits$sel_inf[1,2,2] <- 4.48573166827 # Male


library(Rceattle)
atf_fixed <- Rceattle::fit_mod(data_list = mydata_atf_fixed,
                               inits = NULL, # Initial parameters = 0
                               file = NULL, # Don't save
                               debug = 1, # Estimate
                               random_rec = FALSE, # No random recruitment
                               msmMode = 0, # Single species mode
                               silent = TRUE,
                               recompile = FALSE,
                               phase = "default")

# # Check selectivity
# # - Survey
# round(atf_fixed$quantities$sel[1,,,1] - mydata_atf_fixed$emp_sel[3:4, 6:26],5)
# # - Fishery
# round(atf_fixed$quantities$sel[2,,,1] - mydata_atf_fixed$emp_sel[1:2, 6:26],6)
# #  Selectivities are the same
# 
# 
# round(atf_fixed$quantities$jnll_comp,4)[1:12,1:2]
# 
# safe_jnll <- read.csv( file = "Data/ATF Tests/2018_SAFE_nll_components.csv")
# rownames(safe_jnll) <- safe_jnll[,1]
# safe_jnll = safe_jnll[,-c(1, 4:9)]
# round(safe_jnll,6)[1:12,1:2]
# 
# # Save bits
# srv_biom <- atf_fixed$data_list$srv_biom
# srv_biom$est <- atf_fixed$quantities$srv_bio_hat
# 
# # Look at index
# library(readxl)
# safe_2018_index <- as.data.frame(read_xlsx("Data/2018_safe_expected_survey.xlsx", sheet = 1))
# srv_biom$SAFE <- NA
# index_cols <- data.frame(Index = c(7,1,2,3,4,5,6), Col = c(2,3,4,5,6,7,8))
# for(i in 1:nrow(index_cols)){
#   sub <- which(srv_biom$Fleet_code == index_cols$Index[i])
#   yrs <- srv_biom$Year[sub]
#   bio_hat <- safe_2018_index[which(safe_2018_index$Year %in% yrs),index_cols$Col[i]]
#   srv_biom$SAFE[sub] <- bio_hat
# }
# srv_biom
# srv_biom$RE <- (srv_biom$est - srv_biom$SAFE)/srv_biom$SAFE
# write.csv(srv_biom, file = "srv_biom.csv")
# 
# 
# # Check selectivities
# # Srv 1 is all good
# 
# comp_hat <- mydata_atf_fixed$comp_data[,1:8]
# comp_hat <- cbind(comp_hat, atf_fixed$quantities$comp_hat)
# 
# age_hat <- mydata_atf_fixed$comp_data[,1:8]
# age_hat <- cbind(age_hat, atf_fixed$quantities$age_hat)
# 
# write.csv(comp_hat, file = "comp_hat.csv")
# write.csv(t(atf_fixed$quantities$NByage[1,1,,]), file = "n_hat.csv")
# write.csv(t(atf_fixed$quantities$Zed[1,1,,]), file = "zed_hat.csv")
# write.csv(age_hat, file = "c_hat.csv")
# 



######################### 
# SAFE Models
#########################

library(readxl)
safe2018biomass <- as.data.frame(read_xlsx("Data/ATF Tests/2018_SAFE_atf_estimates.xlsx", sheet = 1))
safe2018ssb <- as.data.frame(read_xlsx("Data/ATF Tests/2018_SAFE_atf_estimates.xlsx", sheet = 2))
safe2018rec <- as.data.frame(read_xlsx("Data/ATF Tests/2018_SAFE_atf_estimates.xlsx", sheet = 3))

atf_safe_list <- list()
for(i in 1:1){
  atf_safe_list[[i]] <- atf_base
  atf_safe_list[[i]]$quantities$biomass[1,1:57] <- t(safe2018biomass[,i+1])
  atf_safe_list[[i]]$quantities$biomassSSB[1,1:57] <- t(safe2018ssb[,i+1])
  atf_safe_list[[i]]$quantities$R[1,1:57] <- t(safe2018rec[,i+1])
}

######################### 
# Plots
#########################
# - SAFE vs SS
file_name <- "Figures/18.5.1/18.5.1_SAFE_vs_ceattle_atf"
mod_list <- c(list(atf_base, atf_fixed, atf_safe_list[[1]]))
mod_names <- c( "CEATTLE est", "CEATTLE natage fixed", "2018 SAFE (mt)")
for(i in 1:length(mod_list)){
  mod_list[[i]]$data_list$endyr = 2017
}


plot_biomass(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
plot_ssb(mod_list, file = file_name, model_names = mod_names, right_adj = 0.27, line_col = NULL, species = 1)
