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


# Run individual models

mydata_pollock$styr = 1977
pollock_base <- Rceattle::fit_mod(data_list = mydata_pollock,
                                  inits = NULL, # Initial parameters = 0
                                  file = "2018pollock", # Don't save
                                  debug = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  silent = TRUE,
                                  recompile = FALSE,
                                  phase = "default")


mydata_atf_est$styr = 1977
atf_base <- Rceattle::fit_mod(data_list = mydata_atf_est,
                              inits = NULL, # Initial parameters = 0
                              file = "2018atf", # Don't save
                              debug = 0, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              silent = TRUE,
                              recompile = FALSE,
                              phase = "default")


mydata_pcod_est$styr = 1977
cod_base <- Rceattle::fit_mod(data_list = mydata_pcod_est,
                              inits = NULL, # Initial parameters = 0
                              file = "2018cod", # Don't save
                              debug = 0, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              silent = TRUE,
                              recompile = FALSE,
                              phase = "default")


# Run combined model
data <-  combine_data(data_list1 = combine_data(data_list1 = mydata_pollock, data_list2 = mydata_atf_est), data_list2 =  mydata_pcod_est)
# data <-   combine_data(data_list1 =  mydata_pollock, data_list2 =  mydata_pollock)
# write_data(data, file = "GOA_Rceattle_model.xlsx")
data$msmMode <- 0


data$styr <- 1977
# data$estDynamics <- c( 0, 0, 1)

inits <- build_params(data)


##### Pollock inits
# Fishery selectivity
for(i in 1:2){
  inits$ln_sel_slp[1:2,8 * i,1] <- 0.5
  inits$sel_inf[1:2,8 * i,1] <- c(5,10)
  # inits$ln_sel_slp_dev[1,8 * i,1,] <- pollock_base$estimated_params$ln_sel_slp_dev[1,8,1,]
  # inits$sel_inf_dev[1,8 * i,1,] <- pollock_base$estimated_params$sel_inf_dev[1,8,1,]
  
  # Fishing mortality
  # inits$ln_mean_F[8 * i] <- pollock_base$estimated_params$ln_mean_F[8]
  # inits$F_dev[8 * i,] <- pollock_base$estimated_params$F_dev[8,]
  # 
  # # Recruitment
  # inits$ln_mean_rec[1 * i] <- pollock_base$estimated_params$ln_mean_rec[1]
  # inits$rec_dev[1 * i,1:42] <- pollock_base$estimated_params$rec_dev[1:42]
  # inits$init_dev[1 * i,1:9] <- pollock_base$estimated_params$init_dev[1:9]
  
  # # Survey 1 - Descending logistic, random walk q
  # inits$ln_sel_slp[2,c(1,7) * i,1] <- pollock_base$estimated_params$ln_sel_slp[2,c(1,7),1]
  # inits$sel_inf[2,c(1,7) * i,1] <- pollock_base$estimated_params$sel_inf[2,c(1,7),1]
  # inits$ln_srv_q[c(1:6) * i] <- pollock_base$estimated_params$ln_srv_q[1:6]
  # inits$ln_srv_q_dev[c(1,3) * i,] <- pollock_base$estimated_params$ln_srv_q_dev[c(1,3),]
  # 
  # # Survey 2 -  Logistic, prior on q
  # inits$ln_sel_slp[1,c(2:3) * i,1] <- pollock_base$estimated_params$ln_sel_slp[1,2:3,1]
  # inits$sel_inf[1,c(2:3) * i,1] <- pollock_base$estimated_params$sel_inf[1,2:3,1] 
}



# Fit model
Mod_18_5_1 <- Rceattle::fit_mod(data_list = data,
                                inits = NULL, # Initial parameters = 0
                                file = NULL, # Don't save
                                debug = 0, # Estimate
                                random_rec = FALSE, # No random recruitment
                                msmMode = 0, # Single species mode
                                silent = TRUE,
                                recompile = FALSE,
                                phase = "default")

plot_biomass(list(Mod_18_5_1))





# Check data is the same
check <- c()
for(i in 1:length(Mod_18_5_1$data_list)){
  name_sub <- names(Mod_18_5_1$data_list)[i]
  
  if(class(pollock_base$data_list[[name_sub]]) == "data.frame"){
    check[i] <- sum(Mod_18_5_1$data_list[[name_sub]][1:nrow(pollock_base$data_list[[name_sub]]),1:ncol(pollock_base$data_list[[name_sub]])] != pollock_base$data_list[[name_sub]], na.rm = TRUE)
  } else {
    check[i] <- sum(Mod_18_5_1$data_list[[name_sub]] != pollock_base$data_list[[name_sub]], na.rm = TRUE)
  }
}

double_check <- which(check > 0)
names(Mod_18_5_1$data_list)[double_check]
double_check
i = 21
name_sub <- names(Mod_18_5_1$data_list)[i]
name_sub
Mod_18_5_1$data_list[[name_sub]][1:nrow(pollock_base$data_list[[name_sub]]),1:ncol(pollock_base$data_list[[name_sub]])]
pollock_base$data_list[[name_sub]]

sum(Mod_18_5_1$data_list[[name_sub]][1:nrow(pollock_base$data_list[[name_sub]]),1:ncol(pollock_base$data_list[[name_sub]])] != pollock_base$data_list[[name_sub]], na.rm = TRUE)



Mod_18_5_1$identified$BadParams[which(Mod_18_5_1$identified$BadParams$Param_check == "Bad"),]
Mod_18_5_1$identified$param_list$ln_sel_slp[1,15,1]


# Map out selectivity

params <- build_params(data)
map <- build_map(data, params)
map[[2]]$sel_inf[1,15,1] <- NA; map[[2]]$sel_inf[2,1,1] <- NA; map[[2]]$sel_inf[2,7,1] <- NA;map[[2]]$sel_inf[2,8,1] <- NA
map[[2]]$ln_sel_slp[1,15,1] <- NA; map[[2]]$ln_sel_slp[2,1,1] <- NA; map[[2]]$ln_sel_slp[2,7,1] <- NA;map[[2]]$ln_sel_slp[2,8,1] <- NA

map[[1]]$ln_sel_slp <- as.factor(map[[2]]$ln_sel_slp)
map[[1]]$sel_inf <- as.factor(map[[2]]$sel_inf)

Mod_18_5_1 <- Rceattle::fit_mod(data_list = data,
                                inits = params, # Initial parameters = 0
                                map = map,
                                file = NULL, # Don't save
                                debug = 0, # Estimate
                                random_rec = FALSE, # No random recruitment
                                msmMode = 0, # Single species mode
                                silent = TRUE,
                                recompile = FALSE,
                                phase = "default")




plot_biomass(list(Mod_18_5_1, cod_base), species = 1)

# Diagnostics

opt <- optim(par <- Mod_18_5_1$obj$par, fn = Mod_18_5_1$obj$fn, gr = Mod_18_5_1$obj$gr, control = list(maxit = 2e6))
opt

Mod_18_5_1$quantities$jnll_comp[,1:8] - pollock_base$quantities$jnll_comp
Mod_18_5_1$quantities$jnll_comp[,9:11] - atf_base$quantities$jnll_comp

# somethings up with fsh_biom and srv_biom
pollock_base$data_list$srv_biom$Hat <- pollock_base$quantities$srv_bio_hat
Mod_18_5_1$data_list$srv_biom$Hat <- Mod_18_5_1$quantities$srv_bio_hat
head(pollock_base$data_list$srv_biom) 
head(Mod_18_5_1$data_list$srv_biom)

# Input data?
data_pollock_base <- rearrange_dat(data1)
data_check_pollock <- rearrange_dat(mydata_pollock)
head(data_pollock_base$srv_biom_obs)
head(data_check_pollock$srv_biom_obs)

# Not a problem

# Catchability?
pollock_base$quantities$srv_q - Mod_18_5_1$quantities$srv_q[1:8,]
atf_base$quantities$srv_q - Mod_18_5_1$quantities$srv_q[9:11,]
# Not a problem

atf_base$estimated_params$srv_q_pow
# srv q power  work

# Total mort?
pollock_base$quantities$Zed[1,1,1:10,1:42] - Mod_18_5_1$quantities$Zed[1,1,1:10,1:42] # Good for pollock
atf_base$quantities$Zed[1,1,1:21,1:42] - Mod_18_5_1$quantities$Zed[2,1,1:21,1:42] # Good ATF
atf_base$quantities$Zed[1,2,1:21,1:42] - Mod_18_5_1$quantities$Zed[2,2,1:21,1:42] # Good ATF
# Not right for ATF

# M2 is good
pollock_base$quantities$M2[1,1,1:10,1:42] - Mod_18_5_1$quantities$M2[1,1,1:10,1:42] # Good for pollock
atf_base$quantities$M2[1,1,1:21,1:42] - Mod_18_5_1$quantities$M2[2,1,1:21,1:42]
atf_base$quantities$M2[1,2,1:21,1:10] - Mod_18_5_1$quantities$M2[2,2,1:21,1:10]

# F_tot 
pollock_base$quantities$F_tot[1,1,1:10,1:42] - Mod_18_5_1$quantities$F_tot[1,1,1:10,1:42] # Good for pollock
atf_base$quantities$F_tot[1,1,1:21,1:42] - Mod_18_5_1$quantities$F_tot[2,1,1:21,1:42] # Not right for ATF
atf_base$quantities$F_tot[1,2,1:21,1:10] - Mod_18_5_1$quantities$F_tot[2,2,1:21,1:10] # Not right for ATF


# F
atf_base$quantities$F[1,1,1:21,1:10] - Mod_18_5_1$quantities$F[9,1,1:21,1:10] # Not right for ATF
atf_base$quantities$F[1,2,1:21,1:10] - Mod_18_5_1$quantities$F[9,2,1:21,1:10] # Not right for ATF
atf_base$quantities$F[2,1,1:21,1:10] - Mod_18_5_1$quantities$F[10,1,1:21,1:10] # Not right for ATF
atf_base$quantities$F[2,2,1:21,1:10] - Mod_18_5_1$quantities$F[10,2,1:21,1:10] # Not right for ATF
atf_base$quantities$F[3,1,1:21,1:10] - Mod_18_5_1$quantities$F[11,1,1:21,1:10] # Not right for ATF
atf_base$quantities$F[3,2,1:21,1:10] - Mod_18_5_1$quantities$F[11,2,1:21,1:10] # Not right for ATF
atf_base$estimated_params$F_dev



# nbyage
pollock_base$quantities$NByage[1,1,1:10,1:42] - Mod_18_5_1$quantities$NByage[1,1,1:10,1:42]
atf_base$quantities$NByage[1,1,1:21,1:42] - Mod_18_5_1$quantities$NByage[2,1,1:21,1:42]
atf_base$quantities$NByage[1,2,1:21,1:10] - Mod_18_5_1$quantities$NByage[2,2,1:21,1:10]
# right


# Sel?
pollock_base$quantities$sel[1,1,1:10,1:42] - Mod_18_5_1$quantities$sel[1,1,1:10,1:42] # Good for pollock
pollock_base$quantities$sel[2,1,1:10,1:42] - Mod_18_5_1$quantities$sel[2,1,1:10,1:42] # Good for pollock
pollock_base$quantities$sel[3,1,1:10,1:42] - Mod_18_5_1$quantities$sel[3,1,1:10,1:42] # Good for pollock
pollock_base$quantities$sel[4,1,1:10,1:42] - Mod_18_5_1$quantities$sel[4,1,1:10,1:42] # Good for pollock
pollock_base$quantities$sel[5,1,1:10,1:42] - Mod_18_5_1$quantities$sel[5,1,1:10,1:42] # Good for pollock
pollock_base$quantities$sel[6,1,1:10,1:42] - Mod_18_5_1$quantities$sel[6,1,1:10,1:42] # Good for pollock
pollock_base$quantities$sel[7,1,1:10,1:42] - Mod_18_5_1$quantities$sel[7,1,1:10,1:42] # Bad for fleet 7
pollock_base$quantities$sel[8,1,1:10,1:42] - Mod_18_5_1$quantities$sel[8,1,1:10,1:42] # Good for pollock


atf_base$quantities$sel[1,1,1:21,1:42] - Mod_18_5_1$quantities$sel[9,1,1:21,1:42]
atf_base$quantities$sel[2,1,1:21,1:42] - Mod_18_5_1$quantities$sel[10,1,1:21,1:42]
atf_base$quantities$sel[3,1,1:21,1:42] - Mod_18_5_1$quantities$sel[11,1,1:21,1:42]

atf_base$quantities$sel[1,2,1:21,1:42] - Mod_18_5_1$quantities$sel[9,2,1:21,1:42]
atf_base$quantities$sel[2,2,1:21,1:42] - Mod_18_5_1$quantities$sel[10,2,1:21,1:42]
atf_base$quantities$sel[3,2,1:21,1:42] - Mod_18_5_1$quantities$sel[11,2,1:21,1:42]
# Sel is good


# nbyage
pollock_base$quantities$NByage[1,1,1:10,1:42] - Mod_18_5_1$quantities$NByage[1,1,1:10,1:42]
atf_base$quantities$NByage[1,1,1:21,1:42] - Mod_18_5_1$quantities$NByage[2,1,1:21,1:42]
atf_base$quantities$NByage[1,2,1:21,1:10] - Mod_18_5_1$quantities$NByage[2,2,1:21,1:10]
# Not right


# Sex ratio is good
pollock_base$quantities$R_sexr
atf_base$quantities$R_sexr
Mod_18_5_1$quantities$R_sexr

# Sex ratio is good
pollock_base$quantities$pop_scalar
atf_base$quantities$pop_scalar
Mod_18_5_1$quantities$pop_scalar

# Recruitment?
pollock_base$quantities$R[1,1:42] - Mod_18_5_1$quantities$R[1,1:42]
atf_base$quantities$R[1,1:42] - Mod_18_5_1$quantities$R[2,1:42]

# Comp data
comp_hat3 <- atf_base$data_list$comp_data
comp_hat3[,9:125] <- atf_base$quantities$comp_hat

mod_comp <- Mod_18_5_1$data_list$comp_data
mod_comp[,9:125] <- Mod_18_5_1$quantities$comp_hat
mod_comp <- mod_comp[which(mod_comp$Species == 2),]

rowSums(comp_hat3[,9:125] - mod_comp[,9:125])
# comp data looks good, like is bad...




##### ATF Inits
# Fishery selectivity - Non-parametric
inits$sel_coff[11,1:2,1:19] <- atf_base$estimated_params$sel_coff[3,1:2,1:19]

# Fishing mortality
inits$ln_mean_F[11] <- atf_base$estimated_params$ln_mean_F[3]
inits$F_dev[11,1:42] <- atf_base$estimated_params$F_dev[3,1:42]

# Recruitment
inits$ln_mean_rec[2] <- atf_base$estimated_params$ln_mean_rec[1]
inits$rec_dev[2,1:42] <- atf_base$estimated_params$rec_dev[1:42]
inits$init_dev[2,1:20] <- atf_base$estimated_params$init_dev[1:20]

# Survey BT - Logistic, q = 1
inits$ln_sel_slp[1:2,9:10,1:2] <- atf_base$estimated_params$ln_sel_slp[1:2,1:2,1:2]
inits$sel_inf[1:2,9:10,1:2] <- atf_base$estimated_params$sel_inf[1:2,1:2,1:2]


