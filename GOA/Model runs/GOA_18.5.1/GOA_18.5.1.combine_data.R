library(Rceattle)
library(readxl)
setwd("Model runs/GOA_18.5.1/")

################################################
# Data and base models
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

# Combine data
GOA_18_5_data <-  combine_data(data_list1 = combine_data(data_list1 = mydata_pollock, data_list2 = mydata_atf_est), data_list2 =  mydata_pcod_est)
GOA_18_5_data$msmMode <- 0
GOA_18_5_data$styr <- 1977
write_data(GOA_18_5_data, file = "Data/GOA_18_5_1_data_1961-2018.xlsx")

# Species = pollock, atf, cod, halibut
Mod_18_5_1 <- Rceattle::fit_mod(data_list = GOA_18_5_data,
                                inits = NULL, # Initial parameters = 0
                                map = NULL,
                                file = NULL, # Don't save
                                debug = 0, # Estimate
                                random_rec = FALSE, # No random recruitment
                                msmMode = 0, # Single species mode
                                silent = TRUE,
                                recompile = FALSE,
                                phase = "default")


######################### 
# Read in different halibut
#########################
mydata_aaf <- Rceattle::read_data( file = "Data/GOA_18_5_1_data_1961-2018_aaf.xlsx")
mydata_coastwide <- Rceattle::read_data( file = "Data/GOA_18_5_1_data_1961-2018_coastwide.xlsx")
mydata_survey <- Rceattle::read_data( file = "Data/GOA_18_5_1_data_1961-2018_survey.xlsx")


################################################
# Scale Halibut to area 3
################################################
# Scale SS numbers at age to the percent of the stock in 3
# From Ian:
# 2018 Stock distribution estimates for all sizes of Pacific halibut captured by the IPHC's fishery-independent setline survey
# These are roughly applicable to ages 5+.
halibut_dist <- read.csv("Data/Halibut_3_dist_age5plus.csv")
halibut_dist_avg <- rbind(data.frame(Year = 1977:1992, Region.3 = mean(halibut_dist$Region.3)), halibut_dist)
halibut_dist_low <- rbind(data.frame(Year = 1977:1992, Region.3 = quantile(halibut_dist$Region.3, probs = 0.25)), halibut_dist) # Lower 25th percentile
halibut_dist_high <- rbind(data.frame(Year = 1977:1992, Region.3 = quantile(halibut_dist$Region.3, probs = 0.75)), halibut_dist) # Upper 75th percentile


# Scale halibut numbers at age
# - No hal
mydata_no_hal_avg <- mydata_coastwide
mydata_no_hal_avg$Pvalue[4] <- 0 # Set ration to 0
mydata_no_hal_avg$NByageFixed[,5:ncol(mydata_no_hal_avg$NByageFixed)] <- mydata_no_hal_avg$NByageFixed[,5:ncol(mydata_no_hal_avg$NByageFixed)] * c(halibut_dist_avg$Region.3, halibut_dist_avg$Region.3)

# Coastwide
mydata_coastwide_avg <- mydata_coastwide
mydata_coastwide_avg$NByageFixed[,5:ncol(mydata_coastwide_avg$NByageFixed)] <- mydata_coastwide_avg$NByageFixed[,5:ncol(mydata_coastwide_avg$NByageFixed)] * c(halibut_dist_avg$Region.3, halibut_dist_avg$Region.3)

mydata_coastwide_low <- mydata_coastwide
mydata_coastwide_low$NByageFixed[,5:ncol(mydata_coastwide_low$NByageFixed)] <- mydata_coastwide_low$NByageFixed[,5:ncol(mydata_coastwide_low$NByageFixed)] * c(halibut_dist_low$Region.3, halibut_dist_low$Region.3)

mydata_coastwide_high <- mydata_coastwide
mydata_coastwide_high$NByageFixed[,5:ncol(mydata_coastwide_high$NByageFixed)] <- mydata_coastwide_high$NByageFixed[,5:ncol(mydata_coastwide_high$NByageFixed)] * c(halibut_dist_high$Region.3, halibut_dist_high$Region.3)

# AAF
mydata_aaf_avg <- mydata_aaf
mydata_aaf_avg$NByageFixed[,5:ncol(mydata_aaf_avg$NByageFixed)] <- mydata_aaf_avg$NByageFixed[,5:ncol(mydata_aaf_avg$NByageFixed)] * c(halibut_dist_avg$Region.3, halibut_dist_avg$Region.3)

mydata_aaf_low <- mydata_aaf
mydata_aaf_low$NByageFixed[,5:ncol(mydata_aaf_low$NByageFixed)] <- mydata_aaf_low$NByageFixed[,5:ncol(mydata_aaf_low$NByageFixed)] * c(halibut_dist_low$Region.3, halibut_dist_low$Region.3)

mydata_aaf_high <- mydata_aaf
mydata_aaf_high$NByageFixed[,5:ncol(mydata_aaf_high$NByageFixed)] <- mydata_aaf_high$NByageFixed[,5:ncol(mydata_aaf_high$NByageFixed)] * c(halibut_dist_high$Region.3, halibut_dist_high$Region.3)


# Update survey data for pre 1993
mydata_survey_avg <- mydata_survey
mydata_survey_avg$NByageFixed[which(mydata_survey_avg$NByageFixed$Sex==1 & mydata_survey_avg$NByageFixed$Year < 1993),5:ncol(mydata_survey_avg$NByageFixed)] <- colMeans(mydata_survey_avg$NByageFixed[which(mydata_survey_avg$NByageFixed$Sex==1 & mydata_survey_avg$NByageFixed$Year > 1992),5:ncol(mydata_survey_avg$NByageFixed)] * c(halibut_dist_avg$Region.3, halibut_dist_avg$Region.3))

# Add model wilt age-dependent population scalar
mydata_survey_avg_age_dep <- mydata_survey_avg
mydata_survey_avg_age_dep$estDynamics[4] <- 3

# Combine in list
mydata_list_long <- list(mydata_no_hal_avg, mydata_coastwide_avg, mydata_coastwide_low, mydata_coastwide_high, mydata_aaf_avg, mydata_aaf_low, mydata_aaf_high)
mydata_list_short <- list( mydata_no_hal_avg, mydata_coastwide_avg, mydata_aaf_avg, mydata_survey_avg, mydata_survey_avg_age_dep)

# Adjust start year
for(i in 1:length(mydata_list_short)){
  mydata_list_short[[i]]$styr <- 1993
}

mydata_list <- c(mydata_list_long, mydata_list_short)

# Make sure species cant cannibalize older species
for(i in 1:length(mydata_list)){
  for(sp in 1:mydata_list[[i]]$nspp){
    for(age in 1:mydata_list[[i]]$nages[sp]){
      mydata_list[[i]]$UobsWtAge$Stomach_proportion_by_weight[which(mydata_list[[i]]$UobsWtAge$Prey == sp & mydata_list[[i]]$UobsWtAge$Pred == sp & mydata_list[[i]]$UobsWtAge$Pred_age == age & mydata_list[[i]]$UobsWtAge$Prey_age > age & mydata_list[[i]]$UobsWtAge$Prey_sex == mydata_list[[i]]$UobsWtAge$Pred_sex)] <- 0
    }
  }
}

################################################
# Single species
################################################
# NOTE: Moved the GOA pollock fishery from double logistic to logisitic
ss_run_list <- list()
for(i in 1:2){
  ss_run_list[[i]] <- Rceattle::fit_mod(data_list = mydata_list[[c(1,11)[i]]],
                                        inits = NULL, # Initial parameters = 0
                                        file = NULL, # Don't save
                                        debug = 0, # Estimate
                                        random_rec = FALSE, # No random recruitment
                                        msmMode = 0, # Single species mode
                                        silent = TRUE,
                                        recompile = FALSE,
                                        phase = "default")
}

ss_run_list_weighted <- list()
# Reweight the models
for(i in 1:2){
  data <- ss_run_list[[i]]$data_list
  data$fleet_control$Comp_weights <- ss_run_list[[i]]$data_list$fleet_control$Est_weights_macallister
  
  # Refit
  ss_run_list_weighted[[i]] <- Rceattle::fit_mod(data_list = data,
                                                 inits = ss_run_list[[i]]$estimated_params, # Initial parameters = 0
                                                 file = NULL, # Don't save
                                                 debug = 0, # Estimate
                                                 random_rec = FALSE, # No random recruitment
                                                 msmMode = 0, # Single species mode
                                                 silent = TRUE,
                                                 recompile = FALSE,
                                                 phase = NULL)
}



######################### 
# Compare with SAFE Models
#########################
# Columns = year, pollock, cod, atf
safe2018biomass <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 1))
safe2018ssb <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 2))
safe2018rec <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 3))

# Assign data to CEATTLE object
Mod_18_SAFE <- ss_run_list_weighted[[1]]
# - Pollock and ATF
Mod_18_SAFE$quantities$biomass[1:2,1:42] <- t(safe2018biomass[1:42,c(2,4)]) * 1000
Mod_18_SAFE$quantities$biomassSSB[1:2,1:42] <- t(safe2018ssb[1:42,c(2,4)]) * 1000

# - Cod
Mod_18_SAFE$quantities$biomass[3,1:42] <- t(safe2018biomass[1:42,c(3)])
Mod_18_SAFE$quantities$biomassSSB[3,1:42] <- t(safe2018ssb[1:42,c(3)])

# Convert to age-3 biomass
Mod_18_5_1_3plusBiomass <- ss_run_list_weighted
for(i in 1:2){
  Mod_18_5_1_3plusBiomass[[i]]$quantities$biomass[1,1:49] <- colSums(Mod_18_5_1_3plusBiomass[[i]]$quantities$biomassByage[1,3:10,1:49])
}

plot_biomass(c(Mod_18_5_1_3plusBiomass, list(Mod_18_SAFE)), file =  "Figures/18.5.1/18.5.1. Bridging weighted March 2021", model_names = c("2018 CEATTLE SS - long", "2018 CEATTLE SS - short", "2018 SAFE"), right_adj = 0.27, line_col = NULL, species = c(1:3))


################################################
# Model 2 - Add multi-species
################################################
mydata_list_ms <- mydata_list
# Update M1 so it is smaller
for(i in 1:length(mydata_list_ms)){
  mydata_list_ms[[i]]$M1_base[1,3] <- .143
  mydata_list_ms[[i]]$M1_base[1,4:12] <- 0.1
  mydata_list_ms[[i]]$M1_base[2,3] <- 0.1
  mydata_list_ms[[i]]$M1_base[3,3] <- 0.01
  mydata_list_ms[[i]]$M1_base[4,3] <- 0.01
}


# Run models
ms_mod_list <- list()
for(i in 1:length(mydata_list_ms)){
  
  # Initialize from ss weighted
  inits <- ss_run_list_weighted[[1]]$estimated_params
  
  # Comp weights
  mydata_list_ms[[i]]$fleet_control$Comp_weights <- ss_run_list[[1]]$data_list$fleet_control$Est_weights_macallister
  
  # Initialize from previous MS mod
  if(i > 2){
    #inits <- ms_mod_list[[i-1]]$estimated_params
  }
  
  if(i >= 8){
    # Initialize from ss weighted
    inits <- ss_run_list_weighted[[2]]$estimated_params
    
    # Comp weights
    mydata_list_ms[[i]]$fleet_control$Comp_weights <- ss_run_list[[2]]$data_list$fleet_control$Est_weights_macallister
    
    # Initialize from previous MS mod
    if(i > 8){
      #inits <- ms_mod_list[[i-1]]$estimated_params
    }
  }
  
  # Fit model
  ms_mod_list[[i]] <- try( Rceattle::fit_mod(
    data_list = mydata_list_ms[[i]],
    inits = inits, # Initial parameters = 0
    file = NULL, # Don't save
    debug = 0, # Estimate
    random_rec = FALSE, # No random recruitment
    msmMode = 1, # Multi species mode
    silent = TRUE, phase = NULL,
    niter = 5),
    silent = TRUE)
  
  
  # Phase in predation if doesnt converge
  if( class(ms_mod_list[[i]]) == "try-error" ){
    
    fday_vec <- seq(0.1,1, by = 0.1)
    
    for(j in 1:length(fday_vec)){
      my_data_tmp <- mydata_list_ms[[i]]
      my_data_tmp$fday <- replace(my_data_tmp$fday, values = rep(fday_vec[j], length(my_data_tmp$fday))) # Set foraging days to half
      
      if(j > 1){
        inits <- ms_mod_list[[i]]$estimated_params
      }
      
      # Re-estimate
      ms_mod_list[[i]] <- Rceattle::fit_mod(
        data_list = my_data_tmp,
        inits = inits, # Initial parameters = 0
        file = NULL, # Don't save
        debug = 0, # Estimate
        random_rec = FALSE, # No random recruitment
        msmMode = 1, # Multi species mode
        silent = TRUE, phase = NULL,
        niter = 5)
    }
  }
  
  
  # Try and phase if likelihood is discontinous
  if( abs(ms_mod_list[[i]]$opt$objective -  ms_mod_list[[i]]$quantities$jnll) > 1 ){
    ms_mod_list[[i]] <- try( Rceattle::fit_mod(
      data_list = mydata_list_ms[[i]],
      inits = ms_mod_list[[i]]$estimated_params, # Initial parameters = 0
      file = NULL, # Don't save
      debug = 0, # Estimate
      random_rec = FALSE, # No random recruitment
      msmMode = 1, # Multi species mode
      silent = TRUE, phase = "default",
      niter = 5),
      silent = TRUE)
  }
}

# Check convergence
sapply(ms_mod_list[1:11], function(x) x$opt$objective)
sapply(ms_mod_list[1:11], function(x) x$quantities$jnll)
sapply(ms_mod_list, function(x) x[1])

# Re-order and name models
# The long time-series models for 1977 to 2018 were: 
#   •	Model 1: a model that did not include predation (single-species models) representing a base model. 
# •	Model 2: a model that did not include halibut predation to allow comparisons in which halibut does not impact the dynamics of groundfish in the GOA. 
# •	Models 3-5: models that included pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide long-time (1917-2018) series model developed by the IPHC. To account for a lack of information on halibut distribution prior to 1993, numbers-at-age prior to 1993 were multiplied by the 50th (model 3), 15th (model 4), and 85th (model 5) quantiles of the distribution of adult halibut in area 3 between 1993 and 2018. 
# •	Models 6-8: as for models 3-5 but using numbers-at-age of Pacific halibut from the areas-as-fleets long-time series model. 

# The five short term models for 1993 to 2018 were: 
#   •	Model 9: a model that does not include predation (model 9) to represent a base single-species model 
# •	Model 10: a model that did not include halibut predation (model 10). 
# •	Model 11: a model with pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide short-time series model. 
# •	Model 12: as for models 11but using numbers-at-age of Pacific halibut from the areas-as-fleets short-time series model 
# •	Model 13: a model relative abundance-at-age of Pacific halibut in area 3 multiplied by an estimated parameter to allow the model to estimate the relative contribution of Pacific halibut predation to describing the dynamics of pollock, Pacific cod, and arrowtooth flounder. 

mod_list_all <- c(list(ss_run_list_weighted[[1]]), ms_mod_list[1:7], list(ss_run_list_weighted[[2]]), ms_mod_list[8:12])

save(mod_list_all, file = paste0("Models/18_5_1_", Sys.Date(),".RData"))