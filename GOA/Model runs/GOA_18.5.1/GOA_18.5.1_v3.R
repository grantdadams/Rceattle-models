library(Rceattle)
library(readxl)
setwd("Model runs/GOA_18.5.1/")


######################### 
# Read in different halibut
#########################
# - Long
mydata_aaf <- Rceattle::read_data( file = "Data/GOA_18_5_1_data_1977-2018_aaf_long.xlsx")
mydata_coastwide <- Rceattle::read_data( file = "Data/GOA_18_5_1_data_1977-2018_coastwide_long.xlsx")

# - Short
mydata_aaf_short <- Rceattle::read_data( file = "Data/GOA_18_5_1_data_1996-2018_aaf_short.xlsx")
mydata_coastwide_short <- Rceattle::read_data( file = "Data/GOA_18_5_1_data_1996-2018_coastwide_short.xlsx")

# - Survey
mydata_survey <- Rceattle::read_data( file = "Data/GOA_18_5_1_data_1993-2018_survey_short.xlsx")


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
# Coastwide long
mydata_coastwide_avg <- mydata_coastwide
mydata_coastwide_avg$NByageFixed[which(mydata_coastwide_avg$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_avg$NByageFixed)] <- mydata_coastwide_avg$NByageFixed[which(mydata_coastwide_avg$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_avg$NByageFixed)] * c(halibut_dist_avg$Region.3, halibut_dist_avg$Region.3)

mydata_coastwide_low <- mydata_coastwide
mydata_coastwide_low$NByageFixed[which(mydata_coastwide_low$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_low$NByageFixed)] <- mydata_coastwide_low$NByageFixed[which(mydata_coastwide_low$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_low$NByageFixed)] * c(halibut_dist_low$Region.3, halibut_dist_low$Region.3)

mydata_coastwide_high <- mydata_coastwide
mydata_coastwide_high$NByageFixed[which(mydata_coastwide_high$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_high$NByageFixed)] <- mydata_coastwide_high$NByageFixed[which(mydata_coastwide_high$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_high$NByageFixed)] * c(halibut_dist_high$Region.3, halibut_dist_high$Region.3)

# AAF long
mydata_aaf_avg <- mydata_aaf
mydata_aaf_avg$NByageFixed[which(mydata_aaf_avg$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_avg$NByageFixed)] <- mydata_aaf_avg$NByageFixed[which(mydata_aaf_avg$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_avg$NByageFixed)] * c(halibut_dist_avg$Region.3, halibut_dist_avg$Region.3)

mydata_aaf_low <- mydata_aaf
mydata_aaf_low$NByageFixed[which(mydata_aaf_low$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_low$NByageFixed)] <- mydata_aaf_low$NByageFixed[which(mydata_aaf_low$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_low$NByageFixed)] * c(halibut_dist_low$Region.3, halibut_dist_low$Region.3)

mydata_aaf_high <- mydata_aaf
mydata_aaf_high$NByageFixed[which(mydata_aaf_high$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_high$NByageFixed)] <- mydata_aaf_high$NByageFixed[which(mydata_aaf_high$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_high$NByageFixed)] * c(halibut_dist_high$Region.3, halibut_dist_high$Region.3)

# Coastwide short
mydata_coastwide_short$NByageFixed[which(mydata_coastwide_short$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_short$NByageFixed)] <- mydata_coastwide_short$NByageFixed[which(mydata_coastwide_short$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_short$NByageFixed)] * c(halibut_dist_avg$Region.3, halibut_dist_avg$Region.3)

# AAF short
mydata_aaf_short$NByageFixed[which(mydata_aaf_short$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_short$NByageFixed)] <- mydata_aaf_short$NByageFixed[which(mydata_aaf_short$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_short$NByageFixed)] * c(halibut_dist_avg$Region.3, halibut_dist_avg$Region.3)


# Combine in list

# - Long time series
# - No hal
mydata_no_hal_avg <- mydata_coastwide_avg
mydata_no_hal_avg$Pvalue[4] <- 0 # Set ration to 0

mydata_list_long <- list(mydata_no_hal_avg, mydata_coastwide_avg, mydata_coastwide_low, mydata_coastwide_high, mydata_aaf_avg, mydata_aaf_low, mydata_aaf_high)


# - No halibut version of survey 1993-2018
mydata_survey_no_hal_srv <- mydata_survey
mydata_survey_no_hal_srv$Pvalue[4] <- 0 # Set ration to 0

mydata_survey_no_hal_srv$styr <- 1993
mydata_survey$styr <- 1993

# - No halibut version of short 1996-2018
mydata_coastwide_short_no_hal <- mydata_coastwide_short
mydata_coastwide_short_no_hal$Pvalue[4] <- 0 # Set ration to 0

mydata_coastwide_short_no_hal$styr <- 1996
mydata_coastwide_short$styr <- 1996
mydata_aaf_short$styr <- 1996

# Set up model list
# The long time-series models for 1977 to 2018 were: 
#   •	Model 1: a model that did not include predation (single-species models) representing a base model. 
# •	Model 2: a model that did not include halibut predation to allow comparisons in which halibut does not impact the dynamics of groundfish in the GOA. 
# •	Models 3-5: models that included pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide long-time (1917-2018) series model developed by the IPHC. To account for a lack of information on halibut distribution prior to 1993, numbers-at-age prior to 1993 were multiplied by the 50th (model 3), 15th (model 4), and 85th (model 5) quantiles of the distribution of adult halibut in area 3 between 1993 and 2018. 
# •	Models 6-8: as for models 3-5 but using numbers-at-age of Pacific halibut from the areas-as-fleets long-time series model. 

# The three moderate term models for 1993 to 2018 were: 
#   •	Model 9: a model that does not include predation (model 9) to represent a base single-species model 
# •	Model 10: a mutlispecies model that did not include halibut predation (model 10). 
# •	Model 11: a mutlispecies model with relative abundance-at-age of Pacific halibut in area 3 multiplied by an estimated parameter to allow the model to estimate the relative contribution of Pacific halibut predation to describing the dynamics of pollock, Pacific cod, and arrowtooth flounder. 

# The four short term models for 1996 to 2018 were: 
#   •	Model 12: a model that does not include predation (model 9) to represent a base single-species model 
# •	Model 13: a mutlispecies model that did not include halibut predation (model 10). 
# •	Model 14: a model with pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide short-time series model. 
# •	Model 15: as for models 11but using numbers-at-age of Pacific halibut from the areas-as-fleets short-time series model 

mydata_list <- list(
  
  # Long time-series 1977-2018
  mydata_no_hal_avg, # 1 - single-species SAFE M
  mydata_no_hal_avg, # 2 - Multi-species est M1 no halibut
  mydata_coastwide_avg, # 3 - Multi-species est M1 coastwide historical average dist
  mydata_coastwide_low, # 4 - Multi-species est M1 coastwide historical low dist
  mydata_coastwide_high, # 5 - Multi-species est M1 coastwide historical high dist
  mydata_aaf_avg, # 6 - Multi-species est M1 aaf historical avg dist
  mydata_aaf_low, # 7 - Multi-species est M1 aaf historical low dist
  mydata_aaf_high,# 8 - Multi-species est M1 aaf historical high dist
  
  # Medium time series 1993-2018
  mydata_survey_no_hal_srv, # 9 - single-species SAFE M 
  mydata_survey_no_hal_srv, # 10 - Multi-species no hal M1 from #3 
  mydata_survey, # 11 - Multi-species survey n-at-age M1 from #4 
  
  # Medium time series 1993-2018 - 50% M1 sensitivity
  mydata_survey_no_hal_srv, # 12 - Multi-species no hal 50% M1 from #3 
  mydata_survey, # 13 - Multi-species survey n-at-age 50% M1 from #4 
  
  # Medium time series 1993-2018 - 150% M1 sensitivity
  mydata_survey_no_hal_srv, # 14 - Multi-species no hal 150% M1 from #3 
  mydata_survey, # 15 - Multi-species survey n-at-age 150% M1 from #4 
  
  # Short time series 1996-2018
  mydata_coastwide_short_no_hal, # 16 - single-species SAFE M 
  mydata_coastwide_short_no_hal, # 17 - Multi-species no hal M1 from #3 
  mydata_coastwide_short, # 18 - Multi-species M1 from #4 - coastwide short
  mydata_aaf_short, # 19 - Multi-species M1 from #5 - aaf short
  
  # Short time series 1996-2018 - 50% M1 Sensitivity
  mydata_coastwide_short_no_hal, # 20 - Multi-species no hal 50% M1 from #3 
  mydata_coastwide_short, # 21 - Multi-species 50% M1 from #4 - coastwide short
  mydata_aaf_short, # 22 - Multi-species 50% M1 from #5 - aaf short
  
  # Short time series 1996-2018 - 150% M1 Sensitivity
  mydata_coastwide_short_no_hal, # 23 - Multi-species no hal 150% M1 from #3 
  mydata_coastwide_short, # 24 - Multi-species 150% M1 from #4 - coastwide short
  mydata_aaf_short # 25 - Multi-species 150% M1 from #5 - aaf short
)


# Set up inits vectors
inits_M1_df <- data.frame(
  Model = 1:25,
  MsmMode = c(0, rep(1,7), # Long
              0, rep(1,2), rep(1,2), rep(1,2), # Medium
              0, rep(1,3), rep(1,3), rep(1,3)), # Short 
  EstM1 = c(0, rep(1,7), rep(0, 17)),
  InitModel = c(NA, rep(1,7), # Long
                NA, rep(9,2), rep(9,2),rep(9,2), # Medium
                NA, rep(16,3), rep(16,3),rep(16,3)), # Short 
  M1_fixed_Model = c(rep(NA, 8), # Long
                     NA, 3:4,3:4,3:4, # Medium
                     NA, 3:5,3:5,3:5), # Short 
  M_mult = c(rep(NA, 8), # Long
             NA, rep(1,2), rep(0.5,2), rep(1.5,2), # Medium
             NA, rep(1,3), rep(0.5,3), rep(1.5,3)) # Short
) 



# Set up M1 estimation switches
for(i in 1:length(mydata_list)){
  mydata_list[[i]]$est_M1 = rep(0,4)
  if(inits_M1_df$EstM1[i] == 1){
    mydata_list[[i]]$est_M1 = c(1,2,1,0)
  }
}


################################################
# Single species
################################################
mod_list_all <- list()

for(i in 1:length(mydata_list)){
  if(inits_M1_df$MsmMode[i] == 0){
    mod_list_all[[i]] <- Rceattle::fit_mod(data_list = mydata_list[[i]],
                                           inits = NULL, # Initial parameters = 0
                                           file = NULL, # Don't save
                                           debug = 0, # Estimate
                                           random_rec = FALSE, # No random recruitment
                                           msmMode = 0, # Single species mode
                                           silent = TRUE,
                                           recompile = FALSE,
                                           phase = "default")
  }
}

# Reweight the single species models
for(i in 1:length(mydata_list)){
  if(inits_M1_df$MsmMode[i] == 0){
    data <- mod_list_all[[i]]$data_list
    data$fleet_control$Comp_weights <- mod_list_all[[i]]$data_list$fleet_control$Est_weights_macallister
    
    # Refit
    mod_list_all[[i]] <- Rceattle::fit_mod(data_list = data,
                                           inits = mod_list_all[[i]]$estimated_params, # Initial parameters = 0
                                           file = NULL, # Don't save
                                           debug = 0, # Estimate
                                           random_rec = FALSE, # No random recruitment
                                           msmMode = 0, # Single species mode
                                           silent = TRUE,
                                           recompile = FALSE,
                                           phase = NULL)
  }
}



######################### 
# Compare with SAFE Models
#########################
# Columns = year, pollock, cod, atf
safe2018biomass <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 1))
safe2018ssb <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 2))
safe2018rec <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 3))

# Assign data to CEATTLE object
Mod_18_SAFE <- mod_list_all[[1]]
# - Pollock and ATF
Mod_18_SAFE$quantities$biomass[1:2,1:42] <- t(safe2018biomass[1:42,c(2,4)]) * 1000
Mod_18_SAFE$quantities$biomassSSB[1:2,1:42] <- t(safe2018ssb[1:42,c(2,4)]) * 1000

# - Cod
Mod_18_SAFE$quantities$biomass[3,1:42] <- t(safe2018biomass[1:42,c(3)])
Mod_18_SAFE$quantities$biomassSSB[3,1:42] <- t(safe2018ssb[1:42,c(3)])

# Convert to age-3 biomass
Mod_18_5_1_3plusBiomass <- mod_list_all[which(inits_M1_df$MsmMode == 0)]
for(i in 1:3){
  Mod_18_5_1_3plusBiomass[[i]]$quantities$biomass[1,1:49] <- colSums(Mod_18_5_1_3plusBiomass[[i]]$quantities$biomassByage[1,3:10,1:49])
}

plot_biomass(c(Mod_18_5_1_3plusBiomass, list(Mod_18_SAFE)), file =  "Figures/18.5.1/18.5.1. Bridging weighted March 2021", model_names = c("2018 CEATTLE SS - long", "2018 CEATTLE SS - medium", "2018 CEATTLE SS - short", "2018 SAFE"), right_adj = 0.27, line_col = NULL, species = c(1:3))



################################################
# Model 2 - Add multi-species
################################################
ms_mod_list <- list()

# Update M1 so it is smaller
for(i in 1:length(mydata_list_ms)){
  
  # Old version
  # mydata_list_ms[[i]]$M1_base[1,3] <- .143
  # mydata_list_ms[[i]]$M1_base[1,4:12] <- 0.1
  # mydata_list_ms[[i]]$M1_base[2,3] <- 0.1
  # mydata_list_ms[[i]]$M1_base[3,3] <- 0.01
  # mydata_list_ms[[i]]$M1_base[4,3] <- 0.01
  
  # From profiles - Grid search would be better
  mydata_list_ms[[i]]$M1_base[1,3:ncol(mydata_list_ms[[i]]$M1_base)] <- 0.3394736 # Pollock
  mydata_list_ms[[i]]$M1_base[2,3:ncol(mydata_list_ms[[i]]$M1_base)] <- 0.207894 # Atf M
  mydata_list_ms[[i]]$M1_base[3,3:ncol(mydata_list_ms[[i]]$M1_base)] <- 0.28684 # Atf F
  mydata_list_ms[[i]]$M1_base[4,3:ncol(mydata_list_ms[[i]]$M1_base)] <- 0.444736 # Cod
}


# Run models
for(i in 1:length(mydata_list_ms)){
  
  
  # Long time series models (1977-2018)
  if(i >= 1 & i < 8){
    # Initialize from ss weighted
    inits <- ss_run_list_weighted[[1]]$estimated_params
    
    # Comp weights
    mydata_list_ms[[i]]$fleet_control$Comp_weights <- ss_run_list[[1]]$data_list$fleet_control$Est_weights_macallister
    
    # Initialize from previous MS mod
    if(i > 2){
      inits <- ms_mod_list[[i-1]]$estimated_params
    }
  }
  
  
  # Medium time series models (1993-2018)
  if(i >= 8 & i < 10){
    # Initialize from ss weighted
    inits <- ss_run_list_weighted[[2]]$estimated_params
    
    # Comp weights
    mydata_list_ms[[i]]$fleet_control$Comp_weights <- ss_run_list[[2]]$data_list$fleet_control$Est_weights_macallister
    
    # Initialize from previous MS mod
    if(i > 8){
      # inits <- ms_mod_list[[i-1]]$estimated_params
    }
  }
  
  # Short time series models (1996-2018)
  if(i >= 10){
    # Initialize from ss weighted
    inits <- ss_run_list_weighted[[3]]$estimated_params
    
    # Comp weights
    mydata_list_ms[[i]]$fleet_control$Comp_weights <- ss_run_list[[3]]$data_list$fleet_control$Est_weights_macallister
    
    # Initialize from previous MS mod
    # if(i > 10){
    #   inits <- ms_mod_list[[i-1]]$estimated_params
    # }
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
    niter = 3),
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
        niter = 3)
    }
  }
  
  
  # If Hessian cant invert
  if( is.null(ms_mod_list[[i]]$opt$objective) ){
    ms_mod_list[[i]] <- try( Rceattle::fit_mod(
      data_list = mydata_list_ms[[i]],
      inits = ms_mod_list[[i]]$estimated_params, # Initial parameters = 0
      file = NULL, # Don't save
      debug = 0, # Estimate
      random_rec = FALSE, # No random recruitment
      msmMode = 1, # Multi species mode
      silent = TRUE, phase = "default",
      niter = 3),
      silent = TRUE)
  }
  
  # Try and phase if likelihood is discontinous
  if(!is.null(ms_mod_list[[i]]$opt$objective)){
    if( abs(ms_mod_list[[i]]$opt$objective -  ms_mod_list[[i]]$quantities$jnll) > 1 ){
      ms_mod_list[[i]] <- try( Rceattle::fit_mod(
        data_list = mydata_list_ms[[i]],
        inits = ms_mod_list[[i]]$estimated_params, # Initial parameters = 0
        file = NULL, # Don't save
        debug = 0, # Estimate
        random_rec = FALSE, # No random recruitment
        msmMode = 1, # Multi species mode
        silent = TRUE, phase = "default",
        niter = 3),
        silent = TRUE)
    }
  }
}


# Check convergence
sapply(ms_mod_list, function(x) x$opt$objective) 
round(sapply(ms_mod_list, function(x) x$quantities$jnll - x$opt$objective), 3)
sapply(ms_mod_list, function(x) x[1])

# Run models again that have divergent likelihoods
for(i in c(11)){
  # Fit model
  ms_mod_list[[i]] <- try( Rceattle::fit_mod(
    data_list = ms_mod_list[[i]]$data_list,
    inits = ms_mod_list[[i]]$estimated_params, # Initial parameters = 0
    file = NULL, # Don't save
    debug = 0, # Estimate
    random_rec = FALSE, # No random recruitment
    msmMode = 1, # Multi species mode
    silent = TRUE, phase = NULL,
    niter = 3),
    silent = TRUE)
}
ms_mod_list_save <- ms_mod_list


# Save
save(mod_list_all, file = paste0("Models/18_5_1_", Sys.Date(),".RData"))
