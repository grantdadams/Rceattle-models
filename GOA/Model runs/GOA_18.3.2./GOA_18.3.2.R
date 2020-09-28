library(Rceattle)
setwd("Model runs/GOA_18.3.1 - with halibut")

# Updated the ALK

################################################
# Data
################################################
# Read the data in
mydata_aaf <- Rceattle::read_data( file = "GOA_18.3.1_small_pcod_removed_aaf_halibut_total_diet2.xlsx")
mydata_coastwide <- Rceattle::read_data( file = "GOA_18.3.1_small_pcod_removed_coastwide_halibut_total_diet2.xlsx")
mydata_survey <- Rceattle::read_data( file = "GOA_18.3.1_small_pcod_removed_survey_halibut_total_diet2.xlsx")
mydata_no_hal <- Rceattle::read_data( file = "GOA_18.3.1_small_pcod_removed_coastwide_halibut_total_no_halibut_uobs.xlsx")

# Note: diet data is from age 0-2, 2-3, 3-4, 4-5,... Nages+. Plus groups for diet data for ATF is 16 for and for Halibut 16 as well.


################################################
# Scale Halibut to area 3
################################################
# Scale SS numbers at age to the percent of the stock in 3
# From Ian:
# 2018 Stock distribution estimates for all sizes of Pacific halibut captured by the IPHC's fishery-independent setline survey
# These are roughly applicable to ages 5+.
halibut_dist <- read.csv("Halibut_3_dist_age5plus.csv")
halibut_dist_avg <- rbind(data.frame(Year = 1977:1992, Region.3 = mean(halibut_dist$Region.3)), halibut_dist)
halibut_dist_low <- rbind(data.frame(Year = 1977:1992, Region.3 = quantile(halibut_dist$Region.3, probs = 0.25)), halibut_dist) # Lower 25th percentile
halibut_dist_high <- rbind(data.frame(Year = 1977:1992, Region.3 = quantile(halibut_dist$Region.3, probs = 0.75)), halibut_dist) # Upper 75th percentile


# Scale halibut numbers at age
# Coastwide
mydata_no_hal_avg <- mydata_no_hal
mydata_no_hal_avg$NByageFixed[,5:ncol(mydata_no_hal_avg$NByageFixed)] <- mydata_no_hal_avg$NByageFixed[,5:ncol(mydata_no_hal_avg$NByageFixed)] * c(halibut_dist_avg$Region.3, halibut_dist_avg$Region.3)

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


# Combine in list
mydata_list_long <- list(mydata_no_hal_avg, mydata_coastwide_avg, mydata_coastwide_low, mydata_coastwide_high, mydata_aaf_avg, mydata_aaf_low, mydata_aaf_high)
mydata_list_short <- list( mydata_no_hal_avg, mydata_coastwide_avg, mydata_aaf_avg, mydata_survey_avg)

# Adjust start year
for(i in 1:length(mydata_list_short)){
  mydata_list_short[[i]]$styr <- 1993
}

mydata_list <- c(mydata_list_long, mydata_list_short)

# Set atf q to 1
for(i in 1:length(mydata_list)){
  mydata_list[[i]]$fleet_control$Estimate_q[9] <- 0
  mydata_list[[i]]$fleet_control$Comp_weights <- 1 # Add comp weights
  mydata_list[[i]]$fday <- replace(mydata_list[[i]]$fday, values = rep(0.5, length(mydata_list[[i]]$fday))) # Set foraging days to half
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
  ss_run_list_weighted[[i]] <- Rceattle::fit_mod(data_list = ss_run_list[[i]]$data_list,
                                        inits = NULL, # Initial parameters = 0
                                        file = NULL, # Don't save
                                        debug = 0, # Estimate
                                        random_rec = FALSE, # No random recruitment
                                        msmMode = 0, # Single species mode
                                        silent = TRUE,
                                        recompile = FALSE,
                                        phase = "default")
}


################################################
# Model 2 - Add multi-species
################################################
mydata_list_ms <- mydata_list
# Update M1 so it is smaller
for(i in 1:length(mydata_list_ms)){
  mydata_list_ms[[i]]$M1_base[1,3] <- .1 + 0.06169283
  mydata_list_ms[[i]]$M1_base[1,4:12] <- 0.1
  mydata_list_ms[[i]]$M1_base[2,3] <- 0.1
  mydata_list_ms[[i]]$M1_base[3,3] <- 0.01
  mydata_list_ms[[i]]$M1_base[4,3] <- 0.01
  mydata_list_ms[[i]]$BTempC <- mydata_list_ms[[i]]$BTempC * 0 + 5.55042
  # 
  # mydata_list_ms[[i]]$M1_base[1,3:32] <- 0.1
  # mydata_list_ms[[i]]$M1_base[2,3:32] <- 0.1
  # mydata_list_ms[[i]]$M1_base[3,3:32] <- 0.1
  # mydata_list_ms[[i]]$M1_base[4,3:32] <- 0.1
  # mydata_list_ms[[i]]$BTempC <- mydata_list_ms[[i]]$BTempC * 0 + 5.55042
}



ms_mod_list <- list()

for(i in 1:length(mydata_list_ms)){
  
  inits <- ss_run_list_weighted[[1]]$estimated_params
  mydata_list_ms[[i]]$fleet_control$Comp_weights <- ss_run_list[[1]]$data_list$fleet_control$Comp_weights
  if(i > 2){
    inits <- ms_mod_list[[2]]$estimated_params
  }
  
  if(i >= 8){
    inits <- ss_run_list_weighted[[2]]$estimated_params
    mydata_list_ms[[i]]$fleet_control$Comp_weights <- ss_run_list[[2]]$data_list$fleet_control$Comp_weights
    if(i > 8){
      inits <- ms_mod_list[[8]]$estimated_params
    }
  }
  
  ms_mod_list[[i]] <- Rceattle::fit_mod(data_list = mydata_list_ms[[i]],
                                        inits = inits, # Initial parameters = 0
                                        file = NULL, # Don't save
                                        debug = 0, # Estimate
                                        random_rec = FALSE, # No random recruitment
                                        msmMode = 1, # Multi species mode
                                        silent = TRUE, phase = NULL,
                                        niter = 5)
}

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



mod_names_long <- c("SS", "MS-No Halibut", "MS-Coast avg", "MS-Coast low", "MS-Coast high", "MS-AAF avg", "MS-AAF low", "MS-AAF high")
mod_names_short <- c("SS", "MS-No Halibut", "MS-Coast", "MS-AAF", "MS-Survey")
mod_list_long <- c(list(ss_run_list_weighted[[1]]), ms_mod_list[1:7])
mod_list_short <- c(list(ss_run_list_weighted[[2]]), ms_mod_list[8:11])

mod_list_all <- c(list(ss_run_list_weighted[[1]]), ms_mod_list[1:7], list(ss_run_list_weighted[[2]]), ms_mod_list[8:11])
mod_names_all <- c(mod_names_long, mod_names_short)

save(ms_mod_list, file = "Models/18_3_2.RData")

file_name <- "Figures/MS_models_long"
plot_biomass(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 9)
plot_ssb(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 9)
plot_recruitment(mod_list_long, file = file_name, add_ci = FALSE, model_names = mod_names_long, right_adj = 9)

# Short
file_name <- "Figures/MS_models_short"
plot_biomass(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 5.5)
plot_ssb(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 5.5)
plot_recruitment(mod_list_short, file = file_name, add_ci = TRUE, model_names = mod_names_short, right_adj = 5.5)


file_name <- "Figures/MS_models_all"
plot_biomass(mod_list_all, file = file_name, model_names = mod_names_all, right_adj = 9)
plot_ssb(mod_list_all, file = file_name, model_names = mod_names_all, right_adj = 9)
plot_recruitment(mod_list_all, file = file_name, add_ci = FALSE, model_names = mod_names_all, right_adj = 9)
