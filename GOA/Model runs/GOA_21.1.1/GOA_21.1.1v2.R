# Fix M
library(Rceattle)
library(readxl)
setwd("Model runs/GOA_21.1.1/")



######################### 
# Read in different halibut
#########################
# - Long
mydata_aaf <- Rceattle::read_data( file = "Data/GOA_21_1_1_data_1977-2021_aaf_long.xlsx")
mydata_coastwide <- Rceattle::read_data( file = "Data/GOA_21_1_1_data_1977-2021_coastwide_long.xlsx")

# - Short
mydata_aaf_short <- Rceattle::read_data( file = "Data/GOA_21_1_1_data_1993-2021_aaf_short.xlsx")
mydata_coastwide_short <- Rceattle::read_data( file = "Data/GOA_21_1_1_data_1993-2021_coastwide_short.xlsx")


################################################
# Scale Halibut to area 3
################################################
# Scale SS numbers at age to the percent of the stock in 3
# From Ian:
# 2018 Stock distribution estimates for all sizes of Pacific halibut captured by the IPHC's fishery-independent setline survey
# These are roughly applicable to ages 5+.
halibut_dist <- read.csv("Data/Halibut_3_dist_age5plus_1993-2020.csv")
halibut_dist_avg <- rbind(data.frame(Year = 1977:1992, Region.3 = mean(halibut_dist$Region.3)), 
                          halibut_dist,
                          data.frame(Year = 2021, Region.3 = mean(halibut_dist$Region.3)))
halibut_dist_low <- rbind(data.frame(Year = 1977:1992, Region.3 = quantile(halibut_dist$Region.3, probs = 0.15)), 
                          halibut_dist, 
                          data.frame(Year = 2021, Region.3 = quantile(halibut_dist$Region.3, probs = 0.15))) # Lower 25th percentile
halibut_dist_high <- rbind(data.frame(Year = 1977:1992, Region.3 = quantile(halibut_dist$Region.3, probs = 0.85)), 
                           halibut_dist, 
                           data.frame(Year = 2021, Region.3 = quantile(halibut_dist$Region.3, probs = 0.85))) # Upper 75th percentile


# Scale halibut numbers at age
# Coastwide long
# - Avg distribution
mydata_coastwide_avg <- mydata_coastwide
mydata_coastwide_avg$NByageFixed[which(mydata_coastwide_avg$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_avg$NByageFixed)] <- mydata_coastwide_avg$NByageFixed[which(mydata_coastwide_avg$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_avg$NByageFixed)] * c(halibut_dist_avg$Region.3, halibut_dist_avg$Region.3)

# - Low distribution
mydata_coastwide_low <- mydata_coastwide
mydata_coastwide_low$NByageFixed[which(mydata_coastwide_low$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_low$NByageFixed)] <- mydata_coastwide_low$NByageFixed[which(mydata_coastwide_low$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_low$NByageFixed)] * c(halibut_dist_low$Region.3, halibut_dist_low$Region.3)

# - High distribution
mydata_coastwide_high <- mydata_coastwide
mydata_coastwide_high$NByageFixed[which(mydata_coastwide_high$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_high$NByageFixed)] <- mydata_coastwide_high$NByageFixed[which(mydata_coastwide_high$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_high$NByageFixed)] * c(halibut_dist_high$Region.3, halibut_dist_high$Region.3)

# AAF long
# - Avg distribution
mydata_aaf_avg <- mydata_aaf
mydata_aaf_avg$NByageFixed[which(mydata_aaf_avg$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_avg$NByageFixed)] <- mydata_aaf_avg$NByageFixed[which(mydata_aaf_avg$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_avg$NByageFixed)] * c(halibut_dist_avg$Region.3, halibut_dist_avg$Region.3)

# - Low distribution
mydata_aaf_low <- mydata_aaf
mydata_aaf_low$NByageFixed[which(mydata_aaf_low$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_low$NByageFixed)] <- mydata_aaf_low$NByageFixed[which(mydata_aaf_low$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_low$NByageFixed)] * c(halibut_dist_low$Region.3, halibut_dist_low$Region.3)

# - High distribution
mydata_aaf_high <- mydata_aaf
mydata_aaf_high$NByageFixed[which(mydata_aaf_high$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_high$NByageFixed)] <- mydata_aaf_high$NByageFixed[which(mydata_aaf_high$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_high$NByageFixed)] * c(halibut_dist_high$Region.3, halibut_dist_high$Region.3)

# Coastwide short
# - Avg distribution
mydata_coastwide_short_avg <- mydata_coastwide_short
mydata_coastwide_short_avg$NByageFixed[which(mydata_coastwide_short_avg$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_short_avg$NByageFixed)] <- mydata_coastwide_short_avg$NByageFixed[which(mydata_coastwide_short_avg$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_short_avg$NByageFixed)] * c(halibut_dist_avg$Region.3[which(halibut_dist_avg$Year >= 1996)], halibut_dist_avg$Region.3[which(halibut_dist_avg$Year >= 1996)])

# - Low distribution
mydata_coastwide_short_low <- mydata_coastwide_short
mydata_coastwide_short_low$NByageFixed[which(mydata_coastwide_short_low$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_short_low$NByageFixed)] <- mydata_coastwide_short_low$NByageFixed[which(mydata_coastwide_short_low$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_short_low$NByageFixed)] * c(halibut_dist_low$Region.3[which(halibut_dist_low$Year >= 1996)], halibut_dist_low$Region.3[which(halibut_dist_low$Year >= 1996)])

# - High distribution
mydata_coastwide_short_high <- mydata_coastwide_short
mydata_coastwide_short_high$NByageFixed[which(mydata_coastwide_short_high$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_short_high$NByageFixed)] <- mydata_coastwide_short_high$NByageFixed[which(mydata_coastwide_short_high$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_coastwide_short_high$NByageFixed)] * c(halibut_dist_high$Region.3[which(halibut_dist_high$Year >= 1996)], halibut_dist_high$Region.3[which(halibut_dist_high$Year >= 1996)])

# AAF short
# - Avg distribution
mydata_aaf_short_avg <- mydata_aaf_short
mydata_aaf_short_avg$NByageFixed[which(mydata_aaf_short_avg$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_short_avg$NByageFixed)] <- mydata_aaf_short_avg$NByageFixed[which(mydata_aaf_short_avg$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_short_avg$NByageFixed)] * c(halibut_dist_avg$Region.3[which(halibut_dist_avg$Year >= 1996)], halibut_dist_avg$Region.3[which(halibut_dist_avg$Year >= 1996)])

# - Low distribution
mydata_aaf_short_low <- mydata_aaf_short
mydata_aaf_short_low$NByageFixed[which(mydata_aaf_short_low$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_short_low$NByageFixed)] <- mydata_aaf_short_low$NByageFixed[which(mydata_aaf_short_low$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_short_low$NByageFixed)] * c(halibut_dist_low$Region.3[which(halibut_dist_low$Year >= 1996)], halibut_dist_low$Region.3[which(halibut_dist_low$Year >= 1996)])

# - High distribution
mydata_aaf_short_high <- mydata_aaf_short
mydata_aaf_short_high$NByageFixed[which(mydata_aaf_short_high$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_short_high$NByageFixed)] <- mydata_aaf_short_high$NByageFixed[which(mydata_aaf_short_high$NByageFixed$Species_name == "Halibut"),5:ncol(mydata_aaf_short_high$NByageFixed)] * c(halibut_dist_high$Region.3[which(halibut_dist_high$Year >= 1996)], halibut_dist_high$Region.3[which(halibut_dist_high$Year >= 1996)])



# Set up model list
# The long time-series models for 1977 to 2018 were: 
#   •	Model 1: a model that did not include predation (single-species models) representing a base model. 
# •	Models 2-4: models that included pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide long-time (1917-2018) series model developed by the IPHC. To account for a lack of information on halibut distribution prior to 1993, numbers-at-age prior to 1993 and after 2018 were multiplied by the 50th (model 3), 15th (model 4), and 85th (model 5) quantiles of the distribution of adult halibut in area 3 between 1993 and 2018. 
# •	Models 5-7: as for models 2-4 but using numbers-at-age of Pacific halibut from the areas-as-fleets long-time series model. 

# The three short term models for 1996 to 2018 were: 
#   •	Model 8: a model that did not include predation (single-species models) representing a base model. 
# •	Models 9-11: models that included pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide short-time (1996-2018) series model developed by the IPHC. To account for a lack of information on halibut distribution prior to 1993, numbers-at-age after 2018 were multiplied by the 50th (model 3), 15th (model 4), and 85th (model 5) quantiles of the distribution of adult halibut in area 3 between 1993 and 2018. 
# •	Models 12-14: as for models 9-11 but using numbers-at-age of Pacific halibut from the areas-as-fleets short-time series model. 

mydata_list <- list(
  
  # Long time-series 1977-2018
  mydata_coastwide_avg, # 1 - single-species SAFE M
  mydata_coastwide_avg, # 2 - Multispecies est M1 no halibut
  mydata_coastwide_avg, # 3 - Multi-species est M1 coastwide historical average dist
  mydata_coastwide_low, # 4 - Multi-species est M1 coastwide historical low dist
  mydata_coastwide_high, # 5 - Multi-species est M1 coastwide historical high dist
  mydata_aaf_avg, # 6 - Multi-species est M1 aaf historical avg dist
  mydata_aaf_low, # 7 - Multi-species est M1 aaf historical low dist
  mydata_aaf_high,# 8 - Multi-species est M1 aaf historical high dist
  
  # Short time series 1996-2018
  mydata_coastwide_short_avg, # 9 - single-species SAFE M
  mydata_coastwide_short_avg, # 10 - Multispecies no halibut
  mydata_coastwide_short_avg, # 11 - Multi-species est M1 coastwide future average dist
  mydata_coastwide_short_low, # 12 - Multi-species est M1 coastwide future low dist
  mydata_coastwide_short_high, # 13 - Multi-species est M1 coastwide future high dist
  mydata_aaf_short_avg, # 14 - Multi-species est M1 aaf future avg dist
  mydata_aaf_short_low, # 15 - Multi-species est M1 aaf future low dist
  mydata_aaf_short_high # 16 - Multi-species est M1 aaf future high dist
)

# No halibut
mydata_list[[2]]$pvalue[4] <- 0
mydata_list[[10]]$pvalue[4] <- 0

# Set up inits vectors
inits_M1_df <- data.frame(
  Model = 1:16,
  MsmMode = c(0, rep(1,7), # Long
              0, rep(1,7)), # Short 
  EstM1 = c(0, rep(1,7), # Long
            0, rep(1,7)), # Short
  CompModel = c(NA, rep(1,7), # Long
                NA, rep(9,7)), # Short
  InitModel = c(NA, 1, rep(2,6), # Long
                NA, 9, rep(10,6)) # Short
) 
inits_M1_df$Divergent_jnll <- NA


# Set up M1 estimation switches
for(i in 1:length(mydata_list)){
  mydata_list[[i]]$projyr = 2023
  mydata_list[[i]]$est_M1 = rep(0,4)
  if(inits_M1_df$EstM1[i] == 1){
    # mydata_list[[i]]$est_M1 = c(1,2,1,0)
  }
  
  # Pcod mort
  mydata_list[[i]]$M1_base[4,3:12] <- 0.536892 # Endyr <- 0.485969
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
                                           debug = FALSE, # Estimate
                                           random_rec = FALSE, # No random recruitment
                                           msmMode = 0, # Single species mode
                                           silent = TRUE,
                                           phase = "default")
  }
}

mod_list_unweighted <- mod_list_all[which(inits_M1_df$MsmMode == 0)]
plot_biomass(mod_list_unweighted)

# Reweight the single species Cod model
for(i in 1:length(mydata_list)){
  if(inits_M1_df$MsmMode[i] == 0){
    
    data <- mydata_list[[i]]
    subs <- which(data$fleet_control$Species == 3) # Species 3 is cod
    data$fleet_control$Comp_weights[subs] <- mod_list_all[[i]]$data_list$fleet_control$Est_weights_mcallister[subs]
    
    inits = mod_list_all[[i]]$estimated_params
    inits$comp_weights[subs] <- data$fleet_control$Comp_weights[subs]
    
    # Refit
    mod_list_all[[i]] <- Rceattle::fit_mod(data_list = data,
                                           inits = inits, # Initial parameters = 0
                                           file = NULL, # Don't save
                                           debug = 0, # Estimate
                                           random_rec = FALSE, # No random recruitment
                                           msmMode = 0, # Single species mode
                                           silent = TRUE,
                                           phase = "default")
  }
  
  # Update composition weights for Cod of data set from Init Model- 
  if(inits_M1_df$MsmMode[i] != 0){
    CompModel <- inits_M1_df$CompModel[i]
    subs <- which(mydata_list[[i]]$fleet_control$Species == 3)
    mydata_list[[i]]$fleet_control$Comp_weights[subs] <- mod_list_all[[CompModel]]$data_list$fleet_control$Comp_weights[subs]
  }
}

plot_biomass(mod_list_all[c(1,9)])


#########################
# Compare with SAFE Models
#########################
# Columns = year, pollock, cod, atf
safe2021biomass <- as.data.frame(read_xlsx("Data/2021_SAFE_biomass_estimate.xlsx", sheet = 1))
safe2021ssb <- as.data.frame(read_xlsx("Data/2021_SAFE_biomass_estimate.xlsx", sheet = 2))
safe2021rec <- as.data.frame(read_xlsx("Data/2021_SAFE_biomass_estimate.xlsx", sheet = 3))

# Assign data to CEATTLE object
Mod_2021_SAFE <- mod_list_all[[1]]
# - Pollock
Mod_2021_SAFE$quantities$biomass[1,1:45] <- safe2021biomass$`2021_Pollock` * 1000000
Mod_2021_SAFE$quantities$biomassSSB[1,1:45] <-  safe2021ssb$`2021_Pollock` * 1000000

# - ATF
Mod_2021_SAFE$quantities$biomass[2,1:45] <- safe2021biomass$`2021_ATF` 
Mod_2021_SAFE$quantities$biomassSSB[2,1:45] <-  safe2021ssb$`2021_ATF` 

# - Cod
Mod_2021_SAFE$quantities$biomass[3,1:45] <- safe2021biomass$`2021_Cod` 
Mod_2021_SAFE$quantities$biomassSSB[3,1:45] <- safe2021ssb$`2021_Cod`

# Convert to age-3 biomass
Mod_2021_1_1_3plusBiomass <- mod_list_all[which(inits_M1_df$MsmMode == 0)]
for(i in 1:length(Mod_2021_1_1_3plusBiomass)){
  Mod_2021_1_1_3plusBiomass[[i]]$quantities$biomass[1,1:(Mod_2021_1_1_3plusBiomass[[i]]$data_list$endyr - Mod_2021_1_1_3plusBiomass[[i]]$data_list$styr + 1)] <- colSums(Mod_2021_1_1_3plusBiomass[[i]]$quantities$biomassByage[1,3:10,1:(Mod_2021_1_1_3plusBiomass[[i]]$data_list$endyr - Mod_2021_1_1_3plusBiomass[[i]]$data_list$styr + 1)])
}

plot_biomass(c(Mod_2021_1_1_3plusBiomass, list(Mod_2021_SAFE)), file =  "Results/21.1.1. Bridging weighted Oct 2021 v2", model_names = c("2021 CEATTLE SS - long", "2021 CEATTLE SS - short", "2021 SAFE"), right_adj = 0.27, line_col = NULL, species = c(1:3))



################################################
# Model 2 - Run multi-species
################################################
# Set M1 to 2018 model estimates

# - Run models
# run_models <- function(inits_M1_df, data_list, iter = 3)
for(i in 1:length(mydata_list)){
  if(inits_M1_df$MsmMode[i] == 1){
    if(is.na(inits_M1_df$Divergent_jnll[i])){
      
      # Set up initial values
      init_model <- inits_M1_df$InitModel[i]
      inits = mod_list_all[[init_model]]$estimated_params
      
      # Set M1 to 2018 model estimates
      inits$ln_M1[1,,] <- log(0.328) # pollock
      inits$ln_M1[2,1,] <- log(0.288) # arrowtooth females
      inits$ln_M1[2,2,] <- log(0.354) # arrowtooth males
      inits$ln_M1[3,,] <- log(0.474) # Pacific cod
      
      # Estimate M1
      # mydata_list[[i]]$est_M1 = c(1,2,1,0)
      
      # Fit model
      mod_list_all[[i]] <- try( Rceattle::fit_mod(
        data_list = mydata_list[[i]],
        inits = mod_list_all[[i]]$estimated_params, # Initial parameters = 0
        file = NULL, # Don't save
        debug = FALSE, # Estimate
        random_rec = FALSE, # No random recruitment
        msmMode = 1, # Multi species mode
        silent = TRUE, phase = NULL,
        niter = 3),
        silent = TRUE)
      
      # mod_list_all[[i]]$quantities$jnll_comp
      # plot_catch(mod_list_all[[i]])
      
      
      # # Fit 5 iter
      # mod_list_all[[i]] <- try( Rceattle::fit_mod(
      #   data_list = data,
      #   inits = mod_list_all[[i]]$estimated_params, # Initial parameters = 0
      #   file = NULL, # Don't save
      #   debug = 0, # Estimate
      #   random_rec = FALSE, # No random recruitment
      #   msmMode = 1, # Multi species mode
      #   silent = TRUE, phase = NULL,
      #   niter = 3),
      #   silent = TRUE)
      
      # If it didn't work - phase it
      if( class(mod_list_all[[i]]) == "try-error" ){
        mod_list_all[[i]] <- try( Rceattle::fit_mod(
          data_list = mydata_list[[i]],
          inits = inits, # Initial parameters = 0
          file = NULL, # Don't save
          debug = 0, # Estimate
          random_rec = FALSE, # No random recruitment
          msmMode = 1, # Multi species mode
          silent = TRUE, phase = "default",
          niter = 3),
          silent = TRUE)
        
        # Phase in predation if doesnt converge
        if( class(mod_list_all[[i]]) == "try-error" ){
          
          fday_vec <- seq(0.5,1, by = 0.1)
          
          for(j in 1:length(fday_vec)){
            my_data_tmp <- mydata_list[[i]]
            my_data_tmp$fday <- replace(my_data_tmp$fday, values = rep(fday_vec[j], length(my_data_tmp$fday))) # Set foraging days to half
            
            if(j > 1){
              inits <- mod_list_all[[i]]$estimated_params
            }
            
            # Re-estimate
            mod_list_all[[i]] <- Rceattle::fit_mod(
              data_list = my_data_tmp,
              inits = inits, # Initial parameters = 0
              file = NULL, # Don't save
              debug = 0, # Estimate
              random_rec = FALSE, # No random recruitment
              msmMode = 1, # Multi species mode
              silent = TRUE, phase = "default",
              niter = 3)
          }
        }
      }
      
      if( class(mod_list_all[[i]]) != "try-error" ){
        # If Hessian cant invert or is discontinuous - PHASE
        if( is.null(mod_list_all[[i]]$opt$objective)){
          mod_list_all[[i]] <- try( Rceattle::fit_mod(
            data_list = mydata_list[[i]],
            inits = inits, # Initial parameters = 0
            file = NULL, # Don't save
            debug = 0, # Estimate
            random_rec = FALSE, # No random recruitment
            msmMode = 1, # Multi species mode
            silent = TRUE, phase = "default",
            niter = 3),
            silent = TRUE)
        }
        
        # Discontinuous ll
        if(!is.null(mod_list_all[[i]]$opt$objective)){
          if(abs(mod_list_all[[i]]$opt$objective -  mod_list_all[[i]]$quantities$jnll) > 1 ){
            mod_list_all[[i]] <- try( Rceattle::fit_mod(
              data_list = mydata_list[[i]],
              inits = mod_list_all[[i]]$estimated_params, # Initial parameters = 0
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
      gc()
    }
  }
}


# Check convergence
inits_M1_df$Objective <- sapply(mod_list_all[1:11], function(x) x$opt$objective)
inits_M1_df$Divergent_jnll <- NA
for(i in 1:length(mod_list_all)){
  if(!is.null(mod_list_all[[i]]$opt$objective)){
    inits_M1_df$Divergent_jnll[i] <- round(mod_list_all[[i]]$quantities$jnll - mod_list_all[[i]]$opt$objective,3)
  }
}


# Plot to see if things are reasonable
plot_biomass(mod_list_all[which(!is.na(inits_M1_df$Divergent_jnll))])


# Change initial value model and rerun
inits_M1_df$InitModel <- c(NA,rep(3,6),NA,11,11,11,11,11,11)


mod_list_all_save <- mod_list_all


# Save
save(mod_list_all, file = paste0("Models/20_1_1_Niter5_", Sys.Date(),".RData"))
