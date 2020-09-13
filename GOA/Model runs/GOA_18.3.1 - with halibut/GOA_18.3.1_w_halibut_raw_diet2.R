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
mydata_list <- list(mydata_coastwide_avg, mydata_coastwide_low, mydata_coastwide_high, mydata_aaf_avg, mydata_aaf_low, mydata_aaf_high, mydata_no_hal_avg, mydata_survey_avg)


# Set atf q to 1
for(i in 1:length(mydata_list)){
  mydata_list[[i]]$fleet_control$Estimate_q[9] <- 0
  mydata_list[[i]]$fday <- 0.5 # Set foraging days to half
}

################################################
# Single species
################################################
# NOTE: Moved the GOA pollock fishery from double logistic to logisitic
ss_run_list <- list()
for(i in 1:2){
  ss_run_list[[i]] <- Rceattle::fit_mod(data_list = mydata_list[[c(1,8)[i]]],
                                        inits = NULL, # Initial parameters = 0
                                        file = "Models/ss_mod0", # Don't save
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

for(i in 1:1){
  
  inits <- ss_run_list[[1]]$estimated_params
  if(i == 8){
    inits <- ss_run_list[[2]]$estimated_params
  }
  
  ms_mod_list[[i]] <- Rceattle::fit_mod(data_list = mydata_list_ms[[i]],
                                        inits = inits, # Initial parameters = 0
                                        file = NULL, # Don't save
                                        debug = 0, # Estimate
                                        random_rec = FALSE, # No random recruitment
                                        msmMode = 1, # Single species mode
                                        silent = TRUE, phase = NULL,
                                        niter = 5)
}

plot_ssb(list(ss_run_base, ms_mod_list[[2]]), model_names = c("ss", "ms"))
plot_biomass(list(ss_run_base, ms_mod_list[[1]]), model_names = c("ss", "ms"))

mod_names <- c("SS", "MS-C avg", "MS-C low", "MS-C high", "MS-AAF avg", "MS-AAF low", "MS-AAF high")
mod_list <- c(list(ss_run_base), ms_mod_list[1:6])

file_name <- "Figures/MS_models"
plot_biomass(mod_list, file = file_name, model_names = mod_names)
plot_ssb(mod_list, file = file_name, model_names = mod_names, right_adj = 8.5)
plot_recruitment(mod_list, file = file_name, add_ci = TRUE, model_names = mod_names)
write_results(mod_list, file = paste0(file_name, ".xlsx"))

# No halibut
mod_names <- c("SS", "MS-C avg", "MS-C low", "MS-C high", "MS-AAF avg", "MS-AAF low", "MS-AAF high", "MS-no halibut", "MS-survey avg")
mod_list <- c(list(ss_run_base), ms_mod_list)

file_name <- "Figures/MS_models_no_halibut"
mod_list <- c(list(ss_run_base), ms_mod_list)
plot_biomass(mod_list, file = file_name, model_names = mod_names)
plot_ssb(mod_list, file = file_name, model_names = mod_names, right_adj = 8.5)
plot_recruitment(mod_list, file = file_name, add_ci = TRUE, model_names = mod_names)


file_name <- "Figures/MS_models_survey"
mod_list <- c(list(ss_run_base), ms_mod_list[7:8])
plot_biomass(mod_list, file = file_name, model_names = mod_names[c(1,8,9)], right_adj = 8.5)
plot_ssb(mod_list, file = file_name, model_names = mod_names[c(1,8,9)], right_adj = 8.5)



#################################
# Check iterations
#################################

ms_iter <- list()

for(i in 3:20){
  ms_iter[[i-2]] <- Rceattle::fit_mod(data_list = mydata_list[[1]],
                                      inits = ms_mod_list[[1]]$estimated_params, # Initial parameters = 0
                                      file = NULL, # Don't save
                                      debug = 1, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 1, # Single species mode
                                      silent = TRUE, phase = NULL,
                                      niter = i)
}

plot_biomass(ms_iter)



ration <- ss_run_list[[i]]$quantities$ration2Age

ration_df <- as.data.frame(ration[1,1,,])
ration_df$Species <- 1
ration_df$Sex <- 0

ration_df2 <- as.data.frame(ration[2,1,,])
ration_df2$Species <- 1
ration_df2$Sex <- 0

ration_df3 <- as.data.frame(ration[3,1,,])
ration_df3$Species <- 3
ration_df3$Sex <- 1

ration_df4 <- as.data.frame(ration[3,2,,])
ration_df4$Species <- 3
ration_df4$Sex <- 2

ration_df5 <- as.data.frame(ration[4,1,,])
ration_df5$Species <- 4
ration_df5$Sex <- 1

ration_df6 <- as.data.frame(ration[4,2,,])
ration_df6$Species <- 4
ration_df6$Sex <- 2


ration_df <- rbind(ration_df, ration_df2, ration_df3, ration_df4, ration_df5, ration_df6)
ration_df$Species_name <- ifelse(ration_df$Species == 1, "Pollock", ifelse(ration_df$Species == 2, "Cod", ifelse(ration_df$Species == 3, "ATF", ifelse(ration_df$Species == 4, "Halibut", NA))))
