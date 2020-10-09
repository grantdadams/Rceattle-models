library(Rceattle)
setwd("Model runs/GOA_18.3.1 - with halibut")

# Updated the ALK

################################################
# Data
################################################
# Read the data in
mydata_aaf <- Rceattle::read_data( file = "GOA_18.3.1_small_pcod_removed_aaf_halibut_total.xlsx")
mydata_coastwide <- Rceattle::read_data( file = "GOA_18.3.1_small_pcod_removed_coastwide_halibut_total.xlsx")


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

# Combine in list
mydata_list <- list(mydata_coastwide_avg, mydata_coastwide_low, mydata_coastwide_high, mydata_aaf_avg, mydata_aaf_low, mydata_aaf_high)

################################################
# Single species
################################################
# NOTE: Moved the GOA pollock fishery from double logistic to logisitc
ss_run_base <- Rceattle::fit_mod(data_list = mydata_aaf,
                                 inits = NULL, # Initial parameters = 0
                                 file = "Models/ss_mod0", # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE,
                                 recompile = FALSE,
                                 phase = "default")




file_name <- "Figures/Base/Base"
plot_index(ss_run_base, file = file_name)
# plot_catch(ss_run_base, file = file_name)
Rceattle::plot_comp(ss_run_base, file = file_name)
plot_biomass(ss_run_base, file = file_name)
plot_ssb(ss_run_base, file = file_name, add_ci = TRUE)
plot_recruitment(ss_run_base, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run_base, file = file_name)
write_results(ss_run_base, file = paste0(file_name, ".xlsx"))




################################################
# Model 2 - Add multi-species
################################################

# Update M1 so it is smaller
for(i in 1:length(mydata_list)){
  mydata_list[[i]]$M1_base[1,3:32] <- 0.1 
  mydata_list[[i]]$M1_base[2,3:32] <- 0.1
  mydata_list[[i]]$M1_base[3,3:32] <- 0.1
  mydata_list[[i]]$M1_base[4,3:32] <- 0.1
  mydata_list[[i]]$M1_base[2,3] <- 0.1
  mydata_list[[i]]$M1_base[3,3:4] <- 0.01
  mydata_list[[i]]$M1_base[4,3] <- 0.01
  mydata_list[[i]]$BTempC <- mydata_list[[i]]$BTempC * 0 + 5.55042
}



ms_mod_list <- list()

for(i in 1:length(mydata_list)){
  ms_mod_list[[i]] <- Rceattle::fit_mod(data_list = mydata_list[[i]],
                                   inits = mod_objects$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   debug = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 1, # Single species mode
                                   silent = TRUE, phase = NULL,
                                   niter = 3)
}

save(ms_mod_list, file = "ms_mod_list_v2.RData")

file_name <- "Figures/MS1/MS0"
plot_index(ms_run_mod1, file = file_name)
# plot_catch(ms_run_mod1, file = file_name)
Rceattle::plot_srv_comp(ms_run_mod1, file = file_name)
Rceattle::plot_fsh_comp(ms_run_mod1, file = file_name)
plot_biomass(ms_mod_list[[1]], file = file_name)
plot_ssb(ms_run_mod1, file = file_name, add_ci = TRUE)
plot_recruitment(ms_run_mod1, file = file_name, add_ci = TRUE)
plot_selectivity(ms_run_mod1, file = file_name)
write_results(ms_run_mod1, file = paste0(file_name, ".xlsx"))

mod_list <- list(ss_run_base, ms_run_mod1)
mod_names <- c("SS", "MS")
plot_biomass(mod_list, model_names = mod_names)
plot_ssb(mod_list, model_names  = mod_names, add_ci = TRUE)
