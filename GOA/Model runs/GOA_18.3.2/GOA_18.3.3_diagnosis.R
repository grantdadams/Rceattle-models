library(Rceattle)
setwd("Model runs/GOA_18.3.2")

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

# Estimate atf q
for(i in 1:length(mydata_list)){
  mydata_list[[i]]$fleet_control$Estimate_q[9] <- 2
  mydata_list[[i]]$fleet_control$Comp_weights <- 1 # Add comp weights
  # mydata_list[[i]]$fday <- replace(mydata_list[[i]]$fday, values = rep(0.5, length(mydata_list[[i]]$fday))) # Set foraging days to half
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


# Lets see what is not converging
ss_run_list[[1]]$sdrep$sd[1] # Both on NaNs
ss_run_list[[2]]$sdrep$sd[1]

# Step 1: Check if hessian has any zeros
diag(ss_run_list[[1]]$obj$he()) #inv var ok
diag(ss_run_list[[2]]$obj$he()) #inv var ok

# Step 2: Can hessian invert without parameter
he_1 <- ss_run_list[[1]]$obj$he()
he_2 <- ss_run_list[[2]]$obj$he()
for(i in 1:nrow(he_1)){
  solve(he_1[1:i,1:i])
}
i

for(j in 1:nrow(he_2)){
  solve(he_2[1:j,1:j])
}
j

he_1_2 <- he_1[-i,-i] # Remove parameter 448
he_2_2 <- he_2[-j,-j] # Remove parameter 330

for(i in 1:nrow(he_1_2)){
  solve(he_1_2[1:i,1:i])
}

for(j in 1:nrow(he_2_2)){
  solve(he_2_2[1:j,1:j])
}

he_2_3 <- he_2_2[-j,-j] # Remove parameter 336
for(j in 1:nrow(he_2_3)){
  solve(he_2_3[1:j,1:j])
}

# Step 2: Check parameter bounds
ss_run_list[[1]]$opt$diagnostics[448,] # Not a bounding issue
ss_run_list[[2]]$opt$diagnostics[c(330,336),] # Not a bounding issue 

# Step 3: Fix parameters
ss_run_fixed <- list()
# Fix sel_slp accending for fleet 8
map_1 <- ss_run_list[[1]]$map
map_1[[2]]$sel_slp[1, 8, 1] <- NA
map_1[[1]]$sel_slp <- as.factor(map_1[[2]]$sel_slp)

ss_run_fixed[[1]] <- Rceattle::fit_mod(data_list = mydata_list[[c(1,11)[1]]],
                                       inits = ss_run_list[[1]]$estimated_params, # Initial parameters = 0
                                       file = NULL, # Don't save
                                       map = map_1,
                                       debug = 0, # Estimate
                                       random_rec = FALSE, # No random recruitment
                                       msmMode = 0, # Single species mode
                                       silent = TRUE,
                                       recompile = FALSE,
                                       phase = NULL)

map_2 <- ss_run_list[[2]]$map # selectivity slope and inf of fleet 8
map_2[[2]]$sel_slp[1, 8, 1] <- NA
map_2[[1]]$sel_slp <- as.factor(map_2[[2]]$sel_slp)

map_2[[2]]$sel_inf[1, 8, 1] <- NA
map_2[[1]]$sel_inf <- as.factor(map_2[[2]]$sel_inf)

ss_run_fixed[[2]] <- Rceattle::fit_mod(data_list = mydata_list[[c(1,11)[2]]],
                                       inits = ss_run_list[[2]]$estimated_params, # Initial parameters = 0
                                       file = NULL, # Don't save
                                       map = map_2,
                                       debug = 0, # Estimate
                                       random_rec = FALSE, # No random recruitment
                                       msmMode = 0, # Single species mode
                                       silent = TRUE,
                                       recompile = FALSE,
                                       phase = NULL)
ss_run_fixed[[2]]$sdrep

he_1 <- ss_run_fixed[[2]]$obj$he()
for(i in 1:nrow(he_1)){
  solve(he_1[1:i,1:i])
}
i
ss_run_list[[2]]$opt$diagnostics[i,] # Not a bounding issue
ss_run_list[[2]]$estimated_params$sel_slp


# Step 4: Change other food

# Change pcod longline survey selectivity to double logistic
mydata_list_2 <- mydata_list
for(i in 1:length(mydata_list_2)){
  mydata_list_2[[i]]$fleet_control$Selectivity[8] <- 3
}


ss_run_list_2 <- list()
for(i in 1:2){
  ss_run_list_2[[i]] <- Rceattle::fit_mod(data_list = mydata_list_2[[c(1,11)[i]]],
                                        inits = ss_run_list[[i]]$estimated_params, # Initial parameters = 0
                                        file = NULL, # Don't save
                                        debug = 0, # Estimate
                                        random_rec = FALSE, # No random recruitment
                                        msmMode = 0, # Single species mode
                                        silent = TRUE,
                                        recompile = FALSE,
                                        phase = "default")
}



# Change pcod longline survey selectivity to double logistic
mydata_list_3 <- mydata_list
for(i in 1:length(mydata_list_3)){
  mydata_list_3[[i]]$fleet_control$Selectivity[8] <- 2
  mydata_list_3[[i]]$fleet_control$Nselages[8] <- 9
  mydata_list_3[[i]]$fleet_control$Time_varying_sel[8] <- 20
  mydata_list_3[[i]]$fleet_control$Sel_sd_prior[8] <- 12.50
}


ss_run_list_3 <- list()
for(i in 1:2){
  ss_run_list_3[[i]] <- Rceattle::fit_mod(data_list = mydata_list_3[[c(1,11)[i]]],
                                          inits = NULL, # Initial parameters = 0
                                          file = NULL, # Don't save
                                          debug = 0, # Estimate
                                          random_rec = FALSE, # No random recruitment
                                          msmMode = 0, # Single species mode
                                          silent = TRUE,
                                          recompile = FALSE,
                                          phase = "default")
}
plot_selectivity(ss_run_list_3[[1]], file = "18.3.3.np_sel_long")
plot_selectivity(ss_run_list_3[[2]], file = "18.3.3.np_sel_short")
