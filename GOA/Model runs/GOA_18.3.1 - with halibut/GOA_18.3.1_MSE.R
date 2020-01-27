library(Rceattle)
setwd("Model runs/GOA_18.3.1 - with halibut")


################################################
# Data
################################################
# Read the data in
mydata_aaf <- Rceattle::read_data( file = "GOA_18.3.1_small_pcod_removed_aaf_halibut_total_diet2.xlsx")
mydata_aaf$projyr <- 2100


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


# AAF
mydata_aaf_avg <- mydata_aaf
mydata_aaf_avg$NByageFixed[,5:ncol(mydata_aaf_avg$NByageFixed)] <- mydata_aaf_avg$NByageFixed[,5:ncol(mydata_aaf_avg$NByageFixed)] * c(halibut_dist_avg$Region.3, halibut_dist_avg$Region.3)


# Adjust halibut
NByageFixed <- data.frame(Species_name = rep("Halibut", 2* length(2019:2050)), 
                                         Species = rep(4, 2* length(2019:2050)),
                                         Sex = rep(c(1,2), each = length(2019:2050)),
                                         Year = rep(c(2019:2050), 2))
NByageFixed_Comp <- data.frame(matrix(0, ncol = 30, nrow = nrow(NByageFixed)))
NByageFixed <- cbind(NByageFixed, NByageFixed_Comp)
colnames(NByageFixed) <- colnames(mydata_aaf_avg$NByageFixed)
mydata_aaf_avg$NByageFixed <- rbind(mydata_aaf_avg$NByageFixed, NByageFixed)

# Set diet to 0 for halibut
mydata_aaf_avg$UobsWtAge$Stomach_proportion_by_weight[which(mydata_aaf_avg$UobsWtAge$Pred == 4)] <- 0
mydata_aaf_avg$UobsWtAge$Stomach_proportion_by_weight[which(mydata_aaf_avg$UobsWtAge$Prey == 4)] <- 0


# Combine in list
mydata_GOA_ss <-  mydata_aaf_avg


# Set atf q to 1 and adjust F prop
f_prop_cod <- mydata_aaf_avg$fsh_biom
f_prop_cod <- f_prop_cod[which(f_prop_cod$Species==2 & f_prop_cod$Year == 2018),]
f_prop_cod <- f_prop_cod$Catch/sum(f_prop_cod$Catch)
f_prop_cod


# mydata_GOA_ss$fleet_control$Estimate_q[9] <- 0
mydata_GOA_ss$fleet_control$proj_F_prop[10:14] <- c(1,f_prop_cod,1)


################################################
# Single species
################################################
# NOTE: Moved the GOA pollock fishery from double logistic to logisitic
ss_GOA <- Rceattle::fit_mod(TMBfilename = "ceattle_v01_07", 
                            cpp_directory = "C:/Users/Grant Adams/Documents/GitHub/Rceattle/inst/executables",
                            data_list = mydata_GOA_ss,
                            inits = NULL, # Initial parameters = 0
                            file = "Models/ss_mod0", # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE,
                            recompile = FALSE,
                            phase = "default")


plot_biomass(ss_run_list, incl_proj = TRUE)
plot_catch(ss_run_list[[2]], incl_proj = TRUE)

################################################
# Model 2 - Add multi-species
################################################

# Update M1 so it is smaller
mydata_GOA_ms <- mydata_GOA_ss
mydata_GOA_ms$M1_base[1,3] <- .1 + 0.06169283
mydata_GOA_ms$M1_base[1,4:6] <- c(0.1, 0.1, 0.1)
mydata_GOA_ms$M1_base[2,3] <- 0.1
mydata_GOA_ms$M1_base[3,3] <- 0.01
mydata_GOA_ms$M1_base[4,3] <- 0.01
mydata_GOA_ms$BTempC <-mydata_GOA_ms$BTempC * 0 + 5.55042

inits <- ss_GOA$estimated_params
ms_GOA <- Rceattle::fit_mod(TMBfilename = "ceattle_v01_07", 
                            cpp_directory = "C:/Users/Grant Adams/Documents/GitHub/Rceattle/inst/executables",
                            data_list =mydata_GOA_ms,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = FALSE, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1 , # Multi-species mode
                            silent = TRUE, phase = NULL, recompile = FALSE,
                            niter = 3)


ss_mse_GOA <- mse_run(operating_model = ss_GOA, estimation_model = ss_GOA, nsim = 20, assessment_period = 2, sampling_period = 2, simulate = TRUE, cap = NULL)
ms_mse_GOA <- mse_run(operating_model = ms_GOA, estimation_model = ss_GOA, nsim = 20, assessment_period = 2, sampling_period = 2, simulate = TRUE, cap = NULL)

