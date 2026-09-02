library(Rceattle)
library(ggplot2)
library(dplyr)
library(tidyverse)
set.seed(123)

#SBF_ATF_hakedata <- Rceattle::read_data(file = "241025_SBF_ATF_Hake.xlsx")
SBF_ATF_hakedata <- Rceattle::read_data(file = "Dev/110226_SBF_ATF_Hake.xlsx")
#SBF_ATF_hakedata$projyr<- 2019

ss_run <- Rceattle::fit_mod(data_list = SBF_ATF_hakedata,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            phase = TRUE,
                            getsd = FALSE,
                            verbose = 1)

ss_run$quantities$jnll #2217.028 (dev-CAAL 2217.028)(different from before because init devs are off)
#save(ss_run, file = "results/models/ATF_SBF/ATF_SBF_ss_run.Rdata")

ms_run <- Rceattle::fit_mod(data_list = SBF_ATF_hakedata,
                            inits = ss_run$estimated_params, # Initial parameters from single species ests
                            M1Fun = build_M1(M1_model = 1,  # Estimate M1
                                             updateM1 = FALSE,
                                             M1_use_prior = FALSE,
                                             M2_use_prior = FALSE),
                            getsd = FALSE,
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            niter = 3, # 3 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # MSVPA based
                            suitMode = 0, # empirical suitability
                            initMode = 2, # Fished start with init devs
                            verbose = 1)

ms_run$quantities$jnll #2229.18 (dev-CAAL 2229.18)





#####################################################################
# FIX SUITABILITY AND SUM across prey ages (NEW PART)
# Create initial parameter list:
test_data <- SBF_ATF_hakedata
test_data$Diet_distribution <- rep(1, test_data$nspp) # Dirichlet
inits = ms_run$estimated_params
map = ms_run$map # gam_a, gam_b, and log_phi are turned off here AND M1

# Create a list prey size preference
inits$log_gam_a = c(0, 3.7, 3.1)  # Mean log weight ratio for ATF, 0 for other species (pred/prey)
inits$log_gam_b = c(0, 1.8, 1.120)


# Set vulnerability matrix
inits$log_phi #Currently all set to 0.5 (keep it)
inits$log_phi[1,1] <- -999 # Hake cannibalism is MSVPA
inits$log_phi[1,2] <- -999 # Fixing so hake do not prey on ATF
inits$log_phi[2,2] <- -999 # Set ATF do not feed on ATF
inits$log_phi[1,3] <- -999 # Fixing so hake do not prey on SBF
inits$log_phi[3,3] <- -999 # Set SBF do not feed on SBF
inits$log_phi[2,3] <- -999 # Set ATF do not feed on SBF
inits$log_phi[3,2] <- -999 # Set SBF do not feed on ATF

# Do this to estimate vulnerability and log_phi :
map$mapList$log_phi[] <- 1:length(map$mapList$log_phi) # Unique number for each parameter
map$mapList$log_phi[1,1] <- NA #so we dont estimate on hake on hake
map$mapList$log_phi[1,2] <- NA #so we dont estimate on hake on atf
map$mapList$log_phi[2,2] <- NA #so we dont estimate atf on atf
map$mapList$log_phi[1,3] <- NA #so we dont estimate on hake on SBF
map$mapList$log_phi[3,3] <- NA #so we dont estimate on SBF on SBF
map$mapList$log_phi[2,3] <- NA #so we dont estimate on atf on sbf
map$mapList$log_phi[3,2] <- NA #so we dont estimate sbf on atf

map$mapFactor$log_phi <- factor(map$mapList$log_phi)

# Turn of DM pars
map$mapList$diet_comp_weights[2:3] <- 2:3
map$mapFactor$diet_comp_weights <- factor(map$mapList$diet_comp_weights)
#
# for(i in 1:length(names(inits))){
#     if(names(inits)[i] %in% names(dev_hake$estimated_params)){
#         inits[[i]][] <- dev_hake$estimated_params[[(names(inits)[i])]]
#     }
# }


# Fit using Hake-test, save, then fit using dev-CAAL and compare.
dev_iter2 <- Rceattle::fit_mod(data_list = test_data,
                                     inits = inits, # Initial parameters from MSVPA
                                     map = map,
                                     M1Fun = build_M1(M1_model = 1,
                                                      M1_use_prior = FALSE,
                                                      M2_use_prior = FALSE),
                                     file = NULL, # Don't save
                                     estimateMode = 0, # Fix
                                     niter = 3, # 5 iterations around population and predation dynamics
                                     random_rec = FALSE, # No random recruitment
                                     msmMode = 1, # MSVPA based
                                     loopnum = 5,
                                     phase = TRUE,
                                     suitMode = c(0, 4, 4), # empirical + LN suitability
                                     initMode = 2,
                                     verbose = 1)

gr <- dev_iter2$obj$gr()
pars <- dev_iter2$obj$par
check <- data.frame(names = names(par), par = par, gr = gr[1,])


dev_iter2 <- Rceattle::fit_mod(data_list = dev_iter2$data_list,
                               inits = dev_iter2$estimated_params, # Initial parameters from MSVPA
                               map = dev_iter2$map,
                               M1Fun = build_M1(M1_model = 1,
                                                M1_use_prior = FALSE,
                                                M2_use_prior = FALSE),
                               file = NULL, # Don't save
                               estimateMode = 0, # Fix
                               niter = 3, # 1 iterations around population and predation dynamics
                               random_rec = FALSE, # No random recruitment
                               msmMode = 1, # MSVPA based
                               loopnum = 5,
                               suitMode = c(0, 4, 4), # empirical + LN suitability
                               initMode = 2,
                               verbose = 1)

# save(run_ms_CSL_2iter, file = "run_ms_CSL_2iter.Rdata")


# Load from hake-test and compare
load(file = "run_ms_CSL_2iter.Rdata")
dev_hake <- mod_objects
dev_caal <- dev_iter2

sum(dev_hake$quantities$jnll_comp[-c(1, 9),]) # w/ DM 2676.533 (dev-CAAL 2676.534)
sum(dev_caal$quantities$jnll_comp) #2713.201 (dev-CAAL 2229.05) w/ DM 2676.533


round(dev_hake$quantities$jnll_comp[-c(1, 9),] - dev_caal$quantities$jnll_comp[-4,], 8)

nyrs <- length(test_data$styr:test_data$endyr)

# Recruitment
testthat::expect_equal(as.numeric(dev_caal$quantities$R[,nyrs]),
                       as.numeric(dev_hake$quantities$R[, nyrs]))
testthat::expect_equal(as.numeric(dev_caal$quantities$biomass[,nyrs]),
                       as.numeric(dev_hake$quantities$biomass[,nyrs]), tolerance = 1e-6)


# Suitability
testthat::expect_equal(exp(dev_caal$estimated_params$log_gam_a),
                       exp(dev_hake$estimated_params$log_gam_a))
testthat::expect_equal(exp(dev_caal$estimated_params$log_gam_b),
                       exp(dev_hake$estimated_params$log_gam_b))
testthat::expect_equal(as.numeric(dev_caal$quantities$vulnerability),
                       as.numeric(dev_hake$quantities$vulnerability))
testthat::expect_equal(as.numeric(dev_caal$quantities$suitability[,,,,1:nyrs]),
                       as.numeric(dev_hake$quantities$suitability[,,,,1:nyrs]))
testthat::expect_equal(as.numeric(dev_caal$quantities$suit_other[,,,1:nyrs]),
                       as.numeric(dev_hake$quantities$suit_other[,,,1:nyrs]))

# M2
testthat::expect_equal(as.numeric(dev_caal$quantities$M2_at_age[,,,1:nyrs]),
                       as.numeric(dev_hake$quantities$M2_at_age[,,,1:nyrs]), tolerance = 1e-6)

# Ration
testthat::expect_equal(as.numeric(dev_caal$quantities$consumption_at_age[,,,1:nyrs]),
                       as.numeric(dev_hake$quantities$consumption_at_age[,,,1:nyrs]))

# N
testthat::expect_equal(as.numeric(dev_caal$quantities$N_at_age[,,,1:nyrs]),
                       as.numeric(dev_hake$quantities$N_at_age[,,,1:nyrs]))

# AvgN
testthat::expect_equal(as.numeric(dev_caal$quantities$avgN_at_age[,,,1:nyrs]),
                       as.numeric(dev_hake$quantities$avgN_at_age[,,,1:nyrs]))

# Avail food
testthat::expect_equal(as.numeric(dev_caal$quantities$avail_food[,,,1:nyrs]),
                       as.numeric(dev_hake$quantities$avail_food[,,,1:nyrs]))

# Selectivity
testthat::expect_equal(as.numeric(dev_caal$quantities$sel_at_age),
                       as.numeric(dev_hake$quantities$sel))

# F
testthat::expect_equal(as.numeric(dev_caal$quantities$F_flt_age),
                       as.numeric(dev_hake$quantities$F_flt_age))

# Q
testthat::expect_equal(as.numeric(dev_caal$quantities$index_q),
                       as.numeric(dev_hake$quantities$index_q))

# Expected and observed diet
order_hake <- order(dev_hake$data_list$diet_data$Pred,
               dev_hake$data_list$diet_data$Prey,
               dev_hake$data_list$diet_data$Pred_age,
               dev_hake$data_list$diet_data$Prey_age,
               dev_hake$data_list$diet_data$Year)
order_caal <- order(dev_caal$data_list$diet_data$Pred,
                    dev_caal$data_list$diet_data$Prey,
                    dev_caal$data_list$diet_data$Pred_age,
                    dev_caal$data_list$diet_data$Prey_age,
                    dev_caal$data_list$diet_data$Year)
testthat::expect_equal(as.numeric(dev_caal$quantities$diet_hat[order_caal,2]),
                       as.numeric(dev_hake$quantities$diet_hat[order_hake,2]))

