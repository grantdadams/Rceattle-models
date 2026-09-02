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
                            M1Fun = build_M1(M1_model = 0,  #do not estimate mortality!
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
test_data$Diet_distribution <- 1 # Dirichlet
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

# Turn on M?

# Run
run_ms_CSL_Mest <- Rceattle::fit_mod(data_list = test_data,
                                     inits = inits, # Initial parameters from single species ests
                                     map = map,
                                     M1Fun = build_M1(M1_model = 1, # Note that because a map is provided that turns off M, M wont be estimated
                                                      updateM1 = FALSE,
                                                      M1_use_prior = FALSE,
                                                      M2_use_prior = FALSE),
                                     file = NULL, # Don't save
                                     estimateMode = 0, # estimate
                                     niter = 3, # 3 iterations around population and predation dynamics
                                     random_rec = FALSE, # No random recruitment
                                     msmMode = 1, # MSVPA based
                                     loopnum = 5,
                                     phase = TRUE,
                                     suitMode = c(0, 4, 4), # empirical + LN suitability
                                     initMode = 2,
                                     verbose = 1)

run_ms_CSL_Mest$quantities$jnll #2713.201 (dev-CAAL 2229.05) w/ DM 2676.533
# Time without ADREPORTing suitability
run_ms_CSL_Mest$run_time # 10.72899 () w/ DM 10.56313 mins
#save(run_ms_CSL_Mest, file = "run_ms_CSL_MestDM.Rdata")


inits2 <- run_ms_CSL_Mest$estimated_params
inits2$log_phi[2:3,1] <- 2:3

run_ms_CSL_Mestph <- Rceattle::fit_mod(data_list = test_data,
                                       inits = inits2, # Initial parameters from single species ests
                                       map = map,
                                       M1Fun = build_M1(M1_model = 1,
                                                        updateM1 = FALSE,
                                                        M1_use_prior = FALSE,
                                                        M2_use_prior = FALSE),
                                       file = NULL, # Don't save
                                       estimateMode = 3, # estimate
                                       niter = 3, # 3 iterations around population and predation dynamics
                                       random_rec = FALSE, # No random recruitment
                                       msmMode = 1, # MSVPA based
                                       suitMode = c(0, 4, 4), # empirical + LN suitability
                                       initMode = 2,
                                       verbose = 1)


sum(run_ms_CSL_Mest$quantities$jnll_comp)
sum(run_ms_CSL_Mestph$quantities$jnll_comp) # changeing phi, changes the likelihood
