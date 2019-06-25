library(Rceattle)
# Make sure you switch to the fit switch version



################################################
# Data
################################################
# Read the data in
mydata <- Rceattle::read_data( file = "GOA_18.1.0 - DOES NOT CONVERGE/GOA2017SS_v3_from_1977_v2_fit_switch.xlsx")
mydata$fsh_control$Nselages[16]

################################################
# Estimation
################################################
srv_fit <- list()
ss_run <- list()
ms_run <- list()

run_mat <- matrix(NA, ncol = 2, nrow = 10)

inits <- build_params(mydata)
inits$ln_mn_rec <- c(4,4,4)

# Run base
ss_run[[1]] <- Rceattle::fit_mod(data_list = mydata,
                                 inits = inits, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 0, # Single species mode
                                 silent = TRUE)

run_mat[1,1] <- sum(!is.na(ss_run[[1]]$sdrep$sd))


# Multi-species
ms_run[[1]] <- Rceattle::fit_mod(data_list = mydata,
                                 inits = ss_run[[1]]$estimated_params, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 debug = 0, # Estimate
                                 random_rec = FALSE, # No random recruitment
                                 msmMode = 1, # Single species mode
                                 silent = TRUE)

run_mat[1,2] <- sum(!is.na(ms_run[[1]]$sdrep$sd))




# Fit removing survey
for(i in 1:6){
  mydata2 <- mydata
  mydata2$srv_control$Fit_0no_1yes[i] <- 0
  
  
  inits <- build_params(mydata2)
  inits$ln_mn_rec <- c(4,4,4)
  
  
  ss_run[[i + 1]] <- Rceattle::fit_mod(data_list = mydata2,
                                       inits = inits, # Initial parameters = 0
                                       file = NULL, # Don't save
                                       debug = 0, # Estimate
                                       random_rec = FALSE, # No random recruitment
                                       msmMode = 0, # Single species mode
                                       silent = TRUE)
  run_mat[i+1,1] <- sum(!is.na(ss_run[[i+1]]$sdrep$sd))
  
  
  ms_run[[i + 1]] <- Rceattle::fit_mod(data_list = mydata2,
                                       inits = ss_run[[i+1]]$estimated_params, # Initial parameters = 0
                                       file = NULL, # Don't save
                                       debug = 0, # Estimate
                                       random_rec = FALSE, # No random recruitment
                                       msmMode = 1, # Single species mode
                                       silent = TRUE,
                                       niter= 10)
  run_mat[i+1,2] <- sum(!is.na(ms_run[[i+1]]$sdrep$sd))
  
}


# Sub out fisheries
fsh <- 2:4
for(i in 1:length(fsh)){
  mydata2 <- mydata
  mydata2$fsh_control$Fit_0no_1yes[fsh[i]] <- 0
  
  
  inits <- build_params(mydata2)
  inits$ln_mn_rec <- c(4,4,4)
  
  
  ss_run[[i+7]] <- Rceattle::fit_mod(data_list = mydata2,
                                     inits = inits, # Initial parameters = 0
                                     file = NULL, # Don't save
                                     debug = 0, # Estimate
                                     random_rec = FALSE, # No random recruitment
                                     msmMode = 0, # Single species mode
                                     silent = TRUE)
  
  run_mat[i+7,1] <- sum(!is.na( ss_run[[i+7]]$sdrep$sd))
  
  ms_run[[i+7]] <- Rceattle::fit_mod(data_list = mydata2,
                                     inits = ss_run[[i+7]]$estimated_params, # Initial parameters = 0
                                     file = NULL, # Don't save
                                     debug = 0, # Estimate
                                     random_rec = FALSE, # No random recruitment
                                     msmMode = 1, # Single species mode
                                     silent = TRUE,
                                     niter= 10)
  
  run_mat[i+7,2] <- sum(!is.na(ms_run[[i+7]]$sdrep$sd))
}
