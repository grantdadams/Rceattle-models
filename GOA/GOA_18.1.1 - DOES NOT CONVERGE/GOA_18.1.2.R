library(Rceattle)
# Make sure you switch to the fit switch version



################################################
# Data
################################################
# Read the data in
mydata <- Rceattle::read_excel( file = "GOA_18.1.0/GOA2017SS_v3_from_1977_v2_fit_switch.xlsx")
mydata$fsh_control$Nselages[16]

################################################
# Estimation
################################################
srv_fit <- list()
ss_run <- list()
ms_run <- list()

run_mat <- matrix(NA, ncol = 2, nrow = 9)

# Fit removing survey
for(i in 1:6){
  mydata2 <- mydata
  mydata2$srv_control$Fit_0no_1yes[i] <- 0

  
  inits <- build_params(mydata2)
  inits$ln_mn_rec <- c(4,4,4)
  
  
  ss_run[[i]] <- Rceattle::fit_mod(data_list = mydata2,
                              inits = inits, # Initial parameters = 0
                              file = NULL, # Don't save
                              debug = 0, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              silent = TRUE)
  run_mat[i,1] <- sum(!is.na(ss_run[[i]]$sdrep$sd))
  
  
  ms_run[[i]] <- Rceattle::fit_mod(data_list = mydata2,
                              inits = ss_run[[i]]$estimated_params, # Initial parameters = 0
                              file = NULL, # Don't save
                              debug = 0, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 1, # Single species mode
                              silent = TRUE,
                              niter= 10)
  run_mat[i,2] <- sum(!is.na(ms_run[[i]]$sdrep$sd))

}


# Sub out fisheries
fsh <- 2:4
for(i in 1:length(fsh)){
  mydata2 <- mydata
  mydata2$fsh_control$Fit_0no_1yes[fsh[i]] <- 0
  
  
  inits <- build_params(mydata2)
  inits$ln_mn_rec <- c(4,4,4)
  
  
  ss_run[[i+6]] <- Rceattle::fit_mod(data_list = mydata2,
                                   inits = inits, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   debug = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 0, # Single species mode
                                   silent = TRUE)
  
  run_mat[i+6,1] <- sum(!is.na( ss_run[[i+6]]$sdrep$sd))
  
  ms_run[[i+6]] <- Rceattle::fit_mod(data_list = mydata2,
                                   inits = ss_run[[i+6]]$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   debug = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 1, # Single species mode
                                   silent = TRUE,
                                   niter= 10)
  
  run_mat[i+6,2] <- sum(!is.na(ms_run[[i+6]]$sdrep$sd))
}
