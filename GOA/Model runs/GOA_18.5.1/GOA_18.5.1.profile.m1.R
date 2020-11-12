library(Rceattle)
setwd("Model runs/GOA_18.5.1")
load("Models/18_5_1_2020-11-08.RData")

################################################
# Model 3 - Profile M1 on multi-species
################################################
m1_vec <- seq(0.01, 0.31, length.out = 20)
ms_m1_profile_list <- list() # Level 1 is species group; 2 is Model;

profile_sp <- list();profile_sp[[1]] <- 1; profile_sp[[2]] <- 2; profile_sp[[3]] <- 3; profile_sp[[4]] <- c(1:3)

# Loop through species profiles
for(sp in 1:4){
  ms_m1_profile_list[[sp]] <- list()
  
  # Loop through models
  for(i in c(2,3,10,11,13)){
    ms_m1_profile_list[[sp]][[i]] <- list()
    
    
    # Loop through M1
    for(m1 in 1:length(m1_vec)){
      
      # Update M1 so it is smaller
      mydata_list_m1 <- mod_list_all[[i]]$data_list
      mydata_list_m1$M1_base[profile_sp[[sp]],3:ncol(mydata_list_m1$M1_base)] <- m1_vec[m1]
      
      
      # Fit model
      mod_tmp <- try( Rceattle::fit_mod(
        data_list = mydata_list_m1,
        inits = mod_list_all[[i]]$estimated_params, # Start from ms mod
        file = NULL, # Don't save
        debug = 0, # Estimate
        random_rec = FALSE, # No random recruitment
        msmMode = 1, # Multi species mode
        silent = TRUE, phase = NULL,
        niter = 5),
        silent = FALSE)
      
      # Try and phase if not estimating
      if( class(mod_tmp) == "try-error"){
        mod_tmp <- try( Rceattle::fit_mod(
          data_list = mydata_list_m1,
          inits = mod_list_all[[i]]$estimated_params, # Initial parameters = 0
          file = NULL, # Don't save
          debug = 0, # Estimate
          random_rec = FALSE, # No random recruitment
          msmMode = 1, # Multi species mode
          silent = TRUE, phase = "default",
          niter = 5),
          silent = TRUE)
      }
      
      
      # Save 
      if( class(mod_tmp) != "try-error"){
        ms_m1_profile_list[[sp]][[i]][[m1]] <- mod_tmp$quantities
      }
    }
  }
}

save(ms_m1_profile_list, file = paste0("Models/18.5.1.ms_m1_profiles_",Sys.Date(),".Rdata"))
