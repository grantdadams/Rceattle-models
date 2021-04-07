library(Rceattle)
setwd("Model runs/GOA_18.5.1")
load("Models/18_5_1_2021-04-02.RData")

################################################
# Model 3 - Profile M1 on multi-species
################################################
# Grid search M1
n_m1 = 10
m1_vec <- seq(0.01, 0.41, length.out = n_m1)
jnll_grid <- array(0, dim = c(n_m1, n_m1, n_m1) )

# Use model 3

# Loop through species profiles
for(sp1 in 1:n_m1){
  for(sp2 in 1:n_m1){
    for(sp3 in 1:n_m1){
      
      # Update M1 so it is smaller
      mydata_list_m1 <- mod_list_all[[3]]$data_list
      
      mydata_list_m1$M1_base[1, 3:ncol(mydata_list_m1$M1_base)] <- m1_vec[sp1]
      mydata_list_m1$M1_base[2:3,3:ncol(mydata_list_m1$M1_base)]<- m1_vec[sp2]
      mydata_list_m1$M1_base[4, 3:ncol(mydata_list_m1$M1_base)] <- m1_vec[sp3]
      
      # Fit model
      mod_tmp <- try( Rceattle::fit_mod(
        data_list = mydata_list_m1,
        inits = mod_list_all[[3]]$estimated_params, # Start from ms mod
        file = NULL, # Don't save
        debug = 0, # Estimate
        random_rec = FALSE, # No random recruitment
        msmMode = 1, # Multi species mode
        silent = TRUE, phase = NULL,
        niter = 3),
        silent = FALSE)
      
      # # Try and phase if not estimating
      # if( class(mod_tmp) == "try-error"){
      #   mod_tmp <- try( Rceattle::fit_mod(
      #     data_list = mydata_list_m1,
      #     inits = mod_list_all[[3]]$estimated_params, # Initial parameters = 0
      #     file = NULL, # Don't save
      #     debug = 0, # Estimate
      #     random_rec = FALSE, # No random recruitment
      #     msmMode = 1, # Multi species mode
      #     silent = TRUE, phase = "default",
      #     niter = 3),
      #     silent = TRUE)
      # }
      
      
      # Save 
      if( class(mod_tmp) != "try-error"){
        jnll_grid[sp1, sp2, sp3] <- mod_tmp$quantities$jnll
      }
    }
  }
}

setwd("~/GitHub/RceattleRuns/GOA/Model runs/GOA_18.5.1")
save(jnll_grid, file = paste0("Models/18_5_1_ms_m1_gridsearch_",Sys.Date(),".Rdata"))
