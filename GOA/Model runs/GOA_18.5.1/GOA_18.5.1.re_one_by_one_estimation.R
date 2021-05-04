library(Rceattle)
setwd("Model runs/GOA_18.5.1")


################################################
# Set-up specifications
################################################
inits_M1_df <- data.frame(
  Model = 1:30,
  Time = c(rep("Long", 8), rep("Medium", 9), rep("Short", 13)),
  MsmMode = c(0, rep(1,7), # Long
              0, rep(1,2), rep(1,2), rep(1,2), rep(1,2), # Medium
              0, rep(1,3), rep(1,3), rep(1,3), rep(1,3)), # Short 
  EstM1 = c(0, rep(1,7), rep(0, 7), rep(1, 2), rep(0, 10), rep(1, 3))
) 
inits_M1_df$Class = ifelse(inits_M1_df$MsmMode == 0, "Single-species", "Multi-species")


################################################
# Estimate 
################################################
for(i in 1:8){
  if(inits_M1_df$MsmMode[i] == 0 | inits_M1_df$EstM1[i] == 1){
    
    
    load("Models/18_5_1_2021-04-23.RData")
    mod_fe = mod_list_all[[i]]
    rm(mod_list_all)
    
    # Update size of data by making smaller projection
    data_tmp <-  mod_fe$data_list
    data_tmp$projyr <- 2020
    nyrs <- data_tmp$projyr-data_tmp$styr +1
    
    # Update rec devs
    inits_tmp <- within(mod_fe$estimated_params, rm(logH_1, logH_1a, logH_1b, logH_2, logH_3, H_4, ln_srv_q_dev_re, ln_sel_slp_dev_re, sel_inf_dev_re))
    inits_tmp$rec_dev <- inits_tmp$rec_dev[,1:nyrs]
    
    # Fit model
    mod_re <- try( Rceattle::fit_mod(
      data_list = data_tmp,
      inits = inits_tmp, # Start from ms mod
      file = NULL, # Don't save
      debug = FALSE, # Estimate
      random_rec = TRUE, # Random recruitment
      msmMode = data_tmp$msmMode,
      silent = TRUE, phase = NULL, getHessian = TRUE,
      niter = 3),
      silent = FALSE)
    

    
    
    if(!is.null(mod_re)){
      if(class(mod_re) != "try-error"){
        save(mod_re, file = paste0("Models/Random_effects_models/18_5_1_re_Mod",i,"_",Sys.Date(),".Rdata"))
      }
    }
    gc()
    rm(mod_re)
  }
}

# Check which ones didnt converge
# inits_M1_df$Converged = sapply(mod_list_re, function(x) class(x) != "try-error" & !is.null(x))


