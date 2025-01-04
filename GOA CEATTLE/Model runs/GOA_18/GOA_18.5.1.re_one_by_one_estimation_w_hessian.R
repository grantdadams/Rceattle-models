library(Rceattle)
rm(list=ls())
setwd("Model runs/GOA_18.5.1")


################################################
# Set-up specifications
################################################
# Set up inits vectors
inits_M1_df <- data.frame(
  Model = 1:16,
  MsmMode = c(0, rep(1,7), # Long
              0, rep(1,2), # Medium
              0, rep(1,4)), # Short 
  EstM1 = c(0, rep(1, 7), # Long
            0, rep(1, 2), # Medium
            0, rep(1, 4)), # Short
  InitModel = c(NA, rep(1,7), # Long
                NA, rep(9,2), # Medium
                NA, rep(12,4)) # Short
) 
inits_M1_df$Class = ifelse(inits_M1_df$MsmMode == 0, "Single-species", "Multi-species")

################################################
# Estimate 
################################################
for(i in 1:16){
  
  # Load fixed effects models
  load("Models/18_5_1_Niter3_2022-01-05.RData")
  mod_fe = mod_list_all_save[[i]]
  data_tmp <-  mod_fe$data_list
  inits_tmp <- mod_fe$estimated_params
  
  # Remove
  rm(mod_fe)
  rm(mod_list_all_save)
  
  # Fit model - 3 iterations
  mod_re <- try( fit_mod(
    data_list = data_tmp,
    inits = inits_tmp, # Start from ms mod
    file = NULL, # Don't save
    debug = 0, # Estimate
    random_rec = TRUE, # Random recruitment
    msmMode = data_tmp$msmMode,
    verbose = 1, 
    phase = NULL, 
    getJointPrecision = TRUE,
    niter = 3),
    silent = FALSE)
  
  
  if(!is.null(mod_re)){
    if(class(mod_re) != "try-error"){
      save(mod_re, file = paste0("Models/Random_effects_models_3iter_w_hessian/18_5_1_re_3iter_Mod_",i,"_",Sys.Date(),".Rdata"))
    }
  }
  gc()
  rm(mod_re)
  rm(inits_tmp)
  rm(data_tmp)
}
