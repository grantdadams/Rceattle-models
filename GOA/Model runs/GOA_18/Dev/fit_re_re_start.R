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

mod_list_all <- list()
re_mods <- list.files("Models/Random_effects_models_3iter_w_hessian")
for(i in 1:length(re_mods)){
  load(paste0("Models/Random_effects_models_3iter_w_hessian/", re_mods[i]))
  mod_no <- as.numeric(gsub('_', '', substr(re_mods[i], 21,22)))
  if(mod_no %in% c(11,16)){
    mod_list_all[[mod_no]] <- mod_re
  }
}

################################################
# Estimate 
################################################
for(i in c(11,16)){
  
  # Load fixed effects models
  data_tmp <-  mod_list_all[[i]]$data_list
  inits_tmp <- mod_list_all[[i]]$estimated_params
  
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
