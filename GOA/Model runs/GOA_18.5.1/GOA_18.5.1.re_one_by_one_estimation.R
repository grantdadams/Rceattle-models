library(Rceattle)
rm(list=ls())
setwd("Model runs/GOA_18.5.1")


################################################
# Set-up specifications
################################################
# Set up inits vectors
inits_M1_df <- data.frame(
  Model = 1:15,
  MsmMode = c(0, rep(1,7), # Long
              0, rep(1,2), # Medium
              0, rep(1,3)), # Short 
  EstM1 = c(0, rep(1,7), # Long
            0, rep(1, 2), # Medium
            0, rep(1, 3)), # Short
  InitModel = c(NA, rep(1,7), # Long
                NA, rep(9,2), # Medium
                NA, rep(12,3)) # Short
) 
inits_M1_df$Class = ifelse(inits_M1_df$MsmMode == 0, "Single-species", "Multi-species")



################################################
# Estimate 
################################################
for(i in 15){
  load("Models/18_5_1_Niter3_2021-06-14.RData")
  mod_fe = mod_list_all[[i]]

  
  # Update size of data by making smaller projection
  data_tmp <-  mod_fe$data_list
  if(i == 11){
    data_tmp$estDynamics <- c(0,0,0,2)
  }
  
  # Update rec devs
  inits_tmp <- mod_list_all[[14]]$estimated_params
  rm(mod_fe)
  rm(mod_list_all)
  
  # Fit model - 3 iterations
  mod_re <- try( fit_mod(
    data_list = data_tmp,
    inits = inits_tmp, # Start from ms mod
    file = NULL, # Don't save
    debug = 0, # Estimate
    random_rec = TRUE, # Random recruitment
    msmMode = data_tmp$msmMode,
    silent = TRUE, 
    phase = NULL, 
    getHessian = FALSE,
    niter = 3,
    recompile = FALSE),
    silent = FALSE)
  
  
  # 5 - iter inites
  load("Models/18_5_1_Niter5_2021-06-13.RData")
  mod_fe = mod_list_all[[i]]
  rm(mod_list_all)
  
  # Update rec devs
  inits_tmp <- mod_fe$estimated_params
  rm(mod_fe)
  
  
  if(!is.null(mod_re)){
    if(class(mod_re) != "try-error"){
      save(mod_re, file = paste0("Models/Random_effects_models_3iter/18_5_1_re_3iter_Mod_",i,"_",Sys.Date(),".Rdata"))
      inits_tmp$ln_rec_sigma <- mod_re$estimated_params$ln_rec_sigma
    }
  }
  gc()
  rm(mod_re)
  
  
  if(inits_M1_df$EstM1[i] == 1){
    # Fit model - 5 iterations
    mod_re <- try( fit_mod(
      data_list = data_tmp,
      inits = inits_tmp, # Start from ms mod
      file = NULL, # Don't save
      debug = 0, # Estimate
      random_rec = TRUE, # Random recruitment
      msmMode = data_tmp$msmMode,
      silent = TRUE, 
      phase = NULL, 
      getHessian = FALSE,
      niter = 5,
      recompile = FALSE),
      silent = FALSE)
    
    if(!is.null(mod_re)){
      if(class(mod_re) != "try-error"){
        save(mod_re, file = paste0("Models/Random_effects_models_5iter/18_5_1_re_5iter_Mod_",i,"_",Sys.Date(),".Rdata"))
      }
    }
    gc()
    rm(mod_re)
  }
}
# 
# # Check which ones didnt converge
# # inits_M1_df$Converged = sapply(mod_list_re, function(x) class(x) != "try-error" & !is.null(x))
# round(mod_fe$quantities$jnll_comp, 3)[,1:10]
# round(mod_re$quantities$jnll_comp, 3)[,1:10]
# 
# check <- c()
# for(i in 1:length(inits_tmp)){
# check[i] = sum(mod_fe$estimated_params[[i]] != mod_re$estimated_params[[i]], na.rm = TRUE)
# }
# 
# 
# sum(mod_fe$obj$par != mod_re$obj$par, na.rm = TRUE)
