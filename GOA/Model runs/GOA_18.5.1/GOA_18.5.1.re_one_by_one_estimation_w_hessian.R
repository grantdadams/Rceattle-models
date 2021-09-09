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
re_mods <- list.files("Models/Random_effects_models_3iter")
mod_numbers <- sapply(strsplit(re_mods, "3iter_Mod_"), "[", 2)
mod_numbers <- as.numeric(sapply(strsplit(mod_numbers, "_2021"), "[", 1))

################################################
# Estimate 
################################################
for(i in 9:15){
  load(paste0("Models/Random_effects_models_3iter/", re_mods[which(mod_numbers == i)]))
  
  
  # Update size of data by making smaller projection
  data_tmp <-  mod_re$data_list
  
  # Update rec devs
  inits_tmp <- mod_re$obj$env$parList()
  rm(mod_re)
  
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
    getJointPrecision = TRUE,
    niter = 3,
    recompile = FALSE),
    silent = FALSE)
  
  
  if(!is.null(mod_re)){
    if(class(mod_re) != "try-error"){
      save(mod_re, file = paste0("Models/Random_effects_models_3iter_w_hessian/18_5_1_re_3iter_Mod_",i,"_",Sys.Date(),".Rdata"))
    }
  }
  gc()
  rm(mod_re)
  rm(inits_tmp)
}
