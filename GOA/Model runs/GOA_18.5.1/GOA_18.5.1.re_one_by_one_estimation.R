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
for(i in 3){
  if(inits_M1_df$MsmMode[i] == 0 | inits_M1_df$EstM1[i] == 1){
    
    
    load("Models/18_5_1_2021-05-07.RData")
    mod_fe = mod_list_all[[i]]
    rm(mod_list_all)
    
    # Update size of data by making smaller projection
    data_tmp <-  mod_fe$data_list
    
    # Update rec devs
    inits_tmp <- mod_fe$estimated_params
    inits_tmp$srv_q_pow <- NULL
    
    # Fit model
    mod_re <- try( fit_mod(
      cpp_dir = "~/GitHub/Rceattle/inst/executables",
      data_list = data_tmp,
      inits = inits_tmp, # Start from ms mod
      file = NULL, # Don't save
      debug = 0, # Estimate
      random_rec = TRUE, # Random recruitment
      msmMode = data_tmp$msmMode,
      silent = TRUE, 
      phase = NULL, 
      getHessian = TRUE,
      niter = 3,
      recompile = FALSE),
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
round(mod_fe$quantities$jnll_comp, 3)[,1:10]
round(mod_re$quantities$jnll_comp, 3)[,1:10]

check <- c()
for(i in 1:length(inits_tmp)){
check[i] = sum(mod_fe$estimated_params[[i]] != mod_re$estimated_params[[i]], na.rm = TRUE)
}


sum(mod_fe$obj$par != mod_re$obj$par, na.rm = TRUE)
