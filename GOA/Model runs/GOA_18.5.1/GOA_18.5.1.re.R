library(Rceattle)
setwd("Model runs/GOA_18.5.1")
load("Models/18_5_1_2020-11-08.RData")

################################################
# Estimate sigmaR
################################################
mod_list_re <- list()

# Loop through species profiles
for(i in 1:length(mod_list_all)){
  
  # Fit model
  mod_list_re[[i]] <- try( Rceattle::fit_mod(
    data_list = mod_list_all[[i]]$data_list,
    inits = mod_list_all[[i]]$estimated_params, # Start from ms mod
    file = NULL, # Don't save
    debug = 0, # Estimate
    random_rec = TRUE, # Random recruitment
    msmMode = mod_list_all[[i]]$data_list$msmMode,
    silent = TRUE, phase = NULL,
    niter = 5),
    silent = FALSE)
}

# Check which ones didnt converge
check <- c()
for(i in 1:length(mod_list_re)){
  check[i] <- class()
}
sapply(mod_list_re, function(x) class(x) == "try-error")

save(mod_list_re, file = paste0("Models/18_5_1_re_",Sys.Date(),".Rdata"))
