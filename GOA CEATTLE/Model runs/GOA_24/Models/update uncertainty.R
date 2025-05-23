library(Rceattle)
library(readxl)
library(dplyr)
#setwd("Model runs/GOA_23.1.1/")
load("Models/GOA_23_1_1_mod_list.RData")
mydata <- Rceattle::read_data( file = "Data/GOA_23_1_1_data_1977_2023_edited.xlsx")
mod_list2 <- list()

# - Est single-species fixed M
mod_list2[[1]] <- Rceattle::fit_mod(data_list = mydata,
                                    inits = mod_list_all[[1]]$estimated_params, 
                                    file = NULL, # Don't save
                                    estimateMode = 0, # Estimate
                                    random_rec = FALSE, # No random recruitment
                                    msmMode = 0, # Single species mode
                                    verbose = 1,
                                    phase = NULL)



# - Est single-species estimate M
mod_list2[[2]] <- Rceattle::fit_mod(data_list = mydata,
                                    inits = mod_list_all[[2]]$estimated_params, 
                                    file = NULL, # Don't save
                                    estimateMode = 0, # Estimate
                                    random_rec = FALSE, # No random recruitment
                                    msmMode = 0, # Single species mode
                                    verbose = 1,
                                    phase = NULL,
                                    M1Fun = build_M1(M1_model = c(1,2,1),
                                                     M1_use_prior = FALSE,
                                                     M2_use_prior = FALSE))


# - Est multi-species
inits <- mod_list2[[2]]$estimated_params
data <- mydata

# - Run model
ms_mod <- try( 
  Rceattle::fit_mod(
    data_list = mydata,
    inits = inits, # Initial parameters = 0
    file = NULL, # Don't save
    estimateMode = 0, # Estimate
    M1Fun = build_M1(M1_model =  c(1,2,1),
                     M1_use_prior = FALSE,
                     M2_use_prior = FALSE),
    random_rec = FALSE, # No random recruitment
    msmMode = 1, # Multi species mode
    meanyr = 2018,
    verbose = 1,
    initMode = 1,
    phase = NULL),
  silent = TRUE)


# Phase in predation if doesnt converge
if( class(ms_mod) == "try-error" ){
  
  fday_vec <- seq(0.5,1, by = 0.1)
  
  for(j in 1:length(fday_vec)){
    my_data_tmp <- data
    my_data_tmp$fday <- replace(my_data_tmp$fday, values = rep(fday_vec[j], length(my_data_tmp$fday))) # Set foraging days to half
    
    if(j > 1){
      inits <- ms_mod$estimated_params
    }
    
    # Re-estimate
    ms_mod <- Rceattle::fit_mod(
      data_list = my_data_tmp,
      inits = inits, # Initial parameters = 0
      file = NULL, # Don't save
      estimateMode = 0, # Estimate
      M1Fun = build_M1(M1_model = c(1,2,1),
                       M1_use_prior = FALSE,
                       M2_use_prior = FALSE),
      random_rec = FALSE, # No random recruitment
      msmMode = 1, # Multi species mode
      meanyr = 2018,
      verbose = 1,
      initMode = 1,
      phase = NULL)
  }
}


# If Hessian cant invert or is discontinuous - PHASE
if( is.null(ms_mod$opt$objective)){
  ms_mod <- try( Rceattle::fit_mod(
    data_list = data,
    inits = inits, # Initial parameters = 0
    file = NULL, # Don't save
    estimateMode = 0, # Estimate
    M1Fun = build_M1(M1_model = c(1,2,1),
                     M1_use_prior = FALSE,
                     M2_use_prior = FALSE),
    random_rec = FALSE, # No random recruitment
    msmMode = 1, # Multi species mode
    meanyr = 2018,
    verbose = 1,
    phase = "default"),
    silent = TRUE)
}

# Discontinuous ll
if(!is.null(ms_mod$opt$objective)){
  if(abs(ms_mod$opt$objective -  ms_mod$quantities$jnll) > 1 ){
    ms_mod <- try( Rceattle::fit_mod(
      data_list = data,
      inits = ms_mod$estimated_params, # Initial parameters = 0
      file = NULL, # Don't save
      M1Fun = build_M1(M1_model = c(1,2,1),
                       M1_use_prior = FALSE,
                       M2_use_prior = FALSE),
      estimateMode = 0, # Estimate
      random_rec = FALSE, # No random recruitment
      msmMode = 1, # Multi species mode
      meanyr = 2018,
      verbose = 1,
      initMode = 1,
      phase = "default"),
      silent = TRUE)
  }
} 

mod_list2[[3]] <- ms_mod

# - Plot
plot_biomass(mod_list2)
plot_b_eaten(mod_list2)
plot_recruitment(mod_list2)

# - Save
mod_list_all <- mod_list2
save(mod_list_all, file = "Models/GOA_23_1_1_mod_list.RData")
