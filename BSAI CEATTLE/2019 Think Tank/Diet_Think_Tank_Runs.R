library(Rceattle)
data(BS2017SS) 

###################################################
# Run in single species mode
ss_no_re <- Rceattle(data_list = BS2017SS,
                   inits = NULL, # Initial parameters = 0
                   file_name = "BSAI/Runs/2019 Think Tank/Models/ss_no_re", # Don't save
                   debug = 0, # Estimate
                   random_rec = FALSE, # No random recruitment
                   msmMode = 0, # Single species mode
                   avgnMode = 0,
                   silent = TRUE)

###################################################
# Run in multi species mode
data(BS2017MS)
ms_no_re <- Rceattle(data_list = BS2017MS,
                   inits = ss_no_re$estimated_params, # Initial parameters = 0
                   file_name = "BSAI/Runs/2019 Think Tank/Models/ms_no_re", # Don't save
                   debug = 0, # Estimate
                   random_rec = FALSE, # No random recruitment
                   msmMode = 1, # Single species mode
                   avgnMode = 0,
                   silent = TRUE)


###################################################
# Set up diet
stom_tau_vec <- c(1,2,5,10,15,20,50,100,150,200)
i = 4
BS2017MS$stom_tau <- stom_tau_vec[i]

###################################################
# Run with length gamma
ms_run1 <- Rceattle(data_list = BS2017MS,
                     inits = ms_no_re$estimated_params, # Initial parameters = 0
                     file_name = "BSAI/Runs/2019 Think Tank/Models/ms_diet1", # Don't save
                     debug = 0, # Estimate
                     random_rec = FALSE, # No random recruitment
                     msmMode = 1, # Single species mode
                     suitMode = 1,
                     avgnMode = 0,
                     silent = TRUE)

###################################################
# Run with time-varying length gamma
ms_run2 <- Rceattle(data_list = BS2017MS,
                    inits = ss_no_re$estimated_params, # Initial parameters = 0
                    file_name = "BSAI/Runs/2019 Think Tank/Models/ms_diet2", # Don't save
                    debug = 0, # Estimate
                    random_rec = FALSE, # No random recruitment
                    msmMode = 1, # Single species mode
                    suitMode = 2,
                    avgnMode = 0,
                    silent = TRUE)

###################################################
# Run with time-varying weight gamma
ms_run3 <- Rceattle(data_list = BS2017MS,
                    inits = ss_no_re$estimated_params, # Initial parameters = 0
                    file_name = "BSAI/Runs/2019 Think Tank/Models/ms_diet3", # Don't save
                    debug = 0, # Estimate
                    random_rec = FALSE, # No random recruitment
                    msmMode = 1, # Single species mode
                    suitMode = 3,
                    avgnMode = 0,
                    silent = TRUE)

###################################################
# Run with length log-normal
ms_run4 <- Rceattle(data_list = BS2017MS,
                    inits = mod$estimated_params, # Initial parameters = 0
                    file_name = "BSAI/Runs/2019 Think Tank/Models/ms_diet4", # Don't save
                    debug = 0, # Estimate
                    random_rec = FALSE, # No random recruitment
                    msmMode = 1, # Single species mode
                    suitMode = 4,
                    avgnMode = 0,
                    silent = TRUE)

###################################################
# Run with time-varying length log-normal
ms_run5 <- Rceattle(data_list = BS2017MS,
                    inits = ss_no_re$estimated_params, # Initial parameters = 0
                    file_name = "BSAI/Runs/2019 Think Tank/Models/ms_diet5", # Don't save
                    debug = 0, # Estimate
                    random_rec = FALSE, # No random recruitment
                    msmMode = 1, # Single species mode
                    suitMode = 3,
                    avgnMode = 0,
                    silent = TRUE)

###################################################
# Run with time-varying weight log-normal
ms_run6 <- Rceattle(data_list = BS2017MS,
                    inits = ss_no_re$estimated_params, # Initial parameters = 0
                    file_name = "BSAI/Runs/2019 Think Tank/Models/ms_diet6", # Don't save
                    debug = 0, # Estimate
                    random_rec = FALSE, # No random recruitment
                    msmMode = 1, # Single species mode
                    suitMode = 6,
                    avgnMode = 0,
                    silent = TRUE)


###################################################
# Fix diet prop

alw <- BS2017MS$aLW
U_array <- BS2017MS$UobsAge

wt <- BS2017MS$wt


WT_Ratio <- U_array
LT_Ratio <- U_array
WT_Ratio <- replace(WT_Ratio, values =  rep(0, length(WT_Ratio)))
LT_Ratio <- replace(LT_Ratio, values =  rep(0, length(LT_Ratio)))
mean_weight_ratio <- c()
var_weight_ratio <- c()

mean_length_ratio <- c()
var_length_ratio <- c()

for(rsp in 1:3){
  for(ksp in 1:3){
    for(r_age in 1:BS2017MS$nages[rsp]){
      for(k_age in 1:BS2017MS$nages[ksp]){
        
        WT_Ratio[rsp, ksp, r_age, k_age] <- U_array[rsp, ksp, r_age, k_age] * log(mean(wt[,r_age, rsp], na.rm = TRUE) / mean(wt[,k_age, ksp], na.rm = TRUE))
        LT_Ratio[rsp, ksp, r_age, k_age] <- U_array[rsp, ksp, r_age, k_age] * log(mean(alw[1,rsp] * wt[,r_age, rsp] ^ alw[2,rsp], na.rm = TRUE) / mean(alw[1,ksp] * wt[,k_age, ksp] ^ alw[2,ksp], na.rm = TRUE) )
        
      }
    }
  }
  # Summarize
  mean_weight_ratio[rsp] <- mean(WT_Ratio[rsp, , , ][which(WT_Ratio[rsp, , , ] != 0)], na.rm = TRUE)
  var_weight_ratio[rsp] <- var(WT_Ratio[rsp, , , ][which(WT_Ratio[rsp, , , ] != 0)], na.rm = TRUE)
  
  mean_length_ratio[rsp] <- mean(LT_Ratio[rsp, , , ][which(LT_Ratio[rsp, , , ] != 0)], na.rm = TRUE)
  var_length_ratio[rsp] <- var(LT_Ratio[rsp, , , ][which(LT_Ratio[rsp, , , ] != 0)], na.rm = TRUE)
}

length_params <- ss_no_re$estimated_params
length_params$log_gam_a <- log(sqrt(var_length_ratio))
length_params$log_gam_b <- log(mean_length_ratio)


weight_params <- ss_no_re$estimated_params
weight_params$log_gam_a <- log(sqrt(var_weight_ratio))
weight_params$log_gam_b <- log(mean_weight_ratio)

# Turn on phi
map <- ss_no_re$map
map$phi <- as.factor(1:length(map$phi))


###################################################
# Run with length log-normal
ms_run7 <- Rceattle(data_list = BS2017MS,
                    inits = length_params, # Initial parameters = 0
                    file_name = "BSAI/Runs/2019 Think Tank/Models/ms_diet7", # Don't save
                    debug = 0, # Estimate
                    random_rec = FALSE, # No random recruitment
                    msmMode = 1, # Single species mode
                    map = map,
                    suitMode = 4,
                    avgnMode = 0,
                    silent = TRUE)

###################################################
# Run with time-varying length log-normal
ms_run8 <- Rceattle(data_list = BS2017MS,
                    inits = length_params, # Initial parameters = 0
                    file_name = "BSAI/Runs/2019 Think Tank/Models/ms_diet8", # Don't save
                    debug = 0, # Estimate
                    random_rec = FALSE, # No random recruitment
                    msmMode = 1, # Single species mode
                    map = map,
                    suitMode = 5,
                    avgnMode = 0,
                    silent = TRUE)

###################################################
# Run with time-varying weight log-normal
ms_run9 <- Rceattle(data_list = BS2017MS,
                    inits = weight_params, # Initial parameters = 0
                    file_name = "BSAI/Runs/2019 Think Tank/Models/ms_diet9", # Don't save
                    debug = 0, # Estimate
                    random_rec = FALSE, # No random recruitment
                    msmMode = 1, # Single species mode
                    map = map,
                    suitMode = 6,
                    avgnMode = 0,
                    silent = TRUE)


