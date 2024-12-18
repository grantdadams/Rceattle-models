library(Rceattle)
data(BS2017SS)
data(BS2017MS)

###################################################
# Run in single species mode
ss_no_re <- Rceattle(data_list = BS2017SS,
                     inits = NULL, # Initial parameters = 0
                     file_name = NULL, # Don't save
                     debug = 0, # Estimate
                     random_rec = FALSE, # No random recruitment
                     msmMode = 0, # Single species mode
                     avgnMode = 0,
                     silent = TRUE)

ms_no_re <- Rceattle(data_list = BS2017SS,
                     inits = ss_no_re$initial_params, # Initial parameters = 0
                     file_name =NULL, # Don't save
                     debug = 0, # Estimate
                     random_rec = FALSE, # No random recruitment
                     msmMode = 0, # Single species mode
                     avgnMode = 0,
                     silent = TRUE)

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

length_params <- ms_no_re$estimated_params
length_params$log_gam_a <- log(sqrt(var_length_ratio))
length_params$log_gam_b <- log(mean_length_ratio)


weight_params <- ms_no_re$estimated_params
weight_params$log_gam_a <- log(sqrt(var_weight_ratio))
weight_params$log_gam_b <- log(mean_weight_ratio)

# Turn on phi
map <- ss_no_re$map
map$phi <- as.factor(1:length(map$phi))

stom_tau_vec <- c(20)
run_mat <- matrix(NA, ncol = 5, nrow = length(stom_tau_vec))

for(i in 1:length(stom_tau_vec)){
  ###################################################
  # Run with length log-normal
  ms_run7 <- Rceattle(data_list = BS2017MS,
                      inits = length_params, # Initial parameters = 0
                      file_name = NULL, # Don't save
                      debug = 0, # Estimate
                      random_rec = FALSE, # No random recruitment
                      msmMode = 1, # Single species mode
                      map = map,
                      suitMode = 4,
                      stom_tau = stom_tau_vec[i],
                      avgnMode = 0,
                      silent = TRUE)
  
  run_mat[i,3] <- sum(!is.na(ms_run7$sdrep$sd))
  
  ###################################################
  # Run with time-varying length log-normal
  ms_run8 <- Rceattle(data_list = BS2017MS,
                      inits = length_params, # Initial parameters = 0
                      file_name = NULL, # Don't save
                      debug = 0, # Estimate
                      random_rec = FALSE, # No random recruitment
                      msmMode = 1, # Single species mode
                      map = map,
                      suitMode = 5,
                      stom_tau = stom_tau_vec[i],
                      avgnMode = 0,
                      silent = TRUE)
  run_mat[i,4] <- sum(!is.na(ms_run8$sdrep$sd))
  
  ###################################################
  # Run with time-varying weight log-normal
  ms_run9 <- Rceattle(data_list = BS2017MS,
                      inits = weight_params, # Initial parameters = 0
                      file_name = NULL, # Don't save
                      debug = 0, # Estimate
                      random_rec = FALSE, # No random recruitment
                      msmMode = 1, # Single species mode
                      map = map,
                      suitMode = 6,
                      avgnMode = 0,
                      stom_tau = stom_tau_vec[i],
                      silent = TRUE)  
  run_mat[i,5] <- sum(!is.na(ms_run9$sdrep$sd))
}

write.csv(run_mat, file = "fixed_diet.csv")


