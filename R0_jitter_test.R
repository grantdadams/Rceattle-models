# Install Rceattle
devtools::install_github("grantdadams/Rceattle", auth_token = "4925b42ac46f1e0aefd671e9dc0c1cf1b3157017")

library(Rceattle)
data(BS2017SS) 

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


###################################################
# Set up initial values for R0
R0_vec <- runif(20, 5 , 15)
run_mat <- matrix(NA, ncol = 2, nrow = length(R0_vec))
run_mat[,1] <- R0_vec

###################################################
# Run in multi species mode using different starting values of R0
mod_list <- list()
ss_no_re2 <- ss_no_re
data(BS2017MS)
for(i in 1:length(R0_vec)){
  ss_no_re2$initial_params$ln_mn_rec <- rep(R0_vec[i], 3)
  mod_list[[i]] <- Rceattle(data_list = BS2017SS,
                            inits = ss_no_re2$initial_params, # Initial parameters = 0
                            file_name = NULL, # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            avgnMode = 0,
                            silent = TRUE)
  run_mat[i,2] <- sum(!is.na(mod_list[[i]]$sdrep$sd)) # Hessian inverted?
}

# Not converged if 0
run_mat 


# Plot recruitment
plot_recruitment(Rceattle = mod_list, 
                 file_name = NULL, model_names = NULL,
                 line_col = 1:length(mod_list),
                 species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", 
                 rep(NA, 3)), 
                 lwd = 3)


###################################################
# Test B_other = 0
ms_no_re <- Rceattle(data_list = BS2017MS,
                     inits = ss_no_re$estimated_params, # Initial parameters = 0
                     file_name = NULL, # Don't save
                     debug = 0, # Estimate
                     random_rec = FALSE, # No random recruitment
                     msmMode = 1, # Single species mode
                     avgnMode = 0,
                     silent = TRUE)

# Set other food = 0
BS2017MS2 <- BS2017MS
BS2017MS2$other_food <- rep(1, 3)
ms_no_re_test <- Rceattle(data_list = BS2017MS2,
                          inits = ss_no_re$estimated_params, # Initial parameters = 0
                          file_name = NULL, # Don't save
                          debug = 0, # Estimate
                          random_rec = FALSE, # No random recruitment
                          msmMode = 1, # Single species mode
                          avgnMode = 0,
                          silent = TRUE)

# Plot
plot_biomass(Rceattle = list(ms_no_re, ms_no_re_test), 
             file_name = NULL, model_names = c("MS", "TEST"),
             line_col = c(1,2),
             species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 3)), lwd = 3)

plot_recruitment(Rceattle = list(ms_no_re, ms_no_re_test), 
                 file_name = NULL, model_names = c("MS", "TEST"),
                 line_col = c(1,2),
                 species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 3)), lwd = 3)
