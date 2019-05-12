library(Rceattle)

################################################
# Data
################################################
# Read the data in
mydata <- Rceattle::read_excel( file = "GOA2017SS_v3_from_1977_v2.xlsx")


################################################
# Estimation
################################################
inits <- build_params(mydata)
inits$ln_mn_rec <- c(4,4,4)

ss_run <- Rceattle::fit_mod(data_list = mydata,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = FALSE,
                            recompile = TRUE)
# ss_run$estimated_params$ln_mean_F
#  ss_run$quantities$jnll_comp
