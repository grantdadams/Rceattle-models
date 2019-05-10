library(Rceattle)
# Make sure you switch to the fit switch version



################################################
# Data
################################################
# Read the data in
mydata <- Rceattle::read_excel( file = "GOA_18.1.0/GOA2017SS_v3_from_1977_v2_fit_switch.xlsx")


################################################
# Estimation
################################################
inits <- build_params(mydata)
inits$ln_mn_rec <- c(4,4,4)


ss_run <- Rceattle::fit_mod(data_list = mydata,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            random_rec = TRUE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = FALSE)



ms_run <- Rceattle::fit_mod(data_list = mydata,
                  inits = ss_run$estimated_params, # Initial parameters = 0
                  file = NULL, # Don't save
                  debug = 0, # Estimate
                  random_rec = TRUE, # No random recruitment
                  msmMode = 1, # Single species mode
                  silent = FALSE,
                  niter= 10)
# ss_run$estimated_params$ln_mean_F
ms_run$quantities$jnll_comp
ms_run$quantities$NByage[1,,]
ms_run$quantities$ration2Age[2,,] # NA on year 3 age 1
mydata$Pyrs[,,2]



library(TMBhelper)
identified <- suppressMessages(TMBhelper::Check_Identifiable(ms_run$obj))

# Make into list
identified_param_list <- ss_run$obj$env$parList(as.numeric(identified$BadParams$Param_check))
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==0,"Not estimated",x), how = "replace")
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==2,"OK",x), how = "replace")
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==1,"BAD",x), how = "replace")

identified_param_list

ss_run$quantities$srv_q
