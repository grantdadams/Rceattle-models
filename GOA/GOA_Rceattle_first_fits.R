
library(Rceattle)

################################################
# Data
################################################
# Read the data in
mydata <- Rceattle::read_excel( file = "C:/Users/Grant Adams/Documents/GitHub/RceattleRuns/GOA/GOA2017SS_v3_from_1977_v2.xlsx")


################################################
# Estimation
################################################
# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
ss_run <- Rceattle::fit_mod(data_list = mydata,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 1, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE)
ss_run$quantities$jnll_comp

inits <- build_params(mydata)
inits$ln_mn_rec <- c(7,7,7)

ss_run <- Rceattle::fit_mod(data_list = mydata,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = FALSE)
ss_run$quantities$jnll_comp

library(TMBhelper)
identified <- suppressMessages(TMBhelper::Check_Identifiable(ss_run$obj))

# Make into list
identified_param_list <- ss_run$obj$env$parList(as.numeric(identified$BadParams$Param_check))
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==0,"Not estimated",x), how = "replace")
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==1,"OK",x), how = "replace")
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==2,"BAD",x), how = "replace")

identified_param_list

# The you can plot the model results using using
plot_biomass(Rceattle =  ss_run)
plot_recruitment(Rceattle =  ss_run)
plot_selectivity(ss_run)
plot_catch(ss_run)
plot_index(ss_run)
