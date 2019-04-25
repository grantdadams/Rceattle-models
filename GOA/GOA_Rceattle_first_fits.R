
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
inits$ln_mean_F <- c(-0.8, -0.8, -0.8, -1.5, -2)

# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
ss_run <- Rceattle::fit_mod(data_list = mydata,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 1, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE)
ss_run$quantities$jnll_comp


ss_run <- Rceattle::fit_mod(data_list = mydata,
                            inits = ss_run$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE)
ss_run$estimated_params$ln_mean_F
 ss_run$quantities$jnll_comp

library(TMBhelper)
identified <- suppressMessages(TMBhelper::Check_Identifiable(ss_run$obj))

# Make into list
identified_param_list <- ss_run$obj$env$parList(as.numeric(identified$BadParams$Param_check))
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==0,"Not estimated",x), how = "replace")
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==2,"OK",x), how = "replace")
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==1,"BAD",x), how = "replace")

identified_param_list

# The you can plot the model results using using
plot_biomass(Rceattle =  ss_run)
plot_recruitment(Rceattle =  ss_run)
plot_selectivity(ss_run)
plot_catch(ss_run)
plot_index(ss_run)

plot_srv_comp(ss_run)
plot_fsh_comp(ss_run)
