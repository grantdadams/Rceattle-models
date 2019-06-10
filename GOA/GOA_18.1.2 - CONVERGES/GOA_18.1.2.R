library(Rceattle)

################################################
# Data
################################################
# Read the data in
mydata <- Rceattle::read_data( file = "GOA_18.1.2 - CONVERGES/GOA2017SS_v3_from_1977_v2_fit_switch.xlsx")
mydata$fsh_control$Nselages[15]

################################################
# Estimation
################################################
inits <- build_params(mydata)
inits$ln_mn_rec <- c(6,6,6)


ss_run <- Rceattle::fit_mod(data_list = mydata,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = FALSE)

ss_run$estimated_params$srv_sel_inf
ss_run$estimated_params$srv_sel_slp



plot_ssb(ss_run, file = NULL, add_ci = TRUE)
 

plot_recruitment(ss_run, file = file_name, add_ci = TRUE)
plot_selectivity(ss_run)
write_results(ss_run, file = paste0(file_name, ".xlsx"))




library(TMBhelper)
identified <- suppressMessages(TMBhelper::Check_Identifiable(ss_run$obj))

# Make into list
identified_param_list <- ss_run$obj$env$parList(as.numeric(identified$BadParams$Param_check))
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==0,"Not estimated",x), how = "replace")
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==2,"OK",x), how = "replace")
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==1,"BAD",x), how = "replace")

identified_param_list

ss_run$quantities$srv_q
