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


ss_run <- Rceattle::fit_mod(data_list = mydata,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE)
# ss_run$estimated_params$ln_mean_F
#  ss_run$quantities$jnll_comp

library(TMBhelper)
identified <- suppressMessages(TMBhelper::Check_Identifiable(ss_run$obj))

# Make into list
identified_param_list <- ss_run$obj$env$parList(as.numeric(identified$BadParams$Param_check))
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==0,"Not estimated",x), how = "replace")
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==2,"OK",x), how = "replace")
identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==1,"BAD",x), how = "replace")

identified_param_list

ss_run$quantities$srv_q



# Map out sel from first run
mydata$msmMode <- 0
inits <- ss_run$estimated_params
map <- build_map(params = inits, data_list = mydata)

# Map out selectivity of pcod pot
map[[1]]$fsh_sel_slp <- as.character(map[[1]]$fsh_sel_slp)
map[[1]]$fsh_sel_inf <- as.character(map[[1]]$fsh_sel_inf)

map[[1]]$fsh_sel_slp[7] <- (NA)
map[[1]]$fsh_sel_inf[7] <- (NA)

map[[1]]$fsh_sel_slp <- as.factor(map[[1]]$fsh_sel_slp)
map[[1]]$fsh_sel_inf <- as.factor(map[[1]]$fsh_sel_inf)

map[[2]]$fsh_sel_slp[7] <- (NA)
map[[2]]$fsh_sel_inf[7] <- (NA)

# Map out ATF sel coef
# map[[2]]$fsh_sel_coff <- replace(map[[2]]$fsh_sel_coff, values = rep(NA, length(map[[2]]$fsh_sel_coff)))
# map[[1]]$fsh_sel_coff <- as.factor(map[[2]]$fsh_sel_coff)

# # Map out F_dev of pcod pot
map[[2]]$F_dev[4:5,] <- NA
map[[1]]$F_dev <- as.factor(map[[2]]$F_dev)
# 
# # Map out ln_mn_F of pcod pot
# map[[2]]$ln_mean_F[4:5] <- NA
# map[[1]]$ln_mean_F <- as.factor(map[[2]]$ln_mean_F)

# Re-run
ss_run2 <- Rceattle::fit_mod(data_list = mydata,
                            inits = inits, # Initial parameters = 0
                            map = map,
                            file = NULL, # Don't save
                            debug = 1, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE)



ss_run$quantities$jnll_comp



#  ss_run$quantities$jnll_comp
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
