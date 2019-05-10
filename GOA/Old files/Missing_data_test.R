
library(Rceattle)

################################################
# Data
################################################
# Read the data in
data("BS2017SS")

write_excel(BS2017SS, file = "BS2017SS.xlsx")

mydata <- Rceattle::read_excel( file = "BS2017SS.xlsx")


################################################
# Estimation
################################################
ss_run <- Rceattle::fit_mod(data_list = mydata,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE)

ss_run2 <- Rceattle::fit_mod(data_list = BS2017SS,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            silent = TRUE)


mod_list <- list(ss_run, ss_run2)
plot_biomass(Rceattle = mod_list)
plot_srv_comp(Rceattle = ss_run2)
