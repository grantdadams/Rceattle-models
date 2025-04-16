################################################
# Libraries
################################################
# devtools::install_github("grantdadams/Rceattle", ref = "dev")
library(Rceattle)
library(dplyr)
library(readxl)


################################################
# Data
################################################
old_hakedata <- Rceattle::read_data(file = "Data/hake_intrasp_250207.xlsx")

new_hakedata <- Rceattle::read_data(file = "Data/NEW_ATF_hake_intrasp_250207.xlsx")

################################################
# Fit initial model
################################################
hake_old <- Rceattle::fit_mod(data_list = old_hakedata,
                                inits = NULL, # Initial parameters = 0
                                file = NULL, # Don't save
                                estimateMode = 0,
                                verbose = 1,
                                msmMode = 0, # Single species mode
                                phase = TRUE,
                                initMode = 3) # Fished start with init devs


hake_new <- Rceattle::fit_mod(data_list = new_hakedata,
                             inits = NULL, # Initial parameters = 0
                             file = NULL, # Don't save
                             estimateMode = 0,
                             verbose = 1,
                             msmMode = 0, # Single species mode
                             phase = TRUE,
                             initMode = 3) # Fished start with init devs

hake_old$quantities$R[1,1:20]
hake_new$quantities$R[1,1:20]
