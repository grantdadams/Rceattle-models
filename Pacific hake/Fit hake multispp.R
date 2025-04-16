################################################
# Libraries
################################################
#devtools::install_github("grantdadams/Rceattle", ref = "dev")
#install.packages("Matrix")
#install.packages("TMB", type = "source")
library(Rceattle)
library(dplyr)
library(readxl)


################################################
# Data
################################################
old_hakedata <- Rceattle::read_data(file = "Data/hake_intrasp_250207.xlsx")
new_hakedata <- Rceattle::read_data(file = "Data/GRANT_ATF_hake_intrasp_250207.xlsx")

################################################
# Fit initial model
################################################
# * Single-species ----
# - Old data
hake_old <- Rceattle::fit_mod(data_list = old_hakedata,
                              inits = NULL, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 0,
                              verbose = 1,
                              msmMode = 0, # Single species mode
                              phase = TRUE,
                              initMode = 2) # Fished start with init devs




# - New data
hake_new <- Rceattle::fit_mod(data_list = new_hakedata,
                              inits = NULL, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 0,
                              verbose = 1,
                              msmMode = 0, # Single species mode
                              phase = TRUE,
                              initMode = 2) # Fished start with init devs

# Compare
plot(x = new_hakedata$styr:new_hakedata$projyr,
     y = hake_old$quantities$ssb[1,], ylab = "SSB", xlab = "Year", type = "l", lwd = 3)
lines(x = new_hakedata$styr:new_hakedata$projyr,
      y = hake_new$quantities$ssb[1,], lwd = 1, col = 2)
# Match!


# * Multi-species (cannibalism only) ----
# - Old data
hake_old_can <- Rceattle::fit_mod(data_list = old_hakedata,
                                  inits = hake_old$estimated_params, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0,
                                  verbose = 1,
                                  msmMode = 1, # Multi species mode
                                  phase = TRUE,
                                  initMode = 2) # Fished start with init devs




# - New data
new_hakedata$Pvalue = c(1, 0) # Turn off ATF predation
hake_new_can <- Rceattle::fit_mod(data_list = new_hakedata,
                                  inits = hake_new$estimated_params, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0,
                                  verbose = 1,
                                  msmMode = 1, # Multi species mode
                                  phase = TRUE,
                                  initMode = 2) # Fished start with init devs

# Compare
plot(x = new_hakedata$styr:new_hakedata$projyr,
     y = hake_old_can$quantities$ssb[1,], ylab = "SSB", xlab = "Year", type = "l", lwd = 3)
lines(x = new_hakedata$styr:new_hakedata$projyr,
      y = hake_new_can$quantities$ssb[1,], lwd = 1, col = 2)
# Match!

hake_old_can$quantities$jnll_comp
hake_new_can$quantities$jnll_comp



# * Multi-species ----
# - New data
new_hakedata$Pvalue = c(1, 1) # Turn on ATF predation
hake_new_ms <- Rceattle::fit_mod(data_list = new_hakedata,
                                 inits = hake_new$estimated_params, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 estimateMode = 0,
                                 verbose = 1,
                                 msmMode = 1, # Multi species mode
                                 phase = TRUE,
                                 initMode = 2) # Fished start with init devs

plot_biomass(list(hake_new_can, hake_new_ms), model_names = c("Cannibalism", "Multi-spp"))
plot_recruitment(list(hake_new_can, hake_new_ms), model_names = c("Cannibalism", "Multi-spp"))
plot_ssb(list(hake_new_can, hake_new_ms), model_names = c("Cannibalism", "Multi-spp"))
