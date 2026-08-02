# Code to fit approximation of 2024 GOA Pollock model in Rceattle

# Load libraries ----
library(Rceattle)
library(readxl)
library(dplyr)

# Read in data ----
# - The data can be modified in excel or R
# Anchor to the model folder so the relative paths resolve.
setwd("~/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA pollock")

mydata_pollock <- Rceattle::read_data( file = "Data/GOA_24_pollock_single_species_1970-2024.xlsx")


# Fit base model ----
# - fixed M, multinomial, no stock-recruit curve
pollock_base <- fit_mod(data_list = mydata_pollock,
                        inits = NULL,       # Initial parameters = 0
                        file = NULL,        # Don't save
                        estimateMode = 0,   # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0,        # Single species mode
                        verbose = 1,        # Minimal messages
                        initMode = 1,       # Unfished equilibrium with init_dev's turned off
                        phase = TRUE)       # Phase

# Compare ----
load("Data/2024pollock.Rdata")
safe <- pollock_base
nyrs <- length(mydata_pollock$styr:mydata_pollock$endyr)
safe$quantities$biomass[,1:nyrs] <- fit$rep$Etotalbio * 1e6
safe$quantities$ssb[,1:nyrs] <- fit$rep$Espawnbio * 1e6

# - Plot
plot_biomass(list(safe, pollock_base), model_names = c("SAFE", "CEATTLE"))
plot_ssb(list(safe, pollock_base), model_names = c("SAFE", "CEATTLE"))

# Cole has priors on selectivity to get DM to work
