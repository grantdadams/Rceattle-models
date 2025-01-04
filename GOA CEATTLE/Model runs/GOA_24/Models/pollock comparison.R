library(Rceattle)
library(readxl)
library(dplyr)
setwd("Model runs/GOA_23.1.1/")

################################################
# Pollock
################################################
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_23.1.1_pollock_single_species_1970-2023.xlsx")
mydata_pollock$msmMode = 0
mydata_pollock$srv_biom$Observation <- mydata_pollock$srv_biom$Observation * 1e6
mydata_pollock$estDynamics = 0


mydata_pollock$endyr <- 2023
# - Fit single-species models
pollock_base <- fit_mod(data_list = mydata_pollock,
                        inits = NULL, # Initial parameters = 0
                        file = NULL, # Don't save
                        estimateMode = 0, # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0, # Single species mode
                        verbose = 1,
                        initMode = 1,
                        phase = "default")

pk_tmb <- f2023fits[[2]]

pk_tmb <- pollock_base
pk_tmb$quantities$biomassSSB[,1:54] <- f2023fits[[2]]$rep$Espawnbio * 1e6


plot_ssb(list(pk_tmb, pollock_base), model_names = c("PK", "CE"))
