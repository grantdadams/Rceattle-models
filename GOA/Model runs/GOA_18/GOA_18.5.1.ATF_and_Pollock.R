library(Rceattle)
library(readxl)
setwd("Model runs/GOA_18.5.1/")

################################################
# Data
################################################

# Pollock
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_18.5.1_pollock_single_species_1970-2018.xlsx")
# Scale n-at-age to vals
mydata_pollock$NByageFixed[,5:15] <- mydata_pollock$NByageFixed[,5:15] * 1000000 # Martin outputs n-at-age as millions, I do not (These aren't used unless estDynamics = 1,2)
mydata_pollock$srv_biom$Observation <- mydata_pollock$srv_biom$Observation * 1000000 # Martin uses CPUE as million mt, I do not
mydata_pollock$estDynamics = 0 # Estimate the age-structured model

# Arrowtooth flounds
mydata_atf <- Rceattle::read_data( file = "Data/GOA_18.5.1_arrowtooth_single_species_1961-2018.xlsx")
mydata_atf$estDynamics = 0 # Estimate the age-structured model


################################################
# Fit single-species models
################################################
# - Pollock
pollock_base <- Rceattle::fit_mod(data_list = mydata_pollock,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  debug = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  silent = TRUE,
                                  recompile = FALSE,
                                  phase = "default")

# -- Plot
plot_biomass(pollock_base)
plot_recruitment(pollock_base)
plot_catch(pollock_base)
plot_selectivity(pollock_base)
plot_logindex(pollock_base)


# - Arrowtooth flounder
atf_base <- Rceattle::fit_mod(data_list = mydata_atf,
                              inits = NULL, # Initial parameters = 0
                              file = NULL, # Don't save
                              debug = 0, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              silent = TRUE,
                              recompile = FALSE,
                              phase = "default")
# -- Plot
plot_biomass(atf_base)
plot_recruitment(atf_base)
plot_catch(atf_base)
plot_selectivity(atf_base)
plot_logindex(atf_base)
