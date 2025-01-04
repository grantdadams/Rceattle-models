library(Rceattle)
library(dplyr)
library(readxl)

################################################
# Data
################################################
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_18.5.1_pollock_single_species_1970-2018.xlsx")

################################################
# Fit initial model
################################################
pk_base <- Rceattle::fit_mod(data_list = mydata_pollock,
                               inits = NULL, # Initial parameters = 0
                               file = NULL, # Don't save
                               estimateMode = 1,
                               msmMode = 0, # Single species mode
                               phase = "default")

################################################
# Bridging
################################################
# - Update catch
bridge1 <- Rceattle::read_data( file = "Data/Bridging/hake_bridging.xlsx")
bridge_mod1 <- Rceattle::fit_mod(data_list = bridge1,
                             inits = NULL, # Initial parameters = 0
                             file = NULL, # Don't save
                             estimateMode = 1,
                             msmMode = 0, # Single species mode
                             phase = "default")
plot_biomass(bridge_mod1)

# Update survey index (remove pollock surveys and add hake survey)
# - Remove comp data for other pollock surveys (using shelikof)
bridge2 <- Rceattle::read_data( file = "Data/Bridging/hake_bridging2.xlsx")
bridge_mod2 <- Rceattle::fit_mod(data_list = bridge2,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 estimateMode = 1,
                                 msmMode = 0, # Single species mode
                                 phase = "default")
plot_biomass(bridge_mod2)


# Update survey comp data
# - Move to non-parametric selectivity
bridge3 <- Rceattle::read_data( file = "Data/Bridging/hake_bridging3.xlsx")
bridge_mod3 <- Rceattle::fit_mod(data_list = bridge3,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 estimateMode = 1,
                                 msmMode = 0, # Single species mode
                                 phase = "default")
plot_biomass(bridge_mod3)
plot_comp(bridge_mod3)


# Update fishery comp data
# - Move to non-parametric selectivity
bridge4 <- Rceattle::read_data( file = "Data/Bridging/hake_bridging4.xlsx")
bridge_mod4 <- Rceattle::fit_mod(data_list = bridge4,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 estimateMode = 1,
                                 msmMode = 0, # Single species mode
                                 phase = "default")
plot_biomass(bridge_mod4)
plot_comp(bridge_mod4)


# Update weight data (maturity) and ageing error
bridge5 <- Rceattle::read_data( file = "Data/Bridging/hake_bridging5.xlsx")
bridge_mod5 <- Rceattle::fit_mod(data_list = bridge5,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 estimateMode = 1,
                                 msmMode = 0, # Single species mode
                                 phase = "default")
plot_biomass(bridge_mod5)
plot_comp(bridge_mod5)


# Update mortality to 0.21 for all ages
bridge5$M1_base[,3:ncol(bridge5$M1_base)] <- 0.21
bridge5$endyr <- 2019
bridge_mod6 <- Rceattle::fit_mod(data_list = bridge5,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 estimateMode = 1,
                                 msmMode = 0, # Single species mode
                                 phase = "default")
plot_biomass(bridge_mod6)
plot_comp(bridge_mod6)


# Update diet data, 20 ages, endyr to 2019
library(Rceattle)
bridge6 <- Rceattle::read_data( file = "Data/Bridging/hake_bridging6.xlsx")
bridge_mod6 <- Rceattle::fit_mod(data_list = bridge6,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 estimateMode = 1,
                                 initMode = 2,
                                 msmMode = 0, # Single species mode
                                 phase = "default")
plot_biomass(bridge_mod6); mtext(side = 3, "Biomass")
plot_ssb(bridge_mod6); mtext(side = 3, "SSB")
# plot_comp(bridge_mod6)



# Est M1
bridge_mod6 <- Rceattle::fit_mod(data_list = bridge6,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 estimateMode = 1,
                                 M1Fun = Rceattle::build_M1(M1_model = 1,
                                                            M1_use_prior = TRUE,
                                                            M1_prior_mean = 0.2,
                                                            M1_prior_sd = .1),
                                 recFun = build_srr(srr_fun = 1,
                                                    proj_mean_rec = FALSE,
                                                    srr_est_mode = 1,
                                                    srr_prior_mean = 0.777,
                                                    srr_prior_sd = 0.113),
                                 msmMode = 0, # Single species mode
                                 phase = "default")
plot_biomass(bridge_mod6); mtext(side = 3, "Biomass")
plot_ssb(bridge_mod6); mtext(side = 3, "SSB")
# plot_comp(bridge_mod6)

# Compare with SS
library(readxl)
ss_dat <- read_xlsx("Data/Stock_synthesis_output.xlsx")
ss_mod <- bridge_mod6
ss_mod$quantities$biomassSSB[1:length(ss_dat$SSB)] <- ss_dat$SSB/2

plot_ssb(list(ss_mod, bridge_mod6), model_names = c("SS", "CEATTLE"))




# Model 7 ----
# Update data with 2022 assessment data
library(Rceattle)
bridge7 <- Rceattle::read_data( file = "Data/Bridging/hake_bridging6_2022.xlsx")
bridge7$endyr <- 2021
bridge_mod7 <- Rceattle::fit_mod(data_list = bridge7,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 estimateMode = 1,
                                 initMode = 2,
                                 msmMode = 0, # Single species mode
                                 phase = "default")
plot_biomass(bridge_mod7); mtext(side = 3, "Biomass")
plot_ssb(bridge_mod7); mtext(side = 3, "SSB")
# plot_comp(bridge_mod7)



# Est M1
bridge_mod7 <- Rceattle::fit_mod(data_list = bridge7,
                                 inits = NULL, # Initial parameters = 0
                                 file = NULL, # Don't save
                                 estimateMode = 1,
                                 M1Fun = Rceattle::build_M1(M1_model = 1,
                                                            M1_use_prior = TRUE,
                                                            M1_prior_mean = 0.2,
                                                            M1_prior_sd = .1),
                                 recFun = build_srr(srr_fun = 1, # Beverton holt
                                                    proj_mean_rec = FALSE,
                                                    srr_est_mode = 2, # Use prior on steepness
                                                    srr_prior_mean = 0.777,
                                                    srr_prior_sd = 0.113),
                                 msmMode = 0, # Single species mode
                                 phase = "default")
plot_biomass(bridge_mod7); mtext(side = 3, "Biomass")
plot_ssb(bridge_mod7); mtext(side = 3, "SSB")
# plot_comp(bridge_mod7)

# Compare with SS
library(readxl)
ss_dat <- read_xlsx("Data/Stock_synthesis_output.xlsx", sheet = 2)
ss_mod <- bridge_mod7
ss_mod$quantities$biomassSSB[1:length(ss_dat$SSB)] <- ss_dat$SSB

plot_ssb(list(ss_mod, bridge_mod7), model_names = c("SS", "CEATTLE"))
