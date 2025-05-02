# Code to run the bering alaska plaice assessment in CEATTLE
# model is a two sex, single-species model

# DATA
# - Fishery
# -- catch
# -- age composition
# -- length comps
# -- weight-at-age
# - Survey
# -- biomass and standard error
# -- age comps

# MODEL
# - Two sex
# - Survey selectivity = sex-specific logistic (normalized based on max-across sexes for survey)
# - Survey q = q
# - Fishery selectivity = sex-specific logistic
# - Empirical weight-at-age (sex-specific, time-invariant)
# - M = 0.13 for females, estimated for males

# Load data ----
library(Rceattle)
mydata <- Rceattle::read_data( file = "Data/plaice_single_species_2021.xlsx")
mydata$estDynamics = 0
mydata$nsex = 2
mydata$srv_biom$Log_sd <- mydata$srv_biom$Log_sd/mydata$srv_biom$Observation
mydata$fsh_biom$Catch <- mydata$fsh_biom$Catch/1000

# - Fix M
bridging_model_1 <- Rceattle::fit_mod(data_list = mydata,
                                          inits = NULL, # Initial parameters = 0
                                          file = NULL, # Don't save
                                          estimateMode = 0, # Estimate
                                          random_rec = FALSE, # No random recruitment
                                          msmMode = 0, # Single species mode
                                          verbose = 1,
                                          phase = TRUE,
                                          initMode = 1)

# # - Est female and male M
# bridging_model_2 <- Rceattle::fit_mod(data_list = mydata,
#                                       inits = bridging_model_1$estimated_params,
#                                       file = NULL, # Don't save
#                                       estimateMode = 0, # Estimate
#                                       random_rec = FALSE, # No random recruitment
#                                       msmMode = 0, # Single species mode
#                                       verbose = 1,
#                                       M1Fun = build_M1(M1_model = c(2)),
#                                       phase = NULL,
#                                       initMode = 2)



# - SAFE model
library(readxl)
SAFE2022_mod <- bridging_model_1
safe_data <- read_excel("Data/2021_ADMB_estimate.xlsx", sheet = 1)
SAFE2022_mod$quantities$biomass[1,1:length(1975:2021)] <- safe_data$Biomass
SAFE2022_mod$quantities$biomassSSB[1,1:length(1975:2021)] <- safe_data$SSB
SAFE2022_mod$quantities$R[1,1:length(1975:2021)] <- safe_data$R


plot_biomass(list(bridging_model_1, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Biomass", line = 1.8)
plot_ssb(list(bridging_model_1, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "SSB", line = 1.8)
plot_recruitment(list(bridging_model_1, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Recruitment", line = 1.8)
