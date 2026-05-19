# Code to run the GOA northern rockfish assessment in CEATTLE
# model is a single-sex, single-species model

# DATA
# - Fishery catch
# - Fishery age and length composition
# - Survey biomass and standard error
# - Survey age and length composition
# - Length-at-age and weight-at-age from surveys
# - Age at maturity input and M fixed

# MODEL
# - Single sex
# - Survey selectivity = sex-combined logistic
# - Survey q
# - Fishery selectivity =  sex-combined logistic
# - Empirical weight-at-age
# - M = estimated with prior

# Load data ----
library(Rceattle)
library(dplyr)
mydata <- Rceattle::read_data( file = "Data/2022_GOA_northern_rockfish.xlsx")



# Model 1 ----
# - Estimate
model1 <- Rceattle::fit_mod(data_list = mydata,
                                      inits = NULL, # Initial parameters = 0
                                      file = NULL, # Don't save
                                      estimateMode = 0, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      phase = TRUE,
                                      initMode = 1 # Assume unfished equilibrium
                                      )


# Model 3 ----
# - Estimate M
model2 <- Rceattle::fit_mod(data_list = mydata,
                                      inits = NULL, # Initial parameters = 0
                                      file = NULL, # Don't save
                                      estimateMode = 0, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      phase = TRUE,
                                      initMode = 1, # Assume unfished equilibrium
                                      M1Fun = build_M1(updateM1 = TRUE,
                                                       M1_model = 1,
                                                       M1_use_prior = TRUE,
                                                       M_prior = 0.06,
                                                       M_prior_sd = 0.05)
                                      )



# - SAFE model
library(readxl)
SAFE2022_mod <- model1
SAFE2022_mod$quantities$biomass[1,1:length(yrs)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 1)$Biomass
SAFE2022_mod$quantities$ssb[1,1:length(yrs)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 1)$SSB
SAFE2022_mod$quantities$R[1,1:length(yrs)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 1)$Recruitment

# - Plot
plot_biomass(list(model1, model2, SAFE2022_mod), model_names = c("CEATTLE fix M", "CEATTLE est M", "ADMB"))
plot_ssb(list(model1, model2, SAFE2022_mod), model_names = c("CEATTLE fix M", "CEATTLE est M", "ADMB"))

