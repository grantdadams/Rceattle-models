# Code to run the GOA northern rockfish assessment in CEATTLE
# model is a single-sex, single-species model
# uses dev_srr branch
# https://github.com/grantdadams/Rceattle/tree/dev_srr

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
# https://github.com/grantdadams/Rceattle/tree/dev_srr
library(Rceattle)
mydata <- Rceattle::read_data( file = "Data/goa_northern_single_species_2022.xlsx")
mydata$estDynamics = 0
mydata$index_data$Log_sd <- sqrt(log(1 + (mydata$index_data$Log_sd^2) / (mydata$index_data$Observation^2)))
yrs <- mydata$styr:mydata$endyr

# Adjust sample sizes
comp_samp <- read.csv("Data/2022 SAFE sample sizes.csv")
comp_samp <- comp_samp %>%
  group_by(Fleet_name) %>%
  mutate(
    nmulti = sqrt(Nhauls * Nsamp),
    nmulti = nmulti/max(nmulti) * 100
  )

mydata$comp_data$Sample_size <- comp_samp$nmulti



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


plot_biomass(list(model1, model2, SAFE2022_mod), model_names = c("CEATTLE fix M", "CEATTLE est M", "ADMB"))
plot_ssb(list(model1, model2, SAFE2022_mod), model_names = c("CEATTLE fix M", "CEATTLE est M", "ADMB"))

