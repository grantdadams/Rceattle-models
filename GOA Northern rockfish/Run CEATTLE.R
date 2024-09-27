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
# https://github.com/grantdadams/Rceattle/tree/dev_srr
library(Rceattle)
mydata <- Rceattle::read_data( file = "Data/goa_northern_single_species_2022.xlsx")
mydata$estDynamics = 0
mydata$srv_biom$Log_sd <- sqrt(log(1 + (mydata$srv_biom$Log_sd^2) / (mydata$srv_biom$Observation^2)))
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
# - fixed parameters
mydata$srr_prior_mean <- 9
mydata$initMode <- 1
inits <- build_params(mydata)

# - R
inits$rec_pars[1,1] <- log(exp(3.52433455863))
rec_devs <- c(-1.20259137396, -1.20698785972, -1.21161424938, -1.21647724332, -1.22158650968, -1.22695489805, -1.23259301765, -1.23851001522, -1.24471625599, -1.25122170100, -1.25803572669, -1.26517090257, -1.27263374996, -1.28043758843, -1.28858913479, -1.29709945527, -1.30597299044, -1.31522178193, -1.32484933276, -1.33486456560, -1.34526901702, -1.35606830151, -1.36726168014, -1.37884662898, -1.39082266545, -1.40318494536, -1.41591731301, -1.42901131012, -1.44244599162, -1.45619846660, -1.47023300238, -1.48449410587, -1.49894489440, -1.51351521068, -1.52812594860, -1.54265362979, -1.55693971961, -1.57035827522, -1.58081185157, -1.58296546628, -1.56687873274, -1.52300062540, -1.46084898066, -1.42494020414, -1.44199910214, -1.45766936798, -1.40088501491, -1.26224511987, -1.09978275086, -0.985173240082, -1.00408697767, -1.11784189657, -1.17749196527, -1.12130565384, -1.11917695446, -1.10050314004, -0.814343654225, -0.486873121459, -0.730194970069, 0.601109862793, -0.619834540083, -0.654270400159, -0.157011432599, -0.585926686346, -0.461308894395, 0.715981712517, 0.208874979646, -0.430958661534, -0.394575773465, -0.270768012779, 0.0170158419119, 0.134258811965, -0.368732584017, 0.637120600232, 0.0506017472232, -0.728701143854, -0.393285506748, -0.323063092133, -1.21634990348, -0.383174772177, -0.776048893898, -0.894546555600, -1.23250542506, 0.419474906260, 0.0101253075013, -0.405297171463, -0.380043607840, 0.206083233007, -0.669937736014, -0.981281834002, -0.764236049127, -1.38358096599, -2.22062609848, -2.18825526968, -1.70327315490, -1.95316701316, -1.84326129023, -1.66338763492, -1.56307785893, -1.83572226268, -2.18013576454, -1.67577927104, -2.04464609456, -1.67801820826, -1.93348976604, -1.60878571237, -1.50060869012, -1.32869850754, -1.21344327744, -1.12499998882)

inits$rec_dev[1,1:length(yrs)] <- rec_devs[49:110]
inits$init_dev[1,1:48] <- rev(rec_devs[1:48])
inits$init_dev[1,49] <- 0

# - Sel
# Survey 1 (bottom trawl) - Logistic
inits$ln_sel_slp[1,1,1] <- log(2.944438979/1.87616784538)
inits$sel_inf[1,1,1] <- 9.02330670058

# Fishery  - Logistic
inits$ln_sel_slp[1,2,1] <- log(2.944438979/4.21707390136) #
inits$sel_inf[1,2,1] <- 8.16749734362 #
1/(1+ exp(-exp(inits$ln_sel_slp[1,1,1]) * (1:50 - inits$sel_inf[1,1,1])))

# - F
inits$ln_mean_F[2] <- -3.65537406043
inits$F_dev[2,] <- c(-1.21583360072, 0.208300944943, 1.02224938665, 1.75087612451, 2.36732240515, 1.98716279645, 1.57546873967, 1.46003896616, 1.16667385291, 0.684302413717, 1.23764528412, 1.26108054811, 0.946551668043, 0.831816077070, 0.788246934281, 0.626721794737, -0.760382072628, -1.05315272548, -1.02505754480, -0.946550206224, -0.428638144379, 0.486578000546, 0.356480232275, -1.01623296605, -2.82344262936, -2.62635371070, -2.02489664990, -1.24585956640, -0.970393495626, -0.901584180679, 0.0263520615835, 0.536684960845, 0.0304446841041, 0.224769920589, 0.169741334402, -0.348591523209, -0.475933369795, -0.429332519077, 0.152780101605, -0.308369351628, -0.355878455487, -0.293536082134, 0.144521056256, 0.0507011141852, -0.0114274320585, 0.0775750318789, -0.0922133229401, -0.116177664258, -0.122910411888, -0.109402453472, -0.199128986019, 0.234452267471, 0.252981671776, 0.178714076939, 0.153366641382, 0.0692357531827, -0.509461886091, -0.216706653358, -0.0150948730738, -0.106000314763, -0.0607257150428, -0.250578313488)

# - Q
inits$ln_srv_q[1] <- -0.0504472640418

# - Run model with fixed parameters
mydata$estDynamics <- 0
bridging_model_1 <- Rceattle::fit_mod(data_list = mydata,
                                      inits = inits, # Initial parameters = 0
                                      file = NULL, # Don't save
                                      estimateMode = 4, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      phase = NULL,
                                      initMode = 1, # Assume unfished equilibrium
                                      M1Fun = build_M1(updateM1 = TRUE, # Fill in M1 from data list
                                                       M1_model = 0) # Fix M (sex-combined)
)
bridging_model_1$quantities$sel[1,1,,1]

# Model 2 ----
# - Estimate
bridging_model_2 <- Rceattle::fit_mod(data_list = mydata,
                                      inits = NULL, # Initial parameters = 0
                                      file = NULL, # Don't save
                                      estimateMode = 0, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      phase = NULL,
                                      initMode = 1 # Assume unfished equilibrium
                                      )


# Model 3 ----
# - Estimate M
bridging_model_3 <- Rceattle::fit_mod(data_list = mydata,
                                      inits = NULL, # Initial parameters = 0
                                      file = NULL, # Don't save
                                      estimateMode = 0, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      phase = NULL,
                                      initMode = 1, # Assume unfished equilibrium
                                      M1Fun = build_M1(updateM1 = TRUE,
                                                       M1_model = 1,
                                                       M1_use_prior = TRUE,
                                                       M1_prior_mean = 0.06,
                                                       M1_prior_sd = 0.05)
                                      )



# - SAFE model
library(readxl)
SAFE2022_mod <- bridging_model_1
SAFE2022_mod$quantities$biomass[1,1:length(yrs)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 1)$Biomass
SAFE2022_mod$quantities$biomassSSB[1,1:length(yrs)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 1)$SSB
SAFE2022_mod$quantities$R[1,1:length(yrs)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 1)$Recruitment


# - Rtmb model
load("report2.Rdata")
RTMB_model <- bridging_model_1
RTMB_model$quantities$biomass[1,1:length(yrs)] <- report2$tot_bio
RTMB_model$quantities$biomassSSB[1,1:length(yrs)] <- report2$spawn_bio
RTMB_model$quantities$R[1,1:length(yrs)] <- report2$Nat[1,]


plot_biomass(list(bridging_model_1, bridging_model_2, SAFE2022_mod, RTMB_model), model_names = c("CEATTLE fix parms", "CEATTLE est parms", "ADMB", "RTMB"))
plot_ssb(list(bridging_model_1, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "SSB", line = 1.8)


dev.off()
plot_selectivity(bridging_model_3)
