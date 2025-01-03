# Code to run the bering northern rock sole assessment in CEATTLE
# https://github.com/afsc-assessments/BSAI_NRS/tree/main (2022 alldata)
# model is a two sex, single-species model

# DATA
# - Fishery catch
# - Fishery age composition
# - Fishery weight-at-age
# - Survey biomass and standard error
# - Survey age composition
# - Survey weight-at-age
# - Age at maturity

# MODEL
# - Two sex
# - Survey selectivity = sex-specific logistic
# - Survey q
# - Fishery selectivity = sex-specific time-varying logistic
# - Ricker recruitment (1978-2017)
# - Empirical weight-at-age
# - M = 0.15 for females, estimated for males

# Load data ----
library(Rceattle)
mydata_nrs <- Rceattle::read_data(file = "Data/nrs_single_species_2022.xlsx")
mydata_nrs$estDynamics = 0
mydata_nrs$srv_biom$Log_sd <- mydata_nrs$srv_biom$Log_sd/mydata_nrs$srv_biom$Observation
mydata_nrs$fsh_biom$Catch <- mydata_nrs$fsh_biom$Catch*1000

# - Fix M
bridging_model_1 <- Rceattle::fit_mod(data_list = mydata_nrs,
                                          inits = NULL, # Initial parameters = 0
                                          file = NULL, # Don't save
                                          estimateMode = 0, # Estimate
                                          random_rec = FALSE, # No random recruitment
                                          msmMode = 0, # Single species mode
                                          verbose = 1,
                                          phase = "default",
                                          initMode = 1)

# - Est female and male M
bridging_model_2 <- Rceattle::fit_mod(data_list = mydata_nrs,
                                      inits = bridging_model_1$estimated_params,
                                      file = NULL, # Don't save
                                      estimateMode = 0, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      M1Fun = build_M1(M1_model = c(2),
                                                       M1_prior_mean = 0.15,
                                                       M1_prior_sd = 0.2,
                                                       M1_use_prior = TRUE),
                                      phase = NULL,
                                      initMode = 1)


# Fixed parameters ----
inits <- bridging_model_1$estimated_params

# ln_q_srv:
inits$ln_srv_q[1] <- log(0.490377126779966)

# natmort_f:
inits$ln_M1[1,1,] <- log(0.191649019826)

# natmort_m:
inits$ln_M1[1,1,] <- log(0.225712300360)

# Linf_f:
40.4644535000
# K_f:
0.126336700000
# t0_f:
0.594268800000
# Linf_m:
36.0729742000
# K_m:
0.138976400000
# t0_m:
0.357008100000

# growth_alpha:
0.380000000000
# q_alpha:
-0.136000000000

# mean_log_rec:
inits$rec_pars[1,1] <- 7.28775531279
# rec_dev:
inits$rec_dev[,1:50] <- c(-1.10180859866, -0.206249460689, -0.797121613091, -0.421614500519, -0.511418504005, -0.274640862223, 0.270631969873, 0.286627231968, 0.228560082727, 0.908047903204, 0.759328639907, 0.681126176478, 1.21244693693, 1.74925591177, 0.783514020365, 0.552105760888, 1.40575752140, 0.688757877362, 0.131878460020, 0.298855718500, -0.155186043811, -0.0878664336820, -0.0138337665679, -0.466391081075, 0.0477675294307, -0.196506753482, 0.525692088471, 0.981600912630, 1.12716629216, 0.934196428032, 0.828607909875, 1.03967676631, -0.177682014648, -1.38114121148, -1.53027255252, -1.82601068255, -1.55665727793, -1.34221562205, -1.36202121411, -1.68424153009, -0.324759074650, 0.187773779312, 0.295839647794, 0.107545812736, 0.0717684451852, 0.0413228251079, -0.282214080319, -0.391046466936, -0.0549316418231, -2.16933102433e-05)

# mean_log_init:
3.61909326312

# init_dev_m:
1.48913035590 1.36790691558 1.51751184957 1.93039556097 1.43958158042 0.957311664961 0.678245662991 0.235281963811 -0.348751386028 -0.917033685307 -0.773282438071 -1.11559332905 -1.12428357537 -1.11784626637 -1.16250016643 -1.17026651844 -1.14769381416 -1.14575151847 -1.14024258164

# init_dev_f:
2.21414141018 1.90167890817 2.01216929073 2.60601369004 2.13121658141 1.26705677251 0.929373541370 0.662288419981 0.440436782084 -0.440266408651 -0.772036539854 -1.44864768661 -1.38321961329 -1.39298872620 -1.44198672775 -1.45096449098 -1.44412805063 -1.42711521215 -1.41514270207


# log_avg_fmort:
inits$ln_mean_F[2] <- -2.34829591526

# fmort_dev:
inits$F_dev <- c(1.26147418940, 1.64424728483, 1.54804517470, 3.29094176605, -0.569816574128, -0.818961675879, -0.951999577968, -0.664200287701, -0.227673527457, 0.811195721273, -0.587578570655, -0.469274304480, 0.0996184737294, 0.782168769942, 0.551527767928, -0.251869867874, 0.403081858138, 0.429623957528, 0.333000750940, 0.423135929003, 0.424152000243, 0.437120722666, 1.37933894683, -0.236169521806, -0.231905529002, -0.144334896245, -0.946287738549, -0.569926642233, -0.760957289622, -0.376065445912, -0.603818414579, -0.491232938275, -0.487452577077, -0.0193924559875, -0.0373716726306, 0.106975995823, 0.167239680339, 0.438404938993, 0.105783415609, 0.0511217742942, -0.0463863827781, -0.130444964162, -0.300904240053, -0.541527423596, -0.501253455240, -0.525051048543, -1.14930055193, -1.08331061749, -0.716872397538, -0.246858466055)

# sel_slope_fsh_f:
inits$ln_sel_slp[1,1,2] <- log(0.976433012822)

# sel50_fsh_f: #FIXME double check
inits$sel_inf[1,1,2] <- 9.24974937395

# sel_slope_fsh_devs_f:
inits$ln_sel_slp_dev[1,1,2,] <- c(0.0562269407009, 0.0728614938076, 0.0760910301221, 0.202501116708, 0.141012440794, 0.0123005346692, 0.0703445441143, 0.207202287279, 0.0718914119069, -0.0429008323411, 0.391889946851, 0.460370519309, 0.244456791661, 0.243660019846, 0.381910955775, 0.142099640446, -0.0914875308057, -0.118941440418, 0.168546360018, -0.0708593329966, 0.0472187018226, 0.0545962866349, 0.136193672544, -0.321079705957, -0.253514181816, -0.367613574888, -0.488237445528, -0.453911171336, -0.342626438269, -0.169276746516, -0.160899811922, -0.0634155859070, 0.0854497005449, 0.105805993528, 0.0961150670299, -0.0632814836660, 0.0214698534610, -0.153244827468, -0.329546740705, -0.104089389155, -0.343005317099, -0.206337672679, -0.410509562483, 0.0860965404891, -0.116368242733, 0.167213208682, 0.325669659069, 0.249840199330, 0.352105145509, 7.14827299664e-06)

# sel50_fsh_devs_f:
0.382901692107 0.472261600760 0.530975414931 0.619294475042 0.0358969102716 -0.351381996307 -0.454556312720 -0.406577924872 -0.127499892109 -0.0451044029670 -0.565220506557 -0.334800849852 -0.376273143796 -0.350843934854 -0.314436155042 -0.242068217036 0.0224210464267 0.0926560633318 0.0450061419811 0.148713449419 0.195946911720 0.283663217935 0.384892813521 0.297853488940 0.243068941629 0.202814760854 0.156909442049 0.128258083357 0.0705105300714 0.0295191781603 -0.00596621646865 -0.0254757507580 -0.0786147553740 -0.0434719815831 0.0153085475202 0.0984621856734 0.0811769570174 0.113961761906 0.123111342602 0.185906387405 0.229744185353 0.168376163318 0.137449060018 -0.203379961980 -0.110734914124 -0.355339770397 -0.385954924216 -0.356488500429 -0.362833615151 -3.75382756115e-05




# - Fix female M and estimate male M
inits <- bridging_model_1$estimated_params
map <- build_map(data_list = bridging_model_2$data_list, params = inits)
map$mapList$ln_M1[1,1,] <- NA
map$mapFactor$ln_M1 <- factor(map$mapList$ln_M1)

bridging_model_3 <- Rceattle::fit_mod(data_list = mydata_nrs,
                                      inits = inits,
                                      map = map,
                                      file = NULL, # Don't save
                                      estimateMode = 0, # Estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      verbose = 1,
                                      phase = NULL,
                                      initMode = 1)

#
# bridging_model_re <- Rceattle::fit_mod(data_list = mydata_nrs,
#                                       inits = bridging_model_1$estimated_params, # Initial parameters = 0
#                                       file = NULL, # Don't save
#                                       estimateMode = 0, # Estimate
#                                       random_rec = TRUE, # No random recruitment
#                                       msmMode = 0, # Single species mode
#                                       verbose = 1,
#                                       phase = NULL,
#                                       initMode = 2)


# - SAFE model
library(readxl)
SAFE2022_mod <- bridging_model_1
SAFE2022_mod$quantities$biomass[1,1:length(1975:2022)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 4)$Est * 1000
SAFE2022_mod$quantities$biomassSSB[1,1:length(1975:2022)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 3)$Est * 1000
SAFE2022_mod$quantities$R[1,1:length(1975:2022)] <- read_excel("Data/2022_ADMB_estimate.xlsx", sheet = 2)$Est * 1000

# Plots ----
plot_biomass(list(bridging_model_2, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Biomass", line = 1.8)
plot_ssb(list(bridging_model_3, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "SSB", line = 1.8)
plot_recruitment(list(bridging_model_3, SAFE2022_mod), model_names = c("CEATTLE", "SAFE")); mtext(side = 2, "Recruitment", line = 1.8)

# dev.off()
plot_selectivity(bridging_model_3)
