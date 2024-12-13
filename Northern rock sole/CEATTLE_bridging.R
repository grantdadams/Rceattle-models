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
mydata_nrs <- Rceattle::read_data(file = "Data/nrs_single_species_2024.xlsx")
mydata_nrs$estDynamics = 0
mydata_nrs$srv_biom$Log_sd <- mydata_nrs$srv_biom$Log_sd/mydata_nrs$srv_biom$Observation
mydata_nrs$fsh_biom$Catch <- mydata_nrs$fsh_biom$Catch*1000

# - Fix M ----
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
inits$ln_srv_q[1] <- log(0.453233537822568)

# natmort_f:
inits$ln_M1[1,1,] <- log(0.196616220256)

# natmort_m:
inits$ln_M1[1,1,] <- log(0.237217216642)

# NOTE: Growth is fixed and assumes last year growth into the future
# # Linf_f:
# 40.4644535000
# # K_f:
# 0.126336700000
# # t0_f:
# 0.594268800000
# # Linf_m:
# 36.0729742000
# # K_m:
# 0.138976400000
# # t0_m:
# 0.357008100000
#
# # growth_alpha:
# 0.380000000000
#
# # q_alpha: # Not active?
# -0.136000000000

# mean_log_rec:
inits$rec_pars[1,1] <- 6.86640138260

# R_logalpha:
inits$rec_pars[1,2] <- 3.19843225582 # log of Ricker alpha

# R_logbeta:
inits$rec_pars[1,3] <- -5.54412755432 # log of Ricker beta
# R_alpha*Stmp*mfexp(-R_beta*Stmp);
# (0.5*norm2(log(SAM_recruits)-log(SRC_recruits+1.0e-3)))/(sigmaR*sigmaR);

# rec_dev:
rec_devs <- c(-2.61299728881, -1.62509017324, -1.76416742029, -1.89288577007, -1.99442449690, -2.05668156073, -2.18993606167, -2.32793432906, -1.68775510360, -1.55736906911, -0.874877918005, -0.722757592681, -0.593997586866, -0.494112235115, 0.0145186384874, 0.272930393219, -0.463610324168, -0.800765026464, -0.783640876661, -0.495509562414, 0.356642124417, -0.271646129277, 0.0919490036372, 0.000462034542878, 0.232408538653, 0.772976327565, 0.788511482253, 0.730710504552, 1.41416380339, 1.27370677024, 1.20962287633, 1.75190534788, 2.29118239208, 1.32224832911, 1.08835556959, 1.94072149820, 1.21853359927, 0.656253363400, 0.820117606298, 0.361069020497, 0.426193457077, 0.499186612692, 0.0451424686091, 0.564039242565, 0.323472992607, 1.05126892962, 1.51088758376, 1.65762094230, 1.46307251007, 1.35452385538, 1.56161510658, 0.336749925341, -0.879116389042, -1.03622753218, -1.34300868930, -1.07917917347, -0.868089136831, -0.890007906114, -1.21591289806, 0.150133926204, 0.668411579474, 0.791833439301, 0.620684652412, 0.574301992545, 0.513743217761, 0.0896436131675, -0.255288189594, -0.0345784047545, 5.19244526842e-05)
inits$rec_dev[,1:50] <- rev(rev(rec_devs)[1:50])

inits$init_dev[1,1:19] <- rec_devs[1:19]

# log_avg_fmort:
inits$ln_mean_F[2] <- -2.55572868577

# fmort_dev:
inits$F_dev[2,] <- c(2.11020803237, 0.455130058571, -0.163184538383, 0.144766907506, -0.687170020550, -0.849529086409, -0.958475759194, -0.654765586611, -0.227021093075, 0.789885123097, -0.516094568676, -0.391830184827, 0.191708150600, 0.876373483126, 0.657021635450, -0.146648716293, 0.502105928298, 0.531701894736, 0.448163532788, 0.535081739124, 0.499625971161, 0.494732143550, 1.58302316633, -0.128099576357, -0.112185716280, -0.0199807887108, -0.818402885893, -0.443999852779, -0.625067054891, -0.230363792249, -0.453753364181, -0.334592847590, -0.327513942523, 0.141995238006, 0.126133057968, 0.271920378222, 0.331104927389, 0.600812894488, 0.263904312977, 0.202251575102, 0.0986398183888, 0.0187096523140, -0.153241794298, -0.383445583956, -0.337989836175, -0.351629204733, -0.972352843633, -0.907812011366, -0.532270949260, -0.147578216370
)

# sel_slope_fsh_f:
inits$ln_sel_slp[1,2,1] <- log(0.987962138436)

# sel50_fsh_f: #FIXME double check
inits$sel_inf[1,2,1] <- 9.12123829199

# sel_slope_fsh_devs_f:
inits$ln_sel_slp_dev[1,2,1,] <- c(0.0843090536040, 0.0401662793276, 0.0281969810861, 0.0403421395573, 0.213168762925, 0.0617099062030, 0.0992121528697, 0.225769981305, 0.0829313044773, -0.0267031833532, 0.411996797830, 0.469237108209, 0.255041600407, 0.262750348766, 0.389020502359, 0.149548711642, -0.0858804823901, -0.112720601833, 0.178574445497, -0.0596886684642, 0.0364229224655, 0.0409052780492, 0.127010106926, -0.313545471775, -0.247535261433, -0.364009352663, -0.486647529170, -0.439831591444, -0.334352304924, -0.167493114673, -0.159275377044, -0.0646157088371, 0.0803798827814, 0.0985665895332, 0.0865086159210, -0.0725450201567, 0.00800753344977, -0.166596700994, -0.345232315237, -0.116418657586, -0.350640044953, -0.212728008781, -0.415961391680, 0.100156224549, -0.108148900282, 0.172961482264, 0.328167766936, 0.247744088268, 0.331786074399, -2.27748782597e-05
)

# sel50_fsh_devs_f:
inits$sel_inf_dev[1,2,1,] <- c(0.451581346490, 0.413298334706, 0.427360292811, 0.516739648557, 0.0121580813852, -0.387566725519, -0.483976526843, -0.424738699504, -0.140567199890, -0.0665442318195, -0.570397558465, -0.332855061027, -0.375494440531, -0.351870237919, -0.307420839919, -0.236769933309, 0.0270663047423, 0.0975941061712, 0.0524128949327, 0.154638508740, 0.194094143635, 0.279742082129, 0.399753416523, 0.303105629247, 0.250632194727, 0.210418844295, 0.163207357558, 0.128789161036, 0.0751621385773, 0.0392665212072, 0.00486535889937, -0.0119025868164, -0.0627563970929, -0.0267389449142, 0.0325116163049, 0.115802723338, 0.0990587646624, 0.131782826648, 0.140914059587, 0.200734657527, 0.241875482082, 0.180895332871, 0.145048429558, -0.201855104187, -0.106909820986, -0.344983184861, -0.372393134143, -0.342087344765, -0.342936092230, 0.000254966796668
)

# sel_slope_fsh_m:
inits$ln_sel_slp[1,2,2] <- log(1.20269326029)

# male_sel_offset:
inits$sel_inf[1,2,2] <- -0.00000000000 #FIXME - turned off

# sel50_fsh_m:
inits$sel_inf[1,2,2] <- 7.32242535007

# sel_slope_fsh_devs_m:
inits$ln_sel_slp_dev[1,2,2,] <- c(0.0392826313798, -0.0362135209486, -0.0265707940705, 0.00268376324330, 0.0757288169373, -0.157456681722, 0.180142809768, -0.132177352583, -0.398598887305, -0.413601856293, 0.432897582162, 0.383467031517, 0.212199394287, 0.460736533242, 0.179451133589, 0.238529235305, 0.00149295559453, -0.0156868907742, 0.307870344394, 0.260336023908, 0.0294658075201, 0.0370234250309, 0.0221487264947, 0.108050198564, -0.0893384481186, -0.173703786847, -0.148240920250, -0.103236335252, 0.0267939655441, -0.0444216235254, -0.0122257322920, -0.0578895490139, -0.0801428945849, -0.0794983557930, -0.0915043901711, -0.161549806801, -0.161807580272, -0.413554976244, -0.381862813302, -0.207892140318, -0.164530631097, -0.321753126618, -0.0253891057078, 0.155699679133, 0.114943839717, -0.000831933504195, 0.167166332550, 0.295802013188, 0.167772677400, -4.66100305729e-06
)

# sel50_fsh_devs_m:
inits$sel_inf_dev[1,2,2,] <- c(0.457890068464, -0.703812511735, -0.478625440485, -0.619282623629, 0.246946991454, -0.262238222526, -0.499294472416, -0.131894696162, 0.245773008083, 0.0881390035721, -0.547346154647, -0.292275302756, -0.285713366529, -0.281267729704, -0.170188224187, -0.254212705750, 0.0529966369724, 0.115906120410, 0.123145152759, 0.128368762709, 0.338535469394, 0.408045345676, 0.643731659961, 0.283701790564, 0.273196190245, 0.248905287563, 0.0728818386307, 0.0281990008141, -0.0403762641297, 0.0725835082975, -0.0514066675475, 0.0695997117035, 0.0588667670855, 0.172666194619, 0.161216675691, 0.210234616971, 0.233302814844, 0.297313521836, 0.221005744071, 0.301409242070, 0.234784804048, 0.0928868336047, -0.0660647305110, -0.161702708104, -0.212868332691, -0.177874791338, -0.229070361394, -0.255334923548, -0.160979352637, -0.000403647433155
)


# sel_slope_srv:
inits$ln_sel_slp[1,1,1] <- log(1.71532862812)
# sel50_srv:
inits$sel_inf[1,1,1] <- 3.66176543369
# sel_slope_srv_m:
inits$ln_sel_slp[1,1,2] <- log(0.358614601969)
# sel50_srv_m:
inits$sel_inf[1,1,2] <- -0.165110610121


fixed_model <- Rceattle::fit_mod(data_list = mydata_nrs,
                                      inits = inits,
                                      map = NULL,
                                      file = NULL, # Don't save
                                      estimateMode = 3, # Do not estimate
                                      random_rec = FALSE, # No random recruitment
                                      msmMode = 0, # Single species mode
                                      recFun = build_srr(srr_fun = 0,
                                                         srr_pred_fun = 4,
                                                         proj_mean_rec = FALSE,
                                                         srr_est_mode = 1, # Freely estimat
                                                         srr_prior_mean = exp(3.22874107555),
                                                         srr_prior_sd = 0.2),
                                      verbose = 1,
                                      phase = NULL,
                                      initMode = 1)


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
                                      recFun = build_srr(srr_fun = 0,
                                                         srr_pred_fun = 4,
                                                         proj_mean_rec = FALSE,
                                                         srr_est_mode = 1, # Freely estimat
                                                         srr_prior_mean = exp(3.22874107555),
                                                         srr_prior_sd = 0.2),
                                      verbose = 1,
                                      phase = NULL,
                                      initMode = 1)


# - SAFE model
library(readxl)
SAFE2024_init_mod <- bridging_model_1
SAFE2024_init_mod$quantities$biomass[1,1:length(1975:2024)] <- read_excel("Data/2024_ADMB_estimate.xlsx", sheet = 4)$Est * 1000
SAFE2024_init_mod$quantities$biomassSSB[1,1:length(1975:2024)] <- read_excel("Data/2024_ADMB_estimate.xlsx", sheet = 3)$Est * 1000
SAFE2024_init_mod$quantities$R[1,1:length(1975:2024)] <- read_excel("Data/2024_ADMB_estimate.xlsx", sheet = 2)$Est * 1000

fixed_model$quantities$biomass <- fixed_model$quantities$biomass * 1000
fixed_model$quantities$biomassSSB <- fixed_model$quantities$biomassSSB * 1000
fixed_model$quantities$R <- fixed_model$quantities$R * 1000

# Plots ----
plot_biomass(list(fixed_model, SAFE2024_init_mod), model_names = c("CEATTLE fix", "SAFE")); mtext(side = 2, "Biomass", line = 1.8)
plot_ssb(list(fixed_model, SAFE2024_init_mod), model_names = c("CEATTLE fix", "SAFE")); mtext(side = 2, "SSB", line = 1.8)
plot_recruitment(list(fixed_model, SAFE2024_init_mod), model_names = c("CEATTLE fix", "SAFE")); mtext(side = 2, "Recruitment", line = 1.8)

# dev.off()
plot_selectivity(bridging_model_3)
