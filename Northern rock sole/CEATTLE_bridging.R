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
inits$ln_srv_q[1] <- log(0.455623674394359)

# natmort_f:
inits$ln_M1[1,1,] <- log(0.199845511083)

# natmort_m:
inits$ln_M1[1,1,] <- log(0.233331849870)

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
inits$rec_pars[1,1] <- 6.88854757663

# R_logalpha:
inits$rec_pars[1,2] <- 3.25271180396 # log of Ricker alpha

# R_logbeta:
inits$rec_pars[1,3] <- -5.50431745500 # log of Ricker beta
# R_alpha*Stmp*mfexp(-R_beta*Stmp);
# (0.5*norm2(log(SAM_recruits)-log(SRC_recruits+1.0e-3)))/(sigmaR*sigmaR);

# rec_dev:
rec_devs <- c(-2.45692567652, -1.49785743962, -1.64659009734, -1.78008020594, -1.88481094390, -1.93924940277, -2.09485973360, -2.27115772452, -1.65331981347, -1.54242661138, -0.894069168435, -0.760194387222, -0.638925825840, -0.540261731019, -0.0300465796724, 0.229857950581, -0.507347621810, -0.840967286673, -0.815068129719, -0.521952201825, 0.329938337209, -0.303737993698, 0.0523160286823, -0.0347619461127, 0.206799064485, 0.751336941136, 0.763875654546, 0.711783099478, 1.40177034685, 1.26099951684, 1.19112113099, 1.73238628386, 2.27487849792, 1.30855136420, 1.07756808993, 1.93346306900, 1.21475162052, 0.655040873314, 0.820133753142, 0.362230709609, 0.426943767813, 0.498291008449, 0.0416118407195, 0.556179479779, 0.312328130723, 1.03792766734, 1.49571453974, 1.64265433323, 1.44980137086, 1.34261593061, 1.55112053941, 0.327117875331, -0.888652988264, -1.04542470957, -1.34825255254, -1.07732151603, -0.865608319464, -0.891511649868, -1.21965343541, 0.148143285335, 0.667796956079, 0.783192193486, 0.596810400830, 0.549079006104, 0.495287785232, 0.0798638708910, -0.257040044088, -0.0332566646777, 5.05572366833e-05)
inits$rec_dev[,1:50] <- rev(rev(rec_devs)[1:50])

inits$init_dev[1,1:19] <- rec_devs[1:19]

# log_avg_fmort:
inits$ln_mean_F[2] <- -2.43909254911

# fmort_dev:
inits$F_dev[2,] <- c(1.76760591387, 2.49486881305, -0.151322844096, 0.138850026758, -0.724466883354, -0.906910708321, -1.00742062891, -0.714570077114, -0.284972206652, 0.727548357927, -0.548356409320, -0.422472115361, 0.163052514303, 0.863934881377, 0.631443216325, -0.163684118717, 0.497098454346, 0.529139893600, 0.437772376890, 0.536715062031, 0.490034870031, 0.500542529669, 1.45431037696, -0.127838254838, -0.124443387993, -0.0361534205427, -0.837635496603, -0.452947370751, -0.655406173268, -0.278815957393, -0.504616643519, -0.390099407232, -0.386530867443, 0.0820006799070, 0.0700711120291, 0.218578487877, 0.277944866343, 0.546415380137, 0.225297893174, 0.166812977404, 0.0737325346014, -0.0247913664914, -0.183665841256, -0.437344296030, -0.388580037423, -0.407885900093, -1.02662430346, -0.953953319914, -0.591294573238, -0.160968918736)

# sel_slope_fsh_f:
inits$ln_sel_slp[1,2,1] <- log(0.967667897824)

# sel50_fsh_f: #FIXME double check
inits$sel_inf[1,2,1] <- 9.27994884170

# sel_slope_fsh_devs_f:
inits$ln_sel_slp_dev[1,2,1,] <- c(0.0765724082520, 0.121316785992, 0.0292448012516, 0.0389057435833, 0.220218184058, 0.0721535535307, 0.0990373715472, 0.238839415629, 0.100313091714, -0.00267127390991, 0.400233144286, 0.472248841587, 0.257883775904, 0.245426043030, 0.386488275480, 0.151522160979, -0.0761885413837, -0.104695981428, 0.172584845307, -0.0669217824230, 0.0424120460981, 0.0483907751554, 0.125142402098, -0.311046251345, -0.253015585425, -0.374582486810, -0.492649667698, -0.479058762432, -0.358341404434, -0.176294749418, -0.164406790817, -0.0682965968995, 0.0835064886333, 0.106223720932, 0.0930855359518, -0.0594412826363, 0.0162210066151, -0.154818623165, -0.348702987662, -0.124268507814, -0.358399185214, -0.219413249616, -0.415768614552, 0.0798825597141, -0.114333690657, 0.169206603843, 0.321068634369, 0.231081802903, 0.324125885395, -2.09389753261e-05)

# sel50_fsh_devs_f:
inits$sel_inf_dev[1,2,1,] <- c(0.410186451283, 0.499781497393, 0.426304738245, 0.513061863302, 0.000685442982713, -0.395050045018, -0.482453331419, -0.434694528326, -0.153287999309, -0.0832081693462, -0.563132571935, -0.336606974079, -0.374296406584, -0.341335982487, -0.308621086081, -0.234259943553, 0.0265327743033, 0.0971378957406, 0.0508436241756, 0.156707002060, 0.187691312436, 0.273656887387, 0.380627647894, 0.303461256631, 0.251464206365, 0.216372511264, 0.174957375871, 0.156773071166, 0.0927672184508, 0.0439817798742, 0.00808975049379, -0.0122403298814, -0.0683949109438, -0.0351930162986, 0.0239835535610, 0.106327736040, 0.0900068111019, 0.122691280785, 0.139593347399, 0.195892232867, 0.241651079792, 0.175776954132, 0.155059951796, -0.192629351324, -0.100366271396, -0.344772155604, -0.374307034557, -0.339872736368, -0.347621320434, 0.000275981987990)

# sel_slope_fsh_m:
inits$ln_sel_slp[1,2,2] <- log(1.23530084403)

# male_sel_offset:
inits$sel_inf[1,2,2] <- -0.181630137280 #FIXME - check useage

# sel50_fsh_m:
inits$sel_inf[1,2,2] <- 7.40449017289

# sel_slope_fsh_devs_m:
inits$ln_sel_slp_dev[1,2,2,] <- c(0.0370804888598, 0.0260291160401, -0.0219656119456, 0.00275144345702, 0.0390465459798, -0.162367880626, 0.202978239380, -0.131311637273, -0.416174063846, -0.396427317275, 0.431603650885, 0.383786869712, 0.211267259642, 0.444892545379, 0.174176845780, 0.236913403768, -0.0218722603318, -0.0381537004898, 0.290815945772, 0.247899078972, 0.0286255800363, 0.0349509759070, 0.0321452233459, 0.0959795067637, -0.102352534614, -0.183752389810, -0.157196491533, -0.121718863044, 0.0147802743964, -0.0524709315866, -0.0116802066894, -0.0549025605413, -0.0804435275397, -0.0860039370505, -0.0930478823130, -0.161761548774, -0.149535584243, -0.399429391400, -0.361687825991, -0.199347589199, -0.165553403332, -0.277150025862, -0.0303573180442, 0.160208191141, 0.108734138865, -0.00537313813781, 0.170418239365, 0.304583178045, 0.202374553213, -3.95797913377e-06)

# sel50_fsh_devs_m:
inits$sel_inf_dev[1,2,2,] <- c(0.380897851501, 0.559897617423, -0.439076410671, -0.575947019534, 0.240336300438, -0.306753307871, -0.549675853278, -0.176620020828, 0.214588387922, 0.0330705609746, -0.576201572323, -0.318495874392, -0.314241217079, -0.299783534664, -0.196860570523, -0.281005452737, 0.0352896212001, 0.0987161447920, 0.106253239753, 0.112005718942, 0.328165008350, 0.401337695406, 0.587085678478, 0.268807256484, 0.256811559129, 0.231066692948, 0.0513191485426, 0.0103676360429, -0.0637102067358, 0.0453711629740, -0.0845731758812, 0.0371007307116, 0.0270047148588, 0.145275758384, 0.132771646832, 0.181852021297, 0.203464848501, 0.264671370863, 0.190520407736, 0.278409477140, 0.214023620916, 0.0337660966798, -0.0926846030669, -0.194235121444, -0.241919805148, -0.210616980506, -0.261347376422, -0.286137682742, -0.199980670463, -0.000380926563317)


# sel_slope_srv:
inits$ln_sel_slp[1,1,1] <- log(1.76203489811)
# sel50_srv:
inits$sel_inf[1,1,1] <- 3.62612322933
# sel_slope_srv_m:
inits$ln_sel_slp[1,1,2] <- log(0.326795738735)
# sel50_srv_m:
inits$sel_inf[1,1,2] <- -0.153764605038


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
