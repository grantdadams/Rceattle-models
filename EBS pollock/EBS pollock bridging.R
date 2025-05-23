# Code to run the bering sea pollock model in CEATTLE
# model is a single sex, single-species model


library(Rceattle) # https://github.com/grantdadams/Rceattle/tree/dev-name-change

# Load data ----
ebs_pollock <- Rceattle::read_data( file = "Data/bsp0.xlsx")
ebs_pollock$estDynamics = 0
ebs_pollock$index_data$Log_sd <- ebs_pollock$index_data$Log_sd/ebs_pollock$index_data$Observation
ebs_pollock$index_data$Observation <- ebs_pollock$index_data$Observation
ebs_pollock$catch_data$Catch <- ebs_pollock$catch_data$Catch

ebs_pollock$catch_data$Log_sd <- 0.05
ebs_pollock$spawn_month = 3
ebs_pollock$fleet_control$Fleet_type[5] <- 2 # Setting ATS age-1 data as survey
# ebs_pollock$fleet_control$Estimate_q[3] <- 0 # Bottom trawl q = mean(ob_bts)/mean(eb_bts)
# ebs_pollock$fleet_control$Estimate_q[6] <- 3 # ATS_1 q = mfexp(mean(log(oa1_ats)-log(ea1_ats)));
yrs <- ebs_pollock$styr:ebs_pollock$endyr
ebs_pollock$age_error[1:15,3:17] <- diag(15) # Removing age error b/c turned off
ebs_pollock$fleet_control$Time_varying_sel[1] <- 1
ebs_pollock$fleet_control$Time_varying_sel[3] <- 0
ebs_pollock$fleet_control$Sel_sd_prior[3] <- 0.1

# Adjust survey timing
ebs_pollock$index_data <- ebs_pollock$index_data %>%
  dplyr::mutate(Month = case_when(
    Fleet_name == "BTS" ~ 6,
    Fleet_name == "BTS_1" ~ 6,
    Fleet_name == "ATS" ~ 6,
    Fleet_name == "ATS_1" ~ 6,
    Fleet_name == "AVO" ~ 0,
    Fleet_name == "Fishery CPUE" ~ 0
  ))


ebs_pollock$comp_data <- ebs_pollock$comp_data %>%
  dplyr::mutate(Month = case_when(
    Fleet_name == "BTS" ~ 6,
    Fleet_name == "ATS" ~ 6
  ))

# Initial fit ----
pollock_base <- Rceattle::fit_mod(data_list = ebs_pollock,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  verbose = 1,
                                  phase = TRUE,
                                  initMode = 4) # Fished equilibrium with init_dev's turned on
plot_biomass(pollock_base)
plot_f(pollock_base)


# Initial fit ----
pollock_re <- Rceattle::fit_mod(data_list = ebs_pollock,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = TRUE, # No random recruitment
                                random_sel = TRUE,
                                msmMode = 0, # Single species mode
                                  verbose = 1,
                                  phase = FALSE,
                                  initMode = 4) # Fished equilibrium with init_dev's turned on


# Fixed selectivity ----
fix_sel_dat <- ebs_pollock
fix_sel_dat$fleet_control$Selectivity <- 0

# - AVO and ATS have same selectivity
fix_sel_dat$emp_sel <- rbind(fix_sel_dat$emp_sel,
                             fix_sel_dat$emp_sel %>%
                               dplyr::filter(Fleet_code == 4) %>%
                               dplyr::mutate(Fleet_code = 2,
                                             Fleet_name = "AVO"))

fixed_sel <- Rceattle::fit_mod(data_list = fix_sel_dat,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  verbose = 1,
                                  phase = TRUE,
                                  initMode = 4) # Unfished equilibrium with init_dev's turned on


plot_biomass(fixed_sel)
plot_f(fixed_sel)



# Fixed N ----
fix_n_dat <- fix_sel_dat
fix_n_dat$estDynamics <- 1
fix_n <- Rceattle::fit_mod(data_list = fix_n_dat,
                                  inits = NULL, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 4, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  verbose = 1,
                                  phase = FALSE,
                                  initMode = 4) # Unfished equilibri


# Fixed parameters ----
inits <- fixed_sel$initial_params

# * R ----
log_avgrec <- log(exp(9.82718578635))# * 1000)
inits$rec_pars[1,1] <- log_avgrec
log_avginit <- log(exp(4.81597738115))# * 1000) # adjust to Finit
# exp(R0) = exp(Finit) * exp(Init)
# exp(R0)/exp(Init) = exp(Finit)
ln_Finit <- log(exp(log_avgrec)/exp(log_avginit))
inits$ln_Finit[1] = ln_Finit

rec_devs <- c(-1.06894462331, 0.121472199329, -0.209933893293, 0.317834599796, 0.174810588286, 0.342331830376, 0.236007619029, -0.252933109107, -0.455349077790, 0.366148191098, 0.0484023031037, -0.121294065592, -0.384488184327, -0.332270520538, 0.273079565822, 1.12410202451, 0.385981086846, 0.542734609276, -0.0156215169754, 1.01398929114, -0.278935072745, 0.562854185852, -0.391817318552, -0.963237975983, -1.22869046387, -0.488847420347, 0.980533926016, 0.353524790161, 0.200944838108, 0.880956432330, -0.197806026076, -0.555039426333, 0.173201274052, 0.472864382156, -0.243950200000, -0.160908660819, 0.270453887899, 0.612864588041, 0.203181377396, -0.273349245472, -1.07252615264, -1.42382830731, -0.500987352317, 0.279441715407, -0.353533150446, 0.963332375132, 0.190147409718, -0.307661581188, -0.451701409490, 0.836012981896, 0.973179973597, 0.135970728478, -0.838105575105, -0.837873281445, -0.115963727680, 1.47640890747, 0.0967757527037, -0.491846764061, -0.478320123701, -0.100487350573, -0.0132916975405)

inits$rec_dev[1,1:length(yrs)] <- rec_devs

init_dev <- c(3.31651457993, 2.83295538747, 1.29811258894, 0.470947202197, 1.14532358564, 0.351402276965, -0.786217891506, -1.24263681525, -1.23564214781, -1.23066044364, -1.22754004161, -1.22916331291, -1.23075162083, -1.23264536937)
inits$init_dev[1,1:14] <- init_dev

# * Sel ----

# * F ----
ln_mean_F <- -1.41909910524
F_dev <- c(-0.652281848461, -0.627345499037, -0.705131720594, -0.164994452564, -0.178914426406, -0.136810826271, 0.225957078791, 0.572908707580, 0.683722553001, 0.814051399002, 0.877296811277, 0.684199771421, 0.575223900387, 0.464115051473, 0.558135228426, 0.609392627305, 0.547254462022, 0.242525295960, -0.190760762721, -0.389535056553, -0.430427933231, -0.434035035407, -0.518871365508, -0.835740361465, -0.529115209443, -0.501252503399, -0.0875977256605, 0.120737903517, 0.427858552259, -0.00159298324970, -0.248932639007, -0.266027624411, -0.0940715011014, -0.0626710867105, -0.242014943492, -0.476499460385, -0.380764504199, -0.243661183198, -0.0790708178771, -0.00326381893725, -0.104666906256, -0.167236411811, 0.0411145829306, 0.124117274423, 0.234856999422, 0.215776377284, 0.0365117789478, 0.346283111670, 0.358254900975, 0.310404859255, 0.257032242425, 0.338020034369, 0.0207283102197, -0.00584338085735, -0.222922826376, -0.109985177681, 0.123218764574, 0.0654462500710, -0.285099865806, -0.301307888314, -0.196697082596)
inits$ln_F[1,] <- ln_mean_F + F_dev

# * Q ----
inits$index_ln_q[1] <- 0.101013734139  # CPUE
inits$index_ln_q[2] <- -8.18718928166  # - AVO
inits$index_ln_q[3] <- log(2.80831)    # BTS mean(ob_bts)/mean(eb_bts)
inits$index_ln_q[4] <- -0.615556187174 # - ATS
inits$index_ln_q[5] <- log(2.80831)    # - BTS_1
inits$index_ln_q[6] <- log(0.0716047)  # - ATS_1, q = mfexp(mean(log(oa1_ats)-log(ea1_ats)));

fixed_parms <- Rceattle::fit_mod(data_list = fix_sel_dat,
                               inits = inits, # Initial parameters = 0
                               file = NULL, # Don't save
                               estimateMode = 4, # Estimate
                               random_rec = FALSE, # No random recruitment
                               msmMode = 0, # Single species mode
                               verbose = 1,
                               phase = TRUE,
                               initMode = 4) # Unfished equilibrium with init_dev's turned on



# ADMB ----
library(readxl)
SAFE2024_mod <- pollock_base
SAFE2024_mod$quantities$ssb[1,1:length(yrs)] <- read_excel("Data/2024_ADMB_estimate.xlsx", sheet = 1)$SSB# * 1000
SAFE2024_mod$quantities$R[1,1:length(yrs)] <- read_excel("Data/2024_ADMB_estimate.xlsx", sheet = 1)$Recruitment# * 1000


fix_n$quantities$ssb <- fix_n$quantities$ssb# * 1000
fix_n$quantities$R[1,1:length(yrs)] <- fix_n$data_list$NByageFixed$Age1# * 1000

# Plot ----
mod_list <- list(pollock_base, pollock_re, fixed_sel, fixed_parms, SAFE2024_mod)
mod_names <- c("CEATTLE est parms", "RE", "CEATTLE fix sel", "fix par", "ADMB")

plot_ssb(mod_list, model_names = mod_names)
plot_recruitment(mod_list, model_names = mod_names)
# plot_f(fixed_parms, model_names = mod_names)
plot_f(list(pollock_base, fixed_sel, fixed_parms), model_names = mod_names[c(1,2,4)])

