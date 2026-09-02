library(Rceattle)
library(dplyr)

# Load data ----
CSL_SBF_ATF_hakedata_DM <- read_data(file = "MSE_hake_yr24_final.xlsx")

CSL_SBF_ATF_hakedata_DM$index_data <- CSL_SBF_ATF_hakedata_DM$index_data %>%
  dplyr::select(-Q_block)

CSL_SBF_ATF_hakedata_DM$endyr <- 2023
CSL_SBF_ATF_hakedata_DM$projyr <- 2030 # Need to changes


# Dirichlet-multinomial for the composition/diet data ----
CSL_SBF_ATF_hakedata_DM$fleet_control$Comp_distribution <- "DirichletMultinomial"  # age-composition (1 works)
CSL_SBF_ATF_hakedata_DM$Diet_distribution <- rep(1, CSL_SBF_ATF_hakedata_DM$nspp)     # diet (1 = DirichletMultinomial)

# Prior on DM weights ----
# A N(0, 2) prior on the log-scale weight for every fleet's age-comps and every
# predator's diet keeps the DM weights identifiable (see setup note 2 above).
comp_flts <- CSL_SBF_ATF_hakedata_DM$fleet_control$Fleet_code
compFun <- build_composition(linkages = list(
  theta_comp = linkage_spec(formula = ~ 1,
                            by = ~ fleet,
                            fleet   = comp_flts,
                            priors = list(`(Intercept)` = prior_lognormal(0, 2))),
  theta_diet = linkage_spec(formula = ~ 1,
                            by = ~ species,
                            species = seq_len(CSL_SBF_ATF_hakedata_DM$nspp),
                            priors = list(`(Intercept)` = prior_lognormal(0, 2)))))

# 1. Single-species: no future F ----
ss_run_DM_CSL <- fit_mod(data_list = CSL_SBF_ATF_hakedata_DM,
                     inits = NULL,
                     compFun = compFun,
                     estimateMode = "Estimate", # 0 or "Estimate" works
                     msmMode = "SingleSpecies", # 0 or "SingleSpecies" works
                     random_rec = FALSE,
                     initMode = "NonEquilibrium", # 2 or "NonEquilibrium" works
                     fit_control = fit_control(phase = TRUE, verbose = 1))
summary(ss_run_DM_CSL)   # 2195.76

plot_recruitment(ss_run_DM_CSL, species =1)

# 2. Single-species: category 1 HCR ----
ss_run_DM_hcr_CSL <- fit_mod(data_list = CSL_SBF_ATF_hakedata_DM,
                         inits = NULL,
                         compFun = compFun,
                         estimateMode = "Estimate",
                         msmMode = "SingleSpecies",
                         random_rec = FALSE,
                         initMode = "NonEquilibrium",
                         HCR = build_hcr(HCR = 6, # Cat 1 HCR
                                         Flimit = c(0.45, 0.45,  0.3, 0.45), # F45%
                                         Ptarget = c(0.4, 0.4, 0.25, 0.4), # Target is 40% B0
                                         Plimit = c(0.1, 0.1, 0.05, 0.1), # No fishing when SB<SB10
                                         Pstar = 0.45,
                                         Sigma = 0.5),
                         fit_control = fit_control(phase = TRUE, verbose = 1))
summary(ss_run_DM_hcr_CSL) #2196.57

# 3. MSVPA with estimated M ----
ms_run_DM_CSL <- fit_mod(data_list = CSL_SBF_ATF_hakedata_DM,
                     inits = ss_run_DM_CSL$estimated_params,
                     compFun = compFun,
                     M1Fun = build_M1(M1_model = "sex_age_invariant"),  # estimate M without prior (1 or "sex_age_invariant" works)
                     estimateMode = "Estimate",
                     msmMode = "MSVPA", # 1 or "MSVPA" works
                     suitMode = "Empirical", # 0 or "Empirical" works
                     niter = 3,
                     random_rec = FALSE,
                     suit_styr  = c(1980, 1980, 1980, 1980),
                     suit_endyr = c(2019, 2019, 2019, 2019),
                     initMode = "NonEquilibrium",
                     fit_control = fit_control(phase = FALSE, verbose = 1))
summary(ms_run_DM_CSL)   # 2201.71

# 4. Estimated suitability ----
# Prey-size preference (gam_a / gam_b) are fixed; only the
# predator-prey vulnerabilities are estimated, and only for the two interaction that
# exist (arrowtooth eating hake, sablefish eating hake). Every other link is fixed to
# very small value (i.e. "predator does not eat this prey").
#
# Reusing ms_run_DM$map to keep gam_a / gam_b fixed.
inits <- ms_run_DM_CSL$estimated_params
map   <- ms_run_DM_CSL$map
inits$log_gam_a <- c(0, log(3.7), log(3.1), 0)    # mean predator/prey weight ratio
inits$log_gam_b <- c(0, log(1.83), log(1.120), 0)

# log_phi[predator, prey]; -999 = predator does not eat this prey
inits$log_phi[1, 2] <- inits$log_phi[2, 2] <- inits$log_phi[1, 3] <- inits$log_phi[2,4]<- -999
inits$log_phi[3, 3] <- inits$log_phi[2, 3] <- inits$log_phi[3, 2] <- inits$log_phi[3,3] <- -999
inits$log_phi[1,4] <- inits$log_phi[2,4]<- inits$log_phi[3,4] <- inits$log_phi[4,4] <- -999
inits$log_phi[4,2] <- inits$log_phi[4,3] <- -999 # Set CSL do not feed on ATF

# Estimate only [2,1] arrowtooth->hake and [3,1] sablefish->hake; fix the rest.
map$mapList$log_phi[] <- seq_len(length(map$mapList$log_phi))
map$mapList$log_phi[1, 1] <- map$mapList$log_phi[1, 2] <- map$mapList$log_phi[2, 2] <- map$mapList$log_phi[2, 4] <-  NA
map$mapList$log_phi[1, 3] <- map$mapList$log_phi[3, 3] <- map$mapList$log_phi[3, 4] <- NA
map$mapList$log_phi[2, 3] <- map$mapList$log_phi[3, 2] <- map$mapList$log_phi[3, 4] <- map$mapList$log_phi[4, 1] <- NA
map$mapList$log_phi[1, 4] <- map$mapList$log_phi[4, 2] <- map$mapList$log_phi[4, 3] <- map$mapList$log_phi[4, 4] <- NA

map$mapFactor$log_phi <- factor(map$mapList$log_phi)

# The donor map came from the empirical-suitability fit above, where the diet
# composition is not fit and the diet DM weight is therefore held fixed. Here
# arrowtooth and sablefish do have their diet fit, so free their weights (hake
# stays on empirical suitability, so its weight stays fixed).
map$mapList$diet_comp_weights[2:3] <- 2:3
map$mapFactor$diet_comp_weights <- factor(map$mapList$diet_comp_weights)

run_ms_CSL_Mest_prior_DM_CSL <- fit_mod(data_list = CSL_SBF_ATF_hakedata_DM,
                                    inits = inits,
                                    map = map,
                                    compFun = compFun,
                                    M1Fun = build_M1(M1_model = "sex_age_invariant",
                                                     M1_use_prior = TRUE,
                                                     M_prior = 0.2,
                                                     M_prior_sd = 0.1),
                                    estimateMode = "Estimate",
                                    msmMode = "MSVPA",
                                    suitMode = c("Empirical", "LognormalWeight", "LognormalWeight", "Empirical"), # c(0, 4, 4) also works
                                    suit_styr  = c(1980, 2013, 2005, 1980),   # hake, arrowtooth, sablefish
                                    suit_endyr = c(2019, 2018, 2008, 2019),
                                    initMode = "NonEquilibrium",
                                    niter = 3,
                                    random_rec = FALSE,
                                    fit_control = fit_control(
                                      loopnum = 5,
                                      phase = TRUE,
                                      verbose = 1))
summary(run_ms_CSL_Mest_prior_DM_CSL)  # 2421.14.

run_ms_CSL_Mest_prior_DM_CSL$quantities$vulnerability
# arrowtooth->hake 0.807
# sablefish->hake 0.754

run_ms_CSL_Mest_prior_DM_CSL$quantities$M1 #0.2719505

run_ms_CSL_Mest_prior_DM_CSL$quantities$jnll #2421.14
run_ms_CSL_Mest_prior_DM_CSL$quantities$jnll_comp
run_ms_CSL_Mest_prior_DM_CSL$quantities$M1 #0.2719505
run_ms_CSL_Mest_prior_DM_CSL$sdrep
run_ms_CSL_Mest_prior_DM_CSL$estimated_params$log_phi
run_ms_CSL_Mest_prior_DM_CSL$quantities$vulnerability #0.8450781 and 0.7672554
plot_diet_comp2(run_ms_CSL_Mest_prior_DM_CSL)

mod_list <- list(run_ms_CSL_Mest_prior_DM_CSL, ss_run_DM_hcr_CSL)
mod_names <- c("MS_model_Est_DM", "SS_HCR")

# Plot biomass trajectory
plot_ssb(Rceattle = mod_list, model_names = mod_names, species = 1) #Now biomass looks alike
plot_biomass(Rceattle = mod_list, model_names = mod_names, species = 1)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, species = 1)

plot_depletionSSB(Rceattle = mod_list, model_names = mod_names, species = 1)

plot_m2_at_age_prop(Rceattle = mod_list, model_names = mod_names, species = 1)
plot_b_eaten_prop(Rceattle = mod_list, model_names = mod_names, species = 1)

plot_mortality(run_ms_CSL_Mest_prior_DM_CSL)

run_ms_CSL_Mest_prior_DM_CSL_stable<- fit_mod(data_list = run_ms_CSL_Mest_prior_DM_CSL$data_list,
                                              inits = run_ms_CSL_Mest_prior_DM_CSL$initial_params,
                                              map = run_ms_CSL_Mest_prior_DM_CSL$map,
                                              compFun = compFun,
                                              M1Fun = build_M1(M1_model = "sex_age_invariant",
                                                               M1_use_prior = TRUE,
                                                               M_prior = 0.2,
                                                               M_prior_sd = 0.1),
                                              estimateMode = "Estimate",
                                              msmMode = "MSVPA",
                                              suitMode = c("Empirical", "LognormalWeight", "LognormalWeight", "Empirical"), # c(0, 4, 4) also works
                                              suit_styr  = c(1980, 2013, 2005, 1980),   # hake, arrowtooth, sablefish
                                              suit_endyr = c(2019, 2018, 2008, 2019),
                                              initMode = "NonEquilibrium",
                                              niter = 3,
                                              random_rec = FALSE,
                                              fit_control = fit_control(
                                                loopnum = 5,
                                                phase = TRUE,
                                                verbose = 1))

summary(run_ms_CSL_Mest_prior_DM_CSL_stable) #2421.14
run_ms_CSL_Mest_prior_DM_CSL_stable$convergence

# MSE ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Single species harvest control rules ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# -- Constant F as a percentage of SB0

ss_run_DM_hcr_B0 <- fit_mod(data_list = CSL_SBF_ATF_hakedata_DM,
                             inits = NULL,
                             compFun = compFun,
                             estimateMode = "Estimate",
                             msmMode = "SingleSpecies",
                             random_rec = FALSE,
                             initMode = "NonEquilibrium",
                             HCR = build_hcr(HCR = 3, # Constant F HCR
                                             Ftarget = 0.4,
                                             DynamicHCR = FALSE),
                             fit_control = fit_control(phase = TRUE, verbose = 1))
summary(ss_run_DM_hcr_B0)

# Current management applied against multi-species model ----
mse1_CSL <- run_mse(om = run_ms_CSL_Mest_prior_DM_CSL_stable,
                em = ss_run_DM_hcr_CSL,
                nsim = 2, cores = 2,
                assessment_period = 1,
                sampling_period = c(1, 2), # Fishery samples yearly, survey every other year

                simulate_data = TRUE,
                sample_rec = TRUE
)

mse_summary(mse1_CSL)
plot_biomass(list(mse1_CSL$Sim_1$OM, run_ms_CSL_Mest_prior_DM_CSL_stable), species =1)
