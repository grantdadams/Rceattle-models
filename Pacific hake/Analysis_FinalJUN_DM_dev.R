########################################################################
## Pacific hake multispecies predation model: hake + arrowtooth (ATF) +
## sablefish (SBF), Dirichlet-multinomial (DM) composition data.
##
## Hake is the focal, fully-estimated stock; arrowtooth and sablefish enter as
## predators whose numbers-at-age are fixed inputs and who impose predation
## mortality on hake. The fit needs three things set up before it will run:
##
##   1. DM likelihoods for the age-composition and diet-composition data
##      (`Comp_distribution` and `Diet_distribution`).
##
##   2. A weakly-informative prior on the DM overdispersion weights. The Hake
##      survey age-comps are under-dispersed relative to a multinomial, so
##      without a prior the DM weight is unidentifiable and runs to the
##      multinomial limit and the fit will not converge. A N(0, 2) prior on the
##      log-scale weight, added with build_composition(), keeps it identifiable.
##
##   3. A separate suitability-averaging window per predator (hake 1980-2019,
##      arrowtooth 2013-2018, sablefish 2005-2008), passed as the per-predator
##      vectors suit_styr / suit_endyr.
##
## The fixed numbers-at-age sheet in the workbook is padded with empty age
## columns; read_data() trims these on read, so no manual fix is needed.
# ---------------------------------------------------------------------------
# The fit is close to, but not identical to, the original hake_test model. The
# differences (~0.1-2% in total likelihood) come from improvements:
#   * recruitment and initial-age deviations are mean-unbiased (centred so mean
#     recruitment equals R0), rather than centered wrong (previous bug)
#   * DM parameterization moved to standard Thorson setup.
# ---------------------------------------------------------------------------

# Load data ----
library(Rceattle)
SBF_ATF_hakedata_DM <- read_data(file = "300426_SBF_ATF_Hake_Final.xlsx")

# Dirichlet-multinomial for the composition data ----
SBF_ATF_hakedata_DM$fleet_control$Comp_distribution <- "DirichletMultinomial"  # age-composition
SBF_ATF_hakedata_DM$Diet_distribution <- rep(1L, SBF_ATF_hakedata_DM$nspp)     # diet (1 = DirichletMultinomial)

# Prior on DM weights ----
# A N(0, 2) prior on the log-scale weight for every fleet's age-comps and every
# predator's diet keeps the DM weights identifiable (see setup note 2 above).
comp_flts <- SBF_ATF_hakedata_DM$fleet_control$Fleet_code
compFun <- build_composition(linkages = list(
  theta_comp = linkage_spec(~ 1, by = ~ fleet,
                            fleet   = comp_flts,
                            priors = list(`(Intercept)` = prior_lognormal(0, 2))),
  theta_diet = linkage_spec(~ 1, by = ~ species,
                            species = seq_len(SBF_ATF_hakedata_DM$nspp),
                            priors = list(`(Intercept)` = prior_lognormal(0, 2)))))

# 1. Single-species: no future F ----
ss_run_DM <- fit_mod(data_list = SBF_ATF_hakedata_DM,
                     inits = NULL,
                     compFun = compFun,
                     estimateMode = "Estimate",
                     msmMode = "SingleSpecies",
                     random_rec = FALSE,
                     initMode = "NonEquilibrium",
                     fit_control = fit_control(phase = TRUE, verbose = 1))
summary(ss_run_DM)   # 2139 (original 2128)

# 2. Single-species: category 1 HCR ----
ss_run_DM_hcr <- fit_mod(data_list = SBF_ATF_hakedata_DM,
                     inits = NULL,
                     compFun = compFun,
                     estimateMode = "Estimate",
                     msmMode = "SingleSpecies",
                     random_rec = FALSE,
                     initMode = "NonEquilibrium",
                     HCR = build_hcr(HCR = 6, # Cat 1 HCR
                                     Flimit = c(0.45, 0.45,  0.3), # F45%
                                     Ptarget = c(0.4, 0.4, 0.25), # Target is 40% B0
                                     Plimit = c(0.1, 0.1, 0.05), # No fishing when SB<SB10
                                     Pstar = 0.45,
                                     Sigma = 0.5),
                     fit_control = fit_control(phase = TRUE, verbose = 1))
summary(ss_run_DM_hcr)

# 3. MSVPA with estimated M ----
ms_run_DM <- fit_mod(data_list = SBF_ATF_hakedata_DM,
                     inits = ss_run_DM$estimated_params,
                     compFun = compFun,
                     M1Fun = build_M1(M1_model = "sex_age_invariant"),  # estimate M without prior
                     estimateMode = "Estimate",
                     msmMode = "MSVPA",
                     suitMode = "Empirical",
                     niter = 3,
                     random_rec = FALSE,
                     suit_styr  = c(1980, 1980, 1980),
                     suit_endyr = c(2019, 2019, 2019),
                     initMode = "NonEquilibrium",
                     fit_control = fit_control(phase = FALSE, verbose = 1))
summary(ms_run_DM)   # 2142 (original 2188); estimated M ~0.30

# 4. Estimated suitability ----
# Prey-size preference (gam_a / gam_b) are fixed; only the
# predator-prey vulnerabilities are estimated, and only for the two interaction that
# exist (arrowtooth eating hake, sablefish eating hake). Every other link is fixed to
# very small value (i.e. "predator does not eat this prey").
#
# Reusing ms_run_DM$map to keep gam_a / gam_b fixed.
inits <- ms_run_DM$estimated_params
map   <- ms_run_DM$map
inits$log_gam_a <- c(0, log(3.7), log(3.1))    # mean predator/prey weight ratio
inits$log_gam_b <- c(0, log(1.83), log(1.120))

# log_phi[predator, prey]; -999 = predator does not eat this prey
inits$log_phi[1, 2] <- inits$log_phi[2, 2] <- inits$log_phi[1, 3] <- -999
inits$log_phi[3, 3] <- inits$log_phi[2, 3] <- inits$log_phi[3, 2] <- -999

# Estimate only [2,1] arrowtooth->hake and [3,1] sablefish->hake; fix the rest.
map$mapList$log_phi[] <- seq_len(length(map$mapList$log_phi))
map$mapList$log_phi[1, 1] <- map$mapList$log_phi[1, 2] <- map$mapList$log_phi[2, 2] <- NA
map$mapList$log_phi[1, 3] <- map$mapList$log_phi[3, 3] <- NA
map$mapList$log_phi[2, 3] <- map$mapList$log_phi[3, 2] <- NA
map$mapFactor$log_phi <- factor(map$mapList$log_phi)

run_ms_CSL_Mest_prior_DM <- fit_mod(data_list = SBF_ATF_hakedata_DM,
                     inits = inits,
                     map = map,
                     compFun = compFun,
                     M1Fun = build_M1(M1_model = "sex_age_invariant",
                                      M1_use_prior = TRUE,
                                      M_prior = 0.2,
                                      M_prior_sd = 0.1),
                     estimateMode = "Estimate",
                     msmMode = "TypeIIMSVPA",
                     suitMode = c("Empirical", "LognormalWeight", "LognormalWeight"),
                     suit_styr  = c(1980, 2013, 2005),   # hake, arrowtooth, sablefish
                     suit_endyr = c(2019, 2018, 2008),
                     initMode = "NonEquilibrium",
                     niter = 3,
                     random_rec = FALSE,
                     fit_control = fit_control(
                         loopnum = 5,
                         phase = TRUE,
                         verbose = 1))
summary(run_ms_CSL_Mest_prior_DM)           # 2262 (original 2259)
run_ms_CSL_Mest_prior_DM$quantities$vulnerability  # arrowtooth->hake 0.82, sablefish->hake 0.77
                                                   #   (original 0.85, 0.77)



# MSE ----
# Current management applied against multi-species model ----
mse1 <- run_mse(om = run_ms_CSL_Mest_prior_DM,
                em = ss_run_DM_hcr,
                nsim = 10, cores = 1,
                assessment_period = 1,
                sampling_period = c(1, 2), # Fishery samples yearly, survey every other year

                simulate_data = TRUE,
                sample_rec = TRUE

                )

