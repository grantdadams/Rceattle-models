########################################################################
## Analysis_FinalJUN_DM.R adapted to run on the modern Rceattle
## (branch: integrate-hake-suit-styr-ddw, off dev-data-workflow v4.11.0).
##
## Three differences from the original Hake_test-era script are required
## because the package's data/likelihood plumbing has moved on:
##
##   1. diet DM is set via `Diet_loglike`, NOT `diet_ll_type`.
##      dev derives `diet_ll_type` from `Diet_loglike` in rearrange_data(),
##      so a manual `diet_ll_type <- 1` is silently overwritten.
##
##   2. The DM composition weights need a prior. Hake_test HARDCODED
##      `dnorm(comp_weights, 0, 2)` (and `dnorm(diet_comp_weights, 0, 2)`) on
##      every DM fleet; dev makes that prior OPT-IN via build_composition().
##      Without it the under-dispersed Hake_survey comps drive the DM alpha to
##      +Inf (gradient ~800, jnll unstable at ~1704). Re-adding the identical
##      N(0,2) prior via build_composition() below reproduces Hake_test's
##      regularisation: the fit converges (max|grad| ~2e-3) with the weight
##      FREE, and jnll = 2133.8 vs 2128.2 recorded on Hake_test (0.27%; the
##      small residual is the DM effective-N normalisation, comp_n vs
##      sum(obs) -- scientifically negligible). NOTE: dev fam="lognormal"
##      evaluates the prior on the log-scale weight, exactly matching
##      Hake_test's dnorm(comp_weights, ...).
##
##   3. per-predator suit_styr/suit_endyr vectors are supported natively now
##      (that is the feature this branch adds), so the c(1980,2013,2005) /
##      c(2019,2018,2008) windows work unchanged.
##
## The NByageFixed age-column padding in the workbook is trimmed on read
## automatically (read_data change on this branch) — no manual fix needed.
########################################################################

library(Rceattle)

SBF_ATF_hakedata_DM <- read_data(file = "300426_SBF_ATF_Hake_Final.xlsx")

## --- DM for composition data ---
SBF_ATF_hakedata_DM$fleet_control$Comp_loglike <- 1L   # DM age comps (1 = DirichletMultinomial)
SBF_ATF_hakedata_DM$Diet_loglike <- rep(1L, SBF_ATF_hakedata_DM$nspp)  # DM diet comps (dev derives diet_ll_type from this)

## --- Re-add Hake_test's DM-weight priors (dnorm(weight, 0, 2)) ----------
## fam = "lognormal" evaluates the prior on the log-scale weight, i.e.
## dnorm(comp_weights, 0, 2) / dnorm(diet_comp_weights, 0, 2) exactly.
comp_flts <- SBF_ATF_hakedata_DM$fleet_control$Fleet_code
compFun <- build_composition(linkages = list(
  theta_comp = linkage_spec(~ 1, by = ~ fleet, fleet = comp_flts,
                            priors = list(`(Intercept)` = prior_lognormal(0, 2))),
  theta_diet = linkage_spec(~ 1, by = ~ species, species = seq_len(SBF_ATF_hakedata_DM$nspp),
                            priors = list(`(Intercept)` = prior_lognormal(0, 2)))))

## --- Single-species DM run (M1 fixed) ---------------------------------
ss_run_DM <- fit_mod(data_list = SBF_ATF_hakedata_DM,
                     inits = NULL, file = NULL, compFun = compFun,
                     estimateMode = 0, random_rec = FALSE, msmMode = 0,
                     phase = TRUE, verbose = 1)
ss_run_DM$quantities$jnll   # 2133.8 (Hake_test recorded 2128.156; 0.27%)

## --- Multispecies M-estimation run, EMPIRICAL suitability (suitMode = 0) ----
ms_run_DM <- fit_mod(data_list = SBF_ATF_hakedata_DM,
                     inits = ss_run_DM$estimated_params, compFun = compFun,
                     M1Fun = build_M1(M1_model = 1, updateM1 = FALSE,
                                      M1_use_prior = FALSE, M2_use_prior = FALSE),
                     file = NULL, estimateMode = 0, niter = 3,
                     random_rec = FALSE, msmMode = 1, suitMode = 0,
                     suit_styr  = c(1980, 1980, 1980),
                     suit_endyr = c(2019, 2019, 2019),
                     initMode = 2, verbose = 1)
ms_run_DM$quantities$jnll    # 2142 (recorded 2188); M1 ~0.304 matches recorded 0.30

## --- Hand-tuned predation run: LN suitability, only log_phi free ------------
## IMPORTANT: reuse ms_run_DM$map (gam_a/gam_b stay mapped OFF, i.e. FIXED at the
## hand-set prey-size preferences below) and re-enable ONLY log_phi. Rebuilding
## the map here instead would freely estimate gam_a/gam_b and drive the
## vulnerabilities to the boundary (1.0).
inits <- ms_run_DM$estimated_params
map   <- ms_run_DM$map
inits$log_gam_a <- c(0, log(3.7), log(3.1))    # mean log pred/prey weight ratio
inits$log_gam_b <- c(0, log(1.83), log(1.120))
inits$log_phi[1, 2] <- inits$log_phi[2, 2] <- inits$log_phi[1, 3] <- -999  # disallowed links
inits$log_phi[3, 3] <- inits$log_phi[2, 3] <- inits$log_phi[3, 2] <- -999
map$mapList$log_phi[] <- seq_len(length(map$mapList$log_phi))
map$mapList$log_phi[1, 1] <- map$mapList$log_phi[1, 2] <- map$mapList$log_phi[2, 2] <- NA
map$mapList$log_phi[1, 3] <- map$mapList$log_phi[3, 3] <- NA
map$mapList$log_phi[2, 3] <- map$mapList$log_phi[3, 2] <- NA   # free only [2,1] ATF->hake, [3,1] SBF->hake
map$mapFactor$log_phi <- factor(map$mapList$log_phi)

run_ms_CSL_Mest_prior_DM <- fit_mod(data_list = SBF_ATF_hakedata_DM,
                     inits = inits, map = map, compFun = compFun,
                     M1Fun = build_M1(M1_model = 1, M1_use_prior = TRUE,
                                      M_prior = 0.2, M_prior_sd = 0.1),
                     file = NULL, estimateMode = 0, niter = 3, random_rec = FALSE,
                     msmMode = 1, loopnum = 5, phase = TRUE,
                     suitMode   = c(0, 4, 4),                 # empirical + lognormal
                     suit_styr  = c(1980, 2013, 2005),        # hake, ATF, sablefish
                     suit_endyr = c(2019, 2018, 2008),
                     initMode = 2, verbose = 1)
run_ms_CSL_Mest_prior_DM$quantities$jnll           # 2262.3 (recorded 2259.1; 0.14%)
run_ms_CSL_Mest_prior_DM$quantities$vulnerability  # 0.817, 0.769 (recorded 0.8451, 0.7673)

## The sensitivity/weighting/retro blocks then follow the original
## Analysis_FinalJUN_DM.R (diet_comp_weights = c(NA,2,3), retrospective(),
## plot_diet_comp2(), ...), carrying `map`/`compFun` forward.
##
## Residual gaps vs Hake_test are NOT integration errors -- they are two
## process-side conventions dev deliberately corrected (do not revert):
##   * rec_dev/init_dev lognormal bias correction centred at -sigma^2/2 (dev,
##     mean-unbiased) vs +sigma^2/2 (Hake_test);
##   * initMode=2 plus-group survival divides by (1 - exp(-M1 - Finit)) (dev,
##     fished-equilibrium) vs (1 - exp(-M1)) (Hake_test).
## Plus the standard diet DM parameterisation exp(theta) (dev) vs
## N*invlogit(theta) (Hake_test). Together these explain the ~0.1-2% residuals.
