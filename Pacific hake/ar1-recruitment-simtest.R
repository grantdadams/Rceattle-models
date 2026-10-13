########################################################################
## Pacific hake multispecies model: with a Rogers-convention state-space (QAR1)
## linkage on the BASE RECRUITMENT PARAMETER R0, plus a simulation test of it.
##
## Rogers et al. (2024) wrote catchability as a latent AR(1) state that is
## fit to an environmental index: log q_y = beta * x_y, where x is an
## AR(1) random effect fit to an index with a known observation sd. Rceattle
## expresses that as a linkage, `ar1(1 | Year)` plus `observe = `/`obs_sd = `.
## This example script uses it for R0, the mean-recruitment level:
##
##   log R_y = log R0 + beta * x_y + rec_dev_y,     x ~ AR1(rho, sigma)
##   x_y observed as env_data$temp_anom, with SD "obs_sd" (known)
##
## i.e. WHAM's Ecov-on-recruitment, in the Rogers state-space form.
##
## Model: the three-species hake / arrowtooth / sablefish predation model.
## Hake is the only estimated species (estDynamics = 0), the two predators are fixed
## numbers-at-age.
########################################################################

library(Rceattle)   # remotes::install_github("afsc-assessments/Rceattle")
library(dplyr)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1. Data ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
hake <- read_data(file = "Data/300426_SBF_ATF_Hake_Final.xlsx")
hake$index_data <- hake$index_data %>% dplyr::select(-Q_block)
hake$projyr <- 2025 # Setting projection year to 2025, to minimize model array size and decrease runtime

# Dirichlet-multinomial for the composition and diet data, with the N(0, 2)
# prior on each log-scale weight that keeps them identifiable (04-mse.R, note 2).
hake$fleet_control$Comp_distribution <- "DirichletMultinomial"
hake$Diet_distribution <- rep(1, hake$nspp)
compFun <- build_composition(linkages = list(
    theta_comp = linkage_spec(formula = ~ 1, by = ~ fleet,
                              fleet = hake$fleet_control$Fleet_code,
                              priors = list(`(Intercept)` = prior_lognormal(0, 2))),
    theta_diet = linkage_spec(formula = ~ 1, by = ~ species,
                              species = seq_len(hake$nspp),
                              priors = list(`(Intercept)` = prior_lognormal(0, 2)))))

# As example covariate, I am using the temperature anomaly in degrees C in "env_data".
# Mean 0 is required because Rceattle doesn't estimate a mean.
hake$env_data$temp_anom <- hake$env_data$Hake - mean(hake$env_data$Hake)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2. Base fits  ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# - single-species model
ss_run <- fit_mod(data_list = hake,
                  compFun = compFun,
                  estimateMode = "Estimate",
                  msmMode = "SingleSpecies",
                  random_rec = FALSE, # Don't run laplace approximation
                  initMode = "NonEquilibrium",
                  fit_control = fit_control(phase = TRUE, verbose = 1))
summary(ss_run)

# - cannibalism only model
cannibalism_model <- fit_mod(
    data_list = hake,
    inits = ss_run$estimated_params, # Starting estimation from single-species MLEs for ease of optimization
    compFun    = compFun,
    M1Fun      = build_M1(M1_model = "sex_age_invariant"),
    msmMode    = "MSVPA",
    suitMode   = "Empirical",
    niter      = 3,
    random_rec = FALSE,
    suit_styr  = c(1980, 1980, 1980),
    suit_endyr = c(2019, 2019, 2019),
    initMode   = "NonEquilibrium",
    estimateMode = "Estimate",
    fit_control = fit_control(phase = FALSE, verbose = 1))
summary(cannibalism_model)

# - cannibalism + predation from sablefish/arrowtooth model
# using the cannibalism_model$map to keep prey size-preference parameters (gam_a / gam_b) fixed.
inits <- cannibalism_model$estimated_params
map   <- cannibalism_model$map
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

# Turn on diet weight-parameters (previous model had them off)
map$mapList$diet_comp_weights[2:3] <- 2:3
map$mapFactor$diet_comp_weights <- factor(map$mapList$diet_comp_weights)

# Models 3 and 4 refit this same specification.
ms4_args <- list(
    compFun    = compFun,
    M1Fun      = build_M1(M1_model = "sex_age_invariant",
                          M1_use_prior = TRUE,
                          M_prior = 0.2,
                          M_prior_sd = 0.1),
    msmMode    = "MSVPA",
    suitMode   = c("Empirical", "LognormalWeight", "LognormalWeight"),
    suit_styr  = c(1980, 2013, 2005),   # hake, arrowtooth, sablefish years where diet data were available
    suit_endyr = c(2019, 2018, 2008),
    niter      = 3,
    random_rec = FALSE,
    initMode   = "NonEquilibrium")

predation_model <- do.call(fit_mod, c(ms4_args, list(
    data_list = hake,
    inits = inits,
    map = map,
    estimateMode = "Estimate",
    fit_control = fit_control(loopnum = 5, phase = TRUE, verbose = 1))))
summary(predation_model)
predation_model$quantities$vulnerability  # arrowtooth->hake, sablefish->hake

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3. The AR1 linkage on R0 ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Assuming no stock recruit-curve by setting srr_fun = "mean".
# For a Beverton-Holt or Ricker curve you'd relate the covariate to alpha or beta.
#
# "obs_sd" is the time series's observation SD and is assumed known (similar to WHAM).
# "sigma" is the marginal sd determining the variance of the random effect.
# Both are in the covariate's units, and the model implies var(temp_anom) = sigma^2 + obs_sd^2.
SIGMA_LATENT <- 0.1
OBS_SD       <- 0.1

recFun_ar1 <- build_srr(
    srr_fun  = "mean",
    linkages = list(
        R0 = linkage_spec(~ ar1(1 | Year),
                          species = "Hake",
                          observe = "temp_anom", # Time series to fit to
                          obs_sd  = OBS_SD,
                          obs_sd_est = FALSE, # Assume known measurement error
                          init    = list(sigma = SIGMA_LATENT) # To estimate sigma, delete the `init` line
                          # priors = list(sigma = prior_lognormal(log(0.1), 0.5)) # Can use priors instead
                          )))

# The number of parameters increases by 41, so the the previous model's MLEs/map cannot be
# passed as `inits` and `map` directly. Build the skeleton once without optimizing and
# copy the previous model's estimates into the new parameter object.
skel <- do.call(fit_mod,
                c(ms_args, list(
                    data_list = hake,
                    inits = NULL,
                    recFun = recFun_ar1,
                    estimateMode = "DebugBuild", # Don't optimize
                    fit_control = fit_control(phase = FALSE, verbose = 0))))
LINK_BLOCKS <- grep("linkage", names(skel$estimated_params), value = TRUE)

inits_qar1 <- skel$estimated_params
.carry <- setdiff(names(inits_qar1), LINK_BLOCKS)
inits_qar1[.carry] <- predation_model$estimated_params[.carry]

# Same log_phi / diet_comp_weights edits as section 2, on the resized map.
map_qar1 <- skel$map
map_qar1$mapList$log_phi[] <- seq_len(length(map_qar1$mapList$log_phi))
map_qar1$mapList$log_phi[1, 1] <- map_qar1$mapList$log_phi[1, 2] <- map_qar1$mapList$log_phi[2, 2] <- NA
map_qar1$mapList$log_phi[1, 3] <- map_qar1$mapList$log_phi[3, 3] <- NA
map_qar1$mapList$log_phi[2, 3] <- map_qar1$mapList$log_phi[3, 2] <- NA
map_qar1$mapFactor$log_phi <- factor(map_qar1$mapList$log_phi)
map_qar1$mapList$diet_comp_weights[2:3] <- 2:3
map_qar1$mapFactor$diet_comp_weights <- factor(map_qar1$mapList$diet_comp_weights)

# Refit model with initial starting values.
qar1_run <- do.call(fit_mod,
                    c(ms4_args, list(
                        data_list = hake,
                        inits = inits_qar1,
                        map = map_qar1,
                        recFun = recFun_ar1,
                        estimateMode = "Estimate",
                        fit_control = fit_control(loopnum = 5, phase = FALSE, verbose = 1))))

# Look at model estimates
qar1_run$quantities$beta_linkage_obs                       # effect size
exp(qar1_run$estimated_params$log_sigma_linkage)           # fixed at 0.1
tanh(qar1_run$estimated_params$trans_rho_linkage)          # AR1 correlation
summary(qar1_run$sdrep, "report")["beta_linkage_obs", ]    # effect size +- SE
qar1_run$quantities$jnll_comp["Linkage random effects", 1] # AR1 + observation log-likelihood

# R0 is now year-varying: log R0_y = rec_pars[1, 1] + beta * x_y.
plot(hake$env_data$Year, qar1_run$quantities$R0[1, seq_len(nrow(hake$env_data))],
     type = "l", xlab = "Year", ylab = "Hake R0 (thousands of recruits)")

# Compare differences in recruitment and SSB
plot_recruitment(list(predation_model, qar1_run),
                 model_names = c("Base", "QAR1 on R0"), species = 1)
plot_ssb(list(predation_model, qar1_run),
         model_names = c("Base", "QAR1 on R0"), species = 1)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 4. Simulation test ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# self_test() cannot currently do this one. sim_mod() does not simulate an observed-AR1 latent covariate.
#
# Per replicate:
#   1. draw a latent AR1 covariate with known rho and sigma,
#   2. set the observed covariate to x plus measurement error,
#   3. build an OM at a known beta and x,
#   4. draw data and fresh recruitment deviations from that OM,
#   5. fit the EM,
#   6. record beta_hat and whether its 95% interval covers the truth.
#
N_SIM  <- 20
RHO_T  <- 0.5
BETA_T <- 5

n_re <- nrow(hake$env_data)

# A stationary AR1 realization with marginal SD
ar1_draw <- function(n, rho, sigma) {
    x <- numeric(n)
    x[1] <- stats::rnorm(1, 0, sigma)
    for (t in seq_len(n - 1)) {
        x[t + 1] <- rho * x[t] + stats::rnorm(1, 0, sigma * sqrt(1 - rho^2))
    }
    return(x)
}

sim_one <- function(i) {
    set.seed(1000 + i)
    x <- ar1_draw(n_re, RHO_T, SIGMA_LATENT)

    dat_om <- hake
    dat_om$env_data$temp_anom <- x + stats::rnorm(n_re, 0, OBS_SD) # Add observation error

    inits_om <- inits_qar1
    inits_om$beta_linkage_re    <- x
    inits_om$beta_linkage_obs   <- BETA_T
    inits_om$log_sigma_linkage  <- log(SIGMA_LATENT)
    inits_om$trans_rho_linkage  <- atanh(RHO_T)
    inits_om$log_obs_sd_linkage <- log(OBS_SD)

    # Pin the latent. With every beta_linkage_re mapped out, fit_mod() leaves it
    # off the `random` list altogether, so the OM evaluates at exactly the drawn
    # state instead of the inner Laplace step's.
    map_om <- map_qar1
    map_om$mapList$beta_linkage_re[] <- NA
    map_om$mapFactor$beta_linkage_re <- factor(map_om$mapList$beta_linkage_re)

    om <- do.call(fit_mod, c(ms4_args, list(
        data_list = dat_om, inits = inits_om, map = map_om, recFun = recFun_ar1,
        estimateMode = "DebugBuild",
        fit_control = fit_control(phase = FALSE, verbose = 0, getsd = FALSE))))
    stopifnot(isTRUE(all.equal(as.numeric(om$quantities$beta_linkage_re), x)))

    # process = "recruitment" redraws the annual deviations
    dat_sim <- sim_mod(om, simulate = TRUE, process = "recruitment")
    rec_dev_om <- attr(dat_sim, "process_sim")$rec_dev[1, seq_len(n_re)]

    # The estimating model, latent free again and beta started at no effect
    inits_em <- inits_om
    inits_em$beta_linkage_re  <- rep(0, n_re)
    inits_em$beta_linkage_obs <- 0

    em <- do.call(fit_mod, c(ms4_args, list(
        data_list = dat_sim, inits = inits_em, map = map_qar1,
        recFun = recFun_ar1,
        estimateMode = "Hindcast",
        fit_control = fit_control(loopnum = 5, phase = FALSE, verbose = 0))))

    se <- NA_real_
    if (!is.null(em$sdrep)) {
        sdr <- summary(em$sdrep, "report")
        if ("beta_linkage_obs" %in% rownames(sdr)) {
            se <- sdr[rownames(sdr) == "beta_linkage_obs", 2][1]
        }
    }
    bh <- as.numeric(em$quantities$beta_linkage_obs)
    data.frame(
        sim        = i,
        beta_true  = BETA_T,
        beta_hat   = bh,
        se         = se,
        covers     = !is.na(se) & abs(bh - BETA_T) < 1.96 * se,
        sigma_hat  = exp(em$estimated_params$log_sigma_linkage),
        rho_hat    = tanh(em$estimated_params$trans_rho_linkage),
        # Regressing the log-recruitment the OM actually generated on the latent:
        # the signal in the data, whatever the estimator does with it.
        ols_slope  = unname(stats::coef(stats::lm(
            I(BETA_T * x + rec_dev_om) ~ 0 + x))[1]),
        status     = em$convergence$status,
        objective  = em$opt$objective)
}

sims <- do.call(rbind, lapply(seq_len(N_SIM), function(i)
    tryCatch(sim_one(i), error = function(e) {
        message("sim ", i, " failed: ", conditionMessage(e)); NULL
    })))

print(sims)
with(sims, c(n          = nrow(sims),
             beta_true  = BETA_T,
             mean_hat   = mean(beta_hat),
             median_hat = median(beta_hat),
             bias       = mean(beta_hat) - BETA_T,
             rmse       = sqrt(mean((beta_hat - BETA_T)^2)),
             coverage   = mean(covers),
             mean_ols   = mean(ols_slope)))

hist(sims$beta_hat, breaks = 12,
     xlab = expression(hat(beta)), main = "QAR1 effect size on R0")
abline(v = BETA_T, col = 2, lwd = 2)
