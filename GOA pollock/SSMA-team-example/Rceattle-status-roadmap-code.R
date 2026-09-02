# =============================================================================
# Rceattle: Status, Applications, and Roadmap
# Follow-along R script for the May 2026 presentation
# =============================================================================


# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
# https://github.com/grantdadams/Rceattle
install.packages("pak")
pak::pkg_install("grantdadams/Rceattle")
library(Rceattle)


# -----------------------------------------------------------------------------
# Slide: Workflow (data)
# -----------------------------------------------------------------------------
pk_data <- read_data("Pollock_2023.xlsx")
pk_data$fleet_control$Proj_F_proportion <- 1
plot_data(pk_data, subplots = 2)


# -----------------------------------------------------------------------------
# Slide: Model fitting
# -----------------------------------------------------------------------------
pk_model <- fit_mod(
  data_list = pk_data,
  estimateMode = 0,
  initMode = "Equilibrium",
  fit_control = fit_control(phase = TRUE,
                            verbose = 0))
summary(pk_model)


# -----------------------------------------------------------------------------
# Slide: Workflow & Output
# -----------------------------------------------------------------------------
summary(pk_model)       # compact summary
AIC(pk_model)           # logLik() with df — just works
vcov(pk_model)          # parameter covariance (sdreport)
residuals(pk_model)     # tidy residuals across data
as.data.frame(pk_model) # derived quantities as a frame
plot(pk_model, what = "ssb") # plot dispatcher


# -----------------------------------------------------------------------------
# Slide: Diagnostics — Jitter, Profiles, Self-test, & Retrospective
# -----------------------------------------------------------------------------
# - Jitters
jitters <- jitter(pk_model, njitter = 10, sd = 0.2)
hist(jitters$nll)
plot_biomass(jitters$Rceattle_list)

# - Profiles
profiles <- profile(
  fitted   = pk_model,
  param    = "sigmaR",
  values   = list(seq(0.5, 1.5, by = 0.1))
)
plot(y = profiles$nll, x = profiles$grid$slot_1)
plot_biomass(profiles$Rceattle_list)

# - Retros
retros <- retrospective(pk_model, peels = 5)
plot_biomass(retros$Rceattle_list)
retros$mohns # Get Mohn's rho (includes forecast)

# - Self test
self_tests <- self_test(pk_model, nsim = 20)
plot_biomass(self_tests)


# -----------------------------------------------------------------------------
# Slide: Diagnostics — Model summaries and plots
# -----------------------------------------------------------------------------
as.data.frame(pk_model)

# - Time series plots
plot_biomass(pk_model)
plot_depletion(pk_model)
plot_selectivity(pk_model)
plot_recruitment(pk_model)
plot_ssb(pk_model)
plot_depletionSSB(pk_model)

# - Diagnostic plots
plot_catch(pk_model)
plot_index(pk_model)
plot_indexresidual(pk_model)
plot_comp(pk_model)


# -----------------------------------------------------------------------------
# Slide: Closed-Loop MSE in Action
# -----------------------------------------------------------------------------
pk_data$fleet_control$Proj_F_proportion <- 1 # One fishing fleet, so F is apportioned all to that

# - Build EM-HCR
em_model <- fit_mod(
  data_list = pk_data,
  estimateMode = 0,
  initMode = "Equilibrium",
  HCR = build_hcr(HCR = "NPFMC",
                  Ftarget = 0.4, # F40% - SPR
                  Flimit  = 0.35, # F35% - SPR
                  Plimit  = 0.2,  # No fishing when SB<SB20
                  Alpha   = 0.05),
  fit_control = fit_control(phase = TRUE,
                            verbose = 0))

# - Run MSE
mse1 <- run_mse(
  om = pk_model, em = em_model,
  nsim = 10,
  # Assessment every other year
  assessment_period = 2,
  # 8 fleets with different sampling frequency
  sampling_period = c(1, 2, 1, 2, 2, 2, 1, 1) # pk_model$data_list$fleet_control$Fleet_name
)

# - Get MSE summary
summ <- mse_summary(mse1)

# - Plot OM vs same OM with 0 fishing
plot_depletionSSB(list(mse1$Sim_1$OM,
                       mse1$Sim_1$OM_no_F),
                  model_names = c("OM: Tier-3 applied", "OM: No Fishing"))


# -----------------------------------------------------------------------------
# Slide: Research Flexibility  (not evaluated in the deck — illustrative)
# -----------------------------------------------------------------------------
# - Create random env variable
yrs <- pk_data$styr:pk_data$projyr
nyrs <- length(yrs)

pk_data$env_data <- data.frame(
  Year = yrs,
  EnvVar1 = rnorm(nyrs), # Random variation
  EnvTrend1 = scale(1:nyrs) * 0.25 + rnorm(nyrs, 0, 0.1), # Warming
  EnvTrend2 = scale(1:nyrs) * -0.1 + rnorm(nyrs, 0, 0.3) # Decreasing trand
)


pk_climate_model <- fit_mod(
  data_list = pk_data,
  estimateMode = 0,
  initMode = "Equilibrium",
  M1Fun = build_M1(
    M1_model = "sex_age_invariant",
    linkages = list(
      M1 = linkage_spec(
        formula = ~ 1 + EnvVar1
      ))),
  recFun = build_srr(
    srr_fun = "mean",
    srr_pred_fun = "Ricker",
    proj_mean_rec = FALSE,
    linkages = list(
      alpha = linkage_spec(
        formula = ~ 1 + EnvTrend1 + EnvTrend2,
        priors = list(
          "EnvTrend1" = normal(0.25, 0.05),
          "EnvTrend2" = normal(-0.1, 0.05)
        )))),
  fit_control = fit_control(phase = TRUE,
                            verbose = 0))

# - Plots
summary(pk_climate_model)
plot_stock_recruit(pk_climate_model)

# - See trend
plot_biomass(pk_climate_model, incl_proj = TRUE)
plot_recruitment(pk_climate_model, incl_proj = TRUE)

# - MSE
mse2 <- run_mse(
  om = pk_climate_model, em = em_model,
  nsim = 10,
  # Assessment every other year
  assessment_period = 2,
  # 8 fleets with different sampling frequency
  sampling_period = c(1, 2, 1, 2, 2, 2, 1, 1) # pk_model$data_list$fleet_control$Fleet_name
)

# - Get MSE summary
summ2 <- mse_summary(mse2)

# - Plot OM vs same OM with 0 fishing
plot_depletionSSB(list(mse1$Sim_1$OM,
                       mse1$Sim_1$OM_no_F),
                  model_names = c("OM: Tier-3 applied", "OM: No Fishing"))



# -----------------------------------------------------------------------------
# Slide: DSEM integration  (not evaluated in the deck — illustrative)
# -----------------------------------------------------------------------------
# sem_iid = "
#   # link, lag, param_name, start_value
#   ScaledBT -> ScaledBT, 1, AR_BT, 0
#   ScaledBT -> recdevs1, 1, BT_to_R, 0
#   recdevs1 <-> recdevs1, 0, sigmaR1, 1
# "
#
# dsem_model <- fit_mod(
#   data_list = pk_data,
#   estimateMode = 0,
#   initMode = "Equilibrium",
#   dsem = build_DSEM(
#     sem = sem_iid,
#     family = "normal"
#   ),
#   random_rec = TRUE, # Random recruitment
#   fit_control = fit_control(phase = TRUE,
#                             verbose = 0))
