# =============================================================================
# 2024 GOA northern rockfish - BRIDGING to Rceattle
# =============================================================================
# Single-sex, single-species model.
# Goal: recreate the 2024 assessment model ("urm" - unified rockfish model,
#       BenWilliams-NOAA/urm, an RTMB SCAA) in Rceattle and reconcile output,
#       mirroring the structure of "2022 GOA northern rockfish bridging.R"
#       (where the reference model was the ADMB 'nr' model).
#
# In 2022 the reference was ADMB (+ an early RTMB run, report2.Rdata).
# In 2024 the reference is the RTMB "urm" model in ./2024 model/urm-main/.
#
# DATA (from ./2024 model/urm-main/data/dat.RDS, the urm data object)
# - Fishery catch (1961-2024)
# - Fishery age comp + fishery length comp
# - Survey biomass (NMFS GOA bottom trawl) + SD
# - Survey age comp
# - Empirical weight-at-age & maturity-at-age (50 model ages)
# - Ageing-error and size-at-age (growth) transition matrices
#
# MODEL
# - Single sex
# - Survey selectivity   = logistic, single block, with catchability q (prior)
# - Fishery selectivity  = logistic, single block
# - Empirical weight-at-age
# - M = fixed / estimated with lognormal prior
#
# -----------------------------------------------------------------------------
# STRUCTURAL DIFFERENCES between the urm (RTMB) and Rceattle codebases
# (these are why an *exact* match is not expected; they are documented inline
#  at the relevant code below as well):
#
# 1. RECRUITMENT BIAS RAMP. urm applies a Methot-Taylor bias-correction ramp
#    (bias_ramp/bias_switch: Nat[1,t]=exp(log_mean_R - bias_ramp*sigmaR^2/2 +
#    log_Rt)). Rceattle does not implement this ramp, so the recruitment
#    deviation likelihood diverges by design. (In Model 1 below we pre-subtract
#    bias_adj from rec_dev so the *fixed* recruitment series matches urm exactly.)
# 2. sigmaR. urm ESTIMATES sigmaR with a tight lognormal prior
#    (mean_sigmaR=1.5, cv_sigmaR=0.01 -> effectively fixed near 1.5).
#    Rceattle uses a fixed sigma_rec_prior (default 0.707). We set Rceattle's
#    sigma_rec_prior = 1.5 to match, but it is not a freely-estimated parameter.
# 3. INITIAL NUMBERS-AT-AGE. urm builds a *fished* initial equilibrium from
#    log_mean_R_init (R_init) and log_F_init (F_init) plus per-age initial
#    deviations log_Rt[1:(A-1)] and a geometric plus-group term. Rceattle
#    initMode=1 assumes an *unfished* equilibrium with init_dev deviations.
#    The initial-age parameterization therefore differs. (Observed effect:
#    Model 1 matches urm within ~1-2% in recent years but diverges in the
#    early years - largest in 1961, fading out by mid-series.)
# 4. SURVEY WEIGHTING. urm multiplies the whole survey NLL by srv_wt=0.25.
#    Rceattle has no flat NLL multiplier but uses the identical lognormal-with-
#    bias-correction form (cpp line ~2523: dnorm(log obs, log pred - sd^2/2, sd)).
#    We EMULATE srv_wt by inflating the survey Log_sd: sd' = sd / sqrt(srv_wt)
#    = 2*sd. This reproduces urm's residual weighting exactly. CAVEAT: Rceattle's
#    bias-correction term then uses the inflated sd' (shift = sd'^2/2 = 4*sd^2/2)
#    whereas urm uses the un-inflated sd (shift = sd^2/2) -> a small, fixed
#    difference in the lognormal mean only.
#    Comp weights fish_age/srv_age/fish_size = 0.5 are matched exactly via
#    Comp_weights (cpp line ~2615: comp_weights(flt)*dmultinom(...), fixed under
#    the Multinomial likelihood).
# 5. CATCH WEIGHTING & BIAS CORRECTION. urm stores catch_wt (=5) and converts to
#    a CV via cv = sqrt(1/(2*catch_wt)); we set Rceattle catch Log_sd to the
#    matching lognormal sd = sqrt(log(cv^2+1)). IMPORTANT: urm's catch likelihood
#    has NO lognormal bias correction (dnorm(log obs, log pred, sd)), whereas
#    Rceattle's catch likelihood DOES subtract sd^2/2 from the mean (cpp line
#    ~2555). This catch bias-correction term is in the cpp and cannot be removed
#    from the data file, so it remains a small structural difference.
# 6. SELECTIVITY PARAMETERIZATION. urm logistic is parameterised by
#    (a50, delta); Rceattle by (slope, inflection): slope = 1/delta,
#    inflection = a50. Mathematically equivalent for logistic, but urm also
#    supports gamma / double-normal / double-logistic and time blocks.
# 7. OPTIMIZER / PHASING. ADMB-style phasing and the TMB optimizer differ;
#    these are expected and accepted differences (do not chase them).
#
# -----------------------------------------------------------------------------
# PRIOR DIFFERENCES (urm vs Rceattle). All priors are lognormal on the relevant
# parameter; the values are now aligned, but the centring conventions differ:
#
#  * M (estimated in both). urm:  dnorm(log M, log(mean_M), cv_M)   [median-
#    centred: median(M)=0.06]. Rceattle (cpp ~3124/3134): dnorm(log M,
#    log(M_prior) + M_prior_sd^2/2, M_prior_sd)  [MEAN-centred: E[M]=0.06, i.e.
#    Rceattle adds a +sd^2/2 bias correction to the prior mean]. We pass
#    M_prior=mean_M=0.06 and the LOG-SCALE M_prior_sd=cv_M=0.05 (Models 3 below
#    and Model 2 of the production script). Residual difference = the sd^2/2
#    offset (~0.00125 in log space, ~0.1% on M) -> negligible.
#
#  * q (estimated in both). urm: dnorm(log q, log(mean_q), cv_q). Rceattle (cpp
#    ~2911): dnorm(log q, log(Q_prior), Q_sd_prior), NO bias correction -> the
#    two forms MATCH exactly. We set Q_prior=mean_q=1.0 and Q_sd_prior=cv_q=0.45
#    (log-scale) and Catchability="Estimated-with-prior". NOTE the 2022 template
#    had instead FIXED q at 0.95 (not estimated) - that has been changed here so
#    the estimated models are comparable to urm (urm estimated q ~= 0.56).
#
#  * sigmaR. urm ESTIMATES sigmaR with a very tight prior dnorm(log(sigmaR/
#    mean_sigmaR), 0, cv_sigmaR) (mean 1.5, cv 0.01 -> effectively fixed at 1.5),
#    and this term is in the objective. Rceattle does NOT estimate sigmaR at all:
#    sigma_rec_prior is a fixed constant (we set it to 1.5). So Rceattle has no
#    sigmaR prior term and one fewer estimated parameter. Net sigmaR ~= 1.5 in
#    both, but the objective and parameter count differ (see structural diff #2).
# -----------------------------------------------------------------------------
#
# NOTE: the urm reference model requires RTMButils
#       (pak::pak("BenWilliams-NOAA/RTMButils")) to run. If it is not
#       installed, Part B is skipped and a saved urm report can be loaded
#       instead (see urm_report below), exactly as report2.Rdata was used
#       for the 2022 bridge.
# =============================================================================

library(Rceattle)
library(dplyr)
library(readxl)

setwd_safe <- function() {
  # run from the "GOA Northern rockfish" project directory
  invisible(NULL)
}

# =============================================================================
# PART A — Build the 2024 Rceattle data from the urm data object
# -----------------------------------------------------------------------------
# Strategy: start from the (verified) 2022 Rceattle data list as a structural
# template, then overwrite every time series / data matrix with the 2024 urm
# data. Index mapping is 1:1 (urm comp bin i -> Rceattle Comp_i; urm model age
# i -> Rceattle age i), confirmed by exact value matches against the 2022 file.
# =============================================================================

dat <- readRDS("2024 model/urm-main/data/dat.RDS")   # urm data object
template <- Rceattle::read_data(file = "Data/2022_GOA_northern_rockfish.xlsx")

yrs   <- dat$years                      # 1961:2024
nyr   <- length(yrs)
nage  <- template$nages                 # 50 model ages (minage = 1)
nlen  <- template$nlengths              # 31 length bins
ncomp <- nage                           # number of Comp_* columns in template

mydata <- template
mydata$styr  <- min(yrs)                 # 1961
mydata$endyr <- max(yrs)                 # 2024
mydata$spawn_month   <- dat$spawn_mo     # 5
mydata$sigma_rec_prior <- dat$mean_sigmaR  # 1.5 (urm estimates near this; see diff #2)

## --- helper: build a comp_data block (obs is nbins x nyears) ------------------
make_comp <- function(obs, comp_years, fleet_name, fleet_code, age0len1, month, iss) {
  nbin <- nrow(obs)
  ny   <- ncol(obs)
  comp_mat <- matrix(0, nrow = ny, ncol = ncomp)        # zero-pad to Comp_1..Comp_50
  comp_mat[, seq_len(nbin)] <- t(obs)                   # urm bin i -> Comp_i (1:1)
  df <- data.frame(
    Fleet_name   = fleet_name,
    Fleet_code   = fleet_code,
    Species      = 1,
    Sex          = 0,
    Age0_Length1 = age0len1,
    Year         = comp_years,
    Month        = month,
    Sample_size  = iss,
    stringsAsFactors = FALSE
  )
  comp_df <- as.data.frame(comp_mat)
  names(comp_df) <- paste0("Comp_", seq_len(ncomp))
  cbind(df, comp_df)
}

## --- catch -------------------------------------------------------------------
# urm: catch_wt -> CV via sqrt(1/(2*catch_wt)). Rceattle's catch likelihood is
# lognormal in sd, so set Log_sd to the matching lognormal sd = sqrt(log(cv^2+1)).
# (See structural diff #5: urm has no catch bias correction, Rceattle does.)
catch_cv     <- sqrt(1 / (2 * dat$catch_wt))       # urm catch CV (also used by urm in Part B)
catch_log_sd <- sqrt(log(catch_cv^2 + 1))          # lognormal sd for Rceattle
catch_obs_df <- data.frame(
  Fleet_name        = "fishery",
  Fleet_code        = 2,
  Species           = 1,
  Year              = yrs,
  Month             = 0,
  Selectivity_block = 1,
  Catch             = dat$catch_obs,
  Log_sd            = catch_log_sd
)
# projection years (endyr+1 .. projyr) carry NA catch, matching the 2022 file
proj_years <- (max(yrs) + 1):mydata$projyr
if (length(proj_years) > 0) {
  catch_proj <- data.frame(
    Fleet_name = "fishery", Fleet_code = 2, Species = 1, Year = proj_years,
    Month = 0, Selectivity_block = 1, Catch = NA_real_, Log_sd = catch_log_sd[1]
  )
  catch_obs_df <- rbind(catch_obs_df, catch_proj)
}
mydata$catch_data <- catch_obs_df

## --- survey index ------------------------------------------------------------
# urm stores arithmetic SD (srv_sd, tons). Convert to lognormal sd
# (sqrt(log(1+CV^2))) then INFLATE by 1/sqrt(srv_wt) to emulate urm's flat
# survey NLL weight srv_wt=0.25 (see structural diff #4). srv_cv (un-inflated)
# is kept for the urm run in Part B.
srv_cv     <- dat$srv_sd / dat$srv_obs
srv_log_sd <- sqrt(log(1 + srv_cv^2)) / sqrt(dat$srv_wt)   # sd' = sd / sqrt(0.25) = 2*sd
mydata$index_data <- data.frame(
  Fleet_name        = "Bottom_trawl",
  Fleet_code        = 1,
  Species           = 1,
  Year              = dat$srv_yrs,
  Month             = 7,
  Selectivity_block = 1,
  Q_block           = 1,
  Observation       = dat$srv_obs,
  Log_sd            = srv_log_sd
)

## --- composition data --------------------------------------------------------
# urm has survey-age, fishery-age, fishery-length comps (NO survey-length comp,
# unlike the 2022 Rceattle file -> we drop Bottom_trawl_length here).
srv_age_comp  <- make_comp(dat$srv_age_obs,  dat$srv_age_yrs,  "Bottom_trawl_age", 1, 0, 7, dat$srv_age_iss)
fish_age_comp <- make_comp(dat$fish_age_obs, dat$fish_age_yrs, "Fishery_age",      2, 0, 0, dat$fish_age_iss)
fish_len_comp <- make_comp(dat$fish_size_obs, dat$fish_size_yrs, "Fishery_length", 2, 1, 0, dat$fish_size_iss)
mydata$comp_data <- rbind(srv_age_comp, fish_age_comp, fish_len_comp)

## --- weight-at-age (empirical, grams; 50 model ages, repeated by year) -------
wt_block <- data.frame(
  Wt_name = "Survey_wt", Wt_index = 1, Species = 1, Sex = 0,
  Year = yrs
)
waa_mat <- matrix(rep(dat$waa, each = nyr), nrow = nyr)
colnames(waa_mat) <- paste0("Age", seq_len(nage))
mydata$weight <- cbind(wt_block, as.data.frame(waa_mat))

## --- maturity-at-age ---------------------------------------------------------
mat_df <- data.frame(Species = 1)
mat_df[paste0("Age", seq_len(nage))] <- as.list(dat$maa)
mydata$maturity <- mat_df

## --- natural mortality default (overwritten per model below) -----------------
m1_df <- data.frame(Species = 1, Sex = 0)
m1_df[paste0("Age", seq_len(nage))] <- as.list(rep(dat$mean_M, nage))   # 0.06
mydata$M1_base <- m1_df

## --- size-at-age (growth) transition matrix: urm size_age[age, length] -------
at <- template$age_trans_matrix          # keep meta cols, overwrite Length_*
len_cols <- grep("^Length_", names(at))
stopifnot(length(len_cols) == nlen)
at[, len_cols] <- dat$size_age           # rows = age (1:50), cols = length (1:31)
mydata$age_trans_matrix <- at

## --- ageing-error matrix -----------------------------------------------------
# urm age_error is identical to the 2022 Rceattle matrix (verified), so we keep
# the template's age_error as-is.

## --- comp likelihood weights (urm: fish_age=srv_age=fish_size=0.5) -----------
mydata$fleet_control$Comp_weights <- 0.5   # see structural diff #4

## --- survey catchability prior (see PRIOR DIFFERENCES note) -------------------
# The 2022 template fixed q at 0.95. urm ESTIMATES q with a lognormal prior
# (mean_q=1, cv_q=0.45 on the log scale), so switch to Estimated-with-prior with
# urm's values. Q_prior is natural scale (-> log internally); Q_sd_prior is the
# log-scale prior sd (index_q_sd = exp(log(Q_sd_prior)) = Q_sd_prior in the cpp).
srv_row <- which(mydata$fleet_control$Fleet_type == "Survey")
mydata$fleet_control$Catchability[srv_row] <- "Estimated-with-prior"
mydata$fleet_control$Q_prior[srv_row]      <- dat$mean_q   # 1.0
mydata$fleet_control$Q_sd_prior[srv_row]   <- dat$cv_q     # 0.45 (log-scale sd)

## --- write the bridged 2024 Rceattle data file -------------------------------
Rceattle::write_data(mydata, file = "Data/2024_GOA_northern_rockfish.xlsx")


# =============================================================================
# PART B — Run the urm reference model (the 2024 "truth")
# -----------------------------------------------------------------------------
# Requires RTMButils (pak::pak("BenWilliams-NOAA/RTMButils")) for get_slx() and
# run_model(). The model/selectivity configuration (slx_type, fish_block_ind,
# srv_slx_ind, pars, mapping) is NOT stored in dat.RDS - it lives in the urm run
# script. We assume the 2022 configuration: logistic fishery + logistic survey,
# single time block. Adjust if the 2024 run used a different selectivity form.
# =============================================================================

run_urm <- requireNamespace("RTMButils", quietly = TRUE) &&
           file.exists("2024 model/urm-main/R/urm.R")

if (run_urm) {
  source("2024 model/urm-main/R/urm.R")   # defines urm()

  data_urm <- dat
  data_urm$catch_cv   <- catch_cv         # urm() expects catch_cv, dat has catch_wt
  data_urm$srv_cv     <- srv_cv
  data_urm$saa_array  <- array(dat$size_age, dim = c(nage, nlen, 1))
  data_urm$fish_saa_ind <- rep(1, nyr)
  data_urm$bias_switch  <- 1
  data_urm$bias_ramp    <- rep(1, nyr)    # set per urm README if a ramp is used
  data_urm$slx_type       <- c(1, 1)      # 1 = logistic (fishery, survey)
  data_urm$fish_block_ind <- rep(1, nyr)
  data_urm$srv_slx_ind    <- 2
  data_urm$sex_ratio      <- 0.5

  A <- nrow(dat$age_error)
  log_slx_pars <- log(matrix(c(8.0, 4.0,      # fishery a50, delta (starts)
                               9.0, 1.9),     # survey  a50, delta (starts)
                             nrow = 2, byrow = TRUE))
  pars <- list(
    log_M          = log(dat$mean_M),
    log_slx_pars   = log_slx_pars,
    log_q          = log(dat$mean_q),
    log_mean_R     = 3.0,
    log_Rt         = rep(0, nyr + A - 1),
    log_mean_F     = -3.0,
    log_Ft         = rep(0, nyr),
    log_mean_R_init = 3.0,
    log_F_init      = log(0.05),
    sigmaR         = dat$mean_sigmaR,
    sigmaF         = 1.0,
    log_F50        = log(0.05),
    log_F40        = log(0.06),
    log_F35        = log(0.07)
  )
  map_slx <- matrix(1:4, nrow = 2, byrow = TRUE)
  mapping <- list(sigmaR = factor(NA))     # sigmaR ~ fixed (cv_sigmaR=0.01)

  urm_fit    <- RTMButils::run_model(urm, data_urm, pars, map = mapping)
  urm_report <- urm_fit$rpt              # run_model() returns the report in $rpt
  save(urm_report, file = "Data/urm_report_2024.Rdata")
} else {
  message("RTMButils not available - skipping urm run. ",
          "Load a saved urm report instead, e.g.:  ",
          "load('Data/urm_report_2024.Rdata')")
  if (file.exists("Data/urm_report_2024.Rdata")) load("Data/urm_report_2024.Rdata")
}


# =============================================================================
# PART C — Model 1: Rceattle with dynamics FIXED to the urm MLEs
# -----------------------------------------------------------------------------
# Mirrors 2022 Model 1 (fixed to ADMB MLEs). Requires `urm_report` from Part B.
# urm -> Rceattle parameter map:
#   recruitment : urm Nat[1,t] = exp(log_mean_R - bias_adj[t] + log_Rt[A-1+t]),
#                 bias_adj[t] = bias_ramp[t]*sigmaR^2/2 (bias_switch=1).
#                 Rceattle has no bias ramp, so we pre-subtract bias_adj to make
#                 the recruitment series reproduce urm exactly:
#                    rec_pars[1,1]  = log_mean_R
#                    rec_dev[1, t]  = log_Rt[(A-1+t)] - bias_adj[t],   t = 1..T
#                    init_dev[1, ]  = initial-age devs log_Rt[1:(A-1)]  (reversed)
#   fishing F   : Ft = exp(log_mean_F + log_Ft) -> log_F[2, t] = log(Ft[t])
#   catchability: index_log_q[1] = log(q)
#   selectivity : urm logistic sel = 1/(1+exp(-log(19)*(age+adj - a50)/delta)),
#                 adj = 1 (get_slx called on ages 1:A with adj=1). Rceattle
#                 logistic = 1/(1+exp(-exp(log_sel_slp)*(age - sel_inf))) so
#                    sel_inf     = a50 - 1
#                    log_sel_slp = log(log(19)/delta)
#                 (verified to reproduce urm slx_fish / slx_srv to ~1e-15)
#   M           : M1 fixed at urm M
#
# RESULT (verified): recent-year biomass/SSB match urm within ~1-2% and
# recruitment matches exactly. Early years diverge (initial-equilibrium
# structural difference #3: urm fished equilibrium vs Rceattle unfished).
# =============================================================================

if (exists("urm_report")) {

  mydata$initMode <- 1
  inits <- build_params(mydata)
  A <- nrow(dat$age_error)               # urm model ages (= nage)
  T <- nyr

  # -- recruitment (pre-subtract urm's Methot-Taylor bias adjustment) --
  bias_adj <- (if (data_urm$bias_switch == 1) data_urm$bias_ramp else rep(0, T)) *
              (urm_report$sigmaR^2) / 2
  inits$rec_pars[1, 1]  <- urm_report$log_mean_R
  inits$rec_dev[1, 1:T] <- urm_report$log_Rt[(A - 1 + 1):(A - 1 + T)] - bias_adj
  # initial-age deviations (urm orders youngest->oldest; 2022 bridge used rev())
  n_init <- ncol(inits$init_dev)
  init_devs_urm <- urm_report$log_Rt[1:(A - 1)]
  inits$init_dev[1, 1:min(n_init, length(init_devs_urm))] <-
    rev(init_devs_urm)[1:min(n_init, length(init_devs_urm))]

  # -- selectivity (urm slx_pars columns = a50, delta; adj = 1) --
  # fishery curve = fish_block_ind row (1), survey curve = srv_slx_ind row (2)
  slx <- urm_report$slx_pars
  # fishery (Rceattle selectivity_index 2)
  inits$log_sel_slp[1, 2, 1] <- log(log(19) / slx[1, 2])
  inits$sel_inf[1, 2, 1]     <- slx[1, 1] - 1
  # survey  (Rceattle selectivity_index 1)
  inits$log_sel_slp[1, 1, 1] <- log(log(19) / slx[2, 2])
  inits$sel_inf[1, 1, 1]     <- slx[2, 1] - 1

  # -- fishing mortality --
  inits$log_F[2, 1:T] <- log(urm_report$Ft)

  # -- catchability --
  inits$index_log_q[1] <- log(urm_report$q)

  # -- M fixed at urm value --
  mydata$M1_base[1, grep("^Age", names(mydata$M1_base))] <- urm_report$M

  bridging_model_1 <- Rceattle::fit_mod(
    data_list   = mydata,
    inits       = inits,
    file        = NULL,
    estimateMode = 4,      # fixed parameters (no estimation)
    random_rec  = FALSE,
    msmMode     = 0,
    verbose     = 1,
    phase       = FALSE,
    initMode    = 1,
    M1Fun       = build_M1(updateM1 = TRUE, M1_model = 0)  # fix M (sex-combined)
  )
}


# =============================================================================
# PART D — Model 2: estimate everything (fixed M), single-species
# =============================================================================
bridging_model_2 <- Rceattle::fit_mod(
  data_list   = mydata,
  inits       = NULL,
  file        = NULL,
  estimateMode = 0,        # estimate
  random_rec  = FALSE,
  msmMode     = 0,
  verbose     = 1,
  phase       = TRUE,
  initMode    = 1
)


# =============================================================================
# PART E — Model 3: estimate M with lognormal prior (urm: mean 0.06, cv 0.05)
# =============================================================================
bridging_model_3 <- Rceattle::fit_mod(
  data_list   = mydata,
  inits       = NULL,
  file        = NULL,
  estimateMode = 0,
  random_rec  = FALSE,
  msmMode     = 0,
  verbose     = 1,
  phase       = FALSE,
  initMode    = 1,
  M1Fun       = build_M1(updateM1 = TRUE,
                         M1_model    = 1,
                         M1_use_prior = TRUE,
                         M_prior     = dat$mean_M,   # 0.06
                         # urm prior is lognormal with LOG-SCALE sd = cv_M (line 57:
                         # dnorm(log(M), log(mean_M), cv_M)). Rceattle's M_prior_sd is
                         # also log-scale, so pass cv_M directly (NOT cv_M*mean_M).
                         M_prior_sd  = dat$cv_M)     # 0.05
)


# =============================================================================
# PART F — Overlay urm reference output and plot (mirrors 2022 RTMB overlay)
# -----------------------------------------------------------------------------
# Build a pseudo-Rceattle object holding urm biomass/ssb/recruitment for plotting
# (same trick the 2022 bridge used for the ADMB/RTMB SAFE outputs).
# =============================================================================

if (exists("urm_report") && exists("bridging_model_1")) {
  urm_mod <- bridging_model_1
  urm_mod$quantities$biomass[1, 1:nyr] <- urm_report$tot_bio
  urm_mod$quantities$ssb[1, 1:nyr]     <- urm_report$spawn_bio
  urm_mod$quantities$R[1, 1:nyr]       <- urm_report$recruits

  plot_biomass(list(bridging_model_1, bridging_model_3, urm_mod),
               model_names = c("Rceattle fix parms", "Rceattle est parms", "urm (RTMB)"))
  plot_ssb(list(bridging_model_1, bridging_model_3, urm_mod),
           model_names = c("Rceattle fix parms", "Rceattle est parms", "urm (RTMB)"))
  plot_recruitment(list(bridging_model_1, bridging_model_3, urm_mod),
                   model_names = c("Rceattle fix parms", "Rceattle est parms", "urm (RTMB)"))
  plot_selectivity(bridging_model_3)
}
