# =============================================================================
# 2023 GOA Pacific ocean perch (POP) - BRIDGING ADMB ("goa_pop") -> Rceattle
# =============================================================================
# Single-sex, single-species, single-fishery + single bottom-trawl-survey model.
# Goal: recreate the 2023 GOA POP assessment (ADMB model_20_1) in Rceattle and
# reconcile output, mirroring the GOA Northern rockfish / BSAI Alaska plaice
# bridging scripts.
#
#   Model 1 (forward pass): Rceattle dynamics FIXED to the ADMB MLEs
#                           (parsed from Data/model_20_1.rep). Verifies the
#                           structural map - reproduces ADMB recruitment, total
#                           biomass and SSB to ~6 significant figures (see
#                           VALIDATION below).
#   Model 2 (estimation):   Rceattle estimates everything, M fixed = 0.0743449
#                           (the ADMB MLE), q estimated, non-parametric sel.
#   Model 3 (estimation):   as Model 2 but M estimated with a prior.
#
# DATA (Data/GOApop_single_species_2023.xlsx, Rceattle format)
# - Fishery (1961-2023): catch, age comp, size comp
# - Bottom-trawl survey (1990-2023, biennial): biomass + SE, age comp
# - Empirical weight-at-age (time-invariant), maturity, ageing error, ALK
#
# MODEL (ADMB goa_pop, model_20_1)
# - Single sex; ages 2-29 (28 model ages, 29+ plus group); rec_age = 2
# - Fishery selectivity = non-parametric (Ianelli) with 4 TIME BLOCKS
#       1961-1976, 1977-1995, 1996-2006, 2007-2023
# - Survey selectivity  = non-parametric (Ianelli), time-invariant
# - Survey catchability q estimated (= 1.73611), with a prior
# - M estimated (= 0.0743449), with a prior
# - sigmaR estimated (= 0.764438)
# - Recruitment: mean recruitment + penalized lognormal deviations
#       (the ADMB spawner-recruit penalty is ~0; no active S-R curve)
# - SSB computed at the START of the year (no mortality discounting): the ADMB
#   reports spawn_biom = 0.5 * sum(N * wt * maturity)
#
# -----------------------------------------------------------------------------
# STRUCTURAL DIFFERENCES / MAPPING (why an *exact* estimation match is not
# expected; the forward pass is exact because every parameter is fixed). Each
# numbered item is applied/handled inline below at the marked code.
#
#  1. NUMBER OF AGES (correction, exact). The shipped Excel control sheet has
#     nages = 24 (ages 2-25). ADMB uses nages_M = 28 (ages 2-29, 29+ plus
#     group). We override d$nages <- 28 so the dynamics + plus group match ADMB.
#     The Excel weight/maturity sheets already carry 28 age columns.
#
#  2. MATURITY (correction). ADMB *estimates* maturity internally (note the
#     "Maturity Likelihood" term in the .rep). The estimated ogive (rep
#     "Maturity", matures ~age 9-12) is very different from the Excel `pmature`
#     sheet (matures ~age 4-5). We overwrite maturity with the ADMB-estimated
#     ogive and treat it as fixed input (Rceattle does not estimate maturity).
#     Using the Excel ogive inflates SSB by up to ~85% in high-F years.
#
#  3. SPAWN TIMING (correction). ADMB reports SSB at the start of the year with
#     NO mortality discounting: spawn_biom = 0.5 * sum(N*wt*maturity). The Excel
#     control has spawn_month = 8. Set spawn_month = 0 so Rceattle's
#     exp(-Z*spawn_month/12) factor = 1 and SSB matches ADMB exactly. (The .dat
#     "spawn_fract = 5" is not applied as a mortality discount in this model.)
#
#  4. SEX RATIO (mapping, exact). Single-sex model; Rceattle multiplies maturity
#     by sex_ratio = 0.5 for 1-sex models (mature_females = maturity * 0.5),
#     reproducing the ADMB female spawn_biom = 0.5 * sum(N*wt*maturity).
#
#  5. FISHERY SELECTIVITY TIME BLOCKS (structural). ADMB fishery selectivity is
#     non-parametric with 4 discrete time blocks. Rceattle's Ianelli
#     non-parametric selectivity (Selectivity = 2) supports time variation ONLY
#     as 'None' or 'IID' annual deviates - NOT discrete blocks (see Rceattle
#     R/2-build_map.R). FORWARD PASS: we sidestep this with EMPIRICAL
#     selectivity (Selectivity = 0), feeding the exact ADMB block sel-at-age per
#     year, so F-at-age = Fully_selected_F * sel matches ADMB. ESTIMATION: the
#     fishery uses a single time-invariant non-parametric ogive (Model 2/3) - a
#     documented simplification of the 4 ADMB blocks.
#
#  6. SELECTIVITY NORMALIZATION (mapping). ADMB normalizes each non-parametric
#     selectivity to max = 1. Rceattle case-2 normalizes by the mean (avg_sel).
#     Selectivity scaling is absorbed by F/q, so SSB/biomass/recruitment are
#     invariant - but for the FORWARD PASS we use empirical selectivity (the
#     exact ADMB-normalized values) together with the ADMB Fully_selected_F, so
#     F-at-age is reproduced exactly.
#
#  7. CATCHABILITY q (estimation). ADMB estimates a single trawl-survey q with a
#     lognormal prior (q = 1.73611). Forward pass fixes index_log_q = log(q).
#     Estimation models estimate q with a prior.
#
#  8. M (estimation). ADMB estimates a single sex-combined M with a prior
#     (M = 0.0743449). Forward pass fixes M; Model 2 fixes M at the ADMB MLE;
#     Model 3 estimates M with a prior.
#
#  9. sigmaR (structural). ADMB estimates sigmaR (= 0.764438). Rceattle fixes
#     sigma_rec_prior (= 0.70710678 in the control sheet) and penalizes
#     recruitment deviations against it. This shifts the recruitment likelihood
#     and the estimated rec deviations - by design.
#
# 10. AGEING ERROR / ALK / COMP BINS (correction). The ADMB ageing-error matrix
#     is 28 (true/model ages) x 24 (reader/data bins) and already accumulates
#     true ages 25-29 into data bin 24 (age 25+). Rceattle wants a square
#     nages x nages matrix, so we embed it as 28 x 28 with data bins 25-28 = 0,
#     and zero-fill the (NA) age-comp bins 25-28 so the multinomial does not see
#     NA. The ALK is extended to 28 ages (ages 26-29 = copy of age 25).
#
# 13. SURVEY INDEX SD (correction). The Excel index_data Log_sd holds the
#     arithmetic survey SE. Rceattle treats Log_sd as a log-scale SD, so we
#     convert SE -> lognormal SD: sqrt(log(1 + (SE/obs)^2)). Without this the
#     survey index likelihood explodes (~1e11) and the estimation diverges. (The
#     forward pass derived quantities do not depend on this, but it is applied
#     uniformly so the survey fit is correct.)
#
# 14. SURVEY SIZE COMPOSITION (correction). ADMB does NOT use the bottom-trawl
#     survey size composition (its likelihood weight is 0 in the .rep). The Excel
#     carries it, so we drop those comp rows; otherwise Rceattle fits data ADMB
#     ignores and the survey selectivity / q / biomass scale are distorted.
#
# 15. CATCH LIKELIHOOD SD (correction). ADMB uses an SSQ catch likelihood with
#     weight 50 -> lognormal sigma = 1/sqrt(2*50) = 0.0707. We set catch
#     Log_sd to match (the Excel default differs).
#
# 11. INITIALIZATION (mapping, exact). The ADMB .par was not available, but the
#     .rep reports the full numbers-at-age matrix. With initMode = 0 Rceattle
#     sets N(age,styr) = exp(init_dev(age-1)) directly, so init_dev[1:27] =
#     log(N_1961[ages 3..29]) reproduces the ADMB initial age structure exactly
#     (age 2 = recruitment, from rec_dev). Subsequent years follow from the
#     recursion with fixed M and F-at-age.
#
# 12. OPTIMIZER / PHASING. ADMB ADMB-phasing vs TMB - expected, not chased.
#
# VALIDATION (Model 1 vs ADMB, run on this machine):
#   Recruitment  : mean |%diff| < 0.0001%   (essentially exact)
#   Total biomass: mean |%diff| < 0.001%
#   SSB          : mean |%diff| < 0.0001% in 1961-2022; the TERMINAL year (2023)
#                  differs ~4.5% because the ADMB-reported SpBiom[2023] is itself
#                  inconsistent with 0.5*sum(N_2023*wt*maturity) from its own N
#                  matrix (an ADMB terminal-year reporting quirk). The Rceattle
#                  2023 numbers-at-age match ADMB to 6 sig figs.
# =============================================================================

library(Rceattle)
library(readxl)
library(dplyr)

setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA pop")

# -----------------------------------------------------------------------------
# Parse the ADMB .rep
# -----------------------------------------------------------------------------
rep_lines <- readLines("Data/model_20_1.rep")
get_row <- function(label) {
  i <- grep(paste0("^", label), rep_lines)[1]
  as.numeric(strsplit(trimws(sub("^[^ ]+ *", "", rep_lines[i])), "[ ]+")[[1]])
}

styr <- 1961; endyr <- 2023; yrs <- styr:endyr; nyr <- length(yrs)
nage <- 28                                   # ADMB nages_M (ages 2-29)

# Scalar MLEs (from the "key parameter estimates" block of the .rep)
M_admb   <- 0.0743449
q_admb   <- 1.73611
sigr     <- 0.764438

# Year-indexed series (1961-2023)
admb_R    <- get_row("Recruitment ")
admb_SSB  <- get_row("SpBiom ")
admb_Btot <- get_row("Tot_biom ")
admb_Fsel <- get_row("Fully_selected_F ")    # fully-selected F per year

# Non-parametric selectivity (4 fishery blocks + survey), maturity, weight
fsh_block_sel <- list(get_row("Fishery_Selectivity_1967-1976"),
                      get_row("Fishery_Selectivity_1977-1995"),
                      get_row("Fishery_Selectivity_1996-2006"),
                      get_row("Fishery_Selectivity_2007-2023"))
srv_sel  <- get_row("Bottom_Trawl_Survey_Selectivity ")
mat_admb <- get_row("Maturity ")             # ADMB ESTIMATED maturity ogive
wt_admb  <- get_row("Weight ")

# Numbers-at-age matrix (1961-2023 x 28)
ni <- grep("^Numbers ", rep_lines)[1]
admb_N <- matrix(NA_real_, nyr, nage)
for (k in 1:nyr) {
  admb_N[k, ] <- as.numeric(strsplit(trimws(rep_lines[ni + k]), "[ ]+")[[1]])[-1][1:nage]
}

# Fishery selectivity time-block index (diff #5)
fsh_block_of_year <- function(y) if (y <= 1976) 1 else if (y <= 1995) 2 else if (y <= 2006) 3 else 4

# ADMB ageing-error matrix (28 true ages x 24 reader/data bins) from the .dat.
# The old true ages (25-29) already accumulate into data bin 24 (age 25+).
.dat   <- readLines("Data/goa_pop_2023.dat")
.i0    <- grep("Ageing error matrix", .dat)
.nr    <- integer(0); .j <- .i0 + 1
while (length(.nr) < nage && .j <= length(.dat)) {
  if (grepl("^[0-9]", trimws(.dat[.j])) && !grepl("^#", .dat[.j])) .nr <- c(.nr, .j)
  .j <- .j + 1
}
A_admb <- t(sapply(.nr, function(r) as.numeric(strsplit(trimws(.dat[r]), "[ ]+")[[1]])))  # 28 x 24

# -----------------------------------------------------------------------------
# Build the base Rceattle data list and apply the structural corrections
# -----------------------------------------------------------------------------
prep_data <- function() {
  d <- suppressMessages(Rceattle::read_data(file = "Data/GOApop_single_species_2023.xlsx"))
  # Survey index SE -> lognormal SD (CV-based), matching the rockfish bridge.
  # Without this the survey index likelihood explodes and estimation diverges.
  d$index_data$Log_sd <- sqrt(log(1 + (d$index_data$Log_sd^2) / (d$index_data$Observation^2)))
  d$nages       <- nage                        # diff #1: 24 -> 28 model ages
  d$spawn_month <- 0                            # diff #3: SSB at start of year
  d$maturity[1, paste0("Age", 1:nage)] <- mat_admb   # diff #2: ADMB est. maturity
  d$M1_base[1, paste0("Age", 1:nage)]  <- M_admb     # diff #8: M baseline

  # diff #10: ageing error 28(true) x 28 with data bins 25-28 = 0 (the ADMB
  # matrix maps true ages 25-29 into data bin 24); ALK extended to 28 ages.
  ae    <- data.frame(Species = 1L, True_age = 1:nage)
  Afull <- cbind(A_admb, matrix(0, nage, nage - 24))
  for (j in 1:nage) ae[[paste0("Obs_age", j)]] <- Afull[, j]
  d$age_error <- ae

  at <- as.data.frame(d$age_trans_matrix)
  d$age_trans_matrix <- do.call(rbind, lapply(unique(at$Age_transition_index), function(ix) {
    sub  <- at[at$Age_transition_index == ix, ]
    last <- sub[sub$Age == max(sub$Age), ]
    add  <- do.call(rbind, lapply((max(sub$Age) + 1):29, function(a) { r <- last; r$Age <- a; r }))
    rbind(sub, add)
  }))

  # Zero-fill the NA age-comp bins 25-28 (the multinomial cannot see NA).
  age_idx <- d$comp_data$Age0_Length1 == 0
  for (cc in paste0("Comp_", 25:28)) d$comp_data[[cc]][age_idx] <- 0

  # diff #14: ADMB does NOT use the bottom-trawl survey SIZE composition (its
  # likelihood weight is 0 in the .rep). Drop those rows so Rceattle does not
  # fit data ADMB ignores (otherwise it distorts the survey selectivity/q/scale).
  d$comp_data <- d$comp_data[!(d$comp_data$Fleet_code == 1 & d$comp_data$Age0_Length1 == 1), ]

  # diff #15: ADMB catch likelihood is SSQ with weight 50 -> lognormal sigma =
  # 1/sqrt(2*50) = 0.0707. Match it (the Excel default differs).
  d$catch_data$Log_sd <- 1 / sqrt(2 * 50)
  d
}

# =============================================================================
# Model 1 - FORWARD PASS: Rceattle dynamics fixed to the ADMB MLEs
# -----------------------------------------------------------------------------
# Empirical selectivity (the exact ADMB block sel-at-age per year) + fixed
# recruitment, initial age structure, M, q and fully-selected F.
# =============================================================================
d1 <- prep_data()
d1$initMode <- 0                               # diff #11: N(age,1) = exp(init_dev)

# diff #5: empirical selectivity per year (fishery = 4 blocks; survey constant)
emp <- data.frame()
for (k in 1:nyr) {
  y <- yrs[k]
  emp <- rbind(emp,
    data.frame(Fleet_name = "POP_fishery", Fleet_code = 2, Species = 1, Sex = 0, Year = y,
               t(setNames(fsh_block_sel[[fsh_block_of_year(y)]], paste0("Comp_", 1:nage)))),
    data.frame(Fleet_name = "Trawl survey", Fleet_code = 1, Species = 1, Sex = 0, Year = y,
               t(setNames(srv_sel, paste0("Comp_", 1:nage)))))
}
d1$emp_sel <- emp
d1$fleet_control$Selectivity <- 0              # empirical selectivity

inits <- suppressMessages(Rceattle::build_params(d1))

# Recruitment: reproduce admb_R exactly (mean + deviations)
inits$rec_pars[1, 1]      <- log(mean(admb_R))
inits$rec_dev[1, 1:nyr]   <- log(admb_R) - inits$rec_pars[1, 1]
# diff #11: initial age structure for ages 3..29 (init_dev index 1..27)
inits$init_dev[1, 1:27]   <- log(admb_N[1, 2:28])
# Fully-selected F on the fishery fleet row (fleet 2)
inits$log_F[2, 1:nyr]     <- log(admb_Fsel)
# Catchability (diff #7)
inits$index_log_q[1]      <- log(q_admb)

bridging_model_1 <- Rceattle::fit_mod(
  data_list    = d1,
  inits        = inits,
  file         = NULL,
  estimateMode = 4,                            # all parameters FIXED at inits
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = FALSE,
  initMode     = 0,
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 0)   # M fixed sex-combined
)

# -- VALIDATION: forward pass vs ADMB -----------------------------------------
q1 <- bridging_model_1$quantities
fp <- data.frame(
  Year   = yrs,
  R_pd   = 100 * (q1$R[1, 1:nyr]       / admb_R    - 1),
  SSB_pd = 100 * (q1$ssb[1, 1:nyr]     / admb_SSB  - 1),
  B_pd   = 100 * (q1$biomass[1, 1:nyr] / admb_Btot - 1))
cat("Forward pass vs ADMB - mean |%diff|:",
    "R",       round(mean(abs(fp$R_pd)),   5),
    "| Biomass", round(mean(abs(fp$B_pd)), 5),
    "| SSB (1961-2022)", round(mean(abs(fp$SSB_pd[-nyr])), 5),
    "| SSB(2023)", round(fp$SSB_pd[nyr], 3), "\n")


# -----------------------------------------------------------------------------
# Shared estimation-model configuration helper. Applies the data corrections in
# prep_data() plus sigmaR = 0.764438 (the ADMB estimate; Rceattle fixes it,
# diff #9) and the chosen selectivity form.
#   sel = 2 : non-parametric (Ianelli) - faithful to the ADMB functional form
#   sel = 1 : logistic                  - lands much closer to ADMB numerically
# -----------------------------------------------------------------------------
config_est <- function(sel) {
  d <- prep_data()
  d$initMode <- 1                              # unfished-equilibrium + init devs
  d$sigma_rec_prior <- sigr                    # diff #9: match ADMB sigmaR
  d$fleet_control$Selectivity      <- sel
  d$fleet_control$N_sel_bins       <- nage
  d$fleet_control$Time_varying_sel <- 0        # time-invariant (no IID/blocks)
  d$fleet_control$Sel_curve_pen1   <- c(20, 0) # survey asymptotic; fishery dome allowed
  d$fleet_control$Sel_curve_pen2   <- c(12.5, 12.5)  # curvature penalty (non-par only)
  d
}

# =============================================================================
# Model 2 - ESTIMATION, non-parametric selectivity (ADMB's form), M fixed
# -----------------------------------------------------------------------------
# Faithful to the ADMB non-parametric selectivity, but a SINGLE time-invariant
# ogive (Rceattle cannot block non-parametric sel, diff #5). The single block is
# over-flexible and trades selectivity shape against population scale, so this
# lands ~55% above ADMB biomass despite matching the functional form.
# =============================================================================
bridging_model_2 <- Rceattle::fit_mod(
  data_list = config_est(sel = 2), inits = NULL, file = NULL,
  estimateMode = 0, random_rec = FALSE, msmMode = 0, verbose = 1, phase = TRUE,
  initMode = 1, M1Fun = build_M1(updateM1 = TRUE, M1_model = 0))   # M fixed

# =============================================================================
# Model 3 - ESTIMATION, logistic selectivity, M fixed (CLOSEST to ADMB)
# -----------------------------------------------------------------------------
# Logistic selectivity is more constrained than the single non-parametric block,
# so it recovers a survey q (~1.46) and a biomass/SSB scale much closer to ADMB
# (within ~15-20%). It departs from the ADMB non-parametric FORM, so the
# remaining gap is structural: single block vs 4 ADMB blocks, fixed vs estimated
# early recruitment, comp weighting, and TMB vs ADMB optimization.
# =============================================================================
bridging_model_3 <- Rceattle::fit_mod(
  data_list = config_est(sel = 1), inits = NULL, file = NULL,
  estimateMode = 0, random_rec = FALSE, msmMode = 0, verbose = 1, phase = TRUE,
  initMode = 1, M1Fun = build_M1(updateM1 = TRUE, M1_model = 0))   # M fixed

# =============================================================================
# Model 4 - ESTIMATION, logistic selectivity, M ESTIMATED (prior near ADMB MLE)
# =============================================================================
bridging_model_4 <- Rceattle::fit_mod(
  data_list = config_est(sel = 1), inits = NULL, file = NULL,
  estimateMode = 0, random_rec = FALSE, msmMode = 0, verbose = 1, phase = FALSE,
  initMode = 1, M1Fun = build_M1(updateM1 = TRUE, M1_model = 1,
                                 M1_use_prior = TRUE, M_prior = M_admb, M_prior_sd = 0.0074))


# =============================================================================
# Overlay the ADMB ("SAFE") reference output and plot
# -----------------------------------------------------------------------------
# Build a pseudo-Rceattle object holding the ADMB series (same trick the
# rockfish / plaice bridges use). Rceattle and ADMB share units, so no rescaling.
# =============================================================================
SAFE2023 <- bridging_model_1
SAFE2023$quantities$biomass[1, 1:nyr] <- admb_Btot
SAFE2023$quantities$ssb[1, 1:nyr]     <- admb_SSB
SAFE2023$quantities$R[1, 1:nyr]       <- admb_R

mods  <- list(bridging_model_1, bridging_model_2, bridging_model_3, bridging_model_4, SAFE2023)
names <- c("Rceattle fix parms (fwd pass)", "Rceattle est non-par (M=0.074)",
           "Rceattle est logistic (M=0.074)", "Rceattle est logistic M", "ADMB (SAFE)")

plot_biomass(mods, model_names = names);     mtext(side = 2, "Total biomass", line = 1.8)
plot_ssb(mods, model_names = names);         mtext(side = 2, "Female SSB",    line = 1.8)
plot_recruitment(mods, model_names = names); mtext(side = 2, "Recruitment",   line = 1.8)
plot_selectivity(bridging_model_3)
