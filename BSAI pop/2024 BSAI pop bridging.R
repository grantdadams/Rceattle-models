# =============================================================================
# 2024 BSAI Pacific ocean perch (POP) - BRIDGING ADMB ("pop24" / m_24_2) -> Rceattle
# =============================================================================
# Single-sex (sexes-combined), single-species, single fishery + TWO surveys
# (AI bottom-trawl + EBS slope). Goal: recreate the 2024 ADMB POP assessment
# (2024 assessment/pop24.tpl, MLEs in m_24_2_reweighted.par) in Rceattle and
# reconcile output, mirroring the GOA Northern rockfish / BSAI Alaska plaice
# bridging scripts.
#
#   Model 1 (forward pass): Rceattle dynamics FIXED to the ADMB MLEs.
#                           Verifies the structural map - should reproduce ADMB
#                           SSB / total biomass / recruitment to ~6 sig figs.
#   Model 2 (estimation):   Rceattle estimates everything, M estimated with the
#                           ADMB lognormal prior (mean 0.05, cv 0.05).
#   Model 3 (estimation):   as Model 2 but M fixed at the ADMB value (0.0506).
#
# DATA (Data/bsai_pop_single_species_2024.xlsx, already in Rceattle format)
# - Fishery: catch (1960-2024), age comp, length comp, fishery weight-at-age
# - AI survey: biomass + SD (1991-2024), age comp, length comp
# - EBS slope survey: biomass + SD (2002-2016), age comp
# - Population weight-at-age, estimated logistic maturity-at-age, ageing error,
#   age->length transition matrix
#
# MODEL (ADMB pop24, m_24_2_reweighted)
# - styr 1960, endyr 2024; 44 model ages (3-46), 38 data ages (3-40, +group 40)
# - Single sex; SSB = 0.5 * sum(wt_pop * maturity * natage * exp(-spmo_frac*Z))
# - Fishery selectivity = BICUBIC SPLINE, time-varying (5 yr-nodes x 5 age-nodes)
# - Survey selectivity   = logistic (both surveys), time-invariant
# - AI survey q estimated WITH lognormal prior (mean 1, cv 0.15) -> 1.0608
#   EBS survey q estimated WITHOUT prior                          -> 0.2595
# - M estimated WITH lognormal prior (mean 0.05, cv 0.05)         -> 0.0506
# - Recruitment: random about a single mean (sr_type=1, NOT Beverton-Holt);
#   rec_dev 1960-2021, last 3 yrs (2022-2024) fixed at exp(mean_log_rec+sigr^2/2)
# - Initial age comps (fyear_ac_option=2): UNFISHED equilibrium from a SEPARATE
#   virgin recruitment log_rinit (historic_catch=0 -> historic_F=0)
# - Empirical weight-at-age (separate population vs fishery weights)
# - Composition likelihood = multinomial
#
# -----------------------------------------------------------------------------
# STRUCTURAL DIFFERENCES / MAPPING (why an *exact* estimation match is not
# expected; the forward pass is exact because every dynamic is fixed). Each item
# is applied/handled inline below at the marked code.
#
#  1. FISHERY SELECTIVITY FORM (mapping for fwd pass; structural for estimation).
#     ADMB fishery selectivity is a 2-D bicubic spline -> a smooth time-varying
#     age x year selectivity surface, heavily constrained by smoothness penalties
#     (lambda 3-6). Rceattle has no bicubic spline. For the FORWARD PASS we
#     sidestep the parameterization entirely by feeding the ADMB selectivity-at-
#     age-by-year matrix as EMPIRICAL selectivity (Selectivity=0, emp_sel) - this
#     reproduces the full time-varying surface EXACTLY (see VALIDATION).
#     For ESTIMATION the natural analog is Rceattle's non-parametric (Ianelli,
#     Selectivity=2) selectivity with IID time-varying deviations. However, the
#     fully-free IID surface (38 age bins x 65 yrs = 2470 deviations) is NOT
#     identifiable from POP's sparse age comps without the spline's smoothness
#     constraint (tested at several dev-sd priors: gradient = NA, non-positive-
#     definite Hessian, degenerate SSB). The estimation models below therefore
#     use a non-parametric TIME-INVARIANT fishery selectivity (Selectivity=2,
#     Time_varying_sel=0), which converges cleanly. The time-varying surface is
#     already reconciled exactly in the forward pass, so this is a deliberate
#     simplification of the *estimation* analog, not a loss of the bridge.
#     NOTE: ADMB estimates selectivity only for nselages=38 ages (3-40); ages
#     41-46 reuse the age-40 value. We extend the empirical sel flat from age 40
#     to ages 41-46 (matches ADMB's plus-age selectivity exactly).
#
#  2. SURVEY SELECTIVITY (mapping, exact for fwd pass). Logistic in ADMB, time-
#     invariant. We feed it as empirical selectivity for the forward pass (exact)
#     and estimate it as logistic (Selectivity=1) for the estimation models.
#
#  3. INITIAL NUMBERS-AT-AGE (mapping, exact). ADMB fyear_ac_option=2 builds an
#     unfished equilibrium from a SEPARATE virgin recruitment log_rinit (=3.964),
#     distinct from the recruitment mean mean_log_rec (=4.313):
#        natage(styr,1)   = exp(mean_log_rec + rec_dev(styr))     [recruit age]
#        natage(styr,j>=2)= exp(log_rinit + sigr^2/2 - M*(j-1))   [older ages]
#        plus group /= (1 - exp(-M))
#     Rceattle's mean-recruitment SRR FORCES R_init = R0 = exp(rec_pars[1,1])
#     (cpp ~1076), so the log_rinit-vs-mean_log_rec gap cannot live in a separate
#     R_init parameter. We absorb it into a CONSTANT init_dev offset:
#        rec_pars[1,1]   = mean_log_rec
#        init_dev[1,a]   = (log_rinit + sigr^2/2 - mean_log_rec)  for all a
#     so N_init[age] = exp(mean_log_rec) * exp(-M*age + init_dev) reproduces ADMB
#     exactly. initMode=1 (unfished equilibrium, Finit=0).
#
#  4. RECRUITMENT (mapping). Rceattle R(yr)=exp(rec_pars[1,1])*exp(rec_dev(yr))
#     (no bias correction in the *realized* series; the +/-sigma^2/2 shift only
#     enters the rec_dev penalty, which is inactive in the fixed forward pass).
#        rec_dev[1, 1960:2021] = ADMB rec_dev          (62 values)
#        rec_dev[1, 2022:2024] = sigr^2/2 = 0.28125    (fixed-recruitment years)
#     Year-1 recruit uses R_init (=R0 here) so rec_dev(1960)=ADMB rec_dev(1960).
#
#  5. FISHING MORTALITY + UNNORMALIZED FISHERY SELECTIVITY (mapping, exact).
#     ADMB F(i,j) = sel_fish_UNNORMALIZED(i,j) * exp(log_avg_fmort+fmort_dev(i)),
#     where the internal fishery selectivity is NOT normalized to a max of 1 (the
#     bicubic spline floats freely; the apical sel-at-age exceeds 1). The
#     REPORTED selfish is rescaled to max 1, and the per-year max-sel is folded
#     into the reported t.series$fmort (so fmort_ts = maxsel * exp(log_avg_fmort+
#     fmort_dev)). Rceattle can hold selectivity UNNORMALIZED too - set the
#     fishery Sel_norm_bin1 = NA (the default here), which makes
#     normalize_and_project_selectivity skip normalization (cpp selectivity.hpp).
#     We therefore reconstruct ADMB's unnormalized fishery selectivity,
#        maxsel(i)        = fmort_ts(i) / exp(log_avg_fmort + fmort_dev(i))
#        sel_unnorm(i, .) = selfish_norm(i, .) * maxsel(i)
#     feed it as empirical selectivity, and set log_F to the LITERAL ADMB fishing
#     parameters:  log_F[1, yr] = log_avg_fmort + fmort_dev(yr). Then
#     F_at_age = sel_unnorm * exp(log_F) reproduces ADMB F (hence the catch and
#     the whole trajectory) exactly. (NOTE: the survey selectivities ARE max-1 in
#     ADMB - logistic saturating to 1 - so they are fed as-is with q carrying the
#     scale.)
#
#  6. NATURAL MORTALITY (mapping). Single time-invariant M = exp(log_avg_M) =
#     0.050576. M1 fixed there for the forward pass; estimated with the ADMB
#     lognormal prior (mean 0.05, log-scale sd = cv_M = 0.05) for Model 2.
#
#  7. CATCHABILITY (mapping). index_log_q = log(ADMB q): AI=log(1.06075),
#     EBS=log(0.259452). Estimated with/without prior for the estimation models.
#
#  8. SPAWNING TIMING (correction). ADMB spmo_frac=(spawn_mo-1)/12=(4-1)/12=0.25.
#     Rceattle uses exp(-Z*spawn_month/12), so set spawn_month = 3 (not the 4 in
#     the shipped Excel) to reproduce exp(-0.25*Z). (Same gotcha as plaice.)
#
#  9. SINGLE-SEX SSB FACTOR (mapping, exact). ADMB multiplies natage by 1/2 for
#     SSB. Rceattle (nsex=1) computes mature_females = maturity * sex_ratio and
#     forces sex_ratio=1.0 for the population (so biomass = full natage). With
#     sex_ratio (R_sexr) = 0.5 in the data, SSB = 0.5*natage*maturity*... matches.
#
# 10. SURVEY INDEX PREDICTION (structural; affects estimation only). ADMB uses a
#     whole-year-averaged available biomass q*sum(natage*(1-exp(-Z))/Z*sel*wt),
#     whereas Rceattle uses a point-in-time q*sum(natage*exp(-Z*mo/12)*sel*wt).
#     This changes the survey FIT (estimation), not the fixed forward-pass
#     biomass/SSB/recruitment. Documented, not chased.
#
# 11. COMP LIKELIHOOD / COMP WEIGHTS (structural). ADMB McAllister-Ianelli
#     reweighted multinomial; Rceattle MultinomialAFSC. Contributes to estimation
#     differences.
#
# 12. OPTIMIZER / PHASING. ADMB phasing vs TMB - expected, not chased.
# =============================================================================

library(Rceattle)
library(dplyr)
library(readxl)

admb_dir <- "2024 assessment"

# -----------------------------------------------------------------------------
# Data
# -----------------------------------------------------------------------------
mydata <- Rceattle::read_data(file = "Data/bsai_pop_single_species_2024.xlsx")

# -- data corrections (see diffs #8, #9, and the EBS species typo) -------------
mydata$spawn_month <- 3                                  # diff #8: (spawn_mo-1)/12 = 0.25
ebs <- mydata$fleet_control$Fleet_name == "EBS_survey"
mydata$fleet_control$Species[ebs]    <- 1                # was 2 (typo; nspp=1)
mydata$fleet_control$Fleet_type[ebs] <- 2                # was 3 (typo; Survey=2)
mydata$initMode <- 1                                     # diff #3: unfished equilibrium, Finit=0
stopifnot(all(mydata$sex_ratio[grep("^Age", names(mydata$sex_ratio))] == 0.5))      # diff #9

# -- survey index sd -> CV (the shipped Excel stores the raw SD in tonnes, but
#    Rceattle's lognormal index likelihood reads Log_sd as a CV; cf. plaice/yfs).
#    Also drop the stray trailing columns that leaked into the Excel index block.
mydata$index_data <- mydata$index_data[, c("Fleet_name", "Fleet_code", "Species",
  "Year", "Month", "Selectivity_block", "Q_block", "Observation", "Log_sd")]
mydata$index_data$Log_sd <- mydata$index_data$Log_sd / mydata$index_data$Observation

styr  <- mydata$styr     # 1960
endyr <- mydata$endyr    # 2024
nages <- mydata$nages    # 44 (ages 3-46)
yrs   <- styr:endyr
nyr   <- length(yrs)     # 65


# =============================================================================
# Parse the ADMB MLEs (m_24_2_reweighted.par) and reference series (.rdat)
# =============================================================================
par_lines <- readLines(file.path(admb_dir, "m_24_2_reweighted.par"))
get_par <- function(name) {
  i <- grep(paste0("^# ", name, ":"), par_lines, fixed = FALSE)
  i <- i[1]
  vals <- c(); j <- i + 1
  while (j <= length(par_lines) && !grepl("^#", par_lines[j])) {
    vals <- c(vals, as.numeric(strsplit(trimws(par_lines[j]), "\\s+")[[1]]))
    j <- j + 1
  }
  vals
}
log_avg_M     <- get_par("log_avg_M")        # -2.98427 -> M = 0.050576
log_avg_fmort <- get_par("log_avg_fmort")    # -3.66442
fmort_dev     <- get_par("fmort_dev")        # 1960..2024 (65)
rec_dev_admb  <- get_par("rec_dev")          # 1960..2021 (62)
mean_log_rec  <- get_par("mean_log_rec")     # 4.31286
log_rinit     <- get_par("log_rinit")        # 3.96407
log_q_srv     <- c(get_par("log_q_srv\\[1\\]"), get_par("log_q_srv\\[2\\]"))  # 0.05898, -1.34918
sigr          <- 0.75                        # pop24.dat 'sigr'
M_admb        <- exp(log_avg_M)              # 0.050576

# ADMB reference time series (the "SAFE" output) from the R data dump
admb <- dget(file.path(admb_dir, "m_24_2_reweighted.rdat"))
ts   <- admb$t.series
admb_ref <- data.frame(
  Year    = as.numeric(ts[, "year"]),
  R       = as.numeric(ts[, "a3recs"]),
  SSB     = as.numeric(ts[, "spbiom"]),
  Biomass = as.numeric(ts[, "totbiom"])
)
admb_ref <- admb_ref[admb_ref$Year %in% yrs, ]


# =============================================================================
# Empirical selectivity (diffs #1, #2): ADMB sel-at-age-by-year for all 3 fleets
# -----------------------------------------------------------------------------
# ADMB reports 38 ages (3-40); extend flat to ages 41-46 (Rceattle cols 39-44),
# matching ADMB's reuse of the age-40 (nselages) selectivity for older ages.
# =============================================================================
extend_sel <- function(mat38) {                  # 65 x 38  ->  65 x 44
  cbind(mat38, matrix(mat38[, ncol(mat38)], nrow = nrow(mat38), ncol = nages - ncol(mat38)))
}
# fishery: reconstruct ADMB's UNNORMALIZED selectivity (diff #5) so log_F can be
# the literal log_avg_fmort + fmort_dev. Sel_norm_bin1 = NA keeps it unnormalized.
fmort_ts <- as.numeric(ts[ts[, "year"] %in% yrs, "fmort"])
maxsel   <- fmort_ts / exp(log_avg_fmort + fmort_dev)      # per-year ADMB max-sel (> 1)
sel_fish <- extend_sel(admb$selfish * maxsel)              # fishery (unnormalized, time-varying)
sel_ai   <- extend_sel(admb$AI_survey_sel)                # AI survey (max-1 logistic, as-is)
sel_ebs  <- extend_sel(admb$EBS_survey_sel)               # EBS survey (max-1 logistic, as-is)

make_emp_sel <- function(sel_mat, fleet_name, fleet_code) {
  df <- data.frame(Fleet_name = fleet_name, Fleet_code = fleet_code,
                   Species = 1, Sex = 0, Year = yrs, stringsAsFactors = FALSE)
  sel_df <- as.data.frame(sel_mat); names(sel_df) <- paste0("Comp_", seq_len(nages))
  cbind(df, sel_df)
}
emp_sel_fwd <- rbind(
  make_emp_sel(sel_fish, "pop_fishery", 1),
  make_emp_sel(sel_ai,   "AI_survey",   2),
  make_emp_sel(sel_ebs,  "EBS_survey",  3)
)


# =============================================================================
# Model 1 - FORWARD PASS: Rceattle dynamics fixed to the ADMB MLEs
# =============================================================================
mydata_fwd <- mydata
mydata_fwd$fleet_control$Selectivity   <- 0        # diffs #1,#2: empirical sel for all fleets
mydata_fwd$fleet_control$Sel_norm_bin1 <- NA       # diff #5: hold sel UNNORMALIZED (default)
mydata_fwd$emp_sel <- emp_sel_fwd
mydata_fwd$M1_base[grep("^Age", names(mydata_fwd$M1_base))] <- M_admb   # diff #6

inits <- build_params(mydata_fwd)

# -- recruitment mean (diff #3/#4) --------------------------------------------
inits$rec_pars[1, 1] <- mean_log_rec

# -- recruitment deviations (diff #4) -----------------------------------------
# rec_dev columns are styr..projyr; fill the hindcast 1960..2024.
rec_dev_full <- numeric(nyr)
rec_dev_full[1:length(rec_dev_admb)] <- rec_dev_admb          # 1960..2021
rec_dev_full[(length(rec_dev_admb) + 1):nyr] <- sigr^2 / 2    # 2022..2024 fixed recruits
inits$rec_dev[1, 1:nyr] <- rec_dev_full

# -- initial-age deviations (diff #3): constant offset reproduces log_rinit ----
init_offset <- log_rinit + sigr^2 / 2 - mean_log_rec
inits$init_dev[1, ] <- init_offset

# -- fishing mortality (diff #5): fishery is fleet row 1 -----------------------
# Literal ADMB fishing parameters (sel is unnormalized, so F = sel_unnorm*exp(log_F)).
inits$log_F[1, 1:nyr] <- log_avg_fmort + fmort_dev

# -- catchability (diff #7): set the two survey-q slots ------------------------
# index_log_q is length n_flt (fishery slot unused); AI=fleet 2, EBS=fleet 3.
inits$index_log_q[2] <- log_q_srv[1]    # AI  -> log(1.06075)
inits$index_log_q[3] <- log_q_srv[2]    # EBS -> log(0.259452)

bridging_model_1 <- Rceattle::fit_mod(
  data_list    = mydata_fwd,
  inits        = inits,
  file         = NULL,
  estimateMode = 4,            # all parameters FIXED at inits (no estimation)
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = FALSE,
  initMode     = 1,            # unfished equilibrium + init devs (matches ADMB)
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 0)   # M fixed (sex-combined)
)

# -- VALIDATION: forward pass vs ADMB -----------------------------------------
fp <- data.frame(
  Year      = yrs,
  SSB_ratio = bridging_model_1$quantities$ssb[1, 1:nyr]     / admb_ref$SSB,
  Bio_ratio = bridging_model_1$quantities$biomass[1, 1:nyr] / admb_ref$Biomass,
  R_ratio   = bridging_model_1$quantities$R[1, 1:nyr]       / admb_ref$R
)
cat("\nForward pass vs ADMB - mean |%diff|:",
    "\n  SSB    ", round(100 * mean(abs(fp$SSB_ratio - 1)), 4), "%",
    "\n  Biomass", round(100 * mean(abs(fp$Bio_ratio - 1)), 4), "%",
    "\n  R      ", round(100 * mean(abs(fp$R_ratio   - 1)), 4), "%\n")


# =============================================================================
# Model 2 - ESTIMATION, M estimated with the ADMB lognormal prior
# -----------------------------------------------------------------------------
# Fishery selectivity -> non-parametric time-invariant (Selectivity=2; diff #1);
# surveys -> logistic (Selectivity=1; diff #2). M prior: mean 0.05, cv 0.05.
# =============================================================================
mydata_est <- mydata
# Fishery: non-parametric (Ianelli) selectivity, TIME-INVARIANT (diff #1 - the
# IID time-varying analog of the ADMB bicubic spline is not identifiable here).
# 18 age bins (not the full 38): the old-age coefficients are otherwise
# unidentified (no fish at age) and the Hessian is non-positive-definite. The
# fishery selectivity domes by ~age 20, so older ages are held flat at bin 18.
# Surveys: logistic, time-invariant (diff #2).
mydata_est$fleet_control$Selectivity        <- c(2, 1, 1)
mydata_est$fleet_control$Time_varying_sel   <- c(0, 0, 0)
mydata_est$fleet_control$N_sel_bins         <- c(18, NA, NA)
mydata_est$fleet_control$Bin_first_selected <- c(1, NA, NA)
mydata_est$emp_sel <- emp_sel_fwd[0, ]                       # no empirical sel

# AI survey q prior (diff #7): without it, free q x sel x N is unidentified and
# the population scale blows up. ADMB priors only the AI q (mean 1, cv 0.15);
# EBS q stays free. This anchors the scale and gives a positive-definite Hessian.
ai_row <- mydata_est$fleet_control$Fleet_name == "AI_survey"
mydata_est$fleet_control$Catchability[ai_row] <- "Estimated-with-prior"
mydata_est$fleet_control$Q_prior[ai_row]      <- 1
mydata_est$fleet_control$Q_sd_prior[ai_row]   <- 0.15

bridging_model_2 <- Rceattle::fit_mod(
  data_list    = mydata_est,
  inits        = NULL,
  file         = NULL,
  estimateMode = 0,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = TRUE,
  initMode     = 1,
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 1, M1_use_prior = TRUE,
                          M_prior = 0.05, M_prior_sd = 0.05)  # diff #6
)


# =============================================================================
# Model 3 - ESTIMATION, M fixed at the ADMB value (0.0506)
# =============================================================================
mydata_est3 <- mydata_est
mydata_est3$M1_base[grep("^Age", names(mydata_est3$M1_base))] <- M_admb
bridging_model_3 <- Rceattle::fit_mod(
  data_list    = mydata_est3,
  inits        = NULL,
  file         = NULL,
  estimateMode = 0,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = TRUE,
  initMode     = 1,
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 0)   # M fixed at 0.0506
)


# =============================================================================
# Overlay the ADMB ("SAFE") reference output and plot
# -----------------------------------------------------------------------------
# Build a pseudo-Rceattle object holding the ADMB series (same trick the rockfish
# and plaice bridges used). Rceattle and ADMB share units (no rescaling).
# =============================================================================
SAFE <- bridging_model_1
SAFE$quantities$ssb[1, 1:nyr]     <- admb_ref$SSB
SAFE$quantities$biomass[1, 1:nyr] <- admb_ref$Biomass
SAFE$quantities$R[1, 1:nyr]       <- admb_ref$R

mods  <- list(bridging_model_1, bridging_model_2, bridging_model_3, SAFE)
names <- c("Rceattle fix parms (fwd pass)", "Rceattle est (M prior)",
           "Rceattle est (M=0.0506)", "ADMB (SAFE)")

print(plot_biomass(mods, model_names = names) + ggplot2::ylab("Total biomass"))
print(plot_ssb(mods, model_names = names) + ggplot2::ylab("Female SSB"))
print(plot_recruitment(mods, model_names = names) + ggplot2::ylab("Recruitment"))
plot_selectivity(bridging_model_1)
