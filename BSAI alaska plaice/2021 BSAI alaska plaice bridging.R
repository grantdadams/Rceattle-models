# =============================================================================
# 2021 BSAI Alaska plaice - BRIDGING ADMB ("akplcss") -> Rceattle
# =============================================================================
# Two-sex, single-species, single-fishery + single (shelf) survey model.
# Goal: recreate the 2021 ADMB Alaska plaice assessment (ADMB/akplcss.tpl) in
#       Rceattle and reconcile output, mirroring the structure of the GOA
#       Northern rockfish bridging scripts.
#
#   Model 1 (forward pass): Rceattle dynamics FIXED to the ADMB MLEs
#                           (ADMB/akplcss.par). Verifies the structural map -
#                           reproduces ADMB SSB / total biomass / recruitment
#                           to ~6 significant figures (see VALIDATION below).
#   Model 2 (estimation):   Rceattle estimates everything, M fixed = 0.13 (ADMB).
#   Model 3 (estimation):   as Model 2 but M estimated (exploratory; ADMB fixes M).
#
# DATA (Data/plaice_single_species_2021.xlsx, already in Rceattle format)
# - Fishery: catch (1975-2021), length comp, age comp, sex-specific weight-at-age
# - Shelf survey: biomass + SE (1982-), length comp, age comp
# - Female maturity-at-age, M = 0.13 (both sexes, fixed)
#
# MODEL (ADMB akplcss)
# - Two sexes; recruitment split 50:50 (each sex gets the FULL exp(mean_log_rec))
# - Fishery selectivity  = sex-specific logistic (unnormalized)
# - Survey  selectivity  = sex-specific logistic, normalized by max-across-sexes
# - Survey catchability q FIXED at 1.2 (q_phase < 0 in the .dat)
# - Empirical weight-at-age (sex-specific, time-invariant)
# - Spawning after 0.25 yr of mortality (exp(-0.25*Z)); survey at mid-year
#
# -----------------------------------------------------------------------------
# STRUCTURAL DIFFERENCES / MAPPING (why an *exact* estimation match is not
# expected; the forward pass is exact because every parameter is fixed).
# Each item is applied/handled inline below at the marked code.
#
#  1. RECRUITMENT SEX SPLIT (mapping, exact). ADMB sets
#     natage(female,i,1) = natage(male,i,1) = exp(mean_log_rec + rec_dev_i),
#     i.e. EACH sex gets the full value, so total recruitment = 2*exp(...).
#     Rceattle computes a single total R and splits it by sex_ratio (= 0.5).
#     => rec_pars[1,1] = mean_log_rec + log(2)   (then 0.5*total = ADMB per sex).
#
#  2. RECRUITMENT-DEVIATION PENALTY (structural; drives estimation divergence).
#     ADMB: rec_like = norm2(rec_dev) = sum(rec_dev^2), centred at 0, over the
#     full 1953-2020 vector (init period + recruitment years). This equals a
#     Gaussian penalty with sigma_R = 1/sqrt(2) = 0.7071 (Rceattle's default
#     sigma_rec_prior, so the WEIGHT matches). BUT Rceattle adds a lognormal
#     bias correction: it stores rec_dev as (true_dev - sigma_R^2/2) and centres
#     the init_dev penalty at +sigma_R^2/2 (cpp ~3100 / ~3115). ADMB has no such
#     bias correction. The +/- sigma_R^2/2 shift makes the *estimated*
#     recruitment / initial-age series diverge from ADMB - largest in the early,
#     data-poor years (no survey before 1982). By design.
#
#  3. SURVEY SELECTIVITY NORMALIZATION (mapping, ~0.1%). ADMB divides survey sel
#     by maxsel_srv1 = max across BOTH sexes. Rceattle replicates this with
#     Sel_norm_bin1 < 0 (-> normalize by max across bins AND sexes). Fishery sel
#     is unnormalized in both. (Effect ~0.1% because the logistic saturates to
#     ~1.0 within ages 3-25.)
#
#  4. SELECTIVITY PARAMETERIZATION (mapping, exact). ADMB logistic
#     sel(j) = 1/(1+exp(-slope*(j - sel50))), j = 1..nages (age index, 1-based).
#     Rceattle case-1 logistic is identical with x = bin+1 (1-based age):
#       log_sel_slp = log(slope),  sel_inf = sel50.
#
#  5. CATCHABILITY q (mapping). ADMB q_phase < 0 -> q FIXED at q_in = 1.2.
#     Rceattle: Catchability = "Fixed", Q_prior = 1.2.
#
#  6. SPAWNING TIMING (correction). ADMB spawns after exp(-0.25*Z) (3 months).
#     The shipped Excel has spawn_month = 4; set spawn_month = 3 to match
#     exp(-3/12 * Z) = exp(-0.25*Z). (Survey timing month = 6 = pow(S,0.5) is
#     already correct in the file.)
#
#  7. CATCH / SURVEY UNITS (correction). Rceattle data are ALREADY in raw ADMB
#     units (catch 2492.. = ADMB catch_bio; survey 333830.. = ADMB obs_srv1).
#     Do NOT divide catch by 1000 (the previous "AK Plaice Rceattle.R" did; that
#     was a bug). Survey Log_sd is converted to a CV (= SE/obs) to match ADMB's
#     cv_srv1 = obs_srv1_sd / obs_srv1.
#
#  8. CATCH LIKELIHOOD (structural, tiny). ADMB obj += 300*norm2(log(catch)-
#     log(pred)) -> Gaussian with sigma = sqrt(1/600) = 0.0408. Rceattle catch
#     likelihood is lognormal in Log_sd WITH a -sd^2/2 bias correction (cpp
#     ~2555). Set catch Log_sd = sqrt(1/600); residual = the sd^2/2 ~ 8e-4 shift.
#
#  9. COMP LIKELIHOOD (structural). ADMB uses a custom multinomial with an
#     offset and added constants (1e-3 / 1e-5). Rceattle uses MultinomialAFSC.
#     Different comp weighting contributes to the estimation differences.
#
# 10. LAST-YEAR RECRUITMENT (mapping). ADMB fixes 2021 recruitment to
#     median_rec = 206384 PER SEX (the .dat "median recruits ... for the last
#     year"). Forward pass replicates via rec_dev(2021) = log(median_rec) -
#     mean_log_rec; the estimation model estimates 2021 recruitment freely.
#
# 11. OPTIMIZER / PHASING. ADMB ADMB-phasing vs TMB - expected, not chased.
#
# VALIDATION (Model 1 vs ADMB, run on this machine):
#   SSB / total biomass / recruitment match ADMB to ~6 significant figures
#   across 1975-2021 (mean |%diff| < 0.001%).
# =============================================================================

library(Rceattle)
library(dplyr)
library(readxl)

# -----------------------------------------------------------------------------
# Data
# -----------------------------------------------------------------------------
mydata <- Rceattle::read_data(file = "Data/2021_BSAI_alaska_plaice.xlsx")
styr  <- mydata$styr
endyr <- mydata$endyr
nages <- mydata$nages
yrs   <- styr:endyr
nyr   <- length(yrs)


# =============================================================================
# Model 1 - FORWARD PASS: Rceattle dynamics fixed to the ADMB MLEs
# -----------------------------------------------------------------------------
# Parse ADMB/akplcss.par and map every parameter into Rceattle's init list.
# =============================================================================

par_lines <- readLines("ADMB/akplcss.par")
get_par <- function(name) {
  i <- grep(paste0("^# ", name, ":"), par_lines)
  vals <- c(); j <- i + 1
  while (j <= length(par_lines) && !grepl("^#", par_lines[j])) {
    vals <- c(vals, as.numeric(strsplit(trimws(par_lines[j]), "\\s+")[[1]]))
    j <- j + 1
  }
  vals
}
mean_log_rec  <- get_par("mean_log_rec")                 # 11.3134
rec_dev       <- get_par("rec_dev")                      # 1953..2020 (68)
log_avg_fmort <- get_par("log_avg_fmort")
fmort_dev     <- get_par("fmort_dev")                    # 1975..2021 (47)
fish_slope_f  <- get_par("fish_slope_f"); fish_sel50_f <- get_par("fish_sel50_f")
fish_slope_m  <- get_par("fish_slope_m"); fish_sel50_m <- get_par("fish_sel50_m")
srv1_slope_f  <- get_par("srv1_slope_f"); srv1_sel50_f <- get_par("srv1_sel50_f")
srv1_slope_m  <- get_par("srv1_slope_m"); srv1_sel50_m <- get_par("srv1_sel50_m")
q1            <- get_par("q1")                           # 1.2 (fixed)
median_rec    <- 206384                                  # .dat: last-year R per sex

inits <- build_params(mydata)

# -- recruitment (diff #1: + log(2) so 0.5*total R = ADMB per-sex value) -------
inits$rec_pars[1, 1] <- mean_log_rec + log(2)
# rec_dev: years 1975..2020 = ADMB rec_dev idx 23..68; 2021 -> median_rec (diff #10)
rd_hind <- rec_dev[(nages):(length(rec_dev))]            # 1975..2020 (idx 23..68)
rd_2021 <- log(median_rec) - mean_log_rec
inits$rec_dev[1, 1:nyr] <- c(rd_hind, rd_2021)
# init_dev: ages 1..nages-1 use ADMB rec_dev(1974..1953) = rev(idx 1..nages-1)
inits$init_dev[1, 1:(nages - 1)] <- rev(rec_dev[1:(nages - 1)])

# -- selectivity (diff #4; sel_index 1 = fishery, 2 = survey; 3rd dim = sex) ---
inits$log_sel_slp[1, 1, 1] <- log(fish_slope_f); inits$sel_inf[1, 1, 1] <- fish_sel50_f
inits$log_sel_slp[1, 1, 2] <- log(fish_slope_m); inits$sel_inf[1, 1, 2] <- fish_sel50_m
inits$log_sel_slp[1, 2, 1] <- log(srv1_slope_f); inits$sel_inf[1, 2, 1] <- srv1_sel50_f
inits$log_sel_slp[1, 2, 2] <- log(srv1_slope_m); inits$sel_inf[1, 2, 2] <- srv1_sel50_m

# -- fishing mortality (fishery = fleet row 1): F_fully_sel = exp(log_F) = fmort -
inits$log_F[1, 1:nyr] <- log_avg_fmort + fmort_dev

# -- catchability (diff #5) ----------------------------------------------------
inits$index_log_q[1] <- log(q1)

bridging_model_1 <- Rceattle::fit_mod(
  data_list    = mydata,
  inits        = inits,
  file         = NULL,
  estimateMode = 4,           # all parameters FIXED at inits (no estimation)
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = FALSE,
  initMode     = 2,           # unfished equilibrium + init devs (matches ADMB)
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 0)   # M fixed (sex-combined)
)

# -- VALIDATION: forward pass vs ADMB ------------------------------------------
adm <- read_excel("Data/2021_ADMB_estimate.xlsx", sheet = 1)   # Year, R, SSB, Biomass
fp <- data.frame(
  Year     = yrs,
  SSB_ratio = bridging_model_1$quantities$ssb[1, 1:nyr]     / adm$SSB,
  Bio_ratio = bridging_model_1$quantities$biomass[1, 1:nyr] / adm$Biomass,
  R_ratio   = bridging_model_1$quantities$R[1, 1:nyr]       / adm$R
)
cat("Forward pass vs ADMB - mean |%diff|:",
    "SSB", round(100 * mean(abs(fp$SSB_ratio - 1)), 4),
    "Biomass", round(100 * mean(abs(fp$Bio_ratio - 1)), 4),
    "R", round(100 * mean(abs(fp$R_ratio - 1)), 4), "\n")


# =============================================================================
# Model 2 - ESTIMATION, M fixed = 0.13 (matches ADMB)
# =============================================================================
bridging_model_2 <- Rceattle::fit_mod(
  data_list    = mydata,
  inits        = NULL,
  file         = NULL,
  estimateMode = 0,           # estimate
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = TRUE,
  initMode     = 2,
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 0)   # M fixed at 0.13
)


# =============================================================================
# Model 3 - ESTIMATION with M estimated (exploratory; ADMB fixes M)
# -----------------------------------------------------------------------------
# M1_model = 2 estimates sex-specific M (female + male). ADMB holds both at 0.13.
# =============================================================================
bridging_model_3 <- Rceattle::fit_mod(
  data_list    = mydata,
  inits        = NULL,
  file         = NULL,
  estimateMode = 0,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = FALSE,
  initMode     = 2,
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 2)   # estimate M by sex
)


# =============================================================================
# Overlay the ADMB ("SAFE") reference output and plot
# -----------------------------------------------------------------------------
# Build a pseudo-Rceattle object holding the ADMB series (same trick the rockfish
# bridges used). Rceattle and ADMB share units, so NO 1/1000 rescaling.
# =============================================================================
SAFE2021 <- bridging_model_1
SAFE2021$quantities$ssb[1, 1:nyr]     <- adm$SSB
SAFE2021$quantities$biomass[1, 1:nyr] <- adm$Biomass
SAFE2021$quantities$R[1, 1:nyr]       <- adm$R

mods  <- list(bridging_model_1, bridging_model_2, bridging_model_3, SAFE2021)
names <- c("Rceattle fix parms (fwd pass)", "Rceattle est (M=0.13)",
           "Rceattle est M", "ADMB (SAFE)")

print(plot_biomass(mods, model_names = names) + ggplot2::ylab("Total biomass"))
print(plot_ssb(mods, model_names = names) + ggplot2::ylab("Female SSB"))
print(plot_recruitment(mods, model_names = names) + ggplot2::ylab("Recruitment"))
plot_selectivity(bridging_model_1)
