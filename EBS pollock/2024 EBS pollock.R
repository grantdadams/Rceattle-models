# =============================================================================
# 2024 EBS pollock assessment in Rceattle (CEATTLE)
# =============================================================================
# Single-sex, single-species model with one fishery and several surveys
# (AVO acoustic index, BTS bottom-trawl survey, ATS acoustic-trawl survey, and
# the BTS/ATS age-1 indices).
#
# DATA (Data/BSP0.xlsx)
# - Fishery catch + fishery age composition + fishery weight-at-age
# - BTS (bottom trawl) and ATS (acoustic trawl) biomass indices + age comps
# - AVO acoustic index; BTS_1 / ATS_1 age-1 abundance indices
# - Empirical weight-at-age (fishery / BTS / ATS / AVO / SSB) and maturity
#
# MODEL
# - One sex (sex_ratio = 0.5 -> female SSB)
# - Mean recruitment with init devs; empirical weight-at-age
# - M age schedule 0.9 (age1), 0.45 (age2), 0.3 (age3-15); fixed or estimated
# - Fishery selectivity = Hake non-parametric (Selectivity 5); survey
#   selectivity = logistic; AVO selectivity mirrored to ATS
#
# The 2024 reference model is the ADMB "pm" assessment (./ADMB/m23/, pm.tpl).
# See "2024 EBS pollock bridging.R" for the forward-pass validation (SSB / catch
# reproduced to ~6 sig figs) and the full list of structural differences between
# the ADMB and Rceattle codebases.
# =============================================================================

library(Rceattle)
library(dplyr)
library(readxl)

# -----------------------------------------------------------------------------
# Load + prepare data
# -----------------------------------------------------------------------------
mydata <- Rceattle::read_data(file = "Data/2024_EBS_pollock.xlsx")
yrs   <- mydata$styr:mydata$endyr
nages <- mydata$nages

# Survey catchability: ADMB solves BTS/ATS q analytically (q = mean(obs)/mean(pred));
# Rceattle Catchability = 3 ("Analytical", Ludwig & Walters 1994) is the analog.
# (BTS/ATS survey weight-at-age match ADMB wt_bts/wt_ats exactly via Weight_index
# 3 / 4.) AVO keeps its estimated log_q.

# Selectivity to mirror the ADMB ("pm" = Ianelli AMAK) structure:
# - Fishery: Ianelli (2018) non-parametric selectivity with annual IID deviations
#   (= ADMB sel_coffs_fsh + sel_devs_fsh). This is the single biggest lever for
#   matching the ADMB trajectory (SSB correlation 0.88 -> 0.999).
# - BTS: logistic with a random walk over time (= ADMB sel_slp_bts/sel_a50_bts
#   + their annual deviations).
mydata$fleet_control$Selectivity[mydata$fleet_control$Fleet_name == "Fishery"]      <- "NonParametric"
mydata$fleet_control$Time_varying_sel[mydata$fleet_control$Fleet_name == "Fishery"] <- "IID"
mydata$fleet_control$Time_varying_sel[mydata$fleet_control$Fleet_name == "BTS"]     <- "RandomWalk"

# - Look at the data
# plot_data(mydata)


# -----------------------------------------------------------------------------
# Model 1 - estimate, M fixed at the ADMB age schedule (0.9/0.45/0.3)
# -----------------------------------------------------------------------------
pollock_base <- Rceattle::fit_mod(
  data_list    = mydata,
  inits        = NULL,
  file         = NULL,
  estimateMode = 0,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = TRUE,
  initMode     = 0,   # free initial N-at-age (= ADMB log_avginit + log_initdevs)
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 0)   # M fixed at age schedule
)



# -----------------------------------------------------------------------------
# Model 2 - estimate age/time-invariant M
# -----------------------------------------------------------------------------
pollock_estM <- Rceattle::fit_mod(
  data_list    = mydata,
  inits        = NULL,
  file         = NULL,
  estimateMode = 0,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = TRUE,
  initMode     = 0,   # free initial N-at-age (= ADMB log_avginit + log_initdevs)
  M1Fun        = build_M1(M1_model = 1)   # estimate a single scalar M
)


# -----------------------------------------------------------------------------
# ADMB ("SAFE") reference and comparison
# -----------------------------------------------------------------------------
# Build a pseudo-Rceattle object holding the ADMB SSB / recruitment series
# (Data/2024_ADMB_estimate.xlsx) so it can be overlaid on the Rceattle output.
adm_ssb <- as.data.frame(read_excel("Data/2024_ADMB_estimate.xlsx", sheet = "SSB"))
adm_r   <- as.data.frame(read_excel("Data/2024_ADMB_estimate.xlsx", sheet = "Recruitment"))

SAFE2024 <- pollock_base
SAFE2024$quantities$ssb[1, 1:length(yrs)] <- adm_ssb$Est
SAFE2024$quantities$R[1, 1:length(yrs)]   <- adm_r$Est

mods  <- list(pollock_base, SAFE2024)
names <- c("CEATTLE (M fixed)", "ADMB (SAFE)")

plot_ssb(mods, model_names = names)
plot_recruitment(mods, model_names = names)
plot_biomass(mods, model_names = names)

# Quantitative comparison (Model 1 vs ADMB SAFE)
ssb <- pollock_base$quantities$ssb[1, 1:length(yrs)]
R   <- pollock_base$quantities$R[1, 1:length(yrs)]
cat("\n--- CEATTLE (M fixed) vs ADMB SAFE ---\n")
cat("SSB correlation:", round(cor(ssb, adm_ssb$Est), 4),
    " mean |%diff|:", round(100 * mean(abs(ssb / adm_ssb$Est - 1)), 1), "%\n")
cat("R   correlation:", round(cor(R[-1], adm_r$Est[-1]), 4),
    " mean |%diff|:", round(100 * mean(abs(R[-1] / adm_r$Est[-1] - 1)), 1), "%\n")
