# =============================================================================
# 2021 BSAI Alaska plaice - Rceattle production run + comparison to ADMB
# =============================================================================
# Two-sex, single-species model (shelf survey + single fishery). Runs the
# Rceattle estimation model and compares SSB / total biomass / recruitment to
# the 2021 ADMB ("akplcss") assessment.
#
# The ADMB <-> Rceattle structural mapping, the data corrections applied in
# prep_plaice_data() below, and the reasons the estimation models do NOT match
# ADMB exactly are documented in "2021 BSAI alaska plaice bridging.R" (which
# also contains the FIXED-parameter forward pass that reproduces ADMB to ~6
# significant figures). Read that file first.
#
# DATA  (Data/plaice_single_species_2021.xlsx, Rceattle format)
# MODEL (ADMB akplcss): two sexes, R split 50:50 (each sex gets the full
#        exp(mean_log_rec)); sex-specific logistic fishery + survey selectivity
#        (survey normalized by max-across-sexes); survey q fixed at 1.2;
#        empirical sex-specific weight-at-age; M = 0.13 (both sexes).
# =============================================================================

library(Rceattle)
library(dplyr)
library(readxl)

# -----------------------------------------------------------------------------
# Data
# -----------------------------------------------------------------------------
mydata <- Rceattle::read_data(file = "Data/2021_BSAI_alaska_plaice.xlsx")
yrs <- mydata$styr:mydata$endyr
nyr <- length(yrs)


# =============================================================================
# Model 1 - estimation, M fixed at 0.13 (matches the ADMB configuration)
# =============================================================================
model1 <- Rceattle::fit_mod(
  data_list    = mydata,
  inits        = NULL,
  file         = NULL,
  estimateMode = 0,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = TRUE,
  initMode     = 2,
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 0)   # M fixed (sex-combined)
)


# =============================================================================
# ADMB ("SAFE") reference and comparison
# -----------------------------------------------------------------------------
# Rceattle and ADMB share units (no 1/1000 rescaling). Build a pseudo-Rceattle
# object that carries the ADMB series for plotting.
# =============================================================================
adm <- read_excel("Data/2021_ADMB_estimate.xlsx", sheet = 1)   # Year, R, SSB, Biomass

SAFE2021 <- model1
SAFE2021$quantities$ssb[1, 1:nyr]     <- adm$SSB
SAFE2021$quantities$biomass[1, 1:nyr] <- adm$Biomass
SAFE2021$quantities$R[1, 1:nyr]       <- adm$R

# -- numeric comparison (Rceattle Model 1 / ADMB) ------------------------------
comparison <- data.frame(
  Year     = yrs,
  SSB_ADMB = adm$SSB,      SSB_CEATTLE = model1$quantities$ssb[1, 1:nyr],
  Bio_ADMB = adm$Biomass,  Bio_CEATTLE = model1$quantities$biomass[1, 1:nyr],
  R_ADMB   = adm$R,        R_CEATTLE   = model1$quantities$R[1, 1:nyr]
)
comparison$SSB_pdiff <- 100 * (comparison$SSB_CEATTLE / comparison$SSB_ADMB - 1)
comparison$Bio_pdiff <- 100 * (comparison$Bio_CEATTLE / comparison$Bio_ADMB - 1)
comparison$R_pdiff   <- 100 * (comparison$R_CEATTLE   / comparison$R_ADMB   - 1)
print(round(comparison, 2))
cat("\nMean |%diff| (Rceattle est M=0.13 vs ADMB):",
    "SSB",     round(mean(abs(comparison$SSB_pdiff)), 2),
    "| Biomass", round(mean(abs(comparison$Bio_pdiff)), 2),
    "| R",       round(mean(abs(comparison$R_pdiff)), 2), "%\n")
cat("NOTE: early (pre-1982, no-survey) years diverge most - this is the\n",
    "recruitment-penalty bias-correction difference (bridging diff #2).\n")


# -- plots ---------------------------------------------------------------------
mods  <- list(model1, SAFE2021)
names <- c("Rceattle (M=0.13)", "ADMB (SAFE)")
plot_biomass(mods, model_names = names);
plot_ssb(mods, model_names = names);
plot_recruitment(mods, model_names = names)
