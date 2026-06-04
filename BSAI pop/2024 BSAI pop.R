# =============================================================================
# 2024 BSAI Pacific ocean perch (POP) - Rceattle production run + ADMB comparison
# =============================================================================
# Single-sex (sexes-combined), single-species model: single fishery + AI bottom-
# trawl survey + EBS slope survey. Runs the Rceattle estimation model and
# compares SSB / total biomass / recruitment to the 2024 ADMB ("pop24" / m_24_2)
# assessment.
#
# The ADMB <-> Rceattle structural mapping, the data corrections applied below,
# and the reasons the estimation model does NOT match ADMB exactly are documented
# in "2024 BSAI pop bridging.R" (which also contains the FIXED-parameter forward
# pass that reproduces ADMB SSB / biomass / recruitment to ~6 significant
# figures). Read that file first.
#
# DATA  (Data/bsai_pop_single_species_2024.xlsx, Rceattle format)
# MODEL (ADMB pop24): single sex (SSB uses natage/2); bicubic-spline time-varying
#        fishery selectivity (-> Rceattle non-parametric); logistic survey
#        selectivity; AI q estimated w/ prior (mean 1, cv 0.15), EBS q estimated;
#        M estimated w/ lognormal prior (mean 0.05, cv 0.05); empirical weight-
#        at-age; recruitment random about a single mean.
# =============================================================================

library(Rceattle)
library(dplyr)
library(readxl)

admb_dir <- "2024 assessment"

# -----------------------------------------------------------------------------
# Data
# (spawn_month=3, SD->CV survey errors, fixed fleet types/species, non-parametric
#  fishery + logistic survey selectivity are already baked into the Excel.)
# -----------------------------------------------------------------------------
mydata <- Rceattle::read_data(file = "2024_BSAI_pop.xlsx")
yrs <- mydata$styr:mydata$endyr
nyr <- length(yrs)

# -- configuration needed for the estimation model to be IDENTIFIED -------------
# 1) AI SURVEY CATCHABILITY PRIOR. The Excel leaves q "Estimated" (free). With no
#    prior, q x selectivity x N is unidentified and the population scale runs
#    away (SSB -> 1e12). ADMB anchors the scale with a lognormal prior on the AI
#    survey q (mean 1, cv 0.15); the EBS q stays free (as in ADMB). Restoring the
#    AI prior is enough to identify the model.
ai <- mydata$fleet_control$Fleet_name == "AI_survey"
mydata$fleet_control$Catchability[ai] <- "Estimated-with-prior"
mydata$fleet_control$Q_prior[ai]      <- 1
mydata$fleet_control$Q_sd_prior[ai]   <- 0.15

# 2) NON-PARAMETRIC SELECTIVITY BINS. 38 age bins leaves the old-age coefficients
#    (ages with ~no fish) unidentified -> non-positive-definite Hessian. The
#    fishery selectivity domes by ~age 20, so estimate 18 bins and hold older
#    ages flat at bin 18. This yields a PD Hessian (clean convergence).
mydata$fleet_control$N_sel_bins[mydata$fleet_control$Fleet_name == "pop_fishery"] <- 18


# =============================================================================
# Model 1 - estimation, M estimated with the ADMB lognormal prior (mean 0.05,
#           cv 0.05). This is the configuration comparable to the ADMB model.
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
  initMode     = 1,        # ADMB starts at an UNFISHED equilibrium (historic_F=0);
                           # initMode=2 would estimate a spurious initial Finit.
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 1, M1_use_prior = TRUE,
                          M_prior = 0.05, M_prior_sd = 0.05)
)

# Convergence check
cat("Converged (PD Hessian):", isTRUE(model1$sdrep$pdHess),
    "| max gradient:", signif(max(abs(model1$sdrep$gradient.fixed)), 3), "\n")

# =============================================================================
# ADMB ("SAFE") reference and comparison
# -----------------------------------------------------------------------------
# Rceattle and ADMB share units (no rescaling). Build a pseudo-Rceattle object
# that carries the ADMB series for plotting.
# =============================================================================
admb <- dget(file.path(admb_dir, "m_24_2_reweighted.rdat"))
ts   <- admb$t.series
adm  <- data.frame(
  Year    = as.numeric(ts[, "year"]),
  R       = as.numeric(ts[, "a3recs"]),
  SSB     = as.numeric(ts[, "spbiom"]),
  Biomass = as.numeric(ts[, "totbiom"])
)
adm <- adm[adm$Year %in% yrs, ]

SAFE <- model1
SAFE$quantities$ssb[1, 1:nyr]     <- adm$SSB
SAFE$quantities$biomass[1, 1:nyr] <- adm$Biomass
SAFE$quantities$R[1, 1:nyr]       <- adm$R

# -- numeric comparison (Rceattle Model 1 / ADMB) -----------------------------
comparison <- data.frame(
  Year = yrs,
  SSB_ADMB = adm$SSB,     SSB_CEATTLE = model1$quantities$ssb[1, 1:nyr],
  Bio_ADMB = adm$Biomass, Bio_CEATTLE = model1$quantities$biomass[1, 1:nyr],
  R_ADMB   = adm$R,       R_CEATTLE   = model1$quantities$R[1, 1:nyr]
)
comparison$SSB_pdiff <- 100 * (comparison$SSB_CEATTLE / comparison$SSB_ADMB - 1)
comparison$Bio_pdiff <- 100 * (comparison$Bio_CEATTLE / comparison$Bio_ADMB - 1)
comparison$R_pdiff   <- 100 * (comparison$R_CEATTLE   / comparison$R_ADMB   - 1)
print(round(comparison, 2))
cat("\nMean |%diff| (Rceattle est vs ADMB):",
    "SSB",       round(mean(abs(comparison$SSB_pdiff)), 2),
    "| Biomass", round(mean(abs(comparison$Bio_pdiff)), 2),
    "| R",       round(mean(abs(comparison$R_pdiff)),   2), "%\n")
cat("NOTE: SSB / total biomass agree to ~17-20%. Residual differences are the\n",
    "documented structural ones (bridging diffs #1 fishery selectivity form -\n",
    "time-invariant 18-bin non-parametric vs ADMB time-varying bicubic spline,\n",
    "#10 survey index prediction, #11 comp weighting, #12 optimizer/phasing)\n",
    "plus an estimated M of ~0.047 vs ADMB 0.051. Year-to-year RECRUITMENT is\n",
    "the most sensitive quantity and diverges most (as in the rockfish/plaice\n",
    "bridges); the FORWARD PASS (bridging script) reconciles recruitment exactly.\n")


# -- plots ---------------------------------------------------------------------
mods  <- list(model1, SAFE)
names <- c("Rceattle (est M)", "ADMB (SAFE)")
plot_biomass(mods, model_names = names)
plot_ssb(mods, model_names = names)
plot_recruitment(mods, model_names = names)
plot_selectivity(model1)
