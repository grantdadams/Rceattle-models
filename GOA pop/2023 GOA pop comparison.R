# =============================================================================
# 2023 GOA Pacific ocean perch (POP) - Rceattle ESTIMATION vs ADMB comparison
# =============================================================================
# Runs the Rceattle estimation model (the bridging Model 2: M fixed at the ADMB
# MLE, non-parametric selectivity) and compares its output to the 2023 ADMB
# assessment (model_20_1). Produces side-by-side time-series tables, summary
# %-difference statistics, and overlay plots.
#
# Companion to "2023 GOA pop bridging.R", which also contains the forward-pass
# validation (Rceattle dynamics fixed to the ADMB MLEs) and the full list of
# structural differences between the two platforms.
#
# The forward pass reproduces ADMB recruitment / total biomass / SSB to ~6
# significant figures. The ESTIMATION model is NOT expected to match exactly.
# We report the CLOSEST estimation model (bridging Model 3: logistic selectivity,
# M fixed), which lands within ~15-20% of ADMB, and also the faithful-form model
# (bridging Model 2: non-parametric, ~55%). The remaining gap is driven by the
# documented structural differences, chiefly:
#   * fishery selectivity: single time-invariant ogive in Rceattle vs 4 discrete
#     ADMB time blocks (Rceattle non-parametric supports only IID/None time
#     variation, not blocks; a logistic ogive is more constrained and recovers a
#     survey q ~1.46 closer to ADMB's 1.74);
#   * sigmaR (matched to the ADMB 0.7644 estimate here);
#   * the 1961 (foreign-fishery) recruitment is essentially unconstrained by
#     composition data, so Rceattle estimates a large terminal-init cohort;
#   * composition likelihood weighting (Rceattle MultinomialAFSC vs ADMB);
#   * TMB vs ADMB optimizer / phasing.
# =============================================================================

library(Rceattle)
library(readxl)
library(dplyr)

setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA pop")

# Build the bridging models (forward pass + estimation). Suppress its plots.
pdf(NULL); source("2023 GOA pop bridging.R"); dev.off()

# -----------------------------------------------------------------------------
# Assemble ADMB vs Rceattle time series. Headline = Model 3 (logistic, closest).
# -----------------------------------------------------------------------------
est <- bridging_model_3      # logistic selectivity, M fixed (closest to ADMB)
q   <- est$quantities

# Quick faithful-form vs closest-form summary
np <- bridging_model_2$quantities
cat("\nEstimation selectivity-form comparison (mean |%diff| vs ADMB):\n")
cat(sprintf("  Model 2 non-parametric (ADMB form): Biomass %.0f%%  SSB %.0f%%\n",
    mean(abs(100 * (np$biomass[1, 1:nyr] / admb_Btot - 1))),
    mean(abs(100 * (np$ssb[1, 1:nyr]     / admb_SSB  - 1)))))
cat(sprintf("  Model 3 logistic (closest)        : Biomass %.0f%%  SSB %.0f%%\n",
    mean(abs(100 * (q$biomass[1, 1:nyr]  / admb_Btot - 1))),
    mean(abs(100 * (q$ssb[1, 1:nyr]      / admb_SSB  - 1)))))

admb <- data.frame(Year = yrs, R = admb_R, SSB = admb_SSB, Biomass = admb_Btot, F = admb_Fsel)

# Rceattle fully-selected F = max F-at-age across ages each year (fishery)
rce_F <- apply(q$F_at_age[1, 1, 1:nage, 1:nyr], 2, max)

cmp <- data.frame(
  Year      = yrs,
  R_admb    = admb$R,        R_rce    = q$R[1, 1:nyr],
  SSB_admb  = admb$SSB,      SSB_rce  = q$ssb[1, 1:nyr],
  Bio_admb  = admb$Biomass,  Bio_rce  = q$biomass[1, 1:nyr],
  F_admb    = admb$F,        F_rce    = rce_F)
cmp$R_pdiff   <- 100 * (cmp$R_rce   / cmp$R_admb   - 1)
cmp$SSB_pdiff <- 100 * (cmp$SSB_rce / cmp$SSB_admb - 1)
cmp$Bio_pdiff <- 100 * (cmp$Bio_rce / cmp$Bio_admb - 1)

pd <- function(x) round(mean(abs(x)), 2)
cat("\n=============================================================\n")
cat("Rceattle estimation (Model 3: logistic, M fixed) vs ADMB - mean |%diff|\n")
cat("=============================================================\n")
cat(sprintf("  Recruitment  : %6.2f %%\n", pd(cmp$R_pdiff)))
cat(sprintf("  SSB          : %6.2f %%\n", pd(cmp$SSB_pdiff)))
cat(sprintf("  Total biomass: %6.2f %%\n", pd(cmp$Bio_pdiff)))
cat(sprintf("  Terminal (2023) SSB:      ADMB %.0f   Rceattle %.0f\n",
            tail(cmp$SSB_admb, 1), tail(cmp$SSB_rce, 1)))
cat(sprintf("  Terminal (2023) biomass:  ADMB %.0f   Rceattle %.0f\n",
            tail(cmp$Bio_admb, 1), tail(cmp$Bio_rce, 1)))

cat("\nKey estimated parameters (Rceattle est. vs ADMB MLE):\n")
cat(sprintf("  q  (trawl survey): Rceattle %.3f   ADMB 1.736\n", est$quantities$index_q[1, 1]))
cat(sprintf("  M               : fixed     0.0743  ADMB 0.0743 (estimated)\n"))
cat("  NOTE: The residual biomass/SSB gap reflects the documented structural\n")
cat("  differences (single fishery sel block vs 4 ADMB blocks; logistic vs the\n")
cat("  ADMB non-parametric form; unconstrained 1961 recruitment; comp weighting;\n")
cat("  TMB vs ADMB optimizer), NOT a mapping error - the forward pass reproduces\n")
cat("  ADMB to ~6 significant figures.\n")

cat("\nFirst & last 5 years (R / SSB / Biomass; admb | rce):\n")
print(round(head(cmp[, c("Year","R_admb","R_rce","SSB_admb","SSB_rce","Bio_admb","Bio_rce")], 5), 0))
print(round(tail(cmp[, c("Year","R_admb","R_rce","SSB_admb","SSB_rce","Bio_admb","Bio_rce")], 5), 0))

write.csv(cmp, "Data/2023_GOApop_Rceattle_vs_ADMB.csv", row.names = FALSE)
cat("\nWrote Data/2023_GOApop_Rceattle_vs_ADMB.csv\n")

# -----------------------------------------------------------------------------
# Overlay plots: forward pass, estimation models, and ADMB
# -----------------------------------------------------------------------------
mods  <- list(bridging_model_1, bridging_model_2, bridging_model_3, bridging_model_4, SAFE2023)
names <- c("Rceattle fix parms (fwd pass)", "Rceattle est non-par (M=0.074)",
           "Rceattle est logistic (M=0.074)", "Rceattle est logistic M", "ADMB (SAFE)")

pdf("Data/2023_GOApop_Rceattle_vs_ADMB.pdf", width = 8, height = 6)
print(plot_biomass(mods, model_names = names) + ggplot2::ylab("Total biomass"))
print(plot_ssb(mods, model_names = names) + ggplot2::ylab("Female SSB"))
print(plot_recruitment(mods, model_names = names) + ggplot2::ylab("Recruitment"))
plot_selectivity(bridging_model_3)
dev.off()
cat("Wrote Data/2023_GOApop_Rceattle_vs_ADMB.pdf\n")
