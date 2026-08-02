# =============================================================================
# Bridging the 2021 GOA pollock stock assessment from WHAM into Rceattle
# =============================================================================
# GOAL.
# 1) Confirm that Rceattle reproduces the accepted WHAM ("pkwham") 2021 GOA
# pollock model when BOTH models are held at the SAME parameter values. The models
# do not converge to the same solution because of a structural difference in how
# each initializes the population.
# 2) Confirm their one-step-ahead (OSA) residuals are identical.
#
#
# WORKFLOW.
#   1. Map WHAM's converged parameters onto Rceattle's parameterization.
#     - "Data/2021 Pollock WHAM.R" - create WHAM model
#     - "2021 pollock update data.R"    - create Rceattle data object
#   2. Hold Rceattle at that solution and confirm the population dynamics
#      (numbers-at-age, SSB, F, catch, survey indices, selectivity) match WHAM.
#   3. Compute OSA residuals from each model and compare them 1:1.
#     - "2021 pollock bridging.R"        # this script
#
# Same idea as tests/comparison/WHAM-OSA-comparison.R.

library(Rceattle)
library(wham)

# -----------------------------------------------------------------------------
# 0. Load the models' inputs ----
# -----------------------------------------------------------------------------
# Anchor to the model folder so the relative paths resolve.
setwd("~/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA pollock")

load("Data/2021pollock_wham.Rdata")        # WHAM 'fit': $opt $rep $parList $input
load("Data/2021pollock_rceattle.Rdata")    # Rceattle data list 'pollock21'

years <- pollock21$styr:pollock21$endyr    # 1970-2021
nyrs  <- length(years)                     # 52
nages <- pollock21$nages                   # ages 1-10
FISH  <- 7L                                # fleet 7 = fishery; 1-6 = surveys
M     <- exp(fit$input$par$M_a)            # fixed natural-mortality-at-age
d     <- fit$input$data
pl    <- fit$parList                       # WHAM's converged parameters

# Rceattle fleets:
#   - 1-6 = surveys 1-6 (Shelikof, NMFS BT, ADF&G, age-1, age-2, summer AT)
#   - 7   = fishery
# WHAM fleets:
#   - 1 = fishery
#   - 2-7 = surveys 1-6
wham_blk   <- c(2, 3, 4, 5, 6, 7, 1)               # Rceattle fleet -> WHAM block
wham_label <- c(paste0("index_", 1:6), "fleet_1")  # Rceattle fleet -> WHAM OSA label

# Surveys 4, 5, 6 have selectivity fixed a priori
pollock21$emp_sel <- pollock21$emp_sel[pollock21$emp_sel$Fleet_code %in% 4:6, ]


# -----------------------------------------------------------------------------
# 1. Map WHAM's converged parameters onto Rceattle's parameterization
# -----------------------------------------------------------------------------
# Build an un-optimized Rceattle object (estimateMode = 3) to get the
# parameter skeleton (`inits`) with the right shapes
nullmod  <- fit_mod(data_list = pollock21,
                    inits = NULL,
                    estimateMode = 3,
                    random_rec = FALSE,
                    msmMode = 0,
                    initMode = 1,
                    fit_control = fit_control(phase = FALSE, verbose = 0))
inits <- nullmod$estimated_params

# -- 1a. Recruitment and initial numbers-at-age -------------------------------
# WHAM initializes unfished equilibrium in 1970 (F = 0 that year):
# age-1 abundance is a free parameter (log_N1_pars[1]) and older ages decay at
# natural mortality from the mean recruitment R0 = exp(mean_rec_pars).
#   - WHAM only uses age1 M (not age-varying)
#   - WHAM fixes the recruitment SD at 1
inits$rec_pars[1, 1]             <- pl$mean_rec_pars
inits$rec_dev[1, 1]              <- pl$log_N1_pars[1] - pl$mean_rec_pars
inits$rec_dev[1, 2:nyrs]         <- pl$log_NAA[, 1] - pl$mean_rec_pars
inits$init_dev[1, 1:(nages - 1)] <- M[1] - M[2:nages]
inits$R_log_sd                   <- pl$log_NAA_sigma
inits$log_Finit[1]               <- -Inf              # F = 0 in the first year

# -- 1b. Fishing mortality ----------------------------------------------------
# WHAM models fishing mortality as a random walk
inits$log_F[FISH, 1] <- pl$log_F1
for (y in 2:nyrs)
  inits$log_F[FISH, y] <- inits$log_F[FISH, y - 1] + pl$F_devs[y - 1, 1]

# -- 1c. Selectivity ----------------------------------------------------------
# WHAM's survey/fishery selectivities come in two forms:
#   * Shelikof (survey 1): age-specific, ages 1-2 fixed at 0 -> Rceattle
#     non-parametric selectivity (the data list zeros ages 1-2 and max-normalizes
#     so the scale matches WHAM's saturating age-specific curve).
#   * NMFS bottom trawl (survey 2), ADF&G (survey 3), and the fishery (fleet 7):
#     double-logistic. WHAM parameterizes each limb by an inflection age (a) and a
#     width (b); Rceattle uses the same inflection (sel_inf) and a log-slope
#     (log_sel_slp = -log(b)). WHAM stores a, b on a bounded logit scale.
#
# WHAM double-logistic decode: sel_par = lower + (upper-lower)/(1+exp(-(logit+re)))
wham_sel <- function(block, col, re = 0)
  d$selpars_lower[block, col] +
  (d$selpars_upper[block, col] - d$selpars_lower[block, col]) /
  (1 + exp(-(pl$logit_selpars[block, col] + re)))

# Shelikof non-parametric: log-selectivity at ages 3-10 (ages 1-2 are zeroed by
# Bin_first_selected = 3).
inits$sel_coff[1, 1, 3:nages] <- log(fit$rep$selAA[[2]][1, 3:nages])

# NMFS BT (fleet 2, WHAM block 3) and ADF&G (fleet 3, WHAM block 4): both limbs.
for (fb in list(c(2, 3), c(3, 4))) {
  fleet <- fb[1]; block <- fb[2]
  inits$sel_inf[1, fleet, 1]     <- wham_sel(block, 13)          # ascending a1
  inits$log_sel_slp[1, fleet, 1] <- -log(wham_sel(block, 14))    # ascending b1
  inits$sel_inf[2, fleet, 1]     <- wham_sel(block, 15)          # descending a2
  inits$log_sel_slp[2, fleet, 1] <- -log(wham_sel(block, 16))    # descending b2
}

# Fishery (fleet 7, WHAM block 1): double-logistic with time-varying on the
# ASCENDING limb only. WHAM stores these as selpars_re:
# elements 1:52 are the a1 deviations, 53:104 the b1 deviations.
a1 <- wham_sel(1, 13); b1 <- wham_sel(1, 14)
inits$sel_inf[1, FISH, 1]     <- a1
inits$log_sel_slp[1, FISH, 1] <- -log(b1)
inits$sel_inf[2, FISH, 1]     <- wham_sel(1, 15)
inits$log_sel_slp[2, FISH, 1] <- -log(wham_sel(1, 16))
for (y in 1:nyrs) {
  a1_y <- wham_sel(1, 13, pl$selpars_re[y])
  b1_y <- wham_sel(1, 14, pl$selpars_re[nyrs + y])
  inits$sel_inf_dev[1, FISH, 1, y]     <- a1_y - a1
  inits$log_sel_slp_dev[1, FISH, 1, y] <- -log(b1_y) + log(b1)
}

# -- 1d. Catchability ---------------------------------------------------------
# WHAM applies survey catchability on the logit scale; Rceattle on the log scale
# (index q = exp(index_log_q + index_q_dev)).
#   Shelikof: Rceattle max-normalizes the non-parametric selectivity,
#   but WHAM's Shelikof normalizes at max = 0.99999935, so
#   normalizing inflates its selectivity by 1/max.
inits$index_log_q[1:6] <- 0
for (i in 1:6) inits$index_q_dev[i, ] <- log(fit$rep$q[, i])
inits$index_q_dev[1, ] <- log(fit$rep$q[, 1] * max(fit$rep$selAA[[2]][1, ]))

# -- 1e. Fix EVERY parameter at the WHAM solution -----------------------------
map_fixed <- nullmod$map
map_fixed$mapList <- lapply(map_fixed$mapList, function(x) { x[] <- NA; x })
map_fixed$mapList$dummy <- 1 # leave the inert `dummy` free
map_fixed$mapFactor <- lapply(map_fixed$mapList, factor)

# -----------------------------------------------------------------------------
# 2. Hold Rceattle at the WHAM solution and confirm the dynamics match
# -----------------------------------------------------------------------------
# fit_control to make Rceattle's likelihood match WHAM's conventions:
#   * bias_adjust_obs = 0 :
#   * comp_offset = 0     :
pk <- fit_mod(data_list = pollock21,
              inits = inits,
              map = map_fixed,
              estimateMode = 0,
              random_rec = FALSE,
              msmMode = 0,
              initMode = 1,
              fit_control = fit_control(phase = FALSE,
                                        verbose = 0,
                                        comp_offset = 0,
                                        bias_adjust_obs = 0,
                                        getsd = FALSE))
q <- pk$quantities

# * Compare ----
cat("\n=== Rceattle at the WHAM solution: max |relative difference| vs WHAM ===\n")
cat(sprintf("  SSB              : %.2e\n", max(abs(q$ssb[1, 1:nyrs] / fit$rep$SSB - 1))))
cat(sprintf("  fishing mortality: %.2e\n", max(abs(q$F_spp[1, 1:nyrs] / fit$rep$F[, 1] - 1))))
cat(sprintf("  catch            : %.2e\n", max(abs(q$catch_hat[1:nyrs] / fit$rep$pred_catch[, 1] - 1))))
index_hat <- matrix(q$index_hat[1:(nyrs * 6)], nyrs, 6)
for (i in 1:6)
  cat(sprintf("  survey %d index   : %.2e\n", i,
              max(abs(index_hat[, i] / fit$rep$pred_indices[, i] - 1))))
cat("Selectivity-at-age (year 1), max |difference| vs WHAM by fleet:\n")
for (i in 1:7)
  cat(sprintf("  fleet %d          : %.2e\n", i,
              max(abs(q$sel_at_age[i, 1, , 1] - fit$rep$selAA[[wham_blk[i]]][1, ]))))


# -----------------------------------------------------------------------------
# 3. One-step-ahead (OSA) residuals from each model
# -----------------------------------------------------------------------------
# Both models are fixed-effects here (WHAM input$random = NULL; Rceattle
# random_rec = FALSE), so their observations are independent given the parameters
# and OSA residuals are invariant to the order in which they are conditioned.

# -- 3a. Rceattle OSA residuals -----------------------------------------------
rce_osa <- Rceattle::osa_residuals(pk, source = c("index", "catch", "comp"),
                                   parallel = FALSE)

# -- 3b. WHAM OSA residuals ---------------------------------------------------
# The saved WHAM 'fit' is a plain list (no TMB object), so rebuild the object
wham_obj <- wham::fit_wham(fit$input, do.osa = FALSE, do.fit = FALSE,
                           do.retro = FALSE, do.sdrep = FALSE,
                           MakeADFun.silent = TRUE)
wham_obj$fn(fit$opt$par) # populate last.par at the optimum
wham_obj$env$last.par.best <- wham_obj$env$last.par
wham_obj$input <- fit$input; wham_obj$opt <- fit$opt; wham_obj$is_sdrep <- TRUE
wham_osa <- wham::make_osa_residuals(
  wham_obj, osa.opts = list(method = "oneStepGaussianOffMode", parallel = FALSE))$osa
wham_osa$year <- wham_osa$year + (pollock21$styr - 1L)   # WHAM year index -> calendar

# -----------------------------------------------------------------------------
# 4. Compare OSA residuals
# -----------------------------------------------------------------------------
# Merge Rceattle and WHAM residuals on their shared columns (year, and age bin for
# compositions)
report <- function(label, m) {
  m <- m[is.finite(m$residual.rce) & is.finite(m$residual.wham), ]
  if (nrow(m) == 0) return(cat(sprintf("  %-16s: no overlap\n", label)))
  cat(sprintf("  %-16s: n=%4d  r=%.6f  max|diff|=%.2e\n", label, nrow(m),
              stats::cor(m$residual.rce, m$residual.wham),
              max(abs(m$residual.rce - m$residual.wham))))
}
merge_agg <- function(rce_type, fleet, wham_type, wham_flt) {
  rce <- if (rce_type == "catch") rce_osa[rce_osa$source == "catch", ]
  else rce_osa[rce_osa$source == "index" & rce_osa$fleet == fleet, ]
  wh  <- wham_osa[wham_osa$type == wham_type &
                    (wham_type == "logcatch" | wham_osa$fleet == wham_flt), ]
  merge(rce[, c("year", "residual")], wh[, c("year", "residual")],
        by = "year", suffixes = c(".rce", ".wham"))
}
merge_comp <- function(fleet, wham_type, wham_flt) {
  rce <- rce_osa[rce_osa$source == "comp" & rce_osa$fleet == fleet,
                 c("year", "age_length_bin", "residual")]
  names(rce)[2] <- "bin"
  wh <- wham_osa[wham_osa$type == wham_type & wham_osa$fleet == wham_flt,
                 c("year", "bin", "residual")]
  merge(rce, wh, by = c("year", "bin"), suffixes = c(".rce", ".wham"))
}

cat("\n=== OSA residuals: Rceattle vs WHAM ===\n")
cat("Aggregate catch & survey indices (lognormal):\n")
report("catch", merge_agg("catch", NA, "logcatch", NA))
for (i in 1:6)
  report(paste0("survey ", i), merge_agg("index", i, "logindex", wham_label[i]))

cat("Age composition (multinomial):\n")
report("fishery comp", merge_comp(7, "catchpaa", "fleet_1"))
for (i in c(1, 2, 3, 6))   # surveys 4 and 5 have no composition data
  report(paste0("survey ", i, " comp"), merge_comp(i, "indexpaa", wham_label[i]))

# Pool every matched residual for the overall statistic and the 1:1 plot.
all_pairs <- rbind(
  merge_agg("catch", NA, "logcatch", NA)[, c("residual.rce", "residual.wham")],
  do.call(rbind, lapply(1:6, function(i)
    merge_agg("index", i, "logindex", wham_label[i])[, c("residual.rce", "residual.wham")])),
  merge_comp(7, "catchpaa", "fleet_1")[, c("residual.rce", "residual.wham")],
  do.call(rbind, lapply(c(1, 2, 3, 6), function(i)
    merge_comp(i, "indexpaa", wham_label[i])[, c("residual.rce", "residual.wham")])))
all_pairs <- all_pairs[is.finite(all_pairs$residual.rce) &
                         is.finite(all_pairs$residual.wham), ]
cat(sprintf("\nOVERALL: n=%d  r=%.8f  max|diff|=%.2e\n", nrow(all_pairs),
            stats::cor(all_pairs$residual.rce, all_pairs$residual.wham),
            max(abs(all_pairs$residual.rce - all_pairs$residual.wham))))

plot(all_pairs$residual.wham, all_pairs$residual.rce, pch = 19, col = "#00000055",
     xlab = "WHAM OSA residual", ylab = "Rceattle OSA residual",
     main = sprintf("2021 GOA pollock OSA residuals (r = %.6f)",
                    stats::cor(all_pairs$residual.rce, all_pairs$residual.wham)))
abline(0, 1, col = "red", lwd = 2)
