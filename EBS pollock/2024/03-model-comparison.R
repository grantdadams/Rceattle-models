# =============================================================================
# EBS pollock 2024 -- production fit and ADMB comparison
# =============================================================================
# Single-sex, single-species model: one fishery + AVO acoustic index, BTS
# bottom-trawl survey, ATS acoustic-trawl survey, and the ATS age-1 index.
#
# Builds the model configuration that aligns Rceattle with the ADMB reference
# ./ADMB/m23_rceattle_full/
#
# Run from the "EBS pollock" project root.
#
# Reads:   Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx  (from "01-build-data.R")
#          ADMB/m23_rceattle_full/pm.rep                         (SSB / R / N for the overlay)
# Writes:  nothing; console comparison tables and interactive plots
# Prereq:  "01-build-data.R", and "00-fit-admb.R" for the ADMB comparison.
#
# THIS FILE CARRIES THE RECONCILIATION LOG (S1-S4, L1-L7, D1-D8, R1 below). The other
# scripts in this folder cite those codes rather than repeating the detail.
# =============================================================================
# ADMB BRIDGING
# -----------------------------------------------------------------------------
# The bridging ADMB "pm" models are:
#   ADMB/m23              - 2024 SAFE (DoCovBTS = TRUE)
#   ADMB/m23_rceattle     - stage 1: structural alignment
#   ADMB/m23_rceattle_full- stage 2: likelihood, data, parameter alignment
# Each edit is flagged with "MODIFIED (m23_rceattle...)" in ADMB/*/pm.tpl.
#
# Stage 1 - m23_rceattle (structural alignment)
#   S1. log_avg_F turned off (phase < 0); log_F_devs a plain bounded vector (sum-to-
#       zero removed) so F = exp(log_avg_F + log_F_devs) has exactly one free
#       parameter per year (control.dat ctrl_flag(4)=0 => no F penalty).
#   S2. BTS selectivity deviation vectors declared as plain bounded vectors with
#       the first year pinned at 0 (sum-to-zero removed)
#   S3. Weight-at-age submodel likelihood (wt_like) excluded from the objective.
#   S4. initial-age geometric series: log_initage(a)=log_initage(a-1)-M(styr,a-1)
#       + log_initdevs (equilibrium + init devs, matching Rceattle initMode = "NonEquilibrium").
#
# Stage 2 - (likelihood, data, and parameter alignment)
#   L1. rec_like(2)/(4) rewritten as FULL normal log-likelihoods
#         norm2/(2 sigma^2) + n*log(sigma) + n*0.5*log(2*pi),  with sigr = 1.
#   L2. rec_like(1) set to 0. Under SrType = 3 it was a
#       second, rec-dev penalty for Ricker curve.
#   L3. steepness turned off (control.dat phase_steepness = -1).
#   L4. eb_ats (ATS biomass index) sums ages mina_ats:nages and now excludes age-1.
#       Age-1 was in BOTH the biomass index and the dedicated age-1 index ea1_ats.
#   L5. pred_avo sums ages mina_ats..nages and now excludes age-1. AVO borrows the ATS
#       selectivity. FIXME: may want an AVO age-1 index?
#   L6. log_q_avo bounded [-15, 0]. avo_like is normal with an
#       absolute sigma, so q_avo -> 0 is a zero-gradient funnel; the bound keeps it
#       at its true optimum (~exp(-8)).
#   L7. When ignore_last_ats_age1 = TRUE, the age-1 index q (qtmp) is now computed
#       over the SAME 1:n_ats_r-1 range as the likelihood (the dropped 2024
#       excluded from q AND fit).
#
# Data / configuration conversions (Rceattle-side only -- no ADMB code edits;
# applied in "01-build-data.R" and baked into the workbook)
#   D1. Catch Log_sd = 0.05, from ADMB ctrl_flag(1) = 200: sigma = 1/sqrt(2*200).
#   D2. Index Log_sd in the xlsx is ALREADY a CV / log-sd -- not rescaled by the
#       observation. BTS_1 / ATS_1 age-1 index sigma = age1_sigma_ats = 1.
#   D3. AVO observations rescaled thousand-tonnes -> million-tonnes to match ADMB
#       obs_avo, so obs, sigma and prediction share a scale under the natural-scale
#       normal with absolute sigma.
#   D4. ATS index Log_sd = sqrt(log(CV^2 + 1)) (CV -> log-sd).
#   D5. 1965-76 Japanese CPUE fit once, as a survey fleet mirroring the fishery
#       selectivity.
#   D6. BTS / ATS composition sample sizes truncated to integer (ADMB stored them
#       as ints); 2020 ATS = 1.
#   D7. BTS age-1: the xlsx had split it into a separate BTS_1 index. Folded back
#       into the BTS age comps and BTS_1 switched Off, so it is not counted twice.
#   D8. AMAK average-selectivity penalty retained (Sel_avgsel_pen = 10); BTS
#       selectivity random-walk penalty applied over the survey period only.
#
# Residual likelihood difference -- Rceattle >= 5.9.0 (NOT reconcilable Rceattle-side)
#   R1. The natural-scale normal index likelihood (Index_distribution = "Normal")
#       is LEFT-TRUNCATED AT ZERO in Rceattle from 5.9.0. An index cannot be
#       negative and data_check() will not accept one, so the density is
#       renormalized over (0, Inf) and each fitted observation carries
#       + log(Phi(mu / sigma)) on top of -log(phi(obs; mu, sigma)).
#       ADMB's avo_like / cpue_like are the untruncated 0.5*(obs - q*pred)^2/sigma^2,
#       so this term has no ADMB counterpart. It applies to the two Normal-family
#       fleets here: AVO (L5/L6/D3) and the 1965-76 Japanese CPUE fleet (D5).
#
#       Size: with the 18 ob_avo_std values and a prediction at the observed AVO
#       scale (~1.74 million tonnes), sum(log Phi(mu/sigma)) is about -0.021,
#       rising to -0.16 at mu = 1.2 and -0.51 at mu = 0.9. The component-by-
#       component bridge in "02-bridge.R" currently reports an AVO residual of
#       -0.00001, so at the nominal scale this term is some three orders of
#       magnitude larger than that agreement. See the "Rceattle >= 5.9.0" section
#       of HANDOFF_pm_full_likelihood.md, which that handoff's plan depends on:
#       it assumes the whole Rceattle-ADMB gap is a constant, and this term is
#       not one.
#
#       It is NOT an additive constant. The term depends on mu = q * pred, so it
#       carries a gradient and shifts the optimum, not just the objective level.
#       Objective comparisons against ADMB for these two fleets therefore no
#       longer match to machine precision, and the difference is not a fixed
#       offset that can be subtracted. For an exact ADMB check, compare the
#       untruncated part -- -sum(dnorm(obs, pred, sigma, log = TRUE)) recomputed
#       from the reported index_hat -- rather than jnll_comp row 1.
#
#       This is a deliberate package change (the simulator and the likelihood now
#       agree on the same density), not a bridging defect. The lognormal and
#       MVN/MVNORM fleets are unaffected.

# The fishery terminal-year length composition is not fit (ADMB use_endyr_len = 0).
# There is nothing to switch off Rceattle-side -- the workbooks carry no length
# composition rows at all -- so it is an ADMB-side setting, not a D-code.
# =============================================================================


library(Rceattle)

n_selages_fsh <- 12
AD <- "ADMB/m23_rceattle_full"   # ADMB reference (used only for the validation comparison below)

# -----------------------------------------------------------------------------
# Data ----
# -----------------------------------------------------------------------------
est   <- read_data("Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx")
styr  <- est$styr
endyr <- est$endyr
yrs   <- styr:endyr
nyr   <- length(yrs)

# -----------------------------------------------------------------------------
# Empirical selectivity start ----
# -----------------------------------------------------------------------------
# The fishery selectivity likelihood is multimodal; from a flat start the optimizer
# lands ~9 nll above ADMB's nll. We initialize the non-parametric fishery coefficients from
# the data instead -- mean observed fishery age comp / numbers-at-age (a throwaway
# fit), normalised and log-centred -- so the fit reaches ADMB's nll without its MLE.
fsh  <- est$fleet_control$Fleet_code[est$fleet_control$Fleet_name == "Fishery"]
m0   <- Rceattle::fit_mod(data_list = est,
                          inits = NULL,
                          estimateMode = 0,
                          random_rec = FALSE,
                          msmMode = 0,
                          initMode = "NonEquilibrium",
                          M1Fun = build_M1(updateM1 = TRUE, M1_model = "fixed"),
                          fit_control = fit_control(verbose = 0,
                                                    phase = TRUE,
                                                    bias_adjust_proc = 0,
                                                    bias_adjust_obs = 0,
                                                    comp_offset = 1e-3))
N   <- m0$quantities$N_at_age[1, 1, , 1:nyr]
cd  <- est$comp_data[est$comp_data$Fleet_code == fsh & est$comp_data$Year > 0 &
                       est$comp_data$Age0_Length1 == 0, ]   # age comps only (exclude length comp)
cc  <- grep("^Comp_", colnames(cd), value = TRUE)[1:est$nages]
sy  <- matrix(NA_real_, nrow(cd), est$nages)
for (i in seq_len(nrow(cd))) {
  yi <- which(yrs == cd$Year[i]); if (!length(yi)) next
  pa <- as.numeric(cd[i, cc]); pa <- pa / sum(pa, na.rm = TRUE)
  s  <- pa / pmax(N[, yi], 1e-8); sy[i, ] <- s / max(s, na.rm = TRUE)
}
sel_bar <- colMeans(sy, na.rm = TRUE)[1:n_selages_fsh]
ls      <- log(pmax(sel_bar / max(sel_bar), 1e-3)); ls <- ls - mean(ls)
inits   <- build_params(est)
inits$sel_coff[1, 1, 1:n_selages_fsh] <- ls

# =============================================================================
# FIT (two-stage) ----
# =============================================================================
# Initializing the fishery time-varying selectivity deviates from default (0)
# does not find the best optimum, so we fit in two stages instead:
#   A. time-varying selectivity OFF (base only) to pin the scale;
#   B. deviates ON, seeded from A.
# This converges reliably and matches ADMB to ~0.1-0.2% (SSB) / ~0.3% (R) for
# 1978-2024, but settles in a local optimum ~8 nll above ADMB's global -- mostly
# extra fishery-selectivity flex (+6.6) and a lower initial abundance (init_dev, +1.0),
# so 1964 SSB sits ~10% below ADMB while modern dynamics are essentially unchanged.
# It is a weak-identification artifact, not a model difference: at ADMB's MLE the
# model reproduces every likelihood component to ~1e-5 -- see the likelihood
# check at the foot of "02-bridge.R", which asserts that component by component
# against pm.rep rather than leaving it as a claim here.
M1Fun <- build_M1(updateM1 = TRUE, M1_model = "fixed")
ctl   <- fit_control(verbose = 1, phase = TRUE,
                     bias_adjust_proc = 0, bias_adjust_obs = 0, comp_offset = 1e-3)

# - Stage 1
est_A <- est
est_A$fleet_control$Time_varying_sel <- "Off"   # No time-varying deviates
ebs_stage1 <- Rceattle::fit_mod(data_list = est_A,
                           inits = inits,
                           file = NULL,
                           estimateMode = 0,
                           random_rec = FALSE,
                           msmMode = 0,
                           initMode = "NonEquilibrium",
                           M1Fun = M1Fun,
                           fit_control = ctl)

# - Stage 2
ebs_2024 <- Rceattle::fit_mod(
  data_list    = est,
  inits        = ebs_stage1$obj$env$parList(),   # initialize deviates fit from the scaled base fit
  file         = NULL,
  estimateMode = 0,
  random_rec   = FALSE,
  msmMode      = 0,
  initMode     = "NonEquilibrium",
  M1Fun        = M1Fun,
  fit_control  = fit_control(
    verbose      = 1,
    phase        = TRUE,
    bias_adjust_proc = 0, bias_adjust_obs = 0, comp_offset = 1e-3)
)

# =============================================================================
# SENSITIVITY: 2D AR1 fishery selectivity (age x year) ----
# =============================================================================
# Replace the fishery's non-parametric random-walk selectivity with a 2D AR1 field
# over age x year (Selectivity = "2DAR1"; Xu et al. 2019 / Cheng et al. 2024): annual
# log-selectivity deviations correlated across age (Sel_curve_pen1) and year
# (Sel_curve_pen2) via two estimated AR1 rhos, deviation SD (Time_varying_sel_sd)
# estimated, integrated out (random_sel = TRUE). CPUE mirrors the fishery selectivity
# (shared Selectivity_index), so it follows.
# FIXME: CPUE should use the baranov, but does not
est_2d    <- est
fsh_block <- est_2d$fleet_control$Selectivity_index[est_2d$fleet_control$Fleet_name == "Fishery"][1]
sel_rows  <- which(est_2d$fleet_control$Selectivity_index == fsh_block)
est_2d$fleet_control$Selectivity[sel_rows]         <- "2DAR1"
est_2d$fleet_control$Time_varying_sel[sel_rows]    <- "Off"          # ignored for 2DAR1 (field is age x year)
est_2d$fleet_control$N_sel_bins[sel_rows]          <- n_selages_fsh  # age bins in the field
est_2d$fleet_control$Bin_first_selected[sel_rows]  <- 1
est_2d$fleet_control$Sel_curve_pen1[sel_rows]      <- 0              # Initial AGE (bin) AR1 rho (logit scale), estimated
est_2d$fleet_control$Sel_curve_pen2[sel_rows]      <- 0              # Initial YEAR AR1 rho (logit scale), estimated
est_2d$fleet_control$Time_varying_sel_sd[sel_rows] <- 1              # deviation SD init
est_2d$fleet_control$Sel_curve_pen3[sel_rows]      <- NA
est_2d$fleet_control$Sel_avgsel_pen[sel_rows]      <- 0              # AMAK base-level penalty is a type-9 term; off here

# Self-contained initialization -- does NOT use the base fit. A flat AR1 field (all
# deviations 0) gives a NaN marginal objective, so the field must be built up before it
# is integrated out. Fit it first as PENALISED effects (random_sel = FALSE -- an
# ordinary likelihood, so no NaN and no scale runaway), then turn on the Laplace
# integration seeded from that non-zero field. Both stages start from the data-driven
# empirical selectivity (`inits`), so the 2D AR1 sensitivity stands alone. SLOW
# (Laplace + phasing, ~25 min). Not anchored to the base fit, it settles in its own
# optimum -- SSB cor ~0.97 with the base (terminal ~3% lower), flexing the historical
# fishery selectivity somewhat more than the base random walk. Like the base it trips
# the estimability / non-PD-Hessian checks -- weak identification (analytical survey q),
# not a 2D AR1 problem (see ebs_2dar1$convergence).
copy_matching <- function(target, source) {
  for (nm in intersect(names(target), names(source)))
    if (identical(dim(target[[nm]]), dim(source[[nm]])) &&
        length(target[[nm]]) == length(source[[nm]]))
      target[[nm]] <- source[[nm]]
  target
}

# Stage 1: AR1 field as penalised effects (random_sel = FALSE) -- builds a non-zero field.
ebs_2dar1_pen <- Rceattle::fit_mod(
  data_list = est_2d, inits = inits, file = NULL,
  estimateMode = 0, random_rec = FALSE, random_sel = FALSE,
  msmMode = 0, initMode = "NonEquilibrium", M1Fun = M1Fun, fit_control = ctl)

# Stage 2: integrate the field out (random_sel = TRUE), seeded from the penalised fit.
inits_2d <- copy_matching(build_params(est_2d), ebs_2dar1_pen$obj$env$parList())
ebs_2dar1 <- Rceattle::fit_mod(
  data_list    = est_2d,
  inits        = inits_2d,
  file         = NULL,
  estimateMode = 0,
  random_rec   = FALSE,
  random_sel   = TRUE,          # Laplace-integrate the 2D AR1 selectivity field
  msmMode      = 0,
  initMode     = "NonEquilibrium",
  M1Fun        = M1Fun,
  fit_control  = fit_control(verbose = 1, phase = TRUE,
                             bias_adjust_proc = 0, bias_adjust_obs = 0, comp_offset = 1e-3)
)

# =============================================================================
# COMPARISON (validation against the ADMB reference) ----
# =============================================================================
q <- ebs_2024$quantities
obj_val <- function(m) {                                    # NULL-safe objective
  o <- tryCatch(m$opt$objective, error = function(e) NULL)
  if (length(o) != 1 || !is.finite(o)) NA_real_ else as.numeric(o)
}
cat(sprintf("\nObjective (base, NonParametricPM fishery sel) = %.3f\n",
            obj_val(ebs_2024)))
cat(sprintf("Objective (2D AR1 fishery sel)                = %s\n",
            ifelse(is.na(obj_val(ebs_2dar1)), "NA (marginal; see note)",
                   sprintf("%.3f", obj_val(ebs_2dar1)))))
# 2D AR1 is mixed-effects (deviations integrated out), so its marginal objective is
# not comparable to the base penalised objective or ADMB; compare trajectories
# instead (opt$objective can also come back NULL if the random-effects sdreport fails).

rl <- readLines(file.path(AD, "pm.rep"))
get_admb <- function(key) {                                # [Year, val] block
  i <- grep(paste0("^", key, "$"), rl)[1]; rows <- list(); j <- i + 1
  while (j <= length(rl)) {
    v <- suppressWarnings(as.numeric(strsplit(trimws(rl[j]), " +")[[1]]))
    if (any(is.na(v)) || length(v) < 2) break
    rows[[length(rows) + 1]] <- v[1:2]; j <- j + 1 }
  setNames(as.data.frame(do.call(rbind, rows)), c("Year", "val"))
}
# pm.rep has no total-biomass series, so build it as numbers-at-age x population
# weight -- the same pop_wt_index weight Rceattle's biomass uses -- so the two are
# on the same footing (the comparison then isolates differences in N-at-age).
get_admb_mat <- function(key, ncol) {                      # [year x ncol] block
  i <- grep(paste0("^", key, "$"), rl)[1]; rows <- list(); j <- i + 1
  while (j <= length(rl)) {
    v <- suppressWarnings(as.numeric(strsplit(trimws(rl[j]), " +")[[1]]))
    if (any(is.na(v)) || length(v) < ncol) break
    rows[[length(rows) + 1]] <- v[1:ncol]; j <- j + 1 }
  do.call(rbind, rows)
}
admb_N <- get_admb_mat("N", est$nages)                     # rows = years, cols = ages
wt_pop <- est$weight[est$weight$Wt_index == est$pop_wt_index, ]
wt_pop <- as.matrix(wt_pop[match(yrs, wt_pop$Year), paste0("Age", 1:est$nages)])
admb_biomass <- data.frame(Year = yrs, val = rowSums(admb_N * wt_pop))
cmp <- function(rvec, admb, lab) {
  d <- merge(data.frame(Year = yrs, R = as.numeric(rvec)), admb, by = "Year")
  d$pct <- 100 * (d$R - d$val) / d$val
  cat(sprintf("\n%s: cor = %.4f | mean|%%| = %.1f | max|%%| = %.1f\n",
              lab, cor(d$R, d$val), mean(abs(d$pct)), max(abs(d$pct))))
  for (y in c(1964, 1978, 1990, 2008, 2024))
    cat(sprintf("  %d: Rceattle = %8.1f  ADMB = %8.1f  (%+.1f%%)\n",
                y, d$R[d$Year == y], d$val[d$Year == y], d$pct[d$Year == y]))
}
cat("\n-- Base (NonParametricPM fishery selectivity) vs ADMB --\n")
cmp(q$ssb[1, 1:nyr], get_admb("SSB"), "SSB")
cmp(q$R[1, 1:nyr],   get_admb("R"),   "R  ")
cmp(q$biomass[1, 1:nyr], admb_biomass, "Biomass")

cat("\n-- 2D AR1 fishery selectivity vs ADMB --\n")
q2 <- ebs_2dar1$quantities
cmp(q2$ssb[1, 1:nyr], get_admb("SSB"), "SSB")
cmp(q2$R[1, 1:nyr],   get_admb("R"),   "R  ")
cmp(q2$biomass[1, 1:nyr], admb_biomass, "Biomass")

# * Plot -- ADMB reference as a pseudo-Rceattle object, alongside both fits
SAFE2024 <- ebs_2024
SAFE2024$quantities$ssb[1, 1:nyr]     <- get_admb("SSB")$val
SAFE2024$quantities$R[1, 1:nyr]       <- get_admb("R")$val
SAFE2024$quantities$biomass[1, 1:nyr] <- admb_biomass$val
mods  <- list(ebs_2024, ebs_2dar1, SAFE2024)
names <- c("Rceattle (NonParametricPM sel)", "Rceattle (2D AR1 sel)",
           "ADMB m23_rceattle_full")
print(plot_biomass(mods, model_names = names) + ggplot2::ylab("Total biomass"))
print(plot_ssb(mods, model_names = names) + ggplot2::ylab("Female SSB"))
print(plot_recruitment(mods, model_names = names) + ggplot2::ylab("Recruitment"))
# Realised fishery selectivity surfaces: penalised random-walk vs 2D AR1 field
print(plot_selectivity(list(ebs_2024, ebs_2dar1),
                       model_names = c("NonParametricPM", "2D AR1")))
