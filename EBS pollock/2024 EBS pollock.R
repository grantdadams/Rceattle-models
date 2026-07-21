# =============================================================================
# 2024 EBS pollock assessment in Rceattle (CEATTLE)
# =============================================================================
# Single-sex, single-species model: one fishery + AVO acoustic index, BTS
# bottom-trawl survey, ATS acoustic-trawl survey, and the ATS age-1 index.
#
# Builds the model configuration that aligns Rceattle with the ADMB reference
# ./ADMB/m23_rceattle_full/ and writes it to
# Data/2024_EBS_pollock_m23_rceattle_full.xlsx. Run this once; the fitting
# script ("2024 EBS pollock.R") reads the xlsx rather than rebuilding it.
# The ADMB-side edits this configuration is matched to are catalogued below.
#
# =============================================================================
# ADMB BRIDGING
# -----------------------------------------------------------------------------
# The bridging ADMB "pm" models are:
#   ADMB/m23              - 2024 SAFE (DoCovBTS = TRUE)
#   ADMB/m23_rceattle     - stage 1: structural alignment
#   ADMB/m23_rceattle_full- stage 2: likelihood alignment
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
#       + log_initdevs (equilibrium + init devs, matching Rceattle initMode = 2).
#
# Stage 2 - (likelihood alignment)
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
# Rebuild the reference:
#   cd ADMB/m23_rceattle_full && export PATH=/usr/local/bin:$PATH \
#     && admb pm && ./pm -nox -iprint 150
# =============================================================================


library(Rceattle)

AD <- "ADMB/m23_rceattle_full"
n_selages_fsh <- 12

# -----------------------------------------------------------------------------
# Data ----
# -----------------------------------------------------------------------------
est   <- read_data("Data/2024_EBS_pollock_m23_rceattle_full.xlsx")
styr  <- est$styr
endyr <- est$endyr
yrs   <- styr:endyr
nyr   <- length(yrs)

# -----------------------------------------------------------------------------
# Empirical selectivity start ----
# -----------------------------------------------------------------------------
# The fishery selectivity likelihood is multimodal: from the default (flat)
# start the optimizer settles ~9 nll units above the basin ADMB reaches. Seed the
# non-parametric fishery coefficients from the data instead -- the mean observed
# fishery age composition divided by numbers-at-age (a throwaway default fit),
# normalised and log-centred. This is the selectivity shape the catch data imply,
# so the fit reaches ADMB's basin without needing ADMB's own MLE.
fsh  <- est$fleet_control$Fleet_code[est$fleet_control$Fleet_name == "Fishery"]
m0   <- Rceattle::fit_mod(data_list = est, inits = NULL, file = NULL,
  estimateMode = 0, random_rec = FALSE, msmMode = 0, initMode = 2,
  M1Fun = build_M1(updateM1 = TRUE, M1_model = 0),
  fit_control = fit_control(verbose = 0, phase = TRUE,
                            bias_adjust_proc = 0, bias_adjust_obs = 0, comp_offset = 1e-3))
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
# The BTS/ATS survey q are solved analytically (arithmetic mean-ratio, matching
# ADMB DoCovBTS), so no index pins the *absolute* population scale -- it is only
# weakly identified by the catch + comps + M. From the default start, freeing the
# time-varying selectivity deviates in one shot opens a flat scale direction and
# log(mean recruitment) runs away (SSB -> 1e12). Fit in two stages instead:
#   A. time-varying selectivity OFF (base selectivity only) to pin the scale;
#   B. deviates ON, seeded from A.
# From default parameters this converges reliably (log_avgrec ~ 9.63) and matches
# ADMB to ~0.1-0.2% in SSB for 1978-2024 and to ~0.3% in recruitment across all
# years -- but it settles in a LOCAL optimum ~8 nll above ADMB's global. The two
# optima differ almost entirely in two directions (component nll, local - global):
#     fishery selectivity deviates +6.6 (default start over-flexes the time-varying
#                                        fishery selectivity; ATS/BTS match ADMB)
#     initial age structure        +1.0 (init_dev ~0.05-0.15 more negative, higher penalty)
#     compositions                 +1.3
#     survey indices               -0.5 (the local optimum fits the indices marginally better)
# i.e. from a flat start the optimizer trades a little index fit for more fishery
# selectivity flexibility and a lower initial abundance. The 1964-1977 biomass is the visible
# consequence of the init_dev shift and sits ~10% below ADMB (1964 SSB -10%); early
# recruitment and the 1978-2024 dynamics are essentially unchanged. This block is
# only weakly identified because the survey catchabilities are solved analytically
# (pinning selectivity/abundance shape, not level) and the 1965-1976 CPUE -- with a
# freely estimated q -- is the sole early-period abundance index, so the absolute
# size of the pre-1964 cohorts is poorly determined.
#
# It is a local-optimum / weak-identification artifact, NOT a model difference: the
# two objective functions are equivalent up to an additive constant. At ADMB's MLE,
# injecting its parameters reproduces every likelihood component to machine
# precision (indices, comps, catch, selectivity likelihoods/penalties incl. the
# AMAK avgsel term, recruitment and initial-age penalties) and SSB/R/N to ~1e-6, and
# seeding from ADMB's MLE and re-optimizing returns there (SSB 0.04%, R 0.04%,
# cor 1.0000, all years).
M1Fun <- build_M1(updateM1 = TRUE, M1_model = 0)
ctl   <- fit_control(verbose = 1, phase = TRUE,
                     bias_adjust_proc = 0, bias_adjust_obs = 0, comp_offset = 1e-3)

est_A <- est
est_A$fleet_control$Time_varying_sel <- "Off"   # base selectivity only
ebs_A <- Rceattle::fit_mod(data_list = est_A, inits = inits, file = NULL,
  estimateMode = 0, random_rec = FALSE, msmMode = 0, initMode = 2,
  M1Fun = M1Fun, fit_control = ctl)

ebs_2024 <- Rceattle::fit_mod(
  data_list    = est,
  inits        = ebs_A$obj$env$parList(),   # seed deviates fit from the scaled base fit
  file         = NULL,
  estimateMode = 0,
  random_rec   = FALSE,
  msmMode      = 0,
  initMode     = 2,
  M1Fun        = M1Fun,
  fit_control  = fit_control(
    verbose      = 1,
    phase        = TRUE,
    bias_adjust_proc = 0, bias_adjust_obs = 0, comp_offset = 1e-3)
)

# =============================================================================
# COMPARISON ----
# =============================================================================
# The bridge covers the HINDCAST (styr:endyr). estimateMode = 0 also runs a
# harvest-control-rule projection past endyr, but the projection horizon and its
# reference points (Amendment-56 SPR proxies) use Rceattle's HCR machinery and are
# not reconciled against ADMB's projection here -- only the hindcast SSB/R/N below.
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
q <- ebs_2024$quantities
cat(sprintf("\nObjective = %.3f\n", ebs_2024$opt$objective))
cmp(q$ssb[1, 1:nyr], get_admb("SSB"), "SSB")
cmp(q$R[1, 1:nyr],   get_admb("R"),   "R  ")
cmp(q$biomass[1, 1:nyr], admb_biomass, "Biomass")

# * Plot ----
# ADMB reference as a pseudo-Rceattle object
SAFE2024 <- ebs_2024
SAFE2024$quantities$ssb[1, 1:nyr]     <- get_admb("SSB")$val
SAFE2024$quantities$R[1, 1:nyr]       <- get_admb("R")$val
SAFE2024$quantities$biomass[1, 1:nyr] <- admb_biomass$val

mods  <- list(ebs_2024, SAFE2024)
names <- c("Rceattle (est)", "ADMB m23_rceattle_full")

print(plot_biomass(mods, model_names = names) + ggplot2::ylab("Total biomass"))
print(plot_ssb(mods, model_names = names) + ggplot2::ylab("Female SSB"))
print(plot_recruitment(mods, model_names = names) + ggplot2::ylab("Recruitment"))
# print(plot_selectivity(ebs_2024))
