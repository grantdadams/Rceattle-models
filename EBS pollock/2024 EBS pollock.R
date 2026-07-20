# =============================================================================
# 2024 EBS pollock assessment in Rceattle (CEATTLE) - FINAL MODEL + ADMB MATCH
# =============================================================================
# Single-sex, single-species model: one fishery + AVO acoustic index, BTS
# bottom-trawl survey, ATS acoustic-trawl survey, and the ATS age-1 index.
# This script fits the FREE-ESTIMATION Rceattle model and compares it to the
# structurally-aligned ADMB reference ./ADMB/m23_rceattle_full/. See
# "2024 EBS pollock bridging.R" for the forward-pass validation.
#
# RESULT (vs ADMB/m23_rceattle_full): recruitment cor ~0.999, SSB cor ~0.99;
# terminal R within ~0.2%. Evaluated at ADMB's parameters, every likelihood
# component matches (catch kernel EXACT 3.256; rec/init penalties EXACT
# 69.14 / 19.37; comps and indices match) - the two differ only by additive,
# parameter-independent lognormal normalizing constants (Rceattle reports the
# full -dnorm(); ADMB the bare kernel) plus ADMB's length-comp term (len_like).
# i.e. the two are the SAME model; a good initial point is needed for Rceattle's
# optimizer to reach the shared optimum basin (see the injection block below).
#
# =============================================================================
# ADMB SOURCE EDITS  (what was changed to align the ADMB model with Rceattle)
# -----------------------------------------------------------------------------
# The reference ADMB "pm" (AMAK) model was edited in two stages. Directories:
#   ADMB/m23              - original 2024 SAFE (DoCovBTS covariance survey)
#   ADMB/m23_rceattle     - stage 1: structural alignment
#   ADMB/m23_rceattle_full- stage 2: likelihood alignment  <-- reference here
# Each edit is flagged "MODIFIED (m23_rceattle...)" in ADMB/*/pm.tpl.
#
# Stage 1 - m23_rceattle (structural; makes the DYNAMICS/parameterisation match)
#   S1. log_avg_F FIXED (phase < 0); log_F_devs a plain bounded vector (sum-to-
#       zero removed) so F = exp(log_avg_F + log_F_devs) has exactly one free
#       parameter per year, as in Rceattle (control.dat ctrl_flag(4)=0 => no F
#       penalty).                                             (pm.tpl ~L1204/1264)
#   S2. BTS selectivity deviation vectors declared as plain bounded vectors with
#       the first year pinned at 0 (sum-to-zero removed).      (pm.tpl ~L1296/1709)
#   S3. Weight-at-age submodel likelihood (wt_like) EXCLUDED from the objective
#       (it is a data-independent constant here).              (pm.tpl ~L1721/6581)
#   S4. initMode-2 initial-age cascade: log_initage(a)=log_initage(a-1)-M(styr,a-1)
#       + log_initdevs (equilibrium + init devs, matching Rceattle initMode = 2).
#
# Stage 2 - m23_rceattle_full (likelihood; makes the OBJECTIVE comparable)
#   L1. rec_like(2)/(4) rewritten as FULL normal log-likelihoods
#         norm2/(2 sigma^2) + n*log(sigma) + n*0.5*log(2*pi),  with sigr = 1,
#       i.e. exactly Rceattle's rec/init penalty at sigma_rec_prior = 1, BAP = 0.
#                                                              (pm.tpl ~L3457)
#   L2. rec_like(1) FORCED TO 0 (unconditionally). Under SrType = 3 it was a
#       second, windowed rec-dev penalty (SR_resids == log_rec_devs); Rceattle
#       applies only ONE rec-dev penalty. Also phase_sr = -1.  (pm.tpl ~L3477/3491/3547)
#   L3. steepness DEACTIVATED (control.dat phase_steepness = -1). Closes the
#       parameter count: 1223 - 5 projection rec_devs = 1218 = Rceattle.
#   L4. eb_ats (ATS biomass index) sums ages mina_ats..nages, EXCLUDING age-1 -
#       removes the age-1 double-count (age-1 was in BOTH the biomass index and
#       the dedicated age-1 index ea1_ats).                    (pm.tpl ~L2953)
#   L5. pred_avo sums ages mina_ats..nages, EXCLUDING age-1 (AVO borrows the ATS
#       selectivity; matches Rceattle Bin_first_selected = 2). (pm.tpl ~L2898)
#   L6. log_q_avo BOUNDED [-15, 0]. avo_like is natural-scale normal with an
#       absolute sigma, so q_avo -> 0 is a zero-gradient trap; the bound keeps it
#       at its true optimum (~exp(-8)).                        (pm.tpl ~L1230)
#   L7. When ignore_last_ats_age1 (last-year ATS numbers CV > 0.4, = 1.81 in
#       2024), the age-1 index q (qtmp) is computed over the SAME 1..n_ats_r-1
#       range as the likelihood (the dropped 2024 excluded from q AND fit),
#       consistent with Rceattle's negative-year convention. (pm.tpl ~L4023)
#
# Rebuild the reference:
#   cd ADMB/m23_rceattle_full && export PATH=/usr/local/bin:$PATH \
#     && admb pm && ./pm -nox -iprint 150
# =============================================================================

library(Rceattle)
library(dplyr)
library(readxl)

AD <- "ADMB/m23_rceattle_full"
n_selages_fsh <- 12; bts_styr <- 1982; ats_styr <- 1994

# -----------------------------------------------------------------------------
# Data
# -----------------------------------------------------------------------------
mydata <- Rceattle::read_data(file = "Data/2024_EBS_pollock.xlsx")
styr <- mydata$styr; endyr <- mydata$endyr; nages <- mydata$nages
yrs  <- styr:endyr; nyr <- length(yrs)
keep_age <- c("Species_name", "Species", "Sex", "Year", paste0("Age", 1:nages))
mydata$NByageFixed <- mydata$NByageFixed[, intersect(keep_age, colnames(mydata$NByageFixed))]
mydata$spawn_month <- 3                                     # ADMB yrfrac 0.25

est <- mydata
est$estDynamics <- 0
fcn <- est$fleet_control$Fleet_name

# -- observation errors --------------------------------------------------------
# NOTE: the xlsx index Log_sd is ALREADY a CV / log-sd (0.05-0.56; ADMB sdnr ~1).
# Do NOT divide it by Observation. catch Log_sd = 0.05 (ADMB ctrl_flag(1)=200 =>
# sigma = 1/sqrt(2*200) = 0.05). ATS age-1 index sigma = age1_sigma_ats = 1.
est$catch_data$Log_sd <- 0.05
est$index_data$Log_sd[est$index_data$Fleet_name %in% c("BTS_1", "ATS_1")] <- 1
est$fleet_control$Fleet_type[fcn %in% c("BTS_1", "ATS_1")] <- 2
est$age_error[1:nages, 3:(nages + 2)] <- diag(nages)       # ageing error off
est$sigma_rec_prior <- 1                                   # full-normal rec penalty (ADMB L1)

# -- selectivity forms (AMAK "pm"): Fishery = Ianelli non-parametric,
#    BTS = logistic + free age-1, ATS/AVO = non-parametric ascending-constrained.
#    Penalty weights come from ADMB ctrl_flags / selvar24.dat.
est$fleet_control$Selectivity[fcn == "Fishery"]               <- "NonParametricPM"
est$fleet_control$Time_varying_sel[fcn == "Fishery"]          <- "RandomWalk"
est$fleet_control$N_sel_bins[fcn == "Fishery"]                <- n_selages_fsh
est$fleet_control$Sel_curve_pen1[fcn == "Fishery"]            <- 12.5    # ctrl_flag(13)
est$fleet_control$Sel_curve_pen2[fcn == "Fishery"]            <- 1/60    # ctrl_flag(11)/nch
est$fleet_control$Sel_curve_pen3                              <- 0
est$fleet_control$Sel_curve_pen3[fcn == "Fishery"]            <- 1       # ctrl_flag(10)/group
est$fleet_control$Sel_norm_bin1[fcn == "Fishery"]             <- NA
est$fleet_control$Time_varying_sel_sd_prior[fcn == "Fishery"] <- 0.5     # selvar24.dat

est$fleet_control$Selectivity[fcn == "BTS"]                <- "LogisticPM"
est$fleet_control$Time_varying_sel[fcn == "BTS"]           <- "RandomWalk"
est$fleet_control$Sel_curve_pen1[fcn == "BTS"]             <- 2          # ctrl_flag(26)
est$fleet_control$Sel_curve_pen2[fcn == "BTS"]             <- 0
est$fleet_control$Sel_curve_pen3[fcn == "BTS"]             <- 8          # age-1-dev RW weight
est$fleet_control$Sel_norm_bin1[fcn == "BTS"]              <- 3          # penalty age-range lo
est$fleet_control$Sel_norm_bin2[fcn == "BTS"]              <- 14         # penalty age-range hi
est$fleet_control$Sel_start_year[fcn == "BTS"]             <- bts_styr
est$fleet_control$Bin_first_selected[fcn == "BTS"]         <- 1
est$fleet_control$Time_varying_sel_sd_prior[fcn == "BTS"]  <- 1

for (fl in c("ATS", "AVO")) {
  est$fleet_control$Selectivity[fcn == fl]               <- "NonParametricPM"
  est$fleet_control$Time_varying_sel[fcn == fl]          <- "RandomWalk"
  est$fleet_control$N_sel_bins[fcn == fl]                <- 8
  est$fleet_control$Sel_curve_pen1[fcn == fl]            <- -1           # penalise INCREASING
  est$fleet_control$Sel_curve_pen2[fcn == fl]            <- 1
  est$fleet_control$Sel_curve_pen3[fcn == fl]            <- 0
  est$fleet_control$Sel_norm_bin1[fcn == fl]             <- NA
  est$fleet_control$Bin_first_selected[fcn == fl]        <- 2            # exclude age-1 (ADMB L4/L5)
  est$fleet_control$Sel_pen_first_bin[fcn == fl]         <- 2            # mina_ats
  est$fleet_control$Sel_start_year[fcn == fl]            <- ats_styr
  est$fleet_control$Time_varying_sel_sd_prior[fcn == fl] <- 0.138        # selvar24.dat
}

# -- survey timing + catchability ---------------------------------------------
est$index_data <- est$index_data %>%
  mutate(Month = case_when(Fleet_name %in% c("BTS", "BTS_1", "ATS", "ATS_1") ~ 6, TRUE ~ 0))
est$comp_data <- est$comp_data %>%
  mutate(Month = case_when(Fleet_name == "BTS" ~ 6, Fleet_name == "ATS" ~ 6, TRUE ~ Month))
est$fleet_control$Catchability <- as.character(est$fleet_control$Catchability)
est$fleet_control$Catchability[fcn == "ATS"]                 <- "1"                 # estimated
est$fleet_control$Catchability[fcn %in% c("BTS_1", "ATS_1")] <- "3"                 # analytical
est$fleet_control$Index_loglike[fcn == "BTS"] <- "MVN"                              # DoCovBTS
est$fleet_control$Catchability[fcn == "BTS"]  <- "AnalyticalArith"
est$index_cov <- list(BTS = as.matrix(read.table("ADMB/data/cov_2024.dat")))

# -- composition likelihood: ADMB offset (AFSC) multinomial (NOT full multinomial)
est$fleet_control$Comp_loglike <- "MultinomialAFSC"

# -- BTS age-1: ADMB keeps age-1 IN the BTS comps and has NO BTS age-1 index; the
#    xlsx relocated it into a separate BTS_1 index (verified identical to the raw
#    comp age-1 count). Restore it to the comps and drop the redundant BTS_1 index.
b1 <- est$index_data[est$index_data$Fleet_name == "BTS_1", c("Year", "Observation")]
for (r in which(est$comp_data$Fleet_name == "BTS")) {
  o <- b1$Observation[abs(b1$Year) == abs(est$comp_data$Year[r])]
  if (length(o) == 1) est$comp_data[r, "Comp_1"] <- o
}
est$fleet_control$Fleet_type[fcn == "BTS_1"] <- 0

# -- ATS age-1 index (ATS_1): ADMB's ignore_last_ats_age1 drops the terminal 2024
#    observation (last-year ATS numbers CV = 1.81 > 0.4, ADMB L7). Exclude it via
#    the negative-year convention (year < 0 => predicted but not fitted, and
#    excluded from the analytical q). Also flip the ATS/ATS_1 index -2020 -> 2020
#    (ADMB yrs_ats_data has 2020 and fits it; the ATS comps already fit 2020).
est$index_data$Year[est$index_data$Fleet_name == "ATS_1" & est$index_data$Year == 2024]  <- -2024
est$index_data$Year[est$index_data$Fleet_name %in% c("ATS", "ATS_1") &
                      est$index_data$Year == -2020] <- 2020

# -----------------------------------------------------------------------------
# Initialisation: seed the estimation at the ADMB MLE so the optimizer reaches
# the shared optimum basin. Rceattle and ADMB have the same likelihood, but the
# pre-survey (1964-1981) block {fishery sel, initial N, early F} is weakly
# identified and multimodal; from a default start Rceattle lands in a worse local
# minimum. Seeding the fishery selectivity increments + rec / F / init-devs at the
# ADMB values reaches the basin ADMB's phased optimizer found.
#   (NonParametricPM = type 9 is a carry-forward walk np_unc(yr)=np_unc(yr-1)+dev,
#    so sel_coff_dev is the per-year INCREMENT, 0 at non-change years.)
# -----------------------------------------------------------------------------
pl <- readLines(file.path(AD, "pm.par"))
gp <- function(nm) { i <- which(pl == paste0("# ", nm, ":"))[1]; v <- c(); j <- i + 1
  while (j <= length(pl) && !grepl("^#", pl[j])) {
    v <- c(v, as.numeric(strsplit(trimws(pl[j]), "[[:space:]]+")[[1]])); j <- j + 1 }; v }

inits <- build_params(est)
inits$rec_pars[1, 1]    <- gp("log_avgrec")
inits$rec_dev[1, 1:nyr] <- gp("log_rec_devs")
inits$log_F[1, 1:nyr]   <- gp("log_avg_F") + gp("log_F_devs")
idv <- gp("log_initdevs"); inits$init_dev[1, 1:length(idv)] <- idv
inits$index_log_q[2]    <- gp("log_q_avo")
# fishery selectivity: base coffs at styr + per-year increments (change years)
coffs <- gp("sel_coffs_fsh")
devs  <- matrix(gp("sel_devs_fsh"), ncol = n_selages_fsh, byrow = TRUE)  # 60 x 12
ych   <- 1965:2024                                                       # change years
inits$sel_coff[1, 1, 1:n_selages_fsh] <- coffs
inits$sel_coff_dev[1, 1, 1:n_selages_fsh, ] <- 0
for (k in seq_along(ych)) {
  yi <- which(yrs == ych[k]) + 1L                # ADMB applies dev(i) to year i+1
  if (!is.na(yi) && yi <= nyr) inits$sel_coff_dev[1, 1, 1:n_selages_fsh, yi] <- devs[k, ]
}

# =============================================================================
# FIT: free estimation, M fixed at the ADMB age schedule
# =============================================================================
ebs_2024 <- Rceattle::fit_mod(
  data_list    = est,
  inits        = inits,
  file         = NULL,
  estimateMode = 0,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = TRUE,
  initMode     = 2,
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 0),
  fit_control  = fit_control(bias_adjust_proc = 0, bias_adjust_obs = 0, comp_offset = 1e-3)
)

# =============================================================================
# COMPARISON vs ADMB (m23_rceattle_full)
# =============================================================================
rl <- readLines(file.path(AD, "pm.rep"))
get_admb <- function(key) {                                # [Year, val] block
  i <- grep(paste0("^", key, "$"), rl)[1]; rows <- list(); j <- i + 1
  while (j <= length(rl)) {
    v <- suppressWarnings(as.numeric(strsplit(trimws(rl[j]), " +")[[1]]))
    if (any(is.na(v)) || length(v) < 2) break
    rows[[length(rows) + 1]] <- v[1:2]; j <- j + 1 }
  setNames(as.data.frame(do.call(rbind, rows)), c("Year", "val"))
}
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
cat(sprintf("\nObjective = %.3f\n", ebs_2024$opt$opt$objective))
cmp(q$ssb[1, 1:nyr], get_admb("SSB"), "SSB")
cmp(q$R[1, 1:nyr],   get_admb("R"),   "R  ")

# ADMB reference as a pseudo-Rceattle object for overlay plots
SAFE2024 <- ebs_2024
SAFE2024$quantities$ssb[1, 1:nyr] <- get_admb("SSB")$val
SAFE2024$quantities$R[1, 1:nyr]   <- get_admb("R")$val
mods  <- list(ebs_2024, SAFE2024)
names <- c("Rceattle (est)", "ADMB m23_rceattle_full")
# plot_*() return ggplot objects (Rceattle >= 4.7.0), so print them; the axis
# labels come from the plot rather than a trailing mtext() on a base device.
print(plot_ssb(mods, model_names = names) + ggplot2::ylab("Female SSB"))
print(plot_recruitment(mods, model_names = names) + ggplot2::ylab("Recruitment"))
print(plot_selectivity(ebs_2024))
