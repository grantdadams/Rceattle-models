# =============================================================================
# 2024 EBS pollock assessment data for Rceattle (CEATTLE)
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
# Stage 3 - (selectivity-penalty / composition alignment)
#   A. BTS selectivity random-walk penalty restricted to the survey period
#      (first_difference over styr_bts:endyr_r) so the flat pre-survey years and
#      the survey-start boundary are outside the penalty.
#   B. Age-composition multinomial multiplier (oac + MN_const) rather than oac,
#      matching Rceattle's MultinomialAFSC = N*(o+c)*(log(o+c)-log(p+c)).
#
# Rceattle-side data encoding (set in the body below to match ADMB's data file /
# likelihood; these are configuration choices, not code edits):
#   - Fishery terminal-year length comp is NOT fit (ADMB use_endyr_len = 0).
#   - AVO acoustic index observations in million-tonnes (ADMB obs_avo), matching
#     the absolute-SD normal likelihood; the base xlsx stored thousand-tonnes.
#   - Japanese CPUE fit exactly once, as a dedicated survey fleet mirroring the
#     fishery selectivity (the base xlsx also carried it on the fishery fleet).
#   - BTS/ATS age-comp sample sizes truncated to integer (ADMB init_ivector).
#   - 2020 ATS age-comp sample size = 1 (ADMB sam_ats), not 0 (COVID-year survey).
#   - ATS biomass index Log_sd = sqrt(log(CV^2 + 1)) (ADMB lvarb_ats CV->log-SD).
#   - AMAK avgsel base-level selectivity penalty enabled (Sel_avgsel_pen = 10).
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
# Data ----
# -----------------------------------------------------------------------------
mydata <- Rceattle::read_data(file = "Data/2024_EBS_pollock.xlsx")
styr <- mydata$styr
endyr <- mydata$endyr
nages <- mydata$nages
yrs  <- styr:endyr
nyr <- length(yrs)

keep_age <- c("Species_name", "Species", "Sex", "Year", paste0("Age", 1:nages))
mydata$NByageFixed <- mydata$NByageFixed[, intersect(keep_age, colnames(mydata$NByageFixed))]
mydata$spawn_month <- 3 # ADMB yrfrac 0.25

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

# AMAK "avgsel" base-level selectivity penalty, ADMB fff += 10*square(avgsel_*)
# with avgsel = log(mean(exp(base coffs))) (pm.tpl:5535 etc.). Applied to the
# non-parametric (type 9) fleets; only the lead fleet of each shared block
# accumulates it (Fishery for block 1, AVO for the AVO/ATS block). ~0 for the
# fishery, ~0.129 for the ATS coefficients.
est$fleet_control$Sel_avgsel_pen[fcn %in% c("Fishery", "ATS", "AVO")] <- 10

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

# -- ATS biomass index: the xlsx Log_sd is a CV (std/obs), but ADMB's lognormal
#    variance is lvarb_ats = log(CV^2 + 1) (the exact CV -> log-scale-SD conversion,
#    pm.tpl:1689-1691), and Rceattle's lognormal likelihood uses Log_sd directly as
#    the log-scale SD. Convert CV -> sqrt(log(CV^2 + 1)) so the ATS biomass variance
#    matches ADMB exactly (the +0.01 inside-log offset is negligible at this scale).
ats_rows <- est$index_data$Fleet_name == "ATS"
est$index_data$Log_sd[ats_rows] <- sqrt(log(est$index_data$Log_sd[ats_rows]^2 + 1))

# -- AVO acoustic index: ADMB avo_like is a natural-scale normal with an ABSOLUTE
#    observation SD (ob_avo_std, pm_24.dat), not a lognormal CV. Fit it with
#    Index_loglike = "Normal" (residual obs - q*pred ~ N(0, ob_avo_std^2)) and
#    supply ob_avo_std directly in Log_sd (provided, not estimated).
est$fleet_control$Index_loglike[fcn == "AVO"]     <- "Normal"
est$fleet_control$Estimate_index_sd[fcn == "AVO"] <- 0
ob_avo_std <- setNames(
  c(0.407974331, 0.79543824, 0.292865177, 0.390095688, 0.579193251, 0.447677778,
    0.371938445, 0.390115995, 0.58024587, 0.406257388, 0.379092753, 0.317389245,
    0.254960502, 0.63539506, 0.529928784, 0.454780316, 0.335349192, 0.250814465),
  as.character(c(2006:2019, 2021:2024)))
avo_rows <- est$index_data$Fleet_name == "AVO"
stopifnot(sum(avo_rows) == length(ob_avo_std))
est$index_data$Log_sd[avo_rows] <- ob_avo_std[as.character(abs(est$index_data$Year[avo_rows]))]
# The base xlsx stores the AVO acoustic biomass in thousand-tonnes (~1741), but ADMB's
# obs_avo (pm_24.dat) is in million-tonnes (~1.74) and the absolute observation SD
# (ob_avo_std ~0.4) and the model prediction (q*wt_avo*N*sel_ats) are on that same
# ~1.74 scale. Since avo_like is a natural-scale normal with an ABSOLUTE sigma, leaving
# obs at 1000x inflates every residual 1000x and avo_like 1e6x. Rescale AVO obs to
# ADMB's million-tonne units so obs, sigma and prediction are consistent.
est$index_data$Observation[avo_rows] <- est$index_data$Observation[avo_rows] / 1000

# -- composition likelihood: ADMB offset (AFSC) multinomial (NOT full multinomial)
est$fleet_control$Comp_loglike <- "MultinomialAFSC"

# -- ADMB reads the survey age-comp sample sizes as integer vectors
#    (init_ivector sam_bts / sam_ats), which TRUNCATES the fractional
#    (McAllister-Ianelli-weighted) sample sizes in the data. The fishery sample
#    size is a float (init_vector sam_fsh) and is left as-is. Truncate the BTS/ATS
#    comp sample sizes to match so the multinomial weights are identical.
for (fl in c("BTS", "ATS"))
  est$comp_data$Sample_size[est$comp_data$Fleet_name == fl] <-
    trunc(est$comp_data$Sample_size[est$comp_data$Fleet_name == fl])
# The 2020 ATS age comp (COVID-year survey) is stored with sample size 0 in the
# xlsx (effectively excluded), but ADMB's data file fits it with sample size 1
# (sam_ats(2020) = 1). Restore it so the ATS multinomial matches ADMB exactly.
est$comp_data$Sample_size[est$comp_data$Fleet_name == "ATS" &
                          est$comp_data$Year == 2020] <- 1

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

# -- Drop the base xlsx's Japanese CPUE copy. The base data attaches the 1965-1976
#    CPUE to the FISHERY fleet (Fleet_code 1) as an index, and it is fit as a fishery
#    index likelihood. ADMB fits the CPUE exactly ONCE (cpue_like). Immediately below
#    we (re)add it as a dedicated CPUE survey fleet mirroring the fishery selectivity
#    with its own estimated q, so leaving the base copy would DOUBLE-COUNT the CPUE
#    (its 12 obs sit in 1965-1976, the only early-period index, so the double-count
#    over-constrains the initial age structure and pulls the early-year fit off ADMB).
fishery_code <- est$fleet_control$Fleet_code[fcn == "Fishery"]
est$index_data <- est$index_data[est$index_data$Fleet_code != fishery_code, ]

# -- Japanese fishery CPUE index (1965-1976). ADMB fits this as a fishery-selected
#    biomass index (pred_cpue = wt_fsh * natage * sel_fsh * q_cpue) with its own
#    estimated catchability. It is the ONLY abundance index before the BTS starts
#    in 1982, so it constrains the early-period numbers-at-age (and the initial
#    age structure) that are otherwise unconstrained -- without it Rceattle has one
#    fewer estimable parameter (log_q_cpue) and a smaller likelihood than ADMB, and
#    the two fits differ in the pre-survey years. Added as a survey fleet that
#    MIRRORS the fishery selectivity (same Selectivity_index) and uses the fishery
#    weight-at-age, with an estimated q. ADMB's cpue_like is a natural-scale normal
#    with an absolute SD (cpue_sd), fit here with Index_loglike = "Normal" and the
#    observation SD supplied directly in Log_sd (provided, not estimated).
cpue_row <- est$fleet_control[fcn == "Fishery", ]          # inherit fishery selectivity
cpue_row$Fleet_name <- "CPUE"
cpue_row$Fleet_code <- max(est$fleet_control$Fleet_code) + 1L
cpue_row$Fleet_type <- "Survey"
cpue_row$Q_index    <- max(est$fleet_control$Q_index, na.rm = TRUE) + 1L
cpue_row$Catchability <- "1"                               # estimated q (log_q_cpue)
cpue_row$Index_loglike     <- "Normal"                     # natural-scale normal, absolute SD
cpue_row$Estimate_index_sd <- 0
avo_r <- which(fcn == "AVO")
for (col in c("Q_prior", "Q_sd_prior", "Time_varying_q", "Time_varying_q_sd_prior",
              "Estimate_index_sd", "Index_sd_prior"))
  cpue_row[[col]] <- est$fleet_control[avo_r, col]
for (col in c("Estimate_catch_sd", "Catch_sd_prior", "proj_F_prop")) cpue_row[[col]] <- NA
est$fleet_control <- rbind(est$fleet_control, cpue_row)

cpue_obs <- c(2816.437428, 3473.580475, 3802.169891, 5257.304601, 6712.468418,
              5679.809828, 5257.331283, 5726.743484, 4787.923949, 4740.992588,
              4271.574460, 4318.523058)
cpue_sd  <- c(563.2874856, 694.716095, 760.4339781, 1051.46092, 1342.493684,
              1135.961966, 1051.466257, 1145.348697, 957.5847898, 948.1985176,
              854.3148919, 863.7046116)
idx0 <- est$index_data[est$index_data$Fleet_name == "AVO", ][1, ]
cpue_idx <- idx0[rep(1, length(cpue_obs)), ]
cpue_idx$Fleet_name  <- "CPUE"
cpue_idx$Fleet_code  <- cpue_row$Fleet_code
cpue_idx$Species     <- 1
cpue_idx$Year        <- 1965:1976
cpue_idx$Month       <- 0
cpue_idx$Observation <- cpue_obs
cpue_idx$Log_sd      <- cpue_sd                            # absolute observation SD (natural-scale normal)
est$index_data <- rbind(est$index_data, cpue_idx)

# -- Fishery length composition: NOT fit. ADMB's control.dat sets use_endyr_len = 0,
#    so the terminal-year fishery length comp (olc_fsh) is EXCLUDED from the ADMB
#    objective: pm.tpl:3608 `if (use_endyr_len>0) NLL(13) += ctrl_flag(7)*len_like;`
#    never fires, and `fff += sum(NLL)` therefore omits len_like (pm.rep len_like =
#    25.795 is reported but not in the total). To keep the two objectives identical we
#    likewise do NOT add the fishery length comp here (no nlengths / age_trans_matrix /
#    Age0_Length1 == 1 comp row). The terminal-year fish are already represented by the
#    fishery AGE comp, so fitting the length comp too would double-count them.

write_data(est, file = "Data/2024_EBS_pollock_m23_rceattle_full.xlsx")
