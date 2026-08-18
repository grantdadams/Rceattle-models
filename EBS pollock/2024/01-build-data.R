# =============================================================================
# EBS pollock 2024 -- build the Rceattle data list
# =============================================================================
# Single-sex, single-species: fishery + AVO acoustic index, BTS bottom-trawl and
# ATS acoustic-trawl surveys, ATS age-1 index, and the 1965-76 Japanese CPUE.
# Aligns Rceattle with the ADMB reference ./ADMB/m23_rceattle_full/ and writes
# Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx (run once; the fit script reads it).
#
# Run from the "EBS pollock" project root so the relative Data/ paths resolve.
#
# Reads:   Data/EBS_24_pollock_single_species_1964-2024.xlsx  (hand-assembled skeleton)
#          Data/BTS_survey_covariance_2024.dat                (BTS index covariance)
# Writes:  Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx
# Prereq:  ADMB reference in ADMB/m23_rceattle_full/ -- "00-fit-admb.R" rebuilds it.
#
# The workbook is the canonical data source; no .Rdata copy is written. The
# S/L/D reconciliation codes cited below are catalogued in "03-model-comparison.R".
#
# ADMB bridge -- the "pm" edits this config matches (flagged "MODIFIED (m23_...)"
# in ADMB/*/pm.tpl):
#   Structural: log_avg_F off + plain log_F_devs (one free F/yr); BTS sel-dev
#     vectors plain with year 1 pinned; wt_like excluded; initial-age geometric
#     series = equilibrium + init devs (Rceattle initMode = "NonEquilibrium").
#   Likelihood: rec_like full normal (sigr = 1); Ricker rec penalty + steepness off;
#     ATS biomass index and AVO exclude age-1; log_q_avo bounded [-15,0]; terminal
#     ATS age-1 obs dropped from q and fit.
#   Data/penalty: BTS sel random-walk penalty over the survey period only; AFSC
#     offset multinomial (oac + MN_const).
#
# Rceattle-side encoding (in the body below; configuration, not ADMB code edits):
#   fishery terminal-year length comp not fit; AVO obs in million-tonnes (absolute-SD
#   normal); CPUE fit once as a survey fleet mirroring the fishery selectivity;
#   BTS/ATS comp sample sizes truncated to integer, 2020 ATS = 1; ATS index
#   Log_sd = sqrt(log(CV^2+1)); AMAK avgsel penalty on (Sel_avgsel_pen = 10).
#
# Rebuild the reference: run "00-fit-admb.R", or by hand --
#   cd ADMB/m23_rceattle_full && export PATH=/usr/local/bin:$PATH && admb pm && ./pm -nox -iprint 150
# =============================================================================

library(Rceattle)
library(dplyr)
library(readxl)

n_selages_fsh <- 12; bts_styr <- 1982; ats_styr <- 1994

# -----------------------------------------------------------------------------
# Data ----
# -----------------------------------------------------------------------------
mydata <- Rceattle::read_data(file = "Data/EBS_24_pollock_single_species_1964-2024.xlsx")
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
est$fleet_control$Fleet_type[fcn %in% c("BTS_1", "ATS_1")] <- "Survey"
est$age_error[1:nages, 3:(nages + 2)] <- diag(nages)       # ageing error off
est$sigma_rec <- 1                                   # full-normal rec penalty (ADMB L1)

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
est$fleet_control$Sel_norm_bin[fcn == "Fishery"]              <- NA
est$fleet_control$Time_varying_sel_sd[fcn == "Fishery"]       <- 0.5     # selvar24.dat

est$fleet_control$Selectivity[fcn == "BTS"]                <- "LogisticPM"
est$fleet_control$Time_varying_sel[fcn == "BTS"]           <- "RandomWalk"
est$fleet_control$Sel_curve_pen1[fcn == "BTS"]             <- 2          # ctrl_flag(26)
est$fleet_control$Sel_curve_pen2[fcn == "BTS"]             <- 0
est$fleet_control$Sel_curve_pen3[fcn == "BTS"]             <- 8          # age-1-dev RW weight
est$fleet_control$Sel_norm_bin[fcn == "BTS"]               <- 3          # penalty age-range lo
est$fleet_control$Sel_norm_bin_upper[fcn == "BTS"]         <- 14         # penalty age-range hi
est$fleet_control$Sel_start_year[fcn == "BTS"]             <- bts_styr
est$fleet_control$Bin_first_selected[fcn == "BTS"]         <- 1
est$fleet_control$Time_varying_sel_sd[fcn == "BTS"]        <- 1

for (fl in c("ATS", "AVO")) {
  est$fleet_control$Selectivity[fcn == fl]               <- "NonParametricPM"
  est$fleet_control$Time_varying_sel[fcn == fl]          <- "RandomWalk"
  est$fleet_control$N_sel_bins[fcn == fl]                <- 8
  est$fleet_control$Sel_curve_pen1[fcn == fl]            <- -1           # penalise INCREASING
  est$fleet_control$Sel_curve_pen2[fcn == fl]            <- 1
  est$fleet_control$Sel_curve_pen3[fcn == fl]            <- 0
  est$fleet_control$Sel_norm_bin[fcn == fl]              <- NA
  est$fleet_control$Bin_first_selected[fcn == fl]        <- 2            # exclude age-1 (ADMB L4/L5)
  est$fleet_control$Sel_pen_first_bin[fcn == fl]         <- 2            # mina_ats
  est$fleet_control$Sel_start_year[fcn == fl]            <- ats_styr
  est$fleet_control$Time_varying_sel_sd[fcn == fl]       <- 0.138        # selvar24.dat
}

# AMAK "avgsel" base-level penalty: fff += 10*square(log(mean(exp(base coffs))))
# (pm.tpl:5535) on the type-9 fleets, accumulated once per shared block (lead fleet).
est$fleet_control$Sel_avgsel_pen[fcn %in% c("Fishery", "ATS", "AVO")] <- 10

# -- survey timing + catchability ---------------------------------------------
est$index_data <- est$index_data %>%
  mutate(Month = case_when(Fleet_name %in% c("BTS", "BTS_1", "ATS", "ATS_1") ~ 6, TRUE ~ 0))
est$comp_data <- est$comp_data %>%
  mutate(Month = case_when(Fleet_name == "BTS" ~ 6, Fleet_name == "ATS" ~ 6, TRUE ~ Month))
est$fleet_control$Catchability <- as.character(est$fleet_control$Catchability)
est$fleet_control$Catchability[fcn == "ATS"]                 <- "Estimated"
est$fleet_control$Catchability[fcn %in% c("BTS_1", "ATS_1")] <- "Analytical"        # geometric-mean
est$fleet_control$Index_distribution[fcn == "BTS"] <- "MVN"                              # DoCovBTS
est$fleet_control$Catchability[fcn == "BTS"]  <- "AnalyticalArith"
# BTS survey biomass variance-covariance matrix (42x42, VAST-derived, 1982-2023).
# It is embedded in the written xlsx (index_cov round-trips), so the fit reads it
# from the xlsx; the source matrix ships with the model in Data/.
est$index_cov <- list(BTS = as.matrix(read.table("Data/BTS_survey_covariance_2024.dat")))

# -- ATS biomass index: the xlsx Log_sd is a CV (std/obs), but ADMB's lognormal
#    variance is lvarb_ats = log(CV^2 + 1) (the exact CV -> log-scale-SD conversion,
#    pm.tpl:1689-1691), and Rceattle's lognormal likelihood uses Log_sd directly as
#    the log-scale SD. Convert CV -> sqrt(log(CV^2 + 1)) so the ATS biomass variance
#    matches ADMB exactly (the +0.01 inside-log offset is negligible at this scale).
ats_rows <- est$index_data$Fleet_name == "ATS"
est$index_data$Log_sd[ats_rows] <- sqrt(log(est$index_data$Log_sd[ats_rows]^2 + 1))

# -- AVO acoustic index: ADMB avo_like is a natural-scale normal with an ABSOLUTE
#    observation SD (ob_avo_std, pm_24.dat), not a lognormal CV. Fit it with
#    Index_distribution = "Normal" (residual obs - q*pred ~ N(0, ob_avo_std^2)) and
#    supply ob_avo_std directly in Log_sd (provided, not estimated).
est$fleet_control$Index_distribution[fcn == "AVO"]     <- "Normal"
# All index SDs are provided (not estimated). Set the WHOLE column as a string alias:
# a per-fleet string assignment would coerce the numeric column to character and leave
# "0" strings on the untouched fleets, which strict (dev-line) validators reject.
est$fleet_control$Estimate_index_sd <- "Fixed"
ob_avo_std <- setNames(
  c(0.407974331, 0.79543824, 0.292865177, 0.390095688, 0.579193251, 0.447677778,
    0.371938445, 0.390115995, 0.58024587, 0.406257388, 0.379092753, 0.317389245,
    0.254960502, 0.63539506, 0.529928784, 0.454780316, 0.335349192, 0.250814465),
  as.character(c(2006:2019, 2021:2024)))
avo_rows <- est$index_data$Fleet_name == "AVO"
stopifnot(sum(avo_rows) == length(ob_avo_std))
est$index_data$Log_sd[avo_rows] <- ob_avo_std[as.character(abs(est$index_data$Year[avo_rows]))]
# AVO obs are thousand-tonnes in the base xlsx (~1741) but ADMB's obs_avo is million-
# tonnes (~1.74); with a natural-scale normal + absolute sigma, rescale to million-
# tonnes so obs, sigma and prediction (q*wt_avo*N*sel_ats) are on the same scale.
est$index_data$Observation[avo_rows] <- est$index_data$Observation[avo_rows] / 1000

# -- composition likelihood: ADMB offset (AFSC) multinomial (NOT full multinomial)
est$fleet_control$Comp_distribution <- "MultinomialAFSC"

# -- ADMB reads survey comp sample sizes as integer vectors (init_ivector sam_bts/
#    sam_ats), truncating the fractional (McAllister-Ianelli) weights; the fishery is a
#    float and left as-is. Truncate BTS/ATS to match so the multinomial weights agree.
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
est$fleet_control$Fleet_type[fcn == "BTS_1"] <- "Off"

# -- ATS age-1 (ATS_1): drop the terminal 2024 obs (ADMB ignore_last_ats_age1, last-
#    year CV 1.81) via the negative-year convention (predicted, not fitted, out of the
#    analytical q). Flip -2020 -> 2020 so ATS/ATS_1 fit 2020 (as the ATS comps do).
est$index_data$Year[est$index_data$Fleet_name == "ATS_1" & est$index_data$Year == 2024]  <- -2024
est$index_data$Year[est$index_data$Fleet_name %in% c("ATS", "ATS_1") &
                      est$index_data$Year == -2020] <- 2020

# -- Drop the base xlsx's CPUE copy (attached to the fishery fleet). ADMB fits the CPUE
#    once (cpue_like); re-adding it below as its own survey fleet would double-count the
#    only early-period index and over-constrain the initial age structure.
fishery_code <- est$fleet_control$Fleet_code[fcn == "Fishery"]
est$index_data <- est$index_data[est$index_data$Fleet_code != fishery_code, ]

# -- Japanese fishery CPUE index (1965-1976): the only abundance index before the BTS
#    (1982), so it pins the early numbers-at-age / initial age structure. Added as a
#    survey fleet mirroring the fishery selectivity (pred = wt_fsh*natage*sel_fsh*q_cpue)
#    with an estimated q; cpue_like is a natural-scale normal, absolute SD in Log_sd.
# Column names throughout this script are the CANONICAL ones. read_data()
# auto-upgrades the older spellings on the way in, so assigning to a deprecated
# name creates a dead column that fit_mod() silently ignores -- the failure mode
# is a mis-built model, not an error. The schema in R/0-column_schema.R is
# authoritative; see .rce_column_schema() aliases for the full mapping.
cpue_row <- est$fleet_control[fcn == "Fishery", ]          # inherit fishery selectivity
cpue_row$Fleet_name <- "CPUE"
cpue_row$Fleet_code <- max(est$fleet_control$Fleet_code) + 1L
cpue_row$Fleet_type <- "Survey"
cpue_row$Catchability_index <- max(est$fleet_control$Catchability_index, na.rm = TRUE) + 1L
cpue_row$Catchability      <- "Estimated"                  # estimated q (log_q_cpue)
cpue_row$Index_distribution <- "Normal"                    # natural-scale normal, absolute SD
cpue_row$Estimate_index_sd  <- "Fixed"
avo_r <- which(fcn == "AVO")
for (col in c("Catchability_init", "Catchability_prior_sd", "Time_varying_q",
              "Time_varying_q_sd", "Estimate_index_sd", "Index_sd"))
  cpue_row[[col]] <- est$fleet_control[avo_r, col]
for (col in c("Estimate_catch_sd", "Catch_sd", "Proj_F_proportion")) cpue_row[[col]] <- NA
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

# -- Fishery length comp: NOT fit. ADMB use_endyr_len = 0 excludes the terminal-year
#    length comp from the objective (pm.rep len_like = 25.795 reported but not summed),
#    so we omit it too -- the terminal fish are already in the fishery AGE comp.

xlsx <- "Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx"
write_data(est, file = xlsx)
message("Wrote ", xlsx)

# -----------------------------------------------------------------------------
# Round-trip check ----
# -----------------------------------------------------------------------------
# The workbook is the only record of this build, so verify read_data() recovers
# what write_data() put there. A silent loss here is how index_cov went missing
# before: an element with no write_data()/read_data() support round-trips to
# nothing and the feature is lost without any error.
rt  <- read_data(file = xlsx)
tol <- 1e-12

cmp <- function(a, b) {
  if (is.data.frame(a) && is.data.frame(b)) {
    if (!identical(dim(a), dim(b))) return(Inf)
    num <- vapply(a, is.numeric, logical(1)) & vapply(b, is.numeric, logical(1))
    if (!any(num)) return(0)
    a <- as.matrix(a[num]); b <- as.matrix(b[num])
  } else if (is.numeric(a) && is.numeric(b)) {
    if (!identical(length(a), length(b))) return(Inf)
  } else {
    return(if (isTRUE(all.equal(a, b))) 0 else Inf)
  }
  d <- abs(as.numeric(a) - as.numeric(b))
  s <- pmax(abs(as.numeric(a)), 1)
  max(d / s, na.rm = TRUE)
}

cat("\n--- Workbook round-trip (write_data -> read_data) ---\n")
bad <- 0
for (nm in intersect(names(est), names(rt))) {
  d <- tryCatch(cmp(est[[nm]], rt[[nm]]), error = function(e) NA_real_)
  if (is.na(d) || d > tol) {
    bad <- bad + 1
    cat(sprintf("  %-24s max rel diff = %-10.3e %s\n", nm, d, "**CHECK**"))
  }
}
missing <- setdiff(names(est), names(rt))
if (length(missing)) cat("  dropped by round-trip:", paste(missing, collapse = ", "), "**CHECK**\n")
if (bad == 0 && !length(missing)) cat("  all elements match to", tol, " OK\n")
