# =============================================================================
# 2024 EBS pollock - BRIDGING ADMB ("pm" / AMAK) -> Rceattle  (FORWARD PASS)
# =============================================================================
# Single-sex, single-species model: one fishery + AVO acoustic index, BTS
# bottom-trawl survey, ATS acoustic-trawl survey, and the ATS age-1 index.
# Reference assessment = the STRUCTURALLY-ALIGNED ADMB model in
# ./ADMB/m23_rceattle_full/ (pm.tpl / pm.par / pm.rep). That model is the 2024
# SAFE "pm" (AMAK) model, edited so its equations and likelihoods match
# Rceattle's (the edits are catalogued in "2024 EBS pollock.R"). Mirrors the
# GOA Northern rockfish / BSAI Alaska plaice bridging scripts.
#
#   Model 1 (forward pass): Rceattle population dynamics FIXED to the ADMB MLEs.
#                           Because the ADMB selectivities are heavily time-
#                           varying (random walks, ~1300 parameters), we do NOT
#                           re-map them parametrically here; we inject ADMB's
#                           REALIZED selectivity (sel_fsh / sel_bts / sel_ats)
#                           through the empirical-selectivity bypass (emp_sel),
#                           and let Rceattle COMPUTE numbers-at-age from the
#                           mapped F / recruitment / initial-devs (estDynamics=0).
#                           VALIDATION below: N / SSB / catch reproduce ADMB to
#                           ~5-6 significant figures. The parametric-selectivity
#                           ESTIMATION model + comparison is "2024 EBS pollock.R".
#
# -----------------------------------------------------------------------------
# STRUCTURAL MAPPING (each item applied inline below; an *exact* forward-pass
# match holds because F / recruitment / init-devs are fixed and the realized
# selectivity is injected). See "2024 EBS pollock.R" for the estimation-time
# differences and the ADMB source edits.
#
#  1. SPAWNING TIMING. ADMB yrfrac = (spawnmo-1)/12 = 0.25 -> spawn_month = 3
#     (Rceattle exp(-Z*spawn_month/12) = exp(-0.25*Z)).
#  2. FEMALE SSB. sex_ratio = 0.5 applied automatically (mature_females =
#     maturity * sex_ratio); do NOT pre-halve maturity.
#  3. SSB / population weight. ssb_wt_index = 5 ("SSB wt"), pop_wt_index = 3.
#  4. NATURAL MORTALITY. Age schedule 0.9 (age1), 0.45 (age2), 0.3 (age3-15),
#     time-invariant; fixed here (M1_model = 0).
#  5. FISHING MORTALITY. F_at_age = exp(log_F[fleet,yr]) * sel_at_age, so
#     log_F = log_avg_F + log_F_devs reproduces ADMB Fmort.
#  6. RECRUITMENT. Mean recruitment (SrType = 3): R = exp(log_avgrec + rec_dev).
#  7. SURVEY CATCHABILITY. ADMB solves q analytically (geometric mean obs/pred);
#     Rceattle Catchability = 3 ("Analytical") is the analog. The ATS age-1
#     index sigma = age1_sigma_ats = 1.
#  8. SELECTIVITY. Injected as realized values via emp_sel (Selectivity = 0),
#     refreshed from ADMB/m23_rceattle_full/pm.rep so the survey/fishery
#     selectivity-at-age equals ADMB exactly.
#
# VALIDATION (Model 1 vs ADMB, this machine):
#   N / SSB reproduce ADMB to ~5 sig figs (ratio in [1, 1.00001]) across
#   1964-2024; catch matches ADMB pred_catch to ~5 sig figs.
# =============================================================================

library(Rceattle)
library(dplyr)
library(readxl)

AD <- "ADMB/m23_rceattle_full"          # aligned/edited ADMB reference

# -----------------------------------------------------------------------------
# Data
# -----------------------------------------------------------------------------
mydata <- Rceattle::read_data(file = "Data/2024_EBS_pollock.xlsx")
styr  <- mydata$styr     # 1964
endyr <- mydata$endyr    # 2024
nages <- mydata$nages    # 15
yrs   <- styr:endyr
nyr   <- length(yrs)

# NByageFixed ships with Age1..Age30 columns; trim to the nages model columns.
keep_age <- c("Species_name", "Species", "Sex", "Year", paste0("Age", 1:nages))
mydata$NByageFixed <- mydata$NByageFixed[, intersect(keep_age, colnames(mydata$NByageFixed))]

# Spawning timing (diff #1): ADMB yrfrac 0.25 -> spawn_month = 3.
mydata$spawn_month <- 3

# -----------------------------------------------------------------------------
# Parse the ADMB MLEs (pm.par) and realized selectivity (pm.rep)
# -----------------------------------------------------------------------------
par_lines <- readLines(file.path(AD, "pm.par"))
get_par <- function(name) {
  i <- which(par_lines == paste0("# ", name, ":"))[1]
  vals <- c(); j <- i + 1
  while (j <= length(par_lines) && !grepl("^#", par_lines[j])) {
    vals <- c(vals, as.numeric(strsplit(trimws(par_lines[j]), "\\s+")[[1]]))
    j <- j + 1
  }
  vals
}
rep_lines <- readLines(file.path(AD, "pm.rep"))
get_sel <- function(name) {                              # [year, age] realized sel
  i0 <- which(rep_lines == name)[1]
  t(sapply(seq_len(nyr), function(k)
    as.numeric(strsplit(trimws(rep_lines[i0 + k]), "[[:space:]]+")[[1]])))
}

log_avgrec   <- get_par("log_avgrec")
log_avg_F    <- get_par("log_avg_F")
log_F_devs   <- get_par("log_F_devs")     # 1964..2024
log_rec_devs <- get_par("log_rec_devs")   # 1964..2024
log_initdevs <- get_par("log_initdevs")   # 14 (ages 2..15)
log_q_avo    <- get_par("log_q_avo")

sel_fsh <- get_sel("sel_fsh")
sel_bts <- get_sel("sel_bts")
sel_ats <- get_sel("sel_ats")             # AVO uses ATS selectivity (pm.tpl)

# =============================================================================
# Model 1 - FORWARD PASS: dynamics computed from the ADMB MLEs
# -----------------------------------------------------------------------------
# estDynamics = 0 -> Rceattle COMPUTES numbers-at-age from the mapped F /
# recruitment / init-devs (this genuinely tests the dynamics, unlike
# estDynamics = 1 which injects N). Selectivity is bypassed empirically.
# =============================================================================
fp <- mydata
fp$estDynamics <- 0
fp$fleet_control$Selectivity <- 0              # empirical selectivity for all fleets
fcn <- fp$fleet_control$Fleet_name
fp$fleet_control$Fleet_type[fcn %in% c("BTS_1", "ATS_1")] <- 2   # age-1 abundance indices
fp$age_error[1:nages, 3:(nages + 2)] <- diag(nages)              # ageing error off (identity)

# -- inject ADMB realized selectivity via emp_sel (diff #8). emp_sel uses
#    Comp_1..Comp_n columns; BTS_1 / ATS_1 keep their own age-1 (1,0,0,..) rows.
admb_sel <- list(Fishery = sel_fsh, BTS = sel_bts, ATS = sel_ats, AVO = sel_ats)
cc  <- paste0("Comp_", 1:nages)
cols <- colnames(fp$emp_sel)
es <- fp$emp_sel[!(fp$emp_sel$Fleet_name %in% names(admb_sel)), ]   # keep BTS_1 / ATS_1
for (fl in names(admb_sel)) {
  add <- fp$emp_sel[0, ]; add[1:nyr, ] <- NA
  add$Fleet_name <- fl; add$Fleet_code <- fp$fleet_control$Fleet_code[fcn == fl]
  add$Species <- 1; add$Sex <- 0; add$Year <- yrs
  for (a in 1:nages) add[[cc[a]]] <- admb_sel[[fl]][, a]
  es <- rbind(es, add[, cols])
}
fp$emp_sel <- es

# -- survey timing (mid-year) + analytical q (diff #7) -------------------------
fp$index_data <- fp$index_data %>%
  mutate(Month = case_when(Fleet_name %in% c("BTS", "BTS_1", "ATS", "ATS_1") ~ 6, TRUE ~ 0))
fp$fleet_control$Catchability <- as.character(fp$fleet_control$Catchability)
fp$fleet_control$Catchability[fcn %in% c("BTS", "ATS", "AVO", "BTS_1", "ATS_1")] <- 3

# -- map the ADMB population MLEs (diff #5, #6) --------------------------------
inits <- build_params(fp)
inits$rec_pars[1, 1]    <- log_avgrec                  # mean recruitment (SrType 3)
inits$rec_dev[1, 1:nyr] <- log_rec_devs
inits$log_F[1, 1:nyr]   <- log_avg_F + log_F_devs      # F = exp(log_F) * sel
inits$init_dev[1, 1:length(log_initdevs)] <- log_initdevs
inits$index_log_q[2]    <- log_q_avo                   # AVO (analytical q for BTS/ATS)

bridging_model_1 <- Rceattle::fit_mod(
  data_list    = fp,
  inits        = inits,
  file         = NULL,
  estimateMode = 4,           # all parameters FIXED at inits (forward pass)
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = FALSE,
  initMode     = 2,           # unfished-equilibrium initial-age cascade + init devs
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 0)   # M fixed at age schedule
)

# -----------------------------------------------------------------------------
# VALIDATION: Model 1 vs ADMB (N / SSB / catch)
# -----------------------------------------------------------------------------
get_blk <- function(name, n = nyr) {                    # [year, age] block from pm.rep
  i0 <- which(rep_lines == name)[1]
  t(sapply(1:n, function(k) as.numeric(strsplit(trimws(rep_lines[i0 + k]), "[[:space:]]+")[[1]])))
}
N_admb   <- t(get_blk("N"))                             # [age, yr]
N_rce    <- bridging_model_1$quantities$N_at_age[1, 1, 1:nages, 1:nyr]
ssb_admb <- get_blk("SSB")[, 2]
ssb_rce  <- as.numeric(bridging_model_1$quantities$ssb[1, 1:nyr])
pred_cat <- as.numeric(strsplit(trimws(rep_lines[which(rep_lines == "pred_catch")[1] + 1]), "[[:space:]]+")[[1]])
cat_rce  <- as.numeric(bridging_model_1$quantities$catch_hat)[1:nyr]

cat("\n--- Forward pass vs ADMB (m23_rceattle_full) ---\n")
cat("N   ratio range :", round(range(N_rce / N_admb), 6), "\n")
cat("SSB ratio range :", round(range(ssb_rce / ssb_admb), 6),
    " | mean |%diff| :", round(100 * mean(abs(ssb_rce / ssb_admb - 1)), 5), "%\n")
cat("Catch mean |%diff| :", round(100 * mean(abs(cat_rce / pred_cat - 1)), 5), "%\n")

plot_ssb(list(bridging_model_1),         model_names = "Rceattle fwd pass")
plot_recruitment(list(bridging_model_1), model_names = "Rceattle fwd pass")
plot_selectivity(bridging_model_1)
