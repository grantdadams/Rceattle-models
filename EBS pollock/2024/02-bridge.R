# =============================================================================
# EBS pollock 2024 -- bridge ADMB ("pm" / AMAK) to Rceattle (FORWARD PASS)
# =============================================================================
# Run from the "EBS pollock" project root.
#
# Two checks, on two workbooks, for two different things:
#
#   DYNAMICS   the raw skeleton, EBS_24_pollock_single_species_1964-2024.xlsx.
#              Parameters are ADMB's, so the config must NOT be inherited from
#              the estimation-time build. Checks N / SSB / catch.
#   LIKELIHOOD the derived workbook from "01-build-data.R". The likelihood only
#              means anything against the bridged configuration (D1-D8), so it
#              has to be the other file. Checks every jnll_comp row against
#              pm.rep.
#
# Reads:   Data/EBS_24_pollock_single_species_1964-2024.xlsx
#          Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx
#          ADMB/m23_rceattle_full/{pm.par, pm.rep}
# Writes:  nothing; console validation tables only
# Prereq:  "00-fit-admb.R" for the ADMB reference, "01-build-data.R" for the
#          derived workbook the likelihood check reads.
#
# Single-sex, single-species model: one fishery + AVO acoustic index, BTS
# bottom-trawl survey, ATS acoustic-trawl survey, and the ATS age-1 index.
# Reference assessment = the STRUCTURALLY-ALIGNED ADMB model in
# ./ADMB/m23_rceattle_full/ (pm.tpl / pm.par / pm.rep). That model is the 2024
# SAFE "pm" (AMAK) model, edited so its equations and likelihoods match
# Rceattle's (the edits are catalogued as S1-S4 / L1-L7 in "03-model-comparison.R").
# Mirrors the GOA Northern rockfish / BSAI Alaska plaice bridging scripts.
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
#                           ~5-6 significant figures, and every likelihood
#                           component to ~1e-5. The parametric-selectivity
#                           ESTIMATION model + comparison is "03-model-comparison.R".
#
# -----------------------------------------------------------------------------
# STRUCTURAL MAPPING (each item applied inline below; an *exact* forward-pass
# match holds because F / recruitment / init-devs are fixed and the realized
# selectivity is injected). See "03-model-comparison.R" for the estimation-time
# differences and the ADMB source edits.
#
#  1. SPAWNING TIMING. ADMB yrfrac = (spawnmo-1)/12 = 0.25 -> spawn_month = 3
#     (Rceattle exp(-Z*spawn_month/12) = exp(-0.25*Z)).
#  2. FEMALE SSB. sex_ratio = 0.5 applied automatically (mature_females =
#     maturity * sex_ratio); do NOT pre-halve maturity.
#  3. SSB / population weight. ssb_wt_index = 5 ("SSB wt"), pop_wt_index = 3.
#  4. NATURAL MORTALITY. Age schedule 0.9 (age1), 0.45 (age2), 0.3 (age3-15),
#     time-invariant; fixed here (M1_model = "fixed").
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
mydata <- Rceattle::read_data(file = "Data/EBS_24_pollock_single_species_1964-2024.xlsx")
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
fp$fleet_control$Selectivity <- "Fixed"        # empirical selectivity for all fleets
fcn <- fp$fleet_control$Fleet_name
fp$fleet_control$Fleet_type[fcn %in% c("BTS_1", "ATS_1")] <- "Survey"   # age-1 abundance indices
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
fp$fleet_control$Catchability[fcn %in% c("BTS", "ATS", "AVO", "BTS_1", "ATS_1")] <- "Analytical"

# -- map the ADMB population MLEs (diff #5, #6) --------------------------------
inits <- build_params(fp)
inits$rec_pars[1, 1]    <- log_avgrec                  # mean recruitment (SrType 3)
inits$rec_dev[1, 1:nyr] <- log_rec_devs
inits$log_F[1, 1:nyr]   <- log_avg_F + log_F_devs      # F = exp(log_F) * sel
inits$init_dev[1, 1:length(log_initdevs)] <- log_initdevs
inits$index_log_q[2]    <- log_q_avo                   # AVO (analytical q for BTS/ATS)

ebs_fixed <- Rceattle::fit_mod(
  data_list    = fp,
  inits        = inits,
  file         = NULL,
  estimateMode = 4,           # all parameters FIXED at inits (forward pass)
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = FALSE,
  initMode     = "NonEquilibrium",           # unfished-equilibrium initial-age cascade + init devs
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = "fixed")   # M fixed at age schedule
)

# -----------------------------------------------------------------------------
# VALIDATION: Model 1 vs ADMB (N / SSB / catch)
# -----------------------------------------------------------------------------
get_blk <- function(name, n = nyr) {                    # [year, age] block from pm.rep
  i0 <- which(rep_lines == name)[1]
  t(sapply(1:n, function(k) as.numeric(strsplit(trimws(rep_lines[i0 + k]), "[[:space:]]+")[[1]])))
}
N_admb   <- t(get_blk("N"))                             # [age, yr]
N_rce    <- ebs_fixed$quantities$N_at_age[1, 1, 1:nages, 1:nyr]
ssb_admb <- get_blk("SSB")[, 2]
ssb_rce  <- as.numeric(ebs_fixed$quantities$ssb[1, 1:nyr])
pred_cat <- as.numeric(strsplit(trimws(rep_lines[which(rep_lines == "pred_catch")[1] + 1]), "[[:space:]]+")[[1]])
cat_rce  <- as.numeric(ebs_fixed$quantities$catch_hat)[1:nyr]

cat("\n--- Forward pass vs ADMB (m23_rceattle_full) ---\n")
cat("N   ratio range :", round(range(N_rce / N_admb), 6), "\n")
cat("SSB ratio range :", round(range(ssb_rce / ssb_admb), 6),
    " | mean |%diff| :", round(100 * mean(abs(ssb_rce / ssb_admb - 1)), 5), "%\n")
cat("Catch mean |%diff| :", round(100 * mean(abs(cat_rce / pred_cat - 1)), 5), "%\n")

plot_ssb(list(ebs_fixed),         model_names = "Rceattle fwd pass")
plot_recruitment(list(ebs_fixed), model_names = "Rceattle fwd pass")
plot_selectivity(ebs_fixed)

# =============================================================================
# LIKELIHOOD CHECK: every component vs ADMB, at ADMB's MLE
# =============================================================================
# The forward pass above fixes the dynamics and checks N / SSB / catch. It runs
# under estimateMode = 4, whose objective is a placeholder (build_map() maps out
# every hindcast parameter, so `dummy` is the only free one) -- it says nothing
# about the likelihood. This block rebuilds the same parameters under
# estimateMode = "DebugBuild", which returns the real objective, and compares
# jnll_comp against pm.rep component by component.
#
# Two adjustments are needed to make the comparison like for like:
#
#  A. AGE-1 (L4/L5). ADMB's ATS biomass index and AVO exclude age 1. Rceattle
#     does that with Bin_first_selected = 2, but Selectivity = "Fixed" reads
#     emp_sel verbatim, so age 1 has to be zeroed in the injected ATS/AVO
#     selectivity. Without it the ATS index is 3.9% high, AVO 5.0%, and the ATS
#     composition reads 393.2 against ADMB's 30.8.
#
#  B. NORMALIZING CONSTANTS. ADMB reports the quadratic part of each index term
#     only. The full negative log-density adds log(sd) + 0.5*log(2*pi) per
#     observation, so the expected gap is n*0.5*log(2*pi) + sum(log(sd)).
#     BTS is the exception: Index_distribution = "MVN" is deliberately the bare
#     quadratic form so the reported value matches ADMB directly.
#     The right long-term fix is to expand the ADMB likelihood statements to
#     full form in pm.tpl; until then this block adds the constants back.
# =============================================================================

# The forward pass above reads the raw skeleton on purpose -- it is testing the
# dynamics and must not inherit the estimation-time configuration. The likelihood
# is the opposite case: it only means anything against the bridged configuration,
# so this block loads the workbook "01-build-data.R" writes (D1-D8 applied:
# MultinomialAFSC comps, index SDs, rescaled AVO, the CPUE fleet, BTS_1 Off).
fpl <- Rceattle::read_data(
  file = "Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx")
fpl$estDynamics <- 0
fpl$fleet_control$Selectivity <- "Fixed"
fcn <- fpl$fleet_control$Fleet_name          # CPUE exists here, unlike the skeleton
fpl$age_error[1:nages, 3:(nages + 2)] <- diag(nages)

sel_ats_no1 <- sel_ats; sel_ats_no1[, 1] <- 0            # (A)
admb_sel_l <- list(Fishery = sel_fsh, BTS = sel_bts, ATS = sel_ats_no1,
                   AVO = sel_ats_no1, CPUE = sel_fsh)
es <- fpl$emp_sel[!(fpl$emp_sel$Fleet_name %in% names(admb_sel_l)), ]
for (fl in names(admb_sel_l)) {
  if (!fl %in% fcn) next
  add <- fpl$emp_sel[0, ]; add[1:nyr, ] <- NA
  add$Fleet_name <- fl; add$Fleet_code <- fpl$fleet_control$Fleet_code[fcn == fl]
  add$Species <- 1; add$Sex <- 0; add$Year <- yrs
  for (a in 1:nages) add[[cc[a]]] <- admb_sel_l[[fl]][, a]
  es <- rbind(es, add[, cols])
}
fpl$emp_sel <- es

# Build the parameters fresh: the derived workbook carries the CPUE fleet, so its
# per-fleet arrays are a row longer than the skeleton's and `inits` above cannot
# be reused. Every ESTIMATED q needs ADMB's value; BTS / BTS_1 / ATS_1 are solved
# analytically in Rceattle, which is the analog of ADMB's solved q.
inits_l <- build_params(fpl)
inits_l$rec_pars[1, 1]    <- log_avgrec
inits_l$rec_dev[1, 1:nyr] <- log_rec_devs
inits_l$log_F[1, 1:nyr]   <- log_avg_F + log_F_devs
inits_l$init_dev[1, 1:length(log_initdevs)] <- log_initdevs
for (q in list(c("AVO", "log_q_avo"), c("ATS", "log_q_ats"), c("CPUE", "log_q_cpue"))) {
  i <- which(fcn == q[1])
  if (length(i)) inits_l$index_log_q[i] <- get_par(q[2])[1]
}

ebs_like <- Rceattle::fit_mod(
  data_list = fpl, inits = inits_l, file = NULL,
  estimateMode = "DebugBuild",             # real objective, unlike mode 4
  random_rec = FALSE, msmMode = 0, initMode = "NonEquilibrium",
  M1Fun = build_M1(updateM1 = TRUE, M1_model = "fixed"),
  fit_control = fit_control(verbose = 0, phase = FALSE, bias_adjust_proc = 0,
                            bias_adjust_obs = 0, comp_offset = 1e-3))

jc   <- ebs_like$quantities$jnll_comp       # rows follow the JnllRow enum
flnm <- ebs_like$data_list$fleet_control$Fleet_name
idxd <- ebs_like$data_list$index_data
gv   <- function(n) as.numeric(strsplit(trimws(rep_lines[which(rep_lines == n)[1] + 1]),
                                        "[[:space:]]+")[[1]])
TOL  <- 1e-3
flag <- function(d) if (abs(d) < TOL) "OK" else "**CHECK**"

cat("\n--- Likelihood components vs ADMB (at ADMB's MLE) ---\n")
cat(sprintf("  %-22s %13s %13s %11s  %s\n", "component", "Rceattle", "ADMB", "diff", ""))

# Index, per fleet. `konst` is the normalizing constant ADMB omits (B); MVN is
# already reported bare, so it takes none.
admb_idx <- c(BTS = gv("surv_like")[1], ATS = gv("surv_like")[2],
              ATS_1 = gv("surv_like")[3], AVO = gv("avo_like"),
              CPUE = gv("cpue_like"))
for (fl in names(admb_idx)) {
  i <- which(flnm == fl); if (!length(i)) next
  rows  <- which(idxd$Fleet_name == fl & idxd$Year > 0 &
                   idxd$Year <= ebs_like$data_list$endyr)
  mvn   <- ebs_like$data_list$fleet_control$Index_distribution[i] %in% c("MVN", "MVNORM", 1, 2)
  konst <- if (mvn) 0 else
    length(rows) * 0.5 * log(2 * pi) + sum(log(idxd$Log_sd[rows]))
  d <- (jc[1, i] - konst) - admb_idx[[fl]]
  cat(sprintf("  index %-16s %13.5f %13.5f %11.5f  %s\n",
              fl, jc[1, i] - konst, admb_idx[[fl]], d, flag(d)))
}

# Composition, per fleet (ADMB age_like: fishery, BTS, ATS)
admb_comp <- c(Fishery = gv("age_like")[1], BTS = gv("age_like")[2],
               ATS = gv("age_like")[3])
for (fl in names(admb_comp)) {
  i <- which(flnm == fl); if (!length(i)) next
  d <- jc[3, i] - admb_comp[[fl]]
  cat(sprintf("  comp  %-16s %13.5f %13.5f %11.5f  %s\n",
              fl, jc[3, i], admb_comp[[fl]], d, flag(d)))
}

# Recruitment penalties. ADMB packs both into rec_like: [2] = recruitment
# deviations, [4] = initial-age deviations. Rceattle reports them as separate
# rows, so compare element by element rather than against the vector sum.
rl_admb <- gv("rec_like")
for (p in list(c(11, 2, "rec_dev"), c(10, 4, "init_dev"))) {
  r <- sum(jc[as.integer(p[1]), ]); a <- rl_admb[as.integer(p[2])]
  cat(sprintf("  %-22s %13.5f %13.5f %11.5f  %s\n", p[3], r, a, r - a, flag(r - a)))
}

cat("\n  Not compared: ADMB's sel_like (", round(sum(gv("sel_like")), 2),
    ") and sel_like_dev (", round(sum(gv("sel_like_dev")), 2),
    ") are selectivity\n  penalties that emp_sel bypasses, and pm.rep carries no catch_like block.\n",
    sep = "")
