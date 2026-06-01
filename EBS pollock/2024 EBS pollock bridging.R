# =============================================================================
# 2024 EBS pollock - BRIDGING ADMB ("pm" / AMAK) -> Rceattle
# =============================================================================
# Single-sex, single-species model with one fishery and several surveys
# (AVO acoustic index, BTS bottom-trawl survey, ATS acoustic-trawl survey, and
# the BTS/ATS age-1 indices). Reference assessment = ADMB ./ADMB/m23/ (pm.tpl,
# pm.par, pm.rep), the 2024 SAFE model. Mirrors the structure of the GOA
# Northern rockfish and BSAI Alaska plaice bridging scripts.
#
#   Model 1 (forward pass): Rceattle population dynamics FIXED to the ADMB MLEs.
#                           Because the ADMB fishery/survey selectivities are
#                           heavily time-varying (random walks, 1366 parameters),
#                           we do NOT re-map them parametrically. Instead we use
#                           the two diagnostic bypasses already shipped in the
#                           data file (same approach as the GOA Pcod bridge):
#                             - estDynamics = 1  -> numbers-at-age fixed to the
#                               ADMB N matrix (sheet 'NByageFixed' = pm.rep "N").
#                             - Selectivity = 0  -> empirical selectivity read
#                               from sheet 'emp_sel' (= ADMB sel_fsh/sel_bts/
#                               sel_ats, verified equal to pm.rep to 6 sig figs).
#                           F and recruitment are mapped from pm.par so that Z
#                           (hence spawning survival) and the reported R series
#                           reproduce ADMB. VALIDATION below: SSB / catch match
#                           ADMB to ~6 significant figures.
#   Model 2 (estimation):   Rceattle estimates everything, M fixed at the ADMB
#                           age schedule (0.9, 0.45, 0.3 x13).
#   Model 3 (estimation):   as Model 2 but age/time-invariant M estimated.
#
# -----------------------------------------------------------------------------
# STRUCTURAL DIFFERENCES / MAPPING (why an *exact* estimation match is not
# expected; the forward pass is exact for the population trajectory because
# numbers-at-age are fixed). Each item is applied/handled inline below.
#
#  1. SPAWNING TIMING (correction, exact). ADMB hardcodes spawnmo = 4 and uses
#     yrfrac = (spawnmo-1)/12 = 0.25 (pm.tpl ~L472), i.e. SSB =
#     elem_prod(natage * S^0.25, p_mature) * wt_ssb. Rceattle uses
#     exp(-Z * spawn_month/12); set spawn_month = 3 so 3/12 = 0.25. (The shipped
#     control sheet has spawn_month = 2.52 - that is the calendar spawn month,
#     NOT the ADMB yrfrac, and must be overridden to 3.)
#
#  2. FEMALE SSB / SEX RATIO (mapping, exact). ADMB sets p_mature *= 0.5
#     (pm.tpl ~L321) so SSB is female spawning biomass in a 1-sex model.
#     Rceattle computes mature_females = maturity * sex_ratio (cpp ~L620); the
#     data 'sex_ratio' sheet = 0.5 for all ages, so the 0.5 is applied
#     automatically - do NOT pre-halve the 'pmature' sheet.
#
#  3. SSB WEIGHT (mapping, exact). ADMB wt_ssb defaults to wt_fsh
#     (use_popwts_ssb == 0, pm.tpl ~L331). The data file ships this as weight
#     index 5 ('SSB wt'); control ssb_wt_index = 5 -> handled automatically.
#
#  4. NATURAL MORTALITY (mapping, exact). ADMB M is age-varying and time-
#     invariant: 0.9 (age1), 0.45 (age2), 0.3 (age3-15). Data 'M1_base' already
#     holds this; Model 1 fixes it (M1_model = 0).
#
#  5. FISHING MORTALITY (mapping, exact). ADMB Fmort = exp(log_avg_F +
#     log_F_devs); F(age) = Fmort * sel_fsh (pm.tpl ~L1770, ~L1779). Rceattle
#     F_at_age = exp(log_F[fleet, yr]) * sel_at_age, so with empirical sel_fsh
#     we set log_F[fishery, yr] = log_avg_F + log_F_devs and F-at-age matches.
#     => predicted catch matches ADMB to ~6 sig figs (validates F + sel_fsh).
#
#  6. RECRUITMENT (mapping). ADMB age-1 N = exp(log_avgrec + log_rec_devs).
#     Under estDynamics = 1 the age-1 numbers are already fixed (NByageFixed),
#     but Rceattle reports R from its own recruitment process; we set
#     rec_pars[1,1] = log_avgrec and rec_dev = log_rec_devs so the reported R
#     series reproduces ADMB for years 2..nyr. CAVEAT: under estDynamics = 1
#     Rceattle does not assign R in the FIRST year (cpp case 1 skips the styr
#     R(sp,0) assignment), so R[styr] reports 0 - we patch it post-hoc from the
#     age-1 numbers for plotting. (Cosmetic only; SSB/biomass are unaffected.)
#
#  7. SURVEY CATCHABILITY (analytical q; survey indices). ADMB predicts survey
#     biomass as eb = q * sum(wt_srv * N * S^0.5 * sel_srv) (pm.tpl ~L2852) where
#     the BTS/ATS weight-at-age (wt_bts / wt_ats) EXACTLY equals the Rceattle
#     survey weight indices (3 = BTS, 4 = ATS) - verified equal - and the
#     selectivity shape equals emp_sel. The catch is that q is NOT exp(log_q_*):
#     ADMB SOLVES q ANALYTICALLY, q_bts = mean(ob_bts)/mean(eb_bts) (pm.tpl
#     ~L3808; phase_q_bts < 0). Rceattle's Catchability = 3 ("Analytical",
#     Ludwig & Walters 1994) does the same closed-form q, so we set BTS/ATS
#     Catchability = 3 and the predicted index then matches ADMB to ~9% (BTS) /
#     ~16% (ATS). RESIDUAL: Rceattle's analytical q is the GEOMETRIC mean of the
#     obs/pred ratios (exp(mean(log(obs/pred)))) whereas ADMB uses the ARITHMETIC
#     mean ratio - a small, structural estimator difference.
#       Two further ADMB-specific BTS q pieces are inactive/handled: a
#       "standard-area" adjustment for the early (pre-1985 + 1986) survey years
#       (log_q_std_area, pm.tpl ~L5937), and an optional bottom-temperature term
#       q_temp = bt_slope*(age-3)*2/3 + q_bts (pm.tpl ~L2256) which is INERT here
#       (bt_slope = 0). Rceattle CAN represent an environmentally-driven q via
#       Catchability = 5/6 -> log(q_y) = q_mu + beta * env_index_y (env_data
#       ships bottom temperature); see Model 2 note. AVO keeps its estimated
#       log_q_avo (acoustic index, not analytical in ADMB).
#
#  8. AGE-1 INDICES (structural). BTS_1 / ATS_1 are age-1 abundance indices with
#     empirically-derived q in ADMB (q = mean(log(obs) - log(pred))); they are
#     set up as surveys (Fleet_type = 2) with empirical age-1 selectivity.
#
#  9. COMPOSITION LIKELIHOOD (structural, but NOT the residual driver). ADMB
#     iteratively reweights comps (Francis: FW_fsh/FW_bts/FW_ats = 0.84/1.27/2.09
#     in pm.rep); Rceattle uses its multinomial with Comp_weights. Setting
#     Comp_weights to the ADMB Francis weights was TESTED and moved SSB < 0.2 %,
#     so it is not the source of the estimation difference (left at 1 here).
#
# 10. sigmaR (structural, but NOT the residual driver). ADMB estimates sigr
#     (~1.0); Rceattle uses a fixed sigma_rec_prior = 1.3 (from control). Setting
#     it to 1.0 was TESTED and moved SSB < 0.2 %. Plus the recruitment / initial-
#     dev penalty bias-correction (Rceattle centres at +/- sigma^2/2, ADMB at 0)
#     diverges by design - largest in early, data-poor years (sub-1 % overall).
#
# 11. SELECTIVITY FORM + INITIALIZATION (the levers that DO matter; applied in
#     Models 2/3 below). The ADMB "pm" model IS Ianelli's AMAK, so:
#       - fishery selectivity = Ianelli (2018) non-parametric with annual devs
#         (= ADMB sel_coffs_fsh + sel_devs_fsh) -> Rceattle Selectivity =
#         "NonParametric" + Time_varying_sel = "IID".
#       - BTS selectivity = logistic random walk (= ADMB sel_slp/a50_bts + devs)
#         -> Time_varying_sel = "RandomWalk".
#       - initial age structure = free (= ADMB log_avginit + log_initdevs)
#         -> initMode = 0 ("FreeParams").
#     Stacking these takes the Model-2 vs ADMB SSB correlation from 0.88 (single
#     time-invariant Hake sel + unfished-equilibrium init) to ~0.999.
#
# 12. SURVEY q ESTIMATOR (the dominant RESIDUAL; see diff #7). After diff #11 the
#     trajectory SHAPE overlays ADMB (corr ~0.999) but a near-constant ~9 % SCALE
#     offset remains (SSB ratio 1.09 +/- 0.03). Cause: the analytical survey q.
#     Rceattle's q is the GEOMETRIC mean of obs/pred ratios (consistent with its
#     lognormal index likelihood; q_bts ~ 2.33); ADMB's is the ARITHMETIC ratio
#     mean(obs)/mean(pred) (consistent with its natural-scale normal survey
#     likelihood; q_bts ~ 2.81). Lower q -> ~9 % higher inferred biomass. Both
#     are the correct closed-form q for their OWN error model (Walters & Ludwig
#     1994: eq.15 lognormal -> geometric; eq.6 normal -> arithmetic). Closing
#     this to ~100 % would require a cpp option for an ADMB-style natural-scale
#     normal survey likelihood + arithmetic q - NOT done here (documented only).
#
# 13. OPTIMIZER / PHASING. ADMB phasing vs the TMB optimizer - expected, not chased.
#
# VALIDATION (run on this machine):
#   Model 1 (forward pass) vs ADMB SAFE:
#     SSB   : mean |%diff| ~ 0.0001 % over 1964-2024 (essentially exact)
#     R     : exact for 1965-2024 (R[1964] reports 0, see mapping #6)
#     Catch : mean |%diff| ~ 0.0002 % over 1964-2024 (validates F + sel_fsh)
#     BTS   : mean |%diff| ~ 9 %, ATS ~ 16 % (analytical q; diff #7/#12)
#   Model 2 (estimation) vs ADMB SAFE:
#     SSB correlation ~0.999, mean |%diff| ~9 % (constant scale, diff #12)
#     R   correlation ~0.97
# =============================================================================

library(Rceattle)
library(dplyr)
library(readxl)

# -----------------------------------------------------------------------------
# Data
# -----------------------------------------------------------------------------
mydata <- Rceattle::read_data(file = "Data/BSP0.xlsx")
styr  <- mydata$styr     # 1964
endyr <- mydata$endyr    # 2024
nages <- mydata$nages    # 15
yrs   <- styr:endyr
nyr   <- length(yrs)

# NByageFixed ships with Age1..Age30 columns; trim to the nages model columns.
keep_age <- c("Species_name", "Species", "Sex", "Year", paste0("Age", 1:nages))
mydata$NByageFixed <- mydata$NByageFixed[, keep_age]

# Spawning timing: ADMB yrfrac = (spawnmo-1)/12 = 0.25 -> spawn_month = 3 (diff #1)
mydata$spawn_month <- 3


# =============================================================================
# Model 1 - FORWARD PASS: Rceattle dynamics fixed to the ADMB MLEs
# -----------------------------------------------------------------------------
# Parse ADMB/m23/pm.par and map F, recruitment, and q into the init list. N-at-
# age and selectivity come from the data bypasses (NByageFixed / emp_sel).
# =============================================================================

par_lines <- readLines("ADMB/m23/pm.par")
get_par <- function(name) {
  hdr <- paste0("# ", name, ":")
  i <- which(par_lines == hdr)[1]
  if (is.na(i)) stop("parameter not found in pm.par: ", name)
  vals <- c(); j <- i + 1
  while (j <= length(par_lines) && !grepl("^#", par_lines[j])) {
    vals <- c(vals, as.numeric(strsplit(trimws(par_lines[j]), "\\s+")[[1]]))
    j <- j + 1
  }
  vals
}
log_avgrec   <- get_par("log_avgrec")      # 9.8272
log_avg_F    <- get_par("log_avg_F")       # -1.4191
log_F_devs   <- get_par("log_F_devs")      # 1964..2024 (61)
log_rec_devs <- get_par("log_rec_devs")    # 1964..2024 (61)
log_q_bts    <- get_par("log_q_bts")       # 0      -> q_bts = 1
log_q_ats    <- get_par("log_q_ats")       # -0.616
log_q_avo    <- get_par("log_q_avo")       # -8.187

# -- forward-pass data: fix N-at-age (estDynamics = 1) + empirical selectivity --
fp_data <- mydata
fp_data$estDynamics       <- 1   # use NByageFixed numbers-at-age (diff: bypass)
fp_data$fleet_control$Selectivity <- 0   # empirical selectivity from emp_sel
# survey timing at mid-year (ADMB pow(S, 0.5), pm.tpl ~L2845)
fp_data$index_data <- fp_data$index_data %>%
  dplyr::mutate(Month = dplyr::case_when(
    Fleet_name %in% c("BTS", "BTS_1", "ATS", "ATS_1") ~ 6, TRUE ~ 0))
# analytical survey q for BTS/ATS (diff #7): ADMB q = mean(obs)/mean(pred)
fp_data$fleet_control$Catchability <- as.character(fp_data$fleet_control$Catchability)
fp_data$fleet_control$Catchability[fp_data$fleet_control$Fleet_name %in% c("BTS", "ATS")] <- 3

inits <- build_params(fp_data)

# -- fishing mortality (fishery = fleet row 1): F = exp(log_F)*sel (diff #5) ----
inits$log_F[1, 1:nyr] <- log_avg_F + log_F_devs

# -- recruitment (diff #6): reproduce reported R = exp(log_avgrec + rec_dev) ----
inits$rec_pars[1, 1]  <- log_avgrec
inits$rec_dev[1, 1:nyr] <- log_rec_devs

# -- AVO catchability (diff #7): Q_index order = 1 Fishery, 2 AVO, 3 BTS, 4 ATS,
#    5 BTS_1, 6 ATS_1. BTS/ATS use analytical q (set above); AVO keeps log_q_avo
inits$index_log_q[2] <- log_q_avo

bridging_model_1 <- Rceattle::fit_mod(
  data_list    = fp_data,
  inits        = inits,
  file         = NULL,
  estimateMode = 4,           # all parameters FIXED at inits (forward pass)
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = FALSE,
  initMode     = 2,
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 0)   # M fixed (age schedule)
)

# -- patch R[styr]: estDynamics=1 does not assign first-year R (diff #6) --------
bridging_model_1$quantities$R[1, 1] <- bridging_model_1$quantities$N_at_age[1, 1, 1, 1]

# -- VALIDATION: forward pass vs ADMB SAFE --------------------------------------
adm_ssb <- as.data.frame(read_excel("Data/2024_ADMB_estimate.xlsx", sheet = "SSB"))
adm_r   <- as.data.frame(read_excel("Data/2024_ADMB_estimate.xlsx", sheet = "Recruitment"))

ssb_fp <- bridging_model_1$quantities$ssb[1, 1:nyr]
R_fp   <- bridging_model_1$quantities$R[1, 1:nyr]

cat("\n--- Forward pass vs ADMB SAFE ---\n")
cat("SSB mean |%diff|:", round(100 * mean(abs(ssb_fp / adm_ssb$Est - 1)), 5),
    "  max:", round(100 * max(abs(ssb_fp / adm_ssb$Est - 1)), 5), "%\n")
cat("R   mean |%diff| (1965-2024):",
    round(100 * mean(abs(R_fp[-1] / adm_r$Est[-1] - 1)), 5), "%\n")

# predicted catch vs ADMB pred_catch (validates F + sel_fsh)
rl <- readLines("ADMB/m23/pm.rep")
get_rep <- function(name) as.numeric(strsplit(trimws(rl[which(rl == name) + 1]), "\\s+")[[1]])
pred_catch <- get_rep("pred_catch")
catch_fp   <- as.numeric(bridging_model_1$quantities$catch_hat)[1:nyr]
cat("Catch mean |%diff|:", round(100 * mean(abs(catch_fp / pred_catch - 1)), 5), "%\n")

# predicted survey biomass vs ADMB eb_bts / eb_ats (analytical q, diff #7)
idx_fp <- bridging_model_1$data_list$index_data
idx_fp$pred <- as.numeric(bridging_model_1$quantities$index_hat)
eb_bts <- get_rep("eb_bts"); eb_ats <- get_rep("eb_ats")
bts_fp <- idx_fp[idx_fp$Fleet_name == "BTS", ]; bts_fp <- bts_fp[order(bts_fp$Year), ]
ats_fp <- idx_fp[idx_fp$Fleet_name == "ATS", ]; ats_fp <- ats_fp[order(ats_fp$Year), ]
cat("BTS index mean |%diff|:", round(100 * mean(abs(bts_fp$pred / eb_bts - 1)), 2), "%\n")
cat("ATS index mean |%diff|:",
    round(100 * mean(abs(ats_fp$pred / eb_ats[seq_len(nrow(ats_fp))] - 1)), 2), "%\n")


# =============================================================================
# Model 2 - ESTIMATION, M fixed at the ADMB age schedule
# -----------------------------------------------------------------------------
# Full Rceattle estimation. Selectivity returns to its parametric forms from the
# data file. The selectivity / initialization are set to mirror the ADMB
# ("pm" = Ianelli AMAK) structure as closely as Rceattle allows (see diff #11):
#   - Fishery: Ianelli (2018) non-parametric selectivity ("NonParametric") with
#     annual IID deviations  (= ADMB sel_coffs_fsh + sel_devs_fsh).
#   - BTS: logistic with a random walk over time (= ADMB sel_slp_bts/sel_a50_bts
#     + annual deviations).
#   - initMode = 0 (free initial N-at-age = ADMB log_avginit + log_initdevs).
#   - BTS/ATS analytical q (diff #7); AVO mirrored to ATS (Selectivity_index = 2).
# Index Log_sd is converted from an absolute SD to a CV (Log_sd / Observation);
# catch Log_sd = 0.05; survey timing = mid-year (month 6); age error off
# (identity); BTS_1 / ATS_1 are treated as age-1 abundance surveys.
#
# RESULT (Model 2 vs ADMB SAFE): SSB correlation ~0.999 (the trajectory shape
# overlays ADMB) with a near-constant ~9 % scale offset (SSB ratio 1.09 +/- 0.03)
# driven by the survey-q estimator (diff #7); R correlation ~0.97. Comp weights
# (ADMB Francis FW_fsh/bts/ats = 0.84/1.27/2.09) and sigmaR (ADMB ~1.0 vs the
# fixed sigma_rec_prior = 1.3) were tested and each moved SSB < 0.2 % - they are
# NOT the source of the residual.
#
# NOTE (diff #7b): to emulate ADMB's bottom-temperature effect on BTS q when it
# is active (bt_slope != 0), set the BTS fleet_control Catchability = 5 and
# Time_varying_q = (env index column) so log(q_y) = q_mu + beta * temperature_y,
# with env_data supplying bottom temperature. In the 2024 run bt_slope = 0 so
# this is left off here.
# =============================================================================
est_data <- mydata
est_data$estDynamics <- 0
est_data$index_data$Log_sd <- est_data$index_data$Log_sd / est_data$index_data$Observation  # SD -> CV
est_data$catch_data$Log_sd <- 0.05
est_data$fleet_control$Fleet_type[5:6]      <- 2   # BTS_1 / ATS_1 age-1 abundance indices
est_data$age_error[1:nages, 3:(nages + 2)]  <- diag(nages)  # age error off (identity)
# fishery = Ianelli non-parametric sel with IID time-varying coefficients; BTS
# logistic with a random walk (diff #11)
est_data$fleet_control$Selectivity[est_data$fleet_control$Fleet_name == "Fishery"]      <- "NonParametric"
est_data$fleet_control$Time_varying_sel[est_data$fleet_control$Fleet_name == "Fishery"] <- "IID"
est_data$fleet_control$Time_varying_sel[est_data$fleet_control$Fleet_name == "BTS"]     <- "RandomWalk"
# survey timing: BTS/ATS surveys at mid-year (month 6); acoustic AVO + fishery at 0
est_data$index_data <- est_data$index_data %>%
  dplyr::mutate(Month = dplyr::case_when(
    Fleet_name %in% c("BTS", "BTS_1", "ATS", "ATS_1") ~ 6,
    TRUE ~ 0))
est_data$comp_data <- est_data$comp_data %>%
  dplyr::mutate(Month = dplyr::case_when(
    Fleet_name == "BTS" ~ 6, Fleet_name == "ATS" ~ 6, TRUE ~ Month))
# analytical survey q for BTS/ATS (matches ADMB; diff #7)
est_data$fleet_control$Catchability <- as.character(est_data$fleet_control$Catchability)
est_data$fleet_control$Catchability[est_data$fleet_control$Fleet_name %in% c("BTS", "ATS")] <- 3

bridging_model_2 <- Rceattle::fit_mod(
  data_list    = est_data,
  inits        = NULL,
  file         = NULL,
  estimateMode = 0,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = TRUE,
  initMode     = 0,   # free initial N-at-age (= ADMB log_avginit + log_initdevs)
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 0)   # M fixed at age schedule
)


# =============================================================================
# Model 3 - ESTIMATION with age/time-invariant M estimated (exploratory)
# -----------------------------------------------------------------------------
# M1_model = 1 estimates a single age/time-invariant M. ADMB fixes the age
# schedule (0.9/0.45/0.3); this explores how much the data move M.
# =============================================================================
bridging_model_3 <- Rceattle::fit_mod(
  data_list    = est_data,
  inits        = NULL,
  file         = NULL,
  estimateMode = 0,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = TRUE,
  initMode     = 0,
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 1)   # estimate scalar M
)


# =============================================================================
# Overlay the ADMB ("SAFE") reference output and plot
# -----------------------------------------------------------------------------
# Build a pseudo-Rceattle object holding the ADMB SSB / recruitment series (same
# trick the rockfish / plaice bridges use). Rceattle and ADMB share units here.
# =============================================================================
SAFE2024 <- bridging_model_1
SAFE2024$quantities$ssb[1, 1:nyr] <- adm_ssb$Est
SAFE2024$quantities$R[1, 1:nyr]   <- adm_r$Est

mods  <- list(bridging_model_1, bridging_model_2, bridging_model_3, SAFE2024)
names <- c("Rceattle fix parms (fwd pass)", "Rceattle est (M fixed)",
           "Rceattle est M", "ADMB (SAFE)")

plot_ssb(mods, model_names = names);         mtext(side = 2, "Female SSB",   line = 1.8)
plot_recruitment(mods, model_names = names); mtext(side = 2, "Recruitment",  line = 1.8)
plot_biomass(mods, model_names = names);     mtext(side = 2, "Total biomass", line = 1.8)
plot_selectivity(bridging_model_2)
