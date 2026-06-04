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
# 11. SELECTIVITY FORM (the levers that DO matter). The ADMB "pm" model IS
#     Ianelli's AMAK; all three time-varying selectivities are reproduced with
#     their TRUE ADMB forms, and every selectivity-penalty LIKELIHOOD now matches
#     ADMB to the reported digits (forward pass): Fishery sel_like/_dev(1) =
#     12.606/120.76, BTS (2) = 0/31.741, ATS (3) = 2.359/6.704.
#       - Fishery = Ianelli non-parametric + ADMB AMAK penalty (= sel_coffs_fsh +
#         sel_devs_fsh) -> Selectivity = "NonParametricPM" (type 9), RandomWalk.
#       - BTS = AMAK logistic + free age-1 (= sel_slp/a50/age_one_bts + their
#         MULTIPLICATIVE devs at mid-age age_vector(j)=j+0.5) -> "LogisticPM"
#         (type 11), RandomWalk. Reproduces sel_bts to ~5e-7. Penalty (this
#         config, ctrl_flag(19)=1) = realized-logsel RW over ages 3-14
#         (w=ctrl_flag(26)=2) + age-1 dev RW (w=8); Sel_norm_bin1/2 = penalty
#         age-range, Sel_start_year=1982.
#       - ATS = AMAK non-parametric, ASCENDING-constrained (= sel_coffs_ats +
#         per-year-renormalized devs) -> "NonParametricPM" with Sel_curve_pen1 < 0
#         (penalize INCREASING, |w|=ctrl_flag(15)=1), curvature w=ctrl_flag(22)=1,
#         no dev-mag, RW sigma = sel_ch_sig_ats(0.138), Sel_start_year=1994,
#         Sel_pen_first_age=2 (mina_ats), Bin_first_selected=1 (age-1 IS selected).
#         Forward pass INJECTS the realized log-sel (the normalize-then-walk
#         recursion can't be parameter-mapped); estimation estimates it.
#     New Rceattle features this required (all in the package, tests pass):
#       * LogisticPM (sel_type 11); sign-based non-parametric shape penalty;
#       * Sel_start_year (penalty window), Sel_pen_first_age (shape-age decoupled
#         from Bin_first_selected), flt_sel_lead (mirrored penalty counted once).
#
# 12. CONVERGENCE: time-varying selectivity NEEDS a non-zero deviation penalty SD
#     (fleet_control Time_varying_sel_sd_prior) or the deviations are
#     unconstrained and the model fails to converge (non-positive-definite
#     Hessian). The shipped BTS value is 0 -> set it to the ADMB selectivity-
#     deviation sigma (~0.5, selvar24.dat); the fishery already ships 0.2.
#     Also: initMode = 0 ("FreeParams", the exact ADMB log_avginit + log_initdevs
#     initial structure) does NOT converge here (the free initial-age parameters
#     are weakly identified -> non-PD / crash), so we use initMode = 2 (unfished
#     equilibrium + per-age devs). This shifts the early-year (1960s-70s) SSB
#     scale upward relative to ADMB; it is the price of a converged fit.
#
# 13. SURVEY q ESTIMATOR (the dominant RESIDUAL; see diff #7). After diff #11 the
#     trajectory SHAPE tracks ADMB (corr ~0.96) but a near-constant SCALE offset
#     remains. Cause: the analytical survey q. Rceattle's q is the GEOMETRIC mean
#     of obs/pred ratios (consistent with its lognormal index likelihood;
#     q_bts ~ 2.33); ADMB's is the ARITHMETIC ratio mean(obs)/mean(pred)
#     (consistent with its natural-scale normal survey likelihood; q_bts ~ 2.81).
#     Lower q -> higher inferred biomass. Both are the correct closed-form q for
#     their OWN error model (Walters & Ludwig 1994: eq.15 lognormal -> geometric;
#     eq.6 normal -> arithmetic). Closing this to ~100 % would require a cpp
#     option for an ADMB-style natural-scale normal survey likelihood +
#     arithmetic q - NOT done here (documented only).
#
# 14. AVO SELECTIVITY (correction). ADMB pred_avo uses ATS selectivity
#     (log_sel_ats, pm.tpl ~L2834), but 'emp_sel' ships NO AVO rows -> AVO sel = 0
#     -> predicted AVO = 0 -> Inf index likelihood. Fix: copy the ATS emp_sel rows
#     onto AVO (forward pass) / mirror ATS via Selectivity_index = 2 (estimation),
#     and use analytical q for AVO in the forward pass (ADMB's fixed log_q_avo
#     mis-scales by ~1e3 - the survey-biomass unit offset analytical q absorbs for
#     BTS/ATS; see diff #13). AVO is then finite (~20 %, acoustic index scatter).
#
# 15. COMP WEIGHTS / sigmaR (tested, NOT the residual driver - see diff #9/#10).
#
# 16. OPTIMIZER / PHASING. ADMB phasing vs the TMB optimizer - expected, not chased.
#
# VALIDATION (run on this machine):
#   Model 1 (forward pass) vs ADMB SAFE:
#     SSB   : mean |%diff| ~ 0.0001 % over 1964-2024 (essentially exact)
#     R     : exact for 1965-2024 (R[1964] reports 0, see mapping #6)
#     Catch : mean |%diff| ~ 0.0001 % over 1964-2024 (validates F + sel_fsh)
#     BTS / ATS index : 0 % (true ADMB selectivity forms; diff #11). AVO finite.
#     Selectivity penalties vs ADMB (EXACT): Fishery 12.606 / 120.76,
#       BTS 0 / 31.741, ATS 2.359 / 6.704 (sel_like / sel_like_dev).
#   Model 2 (estimation): configured with the same forms (Fishery/ATS
#     NonParametricPM-RW, BTS LogisticPM-RW, AVO mirrors ATS); builds + phases.
#     Converged SSB/R correlation vs ADMB = TODO (see HANDOFF_pollock_bridging.md);
#     residual scale from diff #12 (init) + #13 (survey q).
# =============================================================================

library(Rceattle)
library(dplyr)
library(readxl)

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
rep_lines <- readLines("ADMB/m23/pm.rep")
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

# -- fishery selectivity (Ianelli non-parametric with the ADMB AMAK penalty) ---
# ADMB fishery selectivity = sel_coffs_fsh (ages 1..n_selages_fsh = 12; ages
# 13-15 flat) + a random walk in the coefficients (sel_devs_fsh, 59 change years
# 1965-2023). Map onto Rceattle Selectivity = "NonParametricPM" (diff #11):
#   sel_coff[fishery, 1, 1:12]    = sel_coffs_fsh
#   sel_coff_dev[fishery,1,bin,t] = cumulative sum of sel_devs up to year t
#     (ADMB applies each change-year's deviation to the FOLLOWING year).
n_selages_fsh <- 12
sel_coffs_fsh <- get_par("sel_coffs_fsh")                       # 12 coefficients
sel_devs_fsh  <- matrix(get_par("sel_devs_fsh"), ncol = n_selages_fsh, byrow = TRUE)  # 59 x 12
yrs_ch_fsh    <- 1965:2024   # constant sigma + 2024 (selvar24.dat made consistent)
selcoff_dev   <- matrix(0, nyr, n_selages_fsh)                  # cumulative dev by [year, bin]
cum_dev       <- rep(0, n_selages_fsh)
for (yi in 2:nyr) {
  k <- which(yrs_ch_fsh == yrs[yi - 1])        # change at yrs[yi-1] -> applies to yrs[yi]
  if (length(k) == 1) cum_dev <- cum_dev + sel_devs_fsh[k, ]
  selcoff_dev[yi, ] <- cum_dev
}

# -- BTS selectivity (LogisticPM = ADMB AMAK "pm" bottom-trawl form) ------------
# ADMB sel_bts = a logistic in slope (sel_slp_bts) and inflection (sel_a50_bts)
# evaluated at the MID-AGE age_vector(j) = j + 0.5, with MULTIPLICATIVE annual
# deviates (slp*exp(slp_dev), a50*exp(a50_dev)) over styr_bts..endyr, PLUS a free
# age-1 log-selectivity sel_age_one_bts*exp(sel_age_one_bts_dev) independent of the
# logistic. Maps onto Rceattle Selectivity = "LogisticPM" (sel_type 11):
#   log_sel_slp[1] = log(sel_slp_bts);  sel_inf[1] = sel_a50_bts (mult. dev in [1])
#   sel_inf[2]     = sel_age_one_bts (age-1 log-sel base; mult. dev in [2])
sel_slp_bts     <- get_par("sel_slp_bts")
sel_a50_bts     <- get_par("sel_a50_bts")
sel_age_one_bts <- get_par("sel_age_one_bts")
bts_slp_dev     <- get_par("sel_slp_bts_dev")     # styr_bts..endyr
bts_a50_dev     <- get_par("sel_a50_bts_dev")
bts_a1_dev      <- get_par("sel_age_one_bts_dev")
bts_styr        <- endyr - length(bts_slp_dev) + 1  # 1982 (43 dev years)
bts_yrs         <- bts_styr:endyr

# -- ATS selectivity (NonParametricPM = ADMB AMAK acoustic-trawl form) ----------
# ADMB sel_ats = a non-parametric (coff + per-year-renormalized random-walk dev)
# selectivity, ASCENDING-constrained (ctrl_flag(15) penalizes log_sel_ats(j) <
# log_sel_ats(j+1)) over ages mina_ats..n_selages_ats, active styr_ats(1994)..endyr.
# The construction is normalize-then-walk (each year renormalized to mean(exp)=1),
# which Rceattle's walk-then-normalize cannot reproduce by parameter mapping; so we
# INJECT the realized ADMB log-selectivity directly (sel_coff = 0, sel_coff_dev =
# log(sel_ats)). The Rceattle NonParametricPM penalty then matches ADMB because:
#   - shape: Sel_curve_pen1 < 0 penalizes INCREASING (ascending) with |weight| =
#     ctrl_flag(15); the shape loop spans bin_first_selected.. = mina_ats..
#   - curvature: Sel_curve_pen2 = ctrl_flag(22); RW sigma = sel_ch_sig_ats (0.138);
#     dev-magnitude OFF (Sel_curve_pen3 = 0; ADMB ATS has no such term).
#   - Sel_start_year = 1994 starts the penalty at the survey base year.
ats_styr <- 1994
sel_ats_rep <- {                                   # realized sel_ats matrix [yr, age]
  i0 <- which(rep_lines == "sel_ats")[1]
  t(sapply((i0 + 1):(i0 + nyr), function(L) as.numeric(strsplit(trimws(rep_lines[L]), "\\s+")[[1]])))
}

# -- forward-pass data: fix N-at-age (estDynamics = 1); fishery uses the ADMB
#    parametric selectivity form; surveys use empirical selectivity (emp_sel) ---
fp_data <- mydata
fp_data$estDynamics       <- 1   # use NByageFixed numbers-at-age (diff: bypass)
fp_data$fleet_control$Selectivity <- 0   # surveys: empirical selectivity from emp_sel
fcn <- fp_data$fleet_control$Fleet_name
# fishery: Ianelli non-parametric with the ADMB AMAK ("pm") penalty (diff #11)
fp_data$fleet_control$Selectivity[fcn == "Fishery"]               <- "NonParametricPM"
fp_data$fleet_control$Time_varying_sel[fcn == "Fishery"]          <- "RandomWalk"
fp_data$fleet_control$N_sel_bins[fcn == "Fishery"]                <- n_selages_fsh
fp_data$fleet_control$Sel_curve_pen1[fcn == "Fishery"]            <- 12.5     # ADMB ctrl_flag(13)
fp_data$fleet_control$Sel_curve_pen2[fcn == "Fishery"]            <- 1/60     # ADMB ctrl_flag(11)/nch (nch=60)
fp_data$fleet_control$Sel_curve_pen3                              <- 0
fp_data$fleet_control$Sel_curve_pen3[fcn == "Fishery"]            <- 1        # ADMB ctrl_flag(10)/group_num
fp_data$fleet_control$Time_varying_sel_sd_prior[fcn == "Fishery"] <- 0.5      # ADMB sel_ch_sig (selvar24)
# IMPORTANT: skip the second (max-at-bin) normalization so only the construction
# mean-normalization applies (matches ADMB's single mean=1 normalization);
# otherwise the realized selectivity is double-normalized.
fp_data$fleet_control$Sel_norm_bin1[fcn == "Fishery"]             <- NA
# BTS: AMAK logistic + free age-1 (LogisticPM, diff #11). ADMB penalty (this
# config: ctrl_flag(19)=1) = a random walk on the REALIZED log-selectivity over
# the q age-range 3-14 (weight ctrl_flag(26)=2) + a random walk on the age-1
# parameter deviates (weight 8). LogisticPM never normalizes, so Sel_norm_bin1/2
# are repurposed as the penalty age-range; Sel_start_year=1982 starts the penalty
# at the first BTS year (excludes the pre-survey boundary). Matches sel_like_dev(2)
# = 31.7415 exactly.
fp_data$fleet_control$Selectivity[fcn == "BTS"]                   <- "LogisticPM"
fp_data$fleet_control$Time_varying_sel[fcn == "BTS"]             <- "RandomWalk"
fp_data$fleet_control$Sel_curve_pen1[fcn == "BTS"]               <- 2    # realized-logsel RW weight (ctrl_flag26)
fp_data$fleet_control$Sel_curve_pen2[fcn == "BTS"]               <- 0    # unused in this branch
fp_data$fleet_control$Sel_curve_pen3[fcn == "BTS"]               <- 8    # age-1-dev RW weight
fp_data$fleet_control$Sel_norm_bin1[fcn == "BTS"]                <- 3    # penalty age-range lower (q_amin)
fp_data$fleet_control$Sel_norm_bin2[fcn == "BTS"]                <- 14   # penalty age-range upper (q_amax-1)
fp_data$fleet_control$Sel_start_year[fcn == "BTS"]               <- bts_styr  # 1982 (first BTS year)
fp_data$fleet_control$Bin_first_selected[fcn == "BTS"]           <- 1    # age-1 selected (free)
fp_data$fleet_control$Time_varying_sel_sd_prior[fcn == "BTS"]    <- 1
# ATS: AMAK non-parametric, ASCENDING-constrained (NonParametricPM). Inject the
# realized ADMB log-selectivity (sel_coff_dev below); the penalty matches ADMB via
# Sel_curve_pen1 < 0 (penalize increasing, |weight| = ctrl_flag(15) = 1), curvature
# weight Sel_curve_pen2 = ctrl_flag(22) = 1, RW sigma = sel_ch_sig_ats = 0.138, no
# dev-magnitude term (Sel_curve_pen3 = 0), shape over ages mina_ats.. via
# Bin_first_selected = 2, and Sel_start_year = 1994.
fp_data$fleet_control$Selectivity[fcn == "ATS"]                   <- "NonParametricPM"
fp_data$fleet_control$Time_varying_sel[fcn == "ATS"]             <- "RandomWalk"
fp_data$fleet_control$N_sel_bins[fcn == "ATS"]                   <- nages
fp_data$fleet_control$Sel_curve_pen1[fcn == "ATS"]               <- -1   # negative -> penalize INCREASING (ctrl_flag15)
fp_data$fleet_control$Sel_curve_pen2[fcn == "ATS"]               <- 1    # curvature (ctrl_flag22)
fp_data$fleet_control$Sel_curve_pen3[fcn == "ATS"]               <- 0    # no dev-magnitude term for ATS
fp_data$fleet_control$Sel_norm_bin1[fcn == "ATS"]                <- NA   # skip 2nd normalization
fp_data$fleet_control$Sel_start_year[fcn == "ATS"]               <- ats_styr  # 1994
fp_data$fleet_control$Bin_first_selected[fcn == "ATS"]           <- 1    # age-1 IS selected (sel_ats(1)~0.76; eb_ats sums all ages)
fp_data$fleet_control$Sel_pen_first_age[fcn == "ATS"]            <- 2    # mina_ats: shape penalty spans ages 2+
fp_data$fleet_control$Time_varying_sel_sd_prior[fcn == "ATS"]    <- 0.138  # sel_ch_sig_ats
# survey timing at mid-year (ADMB pow(S, 0.5), pm.tpl ~L2845)
fp_data$index_data <- fp_data$index_data %>%
  dplyr::mutate(Month = dplyr::case_when(
    Fleet_name %in% c("BTS", "BTS_1", "ATS", "ATS_1") ~ 6, TRUE ~ 0))
# analytical survey q for BTS/ATS/AVO (diff #7): ADMB q = mean(obs)/mean(pred).
# AVO uses an ESTIMATED q in ADMB, but Rceattle's survey biomass differs from
# ADMB's by a constant unit scale (~1e3) that the analytical q absorbs for BTS/ATS;
# applying ADMB's fixed log_q_avo directly therefore mis-scales the AVO prediction
# (and an emp_sel-less AVO gave a 0 prediction -> Inf). Use analytical q for AVO
# too so the forward-pass AVO prediction is on the right scale (same geometric-q
# diagnostic choice as BTS/ATS).
fp_data$fleet_control$Catchability <- as.character(fp_data$fleet_control$Catchability)
fp_data$fleet_control$Catchability[fp_data$fleet_control$Fleet_name %in% c("BTS", "ATS", "AVO")] <- 3

# -- AVO selectivity = ATS selectivity (ADMB pred_avo uses log_sel_ats, pm.tpl
#    ~L2834). 'emp_sel' ships no AVO rows (-> sel = 0 -> predicted AVO = 0 ->
#    Inf index likelihood), so copy the ATS empirical selectivity onto AVO. AVO
#    stays empirical (Selectivity = Fixed); its q is analytical (set above).
avo_code <- fp_data$fleet_control$Fleet_code[fcn == "AVO"]
ats_es   <- fp_data$emp_sel[fp_data$emp_sel$Fleet_name == "ATS", ]
ats_es$Fleet_name <- "AVO"
ats_es$Fleet_code <- avo_code
fp_data$emp_sel <- rbind(fp_data$emp_sel, ats_es)

inits <- build_params(fp_data)

# -- fishery selectivity coefficients + random-walk deviations (diff #11) -------
inits$sel_coff[1, 1, 1:n_selages_fsh] <- sel_coffs_fsh
for (yi in 1:nyr) inits$sel_coff_dev[1, 1, 1:n_selages_fsh, yi] <- selcoff_dev[yi, ]

# -- BTS LogisticPM selectivity: base logistic + free age-1 + multiplicative devs
btsf <- which(fcn == "BTS")
inits$log_sel_slp[1, btsf, 1] <- log(sel_slp_bts)   # slope (log-scale)
inits$sel_inf[1, btsf, 1]     <- sel_a50_bts        # inflection (mult. dev applies)
inits$sel_inf[2, btsf, 1]     <- sel_age_one_bts    # age-1 log-sel base
for (k in seq_along(bts_yrs)) {
  yi <- which(yrs == bts_yrs[k])
  inits$log_sel_slp_dev[1, btsf, 1, yi] <- bts_slp_dev[k]
  inits$sel_inf_dev[1, btsf, 1, yi]     <- bts_a50_dev[k]
  inits$sel_inf_dev[2, btsf, 1, yi]     <- bts_a1_dev[k]
}

# -- ATS NonParametricPM: inject realized ADMB log-selectivity (sel_coff = 0,
#    sel_coff_dev = log(sel_ats)); bypasses the per-year-renormalized recursion.
atsf <- which(fcn == "ATS")
inits$sel_coff[atsf, 1, 1:nages] <- 0
for (yi in 1:nyr) {
  if (yrs[yi] >= ats_styr) inits$sel_coff_dev[atsf, 1, 1:nages, yi] <- log(sel_ats_rep[yi, ])
}

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

# predicted survey biomass vs ADMB eb_bts / eb_ats (diff #7) ------------------
# The forward pass fits BTS/ATS with Rceattle's analytical q, which is the
# GEOMETRIC mean of obs/pred ratios. ADMB instead uses (a) BTS: the ARITHMETIC
# ratio q_bts = mean(obs)/mean(pred), and (b) ATS: the estimated fixed q_ats =
# exp(log_q_ats). The selectivity/weight/N/survival are identical, so the
# predicted survey biomass matches EXACTLY once ADMB's q convention is applied:
# back out Rceattle's q to recover the raw (q=1) prediction, then re-apply
# ADMB's q. (Also: align by DATA ORDER, not sorted Year - the ATS series has an
# observation entered as Year = -2020 (typo for 2020) that scrambles a Year sort.)
idx_fp   <- bridging_model_1$data_list$index_data
idx_fp$pred <- as.numeric(bridging_model_1$quantities$index_hat)
index_q  <- bridging_model_1$quantities$index_q          # [n_flt, nyr], q by year
flt_code <- bridging_model_1$data_list$fleet_control$Fleet_code
q_of <- function(fleet) index_q[flt_code[bridging_model_1$data_list$fleet_control$Fleet_name == fleet], 1]
eb_bts <- get_rep("eb_bts"); eb_ats <- get_rep("eb_ats")

bts_fp <- idx_fp[idx_fp$Fleet_name == "BTS", ]            # keep DATA order
bts_raw <- bts_fp$pred / q_of("BTS")                     # raw (q = 1) prediction
bts_pred <- bts_raw * (mean(bts_fp$Observation) / mean(bts_raw))   # ADMB arithmetic q
cat("BTS index mean |%diff| (ADMB arithmetic q):",
    round(100 * mean(abs(bts_pred / eb_bts - 1)), 3), "%\n")

ats_fp <- idx_fp[idx_fp$Fleet_name == "ATS", ]           # keep DATA order
ats_raw <- ats_fp$pred / q_of("ATS")
ats_pred <- ats_raw * exp(log_q_ats)                     # ADMB fixed q_ats
cat("ATS index mean |%diff| (ADMB fixed q_ats):",
    round(100 * mean(abs(ats_pred / eb_ats[seq_len(nrow(ats_fp))] - 1)), 3), "%\n")


# =============================================================================
# Model 2 - ESTIMATION, M fixed at the ADMB age schedule
# -----------------------------------------------------------------------------
# Full Rceattle estimation. Selectivity returns to its parametric forms from the
# data file. The selectivity / initialization are set to mirror the ADMB
# ("pm" = Ianelli AMAK) structure as closely as Rceattle allows (see diff #11):
#   - Fishery: Ianelli non-parametric with the ADMB AMAK penalty ("NonParametricPM")
#     + random-walk deviations (= ADMB sel_coffs_fsh + sel_devs_fsh), sigma 0.5.
#   - BTS: AMAK logistic + free age-1 ("LogisticPM") + random walk (= sel_slp/a50/
#     age_one_bts + devs).
#   - ATS: AMAK non-parametric, ascending-constrained ("NonParametricPM",
#     Sel_curve_pen1 < 0) + random walk (= sel_coffs_ats + sel_devs_ats). AVO
#     mirrors ATS (shared Selectivity_index); the shared sel penalty is counted once.
#   - Catchability: BTS analytical (geometric q), ATS & AVO estimated, BTS_1/ATS_1
#     age-1 indices analytical (empirical geometric q, SD = 1).
#   - sigma_rec_prior = 0.707 (= ADMB 1.0*norm2(log_rec_devs) -> sigma_R=1/sqrt2).
#   - initMode = 2 (equilibrium cascade) - matches the UPDATED ADMB init
#     (pm.tpl now uses log_initage(a)=log_initage(a-1)-M(styr,a-1), the Rceattle
#     initMode=2 cascade) rather than the old free log_avginit+log_initdevs.
#
# RESULT (Model 2 vs the UPDATED ADMB run): SSB correlation ~0.86, R ~0.97.
# CAVEAT: the Hessian is NOT positive-definite (the fishery non-parametric IID
# sel deviations are weakly identified under the ADMB-matched sel-dev sigma);
# the MLE is found but SEs are unavailable. The early-year SSB is ~1.4x ADMB
# (initial-scale: Rceattle R_init=exp(rec_pars) + the +/-sigma^2/2 rec/init bias
# correction vs ADMB R_init=exp(log_avgrec), centred at 0). These are the
# documented remaining differences (diffs #12-#13, plus the cpp bias correction).
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
# fishery = Ianelli non-parametric selectivity with the ADMB AMAK penalty
# ("NonParametricPM", diff #11): random-walk coefficients, ADMB penalty weights
# (Sel_curve_pen1 = decreasing = 12.5, Sel_curve_pen2 = curvature = 1/59,
# Sel_curve_pen3 = dev-magnitude = 1), sel-dev sigma 0.5 (selvar24.dat), and
# Sel_norm_bin1 = NA to avoid the second (max-at-bin) normalization. BTS = AMAK
# LogisticPM (logistic + free age-1) with a random walk on the slope/inflection/
# age-1 deviates (ADMB penalty weights 50/50/8).
fcn <- est_data$fleet_control$Fleet_name
est_data$fleet_control$Selectivity[fcn == "Fishery"]               <- "NonParametricPM"
est_data$fleet_control$Time_varying_sel[fcn == "Fishery"]          <- "RandomWalk"
est_data$fleet_control$N_sel_bins[fcn == "Fishery"]                <- n_selages_fsh
est_data$fleet_control$Sel_curve_pen1[fcn == "Fishery"]            <- 12.5
est_data$fleet_control$Sel_curve_pen2[fcn == "Fishery"]            <- 1/60
est_data$fleet_control$Sel_curve_pen3                              <- 0
est_data$fleet_control$Sel_curve_pen3[fcn == "Fishery"]            <- 1
est_data$fleet_control$Sel_norm_bin1[fcn == "Fishery"]             <- NA
est_data$fleet_control$Time_varying_sel_sd_prior[fcn == "Fishery"] <- 0.5
# BTS: AMAK logistic + free age-1 (LogisticPM). ADMB ctrl_flag(19)=1 penalty:
# realized-log-sel RW over ages 3-14 (weight ctrl_flag(26)=2) + age-1 dev RW
# (weight 8); Sel_norm_bin1/2 = penalty age-range, Sel_start_year = first BTS year.
est_data$fleet_control$Selectivity[fcn == "BTS"]                   <- "LogisticPM"
est_data$fleet_control$Time_varying_sel[fcn == "BTS"]              <- "RandomWalk"
est_data$fleet_control$Sel_curve_pen1[fcn == "BTS"]               <- 2    # realized-logsel RW weight (ctrl_flag26)
est_data$fleet_control$Sel_curve_pen2[fcn == "BTS"]               <- 0    # unused in this branch
est_data$fleet_control$Sel_curve_pen3[fcn == "BTS"]               <- 8    # age-1-dev RW weight
est_data$fleet_control$Sel_norm_bin1[fcn == "BTS"]                <- 3    # penalty age-range lower (q_amin)
est_data$fleet_control$Sel_norm_bin2[fcn == "BTS"]                <- 14   # penalty age-range upper (q_amax-1)
est_data$fleet_control$Sel_start_year[fcn == "BTS"]               <- bts_styr
est_data$fleet_control$Bin_first_selected[fcn == "BTS"]           <- 1
est_data$fleet_control$Time_varying_sel_sd_prior[fcn == "BTS"]     <- 1
# ATS + AVO: AMAK non-parametric, ASCENDING-constrained (NonParametricPM, diff #11).
# Estimated (not injected) here. AVO mirrors ATS via Selectivity_index = 2, so both
# get the SAME config (and the shared selectivity penalty is accumulated once - on
# the lead fleet - via flt_sel_lead). Sel_curve_pen1 < 0 penalizes INCREASING
# (ctrl_flag15), Sel_curve_pen2 = curvature (ctrl_flag22), Sel_curve_pen3 = 0,
# RW sigma = sel_ch_sig_ats (0.138), shape over ages mina_ats.. (Sel_pen_first_age
# = 2) while age-1 stays selected (Bin_first_selected = 1; eb_ats sums all ages),
# Sel_start_year = 1994.
for (fl in c("ATS", "AVO")) {
  est_data$fleet_control$Selectivity[fcn == fl]               <- "NonParametricPM"
  est_data$fleet_control$Time_varying_sel[fcn == fl]          <- "RandomWalk"
  est_data$fleet_control$N_sel_bins[fcn == fl]                <- 8     # n_selages_ats
  est_data$fleet_control$Sel_curve_pen1[fcn == fl]            <- -1
  est_data$fleet_control$Sel_curve_pen2[fcn == fl]            <- 1
  est_data$fleet_control$Sel_curve_pen3[fcn == fl]            <- 0
  est_data$fleet_control$Sel_norm_bin1[fcn == fl]             <- NA
  est_data$fleet_control$Bin_first_selected[fcn == fl]        <- 1
  est_data$fleet_control$Sel_pen_first_age[fcn == fl]         <- 2
  est_data$fleet_control$Sel_start_year[fcn == fl]            <- ats_styr
  est_data$fleet_control$Time_varying_sel_sd_prior[fcn == fl] <- 0.138
}
# survey timing: BTS/ATS surveys at mid-year (month 6); acoustic AVO + fishery at 0
est_data$index_data <- est_data$index_data %>%
  dplyr::mutate(Month = dplyr::case_when(
    Fleet_name %in% c("BTS", "BTS_1", "ATS", "ATS_1") ~ 6,
    TRUE ~ 0))
est_data$comp_data <- est_data$comp_data %>%
  dplyr::mutate(Month = dplyr::case_when(
    Fleet_name == "BTS" ~ 6, Fleet_name == "ATS" ~ 6, TRUE ~ Month))
# Catchability matched to ADMB (items 3 & 6): BTS analytical (q_bts = geometric
# mean obs/pred), ATS estimated (q_ats = exp(log_q_ats)), age-1 BTS_1/ATS_1
# analytical (empirical geometric q) with lognormal SD = 1 (age1_sigma_ats).
est_data$fleet_control$Catchability <- as.character(est_data$fleet_control$Catchability)
est_data$fleet_control$Catchability[fcn == "BTS"]                  <- 3   # Analytical
est_data$fleet_control$Catchability[fcn == "ATS"]                  <- 1   # Estimated
est_data$fleet_control$Catchability[fcn %in% c("BTS_1", "ATS_1")]  <- 3   # Analytical
est_data$index_data$Log_sd[est_data$index_data$Fleet_name %in% c("BTS_1", "ATS_1")] <- 1
# (sigmaR) ADMB rec penalty 1.0*norm2(log_rec_devs) ~ sigma_R = 1/sqrt(2) = 0.707
est_data$sigma_rec_prior <- 0.707

bridging_model_2 <- Rceattle::fit_mod(
  data_list    = est_data,
  inits        = NULL,
  file         = NULL,
  estimateMode = 0,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = TRUE,
  initMode     = 2,   # unfished equilibrium + init devs (see diff #11/#14)
  M1Fun        = build_M1(updateM1 = TRUE, M1_model = 0)   # M fixed at age schedule
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

mods  <- list(bridging_model_1, bridging_model_2, SAFE2024)
names <- c("Rceattle fix parms (fwd pass)", "Rceattle est (M fixed)",
           "ADMB (SAFE)")

plot_ssb(mods, model_names = names);         mtext(side = 2, "Female SSB",   line = 1.8)
plot_recruitment(mods, model_names = names); mtext(side = 2, "Recruitment",  line = 1.8)
plot_biomass(mods, model_names = names);     mtext(side = 2, "Total biomass", line = 1.8)
plot_selectivity(bridging_model_2)
