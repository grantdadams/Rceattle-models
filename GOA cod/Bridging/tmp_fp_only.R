# =============================================================================
# Forward-pass validation: ss3_to_rceattle converter at minage = 0
#
# Goal: take the data list produced by ss3_to_rceattle(), inject SS3's
# parameter values, and verify Rceattle reproduces SS3 R/Bio/SSB to ~1e-3.
# This mirrors the empirical bridge in 2024_synthesis_to_pcod.R (which uses
# the Excel-derived data list at minage = 1), but exercises the converter
# at minage = 0. Estimation comes after validation; if forward-pass matches,
# the bridge is sound.
# =============================================================================

library(Rceattle); library(r4ss); library(dplyr); library(tidyr)
setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA cod")
source("R/ss3_to_rceattle.R")

# Null-coalescing operator (defined here in case sourcing order matters)
`%||%` <- function(x, y) if (!is.null(x) && !(length(x) == 1 && is.na(x))) x else y

# ---- r4ss::SS_output workaround (2026-05-31) -------------------------------
# When SS3 finishes with a "variance may be suspect" warning (e.g. one or
# more parameters at a bound, final_grad > target), the SD column in
# Report.sso's DERIVED_QUANTITIES table can land as character rather than
# numeric. r4ss then errors at `sqrt(log((SSB_final_SD/SSB_final_EST)^2 + 1))`
# in the Pstar / OFL sigma calculation, blocking all downstream parsing
# (the FP test needs ss3_rep$timeseries, $natage, $condbase, etc., which
# are populated BEFORE this calc but never returned). Patch: blank the
# Pstar/OFL sigma block and set both to NA_real_. We don't use them.
local({
  src <- as.character(deparse(body(r4ss::SS_output)))
  pstar_line <- grep('Pstar_sigma.*sqrt', src)[1]
  ofl_line   <- grep('OFL_sigma.*sqrt',   src)[1]
  if (!is.na(pstar_line) && !is.na(ofl_line)) {
    # Pstar / OFL each span ~10 lines (if(...) { 4 lines incl sqrt(...) }
    # else { NULL }). Blank both blocks then re-assign their slots to NA.
    for (i in (pstar_line - 4):(pstar_line + 5)) src[i] <- "    "
    src[pstar_line - 4] <- "    returndat[[\"Pstar_sigma\"]] <- NA_real_"
    ofl_line2 <- grep('OFL_sigma.*sqrt', src)[1]
    if (!is.na(ofl_line2)) {
      for (i in (ofl_line2 - 4):(ofl_line2 + 5)) src[i] <- "    "
      src[ofl_line2 - 4] <- "    returndat[[\"OFL_sigma\"]] <- NA_real_"
    }
    new_body <- parse(text = paste(src, collapse = "\n"))[[1]]
    fn <- r4ss::SS_output; body(fn) <- new_body
    assignInNamespace("SS_output", fn, ns = "r4ss")
  }
})
# The patched fn lives in the r4ss namespace, but the search-path copy users
# get via library(r4ss) is still the original unpatched export. Detach +
# re-attach so calls to `SS_output(...)` in this script resolve to the
# patched function.
tryCatch(detach("package:r4ss"), error = function(e) NULL)
suppressMessages(library(r4ss))

# F_Method = 2 in SS3 emits ageselex / sizeselex tables with extra unnamed
# columns ("NoName" duplicates) for the per-fishery F parameters. dplyr can't
# filter a data frame with duplicate column names, so trim them off after
# loading. Apply lazily inside a helper used at every call site below.
.dedupe_cols <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  keep <- !duplicated(colnames(df)) & !grepl("^NoName$", colnames(df))
  # Always keep the original Factor / Fleet / Yr / Seas / Sex / Morph / Label
  # columns even if r4ss emits them under different names; the duplicated
  # check above already preserves them.
  df[, keep, drop = FALSE]
}


# =============================================================================
# 1. Read SS3 outputs and build the converter data list
# =============================================================================
SS3_DIR <- "Data/goa_pcod-no init and ramp"
PAR_FILE <- file.path(SS3_DIR, "ss3.par")
DAT_FILE <- file.path(SS3_DIR, "GOAPcod2024Oct17_1e_5cm.dat")
CTL_FILE <- file.path(SS3_DIR, "Model19_1e.ctl")
# Was: TS_FILE <- "Data/2024pcod_time_series.csv". That CSV was generated
# from the unmodified SS3 run and its per-year F values disagree with the
# modified model's MLE by ~3%. Read F values straight from ss3_rep instead.

parlist <- SS_readpar_3.30(PAR_FILE, datsource = DAT_FILE, ctlsource = CTL_FILE,
                           verbose = FALSE)
datlist <- SS_readdat(DAT_FILE, verbose = FALSE)
ctllist <- SS_readctl(CTL_FILE, use_datlist = TRUE, datlist = datlist,
                      verbose = FALSE)
ss3_rep <- SS_output(SS3_DIR, verbose = FALSE, printstats = FALSE,
                     covar = FALSE, forecast = FALSE)
# F_Method = 2 in SS3 emits duplicate "NoName" columns on the selex tables.
# Strip them so dplyr filter() works downstream. See helper at top of file.
ss3_rep$ageselex   <- .dedupe_cols(ss3_rep$ageselex)
ss3_rep$sizeselex  <- .dedupe_cols(ss3_rep$sizeselex)
# Build a CSV-shaped F time-series straight from ss3_rep$timeseries so we
# don't rely on a stale CSV (init_log_F_from_ss3 expects Yr + F.._N cols).
ts_ss3 <- ss3_rep$timeseries[ss3_rep$timeseries$Era %in% c("INIT","TIME"), ]
f_cols_src <- grep("^F:_[0-9]+$", colnames(ts_ss3), value = TRUE)
for (fc in f_cols_src) {
  new_name <- sub("^F:_", "F.._", fc)
  ts_ss3[[new_name]] <- ts_ss3[[fc]]
}

cod_pcod <- ss3_to_rceattle(
  ss3_dir       = SS3_DIR,
  par_file      = "ss3.par",
  dat_file      = "GOAPcod2024Oct17_1e_5cm.dat",
  ctl_file      = "Model19_1e.ctl",
  spnames       = "Pcod",
  minage        = 0,
  projyr_offset = 5,
  verbose       = FALSE
)

years_hind <- cod_pcod$styr:cod_pcod$endyr
nages_pcod <- cod_pcod$nages[1]
minage_pcod <- cod_pcod$minage[1]   # 0
n_flt <- nrow(cod_pcod$fleet_control)
stopifnot(minage_pcod == 0L, nages_pcod == 11L)
stopifnot(cod_pcod$spawn_month == 0)   # Converter should derive this from datlist$spawn_seas

# Map fleet metadata from fleet_control to the format the SS3-injection helpers expect
fleet_meta <- data.frame(
  name       = cod_pcod$fleet_control$Fleet_name,
  ss3_num    = cod_pcod$fleet_control$Fleet_code,
  fleet_type = cod_pcod$fleet_control$Fleet_type,
  stringsAsFactors = FALSE
)
fleet_meta$has_blocks <- TRUE   # safe default; sel injection will detect actual blocks
cat("Fleets detected:\n"); print(fleet_meta)


# =============================================================================
# 2. Jensen's-gap closure: SSB_WAA := Mat_F_wtatage, maturity := 1/sex_ratio
#    (Same fix as Section 4e in 2024_synthesis_to_pcod.R, but adapted for the
#    converter's minage = 0 layout.)
# =============================================================================
ss3_mfw <- ss3_rep$endgrowth %>%
  dplyr::filter(Sex == 1) %>% dplyr::arrange(int_Age) %>%
  dplyr::select(int_Age, Mat_F_wtatage)
mfw_at <- function(age) {
  v <- ss3_mfw %>% dplyr::filter(int_Age == age) %>% dplyr::pull(Mat_F_wtatage)
  if (length(v) == 0) NA_real_ else v[1]
}

# At minage = 0, Rceattle slot k = SS3 int_Age (k - 1). Slot 1 = age 0,
# slot nages = age (nages - 1) = SS3 plus group (already accumulated by SS3).
# So no plus-group N-weighting is needed at minage = 0 — the direct 1-to-1
# mapping gives the right per-slot Mat_F_wtatage.
mfw_vec <- numeric(nages_pcod)
for (k in seq_len(nages_pcod)) mfw_vec[k] <- mfw_at(k - 1) %||% 0

age_cols_w <- paste0("Age", seq_len(nages_pcod))

# Jensen's-gap fix is INCOMPATIBLE with parametric growth: the C++ overwrites
# weight_hat from VB in the SSB slot, so injecting Mat_F_wtatage there has no
# effect. Skip the fix here; SSB will carry the ~10% Jensen gap until a
# parametric-path equivalent exists. Maturity stays as Len_Mat (the converter's
# default); mature_females = Len_Mat * sex_ratio in C++.
cat("\nJensen fix SKIPPED (incompatible with VB growth)\n")
print(data.frame(Slot = seq_len(nages_pcod), SS3_age = 0:(nages_pcod - 1),
                 Mat_F_wtatage = mfw_vec))


# =============================================================================
# 3. M1 linkage: post-2014 block indicator + SS3 prior on NatM
# =============================================================================
m_block_yrs <- ctllist$Block_Design[[4]]
cat(sprintf("\nM block 4 from ctl spans years %d-%d\n",
            m_block_yrs[1], m_block_yrs[2]))
cod_pcod$env_data$post2014 <-
  as.integer(cod_pcod$env_data$Year >= m_block_yrs[1] &
               cod_pcod$env_data$Year <= m_block_yrs[2])


# =============================================================================
# 3a. LLSrv environmental q linkage (SS3: LnQ_base_LLSrv(5)_ENV_add)
#     SS3 ctl has env_var&link = 101 for LLSrv: env_var index 1 (CFSR),
#     additive link, ENV_add = 0.9147. Rceattle equivalent:
#       Catchability = "Environmental" (code 5)
#       index_q_beta[LLSrv, CFSR_col] = 0.9147
#     The C++ then computes index_q[LLSrv, yr] =
#       exp(index_log_q[LLSrv] + sum_k(index_q_beta[LLSrv, k] * env_index[yr, k]))
#     With only the CFSR column non-zero, this matches SS3's
#       log(q[yr]) = LnQ_base + ENV_add * env_var1[yr]
# =============================================================================
llsrv_idx <- which(cod_pcod$fleet_control$Fleet_name == "LLSrv")
stopifnot(length(llsrv_idx) == 1)
# SS3 env-q decoding (env_var&link = 101 = "env_var index 1, exponential link"):
# From SS_timevaryparm.tpl case 1, SS3 multiplies the LnQ parameter (not
# log(q)) by exp(env_add * env_var), then exponentiates:
#   LnQ_tv[yr] = LnQ_base * exp(env_add * env_var[yr])
#   q[yr]      = exp(LnQ_tv[yr])
# Rceattle's `Catchability = "Environmental"` formula is additive on log(q):
#   q[yr] = exp(LnQ + sum_k env_var[yr, k] * beta[k])
# These two are NOT equivalent for non-zero env_var (the SS3 form is
# transcendental in env_var; Rceattle's is linear). To get machine-precision
# parity WITHOUT modifying the C++, we run LLSrv as `Estimated` with
# `Time_varying_q = "IID"` and inject per-year q-deviates:
#   index_q_dev[LLSrv, yr] = log(SS3_Calc_Q[yr]) - index_log_q[LLSrv]
# Rceattle's formula `exp(LnQ + dev)` then reproduces SS3 Calc_Q exactly.
# Set up the fleet_control + q_dev injection here; the actual q_dev values
# come from ss3_rep$cpue inside the SS3-injection helper below.
cod_pcod$fleet_control$Catchability[llsrv_idx]   <- "Estimated"
cod_pcod$fleet_control$Time_varying_q[llsrv_idx] <- "IID"

# Extract SS3 prior on NatM (PR_type, PRIOR, PR_SD from ctllist).
# SS3 PR_type 3 = lognormal: log(M) ~ N(PRIOR - 0.5*PR_SD^2, PR_SD); PRIOR is
# on the log scale and represents the bias-corrected median's log.
# Rceattle parameterizes: log_M1 ~ N(log(M_prior) + sd^2/2, sd); so
# M_prior = exp(PRIOR - sd^2/2) makes the two equivalent.
extract_ss3_prior <- function(ctllist_section, param_pattern) {
  if (is.null(ctllist_section)) return(NULL)
  idx <- grep(param_pattern, rownames(ctllist_section))
  if (length(idx) == 0) return(NULL)
  row <- ctllist_section[idx[1], ]
  if (!"PR_type" %in% names(row) || row$PR_type == 0 || row$PHASE < 0) return(NULL)
  list(PR_type = row$PR_type, PRIOR = row$PRIOR, PR_SD = row$PR_SD,
       PHASE = row$PHASE, name = rownames(ctllist_section)[idx[1]])
}

m_prior_ss3 <- extract_ss3_prior(ctllist$MG_parms, "NatM_p_1_Fem_GP_1$")
if (!is.null(m_prior_ss3) && m_prior_ss3$PR_type == 3) {
  # SS3 lognormal: log(M) ~ N(PRIOR, PR_SD). Want Rceattle log_M1 to have the
  # same prior, so M_prior_rce = exp(PRIOR - PR_SD^2 / 2).
  M_prior_rce <- exp(m_prior_ss3$PRIOR - 0.5 * m_prior_ss3$PR_SD^2)
  M_prior_sd  <- m_prior_ss3$PR_SD
  cat(sprintf("SS3 M prior: PR_type=%d (lognormal), PRIOR=%.3f, PR_SD=%.3f\n",
              m_prior_ss3$PR_type, m_prior_ss3$PRIOR, m_prior_ss3$PR_SD))
  cat(sprintf("Rceattle M_prior (natural scale) = %.4f, M_prior_sd = %.4f\n",
              M_prior_rce, M_prior_sd))
  use_m_prior <- TRUE
} else {
  cat("No active SS3 prior on NatM -- skipping\n")
  M_prior_rce <- 0.4
  M_prior_sd  <- 0.35
  use_m_prior <- FALSE
}

# M block effect: SS3 has a separate prior on the block-replacement value
# `NatM_BLK4repl_2014` (PR_type=3 lognormal, PRIOR=-0.81, PR_SD=0.41) that
# contributes ~1.10 to SS3 Parm_priors. SS3-estimated block value 0.817
# implies a post-2014 log-offset of log(0.817/0.493) ≈ 0.506.
# Wire that as a prior on the linkage coefficient: Normal(0, M_prior_sd)
# centers the offset at 0 (= no change from base) with the same SD as the
# M-base prior, so departures from base are penalized like SS3's per-block
# prior. Init at the SS3 ESTIM value to start in the right place.
m_block_init <- log(0.817 / M_prior_rce)
M1_block <- build_M1(
  M1_model     = 1,
  M1_use_prior = use_m_prior,
  M2_use_prior = FALSE,
  M_prior      = M_prior_rce,
  M_prior_sd   = M_prior_sd,
  linkages     = list(M1 = linkage_spec(
    formula = ~ post2014 - 1,
    by      = ~ species,
    init    = list(post2014 = m_block_init),
    # SS3 puts its prior on the absolute log(M_block) value at the SAME
    # center as log(M_base) (both N(-0.81, 0.41) independently). The
    # implied prior on the delta is therefore centered at 0 (with sd
    # 0.41*sqrt(2)=0.58 in the joint-independence sense, but here we use
    # the same 0.41 since the M base prior is also being applied).
    priors  = list(post2014 = normal(0, M_prior_sd))
  ))
)


# =============================================================================
# 3b. Switch active fleets to parametric Length-DoubleNormal selectivity
#
# SS3 uses pattern 24 (DoubleNormal): 6 params (peak, top, ascend, descend,
# init_logit, end_logit) per fleet, with time blocks replacing P1-P4 (and P6
# for Srv) in specific year windows. Rceattle's case 8 ("DoubleNormal") is a
# 4-param simplification:
#   sel_inf[1]     = peak       <- SS3 P1
#   sel_inf[2]     = end_logit  <- SS3 P6 (right-tail floor)
#   log_sel_slp[1] = log(sigma_asc)  <- SS3 P3 (ascending limb)
#   log_sel_slp[2] = log(sigma_desc) <- SS3 P4 (descending limb)
# SS3 P2 (top-width) and P5 (init_logit) have no analog -- discrepancies in
# Lsel where these matter are expected.
#
# We use Time_varying_sel = "IID" (not "Block") because Block mode only maps
# years with catch/index observations and zeros out other years -- a problem
# for biennial surveys. IID maps every hindcast year, and we inject dev =
# (block-replacement - base) only for years in a block; pre-block years stay
# at base (dev = 0). All sel params are forward-pass-fixed via estimateMode = 3.
# =============================================================================
active_sel_fleets    <- c("FshTrawl", "FshLL", "FshPot", "Srv", "LLSrv")
fleet_block_pattern  <- c(FshTrawl = 2L, FshLL = 2L, FshPot = 3L, Srv = 1L,
                          LLSrv = NA_integer_)

# Helpers --------------------------------------------------------------------
ss3_sel_base <- function(parlist, fname, fnum) {
  S <- parlist$S_parms
  vapply(1:6, function(p) {
    pat <- sprintf("^SizeSel_P_%d_%s\\(%d\\)$", p, fname, fnum)
    idx <- grep(pat, rownames(S))
    if (length(idx) == 1L) S[idx, "ESTIM"] else NA_real_
  }, numeric(1))
}

ss3_sel_blocks <- function(parlist, fname, fnum, pattern_id) {
  if (is.na(pattern_id)) return(list())
  S <- parlist$S_parms
  pat <- sprintf("^SizeSel_P_(\\d)_%s\\(%d\\)_BLK%drepl_(\\d+)$",
                 fname, fnum, pattern_id)
  hits <- grep(pat, rownames(S), value = TRUE)
  lapply(hits, function(h) {
    parts <- regmatches(h, regexec(pat, h))[[1]]
    list(P = as.integer(parts[2]),
         start_yr = as.integer(parts[3]),
         value    = S[h, "ESTIM"])
  })
}

# A block design is a flat vector of (start1, end1, start2, end2, ...).
block_year_ranges <- function(block_design) {
  n_blk <- length(block_design) %/% 2L
  lapply(seq_len(n_blk), function(b) {
    c(start = block_design[2L * b - 1L], end = block_design[2L * b])
  })
}

# Override fleet_control for the active fleets --------------------------------
# Fleet Month chosen to match SS3's internal index/catch prediction timing:
#   * Fisheries (FshTrawl/FshLL/FshPot): Month = 6 (mid-year). For an annual
#     SS3 model, fisheries operate across the whole year; SS3's predicted
#     catch uses the Baranov equation with sel/WAA at the season midpoint.
#   * Surveys (Srv/LLSrv): Month = 7 (= per-obs month from datlist$CPUE).
#     SS3's index_hat for surveys evaluates the cohort distribution AT THE
#     OBS MONTH (not season midpoint), so Rceattle's fleet Month must equal
#     the obs month for sel_at_age + WAA + survival(mo/12) to all be
#     consistent inside index_hat = q * sum_age N * exp(-Z*mo/12) * sel * W.
fish_fleets <- c("FshTrawl", "FshLL", "FshPot")
surv_fleets <- c("Srv", "LLSrv")
# SS3 pattern-10 age selectivity (used by all Pcod fleets) hardcodes
# sel_a[0] = 0 (see SS_selex.tpl:999). Without zeroing sel_at_age[0],
# Rceattle's length convolution leaks the asc-limb floor into age 0
# (e.g. Srv init = 0.091 → sel_at_age[0] = 0.091), inflating the
# predicted-survey calc by ~35% via the large recruit cohort. Setting
# Age_first_selected = 1 zeros sel_at_age[0] post-convolution.
if (!"Age_first_selected" %in% colnames(cod_pcod$fleet_control)) {
  cod_pcod$fleet_control$Age_first_selected <- 0L  # default no floor
}
# SS3 addtocomp from dat file len_info / age_info (Pcod = 1e-4 for all fleets).
# Adds a small constant to every comp bin before sum=1 normalization;
# determines the floor proportion at tails where raw_pred is ~0.
if (!"Comp_addtocomp" %in% colnames(cod_pcod$fleet_control)) {
  cod_pcod$fleet_control$Comp_addtocomp <- 0
}
if (!"CAAL_addtocomp" %in% colnames(cod_pcod$fleet_control)) {
  cod_pcod$fleet_control$CAAL_addtocomp <- 0
}
cod_pcod$fleet_control$Comp_addtocomp[
  cod_pcod$fleet_control$Fleet_name %in% active_sel_fleets] <- 1e-4
cod_pcod$fleet_control$CAAL_addtocomp[
  cod_pcod$fleet_control$Fleet_name %in% active_sel_fleets] <- 1e-4
for (fname in active_sel_fleets) {
  fi <- which(cod_pcod$fleet_control$Fleet_name == fname)
  if (length(fi) == 0L) next
  cod_pcod$fleet_control$Selectivity[fi]           <- "DoubleNormal"
  cod_pcod$fleet_control$Selectivity_dimension[fi] <- "Length"
  # IID maps every hindcast year (so per-year dev_seq injection covers all).
  # Setting Time_varying_sel_sd_prior <= 0 tells the cpp to SKIP the N(0,σ)
  # penalty on the deviates (Phase 1 forward-pass: devs are pre-baked from
  # SS3, not estimated, so no prior should fire).
  cod_pcod$fleet_control$Time_varying_sel[fi]          <- "IID"
  cod_pcod$fleet_control$Time_varying_sel_sd_prior[fi] <- -1
  # SS3 robust multinomial kernel: NLL = N * sum_j obs_s * log(obs_s/hat_s)
  # with obs/hat smoothed by addtocomp. Matches SS3 Method-5 likelihood.
  cod_pcod$fleet_control$Comp_loglike[fi]          <- "SS3Robust"
  cod_pcod$fleet_control$CAAL_loglike[fi]          <- "SS3Robust"
  # Verified via SS3 source (SS_global.tpl:338) + empirical test: SS3 uses
  # data_timing_seas = 0.5 for both INDEX and CAAL with Pcod obs month=7.
  # Setting Month=7 instead breaks the INDEX (machine precision -> 5% off)
  # without consistently improving CAAL (Age 1 gets worse; the peak cell
  # dominates mean rel err). Mid-year (Month=6) is correct for both.
  cod_pcod$fleet_control$Month[fi] <- 6L
  cod_pcod$fleet_control$Age_first_selected[fi]    <- 2L   # 1-based: zero ages < 2 (= zero age 0 and age 1 in SS3 0-based? no, age 1 in R = age 0 in SS3)
}

# Override per-obs index_data Month to match SS3's "Time = season midpoint"
# convention used by SS3 internal index_hat (regardless of obs month). For
# Pcod nseas=1, season midpoint = month 6. Without this override Rceattle
# uses the per-obs month (= 7 for Pcod surveys), shifting the survival
# factor exp(-Z*mo/12) by 1 month and producing a ~5% predicted-index gap.
for (fname in surv_fleets) {
  fi <- which(cod_pcod$fleet_control$Fleet_name == fname)
  if (length(fi) == 0L) next
  rows <- which(cod_pcod$index_data$Fleet_code == fleet_meta$ss3_num[fi])
  if (length(rows) > 0)
    cod_pcod$index_data$Month[rows] <- 6L
}
# Age_first_selected indexing: 1-based, R->C++ via -1 in rearrange_data.R.
# Setting = 2 means "first selected slot = 2", i.e. zero slot 1 (= age 0 at
# minage=0). The C++ loop `for (age=0; age<1; age++) sel_at_age=0` zeros age 0.

cat("\n--- Selectivity switched to parametric Length-DoubleNormal ---\n")
print(cod_pcod$fleet_control[, c("Fleet_name", "Fleet_type", "Selectivity",
                                 "Selectivity_dimension", "Time_varying_sel")])


# =============================================================================
# 4. Build mod0 (parameter shape only, no fit) to get the inits skeleton
# =============================================================================
# Hoist growthFun so mod0 and the subsequent forward-pass / estimation calls
# share the exact same parameter shape. Adding growth linkages
# (e.g. priors on K/L1/Linf) expands beta_linkage; if mod0 doesn't have
# them but fit_mod does, the inits we pass back will be size-mismatched.
#
# Two specs because forward-pass and estimation need different setups:
#   _forward: NO growth linkages, so our injected log_growth_pars values
#             are used directly. With linkages active the effective param
#             is log_growth_pars + beta_linkage[<intercept>], which double-
#             counts when we inject both.
#   _est:     growth linkages carrying SS3 ctl priors on K, L1, Linf so
#             the optimizer sees the same penalty structure SS3 does.
growthFun_spec <- build_growth(fun = "vonBertalanffy")

growthFun_est_spec <- build_growth(
  fun = "vonBertalanffy",
  linkages = list(
    # Both `init` AND `priors` are NATURAL-scale for intercept rows:
    #  - build_params writes log(init_val) into log_growth_pars
    #  - cpp Slot 19 evaluates intercept-row priors against
    #    b_nat = exp(log_growth_pars[<param>]) (natural scale).
    # SS3 ctl uses Normal priors on the natural-scale K (sd=0.03) and
    # Linf (sd=0.015) -- same convention applies here.
    # Phase A1 bounds (2026-05-31): all lower bounds now strictly > 0.
    # build_bounds.R:99-170 silently skips the push to log-scale base
    # parameters when `lo <= 0` (because `log(0) = -Inf`). With the original
    # `c(0, X)` lower bounds the optimizer was free to walk K -> 0.069 and
    # growth_log_sd -> ~exp(2.4)=11 cm, poisoning the Hessian (NaN). Per
    # HANDOFF_estimation_parity.md Phase A1. Previous bounds in comments
    # next to each line.
    # Phase A1 bounds (2026-05-31): all lower bounds strictly > 0. Natural
    # scale -- build_bounds.R applies log() once when pushing to the
    # log-scale base param (log_growth_pars / growth_log_sd / log_M1).
    # `init` is now the SS3 **ESTIM** (MLE) value, not the ctl INIT prior.
    # Was: priors / starting values from SS3 ctl (K=0.1966, L1=6.1252,
    # Linf=99.4617, sd_L1=3.82037, sd_Linf=7.42895). With those starting
    # values, the linkage system's runtime evaluation of growth used the
    # PRIOR-mean values rather than the MLEs, producing huge FP NLL when
    # mod0 was switched to growthFun_est_spec (Bio rel err 165%).
    K    = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 0.1909988),     # SS3 ESTIM (was 0.1966 = INIT)
                        priors = list("(Intercept)" = lognormal(log(0.1966), 0.03)),
                        bounds = list("(Intercept)" = c(0.05, 1))),
    L1   = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 0.2465480),     # SS3 ESTIM (was 6.1252 = INIT)
                        bounds = list("(Intercept)" = c(0.1, 50))),
    Linf = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 99.4608396),    # SS3 ESTIM (was 99.4617 = INIT)
                        priors = list("(Intercept)" = normal(99.4617, 0.015)),
                        bounds = list("(Intercept)" = c(70, 130))),
    sd_L1   = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 2.9443667),     # SS3 ESTIM CV_young (was 3.82037 = INIT)
                        bounds = list("(Intercept)" = c(0.5, 10))),
    sd_Linf   = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 9.0740724),     # SS3 ESTIM CV_old (was 7.42895 = INIT)
                        bounds = list("(Intercept)" = c(0.5, 20)))
  )
)

# --- Diagnostic: force N-at-age to SS3 values per year (estDynamics=1) ----
# Tests whether the CAAL +5661 gap is from N-at-age dynamics drift (Methot-
# Taylor recruitment-bias-adjustment differences). If CAAL closes with this
# injection, the residual was N-at-age, not the likelihood formula or ALK.
# Must run BEFORE mod0 build so the param shape includes pop_scalar mapping.
inject_natage_ss3 <- FALSE
if (inject_natage_ss3) {
  cat("\n[diagnostic] Injecting SS3 N-at-age per year (estDynamics=1)\n")
  na <- ss3_rep$natage
  na_b <- na[na[["Beg/Mid"]] == "B" & na$Sex == 1 & na$Yr %in% years_hind, ]
  age_cols <- as.character(0:(nages_pcod - 1))
  miss <- setdiff(age_cols, names(na_b))
  if (length(miss) > 0) stop("Missing SS3 age columns: ", paste(miss, collapse=","))
  nbyage <- data.frame(
    Species_name = "Pcod",
    Species      = 1L,
    Sex          = 0L,
    Year         = as.integer(na_b$Yr)
  )
  for (a_i in seq_along(age_cols)) {
    nbyage[[paste0("Age ", a_i)]] <- as.numeric(na_b[[age_cols[a_i]]])
  }
  cod_pcod$NByageFixed <- nbyage
  cod_pcod$estDynamics <- 1L
  cat(sprintf("[diagnostic] NByageFixed: %d rows x %d cols, years %d-%d\n",
              nrow(nbyage), ncol(nbyage), min(nbyage$Year), max(nbyage$Year)))
}


cat("\n--- Building mod0 (parameter shape) ---\n")
# Phase A2 attempt 2026-05-31: tried mod0 + FP + estimation all using
# growthFun_est_spec for a single-skeleton workflow. mod0 build succeeds and
# build_bounds passes with natural-scale linkage bounds (now that L1 bound
# is loosened to encompass SS3's L_at_Amin = 0.247). BUT the FP fit_mod
# with growthFun_est_spec produces wildly wrong predictions:
#   - Bio rel err mean ~165%, Catch +20k, Length +3.5k, CAAL +7k
#   - ALK rel err 1.4e7
# So something in growthFun_est_spec's runtime evaluation reads
# log_growth_pars / beta_linkage differently than growthFun_spec does, and
# our injections don't account for it. Reverting to the split: mod0 with
# growthFun_spec (matches injection-helper expectations), FP with
# growthFun_spec (the working configuration). Estimation continues to use
# growthFun_est_spec separately. Phase A2 full unification needs deeper
# investigation of how linkages interact with cpp's growth evaluation.
mod0 <- Rceattle::fit_mod(
  data_list    = cod_pcod,
  inits        = NULL,
  estimateMode = 3,
  initMode     = 3,
  # Phase A2 unification attempt 2026-05-31 deferred: switching mod0 + FP
  # to growthFun_est_spec causes Bio rel err 165% and Catch +20k regardless
  # of whether linkage_spec init = SS3 INIT or SS3 ESTIM. Rce N drops to ~2%
  # of SS3 N — a population-scaling artifact in how cpp evaluates
  # log_growth_pars + beta_linkage that needs Rceattle-package-level trace.
  # Keeping FP path on growthFun_spec; estimation continues separately on
  # growthFun_est_spec. See HANDOFF Phase A3 follow-up.
  growthFun    = growthFun_spec,    # was: growthFun_est_spec (Bio rel err 165%)
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  fit_control  = fit_control(phase = FALSE, verbose = 1)
)
cat("\nRceattle parameter names:\n",
    paste(names(mod0$estimated_params), collapse = ", "), "\n")


# =============================================================================
# 5. SS3 -> Rceattle parameter injection (adapted for 9 fleets, minage=0)
# =============================================================================
init_from_ss3 <- function(parlist, ctllist, inits, data_list, fleet_meta,
                          years_hind, ss3_rep = NULL) {
  get_par <- function(section, pattern) {
    if (is.null(section)) return(NULL)
    idx <- grep(pattern, rownames(section))
    if (length(idx) == 0) return(NULL)
    section[idx[1], "ESTIM"]
  }

  # --- M: base + post-2014 block ---
  M_base <- get_par(parlist$MG_parms, "NatM_p_1_Fem_GP_1$")
  M_blk  <- get_par(parlist$MG_parms, "NatM_p_1_Fem_GP_1_BLK")
  if (!is.null(M_base) && "log_M1" %in% names(inits)) {
    inits$log_M1[] <- log(M_base)
    cat(sprintf("M_base = %.4f\n", M_base))
  }
  if (!is.null(M_blk) && !is.null(M_base) &&
      "beta_linkage" %in% names(inits)) {
    inits$beta_linkage[1] <- log(M_blk / M_base)
    cat(sprintf("M post-2014 = %.4f (beta = %.4f)\n",
                M_blk, log(M_blk / M_base)))
  }

  # --- log(R0) ---
  ln_R0 <- get_par(parlist$SR_parms, "SR_LN")
  if (!is.null(ln_R0) && "rec_pars" %in% names(inits)) {
    inits$rec_pars[1, 1] <- ln_R0
    cat(sprintf("log(R0) = %.4f  =>  R0 = %.4g\n", ln_R0, exp(ln_R0)))
  }

  # --- Recruitment devs (Methot-Taylor bias-adj applied per year) ---
  sigma_R <- get_par(parlist$SR_parms, "SR_sigmaR") %||% 0.6
  compute_bias_adj <- function(yr) {
    if (is.null(ctllist) || !isTRUE(ctllist$recdev_adv == 1))
      return(rep(1.0, length(yr)))
    bmax   <- ctllist$max_bias_adj
    # SS3 sentinel: max_bias_adj = -1 overrides the Methot-Taylor ramp and
    # sets bias_adj = 1.0 for ALL estimated recdevs. Without this gate,
    # plugging bmax = -1 into the linear ramp formulas below produces
    # negative bias adjustments that propagate into init$rec_dev as wrong-
    # sign offsets (-0.097 -> +0.097 per year on Pcod).
    if (isTRUE(bmax == -1)) return(rep(1.0, length(yr)))
    late0 <- ctllist$last_early_yr_nobias_adj
    first1 <- ctllist$first_yr_fullbias_adj
    last1  <- ctllist$last_yr_fullbias_adj
    first0 <- ctllist$first_recent_yr_nobias_adj
    sapply(yr, function(y) {
      if (y <= late0)  return(0)
      if (y <  first1) return(bmax * (y - late0)  / (first1 - late0))
      if (y <= last1)  return(bmax)
      if (y <  first0) return(bmax * (first0 - y) / (first0 - last1))
      0
    })
  }
  rec_devs <- do.call(rbind, Filter(Negate(is.null), list(
    parlist$recdev_early, parlist$recdev1, parlist$recdev2)))
  if ("rec_dev" %in% names(inits) && !is.null(rec_devs)) {
    ba <- compute_bias_adj(years_hind)
    n_set <- 0
    for (i in seq_len(nrow(rec_devs))) {
      yp <- which(years_hind == rec_devs[i, "year"])
      if (length(yp)) {
        inits$rec_dev[1, yp] <- rec_devs[i, "recdev"] - 0.5 * ba[yp] * sigma_R^2
        n_set <- n_set + 1
      }
    }
    cat(sprintf("Set rec_dev for %d years (sigmaR=%.3f)\n", n_set, sigma_R))
  }

  # --- VB growth (anchored at Rceattle's minage; SS3 references Growth_Age_for_L1/L2) ---
  L_min <- get_par(parlist$MG_parms, "L_at_Amin")
  L_max <- get_par(parlist$MG_parms, "L_at_Amax")
  K_vb  <- get_par(parlist$MG_parms, "VonBert_K")
  SD_y  <- get_par(parlist$MG_parms, "CV_young")
  SD_o  <- get_par(parlist$MG_parms, "CV_old")
  amin_gp <- ctllist$Growth_Age_for_L1 %||% data_list$minage[1]
  amax_gp <- ctllist$Growth_Age_for_L2 %||% (data_list$minage[1] + data_list$nages[1] - 1)
  if (!is.null(L_min) && !is.null(L_max) && !is.null(K_vb) &&
      "log_growth_pars" %in% names(inits)) {
    # Linf: SS3's L_at_Amax is sometimes a sentinel (-9 / 99) meaning Linf
    # itself; otherwise it's the asymptotic VB length at amax_gp years.
    Linf_est <- if (amax_gp >= 99) L_max
    else {
      delta <- exp(-K_vb * (amax_gp - amin_gp))
      (L_max - L_min * delta) / (1 - delta)
    }
    # Rceattle's growth.hpp now defaults the VB anchor age to 0.5 when
    # minage = 0 (matching SS3's default Growth_Age_for_L1), so we can
    # pass SS3's L_at_Amin directly as l1 -- no back-extrapolation needed,
    # which previously gave negative values that had to be floored.
    # At minage >= 1, Rceattle's anchor is minage, so we DO back-extrapolate
    # to the minage point.
    if (data_list$minage[1] == 0L) {
      L1_rce <- L_min  # = SS3 L_at_Amin at Growth_Age_for_L1
    } else {
      L1_rce <- Linf_est - (Linf_est - L_min) *
        exp(-K_vb * (data_list$minage[1] - amin_gp))
    }
    inits$log_growth_pars[1, 1, 1] <- log(K_vb)
    inits$log_growth_pars[1, 1, 2] <- log(max(L1_rce, 0.01))
    inits$log_growth_pars[1, 1, 3] <- log(Linf_est)
    cat(sprintf("Growth: K=%.4f, L1=%.4f (anchor age %.2f), Linf=%.4f\n",
                K_vb, L1_rce,
                if (data_list$minage[1] == 0L) 0.5 else data_list$minage[1],
                Linf_est))
  }
  if (!is.null(SD_y) && "growth_log_sd" %in% names(inits))
    inits$growth_log_sd[1, 1, 1] <- log(SD_y)
  if (!is.null(SD_o) && "growth_log_sd" %in% names(inits))
    inits$growth_log_sd[1, 1, 2] <- log(SD_o)

  # --- Weight-length ---
  W1 <- get_par(parlist$MG_parms, "Wtlen_1_Fem_GP_1")
  W2 <- get_par(parlist$MG_parms, "Wtlen_2_Fem_GP_1")
  if (!is.null(W1) && !is.null(W2) && "weight_length_pars" %in% names(inits)) {
    inits$weight_length_pars[1, 1] <- W1
    inits$weight_length_pars[1, 2] <- W2
    cat(sprintf("W-L: alpha=%.6g, beta=%.4f\n", W1, W2))
  }

  # --- Per-survey catchability ---
  # SS3 reports the realized Calc_Q per year per survey in ss3_rep$cpue.
  # Strategy: inject SS3's LnQ_base as index_log_q (gives constant q for
  # surveys without env), and for surveys with env-q (e.g. LLSrv with
  # env_var&link = 101 = "exponential link"), additionally inject
  # per-year q-deviates so Rceattle's exp(LnQ + dev) reproduces SS3's
  # Calc_Q exactly. The env-q exponential link
  #   q[yr] = exp(LnQ_base * exp(env_add * env_var[yr]))
  # is NOT representable in Rceattle's linear-on-log-q env path; the
  # per-year deviate route preserves exact SS3 parity without modifying
  # Rceattle's C++.
  if ("index_log_q" %in% names(inits)) {
    for (i in seq_len(nrow(fleet_meta))) {
      if (fleet_meta$fleet_type[i] != "Survey") next
      pat <- sprintf("LnQ_base_%s\\(%d\\)$",
                     fleet_meta$name[i], fleet_meta$ss3_num[i])
      q <- get_par(parlist$Q_parms, pat)
      if (!is.null(q)) {
        inits$index_log_q[i] <- q
        cat(sprintf("  q[%s] = %.4f (exp = %.4f)\n",
                    fleet_meta$name[i], q, exp(q)))
      }
      # Per-year q deviates from SS3 Calc_Q. Only applies when this fleet
      # has Time_varying_q != 0 / NA and index_q_dev has been allocated.
      tvq <- data_list$fleet_control$Time_varying_q[i]
      has_tv <- !is.na(tvq) && tvq %in% c("IID", "AR1", "RandomWalk")
      if (has_tv && "index_q_dev" %in% names(inits)) {
        cpue_rows <- ss3_rep$cpue[ss3_rep$cpue$Fleet == fleet_meta$ss3_num[i] &
                                  ss3_rep$cpue$Yr %in% years_hind, , drop = FALSE]
        if (nrow(cpue_rows) > 0) {
          q_base <- inits$index_log_q[i]
          for (k in seq_len(nrow(cpue_rows))) {
            yp <- which(years_hind == as.integer(cpue_rows$Yr[k]))
            if (length(yp) == 1L) {
              ss3_q <- as.numeric(cpue_rows$Calc_Q[k])
              if (!is.na(ss3_q) && ss3_q > 0) {
                inits$index_q_dev[i, yp] <- log(ss3_q) - q_base
              }
            }
          }
          cat(sprintf("  q[%s] per-year q_dev injected for %d years\n",
                      fleet_meta$name[i], nrow(cpue_rows)))
        }
      }
    }
  }
  inits
}


# =============================================================================
# 6. SS3 N-at-age injection (minage = 0: direct 1-to-1 mapping, no plus-group sum)
# =============================================================================
init_state_from_ss3_natage_m0 <- function(inits, ss3_rep, styr, nages) {
  # At minage = 0:  Rceattle slot k = SS3 int_Age (k - 1) for k = 1..nages.
  # Slot nages corresponds to SS3 col "nages-1" which is already the SS3
  # plus group (because SS3's max-age column accumulates the +group cohorts).
  ss3_age_cols <- as.character(0:(nages - 1))
  row <- ss3_rep$natage %>%
    dplyr::filter(Yr == styr, `Beg/Mid` == "B", Sex == 1) %>%
    dplyr::slice(1)
  if (nrow(row) == 0) stop("SS3 natage missing row for styr = ", styr)

  # If SS3 emits an extra plus-group column ("nages"), sum it into slot nages.
  # Otherwise the direct 1-to-1 mapping is correct.
  if (as.character(nages) %in% colnames(row)) {
    extra <- as.numeric(row[1, as.character(nages)])
    ss3_N <- as.numeric(row[1, ss3_age_cols])
    ss3_N[nages] <- ss3_N[nages] + extra
    cat(sprintf("SS3 natage: summing col '%s' (=%.4g) into slot %d (plus group)\n",
                nages, extra, nages))
  } else {
    ss3_N <- as.numeric(row[1, ss3_age_cols])
  }

  cat(sprintf("\nSS3 natage[%d] -> Rceattle slots 1..%d: %s\n", styr, nages,
              paste(sprintf("%.4g", ss3_N), collapse = ", ")))

  # Rceattle C++ (initMode == 0): N[sp, sex, slot_cpp, 0] = exp(init_dev[sp, slot_cpp - 1])
  # for slot_cpp > 0 (i.e., slots 2..nages in 1-indexed). slot 1 is recruits,
  # driven by R_init * exp(rec_dev[0]).
  if (!"init_dev" %in% names(inits)) {
    warning("init_dev not in inits"); return(inits)
  }
  for (k in seq_len(nages - 1)) {
    inits$init_dev[1, k] <- log(max(ss3_N[k + 1], 1e-10))
  }
  cat(sprintf("init_dev[1, 1:%d] set from SS3\n", nages - 1))
  inits
}


# init_state_from_ss3_natage_mode4: pin Rceattle's initMode = 4
# ("NonEquilibriumScaled") initial age-structure to SS3's exact N at styr by
# inverting the cpp formula
#   N_init[a] = R_init * exp(-Finit) * exp(-sum(M1[0..a-1]) + init_dev[a-1])
# for a = 1..nages-2; plus-group at a = nages-1 also includes geometric series
# correction `/ (1 - exp(-M1_last - Finit))`. Solving for init_dev gives:
#   init_dev[a-1] = log(ss3_N[a+1] / sex_ratio) - log(R_init) + sum(M1[0..a-1]) + Finit
# (plus, for the plus group, an extra `+ log(1 - exp(-M1_last - Finit))`).
# This keeps the SS3 regime-shift mechanism (Finit acts as a scalar on R_init)
# AND pins styr N exactly, eliminating the ~62% rel err at older ages observed
# under initMode = 5 (which forces init_dev = 0 and so cannot absorb the
# historical non-equilibrium recruitment structure).
init_state_from_ss3_natage_mode4 <- function(inits, ss3_rep, styr, nages,
                                              R_init, Finit, M1_at_age,
                                              sex_ratio_age0 = 0.5) {
  ss3_age_cols <- as.character(0:(nages - 1))
  row <- ss3_rep$natage %>%
    dplyr::filter(Yr == styr, `Beg/Mid` == "B", Sex == 1) %>%
    dplyr::slice(1)
  if (nrow(row) == 0) stop("SS3 natage missing row for styr = ", styr)
  if (as.character(nages) %in% colnames(row)) {
    extra <- as.numeric(row[1, as.character(nages)])
    ss3_N <- as.numeric(row[1, ss3_age_cols])
    ss3_N[nages] <- ss3_N[nages] + extra
  } else {
    ss3_N <- as.numeric(row[1, ss3_age_cols])
  }
  cat(sprintf("\n[mode 4] SS3 natage[%d] target: %s\n", styr,
              paste(sprintf("%.4g", ss3_N), collapse = ", ")))
  cat(sprintf("[mode 4] R_init = %.4g, Finit = %.4g\n", R_init, Finit))

  # cpp female line at age > 0:
  #   N[female] = R_init * exp(-mort_sum + init_dev) * sex_ratio
  # where mort_sum[a] = sum(M1[0..a-1]) + Finit (mode 4).
  # For sex_ratio = 0.5 on a Nsexes=1 model, sex_ratio appears AFTER the
  # equilibrium formula. We solve init_dev to make
  # N[female, a] = ss3_N[a+1] * sex_ratio (so that the COMBINED-SEX total
  # matches ss3_N[a+1]).
  for (k in seq_len(nages - 1)) {
    # k = 1..nages-1 (mapped to age 1..nages-1 in cpp 0-indexed)
    age <- k                              # cpp 0-indexed age (1..nages-1)
    sum_M <- sum(as.numeric(M1_at_age[1:age]))
    mort_sum <- sum_M + Finit
    target_N <- ss3_N[k + 1]              # SS3 N at age k (combined sex)
    if (age == (nages - 1)) {
      # Plus group has geometric-series correction in cpp
      geom <- 1 - exp(-as.numeric(M1_at_age[nages]) - Finit)
      target_N_eff <- target_N * geom
    } else {
      target_N_eff <- target_N
    }
    # Solve: target_N = R_init * exp(-mort_sum + init_dev) (combined sex,
    # so the sex_ratio split cancels because we sum 2 sex slots).
    inits$init_dev[1, k] <- log(max(target_N_eff, 1e-10)) - log(R_init) + mort_sum
  }
  cat(sprintf("[mode 4] init_dev[1, 1:%d] set to absorb non-equilibrium structure\n",
              nages - 1))
  inits
}


# =============================================================================
# 7. log_F pinning (per-fishery, per-year)
# =============================================================================
init_log_F_from_ss3 <- function(inits, ts_ss3, fleet_meta, years_hind,
                                ss3_rep = NULL) {
  if (!"log_F" %in% names(inits)) return(inits)
  log_F <- inits$log_F

  # Preferred path (F_Method = 2 with parameter-estimated F): read F directly
  # from ss3_rep$parameters, where every fleet/year entry lives as
  # `F_fleet_<ss3_num>_YR_<year>_s_1`. This is robust to r4ss failing to parse
  # corrupted `$timeseries` rows (a common F_Method = 2 output bug where SS3
  # emits multiple `Bratio_YYYY` entries smashed onto one line). Years with
  # no parameter row (truly inactive fleet/year combinations) fall through to
  # the ts-column path below or get the small-F sentinel.
  par_df <- if (!is.null(ss3_rep) && !is.null(ss3_rep$parameters))
    ss3_rep$parameters else NULL
  has_param_F <- !is.null(par_df) &&
    any(grepl("^F_fleet_[0-9]+_YR_[0-9]+", par_df$Label))
  if (has_param_F) {
    cat("\nDetected F_fleet_X_YR_Y parameters (F_Method = 2 path)\n")
    for (i in seq_len(nrow(fleet_meta))) {
      if (fleet_meta$fleet_type[i] != "Fishery") next
      fnum <- fleet_meta$ss3_num[i]
      f_vec <- vapply(years_hind, function(yr) {
        nm <- sprintf("^F_fleet_%d_YR_%d_s_", fnum, yr)
        idx <- grep(nm, par_df$Label)
        if (length(idx) == 1L) as.numeric(par_df$Value[idx])
        else NA_real_
      }, numeric(1))
      f_vec[is.na(f_vec) | f_vec <= 0] <- 1e-9
      log_F[i, seq_along(years_hind)] <- log(f_vec)
      cat(sprintf("  log_F[%s] <- $parameters[F_fleet_%d_YR_*] (yr1=%.3g, mid=%.3g, last=%.3g)\n",
                  fleet_meta$name[i], fnum,
                  f_vec[1], f_vec[length(f_vec) %/% 2], tail(f_vec, 1)))
    }
    inits$log_F <- log_F
    return(inits)
  }

  # Fallback (F_Method = 3 hybrid or earlier convention): read from the
  # ts_ss3 F columns. Was the ONLY path before 2026-05-31.
  ts_sub <- ts_ss3[match(years_hind, ts_ss3$Yr), ]
  ts_f_cols <- grep("^F[._:]_[0-9]+$|^F\\.\\._[0-9]+$|^F\\._[0-9]+$",
                    colnames(ts_sub), value = TRUE)
  if (length(ts_f_cols) == 0) {
    warning("No SS3 F:_n columns in ts file"); return(inits)
  }
  cat(sprintf("\nDetected SS3 ts F-cols: %s\n", paste(ts_f_cols, collapse=", ")))
  for (i in seq_len(nrow(fleet_meta))) {
    if (fleet_meta$fleet_type[i] != "Fishery") next
    if (fleet_meta$ss3_num[i] > length(ts_f_cols)) next
    f_col <- ts_f_cols[fleet_meta$ss3_num[i]]
    f_vec <- as.numeric(ts_sub[[f_col]])
    f_vec[is.na(f_vec) | f_vec <= 0] <- 1e-9
    log_F[i, seq_along(years_hind)] <- log(f_vec)
    cat(sprintf("  log_F[%s] <- ts$%s (yr1=%.3g, mid=%.3g, last=%.3g)\n",
                fleet_meta$name[i], f_col,
                f_vec[1], f_vec[length(f_vec) %/% 2], tail(f_vec, 1)))
  }
  inits$log_F <- log_F
  inits
}


# =============================================================================
# 7a. Inject SS3 Length-DoubleNormal sel params (base + per-block deviates)
# =============================================================================
init_doublenormal_from_ss3 <- function(inits, parlist, ctllist, fleet_meta,
                                       years_hind, ss3_rep = NULL) {
  # Rceattle now uses the SS3-pattern-24 6-param DoubleNormal:
  #   sel_inf[1]     = P1 peak
  #   sel_inf[2]     = P6 logit(right_floor)
  #   sel_inf[3]     = P5 logit(left_floor / init)
  #   log_sel_slp[1] = P3 log(sigma_asc)
  #   log_sel_slp[2] = P4 log(sigma_desc)
  #   log_sel_slp[3] = P2 top-width logit (plateau)
  # SS3 sentinel < -1000 on P5/P6 means "fix at -inf" (corresponding floor
  # is 0). We map to a large-negative logit so 1/(1+exp(-x)) -> 0.
  for (i in seq_len(nrow(fleet_meta))) {
    fname <- fleet_meta$name[i]
    fnum  <- fleet_meta$ss3_num[i]
    if (!fname %in% active_sel_fleets) next
    base <- ss3_sel_base(parlist, fname, fnum)
    if (any(is.na(base[c(1, 3, 4)]))) {
      warning(sprintf("Missing SS3 base P1/P3/P4 for %s -- skipping", fname))
      next
    }
    P1 <- base[1]; P2 <- base[2]; P3 <- base[3]
    P4 <- base[4]; P5 <- base[5]; P6 <- base[6]
    # SS3 sentinel < -1000 -> "fix at -inf" floor. Map to a logit far enough
    # negative that 1/(1+exp(-x)) is below 1e-10 (so init/final round to 0
    # like SS3). Previously -10 gave ~4.5e-5 which polluted small-L sel by
    # ~1e-5 absolute and showed up as ~120x rel err vs SS3's 0/1e-7 values.
    SENTINEL_LOGIT <- -25.0
    if (is.na(P2) || P2 < -100) P2 <- SENTINEL_LOGIT  # no plateau (peak2 ~ peak + binwidth)
    if (is.na(P5) || P5 < -100) P5 <- SENTINEL_LOGIT  # left floor -> 0
    if (is.na(P6) || P6 < -100) P6 <- SENTINEL_LOGIT  # right floor -> 0

    inits$sel_inf[1, i, 1]     <- P1   # peak length (cm)
    inits$sel_inf[2, i, 1]     <- P6   # logit(right_floor)
    inits$sel_inf[3, i, 1]     <- P5   # logit(left_floor / init)
    inits$log_sel_slp[1, i, 1] <- P3   # log(sigma_ascending)
    inits$log_sel_slp[2, i, 1] <- P4   # log(sigma_descending)
    inits$log_sel_slp[3, i, 1] <- P2   # top-width logit
    cat(sprintf("  %s base: peak=%.2f sigma_asc=%.3f sigma_desc=%.3f init=%.4f final=%.4f topW_lt=%.2f\n",
                fname, P1, exp(P3), exp(P4),
                1 / (1 + exp(-P5)), 1 / (1 + exp(-P6)), P2))

    # Per-year deviates: use SS3 SelSizeAdj table which gives the EFFECTIVE
    # per-year parameter values directly (Par1..Par6). This bypasses the
    # complex dev_seq -> effective-param scaling (SS3 applies different
    # scaling per param based on dev_link, dev_se, HI-LO bounds). For each
    # year reported in SelSizeAdj, compute Rce dev = SS3 effective - base.
    # SelSizeAdj only lists CHANGE years; forward-fill across all years.
    if (!is.null(ss3_rep) && !is.null(ss3_rep$SelSizeAdj)) {
      ssa <- ss3_rep$SelSizeAdj
      # F_Method = 2 (instantaneous F estimated per fleet/year) makes SS3
      # emit SelSizeAdj with one EXTRA leading column. The actual sel
      # parameters then live in Par2..Par7 instead of Par1..Par6 (Par1
      # carries an integer index / morph flag = 1). Detect by checking if
      # Par7 has any non-NA values for any fleet (only F_Method = 2 fills
      # it) and shift the column mapping. Was, before 2026-05-31:
      #   ssa_fl <- ssa[ssa$Fleet == fnum & ssa$Yr %in% years_hind, ]
      # always referenced Par1..Par6 below.
      shift_cols <- "Par7" %in% colnames(ssa) && any(!is.na(ssa$Par7))
      if (shift_cols) {
        # SS3 with F_Method = 2 emits TWO rows per fleet/year in SelSizeAdj:
        # one "main morph" with Par1 = 1 (the real sel parameters in Par2..)
        # and one placeholder with Par1 = 0 (all-zero Par2..Par7). Filtering
        # to Par1 == 1 keeps only the real values. Without this filter the
        # forward-fill loop below overwrites real values with zeros from the
        # placeholder rows, producing dev = -P1_base for every year (e.g.
        # peak dev = -65.95 for FshLL).
        # Also exclude rows where Par1 is itself a year-like value (>= 1000)
        # — these are mis-parsed section breaks in Report.sso.
        keep <- !is.na(ssa$Par1) & ssa$Par1 == 1
        ssa <- ssa[keep, , drop = FALSE]
        # Drop Par1 (flag) and rename Par2..Par7 -> Par1..Par6.
        ssa <- ssa[, !colnames(ssa) %in% "Par1", drop = FALSE]
        cn <- colnames(ssa)
        for (k in 2:7) {
          old <- paste0("Par", k); new_ <- paste0("Par", k - 1)
          if (old %in% cn) cn[cn == old] <- new_
        }
        colnames(ssa) <- cn
      }
      ssa_fl <- ssa[ssa$Fleet == fnum & ssa$Yr %in% years_hind, ]
      if (nrow(ssa_fl) > 0) {
        # Forward-fill per-year effective values
        eff_yr <- data.frame(Yr = years_hind, Par1=NA_real_, Par2=NA_real_,
                             Par3=NA_real_, Par4=NA_real_, Par5=NA_real_,
                             Par6=NA_real_)
        # Initialize with base for years before first SelSizeAdj entry
        eff_yr$Par1 <- P1; eff_yr$Par2 <- P2; eff_yr$Par3 <- P3
        eff_yr$Par4 <- P4; eff_yr$Par5 <- P5; eff_yr$Par6 <- P6
        # Overwrite with SS3-reported change years, then carry forward
        for (k in seq_len(nrow(ssa_fl))) {
          y_pos <- which(eff_yr$Yr >= ssa_fl$Yr[k])
          for (pp in 1:6) {
            pcol <- paste0("Par", pp)
            v <- ssa_fl[k, pcol]
            if (!is.na(v) && v > -1000) eff_yr[y_pos, pcol] <- v
          }
        }
        # SS3 sentinel < -1000 means "fix at -inf" -- map to SENTINEL_LOGIT
        for (pp in c("Par2","Par5","Par6")) {
          mask <- !is.na(eff_yr[[pp]]) & eff_yr[[pp]] < -100
          if (any(mask)) eff_yr[[pp]][mask] <- SENTINEL_LOGIT
        }
        # Inject dev = SS3 effective - Rce base into the matching Rce slot
        ss3_to_rce <- list(
          Par1 = list("sel_inf_dev",     1, P1),
          Par2 = list("log_sel_slp_dev", 3, P2),
          Par3 = list("log_sel_slp_dev", 1, P3),
          Par4 = list("log_sel_slp_dev", 2, P4),
          Par5 = list("sel_inf_dev",     3, P5),
          Par6 = list("sel_inf_dev",     2, P6)
        )
        for (pname in names(ss3_to_rce)) {
          arr <- ss3_to_rce[[pname]][[1]]
          slot <- ss3_to_rce[[pname]][[2]]
          base_v <- ss3_to_rce[[pname]][[3]]
          inits[[arr]][slot, i, 1, ] <- eff_yr[[pname]] - base_v
        }
      }
    }
  }
  inits
}


# =============================================================================
# 8. Wire it all up
# =============================================================================
inits <- init_from_ss3(parlist, ctllist, mod0$estimated_params, cod_pcod,
                       fleet_meta, years_hind, ss3_rep = ss3_rep)
inits <- init_state_from_ss3_natage_m0(inits, ss3_rep, cod_pcod$styr, nages_pcod)
inits <- init_log_F_from_ss3(inits, ts_ss3, fleet_meta, years_hind,
                             ss3_rep = ss3_rep)
inits <- init_doublenormal_from_ss3(inits, parlist, ctllist, fleet_meta, years_hind,
                                    ss3_rep = ss3_rep)

# inits$beta_linkage is already shape-correct because mod0 (above) uses
# growthFun_est_spec. The (Intercept) rows are 0 (mapped out, base param
# carries level); the M post2014 design row carries the SS3 MLE from
# linkage_spec(init = m_block_init).

# --- Inject log_Finit from SS3 SR_regime ----------------------------------
# Rceattle's `initMode = 4` ("FishedNonEquilibriumScaled") implements the
# same regime-shift mechanism as SS3's `SR_regime`: the initial-equilibrium
# recruitment is `R_init * exp(-Finit)`, which is mathematically identical
# to SS3's `R0 * exp(SR_regime)` under `Finit <-> -SR_regime`. The Pcod ctl
# estimates `SR_regime_BLK5add_1976 = -0.678228` (the BLK5 additive offset
# active from 1976 onward, i.e. for the styr-1977 initial equilibrium). So
# `log_Finit = log(-SR_regime) = log(0.678228) ≈ -0.388`.
# Note: SS3 ctl puts no formal prior on SR_regime (PR_type=0), but SS3
# still reports +2.78 NLL on the `InitEQ_Regime` row from a hardcoded soft
# penalty. Rceattle has no analogous penalty on `log_Finit` (free param).
sr_regime_mle <- NA_real_
if (!is.null(parlist$SR_parms)) {
  sr_row <- parlist$SR_parms[grep("SR_regime_BLK", rownames(parlist$SR_parms)), ]
  if (nrow(sr_row) > 0) sr_regime_mle <- as.numeric(sr_row[1, "ESTIM"])
}
if (is.finite(sr_regime_mle) && sr_regime_mle < 0) {
  finit_natural <- -sr_regime_mle              # Finit = -SR_regime
  inits$log_Finit[1] <- log(finit_natural)
  cat(sprintf(
    "\nSS3 SR_regime_BLK5add MLE = %.4f -> Finit = exp(log_Finit) = %.4f (log_Finit = %.4f)\n",
    sr_regime_mle, finit_natural, log(finit_natural)))
} else {
  cat("\nSR_regime MLE not negative or missing; leaving log_Finit at default.\n")
}

# Reformulate init_dev for initMode = 4 ("NonEquilibriumScaled"). The cpp
# formula at age > 0 is
#   N_init[a] = R_init * exp(-Finit) * exp(-sum(M1[0..a-1]) + init_dev[a-1])
# so init_dev absorbs whatever non-equilibrium structure SS3's actual styr
# N has on top of the pure regime equilibrium. With mode = 5 init_dev was
# forced to 0 and the styr N rel err was ~62% at older ages; mode 4 with
# this injection pins styr N to SS3 exactly (rel err 1e-6).
#
# Was, before 2026-05-31 (mode 5 path):
#   inits$init_dev[1, ] <- 0
#   cat("init_dev[1, ] zeroed out for initMode = 'EquilibriumScaled' (mode 5)\n")
R_init_pcod <- exp(parlist$SR_parms["SR_LN(R0)", "ESTIM"])
M1_at_age_pcod <- rep(parlist$MG_parms["NatM_p_1_Fem_GP_1", "ESTIM"], nages_pcod)
Finit_pcod <- if (is.finite(sr_regime_mle) && sr_regime_mle < 0) -sr_regime_mle else 0
inits <- init_state_from_ss3_natage_mode4(
  inits, ss3_rep, cod_pcod$styr, nages_pcod,
  R_init = R_init_pcod, Finit = Finit_pcod, M1_at_age = M1_at_age_pcod
)



# =============================================================================
# 8a. Phase-0 audit diagnostics (PARAMS): which slots were injected from SS3?
# =============================================================================
# Walk every slot of mod0$estimated_params after all SS3 injections and report
# per-slot whether the values came from an injection helper or build_params
# defaults. Any "default-only" slot for an active fleet/species is a candidate
# source of forward-pass drift not yet documented in Estimation_Differences.md.
#
# Returns a data.frame; prints a clean per-slot table + a flagged subset.
dump_param_audit <- function(default_params, injected_params,
                             active_fleets = integer(0),
                             active_species = 1L,
                             tol = 1e-9, verbose = TRUE) {
  slot_names <- intersect(names(default_params), names(injected_params))
  rows <- vector("list", length(slot_names))
  for (k in seq_along(slot_names)) {
    nm <- slot_names[[k]]
    dv <- default_params[[nm]]; iv <- injected_params[[nm]]
    if (is.null(dv) || length(dv) == 0L) {
      rows[[k]] <- data.frame(slot = nm, n = 0L, n_changed = 0L,
                              n_default = 0L, n_sentinel = 0L,
                              status = "empty", stringsAsFactors = FALSE)
      next
    }
    dvn <- as.numeric(dv); ivn <- as.numeric(iv)
    n <- length(dvn)
    # Sentinel values (-999, -1000) are mapped-out slots; track separately.
    sentinel <- abs(ivn + 999) < 1 | abs(ivn + 1000) < 1
    n_sentinel <- sum(sentinel, na.rm = TRUE)
    only_na <- xor(is.na(dvn), is.na(ivn))
    changed <- only_na | (!is.na(dvn) & !is.na(ivn) &
                           abs(ivn - dvn) > tol)
    # Sentinel slots aren't "injection from SS3" — count them in their own bin
    changed_nonsent <- changed & !sentinel
    n_changed <- sum(changed_nonsent, na.rm = TRUE)
    n_active <- n - n_sentinel
    n_default <- n_active - n_changed
    status <- if (n_active == 0) {
      "all sentinel/mapped-out"
    } else if (n_changed == n_active) {
      "fully injected"
    } else if (n_changed > 0) {
      sprintf("partial (%d/%d injected)", n_changed, n_active)
    } else {
      "build_params default"
    }
    rows[[k]] <- data.frame(slot = nm, n = n, n_changed = n_changed,
                            n_default = n_default,
                            n_sentinel = n_sentinel,
                            status = status, stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows); rownames(out) <- NULL

  # Slots that legitimately stay at build_params defaults for the FP path
  # (predation slots, projection F, env-q without env data, etc).
  EXPECTED_DEFAULT <- c(
    "dummy",
    "log_pop_scalar",         # only used for population scaling
    "log_M1",                 # build_params writes log(M_prior_rce) from
                              #   SS3 ctl PRIOR (via linkage_spec init); the
                              #   default IS the SS3 value -> equality is
                              #   expected, not a missing injection
    "log_M1_dev", "M1_beta", "M1_rho", "M1_dev_log_sd",  # no M devs configured
    "log_Flimit", "log_Ftarget", "log_Finit", "proj_F_prop",  # projection
    "log_growth_par_devs",    # no growth devs
    "weight_length_pars",     # SS3 alpha/beta passed via data_list at build
    "sel_coff", "sel_coff_dev",  # non-parametric sel slots, not used here
    "index_q_beta",           # env-q link uses index_q_dev (IID) path
                              #   instead — see Section 3a comment
    "index_q_rho", "index_q_dev_log_sd",
    "comp_weights", "caal_weights", "diet_comp_weights",
    "log_gam_a", "log_gam_b", "log_phi",
    "sel_dev_log_sd",         # set by data_list at build_params
    "sel_curve_pen",
    "index_log_sd", "catch_log_sd", "index_log_q",
    "index_q_log_sd",         # q-prior SD: set by data_list$fleet_control$Q_sd_prior
    "R_log_sd"
  )

  if (verbose) {
    cat("\n=== dump_param_audit() (PHASE 0 INPUT VALIDATION) ===\n")
    cat("Per-slot injection coverage (injected = differs from build_params default):\n")
    print(out, row.names = FALSE)

    flagged <- out[out$status == "build_params default" &
                   out$n > 0 &
                   !(out$slot %in% EXPECTED_DEFAULT), ]
    if (nrow(flagged) > 0) {
      cat("\n!! Unexpected build_params defaults (slots not on EXPECTED_DEFAULT list):\n")
      print(flagged, row.names = FALSE)
      cat("Investigate: either extend an injection helper, or add this slot to\n",
          "EXPECTED_DEFAULT inside dump_param_audit() with a comment explaining why.\n",
          sep = "")
    } else {
      cat("\nOK: every non-empty slot is either injected, sentinel, or on the\n",
          "EXPECTED_DEFAULT allow-list.\n", sep = "")
    }
  }
  invisible(out)
}


# =============================================================================
# 8b. Phase-0 audit diagnostics (BOUNDS): Rce bounds vs SS3 ctl LO/HI
# =============================================================================
# Build the bounds object Rceattle will use, walk every estimable slot, and
# compare per-slot bound finiteness + (for mapped slots) SS3 ctllist LO/HI.
# Flags mismatches on the mapped slots and reports default bounds (-Inf, Inf)
# for the rest.
bounds_audit <- function(default_params, data_list, ctllist, species = 1L,
                         verbose = TRUE) {
  # build_bounds reads data_list$suitMode (set on data_list by fit_mod from
  # its `suitMode` arg, not by switch_check). For a standalone audit, default
  # to 0 (no length-based suitability) per fit_mod's default.
  data_list <- Rceattle::switch_check(data_list)
  if (is.null(data_list$suitMode) || length(data_list$suitMode) == 0L) {
    data_list$suitMode <- rep(0L, data_list$nspp)
  }
  bounds <- Rceattle::build_bounds(param_list = default_params,
                                   data_list  = data_list)
  lo <- bounds$lower; hi <- bounds$upper
  slot_names <- names(default_params)

  # ---- Per-slot bound summary -------------------------------------------------
  rows <- vector("list", length(slot_names))
  for (k in seq_along(slot_names)) {
    nm <- slot_names[[k]]
    lv <- as.numeric(lo[[nm]]); hv <- as.numeric(hi[[nm]])
    if (length(lv) == 0) {
      rows[[k]] <- data.frame(slot=nm, n=0L, n_finite_lo=0L, n_finite_hi=0L,
                              min_lo=NA_real_, max_hi=NA_real_,
                              stringsAsFactors=FALSE)
      next
    }
    n <- length(lv)
    fin_lo <- sum(is.finite(lv))
    fin_hi <- sum(is.finite(hv))
    min_lo <- if (fin_lo > 0) min(lv[is.finite(lv)]) else NA_real_
    max_hi <- if (fin_hi > 0) max(hv[is.finite(hv)]) else NA_real_
    rows[[k]] <- data.frame(slot=nm, n=n,
                            n_finite_lo=fin_lo, n_finite_hi=fin_hi,
                            min_lo=signif(min_lo, 4),
                            max_hi=signif(max_hi, 4),
                            stringsAsFactors=FALSE)
  }
  bound_tbl <- do.call(rbind, rows); rownames(bound_tbl) <- NULL

  # ---- Mapped-slot LO/HI comparison vs SS3 ctllist ----------------------------
  get_ss3 <- function(section_name, pat) {
    section <- ctllist[[section_name]]
    if (is.null(section)) return(c(NA_real_, NA_real_))
    idx <- grep(pat, rownames(section))
    if (length(idx) == 0) return(c(NA_real_, NA_real_))
    row <- section[idx[1], ]
    c(as.numeric(row$LO), as.numeric(row$HI))
  }

  # Helper to extract a single Rce bound by linear index path
  rce_bound <- function(slot, idx) {
    list(lo = as.numeric(lo[[slot]][idx]),
         hi = as.numeric(hi[[slot]][idx]))
  }

  # Map (Rce slot, idx, transform) -> (ctllist section, pattern)
  # transform takes SS3 (lo, hi) and returns Rce-scale (lo, hi).
  # All scalar SS3 -> log() transforms applied here (params live on log
  # scale in Rceattle), except SR_LN(R0) which SS3 already stores on log.
  ident <- function(v) v
  log_tx <- function(v) c(log(v[1]), log(v[2]))

  mappings <- list(
    list(label = "M (NatM_p_1_Fem_GP_1)",        slot = "log_M1",
         idx = c(species, 1L, 1L), section = "MG_parms",
         pat = "^NatM_p_1_Fem_GP_1$",            tx  = log_tx),
    list(label = "growth K (VonBert_K)",         slot = "log_growth_pars",
         idx = c(species, 1L, 1L), section = "MG_parms",
         pat = "^VonBert_K_Fem_GP_1$",           tx  = log_tx),
    list(label = "growth L1 (L_at_Amin)",        slot = "log_growth_pars",
         idx = c(species, 1L, 2L), section = "MG_parms",
         pat = "^L_at_Amin_Fem_GP_1$",           tx  = log_tx),
    list(label = "growth Linf (L_at_Amax)",      slot = "log_growth_pars",
         idx = c(species, 1L, 3L), section = "MG_parms",
         pat = "^L_at_Amax_Fem_GP_1$",           tx  = log_tx),
    list(label = "growth sd_young (CV_young)",   slot = "growth_log_sd",
         idx = c(species, 1L, 1L), section = "MG_parms",
         pat = "^CV_young_Fem_GP_1$",            tx  = log_tx),
    list(label = "growth sd_old (CV_old)",       slot = "growth_log_sd",
         idx = c(species, 1L, 2L), section = "MG_parms",
         pat = "^CV_old_Fem_GP_1$",              tx  = log_tx),
    list(label = "R0 (SR_LN(R0))",               slot = "rec_pars",
         idx = c(species, 1L),     section = "SR_parms",
         pat = "^SR_LN.R0.$",                    tx  = ident),
    list(label = "sigma_R (SR_sigmaR)",          slot = "R_log_sd",
         idx = c(species),         section = "SR_parms",
         pat = "^SR_sigmaR$",                    tx  = log_tx)
  )

  cmp_rows <- vector("list", length(mappings))
  for (k in seq_along(mappings)) {
    m <- mappings[[k]]
    ss3 <- get_ss3(m$section, m$pat)
    if (any(is.na(ss3))) {
      cmp_rows[[k]] <- data.frame(
        param = m$label, ss3_lo = NA_real_, ss3_hi = NA_real_,
        rce_lo = NA_real_, rce_hi = NA_real_,
        lo_match = NA, hi_match = NA,
        note = "SS3 param not found",
        stringsAsFactors = FALSE)
      next
    }
    rce <- rce_bound(m$slot, matrix(m$idx, nrow = 1))
    expected <- m$tx(ss3)
    # match-if-within-1e-4 (loose tolerance — bound mismatch of any size matters)
    lo_match <- isTRUE(abs(rce$lo - expected[1]) < 1e-4) ||
                (is.infinite(rce$lo) && is.infinite(expected[1]))
    hi_match <- isTRUE(abs(rce$hi - expected[2]) < 1e-4) ||
                (is.infinite(rce$hi) && is.infinite(expected[2]))
    cmp_rows[[k]] <- data.frame(
      param   = m$label,
      ss3_lo  = signif(ss3[1], 5), ss3_hi = signif(ss3[2], 5),
      rce_lo  = signif(rce$lo, 5), rce_hi = signif(rce$hi, 5),
      lo_match = lo_match,         hi_match = hi_match,
      note    = ifelse(lo_match && hi_match, "OK", "MISMATCH"),
      stringsAsFactors = FALSE)
  }
  cmp_tbl <- do.call(rbind, cmp_rows); rownames(cmp_tbl) <- NULL

  if (verbose) {
    cat("\n=== bounds_audit() (PHASE 0 INPUT VALIDATION) ===\n")
    cat("Per-slot Rce bound summary (n_finite_* = # finite bounds; -Inf/Inf = unbounded):\n")
    print(bound_tbl, row.names = FALSE)

    cat("\nSS3 ctl LO/HI vs Rce bounds for mapped scalar params:\n")
    print(cmp_tbl, row.names = FALSE)

    bad <- cmp_tbl[!is.na(cmp_tbl$note) & cmp_tbl$note == "MISMATCH", ]
    if (nrow(bad) > 0) {
      cat("\n!! Bound MISMATCHES (Rce default differs from SS3 ctl LO/HI):\n")
      print(bad, row.names = FALSE)
      cat("Fix: either (a) extend linkage_spec(bounds=...) for the param,\n",
          "(b) pass bounds=... to fit_mod, or (c) document the diff in\n",
          "Estimation_Differences.md.\n", sep = "")
    } else {
      cat("\nOK: every mapped scalar bound matches SS3 ctl LO/HI.\n")
    }
  }
  invisible(list(bounds = bound_tbl, mapped = cmp_tbl))
}


# Run the audits BEFORE the forward-pass fit, so any drift is flagged early.
.fleet_active <- which(cod_pcod$fleet_control$Fleet_type > 0)
.audit_dump   <- dump_param_audit(default_params = mod0$estimated_params,
                                  injected_params = inits,
                                  active_fleets = .fleet_active,
                                  active_species = 1L)
.audit_bounds <- bounds_audit(default_params = mod0$estimated_params,
                              data_list      = cod_pcod,
                              ctllist        = ctllist,
                              species        = 1L)



# =============================================================================
# 9. Forward-pass fit (estimateMode = 3) and comparison to SS3
# =============================================================================
cat("\n--- Forward-pass fit (estimateMode = 3) ---\n")
# initMode = "NonEquilibriumScaled" (= 4) gives the SS3 SR_regime mechanism
# (Finit acts as a single scalar offset on R_init) WITH per-age init_dev to
# absorb SS3's historical non-equilibrium structure at styr. Combined with
# init_state_from_ss3_natage_mode4 above this pins styr N to SS3 exactly
# (rel err 1e-6). Was, before 2026-05-31:
#   initMode = "EquilibriumScaled"  # mode 5: init_dev = 0, styr N rel err ~62% at older ages
cod_pcod_fixed <- Rceattle::fit_mod(
  data_list    = cod_pcod,
  inits        = inits,
  estimateMode = 3,
  initMode     = "NonEquilibriumScaled",
  # Phase A2 unification deferred (2026-05-31): keeping FP on
  # growthFun_spec. Both attempts (with INIT-valued and ESTIM-valued
  # linkage_spec inits) produced wild Bio rel err.
  growthFun    = growthFun_spec,
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  fit_control  = fit_control(phase = FALSE, verbose = 1)
)

# --- Relative-error summary ---
ny <- length(years_hind)
bio_rce <- as.numeric(cod_pcod_fixed$quantities$biomass[1, 1:ny])
ssb_rce <- as.numeric(cod_pcod_fixed$quantities$ssb[1, 1:ny])
R_rce   <- as.numeric(cod_pcod_fixed$quantities$R[1, 1:ny])

ss3_bio <- ts_ss3$Bio_all[match(years_hind, ts_ss3$Yr)]
ss3_ssb <- ts_ss3$SpawnBio[match(years_hind, ts_ss3$Yr)]
ss3_R   <- ts_ss3$Recruit_0[match(years_hind, ts_ss3$Yr)]

diag_errors <- function(rce, ss3, label) {
  rel <- abs(rce - ss3) / pmax(abs(ss3), 1e-10)
  cat(sprintf("  %-3s max rel err: %.2e  (mean: %.2e)\n",
              label, max(rel, na.rm = TRUE), mean(rel, na.rm = TRUE)))
}
cat("\n=== Forward-pass relative errors vs SS3 ===\n")
diag_errors(R_rce,   ss3_R,   "R")
diag_errors(bio_rce, ss3_bio, "Bio")
diag_errors(ssb_rce, ss3_ssb, "SSB")

cat("\n=== First 5 + last 5 years (Bio / SSB / R) ===\n")
print(data.frame(
  Year      = c(head(years_hind, 5), tail(years_hind, 5)),
  Bio_SS3   = c(head(ss3_bio, 5), tail(ss3_bio, 5)),
  Bio_Rce   = c(head(bio_rce, 5), tail(bio_rce, 5)),
  SSB_SS3   = c(head(ss3_ssb, 5), tail(ss3_ssb, 5)),
  SSB_Rce   = c(head(ssb_rce, 5), tail(ssb_rce, 5)),
  R_SS3     = c(head(ss3_R, 5),   tail(ss3_R, 5)),
  R_Rce     = c(head(R_rce, 5),   tail(R_rce, 5))
))


# =============================================================================
# 9a. Selectivity-at-length and selectivity-at-age comparison (forward-pass)
# =============================================================================
cat("\n=== Sel-at-length (Rceattle) vs SS3 sizeselex Lsel ===\n")
# SS3 sizeselex 'Lsel' is the length-based selectivity per fleet/year/length
# bin. Rceattle's sel_at_length array is [flt, sex, length_bin, year].
ss3_len_bins <- datlist$lbin_vector_pop
nlen <- length(ss3_len_bins)
sel_len_err <- list()
for (i in seq_len(nrow(fleet_meta))) {
  if (!fleet_meta$name[i] %in% active_sel_fleets) next
  ss3_num <- fleet_meta$ss3_num[i]
  ss3_lsel <- ss3_rep$sizeselex %>%
    dplyr::filter(Factor == "Lsel", Fleet == ss3_num, Yr %in% years_hind,
                  Sex == 1)
  if (nrow(ss3_lsel) == 0) next
  lcols <- as.character(ss3_len_bins)
  lcols <- lcols[lcols %in% colnames(ss3_lsel)]
  if (length(lcols) == 0) next
  for (yi in seq_along(years_hind)) {
    yr <- years_hind[yi]
    rows_le <- ss3_lsel %>% dplyr::filter(Yr <= yr) %>%
      dplyr::arrange(dplyr::desc(Yr))
    if (nrow(rows_le) == 0) next
    ss3_vec <- as.numeric(rows_le[1, lcols])
    rce_vec <- as.numeric(cod_pcod_fixed$quantities$sel_at_length[i, 1, , yi])
    if (length(rce_vec) != length(ss3_vec)) next
    rel <- abs(rce_vec - ss3_vec) / pmax(abs(ss3_vec), 1e-4)
    sel_len_err[[length(sel_len_err) + 1]] <- data.frame(
      Fleet = fleet_meta$name[i], Year = yr,
      MaxRelErr  = max(rel, na.rm = TRUE),
      MeanRelErr = mean(rel, na.rm = TRUE),
      Rce_peak_len = ss3_len_bins[which.max(rce_vec)],
      SS3_peak_len = ss3_len_bins[which.max(ss3_vec)]
    )
  }
}
if (length(sel_len_err) > 0) {
  sel_len_err <- do.call(rbind, sel_len_err)
  print(sel_len_err %>% dplyr::group_by(Fleet) %>%
          dplyr::summarise(max_rel = max(MaxRelErr),
                           mean_rel = mean(MeanRelErr),
                           peak_match_rate = mean(Rce_peak_len == SS3_peak_len)))
}

cat("\n=== Sel-at-age (Rceattle, via growth) vs SS3 ageselex Asel2 ===\n")
age_cols_ss3 <- as.character(0:(nages_pcod - 1))
sel_age_err <- list()
for (i in seq_len(nrow(fleet_meta))) {
  if (!fleet_meta$name[i] %in% active_sel_fleets) next
  ss3_num <- fleet_meta$ss3_num[i]
  ss3_sub <- ss3_rep$ageselex %>%
    dplyr::filter(Factor == "Asel2", Fleet == ss3_num, Yr %in% years_hind)
  if (nrow(ss3_sub) == 0) next
  for (yi in seq_along(years_hind)) {
    yr <- years_hind[yi]
    rows_le <- ss3_sub %>% dplyr::filter(Yr <= yr) %>%
      dplyr::arrange(dplyr::desc(Yr))
    if (nrow(rows_le) == 0) next
    ss3_vec <- as.numeric(rows_le[1, age_cols_ss3])
    rce_vec <- as.numeric(cod_pcod_fixed$quantities$sel_at_age[i, 1, , yi])
    rel <- abs(rce_vec - ss3_vec) / pmax(abs(ss3_vec), 1e-4)
    sel_age_err[[length(sel_age_err) + 1]] <- data.frame(
      Fleet = fleet_meta$name[i], Year = yr,
      MaxRelErr = max(rel), MeanRelErr = mean(rel)
    )
  }
}
if (length(sel_age_err) > 0) {
  sel_age_err <- do.call(rbind, sel_age_err)
  print(sel_age_err %>% dplyr::group_by(Fleet) %>%
          dplyr::summarise(max_rel = max(MaxRelErr),
                           mean_rel = mean(MeanRelErr)))
}

# =============================================================================
# 9b. CRITICAL-QUANTITY parity vs SS3 (the three things that matter for a
# model without age-comp data: sel-at-length, ALK, WAA)
# =============================================================================
cat("\n=== Critical-quantity parity check (no age-comp data path) ===\n")
yr_last <- length(years_hind)
yr_last_int <- tail(years_hind, 1)

# --- (0) Catchability per survey per year vs SS3 Calc_Q --------------------
# SS3 reports realized Calc_Q per (fleet, year) in ss3_rep$cpue. For
# constant-q surveys (e.g. Srv), Rceattle's index_q is constant = exp(LnQ).
# For env-q surveys (e.g. LLSrv with env_var&link = 101), Rceattle is set
# to Estimated + Time_varying_q = IID with per-year q_dev injected so
# exp(LnQ + dev) matches SS3 Calc_Q exactly.
cat("\n[Catchability per survey/year vs SS3 Calc_Q]\n")
for (fname in c("Srv", "LLSrv")) {
  i <- which(fleet_meta$name == fname); if (length(i) == 0) next
  cpue_rows <- ss3_rep$cpue[ss3_rep$cpue$Fleet == fleet_meta$ss3_num[i] &
                            ss3_rep$cpue$Yr %in% years_hind, , drop = FALSE]
  if (nrow(cpue_rows) == 0) next
  rce_q <- as.numeric(cod_pcod_fixed$quantities$index_q[i, ])
  rel_per_year <- vapply(seq_len(nrow(cpue_rows)), function(k) {
    yp <- which(years_hind == as.integer(cpue_rows$Yr[k]))
    if (length(yp) != 1L) return(NA_real_)
    ss3q <- as.numeric(cpue_rows$Calc_Q[k])
    abs(rce_q[yp] - ss3q) / max(abs(ss3q), 1e-10)
  }, numeric(1))
  cat(sprintf("  %-6s n=%d  max rel err %.2e  mean %.2e\n",
              fname, sum(!is.na(rel_per_year)),
              max(rel_per_year, na.rm = TRUE),
              mean(rel_per_year, na.rm = TRUE)))
}

# --- (0b) Predicted survey biomass per year vs SS3 Exp ---------------------
# index_hat[yr] = q[yr] * sum_age N * exp(-Z*mo/12) * sel * WAA_flt
# This is the headline downstream check: if growth + sel + q all match SS3,
# the predicted survey index should too. SS3 reports the predicted value
# per observation as `Exp` in ss3_rep$cpue.
cat("\n[Predicted survey index_hat per year vs SS3 Exp]\n")
# index_hat is aligned with the POST-REARRANGE index_data. Use that ordering
# rather than the original cod_pcod$index_data, which may be re-sorted by
# rearrange_dat().
rce_idx_data <- if (!is.null(cod_pcod_fixed$data_list$index_data)) {
  as.data.frame(cod_pcod_fixed$data_list$index_data)
} else {
  cod_pcod$index_data
}
for (fname in c("Srv", "LLSrv")) {
  i <- which(fleet_meta$name == fname); if (length(i) == 0) next
  cpue_rows <- ss3_rep$cpue[ss3_rep$cpue$Fleet == fleet_meta$ss3_num[i] &
                            ss3_rep$cpue$Yr %in% years_hind, , drop = FALSE]
  if (nrow(cpue_rows) == 0) next
  rce_hits <- which(rce_idx_data$Fleet_code == fleet_meta$ss3_num[i] &
                    rce_idx_data$Year %in% cpue_rows$Yr)
  if (length(rce_hits) == 0) next
  rce_idx_vec <- as.numeric(cod_pcod_fixed$quantities$index_hat[rce_hits])
  ss3_exp <- cpue_rows$Exp[match(rce_idx_data$Year[rce_hits], cpue_rows$Yr)]
  rel <- abs(rce_idx_vec - ss3_exp) / pmax(abs(ss3_exp), 1e-10)
  cat(sprintf("  %-6s n=%d  max rel err %.2e  mean %.2e\n",
              fname, length(rel), max(rel), mean(rel)))
  cmp <- data.frame(Year = rce_idx_data$Year[rce_hits],
                    Rce  = signif(rce_idx_vec, 5),
                    SS3  = signif(ss3_exp, 5),
                    RelErr = signif(rel, 3))
  print(rbind(head(cmp, 3), tail(cmp, 3)), row.names = FALSE)
}


# --- (0e) Likelihood + prior contributions vs SS3 likelihoods_used ----------
# Rceattle jnll_comp uses named rows (set R-side); each row sums across
# fleets. The Rceattle component labels printed here are the model's own
# names (from cod_pcod_fixed$quantities$jnll_comp), not a hand-coded map.
cat("\n=== Likelihood + prior components vs SS3 ===\n")
jnll <- cod_pcod_fixed$quantities$jnll_comp
row_totals <- rowSums(jnll)
rce_labels <- rownames(jnll)
cat("Rceattle jnll_comp row totals (across fleets):\n")
print(data.frame(comp = rce_labels, NLL = round(row_totals, 4)))
cat(sprintf("\nTotal Rceattle NLL: %.4f  | SS3 TOTAL: %.4f\n",
            sum(row_totals), ss3_rep$likelihoods_used["TOTAL", "values"]))

# Map SS3 components to Rceattle rows by label-matching heuristic.
ss3_likes <- setNames(ss3_rep$likelihoods_used[, "values"],
                      rownames(ss3_rep$likelihoods_used))
pick <- function(needle) {
  i <- grep(needle, rce_labels, ignore.case = TRUE)
  if (length(i) == 0) NA_real_ else row_totals[i[1]]
}
pick_sum <- function(...) {
  needles <- c(...); tot <- 0
  for (n in needles) {
    i <- grep(n, rce_labels, ignore.case = TRUE)
    if (length(i) > 0) tot <- tot + sum(row_totals[i])
  }
  tot
}
cmp <- rbind(
  data.frame(Component = "Survey index",       SS3 = ss3_likes["Survey"],      Rce = pick("Index")),
  data.frame(Component = "Catch",              SS3 = ss3_likes["Catch"],       Rce = pick("Catch data")),
  data.frame(Component = "Length comp",        SS3 = ss3_likes["Length_comp"], Rce = pick("Composition")),
  data.frame(Component = "Age/CAAL comp",      SS3 = ss3_likes["Age_comp"],    Rce = pick("CAAL")),
  data.frame(Component = "Recruitment dev",    SS3 = ss3_likes["Recruitment"], Rce = pick("Recruitment dev")),
  data.frame(Component = "Init eq / init dev", SS3 = ss3_likes["InitEQ_Regime"], Rce = pick("Initial abundance")),
  data.frame(Component = "Parm priors (incl linkage)",
             SS3 = ss3_likes["Parm_priors"],
             Rce = pick_sum("M prior", "Linkage-table priors", "Catchability prior", "Stock-recruit prior")),
  data.frame(Component = "Parm devs (sel + q)",SS3 = ss3_likes["Parm_devs"],   Rce = pick_sum("Selectivity deviates", "Catchability deviates"))
)
cmp$Diff   <- signif(cmp$Rce - cmp$SS3, 4)
cmp$RelErr <- signif(abs(cmp$Rce - cmp$SS3) / pmax(abs(cmp$SS3), 1e-3), 3)
cmp$SS3 <- signif(cmp$SS3, 6); cmp$Rce <- signif(cmp$Rce, 6)
cat("\nGrouped NLL comparison (SS3 vs Rceattle):\n")
print(cmp, row.names = FALSE)


# --- (0e.A) STRIP additive constants from Rce NLL to compare kernels --------
# Rce uses dnorm()/dmultinom() that include log(sigma*sqrt(2*pi)) and the
# multinomial combinatorial constant. SS3 reports kernel-only. Compute
# Rce_kernel = Rce_total - constants for each component.
cat("\n=== (A) Rce NLL with additive constants stripped ===\n")

dlc <- cod_pcod_fixed$obj$env$data   # rearranged data list as seen by cpp
endyr_use <- max(years_hind)

# (A1) Survey index lognormal:
#   dnorm log-dens = -log(sigma) - 0.5*log(2*pi) - 0.5*resid^2/sigma^2
#   jnll = -dnorm = +log(sigma) + 0.5*log(2*pi) + kernel
# Strip the per-obs constants (+log(sigma) + 0.5*log(2*pi)).
idx_obs   <- dlc$index_obs
idx_ctl   <- dlc$index_ctl
idx_keep  <- idx_ctl[,3] > 0 & idx_ctl[,3] <= endyr_use & idx_obs[,1] > 0
# Active fleet check: flt_type > 0
flt_type  <- dlc$flt_type
fltidx    <- idx_ctl[,1]
idx_keep  <- idx_keep & (flt_type[fltidx] > 0)
# sigma: if est_sigma_index=0, sigma = idx_obs col 2; else estimated. Use col 2 here.
idx_sig   <- idx_obs[idx_keep, 2]
idx_const <- sum(log(idx_sig) + 0.5*log(2*pi))
idx_rce_total  <- row_totals[grep("Index", rce_labels)]
idx_rce_kernel <- idx_rce_total - idx_const
cat(sprintf("  Index   : n=%d Rce=%9.4f const=%9.4f kernel=%9.4f | SS3=%8.4f\n",
            sum(idx_keep), idx_rce_total, idx_const, idx_rce_kernel,
            ss3_likes["Survey"]))

# (A2) Catch lognormal: same
catch_obs <- dlc$catch_obs
catch_ctl <- dlc$catch_ctl
catch_keep<- catch_ctl[,3] > 0 & catch_ctl[,3] <= endyr_use & catch_obs[,1] > 0
catch_keep<- catch_keep & (flt_type[catch_ctl[,1]] == 1)
catch_sig <- catch_obs[catch_keep, 2]
catch_const <- sum(log(catch_sig) + 0.5*log(2*pi))
catch_rce_total  <- row_totals[grep("Catch data", rce_labels)]
catch_rce_kernel <- catch_rce_total - catch_const
cat(sprintf("  Catch   : n=%d Rce=%9.4f const=%9.4f kernel=%9.4f | SS3=%8.4f\n",
            sum(catch_keep), catch_rce_total, catch_const, catch_rce_kernel,
            ss3_likes["Catch"]))

# (A3) Comp multinomial:
#   dmultinom log-dens = lgamma(N+1) - sum(lgamma(x+1)) + sum(x*log(p))
#   jnll = -dmultinom = -lgamma(N+1) + sum(lgamma(x+1)) - sum(x*log(p))
# Constant part of jnll: sum(lgamma(x+1)) - lgamma(N+1)  (positive constants)
comp_obs   <- dlc$comp_obs
comp_n     <- dlc$comp_n
comp_ctl   <- dlc$comp_ctl
comp_yr    <- comp_ctl[,5]
comp_flt   <- comp_ctl[,1]
comp_type  <- comp_ctl[,4]  # 0=age, 1=length
nages_sp   <- cod_pcod$nages[1]
nlen_sp    <- cod_pcod$nlengths[1]
comp_keep  <- comp_yr > 0 & comp_yr <= endyr_use & comp_n[,2] > 0 & flt_type[comp_flt] > 0
comp_const <- 0
for (rr in which(comp_keep)) {
  N  <- comp_n[rr, 2]
  nb <- if (comp_type[rr] == 0) nages_sp else nlen_sp
  x  <- N * (comp_obs[rr, 1:nb] + 1e-5)
  comp_const <- comp_const + sum(lgamma(x + 1)) - lgamma(N + 1)
}
comp_rce_total  <- row_totals[grep("Composition", rce_labels)]
comp_rce_kernel <- comp_rce_total - comp_const
cat(sprintf("  LenComp : n=%d Rce=%9.4f const=%9.4f kernel=%9.4f | SS3=%8.4f\n",
            sum(comp_keep), comp_rce_total, comp_const, comp_rce_kernel,
            ss3_likes["Length_comp"]))

# (A4) CAAL multinomial: same form
caal_obs   <- dlc$caal_obs
caal_n     <- dlc$caal_n
caal_ctl   <- dlc$caal_ctl
caal_yr    <- caal_ctl[,4]
caal_flt   <- caal_ctl[,1]
caal_keep  <- caal_yr > 0 & caal_yr <= endyr_use & caal_n[,1] > 0 & flt_type[caal_flt] > 0
caal_const <- 0
for (rr in which(caal_keep)) {
  N <- caal_n[rr, 1]
  x <- N * (caal_obs[rr, 1:nages_sp] + 1e-5)
  caal_const <- caal_const + sum(lgamma(x + 1)) - lgamma(N + 1)
}
caal_rce_total  <- row_totals[grep("CAAL", rce_labels)]
caal_rce_kernel <- caal_rce_total - caal_const
cat(sprintf("  CAAL    : n=%d Rce=%9.4f const=%9.4f kernel=%9.4f | SS3=%8.4f\n",
            sum(caal_keep), caal_rce_total, caal_const, caal_rce_kernel,
            ss3_likes["Age_comp"]))


# --- (0e.B) Sel-dev penalty breakdown ---------------------------------------
# jnll_comp(5, flt) -= dnorm(sel_inf_dev, 0, sel_dev_sd) for each fleet/sex/yr
# For DoubleNormal (type 8) Rce penalizes 6 deviates per year:
#   sel_inf_dev(0)=P1 peak     log_sel_slp_dev(0)=P3 asc  (sd = sel_dev_sd)
#   sel_inf_dev(1)=P6 final    log_sel_slp_dev(1)=P4 desc (sd = 4 * sel_dev_sd)
#   sel_inf_dev(2)=P5 init     log_sel_slp_dev(2)=P2 topw
#   (Rce code only penalizes the first two pairs; ind 2 not in cpp loops)
cat("\n=== (B) Sel-dev penalty breakdown ===\n")
fc <- cod_pcod$fleet_control
flt_names <- fc$Fleet_name
# Resolve string labels -> numeric via Rceattle's internal dicts
flt_sel_type_num    <- unname(Rceattle:::sel_map[ as.character(fc$Selectivity) ])
flt_varying_sel_num <- unname(Rceattle:::tv_sel_map[ as.character(fc$Time_varying_sel) ])
flt_varying_sel_num[is.na(flt_varying_sel_num)] <-
  suppressWarnings(as.integer(fc$Time_varying_sel[is.na(flt_varying_sel_num)]))
sel_dev_sd_num      <- as.numeric(fc$Time_varying_sel_sd_prior)

cat("Fleet sel-dev configuration (numeric flags from cpp data):\n")
print(data.frame(Fleet = flt_names,
                 sel_type    = flt_sel_type_num,
                 varying_sel = flt_varying_sel_num,
                 sigma       = sel_dev_sd_num))

# Read deviates from the C++ params seen by the fitted obj
pl  <- cod_pcod_fixed$obj$env$parList()
sif <- pl$sel_inf_dev          # dim (3, n_flt, nsex, nyrs_hind)
lsd <- pl$log_sel_slp_dev      # same shape
fleet_meta_active <- which(flt_sel_type_num == 8 & flt_varying_sel_num %in% c(1,2))

cat("\nPer-fleet sel-dev NLL contributions (Rce formula):\n")
cat(sprintf("  %-10s %8s %10s %10s %10s %10s %10s\n",
            "Fleet","sigma","peakP1","ascP3","finalP6","descP4","subtotal"))
total_seldev <- 0
for (f in fleet_meta_active) {
  sig <- sel_dev_sd_num[f]
  if (is.na(sig) || sig <= 0) next
  peak_devs  <- as.numeric(sif[1, f, 1, ])
  final_devs <- as.numeric(sif[2, f, 1, ])
  asc_devs   <- as.numeric(lsd[1, f, 1, ])
  desc_devs  <- as.numeric(lsd[2, f, 1, ])
  nll_peak  <- sum(0.5*log(2*pi) + log(sig)   + 0.5*(peak_devs/sig)^2)
  nll_asc   <- sum(0.5*log(2*pi) + log(4*sig) + 0.5*(asc_devs/(4*sig))^2)
  nll_final <- sum(0.5*log(2*pi) + log(sig)   + 0.5*(final_devs/sig)^2)
  nll_desc  <- sum(0.5*log(2*pi) + log(4*sig) + 0.5*(desc_devs/(4*sig))^2)
  subtotal <- nll_peak + nll_asc + nll_final + nll_desc
  total_seldev <- total_seldev + subtotal
  cat(sprintf("  %-10s %8.3f %10.2f %10.2f %10.2f %10.2f %10.2f\n",
              flt_names[f], sig, nll_peak, nll_asc, nll_final, nll_desc, subtotal))
  cat(sprintf("    peak  range : %7.3f .. %7.3f  (max|dev|/sig    = %6.2f)\n",
              min(peak_devs), max(peak_devs), max(abs(peak_devs))/sig))
  cat(sprintf("    asc   range : %7.3f .. %7.3f  (max|dev|/(4sig) = %6.2f)\n",
              min(asc_devs), max(asc_devs), max(abs(asc_devs))/(4*sig)))
  cat(sprintf("    final range : %7.3f .. %7.3f  (max|dev|/sig    = %6.2f)\n",
              min(final_devs), max(final_devs), max(abs(final_devs))/sig))
  cat(sprintf("    desc  range : %7.3f .. %7.3f  (max|dev|/(4sig) = %6.2f)\n",
              min(desc_devs), max(desc_devs), max(abs(desc_devs))/(4*sig)))
}
cat(sprintf("  TOTAL sel-dev NLL (sum): %.2f\n", total_seldev))
cat(sprintf("  Rce reports for 'Selectivity deviates' row: %.2f\n",
            row_totals[grep("Selectivity deviates", rce_labels)]))


# --- (0e.C) Catch fit check: catch_hat vs catch_obs ------------------------
cat("\n=== (C) Catch fit check: predicted vs observed ===\n")
catch_hat <- as.numeric(cod_pcod_fixed$quantities$catch_hat)
chat_resid_log <- log(pmax(catch_obs[,1], 1e-10)) - log(pmax(catch_hat, 1e-10))
ck <- which(catch_keep)
cat(sprintf("  n obs = %d   |  log-resid range: %.4f .. %.4f  mean abs: %.4f\n",
            length(ck), min(chat_resid_log[ck]), max(chat_resid_log[ck]),
            mean(abs(chat_resid_log[ck]))))
cat("  Worst 5 catch residuals (log(obs) - log(hat)):\n")
worst <- ck[order(-abs(chat_resid_log[ck]))][1:5]
print(data.frame(fleet = flt_names[catch_ctl[worst, 1]],
                 year  = catch_ctl[worst, 3],
                 obs   = signif(catch_obs[worst, 1], 5),
                 hat   = signif(catch_hat[worst],    5),
                 sigma = catch_obs[worst, 2],
                 resid_log = signif(chat_resid_log[worst], 4)),
      row.names = FALSE)


# --- (0e.E) Per-fleet LenComp + CAAL NLL vs SS3 likelihoods_by_fleet -------
cat("\n=== (E) Per-fleet LenComp + CAAL NLL ===\n")
jnll_mat <- cod_pcod_fixed$quantities$jnll_comp  # [comp_idx, flt]
rce_len_per_flt  <- jnll_mat[grep("Composition", rownames(jnll_mat)), ]
rce_caal_per_flt <- jnll_mat[grep("CAAL", rownames(jnll_mat)), ]
fc_names         <- cod_pcod$fleet_control$Fleet_name
if (!is.null(ss3_rep$likelihoods_by_fleet)) {
  Lbf <- ss3_rep$likelihoods_by_fleet
  ss3_len  <- Lbf[Lbf$Label == "Length_like", , drop = FALSE]
  ss3_age  <- Lbf[Lbf$Label == "Age_like",    , drop = FALSE]
  cat("Length comp NLL by fleet (SS3 vs Rce):\n")
  for (fname in active_sel_fleets) {
    fi <- which(fc_names == fname); if (length(fi) == 0) next
    ss3_v <- if (fname %in% names(ss3_len)) as.numeric(ss3_len[[fname]]) else NA_real_
    rce_v <- as.numeric(rce_len_per_flt[fi])
    cat(sprintf("  %-9s SS3=%8.2f  Rce=%8.2f  Diff=%+8.2f\n",
                fname, ss3_v, rce_v, rce_v - ss3_v))
  }
  cat("CAAL NLL by fleet (SS3 vs Rce):\n")
  for (fname in active_sel_fleets) {
    fi <- which(fc_names == fname); if (length(fi) == 0) next
    ss3_v <- if (fname %in% names(ss3_age)) as.numeric(ss3_age[[fname]]) else NA_real_
    rce_v <- as.numeric(rce_caal_per_flt[fi])
    cat(sprintf("  %-9s SS3=%8.2f  Rce=%8.2f  Diff=%+8.2f\n",
                fname, ss3_v, rce_v, rce_v - ss3_v))
  }
}

# Manual recompute per-fleet LenComp using SS3 robust formula in R,
# matching cpp's case 2 exactly, to localize whether the +369 is a
# code-path or a numerical/cell-specific issue.
cat("\nManual recompute LenComp NLL (sanity check vs cpp jnll_comp):\n")
comp_obs_e   <- dlc$comp_obs
comp_hat_e   <- cod_pcod_fixed$quantities$comp_hat
comp_n_e     <- dlc$comp_n
comp_ctl_e   <- dlc$comp_ctl
addtocomp_v  <- cod_pcod$fleet_control$Comp_addtocomp
for (fname in active_sel_fleets) {
  fi <- which(fc_names == fname); if (length(fi)==0) next
  rows <- which(comp_ctl_e[,1] == fi & comp_ctl_e[,4] == 1 &
                comp_ctl_e[,5] > 0 & comp_ctl_e[,5] <= max(years_hind) &
                comp_n_e[,2] > 0)
  if (length(rows) == 0) next
  ac <- addtocomp_v[fi]
  denom <- 1 + nlen_sp * ac
  nll_manual <- 0
  for (rr in rows) {
    N  <- comp_n_e[rr, 2]
    obs_s <- (comp_obs_e[rr, 1:nlen_sp] + ac) / denom
    hat_s <- comp_hat_e[rr, 1:nlen_sp]
    nll_manual <- nll_manual + N * sum(obs_s * log(obs_s / pmax(hat_s, 1e-300)))
  }
  cat(sprintf("  %-9s manual=%9.4f  cpp=%9.4f  diff=%+9.6f\n",
              fname, nll_manual, rce_len_per_flt[fi],
              nll_manual - rce_len_per_flt[fi]))
}

# Phase 0 / Diff #13: decompose per-fleet LenComp NLL gap by length stratum.
# Hypothesis: comp_hat matches SS3 to ~1e-4 for L below the plus-group transition
# (~80 cm) and diverges by 1-25% above it, because Rce's static plus-group LAA
# convention sits ~10 cm lower than SS3's dynamic N-weighted plus-group. If true,
# the per-fleet NLL gap should be concentrated in the L >= 80 stratum.
cat("\nPer-fleet LenComp NLL stratified by length (L < 80 vs L >= 80):\n")
cat("Hypothesis: gap concentrates in L >= 80 (plus-group LAA divergence #13).\n")
ss3_data_lbins_strat <- datlist$lbin_vector
nbin_strat <- length(ss3_data_lbins_strat)
hi_idx <- which(ss3_data_lbins_strat >= 80)
lo_idx <- which(ss3_data_lbins_strat <  80)
for (fname in active_sel_fleets) {
  fi <- which(fc_names == fname); if (length(fi) == 0) next
  rows <- which(comp_ctl_e[,1] == fi & comp_ctl_e[,4] == 1 &
                comp_ctl_e[,5] > 0 & comp_ctl_e[,5] <= max(years_hind) &
                comp_n_e[,2] > 0)
  if (length(rows) == 0) next
  ac <- addtocomp_v[fi]; denom <- 1 + nlen_sp * ac
  rce_lo <- rce_hi <- ss3_lo <- ss3_hi <- 0
  for (rr in rows) {
    yr <- comp_ctl_e[rr, 5]; N <- comp_n_e[rr, 2]
    obs_p <- comp_obs_e[rr, 1:nlen_sp]
    obs_s <- (obs_p + ac) / denom
    hat_s_rce <- comp_hat_e[rr, 1:nlen_sp]
    # SS3 per-yr predicted comp (from lendbase Exp) aligned to data bins
    ss3_yr <- ss3_rep$lendbase %>%
      dplyr::filter(Fleet == fleet_meta$ss3_num[fi], Yr == yr, Sex == 1)
    if (nrow(ss3_yr) == 0) next
    ss3_v <- ss3_yr$Exp[match(ss3_data_lbins_strat, ss3_yr$Bin)]
    if (any(is.na(ss3_v))) next
    ss3_v <- ss3_v / sum(ss3_v)
    hat_s_ss3 <- (ss3_v + ac) / denom
    # Per-bin contribution: N * obs_s * log(obs_s / hat_s)
    per_bin_rce <- N * obs_s * log(obs_s / pmax(hat_s_rce, 1e-300))
    per_bin_ss3 <- N * obs_s * log(obs_s / pmax(hat_s_ss3, 1e-300))
    rce_lo <- rce_lo + sum(per_bin_rce[lo_idx])
    rce_hi <- rce_hi + sum(per_bin_rce[hi_idx])
    ss3_lo <- ss3_lo + sum(per_bin_ss3[lo_idx])
    ss3_hi <- ss3_hi + sum(per_bin_ss3[hi_idx])
  }
  diff_lo <- rce_lo - ss3_lo
  diff_hi <- rce_hi - ss3_hi
  cat(sprintf("  %-9s  L<80: Rce=%7.3f SS3=%7.3f diff=%+6.3f  | L>=80: Rce=%7.3f SS3=%7.3f diff=%+6.3f  | total diff=%+6.3f\n",
              fname, rce_lo, ss3_lo, diff_lo,
                     rce_hi, ss3_hi, diff_hi,
                     diff_lo + diff_hi))
}

# Phase 0 / Diff #13 propagation to CAAL: decompose CAAL NLL gap by Lbin
# stratum. Hypothesis (same as length comp): the plus-group LAA divergence
# concentrates CAAL hat errors at Lbin >= 80 cm, particularly the plus-group
# bins (>= ~95 cm). If true, the per-fleet CAAL NLL gap should be dominated
# by L >= 80.
# Finer CAAL diagnostic: localize the L<80 gap by (a) L-decile and (b) which
# age in the per-cell sum drives most of the per-cell NLL diff. The L-decile
# table shows whether the gap is at L=10-30 (very young, ages 0-2) vs L=40-70
# (ages 3-5). The per-age driver table picks the cell-by-cell dominant age in
# the obs (max obs slot) and groups by that age.
cat("\nSrv L<80 fine-grained CAAL diagnostic (where does the +190 NLL live?):\n")
srv_i_dbg <- which(fc_names == "Srv")
srv_rows_dbg <- which(caal_ctl[,1] == srv_i_dbg & caal_ctl[,4] > 0 &
                      caal_ctl[,4] <= max(years_hind) & caal_n[,1] > 0)
ac_srv <- addtocomp_v[srv_i_dbg]; denom_age_srv <- 1 + nages_sp * ac_srv
ss3_srv_cb <- ss3_rep$condbase %>%
  dplyr::filter(Fleet == fleet_meta$ss3_num[srv_i_dbg], Yr %in% years_hind,
                Sex == 1)
caal_hat_local <- cod_pcod_fixed$quantities$caal_hat
minage_sp_dbg <- cod_pcod$minage[1]
detail <- list()
for (rr in srv_rows_dbg) {
  L_in <- cod_pcod$caal_data$Length[rr]
  if (is.na(L_in) || L_in >= 80) next
  yr <- caal_ctl[rr, 4]; N <- caal_n[rr, 1]
  obs <- caal_obs[rr, 1:nages_sp]
  obs_s <- (obs + ac_srv) / denom_age_srv
  hat_rce <- caal_hat_local[rr, 1:nages_sp]
  # SS3 hat from condbase
  ss3_sub <- ss3_srv_cb %>%
    dplyr::filter(Yr == yr, abs(Lbin_lo - (L_in - 1)) < 0.1)
  ss3_v <- numeric(nages_sp)
  if (nrow(ss3_sub) > 0) {
    for (j in seq_len(nages_sp)) {
      ss3_age <- j - 1L + minage_sp_dbg
      hit <- ss3_sub$Exp[ss3_sub$Bin == ss3_age]
      if (length(hit) > 0) ss3_v[j] <- hit[1]
    }
  }
  if (sum(ss3_v) == 0) next
  ss3_v <- ss3_v / sum(ss3_v)
  hat_ss3 <- (ss3_v + ac_srv) / denom_age_srv
  per_age_rce <- N * obs_s * log(obs_s / pmax(hat_rce, 1e-300))
  per_age_ss3 <- N * obs_s * log(obs_s / pmax(hat_ss3, 1e-300))
  dom_age <- which.max(obs) - 1L + minage_sp_dbg  # SS3-age of the dominant obs
  detail[[length(detail) + 1]] <- data.frame(
    yr = yr, L = L_in, N = N, dom_age = dom_age,
    rce_nll = sum(per_age_rce),
    ss3_nll = sum(per_age_ss3),
    diff = sum(per_age_rce) - sum(per_age_ss3),
    rce_hat_at_dom = hat_rce[which.max(obs)],
    ss3_hat_at_dom = hat_ss3[which.max(obs)]
  )
}
det <- do.call(rbind, detail)
cat(sprintf("  Total Srv L<80 diff: %.2f over %d cells\n",
            sum(det$diff), nrow(det)))

# Group by L decile
det$L_strat <- cut(det$L, breaks = c(0, 20, 30, 40, 50, 60, 70, 80),
                   include.lowest = TRUE, right = FALSE)
agg_L <- aggregate(cbind(diff, rce_nll, ss3_nll) ~ L_strat, det, sum)
agg_L$n <- aggregate(L ~ L_strat, det, length)$L
cat("\n  By L-stratum:\n")
print(agg_L[, c("L_strat", "n", "rce_nll", "ss3_nll", "diff")],
      row.names = FALSE)

# Group by dominant age in obs
agg_a <- aggregate(cbind(diff, rce_nll, ss3_nll) ~ dom_age, det, sum)
agg_a$n <- aggregate(L ~ dom_age, det, length)$L
cat("\n  By dominant observed age:\n")
print(agg_a[, c("dom_age", "n", "rce_nll", "ss3_nll", "diff")],
      row.names = FALSE)

# Top 8 worst Srv L<80 cells by absolute diff
det_sorted <- det[order(-abs(det$diff)), ]
cat("\n  Top 8 worst Srv L<80 cells:\n")
print(head(det_sorted[, c("yr", "L", "N", "dom_age",
                          "rce_nll", "ss3_nll", "diff",
                          "rce_hat_at_dom", "ss3_hat_at_dom")], 8),
      row.names = FALSE, digits = 4)

# Full per-age comparison for the worst Srv cell:
# print obs, Rce_hat, SS3_hat per age, plus the N(a, mid-year) and ALK(a, L)
# components that go into the prediction. This shows whether the diff is in
# N-at-age or ALK.
worst_cell <- det_sorted[1, ]
wcyr <- worst_cell$yr; wcL <- worst_cell$L
rr_worst <- srv_rows_dbg[which(caal_ctl[srv_rows_dbg, 4] == wcyr &
                               abs(cod_pcod$caal_data$Length[srv_rows_dbg] - wcL) < 0.1)[1]]
if (length(rr_worst) > 0 && !is.na(rr_worst)) {
  cat(sprintf("\n  Full per-age detail for yr=%d L=%g:\n", wcyr, wcL))
  obs_w <- caal_obs[rr_worst, 1:nages_sp]
  hat_rce_w <- caal_hat_local[rr_worst, 1:nages_sp]
  yi_w <- which(years_hind == wcyr)
  # Rce N-at-age at Jan 1 of the worst year
  nat_jan1 <- as.numeric(cod_pcod_fixed$quantities$N_at_age[1, 1, , yi_w])
  # Rce length_hat at Srv weight slot (mid-year for Srv)
  srv_wtind <- cod_pcod$fleet_control$Weight_index[srv_i_dbg]
  rce_LAA_mid <- as.numeric(cod_pcod_fixed$quantities$length_hat[srv_wtind, 1, , yi_w])
  # SS3 condbase for this cell
  ss3_cell <- ss3_srv_cb %>%
    dplyr::filter(Yr == wcyr, abs(Lbin_lo - (wcL - 1)) < 0.1)
  ss3_hat_per_age <- numeric(nages_sp)
  for (j in seq_len(nages_sp)) {
    sa <- j - 1L + minage_sp_dbg
    h <- ss3_cell$Exp[ss3_cell$Bin == sa]
    if (length(h) > 0) ss3_hat_per_age[j] <- h[1]
  }
  ss3_hat_per_age <- ss3_hat_per_age / pmax(sum(ss3_hat_per_age), 1e-12)
  print(data.frame(
    age          = 0:(nages_sp - 1) + minage_sp_dbg,
    obs          = signif(obs_w, 4),
    rce_hat      = signif(hat_rce_w, 4),
    ss3_hat      = signif(ss3_hat_per_age, 4),
    rce_div_ss3  = signif(hat_rce_w / pmax(ss3_hat_per_age, 1e-12), 4),
    Jan1_N_Rce   = signif(nat_jan1, 4),
    Rce_LAA_mid  = signif(rce_LAA_mid, 4)
  ), row.names = FALSE)
}

cat("\nPer-fleet CAAL NLL stratified by Lbin (L < 80 vs L >= 80):\n")
cat("  Format: Rce NLL | SS3-via-condbase NLL | Rce-SS3 diff\n")
caal_hat_local <- cod_pcod_fixed$quantities$caal_hat
for (fname in active_sel_fleets) {
  fi <- which(fc_names == fname); if (length(fi) == 0) next
  ss3_caal_flt <- ss3_rep$condbase %>%
    dplyr::filter(Fleet == fleet_meta$ss3_num[fi], Yr %in% years_hind, Sex == 1)
  rows <- which(caal_ctl[,1] == fi & caal_ctl[,4] > 0 &
                caal_ctl[,4] <= max(years_hind) & caal_n[,1] > 0)
  if (length(rows) == 0) next
  ac <- addtocomp_v[fi]; denom_age <- 1 + nages_sp * ac
  rce_lo <- rce_hi <- ss3_lo <- ss3_hi <- 0
  n_lo <- n_hi <- 0L
  for (rr in rows) {
    L_in <- cod_pcod$caal_data$Length[rr]
    if (is.na(L_in)) next
    yr   <- caal_ctl[rr, 4]
    N    <- caal_n[rr, 1]
    obs_s <- (caal_obs[rr, 1:nages_sp] + ac) / denom_age
    hat_rce <- caal_hat_local[rr, 1:nages_sp]
    rce_nll <- N * sum(obs_s * log(obs_s / pmax(hat_rce, 1e-300)))
    # SS3's predicted P(age|L) via condbase. Map SS3 Bin -> Rce slot using
    # the SS3 agebin_vector + minage offset (same logic as #14 fix).
    minage_sp <- cod_pcod$minage[1]
    ss3_sub <- ss3_caal_flt %>%
      dplyr::filter(Yr == yr, abs(Lbin_lo - (L_in - 1)) < 0.1)
    ss3_nll <- NA_real_
    if (nrow(ss3_sub) > 0) {
      ss3_v <- numeric(nages_sp)
      # SS3 Bin k = SS3 age k; Rce slot j = SS3 age (j - 1 + minage)
      for (j in seq_len(nages_sp)) {
        ss3_age <- j - 1L + minage_sp
        hit <- ss3_sub$Exp[ss3_sub$Bin == ss3_age]
        if (length(hit) > 0) ss3_v[j] <- hit[1]
      }
      if (sum(ss3_v) > 0) {
        ss3_v <- ss3_v / sum(ss3_v)
        hat_ss3 <- (ss3_v + ac) / denom_age
        ss3_nll <- N * sum(obs_s * log(obs_s / pmax(hat_ss3, 1e-300)))
      }
    }
    if (L_in < 80) {
      rce_lo <- rce_lo + rce_nll; n_lo <- n_lo + 1L
      if (!is.na(ss3_nll)) ss3_lo <- ss3_lo + ss3_nll
    } else {
      rce_hi <- rce_hi + rce_nll; n_hi <- n_hi + 1L
      if (!is.na(ss3_nll)) ss3_hi <- ss3_hi + ss3_nll
    }
  }
  cat(sprintf("  %-9s  L<80: n=%4d Rce=%8.2f SS3=%8.2f diff=%+8.2f  | L>=80: n=%4d Rce=%8.2f SS3=%8.2f diff=%+8.2f  | TOTAL diff=%+8.2f\n",
              fname, n_lo, rce_lo, ss3_lo, rce_lo - ss3_lo,
                     n_hi, rce_hi, ss3_hi, rce_hi - ss3_hi,
                     (rce_lo - ss3_lo) + (rce_hi - ss3_hi)))
}

# FshLL is the dominant LenComp gap source. Inspect per-year per-bin
# contributions vs SS3 to localize which years/bins drive +265.
cat("\nFshLL per-year LenComp NLL breakdown (top 5 worst-fitting years):\n")
fll_i  <- which(fc_names == "FshLL"); fll_code <- fleet_meta$ss3_num[fll_i]
fll_rows <- which(comp_ctl_e[,1] == fll_i & comp_ctl_e[,4] == 1 &
                  comp_ctl_e[,5] > 0 & comp_ctl_e[,5] <= max(years_hind) &
                  comp_n_e[,2] > 0)
ac_fll <- addtocomp_v[fll_i]; denom_fll <- 1 + nlen_sp * ac_fll
per_yr_nll <- data.frame(yr = integer(), N = numeric(), Rce_nll = numeric(),
                         SS3_nll = numeric(), diff = numeric())
for (rr in fll_rows) {
  yr <- comp_ctl_e[rr, 5]; N <- comp_n_e[rr, 2]
  obs_s <- (comp_obs_e[rr, 1:nlen_sp] + ac_fll) / denom_fll
  hat_s <- comp_hat_e[rr, 1:nlen_sp]
  rce_n <- N * sum(obs_s * log(obs_s / pmax(hat_s, 1e-300)))
  # SS3 per-yr from lendbase: sum N*obs_s*log(obs_s/hat_s_ss3)
  ss3_yr <- ss3_rep$lendbase %>%
    dplyr::filter(Fleet == fll_code, Yr == yr, Sex == 1)
  ss3_n <- NA
  if (nrow(ss3_yr) > 0) {
    ss3_data_lbins <- datlist$lbin_vector
    ss3_v <- ss3_yr$Exp[match(ss3_data_lbins, ss3_yr$Bin)]
    if (!any(is.na(ss3_v))) {
      ss3_v <- ss3_v / sum(ss3_v)   # normalize just in case
      hat_s_ss3 <- (ss3_v + ac_fll) / denom_fll
      ss3_n <- N * sum(obs_s * log(obs_s / pmax(hat_s_ss3, 1e-300)))
    }
  }
  per_yr_nll <- rbind(per_yr_nll, data.frame(yr=yr, N=N, Rce_nll=rce_n,
                                              SS3_nll=ss3_n, diff=rce_n-ss3_n))
}
per_yr_nll <- per_yr_nll[order(-abs(per_yr_nll$diff)), ]
print(head(per_yr_nll, 5), row.names = FALSE)
cat(sprintf("Total FshLL Rce NLL: %.2f  Total via SS3-Exp NLL: %.2f\n",
            sum(per_yr_nll$Rce_nll), sum(per_yr_nll$SS3_nll, na.rm=TRUE)))

# N-at-age verification: Rce vs SS3 for Srv year 2023
cat("\n=== N-at-age check for 2023 (Rce vs SS3) ===\n")
nat_rce <- cod_pcod_fixed$quantities$N_at_age
cat(sprintf("Rce N_at_age dim: %s\n", paste(dim(nat_rce), collapse="x")))
yr_idx <- which(years_hind == 2023)
nat23_rce <- as.numeric(nat_rce[1, 1, , yr_idx])
nat23_ss3 <- as.numeric(ss3_rep$natage[
  ss3_rep$natage[["Beg/Mid"]] == "B" & ss3_rep$natage$Sex == 1 &
  ss3_rep$natage$Yr == 2023,
  as.character(0:(nages_sp - 1))][1, ])
cat("  age   Rce N    SS3 N    Rce/SS3\n")
for (a in 0:(nages_sp - 1)) {
  cat(sprintf("  %3d  %10.1f %10.1f  %.4f\n",
              a, nat23_rce[a+1], nat23_ss3[a+1],
              nat23_rce[a+1] / pmax(nat23_ss3[a+1], 1)))
}


# Srv-specific diagnostic: per-year LenComp + sel match
cat("\n=== Srv per-year LenComp NLL breakdown (top 5 worst) ===\n")
srv_i <- which(fc_names == "Srv"); srv_code <- fleet_meta$ss3_num[srv_i]
srv_rows <- which(comp_ctl_e[,1] == srv_i & comp_ctl_e[,4] == 1 &
                  comp_ctl_e[,5] > 0 & comp_ctl_e[,5] <= max(years_hind) &
                  comp_n_e[,2] > 0)
ac_srv <- addtocomp_v[srv_i]; denom_srv <- 1 + nlen_sp * ac_srv
per_yr_srv <- data.frame(yr = integer(), N = numeric(), Rce_nll = numeric(),
                         SS3_nll = numeric(), diff = numeric())
for (rr in srv_rows) {
  yr <- comp_ctl_e[rr, 5]; N <- comp_n_e[rr, 2]
  obs_s <- (comp_obs_e[rr, 1:nlen_sp] + ac_srv) / denom_srv
  hat_s <- comp_hat_e[rr, 1:nlen_sp]
  rce_n <- N * sum(obs_s * log(obs_s / pmax(hat_s, 1e-300)))
  ss3_yr <- ss3_rep$lendbase %>%
    dplyr::filter(Fleet == srv_code, Yr == yr, Sex == 1)
  ss3_n <- NA
  if (nrow(ss3_yr) > 0) {
    ss3_v <- ss3_yr$Exp[match(datlist$lbin_vector, ss3_yr$Bin)]
    if (!any(is.na(ss3_v))) {
      ss3_v <- ss3_v / sum(ss3_v)
      hat_s_ss3 <- (ss3_v + ac_srv) / denom_srv
      ss3_n <- N * sum(obs_s * log(obs_s / pmax(hat_s_ss3, 1e-300)))
    }
  }
  per_yr_srv <- rbind(per_yr_srv, data.frame(yr=yr, N=N, Rce_nll=rce_n,
                                              SS3_nll=ss3_n, diff=rce_n-ss3_n))
}
per_yr_srv <- per_yr_srv[order(-abs(per_yr_srv$diff)), ]
print(head(per_yr_srv, 5), row.names = FALSE)
cat(sprintf("Total Srv Rce NLL: %.2f  Total via SS3-Exp NLL: %.2f\n",
            sum(per_yr_srv$Rce_nll), sum(per_yr_srv$SS3_nll, na.rm=TRUE)))

# Worst Srv year detail
worst_srv_yr <- per_yr_srv$yr[1]
rr_s <- srv_rows[which(comp_ctl_e[srv_rows, 5] == worst_srv_yr)][1]
ss3_yr_s <- ss3_rep$lendbase %>%
  dplyr::filter(Fleet == srv_code, Yr == worst_srv_yr, Sex == 1)
ss3_v_s <- ss3_yr_s$Exp[match(datlist$lbin_vector, ss3_yr_s$Bin)]
obs_p_s <- comp_obs_e[rr_s, 1:nlen_sp]
hat_p_s <- comp_hat_e[rr_s, 1:nlen_sp]
cat(sprintf("\nSrv detail worst year %d (N=%g):\n",
            worst_srv_yr, comp_n_e[rr_s,2]))
print(data.frame(
  bin = datlist$lbin_vector,
  obs_p = signif(obs_p_s, 4),
  Rce_hat = signif(hat_p_s, 4),
  SS3_hat = signif(ss3_v_s, 4),
  ratio = signif(hat_p_s / pmax(ss3_v_s, 1e-12), 4)
), row.names = FALSE)


# Srv CAAL per-cell diagnostic: predicted vs SS3 condbase
cat("\n=== Srv CAAL per-cell breakdown (top 10 worst) ===\n")
caal_hat <- cod_pcod_fixed$quantities$caal_hat
srv_caal_rows <- which(caal_ctl[,1] == srv_i & caal_ctl[,4] > 0 &
                       caal_ctl[,4] <= max(years_hind))
if (length(srv_caal_rows) > 0) {
  ss3_srv_caal <- ss3_rep$condbase %>%
    dplyr::filter(Fleet == srv_code, Yr %in% years_hind, Sex == 1)
  per_cell <- list()
  for (rr in srv_caal_rows) {
    yr <- caal_ctl[rr, 4]; ln <- caal_ctl[rr, 5]
    L_in <- cod_pcod$caal_data$Length[rr]   # original length value
    N    <- caal_n[rr, 1]
    rce_v <- caal_hat[rr, 1:nages_sp]
    ss3_sub <- ss3_srv_caal %>%
      dplyr::filter(Yr == yr & abs(Lbin_lo - (L_in - 1)) < 0.1)
    if (nrow(ss3_sub) == 0) next
    # r4ss condbase Bin column = 1-based age index (Bin=1 -> age 0).
    # Map ages 0..nages-1 (R index 1..nages) directly to Bin values.
    ss3_v <- ss3_sub$Exp[match(1:nages_sp, ss3_sub$Bin)]
    ss3_v[is.na(ss3_v)] <- 0
    # SS3-cell NLL with the same kernel form
    ac <- addtocomp_v[srv_i]; denom <- 1 + nages_sp * ac
    obs_s <- (caal_obs[rr, 1:nages_sp] + ac) / denom
    rce_n <- N * sum(obs_s * log(obs_s / pmax(rce_v, 1e-300)))
    ss3_n <- N * sum(obs_s * log(obs_s / pmax((ss3_v + ac)/denom, 1e-300)))
    per_cell[[length(per_cell)+1]] <- data.frame(
      yr = yr, L = L_in, N = N,
      rce_nll = rce_n, ss3_nll = ss3_n, diff = rce_n - ss3_n)
  }
  pc <- do.call(rbind, per_cell)
  pc <- pc[order(-abs(pc$diff)), ]
  cat("Top 10 worst CAAL cells (Rce - SS3):\n")
  print(head(pc, 10), row.names = FALSE)
  cat(sprintf("Sum Rce_nll across cells: %.2f  Sum SS3_nll: %.2f  Diff: %.2f\n",
              sum(pc$rce_nll), sum(pc$ss3_nll, na.rm=TRUE),
              sum(pc$rce_nll) - sum(pc$ss3_nll, na.rm=TRUE)))

  # Detail print: worst CAAL cell age distribution
  worst <- pc[1, ]
  rr_w <- which(caal_ctl[,1] == srv_i & caal_ctl[,4] == worst$yr &
                abs(cod_pcod$caal_data$Length - worst$L) < 0.1)[1]
  cat(sprintf("\nWorst Srv CAAL cell: yr=%d L=%g N=%g\n",
              worst$yr, worst$L, worst$N))
  ss3_sub <- ss3_srv_caal %>%
    dplyr::filter(Yr == worst$yr & abs(Lbin_lo - (worst$L - 1)) < 0.1)
  ss3_v <- ss3_sub$Exp[match(1:nages_sp, ss3_sub$Bin)]; ss3_v[is.na(ss3_v)] <- 0
  print(data.frame(
    age = 0:(nages_sp - 1),
    obs = signif(caal_obs[rr_w, 1:nages_sp], 4),
    Rce = signif(caal_hat[rr_w, 1:nages_sp], 4),
    SS3 = signif(ss3_v / pmax(sum(ss3_v), 1e-10), 4),
    ratio = signif(caal_hat[rr_w, 1:nages_sp] /
                   pmax(ss3_v / pmax(sum(ss3_v), 1e-10), 1e-12), 4)
  ), row.names = FALSE)
}


# Rce growth_matrix_pop ALK at L=54 for ages 0..10 vs SS3 expected ALK
cat("\n=== ALK comparison at L=54.5 (Srv mid-year) age 2..5 ===\n")
gm <- cod_pcod_fixed$quantities$growth_matrix_pop
# wtind for Srv: check fleet wt timing
srv_wtind <- cod_pcod$fleet_control$Weight_index[srv_i]
cat(sprintf("Srv weight slot (wtind): %d\n", srv_wtind))
# yr_idx for 2023
yr23 <- which(years_hind == 2023)
# Find pop bin near L=54 (left edge 54)
lp_target <- which.min(abs(cod_pcod$lengths_pop[1, ] - 54))
cat(sprintf("Pop bin for L=54: index=%d (1-based; left edge=%g)\n",
            lp_target, cod_pcod$lengths_pop[1, lp_target]))
# SS3 endgrowth: SD_Mid per age
ss3_endgrow <- ss3_rep$endgrowth
ss3_endgrow <- ss3_endgrow[ss3_endgrow$Settlement == 1 & ss3_endgrow$Platoon == 1, ]
ss3_endgrow <- ss3_endgrow[order(ss3_endgrow$Real_Age), ]
ss3_lmid <- ss3_endgrow$Len_Mid[1:nages_sp]
ss3_smid <- ss3_endgrow$SD_Mid[1:nages_sp]
L_target <- 54.5
cat(sprintf("Comparing Rce ALK[age, lp=%d, yr=2023] vs SS3 dnorm(L=54.5; Len_Mid, SD_Mid):\n", lp_target))
cat(sprintf("  %-3s %10s %10s %10s %10s %10s %10s\n",
            "age","SS3_Lmid","SS3_SDmid","SS3_ALK","Rce_ALK","ratio","Rce/SS3"))
for (age in 0:(nages_sp - 1)) {
  rce_alk <- gm[srv_wtind, 1, age + 1, lp_target, yr23]
  ss3_alk <- dnorm(L_target, mean = ss3_lmid[age + 1], sd = ss3_smid[age + 1])
  cat(sprintf("  %-3d %10.3f %10.3f %10.3e %10.3e %10.3f %10.3f\n",
              age, ss3_lmid[age + 1], ss3_smid[age + 1],
              ss3_alk, rce_alk, rce_alk / pmax(ss3_alk, 1e-30),
              ss3_alk / pmax(rce_alk, 1e-30)))
}


# Check FshLL effective sel params for 1985 vs 2024 (block boundary)
cat("\nFshLL effective sel params per year (base + dev):\n")
fll_yrs_chk <- c(1980, 1985, 1990, 2000, 2010, 2020, 2024)
fll_chk <- sapply(fll_yrs_chk, function(y) {
  idx <- which(years_hind == y)
  if (length(idx) == 0) return(rep(NA, 6))
  c(peak     = inits$sel_inf[1, fll_i, 1] + inits$sel_inf_dev[1, fll_i, 1, idx],
    final_lt = inits$sel_inf[2, fll_i, 1] + inits$sel_inf_dev[2, fll_i, 1, idx],
    init_lt  = inits$sel_inf[3, fll_i, 1] + inits$sel_inf_dev[3, fll_i, 1, idx],
    asc      = inits$log_sel_slp[1, fll_i, 1] + inits$log_sel_slp_dev[1, fll_i, 1, idx],
    desc     = inits$log_sel_slp[2, fll_i, 1] + inits$log_sel_slp_dev[2, fll_i, 1, idx],
    topw     = inits$log_sel_slp[3, fll_i, 1] + inits$log_sel_slp_dev[3, fll_i, 1, idx])
})
colnames(fll_chk) <- as.character(fll_yrs_chk)
print(round(fll_chk, 3))

# Check Rce per-year sel_at_length for 1985 vs SS3 lendbase
fll_sel_pop <- cod_pcod_fixed$quantities$sel_at_length_pop
yr1985_idx <- which(years_hind == 1985)
cat(sprintf("\nFshLL sel_at_length_pop ranges for 1985 (idx=%d):\n", yr1985_idx))
fll_sel_85 <- as.numeric(fll_sel_pop[fll_i, 1, , yr1985_idx])
cat(sprintf("  min=%.4f  max=%.4f  L of max=%.1f\n",
            min(fll_sel_85), max(fll_sel_85),
            cod_pcod$lengths_pop[1, which.max(fll_sel_85)]))

# Detail print: worst year's per-bin obs/Rce_hat/SS3_hat
worst_yr <- per_yr_nll$yr[1]
rr <- fll_rows[which(comp_ctl_e[fll_rows, 5] == worst_yr)][1]
ss3_yr <- ss3_rep$lendbase %>%
  dplyr::filter(Fleet == fll_code, Yr == worst_yr, Sex == 1)
ss3_v <- ss3_yr$Exp[match(datlist$lbin_vector, ss3_yr$Bin)]
obs_p <- comp_obs_e[rr, 1:nlen_sp]
hat_p <- comp_hat_e[rr, 1:nlen_sp]
cat(sprintf("\nFshLL detail for worst year %d (N=%g):\n",
            worst_yr, comp_n_e[rr,2]))
print(data.frame(
  bin     = datlist$lbin_vector,
  obs_p   = signif(obs_p, 4),
  Rce_hat = signif(hat_p, 4),
  SS3_hat = signif(ss3_v, 4),
  ratio   = signif(hat_p / pmax(ss3_v, 1e-12), 4)
), row.names = FALSE)


# --- (0e.D) Index fit check: index_hat vs index_obs ------------------------
cat("\n=== (D) Index fit check: predicted vs observed ===\n")
idx_hat <- as.numeric(cod_pcod_fixed$quantities$index_hat)
ihat_resid_log <- log(pmax(idx_obs[,1], 1e-10)) - log(pmax(idx_hat, 1e-10))
ik <- which(idx_keep)
cat(sprintf("  n obs = %d   |  log-resid range: %.4f .. %.4f  mean abs: %.4f\n",
            length(ik), min(ihat_resid_log[ik]), max(ihat_resid_log[ik]),
            mean(abs(ihat_resid_log[ik]))))
# kernel = sum(0.5 * (resid + sigma^2/2)^2 / sigma^2)
ker_per_obs <- 0.5 * ((ihat_resid_log[ik] + idx_sig^2/2)/idx_sig)^2
cat(sprintf("  Sum kernel 0.5*((resid+sig^2/2)/sig)^2 = %.4f\n", sum(ker_per_obs)))


# --- (0c) Predicted length composition per fleet/year vs SS3 lendbase ------
# Rce comp_hat is a matrix [comp_ind, max_bins]; for length-comp rows the
# entries are predicted PROPORTIONS per data bin. SS3 lendbase$Exp gives
# the same. Compare per (fleet, year, bin).
cat(sprintf("\n[debug] fleet_control Comp_addtocomp values: %s\n",
            paste(cod_pcod$fleet_control$Comp_addtocomp, collapse=", ")))
cat("[debug] quantities$comp_addtocomp from TMB:\n")
print(cod_pcod_fixed$quantities$comp_addtocomp)
cat("[debug] quantities$caal_addtocomp from TMB:\n")
print(cod_pcod_fixed$quantities$caal_addtocomp)

cat("\n[Predicted length-comp comp_hat vs SS3 lendbase Exp]\n")
rce_comp_data <- if (!is.null(cod_pcod_fixed$data_list$comp_data)) {
  as.data.frame(cod_pcod_fixed$data_list$comp_data)
} else {
  cod_pcod$comp_data
}
# Only length-comp rows (Age0_Length1 == 1)
lcomp_rows <- which(rce_comp_data$Age0_Length1 == 1)
if (length(lcomp_rows) > 0) {
  ss3_data_lbins <- datlist$lbin_vector   # 4.5, 9.5, ..., 104.5
  for (fname in active_sel_fleets) {
    fi <- which(fleet_meta$name == fname); if (length(fi) == 0) next
    rows <- intersect(lcomp_rows,
                      which(rce_comp_data$Fleet_code == fleet_meta$ss3_num[fi] &
                            rce_comp_data$Year %in% years_hind))
    if (length(rows) == 0) next
    ss3_sub <- ss3_rep$lendbase %>%
      dplyr::filter(Fleet == fleet_meta$ss3_num[fi], Yr %in% years_hind, Sex == 1)
    if (nrow(ss3_sub) == 0) next
    # Walk per (year, bin) and gather all rel errs
    all_rel <- c()
    for (rr in rows) {
      yr  <- rce_comp_data$Year[rr]
      ss3_yr <- ss3_sub %>% dplyr::filter(Yr == yr)
      if (nrow(ss3_yr) == 0) next
      rce_vec <- as.numeric(cod_pcod_fixed$quantities$comp_hat[rr, 1:length(ss3_data_lbins)])
      ss3_vec <- ss3_yr$Exp[match(ss3_data_lbins, ss3_yr$Bin)]
      rel <- abs(rce_vec - ss3_vec) / pmax(abs(ss3_vec), 1e-6)
      all_rel <- c(all_rel, rel)
    }
    if (length(all_rel) > 0) {
      cat(sprintf("  %-9s n_obs*nbins=%d  max rel err %.2e  mean %.2e\n",
                  fname, length(all_rel), max(all_rel, na.rm = TRUE),
                  mean(all_rel, na.rm = TRUE)))
    }
  }
  # Detail print for FshTrawl 2023
  ft_i <- which(fleet_meta$name == "FshTrawl")
  ft_yr <- 2023
  ft_rows <- intersect(lcomp_rows,
                       which(rce_comp_data$Fleet_code == fleet_meta$ss3_num[ft_i] &
                             rce_comp_data$Year == ft_yr))
  if (length(ft_rows) > 0) {
    rr <- ft_rows[1]
    ss3_yr <- ss3_rep$lendbase %>%
      dplyr::filter(Fleet == fleet_meta$ss3_num[ft_i], Yr == ft_yr, Sex == 1)
    rce_vec <- as.numeric(cod_pcod_fixed$quantities$comp_hat[rr, 1:length(ss3_data_lbins)])
    ss3_vec <- ss3_yr$Exp[match(ss3_data_lbins, ss3_yr$Bin)]
    cat(sprintf("\n  FshTrawl 2023 detail (comp_ind=%d, Rce sum=%.5f, SS3 sum=%.5f):\n",
                rr, sum(rce_vec, na.rm=TRUE), sum(ss3_vec, na.rm=TRUE)))
    print(data.frame(Bin = ss3_data_lbins, Rce = signif(rce_vec, 4),
                     SS3 = signif(ss3_vec, 4),
                     ratio = signif(rce_vec/pmax(ss3_vec, 1e-10), 4)))
  }
}

# --- (0d) Conditional age-at-length (CAAL) per fleet/year/length vs SS3 ----
# Compare against Rce caal_hat (post-addtocomp, sum=1 per row). caal_hat is
# row-aligned with caal_data; match by (Fleet_code, Year, Length).
cat("\n[Predicted CAAL P(age|len) vs SS3 condbase Exp]\n")
rce_caal_data <- if (!is.null(cod_pcod_fixed$data_list$caal_data)) {
  as.data.frame(cod_pcod_fixed$data_list$caal_data)
} else cod_pcod$caal_data
if (!is.null(ss3_rep$condbase) && nrow(ss3_rep$condbase) > 0 &&
    !is.null(rce_caal_data) && nrow(rce_caal_data) > 0) {
  # SS3 condbase Lbin_lo is offset by -1 from raw dat (binwidth?); raw dat
  # uses data-bin left edges (9.5, 14.5, ...). Map ss3 Lbin_lo -> raw left
  # edge by adding 1.0.
  for (fname in active_sel_fleets) {
    fi <- which(fleet_meta$name == fname); if (length(fi) == 0) next
    ss3_sub <- ss3_rep$condbase %>%
      dplyr::filter(Fleet == fleet_meta$ss3_num[fi], Yr %in% years_hind, Sex == 1)
    if (nrow(ss3_sub) == 0) next
    all_rel <- c()
    yr_lbn <- ss3_sub %>% dplyr::distinct(Yr, Lbin_lo)
    for (k in seq_len(nrow(yr_lbn))) {
      yr <- yr_lbn$Yr[k]; lbl <- yr_lbn$Lbin_lo[k]
      # Map SS3 condbase Lbin_lo to Rce caal_data Length (the raw dat-file
      # left edge, which is lbl + 1).
      rce_length <- lbl + 1
      rce_idx <- which(rce_caal_data$Fleet_code == fleet_meta$ss3_num[fi] &
                       rce_caal_data$Year == yr &
                       abs(rce_caal_data$Length - rce_length) < 1e-6)
      if (length(rce_idx) == 0) next
      ci <- rce_idx[1]
      ch <- as.numeric(cod_pcod_fixed$quantities$caal_hat[ci, ])
      # Rce slot k = age (k-1) at minage=0. SS3 Bin k = age k.
      ss3_block <- ss3_sub %>% dplyr::filter(Yr == yr, Lbin_lo == lbl)
      ss3_block <- ss3_block[order(ss3_block$Bin), ]
      ss3_pred <- ss3_block$Exp[ss3_block$Bin <= nages_pcod - minage_pcod]
      if (length(ss3_pred) == 0) next
      # Compare Rce slots 2..(length(ss3_pred)+1) with SS3 Bin 1..length(ss3_pred)
      slot_lo <- 2L; slot_hi <- min(slot_lo + length(ss3_pred) - 1L, length(ch))
      rce_pred <- ch[slot_lo:slot_hi]
      n_cmp <- min(length(rce_pred), length(ss3_pred))
      rel <- abs(rce_pred[1:n_cmp] - ss3_pred[1:n_cmp]) /
             pmax(abs(ss3_pred[1:n_cmp]), 1e-6)
      all_rel <- c(all_rel, rel)
    }
    if (length(all_rel) > 0) {
      cat(sprintf("  %-9s n=%d cells  max rel err %.2e  mean %.2e\n",
                  fname, length(all_rel), max(all_rel, na.rm = TRUE),
                  mean(all_rel, na.rm = TRUE)))
    }
  }
  # Find the worst cell for Srv to understand the max
  fname_w <- "Srv"
  fi_w <- which(fleet_meta$name == fname_w)
  ss3_sub_w <- ss3_rep$condbase %>%
    dplyr::filter(Fleet == fleet_meta$ss3_num[fi_w], Yr %in% years_hind, Sex == 1)
  worst <- list(rel = 0, yr = NA, lbl = NA, age = NA, rce = NA, ss3 = NA)
  yr_lbn_w <- ss3_sub_w %>% dplyr::distinct(Yr, Lbin_lo)
  for (k in seq_len(nrow(yr_lbn_w))) {
    yr <- yr_lbn_w$Yr[k]; lbl <- yr_lbn_w$Lbin_lo[k]
    rce_length <- lbl + 1
    rce_idx <- which(rce_caal_data$Fleet_code == fleet_meta$ss3_num[fi_w] &
                     rce_caal_data$Year == yr &
                     abs(rce_caal_data$Length - rce_length) < 1e-6)
    if (length(rce_idx) == 0) next
    ci <- rce_idx[1]
    ch <- as.numeric(cod_pcod_fixed$quantities$caal_hat[ci, ])
    ss3_block <- ss3_sub_w %>% dplyr::filter(Yr == yr, Lbin_lo == lbl)
    ss3_block <- ss3_block[order(ss3_block$Bin), ]
    ss3_pred <- ss3_block$Exp[ss3_block$Bin <= nages_pcod - minage_pcod]
    if (length(ss3_pred) == 0) next
    slot_lo <- 2L; slot_hi <- min(slot_lo + length(ss3_pred) - 1L, length(ch))
    rce_pred <- ch[slot_lo:slot_hi]
    n_cmp <- min(length(rce_pred), length(ss3_pred))
    for (a in seq_len(n_cmp)) {
      rel_aa <- abs(rce_pred[a] - ss3_pred[a]) / max(abs(ss3_pred[a]), 1e-6)
      if (rel_aa > worst$rel) {
        worst <- list(rel = rel_aa, yr = yr, lbl = lbl, age = a,
                      rce = rce_pred[a], ss3 = ss3_pred[a])
      }
    }
  }
  if (worst$rel > 0) {
    cat(sprintf("\n  Worst Srv CAAL cell: yr=%d Lbin_lo=%g age=%d  Rce=%.4e SS3=%.4e rel=%.2e\n",
                worst$yr, worst$lbl, worst$age, worst$rce, worst$ss3, worst$rel))
  }
}


# --- Probe one Srv CAAL cell vs SS3 condbase ------------------------------
cat("\n[Srv 2007 CAAL detail vs SS3 condbase]\n")
ss3_srv <- ss3_rep$condbase %>%
  dplyr::filter(Fleet == 4, Yr == 2007, Sex == 1) %>%
  dplyr::distinct(Lbin_lo) %>% dplyr::pull(Lbin_lo)
caal_data_rce <- if (!is.null(cod_pcod_fixed$data_list$caal_data)) {
  as.data.frame(cod_pcod_fixed$data_list$caal_data)
} else cod_pcod$caal_data
# Find Rce caal_hat row for Srv 2007 first length
srv_rows <- which(caal_data_rce$Fleet_code == 4 & caal_data_rce$Year == 2007)
if (length(srv_rows) > 0) {
  cat(sprintf("Rce Srv 2007 caal_data rows: %d  first 3 Lengths: %s\n",
              length(srv_rows), paste(caal_data_rce$Length[srv_rows[1:min(3, length(srv_rows))]], collapse=",")))
  rr <- srv_rows[1]
  # Probe raw pred_CAAL for Srv (fleet 4) at year 2007, data bin index for L=9.5
  srv_i_d <- which(fleet_meta$name == "Srv")
  yi_07 <- which(years_hind == 2007)
  ln_idx <- 2  # data bin starting at 9.5 (0-based: 1, 1-based: 2)
  pc_age_vec <- as.numeric(cod_pcod_fixed$quantities$pred_CAAL[srv_i_d, 1, , ln_idx, yi_07])
  cat("Raw pred_CAAL[Srv, sex=1, age=0..10, ln=2 (L=9.5), yr=2007]:\n")
  print(data.frame(slot = 1:nages_pcod, age = 0:(nages_pcod - 1),
                   pred_CAAL = signif(pc_age_vec, 4)))
  cat(sprintf("Sum: %.4e\n", sum(pc_age_vec)))
  cat("Normalized (raw / sum):\n")
  print(signif(pc_age_vec / max(sum(pc_age_vec), 1e-30), 4))
  rce_vec <- as.numeric(cod_pcod_fixed$quantities$caal_hat[rr, 1:nages_pcod])
  ss3_one <- ss3_rep$condbase %>%
    dplyr::filter(Fleet == 4, Yr == 2007, Sex == 1, Lbin_lo == min(ss3_srv))
  ss3_one <- ss3_one[order(ss3_one$Bin), ]
  ss3_vec <- as.numeric(ss3_one$Exp)
  cat(sprintf("Rce Length=%g, SS3 Lbin_lo=%g  (Rce slot k+1 = SS3 Bin k = age k at minage=0)\n",
              caal_data_rce$Length[rr], min(ss3_srv)))
  cat("Per-age Rce vs SS3 (aligned):\n")
  n <- min(length(rce_vec) - 1, length(ss3_vec))
  print(data.frame(Age = 1:n,
                   Rce  = signif(rce_vec[2:(n+1)], 4),
                   SS3  = signif(ss3_vec[1:n], 4),
                   ratio = signif(rce_vec[2:(n+1)] / pmax(ss3_vec[1:n], 1e-12), 4)))
}


# --- Probe Srv index_hat decomposition for 2023 ----------------------------
# Srv/LLSrv are NUMBERS-based surveys (Weight1_Numbers2 = 2): the SS3 + Rce
# index_hat = sum_age N * exp(-Z*mo/12) * sel_at_age  (NO WAA multiplied).
cat("\n[Decompose Srv index_hat @ 2023 vs SS3]\n")
yr_probe <- 2023
yi <- which(years_hind == yr_probe)
srv_i <- which(fleet_meta$name == "Srv")
srv_flt <- fleet_meta$ss3_num[srv_i]
mo_srv <- cod_pcod$fleet_control$Month[srv_i] / 12
N_rce <- as.numeric(cod_pcod_fixed$quantities$N_at_age[1, 1, , yi])
Z_rce <- as.numeric(cod_pcod_fixed$quantities$Z_at_age[1, 1, , yi])
sel_rce <- as.numeric(cod_pcod_fixed$quantities$sel_at_age[srv_i, 1, , yi])
# Numbers-based: no W multiplication
vuln_rce <- sum(N_rce * exp(-Z_rce * mo_srv) * sel_rce)
q_rce <- cod_pcod_fixed$quantities$index_q[srv_i, yi]
ss3_exp <- ss3_rep$cpue$Exp[ss3_rep$cpue$Fleet == srv_flt & ss3_rep$cpue$Yr == yr_probe]
cat(sprintf("  Sum-product (Rce, no W): %.0f  | q*sum=%.0f  | SS3 Exp=%.0f\n",
            vuln_rce, q_rce * vuln_rce, ss3_exp))
cat(sprintf("  Rce Z(age 5)=%.4f | sel(age 5)=%.4f | exp(-Z*mo)(age 5)=%.4f\n",
            Z_rce[6], sel_rce[6], exp(-Z_rce[6] * mo_srv)))
# Per-age contribution to the sum (also: SS3 Asel2 to verify sel match)
ss3_asel <- ss3_rep$ageselex %>% dplyr::filter(Factor=="Asel2", Fleet==srv_flt, Yr==yr_probe)
if (nrow(ss3_asel) > 0) {
  ss3_sel_yr <- as.numeric(ss3_asel[1, age_cols_ss3])
  contrib <- N_rce * exp(-Z_rce * mo_srv) * sel_rce
  ss3_contrib <- N_rce * exp(-Z_rce * mo_srv) * ss3_sel_yr
  cat("  Per-age contribution to sum:\n")
  cat("    a    N     surv   sel_Rce  sel_SS3  contrib_Rce  contrib_SS3\n")
  for (a in 0:(nages_pcod - 1))
    cat(sprintf("    %2d  %.2e  %.3f  %.4f  %.4f   %.4g       %.4g\n",
                a, N_rce[a+1], exp(-Z_rce[a+1] * mo_srv),
                sel_rce[a+1], ss3_sel_yr[a+1],
                contrib[a+1], ss3_contrib[a+1]))
  cat(sprintf("  sum with Rce sel: %.0f  | sum with SS3 sel: %.0f\n",
              sum(contrib), sum(ss3_contrib)))
}


# --- (i) sel-at-length per fleet vs SS3 sizeselex Lsel (on POP grid) -------
# This is the quantity used in catch and index predictions. Compare on the
# FINE pop grid where SS3 reports Lsel.
cat("\n[Sel-at-length on POP grid vs SS3 Lsel]\n")
pop_mid <- 1:cod_pcod$nlengths_pop[1]   # SS3 reports as integer pop bin index
for (fname in active_sel_fleets) {
  i <- which(fleet_meta$name == fname); if (length(i) == 0) next
  ss3_l <- ss3_rep$sizeselex %>%
    dplyr::filter(Factor == "Lsel", Fleet == fleet_meta$ss3_num[i],
                  Yr == yr_last_int, Sex == 1)
  if (nrow(ss3_l) == 0) next
  lcols <- as.character(pop_mid)
  lcols <- lcols[lcols %in% colnames(ss3_l)]
  ss3_vec <- as.numeric(ss3_l[1, lcols])
  rce_vec <- as.numeric(cod_pcod_fixed$quantities$sel_at_length_pop[i, 1, , yr_last])
  n <- min(length(ss3_vec), length(rce_vec))
  abs_err <- abs(rce_vec[1:n] - ss3_vec[1:n])
  rel <- abs_err / pmax(abs(ss3_vec[1:n]), 1e-4)
  k <- which.max(rel)
  cat(sprintf("  %-9s max rel err %.2e  mean %.2e  | worst at L=%d: SS3=%.4e Rce=%.4e abs=%.2e\n",
              fname, max(rel), mean(rel), k, ss3_vec[k], rce_vec[k], abs_err[k]))
}

# --- (ii) Growth matrix (ALK) vs SS3 cohort length distribution -----------
# Already verified earlier: ALK row sums to 1, and the analytic formula on
# the pop grid matches SS3 Len_Beg/SD_Beg/Wt_Beg to machine precision for
# ages 1-9. The growth_matrix_pop is what convert_length_selectivity uses,
# so its parity == sel-at-length pop parity already verified above.
cat("\n[FshPot sel-at-length detail L=90..105 (Rce vs SS3)]\n")
fp_i <- which(fleet_meta$name == "FshPot")
fp_rce <- cod_pcod_fixed$quantities$sel_at_length_pop[fp_i, 1, , yr_last]
ss3_fp <- ss3_rep$sizeselex %>% dplyr::filter(Factor=="Lsel", Fleet==3, Yr==yr_last_int, Sex==1)
for (L in 90:105) {
  cat(sprintf("  L=%3d: Rce=%.5f SS3=%.5f\n", L, fp_rce[L], as.numeric(ss3_fp[1, as.character(L)])))
}

cat("\n[Effective sel params (base + dev) per fleet @ yr_last]\n")
for (fname in active_sel_fleets) {
  i <- which(fleet_meta$name == fname); if (length(i) == 0) next
  peak_eff   <- inits$sel_inf[1, i, 1] + inits$sel_inf_dev[1, i, 1, yr_last]
  final_eff  <- inits$sel_inf[2, i, 1] + inits$sel_inf_dev[2, i, 1, yr_last]
  init_eff   <- inits$sel_inf[3, i, 1] + inits$sel_inf_dev[3, i, 1, yr_last]
  asc_eff    <- inits$log_sel_slp[1, i, 1] + inits$log_sel_slp_dev[1, i, 1, yr_last]
  desc_eff   <- inits$log_sel_slp[2, i, 1] + inits$log_sel_slp_dev[2, i, 1, yr_last]
  topw_eff   <- inits$log_sel_slp[3, i, 1] + inits$log_sel_slp_dev[3, i, 1, yr_last]
  cat(sprintf("  %-9s peak=%.3f final_logit=%.3f init_logit=%.3f asc=%.3f desc=%.3f topw=%.3f\n",
              fname, peak_eff, final_eff, init_eff, asc_eff, desc_eff, topw_eff))
}

cat("\n[Growth matrix on POP grid: row sums + spot check]\n")
gm_pop <- cod_pcod_fixed$quantities$growth_matrix_pop
rs_age5 <- sum(gm_pop[1, 1, 6, , yr_last])
cat(sprintf("  Pop-slot age-5 row sum (should be 1): %.10f\n", rs_age5))
# Spot-check Wt_Beg implied by ALK + W-L params
W1 <- parlist$MG_parms["Wtlen_1_Fem_GP_1", "ESTIM"]
W2 <- parlist$MG_parms["Wtlen_2_Fem_GP_1", "ESTIM"]
pop_mid_cm <- cod_pcod$lengths_pop[1, ] + (cod_pcod$lengths_pop[1, 2] - cod_pcod$lengths_pop[1, 1]) / 2
W_age5_via_alk <- sum(gm_pop[1, 1, 6, , yr_last] * W1 * pop_mid_cm^W2)
W_age5_ss3     <- ss3_rep$endgrowth$Wt_Beg[ss3_rep$endgrowth$Sex == 1 &
                                          ss3_rep$endgrowth$int_Age == 5]
cat(sprintf("  Wt(age=5) via Rce pop ALK: %.6f  | SS3 Wt_Beg: %.6f  rel err: %.2e\n",
            W_age5_via_alk, W_age5_ss3, abs(W_age5_via_alk - W_age5_ss3) / W_age5_ss3))

# --- (iii) WAA per slot (pop, SSB, per-fleet) vs SS3 ---------------------
# Already comprehensively diagnosed in Section 9c below; here just the
# ex-plus-group summary.
cat("\n[WAA parity (ex plus-group; full per-age detail in section 9c)]\n")
yr_idx <- yr_last
eg <- ss3_rep$endgrowth %>% dplyr::filter(Sex == 1) %>% dplyr::arrange(int_Age)
slot_ages <- minage_pcod + seq_len(nages_pcod) - 1L
keep_idx <- match(slot_ages, eg$int_Age)
np <- nages_pcod - 1  # exclude plus group
ss3_W_beg <- eg$Wt_Beg[keep_idx]
rce_W_pop <- as.numeric(cod_pcod_fixed$quantities$weight_hat[1, 1, , yr_idx])
rel_pop <- abs(rce_W_pop[1:np] - ss3_W_beg[1:np]) / pmax(abs(ss3_W_beg[1:np]), 1e-10)
cat(sprintf("  WAA pop  slot: max %.2e mean %.2e\n", max(rel_pop), mean(rel_pop)))
ss3_W_mid <- eg$Wt_Mid[keep_idx]
for (i in seq_len(nrow(fleet_meta))) {
  if (fleet_meta$fleet_type[i] == "Off") next
  rce <- as.numeric(cod_pcod_fixed$quantities$weight_hat[2 + i, 1, , yr_idx])
  # SS3 reports mid-year WAA in endgrowth Wt_Mid; for fleets at Month=6
  # (mid-year) Rceattle should match Wt_Mid.
  rel <- abs(rce[1:np] - ss3_W_mid[1:np]) / pmax(abs(ss3_W_mid[1:np]), 1e-10)
  cat(sprintf("  WAA fleet %-9s (Month=%d) vs SS3 Wt_Mid: max %.2e mean %.2e\n",
              fleet_meta$name[i], cod_pcod$fleet_control$Month[i], max(rel), mean(rel)))
}


# Diagnostic: are the pop arrays populated and being used by convert_length_selectivity?
cat("\n=== POP-array diagnostics (forward pass, FshTrawl) ===\n")
cat("sel_at_length     shape:", paste(dim(cod_pcod_fixed$quantities$sel_at_length), collapse="x"), "\n")
cat("sel_at_length_pop shape:", paste(dim(cod_pcod_fixed$quantities$sel_at_length_pop), collapse="x"), "\n")
cat("growth_matrix     shape:", paste(dim(cod_pcod_fixed$quantities$growth_matrix), collapse="x"), "\n")
cat("growth_matrix_pop shape:", paste(dim(cod_pcod_fixed$quantities$growth_matrix_pop), collapse="x"), "\n")
cat("FshTrawl sel_at_length     range (yr 2024):",
    range(cod_pcod_fixed$quantities$sel_at_length[1, 1, , length(years_hind)]), "\n")
cat("FshTrawl sel_at_length_pop range (yr 2024):",
    range(cod_pcod_fixed$quantities$sel_at_length_pop[1, 1, , length(years_hind)]), "\n")
# Spot-check sel_at_length_pop at L=55 (= pop bin 55) for FshTrawl 2024
cat("FshTrawl sel_at_length_pop[L=55] (yr 2024):",
    cod_pcod_fixed$quantities$sel_at_length_pop[1, 1, 55, length(years_hind)], "\n")
cat("FshTrawl growth_matrix_pop sum-over-pop (age 5, yr 2024):",
    sum(cod_pcod_fixed$quantities$growth_matrix_pop[1, 1, 6, , length(years_hind)]), "\n")

# Manual convolution: should match sel_at_age[FshTrawl, 1, 6, last_yr]
sel_p <- cod_pcod_fixed$quantities$sel_at_length_pop[1, 1, , length(years_hind)]
alk_p <- cod_pcod_fixed$quantities$growth_matrix_pop[3, 1, 6, , length(years_hind)]  # fleet 1 weight slot = 3 (=2+1)
sel_age_manual <- sum(sel_p * alk_p)
sel_age_rce    <- cod_pcod_fixed$quantities$sel_at_age[1, 1, 6, length(years_hind)]
cat(sprintf("FshTrawl manual conv: %.5f  vs sel_at_age from C++: %.5f  diff: %+.2e\n",
            sel_age_manual, sel_age_rce, sel_age_manual - sel_age_rce))
# Try with pop slot (wt index 1) too
alk_pop1 <- cod_pcod_fixed$quantities$growth_matrix_pop[1, 1, 6, , length(years_hind)]
cat(sprintf("FshTrawl manual conv with pop wt slot: %.5f\n", sum(sel_p * alk_pop1)))

# Probe length_hat for FshTrawl fleet wt slot (wt=3) vs SS3 endgrowth Len_Beg
yr_last <- length(years_hind)
cat("FshTrawl length_hat (wt=3, age 5, yr_last):",
    cod_pcod_fixed$quantities$length_hat[3, 1, 6, yr_last], "  (SS3 Len_Beg=57.35, Len_Mid=61.17)\n")
# Where does the ALK peak? Find the pop bin with max prob for age 5
alk_age5 <- cod_pcod_fixed$quantities$growth_matrix_pop[3, 1, 6, , yr_last]
peak_bin <- which.max(alk_age5)
cat(sprintf("FshTrawl ALK age 5 peaks at pop bin %d (~L=%g), prob=%.4f\n",
            peak_bin, cod_pcod$lengths_pop[1, peak_bin] + 0.5, alk_age5[peak_bin]))
# Compute mean LAA from the ALK
mean_LAA_from_alk <- sum(alk_age5 * (cod_pcod$lengths_pop[1, ] + 0.5))
cat(sprintf("FshTrawl mean LAA(age 5) from growth_matrix_pop: %.3f\n", mean_LAA_from_alk))


# Detail probe: per-age comparison for each active fleet in last year, so we
# can see which age contributes the max rel err (peak vs tails differ a lot).
ny_last <- length(years_hind)
yr_last <- tail(years_hind, 1)
for (fname in active_sel_fleets) {
  i <- which(fleet_meta$name == fname)
  if (length(i) == 0) next
  ss3_num <- fleet_meta$ss3_num[i]
  ss3_sub <- ss3_rep$ageselex %>%
    dplyr::filter(Factor == "Asel2", Fleet == ss3_num, Yr <= yr_last) %>%
    dplyr::arrange(dplyr::desc(Yr))
  if (nrow(ss3_sub) == 0) next
  ss3_vec <- as.numeric(ss3_sub[1, age_cols_ss3])
  rce_vec <- as.numeric(cod_pcod_fixed$quantities$sel_at_age[i, 1, , ny_last])
  cat(sprintf("\n  -- %s sel-at-age @ %d --\n", fname, yr_last))
  print(data.frame(Age = 0:(nages_pcod - 1),
                   SS3 = round(ss3_vec, 5),
                   Rce = round(rce_vec, 5),
                   AbsDiff = signif(rce_vec - ss3_vec, 3)))
}

# Detail print: Lsel for LLSrv (no blocks) in last year -- cleanest comparison
llsrv_i <- which(fleet_meta$name == "LLSrv")
if (length(llsrv_i) == 1) {
  ny  <- length(years_hind)
  yr_last <- tail(years_hind, 1)
  ss3_l <- ss3_rep$sizeselex %>%
    dplyr::filter(Factor == "Lsel", Fleet == fleet_meta$ss3_num[llsrv_i],
                  Yr == yr_last, Sex == 1)
  if (nrow(ss3_l) > 0) {
    lcols <- as.character(ss3_len_bins)
    lcols <- lcols[lcols %in% colnames(ss3_l)]
    if (length(lcols) > 0) {
      cat(sprintf("\n--- LLSrv Lsel at %d (Rce vs SS3) ---\n", yr_last))
      rce_vec <- as.numeric(cod_pcod_fixed$quantities$sel_at_length[llsrv_i, 1, , ny])
      ss3_vec <- as.numeric(ss3_l[1, lcols])
      n <- min(length(rce_vec), length(ss3_vec), length(lcols))
      print(data.frame(
        Length = as.numeric(lcols)[1:n],
        Rce    = round(rce_vec[1:n], 4),
        SS3    = round(ss3_vec[1:n], 4)
      ))
    } else {
      cat("\n--- LLSrv Lsel: no matching length-bin columns in SS3 output (skip) ---\n")
    }
  }
}


# =============================================================================
# 9b. GROWTH-OUTPUT comparison vs SS3 (forward-pass, SS3 params injected)
#
# Compares Rceattle's parametric VB outputs against SS3 endgrowth:
#   (i)   length-at-age (Len_Beg / Len_Mid)
#   (ii)  weight-at-age for pop slot vs SS3 Wt_Beg
#   (iii) weight-at-age for SSB slot vs SS3 Wt_Mid (spawn_month = 0 here, so
#         SSB-slot WAA should equal pop-slot)
#   (iv)  per-fleet weight-at-age vs SS3 endgrowth SelWt:_<flt>
#   (v)   growth transition matrix (ALK) row sums and a spot-check vs the
#         analytic VB+SD distribution on the SS3 1cm pop grid
#
# Rceattle quantity layout (R, 1-indexed):
#   weight_hat[1, sex, age, yr]  = pop slot 1   (= SS3 Wt_Beg)
#   weight_hat[2, sex, age, yr]  = SSB slot     (= SS3 Wt_Mid at spawn fracyr)
#   weight_hat[2+i, sex, age, yr] = fleet i WAA
#   length_hat same layout
#   growth_matrix[wt, sex, age, length_bin, yr]
# =============================================================================
cat("\n=== Growth-output comparison vs SS3 (forward-pass) ===\n")

# Reference year for endgrowth comparison: SS3 endgrowth is annual / time-
# invariant for this model so any hindcast year works; use the last hindcast
# year to match what most users care about.
yr_idx <- length(years_hind)
eg <- ss3_rep$endgrowth %>% dplyr::filter(Sex == 1) %>% dplyr::arrange(int_Age)
eg_ages <- eg$int_Age
slot_ages <- minage_pcod + seq_len(nages_pcod) - 1L  # 0..nages-1 at minage=0
keep_idx <- match(slot_ages, eg_ages)

# Helper: print a per-age comparison + per-age and aggregate rel err.
# Reports ex-plus-group rel err too -- the legacy static plus-group length
# correction (Stage D in the SS3 parity work, not yet replaced with SS3-style
# dynamic N-weighting) drives the all-ages max but doesn't reflect the rest
# of the curve.
compare_at_age <- function(rce_vec, ss3_vec, label, age_labels) {
  rel <- abs(rce_vec - ss3_vec) / pmax(abs(ss3_vec), 1e-10)
  n   <- length(rel)
  rel_np <- if (n > 1) rel[-n] else rel
  cat(sprintf("\n[%s] all-ages max %.2e mean %.2e | ex-plus max %.2e mean %.2e\n",
              label,
              max(rel, na.rm = TRUE), mean(rel, na.rm = TRUE),
              max(rel_np, na.rm = TRUE), mean(rel_np, na.rm = TRUE)))
  df <- data.frame(Age = age_labels,
                   SS3 = signif(ss3_vec, 6),
                   Rce = signif(rce_vec, 6),
                   RelErr = signif(rel, 3))
  print(df, row.names = FALSE)
  invisible(rel)
}

# --- (i) Length-at-age (pop slot) vs SS3 Len_Beg --------------------------
rce_LAA_pop <- as.numeric(cod_pcod_fixed$quantities$length_hat[1, 1, , yr_idx])
ss3_LAA_pop <- eg$Len_Beg[keep_idx]
compare_at_age(rce_LAA_pop, ss3_LAA_pop, "LAA pop slot (vs SS3 Len_Beg)", slot_ages)

rce_LAA_ssb <- as.numeric(cod_pcod_fixed$quantities$length_hat[2, 1, , yr_idx])
ss3_LAA_ssb <- eg$Len_Mid[keep_idx]
# spawn_month = 0 means Rceattle fracyr = 0, so SSB-slot LAA should match pop-slot,
# NOT SS3's Len_Mid (which is at fracyr=0.5). Show both so user can see what
# Rceattle's spawn_month is producing.
compare_at_age(rce_LAA_ssb, ss3_LAA_pop,
               sprintf("LAA SSB slot (spawn_month=%g) vs SS3 Len_Beg",
                       cod_pcod$spawn_month[1]),
               slot_ages)

# --- (ii) WAA pop slot vs SS3 Wt_Beg --------------------------------------
rce_WAA_pop <- as.numeric(cod_pcod_fixed$quantities$weight_hat[1, 1, , yr_idx])
ss3_WAA_pop <- eg$Wt_Beg[keep_idx]
compare_at_age(rce_WAA_pop, ss3_WAA_pop, "WAA pop slot (vs SS3 Wt_Beg)", slot_ages)

# --- (iii) WAA SSB slot vs SS3 Wt_Beg (since spawn_month=0) ---------------
rce_WAA_ssb <- as.numeric(cod_pcod_fixed$quantities$weight_hat[2, 1, , yr_idx])
compare_at_age(rce_WAA_ssb, ss3_WAA_pop,
               sprintf("WAA SSB slot (spawn_month=%g) vs SS3 Wt_Beg",
                       cod_pcod$spawn_month[1]),
               slot_ages)

# --- (iv) Per-fleet WAA vs SS3 SelWt:_<flt> -------------------------------
# SS3 endgrowth has SelWt:_<n> columns = weight-at-age weighted by selectivity.
# But what we really want for the Rceattle weight slot is mean weight-at-age
# at the fleet's time of operation (NOT sel-weighted). SS3 reports this less
# directly; the closest is Wt_Mid (mid-year) if the fleet is mid-year. Per
# growth.hpp::estimate_growth_within_yr, Rceattle advances pop-slot LAA by
# fracyr = Month/12 of VB growth, then integrates with the SD at that LAA.
# Comparison: rebuild what SS3 would report at each fleet's modal Month using
# the SS3 ESTIM params, and compare to Rceattle's weight_hat for that fleet.
cat("\n--- Per-fleet WAA timing ---\n")
# Extract SS3 growth ESTIM from parlist (so we don't double-pay for typos)
K_ss3    <- parlist$MG_parms["VonBert_K_Fem_GP_1", "ESTIM"]
L1_ss3   <- parlist$MG_parms["L_at_Amin_Fem_GP_1", "ESTIM"]
Linf_ss3 <- parlist$MG_parms["L_at_Amax_Fem_GP_1", "ESTIM"]
SDy_ss3  <- parlist$MG_parms["CV_young_Fem_GP_1", "ESTIM"]
SDo_ss3  <- parlist$MG_parms["CV_old_Fem_GP_1", "ESTIM"]
alpha_ss3 <- parlist$MG_parms["Wtlen_1_Fem_GP_1", "ESTIM"]
beta_ss3  <- parlist$MG_parms["Wtlen_2_Fem_GP_1", "ESTIM"]
gal1_ss3  <- ctllist$Growth_Age_for_L1 %||% 0.5
# Pop length bins (1cm here): generated from binwidth/min/max in the SS3 dat
pop_bw  <- datlist$binwidth %||% 1
pop_min <- datlist$minimum_size %||% 0.5
pop_max <- datlist$maximum_size %||% 104.5
pop_edges <- seq(pop_min, pop_max, by = pop_bw)
pop_mid   <- pop_edges + pop_bw / 2
n_pop     <- length(pop_edges)

ss3_LAA_at <- function(a) ifelse(a <= gal1_ss3,
                                 pop_min + (L1_ss3 - pop_min) * (a / gal1_ss3),
                                 Linf_ss3 - (Linf_ss3 - L1_ss3) *
                                   exp(-K_ss3 * (a - gal1_ss3)))
ss3_SD_at  <- function(L) ifelse(L <= L1_ss3, SDy_ss3,
                                 SDy_ss3 + (SDo_ss3 - SDy_ss3) *
                                   (L - L1_ss3) / (Linf_ss3 - L1_ss3))
ss3_WAA_at <- function(a) {
  L <- ss3_LAA_at(a); SD <- ss3_SD_at(L)
  prob <- numeric(n_pop)
  for (k in seq_len(n_pop)) {
    if (k == 1)        prob[k] <- pnorm(pop_edges[k + 1], L, SD)
    else if (k == n_pop) prob[k] <- 1 - pnorm(pop_edges[k], L, SD)
    else               prob[k] <- pnorm(pop_edges[k + 1], L, SD) -
                                   pnorm(pop_edges[k], L, SD)
  }
  sum(prob * alpha_ss3 * pop_mid^beta_ss3)
}

# For each active fleet, get its Month and rebuild SS3 reference WAA at that
# within-year offset (slot age = integer_age + Month/12).
# Report both whole-axis and ex-plus-group rel err so the legacy static plus-
# group correction (Stage D, intentionally not yet replaced) doesn't mask
# the rest of the curve. The plus group lives at slot nages.
plus_slot <- nages_pcod
for (i in seq_len(nrow(fleet_meta))) {
  if (fleet_meta$fleet_type[i] == "Off") next
  flt_month <- cod_pcod$fleet_control$Month[i]
  fracyr    <- flt_month / 12
  rce_WAA_flt <- as.numeric(cod_pcod_fixed$quantities$weight_hat[2 + i, 1, , yr_idx])
  ss3_WAA_flt_ref <- vapply(slot_ages, function(a) ss3_WAA_at(a + fracyr), numeric(1))
  rel <- abs(rce_WAA_flt - ss3_WAA_flt_ref) / pmax(abs(ss3_WAA_flt_ref), 1e-10)
  rel_no_plus <- rel[-plus_slot]
  cat(sprintf("  Fleet %-9s (Month=%d): all-ages max %.2e mean %.2e | ex-plus max %.2e mean %.2e\n",
              fleet_meta$name[i], flt_month,
              max(rel), mean(rel),
              max(rel_no_plus), mean(rel_no_plus)))
}

# --- (v) Growth matrix (ALK) row-sum and spot-check -----------------------
# growth_matrix dims: [wtind, sex, age, length, yr]. Rows (over length bins
# per age) should sum to ~1.
cat("\n--- Growth-matrix sanity ---\n")
gm <- cod_pcod_fixed$quantities$growth_matrix
gm_shape <- dim(gm)
cat(sprintf("  shape (wtind, sex, age, length, yr) = %s\n",
            paste(gm_shape, collapse = " x ")))
gm_pop_last <- gm[1, 1, , , yr_idx]   # [age, length]
row_sums <- rowSums(gm_pop_last)
cat(sprintf("  Pop-slot ALK row sums (should be ~1): min=%.6f max=%.6f\n",
            min(row_sums), max(row_sums)))

# Spot-check ALK row for age 5 vs analytic distribution on Rceattle's grid
rce_age5_alk <- gm_pop_last[6, ]   # age 5 = slot 6 at minage=0
# Rceattle uses data-bin grid; rebuild analytic ALK on the SAME grid for fair
# comparison
data_edges <- datlist$lbin_vector   # 4.5, 9.5, ...
n_data <- length(data_edges)
L5_pred  <- ss3_LAA_at(5); SD5_pred <- ss3_SD_at(L5_pred)
analytic_data_alk <- numeric(n_data)
for (k in seq_len(n_data)) {
  if (k == 1) analytic_data_alk[k] <-
    pnorm(data_edges[1] + (data_edges[2] - data_edges[1]) - L5_pred, sd = SD5_pred)
  else if (k == n_data) analytic_data_alk[k] <-
    1 - pnorm(data_edges[k] - L5_pred, sd = SD5_pred)
  else analytic_data_alk[k] <-
    pnorm(data_edges[k + 1] - L5_pred, sd = SD5_pred) -
    pnorm(data_edges[k] - L5_pred, sd = SD5_pred)
}
rel_alk <- abs(rce_age5_alk - analytic_data_alk) / pmax(abs(analytic_data_alk), 1e-10)
cat(sprintf("  Age-5 ALK on data grid: max rel err vs analytic %.3e\n", max(rel_alk)))
cat("  (Both should match each other since Rceattle integrates on data bins;\n")
cat("   the SS3 mismatch is independent and lives in the choice of bin grid.)\n")


# Stop here while we validate parametric Length-DoubleNormal selectivity --
# estimation would currently free the IID sel devs (no map fix yet), which is
# not what we want for an SS3-pinned comparison. Re-enable after sel match.
if (FALSE) {
  cat("\n[stop] Estimation section skipped during sel-parametric validation.\n")
  quit(save = "no", status = 0)
}


# =============================================================================
quit(save="no", status=0)
# 10. Full-MLE estimation (start from SS3 values, optimize)
# =============================================================================
# Estimate everything Rceattle has an SS3 analog for: log_R0, rec_dev, init_dev,
# log_M1, beta_linkage, index_log_q. Selectivity stays "Fixed" via the emp_sel
# Asel2 injection (matches SS3's realized sel by construction). Growth stays
# empirical (WAA fixed at SS3 endgrowth Wt_Beg + Jensen's-gap correction).
# Starting from SS3's MLE means the estimator should stay near it if Rceattle's
# likelihood is structurally compatible with SS3's.
# CAAL_weights = 1 (default). Previously downweighted by 1/45 because the
# CAAL likelihood was misaligned (formula + off-by-one age bug). Both fixed
# now: SS3Robust kernel matches SS3 exactly, and the CAAL age columns are
# aligned (build_caal_data leading-zero pad). So no need to downweight.
cod_pcod$fleet_control$CAAL_weights <- 1

# IMPORTANT: turn the sel-dev sentinel OFF for estimation. The Phase 1
# forward-pass sets Time_varying_sel_sd_prior = -1 (sentinel -> skip the
# N(0, sigma) prior) because injected per-year sel values aren't true
# deviates. But for ESTIMATION, freely-estimated sel devs need the
# regularizing prior; without it the optimizer wanders and the model
# struggles to converge. Restore a sensible positive sigma here.
active_fi <- which(cod_pcod$fleet_control$Fleet_name %in% active_sel_fleets)
cod_pcod$fleet_control$Time_varying_sel_sd_prior[active_fi] <- 1.0
cat(sprintf("\n[estimation] Reset Time_varying_sel_sd_prior to 1.0 for %d active fleets (sentinel off)\n",
            length(active_fi)))

cat("\n--- Full MLE estimation (PHASED, VB growth + K/Linf priors) ---\n")
# Estimation uses growthFun_est_spec (linkages + SS3 priors on K, L1, Linf).
# Forward-pass uses growthFun_spec (no linkages) to avoid the linkage
# system interfering with our direct log_growth_pars injection. So `inits`
# (from forward-pass) doesn't have beta_linkage entries -- let fit_mod
# build its own start from the linkage_spec init values, which now encode
# SS3's K/L1/Linf at the natural scale.
cod_pcod_est <- Rceattle::fit_mod(
  data_list    = cod_pcod,
  estimateMode = 1,                     # hindcast only (no projection)
  initMode     = 2,
  growthFun    = growthFun_est_spec,    # with SS3 priors on K / Linf
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  fit_control  = fit_control(phase = TRUE, verbose = 1)
)

# --- 10a. Time-series comparison (R, Bio, SSB) ---
ny <- length(years_hind)
bio_est <- as.numeric(cod_pcod_est$quantities$biomass[1, 1:ny])
ssb_est <- as.numeric(cod_pcod_est$quantities$ssb[1, 1:ny])
R_est   <- as.numeric(cod_pcod_est$quantities$R[1, 1:ny])

cat("\n=== Estimation: relative errors vs SS3 ===\n")
diag_errors(R_est,   ss3_R,   "R")
diag_errors(bio_est, ss3_bio, "Bio")
diag_errors(ssb_est, ss3_ssb, "SSB")

cat("\n=== TS head + tail (estimation) ===\n")
print(data.frame(
  Year      = c(head(years_hind, 5), tail(years_hind, 5)),
  Bio_SS3   = c(head(ss3_bio, 5), tail(ss3_bio, 5)),
  Bio_Est   = c(head(bio_est, 5), tail(bio_est, 5)),
  SSB_SS3   = c(head(ss3_ssb, 5), tail(ss3_ssb, 5)),
  SSB_Est   = c(head(ssb_est, 5), tail(ssb_est, 5)),
  R_SS3     = c(head(ss3_R, 5),   tail(ss3_R, 5)),
  R_Est     = c(head(R_est, 5),   tail(R_est, 5))
))

# --- 10a.2 Hessian diagnostic ---------------------------------------------
# When TMBhelper reports the Hessian as non-pos-def, the offending params
# are usually (a) at a boundary, (b) unidentifiable (zero gradient and
# zero column in Hessian -> infinite SE), or (c) on a flat ridge with a
# tightly-correlated partner. Walk all three checks.
cat("\n=== Hessian diagnostic ===\n")
obj_est  <- cod_pcod_est$obj
opt_par  <- cod_pcod_est$opt$par
if (is.null(opt_par)) opt_par <- obj_est$env$last.par.best
par_names <- names(opt_par)

g <- as.numeric(obj_est$gr(opt_par))
cat(sprintf("Final par length: %d\n", length(opt_par)))
cat(sprintf("Max abs gradient: %.3e  (idx %d, name=%s)\n",
            max(abs(g)), which.max(abs(g)),
            if (!is.null(par_names)) par_names[which.max(abs(g))] else "?"))
cat(sprintf("Mean abs gradient: %.3e\n", mean(abs(g))))
cat(sprintf("Number of params with |grad| > 1e-3: %d\n", sum(abs(g) > 1e-3)))

# Top 10 worst gradients
cat("\n--- Top 10 worst gradients (poorly converged params) ---\n")
ord_g <- order(abs(g), decreasing = TRUE)[1:10]
for (j in ord_g) {
  nm <- if (!is.null(par_names)) par_names[j] else paste0("par_", j)
  cat(sprintf("  %-30s grad=%9.3e  value=%9.4f\n",
              nm, g[j], opt_par[j]))
}

# Hessian eigen-decomposition
cat("\n--- Hessian eigenvalue analysis ---\n")
H <- tryCatch(obj_est$he(opt_par), error = function(e) NULL)
if (is.null(H)) {
  cat("obj$he() failed; using numDeriv::jacobian(gr).\n")
  H <- tryCatch(numDeriv::jacobian(obj_est$gr, opt_par), error = function(e) NULL)
}
if (!is.null(H)) {
  H <- (H + t(H)) / 2
  # Surface NaN/Inf in the Hessian BEFORE the eigen call -- these point at
  # params that hit numerical pathology during the gradient sweep.
  bad_rows <- which(!is.finite(rowSums(H)))
  cat(sprintf("Hessian rows with NaN/Inf: %d\n", length(bad_rows)))
  if (length(bad_rows) > 0 && !is.null(par_names)) {
    bad_fams <- table(par_names[bad_rows])
    cat("  Bad-row families (top 15):\n")
    print(head(sort(bad_fams, decreasing = TRUE), 15))
  }
  # Drop NaN rows/cols so eigen() can run on the clean submatrix
  good <- which(is.finite(rowSums(H)) & is.finite(colSums(H)))
  if (length(good) < length(opt_par)) {
    cat(sprintf("Restricting eigen analysis to %d/%d clean rows\n",
                length(good), length(opt_par)))
    H <- H[good, good, drop = FALSE]
    g_sub <- g[good]
    par_names_sub <- if (!is.null(par_names)) par_names[good] else NULL
    opt_par_sub <- opt_par[good]
  } else {
    g_sub <- g
    par_names_sub <- par_names
    opt_par_sub <- opt_par
  }
  eig <- eigen(H, symmetric = TRUE)
  cat(sprintf("Hessian shape: %d x %d\n", nrow(H), ncol(H)))
  cat(sprintf("Eigenvalue range: %.3e .. %.3e\n",
              min(eig$values), max(eig$values)))
  cat(sprintf("Non-positive eigenvalues: %d\n", sum(eig$values <= 0)))
  cat(sprintf("Near-zero (|eig| < 1e-8): %d\n", sum(abs(eig$values) < 1e-8)))

  bad_eig <- which(eig$values < 1e-8)
  if (length(bad_eig) > 0) {
    cat(sprintf("\n--- Top loadings on %d non-pd eigenvectors ---\n",
                min(5, length(bad_eig))))
    for (k in head(bad_eig, 5)) {
      v <- abs(eig$vectors[, k])
      top_idx <- order(v, decreasing = TRUE)[1:5]
      cat(sprintf("\n  eig[%d] = %.3e | top contributing params:\n", k, eig$values[k]))
      for (j in top_idx) {
        nm <- if (!is.null(par_names_sub)) par_names_sub[j] else paste0("par_", j)
        cat(sprintf("    %-30s loading=%.4f  value=%9.4f  grad=%9.3e\n",
                    nm, v[j], opt_par_sub[j], g_sub[j]))
      }
    }
  }

  # Params with smallest |diag(H)| = weakest individual information
  diagH <- diag(H)
  cat("\n--- 15 params with smallest |Hessian diagonal| (weakest identification) ---\n")
  ord <- order(abs(diagH))[1:min(15, length(diagH))]
  for (j in ord) {
    nm <- if (!is.null(par_names_sub)) par_names_sub[j] else paste0("par_", j)
    cat(sprintf("  %-30s diag=%9.3e  value=%9.4f  grad=%9.3e\n",
                nm, diagH[j], opt_par_sub[j], g_sub[j]))
  }

  # Group by param name prefix to see WHICH parameter families are weakest
  if (!is.null(par_names)) {
    family <- sub("\\.\\d.*$", "", par_names)  # strip trailing .1, .2 indices
    fam_summary <- data.frame(
      family = family, diag = abs(diagH), grad = abs(g))
    by_fam <- aggregate(cbind(diag, grad) ~ family, fam_summary,
                        FUN = function(x) c(min = min(x), max = max(x),
                                            mean = mean(x), n = length(x)))
    cat("\n--- Per-family Hessian diag + gradient summary ---\n")
    fam_tbl <- do.call(rbind, lapply(seq_len(nrow(by_fam)), function(i) {
      data.frame(family = by_fam$family[i],
                 n = by_fam$diag[i, "n"],
                 diag_min = by_fam$diag[i, "min"],
                 diag_mean = by_fam$diag[i, "mean"],
                 grad_max = by_fam$grad[i, "max"])
    }))
    fam_tbl <- fam_tbl[order(fam_tbl$diag_min), ]
    print(fam_tbl, row.names = FALSE, digits = 3)
  }
} else {
  cat("Hessian unavailable.\n")
}


# --- 10b. Selectivity-at-age comparison ---
cat("\n=== Selectivity-at-age vs SS3 Asel2 (post-estimation) ===\n")
age_cols_ss3 <- as.character(0:(nages_pcod - 1))
sel_compare <- list()
for (i in seq_len(nrow(fleet_meta))) {
  ss3_num <- fleet_meta$ss3_num[i]
  ss3_sub <- ss3_rep$ageselex %>%
    dplyr::filter(Factor == "Asel2", Fleet == ss3_num, Yr %in% years_hind)
  # Forward-fill (SS3 emits sparse rows on block-change years)
  for (yi in seq_along(years_hind)) {
    yr <- years_hind[yi]
    rows_le <- ss3_sub %>% dplyr::filter(Yr <= yr) %>% dplyr::arrange(dplyr::desc(Yr))
    if (nrow(rows_le) == 0) next
    ss3_vec <- as.numeric(rows_le[1, age_cols_ss3])
    rce_vec <- as.numeric(cod_pcod_est$quantities$sel_at_age[i, 1, , yi])
    rel <- abs(rce_vec - ss3_vec) / pmax(abs(ss3_vec), 1e-10)
    sel_compare[[length(sel_compare) + 1]] <- data.frame(
      Fleet = fleet_meta$name[i], Year = yr, MaxRelErr = max(rel),
      MeanRelErr = mean(rel)
    )
  }
}
sel_compare <- do.call(rbind, sel_compare)
print(sel_compare %>% dplyr::group_by(Fleet) %>%
        dplyr::summarise(max_rel = max(MaxRelErr),
                         mean_rel = mean(MeanRelErr)))

# --- 10c. Likelihood components ---
cat("\n=== Likelihood component decomposition ===\n")
# Rceattle: jnll_comp is [component_idx, species]; convert to a labeled vector
jnll <- cod_pcod_est$quantities$jnll_comp
comp_labels <- c("1.Index", "2.Catch", "3.MargComp", "4.CAAL",
                 "5.SelPen", "6.SelDev_RE", "7-9.QPriorSRR", "8.Q?",
                 "9.SRR_prior?", "10.Init_dev", "11.Rec_dev", "12.R_vs_Rhat",
                 "13.FBRP", "14.zeroN", "15.M1prior", "16.M_RE",
                 "17.Ration", "18.Stom", "19.Stom2", "20.ParPrior")
n_comp <- nrow(jnll)
comp_labels <- comp_labels[1:n_comp]
if (length(comp_labels) < n_comp)
  comp_labels <- c(comp_labels, paste0("comp_", (length(comp_labels)+1):n_comp))
rce_nll <- data.frame(Component = comp_labels, NLL = round(jnll[, 1], 4))
print(rce_nll)
cat(sprintf("\nTotal Rceattle NLL: %.4f\n", sum(jnll[, 1])))

# SS3 likelihood (from Report.sso)
if (!is.null(ss3_rep$likelihoods_used)) {
  cat("\nSS3 likelihood components (Report.sso 'likelihoods_used'):\n")
  print(ss3_rep$likelihoods_used)
}
if (!is.null(ss3_rep$likelihoods_by_fleet)) {
  cat("\nSS3 likelihood by fleet:\n")
  print(head(ss3_rep$likelihoods_by_fleet, 20))
}
