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
# Bin_first_selected = 1 zeros sel_at_age[0] post-convolution.
if (!"Bin_first_selected" %in% colnames(cod_pcod$fleet_control)) {
  cod_pcod$fleet_control$Bin_first_selected <- 0L  # default no floor
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
  # Setting Time_varying_sel_sd <= 0 tells the cpp to SKIP the N(0,σ)
  # penalty on the deviates (Phase 1 forward-pass: devs are pre-baked from
  # SS3, not estimated, so no prior should fire).
  cod_pcod$fleet_control$Time_varying_sel[fi]          <- "IID"
  cod_pcod$fleet_control$Time_varying_sel_sd[fi] <- -1
  # SS3 robust multinomial kernel: NLL = N * sum_j obs_s * log(obs_s/hat_s)
  # with obs/hat smoothed by addtocomp. Matches SS3 Method-5 likelihood.
  cod_pcod$fleet_control$Comp_distribution[fi]          <- "SS3Robust"
  cod_pcod$fleet_control$CAAL_distribution[fi]          <- "SS3Robust"
  # Verified via SS3 source (SS_global.tpl:338) + empirical test: SS3 uses
  # data_timing_seas = 0.5 for both INDEX and CAAL with Pcod obs month=7.
  # Setting Month=7 instead breaks the INDEX (machine precision -> 5% off)
  # without consistently improving CAAL (Age 1 gets worse; the peak cell
  # dominates mean rel err). Mid-year (Month=6) is correct for both.
  cod_pcod$fleet_control$Month[fi] <- 6L
  cod_pcod$fleet_control$Bin_first_selected[fi]    <- 2L   # 1-based: zero ages < 2 (= zero age 0 and age 1 in SS3 0-based? no, age 1 in R = age 0 in SS3)
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
# Bin_first_selected indexing: 1-based, R->C++ via -1 in rearrange_data.R.
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
    K    = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 0.1966),
                        priors = list("(Intercept)" = lognormal(log(0.1966), 0.03)),
                        bounds = list("(Intercept)" = c(0.05, 1))),   # was c(0, 1)
    L1   = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 6.1252),
                        bounds = list("(Intercept)" = c(0.5, 50))),   # was c(0, 50)
    Linf = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 99.4617),
                        priors = list("(Intercept)" = normal(99.4617, 0.015)),
                        bounds = list("(Intercept)" = c(70, 130))),   # unchanged (already > 0)
    sd_L1   = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 3.82037),
                        bounds = list("(Intercept)" = c(0.5, 10))),   # was c(0, 10)
    sd_Linf   = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 7.42895),
                        bounds = list("(Intercept)" = c(0.5, 20)))    # was c(0, 10) — also widened upper
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
# mod0 is built with growthFun_spec so the skeleton matches what our
# injection helpers expect (no linkage system intervention on log_growth_pars
# etc.). To enable Phase A2 unification (passing a single `inits` skeleton to
# both FP and estimation, where both use growthFun_est_spec), we ALSO build a
# parallel mod0_est below with growthFun_est_spec and graft its
# `beta_linkage` slot into the inits at injection time. This avoids the
# build_bounds length mismatch that fired when we tried to use mod0_est as
# the sole skeleton.
mod0 <- Rceattle::fit_mod(
  data_list    = cod_pcod,
  inits        = NULL,
  estimateMode = 3,
  initMode     = 3,
  growthFun    = growthFun_spec,
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  fit_control  = fit_control(phase = FALSE, verbose = 1)
)
cat("\nRceattle parameter names:\n",
    paste(names(mod0$estimated_params), collapse = ", "), "\n")

# Parallel mod0_est built with growthFun_est_spec -- used ONLY to extract
# the beta_linkage slot so the FP / estimation fit_mod calls below (both
# using growthFun_est_spec) get an inits with the right linkage structure.
# All other slots come from mod0 / our injection helpers.
cat("\n--- Building mod0_est (beta_linkage skeleton for est_spec path) ---\n")
mod0_est <- Rceattle::fit_mod(
  data_list    = cod_pcod,
  inits        = NULL,
  estimateMode = 3,
  initMode     = 3,
  growthFun    = growthFun_est_spec,
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  fit_control  = fit_control(phase = FALSE, verbose = 1)
)
cat(sprintf("beta_linkage length: %d  values: %s\n",
            length(mod0_est$estimated_params$beta_linkage),
            paste(signif(mod0_est$estimated_params$beta_linkage, 4),
                  collapse = ", ")))


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

# Graft beta_linkage from mod0_est (built with growthFun_est_spec). Intercept
# rows of the linkage table are mapped out and stay at 0; design-col rows
# (e.g. M post2014) carry their SS3 MLE from linkage_spec(init = ...) values.
# This makes `inits` compatible with the growthFun_est_spec path used by
# both the FP fit_mod (just below) and the estimation fit_mod (Section 10).
inits$beta_linkage <- mod0_est$estimated_params$beta_linkage
cat(sprintf("inits$beta_linkage grafted from mod0_est (length %d)\n",
            length(inits$beta_linkage)))

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
options(error = function() {cat('\n=== error traceback ===\n'); traceback(2); cat('=== end ===\n'); q(save="no", status=2)})
