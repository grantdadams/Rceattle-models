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
# Ageing-error injection (replace converter's identity matrix with SS3 def 1)
# =============================================================================
# SS3 stores ageing error in `datlist$ageerror` as a 2*N_def x nages matrix:
# row 2k-1 = expected reported age | true age (bias);
# row 2k   = SD of reported age | true age.
# Pcod 2024 has two definitions: def 1 (biased; mean ~ true+0.6) and def 2
# (unbiased sentinel mean=-1, same SD). We use def 1 because the dominant
# fleet rows reference it (def 2's mean = -1 sentinel is unused unless an
# agecomp row explicitly references the unbiased path).
#
# Rce's `data_list$age_error` is a data.frame with [Species, True_age,
# Obs_age_1..Obs_age_N] where each row is P[obs | true]. rearrange_dat()
# turns it into a 3D array [sp, true_age, obs_age]; the cpp uses it at
# pred_CAAL / pred_age_comp call sites to convolve true ages with the
# reporting-error distribution.
build_ss3_age_error <- function(ageerror_mat, nages, minage = 0L, def_idx = 1L) {
  # def_idx = 1 -> rows 1 (mean) and 2 (sd) of ageerror_mat
  # def_idx = 2 -> rows 3 (mean, sentinel -1 = unbiased) and 4 (sd)
  mean_row <- 2L * def_idx - 1L
  sd_row   <- 2L * def_idx
  means <- as.numeric(ageerror_mat[mean_row, ])
  sds   <- as.numeric(ageerror_mat[sd_row,   ])
  # Sentinel mean = -1 -> use the unbiased (mean = true age) interpretation
  ages_true <- seq.int(minage, minage + nages - 1L)
  means[means < 0] <- ages_true[means < 0]
  stopifnot(length(means) == nages, length(sds) == nages, all(sds > 0))

  P <- matrix(0, nrow = nages, ncol = nages)
  for (i in seq_len(nages)) {
    m <- means[i]; s <- sds[i]
    # Minus group at obs_age = minage: P[obs <= minage + 0.5 | true]
    P[i, 1] <- stats::pnorm(minage + 0.5, m, s)
    # Interior bins
    if (nages >= 3L) {
      for (j in 2:(nages - 1L)) {
        obs <- minage + j - 1L
        P[i, j] <- stats::pnorm(obs + 0.5, m, s) -
                   stats::pnorm(obs - 0.5, m, s)
      }
    }
    # Plus group at obs_age = minage + nages - 1: P[obs >= plusgroup - 0.5]
    plus_obs <- minage + nages - 1L
    P[i, nages] <- 1 - stats::pnorm(plus_obs - 0.5, m, s)
  }
  # Each row should sum to 1 by construction (no mass outside the obs range).
  row_sums <- rowSums(P)
  if (any(abs(row_sums - 1) > 1e-8)) {
    warning(sprintf("age_error row sums differ from 1: max dev = %.2e",
                    max(abs(row_sums - 1))))
  }
  out <- data.frame(Species = 1L, True_age = ages_true)
  Pdf <- as.data.frame(P)
  colnames(Pdf) <- paste0("Obs_age", 1:nages)
  cbind(out, Pdf)
}

cod_pcod$age_error <- build_ss3_age_error(
  ageerror_mat = datlist$ageerror,
  nages        = nages_pcod,
  minage       = minage_pcod,
  def_idx      = 2L
)
# Pcod 2024: 694 of 827 CAAL rows (and 56 of 63 marginal age-comp rows)
# reference ageerr def 2 (unbiased mean = true age, same SDs as def 1);
# def 1 (biased mean ~ true + 0.6) is used by only 140 rows total. Using
# def 2 matches the dominant data path and keeps the matrix close to
# identity for low SDs at young ages.
cat("Injected SS3 ageing-error matrix (def 2, unbiased) into cod_pcod$age_error\n")
cat(sprintf("  shape = %d rows x %d cols (Species, True_age, Obs_age_1..%d)\n",
            nrow(cod_pcod$age_error), ncol(cod_pcod$age_error), nages_pcod))
cat("  P[obs|true=0] head: ",
    paste(signif(as.numeric(cod_pcod$age_error[1, 3:6]), 3), collapse = " "),
    "...\n", sep = "")


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
# From SS_timevaryparm.tpl case 1, SS3 multiplies LnQ_base by exp(env_add *
# env_var[yr]), then exponentiates:
#   LnQ_tv[yr] = LnQ_base * exp(env_add * env_var[yr])
#   q[yr]      = exp(LnQ_tv[yr])
# This is a NESTED exponential in env_var -- not equivalent to the additive
# log-linear form `log(q) = LnQ + beta * env`. Rceattle's `EnvExp` (= 7)
# implements the SS3 case-1 formula in C++ exactly:
#   index_q(flt, yr) = exp(index_log_q(flt) * exp(sum_k index_q_beta(flt, k)
#                                                       * env_index(yr, k)))
# Set Catchability = "EnvExp" + Time_varying_q = "<env_col_idx>" so the env_1
# column of index_q_beta becomes estimable (same convention as "Environmental").
# The MLE values come from the SS3-injection helper below.
cod_pcod$fleet_control$Catchability[llsrv_idx]   <- "EnvExp"
# env_data columns after build_env_data: 1=block_1, 2=block_2, ..., then env_1.
# Look up the env_1 column index dynamically so this stays correct if more
# blocks get added upstream.
.env_1_col <- match("env_1", colnames(cod_pcod$env_data)) - 1L  # -1 to skip Year
stopifnot(!is.na(.env_1_col))
cod_pcod$fleet_control$Time_varying_q[llsrv_idx] <- as.character(.env_1_col)

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
  # BlockDev maps one estimable dev label per (fleet, sub-block) from SS3's
  # ctl Block_Design and NA-locks dev cells outside any sub-block. Combined
  # with Time_varying_sel_sd <= 0 the dev prior is skipped entirely
  # so injected per-year SS3 effective values pass through unaltered. NOTE:
  # under BlockDev, all years within one sub-block share a SINGLE estimable
  # dev value — per-year SS3 dev_seq variation INSIDE a sub-block (#10
  # three-tier) collapses to the value at the first year of the sub-block.
  # This degrades FP NLL relative to the prior IID-with-sentinel path
  # (which mapped every year independently), in exchange for unified
  # Time_varying_sel between FP and estimation paths.
  cod_pcod$fleet_control$Time_varying_sel[fi]          <- "BlockDev"
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
# 3c. Selectivity_block from SS3 ctl: per (fleet, year) sub-block ID
# =============================================================================
# SS3 ctl assigns each (fleet, sel-param) row a Block design integer; the
# Block_Design list gives the year ranges within each design. Multiple sel
# rows for one fleet usually point to the same Block design (e.g. FshTrawl
# P1..P4 -> Design 2). Encode the active SUB-block per (fleet, year) into
# index_data$Selectivity_block / catch_data$Selectivity_block:
#   0 = base years (no block override)
#   k > 0 = k-th sub-block within the fleet's design (1-based)
# Years inside a sub-block share one estimable dev replacing the base; the
# per-year prior loop is rescaled by sel_inf_dev_prior_weight = 1/N(yrs in
# sub-block) so the total prior contribution matches SS3's single per-
# replacement prior. The Rceattle build_map_selectivity helper consumes
# these IDs to factor-share devs across the sub-block years.
populate_selectivity_block <- function(cod_pcod, ctllist, fleet_meta,
                                       active_sel_fleets) {
  sse <- ctllist$size_selex_parms
  blocks <- ctllist$Block_Design
  pat <- "^SizeSel_P_([1-6])_([A-Za-z]+)\\(([0-9]+)\\)$"
  m <- regmatches(rownames(sse), regexec(pat, rownames(sse)))

  # Per-fleet: pick the dominant Block design across that fleet's estimable
  # sel params (mode of the Block column for PHASE >= 0 rows).
  flt_design <- integer(0)
  for (i in seq_len(nrow(fleet_meta))) {
    fname <- fleet_meta$name[i]
    if (!fname %in% active_sel_fleets) next
    designs <- vapply(seq_along(m), function(k) {
      if (length(m[[k]]) < 4L) return(NA_integer_)
      if (m[[k]][3] != fname) return(NA_integer_)
      if (sse$PHASE[k] < 0) return(NA_integer_)
      as.integer(sse$Block[k])
    }, integer(1))
    designs <- designs[!is.na(designs) & designs > 0]
    if (length(designs) > 0L) {
      flt_design[fname] <- as.integer(names(sort(table(designs),
                                                  decreasing = TRUE))[1])
    }
  }

  # Build (fleet, year) -> sub-block ID. Sub-blocks are pairs in Block_Design
  # vectors: design 2 = c(1990,2004,2005,2006,2007,2016,2017,2024) defines
  # 4 sub-blocks (1990-2004, 2005-2006, 2007-2016, 2017-2024).
  hindyr <- cod_pcod$styr:cod_pcod$endyr
  assign_sub_block <- function(yrs, design_idx) {
    if (is.na(design_idx) || design_idx == 0L) {
      return(integer(length(yrs)))
    }
    bd <- blocks[[design_idx]]
    n_sub <- length(bd) %/% 2L
    out <- integer(length(yrs))
    for (k in seq_len(n_sub)) {
      y1 <- bd[2L*k - 1L]; y2 <- bd[2L*k]
      out[yrs >= y1 & yrs <= y2] <- k
    }
    out
  }

  for (i in seq_len(nrow(fleet_meta))) {
    fname <- fleet_meta$name[i]
    fnum  <- fleet_meta$ss3_num[i]
    if (!fname %in% active_sel_fleets) next
    design_i <- flt_design[fname] %||% 0L
    # catch_data rows
    if (fnum %in% cod_pcod$catch_data$Fleet_code) {
      hits <- which(cod_pcod$catch_data$Fleet_code == fnum)
      cod_pcod$catch_data$Selectivity_block[hits] <-
        assign_sub_block(cod_pcod$catch_data$Year[hits], design_i)
    }
    # index_data rows
    if (fnum %in% cod_pcod$index_data$Fleet_code) {
      hits <- which(cod_pcod$index_data$Fleet_code == fnum)
      cod_pcod$index_data$Selectivity_block[hits] <-
        assign_sub_block(cod_pcod$index_data$Year[hits], design_i)
    }
    cat(sprintf("  %s -> Block_Design %d, sub-blocks: %d\n",
                fname, design_i,
                if (design_i == 0L) 0L else length(blocks[[design_i]]) %/% 2L))
  }
  cod_pcod
}

cat("\n--- Populating Selectivity_block from SS3 ctl Block_Design ---\n")
cod_pcod <- populate_selectivity_block(cod_pcod, ctllist, fleet_meta,
                                       active_sel_fleets)


# =============================================================================
# 4. Build mod0 (parameter shape only) to get the inits skeleton
# =============================================================================
# growthFun is hoisted so mod0 + FP + estimation share the same parameter
# shape. linkage_spec inits/priors/bounds are NATURAL-scale; build_params
# writes log() into log_growth_pars / growth_log_sd. Inits = SS3 ESTIM MLEs
# (so the linkage push lands on the MLE, not the ctl prior mean).
growthFun_est_spec <- build_growth(
  fun = "vonBertalanffy",
  linkages = list(
    K    = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 0.1909988),
                        priors = list("(Intercept)" = lognormal(log(0.1966), 0.03)),
                        bounds = list("(Intercept)" = c(0.05, 1))),
    L1   = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 0.2465480),
                        bounds = list("(Intercept)" = c(0.1, 50))),
    Linf = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 99.4608396),
                        priors = list("(Intercept)" = normal(99.4617, 0.015)),
                        bounds = list("(Intercept)" = c(70, 130))),
    sd_L1   = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 2.9443667),
                        bounds = list("(Intercept)" = c(0.5, 10))),
    sd_Linf = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = 9.0740724),
                        bounds = list("(Intercept)" = c(0.5, 20)))
  )
)

cat("\n--- Building mod0 (parameter shape) ---\n")
mod0 <- Rceattle::fit_mod(
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
    # Look up the M post2014 slope row by name in linkage_table -- with
    # `growthFun_est_spec` enabled, growth intercept rows (K/L1/Linf/
    # sd_L1/sd_Linf) come FIRST in the pooled table and the M slope is
    # no longer at row 1. Positional `[1]` was silently writing the M
    # value into the K intercept slot, multiplying K by exp(0.595) at
    # every year (Bio rel err ~165%, ALK 1.4e7) before the package's
    # defensive zero-intercept normalization landed (Rceattle/R/6-fit_mod.R).
    tbl <- mod0$data_list$linkage_table %||% data_list$linkage_table
    m_row <- which(tbl$process == "M" &
                     tbl$design_col == "post2014")
    if (length(m_row) == 1L) {
      inits$beta_linkage[m_row] <- log(M_blk / M_base)
      cat(sprintf("M post-2014 = %.4f (beta = %.4f) row=%d\n",
                  M_blk, log(M_blk / M_base), m_row))
    } else if (length(m_row) == 0L) {
      cat("WARNING: M post2014 row not found in linkage_table; ",
          "leaving beta_linkage untouched.\n")
    } else {
      cat(sprintf("WARNING: %d M post2014 rows matched in linkage_table; ",
                  length(m_row)),
          "expected exactly 1.\n")
    }
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
  # Inject SS3's LnQ_base MLE as index_log_q (constant log-q baseline). For
  # fleets configured as `Catchability = "EnvExp"` (SS3 case-1 exponential
  # env link), additionally inject the SS3 ENV_add MLE into the matching
  # index_q_beta column so the cpp reproduces SS3's nested-exp formula:
  #   q[yr] = exp(index_log_q(flt) * exp(index_q_beta(flt, env_col) * env_index(yr, env_col)))
  # exactly (matches SS3 Calc_Q to machine precision; verified for Pcod LLSrv).
  if ("index_log_q" %in% names(inits)) {
    env_beta_cols <- if (!is.null(inits$index_q_beta)) colnames(inits$index_q_beta) else character(0)
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

      # EnvExp: inject the ENV_add slope into index_q_beta[fleet, "env_<v>"].
      # SS_readpar_3.30 emits both base LnQ and tv ENV_add rows into the SAME
      # parlist$Q_parms table; row name format is
      #   "LnQ_base_<Fleet>(<num>)_ENV_add"
      # with column "ESTIM" holding the MLE. (`parlist$Q_parms_tv` exists in
      # newer r4ss versions but is NULL for our run; falling back to Q_parms.)
      if (data_list$fleet_control$Catchability[i] == "EnvExp" &&
          length(env_beta_cols) > 0L) {
        env_add_pat <- sprintf("LnQ_base_%s\\(%d\\)_ENV_add$",
                               fleet_meta$name[i], fleet_meta$ss3_num[i])
        env_add <- get_par(parlist$Q_parms, env_add_pat)
        if (is.null(env_add) && !is.null(parlist$Q_parms_tv)) {
          env_add <- get_par(parlist$Q_parms_tv, env_add_pat)
        }
        if (!is.null(env_add)) {
          ql_row <- which(rownames(ctllist$Q_parms) ==
                          sprintf("LnQ_base_%s(%d)",
                                  fleet_meta$name[i], fleet_meta$ss3_num[i]))
          if (length(ql_row) == 1L) {
            evl <- as.integer(ctllist$Q_parms[ql_row, "env_var&link"])
            v_idx <- evl %% 100L
            col_name <- sprintf("env_%d", v_idx)
            col_pos <- match(col_name, env_beta_cols)
            if (!is.na(col_pos)) {
              inits$index_q_beta[i, col_pos] <- env_add
              cat(sprintf("  q[%s] EnvExp: index_q_beta[%s, %s] = %.4f (SS3 ENV_add ESTIM)\n",
                          fleet_meta$name[i], fleet_meta$name[i], col_name, env_add))
            }
          }
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
      # it) and shift the column mapping.
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
R_init_pcod <- exp(parlist$SR_parms["SR_LN(R0)", "ESTIM"])
M1_at_age_pcod <- rep(parlist$MG_parms["NatM_p_1_Fem_GP_1", "ESTIM"], nages_pcod)
Finit_pcod <- if (is.finite(sr_regime_mle) && sr_regime_mle < 0) -sr_regime_mle else 0
inits <- init_state_from_ss3_natage_mode4(
  inits, ss3_rep, cod_pcod$styr, nages_pcod,
  R_init = R_init_pcod, Finit = Finit_pcod, M1_at_age = M1_at_age_pcod
)





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
    # Back-transform Rce log-scale bounds to natural scale for an apples-to-
    # apples display against SS3 ctl LO/HI. log_tx => inverse is exp(); ident
    # leaves them alone (SR_LN(R0) is already on log scale in both models).
    rce_nat <- if (identical(m$tx, log_tx))
                 c(exp(rce$lo), exp(rce$hi))
               else c(rce$lo, rce$hi)
    lo_match <- isTRUE(abs(rce$lo - expected[1]) < 1e-4) ||
                (is.infinite(rce$lo) && is.infinite(expected[1]))
    hi_match <- isTRUE(abs(rce$hi - expected[2]) < 1e-4) ||
                (is.infinite(rce$hi) && is.infinite(expected[2]))
    cmp_rows[[k]] <- data.frame(
      param   = m$label,
      ss3_lo  = signif(ss3[1], 5), ss3_hi = signif(ss3[2], 5),
      rce_lo  = signif(rce_nat[1], 5), rce_hi = signif(rce_nat[2], 5),
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


# Use mod0$data_list (not cod_pcod) so build_bounds sees the populated
# linkage_table -- otherwise the linkage-driven bound push into
# log_growth_pars / growth_log_sd doesn't fire and every growth slot reports
# -Inf/Inf as a spurious MISMATCH.
.audit_bounds <- bounds_audit(default_params = mod0$estimated_params,
                              data_list      = mod0$data_list,
                              ctllist        = ctllist,
                              species        = 1L)



# =============================================================================
# 9. Forward-pass fit (estimateMode = 3) and comparison to SS3
# =============================================================================
cat("\n--- Forward-pass fit (estimateMode = 3) ---\n")
# initMode = "NonEquilibriumScaled" (= 4) gives SS3's SR_regime mechanism
# (Finit acts as a single scalar offset on R_init) WITH per-age init_dev to
# absorb SS3's historical non-equilibrium structure at styr. Combined with
# init_state_from_ss3_natage_mode4 this pins styr N to SS3 (rel err 1e-6).
cod_pcod_fixed <- Rceattle::fit_mod(
  data_list    = cod_pcod,
  inits        = inits,
  estimateMode = 3,
  initMode     = "NonEquilibriumScaled",
  growthFun    = growthFun_est_spec,
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


# --- Grouped NLL comparison vs SS3 likelihoods_used --------------------------
# Compact tracker so we see the per-component gap on every run. SS3 component
# labels come from Report.sso `likelihoods_used`; Rce rows are matched by
# regex against `jnll_comp` row names. Full per-fleet breakdown lives in
# Estimation_Differences.md; this is just the headline.
cat("\n=== Grouped NLL components (SS3 vs Rceattle) ===\n")
jnll <- cod_pcod_fixed$quantities$jnll_comp
rce_tot <- rowSums(jnll)
rce_lab <- rownames(jnll)
pick <- function(...) {
  needles <- c(...); s <- 0
  for (n in needles) {
    hits <- grep(n, rce_lab, ignore.case = TRUE)
    if (length(hits) > 0) s <- s + sum(rce_tot[hits])
  }
  s
}
ss3_lik <- setNames(ss3_rep$likelihoods_used[, "values"],
                    rownames(ss3_rep$likelihoods_used))
nll_cmp <- rbind(
  data.frame(Component = "Survey index",         SS3 = ss3_lik["Survey"],        Rce = pick("Index")),
  data.frame(Component = "Catch",                SS3 = ss3_lik["Catch"],         Rce = pick("Catch data")),
  data.frame(Component = "Length comp",          SS3 = ss3_lik["Length_comp"],   Rce = pick("Composition")),
  data.frame(Component = "Age/CAAL comp",        SS3 = ss3_lik["Age_comp"],      Rce = pick("CAAL")),
  data.frame(Component = "Recruitment dev",      SS3 = ss3_lik["Recruitment"],   Rce = pick("Recruitment dev")),
  data.frame(Component = "Init eq / init dev",   SS3 = ss3_lik["InitEQ_Regime"], Rce = pick("Initial abundance")),
  data.frame(Component = "Parm priors",          SS3 = ss3_lik["Parm_priors"],
             Rce = pick("M prior", "Linkage-table priors", "Catchability prior", "Stock-recruit prior")),
  data.frame(Component = "Parm devs (sel+q)",    SS3 = ss3_lik["Parm_devs"],
             Rce = pick("Selectivity deviates", "Catchability deviates"))
)
nll_cmp$Diff <- signif(nll_cmp$Rce - nll_cmp$SS3, 4)
nll_cmp$SS3  <- signif(nll_cmp$SS3, 6)
nll_cmp$Rce  <- signif(nll_cmp$Rce, 6)
print(nll_cmp, row.names = FALSE)
cat(sprintf("\nTOTAL  SS3: %.4f  Rce: %.4f  Diff: %+.4f\n",
            ss3_lik["TOTAL"], sum(rce_tot), sum(rce_tot) - ss3_lik["TOTAL"]))


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
sel_age_err  <- list()
sel_age_cell <- list()  # per-cell rows for the high-rel-err probe below
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
    sel_age_cell[[length(sel_age_cell) + 1]] <- data.frame(
      Fleet = fleet_meta$name[i], Year = yr,
      Age   = seq_along(ss3_vec) - 1L,
      SS3   = ss3_vec, Rce = rce_vec, RelErr = rel,
      stringsAsFactors = FALSE)
  }
}
if (length(sel_age_err) > 0) {
  sel_age_err <- do.call(rbind, sel_age_err)
  print(sel_age_err %>% dplyr::group_by(Fleet) %>%
          dplyr::summarise(max_rel = max(MaxRelErr),
                           mean_rel = mean(MeanRelErr)))

  # Probe: any fleet whose max rel err > 0.5 dumps its 5 worst cells. With
  # a 1e-4 floor in the relative-error denom, large rel err = near-zero SS3
  # sel (age 0 or unselected ages); confirm that's the pattern rather than
  # a structural mismatch before estimation.
  bad_fleets <- sel_age_err %>%
    dplyr::group_by(Fleet) %>%
    dplyr::summarise(max_rel = max(MaxRelErr)) %>%
    dplyr::filter(max_rel > 0.5)
  if (nrow(bad_fleets) > 0) {
    all_cells <- do.call(rbind, sel_age_cell)
    cat("\n[probe] Worst 5 cells per fleet with max_rel > 0.5:\n")
    for (f in bad_fleets$Fleet) {
      cf <- all_cells[all_cells$Fleet == f, ]
      worst <- cf[order(-cf$RelErr), ][seq_len(min(5, nrow(cf))), ]
      worst$SS3    <- signif(worst$SS3, 4)
      worst$Rce    <- signif(worst$Rce, 4)
      worst$RelErr <- signif(worst$RelErr, 3)
      cat(sprintf("  -- %s --\n", f))
      print(worst, row.names = FALSE)
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
