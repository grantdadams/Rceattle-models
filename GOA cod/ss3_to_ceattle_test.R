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


# =============================================================================
# 1. Read SS3 outputs and build the converter data list
# =============================================================================
SS3_DIR <- "Data/goa_pcod"
PAR_FILE <- file.path(SS3_DIR, "ss3.par")
DAT_FILE <- file.path(SS3_DIR, "GOAPcod2024Oct17_1e_5cm.dat")
CTL_FILE <- file.path(SS3_DIR, "Model19_1e.ctl")
TS_FILE  <- "Data/2024pcod_time_series.csv"

parlist <- SS_readpar_3.30(PAR_FILE, datsource = DAT_FILE, ctlsource = CTL_FILE,
                           verbose = FALSE)
datlist <- SS_readdat(DAT_FILE, verbose = FALSE)
ctllist <- SS_readctl(CTL_FILE, use_datlist = TRUE, datlist = datlist,
                      verbose = FALSE)
ss3_rep <- SS_output(SS3_DIR, verbose = FALSE, printstats = FALSE)
ts_ss3  <- read.csv(TS_FILE)

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
# DEFERRED: SS3 has env_var&link = 101 on LLSrv with ENV_add = 0.9147 acting
# additively on log(q). When we replicate that in Rceattle (Catchability =
# "Environmental" + index_q_beta[LLSrv, CFSR_col] = 0.9147), Rceattle's
# log(q) swings ~7x more than SS3's reported Calc_Q does (effective SS3
# slope is ~0.137 vs our 0.9147). SS3 likely standardizes / scales env_var
# internally in a way we haven't decoded yet. Leaving LLSrv at the plain
# "Estimated" q (= exp(LnQ_base_LLSrv) = 1.169 constant) until the SS3 env
# scaling convention is resolved.
# To re-enable: set Catchability <- "Environmental", Time_varying_q <-
# which(setdiff(colnames(cod_pcod$env_data), "Year") == "env_1"),
# and inits$index_q_beta[llsrv_idx, cfsr_col] <- <empirical_slope_~0.137>.

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

M1_block <- build_M1(
  M1_model     = 1,
  M1_use_prior = use_m_prior,
  M2_use_prior = FALSE,
  M_prior      = M_prior_rce,
  M_prior_sd   = M_prior_sd,
  linkages     = list(M1 = linkage_spec(formula = ~ post2014 - 1,
                                        by = ~ species))
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
for (fname in active_sel_fleets) {
  fi <- which(cod_pcod$fleet_control$Fleet_name == fname)
  if (length(fi) == 0L) next
  cod_pcod$fleet_control$Selectivity[fi]           <- "DoubleNormal"
  cod_pcod$fleet_control$Selectivity_dimension[fi] <- "Length"
  cod_pcod$fleet_control$Time_varying_sel[fi]      <- "IID"
}
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
    K    = linkage_spec(formula = ~ 1,
                        priors = list("(Intercept)" = normal(log(0.1966), 0.03))),
    L1   = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = log(6.1252))),
    Linf = linkage_spec(formula = ~ 1,
                        init   = list("(Intercept)" = log(99.4617)),
                        priors = list("(Intercept)" = normal(log(99.4617), 0.015)))
  )
)

cat("\n--- Building mod0 (parameter shape) ---\n")
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


# =============================================================================
# 5. SS3 -> Rceattle parameter injection (adapted for 9 fleets, minage=0)
# =============================================================================
init_from_ss3 <- function(parlist, ctllist, inits, data_list, fleet_meta,
                          years_hind) {
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
    late0 <- ctllist$last_early_yr_nobias_adj
    first1 <- ctllist$first_yr_fullbias_adj
    last1  <- ctllist$last_yr_fullbias_adj
    first0 <- ctllist$first_recent_yr_nobias_adj
    bmax   <- ctllist$max_bias_adj
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

  # --- Per-survey catchability (base log_q + env_add coefficients) ---
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
      # ENV_add coefficient (per SS3 ctl env_var&link). Match label of the
      # form LnQ_base_<name>(<id>)_ENV_add in parlist$Q_parms; place into
      # index_q_beta at the CFSR column found earlier.
      env_pat <- sprintf("LnQ_base_%s\\(%d\\)_ENV_add$",
                        fleet_meta$name[i], fleet_meta$ss3_num[i])
      env_q <- get_par(parlist$Q_parms, env_pat)
      if (!is.null(env_q) && "index_q_beta" %in% names(inits)) {
        env_col_for_q <- data_list$fleet_control$Time_varying_q[i]
        if (!is.na(env_col_for_q)) {
          inits$index_q_beta[i, env_col_for_q] <- env_q
          cat(sprintf("  q[%s] ENV_add = %.4f (into index_q_beta[fleet=%d, env_col=%d])\n",
                      fleet_meta$name[i], env_q, i, env_col_for_q))
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


# =============================================================================
# 7. log_F pinning (per-fishery, per-year)
# =============================================================================
init_log_F_from_ss3 <- function(inits, ts_ss3, fleet_meta, years_hind) {
  if (!"log_F" %in% names(inits)) return(inits)
  log_F <- inits$log_F
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
                                       years_hind) {
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
    if (is.na(P2) || P2 < -100) P2 <- -10.0   # narrow plateau (peak2 ~ peak + binwidth)
    if (is.na(P5) || P5 < -100) P5 <- -10.0   # left floor -> 0
    if (is.na(P6) || P6 < -100) P6 <- -10.0   # right floor -> 0

    inits$sel_inf[1, i, 1]     <- P1   # peak length (cm)
    inits$sel_inf[2, i, 1]     <- P6   # logit(right_floor)
    inits$sel_inf[3, i, 1]     <- P5   # logit(left_floor / init)
    inits$log_sel_slp[1, i, 1] <- P3   # log(sigma_ascending)
    inits$log_sel_slp[2, i, 1] <- P4   # log(sigma_descending)
    inits$log_sel_slp[3, i, 1] <- P2   # top-width logit
    cat(sprintf("  %s base: peak=%.2f sigma_asc=%.3f sigma_desc=%.3f init=%.4f final=%.4f topW_lt=%.2f\n",
                fname, P1, exp(P3), exp(P4),
                1 / (1 + exp(-P5)), 1 / (1 + exp(-P6)), P2))

    # Block deviates --------------------------------------------------------
    bp <- fleet_block_pattern[[fname]]
    if (is.na(bp)) next
    bd  <- ctllist$Block_Design[[bp]]
    bks <- block_year_ranges(bd)
    for (blk in ss3_sel_blocks(parlist, fname, fnum, bp)) {
      # Match SS3 BLKreplN -> block_year_ranges entry by start year
      b_id <- which(vapply(bks, function(b) unname(b["start"]) == blk$start_yr,
                           logical(1)))
      if (length(b_id) == 0L) next
      yr_lo <- unname(bks[[b_id]]["start"]); yr_hi <- unname(bks[[b_id]]["end"])
      yr_idx <- which(years_hind >= yr_lo & years_hind <= yr_hi)
      if (length(yr_idx) == 0L) next
      base_val <- base[blk$P]
      if (is.na(base_val) || base_val < -100) base_val <- -10.0
      dev_val <- blk$value - base_val
      # SS3 param index -> Rceattle (array, slot) destination
      target <- switch(blk$P,
                       `1` = list("sel_inf_dev",     1),  # P1 -> peak
                       `2` = list("log_sel_slp_dev", 3),  # P2 -> top-width
                       `3` = list("log_sel_slp_dev", 1),  # P3 -> asc width
                       `4` = list("log_sel_slp_dev", 2),  # P4 -> desc width
                       `5` = list("sel_inf_dev",     3),  # P5 -> init
                       `6` = list("sel_inf_dev",     2),  # P6 -> final
                       NULL)
      if (is.null(target)) next
      inits[[target[[1]]]][target[[2]], i, 1, yr_idx] <- dev_val
    }
  }
  inits
}


# =============================================================================
# 8. Wire it all up
# =============================================================================
inits <- init_from_ss3(parlist, ctllist, mod0$estimated_params, cod_pcod,
                       fleet_meta, years_hind)
inits <- init_state_from_ss3_natage_m0(inits, ss3_rep, cod_pcod$styr, nages_pcod)
inits <- init_log_F_from_ss3(inits, ts_ss3, fleet_meta, years_hind)
inits <- init_doublenormal_from_ss3(inits, parlist, ctllist, fleet_meta, years_hind)


# =============================================================================
# 9. Forward-pass fit (estimateMode = 3) and comparison to SS3
# =============================================================================
cat("\n--- Forward-pass fit (estimateMode = 3) ---\n")
cod_pcod_fixed <- Rceattle::fit_mod(
  data_list    = cod_pcod,
  inits        = inits,
  estimateMode = 3,
  initMode     = "FreeParams",
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
if (TRUE) {
  cat("\n[stop] Estimation section skipped during sel-parametric validation.\n")
  quit(save = "no", status = 0)
}


# =============================================================================
# 10. Full-MLE estimation (start from SS3 values, optimize)
# =============================================================================
# Estimate everything Rceattle has an SS3 analog for: log_R0, rec_dev, init_dev,
# log_M1, beta_linkage, index_log_q. Selectivity stays "Fixed" via the emp_sel
# Asel2 injection (matches SS3's realized sel by construction). Growth stays
# empirical (WAA fixed at SS3 endgrowth Wt_Beg + Jensen's-gap correction).
# Starting from SS3's MLE means the estimator should stay near it if Rceattle's
# likelihood is structurally compatible with SS3's.
# CAAL likelihood weighting: at default CAAL_weights = 1, Rceattle's CAAL
# NLL is 32k vs SS3's Age_like of ~722 (per-obs ratio ~45). The dominant
# CAAL likelihood was driving the optimizer to bad minima -- log_F 5-10x
# too high, log(R0) ~30% off. Downweight to match SS3's effective per-obs
# influence. SS3 doesn't use CAAL the same way (it splits Length_comp +
# marginal Age_comp), so this is a compromise to make the optimization
# landscape closer to SS3's.
caal_scale <- 1 / 45
cat(sprintf("\nDownweighting CAAL_weights by %.4f to match SS3's per-obs influence\n",
            caal_scale))
cod_pcod$fleet_control$CAAL_weights <- caal_scale

# Switch to parametric VB growth so the C++ populates growth_matrix from
# the VB curve. Empirical growth (growth_model = 0) leaves growth_matrix
# at zero -- pred_CAAL collapses to 0 and the CAAL likelihood gradient is
# uninformative. With VB growth + SS3-injected K/L1/Linf the growth_matrix
# reproduces SS3's ALK, restoring useful CAAL gradients. SIDE EFFECT: the
# C++ will now compute weight_hat from VB (overwriting our Mat_F_wtatage
# injection in the SSB slot), so the Jensen's-gap fix no longer applies
# until a parametric-path equivalent is added (separate thread).
cat("\n--- Full MLE estimation (PHASED, CAAL downweighted, VB growth) ---\n")
cod_pcod_est <- Rceattle::fit_mod(
  data_list    = cod_pcod,
  #inits        = inits,
  estimateMode = 0,                    # hindcast estimation
  initMode     = 2,
  growthFun    = growthFun_est_spec,   # with SS3 priors on K / Linf
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
