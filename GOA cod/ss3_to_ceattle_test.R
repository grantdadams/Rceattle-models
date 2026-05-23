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
ssb_rows <- which(cod_pcod$weight$Wt_index == 2)
for (r in ssb_rows) cod_pcod$weight[r, age_cols_w] <- mfw_vec

sr_val <- as.numeric(cod_pcod$sex_ratio[1, age_cols_w][1])
if (is.na(sr_val) || sr_val == 0) sr_val <- 0.5
cod_pcod$maturity[1, age_cols_w] <- 1 / sr_val

cat(sprintf("\nJensen fix: SSB_WAA <- Mat_F_wtatage; maturity <- 1/%.2f\n", sr_val))
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
  linkages     = list(log_M1 = linkage_spec(formula = ~ post2014 - 1,
                                            by = ~ species))
)


# =============================================================================
# 4. Build mod0 (parameter shape only, no fit) to get the inits skeleton
# =============================================================================
cat("\n--- Building mod0 (parameter shape) ---\n")
mod0 <- Rceattle::fit_mod(
  data_list    = cod_pcod,
  inits        = NULL,
  estimateMode = 3,
  initMode     = 3,
  growthFun    = build_growth(fun = 0),   # empirical WAA -- VB later
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
    Linf_est <- if (amax_gp >= 99) L_max
                else {
                  delta <- exp(-K_vb * (amax_gp - amin_gp))
                  (L_max - L_min * delta) / (1 - delta)
                }
    L1_rce <- Linf_est - (Linf_est - L_min) *
                exp(-K_vb * (data_list$minage[1] - amin_gp))
    inits$log_growth_pars[1, 1, 1] <- log(K_vb)
    inits$log_growth_pars[1, 1, 2] <- log(max(L1_rce, 0.1))
    inits$log_growth_pars[1, 1, 3] <- log(Linf_est)
    cat(sprintf("Growth: K=%.4f, L1(at minage=%d)=%.4f, Linf=%.4f\n",
                K_vb, data_list$minage[1], L1_rce, Linf_est))
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
# 8. Wire it all up
# =============================================================================
inits <- init_from_ss3(parlist, ctllist, mod0$estimated_params, cod_pcod,
                       fleet_meta, years_hind)
inits <- init_state_from_ss3_natage_m0(inits, ss3_rep, cod_pcod$styr, nages_pcod)
inits <- init_log_F_from_ss3(inits, ts_ss3, fleet_meta, years_hind)


# =============================================================================
# 9. Forward-pass fit (estimateMode = 3) and comparison to SS3
# =============================================================================
cat("\n--- Forward-pass fit (estimateMode = 3) ---\n")
cod_pcod_fixed <- Rceattle::fit_mod(
  data_list    = cod_pcod,
  inits        = inits,
  estimateMode = 3,
  initMode     = "FreeParams",
  growthFun    = build_growth(fun = 0),   # empirical WAA from cod_pcod$weight
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
# 10. Full-MLE estimation (start from SS3 values, optimize)
# =============================================================================
# Estimate everything Rceattle has an SS3 analog for: log_R0, rec_dev, init_dev,
# log_M1, beta_linkage, index_log_q. Selectivity stays "Fixed" via the emp_sel
# Asel2 injection (matches SS3's realized sel by construction). Growth stays
# empirical (WAA fixed at SS3 endgrowth Wt_Beg + Jensen's-gap correction).
# Starting from SS3's MLE means the estimator should stay near it if Rceattle's
# likelihood is structurally compatible with SS3's.
cat("\n--- Full MLE estimation from SS3 starting values (PHASED) ---\n")
# Phased estimation mirrors SS3's ADMB-style approach: estimate a stable
# subset first (R, N), then progressively unmap more parameters. With the
# default fit_control(phase = TRUE), Rceattle uses its built-in phasing
# schedule from rceattle_class.R.
cod_pcod_est <- Rceattle::fit_mod(
  data_list    = cod_pcod,
  inits        = inits,
  estimateMode = 1,                    # hindcast estimation
  initMode     = 3,
  growthFun    = build_growth(fun = 0),
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
