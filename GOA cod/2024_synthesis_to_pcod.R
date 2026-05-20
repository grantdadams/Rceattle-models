# GOA Pacific Cod 2024 — Rceattle approximation of the SS3 assessment
# Uses the "dev" version of Rceattle and r4ss for SS3 parameter extraction.
#
# Workflow:
#   1. Read ss3.par via r4ss::SS_readpar_3.30 into a structured parlist.
#   2. init_from_ss3_par() translates parlist → Rceattle inits object.
#   3. Run models: SS3 params fixed (forward-only), estimated from SS3 starts.
#   4. Compare against SS3-derived time series.

library(Rceattle)
library(r4ss)
library(dplyr)
library(tidyr)


# ============================================================================
# 0.  Paths and data
# ============================================================================
SS3_DIR   <- "Data/goa_pcod"
PAR_FILE  <- file.path(SS3_DIR, "ss3.par")
DAT_FILE  <- file.path(SS3_DIR, "GOAPcod2024Oct17_1e_5cm.dat")
CTL_FILE  <- file.path(SS3_DIR, "Model19_1e.ctl")
TS_FILE   <- "Data/2024pcod_time_series.csv"
RCEATTLE_DATA <- "Data/GOA_24_pcod_single_species_1977-2024_w_CAAL.xlsx"

# Diagnostic toggle. When TRUE, hard-codes the year-1 N-at-age from SS3's
# natage report (initMode = "FreeParams"), isolating forward-dynamics error
# from equilibrium-derivation error. Set FALSE once the bridge is verified
# and a parametric init (recdev_early -> init_dev) is implemented.
USE_SS3_INITIAL_NATAGE <- TRUE

cod_caal <- read_data(file = RCEATTLE_DATA)

# Block-M workaround (binary linkage covariate on log_M1):
#   M(yr) = exp(log_M_base + beta * post2014(yr))
cod_caal$env_data <- merge(cod_caal$env_data,
                           data.frame(
                             Year     = cod_caal$styr:cod_caal$endyr,
                             post2014 = as.integer((cod_caal$styr:cod_caal$endyr) >= 2014)
                           )
)
M1_block <- build_M1(
  M1_model = 1, M1_use_prior = FALSE, M2_use_prior = FALSE,
  linkages = list(log_M1 = linkage_spec(formula = ~ post2014, by = ~ species))
)


# ============================================================================
# 1.  Read SS3 parameter file
# ============================================================================
cat("Reading SS3 parameter file...\n")
parlist <- SS_readpar_3.30(
  parfile   = PAR_FILE,
  datsource = DAT_FILE,
  ctlsource = CTL_FILE,
  verbose   = FALSE
)

# Quick look at structure
cat("SS3 parlist sections:", paste(names(parlist), collapse = ", "), "\n")

cat("Reading SS3 data and control files...\n")
datlist_ss3 <- SS_readdat(file = DAT_FILE, verbose = FALSE)
ctllist_ss3 <- SS_readctl(file = CTL_FILE, use_datlist = TRUE, datlist = datlist_ss3, verbose = FALSE)
cat("Block designs found:", ctllist_ss3$N_Block_Designs, "\n")


# ============================================================================
# 2.  Fleet metadata
#     Rows must match the ROW ORDER of fleet_control in the Rceattle data.
#     Fleet numbers in parentheses match SS3 fleet numbering.
# ============================================================================
fleet_meta <- data.frame(
  name       = c("FshTrawl", "FshLL", "FshPot", "Srv", "LLSrv"),
  ss3_num    = c(1, 2, 3, 4, 5),        # SS3 fleet number (used in par names)
  has_blocks = c(TRUE, TRUE, TRUE, TRUE, FALSE),
  fleet_type = c("Fishery", "Fishery", "Fishery", "Survey", "Survey"),
  stringsAsFactors = FALSE
)


# ============================================================================
# 3.  init_from_ss3_par()
#     Translates the r4ss parlist into Rceattle inits arrays.
#
#     SS3 → Rceattle selectivity conversion:
#       SS3: exp(-t^2 / exp(P3))   =   Rceattle: exp(-0.5*(t/sigma)^2)
#       => log_sel_slp = (P3 - log(2)) / 2
#
#       SS3 P6 (end_logit) = logit(right_floor) = sel_inf[2] directly
#
#     Limitations vs SS3:
#       P2 (plateau top_logit) — not implemented in Rceattle
#       P5 (left-tail start_logit) — not implemented in Rceattle
# ============================================================================
init_from_ss3_par <- function(parlist, inits, data_list, fleet_meta, years_hind, ctllist = NULL) {

  # Lookup helper: first matching row from a parlist section
  get_par <- function(section, pattern) {
    if (is.null(section)) return(NULL)
    idx <- grep(pattern, rownames(section))
    if (length(idx) == 0) return(NULL)
    section[idx[1], "ESTIM"]
  }

  # Set a Rceattle parameter by trying multiple candidate names
  set_p <- function(inits, candidates, value) {
    for (nm in candidates) {
      if (nm %in% names(inits)) { inits[[nm]][] <- value; return(inits) }
    }
    warning("Parameter not found: ", paste(candidates, collapse = "/"))
    inits
  }

  # --- 3a. Natural mortality ---
  M_base <- get_par(parlist$MG_parms, "NatM_uniform_Fem_GP_1$")
  M_blk  <- get_par(parlist$MG_parms, "NatM_uniform_Fem_GP_1_BLK")
  if (!is.null(M_base) && "log_M1" %in% names(inits)) {
    inits$log_M1[] <- log(M_base)
    cat(sprintf("M1 base set to %.4f (log = %.4f)\n", M_base, log(M_base)))
  }
  if (!is.null(M_blk) && !is.null(M_base)) {
    beta <- log(M_blk / M_base)
    # beta_linkage[1] is the post-2014 M coefficient when using linkage_spec
    if ("beta_linkage" %in% names(inits) && length(inits$beta_linkage) >= 1) {
      inits$beta_linkage[1] <- beta
      cat(sprintf("M1 post2014 beta set to %.4f (M_post2014 = %.4f)\n", beta, M_blk))
    } else {
      warning("beta_linkage not in inits — M1 post-2014 effect not set")
    }
  }

  # --- 3b. Stock-recruitment ---
  # rec_pars[sp, col]: col 1 = log_R0, col 2 = log_alpha, col 3 = log_beta
  ln_R0 <- get_par(parlist$SR_parms, "SR_LN")
  if (!is.null(ln_R0) && "rec_pars" %in% names(inits)) {
    inits$rec_pars[1, 1] <- ln_R0
    cat(sprintf("ln(R0) set to %.4f\n", ln_R0))
  } else if (!"rec_pars" %in% names(inits)) {
    warning("rec_pars not in inits")
  }

  # --- 3c. Recruitment deviations ---
  # SS3 population dynamics: R(y) = R0 * exp(dev(y) - 0.5 * b(y) * sigmaR^2)
  # Rceattle:                R(y) = R0 * exp(rec_dev(y))
  # => rec_dev(y) = dev(y) - 0.5 * b(y) * sigmaR^2
  # NOTE: b(y) multiplies the variance offset ONLY, not the dev itself.
  # An earlier draft of this script had `ba * dev - 0.5 * ba * sigmaR^2`,
  # which silently shrank the dev toward zero in years where b<1 (the early
  # ramp and the recent taper). That caused Rceattle's R to track R0 in
  # those years instead of tracking SS3's R, producing the diagnostic
  # pattern: R[1977] off by ~36% while R[2024] matched exactly (b=0 there).
  sigma_R <- get_par(parlist$SR_parms, "SR_sigmaR")
  if (is.null(sigma_R)) sigma_R <- 0.6

  compute_bias_adj <- function(yr) {
    if (is.null(ctllist) || !isTRUE(ctllist$recdev_adv == 1)) return(rep(1.0, length(yr)))
    late0  <- ctllist$last_early_yr_nobias_adj
    first1 <- ctllist$first_yr_fullbias_adj
    last1  <- ctllist$last_yr_fullbias_adj
    first0 <- ctllist$first_recent_yr_nobias_adj
    bmax   <- ctllist$max_bias_adj
    sapply(yr, function(y) {
      if (y <= late0)  return(0)
      if (y <  first1) return(bmax * (y - late0)  / (first1 - late0))
      if (y <= last1)  return(bmax)
      if (y <  first0) return(bmax * (first0 - y) / (first0 - last1))
      return(0)
    })
  }

  rec_devs <- do.call(rbind, Filter(Negate(is.null), list(
    parlist$recdev_early,
    parlist$recdev1,
    parlist$recdev2
  )))
  if ("rec_dev" %in% names(inits) && !is.null(rec_devs)) {
    bias_adj_vec <- compute_bias_adj(years_hind)
    n_set <- 0
    for (i in seq_len(nrow(rec_devs))) {
      yr_pos <- which(years_hind == rec_devs[i, "year"])
      if (length(yr_pos)) {
        ba <- bias_adj_vec[yr_pos]
        inits$rec_dev[1, yr_pos] <- rec_devs[i, "recdev"] - 0.5 * ba * sigma_R^2
        n_set <- n_set + 1
      }
    }
    cat(sprintf("Recruitment deviates (bias-corrected, sigmaR=%.3f) set for %d years\n",
                sigma_R, n_set))
  } else if (!"rec_dev" %in% names(inits)) {
    warning("rec_dev not in inits — check names(mod0$estimated_params)")
  }

  # --- 3d. Von Bertalanffy growth ---
  # log_growth_pars[sp, sex, par]: par 1=log_K, 2=log_L1, 3=log_Linf, 4=log_m
  # Rceattle defines l1 = length AT minage(sp) (see growth.hpp:70).
  # SS3 stores L_at_Amin at `Growth_Age_for_L1` (from ctl, often 0.083 yr)
  # and L_at_Amax at `Growth_Age_for_L2` (often 999, meaning L_Amax = Linf).
  # We translate SS3's two reference lengths into:
  #   Linf       = L_Amax if amax_GP >= 99 (default 999 sentinel);
  #                else interpolated to true asymptote.
  #   L1_minage  = Linf - (Linf - L_Amin) * exp(-K * (minage - amin_GP))
  L_min_ss3 <- get_par(parlist$MG_parms, "L_at_Amin")
  L_max_ss3 <- get_par(parlist$MG_parms, "L_at_Amax")
  K_vb      <- get_par(parlist$MG_parms, "VonBert_K")
  SD_y      <- get_par(parlist$MG_parms, "CV_young")
  SD_o      <- get_par(parlist$MG_parms, "CV_old")

  # Pull SS3's growth reference ages from the ctl. Field names in SS_readctl
  # output may vary by version; check both common patterns.
  amin_gp <- ctllist$Growth_Age_for_L1 %||% ctllist$Amin_GP
  amax_gp <- ctllist$Growth_Age_for_L2 %||% ctllist$Amax_GP
  if (is.null(amin_gp) || is.null(amax_gp)) {
    warning("Growth_Age_for_L1/L2 not in ctllist — falling back to (minage, nages-1+minage). Linf likely wrong.")
    amin_gp <- data_list$minage[1]
    amax_gp <- data_list$minage[1] + data_list$nages[1] - 1
  }

  if (!is.null(L_min_ss3) && !is.null(L_max_ss3) && !is.null(K_vb) &&
      "log_growth_pars" %in% names(inits)) {
    minage_rce <- data_list$minage[1]

    Linf_est <- if (amax_gp >= 99) L_max_ss3
                else {
                  delta <- exp(-K_vb * (amax_gp - amin_gp))
                  (L_max_ss3 - L_min_ss3 * delta) / (1 - delta)
                }
    # Length at Rceattle's minage on the SS3 VB curve
    L1_rce <- Linf_est - (Linf_est - L_min_ss3) * exp(-K_vb * (minage_rce - amin_gp))

    cat(sprintf("Growth from SS3:\n  amin_GP=%.3f, amax_GP=%.3f, L_Amin=%.4f, L_Amax=%.4f, K=%.4f\n",
                amin_gp, amax_gp, L_min_ss3, L_max_ss3, K_vb))
    cat(sprintf("  -> Rceattle (l1 at minage=%d): l1=%.3f, Linf=%.3f, K=%.4f\n",
                minage_rce, L1_rce, Linf_est, K_vb))
    inits$log_growth_pars[1, 1, 1] <- log(K_vb)
    inits$log_growth_pars[1, 1, 2] <- log(L1_rce)
    inits$log_growth_pars[1, 1, 3] <- log(Linf_est)
  } else if (!"log_growth_pars" %in% names(inits)) {
    warning("log_growth_pars not in inits — add growthFun to mod0")
  }
  if (!is.null(SD_y) && "growth_log_sd" %in% names(inits))
    inits$growth_log_sd[1, 1, 1] <- log(SD_y)
  if (!is.null(SD_o) && "growth_log_sd" %in% names(inits))
    inits$growth_log_sd[1, 1, 2] <- log(SD_o)

  # Weight-length: W = alpha * L^beta — must match SS3 to get correct WAA from VB growth
  Wtlen_1 <- get_par(parlist$MG_parms, "Wtlen_1_Fem_GP_1")
  Wtlen_2 <- get_par(parlist$MG_parms, "Wtlen_2_Fem_GP_1")
  if (!is.null(Wtlen_1) && !is.null(Wtlen_2) && "weight_length_pars" %in% names(inits)) {
    inits$weight_length_pars[1, 1] <- Wtlen_1
    inits$weight_length_pars[1, 2] <- Wtlen_2
    cat(sprintf("Weight-length: alpha=%.6g, beta=%.4f\n", Wtlen_1, Wtlen_2))
  }

  # --- 3e. Catchability ---
  # index_log_q is a named vector, one entry per fleet (length = nfleets)
  if ("index_log_q" %in% names(inits)) {
    for (i in seq_len(nrow(fleet_meta))) {
      pattern <- sprintf("LnQ_base_%s\\(%d\\)$", fleet_meta$name[i], fleet_meta$ss3_num[i])
      q_val   <- get_par(parlist$Q_parms, pattern)
      if (!is.null(q_val)) {
        inits$index_log_q[i] <- q_val
        cat(sprintf("index_log_q[%s] set to %.4f (q = %.4f)\n",
                    fleet_meta$name[i], q_val, exp(q_val)))
      }
    }
  } else {
    warning("index_log_q not in inits — catchability not set")
  }

  # --- 3f. Selectivity (DoubleNormal, length-based, with blocks) ---
  inits$sel_inf[]         <- 0
  inits$log_sel_slp[]     <- 0
  inits$sel_inf_dev[]     <- 0
  inits$log_sel_slp_dev[] <- 0

  endyr <- max(years_hind)

  for (i in seq_len(nrow(fleet_meta))) {
    flt   <- fleet_meta$name[i]
    ss3id <- fleet_meta$ss3_num[i]

    # Helper: get a SS3 selectivity parameter for this fleet (with optional block suffix)
    get_sel <- function(ptype, blk_suffix = "") {
      pat <- sprintf("Size_DblN_%s_%s\\(%d\\)%s$", ptype, flt, ss3id, blk_suffix)
      get_par(parlist$S_parms, pat)
    }

    if (fleet_meta$has_blocks[i]) {
      # Identify all block-replacement start years for this fleet
      blk_rows <- grep(
        sprintf("%s\\(%d\\).*_BLK.*repl_\\d+$", flt, ss3id),
        rownames(parlist$S_parms)
      )
      blk_yr_matches <- regmatches(
        rownames(parlist$S_parms)[blk_rows],
        regexpr("repl_(\\d+)$", rownames(parlist$S_parms)[blk_rows])
      )
      blk_starts <- sort(unique(as.integer(sub("repl_", "", blk_yr_matches))))

      # Build complete block table: base block + SS3 replacement blocks
      all_starts <- c(min(years_hind), blk_starts)
      all_ends   <- c(blk_starts - 1L, endyr)

      # Fetch base parameters once (used as fallback when block doesn't re-estimate)
      base_vals <- list(
        peak = get_sel("peak"),
        p3   = get_sel("ascend_se"),
        p4   = get_sel("descend_se"),
        p6   = get_sel("end_logit")
      )

      for (b in seq_along(all_starts)) {
        yr_pos <- which(years_hind >= all_starts[b] & years_hind <= all_ends[b])
        if (length(yr_pos) == 0) next

        if (b == 1) {
          v <- base_vals
        } else {
          sfx <- sprintf("_BLK\\d+repl_%d", all_starts[b])
          v <- list(
            peak = get_sel("peak",       sfx) %||% base_vals$peak,
            p3   = get_sel("ascend_se",  sfx) %||% base_vals$p3,
            p4   = get_sel("descend_se", sfx) %||% base_vals$p4,
            p6   = get_sel("end_logit",  sfx) %||% base_vals$p6
          )
        }
        if (is.null(v$p4)) v$p4 <- v$p3   # failsafe

        inits$sel_inf_dev[1, i, 1, yr_pos]     <- v$peak %||% 0
        inits$sel_inf_dev[2, i, 1, yr_pos]     <- v$p6   %||% 0
        inits$log_sel_slp_dev[1, i, 1, yr_pos] <- ((v$p3  %||% 0) - log(2)) / 2
        inits$log_sel_slp_dev[2, i, 1, yr_pos] <- ((v$p4  %||% 0) - log(2)) / 2
      }

    } else {
      # Static fleet — set base params directly
      p3 <- get_sel("ascend_se")
      p4 <- get_sel("descend_se") %||% p3
      inits$sel_inf[1, i, 1]     <- get_sel("peak")      %||% 0
      inits$sel_inf[2, i, 1]     <- get_sel("end_logit") %||% 0
      inits$log_sel_slp[1, i, 1] <- ((p3 %||% 0) - log(2)) / 2
      inits$log_sel_slp[2, i, 1] <- ((p4 %||% 0) - log(2)) / 2
    }
  }

  inits
}

# Null-coalescing operator (base R doesn't have ??)
`%||%` <- function(x, y) if (!is.null(x)) x else y


# ============================================================================
# 3'. init_state_from_ss3_natage()
#     Hard-code the initial (styr) age structure from SS3's reported natage.
#     This isolates "forward dynamics" from "equilibrium derivation": if
#     biomass still drifts after this, the bug is in M/F/growth/rec_dev — not
#     in the initial conditions. Once the bridge is verified, switch back to
#     a parametric init (initMode = "NonEquilibrium" + init_dev from recdev_early).
#
#     Mechanism: with initMode = "FreeParams" (0), Rceattle C++ sets
#       N[sp, sex, age, 0] = exp(init_dev[sp, age-1]) * sex_ratio
#     for age > 0 (ages 2..nages in 1-indexed terms). For the recruit age
#     (age 1 = slot 0), N is still driven by R_init * exp(rec_dev[0]).
# ============================================================================
init_state_from_ss3_natage <- function(inits, ss3_rep, styr, nages, sex_ratio = 1.0) {
  # Age-class alignment (verified empirically):
  #   Rceattle slot 1 = SS3 age 0  (R[1977] = N[1,1977] = SS3 Recruit_0[1977])
  #   Rceattle slot k = SS3 age k-1   for k=1..nages-1
  #   Rceattle slot nages = PLUS GROUP, contains cohorts in their nages-th year
  #     of life or older. Per ceattle_v01_11.cpp:1329, slot nages propagates as
  #     N[nages] = N[nages-1, y-1]*S[nages-1] + N[nages, y-1]*S[nages].
  #   In SS3 with max_acc = nages, col "nages" IS already a plus group.
  #   To make Rceattle's slot-nages population match SS3's "nages-1 yr + older"
  #   biomass, sum SS3 cols (nages-1) and (nages) into Rceattle slot nages.
  ss3_age_cols <- as.character(0:nages)  # 0..nages, includes SS3 plus group
  natage_styr <- ss3_rep$natage %>%
    dplyr::filter(Yr == styr, `Beg/Mid` == "B", Sex == 1) %>%
    dplyr::slice(1)
  if (nrow(natage_styr) == 0) stop("SS3 natage missing row for styr = ", styr)

  available <- intersect(ss3_age_cols, colnames(natage_styr))
  if (length(available) < length(ss3_age_cols)) {
    warning(sprintf("SS3 natage missing some requested cols. Have [%s]; need [%s].",
                    paste(available, collapse = ","),
                    paste(ss3_age_cols, collapse = ",")))
  }
  ss3_N_raw <- as.numeric(natage_styr[1, ss3_age_cols])  # length = nages + 1

  # Build the Rceattle-aligned vector (length nages):
  #   slot 1..(nages-1) <- SS3 ages 0..(nages-2) (one-to-one)
  #   slot nages         <- SS3 age (nages-1) + SS3 plus group (age nages)
  ss3_N_aligned <- numeric(nages)
  ss3_N_aligned[1:(nages - 1)] <- ss3_N_raw[1:(nages - 1)]
  ss3_N_aligned[nages]         <- ss3_N_raw[nages] + ss3_N_raw[nages + 1]

  cat(sprintf("\nSS3 natage[%d] raw cols 0..%d:    %s\n", styr, nages,
              paste(sprintf("%.4g", ss3_N_raw), collapse = ", ")))
  cat(sprintf("Aligned -> Rceattle slots 1..%d: %s\n", nages,
              paste(sprintf("%.4g", ss3_N_aligned), collapse = ", ")))
  cat(sprintf("(Slot %d = SS3 age %d + SS3 plus group, accounting for Rceattle's plus-group convention)\n",
              nages, nages - 1))

  if (!"init_dev" %in% names(inits)) {
    warning("init_dev not in inits — Rceattle build may not support FreeParams init")
    return(inits)
  }
  # Under initMode = FreeParams (C++ case initMode==0):
  #   N_at_age[sp, sex, age_cpp, 0] = exp(init_dev[sp, age_cpp - 1])
  # for C++ age_cpp > 0. C++ age_cpp 0 = Rceattle slot 1 = recruit, set by rec_dev.
  # So init_dev[sp, k] for k = 1..nages-1 maps to Rceattle slots 2..nages.
  for (k in seq_len(nages - 1)) {
    inits$init_dev[1, k] <- log(max(ss3_N_aligned[k + 1] / sex_ratio, 1e-10))
  }
  cat(sprintf("init_dev[1, 1:%d] = [%s]\n", nages - 1,
              paste(sprintf("%.3f", inits$init_dev[1, 1:(nages - 1)]), collapse = ", ")))
  inits
}


# ============================================================================
# 4.  Build SS3-approximation data object
# ============================================================================
cod_ss3 <- cod_caal
years_hind <- cod_ss3$styr:cod_ss3$endyr

# Override fleet_control
cod_ss3$fleet_control$Selectivity           <- "DoubleNormal"
cod_ss3$fleet_control$Selectivity_dimension <- "Length"
cod_ss3$fleet_control$Time_varying_sel      <- ifelse(fleet_meta$has_blocks, "Block", 0)
cod_ss3$fleet_control$Catchability[cod_ss3$fleet_control$Fleet_type == "Survey"] <- "Estimated"

# Derive block break years automatically from the SS3 control file.
# ctllist_ss3$S_parms has a "Block" column (block design index) for each
# selectivity parameter; ctllist_ss3$Block_Design[[idx]] is a vector of
# alternating start-end year pairs for that design.
get_blk_breaks <- function(fleet_name, ss3_num, ctllist, styr) {
  pat <- sprintf("SizeSel_P1_%s\\(%d\\)$", fleet_name, ss3_num)
  idx <- grep(pat, rownames(ctllist$S_parms))
  if (length(idx) == 0) return(styr)
  bd_idx <- suppressWarnings(as.integer(ctllist$S_parms[idx[1], "Block"]))
  if (is.na(bd_idx) || bd_idx == 0 || bd_idx > length(ctllist$Block_Design)) return(styr)
  bd         <- as.numeric(ctllist$Block_Design[[bd_idx]])
  blk_starts <- bd[seq(1, length(bd), by = 2)]
  c(styr, blk_starts)
}

ss3_blk_breaks <- setNames(
  lapply(seq_len(nrow(fleet_meta)), function(i) {
    get_blk_breaks(fleet_meta$name[i], fleet_meta$ss3_num[i], ctllist_ss3, cod_ss3$styr)
  }),
  fleet_meta$name
)
cat("Derived block breaks:\n"); print(ss3_blk_breaks)

assign_blocks <- function(df, flt_code, breaks, styr, endyr) {
  rows <- which(df$Fleet_code == flt_code & df$Year >= styr & df$Year <= endyr)
  if (length(rows) == 0) return(df)
  if (!"Selectivity_block" %in% names(df)) df$Selectivity_block <- NA_integer_
  df$Selectivity_block[rows] <- findInterval(df$Year[rows], breaks)
  df
}

for (i in seq_len(nrow(fleet_meta))) {
  flt_code <- cod_ss3$fleet_control$Fleet_code[i]
  breaks   <- ss3_blk_breaks[[fleet_meta$name[i]]]
  if (fleet_meta$fleet_type[i] == "Fishery") {
    cod_ss3$catch_data <- assign_blocks(
      cod_ss3$catch_data, flt_code, breaks, cod_ss3$styr, cod_ss3$endyr)
  } else if (fleet_meta$has_blocks[i]) {
    cod_ss3$index_data <- assign_blocks(
      cod_ss3$index_data, flt_code, breaks, cod_ss3$styr, cod_ss3$endyr)
  }
}


# ============================================================================
# 4b. Empirical SS3 selectivity-at-age injection
#     Bypass parametric Double Normal: feed SS3's already-evaluated age
#     selectivity matrix from Report.sso directly into Rceattle via emp_sel.
#     This sidesteps the missing P2/P5 parameters entirely. The selectivity
#     term in the population dynamics becomes identical to SS3 by construction.
# ============================================================================
ss3_rep <- r4ss::SS_output(dir = SS3_DIR, verbose = FALSE, printstats = FALSE)

# ageselex Factors: "Asel"  = pre-retention age selectivity (used to scale F_full).
#                   "Asel2" = post-retention. Identical to Asel when no retention.
asel <- ss3_rep$ageselex %>%
  dplyr::filter(Factor == "Asel",
                Yr %in% years_hind,
                Fleet %in% fleet_meta$ss3_num)

a_min <- cod_ss3$minage[1]
a_max <- a_min + cod_ss3$nages[1] - 1
age_cols_ss3 <- as.character(a_min:a_max)
stopifnot(all(age_cols_ss3 %in% colnames(asel)))

build_emp_sel_row <- function(fleet_code, sp, sex, yr, sel_vec, fleet_name) {
  out <- data.frame(
    Fleet_name = fleet_name,
    Fleet_code = as.integer(fleet_code),
    Species    = as.integer(sp),
    Sex        = as.integer(sex),
    Year       = as.integer(yr),
    stringsAsFactors = FALSE
  )
  comp <- as.list(sel_vec)
  names(comp) <- paste0("Comp_", seq_along(sel_vec))
  cbind(out, as.data.frame(comp))
}

emp_sel_rows <- list()
for (i in seq_len(nrow(fleet_meta))) {
  flt_code <- cod_ss3$fleet_control$Fleet_code[i]
  ss3_num  <- fleet_meta$ss3_num[i]
  fleet_nm <- fleet_meta$name[i]
  sub <- asel %>% dplyr::filter(Fleet == ss3_num)
  for (yr in years_hind) {
    row_y <- sub %>% dplyr::filter(Yr == yr)
    if (nrow(row_y) == 0) next  # SS3 only emits rows on block-change years
    sel_vec <- as.numeric(row_y[1, age_cols_ss3])
    emp_sel_rows[[length(emp_sel_rows) + 1]] <-
      build_emp_sel_row(flt_code, sp = 1, sex = 0, yr = yr,
                        sel_vec = sel_vec, fleet_name = fleet_nm)
  }
}
cod_ss3$emp_sel <- do.call(rbind, emp_sel_rows)

# SS3 ageselex writes a row only when selectivity changes (block switches).
# Rceattle's emp_sel lookup is exact-year, so forward-fill every hindcast year.
cod_ss3$emp_sel <- cod_ss3$emp_sel %>%
  tidyr::complete(tidyr::nesting(Fleet_name, Fleet_code, Species, Sex),
                  Year = years_hind) %>%
  dplyr::arrange(Fleet_code, Year) %>%
  dplyr::group_by(Fleet_code) %>%
  tidyr::fill(dplyr::starts_with("Comp_"), .direction = "down") %>%
  dplyr::ungroup() %>%
  as.data.frame()

# Route every fleet through the empirical path. Sel_norm_bin1 = NA disables
# the divide-by-max normalization in selectivity.hpp:62 (becomes -999 -> skip),
# so the SS3 values flow through verbatim.
cod_ss3$fleet_control$Selectivity           <- "Fixed"
cod_ss3$fleet_control$Time_varying_sel      <- 0
cod_ss3$fleet_control$Sel_norm_bin1         <- NA
cod_ss3$fleet_control$Sel_norm_bin2         <- NA
cod_ss3$fleet_control$Selectivity_dimension <- "Age"

cat(sprintf("emp_sel built: %d rows across %d fleets, %d years\n",
            nrow(cod_ss3$emp_sel), nrow(fleet_meta), length(years_hind)))

# initMode must be set BEFORE fit_mod() so the parameter structure (init_dev
# sizing and map) is built correctly. "FreeParams" lets us inject SS3 natage
# directly; "NonEquilibrium" is the standard Rceattle equilibrium-with-devs.
if (USE_SS3_INITIAL_NATAGE) cod_ss3$initMode <- "FreeParams"


# ============================================================================
# 5.  Get Rceattle parameter structure, then fill from SS3 par file
# ============================================================================
mod0 <- Rceattle::fit_mod(
  data_list    = cod_ss3,
  inits        = NULL,
  file         = NULL,
  estimateMode = 3,
  initMode     = if (USE_SS3_INITIAL_NATAGE) "FreeParams" else "NonEquilibrium",
  growthFun    = build_growth(fun = 1),
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = FALSE
)

cat("\nRceattle parameter names:\n", paste(names(mod0$estimated_params), collapse = ", "), "\n")

inits <- init_from_ss3_par(
  parlist    = parlist,
  inits      = mod0$estimated_params,
  data_list  = cod_ss3,
  fleet_meta = fleet_meta,
  years_hind = years_hind,
  ctllist    = ctllist_ss3
)

# Diagnostic injection of SS3's exact year-1 N-at-age (bypasses Rceattle's
# equilibrium derivation). The data-side initMode was already set above.
if (USE_SS3_INITIAL_NATAGE) {
  inits <- init_state_from_ss3_natage(
    inits     = inits,
    ss3_rep   = ss3_rep,
    styr      = cod_ss3$styr,
    nages     = cod_ss3$nages[1],
    sex_ratio = 1.0  # single-sex Pcod
  )
}


# ============================================================================
# 6.  Model runs
# ============================================================================

# Model A: All parameters fixed at SS3 values (forward pass — no optimisation)
cod_ss3_fixed <- Rceattle::fit_mod(
  data_list    = cod_ss3,
  inits        = inits,
  file         = NULL,
  estimateMode = 3,
  initMode     = if (USE_SS3_INITIAL_NATAGE) "FreeParams" else "NonEquilibrium",
  growthFun    = build_growth(fun = 1),
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = FALSE
)

# Model B: Estimate from SS3 starting values
cod_ss3_est <- Rceattle::fit_mod(
  data_list    = cod_ss3,
  inits        = NULL,
  file         = NULL,
  estimateMode = 0,
  growthFun    = build_growth(fun = 1),
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = TRUE
)

# Model C: Baseline (empirical WAA, logistic sel, block M) for context
cod_base <- Rceattle::fit_mod(
  data_list    = cod_caal,
  inits        = NULL,
  file         = NULL,
  estimateMode = 0,
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = TRUE
)


# ============================================================================
# 7.  SS3 reference time series
# ============================================================================
ts_ss3 <- read.csv(TS_FILE)

safe2024 <- cod_ss3_fixed
safe2024$quantities$biomass[, 1:length(years_hind)] <-
  ts_ss3 %>% filter(Yr %in% years_hind) %>% pull(Bio_all)
safe2024$quantities$ssb[, 1:length(years_hind)] <-
  ts_ss3 %>% filter(Yr %in% years_hind) %>% pull(SpawnBio)
safe2024$quantities$R[, 1:length(years_hind)] <-
  ts_ss3 %>% filter(Yr %in% years_hind) %>% pull(Recruit_0)


# ============================================================================
# 8.  Relative-error diagnostics vs SS3
# ============================================================================
ss3_R   <- ts_ss3 %>% filter(Yr %in% years_hind) %>% pull(Recruit_0)
ss3_ssb <- ts_ss3 %>% filter(Yr %in% years_hind) %>% pull(SpawnBio)
ss3_bio <- ts_ss3 %>% filter(Yr %in% years_hind) %>% pull(Bio_all)

diag_errors <- function(mod, label) {
  ny  <- length(years_hind)
  R   <- as.numeric(mod$quantities$R[1, 1:ny])
  ssb <- as.numeric(mod$quantities$ssb[1, 1:ny])
  bio <- as.numeric(mod$quantities$biomass[1, 1:ny])
  rel <- function(est, ref) abs(est - ref) / pmax(abs(ref), 1e-10)
  cat(sprintf("\n--- %s ---\n", label))
  cat(sprintf("  R   max rel err: %.2e  (mean: %.2e)\n", max(rel(R, ss3_R)),   mean(rel(R, ss3_R))))
  cat(sprintf("  SSB max rel err: %.2e  (mean: %.2e)\n", max(rel(ssb, ss3_ssb)), mean(rel(ssb, ss3_ssb))))
  cat(sprintf("  Bio max rel err: %.2e  (mean: %.2e)\n", max(rel(bio, ss3_bio)), mean(rel(bio, ss3_bio))))
}
cat("\n=== Relative errors vs SS3 ===")
diag_errors(cod_ss3_fixed, "cod_ss3_fixed")
diag_errors(cod_ss3_est,   "cod_ss3_est")

# ============================================================================
# 8b. Vector diagnostics: WAA and Sel-at-Age vs SS3
#     Isolates which demographic vector is driving any residual error above
#     the 1e-3 biomass tolerance. Run after cod_ss3_fixed is built.
# ============================================================================

# ----------------------------------------------------------------------------
# 8b.0  Sanity checks — did the bridge actually wire up correctly?
# ----------------------------------------------------------------------------
cat("\n=== Bridge sanity check ===\n")
cat("Rceattle initMode in fit output (need integer 0 if FreeParams):\n")
print(cod_ss3_fixed$data_list$initMode)

cat("\nSS3 natage age columns available (looking for '0' to confirm minage convention):\n")
nat_age_cols <- grep("^[0-9]+$", colnames(ss3_rep$natage), value = TRUE)
print(nat_age_cols)

cat("\nFirst few rows of parlist$MG_parms (sanity-check labels vs values):\n")
print(head(parlist$MG_parms[, c("INIT", "ESTIM")], 12))

cat("\nGrowth reference ages from ctllist:\n")
cat(sprintf("  Growth_Age_for_L1 = %s\n", ctllist_ss3$Growth_Age_for_L1 %||% NA))
cat(sprintf("  Growth_Age_for_L2 = %s\n", ctllist_ss3$Growth_Age_for_L2 %||% NA))
cat(sprintf("emp_sel rows: %d (expect %d = %d fleets x %d years)\n",
            nrow(cod_ss3_fixed$data_list$emp_sel),
            nrow(fleet_meta) * length(years_hind),
            nrow(fleet_meta), length(years_hind)))

ss3_R0   <- exp(parlist$SR_parms["SR_LN(R0)", "ESTIM"])
rce_logR0 <- cod_ss3_fixed$estimated_params$rec_pars[1, 1]
cat(sprintf("\nR0 scale check:\n  SS3 R0           = %.4g\n  Rceattle log(R0) = %.4g  =>  R0 = %.4g\n",
            ss3_R0, rce_logR0, exp(rce_logR0)))
cat(sprintf("  Ratio Rceattle / SS3 = %.4g  (units suspect if not ~1, ~1000, or ~1e-3)\n",
            exp(rce_logR0) / ss3_R0))

cat(sprintf("\nFirst-year recruitment comparison (year %d):\n", years_hind[1]))
cat(sprintf("  SS3 Recruit_0[%d] = %.4g\n", years_hind[1], ss3_R[1]))
cat(sprintf("  Rceattle R[1,1]  = %.4g\n",
            as.numeric(cod_ss3_fixed$quantities$R[1, 1])))
cat(sprintf("  Ratio Rceattle / SS3 = %.4g\n",
            as.numeric(cod_ss3_fixed$quantities$R[1, 1]) / ss3_R[1]))

# Year-by-year R for the first 5 and last 5 hindcast years
ny_show <- min(5, length(years_hind))
r_tbl <- data.frame(
  Year     = c(head(years_hind, ny_show), tail(years_hind, ny_show)),
  SS3_R    = c(head(ss3_R,    ny_show), tail(ss3_R,    ny_show)),
  Rceattle = c(head(as.numeric(cod_ss3_fixed$quantities$R[1, ]), ny_show),
               tail(as.numeric(cod_ss3_fixed$quantities$R[1, 1:length(years_hind)]), ny_show)),
  Ratio    = NA_real_
)
r_tbl$Ratio <- r_tbl$Rceattle / r_tbl$SS3_R
cat("\nRecruitment series (head + tail):\n")
print(r_tbl)


# ----------------------------------------------------------------------------
# 8b.1  Selectivity-at-age comparison (slot is $sel_at_age, not $sel)
# ----------------------------------------------------------------------------
cat("\n--- Rceattle quantities slot probe ---\n")
print(grep("sel|wt|weight|N_at|biomass",
           names(cod_ss3_fixed$quantities),
           value = TRUE, ignore.case = TRUE))
cat("dim(sel_at_age):", dim(cod_ss3_fixed$quantities$sel_at_age), "\n")
cat("dim(weight_hat):", dim(cod_ss3_fixed$quantities$weight_hat), "\n")

rce_sel <- cod_ss3_fixed$quantities$sel_at_age  # (fleet, sex, age, year)

sel_compare <- list()
for (i in seq_len(nrow(fleet_meta))) {
  ss3_num <- fleet_meta$ss3_num[i]
  ss3_sub <- ss3_rep$ageselex %>%
    dplyr::filter(Factor == "Asel", Fleet == ss3_num, Yr %in% years_hind)

  for (yi in seq_along(years_hind)) {
    yr <- years_hind[yi]
    ss3_row <- ss3_sub %>% dplyr::filter(Yr == yr)
    if (nrow(ss3_row) == 0) next
    ss3_vec <- as.numeric(ss3_row[1, age_cols_ss3])
    rce_vec <- as.numeric(rce_sel[i, 1, , yi])
    if (length(rce_vec) != length(ss3_vec)) {
      message(sprintf("Skipping fleet %s yr %d: rce_vec len %d vs ss3 len %d",
                      fleet_meta$name[i], yr, length(rce_vec), length(ss3_vec)))
      next
    }
    rel <- abs(rce_vec - ss3_vec) / pmax(abs(ss3_vec), 1e-10)
    sel_compare[[length(sel_compare) + 1]] <- data.frame(
      Fleet    = fleet_meta$name[i],
      Year     = yr,
      Age      = a_min:a_max,
      SS3      = ss3_vec,
      Rceattle = rce_vec,
      RelErr   = rel
    )
  }
}
sel_compare <- do.call(rbind, sel_compare)
cat("\n--- Selectivity-at-age max relative error by fleet ---\n")
print(sel_compare %>% dplyr::group_by(Fleet) %>%
        dplyr::summarise(max_rel = max(RelErr), mean_rel = mean(RelErr)))


# ----------------------------------------------------------------------------
# 8b.2  Weight-at-age comparison ($weight_hat)
#       weight_hat[i, sex, age, yr]: i=1..nspp = start-of-year pop WAA,
#                                    i=nspp+1..2*nspp = SSB WAA,
#                                    i=2*nspp+1.. = per-fleet WAA
# ----------------------------------------------------------------------------
nspp <- 1
rce_waa_pop <- cod_ss3_fixed$quantities$weight_hat[1, 1, , 1:length(years_hind)]
rce_waa_ssb <- cod_ss3_fixed$quantities$weight_hat[1 + nspp, 1, , 1:length(years_hind)]

# The Pcod 2024 model derives WAA from VB growth + W-L (no empirical wtatage),
# so ss3_rep$wtatage is empty. Use ss3_rep$endgrowth instead, which has the
# derived weight-at-age at each integer age. This is time-invariant for VB.
if (!is.null(ss3_rep$endgrowth) && nrow(ss3_rep$endgrowth) > 0) {
  ss3_waa_vec <- ss3_rep$endgrowth %>%
    dplyr::filter(Sex == 1, int_Age %in% (a_min:a_max)) %>%
    dplyr::arrange(int_Age) %>%
    dplyr::pull(Wt_Beg)  # start-of-year weight; also Wt_Mid available
  cat(sprintf("\nSS3 endgrowth WAA (start-of-year, ages %d:%d): %s\n",
              a_min, a_max, paste(sprintf("%.3f", ss3_waa_vec), collapse = ", ")))
  rce_waa_yr1 <- rce_waa_pop[, 1]
  cat(sprintf("Rceattle WAA year 1 (slot 1):                   %s\n",
              paste(sprintf("%.3f", rce_waa_yr1), collapse = ", ")))
  if (length(ss3_waa_vec) == length(rce_waa_yr1)) {
    waa_rel <- abs(rce_waa_yr1 - ss3_waa_vec) / pmax(abs(ss3_waa_vec), 1e-10)
    cat(sprintf("Pop WAA (yr 1): max rel err %.2e  mean %.2e\n",
                max(waa_rel), mean(waa_rel)))
  }
} else {
  cat("\nss3_rep$endgrowth not available — skipping WAA comparison.\n")
}


# ----------------------------------------------------------------------------
# 8b.3  Initial age structure comparison (year 1)
#       If biomass is 4x off but R is only 35% off, the divergence is most
#       likely cumulative — driven by wrong initial N-at-age in year 1.
# ----------------------------------------------------------------------------
rce_N1 <- as.numeric(cod_ss3_fixed$quantities$N_at_age[1, 1, , 1])
# Rceattle slot k <- SS3 age (k-1), with the plus group at slot nages being
# the sum of SS3 ages (nages-1) and nages (where SS3 col "nages" is plus group).
nages_pcod <- cod_ss3$nages[1]
ss3_natage_row <- ss3_rep$natage %>%
  dplyr::filter(Yr == years_hind[1], `Beg/Mid` == "B", Sex == 1) %>%
  dplyr::slice(1)
ss3_N1_raw <- as.numeric(ss3_natage_row[1, as.character(0:nages_pcod)])
ss3_N1     <- c(ss3_N1_raw[1:(nages_pcod - 1)],
                ss3_N1_raw[nages_pcod] + ss3_N1_raw[nages_pcod + 1])

cat(sprintf("\nInitial N-at-age (year %d) [Rceattle slot 1..%d vs aligned SS3]:\n",
            years_hind[1], nages_pcod))
n1_tbl <- data.frame(
  Slot = 1:nages_pcod,
  SS3_align = ss3_N1,
  Rceattle  = rce_N1,
  Ratio     = rce_N1 / pmax(ss3_N1, 1e-10),
  RelErr    = abs(rce_N1 - ss3_N1) / pmax(abs(ss3_N1), 1e-10)
)
print(n1_tbl)
cat(sprintf("Initial N max rel err: %.2e\n", max(n1_tbl$RelErr)))


# ----------------------------------------------------------------------------
# 8b.4  Direct biomass / SSB comparison — diagnose the 97% mean error.
#       Confirms unit alignment AND whether the error is in year 1 or
#       accumulates over time.
# ----------------------------------------------------------------------------
cat("\n--- Direct biomass / SSB value comparison (first 5 + last 5 yrs) ---\n")
ny <- length(years_hind)
bio_rce <- as.numeric(cod_ss3_fixed$quantities$biomass[1, 1:ny])
ssb_rce <- as.numeric(cod_ss3_fixed$quantities$ssb[1, 1:ny])
bio_ss3 <- ss3_bio
ssb_ss3 <- ss3_ssb

bio_tbl <- data.frame(
  Year    = c(head(years_hind, 5), tail(years_hind, 5)),
  Bio_SS3 = c(head(bio_ss3, 5),    tail(bio_ss3, 5)),
  Bio_Rce = c(head(bio_rce, 5),    tail(bio_rce, 5)),
  Ratio   = c(head(bio_rce / bio_ss3, 5), tail(bio_rce / bio_ss3, 5)),
  SSB_SS3 = c(head(ssb_ss3, 5),    tail(ssb_ss3, 5)),
  SSB_Rce = c(head(ssb_rce, 5),    tail(ssb_rce, 5)),
  SSB_Ratio = c(head(ssb_rce / ssb_ss3, 5), tail(ssb_rce / ssb_ss3, 5))
)
print(bio_tbl)

# Compute year-1 biomass manually from N and WAA to cross-check:
n1_vec   <- as.numeric(cod_ss3_fixed$quantities$N_at_age[1, 1, , 1])
waa1_pop <- as.numeric(cod_ss3_fixed$quantities$weight_hat[1, 1, , 1])  # slot 1 = wt_idx_pop
waa1_ssb <- as.numeric(cod_ss3_fixed$quantities$weight_hat[2, 1, , 1])  # slot 2 = wt_idx_ssb
manual_bio_1 <- sum(n1_vec * waa1_pop)
cat(sprintf("\nYear 1 hand calc:  sum(N * WAA_pop) = %.4g\n", manual_bio_1))
cat(sprintf("Year 1 Rceattle:   biomass[1, 1]    = %.4g\n", bio_rce[1]))
cat(sprintf("Year 1 SS3 ts:     Bio_all[1977]    = %.4g\n", bio_ss3[1]))
cat(sprintf("\nweight_hat dim 1 (which 'WAA slot' is which):\n"))
for (i in 1:dim(cod_ss3_fixed$quantities$weight_hat)[1]) {
  cat(sprintf("  slot %d, age 1-3 in yr 1: %.4f, %.4f, %.4f\n",
              i,
              cod_ss3_fixed$quantities$weight_hat[i, 1, 1, 1],
              cod_ss3_fixed$quantities$weight_hat[i, 1, 2, 1],
              cod_ss3_fixed$quantities$weight_hat[i, 1, 3, 1]))
}

# Also check whether cod_caal had empirical weight data that may be overriding
cat(sprintf("\ncod_ss3$weight rows: %d\n",
            if (is.null(cod_ss3$weight)) 0L else nrow(cod_ss3$weight)))
if (!is.null(cod_ss3$weight) && nrow(cod_ss3$weight) > 0) {
  cat("First few rows:\n"); print(head(cod_ss3$weight))
}


# ============================================================================
# 9.  Comparison plots
# ============================================================================
model_list  <- list(cod_ss3_fixed, cod_ss3_est, cod_base, safe2024)
model_names <- c("Rceattle (SS3 fixed)", "Rceattle (estimated)", "Rceattle (base)", "SS3")

plot_biomass(model_list,     model_names = model_names)
plot_ssb(model_list,         model_names = model_names)
plot_recruitment(model_list, model_names = model_names)
plot_selectivity(list(cod_ss3_fixed, safe2024),
                 model_names = c("Rceattle (SS3 fixed)", "SS3"))
