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


# Block-M workaround (binary linkage covariate on log_M1):
#   M(yr) = exp(log_M_base + beta * m_block(yr))
#
# SS3 block design 4 from the ctl is [start=2014, end=2016] — the M block
# applies ONLY during 2014-2016 (the marine heatwave period). After 2016, M
# reverts to base. Our previous "post2014" indicator (year >= 2014) was wrong:
# it kept M=0.817 forever, causing biomass to collapse starting in 2018.
# Fixed: indicator = 1 only when year is in [2014, 2016]. Name kept as
# "post2014" for downstream column lookup compatibility — but it really
# means "in the 2014-2016 M-block window".
#
# IMPORTANT: use all = TRUE (outer join). cod_caal$env_data may not span the
# full model period (e.g., CFSR_2022 starts in 1979 for this Pcod model).
# With the default inner join, missing years drop out of env_data → Rceattle
# builds linkage_X from a too-short env_data → positional indexing shifts the
# block transition EARLIER than it should (M jumped at 2012 instead of 2014).
m_block_yrs <- ctllist_ss3$Block_Design[[4]]  # [2014, 2016] for Pcod 2024
cat(sprintf("\nM block 4 from ctl spans years %d-%d\n",
            m_block_yrs[1], m_block_yrs[2]))
cod_caal$env_data <- merge(
  cod_caal$env_data,
  data.frame(
    Year     = cod_caal$styr:cod_caal$endyr,
    post2014 = as.integer((cod_caal$styr:cod_caal$endyr) >= m_block_yrs[1] &
                          (cod_caal$styr:cod_caal$endyr) <= m_block_yrs[2])
  ),
  by = "Year", all = TRUE
)
# Fill any pre-1979 NA's in other env columns (e.g., CFSR_2022) so the merged
# env_data has no NA's downstream. Forward-fill from the first non-NA value
# is fine since these covariates aren't used outside the M linkage.
for (col in setdiff(colnames(cod_caal$env_data), c("Year", "post2014"))) {
  v <- cod_caal$env_data[[col]]
  if (any(is.na(v))) {
    first_nonNA <- v[which(!is.na(v))[1]]
    v[is.na(v)] <- first_nonNA
    cod_caal$env_data[[col]] <- v
  }
}
cod_caal$env_data <- cod_caal$env_data[order(cod_caal$env_data$Year), ]
M1_block <- build_M1(
  M1_model = 1, M1_use_prior = FALSE, M2_use_prior = FALSE,
  # `- 1` drops the stats::model.matrix() intercept column. Without it the
  # design matrix has [(Intercept), post2014], and beta_linkage[1] multiplies
  # the all-ones intercept column — shifting M in EVERY year (we observed
  # M = 0.817 in 1977 and 2014 instead of 0.493 → 0.817).
  linkages = list(log_M1 = linkage_spec(formula = ~ post2014 - 1, by = ~ species))
)




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
  # SS3 3.30 label is "NatM_p_1_Fem_GP_1" (not "NatM_uniform_..."). The block
  # parameter appears as "NatM_p_1_Fem_GP_1_BLK4repl_2014" in this model.
  # The earlier "_uniform_" pattern silently missed both rows, leaving M
  # constant at 0.493 instead of dropping to ~0.376 post-2014.
  M_base <- get_par(parlist$MG_parms, "NatM_p_1_Fem_GP_1$")
  M_blk  <- get_par(parlist$MG_parms, "NatM_p_1_Fem_GP_1_BLK")
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

  # --- 3g. Fishing mortality (F) ---
  #   SS3 conditions catch on F via a hybrid Pope/Newton solver — there are no
  #   F parameters in ss3.par. We extract the resolved F-at-age and inject it
  #   into Rceattle's log_F.
  #
  #   Rceattle parameterisation (verified vs ceattle_v01_11.cpp): log_F is
  #   per-fleet x per-hindcast-year. F_at_age[flt, sex, age, yr] = sel_at_age *
  #   exp(log_F[flt, yr]).
  #
  #   Source preference order:
  #     1. ss3_rep$fatage  (F-at-age by fleet/year, includes seasonal & retention)
  #     2. ss3_rep$exploitation  (apical F per fleet/year)
  #   We use (1) when present and reduce to per-fleet apical F = max over ages
  #   (the F that multiplies sel-at-age in the Baranov eq).
  if ("log_F" %in% names(inits)) {
    inits$log_F[] <- log(1e-8)   # near-zero floor (safe default for years with no F)

    have_fatage  <- !is.null(ss3_rep$fatage) && nrow(ss3_rep$fatage) > 0
    have_exploit <- !is.null(ss3_rep$exploitation) && nrow(ss3_rep$exploitation) > 0

    if (have_fatage) {
      age_cols <- as.character(data_list$minage[1]:(data_list$minage[1] + data_list$nages[1] - 1))
      age_cols <- intersect(age_cols, colnames(ss3_rep$fatage))
      for (i in seq_len(nrow(fleet_meta))) {
        sub <- ss3_rep$fatage %>%
          dplyr::filter(Fleet == fleet_meta$ss3_num[i],
                        Yr %in% years_hind)
        if (nrow(sub) == 0) next
        for (yi in seq_along(years_hind)) {
          yr  <- years_hind[yi]
          row <- sub %>% dplyr::filter(Yr == yr)
          if (nrow(row) == 0) next
          fage <- as.numeric(row[1, age_cols])
          fmax <- max(fage, na.rm = TRUE)
          if (is.finite(fmax) && fmax > 0) inits$log_F[i, yi] <- log(fmax)
        }
        cat(sprintf("log_F[%s] set from ss3_rep$fatage (mean F = %.4f)\n",
                    fleet_meta$name[i],
                    exp(mean(inits$log_F[i, ]))))
      }
    } else if (have_exploit) {
      exp_long <- ss3_rep$exploitation %>%
        dplyr::filter(Yr %in% years_hind) %>%
        tidyr::pivot_longer(cols = -c(Yr, Seas), names_to = "Fleet_name", values_to = "F")
      for (i in seq_len(nrow(fleet_meta))) {
        sub <- exp_long %>% dplyr::filter(Fleet_name == fleet_meta$name[i])
        for (yi in seq_along(years_hind)) {
          f <- sub$F[sub$Yr == years_hind[yi]]
          if (length(f) && is.finite(f[1]) && f[1] > 0) inits$log_F[i, yi] <- log(f[1])
        }
        cat(sprintf("log_F[%s] set from ss3_rep$exploitation (mean F = %.4f)\n",
                    fleet_meta$name[i],
                    exp(mean(inits$log_F[i, ]))))
      }
    } else {
      warning("Neither ss3_rep$fatage nor ss3_rep$exploitation available — log_F left at floor.")
    }
  } else {
    warning("log_F not in inits — F not injected. Catch likelihood will not match SS3.")
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
# Initialize log_F[fleet, year] from SS3's ts file (columns "F:_1", "F:_2", ...).
# Under estimateMode = 3, Rceattle holds log_F at the initial value — but only if
# we set it explicitly. Otherwise Rceattle's default initializer (Pope's-derived
# from catches) leaves log_F at values that drift from SS3's F as biomass diverges,
# producing the compounding-error pattern we saw (1.0 -> 0.13 over 47 years).
init_log_F_from_ss3 <- function(inits, ts_ss3, fleet_meta, years_hind) {
  if (!"log_F" %in% names(inits)) {
    warning("log_F not in inits — cannot pin Rceattle F to SS3 values")
    return(inits)
  }
  log_F <- inits$log_F  # dim should be (n_fishery, nyrs) or (n_fleet, nyrs)
  ts_sub <- ts_ss3[match(years_hind, ts_ss3$Yr), ]

  # read.csv mangles "F:_1" -> "F._1" under check.names = TRUE. Resolve the
  # actual F column for each fleet using a regex on the SS3 ts column names.
  ts_f_cols <- grep("^F[._:]_[0-9]+$|^F\\.\\._[0-9]+$|^F\\._[0-9]+$",
                    colnames(ts_sub), value = TRUE)
  if (length(ts_f_cols) == 0) {
    warning("No SS3 F:_n columns detected in ts file — log_F not pinned.")
    return(inits)
  }
  cat(sprintf("  Detected ts_ss3 F columns: %s\n", paste(ts_f_cols, collapse = ", ")))

  for (i in seq_len(nrow(fleet_meta))) {
    if (fleet_meta$fleet_type[i] != "Fishery") next
    if (fleet_meta$ss3_num[i] > length(ts_f_cols)) {
      warning(sprintf("Fleet %s (ss3_num=%d) exceeds detected F columns (%d)",
                      fleet_meta$name[i], fleet_meta$ss3_num[i], length(ts_f_cols)))
      next
    }
    f_col <- ts_f_cols[fleet_meta$ss3_num[i]]
    f_vec <- as.numeric(ts_sub[[f_col]])
    f_vec[is.na(f_vec) | f_vec <= 0] <- 1e-9  # floor for log(0) safety
    log_F[i, seq_along(years_hind)] <- log(f_vec)
    cat(sprintf("  log_F[%s] <- log(ts_ss3$%s): F yr1=%.4g, F mid=%.4g, F last=%.4g\n",
                fleet_meta$name[i], f_col,
                f_vec[1], f_vec[length(f_vec) %/% 2], tail(f_vec, 1)))
  }
  inits$log_F <- log_F
  inits
}


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

# ageselex Factors (SS3 3.30):
#   "Asel"  = INPUT age-based selectivity. For LENGTH-BASED selectivity
#             (sel_type 24, DoubleNormal-Size) these are placeholders —
#             often 1.0 across all ages — NOT the realized selectivity.
#   "Asel2" = REALIZED age-based selectivity, derived by integrating the
#             length-based selectivity over the age->length distribution.
#             This is what SS3 actually uses to compute F at age.
# Pcod 2024 uses length-based DoubleNormal (Size_DblN_*), so we MUST use
# "Asel2". Using "Asel" gave sel=1.0 for all ages > 0, applying full F to
# age-1 fish that SS3 doesn't catch (sel for 15cm fish ≈ 0.001 in reality).
asel <- ss3_rep$ageselex %>%
  dplyr::filter(Factor == "Asel2",
                Yr %in% years_hind,
                Fleet %in% fleet_meta$ss3_num)

a_min <- cod_ss3$minage[1]
a_max <- a_min + cod_ss3$nages[1] - 1
# IMPORTANT: SS3 ageselex columns start at age "0" (recruits). Rceattle slot 1
# corresponds to SS3 age 0 (verified via R[1977] = SS3 Recruit_0[1977]). So we
# need to pull SS3 columns "0":"nages-1" into Comp_1..Comp_nages, NOT
# "1":"nages" — that would shift the entire fleet selectivity by one age
# class, applying F to the wrong cohort and causing compounding biomass drift.
sel_age_cols <- as.character(0:(a_max - 1))
# Old buggy line preserved as documentation:
# age_cols_ss3 <- as.character(a_min:a_max)
age_cols_ss3 <- sel_age_cols  # keep the name for backward compatibility downstream
sel_available <- intersect(sel_age_cols, colnames(asel))
if (length(sel_available) < length(sel_age_cols)) {
  cat(sprintf("WARN: SS3 ageselex missing cols. Have [%s]; need [%s].\n",
              paste(grep("^[0-9]+$", colnames(asel), value = TRUE), collapse = ","),
              paste(sel_age_cols, collapse = ",")))
}
stopifnot(all(sel_age_cols %in% colnames(asel)))

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

# ============================================================================
# 4c. Empirical WAA injection (bypass VB->WAA derivation)
#     Rceattle reads minage=1 from cod_caal, so its growth function returns
#     l1 = length at age 1 in slot 1. But our natage injection puts SS3's
#     age-0 cohort (recruits, ~7 g) in slot 1. To make WAA-per-slot match the
#     cohort-per-slot, we feed SS3's start-of-year weights for ages 0..nages-1
#     directly into cod_ss3$wt as time-invariant fleet/population WAA.
#
#     This is the WAA analogue of emp_sel: collapses VB+W-L+SD_growth into a
#     single lookup table. Once forward dynamics are validated we can revert
#     to parametric WAA.
# ============================================================================
ss3_endgrowth_waa <- ss3_rep$endgrowth %>%
  dplyr::filter(Sex == 1, int_Age %in% (0:(a_max))) %>%
  dplyr::arrange(int_Age) %>%
  dplyr::pull(Wt_Beg)

# Plus-group weight: weight at the SS3 plus-group age (== last available row)
waa_slot <- numeric(cod_ss3$nages[1])
n_avail  <- length(ss3_endgrowth_waa)
# Map: slot k <- SS3 endgrowth int_Age (k-1) for k = 1..nages-1
# Slot nages = plus group = SS3 endgrowth at int_Age (nages-1) and beyond
for (k in seq_len(cod_ss3$nages[1] - 1)) {
  if (k <= n_avail) waa_slot[k] <- ss3_endgrowth_waa[k]
}
waa_slot[cod_ss3$nages[1]] <- ss3_endgrowth_waa[min(cod_ss3$nages[1], n_avail)]

cat(sprintf("\nWAA slot 1..%d (kg): %s\n", cod_ss3$nages[1],
            paste(sprintf("%.4f", waa_slot), collapse = ", ")))

# Build the cod_ss3$wt frame: one row per (Wt_index, Year). The wt_index slots
# map to (1) population WAA, (2) SSB WAA, (3..) per-fleet WAA. We feed the same
# vector to all slots since SS3 derives all of them from the same growth curve.
n_wt_slots <- 2L + nrow(fleet_meta)
wt_names   <- c("Pop_WAA", "SSB_WAA", paste0(fleet_meta$name, "_WAA"))
wt_rows <- list()
age_col_names <- paste0("Age", 1:cod_ss3$nages[1])
for (s in seq_len(n_wt_slots)) {
  for (yr in years_hind) {
    base <- data.frame(
      Wt_name  = wt_names[s],
      Wt_index = s,
      Species  = 1L,
      Sex      = 0L,
      Year     = yr,
      stringsAsFactors = FALSE
    )
    waa_df <- setNames(as.data.frame(as.list(waa_slot)), age_col_names)
    wt_rows[[length(wt_rows) + 1]] <- cbind(base, waa_df)
  }
}
cod_ss3$weight <- do.call(rbind, wt_rows)
cod_ss3$pop_wt_index <- 1
cod_ss3$ssb_wt_index <- 2
cod_ss3$fleet_control$Weight_index <- 3:7
cat(sprintf("cod_ss3$weight built: %d rows across %d WAA slots, %d years\n",
            nrow(cod_ss3$weight), n_wt_slots, length(years_hind)))


# ----------------------------------------------------------------------------
# 4c.1  Plus-group WAA correction.
#       Rceattle slot nages holds (SS3 age nages-1 + SS3 plus group at age nages).
#       The previous WAA setup used only SS3 age nages-1 weight (6.012). The
#       correct value is the year-by-year weighted average using SS3 natage.
# ----------------------------------------------------------------------------
ss3_waa_age <- ss3_rep$endgrowth %>%
  dplyr::filter(Sex == 1) %>% dplyr::arrange(int_Age) %>%
  dplyr::select(int_Age, Wt_Beg)
waa_at <- function(age) {
  v <- ss3_waa_age %>% dplyr::filter(int_Age == age) %>% dplyr::pull(Wt_Beg)
  if (length(v) == 0) NA_real_ else v[1]
}
waa_nm1  <- waa_at(cod_ss3$nages[1] - 1)
waa_plus <- waa_at(cod_ss3$nages[1])

plus_waa_year <- function(year) {
  row <- ss3_rep$natage %>%
    dplyr::filter(Yr == year, `Beg/Mid` == "B", Sex == 1) %>% dplyr::slice(1)
  if (nrow(row) == 0) return(waa_nm1)
  n_nm1  <- as.numeric(row[1, as.character(cod_ss3$nages[1] - 1)])
  n_plus <- as.numeric(row[1, as.character(cod_ss3$nages[1])])
  (n_nm1 * waa_nm1 + n_plus * waa_plus) / max(n_nm1 + n_plus, 1e-10)
}

plus_col <- paste0("Age", cod_ss3$nages[1])
for (yr in years_hind) {
  w <- plus_waa_year(yr)
  for (sname in c("Pop_WAA", "SSB_WAA")) {
    idx <- which(cod_ss3$weight$Wt_name == sname & cod_ss3$weight$Year == yr)
    if (length(idx)) cod_ss3$weight[idx, plus_col] <- w
  }
}
cat(sprintf("Plus-group WAA override: yr 1977 = %.4f, yr 2024 = %.4f (vs raw %.4f)\n",
            plus_waa_year(1977), plus_waa_year(2024), waa_nm1))


# ============================================================================
# 4d. Maturity ogive from SS3 endgrowth (replaces all-"2" stub)
#     Rceattle: mature_females(sp, age) = maturity(sp, age) * sex_ratio(sp, age)
#     Pcod sex_ratio = 0.5 by default. To make mature_females = SS3_mat,
#     set maturity = SS3_mat / sex_ratio.
#     Age convention: Rceattle slot k = SS3 age k-1.
# ============================================================================
ss3_len_mat <- ss3_rep$endgrowth %>%
  dplyr::filter(Sex == 1) %>% dplyr::arrange(int_Age) %>%
  dplyr::select(int_Age, Len_Mat)
mat_at <- function(age) {
  v <- ss3_len_mat %>% dplyr::filter(int_Age == age) %>% dplyr::pull(Len_Mat)
  if (length(v) == 0) NA_real_ else v[1]
}

mat_vec <- numeric(cod_ss3$nages[1])
for (k in 1:(cod_ss3$nages[1] - 1)) {
  v <- mat_at(k - 1)
  mat_vec[k] <- if (is.na(v)) 0 else v
}
# Plus group: weighted by N at styr (proxy for the steady-state mix)
nat_styr <- ss3_rep$natage %>%
  dplyr::filter(Yr == cod_ss3$styr, `Beg/Mid` == "B", Sex == 1) %>% dplyr::slice(1)
n_nm1   <- as.numeric(nat_styr[1, as.character(cod_ss3$nages[1] - 1)])
n_plus  <- as.numeric(nat_styr[1, as.character(cod_ss3$nages[1])])
mat_nm1 <- mat_at(cod_ss3$nages[1] - 1)
mat_plus_val <- mat_at(cod_ss3$nages[1])
mat_vec[cod_ss3$nages[1]] <-
  (n_nm1 * mat_nm1 + n_plus * mat_plus_val) / max(n_nm1 + n_plus, 1e-10)

# Divide by sex_ratio so that mature_females = mat_vec after C++ multiplication
sr_row <- cod_ss3$sex_ratio[1, paste0("Age", 1:cod_ss3$nages[1])]
sr_val <- as.numeric(sr_row[1])
if (is.na(sr_val) || sr_val == 0) sr_val <- 0.5
cod_ss3$maturity[1, paste0("Age", 1:cod_ss3$nages[1])] <- mat_vec / sr_val
# Zero out trailing NA-named columns from the original stub if present
extra_cols <- setdiff(colnames(cod_ss3$maturity),
                     c("Species", paste0("Age", 1:cod_ss3$nages[1])))
if (length(extra_cols)) cod_ss3$maturity[1, extra_cols] <- NA

cat(sprintf("\nMaturity ogive set (sex_ratio = %.2f used as divisor):\n", sr_val))
print(data.frame(Slot = 1:cod_ss3$nages[1],
                 SS3_mat_fraction = mat_vec,
                 Rce_maturity_set = mat_vec / sr_val))

# initMode must be set BEFORE fit_mod() so the parameter structure (init_dev
# sizing and map) is built correctly. "FreeParams" lets us inject SS3 natage
# directly; "NonEquilibrium" is the standard Rceattle equilibrium-with-devs.
if (USE_SS3_INITIAL_NATAGE) cod_ss3$initMode <- "FreeParams"


# ============================================================================
# 4e. SSB Jensen's-gap closure (data-side)
#     SS3:  SSB = sum_age  N * Mat_F_wtatage
#       where Mat_F_wtatage = sex_ratio * E[mat(L) * W(L)] integrated over the
#       length distribution at age (an *expectation*, not point evaluation).
#     Rce (ceattle_v01_11.cpp:1148, 1224):
#       SSB = sum_age  N * exp(-Z * spawn_month/12) * WAA_ssb * mature_females
#       with mature_females = maturity * sex_ratio (when nsex == 1).
#
#     With Section 4d's maturity = Len_Mat / sex_ratio, the product
#     WAA_ssb * mature_females collapses to Wt_Beg * Len_Mat, which is
#     mat(L_bar) * W(L_bar) -- point evaluation. Jensen's inequality opens
#     a ~8-13% SSB underestimate because E[mat*W] > mat(L_bar)*W(L_bar) at
#     intermediate ages.
#
#     Fix (no C++ touch): collapse WAA_ssb * mature_females into Mat_F_wtatage
#     directly by setting
#         WAA_ssb[age]  := Mat_F_wtatage[age]
#         maturity[age] := 1 / sex_ratio[age]
#     so mature_females = 1 after C++ multiplication, and SSB becomes
#         sum_age  N * exp(-Z*sm/12) * Mat_F_wtatage
#     matching SS3 to machine precision.
#
#     CAVEAT: invalidates any downstream SR(SSB) fit because SSB is rescaled
#     into "matured-female-weight" units that include the mat*W expectation.
#     For fixed-param validation (recruitment from init_dev/rec_pars, not
#     SRR), this is fine. Revert before estimating an SR curve.
# ============================================================================
ss3_mfw <- ss3_rep$endgrowth %>%
  dplyr::filter(Sex == 1) %>% dplyr::arrange(int_Age) %>%
  dplyr::select(int_Age, Mat_F_wtatage)
mfw_at <- function(age) {
  v <- ss3_mfw %>% dplyr::filter(int_Age == age) %>% dplyr::pull(Mat_F_wtatage)
  if (length(v) == 0) NA_real_ else v[1]
}

# Ages 1..nages-1: time-invariant Mat_F_wtatage from SS3 endgrowth
mfw_vec_base <- numeric(cod_ss3$nages[1])
for (k in 1:(cod_ss3$nages[1] - 1)) {
  v <- mfw_at(k - 1)
  mfw_vec_base[k] <- if (is.na(v)) 0 else v
}
mfw_nm1  <- mfw_at(cod_ss3$nages[1] - 1)
mfw_plus <- mfw_at(cod_ss3$nages[1])

# Plus group is year-varying because Rceattle slot nages holds
# (SS3 age nages-1 + SS3 plus-group age nages), and the N-weighting between
# those two shifts as cohorts pass through. Mirrors Section 4c.1's logic.
plus_mfw_year <- function(year) {
  row <- ss3_rep$natage %>%
    dplyr::filter(Yr == year, `Beg/Mid` == "B", Sex == 1) %>% dplyr::slice(1)
  if (nrow(row) == 0) return(mfw_nm1)
  n_nm1  <- as.numeric(row[1, as.character(cod_ss3$nages[1] - 1)])
  n_plus <- as.numeric(row[1, as.character(cod_ss3$nages[1])])
  (n_nm1 * mfw_nm1 + n_plus * mfw_plus) / max(n_nm1 + n_plus, 1e-10)
}

# Inject as SSB_WAA (Wt_index = 2). Per-year rows so plus-group can vary.
age_cols_w <- paste0("Age", 1:cod_ss3$nages[1])
plus_col_w <- paste0("Age", cod_ss3$nages[1])
ssb_rows   <- which(cod_ss3$weight$Wt_index == 2)
for (r in ssb_rows) {
  yr <- cod_ss3$weight$Year[r]
  vec <- mfw_vec_base
  vec[cod_ss3$nages[1]] <- plus_mfw_year(yr)
  cod_ss3$weight[r, age_cols_w] <- vec
}

# Maturity := 1 / sex_ratio so mature_females = 1 after C++ multiplication.
sr_val_4e <- as.numeric(cod_ss3$sex_ratio[1, age_cols_w][1])
if (is.na(sr_val_4e) || sr_val_4e == 0) sr_val_4e <- 0.5
cod_ss3$maturity[1, age_cols_w] <- 1 / sr_val_4e

cat(sprintf(
  "\nJensen's-gap closure applied: WAA_ssb := Mat_F_wtatage (plus-group year-weighted); maturity := 1/%.2f\n",
  sr_val_4e))
print(data.frame(
  Slot              = 1:cod_ss3$nages[1],
  SS3_age           = 0:(cod_ss3$nages[1] - 1),
  Mat_F_wtatage_age = mfw_vec_base,
  Plus_mfw_1977     = c(rep(NA, cod_ss3$nages[1] - 1), plus_mfw_year(1977)),
  Plus_mfw_2024     = c(rep(NA, cod_ss3$nages[1] - 1), plus_mfw_year(2024)),
  Rce_maturity      = 1 / sr_val_4e
))


# ============================================================================
# 5.  Get Rceattle parameter structure, then fill from SS3 par file
# ============================================================================
mod0 <- Rceattle::fit_mod(
  data_list    = cod_ss3,
  inits        = NULL,
  file         = NULL,
  estimateMode = 3,
  initMode     = if (USE_SS3_INITIAL_NATAGE) "FreeParams" else "NonEquilibrium",
  growthFun    = build_growth(fun = 0),  # empirical WAA: uses cod_ss3$weight
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

# Pin log_F to SS3's per-year F values (per fishery fleet). Without this,
# Rceattle's default log_F init drifts from SS3 as biomass diverges, producing
# the compounding-error feedback loop.
ts_ss3_for_F <- read.csv(TS_FILE)
cat("\nlog_F initialization from SS3 ts file:\n")
inits <- init_log_F_from_ss3(
  inits      = inits,
  ts_ss3     = ts_ss3_for_F,
  fleet_meta = fleet_meta,
  years_hind = years_hind
)


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
  growthFun    = build_growth(fun = 0),  # empirical WAA: uses cod_ss3$weight
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = FALSE
)

# ============================================================================
# 6b. Growth validation — does Rceattle's VB growth reproduce SS3's WAA?
#     This is the same model as cod_ss3_fixed but with growthFun = 1
#     (vonBertalanffy) instead of "empirical". If WAA matches SS3 endgrowth
#     within 1e-3, the bridge no longer needs the empirical WAA injection.
# ============================================================================
cod_ss3_vb <- Rceattle::fit_mod(
  data_list    = cod_ss3,
  inits        = inits,
  file         = NULL,
  estimateMode = 3,
  initMode     = if (USE_SS3_INITIAL_NATAGE) "FreeParams" else "NonEquilibrium",
  growthFun    = build_growth(fun = 1),  # vonBertalanffy
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
  growthFun    = build_growth(fun = 0),  # empirical WAA: uses cod_ss3$weight
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
# 8.  Relative-error diagnostics vs SS3----
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
# 8b.0  Sanity checks — did the bridge actually wire up correctly?----
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

# M-block sanity: was the post-2014 transition picked up by the SS3 grep?
# Spawn month — SSB Year-1 ratio of 0.917 with M=0.493 implies Rceattle is
# using ~3 months of mortality while SS3 spawns ~1 month in. Verify and align.
cat("\nSpawn month check:\n")
ss3_spawn_seas <- ctllist_ss3$spawn_seas %||% NA
cat(sprintf("  cod_ss3$spawn_month       = %s\n", cod_ss3$spawn_month %||% "NULL"))
cat(sprintf("  ctllist_ss3$spawn_seas    = %s (SS3 ctl value)\n", ss3_spawn_seas))
cat(sprintf("  Rceattle internal spawn_month = %s\n",
            cod_ss3_fixed$data_list$spawn_month %||% "NULL"))

cat("\nM-block wiring:\n")
m_base_logged <- exp(cod_ss3_fixed$estimated_params$log_M1[1, 1, 1])
cat(sprintf("  log_M1[1,1] = %.4f  =>  M_base = %.4f (expect 0.493)\n",
            cod_ss3_fixed$estimated_params$log_M1[1, 1,1], m_base_logged))
if ("beta_linkage" %in% names(cod_ss3_fixed$estimated_params)) {
  b1 <- cod_ss3_fixed$estimated_params$beta_linkage[1]
  cat(sprintf("  beta_linkage[1] = %.4f  =>  M_post2014 = %.4f (expect ~0.376)\n",
              b1, m_base_logged * exp(b1)))
} else {
  cat("  beta_linkage not in params — M block not active\n")
}
cat("\nM_at_age year 1 vs year 38 (post-2014, should drop):\n")
if ("M_at_age" %in% names(cod_ss3_fixed$quantities)) {
  cat(sprintf("  yr 1 (1977): %s\n",
              paste(sprintf("%.4f",
                cod_ss3_fixed$quantities$M_at_age[1, 1, , 1]), collapse = ", ")))
  yr_2014 <- which(years_hind == 2014)
  if (length(yr_2014)) {
    cat(sprintf("  yr %d (2014): %s\n", yr_2014,
                paste(sprintf("%.4f",
                  cod_ss3_fixed$quantities$M_at_age[1, 1, , yr_2014]), collapse = ", ")))
  }
}
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
# 8b.1  Selectivity-at-age comparison (slot is $sel_at_age, not $sel) ----
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
  # Compare against Asel2 (realized age sel from length-based eq) — matches
  # what we inject and what SS3 actually uses for F at age.
  ss3_sub <- ss3_rep$ageselex %>%
    dplyr::filter(Factor == "Asel2", Fleet == ss3_num, Yr %in% years_hind)

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
# 8b.2  Weight-at-age comparison ($weight_hat) ----
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
# 8b.3  Initial age structure comparison (year 1) ----
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
# 8b.GROWTH  VB growth validation — does Rceattle's parametric model ----
#            reproduce SS3 WAA to 1e-3?
# ----------------------------------------------------------------------------
cat("\n=== Growth validation: VB-derived WAA vs SS3 endgrowth ===\n")

# Print every numeric column SS3 emits per integer age (Sex=1).
# This reveals what SD column names actually look like in this r4ss output.
cat("\nSS3 endgrowth columns:\n")
print(colnames(ss3_rep$endgrowth))

ss3_grow <- ss3_rep$endgrowth %>%
  dplyr::filter(Sex == 1, int_Age %in% 0:nages_pcod) %>%
  dplyr::arrange(int_Age) %>%
  dplyr::select(int_Age, dplyr::any_of(c("Len_Beg", "Len_Mid", "Wt_Beg", "Wt_Mid",
                                         "SD_Mid", "SD_Beg", "CV_Beg", "CV_Mid")))
cat("\nSS3 endgrowth per int_Age (target values for Rceattle VB):\n")
print(ss3_grow)

# Compare Rceattle VB-fitted weight_hat to SS3 endgrowth Wt_Beg
# Rceattle slot k = SS3 age k-1, so compare:
#   rce_waa_vb[k] (slot k, year 1)  vs  ss3_grow$Wt_Beg[k] (int_Age = k-1)
rce_waa_vb <- as.numeric(cod_ss3_vb$quantities$weight_hat[1, 1, , 1])

# Build paired comparison
ss3_wt_beg <- ss3_grow$Wt_Beg
n_ss3 <- length(ss3_wt_beg)
ss3_aligned <- numeric(nages_pcod)
ss3_aligned[1:min(nages_pcod, n_ss3)] <- ss3_wt_beg[1:min(nages_pcod, n_ss3)]
if (n_ss3 >= nages_pcod + 1) {
  # Plus group: combine SS3 ages (nages-1) and nages by N-weighting (year 1)
  n_at_age_yr1 <- rce_N1
  w_age_nm1 <- ss3_wt_beg[nages_pcod]      # SS3 age nages-1
  w_age_max <- ss3_wt_beg[nages_pcod + 1]  # SS3 plus group
  # For slot nages = sum of N at age (nages-1) + plus group, the weighted WAA is:
  plus_n <- rce_N1[nages_pcod]
  ss3_aligned[nages_pcod] <- w_age_max  # SS3 plus-group weight (Rceattle slot 10)
}

vb_compare <- data.frame(
  Slot         = 1:nages_pcod,
  SS3_age      = 0:(nages_pcod - 1),
  Rceattle_VB  = rce_waa_vb,
  SS3_Wt_Beg   = ss3_aligned,
  AbsErr       = rce_waa_vb - ss3_aligned,
  RelErr       = abs(rce_waa_vb - ss3_aligned) / pmax(abs(ss3_aligned), 1e-10)
)
cat("\nVB-fitted WAA vs SS3 (Rceattle slot k = SS3 age k-1):\n")
print(vb_compare)
cat(sprintf("\nVB WAA max rel err: %.2e  mean: %.2e  (target ≤ 1e-3)\n",
            max(vb_compare$RelErr), mean(vb_compare$RelErr)))

# Also report what params Rceattle used (for tuning reference)
cat("\nRceattle VB params used:\n")
cat(sprintf("  log_K   = %.4f  =>  K = %.4f\n",
            cod_ss3_vb$estimated_params$log_growth_pars[1, 1, 1],
            exp(cod_ss3_vb$estimated_params$log_growth_pars[1, 1, 1])))
cat(sprintf("  log_L1  = %.4f  =>  L1 = %.4f (at Rceattle minage=%d)\n",
            cod_ss3_vb$estimated_params$log_growth_pars[1, 1, 2],
            exp(cod_ss3_vb$estimated_params$log_growth_pars[1, 1, 2]),
            cod_ss3$minage[1]))
cat(sprintf("  log_Linf= %.4f  =>  Linf = %.4f\n",
            cod_ss3_vb$estimated_params$log_growth_pars[1, 1, 3],
            exp(cod_ss3_vb$estimated_params$log_growth_pars[1, 1, 3])))
if ("growth_log_sd" %in% names(cod_ss3_vb$estimated_params)) {
  cat(sprintf("  log_SD_young = %.4f  =>  SD_y = %.4f\n",
              cod_ss3_vb$estimated_params$growth_log_sd[1, 1, 1],
              exp(cod_ss3_vb$estimated_params$growth_log_sd[1, 1, 1])))
  cat(sprintf("  log_SD_old   = %.4f  =>  SD_o = %.4f\n",
              cod_ss3_vb$estimated_params$growth_log_sd[1, 1, 2],
              exp(cod_ss3_vb$estimated_params$growth_log_sd[1, 1, 2])))
}
cat(sprintf("  W-L alpha = %.6g, beta = %.4f\n",
            cod_ss3_vb$data_list$alpha_wt_len[1] %||%
              cod_ss3_vb$estimated_params$weight_length_pars[1, 1] %||% NA,
            cod_ss3_vb$data_list$beta_wt_len[1] %||%
              cod_ss3_vb$estimated_params$weight_length_pars[1, 2] %||% NA))

# What Rceattle computes for length-at-age (compare to SS3 Len_Beg)
cat("\nRceattle length-at-age (year 1) vs SS3 Len_Beg:\n")
rce_len <- as.numeric(cod_ss3_vb$quantities$length_hat[1, 1, , 1])
ss3_len <- ss3_grow$Len_Beg
ss3_len_aligned <- numeric(nages_pcod)
ss3_len_aligned[1:min(nages_pcod, length(ss3_len))] <-
  ss3_len[1:min(nages_pcod, length(ss3_len))]
print(data.frame(
  Slot     = 1:nages_pcod,
  SS3_age  = 0:(nages_pcod - 1),
  Rce_Len  = rce_len,
  SS3_Len  = ss3_len_aligned,
  RelErr   = abs(rce_len - ss3_len_aligned) /
             pmax(abs(ss3_len_aligned), 1e-10)
))


# ----------------------------------------------------------------------------
# 8b.3b  Year 2 (1978) N-at-age — does N propagation match SS3?
#        Year 1 matches to 1e-6 by construction (we injected it).
#        If year 2 matches, biomass drift comes from WAA used in calc.
#        If year 2 doesn't match, the forward N propagation is wrong.
# ----------------------------------------------------------------------------
rce_N2 <- as.numeric(cod_ss3_fixed$quantities$N_at_age[1, 1, , 2])
ss3_natage2 <- ss3_rep$natage %>%
  dplyr::filter(Yr == years_hind[2], `Beg/Mid` == "B", Sex == 1) %>%
  dplyr::slice(1)
ss3_N2_raw <- as.numeric(ss3_natage2[1, as.character(0:nages_pcod)])
ss3_N2 <- c(ss3_N2_raw[1:(nages_pcod - 1)],
            ss3_N2_raw[nages_pcod] + ss3_N2_raw[nages_pcod + 1])

cat(sprintf("\nYear 2 (%d) N-at-age comparison:\n", years_hind[2]))
n2_tbl <- data.frame(
  Slot     = 1:nages_pcod,
  SS3      = ss3_N2,
  Rceattle = rce_N2,
  Ratio    = rce_N2 / pmax(ss3_N2, 1e-10),
  RelErr   = abs(rce_N2 - ss3_N2) / pmax(abs(ss3_N2), 1e-10)
)
print(n2_tbl)
cat(sprintf("Year 2 N max rel err: %.2e\n", max(n2_tbl$RelErr)))

# Hand-derive year 2 N from year 1 + Z to localize propagation error.
# C++: N[slot k+1, yr+1] = N[slot k, yr] * exp(-Z[slot k, yr])
# Plus group: N[plus, yr+1] = N[plus-1, yr]*exp(-Z[plus-1]) + N[plus, yr]*exp(-Z[plus])
cat("\n--- Hand-derived year 2 N (using Rceattle Z) ---\n")
if ("Z_at_age" %in% names(cod_ss3_fixed$quantities)) {
  z1 <- as.numeric(cod_ss3_fixed$quantities$Z_at_age[1, 1, , 1])
  cat(sprintf("Rceattle Z (yr 1) by slot: %s\n",
              paste(sprintf("%.4f", z1), collapse = ", ")))
  # Build year 2 N from year 1 N and Z
  manual_n2 <- numeric(nages_pcod)
  # Slot 1 in year 2 = R[1978]
  manual_n2[1] <- as.numeric(cod_ss3_fixed$quantities$R[1, 2])
  # Slots 2..nages-1: shifted survival
  for (k in 2:(nages_pcod - 1)) {
    manual_n2[k] <- rce_N1[k - 1] * exp(-z1[k - 1])
  }
  # Plus group
  manual_n2[nages_pcod] <- rce_N1[nages_pcod - 1] * exp(-z1[nages_pcod - 1]) +
                          rce_N1[nages_pcod]     * exp(-z1[nages_pcod])
  cat("Hand-derived year 2 N from year 1 N and Z:\n")
  print(data.frame(Slot = 1:nages_pcod,
                   Manual = manual_n2,
                   Rceattle_N2 = rce_N2,
                   RelErr = abs(manual_n2 - rce_N2) / pmax(abs(rce_N2), 1e-10)))
} else {
  cat("Z_at_age not in quantities — skipping hand-derived check\n")
}


# ----------------------------------------------------------------------------
# 8b.4  Direct biomass / SSB comparison — diagnose the 97% mean error. ----
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

# Year-by-year ratio sweep to localize WHEN divergence starts.
# Early years (1977-1981) currently match to 1e-5; late years (2020+) drift
# to 0.32. We need to see where the breakdown begins.
cat("\n--- Bio_Ratio every 3 years (year-of-onset diagnostic) ---\n")
sweep_idx <- seq(1, length(years_hind), by = 3)
sweep_tbl <- data.frame(
  Year      = years_hind[sweep_idx],
  Bio_SS3   = bio_ss3[sweep_idx],
  Bio_Rce   = bio_rce[sweep_idx],
  Bio_Ratio = bio_rce[sweep_idx] / bio_ss3[sweep_idx],
  SSB_SS3   = ssb_ss3[sweep_idx],
  SSB_Rce   = ssb_rce[sweep_idx],
  SSB_Ratio = ssb_rce[sweep_idx] / ssb_ss3[sweep_idx]
)
print(sweep_tbl)

# F-by-year comparison with both Rce AND SS3 columns side-by-side.
# read.csv converts "F:_1" -> "F._1" via check.names=TRUE, so resolve the
# actual column name pattern first.
cat("\n--- F by fleet, every 5 years (Rce vs SS3) ---\n")
# Find the actual F columns regardless of how read.csv mangled them
ts_f_cols <- grep("^F[._:]_[0-9]+$|^F\\.\\._[0-9]+$|^F\\._[0-9]+$",
                  colnames(ts_ss3), value = TRUE)
cat(sprintf("Detected ts_ss3 F-column names: %s\n",
            paste(ts_f_cols, collapse = ", ")))
if ("F_flt" %in% names(cod_ss3_fixed$quantities) && length(ts_f_cols) > 0) {
  f_yrs <- seq(1, length(years_hind), by = 5)
  ts_idx <- match(years_hind[f_yrs], ts_ss3$Yr)
  for (i in seq_len(nrow(fleet_meta))) {
    if (fleet_meta$fleet_type[i] != "Fishery") next
    # Match the i-th fishery to the i-th F column (in fleet order)
    if (fleet_meta$ss3_num[i] > length(ts_f_cols)) next
    f_col <- ts_f_cols[fleet_meta$ss3_num[i]]
    rce_vals <- as.numeric(cod_ss3_fixed$quantities$F_flt[i, f_yrs])
    ss3_vals <- as.numeric(ts_ss3[[f_col]])[ts_idx]
    if (length(ss3_vals) != length(rce_vals)) {
      cat(sprintf("  %s: skipping (ss3 col '%s' returned %d vals, need %d)\n",
                  fleet_meta$name[i], f_col, length(ss3_vals), length(rce_vals)))
      next
    }
    f_check <- data.frame(Year = years_hind[f_yrs],
                          Rce = rce_vals, SS3 = ss3_vals,
                          Diff = rce_vals - ss3_vals)
    cat(sprintf("\n%s (vs ts_ss3$%s):\n", fleet_meta$name[i], f_col))
    print(f_check)
  }
}


# ----------------------------------------------------------------------------
# 8b.7  Localize the breakpoint — what changes between 2010 and 2013?
# ----------------------------------------------------------------------------
cat("\n--- M_at_age year-by-year, 2010-2016 (M block transition zone) ---\n")
breakpoint_yrs <- 2010:2016
b_idx <- match(breakpoint_yrs, years_hind)
b_idx <- b_idx[!is.na(b_idx)]
m_vec <- sapply(b_idx, function(yi) cod_ss3_fixed$quantities$M_at_age[1, 1, 5, yi])
m_trace <- data.frame(Year = years_hind[b_idx], M_age5 = m_vec)
print(m_trace)

# Look up the post2014 indicator from whatever shape env data ended up in.
env_dl <- cod_ss3_fixed$data_list
cat("\nenv_data / env_index slot shape probe:\n")
for (nm in c("env_data", "env_index", "linkage_X")) {
  if (!is.null(env_dl[[nm]])) {
    cat(sprintf("  %s: ", nm)); print(utils::head(env_dl[[nm]], 3))
  }
}

cat("\n--- env_data post2014 around transition (raw input cod_ss3$env_data) ---\n")
if (!is.null(cod_ss3$env_data) && "post2014" %in% colnames(cod_ss3$env_data)) {
  print(cod_ss3$env_data %>%
        dplyr::filter(Year %in% breakpoint_yrs) %>%
        dplyr::select(Year, post2014))
} else {
  cat("  cod_ss3$env_data missing 'post2014' column. Columns: ",
      paste(colnames(cod_ss3$env_data %||% data.frame()), collapse = ", "), "\n")
}

cat("\n--- F (trawl/LL) and Z (age 5) year-by-year 2008-2024 (extended) ---\n")
detail_yrs <- 2014:2024
d_idx <- match(detail_yrs, years_hind)
d_idx <- d_idx[!is.na(d_idx)]
ts_d_idx <- match(years_hind[d_idx], ts_ss3$Yr)
get_ss3_F <- function(ss3_num) {
  if (length(ts_f_cols) >= ss3_num) as.numeric(ts_ss3[[ts_f_cols[ss3_num]]])[ts_d_idx]
  else rep(NA_real_, length(d_idx))
}
detail_tbl <- data.frame(
  Year        = years_hind[d_idx],
  F_trawl_Rce = sapply(d_idx, function(yi) cod_ss3_fixed$quantities$F_flt[1, yi]),
  F_trawl_SS3 = get_ss3_F(1),
  F_LL_Rce    = sapply(d_idx, function(yi) cod_ss3_fixed$quantities$F_flt[2, yi]),
  F_LL_SS3    = get_ss3_F(2),
  Z_age5_Rce  = sapply(d_idx, function(yi) cod_ss3_fixed$quantities$Z_at_age[1, 1, 5, yi]),
  Bio_ratio   = bio_rce[d_idx] / bio_ss3[d_idx]
)
print(detail_tbl)


# ----------------------------------------------------------------------------
# 8b.8  Breakpoint-2 localization (2016-2024)
#       Bio is 0.9998 in 2016 and 0.52 by 2019. Something activates in
#       2017 or 2018 that we haven't captured. Candidates:
#         (a) Another SS3 selectivity block (trawl typically has multiple blocks)
#         (b) A second M block we missed in the ctl
#         (c) Per-year WAA changing in SS3 but constant in our setup
#       Print year-by-year ratio + all relevant inputs around the breakpoint.
# ----------------------------------------------------------------------------
cat("\n--- Year-by-year Bio_Ratio 2014-2024 ---\n")
bp2_yrs <- 2014:2024
bp2_idx <- match(bp2_yrs, years_hind)
bp2_idx <- bp2_idx[!is.na(bp2_idx)]
bp2_tbl <- data.frame(
  Year       = years_hind[bp2_idx],
  Bio_SS3    = bio_ss3[bp2_idx],
  Bio_Rce    = bio_rce[bp2_idx],
  Ratio      = bio_rce[bp2_idx] / bio_ss3[bp2_idx],
  R_SS3      = ss3_R[bp2_idx],
  R_Rce      = as.numeric(cod_ss3_fixed$quantities$R[1, bp2_idx]),
  M_age5_Rce = sapply(bp2_idx, function(yi) cod_ss3_fixed$quantities$M_at_age[1, 1, 5, yi])
)
print(bp2_tbl)

# Check whether Rceattle's selectivity changed between 2016 and 2018 for any fleet
cat("\n--- Sel-at-age (trawl) year-by-year, peak detection ---\n")
sel_evolution <- data.frame(Year = bp2_yrs)
for (a in 1:nages_pcod) {
  sel_evolution[[paste0("Age", a)]] <-
    sapply(bp2_idx, function(yi) cod_ss3_fixed$quantities$sel_at_age[1, 1, a, yi])
}
print(round(sel_evolution, 4))

# Check the ctl for any additional blocks we might have missed
cat("\n--- All SS3 ctl block design entries ---\n")
if (!is.null(ctllist_ss3$Block_Design)) {
  cat(sprintf("N_Block_Designs: %s\n", ctllist_ss3$N_Block_Designs %||% NA))
  for (i in seq_along(ctllist_ss3$Block_Design)) {
    cat(sprintf("  Block design %d: %s\n", i,
                paste(ctllist_ss3$Block_Design[[i]], collapse = ", ")))
  }
}
cat("\n--- All MG_parms rows (incl. block params we may have missed) ---\n")
print(parlist$MG_parms[, c("INIT", "ESTIM")])

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
  cat("Unique Wt_name / Wt_index pairs:\n")
  print(unique(cod_ss3$weight[, c("Wt_name", "Wt_index")]))
}


# ----------------------------------------------------------------------------
# 8b.5  F diagnostic — locate the biomass collapse. ----
#       SS3 ts file shows F:_1, F:_2, F:_3 in 1977 = ~0.003, ~0.009, 0. Total
#       F ~ 0.012 → Z ~ 0.5 → biomass should DECAY by ~40%/yr from M alone.
#       Rceattle is losing 84% yr 1->2, implying Z ~ 1.8. Need to find the F.
# ----------------------------------------------------------------------------
cat("\n--- Rceattle F at age and total F by fleet (year 1) ---\n")
f_slots <- grep("F_|^F$|log_F|F_flt|F_at_age", names(cod_ss3_fixed$quantities), value = TRUE)
cat("F-related quantities slots:", paste(f_slots, collapse = ", "), "\n")
if ("F_flt_age" %in% names(cod_ss3_fixed$quantities)) {
  fdims <- dim(cod_ss3_fixed$quantities$F_flt_age)
  cat(sprintf("F_flt_age dim: %s\n", paste(fdims, collapse = " x ")))
  for (i in seq_len(nrow(fleet_meta))) {
    f_yr1 <- cod_ss3_fixed$quantities$F_flt_age[i, 1, , 1]
    cat(sprintf("  %s F at age (yr 1): %s\n",
                fleet_meta$name[i], paste(sprintf("%.4f", f_yr1), collapse = ", ")))
  }
}

# F-full (sel-independent) vs SS3 F per year — direct check of log_F injection
cat("\n--- F_full (fleet-level) vs SS3 F:_n per year ---\n")
if ("F_flt" %in% names(cod_ss3_fixed$quantities)) {
  show_yrs <- c(1, 5, 25, 38, length(years_hind))
  show_yrs <- show_yrs[show_yrs <= length(years_hind)]
  f_tbl <- data.frame(Year = years_hind[show_yrs])
  for (i in seq_len(nrow(fleet_meta))) {
    if (fleet_meta$fleet_type[i] != "Fishery") next
    f_tbl[[paste0(fleet_meta$name[i], "_Rce")]] <-
      cod_ss3_fixed$quantities$F_flt[i, show_yrs]
    f_tbl[[paste0(fleet_meta$name[i], "_SS3")]] <-
      ts_ss3[match(years_hind[show_yrs], ts_ss3$Yr),
             sprintf("F:_%d", fleet_meta$ss3_num[i])]
  }
  print(f_tbl)
}

# SS3 fishing mortality from ts file: columns F:_1 ... F:_K
ss3_f_cols <- grep("^F:_[0-9]+$", colnames(ts_ss3), value = TRUE)
if (length(ss3_f_cols) > 0) {
  cat("\nSS3 F (ts file) for 1977-1980:\n")
  print(ts_ss3 %>% dplyr::filter(Yr %in% years_hind[1:4]) %>%
          dplyr::select(Yr, dplyr::all_of(ss3_f_cols)))
}

# Compare Rceattle catch (predicted) vs SS3 obs catch in 1977
cat("\n--- Catch comparison (1977) ---\n")
if ("catch_hat" %in% names(cod_ss3_fixed$quantities)) {
  for (i in seq_len(nrow(fleet_meta))) {
    if (fleet_meta$fleet_type[i] == "Fishery") {
      catch_rce <- cod_ss3_fixed$quantities$catch_hat[i]
      cat(sprintf("  %s Rceattle catch_hat[%d] = %.4g\n",
                  fleet_meta$name[i], years_hind[1], catch_rce))
    }
  }
}
cat("Input catch_data rows for 1977:\n")
print(cod_ss3$catch_data %>% dplyr::filter(Year == years_hind[1]))


# ----------------------------------------------------------------------------
# 8b.6  Maturity diagnostic — SSB = Bio indicates maturity not loaded from SS3----
# ----------------------------------------------------------------------------
cat("\n--- Maturity in Rceattle data vs SS3 ---\n")
if (!is.null(cod_ss3$maturity)) {
  cat("cod_ss3$maturity rows:", nrow(cod_ss3$maturity), "\n")
  if (nrow(cod_ss3$maturity) > 0) print(head(cod_ss3$maturity))
}
cat("Rceattle internal maturity (cod_ss3_fixed$data_list$maturity, first row):\n")
print(cod_ss3_fixed$data_list$maturity)
cat("\nSS3 endgrowth maturity columns (any column with 'Mat' in name):\n")
mat_cols <- grep("Mat", colnames(ss3_rep$endgrowth), value = TRUE)
cat("  Candidate columns:", paste(mat_cols, collapse = ", "), "\n")
ss3_mat <- ss3_rep$endgrowth %>%
  dplyr::filter(Sex == 1, int_Age %in% (a_min:a_max)) %>%
  dplyr::arrange(int_Age) %>%
  dplyr::select(int_Age, dplyr::any_of(c("Age_Mat", "Mat_F_wtatage", "Len_Mat", "Wt_Mat")))
print(ss3_mat)


# ============================================================================
# 8c. Likelihood-component mapping: SS3 vs Rceattle
#     SS3 stores total NLL by component in $likelihoods_used (col "values"),
#     and a fleet-level breakdown in $likelihoods_by_fleet (when available).
#     Rceattle stores them in $quantities$jnll_comp as a 20 x n_col matrix.
#     Row meanings verified against ceattle_v01_11.cpp:2300..3017 :
#
#       R-row  Rceattle component                  SS3 component
#       1      Survey index obs                    "Survey"
#       2      Fishery catch obs                   "Catch"
#       3      Marginal age/length comps           "Length_comp" / "Age_comp" (non-CAAL)
#       4      CAAL (conditional age-at-length)    "Age_comp" if CAAL data present
#       5      Selectivity curvature penalty       (no direct SS3 row)
#       6      Selectivity dev RE                  partial of "Parm_devs"
#       7      q prior + AR1 q penalty             partial of "Parm_priors"
#       8      q dev / env                         partial of "Parm_devs"
#       9      SRR / steepness prior               partial of "Parm_priors"
#       10     init_dev (initial age-struct devs)  partial of "Recruitment"  *
#       11     rec_dev (recruitment devs)          partial of "Recruitment"  *
#       12     R vs R_hat penalty                  partial of "Recruitment"  (usually 0)
#       13     F/B reference-point penalty         off when not in BRP mode
#       14     zero_N floor penalty                no SS3 analogue (small)
#       15     M1 prior                            partial of "Parm_priors"
#       16     M random effects                    partial of "Parm_devs"
#       17–19  ration / stomach (multispecies)     n/a, == 0 in single-spp
#       20     General parameter priors            partial of "Parm_priors"
#
#     * Recruitment bucket is EXPECTED to diverge: SS3 applies the Methot-Taylor
#       bias-adj ramp (b(y) varies by period) inside its NLL contribution, while
#       Rceattle uses a constant 0.5*sigmaR^2 offset. We accept this divergence
#       by design and exclude it from the tolerance gate below. The recruitment
#       *time series* still matches because init_from_ss3_par applies the ramp
#       offset to rec_dev on the input side.
# ============================================================================

# Pull SS3 likelihood breakdown -----------------------------------------------
ss3_ll <- ss3_rep$likelihoods_used
ss3_ll$component <- rownames(ss3_ll)
ss3_val_col <- if ("values" %in% colnames(ss3_ll)) "values" else colnames(ss3_ll)[1]
ss3_lik <- setNames(ss3_ll[[ss3_val_col]], ss3_ll$component)
cat("\n=== SS3 likelihoods_used ===\n"); print(ss3_lik)

# Detect whether SS3 age data is CAAL or marginal ----------------------------
ss3_has_caal <- !is.null(ss3_rep$ladbase) && nrow(ss3_rep$ladbase) > 0
cat(sprintf("\nSS3 CAAL data present: %s\n", ss3_has_caal))

# Pull Rceattle jnll_comp -----------------------------------------------------
jnll_mat <- cod_ss3_fixed$quantities$jnll_comp
if (is.null(jnll_mat)) stop("cod_ss3_fixed$quantities$jnll_comp not found.")
stopifnot(nrow(jnll_mat) >= 20)
rce_row_sum <- function(r) sum(jnll_mat[r, ])     # collapse columns (fleet/species)

# Mapping table: each row says "this SS3 component = sum of these Rceattle rows".
# `expected_match` flags whether the bucket is in the tolerance gate.
ll_map <- list(
  Catch                = list(ss3 = "Catch",                rce_rows = 2,
                              expected_match = TRUE),
  Survey               = list(ss3 = "Survey",               rce_rows = 1,
                              expected_match = TRUE),
  Length_comp          = list(ss3 = "Length_comp",          rce_rows = 3,
                              expected_match = TRUE),
  Age_comp             = list(ss3 = "Age_comp",
                              rce_rows = if (ss3_has_caal) 4 else 3,
                              expected_match = TRUE),
  # Recruitment bucket: structural mismatch in NLL formulation (SS3 bias-adj ramp
  # vs Rceattle's constant variance offset). Time-series matches; NLL won't.
  Recruitment          = list(ss3 = "Recruitment",          rce_rows = c(10, 11, 12),
                              expected_match = FALSE),
  Forecast_Recruitment = list(ss3 = "Forecast_Recruitment", rce_rows = integer(0),
                              expected_match = FALSE),
  Parm_priors          = list(ss3 = "Parm_priors",          rce_rows = c(7, 9, 15, 20),
                              expected_match = TRUE),
  Parm_devs            = list(ss3 = "Parm_devs",            rce_rows = c(6, 8, 16),
                              expected_match = TRUE),
  Parm_softbounds      = list(ss3 = "Parm_softbounds",      rce_rows = integer(0),
                              expected_match = FALSE),
  Crash_Pen            = list(ss3 = "Crash_Pen",            rce_rows = 14,
                              expected_match = FALSE),
  F_Ballpark           = list(ss3 = "F_Ballpark",           rce_rows = 13,
                              expected_match = FALSE)
)

rel_err <- function(a, b) {
  if (is.na(a) || is.na(b)) return(NA_real_)
  denom <- max(abs(b), 1e-10)
  abs(a - b) / denom
}

ll_compare <- do.call(rbind, lapply(names(ll_map), function(nm) {
  m       <- ll_map[[nm]]
  ss3_v   <- if (m$ss3 %in% names(ss3_lik)) unname(ss3_lik[m$ss3]) else NA_real_
  rce_v   <- if (length(m$rce_rows)) sum(sapply(m$rce_rows, rce_row_sum)) else 0
  data.frame(
    Component  = nm,
    SS3_row    = m$ss3,
    Rce_rows   = paste(m$rce_rows, collapse = ","),
    SS3        = ss3_v,
    Rceattle   = rce_v,
    AbsDiff    = if (is.na(ss3_v)) NA_real_ else abs(rce_v - ss3_v),
    RelErr     = rel_err(rce_v, ss3_v),
    Gated      = m$expected_match,
    stringsAsFactors = FALSE
  )
}))

cat("\n=== Likelihood-component comparison (cod_ss3_fixed vs SS3) ===\n")
print(ll_compare, row.names = FALSE, digits = 6)

# TOTAL: Rceattle's `jnll` is the sum of all 20 rows (incl. internal penalties)
# whereas SS3's TOTAL is the sum of its (un-skipped) component rows.
rce_total_jnll <- if (!is.null(cod_ss3_fixed$quantities$jnll))
  cod_ss3_fixed$quantities$jnll else sum(jnll_mat)
ss3_total      <- ss3_lik[["TOTAL"]]
rce_total_agg  <- sum(ll_compare$Rceattle, na.rm = TRUE)

cat(sprintf("\nTOTAL NLL:\n  SS3 TOTAL             = %.6f\n", ss3_total))
cat(sprintf("  Rceattle jnll (full)  = %.6f\n", rce_total_jnll))
cat(sprintf("  Rceattle (mapped sum) = %.6f   rel err vs SS3: %.2e\n",
            rce_total_agg, rel_err(rce_total_agg, ss3_total)))

# ---- Component-wise tolerance check (gated rows only) ----------------------
tol_lik <- 1e-3
gated   <- ll_compare[ll_compare$Gated, ]
fail    <- gated[!is.na(gated$RelErr) & gated$RelErr > tol_lik & gated$SS3 != 0, ]
if (nrow(fail) == 0) {
  cat(sprintf("\nAll gated components within tol = %.0e.\n", tol_lik))
} else {
  cat(sprintf("\nGated components EXCEEDING tol = %.0e:\n", tol_lik))
  print(fail, row.names = FALSE, digits = 6)
}
cat("\nUn-gated buckets (expected-divergence, informational only):\n")
print(ll_compare[!ll_compare$Gated, c("Component", "SS3", "Rceattle", "AbsDiff", "RelErr")],
      row.names = FALSE, digits = 6)

# ---- Optional: fleet-level breakdown for the worst component ---------------
if (!is.null(ss3_rep$likelihoods_by_fleet)) {
  fleet_ll <- ss3_rep$likelihoods_by_fleet
  cat("\n=== SS3 likelihoods_by_fleet (for drill-down) ===\n")
  print(fleet_ll[, intersect(c("Label", "ALL", fleet_meta$name),
                             colnames(fleet_ll))])

  cat("\nRceattle jnll_comp by column (fleet/species index):\n")
  for (r in c(1, 2, 3, 4, 6)) {
    cat(sprintf("  row %2d (%-12s):  %s\n",
                r,
                switch(as.character(r),
                       "1" = "Survey",
                       "2" = "Catch",
                       "3" = "Comp",
                       "4" = "CAAL",
                       "6" = "Sel_dev"),
                paste(sprintf("%.4f", jnll_mat[r, ]), collapse = "  ")))
  }
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
