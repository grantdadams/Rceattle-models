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


# ============================================================================
# 0.  Paths and data
# ============================================================================
SS3_DIR   <- "Data/goa_pcod"
PAR_FILE  <- file.path(SS3_DIR, "ss3.par")
DAT_FILE  <- file.path(SS3_DIR, "GOAPcod2024Oct17_1e_5cm.dat")
CTL_FILE  <- file.path(SS3_DIR, "Model19_1e.ctl")
TS_FILE   <- "Data/2024pcod_time_series.csv"
RCEATTLE_DATA <- "Data/GOA_24_pcod_single_species_1977-2024_w_CAAL.xlsx"

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
init_from_ss3_par <- function(parlist, inits, data_list, fleet_meta, years_hind) {

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
  # rec_dev[sp, yr] — one row per species, one column per hindcast year
  rec_devs <- do.call(rbind, Filter(Negate(is.null), list(
    parlist$recdev_early,
    parlist$recdev1,
    parlist$recdev2
  )))
  if ("rec_dev" %in% names(inits) && !is.null(rec_devs)) {
    n_set <- 0
    for (i in seq_len(nrow(rec_devs))) {
      yr_pos <- which(years_hind == rec_devs[i, "year"])
      if (length(yr_pos)) {
        inits$rec_dev[1, yr_pos] <- rec_devs[i, "recdev"]
        n_set <- n_set + 1
      }
    }
    cat(sprintf("Recruitment deviates set for %d years\n", n_set))
  } else if (!"rec_dev" %in% names(inits)) {
    warning("rec_dev not in inits — check names(mod0$estimated_params)")
  }

  # --- 3d. Von Bertalanffy growth ---
  # log_growth_pars[sp, sex, par]: par 1=log_K, 2=log_L1, 3=log_Linf, 4=log_m
  # Rceattle uses L1 = L(a_min) directly — no t0 needed.
  # Linf derived from: L_at_Amax = Linf - (Linf - L1)*exp(-K*(a_max - a_min))
  L_min <- get_par(parlist$MG_parms, "L_at_Amin")
  L_max <- get_par(parlist$MG_parms, "L_at_Amax")
  K_vb  <- get_par(parlist$MG_parms, "VonBert_K")
  SD_y  <- get_par(parlist$MG_parms, "CV_young")   # "CV" in SS3 par name; absolute SD when CV_Growth_Pattern=2
  SD_o  <- get_par(parlist$MG_parms, "CV_old")

  if (!is.null(L_min) && !is.null(L_max) && !is.null(K_vb) &&
      "log_growth_pars" %in% names(inits)) {
    a_min    <- data_list$minage[1]
    a_max    <- a_min + data_list$nages[1] - 1
    delta    <- exp(-K_vb * (a_max - a_min))
    Linf_est <- (L_max - L_min * delta) / (1 - delta)
    cat(sprintf("Growth: L1=%.3f, Linf=%.2f, K=%.4f\n", L_min, Linf_est, K_vb))
    inits$log_growth_pars[1, 1, 1] <- log(K_vb)
    inits$log_growth_pars[1, 1, 2] <- log(L_min)
    inits$log_growth_pars[1, 1, 3] <- log(Linf_est)
  } else if (!"log_growth_pars" %in% names(inits)) {
    warning("log_growth_pars not in inits — add growthFun to mod0")
  }
  if (!is.null(SD_y) && "growth_log_sd" %in% names(inits))
    inits$growth_log_sd[1, 1, 1] <- log(SD_y)
  if (!is.null(SD_o) && "growth_log_sd" %in% names(inits))
    inits$growth_log_sd[1, 1, 2] <- log(SD_o)

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
# 5.  Get Rceattle parameter structure, then fill from SS3 par file
# ============================================================================
mod0 <- Rceattle::fit_mod(
  data_list    = cod_ss3,
  inits        = NULL,
  file         = NULL,
  estimateMode = 3,
  growthFun    = build_growth(growth_model = 1),
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
  growthFun    = build_growth(growth_model = 1),
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  verbose      = 1,
  phase        = FALSE
)

# Model B: Estimate from SS3 starting values
cod_ss3_est <- Rceattle::fit_mod(
  data_list    = cod_ss3,
  inits        = inits,
  file         = NULL,
  estimateMode = 0,
  growthFun    = build_growth(growth_model = 1),
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
# 8.  Comparison plots
# ============================================================================
model_list  <- list(cod_ss3_fixed, cod_ss3_est, cod_base, safe2024)
model_names <- c("Rceattle (SS3 fixed)", "Rceattle (estimated)", "Rceattle (base)", "SS3")

plot_biomass(model_list,     model_names = model_names)
plot_ssb(model_list,         model_names = model_names)
plot_recruitment(model_list, model_names = model_names)
plot_selectivity(list(cod_ss3_fixed, safe2024),
                 model_names = c("Rceattle (SS3 fixed)", "SS3"))
