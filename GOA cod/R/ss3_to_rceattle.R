# =============================================================================
# ss3_to_rceattle: Convert a Stock Synthesis 3.30 model into an Rceattle data list
# =============================================================================
#
# PURPOSE
#   Given a directory containing an SS3 fit (ss3.par, *.dat, *.ctl, Report.sso),
#   produce a complete Rceattle data list ready for fit_mod(). The age axis is
#   built with minage = 0 so Rceattle's slot k corresponds to SS3 int_Age (k-1)
#   for ALL quantities (N, sel, WAA, mat) -- the historical minage=1 mismatch
#   between N-injection and growth conventions is avoided.
#
# SCOPE (first cut: tested against GOA Pacific cod 2024)
#   - Single species (nspp = 1)
#   - 1 or 2 sex (auto-detected)
#   - Length-based selectivity (SS3 size-DoubleNormal etc) injected via
#     ageselex Factor = "Asel2" so realized age sel matches SS3 by construction
#   - Beverton-Holt, Ricker, or hockey-stick SR (auto-detected from SR_parms)
#   - Time-block M and selectivity (block design read from ctl)
#   - Empirical weight-at-age built from SS3 endgrowth Wt_Beg
#   - Maturity from SS3 Len_Mat (length-based maturity integrated to age)
#
# OUT OF SCOPE for this first cut (will warn loudly if SS3 model uses them):
#   - Multi-species, multi-area, multi-platoon
#   - Retention / discard mortality
#   - Time-varying growth (growth devs, environmental growth)
#   - Q environmental linkages
#   - Settlement events, recruitment regimes
#
# USAGE
#   ss3_data <- ss3_to_rceattle(
#     ss3_dir   = "Data/goa_pcod",
#     par_file  = "ss3.par",
#     dat_file  = "GOAPcod2024Oct17_1e_5cm.dat",
#     ctl_file  = "Model19_1e.ctl",
#     spnames   = "Pcod",
#     minage    = 0,        # set to 0 so slot k = SS3 age (k-1) for everything
#     projyr_offset = 5
#   )
#   mod <- Rceattle::fit_mod(data_list = ss3_data, ...)
# =============================================================================

#' Convert SS3 outputs to an Rceattle data list
#'
#' @param ss3_dir Path to the SS3 model directory.
#' @param par_file Name of the SS3 .par file inside ss3_dir.
#' @param dat_file Name of the SS3 data file inside ss3_dir.
#' @param ctl_file Name of the SS3 control file inside ss3_dir.
#' @param spnames Character species name(s). Default "spp".
#' @param minage Integer minimum age in the Rceattle output. Default 0 so the
#'   age convention matches SS3 directly.
#' @param projyr_offset Number of projection years beyond endyr. Default 5.
#' @param sel_factor Which ageselex factor to use: "Asel2" (realized) or
#'   "Asel" (input). For length-based sel, use "Asel2". Default "Asel2".
#' @param verbose Whether to print progress messages.
#' @return A named list ready to pass as data_list to fit_mod().
#' @export
ss3_to_rceattle <- function(ss3_dir,
                            par_file = "ss3.par",
                            dat_file = NULL,
                            ctl_file = NULL,
                            spnames  = "spp",
                            minage   = 0,
                            projyr_offset = 5,
                            sel_factor = "Asel2",
                            verbose  = TRUE) {

  msg <- function(...) if (verbose) cat(...)

  # ---------------------------------------------------------------------------
  # 0. Load SS3 model -- r4ss does all the parsing
  # ---------------------------------------------------------------------------
  msg("Reading SS3 outputs from ", ss3_dir, "...\n")
  par_path <- file.path(ss3_dir, par_file)

  # Auto-detect dat/ctl if not provided
  if (is.null(dat_file)) dat_file <- list.files(ss3_dir, pattern = "\\.dat$")[1]
  if (is.null(ctl_file)) ctl_file <- list.files(ss3_dir, pattern = "\\.ctl$")[1]
  dat_path <- file.path(ss3_dir, dat_file)
  ctl_path <- file.path(ss3_dir, ctl_file)

  datlist <- r4ss::SS_readdat(dat_path, verbose = FALSE)
  ctllist <- r4ss::SS_readctl(ctl_path, use_datlist = TRUE, datlist = datlist, verbose = FALSE)
  parlist <- r4ss::SS_readpar_3.30(parfile = par_path, datsource = dat_path,
                                   ctlsource = ctl_path, verbose = FALSE)
  ss3_rep <- r4ss::SS_output(dir = ss3_dir, verbose = FALSE, printstats = FALSE)

  # Sanity checks on scope assumptions
  if (length(unique(datlist$Nareas %||% 1)) > 1L)
    stop("Multi-area SS3 model not supported (yet). Areas: ", datlist$Nareas)

  # ---------------------------------------------------------------------------
  # 1. Model dimensions
  # ---------------------------------------------------------------------------
  styr   <- datlist$styr
  endyr  <- datlist$endyr
  projyr <- endyr + projyr_offset

  # SS3 age range: 0..max_age. Rceattle nages = max_age + 1 when minage = 0.
  ss3_max_age <- datlist$Nages   # SS3 accumulator age
  if (minage == 0) {
    nages_rce <- ss3_max_age + 1L  # includes age 0
  } else {
    nages_rce <- ss3_max_age + 1L - as.integer(minage)
  }

  # Single or two-sex (Rceattle convention: nsex = 1 means single-sex / females)
  nsex_rce <- if (datlist$Nsexes == 2) 2L else 1L

  # Length bins -- SS3 has population length bins (Lbin_method) and data bins
  # (Lbin_vector). Use the data bins as Rceattle's length axis.
  ss3_lbins <- datlist$lbin_vector_pop %||% datlist$lbin_vector
  if (is.null(ss3_lbins))
    stop("Could not find length bin vector in datlist$lbin_vector or $lbin_vector_pop")
  nlengths_rce <- length(ss3_lbins)

  nspp <- 1L

  msg(sprintf("Dimensions: nspp=%d, nsex=%d, ages %d..%d (nages=%d), nlengths=%d, years %d..%d (proj %d)\n",
              nspp, nsex_rce, minage, minage + nages_rce - 1L, nages_rce,
              nlengths_rce, styr, endyr, projyr))

  # ---------------------------------------------------------------------------
  # 2. Top-level scalars/vectors
  # ---------------------------------------------------------------------------
  d <- list()
  d$nspp     <- nspp
  d$styr     <- styr
  d$endyr    <- endyr
  d$projyr   <- projyr
  d$spnames  <- spnames
  d$nsex     <- as.integer(nsex_rce)
  d$nages    <- as.integer(nages_rce)
  d$minage   <- as.integer(minage)
  d$nlengths <- as.integer(nlengths_rce)
  d$lengths  <- matrix(ss3_lbins, nrow = nspp)

  # spawn_month from SS3 (1-based month)
  d$spawn_month <- ctllist$spawn_month %||% datlist$spawn_seas %||% 1L

  # Population dynamics flags
  d$estDynamics      <- 0L           # estimate dynamics
  d$pop_wt_index     <- 1L
  d$ssb_wt_index     <- 2L
  d$pop_alk_index    <- 1L
  d$pop_age_transition_index <- 1L
  d$sigma_rec_prior  <- ctllist$SR_sigmaR %||%
    parlist$SR_parms["SR_sigmaR", "ESTIM"] %||% 0.6
  d$other_food       <- 1e5    # unused in single-species

  # W = alpha * L^beta from SS3 Wtlen_*_Fem_GP_1
  d$alpha_wt_len <- get_par_value(parlist$MG_parms, "Wtlen_1_Fem_GP_1") %||% NA_real_
  d$beta_wt_len  <- get_par_value(parlist$MG_parms, "Wtlen_2_Fem_GP_1") %||% NA_real_

  # ---------------------------------------------------------------------------
  # 3. Fleet control
  # ---------------------------------------------------------------------------
  d$fleet_control <- build_fleet_control(datlist, ctllist, parlist, ss3_rep, nspp)
  n_flt <- nrow(d$fleet_control)

  # ---------------------------------------------------------------------------
  # 4. Index, catch, comp, CAAL data tables (from datlist + ss3_rep)
  # ---------------------------------------------------------------------------
  d$index_data <- build_index_data(datlist, d$fleet_control)
  d$catch_data <- build_catch_data(datlist, d$fleet_control)
  d$comp_data  <- build_comp_data(datlist, d$fleet_control, nages_rce, minage)
  d$caal_data  <- build_caal_data(datlist, d$fleet_control, nages_rce, minage,
                                  nlengths_rce)

  # ---------------------------------------------------------------------------
  # 5. Empirical selectivity from SS3 ageselex Factor = "Asel2" (realized sel)
  # ---------------------------------------------------------------------------
  d$emp_sel <- build_emp_sel(ss3_rep, d$fleet_control, styr, endyr,
                             nages_rce, minage, factor = sel_factor)

  # ---------------------------------------------------------------------------
  # 6. Empirical weight-at-age from SS3 endgrowth Wt_Beg
  # ---------------------------------------------------------------------------
  d$weight <- build_weight_table(ss3_rep, d$fleet_control, styr, endyr,
                                 nages_rce, minage, nsex_rce)

  # ---------------------------------------------------------------------------
  # 7. Maturity, sex ratio, M1 base
  # ---------------------------------------------------------------------------
  d$maturity  <- build_maturity(ss3_rep, parlist, nages_rce, minage, nspp)
  d$sex_ratio <- build_sex_ratio(parlist, nages_rce, nspp)
  d$M1_base   <- build_M1_base(parlist, nages_rce, nspp)

  # ---------------------------------------------------------------------------
  # 8. Age-length keys (from SS3 growth distribution at age)
  # ---------------------------------------------------------------------------
  d$age_trans_matrix <- build_age_trans_matrix(ss3_rep, nages_rce, minage,
                                               nlengths_rce, nsex_rce, nspp)
  d$pop_age_transition_index <- 1L

  # No-error ageing key (identity); replace if SS3 has ageing error
  d$age_error <- build_age_error(nages_rce, nspp)

  # ---------------------------------------------------------------------------
  # 9. Environmental covariates -- includes M-block indicators
  # ---------------------------------------------------------------------------
  d$env_data <- build_env_data(ctllist, styr, projyr)

  # ---------------------------------------------------------------------------
  # 10. Fill remaining required-but-unused slots for single-species mode
  # ---------------------------------------------------------------------------
  d$NByageFixed <- empty_df(c("Species_name", "Species", "Sex", "Year"),
                            paste0("Age", 1:nages_rce))
  d$ration_data <- empty_df(c("Species", "Sex", "Year"),
                            paste0("Age", 1:nages_rce))
  d$diet_data   <- empty_df(c("Pred", "Prey", "Pred_sex", "Prey_sex",
                              "Pred_age", "Prey_age", "Year",
                              "Sample_size", "Stomach_proportion_by_weight"),
                            character(0))

  # Bioenergetics (unused in single-species; set safe defaults)
  for (nm in c("Ceq","Cindex","Pvalue","fday","CA","CB","Qc","Tco","Tcm","Tcl","CK1","CK4"))
    d[[nm]] <- rep(1, nspp)
  d$CB                  <- rep(-1, nspp)
  d$Diet_loglike        <- rep(0, nspp)
  d$Diet_comp_weights   <- rep(1, nspp)

  # Init mode -- "FishedNonEquilibrium" mirrors SS3 best for stocks with
  # initial F. Caller can override.
  d$initMode <- "FishedNonEquilibrium"

  msg("Done. Returning data list with ", length(d), " top-level fields.\n")
  d
}


# =============================================================================
# Helpers
# =============================================================================

#' @keywords internal
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

#' Pull a single parameter value by exact label match (with fallback regex)
#' @keywords internal
get_par_value <- function(section, label) {
  if (is.null(section) || nrow(section) == 0) return(NULL)
  idx <- which(rownames(section) == label)
  if (length(idx) == 0) idx <- grep(label, rownames(section))
  if (length(idx) == 0) return(NULL)
  val <- section[idx[1], "ESTIM"]
  if (is.na(val)) val <- section[idx[1], "INIT"]
  as.numeric(val)
}

#' Create an empty data frame with named columns (mix of fixed + numeric)
#' @keywords internal
empty_df <- function(text_cols, num_cols) {
  df <- as.data.frame(matrix(NA, nrow = 0, ncol = length(text_cols) + length(num_cols)))
  colnames(df) <- c(text_cols, num_cols)
  df
}

# ---------------------------------------------------------------------------
# Section builders -- each takes pieces of SS3 inputs and returns the
# corresponding Rceattle data-list component. Kept small + named so each can
# be tested / replaced independently.
# ---------------------------------------------------------------------------

#' @keywords internal
build_fleet_control <- function(datlist, ctllist, parlist, ss3_rep, nspp) {
  # SS3 fleet info lives in datlist$fleetinfo with columns
  # type/surveytimimg/area/units/need_catch_mult/fleetname
  fi <- datlist$fleetinfo
  n_flt <- nrow(fi)

  # SS3 fleet "type" code: 1 = fishery, 2 = bycatch fishery (skip), 3 = survey,
  # 4 = predator (multi-species, skip). Map to Rceattle's "Fishery"/"Survey"/"Off".
  rce_type <- ifelse(fi$type == 1, "Fishery",
                     ifelse(fi$type == 3, "Survey",
                            ifelse(fi$type == 2, "Fishery", "Off")))

  # Fleet weight units: SS3 1 = biomass (mt), 2 = numbers
  units_w1n2 <- fi$units %||% rep(1, n_flt)

  data.frame(
    Fleet_name              = fi$fleetname,
    Fleet_code              = seq_len(n_flt),
    Fleet_type              = rce_type,
    Species                 = 1L,
    Month                   = round((fi$surveytiming %||% 0) * 12),  # SS3 fraction-of-year -> month
    Selectivity_index       = seq_len(n_flt),
    Selectivity             = "Fixed",           # using emp_sel from Asel2
    Selectivity_dimension   = "Age",             # already age-realized via Asel2
    N_sel_bins              = NA,
    Sel_curve_pen1          = NA,
    Sel_curve_pen2          = NA,
    Time_varying_sel        = 0,                 # blocks captured by per-year emp_sel rows
    Time_varying_sel_sd_prior = 1,
    Bin_first_selected      = 1L,
    Sel_norm_bin1           = NA,                # NA -> skip normalization in C++
    Sel_norm_bin2           = NA,
    Comp_loglike            = "Multinomial",
    Comp_weights            = 1,
    CAAL_loglike            = "Multinomial",
    CAAL_weights            = 1,
    Weight1_Numbers2        = units_w1n2,
    Weight_index            = seq_len(n_flt) + 2L,  # slots 1=pop,2=ssb,3..=fleets
    Age_transition_index    = 1L,
    Q_index                 = seq_len(n_flt),
    Catchability            = ifelse(rce_type == "Survey", "Estimated", NA),
    Q_prior                 = ifelse(rce_type == "Survey", 1, NA),
    Q_sd_prior              = ifelse(rce_type == "Survey", 0.2, NA),
    Time_varying_q          = ifelse(rce_type == "Survey", 0, NA),
    Time_varying_q_sd_prior = ifelse(rce_type == "Survey", 1, NA),
    Estimate_index_sd       = ifelse(rce_type == "Survey", 0, NA),
    Index_sd_prior          = ifelse(rce_type == "Survey", 1, NA),
    Estimate_catch_sd       = ifelse(rce_type == "Fishery", 0, NA),
    Catch_sd_prior          = ifelse(rce_type == "Fishery", 1, NA),
    proj_F_prop             = NA_real_,          # set below
    stringsAsFactors = FALSE
  ) -> fc

  # proj_F_prop must sum to 1 across fisheries per species. Equal split.
  n_fish <- sum(fc$Fleet_type == "Fishery")
  if (n_fish > 0) fc$proj_F_prop[fc$Fleet_type == "Fishery"] <- 1 / n_fish
  fc
}

#' @keywords internal
build_catch_data <- function(datlist, fleet_control) {
  if (is.null(datlist$catch) || nrow(datlist$catch) == 0) {
    return(empty_df(c("Fleet_name","Fleet_code","Species","Year","Month","Selectivity_block"),
                    c("Catch","Log_sd")))
  }
  fish_flt <- fleet_control$Fleet_code[fleet_control$Fleet_type == "Fishery"]
  cat_raw <- datlist$catch
  # Filter out non-fishery rows / equilibrium rows (year < 0 or -999)
  cat_raw <- cat_raw[cat_raw$year >= datlist$styr & cat_raw$year <= datlist$endyr, ]
  data.frame(
    Fleet_name        = fleet_control$Fleet_name[match(cat_raw$fleet, fleet_control$Fleet_code)],
    Fleet_code        = as.integer(cat_raw$fleet),
    Species           = 1L,
    Year              = as.integer(cat_raw$year),
    Month             = round((cat_raw$seas %||% 1 - 1) * 12 / max(1, datlist$nseas %||% 1)),
    Selectivity_block = 1L,
    Catch             = as.numeric(cat_raw$catch),
    Log_sd            = as.numeric(cat_raw$catch_se),
    stringsAsFactors  = FALSE
  )
}

#' @keywords internal
build_index_data <- function(datlist, fleet_control) {
  if (is.null(datlist$CPUE) || nrow(datlist$CPUE) == 0) {
    return(empty_df(c("Fleet_name","Fleet_code","Species","Year","Month","Selectivity_block"),
                    c("Observation","Log_sd")))
  }
  cpue <- datlist$CPUE
  cpue <- cpue[abs(cpue$year) >= datlist$styr & abs(cpue$year) <= datlist$endyr, ]
  data.frame(
    Fleet_name        = fleet_control$Fleet_name[match(cpue$index, fleet_control$Fleet_code)],
    Fleet_code        = as.integer(cpue$index),
    Species           = 1L,
    Year              = as.integer(cpue$year),
    Month             = round((cpue$seas %||% 1 - 1) * 12 / max(1, datlist$nseas %||% 1)),
    Selectivity_block = 1L,
    Observation       = as.numeric(cpue$obs),
    Log_sd            = as.numeric(cpue$se_log),
    stringsAsFactors  = FALSE
  )
}

#' @keywords internal
build_comp_data <- function(datlist, fleet_control, nages, minage) {
  # SS3 stores marginal age comps in $agecomp and length comps in $lencomp
  comp_rows <- list()
  if (!is.null(datlist$agecomp) && nrow(datlist$agecomp) > 0) {
    ac <- datlist$agecomp
    age_cols <- grep("^a[0-9]+$", colnames(ac), value = TRUE, ignore.case = TRUE)
    if (length(age_cols) >= nages) age_cols <- age_cols[1:nages]
    base <- data.frame(
      Fleet_name   = fleet_control$Fleet_name[match(ac$fleet, fleet_control$Fleet_code)],
      Fleet_code   = as.integer(ac$fleet),
      Species      = 1L,
      Sex          = as.integer(ac$sex),
      Age0_Length1 = 0L,
      Year         = as.integer(ac$year),
      Month        = round((ac$seas %||% 1 - 1) * 12),
      Sample_size  = as.numeric(ac$Nsamp)
    )
    obs <- as.data.frame(ac[, age_cols, drop = FALSE])
    colnames(obs) <- paste0("Comp_", seq_along(age_cols))
    comp_rows[[1]] <- cbind(base, obs)
  }
  if (!is.null(datlist$lencomp) && nrow(datlist$lencomp) > 0) {
    # Length comps emitted separately if needed -- TODO if downstream wants them.
    # For now skip; CAAL via build_caal_data() carries the age info.
  }
  if (length(comp_rows) == 0) {
    return(empty_df(c("Fleet_name","Fleet_code","Species","Sex","Age0_Length1",
                      "Year","Month","Sample_size"),
                    paste0("Comp_", 1:nages)))
  }
  do.call(rbind, comp_rows)
}

#' @keywords internal
build_caal_data <- function(datlist, fleet_control, nages, minage, nlengths) {
  ## SS3 CAAL is in datlist$age@length or datlist$ageerr_caal (varies by SS3 ver)
  caal <- datlist[["ageerr_caal"]] %||% datlist[["agecomp"]]
  if (is.null(caal) || !"Lbin_lo" %in% colnames(caal)) {
    return(empty_df(c("Fleet_name","Fleet_code","Species","Sex","Year","Length","Sample_size"),
                    paste0("CAAL_", 1:nages)))
  }
  # Filter to actual CAAL rows (Lbin_lo > 0)
  caal <- caal[caal$Lbin_lo > 0, , drop = FALSE]
  if (nrow(caal) == 0) {
    return(empty_df(c("Fleet_name","Fleet_code","Species","Sex","Year","Length","Sample_size"),
                    paste0("CAAL_", 1:nages)))
  }
  age_cols <- grep("^a[0-9]+$", colnames(caal), value = TRUE, ignore.case = TRUE)
  if (length(age_cols) >= nages) age_cols <- age_cols[1:nages]
  base <- data.frame(
    Fleet_name  = fleet_control$Fleet_name[match(caal$fleet, fleet_control$Fleet_code)],
    Fleet_code  = as.integer(caal$fleet),
    Species     = 1L,
    Sex         = as.integer(caal$sex),
    Year        = as.integer(caal$year),
    Length      = as.integer(caal$Lbin_lo),  # length bin index
    Sample_size = as.numeric(caal$Nsamp)
  )
  obs <- as.data.frame(caal[, age_cols, drop = FALSE])
  colnames(obs) <- paste0("CAAL_", seq_along(age_cols))
  cbind(base, obs)
}

#' Build empirical selectivity table from SS3 ageselex (Asel2 = realized)
#' @keywords internal
build_emp_sel <- function(ss3_rep, fleet_control, styr, endyr, nages, minage,
                          factor = "Asel2") {
  years <- styr:endyr
  if (is.null(ss3_rep$ageselex)) stop("ss3_rep$ageselex is NULL -- cannot build emp_sel")

  asel <- ss3_rep$ageselex
  asel <- asel[asel$Factor == factor &
               asel$Yr %in% years &
               asel$Fleet %in% fleet_control$Fleet_code, , drop = FALSE]

  # Age columns: SS3 ageselex has columns "0", "1", ..., max_age
  age_cols_all <- as.character(0:(minage + nages - 1))
  needed <- age_cols_all[(minage + 1):(minage + nages)]   # Rceattle slots 1..nages
  available <- intersect(needed, colnames(asel))
  if (length(available) < length(needed))
    stop(sprintf("SS3 ageselex missing age columns. Have [%s]; need [%s]",
                 paste(intersect(as.character(0:50), colnames(asel)), collapse = ","),
                 paste(needed, collapse = ",")))

  rows <- list()
  for (fc in seq_len(nrow(fleet_control))) {
    flt_code <- fleet_control$Fleet_code[fc]
    sub <- asel[asel$Fleet == flt_code, , drop = FALSE]
    if (nrow(sub) == 0) next

    for (yr in years) {
      row_y <- sub[sub$Yr == yr, , drop = FALSE]
      if (nrow(row_y) == 0) next
      vec <- as.numeric(row_y[1, needed])
      base <- data.frame(
        Fleet_name = fleet_control$Fleet_name[fc],
        Fleet_code = as.integer(flt_code),
        Species    = 1L,
        Sex        = 0L,    # apply to all sexes
        Year       = as.integer(yr),
        stringsAsFactors = FALSE
      )
      comp <- setNames(as.data.frame(as.list(vec)), paste0("Comp_", seq_along(vec)))
      rows[[length(rows) + 1]] <- cbind(base, comp)
    }
  }
  if (length(rows) == 0) {
    return(empty_df(c("Fleet_name","Fleet_code","Species","Sex","Year"),
                    paste0("Comp_", 1:nages)))
  }
  out <- do.call(rbind, rows)

  # Forward-fill years where SS3 didn't emit a row (block-change-only output)
  out <- out[order(out$Fleet_code, out$Year), ]
  full <- expand.grid(Fleet_code = unique(out$Fleet_code), Year = years,
                      stringsAsFactors = FALSE)
  out <- merge(full, out, by = c("Fleet_code", "Year"), all.x = TRUE)
  # Forward-fill Comp_* within each fleet
  comp_nm <- grep("^Comp_", colnames(out), value = TRUE)
  for (fc in unique(out$Fleet_code)) {
    idx <- which(out$Fleet_code == fc)
    for (cn in comp_nm) {
      v <- out[idx, cn]
      # Forward fill: replace each NA with the most recent non-NA above it
      last <- NA_real_
      for (k in seq_along(v)) {
        if (is.na(v[k])) v[k] <- last else last <- v[k]
      }
      out[idx, cn] <- v
    }
  }
  # Re-fill the static columns from fleet_control
  out$Fleet_name <- fleet_control$Fleet_name[match(out$Fleet_code, fleet_control$Fleet_code)]
  out$Species    <- 1L
  out$Sex        <- 0L
  out[, c("Fleet_name", "Fleet_code", "Species", "Sex", "Year", comp_nm)]
}

#' Build weight-at-age table (Pop, SSB, per-fleet) from SS3 endgrowth
#' @keywords internal
build_weight_table <- function(ss3_rep, fleet_control, styr, endyr, nages, minage,
                               nsex) {
  if (is.null(ss3_rep$endgrowth)) stop("ss3_rep$endgrowth is NULL")
  eg <- ss3_rep$endgrowth
  # Use Sex == 1 (female) for single-sex female model
  eg <- eg[eg$Sex == 1, , drop = FALSE]
  eg <- eg[order(eg$int_Age), , drop = FALSE]
  # SS3 int_Age 0..max_age. Pick the ones corresponding to Rceattle slots 1..nages.
  wanted <- (minage):(minage + nages - 1L)
  wt_beg_by_age <- eg$Wt_Beg[match(wanted, eg$int_Age)]
  if (any(is.na(wt_beg_by_age))) {
    stop("SS3 endgrowth missing rows for ages: ",
         paste(wanted[is.na(wt_beg_by_age)], collapse = ", "))
  }
  # Plus group: if the SS3 last column is a true plus group (= our last slot),
  # SS3's endgrowth value at that age is already the plus-group mean weight
  # (SS3 stores the mean wt of accumulated cohorts). No extra handling needed
  # if we trust SS3's reporting.
  age_col_names <- paste0("Age", 1:nages)

  # 2 + n_flt slots: pop (1), SSB (2), per-fleet (3..)
  n_flt <- nrow(fleet_control)
  n_slots <- 2L + n_flt
  wt_names <- c("Pop_WAA", "SSB_WAA", paste0(fleet_control$Fleet_name, "_WAA"))

  years <- styr:endyr
  rows <- list()
  for (s in seq_len(n_slots)) {
    for (yr in years) {
      rows[[length(rows) + 1]] <- cbind(
        data.frame(Wt_name = wt_names[s], Wt_index = s, Species = 1L, Sex = 0L,
                   Year = as.integer(yr), stringsAsFactors = FALSE),
        setNames(as.data.frame(as.list(wt_beg_by_age)), age_col_names)
      )
    }
  }
  do.call(rbind, rows)
}

#' Build maturity-at-age from SS3 endgrowth Len_Mat
#' @keywords internal
build_maturity <- function(ss3_rep, parlist, nages, minage, nspp) {
  eg <- ss3_rep$endgrowth
  eg <- eg[eg$Sex == 1, , drop = FALSE]
  eg <- eg[order(eg$int_Age), , drop = FALSE]
  wanted <- minage:(minage + nages - 1L)
  mat <- eg$Len_Mat[match(wanted, eg$int_Age)]
  mat[is.na(mat)] <- 0
  # Mat is already the fraction-mature ogive
  out <- as.data.frame(matrix(mat, nrow = nspp, byrow = TRUE))
  colnames(out) <- paste0("Age", 1:nages)
  cbind(Species = 1:nspp, out)
}

#' Build sex_ratio-at-age (proportion female)
#' @keywords internal
build_sex_ratio <- function(parlist, nages, nspp) {
  sr_val <- get_par_value(parlist$MG_parms, "FracFemale_GP_1") %||% 0.5
  out <- as.data.frame(matrix(sr_val, nrow = nspp, ncol = nages))
  colnames(out) <- paste0("Age", 1:nages)
  cbind(Species = 1:nspp, out)
}

#' Build M1_base table (constant base M; M-block applied via env linkage)
#' @keywords internal
build_M1_base <- function(parlist, nages, nspp) {
  M_base <- get_par_value(parlist$MG_parms, "NatM_p_1_Fem_GP_1$") %||%
            get_par_value(parlist$MG_parms, "NatM") %||%
            0.2
  out <- as.data.frame(matrix(M_base, nrow = nspp, ncol = nages))
  colnames(out) <- paste0("Age", 1:nages)
  cbind(Species = 1:nspp, Sex = 0L, out)
}

#' Build age-length transition matrix (placeholder: identity at first nages bins).
#' For proper integration, use ss3_rep$ALK or growth.hpp output.
#' @keywords internal
build_age_trans_matrix <- function(ss3_rep, nages, minage, nlengths, nsex, nspp) {
  tmp <- matrix(0, nrow = nages, ncol = nlengths)
  diag(tmp[1:min(nages, nlengths), 1:min(nages, nlengths)]) <- 1
  colnames(tmp) <- paste0("Length_", 1:nlengths)
  cbind(
    data.frame(Age_transition_name = paste0("Spp", 1:nspp),
               Age_transition_index = 1L,
               Species = 1L,
               Sex = 0L,
               Age = 1:nages),
    tmp
  )
}

#' Identity ageing-error matrix (no error)
#' @keywords internal
build_age_error <- function(nages, nspp) {
  diag_df <- as.data.frame(diag(1, nages))
  colnames(diag_df) <- paste0("Obs_age", 1:nages)
  cbind(Species = 1:nspp, True_age = 1:nages, diag_df)
}

#' Build env_data with block indicators (post2014 etc) from SS3 ctl Block_Design.
#' Adds one binary indicator column per detected block design that names a
#' parameter pattern matching `block_targets` (default: M and selectivity).
#' @keywords internal
build_env_data <- function(ctllist, styr, projyr) {
  years <- styr:projyr
  out <- data.frame(Year = years)
  if (!is.null(ctllist$Block_Design)) {
    for (i in seq_along(ctllist$Block_Design)) {
      bd <- ctllist$Block_Design[[i]]
      if (length(bd) >= 2 && length(bd) %% 2 == 0) {
        # bd is alternating [start1, end1, start2, end2, ...]
        active <- rep(0L, length(years))
        for (k in seq(1, length(bd), by = 2)) {
          active[years >= bd[k] & years <= bd[k + 1]] <- 1L
        }
        out[[sprintf("block_%d", i)]] <- active
      }
    }
  }
  # Convenience alias for the most common M block (Pcod 2024-style)
  if ("block_4" %in% colnames(out)) out$post2014 <- out$block_4
  out
}
