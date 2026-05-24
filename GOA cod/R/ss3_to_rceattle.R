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

  # Length bins -- SS3 has data bins (lbin_vector, used for CAAL/lencomp) and
  # finer population bins (lbin_vector_pop, used internally for ALK). CAAL
  # observations are reported on the *data* bins, so Rceattle's length axis
  # must match those or the per-length-bin unique-count check fails.
  ss3_lbins <- datlist$lbin_vector %||% datlist$lbin_vector_pop
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

  # Per-species vectors (length nspp) -- matches Build_data_without_excel.R
  d$nsex     <- rep(as.integer(nsex_rce), nspp)
  d$nages    <- rep(as.integer(nages_rce), nspp)
  d$minage   <- rep(as.integer(minage), nspp)
  d$nlengths <- rep(as.integer(nlengths_rce), nspp)

  # spawn_month: SS3 reports SSB at the START of `Spawn_seas`. For an annual
  # time step (nseas = 1), Spawn_seas = 1 means start of year => Rceattle
  # spawn_month = 0 (since Rceattle applies exp(-Z * spawn_month/12) to N
  # before SSB integration). Newer SS3 ctl files (3.30.20+) carry an explicit
  # spawn_month field; honor it if present, otherwise compute from
  # Spawn_seas + nseas. Don't use raw spawn_seas as a month (it's a season
  # index in [1, nseas], not a calendar month).
  spawn_m <- ctllist$spawn_month
  if (is.null(spawn_m) || is.na(spawn_m)) {
    nseas    <- max(1L, as.integer(datlist$nseas %||% 1L))
    spawn_seas <- as.integer(datlist$spawn_seas %||% 1L)
    # Start of Spawn_seas in months from Jan 1: (spawn_seas - 1) * (12 / nseas)
    spawn_m <- (spawn_seas - 1L) * (12 / nseas)
  }
  spawn_m <- max(0, min(12, as.numeric(spawn_m)))
  d$spawn_month <- rep(spawn_m, nspp)

  # VB anchor age (= SS3 Growth_Age_for_L1). Read from ctl; defaults to
  # max(0.5, minage) downstream in Rceattle::fit_mod() if NA.
  gal1_ss3 <- ctllist$Growth_Age_for_L1
  d$growth_age_L1 <- rep(if (is.null(gal1_ss3) || is.na(gal1_ss3))
                            NA_real_ else as.numeric(gal1_ss3), nspp)

  # Population dynamics flags
  d$estDynamics      <- rep(0L, nspp)       # estimate dynamics
  d$pop_wt_index     <- rep(1L, nspp)        # use Wt_index = 1 for total biomass
  d$ssb_wt_index     <- rep(2L, nspp)        # use Wt_index = 2 for SSB
  d$pop_age_transition_index <- rep(1L, nspp)
  d$sigma_rec_prior  <- rep(ctllist$SR_sigmaR %||%
    get_par_value(parlist$SR_parms, "SR_sigmaR") %||% 0.6, nspp)
  d$other_food       <- rep(1e5, nspp)       # unused in single-species

  # W = alpha * L^beta from SS3 Wtlen_*_Fem_GP_1
  d$alpha_wt_len <- rep(get_par_value(parlist$MG_parms, "Wtlen_1_Fem_GP_1") %||% NA_real_, nspp)
  d$beta_wt_len  <- rep(get_par_value(parlist$MG_parms, "Wtlen_2_Fem_GP_1") %||% NA_real_, nspp)

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
  d$comp_data  <- build_comp_data(datlist, d$fleet_control, nages_rce, minage,
                                   nlengths_rce)
  d$caal_data  <- build_caal_data(datlist, d$fleet_control, nages_rce, minage,
                                  nlengths_rce, ss3_lbins)

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
  d$age_error <- build_age_error(nages_rce, nspp, minage = minage)

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

  # Turn off fleets that have no data contributing to the likelihood. SS3 lets
  # users carry ghost surveys (all observation Year < 0) for diagnostic
  # plotting; in Rceattle those would still cost selectivity / Q parameters
  # without any data anchoring them, so set Fleet_type = "Off" and let the
  # downstream switches/maps drop them cleanly.
  d <- mark_inactive_fleets_off(d, msg = msg)

  msg("Done. Returning data list with ", length(d), " top-level fields.\n")
  d
}

#' Set Fleet_type = "Off" for any fleet whose observation tables contribute
#' nothing to the likelihood. A fleet is considered active if any of
#' catch_data, index_data, comp_data, or caal_data has at least one row with
#' Year > 0 referencing that Fleet_code. Returns the modified data_list.
#' @keywords internal
mark_inactive_fleets_off <- function(d, msg = function(...) invisible()) {
  active_codes <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(integer(0))
    if (!all(c("Fleet_code", "Year") %in% colnames(df))) return(integer(0))
    df <- df[!is.na(df$Year) & df$Year > 0 & !is.na(df$Fleet_code), , drop = FALSE]
    if (nrow(df) == 0) return(integer(0))
    unique(as.integer(df$Fleet_code))
  }
  active <- unique(c(
    active_codes(d$catch_data),
    active_codes(d$index_data),
    active_codes(d$comp_data),
    active_codes(d$caal_data)
  ))
  inactive_idx <- which(!d$fleet_control$Fleet_code %in% active &
                          d$fleet_control$Fleet_type != "Off")
  if (length(inactive_idx) > 0) {
    msg("Auto-Off fleets with no active observations: ",
        paste(d$fleet_control$Fleet_name[inactive_idx], collapse = ", "), "\n")
    d$fleet_control$Fleet_type[inactive_idx] <- "Off"
    # Off-fleets shouldn't claim projection F or estimated Q
    d$fleet_control$proj_F_prop[inactive_idx]  <- NA_real_
    d$fleet_control$Catchability[inactive_idx] <- NA
  }
  d
}


# =============================================================================
# Helpers
# =============================================================================

#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}

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

  # SS3 surveytiming: fraction-of-year in [0, 1], or a negative sentinel meaning
  # "use catch month". Map to a valid Rceattle Month in [0, 12]; clamp negatives
  # to 0 (start of year, the safest default).
  st_raw <- fi$surveytiming %||% rep(0, n_flt)
  st_raw[is.na(st_raw)] <- 0
  st_month <- pmax(0, pmin(12, round(st_raw * 12)))

  data.frame(
    Fleet_name              = fi$fleetname,
    Fleet_code              = seq_len(n_flt),
    Fleet_type              = rce_type,
    Species                 = 1L,
    Month                   = st_month,
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

#' SS3 ghost-observation convention: a negative `fleet` (or in some tables,
#' negative `year`) flags a row that is *predicted but not included in the
#' likelihood*. Rceattle's equivalent is negative `Year`. Returns a data frame
#' with absolute-valued Fleet_code and Year negated where SS3 ghosted the row.
#' @keywords internal
normalize_ss3_ghosts <- function(df, fleet_col, year_col = "year") {
  if (is.null(df) || nrow(df) == 0) return(df)
  flt   <- df[[fleet_col]]
  yr    <- df[[year_col]]
  ghost <- (flt < 0) | (yr < 0)
  df[[fleet_col]] <- abs(flt)
  df[[year_col]]  <- ifelse(ghost, -abs(yr), abs(yr))
  df
}

#' @keywords internal
build_catch_data <- function(datlist, fleet_control) {
  if (is.null(datlist$catch) || nrow(datlist$catch) == 0) {
    return(empty_df(c("Fleet_name","Fleet_code","Species","Year","Month","Selectivity_block"),
                    c("Catch","Log_sd")))
  }
  cat_raw <- datlist$catch
  cat_raw <- normalize_ss3_ghosts(cat_raw, "fleet", "year")
  # Equilibrium catch (SS3 year = -999, etc) is below styr after negation.
  # Keep only hindcast-window rows; SS3 catch is treated as known per fleet/year.
  cat_raw <- cat_raw[abs(cat_raw$year) >= datlist$styr & abs(cat_raw$year) <= datlist$endyr, ]
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
  cpue <- normalize_ss3_ghosts(cpue, "index", "year")
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

#' Detect SS3 agecomp/CAAL age frequency columns. r4ss varies by version:
#' `a0,a1,...`, `f0,f1,...`, or bare numerics `0,1,...`. Pick whichever the
#' frame uses; returns NULL if none match.
#' @keywords internal
detect_age_cols <- function(df) {
  for (pat in c("^a[0-9]+$", "^f[0-9]+$", "^[0-9]+$")) {
    nm <- grep(pat, colnames(df), value = TRUE, ignore.case = TRUE)
    if (length(nm) > 0) return(nm)
  }
  NULL
}

#' Split SS3 agecomp into marginal (Lbin_lo <= 0) and CAAL (Lbin_lo > 0) rows.
#' SS3 uses Lbin_lo = -1 as a sentinel for marginal age comps; CAAL rows carry
#' a positive Lbin_lo (and Lbin_hi). Returns list(marginal=..., caal=...).
#' @keywords internal
split_agecomp <- function(datlist) {
  ac <- datlist$agecomp
  if (is.null(ac) || nrow(ac) == 0) return(list(marginal = NULL, caal = NULL))
  if (!"Lbin_lo" %in% colnames(ac)) {
    # No Lbin_lo column => all rows are marginal
    return(list(marginal = ac, caal = NULL))
  }
  list(
    marginal = ac[ac$Lbin_lo <= 0, , drop = FALSE],
    caal     = ac[ac$Lbin_lo  > 0, , drop = FALSE]
  )
}

#' Pad an age-comp frame so it has exactly nages age columns (1..nages).
#' SS3 dat files often declare fewer comp bins than the population nages
#' (e.g., comp ages 1..10 but population ages 0..10). Missing trailing bins
#' (older ages) are filled with 0; missing leading bins (young ages dropped
#' when minage shifts) are also filled with 0.
#' @keywords internal
pad_comp_cols <- function(obs, nages, prefix) {
  n_have <- ncol(obs)
  if (n_have >= nages) {
    obs <- obs[, 1:nages, drop = FALSE]
  } else {
    pad <- as.data.frame(matrix(0, nrow = nrow(obs), ncol = nages - n_have))
    obs <- cbind(obs, pad)
  }
  colnames(obs) <- paste0(prefix, "_", seq_len(nages))
  obs
}

#' Detect SS3 length-comp frequency columns. r4ss names them `l<lower-edge>`
#' (e.g., `l4.5`, `l9.5`, ...). Returns NULL if none match.
#' @keywords internal
detect_length_cols <- function(df) {
  nm <- grep("^l[0-9]+(\\.[0-9]+)?$", colnames(df), value = TRUE, ignore.case = TRUE)
  if (length(nm) > 0) return(nm)
  NULL
}

#' @keywords internal
build_comp_data <- function(datlist, fleet_control, nages, minage, nlengths) {
  ncomp <- max(nages, nlengths)
  empty_template <- empty_df(c("Fleet_name","Fleet_code","Species","Sex","Age0_Length1",
                              "Year","Month","Sample_size"),
                            paste0("Comp_", seq_len(ncomp)))

  rows_out <- list()

  # ---- Marginal age comp (datlist$agecomp rows with Lbin_lo <= 0) ----
  split <- split_agecomp(datlist)
  ac <- split$marginal
  if (!is.null(ac) && nrow(ac) > 0) {
    ac <- normalize_ss3_ghosts(ac, "fleet", "year")
    age_cols <- detect_age_cols(ac)
    if (!is.null(age_cols)) {
      if (length(age_cols) >= (minage + nages)) {
        age_cols <- age_cols[(minage + 1):(minage + nages)]
      }
      obs <- pad_comp_cols(as.data.frame(ac[, age_cols, drop = FALSE]), ncomp, "Comp")
      rows_out[[length(rows_out) + 1]] <- cbind(
        data.frame(
          Fleet_name   = fleet_control$Fleet_name[match(ac$fleet, fleet_control$Fleet_code)],
          Fleet_code   = as.integer(ac$fleet),
          Species      = 1L,
          Sex          = as.integer(ac$sex),
          Age0_Length1 = 0L,
          Year         = as.integer(ac$year),
          Month        = round((ac$seas %||% 1 - 1) * 12 / max(1, datlist$nseas %||% 1)),
          Sample_size  = as.numeric(ac$Nsamp),
          stringsAsFactors = FALSE
        ),
        obs
      )
    }
  }

  # ---- Length comp (datlist$lencomp) ----
  lc <- datlist$lencomp
  if (!is.null(lc) && nrow(lc) > 0) {
    lc <- normalize_ss3_ghosts(lc, "fleet", "year")
    len_cols <- detect_length_cols(lc)
    if (!is.null(len_cols)) {
      # SS3 emits one column per length bin; keep first nlengths in order.
      if (length(len_cols) > nlengths) len_cols <- len_cols[1:nlengths]
      obs <- pad_comp_cols(as.data.frame(lc[, len_cols, drop = FALSE]), ncomp, "Comp")
      rows_out[[length(rows_out) + 1]] <- cbind(
        data.frame(
          Fleet_name   = fleet_control$Fleet_name[match(lc$fleet, fleet_control$Fleet_code)],
          Fleet_code   = as.integer(lc$fleet),
          Species      = 1L,
          Sex          = as.integer(lc$sex),
          Age0_Length1 = 1L,
          Year         = as.integer(lc$year),
          Month        = round((lc$month %||% lc$seas %||% 1 - 1) * 12 /
                                  max(1, datlist$nseas %||% 1)),
          Sample_size  = as.numeric(lc$Nsamp),
          stringsAsFactors = FALSE
        ),
        obs
      )
    }
  }

  if (length(rows_out) == 0) return(empty_template)
  do.call(rbind, rows_out)
}

#' @keywords internal
build_caal_data <- function(datlist, fleet_control, nages, minage, nlengths,
                            ss3_lbins) {
  # SS3 CAAL rides in datlist$agecomp on rows with Lbin_lo > 0; some SS3
  # versions also have a separate $ageerr_caal table.
  caal <- split_agecomp(datlist)$caal
  if ((is.null(caal) || nrow(caal) == 0) && !is.null(datlist[["ageerr_caal"]])) {
    caal <- datlist[["ageerr_caal"]]
    if ("Lbin_lo" %in% colnames(caal)) {
      caal <- caal[caal$Lbin_lo > 0, , drop = FALSE]
    }
  }
  if (is.null(caal) || nrow(caal) == 0) {
    return(empty_df(c("Fleet_name","Fleet_code","Species","Sex","Year","Length","Sample_size"),
                    paste0("CAAL_", 1:nages)))
  }
  caal <- normalize_ss3_ghosts(caal, "fleet", "year")
  age_cols <- detect_age_cols(caal)
  if (is.null(age_cols))
    stop("build_caal_data: could not find age columns in CAAL data")
  if (length(age_cols) >= (minage + nages)) {
    age_cols <- age_cols[(minage + 1):(minage + nages)]
  }
  obs <- pad_comp_cols(as.data.frame(caal[, age_cols, drop = FALSE]), nages, "CAAL")

  # Rceattle's Length column should hold the actual length VALUE (cm), not a
  # bin index. rearrange_data() does two things with it:
  #   (a) factor(Length) -> Length_bin (rank-ordered integer) for caal_ctl
  #   (b) data_list$lengths[sp, ] <- Length values for weight-length integration
  # If we pass bin indices (1..nlengths), (a) still works but (b) produces a
  # `lengths` array of 1..nlengths instead of cm values, and the C++ weight
  # calc `alpha * lengths^beta` gives wrong WAA scale. Pass the cm value so
  # the lengths array gets actual cm.
  length_idx <- vapply(caal$Lbin_lo, function(x) {
    which.min(abs(ss3_lbins - x))[1]
  }, integer(1))
  length_cm <- ss3_lbins[length_idx]

  base <- data.frame(
    Fleet_name  = fleet_control$Fleet_name[match(caal$fleet, fleet_control$Fleet_code)],
    Fleet_code  = as.integer(caal$fleet),
    Species     = 1L,
    Sex         = as.integer(caal$sex),
    Year        = as.integer(caal$year),
    Length      = as.numeric(length_cm),
    Sample_size = as.numeric(caal$Nsamp),
    stringsAsFactors = FALSE
  )
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
  # Age column spans minage..(minage + nages - 1), matching the data_list
  # convention. At minage=0 this is 0..nages-1, not 1..nages.
  ages_vec <- minage:(minage + nages - 1L)
  cbind(
    data.frame(Age_transition_name = paste0("Spp", 1:nspp),
               Age_transition_index = 1L,
               Species = 1L,
               Sex = 0L,
               Age = ages_vec),
    tmp
  )
}

#' Identity ageing-error matrix (no error)
#' @keywords internal
build_age_error <- function(nages, nspp, minage = 0L) {
  diag_df <- as.data.frame(diag(1, nages))
  colnames(diag_df) <- paste0("Obs_age", 1:nages)
  cbind(Species = 1:nspp,
        True_age = minage:(minage + nages - 1L),
        diag_df)
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
