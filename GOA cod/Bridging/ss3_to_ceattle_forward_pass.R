# =============================================================================
# 2024 Gulf of Alaska Pacific cod: SS3 -> Rceattle forward-pass validation
#
# Rebuilt on the AI cod bridge's pattern. The previous script
# (ss3_to_ceattle_forward_pass_legacy.R) was written against a much older
# Rceattle -- it wrote sel_inf[3, ] and log_sel_slp[3, ] from before
# sel_dn6 existed, and needed three switch values ("BlockDev", "SS3Robust",
# "EnvExp") that only ever lived on origin/dev-cod-bridge, 811 commits back.
#
# Take SS3's MLE, inject it, and check Rceattle reproduces SS3's state and
# likelihood. Estimation comes after the forward pass agrees.
#
# How this differs from AI cod:
#   - Selectivity is NOT injected. GOA has 126 double-normal parameters with
#     block replacements AND 63 per-year DEVmults across five fleets, and
#     Rceattle has no dev array for the SS3 pattern-24 block (there is no
#     sel_dn6_dev). The converter's per-year emp_sel, taken from SS3's
#     realized Asel2, carries blocks and devs exactly and needs no parameters.
#   - Growth is von Bertalanffy (GrowthModel 1), not Richards, and
#     CV_Growth_Pattern = 2 means the two growth SDs are SDs IN CM, so
#     sd_form = "SD". AI cod is pattern 0 and uses CVs.
#   - Recruitment carries a regime shift, SR_regime_BLK5add_1976, additive on
#     log(R0) -- but its block is 1976-1976, a single year, and that year is
#     styr - 1. It shifts the initial equilibrium level only, not any fitted
#     year, and init_dev absorbs it here. The AI model has none.
#   - M's block is 2014-2016 ONLY, not 2014 onwards (the marine heatwave);
#     SS3's M_at_age reverts to the base value in 2017.
#   - There is no initial F. GOA's equilibrium catch row is 0, so SS3 creates
#     no InitF parameter at all (SS_readcontrol_330.tpl:2650 counts only
#     fleets with obs_equ_catch != 0), and initMode is NonEquilibrium.
#   - Three fisheries and two surveys are active; four fleets are Off.
#
# Known gap: LLSrv's catchability carries SS3's env link type 1, which
# MULTIPLIES log q by exp(beta*env) (SS_timevaryparm.tpl:206). Rceattle's
# "Environmental" is the additive form (SS3's type 2), so it is not a
# substitute. LLSrv is the one fleet expected to miss on the predicted index.
# =============================================================================

library(r4ss); library(dplyr); library(tidyr)
# RCEATTLE_PKG selects the checkout, so a worktree carrying in-flight bridge
# features can be used without touching the main one. The DLL is assumed built
# there already; build it in that checkout, not from here, because recompiling
# a checkout another session is using breaks that session's runs.
RCEATTLE_PKG <- Sys.getenv("RCEATTLE_PKG", unset = "../../Rceattle")
pkgload::load_all(RCEATTLE_PKG, compile = FALSE, quiet = TRUE)
source("../SS3-bridge/ss3_to_rceattle.R")

`%||%` <- function(x, y) if (!is.null(x) && !(length(x) == 1 && is.na(x))) x else y

# ---- r4ss::SS_output workaround --------------------------------------------
# When SS3 finishes with a "variance may be suspect" warning, Report.sso's
# DERIVED_QUANTITIES SD column can parse as character and r4ss then errors in
# the Pstar/OFL sigma calculation, before returning timeseries/natage/condbase.
# Blank those two blocks; we do not use them.
local({
  src <- as.character(deparse(body(r4ss::SS_output)))
  pstar_line <- grep('Pstar_sigma.*sqrt', src)[1]
  ofl_line   <- grep('OFL_sigma.*sqrt',   src)[1]
  if (!is.na(pstar_line) && !is.na(ofl_line)) {
    for (i in (pstar_line - 4):(pstar_line + 5)) src[i] <- "    "
    for (i in (ofl_line   - 4):(ofl_line   + 5)) src[i] <- "    "
    src <- append(src, '  Pstar_sigma <- NA_real_; OFL_sigma <- NA_real_',
                  after = max(pstar_line, ofl_line) + 6)
    f <- r4ss::SS_output
    body(f) <- parse(text = paste(src, collapse = "\n"))[[1]]
    assignInNamespace("SS_output", f, ns = "r4ss")
  }
})


# =============================================================================
# 1. Read SS3 outputs and build the converter data list
# =============================================================================
# The CAAL-corrected copy of "no init and ramp" (same control file), with the
# Lbin columns written as the population bin numbers each 5 cm data bin spans.
# The as-written copy is refused by the converter, and rightly: SS3 truncates
# its non-integer Lbin values and fits bins the file does not name. Pete
# Hulson's prep code settles the 5 cm reading -- see CAAL-length-bin-defect.md.
SS3_DIR  <- Sys.getenv("RCE_SS3_DIR", unset = "Data/goa_pcod_caal_bins_fixed")
PAR_FILE <- "ss3.par"
DAT_FILE <- "GOAPcod2024Oct17_1e_5cm.dat"
CTL_FILE <- "Model19_1e.ctl"

parlist <- SS_readpar_3.30(file.path(SS3_DIR, PAR_FILE),
                           datsource = file.path(SS3_DIR, DAT_FILE),
                           ctlsource = file.path(SS3_DIR, CTL_FILE), verbose = FALSE)
datlist <- SS_readdat(file.path(SS3_DIR, DAT_FILE), verbose = FALSE)
ctllist <- SS_readctl(file.path(SS3_DIR, CTL_FILE), use_datlist = TRUE,
                      datlist = datlist, verbose = FALSE)
ss3_rep <- SS_output(SS3_DIR, verbose = FALSE, printstats = FALSE,
                     covar = FALSE, forecast = FALSE)

cod <- ss3_to_rceattle(
  ss3_dir       = SS3_DIR,
  par_file      = PAR_FILE,
  dat_file      = DAT_FILE,
  ctl_file      = CTL_FILE,
  spnames       = "GOApcod",
  minage        = 0,
  projyr_offset = 5,
  # GOA's equilibrium catch row is 0, so there is nothing to carry; the
  # argument is here so the setting is explicit rather than defaulted.
  catch_sd_offset = !identical(tolower(Sys.getenv("RCE_CATCH_SD_OFFSET", "true")), "false"),
  verbose       = FALSE
)

years_hind <- cod$styr:cod$endyr
nages      <- cod$nages[1]
minage     <- cod$minage[1]
n_flt      <- nrow(cod$fleet_control)
cat(sprintf("\nGOA Pcod: styr=%d endyr=%d nages=%d minage=%d fleets=%d\n",
            cod$styr, cod$endyr, nages, minage, n_flt))

fleet_meta <- data.frame(
  name       = cod$fleet_control$Fleet_name,
  ss3_num    = cod$fleet_control$Fleet_code,
  fleet_type = as.character(cod$fleet_control$Fleet_type),
  stringsAsFactors = FALSE
)
print(cod$fleet_control[, c("Fleet_name", "Fleet_type", "Selectivity",
                            "Selectivity_dimension", "Time_varying_sel")])

gp <- function(sec, pat) {
  if (is.null(sec)) return(NA_real_)
  i <- grep(pat, rownames(sec)); if (length(i)) sec[i[1], "ESTIM"] else NA_real_
}


# =============================================================================
# 2. Selectivity: keep the converter's per-year empirical curves
# =============================================================================
# SS3 gives five fleets a pattern-24 double normal with block replacements and,
# for 1977-1989, per-year DEVmult deviations -- 126 selectivity parameters in
# all. Rceattle has no dev array for the pattern-24 block, so none of that can
# be injected parametrically. build_emp_sel() takes SS3's REALIZED Asel2 for
# every year instead, which already contains the blocks and the devs, and sets
# Selectivity = "Fixed" with Time_varying_sel = "Off".
stopifnot(all(cod$fleet_control$Time_varying_sel[cod$fleet_control$Fleet_type != "Off"] %in% c(0, "Off")))
cat(sprintf("Selectivity: converter emp_sel, %d rows across %d years\n",
            nrow(cod$emp_sel), length(unique(cod$emp_sel$Year))))


# =============================================================================
# 3. M1: base plus the heatwave block, as a multiplicative log-linkage
#    M(yr) = M_base * exp(beta * heatwave) reproduces SS3's two values when
#    beta = log(M_block / M_base).
#
#    The block is 2014-2016 ONLY, not 2014 onwards: SS3's M_at_age reports
#    0.7918 for 2014, 2015 and 2016 and 0.4678 from 2017 -- the Gulf of Alaska
#    marine heatwave. Read the span from the ctl rather than assuming it.
# =============================================================================
# Note the naming: the par/ctl call this NatM_p_1_Fem_GP_1 while Report.sso
# labels it NatM_uniform_Fem_GP_1. Match on the common prefix.
M_base  <- gp(parlist$MG_parms, "^NatM_[a-z_0-9]*_?Fem_GP_1$")
M_block <- gp(parlist$MG_parms, "^NatM.*_BLK")
m_blk_idx <- {
  r <- grep("^NatM", rownames(ctllist$MG_parms))
  stopifnot(length(r) >= 1)
  as.integer(ctllist$MG_parms[r[1], "Block"])
}
m_block_yrs <- ctllist$Block_Design[[m_blk_idx]]
stopifnot(length(m_block_yrs) == 2)   # one start/end pair
cat(sprintf("\nM block (design %d) spans %d-%d\n", m_blk_idx,
            m_block_yrs[1], m_block_yrs[2]))
cod$env_data$heatwave <-
  as.integer(cod$env_data$Year >= m_block_yrs[1] & cod$env_data$Year <= m_block_yrs[2])
m_block_beta <- log(M_block / M_base)
cat(sprintf("M_base = %.4f, M_block = %.4f, beta = %.4f (%d block years)\n",
            M_base, M_block, m_block_beta, sum(cod$env_data$heatwave)))

M1_block <- build_M1(
  M1_model     = 1,
  M1_use_prior = FALSE,
  M2_use_prior = FALSE,
  linkages     = list(M1 = linkage_spec(
    formula = ~ heatwave - 1,
    by      = ~ species,
    init    = list(heatwave = m_block_beta)
  ))
)


# =============================================================================
# 4. Growth: von Bertalanffy (GrowthModel 1), SDs in cm (CV_Growth_Pattern 2)
# =============================================================================
K_vb  <- gp(parlist$MG_parms, "VonBert_K")
L_min <- gp(parlist$MG_parms, "L_at_Amin")
L_max <- gp(parlist$MG_parms, "L_at_Amax")
# SS3 always names these par slots CV_young/CV_old whatever CV_Growth_Pattern
# makes them mean. Under pattern 2 they are SDs IN CM, which is what
# Report.sso relabels SD_young_Fem_GP_1 / SD_old_Fem_GP_1.
SD_y  <- gp(parlist$MG_parms, "CV_young")
SD_o  <- gp(parlist$MG_parms, "CV_old")
cat(sprintf("Growth (vonBert): K=%.4f L1=%.4f Linf=%.4f SD_young=%.4f SD_old=%.4f\n",
            K_vb, L_min, L_max, SD_y, SD_o))
stopifnot(identical(as.integer(ctllist$GrowthModel), 1L))
# CV_Growth_Pattern 2 is "SD = f(LAA)": the two endpoints are SDs in cm, which
# is sd_form = "SD". Pattern 0 would make them CVs (AI cod's case).
stopifnot(identical(as.integer(ctllist$CV_Growth_Pattern), 2L))

growthFun_spec <- build_growth(
  fun               = "vonBertalanffy",
  sd_form           = "SD",
  plus_group_length = "none",
  sd_plus_group     = "WHAM",
  pop_lengths       = ss3_rep$lbinspop,
  linkages = list(
    K  = linkage_spec(formula = ~ 1, init = list("(Intercept)" = K_vb),
                      bounds = list("(Intercept)" = c(0.05, 0.6))),
    L1 = linkage_spec(formula = ~ 1, init = list("(Intercept)" = L_min),
                      bounds = list("(Intercept)" = c(0.1, 20))),
    Linf = linkage_spec(formula = ~ 1, init = list("(Intercept)" = L_max),
                        bounds = list("(Intercept)" = c(60, 140)))
  )
)


# =============================================================================
# 5. Single-sex SSB scaling and maturity-at-length
# =============================================================================
# Nsexes = 1, so the modelled sex IS the spawning population and SS3 applies no
# FracFemale multiplier. Rceattle computes SSB = sum(N * sex_ratio * maturity *
# ssb_weight), so sex_ratio = 1.
cod$sex_ratio[, grep("^Age", colnames(cod$sex_ratio))] <- 1.0

# SS3 maturity option 1 (length logistic) writes 1/(1 + exp(slope*(L - L50)))
# with a NEGATIVE slope; Rceattle's slope is positive, so the sign flips.
cod$L50_mat_len   <- gp(parlist$MG_parms, "Mat50%_Fem")
cod$slope_mat_len <- -gp(parlist$MG_parms, "Mat_slope_Fem")
cat(sprintf("Maturity-at-length: L50 = %.3f cm, slope = %.4f per cm\n",
            cod$L50_mat_len, cod$slope_mat_len))


# =============================================================================
# 6. SS3 data weighting (variance adjustment) on comp sample sizes
#    Factor 4 = mult_by_lencomp_N, factor 5 = mult_by_agecomp_N (CAAL rides on
#    agecomp). The converter copies raw input Nsamp, so without these the
#    comp/CAAL likelihood is far too large.
# =============================================================================
va <- ctllist$Variance_adjustment_list
if (!is.null(va) && nrow(va) > 0) {
  for (k in seq_len(nrow(va))) {
    fct <- va$factor[k]; fl <- va$fleet[k]; val <- va$value[k]
    if (fct == 4) {
      rows <- which(cod$comp_data$Fleet_code == fl & cod$comp_data$Age0_Length1 == 1)
      cod$comp_data$Sample_size[rows] <- cod$comp_data$Sample_size[rows] * val
    } else if (fct == 5) {
      rows <- which(cod$comp_data$Fleet_code == fl & cod$comp_data$Age0_Length1 == 0)
      cod$comp_data$Sample_size[rows] <- cod$comp_data$Sample_size[rows] * val
      crows <- which(cod$caal_data$Fleet_code == fl & cod$caal_data$Year > 0)
      cod$caal_data$Sample_size[crows] <- cod$caal_data$Sample_size[crows] * val
    }
    cat(sprintf("Var-adj factor %d fleet %d: N *= %.5f\n", fct, fl, val))
  }
}


# =============================================================================
# 7. Build mod0 (parameter shape only) to get the inits skeleton
# =============================================================================
# No initial F: GOA's equilibrium catch is 0, so SS3 has no InitF parameter and
# the population starts unfished with Early_InitAge deviations on top --
# Rceattle's NonEquilibrium.
INIT_MODE <- Sys.getenv("RCE_INITMODE", unset = "NonEquilibrium")
cat("\ninitMode:", INIT_MODE, "\n")
cat("\n--- Building mod0 (parameter shape) ---\n")
mod0 <- Rceattle::fit_mod(
  data_list    = cod,
  inits        = NULL,
  estimateMode = 3,
  initMode     = INIT_MODE,
  growthFun    = growthFun_spec,
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  # SS3 bias-corrects RECRUITMENT but applies no bias correction to the catch
  # or index observation likelihoods; Rceattle shifts both means by
  # -sigma^2/2 when bias_adjust_obs is TRUE.
  fit_control  = fit_control(phase = FALSE, verbose = 1, bias_adjust_obs = FALSE)
)
cat("\nRceattle parameter names:\n",
    paste(names(mod0$estimated_params), collapse = ", "), "\n")


# =============================================================================
# 8. SS3 -> Rceattle parameter injection
# =============================================================================
init_from_ss3 <- function(parlist, ctllist, inits, data_list, fleet_meta,
                          years_hind, mod0) {
  # --- M base + block coefficient ---
  if ("log_M1" %in% names(inits)) {
    inits$log_M1[] <- log(M_base)
    cat(sprintf("M_base = %.4f\n", M_base))
  }
  if ("beta_linkage" %in% names(inits)) {
    tbl <- mod0$data_list$linkage_table %||% data_list$linkage_table
    m_row <- which(tbl$process == "M" & tbl$design_col == "heatwave")
    if (length(m_row) == 1L) {
      inits$beta_linkage[m_row] <- m_block_beta
      cat(sprintf("M heatwave beta = %.4f (row %d)\n", m_block_beta, m_row))
    } else {
      cat(sprintf("WARNING: %d M heatwave linkage rows (expected 1)\n", length(m_row)))
    }
  }

  # --- log(R0) ---
  # SR_regime sits on block design 5, which is 1976-1976 -- a SINGLE year, and
  # that year is styr - 1. It shifts the INITIAL equilibrium recruitment level,
  # not any fitted year (the hindcast starts in 1977), which is SS3's
  # SR_regime mechanism for initial conditions. It is deliberately NOT folded
  # into rec_pars: init_dev below is derived to pin SS3's styr numbers-at-age
  # exactly, so it absorbs the offset whatever base level is used. This matters
  # only when the initial deviates are estimated rather than pinned.
  ln_R0  <- gp(parlist$SR_parms, "SR_LN")
  regime <- gp(parlist$SR_parms, "SR_regime_BLK")
  reg_blk_idx <- {
    r <- grep("SR_regime", rownames(ctllist$SR_parms))
    if (length(r)) as.integer(ctllist$SR_parms[r[1], "Block"]) else NA_integer_
  }
  if (!is.na(regime) && !is.na(reg_blk_idx) && reg_blk_idx > 0) {
    reg_yrs <- ctllist$Block_Design[[reg_blk_idx]]
    cat(sprintf("SR_regime = %.4f on block design %d (%d-%d); %d fitted year(s) affected\n",
                regime, reg_blk_idx, reg_yrs[1], reg_yrs[2],
                sum(years_hind >= reg_yrs[1] & years_hind <= reg_yrs[2])))
    if (any(years_hind >= reg_yrs[1] & years_hind <= reg_yrs[2])) {
      warning("SR_regime overlaps fitted years; it is not represented here and ",
              "init_dev cannot absorb a shift inside the hindcast.")
    }
  }
  if ("rec_pars" %in% names(inits)) {
    inits$rec_pars[1, 1] <- ln_R0
    cat(sprintf("log(R0) = %.4f  =>  R0 = %.4g\n", ln_R0, exp(ln_R0)))
  }

  # --- Recruitment deviates, with the Methot-Taylor bias-adjustment ramp ---
  # The ramp is applied so REALIZED recruitment matches SS3. The recruitment
  # likelihood still differs by design: Rceattle does not implement the ramp
  # penalty itself.
  sigma_R <- gp(parlist$SR_parms, "SR_sigmaR") %||% 0.6
  compute_bias_adj <- function(yr) {
    if (is.null(ctllist) || !isTRUE(ctllist$recdev_adv == 1)) return(rep(1.0, length(yr)))
    bmax <- ctllist$max_bias_adj
    if (isTRUE(bmax == -1)) return(rep(1.0, length(yr)))
    late0  <- ctllist$last_early_yr_nobias_adj
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
    cat(sprintf("Set rec_dev for %d years (sigmaR = %.3f)\n", n_set, sigma_R))
  }

  # --- von Bertalanffy growth ---
  if ("log_growth_pars" %in% names(inits)) {
    inits$log_growth_pars[1, 1, 1] <- log(K_vb)
    inits$log_growth_pars[1, 1, 2] <- log(max(L_min, 0.01))
    inits$log_growth_pars[1, 1, 3] <- log(L_max)
    cat(sprintf("Growth injected: K=%.4f L1=%.4f Linf=%.4f\n", K_vb, L_min, L_max))
  }
  # sd_form = "SD": growth_log_sd holds log SD in cm at L1 and at Linf.
  if ("growth_log_sd" %in% names(inits)) {
    inits$growth_log_sd[1, 1, 1] <- log(SD_y)
    inits$growth_log_sd[1, 1, 2] <- log(SD_o)
    cat(sprintf("Growth SD (cm): young = %.4f, old = %.4f\n", SD_y, SD_o))
  }

  # --- Weight-length ---
  W1 <- gp(parlist$MG_parms, "Wtlen_1_Fem_GP_1")
  W2 <- gp(parlist$MG_parms, "Wtlen_2_Fem_GP_1")
  if ("weight_length_pars" %in% names(inits)) {
    inits$weight_length_pars[1, 1] <- W1
    inits$weight_length_pars[1, 2] <- W2
    cat(sprintf("W-L: alpha = %.6g, beta = %.4f\n", W1, W2))
  }

  # --- Survey catchability (log scale) ---
  # LLSrv's SS3 q also carries an exponential environmental link, which has no
  # Rceattle counterpart; only the base is injected, so its predicted index
  # will not track SS3.
  if ("index_log_q" %in% names(inits)) {
    for (i in seq_len(nrow(fleet_meta))) {
      if (fleet_meta$fleet_type[i] != "Survey") next
      q <- gp(parlist$Q_parms,
              sprintf("LnQ_base_%s\\(%d\\)$", fleet_meta$name[i], fleet_meta$ss3_num[i]))
      if (!is.na(q)) {
        inits$index_log_q[i] <- q
        cat(sprintf("  q[%s] = %.4f (exp = %.4f)\n", fleet_meta$name[i], q, exp(q)))
      }
    }
  }
  inits
}


# =============================================================================
# 9. Pin the initial numbers-at-age and the fishing mortalities
# =============================================================================
# Derive init_dev so Rceattle's styr numbers equal SS3's. With Finit = 0 the
# decay is sum(M1) alone, which is what initMode 1/2 build.
init_state_from_ss3_natage <- function(inits, ss3_rep, styr, nages, R_init,
                                       M1_at_age) {
  ss3_age_cols <- as.character(0:(nages - 1))
  row <- ss3_rep$natage %>%
    dplyr::filter(Yr == styr, `Beg/Mid` == "B", Sex == 1) %>% dplyr::slice(1)
  if (nrow(row) == 0) stop("SS3 natage missing row for styr = ", styr)
  if (as.character(nages) %in% colnames(row)) {
    extra <- as.numeric(row[1, as.character(nages)])
    ss3_N <- as.numeric(row[1, ss3_age_cols]); ss3_N[nages] <- ss3_N[nages] + extra
  } else {
    ss3_N <- as.numeric(row[1, ss3_age_cols])
  }
  cat(sprintf("\nSS3 natage[%d]: %s\n", styr,
              paste(sprintf("%.4g", ss3_N), collapse = ", ")))
  for (k in seq_len(nages - 1)) {
    mort_sum <- sum(as.numeric(M1_at_age[1:k]))
    target_N <- ss3_N[k + 1]
    if (k == (nages - 1)) target_N <- target_N * (1 - exp(-as.numeric(M1_at_age[nages])))
    inits$init_dev[1, k] <- log(max(target_N, 1e-10)) - log(R_init) + mort_sum
  }
  cat(sprintf("init_dev[1, 1:%d] set to pin styr N\n", nages - 1))
  inits
}

init_log_F_from_ss3 <- function(inits, ss3_rep, fleet_meta, years_hind) {
  if (!"log_F" %in% names(inits)) return(inits)
  log_F <- inits$log_F
  ts <- ss3_rep$timeseries
  ts <- ts[match(years_hind, ts$Yr), ]
  f_cols <- grep("^F:_[0-9]+$", colnames(ts), value = TRUE)
  cat(sprintf("\nSS3 ts F-cols: %s\n", paste(f_cols, collapse = ", ")))
  for (i in seq_len(nrow(fleet_meta))) {
    if (fleet_meta$fleet_type[i] != "Fishery") next
    fcol <- sprintf("F:_%d", fleet_meta$ss3_num[i])
    if (!fcol %in% f_cols) next
    f_vec <- as.numeric(ts[[fcol]])
    f_vec[is.na(f_vec) | f_vec <= 0] <- 1e-9
    log_F[i, seq_along(years_hind)] <- log(f_vec)
    cat(sprintf("  log_F[%s] <- ts$%s (yr1=%.3g, mid=%.3g, last=%.3g)\n",
                fleet_meta$name[i], fcol, f_vec[1],
                f_vec[length(f_vec) %/% 2], tail(f_vec, 1)))
  }
  inits$log_F <- log_F
  inits
}


# =============================================================================
# 10. Wire it up and run the forward pass
# =============================================================================
inits <- init_from_ss3(parlist, ctllist, mod0$estimated_params, cod,
                       fleet_meta, years_hind, mod0)

R_init    <- exp(inits$rec_pars[1, 1])
M1_at_age <- rep(M_base, nages)
inits <- init_state_from_ss3_natage(inits, ss3_rep, cod$styr, nages,
                                    R_init = R_init, M1_at_age = M1_at_age)
inits <- init_log_F_from_ss3(inits, ss3_rep, fleet_meta, years_hind)

# Map out what SS3 holds fixed, so G2's gradient test covers only the
# parameters SS3 actually moved.
ss3_map <- ss3_fix_map(mod0$map, ss3_rep, inits, cod$fleet_control,
                       years_hind, verbose = TRUE)

cat("\n--- Forward-pass fit (estimateMode = 3) ---\n")
fp <- Rceattle::fit_mod(
  data_list    = cod,
  inits        = inits,
  map          = ss3_map,
  estimateMode = 3,
  initMode     = INIT_MODE,
  growthFun    = growthFun_spec,
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  fit_control  = fit_control(phase = FALSE, verbose = 1, bias_adjust_obs = FALSE)
)

cat(sprintf("\nRceattle jnll = %.4f   SS3 TOTAL = %.4f\n",
            fp$quantities$jnll, ss3_rep$likelihoods_used["TOTAL", "values"]))
saveRDS(list(fp = fp, ss3_rep = ss3_rep, cod = cod, inits = inits,
             ss3_map = ss3_map),
        "Bridging/_fp_result.rds")
cat("Saved Bridging/_fp_result.rds\n")
