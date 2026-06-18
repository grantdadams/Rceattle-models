# =============================================================================
# 2024 Aleutian Islands Pacific cod: SS3 -> Rceattle forward-pass validation
#
# Goal: take the data list produced by ss3_to_rceattle(), inject SS3's MLE
# parameter values, and verify Rceattle reproduces SS3 R / Bio / SSB / F to
# ~1e-3 with estimateMode = 3 (no estimation). If the forward pass matches,
# the bridge is sound and estimation (ss3_to_ceattle_estimate.R) can follow.
#
# Structure vs the GOA Pcod template (../../GOA cod/Bridging):
#   - AI cod is SIMPLER: 2 fleets (FshComb fishery + Srv survey), single-sex
#     (Ngenders = 1, FracFemale = 0.5 -> SSB halved), simple survey q (no
#     env-q), and NO time-varying selectivity blocks.
#   - AI cod is structurally DIFFERENT in one place: SS3 growth is Richards
#     (GrowthModel = 2), not von Bertalanffy. We keep the converter's
#     empirical (Asel2 realized age) selectivity + empirical weight-at-age so
#     the POPULATION TRAJECTORY (N/Bio/SSB/F) is independent of the growth
#     curve; growth only feeds the ALK used for length-comp / CAAL prediction.
#     Rceattle's Richards path is exercised here and its ALK is compared to
#     SS3 (see Section 9b). Any residual length-comp/CAAL gap traces to growth.
# =============================================================================

library(Rceattle); library(r4ss); library(dplyr); library(tidyr)
setwd("/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/AI cod")
source("R/ss3_to_rceattle.R")

`%||%` <- function(x, y) if (!is.null(x) && !(length(x) == 1 && is.na(x))) x else y

# ---- r4ss::SS_output workaround --------------------------------------------
# When SS3 finishes with a "variance may be suspect" warning, the SD column in
# Report.sso's DERIVED_QUANTITIES can land as character, and r4ss errors in the
# Pstar / OFL sigma calc, blocking all downstream parsing. Patch: blank that
# block and set the slots to NA (we don't use them). Same fix as GOA cod.
local({
  src <- as.character(deparse(body(r4ss::SS_output)))
  pstar_line <- grep('Pstar_sigma.*sqrt', src)[1]
  ofl_line   <- grep('OFL_sigma.*sqrt',   src)[1]
  if (!is.na(pstar_line) && !is.na(ofl_line)) {
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
tryCatch(detach("package:r4ss"), error = function(e) NULL)
suppressMessages(library(r4ss))


# =============================================================================
# 1. Read SS3 outputs and build the converter data list
# =============================================================================
SS3_DIR  <- "SS3/run"
PAR_FILE <- "ss.par"
DAT_FILE <- "data_echo.ss_new"
CTL_FILE <- "control.ss_new"

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
  spnames       = "AIcod",
  minage        = 0,
  projyr_offset = 5,
  verbose       = FALSE
)

years_hind <- cod$styr:cod$endyr
nages      <- cod$nages[1]
minage     <- cod$minage[1]            # 0
n_flt      <- nrow(cod$fleet_control)
stopifnot(minage == 0L, nages == 14L, n_flt == 2L)

fleet_meta <- data.frame(
  name       = cod$fleet_control$Fleet_name,
  ss3_num    = cod$fleet_control$Fleet_code,
  fleet_type = cod$fleet_control$Fleet_type,
  stringsAsFactors = FALSE
)
cat("Fleets detected:\n"); print(fleet_meta)

# --- CAAL length-axis reconciliation ----------------------------------------
# AI cod's length comps use all 143 SS3 data bins (0.5..142.5), but the survey
# CAAL is only tabulated over the 103 bins where age samples exist (12.5..115.5).
# With parametric growth (growth_model > 0) Rceattle ties the model length axis
# to the CAAL bins and requires unique(caal_data$Length) == nlengths. Empirical
# growth (growth_model = 0) is NOT an option here: in this Rceattle build the
# C++ growth_matrix is left unpopulated in the empirical branch, so BOTH the
# length-comp and CAAL predictions collapse to 0. So we keep all 143 bins (to
# preserve the length-comp likelihood unchanged) and pad caal_data with ghost
# rows (Year < 0, Sample_size = 0) for the 40 unpopulated length bins. Ghost
# rows satisfy the unique-length count without contributing to the likelihood.
ss3_lbins   <- datlist$lbin_vector
caal_have   <- sort(unique(cod$caal_data$Length))
caal_missing <- setdiff(ss3_lbins, caal_have)
if (length(caal_missing) > 0) {
  caal_cols <- grep("^CAAL_", colnames(cod$caal_data), value = TRUE)
  ghost <- cod$caal_data[rep(1, length(caal_missing)), ]
  ghost$Fleet_name  <- fleet_meta$name[fleet_meta$fleet_type == "Survey"][1]
  ghost$Fleet_code  <- fleet_meta$ss3_num[fleet_meta$fleet_type == "Survey"][1]
  # Year = -styr: abs(Year) is in [styr, projyr] so clean_data keeps the row,
  # but the negative sign flags it as a ghost (excluded from the likelihood).
  ghost$Year        <- -cod$styr
  ghost$Length      <- caal_missing
  ghost$Sample_size <- 0
  ghost[, caal_cols] <- 0
  cod$caal_data <- rbind(cod$caal_data, ghost)
  cat(sprintf("Padded caal_data with %d ghost length bins (now %d unique lengths == nlengths %d)\n",
              length(caal_missing), length(unique(cod$caal_data$Length)), cod$nlengths[1]))
}

# --- Ageing-error matrix from SS3 -------------------------------------------
# The converter installs an IDENTITY (no-error) ageing matrix, but SS3 has real
# ageing error (age_error_sd grows from ~0.03 at age 0 to >0.6 by age 5), which
# smears the predicted age-at-length. Without it Rceattle's predicted CAAL is
# too sharp -> inflated CAAL NLL. Build P[obs | true] by integrating
# N(mean_a, sd_a) over the integer observed-age bins [j, j+1), with the minus
# and plus groups accumulating the lower/upper tails. SS3's age_error_mean is
# in the "+0.5" convention (mean obs age = true age + 0.5 = unbiased given the
# [j, j+1) bins).
build_ss3_age_error <- function(ss3_rep, nages, minage = 0L) {
  m <- as.numeric(ss3_rep$age_error_mean$type1)[seq_len(nages)]
  s <- as.numeric(ss3_rep$age_error_sd$type1)[seq_len(nages)]
  ages_true <- seq.int(minage, minage + nages - 1L)
  P <- matrix(0, nages, nages)
  for (i in seq_len(nages)) {
    for (j in seq_len(nages)) {
      obs <- minage + j - 1L
      lo <- if (j == 1L)     -Inf else obs
      hi <- if (j == nages)   Inf else obs + 1
      P[i, j] <- stats::pnorm(hi, m[i], s[i]) - stats::pnorm(lo, m[i], s[i])
    }
  }
  P <- P / rowSums(P)
  Pdf <- as.data.frame(P); colnames(Pdf) <- paste0("Obs_age", seq_len(nages))
  cbind(Species = 1L, True_age = ages_true, Pdf)
}
cod$age_error <- build_ss3_age_error(ss3_rep, nages, minage)
cat(sprintf("Injected SS3 ageing-error matrix; P[obs|true=2] = %s\n",
            paste(signif(as.numeric(cod$age_error[3, 3:6]), 3), collapse = " ")))


# =============================================================================
# 2. M1 linkage: post-2016 block indicator + SS3 NatM block value
#    SS3 NatM uses Block design pattern 2 (Block_Fxn = 2 = direct replacement):
#    M = 0.417 (base), M = 0.5791 for 2016-2024. A multiplicative log-linkage
#    M(yr) = M_base * exp(beta * post2016) reproduces those exact values when
#    beta = log(M_block / M_base).
# =============================================================================
m_block_yrs <- ctllist$Block_Design[[2]]    # NatM points to Block design 2
cat(sprintf("\nM block (design 2) spans years %d-%d\n", m_block_yrs[1], m_block_yrs[2]))
cod$env_data$post2016 <-
  as.integer(cod$env_data$Year >= m_block_yrs[1] & cod$env_data$Year <= m_block_yrs[2])

gp <- function(sec, pat) { i <- grep(pat, rownames(sec)); if (length(i)) sec[i[1], "ESTIM"] else NA_real_ }
M_base  <- gp(parlist$MG_parms, "NatM_p_1_Fem_GP_1$")
M_block <- gp(parlist$MG_parms, "NatM_p_1_Fem_GP_1_BLK")
m_block_beta <- log(M_block / M_base)
cat(sprintf("M_base = %.4f, M_block = %.4f, beta = log(ratio) = %.4f\n",
            M_base, M_block, m_block_beta))

M1_block <- build_M1(
  M1_model     = 1,
  M1_use_prior = FALSE,
  M2_use_prior = FALSE,
  linkages     = list(M1 = linkage_spec(
    formula = ~ post2016 - 1,
    by      = ~ species,
    init    = list(post2016 = m_block_beta)
  ))
)


# =============================================================================
# 3. Growth spec: Richards (SS3 GrowthModel = 2), SS3 MLE inits
#    SS3 Richards params: L_at_Amin, L_at_Amax (=Linf since Growth_Age_for_L2
#    = 999), VonBert_K, Richards (shape), CV_young, CV_old.
# =============================================================================
K_vb   <- gp(parlist$MG_parms, "VonBert_K")
L_min  <- gp(parlist$MG_parms, "L_at_Amin")
L_max  <- gp(parlist$MG_parms, "L_at_Amax")
Rich   <- gp(parlist$MG_parms, "Richards")
CV_y   <- gp(parlist$MG_parms, "CV_young")
CV_o   <- gp(parlist$MG_parms, "CV_old")
cat(sprintf("\nGrowth (Richards): K=%.4f L1=%.4f Linf=%.4f shape=%.4f CVy=%.4f CVo=%.4f\n",
            K_vb, L_min, L_max, Rich, CV_y, CV_o))

# Bounds = SS3 ctl LO/HI (K 0.09-0.5, L_at_Amin 5-30, L_at_Amax 60-135).
# Priors: SS3 estimates growth with PR_type = 0 (no prior) because its comps
# are NOT down-weighted. We apply SS3's Francis variance adjustment (~25x
# down-weight on length comps), which under-identifies growth in Rceattle and
# lets K/Linf run away to bounds (see ss3_to_ceattle_estimate.R). A tight
# normal prior centered on the SS3 MLE restores identifiability and keeps the
# realized growth on the SS3 curve. (growth_log_sd is fixed below to SS3's
# fixed CV-derived SDs, matching SS3 phase = -2 on CV_young / CV_old.)
growthFun_spec <- tryCatch(
  build_growth(
    fun = "Richards",
    linkages = list(
      # Prior SDs are deliberately TIGHTER than SS3's ctl PR_SD (K 0.021,
      # Linf 2): SS3 identifies growth from fully-weighted comps, but here the
      # comps are Francis-down-weighted ~25x, so the CAAL pulls Linf ~13% low
      # (to ~107) and destabilizes the terminal dynamics if the prior is loose.
      # These tight priors substitute the growth information SS3 has and keep
      # the estimated curve on the SS3 MLE while still letting it move.
      K     = linkage_spec(formula = ~ 1, init = list("(Intercept)" = K_vb),
                           priors = list("(Intercept)" = normal(K_vb, 0.01)),
                           bounds = list("(Intercept)" = c(0.09, 0.5))),
      L1    = linkage_spec(formula = ~ 1, init = list("(Intercept)" = L_min),
                           priors = list("(Intercept)" = normal(L_min, 0.5)),
                           bounds = list("(Intercept)" = c(5, 30))),
      Linf  = linkage_spec(formula = ~ 1, init = list("(Intercept)" = L_max),
                           priors = list("(Intercept)" = normal(L_max, 1.0)),
                           bounds = list("(Intercept)" = c(60, 135)))
    )
  ),
  error = function(e) { cat("Richards build_growth failed (", conditionMessage(e),
                            ") -- falling back to vonBertalanffy\n"); NULL })
if (is.null(growthFun_spec)) {
  growthFun_spec <- build_growth(fun = "vonBertalanffy")
}


# =============================================================================
# 3a. Single-sex SSB scaling
#   SS3 AI cod is Ngenders = 1 (NOT -1), so reported SpawnBio = sum(N *
#   Mat_F_wtatage) with NO FracFemale multiplier (verified: equals SS3
#   SpawnBio to 5 sig-figs). Rceattle computes SSB = sum(N * sex_ratio *
#   maturity * ssb_weight), so set sex_ratio = 1.0 (the single modeled sex IS
#   the whole spawning population). NOTE: a residual ~5-6% Jensen gap remains
#   because Rceattle multiplies separately age-integrated maturity and weight,
#   whereas SS3's Mat_F_wtatage integrates maturity(L)*weight(L) jointly over
#   the length distribution. This is the same Jensen gap documented for GOA
#   cod and is a structural difference, not a bug.
# =============================================================================
cod$sex_ratio[, grep("^Age", colnames(cod$sex_ratio))] <- 1.0


# =============================================================================
# 3a-ii. SS3 data-weighting (variance adjustment) on comp sample sizes.
#   SS3 ctl applies multiplicative N adjustments (Francis/TA1.8 style) to comp
#   effective sample sizes: Factor 4 = mult_by_lencomp_N, Factor 5 =
#   mult_by_agecomp_N (CAAL rides on agecomp). The converter copied raw input
#   Nsamp, so without these the comp/CAAL NLL is ~25x too large. Apply them to
#   comp_data (by length/age type) and caal_data so the multinomial weights
#   match SS3's effective sample sizes.
# =============================================================================
va <- ctllist$Variance_adjustment_list
if (!is.null(va) && nrow(va) > 0) {
  for (k in seq_len(nrow(va))) {
    fct <- va$factor[k]; fl <- va$fleet[k]; val <- va$value[k]
    if (fct == 4) {  # length comp
      rows <- which(cod$comp_data$Fleet_code == fl & cod$comp_data$Age0_Length1 == 1)
      cod$comp_data$Sample_size[rows] <- cod$comp_data$Sample_size[rows] * val
    } else if (fct == 5) {  # age comp (marginal) + CAAL
      rows <- which(cod$comp_data$Fleet_code == fl & cod$comp_data$Age0_Length1 == 0)
      cod$comp_data$Sample_size[rows] <- cod$comp_data$Sample_size[rows] * val
      crows <- which(cod$caal_data$Fleet_code == fl & cod$caal_data$Year > 0)
      cod$caal_data$Sample_size[crows] <- cod$caal_data$Sample_size[crows] * val
    }
    cat(sprintf("Var-adj factor %d fleet %d: N *= %.5f\n", fct, fl, val))
  }
}


# =============================================================================
# 3b. Switch both fleets to parametric Length DoubleNormal selectivity.
#   Empirical selectivity (converter default) is age-based only: it populates
#   sel_at_age but leaves sel_at_length = 0, which collapses every length-comp
#   and CAAL prediction to 0 (NLL -> 1e5+). To predict length comps we need a
#   parametric LENGTH selectivity so sel_at_length is populated. Both AI cod
#   fleets are asymptotic (SS3 realized Lsel rises monotonically to 1.0), so
#   Rceattle's 4-param DoubleNormal (case 8) with the right-tail floor pinned
#   near 1 is an ascending-Gaussian-to-flat-top -- exactly the SS3 pattern-24
#   shape when the plateau is wide. We FIT (peak, sigma_asc) to SS3's realized
#   Lsel per fleet rather than translating SS3's 6 params, so the realized
#   sel-at-length matches regardless of parameterization differences.
# =============================================================================
# SS3's catch equation uses mid-season body weight (endgrowth SelWt ~ Wt_Mid),
# which is ~15-20% heavier than begin-year weight at mid ages. fleet_control$
# Month sets the timing Rceattle uses to compute the fleet's weight-at-age from
# growth, so put the fishery at mid-year (month 6) to match SS3's catch weight.
cod$fleet_control$Month[cod$fleet_control$Fleet_type == "Fishery"] <- 6L

for (fi in seq_len(n_flt)) {
  cod$fleet_control$Selectivity[fi]           <- "DoubleNormal"
  cod$fleet_control$Selectivity_dimension[fi] <- "Length"
  # SS3 reports the multinomial DEVIANCE kernel (= 0 at a perfect fit). Use
  # "MultinomialAFSC" (= -1, Martin's form: -N*(obs+e)*log((hat+e)/(obs+e)))
  # rather than "Multinomial" (= 0), whose dmultinom() carries the large
  # lgamma normalizing constant that SS3 omits -- so absolute NLL is
  # comparable to SS3's Length_comp / Age_comp values.
  cod$fleet_control$Comp_loglike[fi] <- "MultinomialAFSC"
  # CAAL only supports Multinomial (0) / DirichletMultinomial (1) in this
  # Rceattle build (no deviance form), so its absolute NLL carries the
  # dmultinom lgamma constant that SS3's Age_comp omits -- a documented offset.
  cod$fleet_control$CAAL_loglike[fi] <- "Multinomial"
}

# Extract SS3 realized sel-at-length (terminal year) per fleet
ss3_lsel <- ss3_rep$sizeselex[ss3_rep$sizeselex$Factor == "Lsel" &
                              ss3_rep$sizeselex$Yr == cod$endyr, , drop = FALSE]
lsel_len_cols <- grep("^[0-9]+(\\.[0-9]+)?$", colnames(ss3_lsel), value = TRUE)
lsel_lengths  <- as.numeric(lsel_len_cols)

# Fit Rceattle case-8 DoubleNormal to a target sel-at-length vector.
# Returns list(peak, log_sig_asc, log_sig_desc, logit_floor).
fit_dn <- function(lengths, target) {
  binw <- if (length(lengths) > 1) lengths[2] - lengths[1] else 1
  rce_dn <- function(p) {
    peak <- p[1]; sa <- exp(p[2]); sd <- exp(p[3]); rf <- 1 / (1 + exp(-p[4]))
    x <- lengths + 0.5 * binw
    w <- 1 / (1 + exp(-20 * (x - peak)))
    asc <- exp(-0.5 * ((x - peak) / sa)^2)
    vdesc <- rf + (1 - rf) * exp(-0.5 * ((x - peak) / sd)^2)
    (1 - w) * asc + w * vdesc
  }
  obj <- function(p) sum((rce_dn(p) - target)^2)
  peak0 <- lengths[which.max(target >= 0.999)[1]]
  if (is.na(peak0)) peak0 <- lengths[which.max(target)]
  fit <- optim(c(peak0, log(15), log(50), 8), obj,
               method = "L-BFGS-B",
               lower = c(min(lengths), log(1), log(5), 2),
               upper = c(max(lengths), log(60), log(200), 12))
  list(peak = fit$par[1], log_sig_asc = fit$par[2],
       log_sig_desc = fit$par[3], logit_floor = fit$par[4],
       sse = fit$value, pred = rce_dn(fit$par))
}

dn_fits <- list()
for (fi in seq_len(n_flt)) {
  fnum <- fleet_meta$ss3_num[fi]
  tgt  <- as.numeric(ss3_lsel[ss3_lsel$Fleet == fnum, lsel_len_cols])
  if (length(tgt) == 0 || all(is.na(tgt))) next
  dn_fits[[fi]] <- fit_dn(lsel_lengths, tgt)
  cat(sprintf("DoubleNormal fit %s: peak=%.2f sig_asc=%.3f floor=%.4f SSE=%.4g\n",
              fleet_meta$name[fi], dn_fits[[fi]]$peak, exp(dn_fits[[fi]]$log_sig_asc),
              1 / (1 + exp(-dn_fits[[fi]]$logit_floor)), dn_fits[[fi]]$sse))
}


# =============================================================================
# 4. Build mod0 (parameter shape only) to get the inits skeleton
# =============================================================================
cat("\n--- Building mod0 (parameter shape) ---\n")
mod0 <- Rceattle::fit_mod(
  data_list    = cod,
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
# 5. SS3 -> Rceattle parameter injection (scalars)
# =============================================================================
init_from_ss3 <- function(parlist, ctllist, inits, data_list, fleet_meta,
                          years_hind, mod0) {
  get_par <- function(section, pattern) {
    if (is.null(section)) return(NULL)
    idx <- grep(pattern, rownames(section))
    if (length(idx) == 0) return(NULL)
    section[idx[1], "ESTIM"]
  }

  # --- M base + post-2016 block (linkage coefficient) ---
  M_base <- get_par(parlist$MG_parms, "NatM_p_1_Fem_GP_1$")
  M_blk  <- get_par(parlist$MG_parms, "NatM_p_1_Fem_GP_1_BLK")
  if (!is.null(M_base) && "log_M1" %in% names(inits)) {
    inits$log_M1[] <- log(M_base)
    cat(sprintf("M_base = %.4f\n", M_base))
  }
  if (!is.null(M_blk) && !is.null(M_base) && "beta_linkage" %in% names(inits)) {
    tbl <- mod0$data_list$linkage_table %||% data_list$linkage_table
    m_row <- which(tbl$process == "M" & tbl$design_col == "post2016")
    if (length(m_row) == 1L) {
      inits$beta_linkage[m_row] <- log(M_blk / M_base)
      cat(sprintf("M post-2016 = %.4f (beta = %.4f) row=%d\n",
                  M_blk, log(M_blk / M_base), m_row))
    } else {
      cat(sprintf("WARNING: %d M post2016 rows in linkage_table (expected 1)\n",
                  length(m_row)))
    }
  }

  # --- log(R0) ---
  ln_R0 <- get_par(parlist$SR_parms, "SR_LN")
  if (!is.null(ln_R0) && "rec_pars" %in% names(inits)) {
    inits$rec_pars[1, 1] <- ln_R0
    cat(sprintf("log(R0) = %.4f  =>  R0 = %.4g\n", ln_R0, exp(ln_R0)))
  }

  # --- Recruitment devs with Methot-Taylor bias-adjustment ramp ---
  # NOTE (memory: feedback_ss3_rec_bias_ramp): the M-T ramp is applied here so
  # the REALIZED recruitment matches SS3; the recruitment LIKELIHOOD will still
  # diverge from SS3 by design (Rceattle does not implement the ramp penalty).
  sigma_R <- get_par(parlist$SR_parms, "SR_sigmaR") %||% 0.6
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
    cat(sprintf("Set rec_dev for %d years (sigmaR=%.3f)\n", n_set, sigma_R))
  }

  # --- Richards/VB growth params ---
  K_vb  <- get_par(parlist$MG_parms, "VonBert_K")
  L_min <- get_par(parlist$MG_parms, "L_at_Amin")
  L_max <- get_par(parlist$MG_parms, "L_at_Amax")
  Rich  <- get_par(parlist$MG_parms, "Richards")
  SD_y  <- get_par(parlist$MG_parms, "CV_young")
  SD_o  <- get_par(parlist$MG_parms, "CV_old")
  if (!is.null(K_vb) && "log_growth_pars" %in% names(inits)) {
    inits$log_growth_pars[1, 1, 1] <- log(K_vb)
    inits$log_growth_pars[1, 1, 2] <- log(max(L_min, 0.01))
    inits$log_growth_pars[1, 1, 3] <- log(L_max)
    # Richards shape parameter: 4th growth slot if the model carries it.
    if (!is.null(Rich) && dim(inits$log_growth_pars)[3] >= 4)
      inits$log_growth_pars[1, 1, 4] <- log(max(Rich, 1e-4))
    cat(sprintf("Growth injected: K=%.4f L1=%.4f Linf=%.4f shape=%.4f\n",
                K_vb, L_min, L_max, Rich %||% NA))
  }
  # Rceattle growth_log_sd = log(absolute SD in cm) at L1 and Linf (growth.hpp
  # interpolates linearly by length). SS3 CV_Growth_Pattern = 0 stores CVs, so
  # convert to SD: SD(L1) = CV_young * L1, SD(Linf) = CV_old * Linf.
  if (!is.null(SD_y) && !is.null(L_min) && "growth_log_sd" %in% names(inits))
    inits$growth_log_sd[1, 1, 1] <- log(SD_y * L_min)
  if (!is.null(SD_o) && !is.null(L_max) && "growth_log_sd" %in% names(inits))
    inits$growth_log_sd[1, 1, 2] <- log(SD_o * L_max)
  if (!is.null(SD_y) && !is.null(SD_o))
    cat(sprintf("Growth SD: SD(L1)=%.3f cm, SD(Linf)=%.3f cm (CV %.3f/%.3f)\n",
                SD_y * L_min, SD_o * L_max, SD_y, SD_o))

  # --- Weight-length ---
  W1 <- get_par(parlist$MG_parms, "Wtlen_1_Fem_GP_1")
  W2 <- get_par(parlist$MG_parms, "Wtlen_2_Fem_GP_1")
  if (!is.null(W1) && !is.null(W2) && "weight_length_pars" %in% names(inits)) {
    inits$weight_length_pars[1, 1] <- W1
    inits$weight_length_pars[1, 2] <- W2
    cat(sprintf("W-L: alpha=%.6g, beta=%.4f\n", W1, W2))
  }

  # --- Survey catchability (simple q, log scale) ---
  if ("index_log_q" %in% names(inits)) {
    for (i in seq_len(nrow(fleet_meta))) {
      if (fleet_meta$fleet_type[i] != "Survey") next
      q <- get_par(parlist$Q_parms,
                   sprintf("LnQ_base_%s\\(%d\\)$", fleet_meta$name[i], fleet_meta$ss3_num[i]))
      if (!is.null(q)) {
        inits$index_log_q[i] <- q
        cat(sprintf("  q[%s] = %.4f (exp = %.4f)\n", fleet_meta$name[i], q, exp(q)))
      }
    }
  }
  inits
}


# =============================================================================
# 6. SS3 N-at-age injection (initMode 4 = NonEquilibriumScaled), pinning styr N.
#    SS3 AI cod has InitF (= 0.0595) applied to the initial equilibrium; in
#    Rceattle initMode 4, Finit plays the same role. init_dev absorbs whatever
#    non-equilibrium structure SS3's styr N carries on top of the equilibrium,
#    pinning N at styr exactly. At minage = 0, slot k = SS3 int_Age (k-1).
# =============================================================================
init_state_from_ss3_natage_mode4 <- function(inits, ss3_rep, styr, nages,
                                              R_init, Finit, M1_at_age) {
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
  cat(sprintf("\n[mode 4] SS3 natage[%d]: %s\n", styr,
              paste(sprintf("%.4g", ss3_N), collapse = ", ")))
  for (k in seq_len(nages - 1)) {
    age <- k
    mort_sum <- sum(as.numeric(M1_at_age[1:age])) + Finit
    target_N <- ss3_N[k + 1]
    if (age == (nages - 1)) {
      geom <- 1 - exp(-as.numeric(M1_at_age[nages]) - Finit)
      target_N <- target_N * geom
    }
    inits$init_dev[1, k] <- log(max(target_N, 1e-10)) - log(R_init) + mort_sum
  }
  cat(sprintf("[mode 4] init_dev[1, 1:%d] set to pin styr N\n", nages - 1))
  inits
}


# =============================================================================
# 7. log_F pinning (single fishery, per-year) from SS3 timeseries F:_1
# =============================================================================
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
# 8. Wire it all up
# =============================================================================
inits <- init_from_ss3(parlist, ctllist, mod0$estimated_params, cod,
                       fleet_meta, years_hind, mod0)

# Inject fitted Length-DoubleNormal selectivity params (case 8):
#   sel_inf[1, flt]     = peak
#   sel_inf[2, flt]     = logit(right_floor)
#   log_sel_slp[1, flt] = log(sigma_ascending)
#   log_sel_slp[2, flt] = log(sigma_descending)
if ("sel_inf" %in% names(inits) && "log_sel_slp" %in% names(inits)) {
  for (fi in seq_len(n_flt)) {
    f <- dn_fits[[fi]]; if (is.null(f)) next
    inits$sel_inf[1, fi, 1]     <- f$peak
    inits$sel_inf[2, fi, 1]     <- f$logit_floor
    inits$log_sel_slp[1, fi, 1] <- f$log_sig_asc
    inits$log_sel_slp[2, fi, 1] <- f$log_sig_desc
  }
  cat("Injected fitted DoubleNormal sel params into inits\n")
}

# Initial F: SS3 InitF (= 0.0595) -> Rceattle log_Finit. init_dev (below)
# pins styr N regardless, but set Finit consistently for the equilibrium base.
Finit_ss3 <- parlist$init_F %||% 0
if (length(Finit_ss3) == 0 || is.na(Finit_ss3[1])) Finit_ss3 <- 0
Finit_ss3 <- as.numeric(Finit_ss3[1])
if ("log_Finit" %in% names(inits) && Finit_ss3 > 0) {
  inits$log_Finit[1] <- log(Finit_ss3)
  cat(sprintf("\nInitF (SS3) = %.5f -> log_Finit = %.4f\n", Finit_ss3, log(Finit_ss3)))
}

R_init    <- exp(parlist$SR_parms["SR_LN(R0)", "ESTIM"])
M1_at_age <- rep(M_base, nages)
inits <- init_state_from_ss3_natage_mode4(inits, ss3_rep, cod$styr, nages,
                                          R_init = R_init, Finit = Finit_ss3,
                                          M1_at_age = M1_at_age)
inits <- init_log_F_from_ss3(inits, ss3_rep, fleet_meta, years_hind)


# =============================================================================
# 9. Forward-pass fit (estimateMode = 3) and comparison to SS3
# =============================================================================
cat("\n--- Forward-pass fit (estimateMode = 3) ---\n")
fp <- Rceattle::fit_mod(
  data_list    = cod,
  inits        = inits,
  estimateMode = 3,
  initMode     = "FishedNonEquilibriumScaled",   # = 4
  growthFun    = growthFun_spec,
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  fit_control  = fit_control(phase = FALSE, verbose = 1)
)

# --- R / Bio / SSB / F vs SS3 -----------------------------------------------
ts_ss3 <- ss3_rep$timeseries[match(years_hind, ss3_rep$timeseries$Yr), ]
rce_ssb <- as.numeric(fp$quantities$ssb[1, seq_along(years_hind)])
rce_bio <- as.numeric(fp$quantities$biomass[1, seq_along(years_hind)])
rce_R   <- as.numeric(fp$quantities$R[1, seq_along(years_hind)])
relerr  <- function(a, b) ifelse(b == 0, NA, abs(a - b) / abs(b))
cmp <- data.frame(
  Year   = years_hind,
  SSB_ss3 = ts_ss3$SpawnBio, SSB_rce = rce_ssb, SSB_re = relerr(rce_ssb, ts_ss3$SpawnBio),
  Bio_ss3 = ts_ss3$Bio_all,  Bio_rce = rce_bio, Bio_re = relerr(rce_bio, ts_ss3$Bio_all),
  R_ss3   = ts_ss3$Recruit_0, R_rce  = rce_R,   R_re  = relerr(rce_R, ts_ss3$Recruit_0)
)
cat("\n=== R / Bio / SSB comparison (head & tail) ===\n")
print(head(cmp), digits = 5); print(tail(cmp), digits = 5)
cat(sprintf("\nMax rel err  SSB=%.2e  Bio=%.2e  R=%.2e\n",
            max(cmp$SSB_re, na.rm = TRUE), max(cmp$Bio_re, na.rm = TRUE),
            max(cmp$R_re, na.rm = TRUE)))

# --- Grouped NLL comparison vs SS3 likelihoods_used -------------------------
jnll <- fp$quantities$jnll_comp
cat("\n=== Rceattle jnll_comp (row sums) ===\n")
print(round(rowSums(jnll), 4))
cat(sprintf("\nRceattle total jnll = %.4f\n", sum(jnll)))
cat("\n=== SS3 likelihoods_used ===\n")
print(ss3_rep$likelihoods_used[, "values", drop = FALSE])

# --- Mapped component comparison (Rceattle vs SS3) ---------------------------
rce <- rowSums(jnll)
ss  <- setNames(ss3_rep$likelihoods_used[, "values"],
                rownames(ss3_rep$likelihoods_used))
gap_tbl <- data.frame(
  Component = c("Survey/Index", "Catch", "Length comp", "CAAL (age comp)",
                "Recruitment"),
  Rceattle  = round(c(rce["Index data"], rce["Catch data"],
                      rce["Composition data"], rce["CAAL data"],
                      rce["Recruitment deviates"]), 2),
  SS3       = round(c(ss["Survey"], ss["Catch"], ss["Length_comp"],
                      ss["Age_comp"], ss["Recruitment"]), 2),
  Note      = c("+~0.5*log(2pi)*n index const (expected)",
                "fishery mid-yr wt + sel-form (structural)",
                "comparable; residual ALK/sel shape",
                "dmultinom const offset (CAAL has no deviance form)",
                "Methot-Taylor bias ramp not implemented (by design)"),
  stringsAsFactors = FALSE
)
cat("\n=== Mapped NLL component comparison ===\n")
print(gap_tbl, row.names = FALSE)
cat(sprintf("\nInit-abundance-dev penalty (Rceattle-only): %.2f\n",
            rce["Initial abundance deviates"]))

# --- SSB / maturity scaling diagnostics -------------------------------------
cat("\n=== SSB detail (single-sex FracFemale check) ===\n")
print(head(cmp[, c("Year", "SSB_ss3", "SSB_rce", "SSB_re")], 4), digits = 6)
cat(sprintf("SSB_rce / SSB_ss3 (year 1): %.4f\n", cmp$SSB_rce[1] / cmp$SSB_ss3[1]))
cat("sex_ratio (Age0..):", paste(round(as.numeric(cod$sex_ratio[1, -1]), 3), collapse = " "), "\n")
cat("maturity (Age0..):",  paste(round(as.numeric(cod$maturity[1, -1]), 3),  collapse = " "), "\n")

# --- Growth / ALK comparison (Richards) -------------------------------------
cat("\n=== Length-at-age: Rceattle length_hat vs SS3 Len_Beg ===\n")
eg <- ss3_rep$endgrowth %>% dplyr::filter(Sex == 1) %>% dplyr::arrange(int_Age)
ss3_laa <- eg$Len_Beg[match(0:(nages - 1), eg$int_Age)]
rce_laa <- tryCatch({
  lh <- fp$quantities$length_hat
  d <- dim(lh); cat("length_hat dim:", paste(d, collapse = "x"), "\n")
  as.numeric(lh[1, 1, , 1])[seq_len(nages)]
}, error = function(e) { cat("length_hat err:", conditionMessage(e), "\n"); rep(NA, nages) })
print(data.frame(SS3_age = 0:(nages - 1), SS3_Len = round(ss3_laa, 2),
                 Rce_Len = round(rce_laa, 2)))

cat("\n=== growth_matrix (ALK) row sums (should be ~1 per age) ===\n")
gm <- tryCatch(fp$quantities$growth_matrix, error = function(e) NULL)
if (!is.null(gm)) {
  cat("growth_matrix dim:", paste(dim(gm), collapse = "x"), "\n")
  rs <- tryCatch(apply(gm[1, 1, , ], 1, sum), error = function(e) NA)
  cat("row sums (age 0..):", paste(round(head(rs, nages), 3), collapse = " "), "\n")
}

saveRDS(list(fp = fp, cmp = cmp, cod = cod, inits = inits, ss3_rep = ss3_rep),
        "Bridging/_fp_result.rds")
cat("\nForward pass complete. Saved Bridging/_fp_result.rds\n")
