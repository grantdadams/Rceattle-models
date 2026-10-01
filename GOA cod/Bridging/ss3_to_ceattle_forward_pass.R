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
# LLSrv's catchability carries SS3's env link type 1, which MULTIPLIES log q by
# exp(beta*env) (SS_timevaryparm.tpl:206). Rceattle's "Environmental" is the
# additive form (SS3's type 2) and is not a substitute, but link = "exponential"
# (5.47.0) is that form, and this script now carries it: it closes 9.9646 nats of
# index likelihood, all of it on LLSrv. RCE_Q_ENV=false drops it again.
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

# ss3_num is the fleet's own code; ss3_src is the SS3 fleet its parameters come
# from, which differs only for a fleet the converter split off to carry a second
# ageing-error matrix. Such a fleet shares its parent's Selectivity_index, so
# that is where SS3's selectivity and catchability for it live.
fleet_meta <- data.frame(
  name       = cod$fleet_control$Fleet_name,
  ss3_num    = cod$fleet_control$Fleet_code,
  ss3_src    = as.integer(cod$fleet_control$Selectivity_index),
  q_src      = as.integer(cod$fleet_control$Catchability_index),
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
# 2. Selectivity: SS3 pattern 24 on LENGTH, varied through selectivity linkages
# =============================================================================
# SS3 gives five fleets a pattern-24 double normal with block replacements and,
# for 1977-1989, per-year DEVmult deviations -- 126 selectivity parameters.
#
# The converter's default is per-year emp_sel from SS3's realized Asel2, which
# carries all of that exactly. It cannot be used here: empirical selectivity is
# AGE-based by construction (selectivity.hpp writes sel_at_age and never
# sel_at_length), and GOA's CAAL data are predicted from selectivity-at-LENGTH.
# On emp_sel, Rceattle warns and scores every CAAL row against a flat
# composition it cannot move.
#
# So the fleets go on DoubleNormalSS3 at Selectivity_dimension = "Length", and
# the time variation goes in as selectivity LINKAGES, which is what the schema
# names form 15 for. Each of the six parameters has a linkage alias matching
# SS3's own name, and an identity-link coefficient is an offset on SS3's own
# scale: P_k(yr) = (sel_dn6_k + off_nat_k(yr)) * exp(off_log_k(yr)).
#
# Rather than reproduce SS3's block and dev ALGEBRA, take its RESULT: SelSizeAdj
# reports the effective P1..P6 for every year, so one identity-link offset per
# (fleet, parameter, year) equal to (effective - base) reproduces blocks and
# DEVmults uniformly, whatever mechanism SS3 used to get there.
# =============================================================================
active_sel <- which(as.character(cod$fleet_control$Fleet_type) != "Off")
for (fi in active_sel) {
  cod$fleet_control$Selectivity[fi]           <- "DoubleNormalSS3"
  cod$fleet_control$Selectivity_dimension[fi] <- "Length"
  cod$fleet_control$Time_varying_sel[fi]      <- "Off"
}

# SS3's effective per-year P1..P6, forward-filled: SelSizeAdj lists only the
# years in which a parameter CHANGES.
ssa <- ss3_rep$SelSizeAdj
stopifnot(!is.null(ssa))
sel_eff <- array(NA_real_, c(n_flt, 6, length(years_hind)))
for (fi in active_sel) {
  d <- ssa[ssa$Fleet == fleet_meta$ss3_src[fi] & ssa$Yr %in% years_hind, ]
  if (!nrow(d)) next
  d <- d[order(d$Yr), ]
  for (k in 1:6) {
    v <- rep(NA_real_, length(years_hind))
    v[match(d$Yr, years_hind)] <- d[[paste0("Par", k)]]
    for (i in seq_along(v)) if (is.na(v[i]) && i > 1) v[i] <- v[i - 1]
    sel_eff[fi, k, ] <- v
  }
}

# A -999 in P5/P6 is SS3's "this end has no floor" sentinel, and Rceattle reads
# it as data (it switches the formula, not just a value), so it must be constant
# through time -- an offset on a sentinel is meaningless.
for (fi in active_sel) for (k in 1:6) {
  v <- sel_eff[fi, k, ]
  if (all(is.na(v))) next
  sent <- abs(v) > 900
  if (any(sent) && !all(sent))
    stop(sprintf("fleet %s P%d moves on and off SS3's -999 sentinel; an offset cannot represent that",
                 fleet_meta$name[fi], k))
}

sel_base <- sel_eff[, , 1, drop = TRUE]                      # value in styr
sel_off  <- sweep(sel_eff, c(1, 2), sel_base, "-")           # effective - base
sel_off[!is.finite(sel_off)] <- 0

# Per-year indicator columns, only for years something actually moves.
vary_yr <- which(apply(abs(sel_off), 3, function(z) max(z, na.rm = TRUE)) > 1e-8)
yr_cols <- sprintf("selyr%d", years_hind[vary_yr])
for (j in seq_along(vary_yr)) {
  cod$env_data[[yr_cols[j]]] <-
    as.integer(cod$env_data$Year == years_hind[vary_yr[j]])
}
cat(sprintf("\nSelectivity linkages: %d year columns (%d-%d)\n",
            length(yr_cols), min(years_hind[vary_yr]), max(years_hind[vary_yr])))

# ---- Option: SS3's own parameterisation, for estimation parity --------------
# RCE_SEL_PARITY=true replaces the per-year design below with SS3's two
# mechanisms, so Rceattle estimates the SAME coefficients SS3 does rather than
# one per varying year (which makes a 10-year block ten parameters):
#
#   blocks  Blk_Fxn = 2 REPLACES the parameter over a year range, so one
#           identity-link column per block, coefficient = block - base.
#   devs    realized = base * exp(dev * dev_se), dev_se fixed at 0.2, so one
#           LOG-link column per dev year, coefficient = dev * 0.2. Verified to
#           SS3's printed precision on all 63 (see GOA-parameter-parity.md).
#
# They compose as base * exp(log_off) + nat_off only because they never overlap:
# devs are 1977-1989 and blocks start 1990 or 1996.
SEL_PARITY <- identical(tolower(Sys.getenv("RCE_SEL_PARITY", "false")), "true")

# SS3's parameter table, for base values, phases and the BLK/DEVmult names.
.ss3_P <- local({
  ln <- readLines(file.path(SS3_DIR, "Report.sso"), warn = FALSE)
  h  <- grep("^Num +Label +Value +Active_Cnt", ln)[1]
  j <- h + 1; lab <- character(0); v <- numeric(0); ph <- character(0)
  while (j <= length(ln)) {
    f <- strsplit(trimws(ln[j]), "[ \t]+")[[1]]
    if (length(f) < 5 || !grepl("^[0-9]+$", f[1])) break
    lab <- c(lab, f[2]); v <- c(v, as.numeric(f[3])); ph <- c(ph, f[5]); j <- j + 1
  }
  list(value = stats::setNames(v, lab), phase = stats::setNames(as.numeric(ph), lab))
})
# SS3 labels a pattern-24 parameter Size_DblN_<name>_<Fleet>(<n>); the names
# carry parentheses, so every lookup here is literal, never a regex.
SS3_PAR_NAME <- c("peak", "top_logit", "ascend_se", "descend_se",
                  "start_logit", "end_logit")
# A fleet split for ageing error (Srv_ae1) shares its parent's curve, so resolve
# through Selectivity_index -- its own name has no SS3 parameters.
.ss3_sel_stem <- function(fi, k) {
  j <- match(fleet_meta$ss3_src[fi], fleet_meta$ss3_num)
  if (is.na(j)) j <- fi
  sprintf("Size_DblN_%s_%s(%d)", SS3_PAR_NAME[k], fleet_meta$name[j],
          fleet_meta$ss3_num[j])
}

# One linkage per pattern-24 parameter, restricted to the fleets that move it.
PAR_LINK <- c("dn_peak", "top_logit", "ascend_se", "descend_se",
              "start_logit", "end_logit")
sel_linkages <- list()
if (SEL_PARITY) {
  # SS3's own coefficients: one per block, one per dev year, per fleet-parameter.
  DEV_SE <- 0.2
  n_blk <- n_blk_held <- n_dev <- 0L
  for (k in 1:6) {
    specs <- list()
    for (src in unique(fleet_meta$ss3_src[active_sel])) {
      grp <- active_sel[fleet_meta$ss3_src[active_sel] == src]
      fi  <- grp[1]
      stem <- .ss3_sel_stem(fi, k)
      nm   <- names(.ss3_P$value)
      blk  <- nm[startsWith(nm, paste0(stem, "_BLK"))]
      dev  <- nm[startsWith(nm, paste0(stem, "_DEVmult_"))]
      if (!length(blk) && !length(dev)) next
      base <- .ss3_P$value[[stem]]

      # -- blocks: identity, coefficient = block value - base, over the range.
      b_cols <- character(0); b_init <- list(); b_held <- character(0)
      for (bn in blk) {
        pat <- as.integer(sub(".*_BLK([0-9]+)(repl|add)_.*", "\\1", bn))
        yr0 <- as.integer(sub(".*_BLK[0-9]+(repl|add)_", "", bn))
        rng <- ctllist$Block_Design[[pat]]
        rng <- matrix(rng, ncol = 2, byrow = TRUE)
        r   <- rng[rng[, 1] == yr0, , drop = FALSE]
        stopifnot(nrow(r) == 1L)
        cn  <- sprintf("s%dp%d_blk%d", src, k, yr0)
        cod$env_data[[cn]] <- as.integer(cod$env_data$Year >= r[1, 1] &
                                         cod$env_data$Year <= r[1, 2])
        b_cols <- c(b_cols, cn)
        b_init[[cn]] <- .ss3_P$value[[bn]] - base
        if (!is.finite(.ss3_P$phase[[bn]]) || .ss3_P$phase[[bn]] <= 0)
          b_held <- c(b_held, cn)
      }
      # -- devs: LOG link, coefficient = dev * dev_se, one year each.
      d_cols <- character(0); d_init <- list()
      for (dn in dev) {
        y  <- as.integer(sub(".*_DEVmult_", "", dn))
        cn <- sprintf("s%dp%d_dev%d", src, k, y)
        cod$env_data[[cn]] <- as.integer(cod$env_data$Year == y)
        d_cols <- c(d_cols, cn)
        d_init[[cn]] <- .ss3_P$value[[dn]] * DEV_SE
      }

      # Held blocks need their own spec: est_phase is per spec, not per column.
      free_b <- setdiff(b_cols, b_held)
      if (length(free_b)) {
        specs[[length(specs) + 1L]] <- linkage_spec(
          formula = stats::reformulate(c("0", free_b)),
          fleet = fleet_meta$ss3_num[grp], link = "identity",
          init = b_init[free_b])
        n_blk <- n_blk + length(free_b)
      }
      if (length(b_held)) {
        specs[[length(specs) + 1L]] <- linkage_spec(
          formula = stats::reformulate(c("0", b_held)),
          fleet = fleet_meta$ss3_num[grp], link = "identity",
          init = b_init[b_held], est_phase = 0)
        n_blk_held <- n_blk_held + length(b_held)
      }
      if (length(d_cols)) {
        specs[[length(specs) + 1L]] <- linkage_spec(
          formula = stats::reformulate(c("0", d_cols)),
          fleet = fleet_meta$ss3_num[grp], link = "log", init = d_init)
        n_dev <- n_dev + length(d_cols)
      }
    }
    if (length(specs)) sel_linkages[[PAR_LINK[k]]] <- specs
  }
  cat(sprintf("\n[sel parity] %d block coefficients (+%d held), %d dev coefficients = %d\n",
              n_blk, n_blk_held, n_dev, n_blk + n_dev))
} else {
for (k in 1:6) {
  flts <- active_sel[sapply(active_sel, function(fi)
    any(abs(sel_off[fi, k, ]) > 1e-8, na.rm = TRUE))]
  if (!length(flts)) next
  cols <- yr_cols[sapply(vary_yr, function(y)
    any(abs(sel_off[flts, k, y]) > 1e-8, na.rm = TRUE))]
  if (!length(cols)) next
  sel_linkages[[PAR_LINK[k]]] <- linkage_spec(
    formula = stats::reformulate(c("0", cols)),
    by      = ~ fleet,
    fleet   = fleet_meta$ss3_num[flts],
    link    = "identity"
  )
  cat(sprintf("  %-12s fleets %s, %d year column(s)\n", PAR_LINK[k],
              paste(fleet_meta$name[flts], collapse = "/"), length(cols)))
}
}
# KNOWN GAP -- SS3's AGE selectivity, which Rceattle cannot multiply in.
# All five fleets carry SS3 age pattern 10, which sets ages 1..nages to 1 and
# leaves age 0 at zero (SS_selex.tpl:997-1001); SS3's realized Asel2 is the
# size-derived curve TIMES that, so age 0 is zeroed. Rceattle has no per-fleet
# age multiplier alongside a length-based curve.
#
# It shows on fleet 4 alone, because it alone has a non-zero initial floor
# (P5 = -2.79 -> 0.0577 at the smallest lengths): its sel_at_age(age 0) is
# 0.0169 against SS3's 0, while ages 1, 2, 3 agree exactly (0.05883, 0.1286,
# 0.49451). The other four fleets' size curves are already ~0 there.
#
# Bin_first_selected is NOT the fix: rule 10 says it is read on the fleet's own
# Selectivity_dimension, which is Length here, so it zeroes population LENGTH
# bin 0 rather than age 0. Setting it to 2 lowers the objective by 6.4 nats,
# but only by breaking the length curve SS3's ramp defines at that bin, so it
# is deliberately left alone.
selFun_spec <- build_selectivity(linkages = sel_linkages)

# ---- LLSrv catchability: SS3's environmental link type 1 --------------------
# control.ss_new gives LnQ_base_LLSrv(5) an env-var of 101: link type 1 on
# environmental variable 1. SS3 holds Svy_log_q = log(q) * exp(beta * x) and takes
# q as its exponential, which is Rceattle's link = "exponential".
# Variable 1 runs 1979-2024 and the model starts in 1977, so the covariate has to
# be filled over the missing years: model.matrix() drops NA rows and the linkage
# refuses a fixed-effect covariate holding NA. Zero is the right fill and is
# provably harmless -- exp(0) = 1 leaves the base q untouched, and LLSrv has no
# index observation before 1990, so nothing fitted reads those years.
q_env <- datlist$envdat[datlist$envdat$variable == 1L, c("year", "value")]
cod$env_data$LLSrv_q_env <- q_env$value[match(cod$env_data$Year, q_env$year)]
cod$env_data$LLSrv_q_env[!is.finite(cod$env_data$LLSrv_q_env)] <- 0
cat(sprintf("\nLLSrv q env link: variable 1 over %d-%d; %d model year(s) filled with 0\n",
            min(q_env$year), max(q_env$year),
            sum(!(cod$env_data$Year %in% q_env$year))))

# RCE_Q_ENV=false drops the link, so the index likelihood can be read with and
# without it; the lognormal constant cancels in the difference.
Q_ENV <- !identical(tolower(Sys.getenv("RCE_Q_ENV", "true")), "false")
qFun_spec <- if (Q_ENV) {
  build_catchability(linkages = list(
    q = linkage_spec(~ LLSrv_q_env, by = ~ fleet, fleet = "LLSrv",
                     link = "exponential")))
} else {
  build_catchability()
}
cat(sprintf("LLSrv q env link: %s\n", if (Q_ENV) "ON" else "OFF (baseline)"))


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

# --- SS3's priors -------------------------------------------------------------
# Verified against SS_objfunc.tpl Get_Prior and SS3's own reported Pr_Like:
#   Pr_type 3 "Log_Norm" = 0.5*((log(x) - Pr)/Psd)^2, NO bias correction
#   Pr_type 6 "Normal"   = 0.5*((x - Pr)/Psd)^2 on the NATURAL scale
# Rceattle's linkage prior on an (Intercept) row is evaluated against the BASE
# parameter, and for fam = normal against its NATURAL-scale value
# (ceattle.cpp: b_nat = exp(b)), so SS3's natural-scale normals map exactly with
# no delta-method conversion. Its lognormal centres at
# log(M_prior) - bias_adjust_proc * sd^2 / 2, so M_prior absorbs that term.
.bd <- function(label, field) {
  i <- grep(label, rownames(ctllist$MG_parms))
  if (!length(i)) return(NA_real_) else as.numeric(ctllist$MG_parms[i[1], field])
}
.pr <- function(label, field) {
  i <- grep(label, rownames(ctllist$MG_parms))
  if (!length(i)) return(NA_real_) else as.numeric(ctllist$MG_parms[i[1], field])
}
M_pr    <- .pr("^NatM", "PRIOR");        M_pr_sd    <- .pr("^NatM", "PR_SD")
Linf_pr <- .pr("L_at_Amax", "PRIOR");    Linf_pr_sd <- .pr("L_at_Amax", "PR_SD")
K_pr    <- .pr("VonBert_K", "PRIOR");    K_pr_sd    <- .pr("VonBert_K", "PR_SD")
cat(sprintf("SS3 bounds: K (%.3g, %.3g)  L1 (%.3g, %.3g)  Linf (%.3g, %.3g)\n",
            .bd("VonBert_K","LO"), .bd("VonBert_K","HI"), .bd("L_at_Amin","LO"),
            .bd("L_at_Amin","HI"), .bd("L_at_Amax","LO"), .bd("L_at_Amax","HI")))
cat(sprintf("\nSS3 priors: M Log_Norm(%.4f, %.4f)  Linf N(%.4f, %.4f)  K N(%.4f, %.4f)\n",
            M_pr, M_pr_sd, Linf_pr, Linf_pr_sd, K_pr, K_pr_sd))
# SS3's Log_Norm prior is on log(M) with mean M_pr directly; Rceattle centres at
# log(M_prior) - bias_adjust_proc*sd^2/2, and bias_adjust_proc is 1 here.
# The prior centre is shifted by +sd^2/2 because bias_adjust_proc subtracts
# sd^2/2 back off inside the likelihood; with that flag off the median would land
# 8.8% low, silently, so assert it rather than rely on the default.
stopifnot("this bridge assumes fit_control(bias_adjust_proc = TRUE)" =
            isTRUE(fit_control()$bias_adjust_proc))
M_prior_nat <- exp(M_pr + M_pr_sd^2 / 2)
cat(sprintf("  -> Rceattle M_prior = exp(%.4f + %.4f^2/2) = %.5f, sd %.3f\n",
            M_pr, M_pr_sd, M_prior_nat, M_pr_sd))
# NOT matched: SS3 also puts Log_Norm(%.2f, %.2f) on the 2014 block's M VALUE
# (worth 0.9887 of its 1.0285 Parm_priors). Rceattle's parameter there is the
# log-ratio log(M_block / M_base), so a prior on the block's M has no home.

M1_block <- build_M1(
  M1_model     = 1,
  M1_use_prior = TRUE,
  M_prior      = M_prior_nat,
  M_prior_sd   = M_pr_sd,
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

# SS3's Linf_decay is -999 for GOA cod -- "replicates 3.24", the 3.24 plus-group
# mean length -- where AI cod's is -998, "not allow growth above maxage". They
# are different settings and the AI value gives the plus group 85.31 cm against
# SS3's 89.76, which then propagates into selectivity-at-age, the age-length
# key and SSB.
stopifnot(identical(as.numeric(ctllist$Exp_Decay %||% NA), -999))  # r4ss name
growthFun_spec <- build_growth(
  fun               = "vonBertalanffy",
  sd_form           = "SD",
  plus_group_length = "SS3.24",
  sd_plus_group     = "WHAM",
  pop_lengths       = ss3_rep$lbinspop,
  linkages = list(
    K  = linkage_spec(formula = ~ 1, init = list("(Intercept)" = K_vb),
                      bounds = list("(Intercept)" = c(max(.bd("VonBert_K","LO"), 1e-3), .bd("VonBert_K","HI"))),
                      priors = list("(Intercept)" = prior_normal(K_pr, K_pr_sd))),
    L1 = linkage_spec(formula = ~ 1, init = list("(Intercept)" = L_min),
                      bounds = list("(Intercept)" = c(max(.bd("L_at_Amin","LO"), 1e-3), .bd("L_at_Amin","HI")))),
    Linf = linkage_spec(formula = ~ 1, init = list("(Intercept)" = L_max),
                        bounds = list("(Intercept)" = c(.bd("L_at_Amax","LO"), .bd("L_at_Amax","HI"))),
                        priors = list("(Intercept)" = prior_normal(Linf_pr, Linf_pr_sd)))
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
# ---- SS3's SR_regime as an unpenalised initial recruitment level -----------
# Read at top level; init_from_ss3()'s own `regime` is local to it. r4ss and
# ss_summary disagree on the suffix for an env-linked parameter (_ENV_add vs
# _ENV_mult), so match the stem only.
regime_shift <- gp(parlist$SR_parms, "SR_regime_BLK")
# Block pattern 5 is 1976-1976, one year at styr - 1: it shifts the level the
# INITIAL age-structure sits at, not any fitted year. Folding it into init_dev
# pins the numbers but charges the shift the recruitment-deviate penalty, which
# the optimiser then retires by lowering R0 (0.37 log units, R0 31% low). An
# `init` linkage carries the level with no penalty instead, so init_dev holds
# only SS3's Early_InitAge departures. RCE_INIT_LINK=false folds it back.
INIT_LINK <- !identical(tolower(Sys.getenv("RCE_INIT_LINK", "true")), "false")
regime_lvl <- if (INIT_LINK && !is.na(regime_shift)) regime_shift else 0
recFun_spec <- if (INIT_LINK && !is.na(regime_shift)) {
  cod$env_data$init_lvl <- 1
  build_srr(linkages = list(init = linkage_spec(
    formula = ~ 0 + init_lvl,
    init    = list(init_lvl = regime_shift))))
} else {
  build_srr()
}
cat(sprintf("init level linkage: %s (SR_regime = %.6f)\n",
            if (INIT_LINK && !is.na(regime_shift)) "ON" else "OFF", regime_shift))

cat("\n--- Building mod0 (parameter shape) ---\n")
mod0 <- Rceattle::fit_mod(
  data_list    = cod,
  inits        = NULL,
  estimateMode = 3,
  initMode     = INIT_MODE,
  growthFun    = growthFun_spec,
  M1Fun        = M1_block,
  recFun       = recFun_spec,
  selFun       = selFun_spec,
  qFun         = qFun_spec,
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
  # Only the BASE log q is injected here; LLSrv's env link type 1 rides on top of
  # it as a q linkage, and its coefficient is injected with the other linkage
  # betas below.
  if ("index_log_q" %in% names(inits)) {
    for (i in seq_len(nrow(fleet_meta))) {
      if (fleet_meta$fleet_type[i] != "Survey") next
      # Read q for the fleet whose catchability block this fleet belongs to, not
      # for the fleet itself. A converter-created fleet (an ageing-error split)
      # has no SS3 fleet of its own, so looking up its own name finds nothing and
      # it keeps a log q of 0 -- which TMB then averages with its block-mates,
      # moving the real fleet's q. That cost Srv 18% of its index, silently.
      src <- fleet_meta$q_src[i]
      j   <- match(src, fleet_meta$ss3_num)
      if (is.na(j)) j <- i
      q <- gp(parlist$Q_parms,
              sprintf("LnQ_base_%s\\(%d\\)$", fleet_meta$name[j], fleet_meta$ss3_num[j]))
      if (!is.na(q)) {
        inits$index_log_q[i] <- q
        cat(sprintf("  q[%s] = %.4f (exp = %.4f)%s\n", fleet_meta$name[i], q, exp(q),
                    if (j != i) sprintf(" [from %s]", fleet_meta$name[j]) else ""))
      }
    }
    # Fleets sharing a Catchability_index share ONE index_log_q, and TMB starts a
    # shared parameter at the mean of its members' values. Differing values are
    # therefore silently averaged, and unlike the deviation sds nothing in
    # Rceattle warns about it, so check here.
    for (ci in unique(stats::na.omit(fleet_meta$q_src))) {
      grp <- which(fleet_meta$q_src == ci & fleet_meta$fleet_type == "Survey")
      if (length(grp) < 2) next
      v <- inits$index_log_q[grp]
      if (diff(range(v)) > 1e-8)
        stop(sprintf("Fleets sharing Catchability_index %d (%s) were given ", ci,
                     paste(fleet_meta$name[grp], collapse = ", ")),
             "different log q (", paste(signif(v, 6), collapse = ", "),
             "). They share one parameter, so it would start at their mean and ",
             "no fleet would keep its own value.")
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
                                       M1_at_age, level = 0) {
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
    # `level` is what an init linkage carries (0 when it is off); subtracting it
    # leaves the SAME numbers-at-age with the level outside the penalised devs.
    inits$init_dev[1, k] <- log(max(target_N, 1e-10)) - log(R_init) + mort_sum - level
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

# --- Selectivity: base P1..P6 and the per-year offsets ---
# The base is SS3's EFFECTIVE value in the first hindcast year, not the ctl
# base parameter: SelSizeAdj already folds in whatever block or dev applied in
# that year, and every offset below is measured against it. -999 goes in raw,
# because fit_mod() derives sel_dn6_ends from sel_dn6[5:6] > -999.
# Under RCE_SEL_PARITY the base must be SS3's BASE parameter instead, because the
# coefficients are SS3's own: a 1977 base would absorb DEVmult_1977 and leave
# three dev coefficients short on FshTrawl.
for (fi in active_sel) {
  v <- sel_base[fi, ]
  if (SEL_PARITY) for (k in 1:6) {
    stem <- .ss3_sel_stem(fi, k)
    if (stem %in% names(.ss3_P$value)) v[k] <- .ss3_P$value[[stem]]
  }
  v[!is.finite(v)] <- -999
  inits$sel_dn6[, fi, 1] <- v
  cat(sprintf("  sel_dn6[%s] base: %s\n", fleet_meta$name[fi],
              paste(signif(v, 6), collapse = " ")))
}

tbl <- mod0$data_list$linkage_table
n_set <- 0; max_off <- 0
for (k in 1:6) {
  if (!PAR_LINK[k] %in% names(sel_linkages)) next
  for (fi in active_sel) for (j in seq_along(vary_yr)) {
    off <- sel_off[fi, k, vary_yr[j]]
    if (!is.finite(off) || abs(off) < 1e-8) next
    row <- which(tbl$process == "sel" & tbl$param == PAR_LINK[k] &
                 tbl$fleet == fleet_meta$ss3_num[fi] & tbl$design_col == yr_cols[j])
    if (length(row) != 1L) {
      warning(sprintf("linkage row for %s/%s/%s: found %d (expected 1)",
                      PAR_LINK[k], fleet_meta$name[fi], yr_cols[j], length(row)))
      next
    }
    inits$beta_linkage[row] <- off
    n_set <- n_set + 1; max_off <- max(max_off, abs(off))
  }
}
cat(sprintf("Set %d selectivity linkage coefficients (largest |offset| = %.4g)\n",
            n_set, max_off))

# LLSrv's env-link coefficient. r4ss labels the .par entry _ENV_add even though
# SS3's type 1 is multiplicative (ss_summary.sso calls it _ENV_mult), so match
# either spelling; the value is the same.
q_beta_row <- grep("LnQ_base_LLSrv\\(5\\)_ENV_(add|mult)",
                   rownames(parlist$Q_parms))
if (Q_ENV && length(q_beta_row) == 1L) {
  q_beta  <- parlist$Q_parms[q_beta_row, "ESTIM"]
  q_b_row <- which(tbl$process == "q" & tbl$design_col == "LLSrv_q_env")
  if (length(q_b_row) == 1L) {
    inits$beta_linkage[q_b_row] <- q_beta
    cat(sprintf("q env link[LLSrv]: beta = %.6f\n", q_beta))
  } else {
    warning(sprintf("q env linkage row: found %d (expected 1)", length(q_b_row)))
  }
}

R_init    <- exp(inits$rec_pars[1, 1])
M1_at_age <- rep(M_base, nages)
inits <- init_state_from_ss3_natage(inits, ss3_rep, cod$styr, nages,
                                    R_init = R_init, M1_at_age = M1_at_age,
                                    level = regime_lvl)
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
  recFun       = recFun_spec,
  selFun       = selFun_spec,
  qFun         = qFun_spec,
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
