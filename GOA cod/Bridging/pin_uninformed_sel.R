# Hold the selectivity coefficients GOA Pcod's data do not inform, and refit.
#
# At the 330-parameter optimum the Hessian is indefinite -- 5 negative
# eigenvalues, condition number 7.1e14 -- so `chol()` fails, TMBhelper returns
# before calling sdreport, and the fit carries no standard errors. The flat
# directions are not spread across the model: 23 of the 24 are dominated by one
# coefficient each of `top_logit` (the double normal's plateau width, logit
# scale) or `descend_se` (its descending-limb width, log scale) -- 15 and 7 of
# them, plus LLSrv's `top_logit` base, with six of the 24 a two-coefficient
# mixture inside that same set. The 24th is L1, below. Every one of the model's
# 15 `top_logit` coefficients is in the flat set. Two mechanisms, both visible
# in SS3's own control file:
#
#  1. FLAT DESCENDING LIMB (19 of the 23). SS3 fixes `end_logit` at 10 for
#     FshTrawl, FshLL and Srv's 1996 and 2006 blocks, i.e. final selectivity
#     0.99995. `selectivity.hpp:624-625` writes the limb as
#     `dsc = 1 + (point2 - 1) R` with `R` in [0, 1], so with `point2` at
#     logistic(10) the WHOLE curve right of the peak is pinned within
#     `1 - logistic(10)` = 4.54e-05 of 1, whatever the limb width or the plateau
#     width do. That is an analytic ceiling on any change, not a measurement,
#     and it is what the 4.9e-05 the check below reports is hitting. Confirmed
#     on the realised `sel_at_length`: min selectivity above the peak is 0.99995
#     in every year on both fleets.
#  2. SATURATED PLATEAU (4). `peak2 = peak + binwidth + (0.99 x_last - peak -
#     binwidth)/(1 + exp(-top_logit))`, a logistic, so `top_logit` stops acting
#     at BOTH ends of its range. Below about -8 the plateau is on its floor --
#     Srv pre-1996, FshPot pre-2017 and LLSrv realise 0.00 cm of plateau, and
#     d(plateau)/d(top_logit) is 1.5e-03 cm per logit unit. Above about +5 it is
#     against the `0.99 x_last` cap: on FshPot's 2017 block SS3's 6.74 and
#     Rceattle's 13.79 both put peak2 within a centimetre of that cap, so the
#     two describe the same curve. (Don't quote a plateau WIDTH in cm here
#     without deriving `x_last` from the model: it is `xmid(nbins - 1)` on the
#     POPULATION grid, and two reviewers read the dat's `104.5` differently --
#     as the last midpoint or as the last lower edge. The conclusion is the same
#     either way, the width is not.)
#
# Both mechanisms are structural -- neither depends on where the data are. (An
# earlier version of this header called FshPot's 2017 block a data-sparsity
# case; it is the upper branch of mechanism 2. Only 0.05% of that fleet's
# 2017-2024 composition mass sits in the top length bin, so the data claim was
# directionally true and not the reason.)
#
# The pin set is chosen by measured curvature -- Hessian diagonal below 1e-2 at
# the 330-parameter optimum -- and the threshold is not a judgement call: the
# sorted diagonals run ... 2.51e-05, 6.45e-05, 7.40e-05 | 5.20e-04 (L1) |
# 7.21e-02, 0.678, 0.904 ..., so any cut in [1e-4, 7e-2] selects the same 23
# after the family filter. The diagonal is also the right statistic to cut on,
# because profile curvature is `H_ii - B'A^-1 B <= H_ii`: a small diagonal is an
# UPPER bound on the information in that coefficient. All 23 have an implied
# standard error of at least 116 on their own scale.
#
# SS3 agrees with the partition, 23 for 23, from its own finite-difference
# Hessian on a run that does invert (`Data/goa_pcod_caal_lambda_on`, and the
# age-0-restored run): every one of the 23 carries an SS3 standard error between
# 46 and 500 on a parameter SS3 bounds to a width of 10-30, while the three
# `top_logit`/`descend_se` coefficients this script KEEPS free carry SS3 errors
# of 0.97, 0.39 and 0.34. No overlap, a 47-fold gap, two different codes and two
# different Hessian methods. That is the strongest evidence here.
#
# Held at SS3's values, not at Rceattle's fitted ones: Rceattle's fit takes four
# of them outside SS3's declared box, to descending limbs of 0.08-0.15 cm, where
# SS3's values are inside it and reproduce SS3's realised curve. Since the
# directions are flat, that choice is worth ~0.002 nats.
#
# "SS3's values" means the ones this bridge INJECTS, i.e. `RCE_SS3_DIR`
# (`..._age0`), not its sibling `..._age0_est`. Their ctl and dat files are
# BYTE-IDENTICAL; the only input difference is `starter.ss`
# `last_estimation_phase`, 0 against 20, so `..._age0` estimated nothing and
# every one of its 330 values is whatever it was handed, while `..._age0_est`
# fitted them. (Hence also: `..._age0/ss3.par` carries a STALE objective,
# 2051.976, copied from the run that wrote it, where its own `Report.sso` says
# 2128.82.) The fitted values therefore differ, but the result is insensitive to
# which set is used for the pin: pinning at
# the re-estimated run's values instead costs 0.00127 rather than 0.00168 nats
# and gives the same 307 of 307 standard errors with the same largest, 45.67.
#
# What it does NOT fix, and this is the larger threat to a catch number: the
# 24th flat direction is `log_growth_pars[2]` = L1, which stays free and parks on
# its 1e-3 lower bound, i.e. 0.001 cm at age 0.5 against SS3's 1.3007. Its
# standard error is the largest of the 307 and is not interpretable, because the
# normal reference distribution does not apply at an active constraint. It does
# not contaminate the rest -- its largest correlation with any other kept
# parameter is -0.13, with K -- but it moves the age-length key: at K = 0.2079
# and Linf = 99.461, length at age 1 is 9.82 cm here against 11.00 cm at SS3's
# L1, age 2 is 26.64 against 27.60, age 3 is 40.30 against 41.08. A ~1 cm shift
# at ages 1-3 moves selectivity-at-age and the age composition behind the catch
# advice, so it matters more than all 23 selectivity coefficients together.
# The circulation study's DSEM cod models hold L1 at SS3's value for exactly
# this reason; this script does not, so that the SS3 comparison keeps estimating
# what SS3 estimates. Two sibling analyses of the same stock therefore make
# opposite calls on the one parameter whose fitted value is biologically
# impossible. Treat this fit as an objective-parity exercise, not as a source of
# a growth curve, until that is settled.
#
# Two things this is NOT. It is not a claim that Rceattle fits these data better
# than SS3: on `..._age0_est`, the age-0-removed run that estimated, SS3 reports
# `Parm_StDev = 0` for all 330 with a stub `covar.sso`. (SS3's other runs DO
# invert, and their standard errors are the corroboration above -- do not read
# the stub as SS3 being unable to fit this model. Nor is `..._age0` evidence
# either way: `last_estimation_phase = 0` told it not to estimate at all.) On
# the Rceattle side the precise statement is that the FIT carries no standard
# errors, because TMBhelper returns at the `chol` gate before calling
# `sdreport`; called directly, `TMB::sdreport()` does return, with
# `pdHess = FALSE` and 5 NaN of 330. And it breaks the 330 = 330 parameter parity on purpose; quote this
# fit as "the 307 directions with non-zero curvature in the PENALISED
# objective", not as "the directions the data inform" -- 13 of the 307 are the
# FshTrawl `descend_se` deviates, whose Hessian diagonal is exactly
# 25.000 = 1/0.2^2, i.e. all of their curvature is their own prior and none of
# it is data.
#
#   Rscript Bridging/pin_uninformed_sel.R
#   RCE_REFIT=true Rscript Bridging/pin_uninformed_sel.R   # ignore the cached 330-par fit
#
# Leaves `pinned` (the refit), `pin` (what was held) and `base_fit` (the
# 330-parameter fit) in the global environment.

# ---- 0. The comparison configuration, as compare_cod_models.R sets it -------
.cfg <- c(RCE_SS3_DIR       = "Data/goa_pcod_caal_bins_fixed_age0",
          RCE_SS3_PHASE_DIR = "Data/goa_pcod_caal_bins_fixed_age0_est",
          RCE_SEL_PARITY    = "true",
          RCE_FIX_LINKAGE   = "false",
          RCE_SEL_DEV_ALL   = "true",
          RCE_SEL_REPLACE   = "true",
          RCE_INIT_PRIOR    = "false")
for (.k in names(.cfg)) if (!nzchar(Sys.getenv(.k))) do.call(Sys.setenv, as.list(.cfg[.k]))

source("Bridging/ss3_to_ceattle_forward_pass.R")

.fit <- function(inits, map, mode, getsd = FALSE) Rceattle::fit_mod(
  data_list = cod, inits = inits, map = map, estimateMode = mode,
  initMode = INIT_MODE, growthFun = growthFun_spec, M1Fun = M1_block,
  recFun = recFun_spec, selFun = selFun_spec, qFun = qFun_spec,
  random_rec = FALSE, msmMode = 0,
  fit_control = fit_control(phase = FALSE, verbose = 0, bias_adjust_obs = FALSE,
                            getsd = getsd))

# ---- 1. The 330-parameter fit, whose Hessian says what is flat --------------
.cache <- "Bridging/_compare_cod_models.rds"
if (file.exists(.cache) && !identical(tolower(Sys.getenv("RCE_REFIT")), "true")) {
  base_fit <- readRDS(.cache)
  cat(sprintf("\nreusing %s: objective %.8f, %d free\n", .cache,
              base_fit$opt$objective, length(base_fit$obj$par)))
} else {
  base_fit <- .fit(inits, ss3_map, 1, getsd = TRUE)
  saveRDS(base_fit, .cache)
}
H <- base_fit$opt[["hessian"]]
if (is.null(H))
  stop("The 330-parameter fit carries no Hessian, so there is nothing to read ",
       "curvature from. `.normalize_fit_tmb()` keeps it as `fit$opt$hessian` ",
       "only on the non-positive-definite path; a fit that converged cleanly ",
       "needs its Hessian taken with optimHess() instead.", call. = FALSE)
hdiag <- diag(H)

# ---- 2. Which coefficients: top_logit / descend_se with no curvature --------
# par positions are block-contiguous and ordered by the map factor's OWN levels.
# Those levels are numeric, not lexicographic, because the map is built from an
# integer vector -- reading them back with factor(as.character(.)) re-sorts "10"
# before "2" and mislabels every eigenvector.
nm <- names(base_fit$obj$par)
mp <- base_fit$map$mapFactor
# The pin is read off the FIT's map and applied to the one handed in, so the two
# must agree on the blocks it touches. They need not match on every block: a
# cached fit built against a DSEM-capable Rceattle carries five dsem_* blocks
# this install cannot produce. Compare the blocks read, not the whole list --
# and step 4 below checks the cache against THIS build's objective, which is
# what makes reusing a cross-build cache safe rather than merely convenient.
for (.b in c("beta_linkage", "sel_dn6"))
  stopifnot(identical(as.character(mp[[.b]]),
                      as.character(ss3_map$mapFactor[[.b]])))
tbl <- as.data.frame(mod0$data_list$linkage_table)
SLOT <- c("peak", "top_logit", "ascend_se", "descend_se", "start_logit", "end_logit")

# SS3's value behind one partition column. The column name carries the SS3
# source fleet and the parameter slot, `s<src>p<k>_blk<year>` or `s<src>p<k>_pre`,
# and `_pre` is SS3's base parameter -- the years no block covers.
.ss3_block_value <- function(dc) {
  m <- regmatches(dc, regexec("^s([0-9]+)p([0-9]+)_(blk([0-9]{4})|pre)$", dc))[[1]]
  if (!length(m)) return(NA_real_)
  stem <- .ss3_sel_stem(match(as.integer(m[2]), fleet_meta$ss3_num), as.integer(m[3]))
  if (identical(m[4], "pre"))
    return(if (stem %in% names(.ss3_P$value)) .ss3_P$value[[stem]] else NA_real_)
  v <- names(.ss3_P$value)
  j <- which(startsWith(v, paste0(stem, "_BLK")) & endsWith(v, paste0("_", m[5])))
  if (length(j) == 1L) .ss3_P$value[[j]] else NA_real_
}
FLAT <- 1e-2                    # curvature below this is at the objective's own
                                # numerical resolution: a +3 perturbation of any
                                # of these moves the 1859-nat objective by at
                                # most 9.3e-04, against 6.7, 66 and 97 nats for
                                # the three coefficients of the same two
                                # families that sit ABOVE the threshold
pin <- NULL
unresolved <- character(0)      # flat, in the two families, but no SS3 value
for (bl in unique(nm)) {
  pos <- which(nm == bl); lv <- levels(mp[[bl]])
  for (j in seq_along(pos)) {
    if (hdiag[pos[j]] >= FLAT) next
    rows <- which(!is.na(mp[[bl]]) & as.character(mp[[bl]]) == lv[j])
    if (bl == "beta_linkage") {
      r <- rows[1]
      if (!identical(as.character(tbl$process[r]), "sel")) next
      prm <- as.character(tbl$param[r])
      if (!prm %in% c("top_logit", "descend_se")) next
      dc <- as.character(tbl$design_col[r])
      ss3 <- .ss3_block_value(dc)
      what <- sprintf("%s %s", dc, prm)
    } else if (bl == "sel_dn6") {
      i <- rows[1]; k <- (i - 1L) %% 6L + 1L; fi <- (i - 1L) %/% 6L + 1L
      if (!SLOT[k] %in% c("top_logit", "descend_se")) next
      stem <- .ss3_sel_stem(fi, k)
      what <- sprintf("%s %s base", fleet_meta$name[fi], SLOT[k])
      ss3 <- if (stem %in% names(.ss3_P$value)) .ss3_P$value[[stem]] else NA_real_
    } else next
    # A coefficient the criterion selected but whose SS3 value will not resolve
    # is a lookup failure in this script, not a property of the model, so it is
    # counted and named rather than skipped silently.
    if (!is.finite(ss3)) { unresolved <- c(unresolved, what); next }
    pin <- rbind(pin, data.frame(block = bl, what = what,
      rows = paste(rows, collapse = ","), ss3 = ss3,
      fitted = as.numeric(base_fit$estimated_params[[bl]])[rows[1]],
      hdiag = hdiag[pos[j]], stringsAsFactors = FALSE))
  }
}
n_unresolved <- length(unresolved)
cat(sprintf("\n[pin] %d coefficients with Hessian diagonal < %g, all top_logit/descend_se\n",
            nrow(pin), FLAT))
print(pin[order(pin$hdiag), c("what", "fitted", "ss3", "hdiag")], row.names = FALSE, digits = 4)
# Account for EVERY flat direction, and distinguish the two reasons one can stay
# free: it belongs to another parameter (expected -- L1 on its bound is one) or
# its SS3 value would not resolve (a lookup that matched 0 or >1 labels), which
# is a defect in this script rather than a property of the model.
flat_all <- sum(hdiag < FLAT)
if (flat_all > nrow(pin) + n_unresolved)
  cat(sprintf("[pin] NOTE %d flat direction(s) are not top_logit/descend_se and stay free\n",
              flat_all - nrow(pin) - n_unresolved))
if (n_unresolved > 0)
  cat(sprintf(paste0("[pin] WARNING %d flat top_logit/descend_se coefficient(s) stayed free ",
                     "because their SS3 value did not resolve: %s\n"),
              n_unresolved, paste(unresolved, collapse = ", ")))

# ---- 3. Pin them, at SS3's values -------------------------------------------
inits_pin <- base_fit$estimated_params
map_pin   <- ss3_map
for (i in seq_len(nrow(pin))) {
  rows <- as.integer(strsplit(pin$rows[i], ",")[[1]])
  bl   <- pin$block[i]
  # A shared Selectivity_index emits one coefficient on several linkage rows, so
  # every row carrying the level must be held -- hold one and the coefficient
  # stays free on the other, with the two rows then free to disagree.
  inits_pin[[bl]][rows] <- pin$ss3[i]
  .v <- as.integer(as.character(map_pin$mapFactor[[bl]])); .v[rows] <- NA_integer_
  map_pin$mapFactor[[bl]] <- droplevels(factor(.v))
  if (!is.null(map_pin$mapList[[bl]])) map_pin$mapList[[bl]][rows] <- NA
}
.nfree <- function(m) sum(vapply(m$mapFactor, function(f)
  length(unique(stats::na.omit(as.character(f)))), integer(1)))
cat(sprintf("[pin] free parameters %d -> %d\n", .nfree(ss3_map), .nfree(map_pin)))

# ---- 4. The pin must not move the curve. Assert it, before refitting --------
a <- .fit(base_fit$estimated_params, ss3_map, 3)
# The cache may come from a different Rceattle build, and everything above was
# read from it -- the Hessian, the fitted values, the parameter count. Evaluating
# its own parameters under THIS build must reproduce its objective, or the
# "+x nats over 23 fewer parameters" below compares two different models.
stopifnot(abs(a$quantities$jnll - base_fit$opt$objective) < 1e-6)
b <- .fit(inits_pin, map_pin, 3)
d_sel <- max(abs(b$quantities$sel_at_age - a$quantities$sel_at_age))
d_ssb <- 100 * max(abs(b$quantities$ssb[1, ] / a$quantities$ssb[1, ] - 1))
d_obj <- b$quantities$jnll - a$quantities$jnll
cat(sprintf("\n[check] holding them costs %+.6g nats; selectivity-at-age moves %.3g; SSB %.4g%%\n",
            d_obj, d_sel, d_ssb))
# abs(), not a one-sided test: the 330-parameter point is a SADDLE (5 negative
# eigenvalues), so holding a coefficient can lower the objective as well as
# raise it, and a mislabelled pin that happens to improve the fit would pass.
if (abs(d_obj) > 0.01 || d_sel > 1e-4 || d_ssb > 0.01)
  stop("Holding these coefficients MOVED the model: they are not uninformed ",
       "after all, and the refit below would be a different model rather than ",
       "the same one with its flat directions removed.", call. = FALSE)

# ---- 5. Refit, and ask for standard errors ----------------------------------
pinned <- .fit(inits_pin, map_pin, 1, getsd = TRUE)
cat(sprintf("\nobjective %.8f (330-par %.8f, %+.6f over %d fewer parameters)\n",
            pinned$opt$objective, base_fit$opt$objective,
            pinned$opt$objective - base_fit$opt$objective,
            length(base_fit$obj$par) - length(pinned$obj$par)))
cat(sprintf("free %d   max|grad| %.3g\n", length(pinned$obj$par), pinned$opt$max_gradient))
print(pinned$convergence)
if (is.null(pinned$sdrep)) {
  cat("\nsdreport STILL failed -- a flat direction survives the pin.\n")
} else {
  s <- summary(pinned$sdrep, "fixed")
  cat(sprintf("\nsdreport returned: pdHess = %s, %d of %d standard errors finite, largest %.4g\n",
              pinned$sdrep$pdHess, sum(is.finite(s[, 2])), nrow(s),
              max(s[is.finite(s[, 2]), 2])))
}
saveRDS(list(pinned = pinned, pin = pin, map_pin = map_pin),
        "Bridging/_pinned_sel_fit.rds")
cat("\nwrote Bridging/_pinned_sel_fit.rds\n")
