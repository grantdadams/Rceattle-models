# =============================================================================
# GOA Pacific cod: likelihood profiles over the two parameters where Rceattle's
# G3 optimum disagrees with SS3's -- length at the youngest age (L1) and natural
# mortality. Each point re-optimises everything else, so the curve is a profile
# and not a slice; a flat arm means the data do not identify the parameter, and
# a minimum away from SS3's value means the two models genuinely disagree.
#
#   Rscript Bridging/profile_g3_goa.R [L1|M|both]
# =============================================================================
which_par <- if (length(commandArgs(trailingOnly = TRUE)))
  commandArgs(trailingOnly = TRUE)[1] else "both"

mode <- "fixsel"
source("Bridging/ss3_to_ceattle_forward_pass.R")
source("Bridging/g3_map.R")

# Start every point from SS3's MLE, which is in bounds by construction. A fit
# saved with newtonsteps > 0 is not usable as a warm start: TMBhelper's Newton
# steps are unconstrained, so it can come back with a parameter outside its
# bound, which fit_mod refuses on entry.
warm <- inits

# jnll_comp rows to record at every profile point, so a penalty can be
# attributed to a component instead of being reported as a bare total.
ROWS <- c("Catch data", "Index data", "Composition data", "CAAL data",
          "Initial abundance deviates", "Recruitment deviates",
          "M prior", "Linkage-table priors")

# Fix one slot of one parameter at `value` (natural scale) and re-optimise the
# rest. fit_mod() droplevels the map itself before it builds bounds and selects
# them by position, so the droplevels() here is only tidiness.
profile_at <- function(par_name, slot, value) {
  ini <- warm
  ini[[par_name]][slot] <- log(value)
  mp  <- map_g3
  f   <- as.character(mp$mapFactor[[par_name]])
  f[slot] <- NA
  mp$mapFactor[[par_name]] <- droplevels(factor(f))
  fit <- tryCatch(
    Rceattle::fit_mod(
      data_list = cod, inits = ini, map = mp, estimateMode = "Hindcast",
      initMode = INIT_MODE, growthFun = growthFun_spec, M1Fun = M1_block,
      selFun = selFun_spec, random_rec = FALSE, msmMode = 0,
      fit_control = fit_control(phase = FALSE, verbose = 0, newtonsteps = 0,
                                bias_adjust_obs = FALSE)),
    error = function(e) { cat("  FAILED:", conditionMessage(e), "\n"); NULL })
  if (is.null(fit)) return(NULL)
  jc <- fit$quantities$jnll_comp
  g  <- try(max(abs(fit$obj$gr(fit$obj$env$last.par.best[fit$obj$env$lfixed()]))),
            silent = TRUE)
  c(obj  = fit$opt$objective,
    grad = if (inherits(g, "try-error")) NA_real_ else g,
    M    = exp(as.numeric(fit$estimated_params$log_M1)[1]),
    logR0 = as.numeric(fit$estimated_params$rec_pars[1, 1]),
    setNames(rowSums(jc[ROWS, , drop = FALSE]), ROWS))
}

run_profile <- function(par_name, slot, grid, lab, ss3_value) {
  cat(sprintf("\n=== profile over %s  (SS3's MLE %.4f) ===\n", lab, ss3_value))
  cat(sprintf("%10s %14s %10s %10s %10s\n", lab, "objective", "grad", "M", "log(R0)"))
  out <- NULL
  for (k in seq_along(grid)) {
    r <- profile_at(par_name, slot, grid[k])
    if (is.null(r)) { cat(sprintf("%10.4g   did not fit\n", grid[k])); next }
    out <- rbind(out, c(value = grid[k], r))
    cat(sprintf("%10.4g %14.4f %10.3g %10.4f %10.4f\n", grid[k], r["obj"],
                r["grad"], r["M"], r["logR0"]))
  }
  out <- as.data.frame(out)
  # A point that stopped early inflates its own objective and the penalty read
  # off it, so say which points are not converged rather than leaving it unsaid.
  bad <- which(!is.finite(out$grad) | out$grad > 0.01)
  if (length(bad))
    cat(sprintf("NOT CONVERGED at %s = %s (max abs gradient %s); treat those ",
                lab, paste(signif(out$value[bad], 5), collapse = ", "),
                paste(signif(out$grad[bad], 3), collapse = ", ")),
        "points as upper bounds on the objective, not as profile values.\n")
  cat("\ncomponent at each point, as a change from the best point on the grid:\n")
  b <- which.min(out$obj)
  d <- out[, ROWS, drop = FALSE]
  d <- sweep(d, 2, as.numeric(d[b, ]), "-")
  print(cbind(value = out$value, round(d, 4)), row.names = FALSE)
  best <- out$value[which.min(out$obj)]
  cat(sprintf("minimum on the grid at %s = %.4g ; SS3 is at %.4g\n",
              lab, best, ss3_value))
  span <- diff(range(out$obj, na.rm = TRUE))
  cat(sprintf("objective spans %.3f nats across the grid\n", span))
  out
}

res <- list()
if (which_par %in% c("L1", "both"))
  res$L1 <- run_profile("log_growth_pars", 2,
                        c(0.001, 0.1, 0.5, 1.3007, 3, 6.3923, 12), "L1", 1.3007)
if (which_par %in% c("M", "both"))
  res$M  <- run_profile("log_M1", 1,
                        c(0.25, 0.3045, 0.35, 0.40, 0.4678, 0.55), "M", 0.4678)
saveRDS(res, "Bridging/_g3_profiles.rds")
cat("\nSaved Bridging/_g3_profiles.rds\n")
