# =============================================================================
# GOA Pacific cod, G3: let Rceattle find its own optimum and compare to SS3's
#
# Run from the stock folder:
#   Rscript Bridging/run_g3_goa.R [fixsel|freesel]
#
# fixsel (default) holds selectivity at SS3's values -- the base pattern-24
# parameters and every per-year linkage offset -- and estimates the rest. That
# asks one question cleanly: given SS3's selectivity, does Rceattle land on
# SS3's growth, M, recruitment and F? It is also the only version comparable to
# SS3 on parameter count, because Rceattle's selectivity offsets carry no
# penalty while SS3 penalises its DEVmults (Parm_devs = 6.49 at its own MLE), so
# freeing 867 of them fits a looser model than SS3 ever did.
#
# freesel frees them, which is NOT an equal-footing comparison; it is here to
# show how much the extra freedom buys.
# =============================================================================
mode <- if (length(commandArgs(trailingOnly = TRUE))) commandArgs(trailingOnly = TRUE)[1] else "fixsel"
source("Bridging/ss3_to_ceattle_forward_pass.R")

source("Bridging/g3_map.R")

# TMBhelper's Newton steps are UNCONSTRAINED: they run after nlminb and can
# walk a parameter past a bound nlminb respected. Set RCE_G3_NEWTON=0 to see
# the bounded optimum on its own.
NEWTON <- as.integer(Sys.getenv("RCE_G3_NEWTON", unset = "3"))
cat(sprintf("\n--- G3 warm start from SS3's MLE (newtonsteps = %d) ---\n", NEWTON))
t0 <- Sys.time()
fit <- tryCatch(
  Rceattle::fit_mod(
    data_list    = cod,
    inits        = inits,
    map          = map_g3,
    estimateMode = "Hindcast",
    initMode     = INIT_MODE,
    growthFun    = growthFun_spec,
    M1Fun        = M1_block,
    selFun       = selFun_spec,
    random_rec   = FALSE,
    msmMode      = 0,
    fit_control  = fit_control(phase = FALSE, verbose = 1,
                               newtonsteps = NEWTON,
                               bias_adjust_obs = FALSE)),
  error = function(e) { cat("FAILED:", conditionMessage(e), "\n"); NULL })
cat("took", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")

if (!is.null(fit)) {
  ss3_tot <- ss3_rep$likelihoods_used["TOTAL", "values"]
  cat(sprintf("\nobjective: forward pass %.4f -> fitted %.4f   (SS3 %.4f)\n",
              fp$quantities$jnll, fit$opt$objective, ss3_tot))
  g <- try(max(abs(fit$obj$gr(fit$obj$env$last.par.best[fit$obj$env$lfixed()]))), silent = TRUE)
  if (!inherits(g, "try-error")) cat(sprintf("max |gradient|: %.3g\n", g))
  cat(sprintf("free parameters: %d   (SS3 estimated 330)\n", length(fit$opt$par)))

  yrs <- cod$styr:cod$endyr
  ts  <- ss3_rep$timeseries; ts <- ts[match(yrs, ts$Yr), ]
  ssb <- as.numeric(fit$quantities$ssb[1, seq_along(yrs)])
  cat(sprintf("\nSSB vs SS3: max rel diff %.3e   terminal %.0f vs %.0f (%+.2f%%)\n",
              max(abs(ssb / ts$SpawnBio - 1)), tail(ssb, 1), tail(ts$SpawnBio, 1),
              100 * (tail(ssb, 1) / tail(ts$SpawnBio, 1) - 1)))
  gp <- function(l) as.numeric(ss3_rep$parameters[l, "Value"])
  cat("\nparameter        SS3        Rceattle\n")
  cat(sprintf("  M          %8.4f   %8.4f\n", gp("NatM_uniform_Fem_GP_1"),
              exp(as.numeric(fit$estimated_params$log_M1)[1])))
  cat(sprintf("  K          %8.4f   %8.4f\n", gp("VonBert_K_Fem_GP_1"),
              exp(as.numeric(fit$estimated_params$log_growth_pars[1, 1, 1]))))
  cat(sprintf("  Linf       %8.4f   %8.4f\n", gp("L_at_Amax_Fem_GP_1"),
              exp(as.numeric(fit$estimated_params$log_growth_pars[1, 1, 3]))))
  cat(sprintf("  log(R0)    %8.4f   %8.4f\n", gp("SR_LN(R0)"),
              as.numeric(fit$estimated_params$rec_pars[1, 1])))
  saveRDS(fit, sprintf("Bridging/_g3_%s.rds", mode))
  cat(sprintf("\nSaved Bridging/_g3_%s.rds\n", mode))
}
