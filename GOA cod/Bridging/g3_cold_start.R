# G3 cold start with SS3's own 330 parameters. Warm first (start AT SS3's MLE and
# see how far Rceattle moves -- the distance between the two optima, free of
# optimiser noise), then cold (the real gate).
Sys.setenv(RCE_SEL_PARITY = "true", RCE_INIT_LINK = "true")
mode <- "freesel"
source("Bridging/ss3_to_ceattle_forward_pass.R")
source("Bridging/g3_map.R")
cat(sprintf("\nforward pass at SS3's MLE: %.4f\n", fp$quantities$jnll))

fit_from <- function(start, label, phase, newton) {
  cat(sprintf("\n================ G3 %s ================\n", label))
  t0 <- Sys.time()
  f <- tryCatch(Rceattle::fit_mod(
        data_list = cod, inits = start, map = map_g3,
        estimateMode = "Hindcast", initMode = INIT_MODE,
        growthFun = growthFun_spec, M1Fun = M1_block, selFun = selFun_spec,
        qFun = qFun_spec, recFun = recFun_spec, random_rec = FALSE, msmMode = 0,
        fit_control = fit_control(phase = phase, verbose = 1,
                                  newtonsteps = newton, bias_adjust_obs = FALSE)),
      error = function(e) { cat("FAILED:", conditionMessage(e), "\n"); NULL })
  cat(sprintf("%s took %.1f min\n", label,
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  f
}

# newtonsteps MUST default to 0. TMBhelper::fit_tmb refines with solve(H, g) after
# nlminb, which throws on a near-singular H -- and GOA cod has 22 parameters SS3
# itself cannot identify, so H is near-singular at the optimum. ADMB never
# inverts H while optimising, which is why SS3 converges on the same model.
NEWT <- as.integer(Sys.getenv("RCE_NEWTON", "0"))
res <- list(warm = fit_from(inits, "warm", FALSE, NEWT))
# A genuine cold start, but not an INCOHERENT one. build_params() defaults P5/P6
# (start_logit/end_logit) to -999, SS3's "this end has no floor" sentinel, and
# that is a STRUCTURAL switch rather than a value: at -999 the formula drops the
# floor, so the slot has no gradient and the optimiser cannot move it. ss3_fix_map
# frees those slots because SS3 estimates them, so they end up free but inert --
# which is why the previous cold start left four of them sitting at -999 and
# landed 30.6 nats above the warm optimum.
# Give every FREED P5/P6 slot a finite start. -5 on the logit is a floor of
# 0.0067, small but live. No SS3 values are used, so this stays a cold start.
# mod0$estimated_params is build_params() run through fit_mod, so it carries the
# linkage-table parameters (beta_linkage et al) that build_params(cod) alone cannot
# know about -- and it is pre-injection, so it holds no SS3 values.
cold_inits <- mod0$estimated_params
.g <- as.character(map_g3$mapFactor$sel_dn6)
.n <- 0L
# Patch per Selectivity_index GROUP, not per slot. -999 switches the formula per
# fleet, and fleets sharing a Selectivity_index estimate ONE block, so leaving one
# member on the sentinel while another is finite asks that block for two different
# curves -- which data_check() refuses, naming Srv / Srv_ae1.
for (src in unique(cod$fleet_control$Selectivity_index)) {
  grp <- which(cod$fleet_control$Selectivity_index == src)
  if (!length(grp)) next
  for (k in c(5L, 6L)) {
    slots <- (grp - 1L) * 6L + k
    slots <- slots[slots <= length(.g) & slots <= prod(dim(cold_inits$sel_dn6)[1:2])]
    if (!length(slots)) next
    if (!any(!is.na(.g[slots]))) next          # nothing in this group is estimated
    for (sl in slots) {
      fl <- ((sl - 1L) %/% 6L) + 1L
      if (fl > dim(cold_inits$sel_dn6)[2]) next
      if (!is.finite(cold_inits$sel_dn6[k, fl, 1]) ||
          cold_inits$sel_dn6[k, fl, 1] <= -900) {
        cold_inits$sel_dn6[k, fl, 1] <- -5; .n <- .n + 1L
      }
    }
  }
}
cat(sprintf("\n[cold init] %d start_logit/end_logit slots moved off the -999 sentinel (group-consistent)\n", .n))
res$cold <- fit_from(cold_inits, "cold", TRUE, NEWT)

ss3_tot <- ss3_rep$likelihoods_used["TOTAL", "values"]
for (nm in names(res)) {
  f <- res[[nm]]; if (is.null(f)) next
  cat(sprintf("\n--- %s ---\nobjective %.4f (SS3 TOTAL %.4f)\n", nm, f$opt$objective, ss3_tot))
  g <- try(max(abs(f$obj$gr(f$obj$env$last.par.best[f$obj$env$lfixed()]))), silent = TRUE)
  if (!inherits(g, "try-error")) cat(sprintf("max |gradient| %.3g\n", g))
  cat(sprintf("log(R0) %.4f   (SS3 %.4f)\n", f$quantities$rec_pars[1, 1],
              log(.ss3_P$value[["SR_LN(R0)"]] * 0 + exp(.ss3_P$value[["SR_LN(R0)"]]))))
  ir <- match("Initial abundance deviates", rownames(f$quantities$jnll_comp))
  cat(sprintf("init_dev penalty %.4f\n", sum(f$quantities$jnll_comp[ir, ])))
}
saveRDS(res, "Bridging/_g3_parity.rds")
cat("\nCOLD DONE\n")
