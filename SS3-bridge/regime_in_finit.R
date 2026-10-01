# Does SS3's SR_regime, carried in Finit instead of init_dev, account for the
# whole +49.75 recruitment residual on GOA Pacific cod?
#
# Under srr_fun = 0 R_init = R0 with no SPRFinit feedback, and under initMode 4
# Finit enters the initial decay ONCE rather than cumulatively, so exp(-Finit) is
# a constant unpenalised multiplier on ages 1..nages-2. The pin is invertible:
# init_dev shifts by +Finit at every age, and the plus group takes a further
# correction because Finit also enters its geometric-series denominator.
#
# The initial numbers-at-age are therefore IDENTICAL both ways. Every fitted
# component must be unchanged and only the init_dev penalty can move.
#
# Run from the "GOA cod" stock folder:
#   RCEATTLE_PKG=<checkout> Rscript ../SS3-bridge/regime_in_finit.R
Sys.setenv(RCE_INITMODE = "FishedNonEquilibriumScaled")
source("Bridging/ss3_to_ceattle_forward_pass.R")

# SS3's regime shift, on the log scale, read with the same helper the forward
# pass uses (`regime` itself is local to init_from_ss3()).
regime <- gp(parlist$SR_parms, "SR_regime_BLK")
Finit  <- -regime
stopifnot(is.finite(Finit), Finit > 0)
cat(sprintf("\n=== SR_regime = %.4f  ->  Finit = %.4f  (log_Finit = %.4f) ===\n",
            regime, Finit, log(Finit)))

# Re-pin with the level in Finit. mort_sum gains Finit once (initMode 4), so
# every init_dev rises by Finit; the plus group's target changes as well because
# its geometric series divides by (1 - exp(-M_plus - Finit)).
inits2 <- inits
inits2$log_Finit[1] <- log(Finit)
inits2$init_dev[1, ] <- inits$init_dev[1, ] + Finit
kp <- nages - 1
Mp <- as.numeric(M1_at_age[nages])
plus_corr <- log((1 - exp(-Mp - Finit)) / (1 - exp(-Mp)))
inits2$init_dev[1, kp] <- inits2$init_dev[1, kp] + plus_corr
cat(sprintf("plus-group correction = %.4f (M_plus = %.4f)\n", plus_corr, Mp))
cat(sprintf("init_dev mean: %.4f -> %.4f   max|.|: %.4f -> %.4f\n",
            mean(inits$init_dev[1, ]),  mean(inits2$init_dev[1, ]),
            max(abs(inits$init_dev[1, ])), max(abs(inits2$init_dev[1, ]))))

cat("\n--- Forward pass with the level in Finit ---\n")
fp2 <- Rceattle::fit_mod(
  data_list    = cod,
  inits        = inits2,
  map          = ss3_map,
  estimateMode = 3,
  initMode     = "FishedNonEquilibriumScaled",
  growthFun    = growthFun_spec,
  M1Fun        = M1_block,
  selFun       = selFun_spec,
  random_rec   = FALSE,
  msmMode      = 0,
  fit_control  = fit_control(phase = FALSE, verbose = 1, bias_adjust_obs = FALSE)
)

# The invariant: identical initial numbers-at-age.
N1 <- fp$quantities$N_at_age[1, , , 1]
N2 <- fp2$quantities$N_at_age[1, , , 1]
cat(sprintf("\nstyr numbers-at-age, max abs rel diff: %.3g\n",
            max(abs(N2 - N1) / pmax(abs(N1), 1e-300))))

cat(sprintf("\njnll: init_dev carries it %.4f -> Finit carries it %.4f  (change %+.4f)\n",
            fp$quantities$jnll, fp2$quantities$jnll,
            fp2$quantities$jnll - fp$quantities$jnll))

cmp <- data.frame(
  row    = rownames(fp$quantities$jnll_comp),
  in_dev = rowSums(fp$quantities$jnll_comp),
  in_F   = rowSums(fp2$quantities$jnll_comp))
cmp$change <- cmp$in_F - cmp$in_dev
print(cmp[abs(cmp$change) > 1e-6 | cmp$in_dev != 0, ], row.names = FALSE)

saveRDS(list(fp2 = fp2, Finit = Finit, regime = regime, cmp = cmp),
        "Bridging/_regime_in_finit.rds")
cat("\nSaved Bridging/_regime_in_finit.rds\nphase = FALSE\n")
