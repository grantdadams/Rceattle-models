# Rceattle as a pure FORWARD PASS over the regime, to pair against
# ss3_age0_fp_shift.R point for point. Nothing is re-estimated on either side, so
# the Rceattle-minus-SS3 recruitment residual is comparable across the grid: flat
# means the +0.79 at the MLE is a missing additive constant, varying means the two
# codes score the initial state differently.
Sys.setenv(RCE_SEL_PARITY = "true", RCE_INIT_LINK = "true")
source("Bridging/ss3_to_ceattle_forward_pass.R")
GRID <- c(-1.2, -0.9, -0.740496, -0.5, -0.2444, 0.0)
ROWS <- c("Index data", "Catch data", "Composition data", "CAAL data",
          "Initial abundance deviates", "Recruitment deviates",
          "M prior", "Linkage-table priors")
tbl <- mod0$data_list$linkage_table
ir  <- which(Rceattle:::.is_init_linkage_row(tbl))
stopifnot(length(ir) == 1)
out <- NULL
for (v in GRID) {
  ini <- inits
  ini$beta_linkage[ir] <- v
  f <- Rceattle::fit_mod(data_list = cod, inits = ini, map = ss3_map,
        estimateMode = 3, initMode = INIT_MODE, growthFun = growthFun_spec,
        M1Fun = M1_block, selFun = selFun_spec, qFun = qFun_spec,
        recFun = recFun_spec, random_rec = FALSE, msmMode = 0,
        fit_control = fit_control(phase = FALSE, verbose = 0, newtonsteps = 0,
                                  bias_adjust_obs = FALSE))
  off <- f$quantities$recruitment_linkage_offset[1, 4, 1]
  if (abs(off - v) > 1e-8) stop(sprintf("pin failed: %.8g vs %.8g", v, off))
  jc <- rowSums(f$quantities$jnll_comp)
  out <- rbind(out, c(regime = v, TOTAL = f$quantities$jnll, jc[ROWS],
                      SSB1977 = as.numeric(f$quantities$ssb[1, 1])))
  cat(sprintf("regime %-9.5g TOTAL %.4f  Comp %.4f  initdev %.4f  lnkpri %.4f\n",
              v, f$quantities$jnll, jc[["Composition data"]],
              jc[["Initial abundance deviates"]], jc[["Linkage-table priors"]]))
}
out <- as.data.frame(out)
cat("\n=== RCEATTLE FORWARD PASS over the regime (nothing re-estimated) ===\n")
print(format(out, digits = 8), row.names = FALSE)

s3 <- readRDS("../SS3-bridge/_ss3_age0_fp_shift.rds")
cat("\n=== recruitment-term residual across the grid ===\n")
cat("Rceattle (init_dev + rec_dev) - [SS3 Recruitment + InitEQ_Regime] - 53.2984\n")
for (i in seq_len(nrow(out))) {
  j <- which.min(abs(s3$regime - out$regime[i]))
  rv <- out$`Initial abundance deviates`[i] + out$`Recruitment deviates`[i]
  sv <- s3$Recruitment[j] + s3$InitEQ_Regime[j]
  cat(sprintf("  regime %-9.5g Rceattle %9.4f   SS3 %9.4f   residual %+8.4f\n",
              out$regime[i], rv, sv, rv - sv - 53.2984))
}
cat("\n=== Composition: Rceattle - SS3 (age 0 selected) ===\n")
for (i in seq_len(nrow(out))) {
  j <- which.min(abs(s3$regime - out$regime[i]))
  cat(sprintf("  regime %-9.5g Rceattle %10.4f   SS3 %10.4f   diff %+8.4f\n",
              out$regime[i], out$`Composition data`[i], s3$Length_comp[j],
              out$`Composition data`[i] - s3$Length_comp[j]))
}
saveRDS(out, "Bridging/_rce_fp_scan.rds")
cat("\nFPSCAN DONE\n")
