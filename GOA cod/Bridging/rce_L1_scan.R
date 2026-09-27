# Rceattle's likelihood over L1 (length at the youngest growth age) with every
# other parameter held at SS3's MLE, component by component.
#
# This is the SLICE that matches SS3-bridge/ss3_L1_scan.R point for point.
# Rceattle's G3 PROFILE over L1 falls monotonically to the lower bound, while
# SS3's slice has a clean minimum at its own MLE (+38.5 nats at L1 = 0.001), and
# a profile and a slice cannot be compared: the profile re-optimises 224 other
# parameters, the slice none. Comparing slice to slice says whether the two
# likelihoods agree about L1 at all, before any optimiser is involved.
#
#   Rscript Bridging/rce_L1_scan.R
source("Bridging/ss3_to_ceattle_forward_pass.R")

L1_GRID <- c(0.001, 0.1, 0.5, 1.3007, 3, 6.3923)

# jnll_comp rows to report, by the display names rename_output() gives them.
# Paired with SS3's components: Catch data <-> Catch, Index data <-> Survey,
# Composition data <-> Length_comp, CAAL data <-> Age_comp, the two deviate rows
# together <-> Recruitment, the two prior rows together <-> Parm_priors.
ROWS <- c("Catch data", "Index data", "Composition data", "CAAL data",
          "Initial abundance deviates", "Recruitment deviates",
          "M prior", "Linkage-table priors")

slice_at <- function(v) {
  ini <- inits
  ini$log_growth_pars[1, 1, 2] <- log(v)
  f <- Rceattle::fit_mod(
    data_list = cod, inits = ini, map = ss3_map, estimateMode = 3,
    initMode = INIT_MODE, growthFun = growthFun_spec, M1Fun = M1_block,
    selFun = selFun_spec, random_rec = FALSE, msmMode = 0,
    fit_control = fit_control(phase = FALSE, verbose = 0,
                              bias_adjust_obs = FALSE))
  jc <- f$quantities$jnll_comp
  got <- intersect(ROWS, rownames(jc))
  if (length(got) != length(ROWS))
    stop("jnll_comp is missing these rows: ",
         paste(setdiff(ROWS, rownames(jc)), collapse = ", "))
  c(jnll = f$quantities$jnll, setNames(rowSums(jc[got, , drop = FALSE]), got))
}

out <- NULL
for (v in L1_GRID) {
  cat(sprintf("L1 = %-8.4g ... ", v)); flush.console()
  r <- slice_at(v)
  cat(sprintf("jnll %.4f\n", r["jnll"]))
  out <- rbind(out, c(L1 = v, r))
}
out <- as.data.frame(out)
out$delta_vs_MLE <- out$jnll - out$jnll[which.min(abs(out$L1 - 1.3007))]

cat("\n=== Rceattle's jnll over L1, everything else at SS3's MLE ===\n")
print(format(out, digits = 6), row.names = FALSE)
cat(sprintf("\nBest on this grid: L1 = %.4g. SS3's MLE is 1.3007.\n",
            out$L1[which.min(out$jnll)]))
saveRDS(out, "Bridging/_rce_L1_scan.rds")
cat("Saved Bridging/_rce_L1_scan.rds\n")
