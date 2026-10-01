# Is the cold start's 8.3-nat advantage over the warm start bought by L1?
# Both fits drive length-at-age-0 far below SS3's 1.286 cm (warm 0.030, cold 0.001
# = its lower bound). Refit both with L1 FIXED at SS3's value; if the two
# objectives then agree, L1 was the whole difference and neither fit is "better".
#
# Context already measured: SS3's OWN L1 profile also minimises at 0.001
# (+3.97 nats at its reported MLE of 1.3007), so the run to the bound is a
# property of the age-0-selected model in both codes, not a Rceattle pathology.
source("Bridging/g3_cold_start.R")   # leaves res, cod, map_g3, mod0, fp, cold_inits

SS3_L1 <- 1.285910   # L_at_Amin_Fem_GP_1, cm, SS3 phase 1 (SE 0.488)
K_L1   <- 2L         # log_growth_pars[sp, sex, k], k = 1:4 = K / L1 / Linf / m

cat("\n--- map structure ---\n")
for (n in names(map_g3)) {
  x <- map_g3[[n]]
  cat(sprintf("  %-12s class %-16s len %d\n", n,
              paste(class(x), collapse = "/"), length(x)))
}
gl <- map_g3$mapList$log_growth_pars
gf <- map_g3$mapFactor$log_growth_pars
cat(sprintf("  mapList$log_growth_pars dim %s\n", paste(dim(gl), collapse = "x")))
cat("  mapFactor$log_growth_pars: ", paste(as.character(gf), collapse = " "), "\n")

# the factor is flat, so resolve [1, 1, K_L1] to its flat position by hand
d  <- dim(gl)
fi <- 1L + (1L - 1L) * d[1] + (K_L1 - 1L) * d[1] * d[2]
cat(sprintf("  L1 is mapList[1,1,%d] = flat index %d (value %s)\n",
            K_L1, fi, as.character(gf[fi])))

map_fix <- map_g3
map_fix$mapList$log_growth_pars[1, 1, K_L1] <- NA
mf <- map_fix$mapFactor$log_growth_pars
mf[fi] <- NA
# a level with no remaining members would still claim a bounds entry
map_fix$mapFactor$log_growth_pars <- droplevels(mf)
cat("  after:  ", paste(as.character(map_fix$mapFactor$log_growth_pars),
                        collapse = " "), "\n")

pin_L1 <- function(p) { p$log_growth_pars[1, 1, K_L1] <- log(SS3_L1); p }

fit_with <- function(start, mp, label, phase) {
  cat(sprintf("\n================ %s ================\n", label))
  t0 <- Sys.time()
  f <- tryCatch(Rceattle::fit_mod(
        data_list = cod, inits = start, map = mp,
        estimateMode = "Hindcast", initMode = INIT_MODE,
        growthFun = growthFun_spec, M1Fun = M1_block, selFun = selFun_spec,
        qFun = qFun_spec, recFun = recFun_spec, random_rec = FALSE, msmMode = 0,
        fit_control = fit_control(phase = phase, verbose = 1, newtonsteps = 0,
                                  bias_adjust_obs = FALSE)),
      error = function(e) { cat("FAILED:", conditionMessage(e), "\n"); NULL })
  cat(sprintf("%s took %.1f min\n", label,
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  f
}

res2 <- list(
  warm_L1fix = fit_with(pin_L1(inits),      map_fix, "warm, L1 fixed", FALSE),
  cold_L1fix = fit_with(pin_L1(cold_inits), map_fix, "cold, L1 fixed", TRUE)
)

cat("\n==================================================\n")
ss3_tot <- ss3_rep$likelihoods_used["TOTAL", "values"]
allf <- c(res, res2)
ROWS <- c("Index data", "Catch data", "Composition data", "CAAL data",
          "Initial abundance deviates", "Recruitment deviates",
          "M prior", "Linkage-table priors")
for (nm in names(allf)) {
  f <- allf[[nm]]
  if (is.null(f)) { cat(sprintf("%-11s FAILED\n", nm)); next }
  d  <- f$convergence$checks$max_gradient$data
  w  <- which(names(d$par) == "log_growth_pars")
  l1 <- if (length(w) >= 2) exp(d$par[w[2]]) else SS3_L1
  cat(sprintf("%-11s objective %9.4f   max|grad| %8.3g   L1 %8.5f   log(R0) %7.4f   %d par\n",
              nm, f$opt$objective, d$max_gradient, l1,
              log(f$quantities$R0[1]), length(d$par)))
}
cat(sprintf("%-11s objective %9.4f   (SS3 TOTAL %.4f)\n", "fwd@SS3MLE",
            fp$quantities$jnll, ss3_tot))

cat("\n--- jnll_comp ---\n")
tab <- sapply(allf, function(f) if (is.null(f)) rep(NA_real_, length(ROWS)) else
              rowSums(f$quantities$jnll_comp)[ROWS])
rownames(tab) <- ROWS
print(round(tab, 4))

cat("\n--- SSB vs SS3 ---\n")
ts <- ss3_rep$timeseries
ts <- ts[ts$Era == "TIME", ]
ss3_ssb <- tapply(ts$SpawnBio, ts$Yr, sum)
yrs <- cod$styr:cod$endyr
ky  <- intersect(yrs, as.integer(names(ss3_ssb)))
a   <- as.numeric(ss3_ssb[as.character(ky)])
for (nm in names(allf)) {
  f <- allf[[nm]]; if (is.null(f)) next
  b <- as.numeric(f$quantities$ssb[1, match(ky, yrs)])
  cat(sprintf("%-11s cor %.5f   mean ratio %.4f   max |rel err| %.3f (%d)   %d ratio %.4f\n",
              nm, stats::cor(a, b), mean(b / a), max(abs(b / a - 1)),
              ky[which.max(abs(b / a - 1))], max(ky),
              b[length(b)] / a[length(a)]))
}
saveRDS(allf, "Bridging/_g3_parity_l1.rds")
cat("\nL1 DONE\n")
