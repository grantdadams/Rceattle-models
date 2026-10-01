# Rceattle's PROFILE over the initial recruitment level, on SS3's grid.
#
# Why this and not the two optima: the cold and warm optima differ in all 317
# parameters, so reading their composition difference as a response to the initial
# state is an attribution, not a measurement. SS3's numbers are a profile (pin the
# regime, re-estimate the other 329 from the MLE). This is the matching
# construction -- pin the init linkage coefficient, start from SS3's MLE,
# re-optimise everything else.
#
# est_phase = 0 on the linkage_spec does NOT pin it when `map` is passed
# explicitly: map_g3's mapFactor$beta_linkage re-frees the row, and the fit comes
# back at 1866.0803 with 317 parameters -- the free warm fit, identical at every
# grid point. Pin it in the map instead, on the row the package's own predicate
# identifies, and assert the pin took.
#
#   Rscript rce_regime_profile.R     (needs RCEATTLE_PKG = the integ worktree)
Sys.setenv(RCE_SEL_PARITY = "true", RCE_INIT_LINK = "true")
source("Bridging/ss3_to_ceattle_forward_pass.R")
source("Bridging/g3_map.R")

GRID <- c(-1.2, -0.9, -0.740496, -0.5, -0.2444, 0.0)
ROWS <- c("Index data", "Catch data", "Composition data", "CAAL data",
          "Initial abundance deviates", "Recruitment deviates",
          "M prior", "Linkage-table priors")

# mapFactor$beta_linkage carries one entry per linkage_table ROW (g3_map.R
# indexes it that way), so the init row is found with the package's predicate.
tbl <- mod0$data_list$linkage_table
ir  <- which(Rceattle:::.is_init_linkage_row(tbl))
stopifnot(length(ir) == 1)
cat(sprintf("\ninit linkage is linkage_table row %d of %d (process %s, param %s)\n",
            ir, nrow(tbl), as.character(tbl$process[ir]),
            as.character(tbl$param[ir])))
n_free <- sum(!is.na(map_g3$mapFactor$beta_linkage))
map_pin <- map_g3
f <- as.character(map_pin$mapFactor$beta_linkage)
stopifnot(!is.na(f[ir]))              # it must be free before we pin it
f[ir] <- NA
map_pin$mapFactor$beta_linkage <- factor(f)
cat(sprintf("beta_linkage free: %d -> %d\n", n_free,
            sum(!is.na(map_pin$mapFactor$beta_linkage))))

START <- Sys.getenv("RCE_PROF_START", "warm")
if (identical(START, "cold")) {
  .cold <- readRDS("Bridging/_g3_parity.rds")$cold
  inits <- .cold$estimated_params
  cat("\nprofiling from the COLD optimum's parameters\n")
}

at <- function(v) {
  ini <- inits
  ini$beta_linkage[ir] <- v
  f <- tryCatch(Rceattle::fit_mod(
        data_list = cod, inits = ini, map = map_pin,
        estimateMode = "Hindcast", initMode = INIT_MODE,
        growthFun = growthFun_spec, M1Fun = M1_block, selFun = selFun_spec,
        qFun = qFun_spec, recFun = recFun_spec, random_rec = FALSE, msmMode = 0,
        fit_control = fit_control(phase = FALSE, verbose = 0, newtonsteps = 0,
                                  bias_adjust_obs = FALSE)),
      error = function(e) { cat("FAILED:", conditionMessage(e), "\n"); NULL })
  if (is.null(f)) return(NULL)
  # the pin must have held: 316 parameters, and the realised offset == v
  d  <- f$convergence$checks$max_gradient$data
  off <- f$quantities$recruitment_linkage_offset[1, 4, 1]
  if (abs(off - v) > 1e-8)
    stop(sprintf("asked for %.8g, model carries %.8g: the pin did not hold.",
                 v, off))
  jc <- rowSums(f$quantities$jnll_comp)
  c(regime = v, TOTAL = f$opt$objective, jc[ROWS], maxgrad = d$max_gradient,
    n_par = length(d$par), lnR0 = log(f$quantities$R0[1]),
    SSB1977 = as.numeric(f$quantities$ssb[1, 1]))
}

out <- NULL
for (v in GRID) {
  cat(sprintf("regime = %-10.5g ", v)); flush.console()
  t0 <- Sys.time()
  r <- at(v)
  if (is.null(r)) next
  cat(sprintf("TOTAL %.4f  Comp %.4f  (%d par, |g| %.3g, %.1f min)\n",
              r[["TOTAL"]], r[["Composition data"]], r[["n_par"]],
              r[["maxgrad"]],
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  out <- rbind(out, r)
}
out <- as.data.frame(out)
out$delta <- out$TOTAL - min(out$TOTAL)

cat("\n=== RCEATTLE PROFILE over the initial recruitment level ===\n")
print(format(out, digits = 7), row.names = FALSE)
cat(sprintf("\nminimum at %.4g   (SS3's MLE -0.7405; Rceattle's cold optimum -0.2444)\n",
            out$regime[which.min(out$TOTAL)]))

cat("\n--- Composition slope, -0.7405 -> -0.2444 ---\n")
a <- out$`Composition data`[which.min(abs(out$regime + 0.740496))]
b <- out$`Composition data`[which.min(abs(out$regime + 0.2444))]
cat(sprintf("  Rceattle PROFILE                  : %.4f -> %.4f  = %+.4f\n",
            a, b, b - a))
cat("  Rceattle two optima (NOT a profile): 1353.9813 -> 1348.1667  = -5.8146\n")
cat("  SS3, age 0 zeroed                 : 1334.3300 -> 1337.2900  = +2.9600\n")
cat("  SS3, age 0 selected               : 1365.1700 -> 1368.9500  = +3.7800\n")
cat("\n--- TOTAL delta ---\n")
cat(sprintf("  Rceattle        : %s\n",
            paste(sprintf("%+.2f", out$delta), collapse = "  ")))
cat("  SS3 age0 zeroed : +2.92  +0.34  +0.00  +0.74  +3.05  +6.61\n")
cat("  SS3 age0 select : +2.45  +0.16  +0.00  +1.04  +3.66  +7.49\n")
saveRDS(out, sprintf("Bridging/_rce_regime_profile_%s.rds", START))
cat("\nRCEPROF DONE\n")
