# G3: does Rceattle's own optimum coincide with SS3's?
#
# Two starts, because they answer different questions:
#   warm -- start AT SS3's MLE and let Rceattle converge. How far it moves is
#           the distance between the two optima, with no optimiser noise.
#   cold -- start from the converter's own inits. That is the real gate, and it
#           also tests whether the surface leads anywhere else.
#
# From the Rceattle-models folder:
#   Rscript SS3-bridge/run_g3.R "AI cod - Dev" [warm|cold|both]
args  <- commandArgs(trailingOnly = TRUE)
stock <- if (length(args)) args[1] else stop('give the stock folder, e.g. "AI cod - Dev"')
which <- if (length(args) > 1) args[2] else "warm"
setwd(stock)
source("Bridging/ss3_to_ceattle_forward_pass.R")   # builds fp, cod, inits, ss3_map, ss3_rep
source("../SS3-bridge/parity_check.R")

# A cold start has to travel; the defaults (rel_tol = 1, newtonsteps = 0) stop
# it well short -- 380 iterations at a gradient of 67.9 on AI cod. Phasing and
# a few Newton steps are the designed path for that.
fit_from <- function(start, label, phase = FALSE, newtonsteps = 0) {
  cat("\n=== G3", label, "start ===\n")
  t0 <- Sys.time()
  f <- tryCatch(
    Rceattle::fit_mod(
      data_list    = cod,
      inits        = start,
      map          = ss3_map,
      estimateMode = "Hindcast",
      initMode     = "FishedNonEquilibriumScaled",
      growthFun    = growthFun_spec,
      M1Fun        = M1_block,
      random_rec   = FALSE,
      msmMode      = 0,
      fit_control  = fit_control(phase = phase, verbose = 1,
                                 newtonsteps = newtonsteps,
                                 bias_adjust_obs = FALSE)),
    error = function(e) { cat("FAILED:", conditionMessage(e), "\n"); NULL })
  cat(label, "took", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
  f
}

res <- list()
if (which %in% c("warm", "both"))
  res$warm <- fit_from(inits, "warm", newtonsteps = 3)
if (which %in% c("cold", "both"))
  res$cold <- fit_from(NULL, "cold", phase = TRUE, newtonsteps = 3)

for (nm in names(res)) {
  f <- res[[nm]]
  if (is.null(f)) next
  cat("\n=== G3", nm, "vs SS3 ===\n")
  print(parity_g3(f, fp, ss3_rep), row.names = FALSE)
  cat(sprintf("objective: %s %.4f   forward pass at SS3 MLE %.4f\n",
              nm, f$opt$objective, as.numeric(fp$obj$fn(fp$obj$par))))
  g <- try(max(abs(f$obj$gr(f$obj$env$last.par.best[f$obj$env$lfixed()]))), silent = TRUE)
  if (!inherits(g, "try-error")) cat(sprintf("max |gradient| at the %s optimum: %.3g\n", nm, g))
}
saveRDS(res, "Bridging/_g3_result.rds")
cat("\nSaved Bridging/_g3_result.rds\n")
