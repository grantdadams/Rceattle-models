# Which likelihood component carries the gradient on log_growth_pars?
# jnll_comp is REPORTed, so perturb each growth parameter and difference every
# row. That attributes the gradient rather than guessing at it.
# Run from the stock folder, e.g. "AI cod - Dev".
RCE <- Sys.getenv("RCEATTLE_PKG", unset = "../../Rceattle")
suppressMessages(pkgload::load_all(RCE, compile = FALSE, quiet = TRUE))
res <- readRDS("Bridging/_fp_result.rds")
fp <- res$fp
obj <- fp$obj
par <- obj$env$last.par.best[obj$env$lfixed()]
if (!length(par)) par <- obj$par
g <- as.numeric(obj$gr(par)); names(g) <- names(par)
idx <- which(names(par) == "log_growth_pars")
cat("log_growth_pars gradients:", paste(signif(g[idx], 4), collapse = "  "), "\n\n")

rows <- rownames(fp$quantities$jnll_comp)
h <- 1e-5
out <- matrix(NA_real_, length(rows), length(idx),
              dimnames = list(rows, paste0("gp", seq_along(idx))))
for (j in seq_along(idx)) {
  pp <- par; pp[idx[j]] <- pp[idx[j]] + h
  pm <- par; pm[idx[j]] <- pm[idx[j]] - h
  rp <- rowSums(obj$report(pp)$jnll_comp)
  rm <- rowSums(obj$report(pm)$jnll_comp)
  out[, j] <- (rp - rm) / (2 * h)
}
out <- out[apply(abs(out), 1, max) > 1e-6, , drop = FALSE]
cat("=== d(component) / d(log_growth_pars), by component ===\n")
print(round(out, 2))
cat("\ncolumn sums (should match the gradients above):\n")
print(round(colSums(out), 2))
