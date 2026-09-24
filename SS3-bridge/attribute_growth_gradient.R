# Which likelihood component carries the gradient on log_growth_pars?
# Sources the forward pass so the TMB pointer is live (a saved obj cannot be
# re-taped in a fresh session). Run from Rceattle-models.
args <- commandArgs(trailingOnly = TRUE)
setwd(if (length(args)) args[1] else "AI cod - Dev")
source("Bridging/ss3_to_ceattle_forward_pass.R")

obj <- fp$obj
par <- obj$env$last.par.best[obj$env$lfixed()]
if (!length(par)) par <- obj$par
g <- as.numeric(obj$gr(par)); names(g) <- names(par)
idx <- which(names(par) == "log_growth_pars")
cat("\n=== log_growth_pars gradients ===\n")
print(signif(g[idx], 4))
cat("\nvalues (log scale):", paste(signif(par[idx], 6), collapse = "  "), "\n")
cat("exp(values)        :", paste(signif(exp(par[idx]), 6), collapse = "  "), "\n")

rows <- rownames(fp$quantities$jnll_comp)
h <- 1e-5
out <- matrix(NA_real_, length(rows), length(idx),
              dimnames = list(rows, paste0("gp", seq_along(idx))))
for (j in seq_along(idx)) {
  pp <- par; pp[idx[j]] <- pp[idx[j]] + h
  pm <- par; pm[idx[j]] <- pm[idx[j]] - h
  out[, j] <- (rowSums(obj$report(pp)$jnll_comp) -
               rowSums(obj$report(pm)$jnll_comp)) / (2 * h)
}
out <- out[apply(abs(out), 1, max) > 1e-6, , drop = FALSE]
cat("\n=== d(component) / d(log_growth_pars) ===\n")
print(round(out, 3))
cat("\ncolumn sums (should match the gradients above):\n")
print(round(colSums(out), 3))
