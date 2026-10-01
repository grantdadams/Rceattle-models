# At SS3's MLE: do the likelihood components agree up to additive constants, and
# is Rceattle's gradient zero there?
#
# The gradient is the sharp test. SS3's gradient at its own MLE is ~0 by
# definition, so if the two likelihoods differ only by constants, Rceattle's
# gradient at the SAME parameter vector must also be ~0. Every component that is
# not names a parameter the two codes disagree about.
#
#   Rscript fp_parity.R      (needs RCEATTLE_PKG = the integ worktree)
Sys.setenv(RCE_SEL_PARITY = "true", RCE_INIT_LINK = "true")
source("Bridging/ss3_to_ceattle_forward_pass.R")
source("Bridging/g3_map.R")

cat(sprintf("\n================ TOTALS ================\n"))
ss3_L <- ss3_rep$likelihoods_used
cat(sprintf("Rceattle forward pass at SS3's MLE : %12.4f\n", fp$quantities$jnll))
cat(sprintf("SS3 TOTAL                          : %12.4f\n",
            ss3_L["TOTAL", "values"]))

cat("\n================ COMPONENTS ================\n")
jc <- rowSums(fp$quantities$jnll_comp)
jc <- jc[jc != 0]
cat("-- Rceattle jnll_comp (nonzero rows) --\n")
print(round(jc, 5))
cat("\n-- SS3 likelihoods_used --\n")
print(round(ss3_L[, "values", drop = FALSE], 5))

# Mapping used by Bridging/rce_L1_scan.R: Catch<->Catch, Index<->Survey,
# Composition<->Length_comp, CAAL<->Age_comp, the two deviate rows together
# <->Recruitment, the two prior rows together <->Parm_priors.
PAIRS <- list(
  Catch       = "Catch data",
  Survey      = "Index data",
  Length_comp = "Composition data",
  Age_comp    = "CAAL data",
  Recruitment = c("Initial abundance deviates", "Recruitment deviates"),
  Parm_priors = c("M prior", "Linkage-table priors")
)
cat("\n-- paired, with the implied additive constant --\n")
cat(sprintf("%-14s %12s %12s %12s\n", "component", "SS3", "Rceattle", "R - SS3"))
tot_r <- tot_s <- 0
for (s in names(PAIRS)) {
  if (!s %in% rownames(ss3_L)) next
  a <- as.numeric(ss3_L[s, "values"])
  rr <- PAIRS[[s]]
  rr <- rr[rr %in% names(jc)]
  b <- sum(jc[rr])
  cat(sprintf("%-14s %12.4f %12.4f %12.4f\n", s, a, b, b - a))
  tot_r <- tot_r + b; tot_s <- tot_s + a
}
cat(sprintf("%-14s %12.4f %12.4f %12.4f\n", "(paired sum)", tot_s, tot_r,
            tot_r - tot_s))
cat(sprintf("\nSS3 rows NOT paired: %s\n",
            paste(sprintf("%s %.4f", setdiff(rownames(ss3_L), names(PAIRS)),
                          ss3_L[setdiff(rownames(ss3_L), names(PAIRS)),
                                "values"]), collapse = "; ")))
cat(sprintf("Rceattle rows NOT paired: %s\n",
            paste(sprintf("%s %.4f", setdiff(names(jc), unlist(PAIRS)),
                          jc[setdiff(names(jc), unlist(PAIRS))]),
                  collapse = "; ")))

cat("\n================ GRADIENT AT SS3's MLE ================\n")
g <- as.numeric(fp$obj$gr(fp$obj$par))
nm <- names(fp$obj$par)
cat(sprintf("n fixed effects %d   max |gradient| %.6g   sum |gradient| %.6g\n",
            length(g), max(abs(g)), sum(abs(g))))

cat("\n-- by parameter block --\n")
bl <- split(seq_along(g), nm)
tb <- do.call(rbind, lapply(names(bl), function(b) {
  i <- bl[[b]]
  data.frame(block = b, n = length(i), max_abs = max(abs(g[i])),
             sum_abs = sum(abs(g[i])))
}))
tb <- tb[order(-tb$max_abs), ]
print(data.frame(block = tb$block, n = tb$n,
                 max_abs = signif(tb$max_abs, 4),
                 sum_abs = signif(tb$sum_abs, 4)), row.names = FALSE)

cat("\n-- 25 largest individual gradients --\n")
ix <- fp$convergence$checks$max_gradient$data$index
lab <- function(i) {
  if (is.null(ix)) return(nm[i])
  j <- match(i, ix$par_index)
  if (is.na(j)) nm[i] else sprintf("%s: %s", nm[i], as.character(ix$label[j]))
}
o <- order(abs(g), decreasing = TRUE)[seq_len(min(25, length(g)))]
for (i in o) cat(sprintf("  %9.4f   %-56s value %10.4f\n", g[i], lab(i),
                         fp$obj$par[i]))

saveRDS(list(jnll_comp = fp$quantities$jnll_comp, gradient = g, names = nm,
             par = as.numeric(fp$obj$par), index = ix, ss3 = ss3_L),
        "Bridging/_fp_parity.rds")
cat("\nFP DONE\n")
