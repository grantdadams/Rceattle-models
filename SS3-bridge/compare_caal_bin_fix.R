# AI cod M24_1_adjusted as written vs with the CAAL length bins corrected.
# Same SS3 3.30.22.1 executable, same control file; only the CAAL Lbin_lo/Lbin_hi
# columns differ (lengths -> population bin numbers, one bin per row).
args <- commandArgs(trailingOnly = TRUE)
suppressMessages(library(r4ss))
a <- SS_output(if (length(args) >= 1) args[1] else "AI cod - Dev/Data/M24_1_adjusted", verbose = FALSE, printstats = FALSE,
               covar = FALSE, forecast = FALSE)
b <- SS_output(if (length(args) >= 2) args[2] else "AI cod - Dev/Data/M24_1_caal_bins_fixed", verbose = FALSE, printstats = FALSE,
               covar = FALSE, forecast = FALSE)

cat("=== the bins SS3 actually used ===\n")
for (nm in c("as written", "corrected")) {
  r <- if (nm == "as written") a else b
  cb <- r$condbase
  lo <- sort(unique(cb$Lbin_lo)); hi <- sort(unique(cb$Lbin_hi))
  cat(sprintf("%-11s condbase Lbin_lo %.1f..%.1f  Lbin_hi %.1f..%.1f  span %s cm\n",
              nm, min(lo), max(lo), min(hi), max(hi),
              paste(unique(cb$Lbin_hi - cb$Lbin_lo), collapse = "/")))
}

cat("\n=== likelihood ===\n")
lk <- merge(data.frame(comp = rownames(a$likelihoods_used), as_written = a$likelihoods_used[, "values"]),
            data.frame(comp = rownames(b$likelihoods_used), corrected  = b$likelihoods_used[, "values"]),
            by = "comp")
lk$diff <- lk$corrected - lk$as_written
print(lk[abs(lk$diff) > 1e-6 | lk$comp == "TOTAL", ], row.names = FALSE, digits = 6)

cat("\n=== growth and key parameters ===\n")
keep <- c("L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1", "VonBert_K_Fem_GP_1",
          "Richards_Fem_GP_1", "CV_young_Fem_GP_1", "CV_old_Fem_GP_1",
          "NatM_uniform_Fem_GP_1", "SR_LN(R0)", "LnQ_base_Srv(2)")
pa <- a$parameters[match(keep, a$parameters$Label), c("Label", "Value")]
pb <- b$parameters[match(keep, b$parameters$Label), c("Label", "Value")]
p <- data.frame(par = keep, as_written = pa$Value, corrected = pb$Value)
p$pct <- round(100 * (p$corrected - p$as_written) / abs(p$as_written), 2)
print(p, row.names = FALSE, digits = 6)

cat("\n=== mean length at age (cm), end year ===\n")
ega <- a$endgrowth[a$endgrowth$Sex == 1, ]; egb <- b$endgrowth[b$endgrowth$Sex == 1, ]
g <- data.frame(age = 0:13,
                as_written = ega$Len_Beg[match(0:13, ega$int_Age)],
                corrected  = egb$Len_Beg[match(0:13, egb$int_Age)])
g$diff_cm <- round(g$corrected - g$as_written, 3)
g$pct <- round(100 * g$diff_cm / g$as_written, 2)
print(g, row.names = FALSE, digits = 5)

cat("\n=== SSB (mt) ===\n")
ta <- a$timeseries; tb <- b$timeseries
yrs <- intersect(ta$Yr, tb$Yr); yrs <- yrs[yrs >= a$startyr & yrs <= a$endyr]
sa <- ta$SpawnBio[match(yrs, ta$Yr)]; sb <- tb$SpawnBio[match(yrs, tb$Yr)]
cat(sprintf("terminal (%d): %.1f -> %.1f  (%+.2f%%)\n", max(yrs),
            sa[length(sa)], sb[length(sb)], 100 * (sb[length(sb)] / sa[length(sa)] - 1)))
cat(sprintf("mean over %d-%d: %+.2f%%   max |change|: %.2f%%\n", min(yrs), max(yrs),
            100 * (mean(sb) / mean(sa) - 1), max(abs(100 * (sb / sa - 1)))))

cat("\n=== management quantities ===\n")
dq <- function(r, lab) r$derived_quants$Value[r$derived_quants$Label == lab]
for (lab in c("SSB_Virgin", "SSB_unfished", "SSB_Btgt", "SSB_SPR", "annF_SPR",
              "annF_Btgt", "Dead_Catch_SPR", "Dead_Catch_Btgt", "OFLCatch_2025",
              "ForeCatch_2025")) {
  va <- dq(a, lab); vb <- dq(b, lab)
  if (length(va) && length(vb) && is.finite(va) && va != 0)
    cat(sprintf("  %-18s %12.4g -> %12.4g  (%+.2f%%)\n", lab, va, vb, 100 * (vb / va - 1)))
}
