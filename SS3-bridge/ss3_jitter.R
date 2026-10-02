# Does SS3 have more than one optimum on GOA Pacific cod?
#
# Rceattle finds two, 8.3 nats apart, separated in the selectivity parameters --
# which is where SS3 itself reports standard errors up to 507. If the second basin
# is a property of the MODEL rather than of Rceattle's rendering of it, SS3 should
# find it too from a perturbed start. Jitter is the right tool: it needs no
# parameter mapping between the two codes, and it is the diagnostic an assessment
# author would run.
#
# SS3's own jitter (starter.ss #_jitter_fraction) perturbs every estimated
# parameter on its (LO, HI) range from the ss3.par start, then re-estimates all 330
# through 20 phases. Reference: the MLE total is 2058.00.
#
#   Rscript ss3_jitter.R [n] [fraction]
suppressMessages(library(r4ss))

args <- commandArgs(trailingOnly = TRUE)
N    <- as.integer(if (length(args) > 0) args[1] else 24)
FRAC <- as.numeric(if (length(args) > 1) args[2] else 0.1)
SRC  <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA cod/Data/goa_pcod_caal_lambda_on"
EXE  <- "/private/tmp/claude-501/-Users-grantadams-Documents-GitHub-Rceattle-ecosystem-Rceattle/6adf6d97-2cdc-48dd-84b0-c6b720cb5b45/scratchpad/ss3bin/ss3"
ROOT <- file.path(tempdir(), "ss3jit")
MLE  <- 2058.00

run <- function(i) {
  d <- file.path(ROOT, sprintf("j%02d", i))
  unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(SRC, full.names = TRUE), d, overwrite = TRUE)
  # SS3's shipped outputs must go, or a binary that fails to launch leaves
  # SS_output() reading the stale MLE report and every jitter looks identical.
  unlink(file.path(d, c("Report.sso", "CompReport.sso", "ss_summary.sso",
                        "warning.sso", "covar.sso", "Forecast-report.sso")))
  st <- readLines(file.path(d, "starter.ss"))
  st[grep("#_init_values_src", st)]       <- "1 #_init_values_src"
  st[grep("#_last_estimation_phase", st)] <- "20 #_last_estimation_phase"
  st[grep("#_jitter_fraction", st)] <- sprintf("%g #_jitter_fraction", FRAC)
  # SS3 seeds its jitter from the run number, so each directory needs its own.
  rn <- file.path(d, "runnumber.ss")
  if (file.exists(rn)) writeLines(as.character(i), rn)
  writeLines(st, file.path(d, "starter.ss"))

  old <- setwd(d)
  s <- system2(EXE, "-nohess", stdout = "console.log", stderr = "console.log",
               timeout = 7200)
  setwd(old)
  if (!identical(as.integer(s), 0L)) return(c(i = i, TOTAL = NA_real_))
  r <- tryCatch(suppressWarnings(SS_output(d, verbose = FALSE, printstats = FALSE,
                                          covar = FALSE, forecast = FALSE)),
                error = function(e) NULL)
  if (is.null(r)) return(c(i = i, TOTAL = NA_real_))
  L <- r$likelihoods_used
  ts <- r$timeseries
  ts <- ts[ts$Era == "TIME", ]
  c(i = i, TOTAL = as.numeric(L["TOTAL", "values"]),
    Length_comp = as.numeric(L["Length_comp", "values"]),
    Age_comp = as.numeric(L["Age_comp", "values"]),
    Survey = as.numeric(L["Survey", "values"]),
    Recruitment = as.numeric(L["Recruitment", "values"]),
    lnR0 = as.numeric(r$parameters["SR_LN(R0)", "Value"]),
    regime = as.numeric(r$parameters["SR_regime_BLK5add_1976", "Value"]),
    L1 = as.numeric(r$parameters["L_at_Amin_Fem_GP_1", "Value"]),
    maxgrad = as.numeric(r$maximum_gradient_component),
    SSB1977 = as.numeric(tapply(ts$SpawnBio, ts$Yr, sum)["1977"]),
    SSB2024 = as.numeric(tapply(ts$SpawnBio, ts$Yr, sum)["2024"]))
}

dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
cat(sprintf("SS3 jitter: %d starts at fraction %g; MLE total is %.2f\n\n",
            N, FRAC, MLE))
out <- NULL
for (i in seq_len(N)) {
  t0 <- Sys.time()
  r <- run(i)
  mins <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  if (is.na(r[["TOTAL"]])) {
    cat(sprintf("  %2d  FAILED  (%.1f min)\n", i, mins))
  } else {
    cat(sprintf("  %2d  TOTAL %9.2f  (%+7.2f)  regime %7.4f  L1 %7.4f  |g| %8.3g  (%.1f min)\n",
                i, r[["TOTAL"]], r[["TOTAL"]] - MLE, r[["regime"]], r[["L1"]],
                r[["maxgrad"]], mins))
  }
  flush.console()
  out <- rbind(out, r)
}
out <- as.data.frame(out)
ok <- out[!is.na(out$TOTAL), ]
cat(sprintf("\n=== %d of %d converged ===\n", nrow(ok), N))
cat(sprintf("best   %.4f  (%+.4f vs the MLE)\n", min(ok$TOTAL),
            min(ok$TOTAL) - MLE))
cat(sprintf("worst  %.4f\n", max(ok$TOTAL)))
cat(sprintf("below the MLE by more than 0.1 nats: %d of %d\n",
            sum(ok$TOTAL < MLE - 0.1), nrow(ok)))
cat("\n--- sorted ---\n")
print(format(ok[order(ok$TOTAL), c("i", "TOTAL", "Length_comp", "regime", "L1",
                                   "lnR0", "SSB1977", "SSB2024", "maxgrad")],
             digits = 6), row.names = FALSE)
saveRDS(out, "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/SS3-bridge/_ss3_jitter.rds")
cat("\nJITTER DONE\n")
