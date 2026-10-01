# Profile SS3's own likelihood over the initial recruitment regime offset
# (SR_regime_BLK5add_1976), re-estimating every other parameter from the MLE.
#
# Why: Rceattle's cold start converges 8.9 nats BELOW the optimum SS3's MLE sits
# in, and the whole difference is the initial state -- an initial recruitment
# level of exp(-0.244) = 0.78 of R0 against SS3's exp(-0.7405) = 0.48, giving a
# 1977 SSB 1.90x SS3's that decays to 1.09x by 1984. The open question is whether
# SS3's own likelihood also prefers the higher initial state, as it does for L1
# (its L1 profile minimises at 0.001, +3.97 nats at its reported MLE of 1.3007),
# or whether Rceattle's second optimum is specific to Rceattle's rendering.
#
# Built on the construction in SS3-bridge/ss3_L1_profile.R.
#   Rscript ss3_regime_profile.R [src_dir] [ss3_exe]
suppressMessages(library(r4ss))

args <- commandArgs(trailingOnly = TRUE)
SRC <- normalizePath(if (length(args) > 0) args[1] else
  "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA cod/Data/goa_pcod_caal_lambda_on")
EXE <- normalizePath(if (length(args) > 1) args[2] else
  "/private/tmp/claude-501/-Users-grantadams-Documents-GitHub-Rceattle-ecosystem-Rceattle/6adf6d97-2cdc-48dd-84b0-c6b720cb5b45/scratchpad/ss3bin/ss3")
ROOT <- file.path(tempdir(), "ss3regprof")

# SS3's MLE is -0.740496; Rceattle's cold optimum is -0.2444.
GRID <- c(-1.2, -0.9, -0.740496, -0.5, -0.2444, 0.0)

run_ss3 <- function(tag, v) {
  d <- file.path(ROOT, tag)
  unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(SRC, full.names = TRUE), d, overwrite = TRUE)
  # SS3's shipped outputs must go, or a binary that will not launch leaves
  # SS_output() reading the MLE report and every point comes back identical.
  unlink(file.path(d, c("Report.sso", "CompReport.sso", "ss_summary.sso",
                        "warning.sso", "covar.sso", "Forecast-report.sso")))

  st <- readLines(file.path(d, "starter.ss"))
  st[grep("#_init_values_src", st)]       <- "1 #_init_values_src"
  st[grep("#_last_estimation_phase", st)] <- "20 #_last_estimation_phase"
  writeLines(st, file.path(d, "starter.ss"))

  # The SR block row carries LO HI INIT PRIOR PR_SD PR_type PHASE.
  ctl <- readLines(file.path(d, "Model19_1e.ctl"))
  i <- grep("SR_regime_BLK5add_1976", ctl)
  stopifnot(length(i) == 1)
  f <- strsplit(trimws(ctl[i]), "[ \t]+")[[1]]
  f[3] <- format(v, digits = 15, scientific = FALSE)   # INIT
  f[7] <- "-1"                                         # PHASE
  ctl[i] <- paste(f, collapse = " ")
  writeLines(ctl, file.path(d, "Model19_1e.ctl"))

  # ss3.par is read first, so the pinned value has to be there too. v3.30.25's
  # par has no SR_ labels; SRparm[6] is the block offset (value -0.740496 in the
  # MLE par, matching ss_summary's SR_regime_BLK5add_1976).
  p <- readLines(file.path(d, "ss3.par"))
  j <- grep("^# SRparm\\[6\\]:$", p)
  stopifnot(length(j) == 1)
  p[j + 1] <- format(v, digits = 15, scientific = FALSE)
  writeLines(p, file.path(d, "ss3.par"))

  old <- setwd(d)
  st2 <- system2(EXE, "-nohess", stdout = "console.log", stderr = "console.log",
                 timeout = 7200)
  setwd(old)
  if (!identical(as.integer(st2), 0L))
    stop(sprintf("SS3 exited %s for %s; see %s", st2, tag,
                 file.path(d, "console.log")))
  r <- suppressWarnings(SS_output(d, verbose = FALSE, printstats = FALSE,
                                  covar = FALSE, forecast = FALSE))
  used <- as.numeric(r$parameters["SR_regime_BLK5add_1976", "Value"])
  if (abs(used - v) > 1e-6 * max(1, abs(v)))
    stop(sprintf("asked for %.8g, SS3 reports %.8g: it was not pinned.", v, used))
  ts <- r$timeseries
  ts <- ts[ts$Era == "TIME", ]
  ssb <- tapply(ts$SpawnBio, ts$Yr, sum)
  L <- r$likelihoods_used
  c(setNames(L$values, rownames(L)), used = used,
    n_est = sum(!is.na(r$parameters$Phase) & r$parameters$Phase > 0),
    lnR0 = as.numeric(r$parameters["SR_LN(R0)", "Value"]),
    SSB1977 = as.numeric(ssb["1977"]), SSB2024 = as.numeric(ssb["2024"]))
}

dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
keep <- c("TOTAL", "Catch", "Survey", "Length_comp", "Age_comp", "Recruitment",
          "Parm_priors", "Parm_softbounds", "Parm_devs")
out <- NULL
for (v in GRID) {
  cat(sprintf("regime = %-10.5g ", v)); flush.console()
  t0 <- Sys.time()
  r <- tryCatch(run_ss3(sprintf("reg_%s", gsub("[.-]", "_", signif(v, 6))), v),
                error = function(e) { cat("FAILED:", conditionMessage(e), "\n")
                                      NULL })
  if (is.null(r)) next
  cat(sprintf("TOTAL %.4f  (%d est, SSB77 %.0f, %.1f min)\n", r["TOTAL"],
              r["n_est"], r["SSB1977"],
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  out <- rbind(out, c(regime = v, r[c(keep, "n_est", "lnR0",
                                      "SSB1977", "SSB2024")]))
}
out <- as.data.frame(out)
out$delta <- out$TOTAL - min(out$TOTAL)

cat("\n=== SS3 PROFILE over SR_regime_BLK5add_1976 ===\n")
print(format(out, digits = 7), row.names = FALSE)
cat(sprintf("\nminimum at regime = %.4g ; SS3's MLE is -0.740496, ",
            out$regime[which.min(out$TOTAL)]))
cat("Rceattle's cold optimum is -0.2444\n")
saveRDS(out, "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/SS3-bridge/_ss3_regime_profile.rds")
cat("\nREGIME DONE\n")
