# Is the +0.79-nat recruitment residual a missing CONSTANT or a real likelihood
# difference? A constant does not move when the parameters do.
#
# SS3's age-0-selected variant as a pure forward pass (estimation OFF) at the real
# MLE except for SR_regime_BLK5add_1976, evaluated on the same grid Rceattle will
# be. If the Rceattle-minus-SS3 recruitment residual is flat across the grid it is
# a constant and harmless; if it varies, the two codes score the initial state
# differently and THAT is the bridge defect behind the 8.9-nat optimum gap.
suppressMessages(library(r4ss))

SRC <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA cod/Data/goa_pcod_caal_lambda_on"
# No SS3 binary is checked in. Point SS3_EXE at one, or pass it as an
# argument; get a matching build with
# r4ss::get_ss3_exe(version = "v3.30.22.1"). v3.30.25.1 is the only
# binary that runs on macOS arm64 and reproduces this model's MLE total.
EXE <- Sys.getenv("SS3_EXE", "ss3")
GRID <- c(-1.2, -0.9, -0.740496, -0.5, -0.2444, 0.0)

age0_patch <- function(ctl) {
  h <- grep("^\\s*#_age_selex_patterns", ctl)
  stopifnot(length(h) == 1)
  n <- seen <- 0L
  for (i in (h + 1):min(h + 14, length(ctl))) {
    f <- strsplit(trimws(ctl[i]), "[ \t]+")[[1]]
    if (length(f) < 4 || is.na(suppressWarnings(as.numeric(f[1])))) next
    seen <- seen + 1L
    if (identical(f[1], "10")) { f[1] <- "0"; ctl[i] <- paste(f, collapse = " ")
                                 n <- n + 1L }
    if (seen >= 9L) break
  }
  stopifnot(seen == 9L, n == 5L)
  ctl
}

run <- function(tag, v) {
  d <- file.path(tempdir(), tag)
  unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(SRC, full.names = TRUE), d, overwrite = TRUE)
  unlink(file.path(d, c("Report.sso", "CompReport.sso", "ss_summary.sso",
                        "warning.sso", "covar.sso", "Forecast-report.sso")))
  st <- readLines(file.path(d, "starter.ss"))
  st[grep("#_init_values_src", st)]       <- "1 #_init_values_src"
  st[grep("#_last_estimation_phase", st)] <- "0 #_last_estimation_phase"
  writeLines(st, file.path(d, "starter.ss"))

  ctl <- age0_patch(readLines(file.path(d, "Model19_1e.ctl")))
  i <- grep("SR_regime_BLK5add_1976", ctl)
  stopifnot(length(i) == 1)
  f <- strsplit(trimws(ctl[i]), "[ \t]+")[[1]]
  f[3] <- format(v, digits = 15, scientific = FALSE)
  f[7] <- "-1"
  ctl[i] <- paste(f, collapse = " ")
  writeLines(ctl, file.path(d, "Model19_1e.ctl"))

  p <- readLines(file.path(d, "ss3.par"))
  j <- grep("^# SRparm\\[6\\]:$", p)
  stopifnot(length(j) == 1)
  p[j + 1] <- format(v, digits = 15, scientific = FALSE)
  writeLines(p, file.path(d, "ss3.par"))

  old <- setwd(d)
  s <- system2(EXE, "-nohess", stdout = "console.log", stderr = "console.log",
               timeout = 7200)
  setwd(old)
  if (!identical(as.integer(s), 0L))
    stop(sprintf("SS3 exited %s for %s; see %s", s, tag,
                 file.path(d, "console.log")))
  r <- suppressWarnings(SS_output(d, verbose = FALSE, printstats = FALSE,
                                  covar = FALSE, forecast = FALSE))
  used <- as.numeric(r$parameters["SR_regime_BLK5add_1976", "Value"])
  if (abs(used - v) > 1e-6 * max(1, abs(v)))
    stop(sprintf("asked %.8g, got %.8g", v, used))
  # estimation is off, so nothing but the pinned value may have moved
  r0 <- as.numeric(r$parameters["SR_LN(R0)", "Value"])
  nest <- sum(!is.na(r$parameters$Phase) & r$parameters$Phase > 0)
  ts <- r$timeseries
  ts <- ts[ts$Era == "TIME", ]
  L <- r$likelihoods_used
  rows <- c("TOTAL", "Catch", "Survey", "Length_comp", "Age_comp",
            "Recruitment", "InitEQ_Regime", "Parm_priors", "Parm_devs")
  c(regime = v, setNames(as.numeric(L[rows, "values"]), rows), lnR0 = r0,
    n_est = nest,
    SSB1977 = as.numeric(tapply(ts$SpawnBio, ts$Yr, sum)["1977"]))
}

out <- NULL
for (v in GRID) {
  cat(sprintf("regime = %-10.5g ", v)); flush.console()
  t0 <- Sys.time()
  r <- tryCatch(run(sprintf("fps_%s", gsub("[.-]", "_", signif(v, 6))), v),
                error = function(e) { cat("FAILED:", conditionMessage(e), "\n")
                                      NULL })
  if (is.null(r)) next
  cat(sprintf("TOTAL %.4f  Lcomp %.4f  Recr %.4f  InitEQ %.4f  lnR0 %.7f  nest %d  (%.1f min)\n",
              r[["TOTAL"]], r[["Length_comp"]], r[["Recruitment"]],
              r[["InitEQ_Regime"]], r[["lnR0"]], r[["n_est"]],
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  out <- rbind(out, r)
}
out <- as.data.frame(out)
cat("\n=== SS3 age-0-SELECTED FORWARD PASS over the regime (estimation OFF) ===\n")
print(format(out, digits = 8), row.names = FALSE)
saveRDS(out, "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/SS3-bridge/_ss3_age0_fp_shift.rds")
cat("\nSHIFT DONE\n")
