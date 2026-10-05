# The same regime profile as ss3_regime_profile.R, but on the SS3 variant that
# SELECTS age 0 -- the only variant whose likelihood Rceattle's matches (every
# component to 1e-3; see SS3-bridge/GOA-remaining-gap.md "Full accounting").
#
# The question: on the real assessment SS3's Length_comp gets WORSE as the initial
# recruitment regime rises (+2.96 nats from -0.7405 to -0.2444) while Rceattle's
# Composition gets BETTER (-5.81). If that sign flip is SS3's age-0 zeroing, then
# the age-0-SELECTED variant should flip with Rceattle, and the whole 8.9-nat
# initial-state disagreement is the one feature Rceattle cannot express.
#
# Variant recipe from GOA-remaining-gap.md "Reproducing": the five `10` entries
# under #_age_selex_patterns become `0` ("constant age-specific selex for ages 0
# to nages", SS_selex.tpl:990-993).
#
#   Rscript ss3_regime_profile_age0.R
suppressMessages(library(r4ss))

SRC <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA cod/Data/goa_pcod_caal_lambda_on"
# No SS3 binary is checked in. Point SS3_EXE at one, or pass it as an
# argument; get a matching build with
# r4ss::get_ss3_exe(version = "v3.30.22.1"). v3.30.25.1 is the only
# binary that runs on macOS arm64 and reproduces this model's MLE total.
EXE <- Sys.getenv("SS3_EXE", "ss3")
ROOT <- file.path(tempdir(), "ss3regage0")
GRID <- c(-1.2, -0.9, -0.740496, -0.5, -0.2444, 0.0)

# Turn the age selectivity patterns from 10 to 0. The block is the five lines
# after the #_age_selex_patterns header; only the five ACTIVE fleets carry 10.
# The block is exactly the 9 fleet rows after the two header lines (164-174 in
# Model19_1e.ctl: fleets 1-5 carry 10, fleets 6-9 already 0). Bounded to those
# rows so a LO field of 10 in the SizeSelex block below cannot be hit.
age0_patch <- function(ctl) {
  h <- grep("^\\s*#_age_selex_patterns", ctl)
  stopifnot(length(h) == 1)
  n <- 0L
  seen <- 0L
  for (i in (h + 1):min(h + 14, length(ctl))) {
    f <- strsplit(trimws(ctl[i]), "[ \t]+")[[1]]
    if (length(f) < 4 || is.na(suppressWarnings(as.numeric(f[1])))) next
    seen <- seen + 1L
    if (identical(f[1], "10")) {
      f[1] <- "0"
      ctl[i] <- paste(f, collapse = " ")
      n <- n + 1L
    }
    if (seen >= 9L) break
  }
  stopifnot(seen == 9L, n == 5L)   # 9 fleets, 5 of them pattern 10
  attr(ctl, "n_patched") <- n
  ctl
}

run_ss3 <- function(tag, v) {
  d <- file.path(ROOT, tag)
  unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(SRC, full.names = TRUE), d, overwrite = TRUE)
  unlink(file.path(d, c("Report.sso", "CompReport.sso", "ss_summary.sso",
                        "warning.sso", "covar.sso", "Forecast-report.sso")))

  st <- readLines(file.path(d, "starter.ss"))
  st[grep("#_init_values_src", st)]       <- "1 #_init_values_src"
  st[grep("#_last_estimation_phase", st)] <- "20 #_last_estimation_phase"
  writeLines(st, file.path(d, "starter.ss"))

  ctl <- readLines(file.path(d, "Model19_1e.ctl"))
  ctl <- age0_patch(ctl)
  np <- attr(ctl, "n_patched")
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
    stop(sprintf("asked for %.8g, SS3 reports %.8g: not pinned.", v, used))
  ts <- r$timeseries
  ts <- ts[ts$Era == "TIME", ]
  ssb <- tapply(ts$SpawnBio, ts$Yr, sum)
  L <- r$likelihoods_used
  c(setNames(L$values, rownames(L)), used = used, n_patched = np,
    n_est = sum(!is.na(r$parameters$Phase) & r$parameters$Phase > 0),
    lnR0 = as.numeric(r$parameters["SR_LN(R0)", "Value"]),
    SSB1977 = as.numeric(ssb["1977"]))
}

dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
keep <- c("TOTAL", "Catch", "Survey", "Length_comp", "Age_comp", "Recruitment",
          "Parm_priors", "Parm_softbounds", "Parm_devs")
out <- NULL
for (v in GRID) {
  cat(sprintf("regime = %-10.5g ", v)); flush.console()
  t0 <- Sys.time()
  r <- tryCatch(run_ss3(sprintf("a0_%s", gsub("[.-]", "_", signif(v, 6))), v),
                error = function(e) { cat("FAILED:", conditionMessage(e), "\n")
                                      NULL })
  if (is.null(r)) next
  cat(sprintf("TOTAL %.4f  Length_comp %.4f  (%d patched, %d est, %.1f min)\n",
              r["TOTAL"], r["Length_comp"], r["n_patched"], r["n_est"],
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  out <- rbind(out, c(regime = v, r[c(keep, "n_patched", "n_est", "lnR0",
                                      "SSB1977")]))
}
out <- as.data.frame(out)
out$delta <- out$TOTAL - min(out$TOTAL)

cat("\n=== SS3 (AGE 0 SELECTED) PROFILE over SR_regime_BLK5add_1976 ===\n")
print(format(out, digits = 7), row.names = FALSE)
cat(sprintf("\nminimum at regime = %.4g\n", out$regime[which.min(out$TOTAL)]))
cat("Real assessment minimised at -0.7405; Rceattle's cold optimum is -0.2444\n")
cat("\nLength_comp slope, -0.7405 -> -0.2444:\n")
a <- out$Length_comp[which.min(abs(out$regime + 0.740496))]
b <- out$Length_comp[which.min(abs(out$regime + 0.2444))]
cat(sprintf("  age-0 SELECTED : %.4f -> %.4f  = %+.4f\n", a, b, b - a))
cat("  real assessment: 1334.3300 -> 1337.2900  = +2.9600\n")
cat("  Rceattle       : 1353.9813 -> 1348.1667  = -5.8146\n")
saveRDS(out, "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/SS3-bridge/_ss3_regime_profile_age0.rds")
cat("\nAGE0 DONE\n")
