# SS3's age-0-SELECTED variant as a pure forward pass at the real MLE, so its
# components can be differenced against Rceattle's forward pass (1934.4047) at the
# same parameter vector. Estimation OFF (last_estimation_phase 0), so this is the
# same parameters, one structural switch changed.
#
# Closes the question "does the total match": it cannot, because SS3 drops density
# constants Rceattle keeps and carries three components Rceattle has no analogue
# for. This run measures every term of that sum at head instead of reciting it.
suppressMessages(library(r4ss))

SRC <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA cod/Data/goa_pcod_caal_lambda_on"
# No SS3 binary is checked in. Point SS3_EXE at one, or pass it as an
# argument; get a matching build with
# r4ss::get_ss3_exe(version = "v3.30.22.1"). v3.30.25.1 is the only
# binary that runs on macOS arm64 and reproduces this model's MLE total.
EXE <- Sys.getenv("SS3_EXE", "ss3")

age0_patch <- function(ctl) {
  h <- grep("^\\s*#_age_selex_patterns", ctl)
  stopifnot(length(h) == 1)
  n <- seen <- 0L
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
  stopifnot(seen == 9L, n == 5L)
  ctl
}

run <- function(tag, patch_age0) {
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
  if (patch_age0) {
    ctl <- age0_patch(readLines(file.path(d, "Model19_1e.ctl")))
    writeLines(ctl, file.path(d, "Model19_1e.ctl"))
  }
  old <- setwd(d)
  s <- system2(EXE, "-nohess", stdout = "console.log", stderr = "console.log",
               timeout = 7200)
  setwd(old)
  if (!identical(as.integer(s), 0L))
    stop(sprintf("SS3 exited %s for %s; see %s", s, tag,
                 file.path(d, "console.log")))
  r <- suppressWarnings(SS_output(d, verbose = FALSE, printstats = FALSE,
                                  covar = FALSE, forecast = FALSE))
  # assert the parameters really were held
  ne <- sum(!is.na(r$parameters$Phase) & r$parameters$Phase > 0)
  list(L = r$likelihoods_used, n_est = ne,
       lnR0 = as.numeric(r$parameters["SR_LN(R0)", "Value"]),
       reg = as.numeric(r$parameters["SR_regime_BLK5add_1976", "Value"]))
}

cat("--- SS3 forward pass, age 0 ZEROED (the assessment) ---\n")
a <- run("ss3fp_real", FALSE)
cat(sprintf("  lnR0 %.6f  regime %.6f  (%d phase>0 params, estimation off)\n",
            a$lnR0, a$reg, a$n_est))
cat("--- SS3 forward pass, age 0 SELECTED ---\n")
b <- run("ss3fp_age0", TRUE)
cat(sprintf("  lnR0 %.6f  regime %.6f\n", b$lnR0, b$reg))
if (abs(a$lnR0 - b$lnR0) > 1e-9 || abs(a$reg - b$reg) > 1e-9)
  stop("the two runs are not at the same parameters")

RCE <- c(`Index data` = 42.45730, `Catch data` = -264.31948,
         `Composition data` = 1414.20851, `CAAL data` = 732.01718,
         `Initial abundance deviates` = 5.57631,
         `Recruitment deviates` = 35.88985, `M prior` = 0.09152,
         `Linkage-table priors` = -31.51650)
RCE_TOT <- 1934.4047

rows <- c("TOTAL", "Catch", "Survey", "Length_comp", "Age_comp", "Recruitment",
          "InitEQ_Regime", "Parm_priors", "Parm_softbounds", "Parm_devs")
tab <- data.frame(row = rows,
                  age0_zeroed = as.numeric(a$L[rows, "values"]),
                  age0_selected = as.numeric(b$L[rows, "values"]))
tab$diff <- tab$age0_selected - tab$age0_zeroed
cat("\n=== SS3 components at the SAME parameters ===\n")
print(format(tab, digits = 8), row.names = FALSE)
cat(sprintf("\nage-0 zeroing is worth %.4f nats on Length_comp, %.4f on TOTAL\n",
            -tab$diff[tab$row == "Length_comp"], -tab$diff[tab$row == "TOTAL"]))

# Pair Rceattle against the age-0-SELECTED variant -- the only one whose
# likelihood Rceattle's matches. InitEQ_Regime is SS3's penalty on the initial
# recruitment level; Rceattle's analogue is the init linkage prior, which sits in
# Linkage-table priors, so it pairs there and NOT with the deviate rows.
g <- function(r) as.numeric(b$L[r, "values"])
CONST <- c(Catch = -265.5188, Survey = 45.9469, Recruitment = 53.2984)
cat("\n=== Rceattle vs age-0-SELECTED SS3, at SS3's MLE ===\n")
cat(sprintf("%-16s %13s %13s %11s %11s\n", "component", "Rceattle", "SS3",
            "constant", "residual"))
pr <- function(nm, rv, sv, k) {
  cat(sprintf("%-16s %13.4f %13.4f %11s %11.4f\n", nm, rv, sv,
              if (is.na(k)) "--" else sprintf("%.4f", k), rv - sv - (if (is.na(k)) 0 else k)))
  rv - sv - (if (is.na(k)) 0 else k)
}
res <- c(
  pr("Length_comp", RCE[["Composition data"]], g("Length_comp"), NA),
  pr("Age_comp",    RCE[["CAAL data"]],        g("Age_comp"),    NA),
  pr("Catch",       RCE[["Catch data"]],       g("Catch"),       CONST[["Catch"]]),
  pr("Survey",      RCE[["Index data"]],       g("Survey"),      CONST[["Survey"]]),
  pr("Recruitment", RCE[["Initial abundance deviates"]] +
                    RCE[["Recruitment deviates"]],
     g("Recruitment") + g("InitEQ_Regime"),    CONST[["Recruitment"]]))
cat(sprintf("\nsum of residuals on the five shared components: %+.4f\n", sum(res)))

cat("\n=== does the TOTAL match? ===\n")
extra <- g("Parm_priors") + g("Parm_softbounds") + g("Parm_devs")
cat(sprintf("SS3 TOTAL (age 0 selected)                 %12.4f\n", g("TOTAL")))
cat(sprintf("Rceattle forward pass                      %12.4f\n", RCE_TOT))
cat(sprintf("  raw difference                           %12.4f\n",
            RCE_TOT - g("TOTAL")))
cat(sprintf("less the density constants SS3 drops        %12.4f\n", sum(CONST)))
cat(sprintf("less SS3 rows Rceattle has no analogue for  %12.4f\n", -extra))
cat(sprintf("  Parm_priors %.4f  Parm_devs %.4f  Parm_softbounds %.4f\n",
            g("Parm_priors"), g("Parm_devs"), g("Parm_softbounds")))
cat(sprintf("plus Rceattle rows SS3 has no analogue for  %12.4f\n",
            RCE[["M prior"]] + RCE[["Linkage-table priors"]]))
cat(sprintf("  M prior %.4f  Linkage-table priors %.4f\n",
            RCE[["M prior"]], RCE[["Linkage-table priors"]]))
cat(sprintf("\nUNEXPLAINED REMAINDER                      %12.4f\n",
            RCE_TOT - g("TOTAL") - sum(CONST) + extra -
            (RCE[["M prior"]] + RCE[["Linkage-table priors"]])))
cat("\nAGE0FP DONE\n")
