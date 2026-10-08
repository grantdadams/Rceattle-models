# Build Data/goa_pcod_caal_bins_fixed_age0: the bridged model with SS3's age-0
# selectivity zeroing REMOVED, as a persistent directory rather than a tempdir
# run. age_selex_pattern 10 ("selectivity = 1 from age 1") becomes 0 ("= 1 for
# all ages"), on the five length-selected fleets. Estimation is OFF and the
# parameters are read from ss3.par, so this is the SAME MLE with one structural
# switch changed -- directly comparable to Rceattle, which has no age-0 zeroing.
suppressMessages(library(r4ss))
SRC <- "Data/goa_pcod_caal_bins_fixed"
DST <- "Data/goa_pcod_caal_bins_fixed_age0"
EXE <- normalizePath("Data/_iso_v25_base/ss3")
CTL <- "Model19_1e.ctl"

# Verbatim from SS3-bridge/ss3_age0_fp.R, so this reproduces that variant.
age0_patch <- function(ctl) {
  h <- grep("^\\s*#_age_selex_patterns", ctl)
  stopifnot(length(h) == 1)
  n <- seen <- 0L
  for (i in (h + 1):min(h + 14, length(ctl))) {
    f <- strsplit(trimws(ctl[i]), "[ \t]+")[[1]]
    if (length(f) < 4 || is.na(suppressWarnings(as.numeric(f[1])))) next
    seen <- seen + 1L
    if (identical(f[1], "10")) { f[1] <- "0"; ctl[i] <- paste(f, collapse = " "); n <- n + 1L }
    if (seen >= 9L) break
  }
  stopifnot(seen == 9L, n == 5L)
  ctl
}

unlink(DST, recursive = TRUE); dir.create(DST, recursive = TRUE)
keep <- setdiff(list.files(SRC), c("Report.sso","CompReport.sso","ss_summary.sso",
                                   "warning.sso","covar.sso","Forecast-report.sso",
                                   "control.ss_new","data_echo.ss_new","starter.ss_new"))
file.copy(file.path(SRC, keep), DST, overwrite = TRUE)

st <- readLines(file.path(DST, "starter.ss"))
st[grep("#_init_values_src", st)]       <- "1 #_init_values_src"
st[grep("#_last_estimation_phase", st)] <- "0 #_last_estimation_phase"
writeLines(st, file.path(DST, "starter.ss"))
writeLines(age0_patch(readLines(file.path(DST, CTL))), file.path(DST, CTL))

old <- setwd(DST)
s <- system2(EXE, "-nohess", stdout = "console.log", stderr = "console.log", timeout = 7200)
setwd(old)
if (!identical(as.integer(s), 0L))
  stop("SS3 exited ", s, "; see ", file.path(DST, "console.log"))

r <- suppressWarnings(SS_output(DST, verbose = FALSE, printstats = FALSE,
                                covar = FALSE, forecast = FALSE))
L <- r$likelihoods_used
g <- function(n) if (n %in% rownames(L)) L[n, "values"] else NA_real_
sc <- readRDS("../SS3-bridge/_ss3_L1_scan_goa_age0.rds")
ref <- sc[which.min(abs(sc$L1 - 1.3007)), ]
cat("\n=== new run vs the saved age-0 variant (_ss3_L1_scan_goa_age0.rds, L1 = 1.3007) ===\n")
cat(sprintf("%-14s %14s %14s %12s\n","component","new run","saved","diff"))
for (k in c("TOTAL","Catch","Survey","Length_comp","Age_comp","Recruitment",
            "Parm_priors","Parm_devs")) {
  rv <- g(k); sv <- if (k %in% names(ref)) ref[[k]] else NA_real_
  cat(sprintf("%-14s %14.4f %14s %12s\n", k, rv,
      if (is.na(sv)) "-" else sprintf("%.4f", sv),
      if (is.na(sv)) "-" else sprintf("%+.4f", rv - sv)))
}
