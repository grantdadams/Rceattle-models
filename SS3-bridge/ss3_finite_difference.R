# Finite-difference SS3's OWN likelihood components with respect to each growth
# parameter, on the log scale, so they can be compared term by term against
# Rceattle's d(jnll_comp)/d(log_growth_pars).
#
# SS3 is run as a pure forward pass: starter's init_values_src is set to read
# ss3.par, and "turn off estimation after phase" to 0, so no parameter moves.
suppressMessages(library(r4ss))

# Run from the Rceattle-models folder. Get a matching binary with
#   r4ss::get_ss3_exe(dir = <d>, version = "v3.30.22.1")
args <- commandArgs(trailingOnly = TRUE)
SRC  <- if (length(args) > 0) args[1] else "AI cod - Dev/Data/M24_1_caal_bins_fixed"
EXE  <- if (length(args) > 1) args[2] else Sys.getenv("SS3_EXE", "ss3")
ROOT <- if (length(args) > 2) args[3] else file.path(tempdir(), "ss3fd")
SRC  <- normalizePath(SRC); EXE <- normalizePath(EXE)

# MGparm index in ss3.par -> the name Rceattle's gp column carries.
PARS <- c(gp1_K = 4L, gp2_L1 = 2L, gp3_Linf = 3L, gp4_Richards = 5L)
H <- 2e-3          # log-scale step; large enough for Report.sso's 6 digits

run_ss3 <- function(tag, mg_idx = NA, mult = 1) {
  d <- file.path(ROOT, tag)
  unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (f in c("data.ss", "control.ss", "forecast.ss", "starter.ss", "ss3.par"))
    file.copy(file.path(SRC, f), file.path(d, f), overwrite = TRUE)

  st <- readLines(file.path(d, "starter.ss"))
  st[grep("0=use init values in control file", st)] <- "1 # use ss3.par"
  st[grep("Turn off estimation for parameters entering after this phase", st)] <-
    "0 # Turn off estimation for parameters entering after this phase"
  writeLines(st, file.path(d, "starter.ss"))

  if (!is.na(mg_idx)) {
    p <- readLines(file.path(d, "ss3.par"))
    i <- grep(sprintf("^# MGparm\\[%d\\]:$", mg_idx), p)
    stopifnot(length(i) == 1)
    p[i + 1] <- format(as.numeric(p[i + 1]) * mult, digits = 15, scientific = FALSE)
    writeLines(p, file.path(d, "ss3.par"))
  }

  # SS3 reads starter.ss from the process working directory, so run it there.
  old <- setwd(d); on.exit(setwd(old), add = TRUE)
  system2(EXE, "-nohess", stdout = "console.log", stderr = "console.log",
          timeout = 900)
  setwd(old)
  r <- suppressWarnings(SS_output(d, verbose = FALSE, printstats = FALSE,
                                  covar = FALSE, forecast = FALSE))
  L <- r$likelihoods_used
  setNames(L$values, rownames(L))
}

dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
cat("baseline...\n"); base <- run_ss3("base")
cat(sprintf("baseline TOTAL = %.5f  (Report.sso at the MLE says 532.9030)\n\n", base["TOTAL"]))

keep <- c("TOTAL", "Catch", "Equil_catch", "Survey", "Length_comp", "Age_comp",
          "Recruitment", "Parm_softbounds")
out <- matrix(NA_real_, length(keep), length(PARS), dimnames = list(keep, names(PARS)))
for (j in seq_along(PARS)) {
  cat("perturbing", names(PARS)[j], "...\n")
  up <- run_ss3(paste0(names(PARS)[j], "_up"), PARS[j], exp(H))
  dn <- run_ss3(paste0(names(PARS)[j], "_dn"), PARS[j], exp(-H))
  out[, j] <- (up[keep] - dn[keep]) / (2 * H)
}
cat("\n=== SS3: d(component) / d(log growth parameter) ===\n")
print(round(out, 3))
saveRDS(out, file.path(ROOT, "ss3_fd.rds"))
cat("\nsaved", file.path(ROOT, "ss3_fd.rds"), "\n")
