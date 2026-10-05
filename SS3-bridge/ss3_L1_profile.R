# Profile SS3's own likelihood over L1: fix L_at_Amin at each grid value and let
# SS3 re-estimate all 329 other parameters. This is the only construction
# comparable to Rceattle's G3 profile; SS3-bridge/ss3_L1_scan.R holds everything
# else fixed and is a slice.
#
# Why it is worth six full SS3 estimations: Rceattle's profile runs L1 down to a
# numerical floor, 8.27 nats better than SS3's MLE value, driven by the
# composition components. Both models' SLICES prefer a smaller L1 once SS3's
# age-0 selectivity zeroing is removed, so the open question is whether SS3's
# age-0 variant also runs L1 to zero on a profile -- i.e. whether L1 is
# unidentified in the model itself or only in Rceattle's rendering of it.
#
#   Rscript SS3-bridge/ss3_L1_profile.R [src_dir] [ss3_exe]
suppressMessages(library(r4ss))

# No SS3 binary is checked in. Point SS3_EXE at one, or pass it as an
# argument; get a matching build with
# r4ss::get_ss3_exe(version = "v3.30.22.1"). v3.30.25.1 is the only
# binary that runs on macOS arm64 and reproduces this model's MLE total.
args <- commandArgs(trailingOnly = TRUE)
# The age-0-SELECTED variant, which is not checked in either: build it with the
# recipe in GOA-remaining-gap.md "Reproducing" (the five `10` entries under
# #_age_selex_patterns become `0`), then name it here or in SS3_AGE0_DIR.
.src <- if (length(args) > 0) args[1] else Sys.getenv("SS3_AGE0_DIR", "")
if (!nzchar(.src) || !dir.exists(.src)) {
  stop("give the age-0-selected SS3 directory as argument 1 or in SS3_AGE0_DIR; ",
       "see GOA-remaining-gap.md \"Reproducing\" for how to build it.",
       call. = FALSE)
}
SRC  <- normalizePath(.src)
EXE  <- normalizePath(if (length(args) > 1) args[2] else Sys.getenv("SS3_EXE", "ss3"))
ROOT <- file.path(tempdir(), "ss3L1prof")

L1_GRID <- c(0.001, 0.1, 0.5, 1.3007, 3)

run_ss3 <- function(tag, l1) {
  d <- file.path(ROOT, tag)
  unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(SRC, full.names = TRUE), d, overwrite = TRUE)
  # SS3's shipped outputs must go, or a binary that will not launch leaves
  # SS_output() reading the MLE report and every point comes back identical.
  unlink(file.path(d, c("Report.sso", "CompReport.sso", "ss_summary.sso",
                        "warning.sso", "covar.sso", "Forecast-report.sso")))

  # Start from the MLE and estimate everything; only L1 is pinned.
  st <- readLines(file.path(d, "starter.ss"))
  st[grep("#_init_values_src", st)]      <- "1 #_init_values_src"
  st[grep("#_last_estimation_phase", st)] <- "20 #_last_estimation_phase"
  writeLines(st, file.path(d, "starter.ss"))

  # L_at_Amin to phase -1 at the profiled value. The MG_parms row carries
  # LO HI INIT PRIOR PR_SD PR_type PHASE in its first seven fields.
  ctl <- readLines(file.path(d, "Model19_1e.ctl"))
  i <- grep("L_at_Amin", ctl)
  stopifnot(length(i) == 1)
  f <- strsplit(trimws(ctl[i]), "[ \t]+")[[1]]
  f[3] <- format(l1, digits = 15, scientific = FALSE)   # INIT
  f[7] <- "-1"                                          # PHASE
  ctl[i] <- paste(f, collapse = " ")
  writeLines(ctl, file.path(d, "Model19_1e.ctl"))

  # ss3.par is read first, so the pinned value has to be there too.
  p <- readLines(file.path(d, "ss3.par"))
  j <- grep("^# MGparm\\[2\\]:$", p)
  stopifnot(length(j) == 1)
  p[j + 1] <- format(l1, digits = 15, scientific = FALSE)
  writeLines(p, file.path(d, "ss3.par"))

  old <- setwd(d); on.exit(setwd(old), add = TRUE)
  st2 <- system2(EXE, "-nohess", stdout = "console.log", stderr = "console.log",
                 timeout = 7200)
  setwd(old)
  if (!identical(as.integer(st2), 0L))
    stop(sprintf("SS3 exited %s for %s; see %s", st2, tag,
                 file.path(d, "console.log")))
  r <- suppressWarnings(SS_output(d, verbose = FALSE, printstats = FALSE,
                                 covar = FALSE, forecast = FALSE))
  L <- r$likelihoods_used
  used <- as.numeric(r$parameters["L_at_Amin_Fem_GP_1", "Value"])
  if (abs(used - l1) > 1e-6 * max(1, abs(l1)))
    stop(sprintf("asked for L1 = %.8g, SS3 reports %.8g: it was not pinned.",
                 l1, used))
  nest <- sum(!is.na(r$parameters$Phase) & r$parameters$Phase > 0)
  c(setNames(L$values, rownames(L)), L1_used = used, n_est = nest)
}

dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
keep <- c("TOTAL", "Catch", "Survey", "Length_comp", "Age_comp", "Recruitment",
          "Parm_priors", "Parm_softbounds", "Parm_devs")
out <- NULL
for (v in L1_GRID) {
  cat(sprintf("L1 = %-8.4g ", v)); flush.console()
  t0 <- Sys.time()
  r <- run_ss3(sprintf("l1_%s", signif(v, 5)), v)
  cat(sprintf("TOTAL %.4f  (%d estimated, %.1f min)\n", r["TOTAL"], r["n_est"],
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  out <- rbind(out, c(L1 = v, r[c(keep, "n_est")]))
}
out <- as.data.frame(out)
out$delta <- out$TOTAL - min(out$TOTAL)

cat("\n=== SS3 PROFILE over L1 (all other parameters re-estimated) ===\n")
print(format(out, digits = 6), row.names = FALSE)
cat(sprintf("\nminimum at L1 = %.4g ; SS3's own MLE value is 1.3007\n",
            out$L1[which.min(out$TOTAL)]))
dest <- file.path("SS3-bridge", sprintf("_ss3_L1_profile_%s.rds", basename(SRC)))
saveRDS(out, dest); cat("Saved ", dest, "\n", sep = "")
