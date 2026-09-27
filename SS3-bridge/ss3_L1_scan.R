# Scan SS3's own likelihood over L1 (length at the youngest growth age) with
# every other parameter held at SS3's MLE, and report it component by component.
#
# Why: Rceattle's G3 profile on GOA Pacific cod falls monotonically as L1 goes to
# zero and is 8.27 nats better at L1 = 0.001 than at SS3's MLE of 1.3007. Since
# the forward pass reproduces both of SS3's composition components to ~0.001 AT
# that MLE, SS3 ought to be able to claim the same 8.27 nats. Either it can -- in
# which case its MLE is not at its own L1 optimum and Rceattle is right -- or it
# pays a term Rceattle has no analogue for, and the scan says which.
#
# This is a SLICE, not a profile: nothing else re-optimises. Compare it against
# Rceattle's slice (rce_L1_scan.R), not against Rceattle's profile.
#
#   Rscript SS3-bridge/ss3_L1_scan.R [src_dir] [ss3_exe]
suppressMessages(library(r4ss))

args <- commandArgs(trailingOnly = TRUE)
SRC  <- normalizePath(if (length(args) > 0) args[1] else
                      "GOA cod/Data/goa_pcod_caal_bins_fixed")
EXE  <- normalizePath(if (length(args) > 1) args[2] else
                      Sys.getenv("SS3_EXE", "ss3"))
ROOT <- file.path(tempdir(), "ss3L1")

MG_L1  <- 2L                     # MGparm[2] in ss3.par is L_at_Amin
L1_GRID <- c(0.001, 0.1, 0.5, 1.3007, 3, 6.3923)

# starter.ss labels differ between models -- the AI control uses the long
# phrases, the GOA one short keys -- so match either.
set_forward_pass <- function(path) {
  st <- readLines(path)
  hit <- function(...) { for (p in c(...)) { i <- grep(p, st, fixed = TRUE)
                         if (length(i)) return(i[1]) }; NA_integer_ }
  i <- hit("0=use init values in control file", "#_init_values_src")
  stopifnot(!is.na(i)); st[i] <- "1 # use ss3.par"
  j <- hit("Turn off estimation for parameters entering after this phase",
           "#_last_estimation_phase")
  stopifnot(!is.na(j)); st[j] <- "0 # last_estimation_phase"
  writeLines(st, path)
}

run_ss3 <- function(tag, l1 = NA) {
  # Copy the whole model folder: starter.ss names its own data and control
  # files, and those names differ between the AI and GOA models.
  d <- file.path(ROOT, tag)
  unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(SRC, full.names = TRUE), d, overwrite = TRUE)
  # The source folder ships SS3's own outputs. Delete them, or a binary that
  # fails to launch leaves SS_output() reading the MLE report and every point of
  # the scan comes back identical and wrong.
  unlink(file.path(d, c("Report.sso", "CompReport.sso", "ss_summary.sso",
                        "warning.sso", "covar.sso", "Forecast-report.sso")))
  set_forward_pass(file.path(d, "starter.ss"))
  if (!is.na(l1)) {
    p <- readLines(file.path(d, "ss3.par"))
    i <- grep(sprintf("^# MGparm\\[%d\\]:$", MG_L1), p)
    stopifnot(length(i) == 1)
    p[i + 1] <- format(l1, digits = 15, scientific = FALSE)
    writeLines(p, file.path(d, "ss3.par"))
  }
  old <- setwd(d); on.exit(setwd(old), add = TRUE)
  st <- system2(EXE, "-nohess", stdout = "console.log", stderr = "console.log",
                timeout = 1800)
  setwd(old)
  if (!identical(as.integer(st), 0L))
    stop(sprintf("SS3 exited %s for %s (126 means the binary will not run on ",
                 st, tag), "this platform); see ", file.path(d, "console.log"))
  if (!file.exists(file.path(d, "Report.sso")))
    stop("SS3 wrote no Report.sso for ", tag)
  r <- suppressWarnings(SS_output(d, verbose = FALSE, printstats = FALSE,
                                 covar = FALSE, forecast = FALSE))
  L <- r$likelihoods_used
  used <- as.numeric(r$parameters["L_at_Amin_Fem_GP_1", "Value"])
  # The point is only a point if SS3 actually held the value we asked for.
  if (!is.na(l1) && abs(used - l1) > 1e-6 * max(1, abs(l1)))
    stop(sprintf("asked SS3 for L1 = %.8g but it reports %.8g: the par file was ",
                 l1, used), "not read, or the run re-estimated.")
  c(setNames(L$values, rownames(L)), L1_used = used)
}

dir.create(ROOT, recursive = TRUE, showWarnings = FALSE)
keep <- c("TOTAL", "Catch", "Equil_catch", "Survey", "Length_comp", "Age_comp",
          "Recruitment", "Parm_priors", "Parm_softbounds", "Parm_devs")
out <- NULL
for (v in L1_GRID) {
  cat(sprintf("L1 = %-8.4g ... ", v)); flush.console()
  r <- run_ss3(sprintf("l1_%s", signif(v, 5)), v)
  cat(sprintf("TOTAL %.4f (L1 read back %.5g)\n", r["TOTAL"], r["L1_used"]))
  out <- rbind(out, c(L1 = v, r[keep]))
}
out <- as.data.frame(out)
ref <- out$TOTAL[which.min(abs(out$L1 - 1.3007))]
out$delta_vs_MLE <- out$TOTAL - ref

cat("\n=== SS3's own likelihood over L1, everything else at its MLE ===\n")
print(format(out, digits = 6), row.names = FALSE)
cat(sprintf("\nSS3's MLE L1 is 1.3007. Best on this grid: L1 = %.4g (%.4f nats better).\n",
            out$L1[which.min(out$TOTAL)], ref - min(out$TOTAL)))
# Name the file after the model, or scanning a second variant silently replaces
# the first one's results.
dest <- file.path("SS3-bridge", sprintf("_ss3_L1_scan_%s.rds", basename(SRC)))
saveRDS(out, dest)
cat("Saved ", dest, "\n", sep = "")
