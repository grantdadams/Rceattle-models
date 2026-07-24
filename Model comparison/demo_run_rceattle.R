# =============================================================================
# demo_run_rceattle.R -- exercise run_rceattle() against a real ASSAMC OM
# replicate and self-test the recovery against operating-model truth.
#
# This driver is for local development only; it is NOT part of the ASSAMC
# package. It points `casedir` at one of the OM cases bundled in the local
# clone of Age_Structured_Stock_Assessment_Model_Comparison, runs the RCEATTLE
# estimation model over its replicate(s), reloads the saved result files, and
# (because the OM truth is known) checks that the fitted SSB / F / recruitment
# track the truth.
#
# Usage:
#   export PATH=/usr/bin:$PATH            # system toolchain (see Rceattle CLAUDE.md)
#   Rscript demo_run_rceattle.R           # defaults to the stochastic C1 case
#
# Choose the case by editing `case` below:
#   FIMS_C1 / FIMS_C2  logR_sd = 0.4  -- stochastic recruitment; all three
#                                        scenarios (incl. random effects) are
#                                        well-posed.
#   FIMS_C0 / C0noPhiF logR_sd = 0    -- DETERMINISTIC; the random-effects
#                                        scenarios are ill-posed (true sigmaR = 0)
#                                        and may time out (recorded as
#                                        non-converged). Good for testing the
#                                        fixed-effects scenario and the guard.
# =============================================================================

suppressMessages({
  library(Rceattle)
  library(dplyr)
})

# --- Locate the local ASSAMC clone and pick an OM case ----------------------
assamc_dir <- "/Users/grantadams/Documents/GitHub/Assessments/Age_Structured_Stock_Assessment_Model_Comparison"
case       <- "FIMS_C1"
casedir    <- file.path(assamc_dir, "FIMS_integration_test_data", case)
stopifnot(dir.exists(file.path(casedir, "output", "OM")))

# --- Source the estimation model --------------------------------------------
here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())
source(file.path(here, "om_to_rceattle.R"))
source(file.path(here, "run_rceattle.R"))

# --- Run the RCEATTLE EM over replicate(s) ----------------------------------
om_sim_num <- 1L
message(sprintf("Running RCEATTLE EM on %s (%d replicate[s])...", case, om_sim_num))
run_rceattle(
  maindir     = assamc_dir,
  subdir      = "RCEATTLE",
  om_sim_num  = om_sim_num,
  casedir     = casedir,
  em_bias_cor = FALSE
)

# --- Reload results and self-test against OM truth --------------------------
sim <- 1L
outdir <- file.path(casedir, "output", "RCEATTLE", paste0("s", sim))
cat("\nResult files written to", outdir, ":\n")
print(list.files(outdir))

# OM truth for this replicate
e <- new.env()
load(file.path(casedir, "output", "OM", paste0("OM", sim, ".RData")), envir = e)
true_ssb <- e$om_output$SSB
true_F   <- e$om_output$f
true_R   <- e$om_output$N.age[, 1]
yrs      <- e$om_input$year

cat("\n--- Self-test: estimated vs OM truth, per scenario ---\n")
fits <- list()
for (scn in RCEATTLE_SCENARIOS) {
  est_file <- file.path(outdir, sprintf("fit_rceattle_%s.RDS", scn))
  conv     <- readRDS(file.path(outdir, sprintf("optimizer_convergence_rceattle_%s.RDS", scn)))
  if (!file.exists(est_file) || is.null(readRDS(est_file))) {
    cat(sprintf("%-32s : no fit (convergence code %s)\n", scn, conv)); next
  }
  est <- readRDS(est_file)
  ssb <- est$estimate[est$label == "SSB"]
  Fv  <- est$estimate[est$label == "F"]
  Rv  <- est$estimate[est$label == "recruitment"]
  cat(sprintf(
    "%-32s : conv=%s  SSB cor=%.3f (rel.err=%.3f)  F cor=%.3f  R cor=%.3f\n",
    scn, conv,
    stats::cor(ssb, true_ssb), mean(abs(ssb - true_ssb) / true_ssb),
    stats::cor(Fv, true_F), stats::cor(Rv, true_R)))

  # Rebuild a lightweight object for plotting via the full fit (if present)
  full <- file.path(outdir, sprintf("full_fit_rceattle_%s.RDS", scn))
  if (file.exists(full)) fits[[scn]] <- readRDS(full)
}

# --- Comparison plots (Rceattle-models idiom) -------------------------------
# Collect the fitted scenarios and overlay their SSB / recruitment. Truth is
# available in `true_ssb` / `true_R` for reference.
if (length(fits) >= 1L) {
  model_names <- names(fits)
  message("\nPlotting SSB / biomass / recruitment across scenarios...")
  try(Rceattle::plot_ssb(fits, model_names = model_names, add_ci = TRUE))
  try(Rceattle::plot_biomass(fits, model_names = model_names))
  try(Rceattle::plot_recruitment(fits, model_names = model_names))
}

message("Demo complete.")
