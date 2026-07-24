# =============================================================================
# demo_run_rceattle.R -- exercise run_rceattle() against a real ASSAMC OM
# replicate and self-test the recovery against operating-model truth.
#
# This is for local development only. It points `casedir` at one of the OM cases
# of Age_Structured_Stock_Assessment_Model_Comparison, runs the RCEATTLE
# estimation model over its replicate(s), reloads the saved result files, and
# checks that the fitted SSB / F / recruitment against the OM.
#
# Cases:
#   FIMS_C1 / FIMS_C2  logR_sd = 0.4  -- stochastic recruitment; all three
#                                        scenarios incl. random effects.
#   FIMS_C0 / C0noPhiF logR_sd = 0    -- DETERMINISTIC; the random-effects
#                                        scenarios are ill-posed (true sigmaR = 0)
#                                        and may time out (recorded as
#                                        non-converged).
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


# --- Reload results and compare against OM --------------------------
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
series <- list()
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
  series[[scn]] <- list(SSB = ssb, F = Fv, recruitment = Rv)
}

# --- Comparison plots: RCEATTLE scenarios vs the OM ------------------------
# In every panel the OM (operating-model TRUTH) is the thick BLACK solid line;
# each RCEATTLE estimation-model scenario is a coloured DASHED line. The legend
# names every series, so which line is the OM and which are RCEATTLE is explicit.
if (length(series) >= 1L) {
  scen_col <- c(random_effects                 = "#1b9e77",
                random_effects_sigmaR_constant = "#d95f02",
                fixed_effects                  = "#7570b3")
  panels <- list(
    SSB         = list(truth = true_ssb, ylab = "SSB"),
    F           = list(truth = true_F,   ylab = "Fully-selected F"),
    recruitment = list(truth = true_R,   ylab = "Recruitment (age-1 N)"))

  pdf_path <- file.path(here, sprintf("demo_rceattle_vs_om_%s.pdf", case))
  grDevices::pdf(pdf_path, width = 11, height = 4)
  graphics::par(mfrow = c(1, 3), mar = c(4, 4.5, 3, 1), oma = c(0, 0, 2, 0))
  for (key in names(panels)) {
    truth <- panels[[key]]$truth
    yall  <- c(truth, unlist(lapply(series, `[[`, key)))
    graphics::plot(yrs, truth, type = "l", lwd = 3, col = "black",
                   ylim = range(yall, na.rm = TRUE), xlab = "Year",
                   ylab = panels[[key]]$ylab, main = panels[[key]]$ylab)
    for (scn in names(series)) {
      graphics::lines(yrs, series[[scn]][[key]], col = scen_col[scn],
                      lwd = 2, lty = 2)
    }
    if (key == "SSB") {
      graphics::legend(
        "bottomleft", bty = "n", cex = 0.9,
        legend = c("OM (truth)", paste("RCEATTLE:", names(series))),
        col    = c("black", scen_col[names(series)]),
        lwd    = c(3, rep(2, length(series))),
        lty    = c(1, rep(2, length(series))))
    }
  }
  graphics::mtext(
    sprintf("RCEATTLE estimation model vs operating-model truth  (%s, replicate %d)",
            case, sim),
    outer = TRUE, cex = 1.0, font = 2)
  grDevices::dev.off()

  message(sprintf("\nWrote comparison figure: %s", pdf_path))
  message("  Thick BLACK solid line = OM (truth); coloured DASHED lines = RCEATTLE scenarios (see legend).")
}

