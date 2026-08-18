# =============================================================================
# EBS pollock 2024 -- build and run the ADMB reference model
# =============================================================================
# Rebuilds the "pm" (AMAK) reference that "02-bridge.R" and "03-model-comparison.R"
# read. That model is the 2024 SAFE assessment with the m23_rceattle edits applied
# so its equations and likelihoods match Rceattle's; the edits are flagged
# "MODIFIED (m23_rceattle...)" in pm.tpl and catalogued as S1-S4 / L1-L7 in
# "03-model-comparison.R".
#
# Run from the "EBS pollock" project root.
#
# Reads:   ADMB/m23_rceattle_full/{pm.tpl, pm.dat, control.dat}
#          ADMB/data/{pm_24.dat, selvar24.dat, ...}  (pm.dat is the starter file)
# Writes:  ADMB/m23_rceattle_full/{pm.rep, pm.par, pm.std, ...}
#
# The run outputs are committed, so the rest of the pipeline works without an
# ADMB toolchain installed. This script only re-derives them; if admb is not on
# PATH it reports that and leaves the committed files untouched.
# =============================================================================

ADMB_DIR <- "ADMB/m23_rceattle_full"   # stage 2: likelihood, data, parameter alignment
EXEC     <- "pm"
ARGS     <- c("-nox", "-iprint", "150")
VERSION  <- "m23_rceattle_full: 2024 final"

stopifnot(dir.exists(ADMB_DIR))

# The whole body runs inside a function so that on.exit() has a frame to attach
# to. At the top level of a script on.exit() either never fires (Rscript) or
# fires immediately (source()), and either way the working directory ends up
# wrong for every script that follows.
fit_admb <- function(admb_dir = ADMB_DIR, exec = EXEC, args = ARGS) {

  owd <- setwd(admb_dir)          # ADMB writes into its own directory
  on.exit(setwd(owd), add = TRUE)

  # Toolchain ----
  # admb wraps the tpl -> cpp -> executable build. Without it the committed
  # pm.rep / pm.par stand as the reference and the downstream scripts are
  # unaffected. Note the committed "pm" binary is macOS/arm64 -- on Windows the
  # rebuild is the only way to get a runnable executable.
  if (!nzchar(Sys.which("admb"))) {
    message("admb not found on PATH -- skipping the rebuild.\n",
            "  Using the committed ", admb_dir, "/pm.rep and pm.par as the reference.\n",
            "  To rebuild by hand:\n",
            "    cd ", admb_dir, "\n",
            "    admb ", exec, "\n",
            "    ", exec, " ", paste(args, collapse = " "))
  } else {

    # Build ----
    # admb returns 0 even when it fails, so check for the executable itself.
    # The committed "pm" is a macOS/arm64 binary and is useless on Windows.
    bin <- if (.Platform$OS.type == "windows") paste0(exec, ".exe") else exec
    message("Building ", VERSION, " ...")
    system2("admb", exec)
    if (!file.exists(bin))
      stop("admb did not produce ", bin, " in ", admb_dir,
           " -- check the ADMB toolchain (its library must match the ",
           "installed compiler).")

    # Run ----
    # -nox suppresses the per-iteration gradient dump; -iprint 150 reports every
    # 150 iterations. Convergence is judged from the maximum gradient in pm.par.
    message("Running ", bin, " ", paste(args, collapse = " "), " ...")
    if (system2(file.path(".", bin), args) != 0)
      stop(bin, " did not run to completion")

    message("Wrote ", admb_dir, "/pm.rep and pm.par")
  }

  # Report the reference objective and maximum gradient so a rebuild is
  # verifiable against the previous one at a glance. Runs inside admb_dir.
  if (file.exists("pm.par")) message("pm.par: ", readLines("pm.par", n = 1))

  invisible(NULL)
}

fit_admb()
