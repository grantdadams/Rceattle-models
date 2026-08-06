# =============================================================================
# GOA pollock 2025 -- fit goa_pk (Cole's model), original and corrected
#
# Produces the two goa_pk objects every downstream 2025 script depends on:
#
#   Data/2024pollock.Rdata       -> `fit`  goa_pk as published (23d: 2024 final)
#   Data/2024pollock_mfix.Rdata  -> `fit`  goa_pk with corrections A1-A9
#
# The corrections are listed in the RECONCILIATION LOG at the top of
# "03-model-comparison.R". These live in the tagged copy of the model
# (2025/goa_pk/goa_pk_mfix.cpp, every change marked "GRANT"):
#
#   A1. Initial age-structure M-index bug fix (exp(-M(j+1)) -> exp(-M(j))).
#   A3. Removed the q1/q2 random-walk penalties (~527 units of normalizing
#       constant on deviates that are pinned at 0).
#   A4. Lognormal bias correction (-sd^2/2) on total catch.
#   A8. Removed the descending fishery selectivity random-walk penalties
#       (149.4338 of constant on deviates mapped off at 0; same case as A3).
#   A9. Removed the selectivity priors on mapped-off limbs (srv2 descending,
#       srv6 ascending), which goa_pk already omits for srv1 and srv3.
#
# the rest are data- or map-side and are applied here:
#
#   A2. Constant q3 random-walk SD (0.05 for all years, replacing the per-year
#       0.001 / 0.05 vector) -- Rceattle has one RW SD per fleet.
#   A5. Aging-error matrix rows normalized to sum to 1.
#   A6. Composition observations normalized to sum to 1 (pk24_12.txt rows sum
#       to ~1.00001; Rceattle normalizes every comp row in rearrange_data()).
#
# Also applied to BOTH fits, straight from Cole's data/2024/run_assessment.R:
#   * age-1 / age-2 Shelikof index catchabilities mapped off (log_q4/log_q5);
#   * srv1 / srv3 / srv6 composition ESS doubled.
#
# The model sources (Cole's original goa_pk.cpp + our corrected goa_pk_mfix.cpp)
# and the dat file (pk24_12.txt) are VENDORED in 2025/goa_pk/, so this folder is
# standalone. Only Cole's GOApollock *package* (prepare_pk_input / read_dat /
# fit_pk) is an external dependency. The staged sources are copied into
# Data/goa_pk_2024/ and compiled there (his working tree is never touched).
#
# Run needs a C++ toolchain (Rtools on Windows). GOApollock is not on CRAN:
#   remotes::install_github("kaskr/TMBhelper")            # hard dependency
#   remotes::install_github("afsc-assessments/GOApollock")
# or set the GOAPOLLOCK_SRC env var to a local source checkout to load_all().
# =============================================================================

library(TMB)
library(dplyr)
setwd("~/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA pollock")

GOAPOLLOCK_SRC <- Sys.getenv("GOAPOLLOCK_SRC", unset = NA)  # optional local checkout
VENDOR         <- "2025/goa_pk"        # vendored model sources + dat (standalone)
BUILD          <- "Data/goa_pk_2024"   # scratch build dir (gitignored)
DATFILE        <- "pk24_12.txt"
VERSION        <- "23d: 2024 final"

# GOApollock package (functions only). Prefer the installed package; else load a
# source checkout via GOAPOLLOCK_SRC.
if (requireNamespace("GOApollock", quietly = TRUE)) {
  library(GOApollock)
} else if (!is.na(GOAPOLLOCK_SRC) && dir.exists(GOAPOLLOCK_SRC)) {
  pkgload::load_all(GOAPOLLOCK_SRC, quiet = TRUE)
} else {
  stop("GOApollock is not installed. Install it (see header) or set the ",
       "GOAPOLLOCK_SRC environment variable to a source checkout.")
}

# ---- Stage the build directory from the vendored inputs --------------------
# Cole's original source + our tagged corrected copy side by side, so `diff`
# shows exactly what the corrections did.
dir.create(BUILD, showWarnings = FALSE, recursive = TRUE)
stage <- function(from, to) {
  if (!file.exists(from)) stop("missing input: ", from)
  file.copy(from, file.path(BUILD, to), overwrite = TRUE)
}
stage(file.path(VENDOR, DATFILE),           DATFILE)           # dat file
stage(file.path(VENDOR, "goa_pk.cpp"),      "goa_pk.cpp")      # Cole's original source
stage(file.path(VENDOR, "goa_pk_mfix.cpp"), "goa_pk_mfix.cpp") # corrected (A1/A3/A4)

# ---- Shared input ----------------------------------------------------------
# Mirrors data/2024/run_assessment.R.
base_input <- function(modfile) {
  x <- prepare_pk_input(path = BUILD, datfile = DATFILE,
                        version = VERSION, complike = "D-M",
                        modfile = modfile)
  # age-1 / age-2 Shelikof indices are off, so don't estimate their catchability
  x$map$log_q4 <- x$map$log_q5 <- factor(NA)
  # ESS bumped so the log_DM pars don't hit their bounds
  x$dat$multN_srv1 <- x$dat$multN_srv1 * 2
  x$dat$multN_srv3 <- x$dat$multN_srv3 * 2
  x$dat$multN_srv6 <- x$dat$multN_srv6 * 2
  x
}

# ---- (1) goa_pk as published ------------------------------------------------
message("Fitting goa_pk (original) ...")
fit <- fit_pk(base_input("goa_pk"), getsd = TRUE, filename = NULL, verbose = FALSE)
save(fit, file = "Data/2024pollock.Rdata")
message("  Wrote Data/2024pollock.Rdata   marginal ", round(fit$opt$objective, 4),
        " | conditional ", round(-sum(fit$rep$loglik), 4))
fit_orig <- fit

# ---- (2) goa_pk corrected (A1-A9) ------------------------------------------
input <- base_input("goa_pk_mfix")

# A2: single random-walk SD on q3. pk24_12.txt carries 0.001 in most years and
# 0.05 in a few; Rceattle has one RW SD per fleet, so flatten to 0.05.
input$dat$q3_rwlk_sd[] <- 0.05

# A5: aging-error rows sum to ~0.9999 over ages 5-8 in the dat file.
input$dat$age_trans <- input$dat$age_trans / rowSums(input$dat$age_trans)

# A6: composition observations sum to ~1.00001. Normalize the age and length
# comps for every fleet, skipping all-zero rows (turned-off observations).
renorm <- function(m) {
  s <- rowSums(m)
  ok <- is.finite(s) & s > 0
  m[ok, ] <- m[ok, ] / s[ok]
  m
}
comp_mats <- c("catp", "srvp1", "srvp2", "srvp3", "srvp6",
               "lenp", "srvlenp1", "srvlenp2", "srvlenp3", "srvlenp6")
for (nm in comp_mats) input$dat[[nm]] <- renorm(input$dat[[nm]])

# A7: fix the same end of the fishery ascending random walk that Rceattle does.
# goa_pk fixes the MEAN (log_slp1_fsh_mean, inf1_fsh_mean) and estimates all 55
# deviates; Rceattle's rw() fixes the FIRST deviate and estimates the mean. Both
# reach the same year-by-year selectivity with the same 55 free parameters, and
# the walk penalty is identical because it sees only successive differences -- so
# the two are already equivalent for the likelihood. Where they differ is the
# prior: the template carries dnorm(log_slp1_fsh_mean, -1, 1.5) and
# dnorm(inf1_fsh_mean, 0, 3), matching the Rceattle selectivity priors, but a
# prior on a mapped-off parameter is an inert constant. Freeing the mean and
# fixing the first deviate activates it, so the two models become identical
# rather than equivalent-up-to-that-prior.
ndev <- length(input$pars$slp1_fsh_dev)
input$map$log_slp1_fsh_mean <- factor(1)
input$map$inf1_fsh_mean     <- factor(1)
input$map$slp1_fsh_dev <- factor(c(NA, seq_len(ndev - 1L)))
input$map$inf1_fsh_dev <- factor(c(NA, seq_len(ndev - 1L)))
input$pars$slp1_fsh_dev[1] <- 0
input$pars$inf1_fsh_dev[1] <- 0

message("Fitting goa_pk (corrected, A1-A9) ...")
fit <- fit_pk(input, getsd = TRUE, filename = NULL, verbose = FALSE)
save(fit, file = "Data/2024pollock_mfix.Rdata")
message("  Wrote Data/2024pollock_mfix.Rdata   marginal ", round(fit$opt$objective, 4),
        " | conditional ", round(-sum(fit$rep$loglik), 4))
fit_mfix <- fit   # keep a handle to the corrected fit for the summary below

# ---- (3) goa_pk corrected, sigmaR ESTIMATED --------------------------------
# For like-for-like uncertainty with Rceattle (which estimates the recruitment-
# process SD): refit with sigmaR estimated and rec devs integrated (Laplace,
# GOApollock's estSigR path). Lands on ~1.016, matching Rceattle's R_sd.
input_estSigR <- input
input_estSigR$map$sigmaR <- factor(1)          # free the recruitment-process SD
# Append, don't replace: the input already integrates the Shelikof Ecov latent,
# and overwriting dropped it, leaving that latent a fixed effect here while
# Rceattle integrates it -- not a like-for-like uncertainty comparison.
input_estSigR$random <- union(input_estSigR$random, "dev_log_recruit")
message("Fitting goa_pk (corrected, sigmaR estimated) ...")
fit <- fit_pk(input_estSigR, getsd = TRUE, filename = NULL, verbose = FALSE)
save(fit, file = "Data/2024pollock_mfix_estSigR.Rdata")
message("  Wrote Data/2024pollock_mfix_estSigR.Rdata   sigmaR_hat ",
        round(fit$parList$sigmaR, 4), " | marginal ", round(fit$opt$objective, 4))
fit_estSigR <- fit

# ---- Summary ---------------------------------------------------------------
# The conditional column (-sum(rep$loglik), deviates at their modes) is the
# like-for-like number Rceattle reproduces; the marginal is the Laplace
# objective and the two models integrate different sets of random effects.
cat("\n== goa_pk fits ==\n")
print(data.frame(
  model       = c("original", "corrected (A1-A9)", "corrected, sigmaR est."),
  marginal    = round(c(fit_orig$opt$objective, fit_mfix$opt$objective,
                        fit_estSigR$opt$objective), 4),
  conditional = round(c(-sum(fit_orig$rep$loglik), -sum(fit_mfix$rep$loglik),
                        -sum(fit_estSigR$rep$loglik)), 4),
  max_grad    = signif(c(fit_orig$opt$max_gradient, fit_mfix$opt$max_gradient,
                         fit_estSigR$opt$max_gradient), 3)))
