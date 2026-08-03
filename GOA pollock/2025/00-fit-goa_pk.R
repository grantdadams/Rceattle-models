# =============================================================================
# GOA pollock 2025 -- fit goa_pk (Cole's model), original and corrected
#
# Produces the two goa_pk objects every downstream 2025 script depends on:
#
#   Data/2024pollock.Rdata       -> `fit`  goa_pk as published (23d: 2024 final)
#   Data/2024pollock_mfix.Rdata  -> `fit`  goa_pk with corrections A1-A6
#
# The corrections are the ones listed in the RECONCILIATION LOG at the top of
# "03-model.R". Three are source-side and live in the tagged copy of the model
# (reference/goa_pk_2024_mfix.cpp, every change marked "GRANT"):
#
#   A1. Initial age-structure M-index bug fix (exp(-M(j+1)) -> exp(-M(j))).
#   A3. Removed the q1/q2 random-walk penalties (~527 units of normalizing
#       constant on deviates that are pinned at 0).
#   A4. Lognormal bias correction (-sd^2/2) on total catch.
#
# and three are data-side and are applied here:
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
# Cole's package is loaded from a source checkout rather than installed, so his
# working tree is never touched: the dat file and both model sources are copied
# into Data/goa_pk_2024/ and compiled there.
#
# Run from the "GOA pollock" project root. Compilation needs Rtools.
# =============================================================================

library(TMB)
library(dplyr)

GOAPOLLOCK_SRC <- "C:/Users/grant.adams/GitHub/AFSC assessments/GOApollock"
BUILD          <- "Data/goa_pk_2024"
DATFILE        <- "pk24_12.txt"
VERSION        <- "23d: 2024 final"

if (!requireNamespace("GOApollock", quietly = TRUE)) {
  if (!dir.exists(GOAPOLLOCK_SRC))
    stop("GOApollock is not installed and the source checkout is missing: ", GOAPOLLOCK_SRC)
  pkgload::load_all(GOAPOLLOCK_SRC, quiet = TRUE)
} else {
  library(GOApollock)
}

# ---- Stage the build directory ---------------------------------------------
# Cole's dat file + his unmodified source, plus our tagged copy. Keeping both
# .cpp files side by side means `diff` shows exactly what the corrections did.
dir.create(BUILD, showWarnings = FALSE, recursive = TRUE)
stage <- function(from, to) {
  if (!file.exists(from)) stop("missing input: ", from)
  file.copy(from, file.path(BUILD, to), overwrite = TRUE)
}
stage(file.path(GOAPOLLOCK_SRC, "data/2024", DATFILE), DATFILE)
stage(file.path(GOAPOLLOCK_SRC, "source/goa_pk.cpp"),  "goa_pk.cpp")
stage("reference/goa_pk_2024_mfix.cpp",                "goa_pk_mfix.cpp")

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

# ---- (2) goa_pk corrected (A1-A6) ------------------------------------------
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

message("Fitting goa_pk (corrected, A1-A6) ...")
fit <- fit_pk(input, getsd = TRUE, filename = NULL, verbose = FALSE)
save(fit, file = "Data/2024pollock_mfix.Rdata")
message("  Wrote Data/2024pollock_mfix.Rdata   marginal ", round(fit$opt$objective, 4),
        " | conditional ", round(-sum(fit$rep$loglik), 4))

# ---- Summary ---------------------------------------------------------------
# The conditional column (-sum(rep$loglik), deviates at their modes) is the
# like-for-like number Rceattle reproduces; the marginal is the Laplace
# objective and the two models integrate different sets of random effects.
cat("\n== goa_pk fits ==\n")
print(data.frame(
  model       = c("original", "corrected (A1-A6)"),
  marginal    = round(c(fit_orig$opt$objective, fit$opt$objective), 4),
  conditional = round(c(-sum(fit_orig$rep$loglik), -sum(fit$rep$loglik)), 4),
  max_grad    = signif(c(fit_orig$opt$max_gradient, fit$opt$max_gradient), 3)))
