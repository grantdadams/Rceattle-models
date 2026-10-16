# Run the SS3-bridged GOA Pacific cod model in Rceattle.
#
# Everything the model needs is built by the bridge's forward-pass script, which
# reads the SS3 files and returns three things into the global environment:
#
#   cod      the Rceattle data_list (1977-2024, 1 species, 10 fleets, env_data)
#   inits    SS3's MLE as an Rceattle parameter list (54 blocks)
#   ss3_map  SS3's pinning -- the parameters SS3 held at a negative phase, plus
#            the factor-shared selectivity block deviates
#
# Run it from this folder ("GOA cod"), not from Rceattle-models.
#
#   Rscript run_goa_cod.R
#
# Two environment variables select what you build against:
#
#   RCEATTLE_PKG   which Rceattle checkout to load (default ../../Rceattle).
#                  The TMB DLL must already be built THERE -- build it in that
#                  checkout, because recompiling one another session is using
#                  breaks that session's runs.
#   RCE_SS3_DIR    which SS3 run to read (default Data/goa_pcod_caal_lambda_on).
#                  Use Data/goa_pcod_caal_bins_fixed_age0 to compare against the
#                  numbers in ../SS3-bridge/GOA-estimation-parity.md; the SS3
#                  runs differ, and mixing them is the easiest way to think a
#                  number has moved when it has not.
#
# For estimation parity against SS3, set all six variables listed at step 2b --
# four of them change the model, and RCE_INIT_PRIOR=false is the one most easily
# forgotten. The defaults here build the model, not the comparison.

# ---- 1. Build the model ------------------------------------------------------
# This prints a long injection log (selectivity bases, F columns, what SS3
# pinned). Wrap in suppressMessages() if you do not want it.
source("Bridging/ss3_to_ceattle_forward_pass.R")

cat("\ndata_list:", cod$styr, "-", cod$endyr,
    " projyr", cod$projyr, " fleets", nrow(cod$fleet_control), "\n")

# ---- 2a. Forward pass: evaluate AT SS3's MLE, do not optimise ----------------
# `fp` is already this, built by the script above with estimateMode = 3. Its
# objective is the number to compare against SS3.
cat("forward pass at SS3's MLE:", sprintf("%.4f", fp$quantities$jnll), "\n")

# Per-component, beside SS3. `residual` is the gap AFTER each density's additive
# constant, and only that column is a difference in fit. Do NOT compare the raw
# rows: Catch carries -265.5188 and the index likelihoods differ from SS3's by a
# lognormal constant.
source("../SS3-bridge/parity_check.R")

# Score the LIKELIHOOD against the age-0-SELECTED SS3 variant, not the raw
# Report.sso rows. SS3's age_selex_pattern = 10 zeroes age-0 selectivity and
# Rceattle has no analogue (Bridging/Estimation_Differences.md #19), so the two
# are different models at age 0; the patched variant is the comparable one. Its
# components are already saved -- no SS3 binary needed -- at its own MLE,
# L1 = 1.3007. Without this the Length_comp residual reads ~+76 nats, which is
# that model difference and not a difference in fit.
.scan <- readRDS("../SS3-bridge/_ss3_L1_scan_goa_age0.rds")
.age0 <- .scan[which.min(abs(.scan$L1 - 1.3007)), ]
ss3_nll_age0 <- c(Length_comp = .age0$Length_comp, Age_comp = .age0$Age_comp,
                  Catch = .age0$Catch, Survey = .age0$Survey,
                  Recruitment = .age0$Recruitment)

parity <- parity_report(fp, ss3_rep,
                        fixed_in_ss3 = if (exists("fixed_in_ss3")) fixed_in_ss3
                                       else character(),
                        ss3_nll = ss3_nll_age0)

# ---- 2b. Free fit: let Rceattle find its own optimum -------------------------
# Same data, same process specs, same SS3 pinning -- only estimateMode changes.
#
# DO NOT READ THIS BLOCK'S OBJECTIVE AS PARITY WITH SS3. The forward pass in 2a
# is bridged (24/24 gates, all five likelihood components within 0.0013 nats);
# estimation is NOT, because the two models do not parameterise time-varying
# selectivity the same way. SS3's blocks REPLACE the base parameter (Blk_Fxn = 2,
# labels `..._BLK2repl_1990`); Rceattle's linkage ADDS to it. With the default
# settings below most sel_dn6 entries are held at SS3's values and the fit is
# conditional on them; add RCE_SEL_FREE_BASE=true and the base goes free but
# directions become non-identifiable, because base and block are confounded
# wherever blocks tile the series. Neither is a like-for-like comparison, and
# both report convergence FAIL with a non-invertible Hessian.
#
# Use compare_cod_models.R for the parity numbers rather than this block: under
# RCE_SEL_REPLACE both sides estimate 330 parameters and the optima agree to
# 0.036 nats net of two penalties SS3 charges that Rceattle cannot express.
# See ../SS3-bridge/GOA-estimation-parity.md, measured 2026-10-15.
#
#   RCE_SS3_DIR="Data/goa_pcod_caal_bins_fixed_age0"
#   RCE_SS3_PHASE_DIR="Data/goa_pcod_caal_bins_fixed_age0_est"
#   RCE_SEL_PARITY=true RCE_FIX_LINKAGE=false RCE_SEL_DEV_ALL=true
#   RCE_INIT_PRIOR=false
#
# RCE_INIT_PRIOR=false is NOT optional for a comparison against SS3. The
# bridge's ctl sets lambda = 0 on SS3's InitEQ_Regime, so SS3 estimates the
# initial-level shift and charges nothing for it; leaving Rceattle's prior on
# costs 4.13 nats SS3 does not pay and moves the optimum 7.59 nats. Note that
# lambda row is the bridge's, not the 2024 assessment's, which penalises it.
#
# Without those settings this is a DIFFERENT MODEL from SS3 and its objective
# means nothing against SS3's: unmapped, `beta_linkage` leaves far more
# selectivity directions free than SS3 estimates and the fit overfits. The
# forward pass in 2a is unaffected either way -- estimateMode = 3 builds
# without optimising, so a free parameter sitting at SS3's injected value
# contributes what SS3's value contributes.
cod_fit <- Rceattle::fit_mod(
  data_list    = cod,
  inits        = inits,          # warm start from SS3's MLE
  map          = ss3_map,
  estimateMode = 1,              # 1 = hindcast only; use 0 to add the projection
  initMode     = INIT_MODE,
  growthFun    = growthFun_spec,
  M1Fun        = M1_block,
  recFun       = recFun_spec,
  selFun       = selFun_spec,
  qFun         = qFun_spec,
  random_rec   = FALSE,
  msmMode      = 0,
  fit_control  = fit_control(phase = FALSE, verbose = 1,
                             bias_adjust_obs = FALSE)
)
cat("\nRceattle's own optimum:", sprintf("%.4f", cod_fit$opt$objective),
    "  max|grad|", sprintf("%.3g", cod_fit$opt$max_gradient), "\n")

# ss3_rep is saved too: without it nothing downstream can re-derive a per-fleet
# or per-age comparison from this file, which is most of what the result is for.
saveRDS(list(cod = cod, inits = inits, ss3_map = ss3_map, ss3_rep = ss3_rep,
             ss3_dir = SS3_DIR, fp = fp, cod_fit = cod_fit, parity = parity),
        "Bridging/_run_goa_cod.rds")


# ---- 3. Using this cod model in GOA_circlulation_study -----------------------
# `cod` is a plain Rceattle data_list, so it drops straight into the circulation
# study in place of the `pcoddata` read from the xlsx. Three things differ from
# that object and need handling before the DSEM models will mean the same thing:
#
#   * YEARS. This is 1977-2024 against the study's own window. The study trims
#     each stock's env_data to its own styr, because a linkage design matrix
#     aligns by ROW POSITION -- a covariate series starting before styr shifts
#     every linkage forward. Re-run that trim against THIS styr.
#
#   * env_data. The bridge already attaches an env_data (it carries SS3's
#     environmental q link for LLSrv). The study's covariates must be joined
#     onto it with `full_join(envdata, by = "Year")` -- naming the `by` is not
#     optional, a bare join matches on every shared column and duplicates every
#     year, which silently breaks the DSEM's AR(1) GMRF.
#
#   * random_rec. The forward pass fits with random_rec = FALSE. A DSEM makes
#     the recruitment deviations the latent states of a GMRF, so it needs
#     random_rec = TRUE, and `proj_mean_rec = FALSE` in build_srr() if you want
#     the DSEM rather than mean recruitment to supply the forecast.
#
# Sketch, with the study's own SEM:
#
#   coddata <- cod
#   coddata$projyr <- coddata$endyr
#   coddata$env_data <- coddata$env_data %>%
#     full_join(envdata, by = "Year") %>% arrange(Year) %>%
#     filter(Year >= coddata$styr)
#
#   cod_dsem <- fit_mod(
#     data_list    = coddata,
#     estimateMode = 0, random_rec = TRUE, msmMode = 0, initMode = INIT_MODE,
#     growthFun = growthFun_spec, M1Fun = M1_block, selFun = selFun_spec,
#     qFun = qFun_spec,
#     recFun = build_srr(srr_fun = 0, proj_mean_rec = FALSE),
#     dsem   = build_DSEM(sem = codsem, family = "fixed", sigmaR_prior_sd = 0.5),
#     fit_control = fit_control(verbose = 1, phase = TRUE))
#
# Note this drops `map = ss3_map`: the study estimates its own model rather than
# reproducing SS3's, so holding SS3's selectivity blocks fixed is not wanted.
