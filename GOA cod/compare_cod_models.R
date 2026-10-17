# Compare the SS3-bridged GOA Pacific cod model against Stock Synthesis.
#
# Five states, because the Rceattle states answer to DIFFERENT SS3 runs and
# scoring them all against one is the easiest mistake to make here:
#
#   1. SS3, re-estimated      the age-0-selected variant at its own MLE
#                             (TOTAL 2075.78, 330 estimated parameters)
#   2. Rceattle, optimised    Rceattle's own optimum on the same data, 330 free.
#                             Its Hessian is indefinite, so it has NO standard
#                             errors -- see Bridging/pin_uninformed_sel.R
#   3. SS3, all params fixed  the run whose parameter values the bridge injects
#                             (TOTAL 2128.82). Its ctl and dat are BYTE-identical
#                             to (1); the only difference is starter.ss
#                             last_estimation_phase, 0 against 20, so it
#                             estimated nothing at all and its ss3.par carries a
#                             stale objective from the run that wrote it
#   4. Rceattle, forward pass SS3's parameters from (3) evaluated WITHOUT
#                             optimising -- the check that the two models
#                             compute the same population from the same inputs
#   5. Rceattle, pinned       (2) with the 23 selectivity coefficients the data
#                             do not inform held at SS3's values: 307 free, and
#                             the only Rceattle state that returns standard
#                             errors (307 of 307, pdHess TRUE)
#
# (4) is differenced against (3); (2) and (5) against (1). Scoring (4) against
# (1) reads ~13% on SSB, which is the gap between two SS3 runs rather than
# anything about Rceattle.
#
# Both SS3 runs are the age-0-selected variant: SS3's `age_selex_pattern = 10`
# zeroes age-0 selectivity and Rceattle has no analogue, so the comparable SS3
# model is the one with that removed (Bridging/Estimation_Differences.md #19).
#
# ---------------------------------------------------------------------------
# EVERY DIFFERENCE FROM THE 2024 GOA PACIFIC COD ASSESSMENT
#
# None of the five states below is the assessment. `Data/goa_pcod` is, at 196
# estimated parameters and SSB 2024 = 102,580 mt. Read any number here as code
# verification, not as catch advice.
#
# A. CHANGES MADE TO SS3, so Rceattle could be compared at all
#   1. F_Method 3 (hybrid) -> 2 (explicit annual F). Rceattle has no hybrid F
#      solver, so F becomes 134 estimated parameters: 196 -> 330. No effect on
#      the fit, a large one on any parameter count.
#   2. age_selex_pattern 10 -> 0 on all five length-selected fleets. SS3's
#      pattern 10 zeroes age-0 selectivity; Rceattle has no per-fleet AGE
#      multiplier to compose with a length-based curve (#19). Costs SS3 23.80
#      nats, and it is the modification that destroys SS3's own Hessian:
#      restoring pattern 10 and nothing else returns a full 247,463-line
#      covar.sso and lifts L_at_Amin off its bound to 1.3007. Worth +6.1% SSB
#      and +11.1% recruitment.
#   3. like_comp 18 (InitEQ_Regime) lambda -> 0, i.e. SS3's soft penalty on the
#      initial-level shift turned off. Worth ~15.8 nats, and most of the
#      remaining gap: ~18% SSB and ~31% recruitment. NOT a clean measurement --
#      goa_pcod_caal_lambda_on also differs in starter.ss init_values_src (0
#      against 1), so part of that may be a different local optimum.
#   4. CAAL Lbin_lo/Lbin_hi rewritten as the population bin numbers each 5 cm
#      data bin spans. This is a CORRECTION, not a simplification: SS3 <= 3.30.24
#      truncates the non-integer values the assessment file carries, fitting
#      every CAAL row one bin low (CAAL-length-bin-defect.md).
#   5. SS3 version: the assessment is v3.30.22.1, the bridge's runs v3.30.25.1.
#
# Net effect of 2 + 3 on the quota-relevant numbers:
#   SSB 2024   assessment 102,580 | lambda_on 102,131 | age-0 variant 89,856
#   SSB 1977   assessment 162,823 | lambda_on 147,420 | age-0 variant  76,051
# So the variant scored against here sits 12% below the assessment on terminal
# SSB and 53% below on initial SSB, before Rceattle is involved at all.
#
# B. WHAT RCEATTLE CANNOT EXPRESS (the residual, after A)
#   1. No age-selectivity multiplier alongside a length-based curve (#19) -- the
#      reason for A2, and the one open feature.
#   2. SS3's prior on the 2014-16 M block applies to the block's ABSOLUTE M;
#      Rceattle's parameter is a log offset from an estimated base, so the
#      equivalent prior is on a sum of two estimated parameters, which the
#      linkage table cannot express (#22). 0.894 nats.
#   3. No `Parm_softbounds` analogue. 0.013 nats.
#   4. No equilibrium-catch concept. Inert for GOA (its row is 0), live for AI.
#   5. CAAL carries no per-observation month; every row is predicted at the
#      fleet's own month.
#   6. Bounds differ. SS3 declares a finite box on all 330; Rceattle leaves
#      log(R0), both log q, both growth SDs and every selectivity coefficient
#      unbounded, and is far wider where it is finite (F to 22026 against 1.5,
#      rec_dev +/-15 against +/-5). Only Linf matches exactly. At the optimum 4
#      of 40 block coefficients sit outside SS3's box.
#
# C. CHOICES MADE IN THIS BRIDGE, not forced by either code
#   1. State 5 holds 23 top_logit/descend_se coefficients the data do not inform
#      (Bridging/pin_uninformed_sel.R). Without it no Rceattle state here has
#      standard errors.
#   2. L1 is left FREE and settles on its 1e-3 bound, where SS3 gets 1.3007.
#      Deliberate, so the comparison keeps estimating what SS3 estimates -- but
#      it moves length at age 1 from 11.00 to 9.82 cm and ages 2-3 by ~1 cm,
#      which shifts the age-length key. Do not take a growth curve from here.
#   3. Catch SDs inflated 10% (0.05 -> 0.055) to reproduce SS3's catch kernel,
#      log(1.1 obs) - log(pred + 0.1 obs), which is not a plain lognormal.
#   4. RCE_INIT_PRIOR=false, matching A3 rather than the assessment.
#
# D. WHAT IS NOT COMPARED HERE AT ALL
#   Reference points. Ftarget and Flimit are both exactly 1 in every Rceattle
#   state -- the placeholder for a quantity this configuration never estimated
#   -- so nothing below speaks to F40%, F35% or B40%.
# ---------------------------------------------------------------------------
#
#   Rscript compare_cod_models.R          # fits, then writes the figure
#   (state 5 needs Bridging/_pinned_sel_fit.rds -- see pin_uninformed_sel.R)
#   RCE_REFIT=true Rscript compare_cod_models.R   # ignore the cached fit
#
# The optimisation takes several minutes; the fit is cached beside the figure so
# the plotting can be iterated on without re-fitting.

# ---- 0. Configuration -------------------------------------------------------
# Set here rather than left to the caller. Every one of these changes the model,
# and a comparison run with one of them wrong looks like a result rather than a
# mistake. Override from the environment only deliberately.
.cfg <- c(
  # The SS3 run supplying parameter VALUES. Not the re-estimated one: its L1 is
  # 4.93e-08, below Rceattle's 1e-3 floor, so a forward pass there is refused
  # outright with "Initial parameter values are not within bounds".
  RCE_SS3_DIR       = "Data/goa_pcod_caal_bins_fixed_age0",
  # The SS3 run supplying PHASES, i.e. what SS3 held fixed. Read from the
  # re-estimated run, because the run above estimated nothing.
  RCE_SS3_PHASE_DIR = "Data/goa_pcod_caal_bins_fixed_age0_est",
  RCE_SEL_PARITY    = "true",    # SS3's own blocks and devs, not a per-year design
  RCE_FIX_LINKAGE   = "false",   # estimate the selectivity coefficients
  RCE_SEL_DEV_ALL   = "true",    # all 63 deviates, matching SS3's count
  RCE_SEL_REPLACE   = "true",    # SS3 blocks REPLACE the base; see #24
  # SS3's ctl sets lambda = 0 on InitEQ_Regime, so it estimates the initial
  # level and charges nothing for it. Leaving Rceattle's prior on costs 4.13
  # nats SS3 never pays and moves the optimum 7.59.
  RCE_INIT_PRIOR    = "false")
for (.k in names(.cfg)) if (!nzchar(Sys.getenv(.k))) do.call(Sys.setenv, as.list(.cfg[.k]))

suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))

# ---- 1. Build the model and the forward pass --------------------------------
# Sourcing the bridge leaves `cod`, `inits`, `ss3_map`, the process specs and
# `fp` (estimateMode = 3, i.e. state 4) in the global environment.
source("Bridging/ss3_to_ceattle_forward_pass.R")

# ---- 2. Rceattle's own optimum ----------------------------------------------
# Warm-started from SS3's MLE, which is the bridge's default and NOT a cold
# start: it asks "is SS3's solution also Rceattle's", not "does Rceattle find it
# unaided".
.cache <- "Bridging/_compare_cod_models.rds"
if (file.exists(.cache) && !identical(tolower(Sys.getenv("RCE_REFIT")), "true")) {
  cod_fit <- readRDS(.cache)
  cat("\nreusing the cached fit (RCE_REFIT=true to re-fit)\n")
} else {
  cod_fit <- Rceattle::fit_mod(
    data_list = cod, inits = inits, map = ss3_map, estimateMode = 1,
    initMode = INIT_MODE, growthFun = growthFun_spec, M1Fun = M1_block,
    recFun = recFun_spec, selFun = selFun_spec, qFun = qFun_spec,
    random_rec = FALSE, msmMode = 0,
    fit_control = fit_control(phase = FALSE, verbose = 0, bias_adjust_obs = FALSE))
  saveRDS(cod_fit, .cache)
}

# ---- 3. The two SS3 runs -----------------------------------------------------
.rd <- function(d) r4ss::SS_output(d, covar = FALSE, verbose = FALSE,
                                   printstats = FALSE, forecast = FALSE, warn = FALSE)
ss3_est <- .rd(Sys.getenv("RCE_SS3_PHASE_DIR"))   # re-estimated
ss3_fix <- .rd(Sys.getenv("RCE_SS3_DIR"))         # selectivity-fixed

# ---- 4. Assemble and compare -------------------------------------------------
yrs <- cod$styr:cod$endyr
.q  <- function(x, q) as.numeric(x$quantities[[q]][1, seq_along(yrs)])
# SS3 reports SSB and recruitment on the same rows; `pred_recr` is the realised
# recruitment, not the stock-recruit expectation.
.s3 <- function(o) { r <- o$recruit[match(yrs, o$recruit$Yr), ]
                     list(SSB = r$SpawnBio, R = r$pred_recr) }
E <- .s3(ss3_est); W <- .s3(ss3_fix)

# State 5, the pinned fit, is the only one of the Rceattle states that returns
# standard errors. Built by Bridging/pin_uninformed_sel.R; read from its cache
# if present, because refitting it here would duplicate that script.
.pin <- "Bridging/_pinned_sel_fit.rds"
cod_pin <- if (file.exists(.pin)) readRDS(.pin)$pinned else NULL
if (is.null(cod_pin))
  cat("\nNOTE Bridging/_pinned_sel_fit.rds absent, so state 5 is omitted.",
      "\n     Run `Rscript Bridging/pin_uninformed_sel.R` to produce it.\n")

LAB <- c("SS3 re-estimated (own MLE, 330 free)",
         sprintf("Rceattle optimised (%d free)", length(cod_fit$obj$par)),
         "SS3 all parameters fixed (the values the bridge injects)",
         "Rceattle forward pass at those values",
         if (!is.null(cod_pin))
           sprintf("Rceattle, uninformed selectivity held (%d free, has SEs)",
                   length(cod_pin$obj$par)))
d <- bind_rows(
  data.frame(Year = yrs, SSB = E$SSB,           R = E$R,           src = LAB[1], rS = E$SSB, rR = E$R),
  data.frame(Year = yrs, SSB = .q(cod_fit,"ssb"), R = .q(cod_fit,"R"), src = LAB[2], rS = E$SSB, rR = E$R),
  data.frame(Year = yrs, SSB = W$SSB,           R = W$R,           src = LAB[3], rS = W$SSB, rR = W$R),
  data.frame(Year = yrs, SSB = .q(fp,"ssb"),    R = .q(fp,"R"),    src = LAB[4], rS = W$SSB, rR = W$R),
  # Differenced against the re-estimated run, like state 2: it is that model
  # with 23 directions removed, not a different SS3 comparison.
  if (!is.null(cod_pin))
    data.frame(Year = yrs, SSB = .q(cod_pin,"ssb"), R = .q(cod_pin,"R"),
               src = LAB[5], rS = E$SSB, rR = E$R)) %>%
  mutate(SSB_pct = 100 * (SSB - rS) / rS, R_pct = 100 * (R - rR) / rR,
         src = factor(src, levels = LAB))

# `Report.sso` prints components to 6 significant figures, so the total it
# implies is ~0.004 out -- a tenth of the gap measured here. `ss3.par`'s header
# carries full precision, but only for a run SS3 actually estimated: the
# selectivity-fixed run never was, so its par file is a leftover from the model
# that wrote it (2051.98, the pattern-10 total) and its Report is the honest read.
.ss3_par_obj <- function(d) as.numeric(sub(".*Objective function value = ([0-9.]+).*",
                                           "\\1", readLines(file.path(d, "ss3.par"), 1L)))
cat("\n=== objectives ===\n")
print(data.frame(
  state = LAB,
  objective = c(.ss3_par_obj(Sys.getenv("RCE_SS3_PHASE_DIR")), cod_fit$opt$objective,
                ss3_fix$likelihoods_used["TOTAL", "values"], fp$quantities$jnll,
                if (!is.null(cod_pin)) cod_pin$opt$objective),
  free = c(330L, length(cod_fit$obj$par), 330L, length(fp$obj$par),
           if (!is.null(cod_pin)) length(cod_pin$obj$par)),
  standard_errors = c("330 (SS3)", "none - Hessian indefinite", "none - not estimated",
                      "n/a - not optimised",
                      if (!is.null(cod_pin))
                        sprintf("%d of %d", sum(is.finite(summary(cod_pin$sdrep, "fixed")[, 2])),
                                length(cod_pin$obj$par)))),
  row.names = FALSE, digits = 12)
cat("\n=== state, against each series' own SS3 reference (%) ===\n")
print(as.data.frame(d %>% group_by(state = src) %>% summarise(
  SSB_mean = mean(SSB_pct), SSB_max_abs = max(abs(SSB_pct)),
  R_mean = mean(R_pct), R_max_abs = max(abs(R_pct)), .groups = "drop")),
  row.names = FALSE, digits = 3)

# ---- 5. Figure ---------------------------------------------------------------
long <- bind_rows(
  d %>% transmute(Year, src, value = SSB / 1000, panel = "SSB (1000 t)"),
  d %>% transmute(Year, src, value = R / 1e6,    panel = "Recruitment (billions)"),
  d %>% transmute(Year, src, value = SSB_pct,    panel = "SSB, % from its own SS3 reference"),
  d %>% transmute(Year, src, value = R_pct,      panel = "Recruitment, % from its own SS3 reference"))
long$panel <- factor(long$panel, levels = c("SSB (1000 t)", "Recruitment (billions)",
  "SSB, % from its own SS3 reference", "Recruitment, % from its own SS3 reference"))

p <- ggplot(long, aes(Year, value, colour = src, linetype = src)) +
  geom_hline(data = data.frame(panel = factor(levels(long$panel)[3:4], levels(long$panel))),
             aes(yintercept = 0), colour = "grey70", linewidth = 0.3, inherit.aes = FALSE) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ panel, scales = "free_y", ncol = 2) +
  # SS3 states dark, Rceattle states coloured; the forward pass dashed because it
  # lies on top of the SS3 line it is differenced against. #542788 completes the
  # PuOr pair with #e08214 rather than introducing a new hue family.
  scale_colour_manual(
    values = setNames(c("grey15", "#1b6ca8", "#8c6d31", "#e08214", "#542788")[seq_along(LAB)], LAB),
    name = NULL) +
  scale_linetype_manual(
    values = setNames(c("solid", "solid", "solid", "22", "solid")[seq_along(LAB)], LAB),
    name = NULL) +
  labs(x = NULL, y = NULL,
       title = "GOA Pacific cod: SS3 against the Rceattle bridge, 1977-2024",
       subtitle = sprintf(paste0(
         "Each Rceattle series is differenced against the SS3 run it corresponds to. The forward pass ",
         "(jnll %.4f) lies on\nthe selectivity-fixed SS3 line; the optimum (%.4f, warm-started from ",
         "SS3) is Rceattle's own solution."),
         fp$quantities$jnll, cod_fit$opt$objective)) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top", legend.direction = "vertical",
        plot.subtitle = element_text(size = 8, colour = "grey30"),
        panel.grid.minor = element_blank())
ggsave("Bridging/_cod_bridge_comparison.png", p, width = 10.5, height = 7.4, dpi = 150)
cat("\nwrote Bridging/_cod_bridge_comparison.png\n")
