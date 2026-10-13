# Compare the SS3-bridged GOA Pacific cod model against Stock Synthesis.
#
# Four states, because the two Rceattle states answer to DIFFERENT SS3 runs and
# scoring both against one of them is the easiest mistake to make here:
#
#   1. SS3, re-estimated      the age-0-selected variant at its own MLE
#                             (TOTAL 2075.78, 330 estimated parameters)
#   2. Rceattle, optimised    Rceattle's own optimum on the same data
#   3. SS3, selectivity-fixed the run whose parameter values the bridge injects
#                             (TOTAL 2128.82; its selectivity was never re-fit
#                             for the age-0 structure, so its values differ)
#   4. Rceattle, forward pass SS3's parameters from (3) evaluated WITHOUT
#                             optimising -- the check that the two models
#                             compute the same population from the same inputs
#
# (4) is differenced against (3) and (2) against (1). Scoring (4) against (1)
# reads ~13% on SSB, which is the gap between two SS3 runs rather than anything
# about Rceattle.
#
# Both SS3 runs are the age-0-selected variant: SS3's `age_selex_pattern = 10`
# zeroes age-0 selectivity and Rceattle has no analogue, so the comparable SS3
# model is the one with that removed (Bridging/Estimation_Differences.md #19).
#
#   Rscript compare_cod_models.R          # fits, then writes the figure
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

LAB <- c("SS3 re-estimated (own MLE, 330 free)",
         sprintf("Rceattle optimised (%d free)", length(cod_fit$obj$par)),
         "SS3 selectivity-fixed (the values the bridge injects)",
         "Rceattle forward pass at those values")
d <- bind_rows(
  data.frame(Year = yrs, SSB = E$SSB,           R = E$R,           src = LAB[1], rS = E$SSB, rR = E$R),
  data.frame(Year = yrs, SSB = .q(cod_fit,"ssb"), R = .q(cod_fit,"R"), src = LAB[2], rS = E$SSB, rR = E$R),
  data.frame(Year = yrs, SSB = W$SSB,           R = W$R,           src = LAB[3], rS = W$SSB, rR = W$R),
  data.frame(Year = yrs, SSB = .q(fp,"ssb"),    R = .q(fp,"R"),    src = LAB[4], rS = W$SSB, rR = W$R)) %>%
  mutate(SSB_pct = 100 * (SSB - rS) / rS, R_pct = 100 * (R - rR) / rR,
         src = factor(src, levels = LAB))

cat("\n=== objectives ===\n")
print(data.frame(
  state = LAB,
  objective = c(ss3_est$likelihoods_used["TOTAL", "values"], cod_fit$opt$objective,
                ss3_fix$likelihoods_used["TOTAL", "values"], fp$quantities$jnll),
  free = c(330L, length(cod_fit$obj$par), 330L, NA_integer_)), row.names = FALSE, digits = 10)
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
  scale_colour_manual(values = setNames(c("grey15", "#1b6ca8", "#8c6d31", "#e08214"), LAB),
                      name = NULL) +
  scale_linetype_manual(values = setNames(c("solid", "solid", "solid", "22"), LAB), name = NULL) +
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
