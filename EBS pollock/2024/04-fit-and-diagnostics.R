# =============================================================================
# EBS pollock 2024 -- fit and run the standard diagnostic suite
# =============================================================================
# Convergence checks, data/fit/residual plots, OSA residuals, retrospectives
# (Mohn's rho), jitters, a self-test, and a likelihood profile.
# https://grantdadams.github.io/Rceattle/articles/model-diagnostics.html
#
# Run from the "EBS pollock" project root.
#
# Reads:   Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx (or the 05- roll-forward)
# Writes:  nothing; console tables and interactive plots
# Prereq:  "01-build-data.R"
#
# The fit repeats the two-stage optimization of "03-model-comparison.R": analytical
# survey q leaves the scale weakly identified, so fishery selectivity is started
# from the data and the time-varying deviations are switched on only after a base
# fit pins the scale.
# =============================================================================

library(Rceattle)
library(ggplot2)

XLSX          <- "Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx"  # or the 2025 roll-forward
n_selages_fsh <- 12

# Data ----
est   <- read_data(XLSX)
styr  <- est$styr; endyr <- est$endyr; yrs <- styr:endyr; nyr <- length(yrs)
M1Fun <- build_M1(updateM1 = TRUE, M1_model = "fixed")
ctl   <- fit_control(verbose = 1, phase = TRUE,
                     bias_adjust_proc = 0, bias_adjust_obs = 0, comp_offset = 1e-3)

# Empirical fishery-selectivity start ----
# Mean observed fishery age composition / numbers-at-age (a throwaway default
# fit), normalised and log-centred — the selectivity shape the catch data imply.
fsh <- est$fleet_control$Fleet_code[est$fleet_control$Fleet_name == "Fishery"]
m0  <- fit_mod(data_list = est, inits = NULL, file = NULL, estimateMode = 0,
               random_rec = FALSE, msmMode = 0, initMode = "NonEquilibrium", M1Fun = M1Fun,
               fit_control = ctl)
N   <- m0$quantities$N_at_age[1, 1, , 1:nyr]
cd  <- est$comp_data[est$comp_data$Fleet_code == fsh & est$comp_data$Year > 0 &
                     est$comp_data$Age0_Length1 == 0, ]
cc  <- grep("^Comp_", colnames(cd), value = TRUE)[1:est$nages]
sy  <- matrix(NA_real_, nrow(cd), est$nages)
for (i in seq_len(nrow(cd))) {
  yi <- which(yrs == cd$Year[i]); if (!length(yi)) next
  pa <- as.numeric(cd[i, cc]); pa <- pa / sum(pa, na.rm = TRUE)
  s  <- pa / pmax(N[, yi], 1e-8); sy[i, ] <- s / max(s, na.rm = TRUE)
}
sel_bar <- colMeans(sy, na.rm = TRUE)[1:n_selages_fsh]
ls      <- log(pmax(sel_bar / max(sel_bar), 1e-3)); ls <- ls - mean(ls)
inits   <- build_params(est)
inits$sel_coff[1, 1, 1:n_selages_fsh] <- ls

# Two-stage fit ----
est_A <- est
est_A$fleet_control$Time_varying_sel <- "Off"          # base selectivity only (pin scale)
ebs_stage1 <- fit_mod(data_list = est_A,
                 inits = inits,
                 file = NULL,
                 estimateMode = 0,
                 random_rec = FALSE,
                 msmMode = 0,
                 initMode = "NonEquilibrium",
                 M1Fun = M1Fun,
                 fit_control = ctl)

ebs_2024   <- fit_mod(data_list = est,
                 inits = ebs_stage1$obj$env$parList(),
                 file = NULL,
                 estimateMode = 0,
                 random_rec = FALSE,
                 msmMode = 0,
                 initMode = "NonEquilibrium",
                 M1Fun = M1Fun, fit_control = ctl)

# Diagnostics ----
# * Summaries ----
summary(ebs_2024)
convergence_diagnostics(ebs_2024)               # gradient / Hessian conditioning / bounds

# * Plots ----
plot_data(est)                             # data coverage
plot_index(ebs_2024)
plot_logindex(ebs_2024)
plot_indexresidual(ebs_2024)
plot_comp(ebs_2024)
plot_catch(ebs_2024)
plot_selectivity(ebs_2024)

# * OSA residuals ----
osa <- osa_residuals(ebs_2024)
head(osa)
osa_diagnostics(osa)                       # SDNR + lower/upper tail (Stewart & Monnahan 2025)
plot(osa)                                  # Q-Q (with SDNR/tail annotation) + residual-by-year

# * Retrospectives ----
# Slow (each peel is a full refit). Peels refit single-stage, so one may settle in the
# early-period local optimum from "03-model-comparison.R"; inspect the peel trajectories.
ebs_2024_retro <- retrospective(Rceattle = ebs_2024, peels = 5)
ebs_2024_retro$mohns                            # Mohn's rho per quantity
plot_biomass(ebs_2024_retro$Rceattle_list)

# * Jitters ----
ebs_2024_jitters <- jitter(Rceattle = ebs_2024, njitter = 50, phase = TRUE)
hist(log(ebs_2024_jitters$nll - min(ebs_2024_jitters$nll)),
     main = "Jitter NLL spread (log scale)", xlab = "log(NLL - min NLL)")
plot_biomass(ebs_2024_jitters$Rceattle_list) + theme(legend.position="none")    # tight overlap => stable optimum

# * Self-test ----
ebs_2024_sims <- self_test(ebs_2024, nsim = 50)
length(ebs_2024_sims)                           # simulations that converged (non-converged dropped)

plot_biomass(c(ebs_2024_sims, list(ebs_2024)), line_col = c(rep("grey", length(ebs_2024_sims)), 1)) + theme(legend.position="none")

# * Likelihood profile ----
# Profile age-3+ M, not age 1. The ADMB schedule fixes M at 0.9 / 0.45 / 0.3 for
# ages 1 / 2 / 3-15, and only the age-3+ value is identified by these data: it
# profiles to a clean minimum at 0.35, putting the assumed 0.30 about 0.05 nll
# from the optimum. Age-1 M is not identified -- its profile runs monotonically
# to whichever endpoint you choose (~2 nll units across 0.2-1.3), because age-1
# fish barely enter the fitted data once BTS_1 is folded into the BTS comps.
# The range must bracket the assumed value or the profile only reports its own
# endpoint.
prof_M1 <- profile(fitted = ebs_2024, param = "M1",
                  slots = list(c(1, 1, 3)), # sp, sex, age
                  values = list(seq(0.15, 0.50, by = 0.05)))
plot(prof_M1$grid$slot_1, prof_M1$nll - min(prof_M1$nll, na.rm = TRUE),
     type = "l", xlab = "M (ages 3-15)", ylab = "dNLL")
