# =============================================================================
# 2024 EBS pollock — model diagnostics
# =============================================================================
# Fits the EBS pollock ADMB-bridge model and runs the Rceattle diagnostics:
# convergence checks, data/fit/residual plots, OSA residuals, retrospectives
# (Mohn's rho), jitters, a self-test, and a likelihood profile.
# https://grantdadams.github.io/Rceattle/articles/model-diagnostics.html
#
# The fit is the two-stage optimization of the comparison script ("2024 EBS
# pollock.R") -- analytical survey q leaves the scale weakly identified, so the
# fishery selectivity is started from the data and the time-varying deviations are
# switched on only after a base fit pins the scale.
# =============================================================================

library(Rceattle)

XLSX          <- "Data/2024_EBS_pollock_m23_rceattle_full.xlsx"  # or the 2025 roll-forward
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
mod_A <- fit_mod(data_list = est_A,
                 inits = inits,
                 file = NULL,
                 estimateMode = 0,
                 random_rec = FALSE,
                 msmMode = 0,
                 initMode = "NonEquilibrium",
                 M1Fun = M1Fun,
                 fit_control = ctl)

mod   <- fit_mod(data_list = est,
                 inits = mod_A$obj$env$parList(),
                 file = NULL,
                 estimateMode = 0,
                 random_rec = FALSE,
                 msmMode = 0,
                 initMode = "NonEquilibrium",
                 M1Fun = M1Fun, fit_control = ctl)

# Diagnostics ----
# * Summaries ----
summary(mod)
convergence_diagnostics(mod)               # gradient / Hessian conditioning / bounds

# * Plots ----
plot_data(est)                             # data coverage
plot_index(mod)
plot_logindex(mod)
plot_indexresidual(mod)
plot_comp(mod)
plot_catch(mod)
plot_selectivity(mod)

# * OSA residuals ----
osa <- osa_residuals(mod)
head(osa)
osa_diagnostics(osa)                       # SDNR + lower/upper tail (Stewart & Monnahan 2025)
plot(osa)                                  # Q-Q (with SDNR/tail annotation) + residual-by-year

# * Retrospectives ----
# Slow (each peel is a full refit). Peels refit single-stage, so one may settle in the
# early-period local optimum from "2024 EBS pollock.R"; inspect the peel trajectories.
mod_retro <- retrospective(Rceattle = mod, peels = 5)
mod_retro$mohns                            # Mohn's rho per quantity
plot_biomass(mod_retro$Rceattle_list)

# * Jitters ----
mod_jitter <- jitter(Rceattle = mod, njitter = 50, phase = TRUE)
hist(log(mod_jitter$nll - min(mod_jitter$nll)),
     main = "Jitter NLL spread (log scale)", xlab = "log(NLL - min NLL)")
plot_biomass(mod_jitter$Rceattle_list)     # tight overlap => stable optimum

# * Self-test ----
mod_sims <- self_test(mod, nsim = 50)
length(mod_sims)                           # simulations that converged (non-converged dropped)
plot_biomass(c(list(mod), mod_sims),
             model_names = c("fit", names(mod_sims)))

# * Likelihood profile ----
prof_M <- profile(fitted = mod, param = "M1",
                  slots = list(c(1, 1, 1)), # sp, sex, age
                  values = list(seq(0.20, 0.50, by = 0.025)))
plot(prof_M$grid$slot_1, prof_M$nll - min(prof_M$nll, na.rm = TRUE),
     type = "l", xlab = "M1", ylab = "dNLL")
