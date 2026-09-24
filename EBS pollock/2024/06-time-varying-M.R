# =============================================================================
# EBS pollock 2024 -- time-varying natural mortality
# =============================================================================
# Two choices about M, crossed, on the "04-fit-and-diagnostics.R" configuration:
#
#   level      the assumed age schedule (0.9 / 0.45 / 0.3 for ages 1 / 2 / 3-15)
#              vs ONE estimated age-invariant M
#   deviation  none, IID year deviations, or AR1 year deviations
#
#   M1(age, year) = M1_level(age) * exp(eps_year)
#
# eps_y is a random-effect linkage on M1, integrated out by the Laplace
# approximation: IID is eps_y ~ N(0, sigma^2), AR1 is a stationary AR1 with
# marginal sd sigma and correlation rho. The leading "0 +" drops the linkage
# intercept, so eps_y is a zero-mean deviation about the level rather than a
# second, confounded, estimate of it. env_data supplies the year grid. This
# workbook's env_data is 42 rows, 1981-2024, and fit_mod() prepends 1964-1980
# with NA covariates (with a message) to reach styr -- fine for a `(1 | Year)`
# term, which needs only the grouping level, but a FIXED covariate starting
# after styr is rejected rather than rescued. Projection years past the last row
# get a zero offset, so M returns to its level for the reference points.
#
# The level is build_M1(M1_model = ): "fixed" holds log_M1 at the input
# schedule, "sex_age_invariant" shares one log_M1 across every age and
# estimates it -- the age structure of M is dropped, and the fit starts from
# the geometric mean of the schedule (0.332), because TMB averages the values
# of the parameters a map collapses onto one level.
#
# The deviations are NOT the M1_re switch. build_map_m1() frees log_M1_dev only
# when M1_model is 1 or 2, so build_M1(M1_model = "fixed", M1_re = "iid_year")
# maps out every deviation -- but it does NOT reduce to the fixed-M model, which
# would at least be harmless. `map_list$M1_dev_log_sd[sp, ] <- sp` sits outside
# that M1_model guard (R/3-build_map.R), so the sd stays free: one extra
# estimated parameter scoring a density against a vector of zeros, worth 56.06
# in the "M random effects" jnll row (= 61 * (log 1 + log(2*pi)/2)) and
# minimised by driving sigma to its bound. The objective is then not comparable
# with anything. Avoid that combination; the linkage works against either level,
# which is what lets the two choices cross here.
#
# HEADLINE, AND IT IS A NEGATIVE ONE ABOUT THE METHOD, NOT ABOUT M. sigma_M
# goes to the zero boundary at the 0.50 fishery sd "04-" uses (1.8e-06) and at
# the 0.15 here (2.3e-06, AR1 rho = 0), with the marginal likelihood equal to
# the fixed-M one to every digit. That is NOT evidence that M is constant,
# because the estimator returns the same answer when M is known to vary: see
# "Can a nonzero sigma_M be recovered at all?" at the end of this script.
# Simulating from sigma_M = 0.20 and refitting with sigma free returns
# sigma_hat ~ 1.4e-06, uncorrelated with the deviations that generated the data,
# on simulated observations that differ from the originals by thousands of
# units. Four replicates, all the same.
#
# So this configuration has NO POWER to detect time-varying M, and every zero
# below is uninformative rather than a finding. Free recruitment deviations,
# an F pinned by tightly-fit catch, and a 12-age x 61-year nonparametric fishery
# selectivity random walk absorb the signal before it reaches sigma_M.
# Tightening the fishery walk does not recover it; SEL_SD_GRID goes to 0.05 for
# that reason. Do not report "EBS pollock shows no time-varying M" off this
# script -- the defensible statement is "these data, under this configuration,
# cannot detect it either way".
#
# Run from the "EBS pollock" project root.
#
# Reads:   Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx
# Writes:  nothing; console tables and interactive plots
# Prereq:  "01-build-data.R"
#
# The base fit repeats the two-stage optimization of "03-model-comparison.R" and
# "04-fit-and-diagnostics.R": analytical survey q leaves the scale weakly
# identified, so fishery selectivity is started from the data and the
# time-varying deviations are switched on only after a base fit pins the scale.
# =============================================================================

library(Rceattle)
library(ggplot2)

XLSX          <- "Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx"
n_selages_fsh <- 12
# The fishery random-walk selectivity sd. "04-" uses 0.50; tightening it to 0.15
# moves 2024 SSB from 3.52 to 4.32 million t (+23%) on its own, before M is
# touched at all -- so treat it as an assumption of this script, not a nuisance
# setting.
FSH_SEL_SD    <- 0.15
M_PRIOR_SD    <- 0.20   # lognormal sd (log scale) for the M prior; your call


# Data ----
est   <- read_data(XLSX)
est$diet_data <- NULL
styr  <- est$styr; endyr <- est$endyr; yrs <- styr:endyr; nyr <- length(yrs)
ctl   <- fit_control(verbose = 1, phase = TRUE,
                     bias_adjust_proc = 0, bias_adjust_obs = 0, comp_offset = 1e-3)
fsh   <- est$fleet_control$Fleet_code[est$fleet_control$Fleet_name == "Fishery"]

# Year deviations on M1, constant across ages, integrated out. `by = ~ species`
# is the canonical form and is a no-op in this one-species model.
# `rw(1 | Year)` gives a non-stationary random walk on the same footing; its
# sigma is the innovation (step) sd, not the marginal, so it would not be
# comparable with the AR1 sigma.
#
# THE INTERCEPT IS NOT COSMETIC. M's level is estimated only when M1_model
# frees log_M1 AND the formula carries an intercept. map_linkage_adjuster()
# masks the base parameter for a slope-only (no-intercept) group -- so that a
# `~ 0 + temp` covariate offsets a FIXED base -- and masking is one-way. Pair a
# no-intercept formula with M1_model = "sex_age_invariant" and log_M1 is mapped
# out with no message: M1 reverts to the input 0.9 / 0.45 / 0.3 schedule and the
# fit is the fixed-M one to every digit, not the age-invariant M you asked for.
# So:
#   ~ 0 + ...  with M1_model = "fixed"              level held at the schedule
#   ~ 1 + ...  with M1_model = "sex_age_invariant"  level estimated, flat in age
# (`~ 1 + ...` under M1_model = "fixed" is the first case again -- the base is
# already mapped out, and an intercept row cannot unmask it. The intercept row
# routes to log_M1 rather than adding a second level parameter, so there is no
# double counting either way.)
dev_iid_fixed <- linkage_spec(~ 0 + (1 | Year),    by = ~ species)
dev_ar1_fixed <- linkage_spec(~ 0 + ar1(1 | Year), by = ~ species)
dev_iid_est   <- linkage_spec(~ 1 + (1 | Year),    by = ~ species)
dev_ar1_est   <- linkage_spec(~ 1 + ar1(1 | Year), by = ~ species)

# AND EXPECT THE ESTIMATED-LEVEL DEVIATION MODELS NOT TO IDENTIFY. A free level
# plus a deviation field whose realized mean is free is over-parameterised by
# one: log_M1 and mean(eps) move against each other along a ridge, and only the
# IID/AR1 density's weak pull toward zero distinguishes them. Measured on
# `M estimated + IID` here: log_M1 slid to 0.0098 against its 0.001 lower bound
# while sigma inflated to 1.54 and the deviations rose to absorb it (mean +0.46,
# max +3.87, i.e. exp() = 48x), sdreport failed, and max|grad| finished at
# 3.1e+11 on log_F. `M estimated + AR1` fails the same way, less violently:
# max|grad| 10188, sdreport again NULL. Their nlls (948.3 and 944.7) are the
# lowest in the table and are not results -- do not score them.
#
# Break the ridge before believing anything from these two: put a prior on the
# level (M1_use_prior = TRUE, as `M1_mean_prior` below does) or pin sigma the
# way the profile section does. They are fitted here because the crossing is
# what was asked for, and because the failure is worth seeing.

# * M level crossed with deviation structure -- six models ----
M1_fixed      <- build_M1(updateM1 = TRUE, M1_model = "fixed")
M1_fixed_iid  <- build_M1(updateM1 = TRUE, M1_model = "fixed",
                          linkages = list(M1 = dev_iid_fixed))
M1_fixed_ar1  <- build_M1(updateM1 = TRUE, M1_model = "fixed",
                          linkages = list(M1 = dev_ar1_fixed))
# updateM1 = FALSE on the estimated-level models. `updateM1 = TRUE` rebuilds
# log_M1 from M1_base INSIDE fit_mod(), after `inits` have been accepted
# (R/6-fit_mod.R), so it silently discards a warm-started level: the deviations
# arrive conditioned on the source fit's M while M itself is reset to the
# schedule. Harmless where the level is fixed (it is the schedule either way),
# wrong where the level is a parameter being carried forward.
M1_mean       <- build_M1(updateM1 = FALSE, M1_model = "sex_age_invariant")
M1_mean_iid   <- build_M1(updateM1 = FALSE, M1_model = "sex_age_invariant",
                          linkages = list(M1 = dev_iid_est))
M1_mean_ar1   <- build_M1(updateM1 = FALSE, M1_model = "sex_age_invariant",
                          linkages = list(M1 = dev_ar1_est))

# The same estimated M, given a lognormal prior. Freed with no prior, M runs to
# 0.053 -- interior (the bounds are 0.001-2), 35 nll units better than the
# assumed schedule, and it takes 2024 SSB from 4.32 to 2.27 million t (-47%).
# That is far below anything this stock supports, and a large move in the number
# that sets the quota, bought with one freed parameter: the M/scale confound the
# "04-" header warns about, arriving as a number rather than as a failure.
#
# Anchor the prior on the geometric mean of the assumed schedule -- what an
# age-invariant M is standing in for, and where M1_model = 1 starts anyway.
# Under M1_model = 1 the prior is evaluated exactly once (ceattle.cpp pins
# nage_tmp = nsex_tmp = 1 for that mode), so there is no hidden 15x from the
# collapsed age loop.
#
# `M_prior` is NOT quite the natural-scale mean, despite what ?build_M1 says.
# The template forms the log-mean as log(M_prior) + sd^2/2, where a lognormal
# with natural mean m needs log(m) - sd^2/2. So the prior sits slightly high:
# at M_PRIOR_SD = 0.20 its median is 0.338 and its mean 0.345 against the 0.332
# passed in (+2% / +4%), and at build_M1's own default sd of 0.35 the mean is
# 13% high. Small next to the effect being measured here, but know where the
# anchor actually is before quoting it.
M_PRIOR <- exp(mean(log(as.numeric(est$M1_base[1, (1:est$nages) + 2]))))   # 0.332
M1_mean_prior <- build_M1(updateM1 = FALSE, M1_model = "sex_age_invariant",
                          M1_use_prior = TRUE,
                          M_prior = M_PRIOR, M_prior_sd = M_PRIOR_SD)


# Helpers ----
# * Fishery selectivity sd ----
# `Time_varying_sel_sd` is the STARTING value of the `sel_dev_log_sd` PARAMETER,
# which build_map() holds fixed unless `random_sel` is on. A warm start reads
# the parameter, not the column, so editing the column alone is a silent no-op:
# the fit runs, converges, and reports the old sd. Set both -- the column so a
# fresh build and every refit diagnostic (retrospective, jitter) agree, and the
# parameter so this fit uses it.
set_sel_sd <- function(data_list, inits, fleet, sd) {
  data_list$fleet_control$Time_varying_sel_sd[fleet] <- sd
  inits$sel_dev_log_sd[fleet] <- log(sd)
  list(data_list = data_list, inits = inits)
}

# * Warm start across a change in linkage structure ----
# A fit with no linkages carries a zero-length `beta_linkage_re` /
# `log_sigma_linkage`, and fit_mod() refuses `inits` whose blocks do not match
# the parameters `data_list` implies. So build the linkage model's own parameter
# skeleton (estimateMode = "DebugBuild" builds without optimizing) and copy the
# source fit's values into every block of the same length.
warm_start <- function(data_list, M1Fun, from) {
  skel <- suppressWarnings(fit_mod(
    data_list = data_list, inits = NULL, file = NULL, estimateMode = "DebugBuild",
    random_rec = FALSE, msmMode = 0, initMode = "NonEquilibrium", M1Fun = M1Fun,
    fit_control = fit_control(verbose = 0, phase = FALSE, getsd = FALSE)))$estimated_params
  src <- from$estimated_params
  for (nm in intersect(names(skel), names(src))) {
    if (length(unlist(skel[[nm]])) == length(unlist(src[[nm]]))) skel[[nm]] <- src[[nm]]
  }
  skel
}

# * What did each fit actually free? ----
# The masking above is silent, so check rather than assume: a level that was
# mapped out is an absent row, not an error. Read the LEVEL off `log_M1` rather
# than off M1_at_age, so the answer does not depend on which year you look at
# once the deviations are on. `M_free` counts free log_M1 entries in the
# reduced parameter vector: 0 when the map holds it, 1 when one age-invariant M
# is estimated. `n_dev` and `n_sigma` also count the M1_re switch's parameters,
# so the table stays honest if that path is used instead. `n_rho` and `struct`
# separate AR1 from IID: without them the two are identical rows, and
# `init = list(rho = )` silently maps trans_rho_linkage out, demoting an AR1 to
# a fixed-rho model with nothing in the counts to show it.
m_structure <- function(models) {
  do.call(rbind, lapply(names(models), function(nm) {
    f   <- models[[nm]]; np <- names(f$obj$env$par)
    lvl <- exp(as.numeric(f$estimated_params$log_M1[1, 1, ]))
    data.frame(Model     = nm,
               M_free    = sum(np == "log_M1"),
               n_dev     = sum(np == "beta_linkage_re") + sum(np == "log_M1_dev"),
               n_sigma   = sum(np == "log_sigma_linkage") + sum(np == "M1_dev_log_sd"),
               n_rho     = sum(np == "trans_rho_linkage") + sum(np == "M1_rho"),
               struct    = {
                 lt <- f$data_list$linkage_table
                 if (is.null(lt) || !nrow(lt)) "-" else
                   paste(unique(stats::na.omit(lt$re_struct)), collapse = ",")
               },
               M_age1    = lvl[1],
               M_age3    = lvl[3],
               M_flat    = isTRUE(all.equal(max(lvl), min(lvl))),
               row.names = NULL)
  }))
}

# * Two-stage fit ----
two_stage <- function(data_list, M1Fun, inits) {
  d0 <- data_list
  d0$fleet_control$Time_varying_sel <- "Off"          # base selectivity only (pin scale)
  s1 <- fit_mod(data_list = d0, inits = inits, file = NULL, estimateMode = "Estimate",
                random_rec = FALSE, msmMode = 0, initMode = "NonEquilibrium",
                M1Fun = M1Fun, fit_control = ctl)
  fit_mod(data_list = data_list, inits = s1$estimated_params, file = NULL,
          estimateMode = "Estimate", random_rec = FALSE, msmMode = 0,
          initMode = "NonEquilibrium", M1Fun = M1Fun, fit_control = ctl)
}


# Empirical fishery-selectivity start ----
# Mean observed fishery age composition / numbers-at-age (a throwaway default
# fit), normalised and log-centred -- the selectivity shape the catch data imply.
m0  <- fit_mod(data_list = est, inits = NULL, file = NULL, estimateMode = "Estimate",
               random_rec = FALSE, msmMode = 0, initMode = "NonEquilibrium",
               M1Fun = M1_fixed, fit_control = ctl)
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


# Fits ----
# * The "04-" model, fishery selectivity sd 0.50 ----
ebs_2024 <- two_stage(est, M1_fixed, inits)

# * Same model at the tightened fishery selectivity sd ----
# The reference the two time-varying-M models are compared against: it differs
# from them only in M, so the likelihood difference is the M model alone.
tight   <- set_sel_sd(est, ebs_2024$estimated_params, fsh, FSH_SEL_SD)
est_tv  <- tight$data_list

m_fixed <- fit_mod(data_list = est_tv, inits = tight$inits, file = NULL,
                   estimateMode = "Estimate", random_rec = FALSE, msmMode = 0,
                   initMode = "NonEquilibrium", M1Fun = M1_fixed, fit_control = ctl)

# * Year deviations on the assumed schedule ----
# Each starts from the model it nests: the AR1 nests the IID at rho = 0, so the
# IID optimum is an interior start rather than a corner.
m_iid <- fit_mod(data_list = est_tv, inits = warm_start(est_tv, M1_fixed_iid, m_fixed),
                 file = NULL, estimateMode = "Estimate", random_rec = FALSE,
                 msmMode = 0, initMode = "NonEquilibrium", M1Fun = M1_fixed_iid,
                 fit_control = ctl)

m_ar1 <- fit_mod(data_list = est_tv, inits = warm_start(est_tv, M1_fixed_ar1, m_iid),
                 file = NULL, estimateMode = "Estimate", random_rec = FALSE,
                 msmMode = 0, initMode = "NonEquilibrium", M1Fun = M1_fixed_ar1,
                 fit_control = ctl)

# * One estimated age-invariant M, then the same two deviation structures ----
# M and survey q trade off against each other, and q here is analytical, so the
# scale is weakly identified before M is freed. These three estimate M with
# nothing holding it: read them as what the data alone say, and read them next
# to the prior fit below, not on their own. The "04-" profile of age-3+ M put
# the data's optimum near 0.35, about 0.05 nll from the assumed 0.30 -- that is
# a profile at the assumed age SHAPE, and it is not what an age-invariant M
# converges to once the shape is dropped.
m_mean <- fit_mod(data_list = est_tv, inits = warm_start(est_tv, M1_mean, m_fixed),
                  file = NULL, estimateMode = "Estimate", random_rec = FALSE,
                  msmMode = 0, initMode = "NonEquilibrium", M1Fun = M1_mean,
                  fit_control = ctl)

m_mean_iid <- fit_mod(data_list = est_tv, inits = warm_start(est_tv, M1_mean_iid, m_mean),
                      file = NULL, estimateMode = "Estimate", random_rec = FALSE,
                      msmMode = 0, initMode = "NonEquilibrium", M1Fun = M1_mean_iid,
                      fit_control = ctl)

m_mean_ar1 <- fit_mod(data_list = est_tv, inits = warm_start(est_tv, M1_mean_ar1, m_mean_iid),
                      file = NULL, estimateMode = "Estimate", random_rec = FALSE,
                      msmMode = 0, initMode = "NonEquilibrium", M1Fun = M1_mean_ar1,
                      fit_control = ctl)

# * Estimated M with a prior on it ----
# Not a competitor in the AIC table -- a prior is not a free parameter and the
# objective now carries a penalty the others do not. It is here to say how far
# M moves when it is asked to stay near the schedule it is replacing, and what
# that costs in SSB.
#
# Measured: M = 0.144, 2024 SSB 2.78 million t, and the "M prior" jnll row at 8.50.
# That is 4.3 prior SDs below the prior mean -- the data are not being nudged,
# they are being fought. Do not read it as "M is really 0.14". Two things are
# confounded in that number and neither is mean M:
#   - M against the scale, since survey q is analytical here; and
#   - the age SHAPE, since an age-invariant M drops the schedule's 0.9 at age 1
#     to the same value as every other age. Much of the 35 nll the free fit buys
#     is the age-1 assumption being relaxed, absorbed by recruitment, and not
#     evidence about the level.
# "Estimate the level, keep the age shape" is NOT expressible here, so do not go
# looking for it. A linkage intercept always routes to log_M1 rather than
# becoming a free multiplier, so under M1_model = "fixed" `~ 1` estimates
# nothing at all (measured: log_M1 free = 0, beta_linkage free = 0, M1 still
# 0.9 / 0.45 / 0.3). The only modes that free a level are M1_model 1 (one flat
# M) and 3 (every age free) -- neither holds the schedule's shape while scaling
# it. Estimating the level therefore costs the shape, and the 35 nll is buying
# both at once.
m_mean_prior <- fit_mod(data_list = est_tv, inits = warm_start(est_tv, M1_mean_prior, m_fixed),
                        file = NULL, estimateMode = "Estimate", random_rec = FALSE,
                        msmMode = 0, initMode = "NonEquilibrium", M1Fun = M1_mean_prior,
                        fit_control = ctl)

models <- list(`M fixed (sel sd 0.50)` = ebs_2024,
               `M fixed`               = m_fixed,
               `M fixed + IID`         = m_iid,
               `M fixed + AR1`         = m_ar1,
               `M estimated`           = m_mean,
               `M estimated + IID`     = m_mean_iid,
               `M estimated + AR1`     = m_mean_ar1,
               `M estimated, prior`    = m_mean_prior)
# The plotters label series "Model 1..n" unless told otherwise -- they do not
# read a list's names.
mod_nms <- names(models)


# Comparison ----
# * Read this first: what each fit actually estimated ----
# Expected: the four "M fixed" rows have M_free = 0, M_age1 = 0.9, M_age3 = 0.3,
# M_flat FALSE; the four "M estimated" rows have M_free = 1 and M_flat TRUE. An
# "M estimated" row showing M_free = 0 with the 0.9 / 0.3 schedule is the
# masked-level trap, and that model is a duplicate of its "M fixed" counterpart,
# not a result.
print(m_structure(models))

# * Likelihood and AIC ----
# The default HCR is "NoFishing", so no projection re-optimization runs and
# `opt` / `sdrep` belong to the hindcast -- objective, parameter count and every
# standard error below are the hindcast's. (Under any other HCR they would be
# the projection's, and the hindcast standard errors would all be exactly 0.)
#
# The deviations are integrated out, so `opt$objective` is the marginal
# (Laplace) likelihood and `opt$par` counts only the fixed effects: one extra
# parameter for IID (sigma), two for AR1 (sigma, rho). Read this as a marginal
# AIC, and read it loosely -- the recruitment and selectivity deviations are
# penalized fixed effects, so the model does not have a single well-defined
# parameter count.
#
# Two rows are not on the others' scale and must not be scored against them.
# The first fits its selectivity deviations under a looser penalty, and a looser
# penalty on a fixed effect always buys a lower objective without buying any
# fit. The last adds a prior, which is a penalty the others do not carry and is
# not a free parameter either. Both are in the table to be plotted against.
cmp <- data.frame(
  Model = names(models),
  nll   = sapply(models, function(x) x$opt$objective),
  npar  = sapply(models, function(x) length(x$opt$par)),
  AIC   = sapply(models, function(x) TMBAIC(x$opt)),
  row.names = NULL)
# Anchor dAIC on the SCORABLE rows only. `min()` over all of them would measure
# every model from the sel-sd-0.50 row, which wins by construction: the
# NonParametricPM random-walk penalty is a bare SSQ with no normalizing constant
# (ceattle.cpp), so a looser sd lowers the objective at every parameter vector
# while the parameter count is unchanged. The table would then rank first the
# one model the comment above says not to score.
scorable <- !(cmp$Model %in% c("M fixed (sel sd 0.50)", "M estimated, prior"))
cmp$dAIC  <- NA_real_
cmp$dAIC[scorable] <- cmp$AIC[scorable] - min(cmp$AIC[scorable])
print(cmp, digits = 5)

# Which likelihood components moved. A row's columns count fleets on rows 1-8
# and species on rows 9-21, so compare a model against another within a row --
# a column total would pool two different axes.
# "Linkage random effects" is the density of the M deviations themselves.
sapply(models, function(x) rowSums(x$quantities$jnll_comp))

# * Estimated M level and deviation scale ----
# `log_M1` is the log of age-invariant M, estimated only in the "M estimated"
# models. `log_sigma_linkage` is the log deviation sd -- for AR1 it is the
# MARGINAL sd (glmmTMB convention), so it is not directly comparable with the
# IID one. `trans_rho_linkage` is on the rho_trans scale, rho = 2/(1+exp(-2x))-1.
# A sigma that has gone to ~0 with a huge standard error is the deviation field
# switched off, not a small effect: the likelihood is flat there.
for (nm in names(models)) {
  s <- summary(models[[nm]]$sdrep, "fixed")
  s <- s[rownames(s) %in% c("log_M1", "log_sigma_linkage", "trans_rho_linkage"), ,
         drop = FALSE]
  if (!nrow(s)) next
  cat("\n", nm, "\n"); print(s)
  if ("log_M1" %in% rownames(s))
    cat("  M     =", exp(s["log_M1", "Estimate"]), "\n")
  if ("log_sigma_linkage" %in% rownames(s))
    cat("  sigma =", exp(s["log_sigma_linkage", "Estimate"]), "\n")
  if ("trans_rho_linkage" %in% rownames(s))
    cat("  rho   =", 2 / (1 + exp(-2 * s["trans_rho_linkage", "Estimate"])) - 1, "\n")
}

# * Convergence ----
lapply(models, convergence_diagnostics)

# * The M deviations themselves ----
# M_linkage_offset is the log-scale offset actually applied, [spp, sex, age, yr].
# It is age-invariant here, so age 1 stands for every age.
dev_models <- models[c("M fixed + IID", "M fixed + AR1",
                       "M estimated + IID", "M estimated + AR1")]
M_dev <- do.call(rbind, lapply(names(dev_models), function(nm) data.frame(
  Year  = yrs, Model = nm,
  eps   = dev_models[[nm]]$quantities$M_linkage_offset[1, 1, 1, 1:nyr])))
ggplot(M_dev, aes(Year, eps, colour = Model)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(linewidth = 1) +
  labs(y = "M deviation (log scale)", x = "Year")

# M at age, on the natural scale. Age 3 is where the assumed schedule flattens
# to 0.30 and where the "04-" profile put the data's own optimum near 0.35.
# Age 1 separates the two M levels most: the schedule assumes 0.9 there, and an
# age-invariant M has to average it away.
plot_m_at_age(models, age = 3, model_names = mod_nms)
plot_m_at_age(models, age = 1, model_names = mod_nms)

# * What it does to the assessment quantities ----
plot_ssb(models, model_names = mod_nms)
plot_recruitment(models, model_names = mod_nms)
plot_biomass(models, model_names = mod_nms)
plot_selectivity(models, model_names = mod_nms)   # did the fishery walk shrink where M took over?
plot_index(models, model_names = mod_nms)
plot_indexresidual(models, model_names = mod_nms)

# * OSA residuals and retrospective ----
# Set this deliberately off the tables above. It defaults to `m_fixed` because
# on this data set that is where the comparison lands -- where sigma_M collapses
# the deviation models ARE the fixed-M model, and the estimated-level models do
# not converge (see below). Change it if your tables say something else; every
# residual and retrospective in this block is about whatever it points at.
m_pick <- m_fixed

osa <- osa_residuals(m_pick)
osa_diagnostics(osa)              # SDNR + lower/upper tail (Stewart & Monnahan 2025)
plot(osa)

# Slow (each peel is a full refit, and each refit integrates the deviations).
# A time-varying M can flatter a retrospective by absorbing the peel, so read
# Mohn's rho next to the M deviations, not on its own.
m_retro <- retrospective(Rceattle = m_pick, peels = 5)
m_retro$mohns
plot_biomass(m_retro$Rceattle_list)


# Sensitivity to the fishery selectivity sd ----
# The one tuning choice this script makes. A year-varying M and a random-walk
# fishery selectivity are competing for the same signal, so the estimated
# sigma_M is partly a statement about how much the fishery walk was allowed to
# move. Re-fit the IID model across a grid and read sigma_M and terminal SSB
# next to each other; if sigma_M collapses toward 0 at the loose end, the walk
# is taking the signal.
# Measured on this data set: sigma_M lands on the zero boundary at BOTH 0.50
# (1.8e-06) and 0.15 (2.3e-06), and the marginal likelihood is the fixed-M one
# to every digit. Tightening the fishery walk alone does not free M here. Run
# the grid before assuming a value; and if sigma_M is at the boundary
# everywhere, profile it (below) rather than reporting the point estimate.
SEL_SD_GRID <- c(0.50, 0.30, 0.15, 0.05)

sel_sd_sens <- do.call(rbind, lapply(SEL_SD_GRID, function(sd) {
  tt <- set_sel_sd(est, ebs_2024$estimated_params, fsh, sd)
  f0 <- fit_mod(data_list = tt$data_list, inits = tt$inits, file = NULL,
                estimateMode = "Estimate", random_rec = FALSE, msmMode = 0,
                initMode = "NonEquilibrium", M1Fun = M1_fixed, fit_control = ctl)
  f1 <- try(fit_mod(data_list = tt$data_list,
                    inits = warm_start(tt$data_list, M1_fixed_iid, f0), file = NULL,
                    estimateMode = "Estimate", random_rec = FALSE, msmMode = 0,
                    initMode = "NonEquilibrium", M1Fun = M1_fixed_iid, fit_control = ctl))
  if (inherits(f1, "try-error")) return(data.frame(sel_sd = sd, sigma_M = NA, dnll = NA,
                                                   SSB_2024 = NA, max_grad = NA))
  data.frame(sel_sd   = sd,
             sigma_M  = exp(f1$estimated_params$log_sigma_linkage),
             dnll     = f0$opt$objective - f1$opt$objective,
             # Female spawning-stock biomass in THOUSAND t -- the unit the
             # workbook is in (1972 catch reads 1874.534 for ~1.87 million t).
             SSB_2024 = f1$quantities$ssb[1, nyr],
             max_grad = f1$.conv_hindcast$max_gradient)
}))
print(sel_sd_sens, digits = 4)


# Profile of sigma_M ----
# sigma_M is a variance component, and zero is on its boundary. A free estimate
# that lands there reports "no time-varying M" with a standard error that means
# nothing -- the likelihood is flat, not sharp. The honest statement is the
# profile: fix sigma, leave the deviations integrated, and read how much
# marginal likelihood each sigma costs.
#
# `init = list(sigma = )` supplied WITHOUT a prior on sigma fixes it, so each
# fit below is one point of a profile marginal likelihood, comparable across the
# grid. Anchor it against `m_fixed`, which is the sigma = 0 endpoint.
#
# Measured on this data set, fixed level, fishery sel sd 0.15:
#
#   sigma_M    0       0.05     0.10     0.20     0.40
#   dNLL       0     +14.14   +21.04   +25.95   +32.50
#   max|eps|   0       0.125    0.357    0.559    0.795
#   SSB 2024  4321     4339     4387     4408     3919   (thousand t)
#
# Monotone increasing and steep off zero, which LOOKS like an informative
# surface pinning sigma_M at zero. Do not read it that way. The recovery check
# below simulates from sigma_M = 0.20 -- a point on this very grid, costing
# +25.95 here -- and the free-sigma refit still returns ~1e-06. A profile that
# climbs while the estimator cannot recover the truth is measuring the cost of
# forcing deviations the rest of the model then has to fight, not evidence
# against time-varying M. Read the two together, in that order; the profile
# alone licenses nothing.
SIGMA_M_GRID <- c(0.05, 0.10, 0.20, 0.40)

# Keep the fits, not just the summary: the recovery check at the end of this
# script uses the sigma = SIGMA_TRUE one as its operating model.
sigma_fits <- lapply(SIGMA_M_GRID, function(sg) {
  M1s <- build_M1(updateM1 = TRUE, M1_model = "fixed",
                  linkages = list(M1 = linkage_spec(~ 0 + (1 | Year), by = ~ species,
                                                    init = list(sigma = sg))))
  try(fit_mod(data_list = est_tv, inits = warm_start(est_tv, M1s, m_fixed),
              file = NULL, estimateMode = "Estimate", random_rec = FALSE,
              msmMode = 0, initMode = "NonEquilibrium", M1Fun = M1s,
              fit_control = ctl))
})
names(sigma_fits) <- format(SIGMA_M_GRID)

sigma_prof <- do.call(rbind, Map(function(sg, f) {
  if (inherits(f, "try-error")) return(data.frame(sigma_M = sg, nll = NA, max_eps = NA,
                                                  SSB_2024 = NA))
  data.frame(sigma_M  = sg,
             nll      = f$opt$objective,
             max_eps  = max(abs(f$quantities$M_linkage_offset[1, 1, 1, 1:nyr])),
             SSB_2024 = f$quantities$ssb[1, nyr])
}, SIGMA_M_GRID, sigma_fits))
sigma_prof <- rbind(data.frame(sigma_M = 0, nll = m_fixed$opt$objective,
                               max_eps = 0, SSB_2024 = m_fixed$quantities$ssb[1, nyr]),
                    sigma_prof)
sigma_prof$dnll <- sigma_prof$nll - min(sigma_prof$nll, na.rm = TRUE)
print(sigma_prof, digits = 5)

plot(sigma_prof$sigma_M, sigma_prof$dnll, type = "b",
     xlab = expression(sigma[M]), ylab = "dNLL (marginal)")


# Can a nonzero sigma_M be recovered at all? ----
# The conclusion this script reaches -- "the data put sigma_M at zero" -- is only
# worth stating if the density could have found a nonzero sigma_M had one been
# there. A process that never reaches the likelihood produces the identical
# result, and nothing above distinguishes the two. So simulate from a KNOWN
# sigma_M and refit with sigma free.
#
# The operating model is the profile fit with sigma pinned at SIGMA_TRUE;
# sim_mod(process = "M") redraws the M linkage deviations alongside every
# observation, and the estimating model is the same structure with sigma free.
# Recovery means sigma_hat near SIGMA_TRUE and the recovered deviation series
# correlated with the one that generated the data -- BOTH: a sigma_hat of the
# right size attached to an uncorrelated series is not recovery.
#
# Compare against attr(sim, "process_sim")$beta_linkage_re, NOT against the OM's
# fitted deviations: `process = "M"` redraws them, so the fitted values are no
# longer what generated the data. Restrict to the `_drawn` cells -- that vector
# covers every random-linkage slot whether or not its process was redrawn, so
# over the whole array the statistic reports perfect recovery on cells nothing
# touched. Both points are in ?sim_mod's Value section.
SIGMA_TRUE <- 0.20
NSIM       <- 20

om <- sigma_fits[[which(SIGMA_M_GRID == SIGMA_TRUE)]]

M1_free <- build_M1(updateM1 = TRUE, M1_model = "fixed",
                    linkages = list(M1 = linkage_spec(~ 0 + (1 | Year), by = ~ species)))

set.seed(20240908)
recovery <- do.call(rbind, lapply(seq_len(NSIM), function(i) {
  d <- try(sim_mod(om, simulate = TRUE, process = "M"))
  if (inherits(d, "try-error")) return(NULL)
  truth <- attr(d, "process_sim")
  if (is.null(truth$beta_linkage_re)) {
    warning("sim ", i, ": M was not redrawn; there is no truth to recover.")
    return(NULL)
  }
  drawn    <- truth$beta_linkage_re_drawn
  eps_true <- as.numeric(truth$beta_linkage_re)[drawn]

  d$fleet_control$Time_varying_sel_sd[fsh] <- FSH_SEL_SD
  f <- try(fit_mod(data_list = d, inits = warm_start(d, M1_free, om), file = NULL,
                   estimateMode = "Estimate", random_rec = FALSE, msmMode = 0,
                   initMode = "NonEquilibrium", M1Fun = M1_free, fit_control = ctl))
  if (inherits(f, "try-error")) return(NULL)
  eps_hat <- as.numeric(f$estimated_params$beta_linkage_re)[drawn]
  data.frame(sim        = i,
             sd_eps_true = stats::sd(eps_true),
             sigma_hat  = exp(f$estimated_params$log_sigma_linkage),
             sd_eps_hat = stats::sd(eps_hat),
             cor_eps    = suppressWarnings(stats::cor(eps_hat, eps_true)),
             max_grad   = f$.conv_hindcast$max_gradient)
}))
print(recovery, digits = 4)
summary(recovery$sigma_hat)

# Read it against SIGMA_TRUE. sigma_hat clustered near SIGMA_TRUE with positive
# cor_eps means the zero above is the data speaking. sigma_hat collapsing to
# ~1e-06 on SIMULATED data that genuinely contains deviations of that size means
# the reverse: this configuration cannot see sigma_M, and every "no support for
# time-varying M" statement in this script is unsupported and must be withdrawn.
#
# MEASURED, and it is the second case. Four replicates at SIGMA_TRUE = 0.20:
#
#   sim  sd(eps_true)  sigma_hat   cor_eps   index data moved by
#    1      0.166       1.39e-06    0.19          21900
#    2      0.196       1.53e-06    0.30          10400
#    3      0.218       1.02e-06    0.45          19900
#    4      0.187       1.52e-06    0.27          12300
#
# Every refit converged cleanly (max|grad| ~5e-05) to a sigma five orders of
# magnitude below the truth, with the deviations shrunk to ~1e-12 and no useful
# correlation with the series that generated the data -- on observations that
# differ from the originals by thousands of units, so the signal was genuinely
# injected and genuinely present.
#
# Therefore: the zeros earlier in this script say nothing about whether M varies
# in the EBS. They say this configuration cannot tell. Anything that would
# change that conclusion has to change the configuration -- fewer free
# recruitment deviations, a less flexible fishery selectivity, or an external
# constraint on M -- and then re-run THIS check before believing the result.
