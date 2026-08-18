# =============================================================================
# EBS pollock 2024 -- environment-linked recruitment via DSEM
# =============================================================================
# OFF-PIPELINE (un-numbered): a research run, not part of the assessment
# sequence. Links environmental indices to recruitment with a dynamic structural
# equation model (DSEM) and compares environment-free (IID) vs environment-linked
# recruitment by AIC.
#
# NOT AN ASSESSMENT RESULT: the environmental columns below are random
# placeholders. Replace them with cohort-aligned ESP indicators before fitting.
#
# Run from the "EBS pollock" project root.
# Reads:  Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx
# Prereq: "01-build-data.R"
#
# REQUIRES the DSEM build of Rceattle (a separate branch):
#   remotes::install_version("dsem", version = "3.0.0")
#   remotes::install_github("grantdadams/Rceattle@dev-DSEM")
#
# NOTE -- departs from the ADMB bridge: the assessment ("03-model-comparison.R") treats
# recruitment deviations as penalised fixed effects (random_rec = FALSE), but a DSEM
# models them as random effects, so this sets random_rec = TRUE. Everything else is
# inherited unchanged from the bridge workbook.
# =============================================================================

library(Rceattle)   # dev-DSEM
library(dplyr)

n_selages_fsh <- 12

# Data ----
est  <- read_data(file = "Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx")
styr <- est$styr; endyr <- est$endyr; yrs <- styr:endyr; nyr <- length(yrs)

# * Environmental indices ----
# TODO (@kalei): replace these random placeholders with the real EBS indices
# (e.g. summer bottom temperature, the Cold Pool Index, the spring SST anomaly).
# One column per driver, on the recruitment years; DSEM standardises internally.
set.seed(1)
env_new <- data.frame(Year     = yrs,
                      BottomTemp = rnorm(nyr),
                      ColdPool   = rnorm(nyr),
                      SST        = rnorm(nyr))
est$env_data <- if (is.null(est$env_data)) env_new else
  dplyr::full_join(est$env_data, env_new, by = "Year")

plot_data(est)

# Empirical fishery-selectivity start ----
# Same data-driven start the assessment uses, so the selectivity scale is pinned
# from the outset (see "03-model-comparison.R"). Without it the flat default start
# can open a weakly-identified scale direction when recruitment is a random effect.
M1Fun <- build_M1(updateM1 = TRUE, M1_model = "fixed")
ctl   <- fit_control(verbose = 1, phase = TRUE,
                     bias_adjust_proc = 0, bias_adjust_obs = 0, comp_offset = 1e-3)
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

# SEM specifications ----
# Columns: source -> target, lag, param_name, start. recdevs1 = species-1
# recruitment deviations; the *_AR1 rows give each environmental index its own
# AR1 process; sigmaR1 is the recruitment process-error SD.

# * IID recruitment (environment-free baseline, for model comparison) ----
ebs_iid_sem <- "
  BottomTemp  ->  BottomTemp,   1,  BottomTemp_AR1,   0
  ColdPool    ->  ColdPool,     1,  ColdPool_AR1,     0
  SST         ->  SST,          1,  SST_AR1,          0

  recdevs1   <->  recdevs1,     0,  sigmaR1,          1
"

# * Full SEM (environment -> recruitment) ----
ebs_sem <- "
  BottomTemp  ->  BottomTemp,   1,  BottomTemp_AR1,   0
  ColdPool    ->  ColdPool,     1,  ColdPool_AR1,     0
  SST         ->  SST,          1,  SST_AR1,          0

  BottomTemp  ->  recdevs1,     1,  BottomTemp_to_R,  0
  ColdPool    ->  recdevs1,     1,  ColdPool_to_R,    0
  SST         ->  recdevs1,     1,  SST_to_R,         0

  recdevs1   <->  recdevs1,     0,  sigmaR1,          1
"

# Fit ----
# * IID SEM ----
ebs_iid <- fit_mod(
  data_list  = est, inits = inits, file = NULL,
  estimateMode = 0, random_rec = TRUE, msmMode = 0, initMode = "NonEquilibrium", M1Fun = M1Fun,
  dsem = build_DSEM(sem = ebs_iid_sem, family = "fixed",
                    sigmaR_prior_sd = 0.5),   # SD prior aids convergence with sparse env data
  fit_control = ctl)
summary(ebs_iid); AIC(ebs_iid)

# * Full SEM ----
ebs_dsem <- fit_mod(
  data_list  = est, inits = inits, file = NULL,
  estimateMode = 0, random_rec = TRUE, msmMode = 0, initMode = "NonEquilibrium", M1Fun = M1Fun,
  dsem = build_DSEM(sem = ebs_sem, family = "fixed",
                    sigmaR_prior_sd = 0.5),
  fit_control = ctl)
summary(ebs_dsem); AIC(ebs_dsem)

# Environment-linked vs IID recruitment: lower AIC favours the environmental links
plot_biomass(list(ebs_iid, ebs_dsem),
             model_names = c("IID recruitment", "Environment-linked (DSEM)"))
