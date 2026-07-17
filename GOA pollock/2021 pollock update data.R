# =============================================================================
# Build the Rceattle data list for the 2021 GOA pollock WHAM ("pkwham") bridge
# =============================================================================
# Assembles an Rceattle data list from the *WHAM input* itself, so the two models
# see byte-identical data (catch, survey indices, age compositions, weight- and
# maturity-at-age, natural mortality, fixed selectivities). This is what lets the
# bridging script confirm the models agree observation-for-observation.
#
# We build from the WHAM input, NOT from the 2023/2024 pollock .xlsx files: those
# are a different data vintage and a different (master-branch) Rceattle format.
#
# The output is consumed by "2021 pollock bridging.R".
#
# REPRODUCE (from the 'GOA pollock' project root):
#   export PATH=/usr/bin:$PATH               # macOS: system toolchain for TMB
#   Rscript "Data/2021 Pollock WHAM.R"       # first: -> Data/2021pollock_wham.Rdata
#   Rscript "2021 pollock update data.R"     # this script: -> Data/2021pollock_rceattle.Rdata

library(Rceattle)
library(dplyr)

load("Data/2021pollock_wham.Rdata")   # 'fit' from Data/2021 Pollock WHAM.R
dat <- fit$input$data

years  <- 1970:2021
nyrs   <- length(years)          # 52
nages  <- 10
nindex <- 6

stopifnot(dat$n_years_model == nyrs, dat$n_ages == nages, dat$n_indices == nindex)

# Fleets: 1-6 = indices (Shelikof, NMFS BT, ADF&G, age-1, age-2, summer acoustic), 7 = fishery.
index_names <- c("Shelikof", "NMFS_BT", "ADFG", "Age1_index", "Age2_index", "Summer_AT")
fleet_names <- c(index_names, "Fishery")
FISH <- 7L

# Skeleton ----
pk <- Rceattle::GOAcod   # overwritten field by field below
pk$styr   <- min(years); pk$endyr <- max(years)
pk$projyr <- max(years)
pk$nspp   <- 1
pk$nages  <- nages
pk$nsex   <- 1
pk$spnames <- "Pollock"
pk$estDynamics <- 0
pk$nlengths <- nages          # no length data; length bins unused
pk$pop_wt_index         <- 3  # waa_pointer_jan1
pk$ssb_wt_index         <- 2  # waa_pointer_ssb
pk$pop_age_transition_index <- 1
pk$spawn_month <- dat$fracyr_SSB[1] * 12          # 0.21 -> 2.52
pk$sigma_rec_prior <- 1                            # WHAM sigmaR FIXED at 1
pk$other_food <- 0
pk$srr_fun <- 0; pk$srr_pred_fun <- 0              # random-about-mean recruitment

# Fleet control ----
# WHAM selblock_models: 1 = age-specific, 3 = double-logistic. Block 1 = fishery,
# blocks 2:7 = indices 1:6. Indices 4/5/6 are fixed selectivity -> empirical.
#   idx1 Shelikof  age-specific (ages 1-2 fixed 0)      -> NonParametric  (see note)
#   idx2 NMFS BT   double-logistic                      -> DoubleLogistic
#   idx3 ADF&G     double-logistic                      -> DoubleLogistic
#   idx4 age-1     fixed c(1,0,...)                     -> Fixed (emp_sel)
#   idx5 age-2     fixed c(0,1,0,...)                   -> Fixed (emp_sel)
#   idx6 summer AT fixed all 1                          -> Fixed (emp_sel)
#   fishery        double-logistic, time-varying on the ASCENDING limb only
fc <- pk$fleet_control[1, ][rep(1, 7), ]
rownames(fc) <- NULL
fc$Fleet_name <- fleet_names
fc$Fleet_code <- 1:7
fc$Fleet_type <- c(rep(2L, nindex), 1L)      # 2 = survey/index, 1 = fishery
fc$Species    <- 1
fc$Month      <- c(dat$fracyr_indices[1, ] * 12, 0)   # 2.508 6.516 7.319 0 0 6.228 ; fishery 0
fc$Selectivity_index <- 1:7
fc$Selectivity <- c("NonParametric", "DoubleLogistic", "DoubleLogistic",
                    "Fixed", "Fixed", "Fixed", "DoubleLogistic")
fc$N_sel_bins <- nages          # data_check requires 1:nages even for Fixed blocks
# Shelikof (block 1) is WHAM's age-specific block: ages 1-2 fixed at 0, ages 3-10 free.
#   - NonParametric with Bin_first_selected = 3 zeros ages 1-2 (cpp zeroes bins < bfs-1,
#     R subtracts 1: R value 3 -> C++ 2 -> zero C++ bins 0,1 = ages 1,2). VERIFIED.
#   - Sel_curve_pen1/2 = 0 so the Ianelli monotonicity/curvature penalties are OFF (WHAM's
#     age-specific block has no shape penalty). NOTE: the hardcoded avg_sel normalization
#     penalty (cpp ~2780, weight 2) is always on for NonParametric -- it soft-pins the SCALE
#     (mean(exp(sel))=1), which is confounded with q for a survey, so it moves q not shape.
#   - Sel_norm_bin1 < 0 -> normalize by max. WHAM's age-specific saturates at 1 (ages 6-7),
#     so max-normalization reproduces WHAM's scale exactly (VERIFIED to 6.5e-07).
fc$Sel_curve_pen1 <- c(0, rep(NA, 5), 0)
fc$Sel_curve_pen2 <- c(0, rep(NA, 5), 0)
# Fishery selectivity: time-varying ascending limb only (WHAM selpars_re on pars 13 & 14 =
# ascending inflection + slope, iid, SD FIXED at exp(sel_repars) = 0.1). Use IID here; the
# bridging script's custom map NA's out the descending-limb deviates so only the ascending
# limb varies (RandomWalkAscending would force a random walk with the first deviate fixed).
fc$Time_varying_sel <- c(rep(0L, nindex), 1L)         # 1 = IID (ascending-only via map)
fc$Time_varying_sel_sd_prior <- NA
fc$Time_varying_sel_sd_prior[FISH] <- 0.1
fc$Bin_first_selected <- c(3L, rep(1L, 6))            # Shelikof: zero ages 1-2
fc$Sel_norm_bin1 <- c(-1L, rep(NA, 6))               # Shelikof: normalize by max
fc$Sel_norm_bin2 <- NA
fc$Comp_loglike <- 0                                  # multinomial
fc$Comp_weights <- 1
fc$CAAL_loglike <- 0
fc$Weight1_Numbers2 <- c(dat$units_indices, 1L)       # 1,1,1,2,2,1 ; fishery biomass
fc$Weight_index <- c(2L, 3L, 3L, 2L, 2L, 4L, 1L)      # waa_pointer_indices ; fishery -> waa[1]
fc$Age_transition_index <- 1
fc$Q_index <- c(1:6, NA)
# q1 (Shelikof) and q3 (ADF&G) are time-varying; SDs FIXED at 0.038 / 0.05.
# WHAM uses AR1 with rho par = 10 -> tanh(10) ~ 1, i.e. effectively a RANDOM WALK. Rceattle's
# native "AR1" catchability (est_index_q=6) is the Rogers et al env-index-driven variant (it
# also fits the deviates to an environmental index, cpp ~3037), which WHAM does NOT do. So use
# RandomWalk (dnorm(dev_y - dev_{y-1}, 0, sd)) -- the correct match for WHAM's rho~1 AR1.
# NOTE: no q prior -- pkwham's use_q_prior = 0 (see bridging script header).
fc$Catchability <- c(1L, 1L, 1L, 1L, 1L, 1L, NA)      # 1 = Estimated
fc$Q_prior <- NA; fc$Q_sd_prior <- NA
fc$Time_varying_q <- c(4L, 0L, 4L, 0L, 0L, 0L, NA)    # 4 = RandomWalk (indices 1, 3)
fc$Time_varying_q_sd_prior <- c(0.038, NA, 0.05, NA, NA, NA, NA)
fc$Estimate_index_sd <- 0                             # SDs are data (agg_index_sigma)
fc$Index_sd_prior <- NA
fc$Estimate_catch_sd <- 0                             # catch SD = 0.05, fixed
fc$Catch_sd_prior <- NA
fc$proj_F_prop <- c(rep(NA, nindex), 1)
pk$fleet_control <- fc

# Catch data ----
# WHAM agg_catch is in mt; catch SD is agg_catch_sigma (0.05).
pk$catch_data <- data.frame(
  Fleet_name = "Fishery", Fleet_code = FISH, Species = 1,
  Year = years, Month = 0, Selectivity_block = 1,
  Catch  = dat$agg_catch[, 1],
  Log_sd = dat$agg_catch_sigma[, 1]
)

# Index data ----
# -999 is WHAM's missing-value code in agg_indices; use_indices flags the fitted years.
# A NEGATIVE Year turns an observation off in Rceattle.
index_list <- lapply(seq_len(nindex), function(i) {
  yr <- years
  yr[dat$use_indices[, i] == 0] <- -yr[dat$use_indices[, i] == 0]
  data.frame(
    Fleet_name = index_names[i], Fleet_code = i, Species = 1,
    Year = yr, Month = dat$fracyr_indices[1, i] * 12,
    Selectivity_block = 1, Q_block = 1,
    Observation = dat$agg_indices[, i],
    Log_sd = dat$agg_index_sigma[, i]
  )
})
pk$index_data <- do.call(rbind, index_list)
# Placeholder for the -999 rows so Rceattle's data checks see a positive observation;
# they are switched off via the negative Year above.
pk$index_data$Observation[pk$index_data$Observation <= 0] <- 1

# Comp data ----
# Multinomial; Neff from catch_Neff / index_Neff. Indices 4 and 5 have no comps.
# Month drives the survey-timing mortality of the predicted SURVEY age comp:
# ceattle_v01_11.cpp:1948 reads it from the comp row (comp_n(.,0)) for growth_model = 0,
# so it must equal the survey month (fracyr_indices*12) or the comp gets NO exp(-mo/12*Z)
# decay and age-1 (highest M) is over-predicted. The fishery uses the Baranov catch-at-age
# (no timing exponent), so its comp month is irrelevant -> 0.
mk_comp <- function(paa, Neff, use, fleet_name, fleet_code, month) {
  yr <- years
  yr[use == 0] <- -yr[use == 0]
  tmp <- as.data.frame(paa)
  colnames(tmp) <- paste0("Comp_", 1:nages)
  cbind(data.frame(Fleet_name = fleet_name, Fleet_code = fleet_code, Species = 1,
                   Sex = 0, Age0_Length1 = 0, Year = yr, Month = month,
                   Sample_size = Neff),
        tmp)
}
comp_list <- list(mk_comp(dat$catch_paa[1, , ], dat$catch_Neff[, 1],
                          dat$use_catch_paa[, 1], "Fishery", FISH, 0))
for (i in seq_len(nindex)) {
  if (all(dat$use_index_paa[, i] == 0)) next          # indices 4 and 5: no comps
  comp_list[[length(comp_list) + 1]] <-
    mk_comp(dat$index_paa[i, , ], dat$index_Neff[, i], dat$use_index_paa[, i],
            index_names[i], i, dat$fracyr_indices[1, i] * 12)
}
pk$comp_data <- do.call(rbind, comp_list)

# Empirical selectivity ----
# Only the fixed blocks (indices 4, 5, 6) are supplied; the rest are estimated.
emp <- expand.grid(Fleet_code = 4:6, Year = 0)
sel_fixed <- rbind(c(1, rep(0, 9)), c(0, 1, rep(0, 8)), rep(1, 10))
emp_sel <- cbind(
  data.frame(Fleet_name = index_names[4:6], Fleet_code = 4:6, Species = 1,
             Sex = 0, Year = 0),                       # 0 fills all years
  setNames(as.data.frame(sel_fixed), paste0("Comp_", 1:nages))
)
pk$emp_sel <- emp_sel

# Weight-at-age ----
# 4 year-varying matrices (dat$waa is 4 x 52 x 10).
wt_list <- lapply(1:4, function(k) {
  tmp <- as.data.frame(dat$waa[k, , ])
  colnames(tmp) <- paste0("Age", 1:nages)
  cbind(data.frame(Wt_name = paste0("WAA", k), Wt_index = k, Species = 1,
                   Sex = 0, Year = years), tmp)
})
pk$weight <- do.call(rbind, wt_list)

# Maturity ----
# Time-invariant (all 52 rows identical) and ALREADY halved for female-only SSB.
stopifnot(all(apply(dat$mature, 2, function(x) diff(range(x))) == 0))
mat <- as.data.frame(matrix(dat$mature[1, ], nrow = 1))
colnames(mat) <- paste0("Age", 1:nages)
pk$maturity <- cbind(data.frame(Species = 1), mat)

# Sex ratio ----
# WHAM's 'mature' already carries the 0.5 -> sex_ratio must be 1 or SSB is halved twice.
sr <- as.data.frame(matrix(1, nrow = 1, ncol = nages))
colnames(sr) <- paste0("Age", 1:nages)
pk$sex_ratio <- cbind(data.frame(Species = 1), sr)

# Natural mortality ----
# FIXED, age-varying, time-invariant.
m1 <- as.data.frame(matrix(exp(fit$input$par$M_a), nrow = 1))
colnames(m1) <- paste0("Age", 1:nages)
pk$M1_base <- cbind(data.frame(Species = 1, Sex = 0), m1)

# Age error / age transition ----
ae <- as.data.frame(diag(1, nages))
colnames(ae) <- paste0("Obs_age", 1:nages)
pk$age_error <- cbind(data.frame(Species = 1, True_age = 1:nages), ae)

at <- as.data.frame(diag(1, nages))
colnames(at) <- paste0("Length_", 1:nages)
pk$age_trans_matrix <- cbind(data.frame(Age_transition_name = "Base",
                                        Age_transition_index = 1, Species = 1,
                                        Sex = 0, Age = 1:nages), at)

# Unused blocks ----
pk$NByageFixed <- NULL
pk$env_data <- data.frame(Year = years, EnvData = 1)
rat <- as.data.frame(matrix(1, nrow = 1, ncol = nages))
colnames(rat) <- paste0("Age", 1:nages)
pk$ration_data <- cbind(data.frame(Species = 1, Sex = 0, Year = 0), rat)

pollock21 <- pk
save(pollock21, file = "Data/2021pollock_rceattle.Rdata")
