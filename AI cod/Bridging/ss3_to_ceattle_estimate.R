# =============================================================================
# 2024 Aleutian Islands Pacific cod: Rceattle ESTIMATION model vs SS3
#
# Sources the forward-pass script to reuse the converter data list, the SS3-
# faithful configuration (length DoubleNormal sel, M-block, single-sex SSB,
# variance-adjusted comp N, mid-year catch weight), the Richards growth spec,
# and the SS3-injected starting values. Then it ESTIMATES the model with
# Rceattle's TMB optimizer (estimateMode = 1) starting from the SS3 MLEs and
# compares the converged Rceattle fit to the SS3 (ADMB) outputs.
#
# Scope / known structural differences (see README_bridging.md):
#   * Growth is held FIXED at the SS3 Richards MLEs. Rceattle's Richards path
#     reproduces SS3's length-at-age in the forward pass, but estimating it
#     jointly is out of scope (and the ALK feeds only the comp likelihoods,
#     which already carry documented form/constant differences).
#   * Recruitment likelihood diverges by design (no Methot-Taylor bias ramp).
#   * Selectivity uses Rceattle's 4-param length DoubleNormal (asymptotic for
#     both AI cod fleets), not SS3's 6-param pattern 24.
# =============================================================================

# --- Build everything via the forward-pass script ---------------------------
# (defines: cod, inits, growthFun_spec, M1_block, fleet_meta, years_hind,
#  nages, ss3_rep, fp). Forward-pass parity is printed during the source.
source("Bridging/ss3_to_ceattle_forward_pass.R")

cat("\n\n#####################################################################\n")
cat("# ESTIMATION: Rceattle MLE starting from SS3 values\n")
cat("#####################################################################\n")

# Growth handling: HOLD growth FIXED at the SS3 Richards MLEs.
#
# We attempted to ESTIMATE growth (K/L1/Linf) under priors centred on the SS3
# MLEs (the rebuilt Rceattle correctly re-targets `(Intercept)` priors onto
# log_growth_pars -- verified by tests-Linkage/test-intercept-prior-base-
# parameter.R). It is degenerate for this stock, for two compounding reasons
# established empirically (see README_bridging.md "Why growth is held fixed"):
#   1. Terminal recruitment is unconstrained. SS3's Methot-Taylor bias ramp is
#      out of scope; without it, freeing growth lets recent recdevs crash and
#      terminal SSB collapses (2024 SSB -> ~360 vs SS3 49,350; log_F gradient
#      ~200, non-invertible Hessian). Pinning M_block and warm-starting from
#      the converged growth-fixed fit do NOT fix it.
#   2. Rceattle's CAAL prefers Linf ~108-113 (< SS3's 123) regardless of comp
#      weighting -- removing SS3's Francis down-weights pulls Linf further from
#      SS3 (107.9) and worsens the collapse, so SS3's Linf is not recoverable
#      from the CAAL alone here. The forward pass already proved Rceattle's
#      Richards reproduces SS3 length-at-age at the SS3 MLEs, so fixing growth
#      is well-justified.
# We reuse mod0's map and NA-out log_growth_pars + growth_log_sd. Everything SS3
# estimates for the population dynamics (R0, recdevs, init_dev, M-block, q, F,
# Finit, selectivity) stays estimable.
est_map <- mod0$map
est_map$mapFactor$log_growth_pars <-
  factor(rep(NA, length(est_map$mapFactor$log_growth_pars)))
est_map$mapFactor$growth_log_sd <-
  factor(rep(NA, length(est_map$mapFactor$growth_log_sd)))

# Fix the DoubleNormal right-tail floor logits (sel_inf par index 2 per fleet).
# Both AI cod fleets are asymptotic, so the floor sits at ~1 on a flat ridge
# and is non-identifiable (flagged by check_estimability). sel_inf is
# [par, fleet, sex] flattened column-major -> floor entries are at the even
# positions (2, 4). Hold them at the fitted near-1 logit from the forward pass.
sel_inf_len <- length(est_map$mapFactor$sel_inf)
sel_inf_lvl <- as.integer(est_map$mapFactor$sel_inf)
floor_pos <- seq(2, sel_inf_len, by = 2)   # par index 2 = right_floor
sel_inf_lvl[floor_pos] <- NA
est_map$mapFactor$sel_inf <- factor(sel_inf_lvl)

# Match SS3's estimated/fixed selectivity split exactly. SS3 estimates only
# P1 (peak, -> sel_inf[1]) and P3 (ascending, -> log_sel_slp[1]); P2/P4/P5/P6
# are all fixed (phase < 0). In particular SS3 FIXES the descending limb P4,
# so fix Rceattle's descending slope log_sel_slp[2] (even positions in the
# flattened [par, fleet, sex] vector). Both fleets are asymptotic, so this
# slope is also weakly identified -- fixing it matches SS3 and improves
# conditioning.
slp_len <- length(est_map$mapFactor$log_sel_slp)
slp_lvl <- as.integer(est_map$mapFactor$log_sel_slp)
slp_lvl[seq(2, slp_len, by = 2)] <- NA       # par index 2 = descending slope
est_map$mapFactor$log_sel_slp <- factor(slp_lvl)

# Match SS3's recruitment estimable set: SS3 estimates MAIN recdevs only through
# `last_yr_mainrecr_devs` (2021); 2022+ are forecast (not estimated). Rceattle
# estimates rec_dev for every hindcast year (1991-2024), so fix the 3 forecast
# years (2022-2024) at their injected values. rec_dev is [sp, year] flattened;
# hindcast years 1991-2024 -> indices 1-34. (Rceattle's init_dev[1:13] is the
# analog of SS3's 13 early recdevs 1978-1990 for the initial age structure.)
last_main_yr <- ctllist$last_yr_mainrecr_devs %||% 2021
rec_yrs <- cod$styr:cod$projyr
rd_lvl  <- as.integer(est_map$mapFactor$rec_dev)
rd_lvl[rec_yrs > last_main_yr & rec_yrs <= cod$endyr] <- NA
est_map$mapFactor$rec_dev <- factor(rd_lvl)

# --- MLE estimation ---------------------------------------------------------
est <- Rceattle::fit_mod(
  data_list    = cod,
  inits        = inits,                 # SS3 MLEs as starting values
  map          = est_map,               # growth fixed; pop dynamics estimated
  estimateMode = 1,                     # estimate (MLE)
  initMode     = "FishedNonEquilibriumScaled",
  growthFun    = growthFun_spec,
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  fit_control  = fit_control(verbose = 1)
)

# --- Convergence diagnostics ------------------------------------------------
cat("\n=== Convergence ===\n")
cat("Status:", tryCatch(est$convergence$status, error = function(e) "unknown"), "\n")
maxgrad <- tryCatch(max(abs(est$opt$opt$grad)), error = function(e) NA)
if (is.na(maxgrad))
  maxgrad <- tryCatch(max(abs(est$opt$h$gradient)), error = function(e) NA)
cat(sprintf("Final objective = %.4f\n",
            tryCatch(est$opt$opt$objective, error = function(e) NA)))
hess_pd <- tryCatch(est$sdrep$pdHess, error = function(e) NA)
cat(sprintf("Positive-definite Hessian: %s\n", hess_pd))
cat("Convergence checks:\n")
print(tryCatch(est$convergence$checks, error = function(e) NULL))

# --- Estimable-parameter count: Rceattle vs SS3 -----------------------------
# SS3 estimable (phase > 0): 4 growth + 1 M-block + 1 R0 + 1 q + 4 sel (P1/P3
# x 2 fleets) + 1 InitF + 13 early recdevs + 31 main recdevs = 56.
n_rce <- sum(vapply(est$map$mapFactor,
                    function(x) sum(!is.na(x)), integer(1)))
n_ss3 <- 56L
cat("\n=== Estimable-parameter count ===\n")
cat(sprintf("Rceattle: %d   SS3: %d\n", n_rce, n_ss3))
cat("Matched classes: R0(1), M-block(1), q(1), InitF(1), sel peak+asc(4),\n")
cat("  recruitment (rec_dev 31 + init_dev 13 = 44 == SS3 main 31 + early 13).\n")
cat("Remaining structural differences in the count:\n")
cat("  + log_F (34): Rceattle estimates annual F; SS3 uses hybrid F (method 3,\n")
cat("    0 F params, F solved internally from catch). Not reconcilable without\n")
cat("    a hybrid-F mode in Rceattle.\n")
cat("  - growth (4): SS3 estimates K/L1/Linf/Richards; held fixed here\n")
cat("    (degenerate -- see README 'Why growth is held fixed').\n")

# --- Trajectory comparison: Rceattle MLE vs SS3 -----------------------------
ts_ss3 <- ss3_rep$timeseries[match(years_hind, ss3_rep$timeseries$Yr), ]
relerr <- function(a, b) ifelse(b == 0, NA, abs(a - b) / abs(b))
est_ssb <- as.numeric(est$quantities$ssb[1, seq_along(years_hind)])
est_bio <- as.numeric(est$quantities$biomass[1, seq_along(years_hind)])
est_R   <- as.numeric(est$quantities$R[1, seq_along(years_hind)])
cmp_est <- data.frame(
  Year = years_hind,
  SSB_ss3 = ts_ss3$SpawnBio, SSB_est = est_ssb, SSB_re = relerr(est_ssb, ts_ss3$SpawnBio),
  Bio_ss3 = ts_ss3$Bio_all,  Bio_est = est_bio, Bio_re = relerr(est_bio, ts_ss3$Bio_all),
  R_ss3   = ts_ss3$Recruit_0, R_est  = est_R,   R_re  = relerr(est_R, ts_ss3$Recruit_0))
cat("\n=== Estimated trajectory vs SS3 (head & tail) ===\n")
print(head(cmp_est), digits = 5); print(tail(cmp_est), digits = 5)
cat(sprintf("\nMax rel err (MLE vs SS3)  SSB=%.3f  Bio=%.3f  R=%.3f\n",
            max(cmp_est$SSB_re, na.rm = TRUE), max(cmp_est$Bio_re, na.rm = TRUE),
            max(cmp_est$R_re, na.rm = TRUE)))

# --- Estimated key parameters vs SS3 MLE ------------------------------------
gp <- function(sec, pat) { i <- grep(pat, rownames(sec)); if (length(i)) sec[i[1], "ESTIM"] else NA_real_ }
ep <- est$estimated_params
ltbl <- mod0$data_list$linkage_table
m_row <- which(ltbl$process == "M" & ltbl$design_col != "(Intercept)")[1]
M_block_rce <- exp(ep$log_M1[1]) * exp(ep$beta_linkage[m_row])
par_cmp <- data.frame(
  Parameter = c("log(R0)", "M_base", "M_block", "ln_q_Srv",
                "VonBert_K", "L_at_Amin", "L_at_Amax (Linf)",
                "Fsh_sel_peak", "Srv_sel_peak"),
  SS3 = c(gp(parlist$SR_parms, "SR_LN"),
          M_base, M_block,
          gp(parlist$Q_parms, "LnQ_base_Srv"),
          K_vb, L_min, L_max,
          NA, NA),
  Rceattle = c(ep$rec_pars[1, 1],
               exp(ep$log_M1[1]), M_block_rce,
               ep$index_log_q[2],
               exp(ep$log_growth_pars[1, 1, 1]),   # K
               exp(ep$log_growth_pars[1, 1, 2]),   # L1
               exp(ep$log_growth_pars[1, 1, 3]),   # Linf
               ep$sel_inf[1, 1, 1], ep$sel_inf[1, 2, 1]),
  stringsAsFactors = FALSE)
cat("\n=== Estimated parameters: Rceattle MLE vs SS3 ===\n")
print(par_cmp, digits = 5, row.names = FALSE)

# --- Grouped NLL: estimated model -------------------------------------------
cat("\n=== Estimated-model jnll components ===\n")
print(round(rowSums(est$quantities$jnll_comp), 3)[
  c("Index data","Catch data","Composition data","CAAL data","Recruitment deviates")])

# --- Plots: Rceattle MLE vs SS3 (SAFE overlay) ------------------------------
# Build an SS3 "model" object by copying the FP fit and overwriting trajectory
# with SS3 reported values, so Rceattle's plot_* can overlay them.
safe <- fp
safe$quantities$ssb[1, seq_along(years_hind)]     <- ts_ss3$SpawnBio
safe$quantities$biomass[1, seq_along(years_hind)] <- ts_ss3$Bio_all
safe$quantities$R[1, seq_along(years_hind)]        <- ts_ss3$Recruit_0

pdf("Bridging/AIcod_estimate_vs_SS3.pdf", width = 9, height = 6)
tryCatch({
  Rceattle::plot_biomass(list(est, safe), model_names = c("Rceattle MLE", "SS3"))
  Rceattle::plot_ssb(list(est, safe), model_names = c("Rceattle MLE", "SS3"))
  Rceattle::plot_recruitment(list(est, safe), model_names = c("Rceattle MLE", "SS3"))
}, error = function(e) cat("plot error:", conditionMessage(e), "\n"))
dev.off()

saveRDS(list(est = est, cmp_est = cmp_est, par_cmp = par_cmp),
        "Bridging/_estimate_result.rds")
cat("\nEstimation complete. Saved Bridging/_estimate_result.rds and AIcod_estimate_vs_SS3.pdf\n")
