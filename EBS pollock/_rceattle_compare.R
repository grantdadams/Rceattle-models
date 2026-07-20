# =============================================================================
# Scratch harness: fit the EBS pollock model in current dev-ebs-pk Rceattle and
# diff every likelihood component / quantity against the RTMB target (base.rds).
# Not committed; used to drive the machine-precision matching work.
# =============================================================================
options(warn = 1)
MODEL_DIR <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle-models/EBS pollock"
PKG_DIR   <- "/Users/grantadams/Documents/GitHub/Rceattle ecosystem/Rceattle"
setwd(MODEL_DIR)

suppressMessages({
  pkgload::load_all(PKG_DIR, quiet = TRUE)
  library(dplyr)
  library(readxl)
})

# ---- target ----
target <- readRDS(file.path(MODEL_DIR, "base.rds"))$report

# ---- data + bridging Model-2 (AMAK-matching) config ----
mydata <- Rceattle::read_data(file = "Data/2024_EBS_pollock.xlsx")
styr  <- mydata$styr; endyr <- mydata$endyr; nages <- mydata$nages
yrs <- styr:endyr; nyr <- length(yrs)
mydata$spawn_month <- 3
n_selages_fsh <- 12; bts_styr <- 1982; ats_styr <- 1994

est_data <- mydata
est_data$estDynamics <- 0
est_data$index_data$Log_sd <- est_data$index_data$Log_sd / est_data$index_data$Observation
est_data$catch_data$Log_sd <- 0.05
est_data$fleet_control$Fleet_type[5:6] <- 2
est_data$age_error[1:nages, 3:(nages + 2)] <- diag(nages)
fcn <- est_data$fleet_control$Fleet_name
est_data$fleet_control$Selectivity[fcn == "Fishery"]               <- "NonParametricPM"
est_data$fleet_control$Time_varying_sel[fcn == "Fishery"]          <- "RandomWalk"
est_data$fleet_control$N_sel_bins[fcn == "Fishery"]                <- n_selages_fsh
est_data$fleet_control$Sel_curve_pen1[fcn == "Fishery"]            <- 12.5
est_data$fleet_control$Sel_curve_pen2[fcn == "Fishery"]            <- 1/60
est_data$fleet_control$Sel_curve_pen3                              <- 0
est_data$fleet_control$Sel_curve_pen3[fcn == "Fishery"]            <- 1
est_data$fleet_control$Sel_norm_bin1[fcn == "Fishery"]             <- NA
est_data$fleet_control$Time_varying_sel_sd_prior[fcn == "Fishery"] <- 0.5
est_data$fleet_control$Selectivity[fcn == "BTS"]                   <- "LogisticPM"
est_data$fleet_control$Time_varying_sel[fcn == "BTS"]              <- "RandomWalk"
est_data$fleet_control$Sel_curve_pen1[fcn == "BTS"]               <- 2
est_data$fleet_control$Sel_curve_pen2[fcn == "BTS"]               <- 0
est_data$fleet_control$Sel_curve_pen3[fcn == "BTS"]               <- 8
est_data$fleet_control$Sel_norm_bin1[fcn == "BTS"]                <- 3
est_data$fleet_control$Sel_norm_bin2[fcn == "BTS"]                <- 14
est_data$fleet_control$Sel_start_year[fcn == "BTS"]               <- bts_styr
est_data$fleet_control$Bin_first_selected[fcn == "BTS"]           <- 1
est_data$fleet_control$Time_varying_sel_sd_prior[fcn == "BTS"]     <- 1
for (fl in c("ATS", "AVO")) {
  est_data$fleet_control$Selectivity[fcn == fl]               <- "NonParametricPM"
  est_data$fleet_control$Time_varying_sel[fcn == fl]          <- "RandomWalk"
  est_data$fleet_control$N_sel_bins[fcn == fl]                <- 8
  est_data$fleet_control$Sel_curve_pen1[fcn == fl]            <- -1
  est_data$fleet_control$Sel_curve_pen2[fcn == fl]            <- 1
  est_data$fleet_control$Sel_curve_pen3[fcn == fl]            <- 0
  est_data$fleet_control$Sel_norm_bin1[fcn == fl]             <- NA
  est_data$fleet_control$Bin_first_selected[fcn == fl]        <- 1
  est_data$fleet_control$Sel_pen_first_bin[fcn == fl]         <- 2
  est_data$fleet_control$Sel_start_year[fcn == fl]            <- ats_styr
  est_data$fleet_control$Time_varying_sel_sd_prior[fcn == fl] <- 0.138
}
est_data$index_data <- est_data$index_data %>%
  dplyr::mutate(Month = dplyr::case_when(
    Fleet_name %in% c("BTS", "BTS_1", "ATS", "ATS_1") ~ 6, TRUE ~ 0))
est_data$comp_data <- est_data$comp_data %>%
  dplyr::mutate(Month = dplyr::case_when(
    Fleet_name == "BTS" ~ 6, Fleet_name == "ATS" ~ 6, TRUE ~ Month))
est_data$fleet_control$Catchability <- as.character(est_data$fleet_control$Catchability)
est_data$fleet_control$Catchability[fcn == "BTS"]                  <- 3
est_data$fleet_control$Catchability[fcn == "ATS"]                  <- 1
est_data$fleet_control$Catchability[fcn %in% c("BTS_1", "ATS_1")]  <- 3
est_data$index_data$Log_sd[est_data$index_data$Fleet_name %in% c("BTS_1", "ATS_1")] <- 1
est_data$sigma_rec_prior <- 0.707

cat("==== FITTING (estimateMode = 0) ====\n")
fit <- tryCatch(
  Rceattle::fit_mod(
    data_list = est_data, inits = NULL, file = NULL,
    estimateMode = 0, random_rec = FALSE, msmMode = 0,
    verbose = 1, phase = TRUE, initMode = 2,
    M1Fun = build_M1(updateM1 = TRUE, M1_model = 0)),
  error = function(e) { cat("FIT ERROR:", conditionMessage(e), "\n"); NULL })

saveRDS(fit, file.path(MODEL_DIR, "_rceattle_fit.rds"))

if (!is.null(fit)) {
  q <- fit$quantities
  cat("\n==== jnll components (Rceattle) ====\n")
  print(round(q$jnll_comp, 4))
  cat("\ntotal jnll:", sum(q$jnll_comp), "\n")

  cat("\n==== TARGET (base.rds) components ====\n")
  for (nm in c("cat_like","bts_like","ats_like","ats_age1_like","cpue_like",
               "avo_like","Fpen_like","avgsel_like"))
    cat(sprintf("  %-15s %s\n", nm, paste(round(target[[nm]],4), collapse=", ")))
  cat("  age_like      ", paste(round(target$age_like,4), collapse=", "), "\n")
  cat("  rec_like      ", paste(round(target$rec_like,4), collapse=", "), "\n")
  cat("  sel_like      ", paste(round(target$sel_like,4), collapse=", "), "\n")
  cat("  sel_like_dev  ", paste(round(target$sel_like_dev,4), collapse=", "), "\n")

  ssb <- q$ssb[1, 1:nyr]; R <- q$R[1, 1:nyr]; bio <- q$biomass[1, 1:nyr]
  tssb <- as.numeric(target$SSB); tR <- as.numeric(target$recruitment)
  cat("\n==== dynamics vs target ====\n")
  cat(sprintf("SSB corr %.5f  mean|%%diff| %.3f%%  max %.3f%%\n",
      cor(ssb, tssb), 100*mean(abs(ssb/tssb-1)), 100*max(abs(ssb/tssb-1))))
  cat(sprintf("R   corr %.5f  mean|%%diff| %.3f%%\n",
      cor(R[-1], tR[-1]), 100*mean(abs(R[-1]/tR[-1]-1))))
  cat("\nconvergence (max gradient):", fit$opt$max_gradient,
      " pdHess:", fit$opt$convergence, "\n")
} else {
  cat("\nFIT FAILED — see error above.\n")
}
cat("\n==== DONE ====\n")
