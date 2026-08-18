# =============================================================================
# EBS pollock 2024 -- roll the assessment data forward one year
# =============================================================================
# Extends the 2024 ADMB-bridge build (from "01-build-data.R") to a new end year
# with placeholders, then builds the result to check it before writing. Fill the
# placeholders, then refit with "04-fit-and-diagnostics.R". The bridge encoding
# already lives in the xlsx and round-trips, so this only adds records.
# https://grantdadams.github.io/Rceattle/articles/data-without-excel.html
#
# Placeholders: 99999 for catch and survey observations, Sample_size = 0 for
# comps, weight-at-age and the BTS covariance carried forward.
#
# Run from the "EBS pollock" project root.
#
# Reads:   Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx
# Writes:  Data/EBS_25_pollock_m23_rceattle_full_1964-2025.xlsx
# Prereq:  "01-build-data.R"
# =============================================================================

library(Rceattle)
library(dplyr)

new_endyr <- 2025

# Data ----
est <- Rceattle::read_data(file = "Data/EBS_24_pollock_m23_rceattle_full_1964-2024.xlsx")
old_endyr <- est$endyr
est$endyr <- new_endyr

# * Catch ----
# catch_data already runs to projyr, so new_endyr exists as a projection row with
# NA catch. Fill it -- appending duplicates (Fleet_code, Year).
catch_rows <- which(est$catch_data$Year == new_endyr)
if (length(catch_rows)) {
  est$catch_data$Catch[catch_rows] <- 99999               # TODO: new-year fishery catch (THOUSAND t, as ADMB obs_catch)
} else {
  catch_row       <- est$catch_data[which.max(est$catch_data$Year), ]
  catch_row$Year  <- new_endyr
  catch_row$Catch <- 99999                                # TODO: as above
  est$catch_data  <- rbind(est$catch_data, catch_row)
}

# * Composition ----
# Terminal-year comps copied for the column shape, with Sample_size = 0 so they
# carry no weight if the workbook is fitted before the real ones arrive.
# TODO: overwrite Comp_* and Sample_size with the new-year comps.
comp_new <- est$comp_data %>%
  dplyr::group_by(Fleet_code, Age0_Length1) %>%
  dplyr::slice(dplyr::n()) %>%
  dplyr::mutate(Year = new_endyr, Sample_size = 0) %>%
  as.data.frame()
est$comp_data <- rbind(est$comp_data, comp_new) %>%
  dplyr::arrange(Fleet_code, Age0_Length1, Year)

# * Weight at age ----
# Every Wt_index must span the hindcast or the model will not build. Terminal
# year carried forward.
# TODO: replace with the new year's weight-at-age.
wt_new <- est$weight %>%
  dplyr::group_by(Wt_index, Sex) %>%
  dplyr::slice(dplyr::n()) %>%
  dplyr::mutate(Year = new_endyr) %>%
  as.data.frame()
est$weight <- rbind(est$weight, wt_new) %>%
  dplyr::arrange(Wt_index, Sex, Year)

# * Survey indices ----
# Placeholder new-year rows for each survey (BTS, ATS, ATS age-1, AVO); CPUE
# ended in 1976 and gets none.
#
# Year is written NEGATIVE -- the "predicted, not fitted" convention already
# used for the dropped terminal ATS_1 observation. A placeholder with a positive
# year would be fitted: BTS and ATS_1 solve q analytically, so 99999 enters the
# mean and drags q (+31% on BTS, measured), corrupting index_hat over the whole
# 1982-2024 series while the model still builds.
#
# TODO: paste the real Observation and Log_sd on each fleet's own scale
# (BTS = MVN covariance, ATS = sqrt(log(CV^2+1)), AVO/CPUE = absolute SD), then
# FLIP THE YEAR POSITIVE. For BTS that also needs the new VAST covariance --
# index_cov$BTS must be square on the fitted rows, so the build fails until it
# is supplied. That failure is deliberate: it is what stops a real observation
# being fitted against a stale Sigma.
add_index <- function(index_data, fleet, year, obs = 99999, log_sd = 99999) {
  rows <- index_data[index_data$Fleet_name == fleet, ]
  if (nrow(rows) == 0) return(index_data)
  tmpl <- rows[nrow(rows), ]
  tmpl$Year <- -abs(year); tmpl$Observation <- obs; tmpl$Log_sd <- log_sd
  rbind(index_data, tmpl)
}
for (fl in c("BTS", "ATS", "ATS_1", "AVO"))
  est$index_data <- add_index(est$index_data, fl, new_endyr)

# * BTS index covariance ----
# Left alone deliberately. The placeholder BTS row carries a negative year, so
# it is not a fitted row and Sigma still matches at 42x42. It only needs growing
# when the real observation goes in, and that is the VAST matrix's job, not an
# extrapolation from here.
bts_cov <- est$index_cov[["BTS"]]
n_bts_fit <- sum(est$index_data$Fleet_name == "BTS" & est$index_data$Year > 0 &
                   est$index_data$Year <= new_endyr &
                   est$index_data$Observation > 0)
if (!is.null(bts_cov) && nrow(bts_cov) != n_bts_fit) {
  stop("index_cov$BTS is ", nrow(bts_cov), "x", ncol(bts_cov), " but BTS has ",
       n_bts_fit, " fitted rows. Supply the matching VAST covariance.",
       call. = FALSE)
}

# Check ----
# Build before writing so a structurally broken roll fails here, not next year
# mid-fit. DebugBuild runs MakeADFun without optimising; calling data_check()
# directly instead would fail on defaults fit_mod() fills in.
build <- tryCatch(
  suppressWarnings(suppressMessages(
    fit_mod(data_list = est, inits = NULL, file = NULL,
            estimateMode = "DebugBuild", msmMode = "SingleSpecies",
            initMode = "NonEquilibrium",
            M1Fun = build_M1(updateM1 = TRUE, M1_model = "fixed"),
            fit_control = fit_control(verbose = 0)))),
  error = function(e) e)
if (inherits(build, "error")) {
  stop("The rolled data_list does not build:\n", conditionMessage(build),
       call. = FALSE)
}
message("Builds cleanly (estimateMode = \"DebugBuild\").")

# Save data ----
xlsx <- paste0("Data/EBS_", substr(new_endyr, 3, 4),
               "_pollock_m23_rceattle_full_1964-", new_endyr, ".xlsx")
write_data(est, file = xlsx)
message("Rolled EBS pollock data from endyr ", old_endyr, " to ", new_endyr,
        " -> ", xlsx, "\nFill the placeholders before fitting.")
