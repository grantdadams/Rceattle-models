# =============================================================================
# EBS pollock 2024 -- roll the assessment data forward one year
# =============================================================================
# Extends the 2024 ADMB-bridge build (from "01-build-data.R") to a new end
# year, appending placeholder rows (99999 / TODO) for the new year's catch, comps,
# and survey indices; fill them with real data, then refit with
# "04-fit-and-diagnostics.R". The bridge encoding already lives in the xlsx
# and round-trips, so this only adds records -- it does not re-derive the config.
# https://grantdadams.github.io/Rceattle/articles/data-without-excel.html
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
# Carry the last catch row forward as a template (one fishery).
catch_row       <- est$catch_data[nrow(est$catch_data), ]
catch_row$Year  <- new_endyr
catch_row$Catch <- 99999                                  # TODO: new-year fishery catch (THOUSAND t, as ADMB obs_catch)
est$catch_data  <- rbind(est$catch_data, catch_row)

# * Composition ----
# Duplicate each fleet x type's terminal-year comp for the right column shape.
# TODO: overwrite Comp_* proportions and Sample_size with the real new-year comps.
comp_new <- est$comp_data %>%
  dplyr::group_by(Fleet_code, Age0_Length1) %>%
  dplyr::slice(dplyr::n()) %>%
  dplyr::mutate(Year = new_endyr) %>%
  as.data.frame()
est$comp_data <- rbind(est$comp_data, comp_new) %>%
  dplyr::arrange(Fleet_code, Age0_Length1, Year)

# * Survey indices ----
# Placeholder new-year obs for each survey (BTS, ATS, ATS age-1, AVO); CPUE ended in
# 1976 and gets none. TODO: fill Observation and Log_sd on each fleet's own scale
# (BTS = MVN covariance, ATS = sqrt(log(CV^2+1)), AVO/CPUE = absolute SD).
add_index <- function(index_data, fleet, year, obs = 99999, log_sd = 99999) {
  rows <- index_data[index_data$Fleet_name == fleet, ]
  if (nrow(rows) == 0) return(index_data)
  tmpl <- rows[nrow(rows), ]
  tmpl$Year <- year; tmpl$Observation <- obs; tmpl$Log_sd <- log_sd
  rbind(index_data, tmpl)
}
for (fl in c("BTS", "ATS", "ATS_1", "AVO"))
  est$index_data <- add_index(est$index_data, fl, new_endyr)

# Save data ----
message("Rolled EBS pollock data from endyr ", old_endyr, " to ", new_endyr,
        " (placeholders marked 99999 -- fill before fitting).")
write_data(est, file = "Data/EBS_25_pollock_m23_rceattle_full_1964-2025.xlsx")
