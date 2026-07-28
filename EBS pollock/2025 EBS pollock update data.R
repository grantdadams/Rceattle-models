# =============================================================================
# 2025 EBS pollock — roll the assessment data forward one year
# =============================================================================
# Takes the 2024 EBS pollock model configuration (the ADMB-bridge build written
# by "2024 EBS pollock data.R") and extends it to a 2025 end year, appending
# placeholder rows for the new year's catch, compositions, and survey indices.
# Fill the placeholders (marked 99999 / TODO) with the real 2025 observations,
# then refit with "2025 EBS pollock diagnostics.R".
#
# The ADMB-bridge encoding (observation-error scalings, avgsel penalty, the
# MVN BTS covariance, the CPUE fleet, etc.) already lives in the xlsx and
# round-trips through read_data()/write_data(), so it is preserved automatically
# — this script only adds new-year records, it does not re-derive the config.
# See vignette: https://grantdadams.github.io/Rceattle/articles/data-without-excel.html
# =============================================================================

library(Rceattle)
library(dplyr)

new_endyr <- 2025

# Data ----
est <- Rceattle::read_data(file = "Data/2024_EBS_pollock_m23_rceattle_full.xlsx")
old_endyr <- est$endyr

# * Controls ----
est$endyr <- new_endyr                                     # new terminal year

# * Catch ----
# One fishery in EBS pollock; carry the last catch row forward as a template.
catch_row       <- est$catch_data[nrow(est$catch_data), ]
catch_row$Year  <- new_endyr
catch_row$Catch <- 99999                                  # TODO: 2025 fishery catch (t)
est$catch_data  <- rbind(est$catch_data, catch_row)

# * Composition ----
# Duplicate the terminal-year comp of each fleet x data-type (age vs length) and
# stamp it with the new year, so the new-year comp has the right column shape.
# TODO: overwrite Comp_* proportions and Sample_size with the real 2025 comps.
comp_new <- est$comp_data %>%
  dplyr::group_by(Fleet_code, Age0_Length1) %>%
  dplyr::slice(dplyr::n()) %>%
  dplyr::mutate(Year = new_endyr) %>%
  as.data.frame()
est$comp_data <- rbind(est$comp_data, comp_new) %>%
  dplyr::arrange(Fleet_code, Age0_Length1, Year)

# * Survey indices ----
# Append a placeholder 2025 observation for each survey that would deliver data
# in an assessment year (BTS bottom-trawl, ATS acoustic-trawl, ATS age-1, AVO).
# The CPUE fleet ended in 1976 and gets no new row. TODO: fill Observation and
# Log_sd (on each fleet's own scale — see "2024 EBS pollock data.R": BTS uses the
# MVN covariance, ATS a log-scale SD sqrt(log(CV^2+1)), AVO/CPUE an absolute SD).
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
        " (placeholders marked 99999 — fill before fitting).")
write_data(est, file = "Data/2025_EBS_pollock_m23_rceattle_full.xlsx")
