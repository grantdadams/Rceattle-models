# Roll the GOA pollock data forward through the 2026 assessment year.
# Catch and weight-at-age must span every model year; survey indices and age
# comps are added as placeholders for the annually-sampled fleets. Placeholder
# observations (catch/index) carry a 99999 sentinel; placeholder age comps carry
# Sample_size = 0 (no likelihood weight) until @kalei fills the realized data.
# Data-list reference:
# https://grantdadams.github.io/Rceattle/articles/data-without-excel.html

library(Rceattle)
library(dplyr)

# Data ----
# Current assessment data (1970-2024), from "01-build-data.R".
# Run from the "GOA pollock" project root so the relative Data/ paths resolve.
#
# The workbook is the canonical data source -- Data/*.Rdata is gitignored, so
# the xlsx is what travels with the repo and what Cole edits directly.
pollock25 <- read_data("Data/GOA_25_pollock_single_species_1970-2024.xlsx")
pollock25$estDynamics <- "Estimated"

SHELIKOF <- 1L; BOTTOM_TRAWL <- 2L; FISHERY <- 8L
target_year <- 2026
pollock25$endyr <- target_year

# Repeat a row across every year from just after it through the terminal year.
to_terminal <- function(row) {
  if (row$Year >= target_year) return(row[0, ])
  do.call(rbind, lapply((row$Year + 1):target_year, function(y) { row$Year <- y; row }))
}
last_by <- function(df, ...) df %>% group_by(...) %>% slice(n()) %>% as.data.frame()

# * Catch (fishery = fleet 8; required every year) ----
new_catch <- to_terminal(tail(pollock25$catch_data, 1))
new_catch$Catch <- 99999                     # @kalei realized/assumed catch
pollock25$catch_data <- rbind(pollock25$catch_data, new_catch)

# * Weight-at-age (required every year; hold the terminal year until updated) ----
new_wt <- do.call(rbind, lapply(split(last_by(pollock25$weight, Wt_index), 1:5), to_terminal))
pollock25$weight <- rbind(pollock25$weight, new_wt) %>% arrange(Wt_index, Year)
# Projection weight-at-age (used only by the HCR projection).
pollock25$ration_data <- rbind(pollock25$ration_data,
                               to_terminal(tail(pollock25$ration_data, 1))) %>% arrange(Year)

# * Survey index (Shelikof acoustic = fleet 1, sampled annually) ----
new_idx <- to_terminal(last_by(pollock25$index_data, Fleet_code) %>% filter(Fleet_code == SHELIKOF))
new_idx$Observation <- 99999                 # @kalei Shelikof biomass
new_idx$Log_sd      <- 99999                  # @kalei its CV
pollock25$index_data <- rbind(pollock25$index_data, new_idx)
# @kalei add the bottom-trawl (fleet 2, biennial), ADF&G (3) and summer (6)
#        acoustic indices for their sampled years.

# * Age compositions (fishery and Shelikof, sampled annually) ----
# Inert placeholder rows (Sample_size = 0) so the terminal years are wired up but
# carry no likelihood weight until @kalei enters the observed proportions and N.
last_age <- pollock25$comp_data %>% filter(Fleet_code %in% c(FISHERY, SHELIKOF),
                                           Age0_Length1 == 0)
new_comp <- do.call(rbind, lapply(split(last_by(last_age, Fleet_code), 1:2), to_terminal))
new_comp$Sample_size <- 0
pollock25$comp_data <- rbind(pollock25$comp_data, new_comp) %>%
  arrange(Fleet_code, Age0_Length1, Year)
# @kalei add fishery/Shelikof length comps and the periodic-survey comps for
#        their sampled years.

# Save ----
# A workbook, not an .Rdata: this is the hand-off @kalei fills in, and the
# placeholder rows are meant to be edited by hand. Data/*.Rdata is gitignored,
# so an .Rdata here would neither travel with the repo nor be editable.
out <- sprintf("Data/GOA_26_pollock_single_species_1970-%d.xlsx", target_year)
write_data(pollock25, out)
message("Wrote ", out)
