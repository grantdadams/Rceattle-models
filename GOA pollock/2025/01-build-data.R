# =============================================================================
# GOA pollock 2025 -- build the Rceattle data list
#
# Converts `read_dat()` / `prepare_pk_input()` into an Rceattle
# `data_list`, and writes the xlsx file the bridging / model scripts read.
#
# The "2025 model" uses pk24_12.txt and has:
#   * Dirichlet-multinomial age composition (fishery + surveys 1,2,3,6);
#   * Rogers et al. (2024) AR1 catchability on the Shelikof acoustic survey.
#   * normal priors on the logistic selectivity parameters.
#
# The inputs are identical to my previous 2024 Rceattle bridge, so this
# uses (`Data/2024pollock.Rdata`) -- which already ran
# `read_dat()` -- as the authoritative data source, and the existing
# `GOA_24_..._1970-2024.xlsx` as the structural skeleton (fleet_control, species
# controls, weight / comp metadata, maturity, ageing error, hard-coded M). All
# data blocks are then rebuilt from Cole's object so the conversion is explicit.
#
# Units: goa_pk biomass/recruitment in millions; Rceattle in absolute
# numbers, so survey indices are scaled by 1e6 (catch is in tons).
# Population weight: wt_pop = wt_srv2 (bottom-trawl), wt_spawn = wt_srv1 (Shelikof);
# Indexed by by pop_wt_index / ssb_wt_index in Rceattle.
# =============================================================================

library(Rceattle)
library(dplyr)
library(tidyr)

# ---- Inputs ---------------------------------------------------------------
load("Data/2024pollock_mfix.Rdata")       # corrected goa_pk model
pk <- read_data("Data/GOA_24_pollock_single_species_1970-2024.xlsx")   # data
dat  <- fit$input$dat
edat <- fit$obj$env$data
endyr <- 2024L
ages  <- 1:10
yrs   <- 1970:endyr

# ---- Controls --------------------------------------------------------------
pk$endyr <- endyr
pk$fleet_control$Fleet_type[4:5] <- "Off"  # age-1 / age-2 Shelikof indices off (log_q4/q5 mapped NA)

# The GOA_24 skeleton predates the alpha_wt_len / beta_wt_len control entries,
# so read_data() leaves them NULL and write_data() drops two rows from the
# control sheet. fit_mod() only uses them for length-based suitability
# (multispecies), so the package defaults are correct for this single-species
# model -- set them explicitly so the workbook round-trips.
pk$alpha_wt_len <- 1e-6
pk$beta_wt_len  <- 3

# ---- Aging-error matrix: normalize rows to sum to 1 --------
# age_trans has a missing ~1e-4 across ages 5-8.
# Rceattle re-normalizes the predicted comps and I updated goa_pk to
# normalize age_trans to align models.
ae_cols <- paste0("Obs_age", ages)
pk$age_error[, ae_cols] <- pk$age_error[, ae_cols] / rowSums(pk$age_error[, ae_cols])

# ---- Maturity-at-age: use goa_pk's `mat` vector (the 2024 ogive) ------------
pk$maturity[1, paste0("Age", ages)] <- dat$mat

# ---- Catch (tons; single fishery = fleet 8) --------------------------------
catch <- pk$catch_data[1, ]
catch_data <- do.call(rbind, lapply(yrs, function(y) { r <- catch; r$Year <- y; r }))
catch_data$Catch <- dat$cattot
catch_data$Log_sd <- dat$cattot_log_sd    # catch CV (0.05)
pk$catch_data <- catch_data

# ---- Environmental covariate for the AR1 (QAR1) catchability ---------------
# Standardized Ecov index (pk24_12.txt), observed 1983-2024. Used by the
# ar1()/Rogers-2024 catchability linkage in the bridging script.
pk$env_data <- data.frame(Year = dat$Ecov_obs_year, QcovPol = dat$Ecov_obs)

# ---- Composition (age + length) --------------------------------------------
# Fleet_codex: fishery = 8; srv1 = 1 (Shelikof AT),
# srv2 = 2 (NMFS BT), srv3 = 3 (ADF&G), srv6 = 6 (summer AT).
# Rceattle applies exp(-(Month/12) Z) survey timing, so goa_pk's
# fraction-of-year yrfrct is scaled to a month here (yrfrct * 12).
acomp <- function(fl, yrv, N, mon, props) {
  colnames(props) <- paste0("Comp_", seq_len(ncol(props)))
  cbind(data.frame(Year = yrv, Sample_size = N, Month = mon,
                   Fleet_code = fl, Age0_Length1 = 0L), props)
}
lcomp <- function(fl, yrv, N, mon, props) {
  colnames(props) <- paste0("Comp_", seq_len(ncol(props)))
  cbind(data.frame(Year = yrv, Sample_size = N, Month = mon,
                   Fleet_code = fl, Age0_Length1 = 1L), props)
}

# Per-year survey timing: yrfrct_srvN is indexed by calendar year (length =
# endyr - styr + 1); srv2's timing varies by year (0.543 / 0.584), so index it by
# each comp observation's year rather than taking a single value. -> month (x12).
smon <- function(s, yrv) edat[[paste0("yrfrct_srv", s)]][yrv - yrs[1] + 1] * 12
comp_blocks <- list(
  acomp(8, edat$fshyrs,     edat$multN_fsh,  0,                    edat$catp),
  acomp(1, edat$srv_acyrs1, edat$multN_srv1, smon(1, edat$srv_acyrs1),  edat$srvp1),
  acomp(2, edat$srv_acyrs2, edat$multN_srv2, smon(2, edat$srv_acyrs2),  edat$srvp2),
  acomp(3, edat$srv_acyrs3, edat$multN_srv3, smon(3, edat$srv_acyrs3),  edat$srvp3),
  acomp(6, edat$srv_acyrs6, edat$multN_srv6, smon(6, edat$srv_acyrs6),  edat$srvp6),
  lcomp(8, edat$fshlenyrs,   edat$multNlen_fsh,  0,                   edat$lenp),
  lcomp(1, edat$srv_lenyrs1, edat$multNlen_srv1, smon(1, edat$srv_lenyrs1), edat$srvlenp1),
  lcomp(2, edat$srv_lenyrs2, edat$multNlen_srv2, smon(2, edat$srv_lenyrs2), edat$srvlenp2),
  lcomp(3, edat$srv_lenyrs3, edat$multNlen_srv3, smon(3, edat$srv_lenyrs3), edat$srvlenp3),
  lcomp(6, edat$srv_lenyrs6, edat$multNlen_srv6, smon(6, edat$srv_lenyrs6), edat$srvlenp6)
)

comp_info <- pk$comp_data %>% distinct(Fleet_code, Fleet_name, Species, Sex)
pk$comp_data <- comp_info %>%
  full_join(bind_rows(comp_blocks), by = "Fleet_code") %>%
  dplyr::select(Fleet_name, Fleet_code, Species, Sex, Age0_Length1, Month, Year,
                Sample_size, paste0("Comp_", 1:10)) %>%
  arrange(Fleet_code, Age0_Length1, Year)

# ---- Survey indices (scaled to absolute numbers) ---------------------------
idx <- function(fl, yrv, obs, logsd, off = FALSE) data.frame(
  Fleet_code = fl, Year = if (off) -yrv else yrv,
  Observation = obs * 1e6, Log_sd = logsd)
index_rows <- rbind(
  idx(1, dat$srvyrs1, dat$indxsurv1, dat$indxsurv_log_sd1),
  idx(2, dat$srvyrs2, dat$indxsurv2, dat$indxsurv_log_sd2),
  idx(3, dat$srvyrs3, dat$indxsurv3, dat$indxsurv_log_sd3),
  idx(4, dat$srvyrs4, dat$indxsurv4, dat$indxsurv_log_sd4, off = TRUE),  # age-1, off
  idx(5, dat$srvyrs5, dat$indxsurv5, dat$indxsurv_log_sd5, off = TRUE),  # age-2, off
  idx(6, dat$srvyrs6, dat$indxsurv6, dat$indxsurv_log_sd6))
pk$index_data <- pk$index_data %>%
  distinct(Fleet_name, Fleet_code, Species, Month) %>%
  left_join(index_rows, by = "Fleet_code") %>%
  dplyr::filter(Fleet_code %in% 1:6)

# ---- Weight-at-age (Wt_index 1 fsh, 2 srv1, 3 srv2, 4 srv3, 5 srv6) --------
waa <- function(idx, mat) { colnames(mat) <- paste0("Age", ages)
  cbind(data.frame(Year = yrs, Wt_index = idx), mat) }
weight_rows <- rbind(waa(1, dat$wt_fsh), waa(2, dat$wt_srv1), waa(3, dat$wt_srv2),
                     waa(4, dat$wt_srv3), waa(5, dat$wt_srv6))
pk$weight <- pk$weight %>% group_by(Wt_index) %>% slice(1) %>%
  select(Wt_name, Wt_index, Species, Sex) %>%
  full_join(weight_rows, by = "Wt_index") %>% as.data.frame()

# ---- Projection weight-at-age --------------------------------
# the terminal weight-at-age used for the projection

# ---- fleet_control: composition likelihood + selectivity normalization -----
# Dirichlet-multinomial on the age comps (fishery + srv1/2/3/6); length comps
# stay multinomial in goa_pk.
pk$fleet_control$Comp_distribution <- "DirichletMultinomial"
# Selectivity normalization bin (the bin at which selectivity = 1).
# goa_pk normalizes the fishery at age 7 and the Shelikof block at age 3.
pk$fleet_control$Sel_norm_bin[7] <- 3
pk$fleet_control$Sel_norm_bin[8] <- 7

# ---- Write -----------------------------------------------------------------
# The workbook is the only output. Everything downstream (02-bridge.R,
# 03-model.R, 04-diagnostics.R, 05-update-data.R, dsem.R) reads it back with
# read_data(), so there is one source of truth and Cole can edit it directly.
# No .Rdata copy of the data_list is written -- a second serialization only
# drifts from the workbook once someone edits one and not the other.
xlsx <- "Data/GOA_25_pollock_single_species_1970-2024.xlsx"
write_data(pk, xlsx)
message("Wrote ", xlsx)

# ---- Round-trip check ------------------------------------------------------
# The workbook is the ONLY shared artifact -- Data/*.Rdata is gitignored (large,
# undiffable, rewritten every run), so everything downstream has to be
# reconstructable from this file via read_data(). Check the whole data_list
# rather than a few hand-picked blocks: the fleet_control flags this script sets
# (Comp_distribution, Sel_norm_bin, Fleet_type) and the control-sheet scalars
# matter just as much as the data matrices.
rt <- read_data(xlsx)

# Relative, not absolute: survey indices are ~1e9 after the 1e6 scaling, so an
# absolute tolerance would flag ordinary float64 round-off in the xlsx.
reldiff <- function(x, y) max(abs(x - y) / pmax(abs(x), 1), na.rm = TRUE)
compare <- function(x, y) {
  if (is.null(x) || is.null(y)) return(if (is.null(x) && is.null(y)) 0 else Inf)
  if (is.data.frame(x) && is.data.frame(y)) {
    if (!identical(dim(x), dim(y)) || length(setdiff(names(x), names(y)))) return(Inf)
    return(max(vapply(intersect(names(x), names(y)), function(cn) {
      xv <- x[[cn]]; yv <- y[[cn]]
      if (is.numeric(xv) && is.numeric(yv)) reldiff(xv, yv)
      else if (identical(as.character(xv), as.character(yv))) 0 else Inf
    }, numeric(1)), 0))
  }
  if (is.numeric(x) && is.numeric(y))
    return(if (length(x) != length(y)) Inf else reldiff(x, y))
  if (identical(as.character(x), as.character(y))) 0 else Inf
}

# read_data() legitimately adds single-species defaults, and drops three
# sex-ratio entries the schema marks "orphan" / "ignored if nsex = 1". Neither
# affects the fit -- verified by refitting from the workbook alone.
EXPECTED_ASYMMETRY <- c("R_sexr", "est_sex_ratio", "sex_ratio_sigma",
                        "Diet_distribution", "Diet_comp_weights")

shared <- setdiff(intersect(names(pk), names(rt)), EXPECTED_ASYMMETRY)
diffs  <- vapply(shared, function(nm) compare(pk[[nm]], rt[[nm]]), numeric(1))
lost   <- setdiff(setdiff(names(pk), names(rt)), EXPECTED_ASYMMETRY)

cat("== xlsx round-trip ==\n")
cat(sprintf("  %d/%d elements identical (tol 1e-12 relative)\n",
            sum(diffs <= 1e-12), length(diffs)))
if (length(lost))
  cat("  **CHECK** dropped by the workbook: ", paste(lost, collapse = ", "), "\n")
if (any(diffs > 1e-12)) {
  cat("  **CHECK** elements that did not survive:\n")
  for (nm in names(diffs)[diffs > 1e-12])
    cat(sprintf("    %-24s rel|diff| = %.3e\n", nm, diffs[[nm]]))
} else if (!length(lost)) {
  cat("  OK -- the workbook fully reconstructs the data_list\n")
}
