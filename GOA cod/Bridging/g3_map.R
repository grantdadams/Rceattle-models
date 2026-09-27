# =============================================================================
# The G3 free-parameter map: which parameters Rceattle estimates so that the
# comparison to SS3 is of the same model. Sourced by run_g3_goa.R and by
# profile_g3_goa.R, so the two cannot drift apart.
#
# Expects the forward pass to have been sourced already (mod0, ss3_map,
# ss3_rep, fleet_meta, active_sel) and `mode` to be "fixsel" or "freesel".
# =============================================================================
if (!exists("mode")) mode <- "fixsel"

map_g3 <- ss3_map
if (identical(mode, "fixsel")) {
  # Hold every selectivity linkage coefficient. The linkage table says which
  # rows are selectivity; the rest (growth, M) stay as the forward pass set them.
  tbl <- mod0$data_list$linkage_table
  sel_rows <- which(tbl$process == "sel")
  f <- map_g3$mapFactor$beta_linkage
  if (!is.null(f) && length(sel_rows)) {
    f <- as.character(f); f[sel_rows] <- NA; map_g3$mapFactor$beta_linkage <- factor(f)
  }
  cat(sprintf("\n[fixsel] held %d selectivity linkage coefficients; %d beta_linkage free\n",
              length(sel_rows), sum(!is.na(map_g3$mapFactor$beta_linkage))))
}

# SS3 estimates M (phase 5) and the pattern-24 base parameters; ss3_fix_map held
# log_M1 and every sel_dn6 slot, so free the ones SS3 moved. Without this
# Rceattle optimises 206 parameters against SS3's 330 and the comparison is of
# two different models.
if (!identical(Sys.getenv("RCE_G3_FREE", "true"), "false")) {
  # log_M1: M1_model 1 uses one scalar per species, slot [sp, 1, 1].
  f <- as.character(map_g3$mapFactor$log_M1)
  f[1] <- "1"; map_g3$mapFactor$log_M1 <- factor(f)
  # sel_dn6 for the fleets SS3 estimates, minus the slots it fixed. ss3_fix_map
  # already recorded those, so only re-free slots it did NOT name.
  sp <- ss3_rep$parameters
  est <- rownames(sp)[!is.na(sp$Phase) & sp$Phase > 0 & grepl("^Size_DblN", rownames(sp)) &
                      !grepl("BLK|DEVmult", rownames(sp))]
  nm  <- c("dn_peak","top_logit","ascend_se","descend_se","start_logit","end_logit")
  lab <- c("peak","top_logit","ascend_se","descend_se","start_logit","end_logit")
  g <- as.character(map_g3$mapFactor$sel_dn6)
  nxt <- suppressWarnings(max(as.integer(g), na.rm = TRUE)); if (!is.finite(nxt)) nxt <- 0L
  n_freed <- 0
  for (fi in active_sel) {
    for (k in seq_along(lab)) {
      hit <- grepl(sprintf("^Size_DblN_%s_%s\\(", lab[k], fleet_meta$name[fi]), est)
      if (!any(hit)) next
      j <- (fi - 1L) * 6L + k          # sel_dn6 is [6, n_flt, sex] -> column-major
      if (is.na(g[j])) { nxt <- nxt + 1L; g[j] <- as.character(nxt); n_freed <- n_freed + 1 }
    }
  }
  map_g3$mapFactor$sel_dn6 <- factor(g)
  cat(sprintf("[free] log_M1 slot 1; %d sel_dn6 base slots freed (SS3 estimates %d)\n",
              n_freed, length(est)))
}

