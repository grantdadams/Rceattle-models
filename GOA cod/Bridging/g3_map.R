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

# Under RCE_SEL_PARITY the selectivity design columns ARE SS3's coefficients, so
# two linkage rows carrying the same design_col are the same SS3 parameter -- the
# Srv / Srv_ae1 ageing-error split is one curve in SS3 and must be one
# coefficient here. The offset is applied per fleet in the template, so both rows
# have to exist; sharing a map LEVEL makes them one estimated parameter.
if (identical(tolower(Sys.getenv("RCE_SEL_PARITY", "false")), "true")) {
  tbl <- mod0$data_list$linkage_table
  f   <- as.character(map_g3$mapFactor$beta_linkage)
  sel <- which(tbl$process == "sel" & !is.na(f))
  if (length(sel)) {
    key <- paste(tbl$param[sel], tbl$design_col[sel], sep = "|")
    f[sel] <- paste0("selp", match(key, unique(key)))
    map_g3$mapFactor$beta_linkage <- factor(f)
    cat(sprintf("[sel parity] %d selectivity rows share %d coefficients\n",
                length(sel), length(unique(key))))
  }
}

# Hold what SS3 could not identify, at SS3's own value. RCE_HOLD_SE is the
# standard-error threshold: on GOA cod 22 parameters have SE > 50 -- 15 top_logit
# and 7 descend_se -- where AI cod and EBS cod have NONE above 10. Estimating them
# reproduces SS3's parameter COUNT and its flat directions with it, and Rceattle's
# dgesv refuses the resulting singular system where SS3 inverts it and reports the
# large SEs instead. Fixing at the FITTED value, not at 0: top_logit is
# logit(plateau fraction), so 0 means a plateau halfway to maxlen where the fitted
# -5 means 0.6% -- a different curve. For the 13 descend_se DEVmults the fitted
# value IS ~3e-07, so those are effectively fixed at zero.
# OFF by default: the singularity it was written for was an artifact of
# newtonsteps > 0, not of the parameters. See the note in GOA-parameter-parity.md.
.hold_se <- suppressWarnings(as.numeric(Sys.getenv("RCE_HOLD_SE", "Inf")))
if (is.finite(.hold_se)) {
  tbl <- mod0$data_list$linkage_table
  # SS3 label for a selectivity linkage row, matching the forward pass's scheme.
  big <- names(.ss3_report_values)[
    vapply(names(.ss3_report_values), function(n) {
      i <- which(names(.ss3_P$sd) == n)
      length(i) == 1L && is.finite(.ss3_P$sd[[i]]) && .ss3_P$sd[[i]] > .hold_se
    }, logical(1))]
  if (length(big)) {
    cat(sprintf("[hold] %d SS3 parameters have SE > %g\n", length(big), .hold_se))
    # sel_dn6 base slots
    g <- as.character(map_g3$mapFactor$sel_dn6)
    nmv <- c("peak","top_logit","ascend_se","descend_se","start_logit","end_logit")
    nheld <- 0L
    # sel_dn6 is [6, n_flt, sex], so slot (fleet, k) sits at (fleet-1)*6 + k.
    # Look the SE up directly rather than through a name-set membership test.
    for (fi in active_sel) for (k in seq_along(nmv)) {
      stem <- .ss3_sel_stem(fi, k)
      sdv  <- if (stem %in% names(.ss3_P$sd)) .ss3_P$sd[[stem]] else NA_real_
      j <- (fi - 1L) * 6L + k
      if (j > length(g)) next
      if (is.finite(sdv) && sdv > .hold_se && !is.na(g[j])) {
        g[j] <- NA; nheld <- nheld + 1L
        cat(sprintf("  [hold] sel_dn6 %s %s (SS3 SE %.1f)\n",
                    fleet_meta$name[fi], nmv[k], sdv))
      }
    }
    map_g3$mapFactor$sel_dn6 <- factor(g)
    # linkage coefficients: the design column name carries the SS3 identity
    f <- as.character(map_g3$mapFactor$beta_linkage)
    nb <- 0L
    for (i in seq_len(nrow(tbl))) {
      if (is.na(f[i]) || tbl$process[i] != "sel") next
      dc <- as.character(tbl$design_col[i])
      # s<src>p<k>_blk<yr> / _dev<yr> -> the SS3 parameter it came from
      m <- regmatches(dc, regexec("^s([0-9]+)p([0-9]+)_(blk|dev)([0-9]+)$", dc))[[1]]
      if (length(m) != 5L) next
      fi <- match(as.integer(m[2]), fleet_meta$ss3_num)
      if (is.na(fi)) next
      stem <- .ss3_sel_stem(fi, as.integer(m[3]))
      pat  <- if (m[4] == "blk") "_BLK[0-9]+repl_" else "_DEVmult_"
      hit  <- grep(paste0("^", gsub("([()])", "\\\\\\1", stem), pat, m[5], "$"), big)
      if (length(hit)) { f[i] <- NA; nb <- nb + 1L }
    }
    map_g3$mapFactor$beta_linkage <- factor(f)
    cat(sprintf("[hold] held %d sel_dn6 slots and %d linkage coefficients\n", nheld, nb))
  }
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

