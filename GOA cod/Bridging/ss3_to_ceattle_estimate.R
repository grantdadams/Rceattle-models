# =============================================================================
# Full-MLE estimation of the bridged GOA Pcod model.
#
# Sources ss3_to_ceattle_test.R first (FP bridge: load SS3 -> inject -> FP fit)
# and then estimates log_R0, rec_dev, init_dev, log_M1, beta_linkage,
# index_log_q, and sel base / dev. Growth uses growthFun_est_spec (SS3 priors
# on K / L1 / Linf).
#
# Honors SS3 ctl PHASE: any param with PHASE < 0 in SS3 is fixed at its
# injected (SS3 MLE) value in Rce via a `map = NA` patch. Currently covers:
#   - DoubleNormal sel base params P1..P6 (size_selex_parms$PHASE)
#   - log_Finit (SR_regime PHASE = -3, injected from SS3 SR_regime MLE)
# Per-block sel devs and dev_PH are NOT yet handled — devs stay estimable
# under the N(0, sigma=1.0) regularizing prior.
# =============================================================================

source("ss3_to_ceattle_forward_pass.R")

# CAAL kernel + bin alignment now match SS3 exactly; the historical 1/45
# downweight is no longer needed.
cod_pcod$fleet_control$CAAL_weights <- 1

# Restore the regularizing prior on sel deviates (FP set the -1 sentinel
# because injected values aren't true devs; estimation needs the prior).
# sigma=0.3 is closer to SS3's effective regularization scale; sigma=1.0
# let devs explode (-5 to +4), overfitting length comp via sel devs and
# producing a discontinuous-likelihood at convergence.
# Same applies to LLSrv's IID q-dev prior (was sigma=1.0 from the
# converter default Time_varying_q_sd_prior=1, letting Survey NLL drop
# 67 below SS3 via overfitting per-year q deviations).
active_fi <- which(cod_pcod$fleet_control$Fleet_name %in% active_sel_fleets)
cod_pcod$fleet_control$Time_varying_sel_sd_prior[active_fi] <- 0.3

# CRITICAL: build_params writes sel_dev_log_sd / index_q_dev_log_sd into the
# parameter list from data_list$fleet_control AT mod0 BUILD TIME. mod0 was
# built with Time_varying_sel_sd_prior = -1 (FP sentinel: skip prior), so
# inits$sel_dev_log_sd is currently log(exp(-999)) = -999 (skip prior).
# Likewise inits$index_q_dev_log_sd is log(1.0) = 0 (sigma=1 for LLSrv).
# Updating fleet_control here doesn't re-run build_params, so we have to
# write the new sigmas directly into `inits` for them to reach the cpp.
inits$sel_dev_log_sd[active_fi] <- log(0.3)
if (length(llsrv_idx)) inits$index_q_dev_log_sd[llsrv_idx] <- log(0.3)
cat(sprintf("\n[dev-priors] inits$sel_dev_log_sd[active] <- log(0.3) = %.3f\n", log(0.3)))
cat(sprintf("[dev-priors] inits$index_q_dev_log_sd[LLSrv] <- log(0.3) = %.3f\n", log(0.3)))


# =============================================================================
# Map override: honor SS3 PHASE
# =============================================================================
# Walk SS3 ctl PHASE columns and emit a map-patch dictionary keyed by Rce
# slot. Build_map runs first (with fit_mod's defaults via mod0$data_list),
# then we NA-out the patched entries and refactor.

# SS3 DoubleNormal Par_i -> Rce (array, slot) mapping. Rce stores 6 slots
# split across two arrays:
#   sel_inf[1, flt, sx]      = P1 peak
#   sel_inf[2, flt, sx]      = P6 logit(right_floor)
#   sel_inf[3, flt, sx]      = P5 logit(left_floor / init)
#   log_sel_slp[1, flt, sx]  = P3 log(sigma_ascending)
#   log_sel_slp[2, flt, sx]  = P4 log(sigma_descending)
#   log_sel_slp[3, flt, sx]  = P2 top-width logit
.ss3_to_rce_sel <- list(
  list(arr = "sel_inf",     slot = 1),  # P1 peak
  list(arr = "log_sel_slp", slot = 3),  # P2 top-width
  list(arr = "log_sel_slp", slot = 1),  # P3 asc-sigma
  list(arr = "log_sel_slp", slot = 2),  # P4 desc-sigma
  list(arr = "sel_inf",     slot = 3),  # P5 init-floor
  list(arr = "sel_inf",     slot = 2)   # P6 final-floor
)

apply_ss3_sel_phase_fixes <- function(map_list, ctllist, fleet_meta) {
  sse <- ctllist$size_selex_parms
  pat <- "^SizeSel_P_([1-6])_([A-Za-z]+)\\(([0-9]+)\\)$"
  m <- regmatches(rownames(sse), regexec(pat, rownames(sse)))
  n_fix <- 0L
  for (i in seq_along(m)) {
    if (length(m[[i]]) < 4L) next
    if (as.numeric(sse$PHASE[i]) >= 0) next   # estimated, leave alone
    p_idx    <- as.integer(m[[i]][2])
    flt_name <- m[[i]][3]
    flt_row  <- which(fleet_meta$name == flt_name)
    if (length(flt_row) == 0L) next            # inactive fleet
    map_info <- .ss3_to_rce_sel[[p_idx]]
    # 3D array: [slot, fleet, sex]. Fix every sex (only nsex=1 for Pcod).
    map_list[[map_info$arr]][map_info$slot, flt_row, ] <- NA
    n_fix <- n_fix + 1L
    cat(sprintf("  fix: SS3 P%d %s (PHASE=%d) -> Rce %s[%d, %d, ]\n",
                p_idx, flt_name, as.integer(sse$PHASE[i]),
                map_info$arr, map_info$slot, flt_row))
  }
  cat(sprintf("[phase-fix] %d Rce sel base slots fixed at SS3 PHASE<0 values\n",
              n_fix))
  map_list
}

# =============================================================================
# BlockDev: switch Time_varying_sel to the SS3-faithful enum (= 6)
# =============================================================================
# Rceattle now supports `Time_varying_sel = "BlockDev"` natively (added to
# [Rceattle/R/0-constants.R](../../Rceattle/R/0-constants.R) and handled in
# build_map_selectivity DoubleNormal branch + rearrange_data prior_weight
# populator). For each (fleet, year) in a sub-block:
#   * base sel parameter remains estimable
#   * dev cells in sub-block years share ONE estimable label per sub-block
#   * dev cells in non-block years are NA-locked at 0
#   * sel_inf_dev_prior_weight[fleet, year] = 1/N (auto-populated from
#     Selectivity_block in rearrange_data); the per-year cpp prior loop
#     collapses to exactly one N(0, sigma) contribution per sub-block.
#
# Flip ALL active sel fleets to BlockDev. The build_map_selectivity
# DoubleNormal branch NA-locks every dev cell first, then factor-shares
# only within sub-blocks (max_block > 0 case). So fleets with no sel
# block (e.g. LLSrv) get sel-devs fully NA-locked at 0 -- matching SS3,
# which doesn't estimate any time-varying sel for those fleets. This
# replaces the previous "leave LLSrv at IID" which left 6*53 = 318
# unconstrained sel-dev cells per fleet.
active_fi <- which(cod_pcod$fleet_control$Fleet_name %in% active_sel_fleets)
cod_pcod$fleet_control$Time_varying_sel[active_fi] <- "BlockDev"
cat("\n[blockdev] Time_varying_sel set to 'BlockDev' for active fleets:\n  ",
    paste(cod_pcod$fleet_control$Fleet_name[active_fi], collapse = ", "),
    "\n", sep = "")


# Build map externally, patch, refactor, hand to fit_mod.
# Important: copy the BlockDev override into mod0$data_list so build_map's
# tv-sel branch sees it (mod0 was built with the FP-default Time_varying_sel
# = "IID" because BlockDev didn't exist when it was constructed). Without
# this, the externally-built map uses IID semantics and our cod_pcod
# BlockDev override never reaches the map — leaving 6 * nyrs_hind dev cells
# estimable per active fleet (= the +1300 param-count bloat).
.dl_for_map <- mod0$data_list
.dl_for_map$fleet_control$Time_varying_sel <- cod_pcod$fleet_control$Time_varying_sel
.dl_for_map$fleet_control$Catchability     <- cod_pcod$fleet_control$Catchability
.dl_for_map$fleet_control$Time_varying_q   <- cod_pcod$fleet_control$Time_varying_q
.dl_for_map$catch_data <- cod_pcod$catch_data   # has Selectivity_block populated
.dl_for_map$index_data <- cod_pcod$index_data

cat("\n--- Building map + applying SS3 PHASE fixes ---\n")
.fitmap <- Rceattle::build_map(
  data_list  = .dl_for_map,       # BlockDev + Selectivity_block + linkage_table
  params     = inits,
  debug      = FALSE,
  random_rec = FALSE,
  random_sel = FALSE
)
.fitmap$mapList <- apply_ss3_sel_phase_fixes(.fitmap$mapList, ctllist, fleet_meta)

# Fix log_Finit at the injected SS3 MLE (SS3 SR_regime PHASE = -3).
if ("log_Finit" %in% names(.fitmap$mapList)) {
  .fitmap$mapList$log_Finit[] <- NA
  cat("[phase-fix] log_Finit fixed at injected SS3 SR_regime MLE\n")
}

# BlockDev factor-sharing + non-block NA-out is now handled inside
# build_map_selectivity (package side) — no manual patches needed here.
.fitmap$mapFactor <- lapply(.fitmap$mapList, factor)


# =============================================================================
# Estimation
# =============================================================================
# --- Pre-flight: build obj only (estimateMode = 3 skips nlminb) and check ---
# that the gradient at the starting point is finite. NaN gradient at the
# start usually = (a) an estimated param at log(0) or some sentinel, (b)
# a discontinuous kernel, or (c) a fixed parameter accidentally still in
# obj$par. We need to know WHICH parameter before nlminb dies.
cat("\n--- Pre-flight: build obj + check gradient at SS3-injected MLEs ---\n")
.preflight <- Rceattle::fit_mod(
  data_list    = cod_pcod,
  inits        = inits,
  map          = .fitmap,
  estimateMode = 3,                              # build obj, skip nlminb
  initMode     = "NonEquilibriumScaled",
  growthFun    = growthFun_est_spec,
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  fit_control  = fit_control(phase = FALSE, verbose = 1)
)
.obj0 <- .preflight$obj
.par0 <- .obj0$par
.g0   <- as.numeric(.obj0$gr(.par0))
.f0   <- as.numeric(.obj0$fn(.par0))
.bad  <- which(!is.finite(.g0))
cat(sprintf("\n[preflight] obj$fn = %.6g | obj$par length = %d | %d / %d NaN/Inf gradients\n",
            .f0, length(.par0), length(.bad), length(.g0)))

if (length(.bad) > 0) {
  pn <- names(.par0)
  fam_tbl <- sort(table(if (is.null(pn)) rep("?", length(.bad))
                        else pn[.bad]), decreasing = TRUE)
  cat("\n[preflight] NaN-gradient parameter families:\n")
  print(head(fam_tbl, 15))
  cat("\n[preflight] First 20 NaN entries:\n")
  for (i in head(.bad, 20)) {
    nm <- if (is.null(pn)) sprintf("par_%d", i) else pn[i]
    cat(sprintf("  [%5d] %-25s value=%12.6g grad=%s\n",
                i, nm, .par0[i], as.character(.g0[i])))
  }
  stop("NaN gradient at start point — fix before running nlminb")
}
cat("[preflight] gradient finite, proceeding to optimization\n")


cat("\n--- Full MLE estimation (no phasing; user-supplied map) ---\n")
# Phasing OFF here because fit_mod's internal phase loop rewrites the map
# at each step, which would conflict with our PHASE-honoring map override.
# If single-shot optimization converges we can layer phasing back on by
# extending `apply_ss3_sel_phase_fixes()` to re-fire after each phase step.
cod_pcod_est <- Rceattle::fit_mod(
  data_list    = cod_pcod,
  inits        = inits,
  map          = .fitmap,
  estimateMode = 1,
  initMode     = "NonEquilibriumScaled",
  growthFun    = growthFun_est_spec,
  M1Fun        = M1_block,
  random_rec   = FALSE,
  msmMode      = 0,
  fit_control  = fit_control(phase = FALSE, verbose = 1,
                             nlminb_control = list(eval.max = 5000,
                                                   iter.max = 2000,
                                                   trace = 0))
)


# =============================================================================
# Post-estimation diagnostics
# =============================================================================

# --- R / Bio / SSB vs SS3 ---
ny <- length(years_hind)
bio_est <- as.numeric(cod_pcod_est$quantities$biomass[1, 1:ny])
ssb_est <- as.numeric(cod_pcod_est$quantities$ssb[1, 1:ny])
R_est   <- as.numeric(cod_pcod_est$quantities$R[1, 1:ny])

cat("\n=== Estimation: relative errors vs SS3 ===\n")
diag_errors(R_est,   ss3_R,   "R")
diag_errors(bio_est, ss3_bio, "Bio")
diag_errors(ssb_est, ss3_ssb, "SSB")

cat("\n=== TS head + tail (estimation) ===\n")
print(data.frame(
  Year    = c(head(years_hind, 5), tail(years_hind, 5)),
  Bio_SS3 = c(head(ss3_bio, 5), tail(ss3_bio, 5)),
  Bio_Est = c(head(bio_est, 5), tail(bio_est, 5)),
  SSB_SS3 = c(head(ss3_ssb, 5), tail(ss3_ssb, 5)),
  SSB_Est = c(head(ssb_est, 5), tail(ssb_est, 5)),
  R_SS3   = c(head(ss3_R, 5),   tail(ss3_R, 5)),
  R_Est   = c(head(R_est, 5),   tail(R_est, 5))
))


# --- Grouped NLL components ---
cat("\n=== Estimation: grouped NLL components vs SS3 ===\n")
jnll <- cod_pcod_est$quantities$jnll_comp
rce_tot <- rowSums(jnll)
rce_lab <- rownames(jnll)
pick <- function(...) {
  s <- 0
  for (n in c(...)) {
    h <- grep(n, rce_lab, ignore.case = TRUE)
    if (length(h) > 0) s <- s + sum(rce_tot[h])
  }
  s
}
ss3_lik <- setNames(ss3_rep$likelihoods_used[, "values"],
                    rownames(ss3_rep$likelihoods_used))
nll_cmp <- rbind(
  data.frame(Component = "Survey index",       SS3 = ss3_lik["Survey"],        Rce = pick("Index")),
  data.frame(Component = "Catch",              SS3 = ss3_lik["Catch"],         Rce = pick("Catch data")),
  data.frame(Component = "Length comp",        SS3 = ss3_lik["Length_comp"],   Rce = pick("Composition")),
  data.frame(Component = "Age/CAAL comp",      SS3 = ss3_lik["Age_comp"],      Rce = pick("CAAL")),
  data.frame(Component = "Recruitment dev",    SS3 = ss3_lik["Recruitment"],   Rce = pick("Recruitment dev")),
  data.frame(Component = "Init eq / init dev", SS3 = ss3_lik["InitEQ_Regime"], Rce = pick("Initial abundance")),
  data.frame(Component = "Parm priors",        SS3 = ss3_lik["Parm_priors"],
             Rce = pick("M prior", "Linkage-table priors", "Catchability prior", "Stock-recruit prior")),
  data.frame(Component = "Parm devs (sel+q)",  SS3 = ss3_lik["Parm_devs"],
             Rce = pick("Selectivity deviates", "Catchability deviates"))
)
nll_cmp$Diff <- signif(nll_cmp$Rce - nll_cmp$SS3, 4)
nll_cmp$SS3  <- signif(nll_cmp$SS3, 6)
nll_cmp$Rce  <- signif(nll_cmp$Rce, 6)
print(nll_cmp, row.names = FALSE)
cat(sprintf("\nTOTAL  SS3: %.4f  Rce: %.4f  Diff: %+.4f\n",
            ss3_lik["TOTAL"], sum(rce_tot), sum(rce_tot) - ss3_lik["TOTAL"]))


# --- Hessian diagnostic ---
# Non-pos-def Hessian almost always = (a) param at a boundary, (b) unidentified
# (zero gradient and zero column), or (c) flat ridge with a correlated partner.
cat("\n=== Hessian diagnostic ===\n")
obj_est <- cod_pcod_est$obj
opt_par <- cod_pcod_est$opt$par %||% obj_est$env$last.par.best
par_names <- names(opt_par)

g <- as.numeric(obj_est$gr(opt_par))
cat(sprintf("Final par length: %d | max |grad|: %.3e (idx %d, %s) | mean: %.3e | #|grad|>1e-3: %d\n",
            length(opt_par), max(abs(g)), which.max(abs(g)),
            par_names[which.max(abs(g))] %||% "?",
            mean(abs(g)), sum(abs(g) > 1e-3)))

cat("\n--- Top 10 worst gradients ---\n")
for (j in order(abs(g), decreasing = TRUE)[1:10]) {
  nm <- par_names[j] %||% paste0("par_", j)
  cat(sprintf("  %-30s grad=%9.3e  value=%9.4f\n", nm, g[j], opt_par[j]))
}

H <- tryCatch(obj_est$he(opt_par), error = function(e) NULL)
if (is.null(H)) H <- tryCatch(numDeriv::jacobian(obj_est$gr, opt_par),
                              error = function(e) NULL)
if (!is.null(H)) {
  H <- (H + t(H)) / 2
  bad_rows <- which(!is.finite(rowSums(H)))
  cat(sprintf("\nHessian rows with NaN/Inf: %d\n", length(bad_rows)))
  if (length(bad_rows) > 0 && !is.null(par_names)) {
    cat("  Bad-row families (top 15):\n")
    print(head(sort(table(par_names[bad_rows]), decreasing = TRUE), 15))
  }
  good <- which(is.finite(rowSums(H)) & is.finite(colSums(H)))
  if (length(good) == 0L) {
    cat("Hessian is entirely NaN/Inf -- skipping eigen analysis.\n")
    cat("This is a convergence-failure signal: many params have undefined\n",
        "second derivatives at the partial fit point.\n", sep = "")
  } else {
    H <- H[good, good, drop = FALSE]
    par_names_sub <- par_names[good]
    opt_par_sub   <- opt_par[good]
    g_sub         <- g[good]

    eig <- eigen(H, symmetric = TRUE)
    cat(sprintf("Hessian %d x %d | eig range %.3e .. %.3e | non-pd: %d | near-zero: %d\n",
                nrow(H), ncol(H), min(eig$values), max(eig$values),
                sum(eig$values <= 0), sum(abs(eig$values) < 1e-8)))

    bad_eig <- which(eig$values < 1e-8)
    if (length(bad_eig) > 0) {
      cat(sprintf("\n--- Top loadings on %d non-pd eigenvectors ---\n",
                  min(5, length(bad_eig))))
      for (k in head(bad_eig, 5)) {
        v <- abs(eig$vectors[, k])
        cat(sprintf("\n  eig[%d] = %.3e:\n", k, eig$values[k]))
        for (j in order(v, decreasing = TRUE)[1:5]) {
          nm <- par_names_sub[j] %||% paste0("par_", j)
          cat(sprintf("    %-30s loading=%.4f  value=%9.4f  grad=%9.3e\n",
                      nm, v[j], opt_par_sub[j], g_sub[j]))
        }
      }
    }

    cat("\n--- 15 weakest |Hessian diagonal| ---\n")
    diagH <- diag(H)
    for (j in order(abs(diagH))[1:min(15, length(diagH))]) {
      nm <- par_names_sub[j] %||% paste0("par_", j)
      cat(sprintf("  %-30s diag=%9.3e  value=%9.4f  grad=%9.3e\n",
                  nm, diagH[j], opt_par_sub[j], g_sub[j]))
    }
  }
}


# --- Selectivity-at-age comparison ---
cat("\n=== Selectivity-at-age vs SS3 Asel2 (post-estimation) ===\n")
age_cols_ss3 <- as.character(0:(nages_pcod - 1))
sel_compare <- list()
for (i in seq_len(nrow(fleet_meta))) {
  ss3_num <- fleet_meta$ss3_num[i]
  ss3_sub <- ss3_rep$ageselex |>
    dplyr::filter(Factor == "Asel2", Fleet == ss3_num, Yr %in% years_hind)
  for (yi in seq_along(years_hind)) {
    yr <- years_hind[yi]
    rows_le <- ss3_sub |>
      dplyr::filter(Yr <= yr) |>
      dplyr::arrange(dplyr::desc(Yr))
    if (nrow(rows_le) == 0) next
    ss3_vec <- as.numeric(rows_le[1, age_cols_ss3])
    rce_vec <- as.numeric(cod_pcod_est$quantities$sel_at_age[i, 1, , yi])
    rel <- abs(rce_vec - ss3_vec) / pmax(abs(ss3_vec), 1e-4)
    sel_compare[[length(sel_compare) + 1]] <- data.frame(
      Fleet = fleet_meta$name[i], Year = yr,
      MaxRelErr = max(rel), MeanRelErr = mean(rel))
  }
}
print(do.call(rbind, sel_compare) |>
        dplyr::group_by(Fleet) |>
        dplyr::summarise(max_rel = max(MaxRelErr),
                         mean_rel = mean(MeanRelErr)))
