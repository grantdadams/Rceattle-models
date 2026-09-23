# =============================================================================
# Parity gates for an SS3 -> Rceattle bridge
#
# The bridge is finished when Rceattle, fitted from a cold start, lands on the
# same solution as the SS3 reference run. Three gates get there in order:
#
#   G1  forward state -- with SS3's MLE injected (estimateMode = 3), length-,
#       weight- and fecundity-at-age, the age-length key, selectivity, N-at-age,
#       SSB and recruitment match SS3 element by element.
#   G2  likelihood -- at SS3's MLE the Rceattle gradient is zero. Component
#       NLLs are listed beside SS3's; they may differ only by constants.
#   G3  cold start -- a fit from Rceattle's default starting values reaches the
#       objective, parameters, SSB and recruitment of the G2 point.
#
# Usage, from a stock folder (e.g. "AI cod - Dev"):
#   source("Bridging/ss3_to_ceattle_forward_pass.R")   # builds fp, ss3_rep
#   source("../SS3-bridge/parity_check.R")
#   parity_report(fp, ss3_rep)                         # G1 + G2
#   parity_g3(cold_fit, fp, ss3_rep)                   # after a cold-start fit
#
# SS3 quantities come from r4ss::SS_output(). SS3 reports numbers in thousands
# and biomass in metric tons; the bridge keeps those units in Rceattle.
# =============================================================================

# Relative error, with the denominator floored at 1e-8 of the largest SS3
# value so cells near zero (age-0 weight, tail recruitment) do not dominate.
.rel_err <- function(a, b) abs(a - b) / pmax(abs(b), 1e-8 * max(abs(b)))

# `scale = "abs"` for probabilities (age-length key, selectivity), which are
# already on a 0-1 scale and where relative error on a 1e-12 cell is noise.
.gate_row <- function(check, a, b, tol, scale = c("rel", "abs")) {
  scale <- match.arg(scale)
  a <- as.numeric(a); b <- as.numeric(b)
  ok <- is.finite(a) & is.finite(b)
  err <- if (!any(ok)) NA_real_ else if (scale == "abs") max(abs(a[ok] - b[ok])) else max(.rel_err(a[ok], b[ok]))
  data.frame(check = check, n = sum(ok), scale = scale, max_err = signif(err, 3),
             tol = tol, pass = isTRUE(err <= tol), stringsAsFactors = FALSE)
}

# SS3's age-length key is on its population length bins. Sum those bins into
# Rceattle's length bins (lower edges `rce_edges`) so the two keys compare
# bin for bin. Returns [length bin, age].
.ss3_alk_on_bins <- function(ss3_rep, rce_edges, subseas = 1) {
  alk <- ss3_rep$ALK
  key <- grep(sprintf("Sub_Seas: %d", subseas), dimnames(alk)[[3]], value = TRUE)[1]
  if (is.na(key)) key <- dimnames(alk)[[3]][1]
  m <- alk[, , key]
  pop_len <- as.numeric(rownames(m))
  m <- m[order(pop_len), , drop = FALSE]
  pop_len <- sort(pop_len)
  bin <- findInterval(pop_len, rce_edges, left.open = FALSE)
  bin[bin < 1] <- 1
  out <- matrix(0, length(rce_edges), ncol(m))
  for (j in seq_along(rce_edges)) {
    rows <- which(bin == j)
    if (length(rows)) out[j, ] <- colSums(m[rows, , drop = FALSE])
  }
  out
}

#' G1: forward state at SS3's MLE.
#' The default tolerance is Report.sso's printed precision: SS3 writes these
#' quantities to about six significant figures.
parity_g1 <- function(fp, ss3_rep, tol = 1e-5) {
  dl     <- fp$data_list
  q      <- fp$quantities
  nages  <- dl$nages[1]
  ages   <- seq_len(nages) - 1 + dl$minage[1]
  yrs    <- dl$styr:dl$endyr
  ey     <- length(yrs)                               # end-year column
  eg     <- ss3_rep$endgrowth[ss3_rep$endgrowth$Sex == 1, ]
  eg     <- eg[match(ages, eg$int_Age), ]

  rows <- list(
    .gate_row("length-at-age, Jan 1",  q$length_hat[1, 1, seq_len(nages), ey], eg$Len_Beg, tol),
    .gate_row("weight-at-age, Jan 1",  q$weight_hat[1, 1, seq_len(nages), ey], eg$Wt_Beg,  tol),
    # SS3 sets fecundity to 0 below First_Mature_Age; Rceattle has no such cut,
    # so those ages (worth ~1e-9 of SSB for AI cod) are left out of the check.
    .gate_row("fecundity-at-age (mature ages)",
              q$spawn_output[1, seq_len(nages), ey][eg[["Mat*Fecund"]] > 0],
              eg[["Mat*Fecund"]][eg[["Mat*Fecund"]] > 0], tol)
  )

  # Age-length key at Jan 1, SS3 population bins summed into Rceattle bins
  rce_edges <- as.numeric(dl$lengths[1, ])
  rce_edges <- rce_edges[is.finite(rce_edges)]
  ss_alk  <- .ss3_alk_on_bins(ss3_rep, rce_edges, subseas = 1)
  rce_alk <- t(q$growth_matrix[1, 1, seq_len(nages), seq_along(rce_edges), ey])
  rows[[length(rows) + 1]] <- .gate_row("age-length key, Jan 1", rce_alk, ss_alk, tol, "abs")

  # Selectivity-at-age by fleet, end year. Both scaled to a maximum of 1:
  # SS3's Asel2 and Rceattle's sel_at_age share the shape, and F carries the scale.
  sel <- ss3_rep$ageselex
  for (i in seq_len(nrow(dl$fleet_control))) {
    fl <- dl$fleet_control$Fleet_code[i]
    s3 <- sel[sel$Factor == "Asel2" & sel$Fleet == fl & sel$Yr == dl$endyr, as.character(ages)]
    if (!nrow(s3)) next
    s3 <- as.numeric(s3[1, ]); rc <- as.numeric(q$sel_at_age[i, 1, seq_len(nages), ey])
    rows[[length(rows) + 1]] <- .gate_row(
      sprintf("selectivity-at-age, %s", dl$fleet_control$Fleet_name[i]),
      rc / max(rc), s3 / max(s3), tol, "abs")
  }

  # N-at-age, SSB, recruitment over the hindcast
  na <- ss3_rep$natage
  na <- na[na$`Beg/Mid` == "B" & na$Sex == 1 & na$Yr %in% yrs, ]
  na <- na[order(na$Yr), as.character(ages)]
  rows[[length(rows) + 1]] <- .gate_row("N-at-age, Jan 1",
                                        t(q$N_at_age[1, 1, seq_len(nages), seq_along(yrs)]), as.matrix(na), tol)
  ts <- ss3_rep$timeseries[match(yrs, ss3_rep$timeseries$Yr), ]
  rows[[length(rows) + 1]] <- .gate_row("SSB",         q$ssb[1, seq_along(yrs)], ts$SpawnBio,  tol)
  rows[[length(rows) + 1]] <- .gate_row("recruitment", q$R[1, seq_along(yrs)],   ts$Recruit_0, tol)

  do.call(rbind, rows)
}

# Rceattle jnll_comp rows beside the SS3 likelihood component they answer to.
.JNLL_TO_SS3 <- c(
  "Index data"                 = "Survey",
  "Catch data"                 = "Catch",
  "Composition data"           = "Length_comp",
  "CAAL data"                  = "Age_comp",
  "Recruitment deviates"       = "Recruitment",
  "Initial abundance deviates" = "Recruitment"
)

#' G2: likelihood and gradient at SS3's MLE.
parity_g2 <- function(fp, ss3_rep, grad_tol = 1e-3, top = 10) {
  obj  <- fp$obj
  par  <- obj$par
  g    <- as.numeric(obj$gr(par))
  names(g) <- names(par)
  grad <- data.frame(parameter = names(g), gradient = signif(g, 3))
  grad <- grad[order(-abs(grad$gradient)), ][seq_len(min(top, length(g))), ]

  rce <- rowSums(fp$quantities$jnll_comp)
  rce <- rce[abs(rce) > 0]
  ss  <- setNames(ss3_rep$likelihoods_used[, "values"], rownames(ss3_rep$likelihoods_used))
  ss3_name <- unname(.JNLL_TO_SS3[names(rce)])
  comp <- data.frame(rceattle = names(rce), rce_nll = round(rce, 4),
                     ss3 = ss3_name, stringsAsFactors = FALSE)
  comp <- aggregate(rce_nll ~ ss3, data = transform(comp, ss3 = ifelse(is.na(ss3), paste0("[Rce only] ", rceattle), ss3)), sum)
  comp$ss3_nll <- round(ss[comp$ss3], 4)
  comp$diff    <- round(comp$rce_nll - comp$ss3_nll, 4)

  list(max_abs_grad = max(abs(g)), pass = max(abs(g)) <= grad_tol,
       gradient = grad, components = comp,
       total = c(rceattle = sum(fp$quantities$jnll_comp), ss3 = unname(ss["TOTAL"])))
}

#' G3: a cold-start fit reaches the G2 point.
parity_g3 <- function(cold, fp, ss3_rep, obj_tol = 1e-3, tol = 1e-4) {
  yrs <- fp$data_list$styr:fp$data_list$endyr
  ts  <- ss3_rep$timeseries[match(yrs, ss3_rep$timeseries$Yr), ]
  p_fp   <- fp$obj$par
  p_cold <- cold$obj$env$last.par.best[names(fp$obj$env$last.par.best) %in% names(p_fp)]
  obj_fp <- as.numeric(fp$obj$fn(p_fp))
  rbind(
    data.frame(check = "objective, cold minus SS3 MLE", n = 1, scale = "abs",
               max_err = signif(cold$opt$objective - obj_fp, 3), tol = obj_tol,
               pass = abs(cold$opt$objective - obj_fp) <= obj_tol),
    .gate_row("estimated parameters", p_cold, p_fp, tol),
    .gate_row("SSB",         cold$quantities$ssb[1, seq_along(yrs)], ts$SpawnBio,  tol),
    .gate_row("recruitment", cold$quantities$R[1, seq_along(yrs)],   ts$Recruit_0, tol)
  )
}

#' Print G1 and G2 together.
parity_report <- function(fp, ss3_rep, tol = 1e-5, grad_tol = 1e-3) {
  g1 <- parity_g1(fp, ss3_rep, tol)
  g2 <- parity_g2(fp, ss3_rep, grad_tol)
  cat("\n=== G1: forward state at SS3 MLE ===\n"); print(g1, row.names = FALSE)
  cat(sprintf("\n=== G2: max |gradient| at SS3 MLE = %.3g  (%s) ===\n",
              g2$max_abs_grad, if (g2$pass) "PASS" else "FAIL"))
  print(g2$gradient, row.names = FALSE)
  cat("\nNLL components (Rceattle vs SS3):\n"); print(g2$components, row.names = FALSE)
  cat(sprintf("Total: Rceattle %.4f, SS3 %.4f\n", g2$total[1], g2$total[2]))
  invisible(list(g1 = g1, g2 = g2))
}
