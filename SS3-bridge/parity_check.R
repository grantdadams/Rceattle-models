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

  # The state above can match while the composition PREDICTIONS do not: the
  # predicted CAAL is a further function of the age-length key, so it needs its
  # own check. For AI cod every row above passed while the predicted CAAL was
  # out by 0.118 in probability, which is where the growth gradient lives.
  rows <- c(rows, .g1_caal(fp, ss3_rep, tol), .g1_lencomp(fp, ss3_rep, tol),
            .g1_fitted_series(fp, ss3_rep, tol))

  do.call(rbind, rows)
}

#' Predicted survey index and predicted catch against SS3's own expectations.
#'
#' These are the two fitted time series, and nothing else in G1 covers them:
#' the state checks stop at N, SSB and weight-at-age. They are the quantities
#' the index and catch likelihoods actually see, so a difference here explains
#' a gradient that the state checks cannot.
.g1_fitted_series <- function(fp, ss3_rep, tol) {
  dl <- fp$data_list
  q  <- fp$quantities
  out <- list()

  idx <- dl$index_data
  cp  <- ss3_rep$cpue
  if (!is.null(idx) && !is.null(cp) && nrow(cp)) {
    keep <- which(idx$Year > 0)
    m <- match(paste(idx$Fleet_code[keep], idx$Year[keep]), paste(cp$Fleet, cp$Yr))
    ok <- !is.na(m)
    if (any(ok))
      out[[length(out) + 1]] <- .gate_row("predicted survey index",
        as.numeric(q$index_hat)[keep][ok], cp$Exp[m[ok]], tol)
  }

  cat_d <- dl$catch_data
  ct    <- ss3_rep$catch
  if (!is.null(cat_d) && !is.null(ct) && nrow(ct)) {
    keep <- which(cat_d$Year >= dl$styr & cat_d$Year <= dl$endyr)
    m <- match(paste(cat_d$Fleet_code[keep], cat_d$Year[keep]), paste(ct$Fleet, ct$Yr))
    ok <- !is.na(m)
    if (any(ok))
      out[[length(out) + 1]] <- .gate_row("predicted catch",
        as.numeric(q$catch_hat)[keep][ok], ct$Exp[m[ok]], tol)
  }
  out
}

# SS3 reports composition expectations AFTER adding `addtocomp` to every bin and
# renormalising. Undo that so the raw prediction is what gets compared.
.strip_addtocomp <- function(p, offset, nbins) {
  if (!is.finite(offset) || offset <= 0) return(p)
  pmax(p * (1 + nbins * offset) - offset, 0)
}

#' Predicted conditional age-at-length against SS3's `condbase` expectations.
#' Compared on cells SS3 expects above 1e-3, where the add-to-comp floor and
#' Report.sso's print precision are both negligible.
.g1_caal <- function(fp, ss3_rep, tol) {
  dl <- fp$data_list
  cd <- dl$caal_data
  cb <- ss3_rep$condbase
  if (is.null(cb) || !nrow(cb) || is.null(cd) || !nrow(cd)) return(list())
  nages  <- dl$nages[1]
  minage <- dl$minage[1]
  offset <- if (is.null(dl$comp_offset)) 0 else dl$comp_offset[1]
  real   <- which(cd$Year > 0)
  cb$key <- paste(cb$Yr, cb$Lbin_lo)
  sp     <- split(seq_len(nrow(cb)), cb$key)
  a <- b <- numeric(0)
  for (i in real) {
    idx <- sp[[paste(cd$Year[i], cd$Length[i])]]
    if (is.null(idx)) next
    s <- cb[idx, ]
    s <- s[!duplicated(s$Bin), ]
    s <- s[order(s$Bin), ]
    e <- .strip_addtocomp(s$Exp, offset, nrow(s))
    r <- as.numeric(fp$quantities$caal_hat[i, s$Bin - minage + 1])
    keep <- which(e > 1e-3)
    a <- c(a, r[keep]); b <- c(b, e[keep])
  }
  if (!length(b)) return(list())
  list(.gate_row("predicted CAAL", a, b, tol, "abs"))
}

#' Predicted length composition against SS3's `lendbase` expectations.
.g1_lencomp <- function(fp, ss3_rep, tol) {
  dl <- fp$data_list
  cm <- dl$comp_data
  lb <- ss3_rep$lendbase
  if (is.null(lb) || !nrow(lb) || is.null(cm) || !nrow(cm)) return(list())
  offset <- if (is.null(dl$comp_offset)) 0 else dl$comp_offset[1]
  lens   <- as.numeric(dl$lengths[1, ])
  lens   <- lens[is.finite(lens)]
  lb$key <- paste(lb$Fleet, lb$Yr)
  sp     <- split(seq_len(nrow(lb)), lb$key)
  out <- list()
  for (fi in seq_len(nrow(dl$fleet_control))) {
    fl <- dl$fleet_control$Fleet_code[fi]
    a <- b <- numeric(0)
    for (i in which(cm$Year > 0 & cm$Fleet_code == fl)) {
      idx <- sp[[paste(fl, cm$Year[i])]]
      if (is.null(idx)) next
      s <- lb[idx, ]
      s <- s[!duplicated(s$Bin), ]
      s <- s[order(s$Bin), ]
      col <- match(s$Bin, lens)
      ok  <- !is.na(col)
      e <- .strip_addtocomp(s$Exp[ok], offset, nrow(s))
      r <- as.numeric(fp$quantities$comp_hat[i, col[ok]])
      keep <- which(e > 1e-3)
      a <- c(a, r[keep]); b <- c(b, e[keep])
    }
    if (!length(b)) next
    out[[length(out) + 1]] <- .gate_row(
      sprintf("predicted length comp, %s", dl$fleet_control$Fleet_name[fi]),
      a, b, tol, "abs")
  }
  out
}

# Rceattle jnll_comp rows beside the SS3 likelihood component they answer to.
# The initial-abundance deviates DO have an SS3 counterpart: `Early_InitAge_*`,
# which SS3 penalises inside `Recruitment` along with the main deviates. For AI
# cod there are 13 of each, one for one.
.JNLL_TO_SS3 <- c(
  "Index data"                 = "Survey",
  "Catch data"                 = "Catch",
  "Composition data"           = "Length_comp",
  "CAAL data"                  = "Age_comp",
  "Recruitment deviates"       = "Recruitment",
  "Initial abundance deviates" = "Recruitment"
)

# Likelihood constants SS3 drops and Rceattle keeps, by SS3 component name.
# Rceattle evaluates full densities; SS3 writes the kernel only, so at the same
# parameters the two differ by a known number of nats and nothing else. What is
# left after subtracting these is a real difference in fit.
#   Catch   SS3 keeps 0.5 z^2 alone            -> log(sigma) + 0.5 log(2 pi) per row
#   Survey  SS3 keeps log(sigma) + 0.5 z^2     -> 0.5 log(2 pi) per row
#   Recruit SS3 keeps log(sigmaR) + the kernel -> 0.5 log(2 pi) per deviate,
#           counting the initial-abundance deviates, whose SS3 counterparts
#           (`Early_InitAge_*`) are penalised in the same component.
.ss3_constants <- function(fp) {
  dl   <- fp$data_list
  l2pi <- 0.5 * log(2 * pi)
  k <- c(Catch = NA_real_, Survey = NA_real_, Recruitment = NA_real_)
  # Only the hindcast is fitted; catch_data also carries the projection years.
  hind <- function(d) d[d$Year >= dl$styr & d$Year <= dl$endyr, ]
  cat_d <- dl$catch_data
  if (!is.null(cat_d)) {
    cat_d <- hind(cat_d)
    k["Catch"] <- sum(log(cat_d$Log_sd) + l2pi)
  }
  idx <- dl$index_data
  if (!is.null(idx)) k["Survey"] <- nrow(hind(idx)) * l2pi
  # Count the deviates the OBJECTIVE penalises, not the ones the map leaves
  # free: `ceattle.cpp` loops rec_dev over every hindcast year and init_dev over
  # ages 1..nages-1 regardless of the map, so a deviate fixed at 0 still costs a
  # full density. Counting free parameters here understated the constant by
  # 3 devs on AI cod and inflated the residual from +0.73 to +3.49.
  n_rec <- length(dl$styr:dl$endyr) + sum(dl$nages - 1L)
  if (n_rec > 0) k["Recruitment"] <- n_rec * l2pi
  k
}

#' G2: likelihood and gradient at SS3's MLE.
#'
#' `fixed_in_ss3` names the Rceattle parameter blocks whose SS3 counterparts are
#' fixed (a negative phase). SS3 never moved those, so its solution says nothing
#' about their gradient and they cannot be part of the test. They are still
#' printed, marked `fixed`.
parity_g2 <- function(fp, ss3_rep, grad_tol = 1e-3, top = 10,
                      fixed_in_ss3 = character()) {
  obj  <- fp$obj
  par  <- obj$par
  g    <- as.numeric(obj$gr(par))
  names(g) <- names(par)
  est  <- !(names(g) %in% fixed_in_ss3)
  grad <- data.frame(parameter = names(g), gradient = signif(g, 3),
                     ss3 = ifelse(est, "estimated", "fixed"))
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
  k <- .ss3_constants(fp)
  comp$constant <- round(unname(k[comp$ss3]), 4)
  comp$residual <- round(comp$diff - ifelse(is.na(comp$constant), 0, comp$constant), 4)

  gmax <- if (any(est)) max(abs(g[est])) else NA_real_
  list(max_abs_grad = gmax, pass = isTRUE(gmax <= grad_tol),
       max_abs_grad_all = max(abs(g)), n_fixed = sum(!est),
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
parity_report <- function(fp, ss3_rep, tol = 1e-5, grad_tol = 1e-3,
                          fixed_in_ss3 = character()) {
  g1 <- parity_g1(fp, ss3_rep, tol)
  g2 <- parity_g2(fp, ss3_rep, grad_tol, fixed_in_ss3 = fixed_in_ss3)
  cat("\n=== G1: forward state at SS3 MLE ===\n"); print(g1, row.names = FALSE)
  cat(sprintf("\n=== G2: max |gradient| at SS3 MLE = %.3g  (%s) ===\n",
              g2$max_abs_grad, if (g2$pass) "PASS" else "FAIL"))
  if (g2$n_fixed > 0)
    cat(sprintf("     over the %d parameters SS3 estimated; %d that SS3 fixes are excluded (max |gradient| there %.3g)\n",
                length(fp$obj$par) - g2$n_fixed, g2$n_fixed, g2$max_abs_grad_all))
  print(g2$gradient, row.names = FALSE)
  cat("\nNLL components (Rceattle vs SS3). `residual` is the gap after the\n")
  cat("densities' constants; only that column is a difference in fit.\n")
  print(g2$components, row.names = FALSE)
  cat(sprintf("Total: Rceattle %.4f, SS3 %.4f\n", g2$total[1], g2$total[2]))
  invisible(list(g1 = g1, g2 = g2))
}
