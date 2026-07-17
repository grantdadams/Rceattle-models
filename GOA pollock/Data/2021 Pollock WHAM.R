# Refit the 2021 GOA pollock WHAM model ("pkwham") and save it as the bridging reference.
# Source: https://github.com/afsc-assessments/GOApollock/tree/main/alt_models/pkwham
#
# The pkwham repo archives the prepared WHAM input as 'akwham_input_2021.RDS', built by
# match_input() in functions.R from the pk_wham ADMB report + goa_pk_asap3.txt. That RDS was
# written under wham 1.0.6.9000; the current wham (1.0.7.9000) added the length/growth data
# fields, so the archived input no longer builds ("Error when reading the variable:
# 'n_lengths'"). Rather than pin an old wham, we rebuild the skeleton with the installed wham
# and transplant the archived values. This is safe because *everything* match_input() derives
# from the ADMB report is an initial value for an estimated parameter -- the only quantities
# that are held fixed are explicit constants (q_repars, sigmaR, the fixed selectivity blocks),
# which we set below.
#
# Two consequences of the version change, both checked and benign:
#   1. The asap3-derived data is byte-identical between versions; the only fields that differ
#      are the five match_input() overrides re-applied below.
#   2. Current wham drops the Shelikof (index 1) age-1/age-2 composition observations because
#      that selectivity block is fixed at 0 for those ages (obs 1190 -> 1134). Those observed
#      proportions are exactly 0 in the data, so the multinomial contribution is unchanged.

library(wham)
library(dplyr)

# Run from the 'GOA pollock' project root
dir <- "Data/pkwham"

# Build input ----
old     <- readRDS(file.path(dir, "akwham_input_2021.RDS"))
asap3   <- read_asap3_dat(file.path(dir, "goa_pk_asap3.txt"))

# - Fishery + surveys 2, 3, 6 are double-logistic; surveys 1, 4, 5 are age-specific so that
#   ages can be fixed at 0/1. Shelikof (1) and ADF&G (3) have AR1 time-varying catchability.
selmods <- rep("double-logistic", 7); selmods[c(2, 5:6)] <- "age-specific"
selres  <- c("iid", rep("none", 6))
qres    <- rep("none", 6); qres[c(1, 3)] <- "ar1"
NAA_re  <- list(sigma = "rec", cor = "iid", N1_model = 1, N1_pars = c(exp(13.5), 0))

input <- prepare_wham_input(asap3,
                            recruit_model = 2,
                            model_name    = "GOA pollock",
                            selectivity   = list(model = selmods, re = selres),
                            NAA_re        = NAA_re,
                            catchability  = list(re = qres))
input$random <- NULL

# - match_input() data overrides
input$data$mature          <- old$data$mature           # x 0.5, female-only SSB
input$data$waa             <- old$data$waa              # ADMB rounds the WAA matrices
input$data$fracyr_indices  <- old$data$fracyr_indices   # 0.209 0.543 0.60989 0 0 0.519
input$data$bias_correct_oe <- 0
input$data$bias_correct_pe <- 0
input$data$selpars_lower[, 13:16] <- -10
input$data$selpars_upper[, 13:16] <-  20

# - Parameter initial values from the archived input
for (n in names(old$par)) {
  a <- old$par[[n]]; b <- input$par[[n]]
  if (is.null(b)) next
  if (identical(dim(a) %||% length(a), dim(b) %||% length(b))) input$par[[n]] <- a
}
input$par$logit_selpars[, 1:16] <- old$par$logit_selpars  # grew 7x16 -> 7x30 (length sel pars)

# - match_input() map
input$map$log_N1_pars <- factor(c(1, NA))   # F = 0 at the start of the time series
# Fishery selectivity deviates: the first 104 elements of selpars_re are the 52 annual
# deviates on each of the ascending-limb inflection (par 13) and slope (par 14).
input$map$selpars_re  <- factor(c(1:104, rep(NA, length(input$par$selpars_re) - 104)))
tmp <- matrix(NA_integer_, nrow = 7, ncol = ncol(input$par$logit_selpars))
tmp[, 1:16] <- suppressWarnings(as.integer(as.character(matrix(old$map$logit_selpars, nrow = 7))))
input$map$logit_selpars <- factor(tmp)
for (n in c("q_re", "logit_q", "q_prior_re", "M_a", "M_re", "M_repars"))
  input$map[[n]] <- old$map[[n]]

# - Map the process variances off so WHAM matches the ADMB penalized likelihood (run_wham.R).
#   These become FIXED constants and therefore define the model:
#     sigmaR      = exp(log_NAA_sigma) = exp(0)      = 1
#     q1 / q3 SD  = exp(q_repars[,1])  = 0.038 / 0.05 , AR1 rho par = 10
#     sel SD      = exp(sel_repars)    = 0.1
mapoff <- function(name) input$map[[name]] <<- as.factor(input$par[[name]] * NA)
mapoff("sel_repars")     # selectivity deviate variance
mapoff("log_NAA_sigma")  # recruitment deviate variance
mapoff("q_repars")       # catchability deviate variance

# Fit ----
# fit_wham(do.fit = TRUE) crashes on this model: the Shelikof age-specific selectivity
# saturates at 1 for ages 5-7, so the Hessian is singular and the estimability check dies.
# Optimize the TMB object directly instead.
#
# That same saturation makes the surface badly multimodal and ill-conditioned, so plain
# nlminb + restarts is NOT enough: from the archived inits it stalls at "false convergence
# (8)" around 505-629 with a large gradient, and the point it lands on is BLAS/platform
# dependent. A 2026-07-14 Windows run reported objective 474.0967 / convergence 0; on macOS
# that basin has not been reachable. Basin hopping below converges cleanly (max gradient
# ~1e-05) to 489.9975, whose likelihood components are all close to the 474.0967 run's.
#
# NOTE: 489.9975 != 474.0967. If you need the reference fit to be the global optimum, this
# is still open -- see HANDOFF "1. The reference fit does not reproduce".
set.seed(42)
obj <- fit_wham(input, do.osa = FALSE, do.fit = FALSE, do.retro = FALSE,
                do.sdrep = FALSE, MakeADFun.silent = TRUE)

polish <- function(p) {
  for (i in 1:8) {
    o <- try(suppressWarnings(nlminb(p, obj$fn, obj$gr,
             control = list(eval.max = 1e5, iter.max = 1e5, rel.tol = 1e-13))), silent = TRUE)
    if (inherits(o, "try-error") || !is.finite(o$objective)) return(list(objective = Inf, par = p))
    p <- o$par
  }
  o2 <- try(suppressWarnings(optim(p, obj$fn, obj$gr, method = "BFGS",
            control = list(maxit = 20000, reltol = 1e-15))), silent = TRUE)
  if (!inherits(o2, "try-error") && is.finite(o2$value) && o2$value < o$objective)
    return(list(objective = o2$value, par = o2$par))
  list(objective = o$objective, par = o$par)
}

best <- polish(obj$par)
for (sd in c(0.30, 0.20, 0.10, 0.05, 0.02)) {   # basin hopping, decreasing jitter
  for (k in 1:10) {
    r <- try(polish(best$par + rnorm(length(best$par), 0, sd)), silent = TRUE)
    if (inherits(r, "try-error") || !is.finite(r$objective)) next
    if (r$objective < best$objective - 1e-6) best <- r
  }
  cat("jitter sd", sd, "-> best", best$objective, "\n")
}

opt <- list(objective = best$objective, par = best$par,
            convergence = NA)   # nlminb's code is meaningless after basin hopping
fit <- list(opt     = opt,
            rep     = obj$report(best$par),
            parList = obj$env$parList(best$par),
            input   = input)

# - Expect: objective ~489.9975, max gradient ~1e-05 (see note above re 474.0967)
cat("objective:", opt$objective, " max gradient:", max(abs(obj$gr(best$par))), "\n")

save(fit, file = "Data/2021pollock_wham.Rdata")

# Checks ----
# - Fishery selectivity is time-varying on the ascending limb only: ages 3-5 vary by ~0.9
#   across years while age 10 is pinned at 0.3804 (the descending limb has no deviates).
apply(fit$rep$selAA[[1]], 2, function(x) diff(range(x)))

# - Survey selectivity (time-invariant): 1 = Shelikof (ages 1-2 fixed at 0, 5-7 saturate at 1),
#   2 = NMFS bottom trawl, 3 = ADF&G, 4 = age-1 index, 5 = age-2 index, 6 = summer acoustic (all 1)
t(sapply(fit$rep$selAA[2:7], function(x) round(x[1, ], 4)))

# - Catchability: q1 (Shelikof) and q3 (ADF&G) are the AR1 time-varying ones
round(fit$rep$q[c(1, 26, 52), ], 4)

# - Likelihood components
sapply(fit$rep[grep("^nll", names(fit$rep))], sum) |> (\(x) x[x != 0])()
