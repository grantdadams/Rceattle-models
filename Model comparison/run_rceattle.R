# =============================================================================
# run_rceattle() -- RCEATTLE estimation model for the ASSAMC OM-EM comparison.
#
# analogue of run_fims() / run_wham() from the ASSAMC package.
# Written so it can be copied verbatim into ASSAMC:
#   * run_rceattle(maindir, subdir, om_sim_num, casedir, em_bias_cor)
#   * loop over 1..om_sim_num replicates in a parallel foreach
#   * each worker load()s casedir/output/OM/OM{i}.RData -> om_input, om_output,
#     em_input
#   * fit three recruitment scenarios and saveRDS() a fixed set of result files
#     into casedir/output/<subdir>/s{i}/
#   * no return value
#
# The three scenarios mirror FIMS/WHAM. All use RCEATTLE mean recruitment
# (build_srr(srr_fun = 0): recruitment = R0 * exp(rec_dev), effective steepness
# ~ 0.99). The EM scenarios differ only in how the recruitment deviations are treated:
#
#   random_effects                  rec_dev random effects, sigmaR estimated
#   random_effects_sigmaR_constant  rec_dev random effects, sigmaR fixed
#   fixed_effects                   rec_dev penalised fixed effects, sigmaR fixed
#
# Requires: Rceattle, foreach, doParallel, parallel.
# =============================================================================

utils::globalVariables(c("om_input", "om_output", "em_input", "om_sim"))

# Scenario identifiers, reused for the file names.
RCEATTLE_SCENARIOS <- c(
  "random_effects",
  "random_effects_sigmaR_constant",
  "fixed_effects"
)


#' Seed RCEATTLE starting parameters at OM truth
#'
#' Initialise the recruitment and rec devs parameters before fitting.
#'
#' @param data_list An Rceattle data_list from om_to_rceattle().
#' @param om_input The OM truth list (for R0 and logR.resid).
#' @return A parameter list suitable for `fit_mod(inits = )`.
seed_rceattle_inits <- function(data_list, om_input) {
  p <- suppressWarnings(Rceattle::build_params(data_list))
  p$rec_pars[1, "R0"] <- log(om_input[["R0"]])            # log-scale R0
  resid <- as.numeric(om_input[["logR.resid"]])
  p$rec_dev[1, seq_along(resid)] <- resid
  p
}


#' Fit one RCEATTLE recruitment scenario
#'
#' @param data_list Rceattle data_list.
#' @param inits Initialized parameter list from seed_rceattle_inits().
#' @param scenario One of RCEATTLE_SCENARIOS.
#' @param initMode Initial-age-structure mode (default 1 = equilibrium).
#' @param time_limit Wall-clock seconds after which a fit is abandoned (returns
#'   NULL, recorded as non-converged). Guards against the pathological
#'   random-effects fit on a deterministic OM case (true sigmaR = 0), where the
#'   Laplace inner problem is ill-conditioned and the optimiser can stall.
#' @return A fitted object of class "Rceattle" (with `$run_time` populated by a
#'   wrapping system.time()), or NULL on error / time-out.
fit_rceattle_scenario <- function(data_list, inits, scenario, initMode = 1,
                                  time_limit = 300) {
  random_rec <- scenario %in%
    c("random_effects", "random_effects_sigmaR_constant")

  # Optimiser controls. The fit_control() defaults (loopnum = 5,
  # getJointPrecision = TRUE)
  ctl <- Rceattle::fit_control(
    getsd = TRUE, getJointPrecision = FALSE,
    loopnum = 1, newtonsteps = 1, phase = FALSE, verbose = 0)

  # Arguments shared between the (optional) map-building build and the fit.
  common <- list(
    data_list  = data_list,
    inits      = inits,
    msmMode    = 0,                                  # single species
    random_rec = random_rec,
    recFun     = Rceattle::build_srr(srr_fun = 0),   # mean recruitment
    initMode   = initMode
  )

  # Parameter map. fit_mod() builds the map internally when `map = NULL`, which
  # is what the two default scenarios want (random_effects: R_log_sd estimated;
  # fixed_effects: random_rec = FALSE already maps R_log_sd off and treats
  # rec_dev as penalised fixed effects). For the sigmaR-constant scenario we
  # keep rec_dev as random effects but hold R_log_sd fixed at its initial value
  # log(sigma_rec_prior).
  map <- NULL
  if (scenario == "random_effects_sigmaR_constant") {
    build <- do.call(Rceattle::fit_mod, c(common, list(
      estimateMode = 3,
      fit_control  = Rceattle::fit_control(getsd = FALSE, verbose = 0))))
    map <- build$map
    map$mapFactor$R_log_sd <- factor(rep(NA, length(map$mapFactor$R_log_sd)))
    if (!is.null(map$mapList)) map$mapList$R_log_sd[] <- NA
  }

  fit <- NULL
  elapsed <- system.time({
    fit <- tryCatch({
      setTimeLimit(elapsed = time_limit, transient = TRUE)
      on.exit(setTimeLimit(), add = TRUE)          # clear the limit afterwards
      do.call(Rceattle::fit_mod, c(common, list(
        map          = map,
        estimateMode = 1,                        # hindcast (no HCR projection)
        fit_control  = ctl)))
    },
      error = function(e) {
        message(sprintf("  [%s] fit_mod error: %s", scenario, conditionMessage(e)))
        NULL
      }
    )
  })

  if (!is.null(fit)) attr(fit, "elapsed_total") <- elapsed[["elapsed"]]
  fit
}


#' Extract a tidy estimates table from a fitted RCEATTLE model
#'
#' Comparable in spirit to FIMS::get_estimates(): one row per (quantity, year)
#' with a point estimate and standard error. Standard errors come from the
#' sdreport for the ADREPORTed quantities (ssb, biomass, R); F_spp is not
#' ADREPORTed in the production TMB template, so its uncertainty is NA.
#'
#' @param fit A fitted "Rceattle" object (single species).
#' @param nyrs Number of hindcast years to report (defaults to all model years).
#' @return A data.frame with columns
#'   `label, year, age, estimate, uncertainty` (uncertainty = SE).
rceattle_estimates <- function(fit, nyrs = NULL) {
  q <- fit$quantities
  model_yrs <- fit$data_list$styr:fit$data_list$endyr
  if (is.null(nyrs)) nyrs <- length(model_yrs)
  yr_idx <- seq_len(nyrs)
  years  <- model_yrs[yr_idx]

  # sdreport SEs (named vectors in fit$sdrep$value / $sd)
  se_of <- function(name) {
    if (is.null(fit$sdrep)) return(rep(NA_real_, nyrs))
    hits <- which(names(fit$sdrep$value) == name)
    if (!length(hits)) return(rep(NA_real_, nyrs))
    se <- fit$sdrep$sd[hits]
    length(se) <- length(model_yrs)      # guard against proj-year padding
    se[yr_idx]
  }

  mk <- function(label, est, se) {
    data.frame(label = label, year = years, age = NA_integer_,
               estimate = as.numeric(est)[yr_idx],
               uncertainty = as.numeric(se),
               stringsAsFactors = FALSE)
  }

  rbind(
    mk("SSB",         q$ssb[1, yr_idx],     se_of("ssb")),
    mk("biomass",     q$biomass[1, yr_idx], se_of("biomass")),
    mk("recruitment", q$R[1, yr_idx],       se_of("R")),
    mk("F",           q$F_spp[1, yr_idx],   rep(NA_real_, nyrs))
  )
}


#' Count NA standard errors across a sdreport (fixed + random + report)
#' Mirrors the run_fims.R helper of the same name.
count_na_standard_errors <- function(sdreport) {
  if (is.null(sdreport)) return(NA_integer_)
  total_na <- 0L
  for (which_par in c("fixed", "random", "report")) {
    # A fixed-effects model has an empty "random" summary, which warns; that is
    # expected, so silence it.
    s <- tryCatch(suppressWarnings(summary(sdreport, which_par)),
                  error = function(e) NULL)
    if (!is.null(s) && nrow(s) > 0) {
      total_na <- total_na + sum(is.na(s[, "Std. Error"]))
    }
  }
  total_na
}


#' Condition number of the Hessian
#'
#' Same quantity as the run_fims.R helper (kappa of the Hessian), but read from
#' the value RCEATTLE already computes inside fit_mod() during its own
#' convergence check (fit$convergence$checks$hessian_conditioning).
#'
#' Rceattle 5.26.0 reads its own conditioning check on the CORRELATION matrix, so
#' `data$condition_number` is no longer kappa of the Hessian and is not
#' comparable with FIMS. `data$covariance_condition_number` is the quantity this
#' helper wants; the bare name is the pre-5.26.0 fallback.
#'
#' @param fit A fitted "Rceattle" object.
get_condition_number <- function(fit) {
  d <- tryCatch(fit$convergence$checks$hessian_conditioning$data,
                error = function(e) NULL)
  cn <- d$covariance_condition_number
  if (is.null(cn) && !is.null(d) && is.null(d$se_ratio)) {
    cn <- d$condition_number          # pre-5.26.0: this WAS the covariance kappa
  }
  if (!is.null(cn) && is.finite(cn)) return(as.numeric(cn))
  # Safe fallback: dense Hessian, fixed-effects models only (never spHess).
  if (length(fit$obj[["env"]][["random"]]) == 0L) {
    return(tryCatch(kappa(as.matrix(fit$obj$he(fit$opt$par))),
                    error = function(e) NA_real_))
  }
  NA_real_
}


#' Save the standard result files for one fitted scenario
#'
#' Writes the same set of RDS files run_fims() / run_wham() write, tagged
#' `rceattle` and suffixed by scenario, into `outdir`.
save_rceattle_outputs <- function(fit, scenario, outdir, nyrs = NULL) {
  tag <- function(stub) file.path(outdir, sprintf("%s_%s.RDS", stub, scenario))

  # A NULL fit still records a non-convergence so downstream summaries see it.
  if (is.null(fit)) {
    saveRDS(NULL,        tag("fit_rceattle"))
    saveRDS(1L,          tag("optimizer_convergence_rceattle"))  # 1 = not converged
    saveRDS(NA_real_,    tag("max_gradient_rceattle"))
    return(invisible(NULL))
  }

  # Estimates table (primary downstream product)
  saveRDS(rceattle_estimates(fit, nyrs), tag("fit_rceattle"))
  # Full fit object as a fallback for anything the table omits
  saveRDS(fit, tag("full_fit_rceattle"))

  # Run time: fill the same 4-name vector FIMS uses. RCEATTLE reports a single
  # model time on $run_time (a difftime, whose auto-selected units we force to
  # seconds so slow fits are not silently reported in minutes) and does not
  # separate optimisation from sdreport, so those two fields are NA. `total` is
  # our wall-clock elapsed (optimisation + sdreport + overhead), in seconds.
  rt_secs <- if (!is.null(fit$run_time)) as.numeric(fit$run_time, units = "secs") else NA_real_
  total   <- attr(fit, "elapsed_total")
  total   <- if (!is.null(total)) as.numeric(total) else NA_real_
  run_time <- c(
    fit_optimization = NA_real_,
    fit_sdreport     = NA_real_,
    fit_total        = rt_secs,
    total            = total
  )
  saveRDS(run_time, tag("run_time_rceattle"))

  # Max absolute (marginal) gradient. RCEATTLE stores the final value on
  # fit$opt$max_gradient (via TMBhelper::fit_tmb); fall back to obj$gr() only if
  # that field is absent. RCEATTLE's opt carries NO nlminb `convergence` field.
  maxgrad <- tryCatch(fit$opt$max_gradient, error = function(e) NULL)
  if (is.null(maxgrad)) {
    maxgrad <- tryCatch(max(abs(fit$obj$gr())), error = function(e) NA_real_)
  }
  maxgrad <- as.numeric(maxgrad)
  saveRDS(maxgrad, tag("max_gradient_rceattle"))

  # Convergence code (0 = converged). Derived to match ASSAMC's
  # check_convergence.R, which treats a maximum gradient < 0.1 together with a
  # positive-definite Hessian as converged.
  pdhess <- !is.null(fit$sdrep) && isTRUE(fit$sdrep$pdHess)
  conv <- if (!is.na(maxgrad) && maxgrad < 0.1 && pdhess) 0L else 1L
  saveRDS(conv, tag("optimizer_convergence_rceattle"))

  # Hessian-based diagnostics (only when a sdreport is available)
  if (!is.null(fit$sdrep)) {
    saveRDS(isTRUE(fit$sdrep$pdHess), tag("hessian_rceattle"))
    saveRDS(count_na_standard_errors(fit$sdrep),
            file.path(outdir, sprintf("na_count_%s.RDS", scenario)))
    saveRDS(get_condition_number(fit),
            file.path(outdir, sprintf("condition_number_%s.RDS", scenario)))
  }
  invisible(NULL)
}


#' Run the RCEATTLE estimation model over the OM replicates
#'
#' @param maindir Path to the main working directory (unused directly; kept for
#'   signature compatibility with the other run_*() functions).
#' @param subdir Output subfolder under `casedir/output`. Default "RCEATTLE".
#' @param om_sim_num Number of OM replicates (OM1.RData .. OM{n}.RData).
#' @param casedir Case directory containing `output/OM/OM{i}.RData`.
#' @param em_bias_cor Bias-correction flag (accepted for signature parity;
#'   RCEATTLE handles rec-dev bias correction via its own fit_control).
#' @return Invisibly NULL. Results are written under
#'   `casedir/output/<subdir>/s{i}/`.
run_rceattle <- function(maindir = maindir,
                         subdir = "RCEATTLE",
                         om_sim_num = NULL,
                         casedir = casedir,
                         em_bias_cor = em_bias_cor) {

  if (!("Rceattle" %in% rownames(installed.packages()))) stop("Please install Rceattle!")

  # Clean and (re)create per-replicate output folders.
  unlink(list.files(file.path(casedir, "output", subdir), full.names = TRUE),
         recursive = TRUE)
  for (x in seq_len(om_sim_num)) {
    dir.create(file.path(casedir, "output", subdir, paste0("s", x)),
               recursive = TRUE, showWarnings = FALSE)
  }

  closeAllConnections()
  cores <- if (parallel::detectCores() == 1) 1 else parallel::detectCores() - 2
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, {
    suppressMessages(library(Rceattle))
    TMB::openmp(n = 1)
  })

  `%dopar%` <- foreach::`%dopar%`
  foreach::foreach(
    om_sim = seq_len(om_sim_num),
    .packages = c("Rceattle"),
    # casedir / subdir are auto-exported (referenced in the loop body); list
    # only the helper functions foreach cannot infer.
    .export   = c("RCEATTLE_SCENARIOS",
                  "om_to_rceattle", "cv_2_sd", "seed_rceattle_inits",
                  "fit_rceattle_scenario", "rceattle_estimates",
                  "count_na_standard_errors", "get_condition_number",
                  "save_rceattle_outputs")
  ) %dopar% {
    load(file = file.path(casedir, "output", "OM", paste0("OM", om_sim, ".RData")))
    outdir <- file.path(casedir, "output", subdir, paste0("s", om_sim))

    data_list <- om_to_rceattle(om_input, om_output, em_input)
    inits     <- seed_rceattle_inits(data_list, om_input)
    nyrs      <- length(om_input[["year"]])

    for (scenario in RCEATTLE_SCENARIOS) {
      fit <- fit_rceattle_scenario(data_list, inits, scenario)
      save_rceattle_outputs(fit, scenario, outdir, nyrs = nyrs)
    }
    NULL
  }
  invisible(NULL)
}
