# =============================================================================
# om_to_rceattle() -- translate an ASSAMC operating-model replicate into an
# Rceattle single-species data_list.
#
# This is the RCEATTLE analogue of FIMS' prepare_data_fims() (see run_fims.R in
# the Age_Structured_Stock_Assessment_Model_Comparison package). The OM hands
# each estimation model three lists via load("OM{i}.RData"):
#
#   om_input   true inputs / spec  (year, ages, M.age, W.mt, mat.age, sel_*, ...)
#   om_output  true dynamics       (SSB, N.age, f, survey_q, ...)  -- truth, for
#                                   self-tests; NOT fed to the EM
#   em_input   observed-with-error (L.obs, survey.obs, L.age.obs, survey.age.obs,
#                                   n.L, n.survey, cv.L, cv.survey)  -- the data
#                                   the EM actually fits
#
# We build the data_list the same way the package's own test fixture
# make_test_data() does: assemble the component data.frames with the pre-clean
# column names, then hand off to the exported Rceattle::clean_data() +
# Rceattle::switch_check() to normalise column names, fill fleet_control
# defaults, and produce a fit_mod()-ready object. This deliberately uses ONLY
# the released (main-branch) public API -- no build_data()/data_requirements(),
# which exist only on the dev-data-workflow branch.
#
# The OM configuration mirrored here is the canonical one-population case:
# single species, single sex, one fishery + one survey, age composition only
# (no length/CAAL), age-invariant & time-invariant weight-at-age, logistic
# selectivity, Beverton-Holt recruitment.
# =============================================================================


#' Convert a CV to a lognormal standard deviation
#'
#' Identical to the helper of the same name in run_fims.R, so index / catch
#' observation error is specified the same way across estimation models.
#' @param x numeric vector of coefficients of variation
#' @return numeric vector of lognormal SDs, sqrt(log(x^2 + 1))
cv_2_sd <- function(x) {
  sqrt(log(x^2 + 1))
}


#' Translate one OM replicate into an Rceattle data_list
#'
#' @param om_input,om_output,em_input The three lists injected by
#'   load("OM{i}.RData"). `om_output` is accepted for interface symmetry with
#'   prepare_data_fims() and for optional self-tests; the EM data come from
#'   `om_input` (dimensions / biology) and `em_input` (observations).
#' @param proj_years Number of projection years appended after the terminal
#'   year. Rceattle requires `projyr > endyr`; a short horizon is enough for a
#'   hindcast fit (estimateMode = 1). Increase when reference points from an HCR
#'   projection (estimateMode = 0) are wanted.
#' @param sigmaR_floor Lower bound applied to the OM `logR_sd` when seeding
#'   `sigma_rec`. The deterministic OM case (C0) has `logR_sd == 0`, which
#'   is a degenerate recruitment SD; the floor keeps the parameter well-defined.
#'   The value used is recorded on the returned object as
#'   `attr(., "sigmaR_true")` / `attr(., "sigmaR_floored")` so callers can see
#'   whether the floor bit.
#'
#' @return An Rceattle `data_list` (a plain list) ready for `fit_mod()`.
om_to_rceattle <- function(om_input, om_output = NULL, em_input,
                           proj_years = 1L, sigmaR_floor = 0.05) {

  # ---- Dimensions -----------------------------------------------------------
  years  <- om_input[["year"]]
  nyrs   <- length(years)
  nages  <- om_input[["nages"]]
  ages   <- om_input[["ages"]]
  minage <- min(ages)

  age_cols  <- paste0("Age",  seq_len(nages))
  comp_cols <- paste0("Comp_", seq_len(nages))

  # ---- Observation error (CV -> lognormal SD) -------------------------------
  # cv.L / cv.survey may be scalars or length-nyrs vectors.
  catch_log_sd <- cv_2_sd(rep_len(unlist(em_input[["cv.L"]])[1],      nyrs))
  index_log_sd <- cv_2_sd(rep_len(unlist(em_input[["cv.survey"]])[1], nyrs))

  # ---- Survey observation field --------------------------------------------
  # OM versions differ in which field carries the survey observation: a
  # biomass index (`surveyB.obs`) or a numbers-based index (`survey.obs`).
  # Prefer a non-empty biomass field; otherwise fall back to the numbers index.
  # `Observation_units` on the survey fleet is set to match (1 = biomass,
  # 2 = numbers). This choice is a coordination item with the FIMS team -- see
  # README.md.
  surveyB <- em_input[["surveyB.obs"]]
  if (!is.null(surveyB) && length(surveyB) >= 1L && length(surveyB[[1]]) == nyrs) {
    survey_obs        <- as.numeric(surveyB[[1]])
    survey_units_code <- 1L   # biomass
  } else {
    survey_obs        <- as.numeric(em_input[["survey.obs"]][[1]])
    survey_units_code <- 2L   # numbers
  }

  # ---- Composition sample sizes --------------------------------------------
  # n.L / n.survey are per-year annual sample sizes (constant in these OMs).
  fishery_n <- rep_len(unlist(em_input[["n.L"]])[1],      nyrs)
  survey_n  <- rep_len(unlist(em_input[["n.survey"]])[1], nyrs)

  # ---- Recruitment SD --------------------------------------------------------
  sigmaR_true    <- as.numeric(om_input[["logR_sd"]])
  sigmaR_floored <- max(sigmaR_true, sigmaR_floor)

  simData <- list()

  # ---- Scalars / dimension vectors (nspp == 1) ------------------------------
  simData$nspp                     <- 1L
  simData$styr                     <- min(years)
  simData$endyr                    <- max(years)
  simData$projyr                   <- max(years) + as.integer(proj_years)
  simData$spnames                  <- "OM_species"
  simData$nsex                     <- 1L
  simData$spawn_month              <- 0            # SSB at the start of the year
  simData$nages                    <- nages
  simData$minage                   <- minage
  simData$nlengths                 <- nages        # age-based: no length bins
  simData$pop_wt_index             <- 1L
  simData$ssb_wt_index             <- 1L
  simData$alpha_wt_len             <- 0.0001
  simData$beta_wt_len              <- 3
  simData$pop_age_transition_index <- 1L
  simData$sigma_rec          <- sigmaR_floored
  simData$other_food               <- 1e6
  simData$estDynamics              <- 0

  # ---- Fleet control: fleet 1 = fishery, fleet 2 = survey -------------------
  # Columns and integer switch codes match the released bundled-data schema
  # (e.g. data(GOApollock)$fleet_control), i.e. the post-read_data form the
  # model consumes internally. clean_data() + switch_check() below fill the
  # remaining defaults. Fleet_code MUST equal the row number (data_check()).
  #
  # Switch codes used here (see Rceattle::switch_check / R/0-switches.R):
  #   Fleet_type   Fishery = 1, Survey = 2
  #   Selectivity  Logistic = 1
  #   Comp_distribution Multinomial = 0
  #   Catchability Analytical = 3  (closed-form MLE q; robust, no init needed)
  #
  # The survey uses Analytical catchability rather than a freely estimated q so
  # the self-test is not sensitive to a q starting value; this is a modelling
  # choice to revisit with the FIMS team (FIMS estimates log_q as a parameter).
  simData$fleet_control <- data.frame(
    Fleet_name                = c("Fishery", "Survey"),
    Fleet_code                = 1:2,
    Fleet_type                = c(1L, 2L),
    Species                   = 1,
    Month                     = 0,
    Selectivity_index         = 1:2,
    Selectivity               = 1L,                 # Logistic
    N_sel_bins                = NA,
    Sel_curve_pen1            = NA,
    Sel_curve_pen2            = NA,
    Time_varying_sel          = 0,
    Time_varying_sel_sd = 1,
    Bin_first_selected        = 1,
    Sel_norm_bin             = NA,
    Sel_norm_bin_upper             = NA,
    Comp_distribution              = 0L,                 # Multinomial
    Comp_weights              = 1,
    CAAL_distribution              = 0,
    Observation_units          = c(1L, survey_units_code),  # fishery biomass; survey per units
    Weight_index              = 1,
    Age_transition_index      = 1,
    Catchability_index                   = c(NA, 1),
    Catchability              = c(NA, 3L),          # survey: Analytical q
    Catchability_init                   = c(NA, 1),
    Catchability_prior_sd                = c(NA, 0.2),
    Time_varying_q            = c(NA, 0),
    Time_varying_q_sd   = c(NA, 1),
    Estimate_index_sd         = c(NA, 0),
    Index_sd            = c(NA, 1),
    Estimate_catch_sd         = c(0, NA),
    Catch_sd            = c(1, NA),
    Proj_F_proportion               = c(1, NA),
    stringsAsFactors          = FALSE
  )

  # ---- Observations ---------------------------------------------------------
  # Fishery landings (biomass, mt)
  simData$catch_data <- data.frame(
    Fleet_name        = "Fishery",
    Fleet_code        = 1,
    Species           = 1,
    Year              = years,
    Month             = 0,
    Selectivity_block = 1,
    Catch             = as.numeric(em_input[["L.obs"]][[1]]),
    Log_sd            = catch_log_sd,
    stringsAsFactors  = FALSE
  )

  # Survey index
  simData$index_data <- data.frame(
    Fleet_name        = "Survey",
    Fleet_code        = 2,
    Species           = 1,
    Year              = years,
    Month             = 0,
    Selectivity_block = 1,
    Observation       = survey_obs,
    Log_sd            = index_log_sd,
    stringsAsFactors  = FALSE
  )

  # ---- Age composition (proportions; one block per fleet x year) ------------
  # em_input comps are already row-normalised proportions (verified against the
  # OM). Sample_size carries the annual effective N for the multinomial.
  make_comp_block <- function(prop_mat, fleet_name, fleet_code, samp_n) {
    prop_mat <- as.matrix(prop_mat)
    colnames(prop_mat) <- comp_cols
    cbind(
      data.frame(
        Fleet_name   = fleet_name,
        Fleet_code   = fleet_code,
        Species      = 1L,
        Sex          = 0L,
        Age0_Length1 = 0L,      # age composition
        Year         = years,
        Month        = 0,
        Sample_size  = samp_n,
        stringsAsFactors = FALSE
      ),
      as.data.frame(prop_mat)
    )
  }

  simData$comp_data <- rbind(
    make_comp_block(em_input[["L.age.obs"]][["fleet1"]], "Fishery", 1L, fishery_n),
    make_comp_block(em_input[["survey.age.obs"]][[1]],   "Survey",  2L, survey_n)
  )

  # ---- Empty optional blocks (age-based, no length / CAAL / diet) -----------
  caal_cols <- c("Fleet_name", "Fleet_code", "Species", "Sex", "Year",
                 "Length", "Sample_size", paste0("CAAL_", seq_len(nages)))
  simData$caal_data <- setNames(
    data.frame(matrix(numeric(0), nrow = 0, ncol = length(caal_cols))), caal_cols)

  simData$emp_sel <- setNames(
    data.frame(matrix(numeric(0), nrow = 0, ncol = 5 + nages)),
    c("Fleet_name", "Fleet_code", "Species", "Sex", "Year", comp_cols))

  simData$NByageFixed <- setNames(
    data.frame(matrix(numeric(0), nrow = 0, ncol = 4 + nages)),
    c("Species_name", "Species", "Sex", "Year", age_cols))

  # ---- Age-transition & ageing-error (identity: age-based, no ageing error) --
  age_transition <- as.data.frame(diag(nages))
  colnames(age_transition) <- paste0("Length_", seq_len(nages))
  simData$age_trans_matrix <- cbind(
    data.frame(Age_transition_name = "Base", Age_transition_index = 1,
               Species = 1, Sex = 0, Age = minage:(minage + nages - 1L)),
    age_transition)

  age_error <- as.data.frame(diag(nages))
  colnames(age_error) <- paste0("Obs_age", seq_len(nages))
  simData$age_error <- cbind(
    data.frame(Species = 1, True_age = minage:(minage + nages - 1L)), age_error)

  # ---- Biology --------------------------------------------------------------
  # Weight-at-age: age- and time-invariant (Year = 0 broadcasts to all years).
  waa <- as.data.frame(matrix(as.numeric(om_input[["W.mt"]]), nrow = 1))
  colnames(waa) <- age_cols
  simData$weight <- cbind(
    data.frame(Wt_name = "Base", Wt_index = 1, Species = 1, Sex = 0, Year = 0), waa)

  mat <- as.data.frame(matrix(as.numeric(om_input[["mat.age"]]), nrow = 1))
  colnames(mat) <- age_cols
  simData$maturity <- cbind(data.frame(Species = 1), mat)

  # proportion.female is length-nages (or scalar); recycle to age vector.
  sr <- as.data.frame(matrix(rep_len(as.numeric(om_input[["proportion.female"]]), nages),
                             nrow = 1))
  colnames(sr) <- age_cols
  simData$sex_ratio <- cbind(data.frame(Species = 1), sr)

  m1 <- as.data.frame(matrix(as.numeric(om_input[["M.age"]]), nrow = 1))
  colnames(m1) <- age_cols
  simData$M1_base <- cbind(data.frame(Species = 1, Sex = 0), m1)

  # ---- Bioenergetics / diet placeholders (unused in single-species) ---------
  simData$Ceq <- 1; simData$Cindex <- 1; simData$Pvalue <- 1; simData$fday <- 1
  simData$CA <- 1; simData$CB <- 1; simData$Qc <- 1
  simData$Tco <- 1; simData$Tcm <- 1; simData$Tcl <- 1; simData$CK1 <- 1; simData$CK4 <- 1
  simData$Diet_distribution <- 1; simData$Diet_comp_weights <- 1

  simData$env_data <- data.frame(Year = years, Index1 = 0)

  simData$ration_data <- simData$weight |>
    dplyr::select(Species, Sex, Year, dplyr::contains("Age"))

  simData$diet_data <- setNames(
    data.frame(matrix(numeric(0), nrow = 0, ncol = 9)),
    c("Pred", "Prey", "Pred_sex", "Prey_sex", "Pred_age", "Prey_age",
      "Year", "Sample_size", "Stomach_proportion_by_weight"))

  # ---- Normalise + fill defaults via the exported public helpers ------------
  simData <- Rceattle::clean_data(simData)
  simData <- suppressMessages(Rceattle::switch_check(simData))

  attr(simData, "sigmaR_true")    <- sigmaR_true
  attr(simData, "sigmaR_floored") <- sigmaR_floored
  attr(simData, "survey_units")   <- if (survey_units_code == 1L) "biomass" else "numbers"
  simData
}
