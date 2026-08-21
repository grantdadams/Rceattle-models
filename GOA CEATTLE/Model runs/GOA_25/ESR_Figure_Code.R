library(ggplot2)

#'
#' Figs for EcoCons CEATTLE
#'
#' Updated for Rceattle >= 5.10.0. See the header of ESP_Figure_Code.R for the
#' package changes these functions had to follow (`ration` ->
#' `consumption_at_age`, `biomassByage` -> `biomass_at_age`, population
#' consumption = ration x numbers, and ggplot-returning `plot_*()`).
#'


#' Population-level consumption for ages `minage`+, in mt/yr
#'
#' Duplicated verbatim from ESP_Figure_Code.R so this file can be sourced on its
#' own (the ESR document does not source the ESP one). Keep the two in sync --
#' whichever file is sourced last defines the copy that runs.
#'
#' The individual annual ration (`consumption_at_age`, kg/yr) multiplied by
#' average numbers-at-age (`avgN_at_age`, thousands) and summed over sex and
#' age. kg x thousands = mt, so the result is total consumption in mt/yr. This
#' is how the TMB template forms total consumption (`avgN_at_age * ration` in
#' `predation.hpp`), so it reconciles with `Rceattle::plot_ration()`.
#'
#' @param Rceattle A fitted Rceattle model.
#' @param spp Species index.
#' @param endyr Last year to return. `NULL` (default) uses the model's `endyr`.
#' @param minage,maxage Age range to sum over. `maxage` defaults to the
#'   species' own `nages`.
#' @param sex Sex indices to sum over. `NULL` (default) uses all sexes.
weighted_ration <- function(Rceattle, spp = 1, endyr = NULL, minage = 4,
                            maxage = NULL, sex = NULL) {
  dl <- Rceattle$data_list
  if (is.null(endyr))  endyr  <- dl$endyr
  if (is.null(maxage)) maxage <- dl$nages[spp]
  if (is.null(sex))    sex    <- seq_len(dl$nsex[spp])

  yrs  <- dl$styr:endyr
  ages <- minage:maxage

  cons <- Rceattle$quantities$consumption_at_age[spp, sex, ages, seq_along(yrs), drop = FALSE]
  numb <- Rceattle$quantities$avgN_at_age[spp, sex, ages, seq_along(yrs), drop = FALSE]

  # drop = FALSE keeps the year dimension 4th even for a one-sex, one-age slice.
  apply(cons * numb, 4, sum)
}


ESR_plot_biomass_consumed <- function(Rceattle, endyr = NULL) {
  library(dplyr)
  library(ggplot2)

  if (is.null(endyr)) endyr <- Rceattle$data_list$endyr
  yrs <- Rceattle$data_list$styr:endyr
  ny  <- length(yrs)

  Biomass_eaten <- rbind(
    data.frame(Year = yrs, Biomass = apply(Rceattle$quantities$B_eaten_as_prey[1, , , 1:ny, drop = FALSE], 4, sum) / 1e6, Species = "a) Prey: Walleye pollock"),
    data.frame(Year = yrs, Biomass = apply(Rceattle$quantities$B_eaten_as_prey[3, , , 1:ny, drop = FALSE], 4, sum) / 1e6, Species = "b) Prey: Pacific cod"),
    data.frame(Year = yrs, Biomass = apply(Rceattle$quantities$B_eaten_as_prey[2, , , 1:ny, drop = FALSE], 4, sum) / 1e6, Species = "c) Prey: Arrowtooth flounder"))

  Biomass_eaten$Species <- factor(Biomass_eaten$Species,
                                  levels = c("a) Prey: Walleye pollock", "b) Prey: Pacific cod",
                                             "c) Prey: Arrowtooth flounder"))

  mn <- Biomass_eaten %>%
    group_by(Species) %>%
    summarize(mn = mean(Biomass, na.rm = TRUE),
              sd = sd(Biomass, na.rm = TRUE))

  Biomass_eaten <- merge(Biomass_eaten, mn, by = "Species")
  Biomass_eaten$upper <- Biomass_eaten$mn + Biomass_eaten$sd
  Biomass_eaten$lower <- Biomass_eaten$mn - Biomass_eaten$sd

  ggplot(data = Biomass_eaten, aes(x = Year, y = Biomass, color = Species, fill = Species)) +
    geom_line(data = Biomass_eaten, aes(x = Year, y = upper), color = "gray", show.legend = FALSE, linetype = "dashed") +
    geom_line(data = Biomass_eaten, aes(x = Year, y = mn), color = "gray", show.legend = FALSE) +
    geom_line(data = Biomass_eaten, aes(x = Year, y = lower), color = "gray", show.legend = FALSE, linetype = "dashed") +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, round(length(yrs) / 5, 0)),
                se = TRUE, show.legend = FALSE, alpha = .2) +
    geom_point(show.legend = FALSE) +
    facet_grid(Species ~ ., scales = "free_y") +
    scale_color_viridis_d(begin = 0, end = .6) +
    scale_fill_viridis_d(begin = 0, end = .6) +
    theme(panel.background = element_rect(fill = NA, color = "grey"),
          panel.grid.major = element_blank(), legend.key = element_blank(),
          strip.background = element_blank()) +
    ylab("Biomass eaten by all model predators (million mt)")
}


ESR_plot_annual_ration <- function(Rceattle, minage = 4, endyr = NULL) {
  library(dplyr)
  library(ggplot2)

  if (is.null(endyr)) endyr <- Rceattle$data_list$endyr
  yrs <- Rceattle$data_list$styr:endyr

  Annual_ration <- rbind(
    data.frame(Year = yrs, Ration = weighted_ration(Rceattle, spp = 1, endyr, minage) / 1e6, Species = "a) Predator: Walleye pollock"),
    data.frame(Year = yrs, Ration = weighted_ration(Rceattle, spp = 3, endyr, minage) / 1e6, Species = "b) Predator: Pacific cod"),
    data.frame(Year = yrs, Ration = weighted_ration(Rceattle, spp = 2, endyr, minage) / 1e6, Species = "c) Predator: Arrowtooth flounder"))
  Annual_ration$Species <- factor(Annual_ration$Species,
                                  levels = c("a) Predator: Walleye pollock", "b) Predator: Pacific cod",
                                             "c) Predator: Arrowtooth flounder"))

  mn <- Annual_ration %>%
    group_by(Species) %>%
    summarize(mn = mean(Ration, na.rm = TRUE),
              sd = sd(Ration, na.rm = TRUE))
  Annual_ration <- merge(Annual_ration, mn, by = "Species")
  Annual_ration$upper <- Annual_ration$mn + Annual_ration$sd
  Annual_ration$lower <- Annual_ration$mn - Annual_ration$sd

  ggplot(data = Annual_ration, aes(x = Year, y = Ration, color = Species, fill = Species)) +
    geom_line(data = Annual_ration, aes(x = Year, y = upper), color = "gray", show.legend = FALSE, linetype = "dashed") +
    geom_line(data = Annual_ration, aes(x = Year, y = mn), color = "gray", show.legend = FALSE) +
    geom_line(data = Annual_ration, aes(x = Year, y = lower), color = "gray", show.legend = FALSE, linetype = "dashed") +
    geom_point(show.legend = FALSE) +
    geom_smooth(method = lm, formula = y ~ splines::bs(x, round(length(yrs) / 5, 0)),
                se = TRUE, show.legend = FALSE, alpha = .2) +
    facet_grid(Species ~ ., scales = "free_y") +
    scale_color_viridis_d(begin = 0, end = .6) +
    scale_fill_viridis_d(begin = 0, end = .6) +
    theme(panel.background = element_rect(fill = NA, color = "grey"),
          panel.grid.major = element_blank(), legend.key = element_blank(),
          strip.background = element_blank()) +
    ylab(paste0("Annual ration (age ", minage, "+; million mt)"))
}


ESR_plot_M_age <- function(msModel, ssModel, age = 1, endyr = NULL) {
  library(dplyr)
  library(ggplot2)

  if (is.null(endyr)) endyr <- msModel$data_list$endyr
  yrs <- msModel$data_list$styr:endyr
  ny  <- length(yrs)

  m_panels <- list(
    list(sp = 1, sex = 1, label = "a) Prey: Walleye pollock"),
    list(sp = 3, sex = 1, label = "b) Prey: Pacific cod"),
    list(sp = 2, sex = 1, label = "c) Prey: Arrowtooth (F)"),
    list(sp = 2, sex = 2, label = "d) Prey: Arrowtooth (M)"))

  M2 <- do.call(rbind, lapply(m_panels, function(p) {
    rbind(
      data.frame(Year = yrs, Model = "SSM", Species = p$label,
                 totalM = as.numeric(ssModel$quantities$M_at_age[p$sp, p$sex, age, 1:ny])),
      data.frame(Year = yrs, Model = "MSM", Species = p$label,
                 totalM = as.numeric(msModel$quantities$M_at_age[p$sp, p$sex, age, 1:ny])))
  }))
  M2$Species <- factor(M2$Species,
                       levels = c("a) Prey: Walleye pollock", "b) Prey: Pacific cod",
                                  "c) Prey: Arrowtooth (F)", "d) Prey: Arrowtooth (M)"))
  M2$Model <- factor(M2$Model, levels = c("MSM", "SSM"))

  ggplot(data = M2, aes(x = Year, y = totalM, color = Species, fill = Species, linetype = Model)) +
    geom_line(data = M2 %>% filter(Model == "SSM"), show.legend = FALSE) +
    coord_cartesian(xlim = range(yrs)) +
    geom_point(data = M2 %>% filter(Model == "MSM"), show.legend = FALSE) +
    geom_smooth(data = M2 %>% filter(Model == "MSM"), method = lm,
                formula = y ~ splines::bs(x, round(length(yrs) / 5, 0)),
                se = TRUE, show.legend = FALSE, alpha = .2) +
    facet_grid(Species ~ ., scales = "free_y") +
    scale_color_viridis_d(begin = 0, end = .6) +
    scale_fill_viridis_d(begin = 0, end = .6) +
    ylab("Mortality rate (M1+M2)") +
    theme(panel.background = element_rect(fill = NA, color = "grey"),
          panel.grid.major = element_blank(), legend.key = element_blank(),
          strip.background = element_blank())
}


#' Share of predation mortality (M2) on one prey age attributable to each predator
#'
#' Rceattle >= 5.x exports this as `plot_m2_at_age_prop()`, which is where the
#' indexing of the 5-D `M2_prop` array (predator x prey x predator age x prey
#' age x year, with sex folded into the species dimension) is maintained. The
#' hand-rolled version this file used to carry indexed `M2_at_age` as if it were
#' `M2_prop` and divided by a total it never formed, so it is gone -- call the
#' package function instead.
#'
#' @param Rceattle A fitted multi-species Rceattle model.
#' @param age Prey age.
#' @param species Prey species index (all species if `NULL`).
#' @param endyr Ignored; kept so existing calls do not error. The package
#'   function plots the hindcast and takes `minyr` / `maxyr` instead.
#' @param sex Ignored; the package function panels both sexes of a sexed prey.
ESR_plot_propM <- function(Rceattle, age = 1, species = 1, sex = 1, endyr = NULL) {
  if (!missing(sex) || !is.null(endyr)) {
    message("`sex` and `endyr` are ignored: plot_m2_at_age_prop() panels each ",
            "sex of a sexed prey and plots the hindcast. Use `minyr` / `maxyr` ",
            "to trim the years.")
  }
  Rceattle::plot_m2_at_age_prop(Rceattle, age = age, species = species)
}
