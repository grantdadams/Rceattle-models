library(ggplot2)

#'
#' Figs for ESP CEATTLE
#'
#' Updated for Rceattle >= 5.10.0. Three things changed in the package that this
#' file depended on:
#'
#'  * `quantities$ration` is now `quantities$consumption_at_age` -- the annual
#'    ration of ONE fish (kg/yr) -- and `quantities$biomassByage` is
#'    `quantities$biomass_at_age`.
#'  * population-level consumption is `consumption_at_age * avgN_at_age`, not
#'    ration x biomass. See `plot_ration()` and `predation.hpp`.
#'  * the `plot_*()` functions return ggplot objects, so a figure is saved with
#'    `ggplot2::ggsave()` (or the plotter's own `file =` argument), not by
#'    wrapping the call in `jpeg()` / `dev.off()`, and annotations are added as
#'    layers rather than with `mtext()` / `legend()`.
#'
#' NOTE: `weighted_ration()` is duplicated verbatim in ESR_Figure_Code.R, which
#' is sourced after this file. Keep the two in sync.
#'


#' Population-level consumption for ages `minage`+, in mt/yr
#'
#' The individual annual ration (`consumption_at_age`, kg/yr) multiplied by
#' average numbers-at-age (`avgN_at_age`, thousands) and summed over sex and
#' age. kg x thousands = mt, so the result is total consumption in mt/yr.
#'
#' This is how the TMB template itself forms total consumption
#' (`avgN_at_age * ration` in `predation.hpp`), so it reconciles with
#' `Rceattle::plot_ration()` and with the predation that generates
#' `B_eaten_as_prey`.
#'
#' It is also the TMB equivalent of the ADMB `biomassByage * ration2` used for
#' the EBS indices: in ADMB `ration2` is a *relative* ration (g/g of body
#' weight), so biomass x ration2 is mt/yr. Rceattle reports the ration in
#' absolute kg/fish/yr instead, so multiplying it by biomass -- which earlier
#' versions of this file did -- multiplies by weight-at-age a second time and
#' is not a quantity in any unit.
#'
#' @param Rceattle A fitted Rceattle model.
#' @param spp Species index.
#' @param endyr Last year to return. `NULL` (default) uses the model's `endyr`,
#'   i.e. the end of the hindcast.
#' @param minage,maxage Age range to sum over. `maxage` defaults to the
#'   species' own `nages`.
#' @param sex Sex indices to sum over. `NULL` (default) uses all sexes the
#'   species has.
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


#' Three-panel ESP figure for one species: M-at-age, biomass eaten, ration
#'
#' @param msModel,ssModel Fitted multi- and single-species models.
#' @param age Prey age for the mortality panel.
#' @param minage First predator age for the ration panel.
#' @param endyr Last year to plot. `NULL` (default) uses the model's `endyr`.
#' @param species Species index to plot (1 = pollock, 2 = arrowtooth,
#'   3 = Pacific cod in the GOA model).
ESP_plot <- function(msModel, ssModel, age = 1, minage = 4, endyr = NULL,
                     species = 1) {
  library(dplyr)
  library(ggplot2)

  if (is.null(endyr)) endyr <- msModel$data_list$endyr
  yrs <- msModel$data_list$styr:endyr
  ny  <- length(yrs)

  # - M-at-age ----
  # One row per (species, sex) panel the ESP reports. Colour/facet follow the
  # species; `Model` separates the single- and multi-species M.
  m_panels <- list(
    list(sp = 1, sex = 1, spp = 1, label = "Prey: Walleye pollock"),
    list(sp = 3, sex = 1, spp = 3, label = "Prey: Pacific cod"),
    list(sp = 2, sex = 1, spp = 2, label = "Prey: Females"),
    list(sp = 2, sex = 2, spp = 2, label = "Prey: Males"))

  M2 <- do.call(rbind, lapply(m_panels, function(p) {
    rbind(
      data.frame(Year = yrs, Model = "SSM", Spp = p$spp, Species = p$label,
                 totalM = as.numeric(ssModel$quantities$M_at_age[p$sp, p$sex, age, 1:ny])),
      data.frame(Year = yrs, Model = "MSM", Spp = p$spp, Species = p$label,
                 totalM = as.numeric(msModel$quantities$M_at_age[p$sp, p$sex, age, 1:ny])))
  }))
  M2$Species <- factor(M2$Species,
                       levels = c("Prey: Walleye pollock", "Prey: Pacific cod",
                                  "Prey: Females", "Prey: Males"))
  M2$Model <- factor(M2$Model, levels = c("MSM", "SSM"))
  M2 <- M2 %>% filter(Spp == species)

  g1 <- ggplot(data = M2, aes(x = Year, y = totalM, color = Species,
                              fill = Species, linetype = Model)) +
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


  # B-eaten ----
  # Summed over sex and age: the total biomass of this species removed by all
  # model predators, in million mt (the model reports it in mt).
  Biomass_eaten <- rbind(
    data.frame(Year = yrs, Biomass = apply(msModel$quantities$B_eaten_as_prey[1, , , 1:ny, drop = FALSE], 4, sum) / 1e6, Spp = 1, Species = "Prey: Walleye pollock"),
    data.frame(Year = yrs, Biomass = apply(msModel$quantities$B_eaten_as_prey[3, , , 1:ny, drop = FALSE], 4, sum) / 1e6, Spp = 3, Species = "Prey: Pacific cod"),
    data.frame(Year = yrs, Biomass = apply(msModel$quantities$B_eaten_as_prey[2, , , 1:ny, drop = FALSE], 4, sum) / 1e6, Spp = 2, Species = "Prey: Arrowtooth flounder"))

  Biomass_eaten$Species <- factor(Biomass_eaten$Species,
                                  levels = c("Prey: Walleye pollock", "Prey: Pacific cod",
                                             "Prey: Arrowtooth flounder"))

  mn <- Biomass_eaten %>%
    group_by(Species) %>%
    summarize(mn = mean(Biomass, na.rm = TRUE),
              sd = sd(Biomass, na.rm = TRUE))

  Biomass_eaten <- merge(Biomass_eaten, mn, by = "Species")
  Biomass_eaten$upper <- Biomass_eaten$mn + Biomass_eaten$sd
  Biomass_eaten$lower <- Biomass_eaten$mn - Biomass_eaten$sd

  Biomass_eaten <- Biomass_eaten %>%
    filter(Spp == species)

  g2 <- ggplot(data = Biomass_eaten, aes(x = Year, y = Biomass, color = Species, fill = Species)) +
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
    ylab("Biomass eaten by predators (million mt)")


  # Ration ----
  Annual_ration <- rbind(
    data.frame(Year = yrs, Ration = weighted_ration(msModel, spp = 1, endyr, minage) / 1e6, Spp = 1, Species = "Predator: Walleye pollock"),
    data.frame(Year = yrs, Ration = weighted_ration(msModel, spp = 3, endyr, minage) / 1e6, Spp = 3, Species = "Predator: Pacific cod"),
    data.frame(Year = yrs, Ration = weighted_ration(msModel, spp = 2, endyr, minage) / 1e6, Spp = 2, Species = "Predator: Arrowtooth flounder"))
  Annual_ration$Species <- factor(Annual_ration$Species,
                                  levels = c("Predator: Walleye pollock", "Predator: Pacific cod",
                                             "Predator: Arrowtooth flounder"))

  mn <- Annual_ration %>%
    group_by(Species) %>%
    summarize(mn = mean(Ration, na.rm = TRUE),
              sd = sd(Ration, na.rm = TRUE))
  Annual_ration <- merge(Annual_ration, mn, by = "Species")
  Annual_ration$upper <- Annual_ration$mn + Annual_ration$sd
  Annual_ration$lower <- Annual_ration$mn - Annual_ration$sd

  Annual_ration <- Annual_ration %>%
    filter(Spp == species)

  g3 <- ggplot(data = Annual_ration, aes(x = Year, y = Ration, color = Species, fill = Species)) +
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

  # align/axis line the three panels' plotting areas up: the facet strips and
  # y-axis labels are different widths, which otherwise staggers them.
  cowplot::plot_grid(g1, g2, g3, ncol = 1, align = "v", axis = "lr")
}
