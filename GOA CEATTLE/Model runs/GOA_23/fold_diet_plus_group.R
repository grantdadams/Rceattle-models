# Fold diet rows for ages past a species' oldest modelled age (minage + nages - 1) into
# that plus group. Prey proportions are summed within each stomach; predator stomachs
# are combined, each weighted by its sample size, and their sample sizes add. The 2023
# GOA workbook carries cod diet at ages 11-12 against a cod model of ages 1-10.
fold_diet_plus_group <- function(data_list) {
  oldest <- data_list$minage + data_list$nages - 1
  keys   <- c("Pred", "Pred_sex", "Pred_age", "Year", "Prey", "Prey_sex", "Prey_age")
  dd <- data_list$diet_data

  # Prey side: rows of one stomach share its sample size.
  dd$Prey_age <- pmin(dd$Prey_age, oldest[dd$Prey])
  dd <- dd |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(Sample_size = dplyr::first(Sample_size),
                     Stomach_proportion_by_weight = sum(Stomach_proportion_by_weight),
                     .groups = "drop")

  # Predator side: the proportion is computed before Sample_size is summed.
  dd$Pred_age <- pmin(dd$Pred_age, oldest[dd$Pred])
  dd <- dd |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      Stomach_proportion_by_weight = if (sum(Sample_size) > 0) {
        sum(Stomach_proportion_by_weight * Sample_size) / sum(Sample_size)
      } else {
        mean(Stomach_proportion_by_weight)
      },
      Sample_size = sum(Sample_size),
      .groups = "drop")

  data_list$diet_data <- as.data.frame(dd[c("Pred", "Prey", "Pred_sex", "Prey_sex",
                                            "Pred_age", "Prey_age", "Year", "Sample_size",
                                            "Stomach_proportion_by_weight")])
  data_list
}
