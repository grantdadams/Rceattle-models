


#' Title
#'
#' @param Rceattle
#'
#' @return
#' @export
#'
#' @description
#' https://github.com/nmfs-ost/asar
#'
#'
#' @examples
convert_asar <- function(Rceattle = NULL){


  # https://github.com/nmfs-ost/asar/blob/5054b071f9b3fbf5c1964b52d92612bcbca5b898/R/convert_output.R#L54-L93
  out_new <- data.frame(
    label = NA,
    time = NA,
    era = NA,
    year = NA,
    fleet = NA,
    area = NA,
    season = NA,
    subseason = NA,
    age = NA,
    sex = NA,
    growth_pattern = NA,
    len_bins = NA,
    initial = NA,
    estimate = NA,
    uncertainty = NA,
    uncertainty_label = NA,
    likelihood = NA,
    gradient = NA,
    estimated = NA, # TRUE/FALSE
    module_name = NA,
    # Additional factors from SS3
    bio_pattern = NA,
    birthseas = NA,
    settlement = NA,
    morph = NA,
    # beg/mid = NA, # need to identify df where this is applicable
    type = NA,
    factor = NA,
    platoon = NA,
    month = NA,
    sexes = NA,
    part = NA,
    bin = NA,
    kind = NA,
    nsim = NA,
    age_a = NA,
    count = NA,
    morph = NA
  )


}
