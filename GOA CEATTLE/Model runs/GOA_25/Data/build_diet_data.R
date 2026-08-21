# =============================================================================
# Rebuild the GOA_25 diet_data sheet from the full-age source
# =============================================================================
#
# WHY
#
# `GOA_25_data_1977_2025.xlsx` shipped with `diet_data` truncated to age 10 for
# every species: 1,600 rows, max Pred_age/Prey_age = 10/10/10. The 2023 and 2024
# files carry the same numbers at 10/21/12 (4,096 rows on the three-species
# block, identical to 1e-17 on every shared cell) -- so rows were dropped, not
# revised.
#
# Arrowtooth flounder has 21 ages, so the cut removed 79.2% of its diet mass.
# A predator age with no diet rows gets `suit_other = 1` in the TMB model: it
# eats only "other food", exerts no predation, and is itself never eaten. In the
# shipped 2025 fit, arrowtooth females at ages 12 and 18 both had
# `suit_other = 1.000` -- the most piscivorous half of the dominant GOA predator
# was switched off. Arrowtooth diet mass climbs from 0.13 at age 10 to 0.26 at
# age 21, so this is exactly the wrong half to lose.
#
# The truncation almost certainly came from `data_check()`, which errors on
# "Pred ages in 'diet_data' > 'nages'". The 2023/2024 sheets do violate that --
# Pacific cod (Pred 3) carries ages 11-12 against `nages = 10` -- but the fix is
# to pool those two ages into the cod plus group, not to truncate all three
# species at 10.
#
# WHAT THIS SCRIPT DOES
#
#  1. reads the three-species block of the 2024 `UobsWtAge` sheet (Pred and
#     Prey in 1:3; the 2024 file also carries a halibut predator, which this
#     three-species model has no place for),
#  2. accumulates prey ages above `nages` into the prey plus group by SUMMING
#     (they are proportions of one predator's stomach, so they add),
#  3. pools predator ages above `nages` into the predator plus group by a
#     Sample_size-weighted MEAN (these are different predators' stomachs --
#     summing them would invent diet mass). This affects Pacific cod only, and
#     raises its age-10+ diet total from 0.0238 to 0.0286.
#  4. renumbers `stratum_id` / `stomach_id` via `clean_data()`, validates with
#     `data_check()`, and writes a new workbook.
#
# The pooling weights in step 3 should really be predator numbers-at-age; the
# sheet carries only `Sample_size`, which is a flat 20 everywhere, so the
# weighted mean reduces to a plain mean over ages 10, 11 and 12. Set
# POOL_PLUS_GROUP <- FALSE below to drop those rows instead, which is what the
# shipped file effectively did.
#
# Under `suitMode = 0` (this model) the diet likelihood is inactive -- the
# "Stomach content data" row of `jnll_comp` is identically zero -- so
# `Sample_size` does not touch the fit. It would matter under estimated
# suitability (`suitMode > 0`).
#
# Run from the "Model runs" directory.
# =============================================================================

library(Rceattle)
library(readxl)
library(dplyr)

SOURCE_FILE     <- "GOA_24/Data/GOA_24_data_1977_2024.xlsx"
SOURCE_SHEET    <- "UobsWtAge"
TARGET_FILE     <- "GOA_25/Data/GOA_25_data_1977_2025.xlsx"
OUT_FILE        <- "GOA_25/Data/GOA_25_data_1977_2025_dietfix.xlsx"
POOL_PLUS_GROUP <- TRUE


# -- 1. Target model dimensions ------------------------------------------------
data_list <- read_data(file = TARGET_FILE)
nspp   <- data_list$nspp
nages  <- data_list$nages
minage <- data_list$minage

message("Target model: ", nspp, " species, nages = ", paste(nages, collapse = "/"))
message("Shipped diet_data: ", nrow(data_list$diet_data), " rows, max Pred_age = ",
        paste(tapply(data_list$diet_data$Pred_age, data_list$diet_data$Pred, max),
              collapse = "/"))


# -- 2. Full-age source --------------------------------------------------------
src <- as.data.frame(read_excel(SOURCE_FILE, sheet = SOURCE_SHEET))
# The 2024 sheet reads the proportion column as character.
src$Stomach_proportion_by_weight <- as.numeric(src$Stomach_proportion_by_weight)

src <- src[src$Pred %in% seq_len(nspp) & src$Prey %in% seq_len(nspp), ]
message("Source rows for this species set: ", nrow(src), ", max Pred_age = ",
        paste(tapply(src$Pred_age, src$Pred, max), collapse = "/"))
stopifnot(!anyNA(src$Stomach_proportion_by_weight))


# -- 3. Map ages onto the model's age structure --------------------------------
# Both directions are capped at the species' own plus group. Rows at or below
# `minage` are left alone: negative and zero ages are the aggregated diet
# formats (prey-summed / predator-averaged) that the TMB template skips for
# MSVPA suitability, and they must not be folded into a real age.
plus_group <- function(age, species) {
  ifelse(age >= minage[species], pmin(age, nages[species]), age)
}

src$Prey_age_new <- plus_group(src$Prey_age, src$Prey)
src$Pred_age_new <- plus_group(src$Pred_age, src$Pred)

n_prey_pooled <- sum(src$Prey_age_new != src$Prey_age)
n_pred_pooled <- sum(src$Pred_age_new != src$Pred_age)
message("Prey rows folded into a plus group: ", n_prey_pooled,
        " (mass ", signif(sum(src$Stomach_proportion_by_weight[src$Prey_age_new != src$Prey_age]), 4), ")")
message("Pred rows folded into a plus group: ", n_pred_pooled,
        " (mass ", signif(sum(src$Stomach_proportion_by_weight[src$Pred_age_new != src$Pred_age]), 4), ")")

if (!POOL_PLUS_GROUP) {
  src <- src[src$Pred_age_new == src$Pred_age & src$Prey_age_new == src$Prey_age, ]
  message("POOL_PLUS_GROUP = FALSE: dropped the out-of-range rows instead.")
}

# `Sample_size` is a property of the predator's stomach sample, so it has to be
# constant within a predator cell. The prey-side grouping below carries it as a
# key: if it varied across the prey ages being folded into a plus group, those
# rows would stay separate instead of summing, and the accumulation would be
# silently partial. Weights of 0 would also make the weighted mean NaN.
ss_by_pred <- src |>
  group_by(Pred, Pred_sex, Pred_age, Year) |>
  summarise(n_distinct_ss = dplyr::n_distinct(Sample_size),
            min_ss = min(Sample_size), .groups = "drop")
stopifnot(all(ss_by_pred$n_distinct_ss == 1), all(ss_by_pred$min_ss > 0))

# Prey side: proportions of the same stomach, so they SUM.
diet <- src |>
  group_by(Pred, Prey, Pred_sex, Prey_sex, Pred_age, Pred_age_new,
           Prey_age = Prey_age_new, Year, Sample_size) |>
  summarise(Stomach_proportion_by_weight = sum(Stomach_proportion_by_weight),
            .groups = "drop")

# Predator side: separate stomachs, so take the Sample_size-weighted MEAN across
# the pooled predator ages. `Sample_size` becomes the number of stomachs behind
# the pooled age -- the sum of the distinct per-age sample sizes, counted once
# each rather than once per row.
#
# Group by `Pred_age_new` and rename afterwards: grouping by
# `Pred_age = Pred_age_new` would shadow the original `Pred_age` inside
# summarise() with the single grouping value, so the sample sizes of the pooled
# ages would collapse to one.
diet <- diet |>
  group_by(Pred, Prey, Pred_sex, Prey_sex, Pred_age_new, Prey_age, Year) |>
  summarise(
    Stomach_proportion_by_weight =
      stats::weighted.mean(Stomach_proportion_by_weight, w = Sample_size),
    Sample_size = sum(unique(data.frame(age = Pred_age, n = Sample_size))$n),
    .groups = "drop") |>
  rename(Pred_age = Pred_age_new) |>
  select(Pred, Prey, Pred_sex, Prey_sex, Pred_age, Prey_age, Year,
         Sample_size, Stomach_proportion_by_weight) |>
  as.data.frame()

message("Sample_size behind each predator age: ",
        paste(sort(unique(diet$Sample_size)), collapse = ", "))

message("Rebuilt diet_data: ", nrow(diet), " rows, max Pred_age = ",
        paste(tapply(diet$Pred_age, diet$Pred, max), collapse = "/"))


# -- 4. Diet mass preserved? ---------------------------------------------------
# Predator totals must be unchanged except for the two cod ages that were
# averaged into the plus group.
cmp <- function(d, lab) {
  ok <- d$Pred_age >= minage[d$Pred] & d$Prey_age >= minage[d$Prey]
  round(tapply(d$Stomach_proportion_by_weight[ok], d$Pred[ok], sum), 4)
}
message("Diet mass by predator -- source: ", paste(cmp(src), collapse = " / "))
message("Diet mass by predator -- rebuilt: ", paste(cmp(diet), collapse = " / "))
message("Diet mass by predator -- shipped: ", paste(cmp(data_list$diet_data), collapse = " / "))


# -- 5. Renumber and write -----------------------------------------------------
data_list$diet_data <- diet
data_list <- clean_data(data_list)      # arranges + rebuilds stratum_id / stomach_id

write_data(data_list, file = OUT_FILE)
message("Wrote ", OUT_FILE)


# -- 6. Confirm the round trip, and that the switched-off ages are back --------
chk <- read_data(file = OUT_FILE)
# clean_data() sorts by stomach_id, so compare on the key rather than row order.
key <- function(d) paste(d$Pred, d$Prey, d$Pred_sex, d$Prey_sex,
                         d$Pred_age, d$Prey_age, d$Year, sep = "|")
i <- match(key(diet), key(chk$diet_data))
stopifnot(identical(nrow(chk$diet_data), nrow(diet)), !anyNA(i),
          isTRUE(all.equal(chk$diet_data$Stomach_proportion_by_weight[i],
                           diet$Stomach_proportion_by_weight, tolerance = 1e-12)))
message("Round trip OK: ", nrow(chk$diet_data), " rows, max Pred_age = ",
        paste(tapply(chk$diet_data$Pred_age, chk$diet_data$Pred, max), collapse = "/"))

# estimateMode = 3 builds the model without optimizing -- it runs data_check()
# on the way through and reports the suitability the new diet data implies.
build <- fit_mod(data_list = chk, estimateMode = 3, msmMode = 1, random_rec = FALSE,
                 suit_styr = 1990, suit_endyr = 2015,
                 M1Fun = build_M1(M1_model = c(1, 2, 1),
                                  M1_use_prior = FALSE, M2_use_prior = FALSE),
                 fit_control = fit_control(verbose = 0, getsd = FALSE))
so <- build$quantities$suit_other
message("Arrowtooth female suit_other (1.0 = eats only other food, exerts no predation):")
for (a in c(1, 4, 8, 12, 18, 21)) message("  age ", a, ": ", round(so[2, 1, a, 1], 4))
stopifnot(all(so[2, 1, minage[2]:nages[2], 1] < 1))
message("No arrowtooth age is switched out of the predation calculation.")
