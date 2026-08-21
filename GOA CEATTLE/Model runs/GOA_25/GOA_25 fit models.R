library(Rceattle)
library(readxl)
library(dplyr)
setwd("Model runs/GOA_25/")

# Rceattle >= 5.x: the optimizer / sdreport / phasing knobs (`phase`, `getsd`,
# `verbose`, `newtonsteps`, ...) moved off fit_mod() and onto fit_control().
# Passing them directly still works but warns, and will be removed.

# Manually add in diet data
#
# The `_dietfix` file is the 2025 data with its `diet_data` sheet rebuilt: the
# shipped sheet was truncated at age 10, which dropped arrowtooth ages 11-21 --
# 79% of that predator's diet mass. An age with no diet row is not estimated
# under `suitMode = 0`, it is switched off (`suit_other = 1`: eats only other
# food, exerts no predation, is never eaten), so the shipped file removed the
# most piscivorous half of the dominant GOA predator. Regenerate the file with
# Data/build_diet_data.R, which documents the repair.
combined_data <- read_data(file = "Data/GOA_25_data_1977_2025_dietfix.xlsx")
plot_data(combined_data)

# What the configuration below actually requires / ignores
data_requirements(combined_data, msmMode = 1)


# - Est single-species fixed M
ss_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Hindcast + HCR projection
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            fit_control = fit_control(verbose = 1,
                                                      phase = TRUE))
summary(ss_mod)
convergence_diagnostics(ss_mod)


# - Est single-species estimate M
# M1_model codes now also accept their names: 1 = "sex_age_invariant",
# 2 = "sex_specific" (arrowtooth), 3 = "sex_age_specific", 0 = "fixed".
ssm <- Rceattle::fit_mod(data_list = combined_data,
                         inits = ss_mod$estimated_params,
                         file = NULL, # Don't save
                         estimateMode = 0, # Hindcast + HCR projection
                         random_rec = FALSE, # No random recruitment
                         msmMode = 0, # Single species mode
                         fit_control = fit_control(verbose = 1,
                                                   phase = TRUE),
                         M1Fun = build_M1(M1_model = c("sex_age_invariant",
                                                       "sex_specific",
                                                       "sex_age_invariant"),
                                          M1_use_prior = FALSE,
                                          M2_use_prior = FALSE))
summary(ssm)
convergence_diagnostics(ssm)


# - Est multi-species
ms_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = ss_mod$estimated_params,
                            file = NULL, # Don't save
                            estimateMode = 0, # Hindcast + HCR projection
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # Multi species mode
                            suit_styr = 1990,
                            suit_endyr = 2015,
                            fit_control = fit_control(verbose = 1,
                                                      phase = FALSE),
                            M1Fun = build_M1(M1_model = c("sex_age_invariant",
                                                          "sex_specific",
                                                          "sex_age_invariant"),
                                             M1_use_prior = FALSE,
                                             M2_use_prior = FALSE))
summary(ms_mod)
convergence_diagnostics(ms_mod)


# - Plot
# plot_*() return ggplot objects; print them or save with `file =` / ggsave().
mod_list_all <- list(ss_mod, ssm, ms_mod)
mod_names <- c("Single-spp CEATTLE (fixed M)",
               "Single-spp CEATTLE (est. M)",
               "Multi-spp CEATTLE")

plot_biomass(mod_list_all, model_names = mod_names)
plot_b_eaten(mod_list_all, model_names = mod_names)
plot_recruitment(mod_list_all, model_names = mod_names)
plot_ration(ms_mod, minage = 4)
plot_m_at_age(mod_list_all, age = 1, model_names = mod_names)

# - Save
# The run configuration of each fit is on `$run_config`; save_config() writes it
# to a YAML that fit_mod(config = load_config(...)) reproduces exactly.
save_config(ms_mod, file = "Models/GOA_25_ms_mod.yaml")
save(mod_list_all, file = "Models/GOA_25_mod_list.RData")
