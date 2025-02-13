# Code to fit 2024 GOA Pollock model in Rceattle


# Install dependencies ----
install.packages("pacman")
install.packages("TMB", type = "source")
install.packages("Matrix", type = "source")
pacman::p_load(dplyr,
               ggplot2,
               MASS,
               oce,
               readxl,
               TMB,
               devtools,
               writexl,
               reshape2,
               gplots,
               tidyr,
               testthat,
               foreach,
               R.utils,
               knitr,
               doParallel)
devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
remotes::install_github("grantdadams/Rceattle", ref = "dev_srr") # dev_srr branch is most up to date


# Load libraries ----
library(Rceattle)
library(readxl)
library(dplyr)


# Read in data ----
# - The data can be modified in excel or R
mydata_pollock <- Rceattle::read_data( file = "GOA_24_pollock_single_species_1970-2024.xlsx")


# FIT HINDCASTS ----
# 1) Fit base single-species model ----
# - fixed M, multinomial, no stock-recruit curve
pollock_base <- fit_mod(data_list = mydata_pollock,
                        inits = NULL,       # Initial parameters = 0
                        file = NULL,        # Don't save
                        estimateMode = 0,   # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0,        # Single species mode
                        verbose = 1,        # Minimal messages
                        initMode = 1,       # Unfished equilibrium with init_dev's turned off
                        phase = TRUE)       # Phase


# 2) Estimate age-invariant M ----
pollock_estM <- fit_mod(data_list = mydata_pollock,
                        inits = NULL,       # Initial parameters = 0
                        file = NULL,        # Don't save
                        estimateMode = 0,   # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0,        # Single species mode
                        verbose = 1,        # Minimal messages
                        M1Fun = build_M1(M1_model = 1), # Estimate age and time invariant M: see ?build_M1 for more details
                        initMode = 1,       # Unfished equilibrium with init_dev's turned off
                        phase = TRUE)       # Phase


# 3) Estimate age-invariant M and Ricker SRR ----
pollock_estM_ricker <- fit_mod(data_list = mydata_pollock,
                               inits = NULL,       # Initial parameters = 0
                               file = NULL,        # Don't save
                               estimateMode = 0,   # Estimate
                               random_rec = FALSE, # No random recruitment
                               msmMode = 0,        # Single species mode
                               verbose = 1,        # Minimal messages
                               M1Fun = build_M1(M1_model = 1), # Estimate age and time invariant M: see ?build_M1 for more details
                               recFun = build_srr(srr_fun = 0, # Default no-stock recruit curve
                                                  srr_pred_fun = 4, # Ricker curve as additional penalty sensu Ianelli (if srr_fun and srr_pred_fun are the same, no penalty is used)
                                                  srr_est_mode = 1, # Freely estimate alpha
                                                  srr_hat_styr = 1977, # Estimate starting 7 years after styr = 1970
                                                  srr_hat_endyr = 2020
                               ),
                               initMode = 1,       # Unfished equilibrium with init_dev's turned off
                               phase = TRUE)       # Phase



# PROJECTIONS ----
# - Can also be done above
pollock_base_tier3 <- fit_mod(data_list = pollock_base$data_list,
                        inits = pollock_base$estimated_params,       # Initial parameters = 0
                        file = NULL,             # Don't save
                        estimateMode = 2,        # Estimate projection only
                        random_rec = FALSE,      # No random recruitment
                        msmMode = 0,             # Single species mode
                        verbose = 1,             # Minimal messages
                        initMode = 1,            # Unfished equilibrium with init_dev's turned off
                        phase = FALSE,           # Don't Phase
                        HCR = build_hcr(HCR = 5, # Tier3 HCR
                                        FsprTarget = 0.4, # F40% - SPR
                                        FsprLimit = 0.35, # F35% - SPR
                                        Plimit = c(0.2),  # No fishing when SB<SB20
                                        Alpha = 0.05))



# PLOTS ----
mod_list <- list(pollock_base, pollock_estM, pollock_estM_ricker, pollock_base_tier3)
model_names <- c("Base", "est M", "M & Ricker", "Base Tier 3")

# * Time-series ----
plot_biomass(mod_list, model_names = model_names)
plot_recruitment(mod_list, model_names = model_names)
plot_ssb(mod_list, model_names = model_names, add_ci = TRUE, incl_proj = TRUE)

plot_stock_recruit(pollock_estM_ricker)
plot_selectivity(pollock_estM_ricker)

# * Diagnostics ----
plot_index(mod_list, model_names = model_names)
plot_catch(mod_list, model_names = model_names)
plot_comp(pollock_estM_ricker)


# RETROSPECTIVES ----
pollock_base_retro <- retrospective(pollock_base, peels = 5)
plot_biomass(pollock_base_retro$Rceattle_list,
             model_names = paste("Mohn's rho =", round(pollock_base_retro$mohns[1,2], 3)))


# MSE ----
mse <- mse_run_parallel(om = pollock_estM_ricker, # Ricker OM
                        em = pollock_base_tier3,  # Fixed M EM w/ Tier 3 HCR
                        nsim = 1,
                        assessment_period = 1, sampling_period = 1, # Can be a vector
                        simulate_data = TRUE, sample_rec = TRUE,
                        dir = NULL, file = NULL)

plot_biomass(mse[[1]]$EM, reference = mse[[1]]$OM)
plot_depletionSSB(mse[[1]]$EM, reference = mse[[1]]$OM)


