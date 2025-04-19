# Code to fit 2024 GOA Pollock model in Rceattle

# Load libraries ----
library(Rceattle)
library(readxl)
library(dplyr)


# Read in data ----
mydata_pollock <- Rceattle::read_data( file = "Data/GOA_24_pollock_single_species_1970-2024.xlsx")


# - Fit single-species models
pollock_base <- fit_mod(data_list = mydata_pollock,
                        inits = NULL,       # Initial parameters = 0
                        file = NULL,        # Don't save
                        estimateMode = 0,   # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0,        # Single species mode
                        verbose = 1,        # Minimal messages
                        initMode = 1,       # Unfished equilibrium with init_dev's turned off
                        phase = TRUE)       # Phase

# - Estimate age-invariant M
pollock_estM <- fit_mod(data_list = mydata_pollock,
                        inits = pollock_base$estimated_params,       # Initial parameters = 0
                        file = NULL,        # Don't save
                        estimateMode = 0,   # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0,        # Single species mode
                        verbose = 1,        # Minimal messages
                        M1Fun = build_M1(M1_model = 1), # Estimate age and time invariant M: see ?build_M1 for more details
                        initMode = 1,       # Unfished equilibrium with init_dev's turned off
                        phase = TRUE)       # Phase


# - Estimate age-invariant M and Ricker SRR
pollock_estM_ricker <- fit_mod(data_list = mydata_pollock,
                        inits = NULL,       # Initial parameters = 0
                        file = NULL,        # Don't save
                        estimateMode = 0,   # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0,        # Single species mode
                        verbose = 1,        # Minimal messages
                        M1Fun = build_M1(M1_model = 1), # Estimate age and time invariant M: see ?build_M1 for more details
                        recFun = build_srr(srr_fun = 0, # Default no-stock recruit curve
                                           srr_pred_fun = 4, # Ricker curve as additional penalty (if srr_fun and srr_pred_fun are the same, no penalty is used)
                                           srr_est_mode = 1, # Freely estimate alpha
                                           srr_hat_styr = 1977, # Estimate starting 7 years after styr = 1970
                                           srr_hat_endyr = 2020
                        ),
                        initMode = 1,       # Unfished equilibrium with init_dev's turned off
                        phase = TRUE)       # Phase


# - Plot
mod_list <- list(pollock_base, pollock_estM, pollock_estM_ricker)
model_names <- c("Base", "est M", "M & Ricker")

plot_biomass(mod_list, model_names = model_names)
plot_recruitment(mod_list, model_names = model_names)
plot_ssb(mod_list, model_names = model_names)
plot_stock_recruit(pollock_estM_ricker)

