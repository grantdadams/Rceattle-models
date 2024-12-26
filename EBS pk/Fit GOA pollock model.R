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
mydata_pollock <- Rceattle::read_data( file = "GOA_24_pollock_single_species_1970-2024.xlsx")


# - Fit single-species models
pollock_base <- fit_mod(data_list = mydata_pollock,
                        inits = NULL, # Initial parameters = 0
                        file = NULL, # Don't save
                        estimateMode = 0, # Estimate
                        random_rec = FALSE, # No random recruitment
                        msmMode = 0, # Single species mode
                        verbose = 1,
                        initMode = 1,
                        phase = TRUE)

