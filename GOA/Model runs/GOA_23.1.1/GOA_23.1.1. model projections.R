library(Rceattle)
library(readxl)
library(dplyr)
library(tidyr)
setwd("Model runs/GOA_23.1.1/")
load("Models/GOA_23_1_1_mod_list.RData")
combined_data <- read_data(file = "Data/GOA_23_1_1_data_1977_2023_edited.xlsx")
combined_data$projyr <- 2100

# Ajust inits ----
for(i in 1:length(mod_list_all)){
  mod_list_all[[i]]$estimated_params$rec_dev <- cbind(
    mod_list_all[[i]]$estimated_params$rec_dev, matrix(0, nrow = 3, ncol = 50))
}

# Hindcast ----
# - Est single-species fixed M
ss_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = mod_list_all[[1]]$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            verbose = 1,
                            phase = NULL)

# - Est multi-species
inits <- mod_list_all[[3]]$estimated_params
ms_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # Multi species mode
                            verbose = 1,
                            niter = 3,
                            meanyr = 2018,
                            phase = NULL,
                            M1Fun = build_M1(M1_model = c(1,2,1),
                                             M1_use_prior = FALSE,
                                             M2_use_prior = FALSE))

# Adjust f prop ----
mod_list_all <- list(ss_mod, ms_mod)

for(i in 1:2){
  avg_F <- (exp(mod_list_all[[i]]$estimated_params$ln_mean_F+mod_list_all[[i]]$estimated_params$F_dev)) # Average F from last 2 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
  f_ratio <- avg_F[14:16]
  f_ratio <- f_ratio/sum(f_ratio)
  
  # Adjust future F proportion to each fleet
  mod_list_all[[i]]$data_list$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio, 0, 0)
  mod_list_all[[i]]$estimated_params$proj_F_prop <- mod_list_all[[i]]$data_list$fleet_control$proj_F_prop
}


ss_mod <- mod_list_all[[1]]
ms_mod <- mod_list_all[[2]]



# Projections ----
# - Project single-species fixed M
ss_mod_tier3 <- Rceattle::fit_mod(data_list = ss_mod$data_list,
                            inits = ss_mod$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            HCR = build_hcr(HCR = 5, # Tier3 HCR
                                            FsprTarget = 0.4, # F40%
                                            FsprLimit = 0.35, # F35%
                                            Plimit = c(0.2, 0.2, 0), # No fishing when SB<SB20
                                            Alpha = 0.05),
                            verbose = 1,
                            phase = NULL)

# - Project multi-species
ms_mod_tier3 <- Rceattle::fit_mod(data_list = ms_mod$data_list,
                            inits = ms_mod$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # Multi species mode
                            verbose = 1,
                            niter = 3,
                            meanyr = 2018,
                            phase = NULL,
                            M1Fun = build_M1(M1_model = c(1,2,1),
                                             M1_use_prior = FALSE,
                                             M2_use_prior = FALSE),
                            HCR = build_hcr(HCR = 3, # Constant F HCR
                                            DynamicHCR = FALSE, # Use dynamic reference points
                                            FsprTarget = 0.4))


# Plot ----
model_names <- c("SS no F", "MS no F", "SS Tier 3", "MS Tier 3")
proj_list_all <- list(ss_mod, ms_mod, ss_mod_tier3, ms_mod_tier3)
plot_biomass(proj_list_all, incl_proj = TRUE, model_names = model_names, file = "proj")
plot_b_eaten(proj_list_all, incl_proj = TRUE, model_names = model_names, file = "proj")
plot_recruitment(proj_list_all, incl_proj = TRUE, model_names = model_names, file = "proj")
plot_catch(proj_list_all, incl_proj = TRUE, model_names = model_names, file = "proj")


# Save ----
# - Model
save(proj_list_all, file = "Models/GOA_23_mod_projections.RData")

# - Catch
catch_list <- list()
catch_list[["SS"]] <- ss_mod_tier3$data_list$fsh_biom
catch_list[["SS"]]$Projected_catch <- ss_mod_tier3$quantities$fsh_bio_hat
catch_list[["SS"]] <- catch_list[["SS"]] %>%
  select(-Fleet_code, - Species, - Month, - Selectivity_block, -Log_sd) %>%
  pivot_wider(names_from = Fleet_name, values_from = c(Catch, Projected_catch)) %>%
  as.data.frame()

catch_list <- list()
catch_list[["MS"]] <- ss_mod_tier3$data_list$fsh_biom
catch_list[["MS"]]$Projected_catch <- ss_mod_tier3$quantities$fsh_bio_hat
catch_list[["MS"]] <- catch_list[["MS"]] %>%
  select(-Fleet_code, - Species, - Month, - Selectivity_block, -Log_sd) %>%
  pivot_wider(names_from = Fleet_name, values_from = c(Catch, Projected_catch)) %>%
  as.data.frame()
