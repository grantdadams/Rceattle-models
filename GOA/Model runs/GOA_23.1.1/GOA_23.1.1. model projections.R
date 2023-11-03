pacman::p_load(Rceattle, readxl, dplyr, tidyr)
setwd("Model runs/GOA_23.1.1/")
load("Models/GOA_23_1_1_mod_list.RData")
combined_data <- read_data(file = "Data/GOA_23_1_1_data_1977_2023_edited.xlsx")
combined_data$projyr <- 2100

# Ajust inits ----
for(i in 1:length(mod_list_all)){
  mod_list_all[[i]]$estimated_params$rec_dev <- cbind(
    mod_list_all[[i]]$estimated_params$rec_dev, matrix(0, nrow = 3, ncol = 50))
}


# Climate data ----
climate_data <- read.csv("Data/GOA_NEP_ROMZ_avg_temp_610_to_630_FebApril_300m.csv")
climate_data <- climate_data %>%
  filter(depthclass == "Bottom", hind == "yes") %>%
  pivot_wider(names_from = simulation, values_from = mean_value_dc_610_to_630) %>%
  select(year, ssp126, ssp585) %>%
  rename(Year = year)

climate_sub <- data.frame(Year = 1977:1979, 
           ssp126 = mean(climate_data$ssp126[1:10]), 
           ssp585 = mean(climate_data$ssp585[1:10]))

climate_data <- rbind(climate_sub, climate_data)


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

# -- SSP126
ssp_dat <- ss_mod$data_list
ssp_dat$env_data <- climate_data
ss_mod$estimated_params$beta_rec_pars <- matrix(0, 3, 1)
ss_mod_ssp126 <- Rceattle::fit_mod(data_list = ssp_dat,
                            inits = ss_mod$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            recFun = build_srr(srr_fun = 1,
                                               srr_env_indices = 1),
                            msmMode = 0, # Single species mode
                            verbose = 1,
                            phase = NULL)

# -- SSP526
ss_mod_ssp526 <- Rceattle::fit_mod(data_list = ssp_dat,
                                   inits = ss_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   recFun = build_srr(srr_fun = 1,
                                                      srr_env_indices = 2),
                                   msmMode = 0, # Single species mode
                                   verbose = 1,
                                   phase = NULL)



# - Est multi-species
inits <- mod_list_all[[3]]$estimated_params
ms_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = mod_list_all[[3]]$estimated_params, # Initial parameters = 0
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

# -- SSP126
ms_mod$estimated_params$beta_rec_pars <- matrix(0, 3, 1)
ms_mod_ssp126 <- Rceattle::fit_mod(data_list = ssp_dat,
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
                                   recFun = build_srr(srr_fun = 1,
                                                      srr_env_indices = 1))

# -- SSP526
ms_mod_ssp526 <- Rceattle::fit_mod(data_list = ssp_dat,
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
                                   recFun = build_srr(srr_fun = 1,
                                                      srr_env_indices = 2))


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
plot_biomass(proj_list_all, incl_proj = TRUE, model_names = model_names, file = "Results/Projections/proj")
plot_ssb(proj_list_all, incl_proj = TRUE, model_names = model_names, file = "Results/Projections/proj")
plot_b_eaten(proj_list_all, incl_proj = TRUE, model_names = model_names, file = "Results/Projections/proj")
plot_recruitment(proj_list_all, incl_proj = TRUE, model_names = model_names, file = "Results/Projections/proj")
plot_catch(proj_list_all, incl_proj = TRUE, model_names = model_names, file = "Results/Projections/proj")


# Save ----
# - Model
save(proj_list_all, file = "Models/GOA_23_mod_projections.RData")
load("Models/GOA_23_mod_projections.RData")
ss_mod <- proj_list_all[[1]]
ms_mod <- proj_list_all[[2]]
ss_mod_tier3 <- proj_list_all[[3]]
ms_mod_tier3 <- proj_list_all[[3]]

# - Catch
catch_list <- list()
catch_list[["SS"]] <- ss_mod_tier3$data_list$fsh_biom
catch_list[["SS"]]$Projected_catch <- ss_mod_tier3$quantities$fsh_bio_hat
catch_list[["SS"]] <- catch_list[["SS"]] %>%
  select(-Fleet_code, - Species, - Month, - Selectivity_block, -Log_sd) %>%
  pivot_wider(names_from = Fleet_name, values_from = c(Catch, Projected_catch)) %>%
  as.data.frame()

catch_list[["MS"]] <- ms_mod_tier3$data_list$fsh_biom
catch_list[["MS"]]$Projected_catch <- ms_mod_tier3$quantities$fsh_bio_hat
catch_list[["MS"]] <- catch_list[["MS"]] %>%
  select(-Fleet_code, - Species, - Month, - Selectivity_block, -Log_sd) %>%
  pivot_wider(names_from = Fleet_name, values_from = c(Catch, Projected_catch)) %>%
  as.data.frame()
writexl::write_xlsx(catch_list, path = "projected_catches.xlsx")
