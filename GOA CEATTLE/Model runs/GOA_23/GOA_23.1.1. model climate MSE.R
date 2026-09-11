pacman::p_load(Rceattle, readxl, dplyr, tidyr, nmfspalette, writexl)
setwd("Model runs/GOA_23.1.1/")
load("Models/GOA_23_1_1_mod_list.RData") # Inits predate the current parameter set: re-run "GOA_23.1.1. fit models.R" first
combined_data <- read_data(file = "Data/GOA_23_1_1_data_1977_2023_edited.xlsx")
# Cod diet at ages 11-12 lies past the cod model's ages 1-10: fold it into age 10.
source("fold_diet_plus_group.R")
combined_data <- fold_diet_plus_group(combined_data)
combined_data$projyr <- 2100


## Ajust inits ----
for(i in 1:length(mod_list_all)){
  mod_list_all[[i]]$estimated_params$rec_dev <- cbind(
    mod_list_all[[i]]$estimated_params$rec_dev, matrix(0, nrow = 3, ncol = 50))
}


## Climate data ----
summer_bt_data <- read.csv("Data/goa_temp_610_to_630_summer_300M.csv") %>%
  filter(depthclass == "Bottom", hind == "yes")  %>%
  mutate(varname = "summer bt") %>%
  rename(value = mean_value_dc_610_to_630)

fall_sst_data <- read.csv("Data/goa_temp_610_to_630_winter_300M.csv") %>%
  filter(depthclass == "Surface", hind == "yes")  %>%
  mutate(varname = "winter sst") %>%
  rename(value = mean_value_dc_610_to_630)

zoo_data <- read.csv("Data/goa_large_zoo_610_to_630_fall_300M.csv") %>%
  filter(depthclass == "Surface", hind == "yes") %>%
  mutate(varname = "mzl") %>%
  rename(value = mean_value_dc_610_to_630)

climate_data <- rbind(summer_bt_data, fall_sst_data, zoo_data) %>%
  mutate(value_squared = value^2) %>%
  pivot_wider(names_from = c(simulation), values_from = c(value, value_squared)) %>%
  select(-depthclass, -hind) %>%
  rename(Year = year) %>%
  group_by(varname) %>%
  mutate(value_ssp126z = scale(value_ssp126 ),
         value_ssp245z = scale(value_ssp245 ),
         value_ssp585z = scale(value_ssp585 ),
         value_squared_ssp126z = scale(value_squared_ssp126 ),
         value_squared_ssp245z = scale(value_squared_ssp245 ),
         value_squared_ssp585z = scale(value_squared_ssp585 )
         ) %>%
  ungroup() %>%
  as.data.frame()

# - Add missing years
# -- Fall SST
summer_bt_data <- climate_data %>%
  filter(varname == "summer bt") %>%
  select(-varname)

temp_sub <- data.frame(Year = 1977:1979, 
                          value_ssp126 = mean(summer_bt_data$value_ssp126[1:10]), 
                          value_ssp245 = mean(summer_bt_data$value_ssp245[1:10]),
                          value_ssp585 = mean(summer_bt_data$value_ssp585[1:10]), 
                          value_squared_ssp126 = mean(summer_bt_data$value_squared_ssp126[1:10]), 
                          value_squared_ssp245 = mean(summer_bt_data$value_squared_ssp245[1:10]),
                          value_squared_ssp585 = mean(summer_bt_data$value_squared_ssp585[1:10]), 
                          value_ssp126z = 0,
                          value_ssp245z = 0,
                          value_ssp585z = 0,
                          value_squared_ssp126z = 0,
                          value_squared_ssp245z = 0,
                          value_squared_ssp585z = 0)

summer_bt_data <- rbind(temp_sub, summer_bt_data) 
colnames(summer_bt_data) <- c("Year", paste0("BT_", colnames(summer_bt_data)[2:ncol(summer_bt_data)]))


# -- Summer BT
fall_sst_data <- climate_data %>%
  filter(varname == "winter sst") %>%
  select(-varname)

temp_sub <- data.frame(Year = 1977:1979, 
                       value_ssp126 = mean(fall_sst_data$value_ssp126[1:10]), 
                       value_ssp245 = mean(fall_sst_data$value_ssp245[1:10]),
                       value_ssp585 = mean(fall_sst_data$value_ssp585[1:10]), 
                       value_squared_ssp126 = mean(fall_sst_data$value_squared_ssp126[1:10]), 
                       value_squared_ssp245 = mean(fall_sst_data$value_squared_ssp245[1:10]),
                       value_squared_ssp585 = mean(fall_sst_data$value_squared_ssp585[1:10]), 
                       value_ssp126z = 0,
                       value_ssp245z = 0,
                       value_ssp585z = 0,
                       value_squared_ssp126z = 0,
                       value_squared_ssp245z = 0,
                       value_squared_ssp585z = 0)

fall_sst_data <- rbind(temp_sub, fall_sst_data) 
colnames(fall_sst_data) <- c("Year", paste0("SST_", colnames(fall_sst_data)[2:ncol(fall_sst_data)]))


# -- Zooplankton
zoo_data <- climate_data %>%
  filter(varname == "mzl") %>%
  select(-varname)

mzl_sub <- data.frame(Year = 1977:1979, 
                       value_ssp126 = mean(zoo_data$value_ssp126[1:10]), 
                       value_ssp245 = mean(zoo_data$value_ssp245[1:10]),
                       value_ssp585 = mean(zoo_data$value_ssp585[1:10]), 
                       value_squared_ssp126 = mean(zoo_data$value_squared_ssp126[1:10]), 
                       value_squared_ssp245 = mean(zoo_data$value_squared_ssp245[1:10]),
                       value_squared_ssp585 = mean(zoo_data$value_squared_ssp585[1:10]), 
                       value_ssp126z = 0,
                       value_ssp245z = 0,
                       value_ssp585z = 0,
                       value_squared_ssp126z = 0,
                       value_squared_ssp245z = 0,
                       value_squared_ssp585z = 0)
zoo_data <- rbind(mzl_sub, zoo_data) 
colnames(zoo_data) <- c("Year", paste0("MZL_", colnames(zoo_data)[2:ncol(zoo_data)]))

# - Combine
climate_data <- fall_sst_data %>% 
  inner_join(zoo_data, by = "Year") %>%
  inner_join(summer_bt_data, by = "Year") %>%
  arrange(Year)


# - add to Rceattle object
# Pcod spawning and seine surveys have an index but no composition data to inform
# their selectivity, so they are excluded (Fleet_type 0 = "Off").
combined_data$fleet_control$Fleet_type[combined_data$fleet_control$Fleet_name %in% c("Pcod_spawn_srv", "Pcod_seine_srv")] <- 0
ssp_dat_126 <- ssp_dat_245 <- ssp_dat_585 <- combined_data

ssp_dat_126$env_data <- climate_data %>%
  select(Year, BT_value_ssp126, SST_value_ssp126z, SST_value_squared_ssp126z, MZL_value_ssp126z )

ssp_dat_245$env_data <- climate_data %>%
  select(Year, BT_value_ssp245, SST_value_ssp245z, SST_value_squared_ssp245z, MZL_value_ssp245z )

ssp_dat_585$env_data <- climate_data %>%
  select(Year, BT_value_ssp585, SST_value_ssp585z, SST_value_squared_ssp585z, MZL_value_ssp585z )

# * Recruitment-environment linkage ----
# srr_env_indices = c(2,3,4) counted env_data columns after Year: winter SST,
# SST squared and zooplankton (z-scores). Rceattle now expresses that log-linear
# effect, one slope per species per covariate, as a linkage on R0;
# scenario-agnostic column names let one spec serve every SSP.
# Caveat: under proj_mean_rec = TRUE (the default), projected years use hindcast mean
# recruitment without this R0 effect (2023 applied it).
env_names <- c("Year", "BT", "SST_z", "SST2_z", "MZL_z")
names(ssp_dat_126$env_data) <- names(ssp_dat_245$env_data) <- names(ssp_dat_585$env_data) <- env_names
rec_env <- linkage_spec(~ SST_z + SST2_z + MZL_z)


## Hindcast (climate naive) ----
# - Est single-species fixed M
ss_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = mod_list_all[[1]]$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            fit_control = fit_control(phase = FALSE, verbose = 1),
                            initMode = 2)

# - Est single-species estimated M
ss_mod_M <- Rceattle::fit_mod(data_list = combined_data,
                              inits = mod_list_all[[2]]$estimated_params, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 0, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              fit_control = fit_control(phase = FALSE, verbose = 1),
                              initMode = 2,
                              M1Fun = build_M1(M1_model = c(1,2,1),
                                               M1_use_prior = FALSE,
                                               M2_use_prior = FALSE))

# - Est multi-species
ms_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = mod_list_all[[3]]$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # Multi species mode
                            fit_control = fit_control(phase = FALSE, verbose = 1),
                            niter = 5,
                            suit_endyr = 2023,
                            initMode = 2,
                            M1Fun = build_M1(M1_model = c(1,2,1),
                                             M1_use_prior = FALSE,
                                             M2_use_prior = FALSE),
                            recFun = build_srr(srr_mse_switchyr = 2023)) # Mean recruitment over 1977-2023

## Climate projections ----
# * Single species ----
# -- SSP126
ss_mod_ssp126 <- Rceattle::fit_mod(data_list = ssp_dat_126,
                                   inits = ss_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   recFun = build_srr(srr_fun = 0,
                                                      linkages = list(R0 = rec_env)),
                                   msmMode = 0, # Single species mode
                                   fit_control = fit_control(phase = FALSE, verbose = 1),
                                   initMode = 2)

# -- SSP245
ss_mod_ssp245 <- Rceattle::fit_mod(data_list = ssp_dat_245,
                                   inits = ss_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   recFun = build_srr(srr_fun = 0,
                                                      linkages = list(R0 = rec_env)),
                                   msmMode = 0, # Single species mode
                                   fit_control = fit_control(phase = FALSE, verbose = 1),
                                   initMode = 2)

# -- SSP585
ss_mod_ssp585 <- Rceattle::fit_mod(data_list = ssp_dat_585,
                                   inits = ss_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   recFun = build_srr(srr_fun = 0,
                                                      linkages = list(R0 = rec_env)),
                                   msmMode = 0, # Single species mode
                                   fit_control = fit_control(phase = FALSE, verbose = 1),
                                   initMode = 2)




# * Multi-species ----
# -- SSP126
ms_mod_ssp126 <- Rceattle::fit_mod(data_list = ssp_dat_126,
                                   inits = ms_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 1, # Multi species mode
                                   fit_control = fit_control(phase = FALSE, verbose = 1),
                                   niter = 5,
                                   suit_endyr = 2023,
                                   initMode = 2,
                                   M1Fun = build_M1(M1_model = c(1,2,1),
                                                    M1_use_prior = FALSE,
                                                    M2_use_prior = FALSE),
                                   recFun = build_srr(srr_fun = 0,
                                                      linkages = list(R0 = rec_env),
                                                      srr_mse_switchyr = 2023))

# -- SSP245
ms_mod_ssp245 <- Rceattle::fit_mod(data_list = ssp_dat_245,
                                   inits = ms_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 1, # Multi species mode
                                   fit_control = fit_control(phase = FALSE, verbose = 1),
                                   niter = 5,
                                   suit_endyr = 2023,
                                   initMode = 2,
                                   M1Fun = build_M1(M1_model = c(1,2,1),
                                                    M1_use_prior = FALSE,
                                                    M2_use_prior = FALSE),
                                   recFun = build_srr(srr_fun = 0,
                                                      linkages = list(R0 = rec_env),
                                                      srr_mse_switchyr = 2023))

# -- SSP585
ms_mod_ssp585 <- Rceattle::fit_mod(data_list = ssp_dat_585,
                                   inits = ms_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 1, # Multi species mode
                                   fit_control = fit_control(phase = FALSE, verbose = 1),
                                   niter = 5,
                                   suit_endyr = 2023,
                                   initMode = 2,
                                   M1Fun = build_M1(M1_model = c(1,2,1),
                                                    M1_use_prior = FALSE,
                                                    M2_use_prior = FALSE),
                                   recFun = build_srr(srr_fun = 0,
                                                      linkages = list(R0 = rec_env),
                                                      srr_mse_switchyr = 2023))


## Adjust f prop ----
mod_list_all <- list(ss_mod, ss_mod_M, ms_mod, 
                     ss_mod_ssp126, ss_mod_ssp245, ss_mod_ssp585, 
                     ms_mod_ssp126, ms_mod_ssp245, ms_mod_ssp585)

for(i in 1:length(mod_list_all)){
  avg_F <- (exp(mod_list_all[[i]]$estimated_params$log_F)) # Average F from last 2 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
  f_ratio <- avg_F[14:16]
  f_ratio <- f_ratio/sum(f_ratio)
  
  # Adjust future F proportion to each fleet
  mod_list_all[[i]]$data_list$fleet_control$Proj_F_proportion <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio, 0, 0)
  mod_list_all[[i]]$estimated_params$proj_F_prop <- mod_list_all[[i]]$data_list$fleet_control$Proj_F_proportion
}

ss_mod <- mod_list_all[[1]]
ss_mod_M <- mod_list_all[[2]]
ms_mod <- mod_list_all[[3]]
ss_mod_ssp126 <- mod_list_all[[4]]
ss_mod_ssp245 <- mod_list_all[[5]]
ss_mod_ssp585 <- mod_list_all[[6]]

ms_mod_ssp126 <- mod_list_all[[7]]
ms_mod_ssp245 <- mod_list_all[[8]]
ms_mod_ssp585 <- mod_list_all[[9]]


## Management strategies ----
# - Single-species fixed M
ss_mod_tier3 <- Rceattle::fit_mod(data_list = ss_mod$data_list,
                                  inits = ss_mod$estimated_params, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                  Ftarget = 0.4, # F40%
                                                  Flimit = 0.35, # F35%
                                                  Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                                                  Alpha = 0.05),
                                  fit_control = fit_control(phase = FALSE, verbose = 1),
                                  initMode = 2)

# - Est single-species estimated M
ss_mod_M_tier3 <- Rceattle::fit_mod(data_list = ss_mod_M$data_list,
                                    inits = ss_mod_M$estimated_params, # Initial parameters = 0
                                    file = NULL, # Don't save
                                    estimateMode = 0, # Estimate
                                    random_rec = FALSE, # No random recruitment
                                    msmMode = 0, # Single species mode
                                    fit_control = fit_control(phase = FALSE, verbose = 1),
                                    initMode = 2,
                                    M1Fun = build_M1(M1_model = c(1,2,1),
                                                     M1_use_prior = FALSE,
                                                     M2_use_prior = FALSE),
                                    HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                    Ftarget = 0.4, # F40%
                                                    Flimit = 0.35, # F35%
                                                    Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                                                    Alpha = 0.05)
  )


# Plot ----
ss_col <- nmfspalette::nmfs_palette("seagrass")(7)[1:4]
ms_col <- nmfspalette::nmfs_palette("oceans")(7)[1:4]
model_names <- c("Climate naive", "SSP-126", "SSP-245", "SSP-585")

hcr_list <- list(ss_mod_tier3, ss_mod_M_tier3)

om_list_ss <- list(ss_mod, ss_mod_ssp126, ss_mod_ssp245, ss_mod_ssp585)
om_list_ms <- list(ms_mod, ms_mod_ssp126, ms_mod_ssp245, ms_mod_ssp585)


plot_biomass(c(om_list_ss, om_list_ms), incl_proj = TRUE, 
             model_names = paste0("SS ", model_names), 
             line_col = c(ss_col, ms_col),
             file = "Results/Projections/proj")

plot_ssb(c(om_list_ms, om_list_ss), incl_proj = TRUE, 
         model_names = paste0("MS ", model_names), 
         line_col = c(ms_col, ss_col),
         file = "Results/Projections/proj")

plot_b_eaten(om_list_ms, incl_proj = TRUE, 
             model_names = paste0("MS ", model_names), 
             line_col = ms_col,
             file = "Results/Projections/proj")

plot_recruitment(c(om_list_ss, om_list_ms), incl_proj = TRUE, 
                 line_col = c(ss_col, ms_col), 
                 file = "Results/Projections/proj")


## Save ----
# - Model
proj_list_all <- c(hcr_list, om_list_ss, om_list_ms)
save(proj_list_all, file = "Models/GOA_23_mod_projections.RData")



load(file = "Models/GOA_23_mod_projections.RData")


ss_mod_tier3 <- proj_list_all[[1]]
ss_mod_M_tier3 <- proj_list_all[[2]]
ss_mod <- proj_list_all[[3]]
ss_mod_ssp126 <- proj_list_all[[4]]
ss_mod_ssp245 <- proj_list_all[[5]]
ss_mod_ssp585 <- proj_list_all[[6]]
ms_mod <- proj_list_all[[7]]
ms_mod_ssp126 <- proj_list_all[[8]]
ms_mod_ssp245 <- proj_list_all[[9]]
ms_mod_ssp585 <- proj_list_all[[10]]

hcr_list <- list(ss_mod_tier3, ss_mod_M_tier3)

om_list_ss <- list(ss_mod, ss_mod_ssp126, ss_mod_ssp245, ss_mod_ssp585)
om_list_ms <- list(ms_mod, ms_mod_ssp126, ms_mod_ssp245, ms_mod_ssp585)

# * OMs ----
om_list <- c(om_list_ss, om_list_ms)
model_names <- c("Climate naive", "SSP-126", "SSP-245", "SSP-585")
om_names <- paste0(rep(c("SS-", "MS-"), each = 4), model_names)

# * Test env significance
aic_vec <- sapply(om_list, function(x) TMBAIC(x$opt))
write.csv(data.frame(model = om_names, AIC = aic_vec), file = "proj_aic.csv")

## MSE ----
# The MSE for these operating models is maintained in ../Climate_MSE
# (entry point R/Climate_MSE_GOA_runs.R), which runs them through Rceattle::run_mse().
