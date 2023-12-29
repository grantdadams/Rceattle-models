pacman::p_load(Rceattle, readxl, dplyr, tidyr, nmfspalette)
setwd("Model runs/GOA_23.1.1/")
load("Models/GOA_23_1_1_mod_list.RData")
combined_data <- read_data(file = "Data/GOA_23_1_1_data_1977_2023_edited.xlsx")
combined_data$projyr <- 2100


## Ajust inits ----
for(i in 1:length(mod_list_all)){
  mod_list_all[[i]]$estimated_params$rec_dev <- cbind(
    mod_list_all[[i]]$estimated_params$rec_dev, matrix(0, nrow = 3, ncol = 50))
  
  mod_list_all[[i]]$estimated_params$beta_rec_pars <- matrix(0, 3, 1)
}


## Climate data ----
temp_data <- read.csv("Data/goa_temp_610_to_630_winter_300M.csv") %>%
  filter(depthclass == "Bottom", hind == "yes")  %>%
  mutate(varname = "temp") %>%
  rename(value = mean_value_dc_610_to_630)

zoo_data <- read.csv("Data/goa_large_zoo_610_to_630_fall_300M.csv") %>%
  filter(depthclass == "Surface", hind == "yes") %>%
  mutate(varname = "mzl") %>%
  rename(value = mean_value_dc_610_to_630)

climate_data <- rbind(temp_data, zoo_data) %>%
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

temp_data <- climate_data %>%
  filter(varname == "temp") %>%
  select(-varname)

zoo_data <- climate_data %>%
  filter(varname == "mzl") %>%
  select(-varname)

# - Add missing years
temp_sub <- data.frame(Year = 1977:1979, 
                          value_ssp126 = mean(temp_data$value_ssp126[1:10]), 
                          value_ssp245 = mean(temp_data$value_ssp245[1:10]),
                          value_ssp585 = mean(temp_data$value_ssp585[1:10]), 
                          value_squared_ssp126 = mean(temp_data$value_squared_ssp126[1:10]), 
                          value_squared_ssp245 = mean(temp_data$value_squared_ssp245[1:10]),
                          value_squared_ssp585 = mean(temp_data$value_squared_ssp585[1:10]), 
                          value_ssp126z = 0,
                          value_ssp245z = 0,
                          value_ssp585z = 0,
                          value_squared_ssp126z = 0,
                          value_squared_ssp245z = 0,
                          value_squared_ssp585z = 0)

temp_data <- rbind(temp_sub, temp_data) 
colnames(temp_data) <- c("Year", paste0("BT_", colnames(zoo_data)[2:ncol(zoo_data)]))

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

climate_data <- merge(temp_data, zoo_data, by = "Year") %>%
  arrange(Year)

# - add to Rceattle object
combined_data$fleet_control$Fleet_type[18] <- 0
ssp_dat_126 <- ssp_dat_245 <- ssp_dat_585 <- combined_data

ssp_dat_126$env_data <- climate_data %>%
  select(Year, BT_value_ssp126, BT_value_ssp126z, BT_value_squared_ssp126z, MZL_value_ssp126z )

ssp_dat_245$env_data <- climate_data %>%
  select(Year, BT_value_ssp245, BT_value_ssp245z, BT_value_squared_ssp245z, MZL_value_ssp245z )

ssp_dat_585$env_data <- climate_data %>%
  select(Year, BT_value_ssp585, BT_value_ssp585z, BT_value_squared_ssp585z, MZL_value_ssp585z )


## Hindcast (climate naive) ----
# - Est single-species fixed M
ss_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = mod_list_all[[1]]$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            verbose = 1,
                            phase = NULL)

# - Est single-species estimated M
ss_mod_M <- Rceattle::fit_mod(data_list = combined_data,
                              inits = mod_list_all[[2]]$estimated_params, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 0, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              verbose = 1,
                              phase = NULL,
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
                            verbose = 1,
                            niter = 5,
                            meanyr = 2018,
                            phase = NULL,
                            M1Fun = build_M1(M1_model = c(1,2,1),
                                             M1_use_prior = FALSE,
                                             M2_use_prior = FALSE))

## Climate projections ----
# * Single species ----
# -- SSP126
ss_mod_ssp126 <- Rceattle::fit_mod(data_list = ssp_dat_126,
                                   inits = ss_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   recFun = build_srr(srr_fun = 1,
                                                      srr_env_indices = c(2,3,4)),
                                   msmMode = 0, # Single species mode
                                   verbose = 1,
                                   phase = NULL)

# -- SSP245
ss_mod_ssp245 <- Rceattle::fit_mod(data_list = ssp_dat_245,
                                   inits = ss_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   recFun = build_srr(srr_fun = 1,
                                                      srr_env_indices = c(2,3,4)),
                                   msmMode = 0, # Single species mode
                                   verbose = 1,
                                   phase = NULL)

# -- SSP585
ss_mod_ssp585 <- Rceattle::fit_mod(data_list = ssp_dat_585,
                                   inits = ss_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   recFun = build_srr(srr_fun = 1,
                                                      srr_env_indices = c(2,3,4)),
                                   msmMode = 0, # Single species mode
                                   verbose = 1,
                                   phase = NULL)




# * Multi-species ----
# -- SSP126
ms_mod_ssp126 <- Rceattle::fit_mod(data_list = ssp_dat_126,
                                   inits = ms_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 1, # Multi species mode
                                   verbose = 1,
                                   niter = 5,
                                   meanyr = 2018,
                                   phase = NULL,
                                   M1Fun = build_M1(M1_model = c(1,2,1),
                                                    M1_use_prior = FALSE,
                                                    M2_use_prior = FALSE),
                                   recFun = build_srr(srr_fun = 1,
                                                      srr_env_indices = c(2,3,4)))

# -- SSP245
ms_mod_ssp245 <- Rceattle::fit_mod(data_list = ssp_dat_245,
                                   inits = ms_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 1, # Multi species mode
                                   verbose = 1,
                                   niter = 5,
                                   meanyr = 2018,
                                   phase = NULL,
                                   M1Fun = build_M1(M1_model = c(1,2,1),
                                                    M1_use_prior = FALSE,
                                                    M2_use_prior = FALSE),
                                   recFun = build_srr(srr_fun = 1,
                                                      srr_env_indices = c(2,3,4)))

# -- SSP585
ms_mod_ssp585 <- Rceattle::fit_mod(data_list = ssp_dat_585,
                                   inits = ms_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 1, # Multi species mode
                                   verbose = 1,
                                   niter = 5,
                                   meanyr = 2018,
                                   phase = NULL,
                                   M1Fun = build_M1(M1_model = c(1,2,1),
                                                    M1_use_prior = FALSE,
                                                    M2_use_prior = FALSE),
                                   recFun = build_srr(srr_fun = 1,
                                                      srr_env_indices = c(2,3,4)))


## Adjust f prop ----
mod_list_all <- list(ss_mod, ss_mod_M, ms_mod, 
                     ss_mod_ssp126, ss_mod_ssp245, ss_mod_ssp585, 
                     ms_mod_ssp126, ms_mod_ssp245, ms_mod_ssp585)

for(i in 1:length(mod_list_all)){
  avg_F <- (exp(mod_list_all[[i]]$estimated_params$ln_mean_F+mod_list_all[[i]]$estimated_params$F_dev)) # Average F from last 2 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
  f_ratio <- avg_F[14:16]
  f_ratio <- f_ratio/sum(f_ratio)
  
  # Adjust future F proportion to each fleet
  mod_list_all[[i]]$data_list$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio, 0, 0)
  mod_list_all[[i]]$estimated_params$proj_F_prop <- mod_list_all[[i]]$data_list$fleet_control$proj_F_prop
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
                                                  FsprTarget = 0.4, # F40%
                                                  FsprLimit = 0.35, # F35%
                                                  Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                                                  Alpha = 0.05),
                                  verbose = 1,
                                  phase = NULL)

# - Est single-species estimated M
ss_mod_M_tier3 <- Rceattle::fit_mod(data_list = ss_mod_M$data_list,
                                    inits = ss_mod_M$estimated_params, # Initial parameters = 0
                                    file = NULL, # Don't save
                                    estimateMode = 0, # Estimate
                                    random_rec = FALSE, # No random recruitment
                                    msmMode = 0, # Single species mode
                                    verbose = 1,
                                    phase = NULL,
                                    M1Fun = build_M1(M1_model = c(1,2,1),
                                                     M1_use_prior = FALSE,
                                                     M2_use_prior = FALSE),
                                    HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                    FsprTarget = 0.4, # F40%
                                                    FsprLimit = 0.35, # F35%
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


## MSE ----
# * OMs ----
om_list <- c(om_list_ss, om_list_ms)
om_names <- paste0(rep(c("SS-", "MS-"), each = 4), model_names)

# * Test env significance
aic_vec <- sapply(om_list, function(x) x$opt$AIC)


# * Management strategies ----
# 1. Single-species fix M
# 2. Single-species estimate M
# Tier 3 HCR
sampling_period <- c(2,2,1,2,2,2,2,1,2,2,1,2,2,1,1,1,1,1)
em_hcr_list <- hcr_list
em_hcr_names <- c("SS_fixM_Tier3_EM", "SS_estM_Tier3_EM")


# - Run the MSE
source("~/GitHub/Rceattle_MSE/R/Functions/Run_full_MSE_function.R", echo=TRUE)
source("~/GitHub/Rceattle/R/11a-mse_run_parallel.R", echo=TRUE)
run_mse(system = "GOA1977", recname = "ConstantR", om_list = om_list, om_names = om_names, em_hcr_list = em_hcr_list, em_hcr_names = em_hcr_names, sampling_period = sampling_period, nsim = 10, cap = c(1, 0.17, 1))


# * Get catch ----
catch_list <- list()
model_names_all <- c(paste0("SS ", model_names), 
                     paste0("MS ", model_names))
for(i in c(4:6, 10:12)){
  catch_list[[model_names_all[i]]] <- proj_list_all[[i]]$data_list$fsh_biom
  
  catch_list[[model_names_all[i]]]$Catch[which(catch_list[[model_names_all[i]]]$Year > 2023)] <- proj_list_all[[i]]$quantities$fsh_bio_hat[which(catch_list[[model_names_all[i]]]$Year > 2023)]
  
  catch_list[[model_names_all[i]]] <- catch_list[[model_names_all[i]]] %>%
    select(-Fleet_code, - Species, - Month, - Selectivity_block, -Log_sd) %>%
    pivot_wider(names_from = Fleet_name, values_from = c(Catch)) %>%
    as.data.frame()
}

writexl::write_xlsx(catch_list, path = "Results/Projections/GOA_CEATTLE_projected_catches.xlsx")
