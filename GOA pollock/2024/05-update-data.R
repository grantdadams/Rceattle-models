# Uses "master" branch
library(Rceattle)
library(dplyr)
library(tidyr)
library(TMB)

# Load data ----
# Anchor to the model folder so the relative paths resolve.
setwd("~/Documents/GitHub/Rceattle ecosystem/Rceattle-models/GOA pollock")

pollock23 <- read_data("Data/Pollock_2023.xlsx")
load("Data/2024pollock.Rdata")


# Update data ----
# * Controls ----
pollock23$endyr <- 2024
pollock23$fleet_control$Fleet_type[4:5] <- 0 # Turn off age-1 indices


# * Catch ----
catch_data <- pollock23$catch_data
catch_data <- catch_data[1,]
catch_data$Year <- pollock23$endyr
pollock23$catch_data <- rbind(pollock23$catch_data , catch_data)
pollock23$catch_data$Catch <- fit$input$dat$cattot


# * Env data ----
pollock23$env_data <- data.frame(Year = fit$input$dat$Ecov_obs_year,
                                 QcovPol = fit$input$dat$Ecov_obs)

# * Comp ----
# (same number in CEATTLE)
# - Age
colnames(fit$obj$env$data$srvp1) <- paste0("Comp_",1:10) # 1
colnames(fit$obj$env$data$srvp2) <- paste0("Comp_",1:10) # 2
colnames(fit$obj$env$data$srvp3) <- paste0("Comp_",1:10) # 3
colnames(fit$obj$env$data$srvp6) <- paste0("Comp_",1:10) # 6
colnames(fit$obj$env$data$catp) <- paste0("Comp_",1:10) # 8

# - Length
colnames(fit$obj$env$data$srvlenp1) <- paste0("Comp_",1:7) # 1
colnames(fit$obj$env$data$srvlenp2) <- paste0("Comp_",1:7) # 2
colnames(fit$obj$env$data$srvlenp3) <- paste0("Comp_",1:7) # 3
colnames(fit$obj$env$data$srvlenp6) <- paste0("Comp_",1:7) # 6
colnames(fit$obj$env$data$lenp) <- paste0("Comp_",1:8) # 8

fsh_acomp <- cbind(data.frame(Year = fit$obj$env$data$fshyrs,
                              Sample_size = fit$obj$env$data$multN_fsh, # * 2,
                              Month = 0,
                              Fleet_code = 8, Age0_Length1 = 0),
                   fit$obj$env$data$catp
)

fsh_lcomp <- cbind(data.frame(Year = fit$obj$env$data$fshlenyrs,
                              Sample_size = fit$obj$env$data$multNlen_fsh, # * 2,
                              Month = 0,
                              Fleet_code = 8, Age0_Length1 = 1),
                   fit$obj$env$data$lenp
)

srv1_acomp <- cbind(data.frame(Year = fit$obj$env$data$srv_acyrs1,
                               Sample_size = fit$obj$env$data$multN_srv1, # * 2,
                               Month = fit$obj$env$data$yrfrct_srv1[1],
                               Fleet_code = 1, Age0_Length1 = 0),
                    fit$obj$env$data$srvp1
)

srv1_lcomp <- cbind(data.frame(Year = fit$obj$env$data$srv_lenyrs1,
                               Sample_size = fit$obj$env$data$multNlen_srv1, # * 3,
                               Month = fit$obj$env$data$yrfrct_srv1[1],
                               Fleet_code = 1, Age0_Length1 = 1),
                    fit$obj$env$data$srvlenp1
)

srv2_acomp <- cbind(data.frame(Year = fit$obj$env$data$srv_acyrs2,
                               Sample_size = fit$obj$env$data$multN_srv2, # * 2,
                               Month = fit$obj$env$data$yrfrct_srv2[1],
                               Fleet_code = 2, Age0_Length1 = 0),
                    fit$obj$env$data$srvp2
)

srv2_lcomp <- cbind(data.frame(Year = fit$obj$env$data$srv_lenyrs2,
                               Sample_size = fit$obj$env$data$multNlen_srv2, # * 3,
                               Month = fit$obj$env$data$yrfrct_srv2[1],
                               Fleet_code = 2, Age0_Length1 = 1),
                    fit$obj$env$data$srvlenp2
)

srv3_acomp <- cbind(data.frame(Year = fit$obj$env$data$srv_acyrs3,
                               Sample_size = fit$obj$env$data$multN_srv3, # * 2,
                               Month = fit$obj$env$data$yrfrct_srv3[1],
                               Fleet_code = 3, Age0_Length1 = 0),
                    fit$obj$env$data$srvp3
)

srv3_lcomp <- cbind(data.frame(Year = fit$obj$env$data$srv_lenyrs3,
                               Sample_size = fit$obj$env$data$multNlen_srv3, # * 3,
                               Month = fit$obj$env$data$yrfrct_srv3[1],
                               Fleet_code = 3, Age0_Length1 = 1),
                    fit$obj$env$data$srvlenp3
)

srv6_acomp <- cbind(data.frame(Year = fit$obj$env$data$srv_acyrs6,
                               Sample_size = fit$obj$env$data$multN_srv6, # * 3,
                               Month = fit$obj$env$data$yrfrct_srv6[1],
                               Fleet_code = 6, Age0_Length1 = 0),
                    fit$obj$env$data$srvp6
)

srv6_lcomp <- cbind(data.frame(Year = fit$obj$env$data$srv_lenyrs6,
                               Sample_size = fit$obj$env$data$multNlen_srv6, # * 3,
                               Month = fit$obj$env$data$yrfrct_srv6[1],
                               Fleet_code = 6, Age0_Length1 = 1),
                    fit$obj$env$data$srvlenp6
)

comp_info <- pollock23$comp_data %>%
  distinct(Fleet_code, Fleet_name, Species, Sex)

comp_data <- comp_info %>%
  full_join(do.call("bind_rows", list(srv1_acomp, srv2_acomp, srv3_acomp, srv6_acomp, fsh_acomp,
                                      srv1_lcomp, srv2_lcomp, srv3_lcomp, srv6_lcomp, fsh_lcomp))) %>%
  dplyr::select(Fleet_name, Fleet_code, Species, Sex, Age0_Length1, Month, Year, Sample_size, paste0("Comp_", 1:10)) %>%
  arrange(Fleet_code, Age0_Length1, Year)

pollock23$comp_data <- comp_data


# * Index data ----
head(pollock23$index_data)
srv1 <- data.frame(Fleet_code = 1, Year = fit$input$dat$srvyrs1, Observation =
                     fit$input$dat$indxsurv1 * 1e6, Log_sd = fit$input$dat$indxsurv_log_sd1)

srv2 <- data.frame(Fleet_code = 2, Year = fit$input$dat$srvyrs2, Observation =
                     fit$input$dat$indxsurv2 * 1e6, Log_sd = fit$input$dat$indxsurv_log_sd2)

srv3 <- data.frame(Fleet_code = 3, Year = fit$input$dat$srvyrs3, Observation =
                     fit$input$dat$indxsurv3 * 1e6, Log_sd = fit$input$dat$indxsurv_log_sd3)

srv4 <- data.frame(Fleet_code = 4, Year = -fit$input$dat$srvyrs4, Observation =
                     fit$input$dat$indxsurv4 * 1e6, Log_sd = fit$input$dat$indxsurv_log_sd4) # Turned off

srv5 <- data.frame(Fleet_code = 5, Year = -fit$input$dat$srvyrs5, Observation =
                     fit$input$dat$indxsurv5 * 1e6, Log_sd = fit$input$dat$indxsurv_log_sd5) # Turned off

srv6 <- data.frame(Fleet_code = 6, Year = fit$input$dat$srvyrs6, Observation =
                     fit$input$dat$indxsurv6 * 1e6, Log_sd = fit$input$dat$indxsurv_log_sd6)

index_data <- pollock23$index_data %>%
  distinct(Fleet_name, Fleet_code, Species, Selectivity_block, Q_block, Month) %>%
  left_join(do.call("rbind", list(srv1, srv2, srv3, srv4, srv5, srv6))) %>%
  dplyr::filter(Fleet_code %in% c(1:6))

pollock23$index_data <- index_data


# * WT ----
colnames(fit$input$dat$wt_srv1) <- paste0("Age",1:10) # 2
colnames(fit$input$dat$wt_srv2) <- paste0("Age",1:10) # 3
colnames(fit$input$dat$wt_srv3) <- paste0("Age",1:10) # 4
colnames(fit$input$dat$wt_srv6) <- paste0("Age",1:10) # 5
colnames(fit$input$dat$wt_fsh) <- paste0("Age",1:10) # 1

fsh_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 1),
                    fit$input$dat$wt_fsh
)

srv1_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 2),
                     fit$input$dat$wt_srv1
)

srv2_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 3),
                     fit$input$dat$wt_srv2
)

srv3_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 4),
                     fit$input$dat$wt_srv3
)

srv6_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 5),
                     fit$input$dat$wt_srv6
)


wt <- pollock23$weight %>%
  group_by(Wt_index) %>%
  slice(1) %>%
  select(Wt_name, Wt_index, Species, Sex) %>%
  full_join(do.call("rbind", list(fsh_weight, srv1_weight, srv2_weight, srv3_weight, srv6_weight))) %>%
  as.data.frame()

pollock23$weight <- wt


# * Pyrs ----
tail(pollock23$Pyrs)
pyrs_new <- data.frame(Species = 1, Sex = 0, Year = (max(pollock23$Pyrs$Year)+1):2024)
pyrs_new <- pyrs_new %>% cbind(pollock23$Pyrs %>%
                                 dplyr::slice(n()) %>%
                                 dplyr::select(paste0("Age",1:10)))

pollock23$Pyrs <- rbind(pollock23$Pyrs %>%
                          dplyr::select(Species, Sex, Year, paste0("Age",1:10)),
                        pyrs_new)

# Fit dirichlet model ----
pollock23$fleet_control$Comp_distribution <- 1
pollock23$fleet_control$Catchability[1] <- 6
pollock23$fleet_control$Time_varying_q[1] <- 1

# * Sel normalization age ----
pollock23$fleet_control$Sel_norm_bin[7] <- 3
pollock23$fleet_control$Sel_norm_bin[8] <- 7



write_data(pollock23, "Data/GOA_24_pollock_single_species_1970-2024.xlsx")
