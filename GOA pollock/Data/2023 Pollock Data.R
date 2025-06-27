# Load data ----
pollock23 <- read_data("Data/GOA_23_pollock_single_species_1970-2023.xlsx")
pollock23$index_data$Observation <- pollock23$index_data$Observation * 1e6
pollock23$fleet_control$Q_sd_prior[2] = 0.1
pollock23$fleet_control$Q_prior[2] <- 0.85

pollock23$fleet_control <- pollock23$fleet_control %>%
  select(-c(Accumatation_age_upper, Accumatation_age_lower)) %>%
  mutate(Comp_loglike = -1,
         Age_max_selected = c(3, 10, 10, NA, NA, 1, NA, 7))

pollock23$catch_data$Catch <- fit$obj$env$data$cattot

# * Expand survey data ----
index_expanded <- pollock23$index_data %>%
  dplyr::distinct(Fleet_name, Fleet_code, Species, Month) %>%
  dplyr::cross_join(data.frame(Year = pollock23$styr:pollock23$endyr))

index_data <- pollock23$index_data %>%
  dplyr::full_join(index_expanded) %>%
  dplyr::arrange(Fleet_code, Year) %>%
  dplyr::mutate(Selectivity_block = 1,
                Q_block = 1,
                Year = ifelse(is.na(Observation), - Year, Year),
                Observation = ifelse(is.na(Observation), NA, Observation),
                Log_sd = ifelse(is.na(Log_sd), NA, Log_sd))
pollock23$index_data <- index_data


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

write_data(pollock23, file = "Data/Pollock_2023.xlsx")
