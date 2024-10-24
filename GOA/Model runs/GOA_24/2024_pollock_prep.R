library(Rceattle)
library(dplyr)
library(tidyr)
setwd("Model runs/GOA_24")


pollock23 <- read_data("Data/GOA_23_pollock_single_species_1970-2023.xlsx")
safe24 <- readRDS("Data/SAFE data/fit.RDS")

# Env data
pollock23$env_data <- data.frame(Year = safe24$input$dat$Ecov_obs_year,
                                 QcovPol = safe24$input$dat$Ecov_obs)

# Comp (same number in CEATTLE)
colnames(safe24$input$dat$srvp1) <- paste0("Age",1:10) # 1
colnames(safe24$input$dat$srvp2) <- paste0("Age",1:10) # 2
colnames(safe24$input$dat$srvp3) <- paste0("Age",1:10) # 3
colnames(safe24$input$dat$srvp6) <- paste0("Age",1:10) # 6
colnames(safe24$input$dat$catp) <- paste0("Age",1:10) # 8

fsh_acomp <- cbind(data.frame(Year = safe24$input$dat$fshyrs, Fleet_code = 8, Age0_Length1 = 0),
                    safe24$input$dat$catp
)

srv1_acomp <- cbind(data.frame(Year = safe24$input$dat$srv_acyrs1, Fleet_code = 1, Age0_Length1 = 0),
                     safe24$input$dat$srvp1
)

srv2_acomp <- cbind(data.frame(Year = safe24$input$dat$srv_acyrs2, Fleet_code = 2, Age0_Length1 = 0),
                     safe24$input$dat$srvp2
)

srv3_acomp <- cbind(data.frame(Year = safe24$input$dat$srv_acyrs3, Fleet_code = 3, Age0_Length1 = 0),
                     safe24$input$dat$srvp3
)

srv6_acomp <- cbind(data.frame(Year = safe24$input$dat$srv_acyrs6, Fleet_code = 6, Age0_Length1 = 0),
                     safe24$input$dat$srvp6
)


comp_data <- comp_data %>%
  select(Fleet_name, Fleet_code, Species, Sex, Age0_Length1, Year, Month, Sample_size) %>%
  full_join(do.call("rbind", list(srv1_acomp, srv2_acomp, srv3_acomp, srv6_acomp, fsh_acomp))) %>%
  arrange(Fleet_code, Year)

pollock23$comp_data <- comp_data


# WT
colnames(safe24$input$dat$wt_srv1) <- paste0("Age",1:10) # 2
colnames(safe24$input$dat$wt_srv2) <- paste0("Age",1:10) # 3
colnames(safe24$input$dat$wt_srv3) <- paste0("Age",1:10) # 4
colnames(safe24$input$dat$wt_srv6) <- paste0("Age",1:10) # 5
colnames(safe24$input$dat$wt_fsh) <- paste0("Age",1:10) # 1

fsh_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 1),
                    safe24$input$dat$wt_fsh
)

srv1_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 2),
                    safe24$input$dat$wt_srv1
)

srv2_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 3),
                     safe24$input$dat$wt_srv2
)

srv3_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 4),
                     safe24$input$dat$wt_srv3
)

srv6_weight <- cbind(data.frame(Year = 1970:2024, Wt_index = 5),
                     safe24$input$dat$wt_srv6
)

wt <- pollock23$wt
wt <- wt %>%
  select(Wt_name, Wt_index, Species, Sex, Year) %>%
  left_join(do.call("rbind", list(fsh_weight, srv1_weight, srv2_weight, srv3_weight, srv6_weight)))

pollock23$wt <- wt
