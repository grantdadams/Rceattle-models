library(Rceattle)
library(wesanderson)
library(ggplot2)
library(readxl)

# Load models
setwd("Model runs/GOA_18.5.1")
load("Models/18_5_1_Niter3_2021-06-14.RData")

re_mods <- list.files("Models/Random_effects_models")
for(i in 1:length(re_mods)){
  load(paste0("Models/Random_effects_models/", re_mods[i]))
  mod_no <- as.numeric(gsub('_', '', substr(re_mods[i], 14,15)))
  mod_list_all[[mod_no]] <- mod_re
}


# The long time-series models for 1977 to 2018 
# •	Models 3-5: models that included pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide long-time (1917-2018) series model developed by the IPHC. To account for a lack of information on halibut distribution prior to 1993, numbers-at-age prior to 1993 were multiplied by the 50th (model 3), 15th (model 4), and 85th (model 5) quantiles of the distribution of adult halibut in area 3 between 1993 and 2018. 
# •	Models 6-8: as for models 3-5 but using numbers-at-age of Pacific halibut from the areas-as-fleets long-time series model.



mod_names <- c("3. MS-Coast long avg", "4. MS-Coast long low", "5. MS-Coast long high", "6. MS-AAF long avg", "7. MS-AAF long low", "8. MS-AAF long high")
mod_list <- mod_list_all[c(3:8)]

mod_avg <- mod_list[[1]]
for(i in 1:length(mod_avg$quantities)){
  mod_avg$quantities[[i]] <- mod_avg$quantities[[i]]/length(mod_list)
  for(mod in 2:length(mod_list)){
    mod_avg$quantities[[i]] <- mod_avg$quantities[[i]] + mod_list[[mod]]$quantities[[i]]/length(mod_list)
  }
}

file_name <- "Figures/18.5.1/Model_avg/18.5.1_model_avg"
plot_b_eaten(mod_avg, file = file_name, add_ci = FALSE, model_names = NULL, right_adj = 0, line_col = 1, lwd = 2, species = 3, incl_mean = TRUE)
plot_b_eaten_prop(mod_avg, file = file_name, add_ci = FALSE, model_names = NULL, right_adj = 0, line_col = 1, lwd = 2, species = 3, incl_mean = FALSE)


# Cod M-at-age
for(age in 1:4){
  plot_m_at_age(Rceattle = mod_avg, age = age, file = file_name, add_ci = FALSE, model_names = NULL, right_adj = 0, line_col = 1, lwd = 2, species = 3, incl_mean = TRUE)
  plot_m2_at_age_prop(Rceattle = mod_avg, age = age, file = file_name, add_ci = FALSE, model_names = NULL, right_adj = 0.23, line_col = 1, lwd = 2, species = 3, incl_mean = FALSE)
}


# Table 1
table1 <- data.frame(
  Year = mod_avg$data_list$styr:mod_avg$data_list$projyr,
  Age1M2 = round((mod_avg$quantities$M2[3,1,1,]),3),
  Age1Z = round((mod_avg$quantities$Zed[3,1,1,]),3),
  Age2M2 = round((mod_avg$quantities$M2[3,1,2,]),3),
  Age2Z = round((mod_avg$quantities$Zed[3,1,2,]),3),
  Age3M2 = round((mod_avg$quantities$M2[3,1,3,]),3),
  Age4Z = round((mod_avg$quantities$Zed[3,1,3,]),3),
  Age4M2 = round((mod_avg$quantities$M2[3,1,4,]),3),
  Age4Z = round((mod_avg$quantities$Zed[3,1,4,]),3))
write.csv(table1, file = paste0(file_name, "age-1to4_time_series_zed.csv"))


# Table 2 Average M-at-age
table2 <- data.frame(
  age = 1:4,
  M1 = round((mod_avg$quantities$M1[3,1,1:4]),3),
  M2 = round(rowMeans(mod_avg$quantities$M2[3,1,1:4,1:42]),3),
  M = round(rowMeans(mod_avg$quantities$M[3,1,1:4,1:42]),3),
  Fmort = round(rowMeans(mod_avg$quantities$F_tot[3,1,1:4,1:42]),3),
  Z = round(rowMeans(mod_avg$quantities$Zed[3,1,1:4,1:42]),3))
write.csv(table2, file = paste0(file_name, "age-1to4_zed.csv"))


# M through time
plot_mortality(Rceattle = mod_avg,
               file = file_name,
               incl_proj = FALSE,
               contour = FALSE, spp = 3, maxage = 12, log = FALSE,
               title = "Model average")



######################### 
# Compare with SAFE Models
#########################
# Columns = year, pollock, cod, atf
safe2018biomass <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 1))
safe2018ssb <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 2))
safe2018rec <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 3))

# Assign data to CEATTLE object
Mod_18_SAFE <- mod_list_all[[1]]

# - Cod
Mod_18_SAFE$quantities$biomass[3,1:42] <- t(safe2018biomass[1:42,c(3)])
Mod_18_SAFE$quantities$biomassSSB[3,1:42] <- t(safe2018ssb[1:42,c(3)])


plot_biomass(list(mod_avg, Mod_18_SAFE), file =  paste0(file_name), model_names = c("Model average", "2018 SAFE"), right_adj = 0.27, line_col = NULL, species = c(3))


