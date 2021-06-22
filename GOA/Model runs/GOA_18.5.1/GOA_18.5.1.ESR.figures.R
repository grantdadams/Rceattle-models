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
plot_b_eaten(mod_avg, file = file_name, add_ci = FALSE, model_names = NULL, right_adj = 0, line_col = 1, lwd = 2, species = 1:3, incl_mean = TRUE)
plot_b_eaten_prop(mod_avg, file = file_name, add_ci = FALSE, model_names = NULL, right_adj = 0, line_col = 1, lwd = 2, species = 1:3, incl_mean = FALSE)
plot_m_at_age(Rceattle = mod_avg, age = 1, file = file_name, add_ci = FALSE, model_names = NULL, right_adj = 0, line_col = 1, lwd = 2, species = 1:3, incl_mean = TRUE)
plot_m2_at_age_prop(Rceattle = mod_avg, age = 1, file = file_name, add_ci = FALSE, model_names = NULL, right_adj = 0, line_col = 1, lwd = 2, species = 1:3, incl_mean = FALSE)
