library(Rceattle)
library(wesanderson)
library(ggplot2)
library(readxl)

# Load models
setwd("Model runs/GOA_21.1.1/")
load("Models/18_5_1_Niter3_2021-06-14.RData")


# Set up model list
# The long time-series models for 1977 to 2021 were: 
#   •	Model 1: a model that did not include predation (single-species models) representing a base model. 
# •	Models 2-4: models that included pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide long-time (1917-2018) series model developed by the IPHC. To account for a lack of information on halibut distribution prior to 1993, numbers-at-age prior to 1993 and after 2018 were multiplied by the 50th (model 3), 15th (model 4), and 85th (model 5) quantiles of the distribution of adult halibut in area 3 between 1993 and 2018. 
# •	Models 5-7: as for models 2-4 but using numbers-at-age of Pacific halibut from the areas-as-fleets long-time series model. 

# The three short term models for 1996 to 2018 were: 
#   •	Model 8: a model that did not include predation (single-species models) representing a base model. 
# •	Models 9-11: models that included pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide short-time (1996-2018) series model developed by the IPHC. To account for a lack of information on halibut distribution prior to 1993, numbers-at-age after 2018 were multiplied by the 50th (model 3), 15th (model 4), and 85th (model 5) quantiles of the distribution of adult halibut in area 3 between 1993 and 2018. 
# •	Models 12-14: as for models 9-11 but using numbers-at-age of Pacific halibut from the areas-as-fleets short-time series model. 


mod_names <- c("3. MS-Coast long avg", "4. MS-Coast long low", "5. MS-Coast long high", "6. MS-AAF long avg", "7. MS-AAF long low", "8. MS-AAF long high")
mod_list <- mod_list_all[c(3:5)]

mod_avg <- mod_list_all[[6]]
# for(i in 1:length(mod_avg$quantities)){
#   mod_avg$quantities[[i]] <- mod_avg$quantities[[i]]/length(mod_list)
#   for(mod in 2:length(mod_list)){
#     mod_avg$quantities[[i]] <- mod_avg$quantities[[i]] + mod_list[[mod]]$quantities[[i]]/length(mod_list)
#   }
# }

file_name <- "Results/ESR/21.1.1_model_avg"
plot_b_eaten(mod_avg, file = file_name, add_ci = FALSE, model_names = NULL, right_adj = 0, line_col = 1, lwd = 2, species = 1:3, incl_mean = TRUE)
plot_b_eaten(mod_avg, file = file_name, add_ci = FALSE, model_names = NULL, right_adj = 0, line_col = 1, lwd = 2, species = 1:3, incl_mean = TRUE)
plot_b_eaten_prop(mod_avg, file = file_name, add_ci = FALSE, model_names = NULL, right_adj = 0, line_col = 1, lwd = 2, species = 1:3, incl_mean = FALSE)
plot_m_at_age(Rceattle = mod_avg, age = 1, file = file_name, add_ci = FALSE, model_names = NULL, right_adj = 0, line_col = 1, lwd = 2, species = 1:3, incl_mean = TRUE)
plot_m2_at_age_prop(Rceattle = mod_avg, age = 1, file = file_name, add_ci = FALSE, model_names = NULL, right_adj = 0.23, line_col = 1, lwd = 2, species = 1:3, incl_mean = FALSE)

# Mean M1
round(mean(mod_avg$quantities$M[1,1,1,]),3)
round(mean(mod_avg$quantities$M[2,1,1,]),3)
round(mean(mod_avg$quantities$M[2,2,1,]),3)
round(mean(mod_avg$quantities$M[3,1,1,]),3)

# Age - 1
round(mean(mod_avg$quantities$B_eaten[1,1,1,]),0)
round(mean(mod_avg$quantities$B_eaten[2,1,1,] + mod_avg$quantities$B_eaten[2,2,1,]),0)
round(mean(mod_avg$quantities$B_eaten[2,2,1,]),0)
round(mean(mod_avg$quantities$B_eaten[3,1,1,]),0)


c(round(mean(rowSums(mod_avg$quantities$B_eaten[1,1,,])),0),
  round(mean(rowSums(mod_avg$quantities$B_eaten[2,1,,]+mod_avg$quantities$B_eaten[2,2,,])),0),
  round(mean(rowSums(mod_avg$quantities$B_eaten[3,1,,])),0))


table1 <- data.frame(
  Year = mod_avg$data_list$styr:mod_avg$data_list$projyr,
  PollockM2 = round((mod_avg$quantities$M2[1,1,1,]),3),
                     PollockZ = round((mod_avg$quantities$Zed[1,1,1,]),3),
                     ATFFM2 = round((mod_avg$quantities$M2[2,1,1,]),3),
                     ATFFZ = round((mod_avg$quantities$Zed[2,1,1,]),3),
                     AFTMM2 = round((mod_avg$quantities$M2[2,2,1,]),3),
                     AFTMZ = round((mod_avg$quantities$Zed[2,2,1,]),3),
                     CODM2 = round((mod_avg$quantities$M2[3,1,1,]),3),
                     CODZ = round((mod_avg$quantities$Zed[3,1,1,]),3))
write.csv(table1, file = paste0(file_name, "age-1_zed.csv"))

# M1
c(round(mean((mod_avg$quantities$M1[1,1,1])),3),
  round(mean((mod_avg$quantities$M1[2,1,1])),3),
  round(mean((mod_avg$quantities$M1[2,2,1])),3),
  round(mean((mod_avg$quantities$M1[3,1,1])),3))


# Ration
persp(x = 1:10, y = 1:45, z = mod_avg$quantities$ration2Age[1,1,1:10,1:45], theta = 220, main = "Pollock")
persp(x = 1:21, y = 1:45, z = mod_avg$quantities$ration2Age[2,1,1:21,1:45], theta = 220, main = "ATF F")
persp(x = 1:21, y = 1:45, z = mod_avg$quantities$ration2Age[2,2,1:21,1:45], theta = 220, main = "ATF M")
persp(x = 1:10, y = 1:45, z = mod_avg$quantities$ration2Age[3,1,1:10,1:45], theta = 220, main = "Cod")
persp(x = 1:30, y = 1:45, z = mod_avg$quantities$ration2Age[4,1,1:30,1:45], theta = 220, main = "H F")
persp(x = 1:30, y = 1:45, z = mod_avg$quantities$ration2Age[4,2,1:30,1:45], theta = 220, main = "H M")



# N-at-age
# - Pollock
par(mfrow = c(1,3), mar = c(0,1,1,0))
persp(x = 1:10, y = 1977:2021, z = mod_avg$quantities$NByage[1,1,1:10,1:45]/1000000, theta = 130, main = "Pollock N-at-age", zlab = "Numbers (millions)", xlab = "Age")
persp(x = 1:10, y = 1977:2021, z = mod_avg$quantities$M[1,1,1:10,1:45], theta = 130, main = "Pollock M1 + M2", zlab = "M yr^-1", xlab = "Age")
persp(x = 1:10, y = 1977:2021, z = mod_avg$quantities$B_eaten[1,1,1:10,1:45], theta = 130, main = "Pollock consumed", zlab = "Biomass consumed (million mt)", xlab = "Age")

# - Cod
par(mfrow = c(1,3), mar = c(0,1,1,0))
persp(x = 1:10, y = 1977:2021, z = mod_avg$quantities$NByage[3,1,1:10,1:45]/1000000, theta = 130, main = "Cod N-at-age", zlab = "Numbers (millions)", xlab = "Age")
persp(x = 1:10, y = 1977:2021, z = mod_avg$quantities$M[3,1,1:10,1:45], theta = 130, main = "Cod M1 + M2", zlab = "M yr^-1", xlab = "Age")
persp(x = 1:10, y = 1977:2021, z = mod_avg$quantities$B_eaten[3,1,1:10,1:45], theta = 130, main = "Cod consumed", zlab = "Biomass consumed (million mt)", xlab = "Age")

# - ATF
par(mfrow = c(2,3), mar = c(0,1,1,0))
# -- Females
persp(x = 1:21, y = 1977:2021, z = mod_avg$quantities$NByage[2, 1,1:21,1:45]/1000000, theta = 130, main = "ATF Females N-at-age", zlab = "Numbers (millions)", xlab = "Age")
persp(x = 1:21, y = 1977:2021, z = mod_avg$quantities$M[2, 1,1:21,1:45], theta = 130, main = "ATF Females M1 + M2", zlab = "M yr^-1", xlab = "Age")
persp(x = 1:21, y = 1977:2021, z = mod_avg$quantities$B_eaten[2, 1,1:21,1:45], theta = 130, main = "ATF Females consumed", zlab = "Biomass consumed (million mt)", xlab = "Age")

# -- Males
persp(x = 1:21, y = 1977:2021, z = mod_avg$quantities$NByage[2, 2,1:21,1:45]/1000000, theta = 130, main = "ATF Males N-at-age", zlab = "Numbers (millions)", xlab = "Age")
persp(x = 1:21, y = 1977:2021, z = mod_avg$quantities$M[2, 2,1:21,1:45], theta = 130, main = "ATF Males M1 + M2", zlab = "M yr^-1", xlab = "Age")
persp(x = 1:21, y = 1977:2021, z = mod_avg$quantities$B_eaten[2, 2,1:21,1:45], theta = 130, main = "ATF Males consumed", zlab = "Biomass consumed (million mt)", xlab = "Age")
