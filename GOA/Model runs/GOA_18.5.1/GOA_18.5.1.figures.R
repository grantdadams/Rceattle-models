library(Rceattle)
library(wesanderson)
library(ggplot2)


setwd("Model runs/GOA_18.5.1")
load("Models/18_5_1_2021-04-02.RData")

# SAFE Models
library(readxl)
safe2018biomass <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 1))
safe2018ssb <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 2))
safe2018rec <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 3))
mod_list_all[[14]] <- mod_list_all[[1]]
mod_list_all[[14]]$quantities$biomass[1:3,1:42] <- t(safe2018biomass[,2:4]) * 100
mod_list_all[[14]]$quantities$biomassSSB[1:3,1:42] <- t(safe2018ssb[,2:4]) * 100
mod_list_all[[14]]$quantities$R[1:3,1:42] <- t(safe2018rec[,2:4]) * 1000000

# Check convergence
abs(sapply(mod_list_all[1:15], function(x) x$opt$objective) - sapply(mod_list_all[1:15], function(x) x$quantities$jnll)>1)
sapply(mod_list_all[1:15], function(x) x$sdrep$sd[1])

# The long time-series models for 1977 to 2018 were: 
#   •	Model 1: a model that did not include predation (single-species models) representing a base model. 
# •	Model 2: a model that did not include halibut predation to allow comparisons in which halibut does not impact the dynamics of groundfish in the GOA. 
# •	Models 3-5: models that included pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide long-time (1917-2018) series model developed by the IPHC. To account for a lack of information on halibut distribution prior to 1993, numbers-at-age prior to 1993 were multiplied by the 50th (model 3), 15th (model 4), and 85th (model 5) quantiles of the distribution of adult halibut in area 3 between 1993 and 2018. 
# •	Models 6-8: as for models 3-5 but using numbers-at-age of Pacific halibut from the areas-as-fleets long-time series model. 

# The three moderate term models for 1993 to 2018 were: 
#   •	Model 9: a model that does not include predation (model 9) to represent a base single-species model 
# •	Model 10: a mutlispecies model that did not include halibut predation (model 10). 
# •	Model 11: a mutlispecies model with relative abundance-at-age of Pacific halibut in area 3 multiplied by an estimated parameter to allow the model to estimate the relative contribution of Pacific halibut predation to describing the dynamics of pollock, Pacific cod, and arrowtooth flounder. 
# The four short term models for 1996 to 2018 were: 
#   •	Model 12: a model that does not include predation (model 9) to represent a base single-species model 
# •	Model 13: a mutlispecies model that did not include halibut predation (model 10). 
# •	Model 14: a model with pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide short-time series model. 
# •	Model 15: as for models 11but using numbers-at-age of Pacific halibut from the areas-as-fleets short-time series model


mod_names_long <- c("1. SS long", "2. MS-No Halibut long", "3. MS-Coast long avg", "4. MS-Coast long low", "5. MS-Coast long high", "6. MS-AAF long avg", "7. MS-AAF long low", "8. MS-AAF long high")
mod_names_medium <- c("9. SS med", "10. MS-No Halibut med", "11. MS-Survey med")
mod_names_short <- c("12. SS short", "13. MS-No Halibut short", "14. MS-Coast short", "15. MS-AAF short")
mod_names_ss <- c(mod_names_long[1], mod_names_medium[1], mod_names_short[1])


ms_mod_list <- mod_list_all[c(2:8, 10:15)]
mod_list_long <- mod_list_all[1:8]
mod_list_medium <- mod_list_all[9:11]
mod_list_short <- mod_list_all[12:15]
mod_list_ss <- mod_list_all[c(1,9,12)]


mod_names_all <- c(mod_names_long , mod_names_short)
line_col <- oce::oce.colorsViridis(16)[1:15] 
line_col_long <- line_col[1:8]
line_col_medium <- line_col[9:11]
line_col_short <- line_col[12:15]

# Bounds diagnostics
rel_diff_upper <- abs(mod_list_all[[2]]$opt$diagnostics$Upper - mod_list_all[[2]]$opt$diagnostics$MLE)
rel_diff_upper[which(rel_diff_upper != Inf)]
rel_diff_lower <- abs(mod_list_all[[2]]$opt$diagnostics$Lower - mod_list_all[[2]]$opt$diagnostics$MLE)
rel_diff_lower[which(rel_diff_lower != Inf)]

######################### 
# Compare with SAFE Models
#########################
# Columns = year, pollock, cod, atf
safe2018biomass <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 1))
safe2018ssb <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 2))
safe2018rec <- as.data.frame(read_xlsx("Data/2018_SAFE_biomass_estimate.xlsx", sheet = 3))

# Assign data to CEATTLE object
Mod_18_SAFE <- mod_list_all[[1]]
# - Pollock and ATF
Mod_18_SAFE$quantities$biomass[1:2,1:42] <- t(safe2018biomass[1:42,c(2,4)]) * 1000
Mod_18_SAFE$quantities$biomassSSB[1:2,1:42] <- t(safe2018ssb[1:42,c(2,4)]) * 1000

# - Cod
Mod_18_SAFE$quantities$biomass[3,1:42] <- t(safe2018biomass[1:42,c(3)])
Mod_18_SAFE$quantities$biomassSSB[3,1:42] <- t(safe2018ssb[1:42,c(3)])

# Convert to age-3 biomass
Mod_18_5_1_3plusBiomass <- mod_list_ss
for(i in 1:length(Mod_18_5_1_3plusBiomass)){
  Mod_18_5_1_3plusBiomass[[i]]$quantities$biomass[1,1:49] <- colSums(Mod_18_5_1_3plusBiomass[[i]]$quantities$biomassByage[1,3:10,1:49])
}

plot_biomass(c(Mod_18_5_1_3plusBiomass, list(Mod_18_SAFE)), file =  "Figures/18.5.1/18.5.1. Bridging weighted March 2021", model_names = c(mod_names_ss, "2018 SAFE"), right_adj = 0.27, line_col = NULL, species = c(1:3))


# - Long time series
file_name <- "Figures/18.5.1/18.5.1_models_long"
plot_biomass(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.2, line_col = line_col_long)
plot_ssb(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.2, line_col = line_col_long)
plot_recruitment(mod_list_long, file = file_name, add_ci = TRUE, model_names = mod_names_long, right_adj = 0.27, line_col = line_col_long)
plot_index(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0, line_col = line_col_long)
plot_logindex(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.25, top_adj = 0.27, line_col = line_col_long)
plot_catch(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.25, top_adj = 0, line_col = line_col_long)
plot_b_eaten(mod_list_long, file = file_name, add_ci = FALSE, model_names = mod_names_long, right_adj = 0.17, line_col = line_col, lwd = 2)

nll_long <- data.frame(nll = sapply(mod_list_long, function(x) x$opt$objective) - (999^2)*10,
                       aic = sapply(mod_list_long, function(x) x$opt$AIC) - (999^2)*10 * 2)
nll_long$daic <- nll_long$aic - min(nll_long$aic)
nll_long <- round(nll_long)
write.csv(nll_long, "Figures/18.5.1/18.5.1.long_model_nll.csv")


# Plot an individual model
file_name <- "Figures/18.5.1/Time-series plots/18.5.1_mod3"
plot_biomass(mod_list_long[3], file = file_name, model_names = mod_names_long[3], line_col = line_col_long[3])
plot_ssb(mod_list_long[3], file = file_name, model_names = mod_names_long[3], line_col = line_col_long[3])
plot_recruitment(mod_list_long[3], file = file_name, add_ci = TRUE, model_names = mod_names_long[3], line_col = line_col_long[3])


# Medium time-series
file_name <- "Figures/18.5.1/18.5.1_models_medium"
plot_biomass(mod_list_medium, file = file_name, model_names = mod_names_medium, right_adj = 0.17, line_col = line_col_medium)
plot_ssb(mod_list_medium, file = file_name, model_names = mod_names_medium, right_adj = 0.17, line_col = line_col_medium)
plot_recruitment(mod_list_medium, file = file_name, add_ci = TRUE, model_names = mod_names_medium, right_adj = 0.17, line_col = line_col_medium)
plot_index(mod_list_medium, file = file_name, model_names = mod_names_medium, right_adj = 0, line_col = line_col_medium)
plot_logindex(mod_list_medium, file = file_name, model_names = mod_names_medium, right_adj = 0.1, line_col = line_col_medium)
plot_catch(mod_list_medium, file = file_name, model_names = mod_names_medium, right_adj = 0, line_col = line_col_medium)
nll_medium <- data.frame(nll = sapply(mod_list_medium, function(x) x$opt$objective),
                        aic = sapply(mod_list_medium, function(x) x$opt$AIC))
nll_medium$daic <- nll_medium$aic - min(nll_medium$aic)
nll_medium <- round(nll_medium)
write.csv(nll_medium, "Figures/18.5.1/18.5.1.medium_model_nll.csv")


# Short time-series
file_name <- "Figures/18.5.1/18.5.1_models_short"
plot_biomass(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0.17, line_col = line_col_short)
plot_ssb(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0.17, line_col = line_col_short)
plot_recruitment(mod_list_short, file = file_name, add_ci = TRUE, model_names = mod_names_short, right_adj = 0.17, line_col = line_col_short)
plot_index(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0, line_col = line_col_short)
plot_logindex(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0.1, line_col = line_col_short)
plot_catch(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0, line_col = line_col_short)
nll_short <- data.frame(nll = sapply(mod_list_short, function(x) x$opt$objective),
                        aic = sapply(mod_list_short, function(x) x$opt$AIC))
nll_short$daic <- nll_short$aic - min(nll_short$aic)
nll_short <- round(nll_short)
write.csv(nll_short, "Figures/18.5.1/18.5.1.short_model_nll.csv")


# All plots
file_name <- "Figures/18.5.1/18.5.1_models_all"
plot_biomass(mod_list_all[1:15], file = file_name, model_names = mod_names_all, right_adj = 0.17, line_col = line_col, lwd = 2)
plot_ssb(mod_list_all[1:15], file = file_name, add_ci = FALSE, model_names = mod_names_all, right_adj = 0.17, line_col = line_col, lwd = 2)
plot_b_eaten(mod_list_all[1:15], file = file_name, add_ci = FALSE, model_names = mod_names_all, right_adj = 0.17, line_col = line_col, lwd = 2)

plot_b_eaten_prop(mod_list_all[[2]], file = "Figures/18.5.1/mod_2", add_ci = FALSE, model_names = mod_names_all[2], right_adj = 0.17, line_col = line_col, lwd = 2.5)
plot_biomass(mod_list_all[[2]], file = "Figures/18.5.1/mod_2", model_names = mod_names_all[2], right_adj = 0.17, line_col = line_col, lwd = 2.5)

plot_recruitment(mod_list_all[1:15], file = file_name, add_ci = FALSE, model_names = mod_names_all, right_adj = 0.17, line_col = line_col, lwd = 2)
plot_logindex(mod_list_all[1:15], file = file_name, model_names = mod_names_all, right_adj = 0, line_col = line_col)
nll_all <- rbind(nll_long, nll_short)
write.csv(nll_all, "Figures/18.5.1/18.5.1.all_model_aic_nll.csv")


# Table of likelihood components
jnll_summary <- data.frame(matrix(NA, nrow = nrow(mod_list_all[[1]]$quantities$jnll_comp), ncol = (length(mod_list_all[1:15])+1)))
jnll_summary[,1] = rownames(mod_list_all[[1]]$quantities$jnll_comp)

for(i in 1:length(mod_list_all[1:15])){
  jnll_summary[,i+1] <- rowSums(mod_list_all[[i]]$quantities$jnll_comp)
  if(i < 9){
    jnll_summary[12,i+1] = jnll_summary[12,i+1] - (999^2 * 10)
  }
}
jnll_summary <- rbind(jnll_summary, c("jnll", sapply(mod_list_all[1:15], function(x) x$opt$objective)))
jnll_summary <- rbind(jnll_summary, c("k", sapply(mod_list_all[1:15], function(x) x$opt$number_of_coefficients[1])))
for(i in 1:length(mod_list_all[1:15])){
  jnll_summary[,i+1] <- as.numeric(jnll_summary[,i+1])
}
jnll_summary = jnll_summary[-which(rowSums(jnll_summary[,2:14]) == 0),]
write.csv(jnll_summary, "Figures/18.5.1/18.5.1.all_model_jnll_summary.csv")


# Different in likelihood components
for(i in 1:(nrow(jnll_summary)-1)){
  jnll_summary[i,2:9] <- jnll_summary[i,2:9] - min(jnll_summary[i,2:9])
  jnll_summary[i,10:14] <- jnll_summary[i,10:14] - min(jnll_summary[i,10:14])
}
jnll_summary <- rbind(jnll_summary, c("DAIC", nll_long$daic, nll_short$daic))
write.csv(jnll_summary, "Figures/18.5.1/18.5.1.all_model_delta_nll_summary.csv")



# Table of likelihood bits
jnll_summary <- data.frame(matrix(NA, nrow = length(mod_list_all[[1]]$quantities$jnll_comp), ncol = (length(mod_list_all[1:15])+3)))
jnll_summary[,1] = rep(rownames(mod_list_all[[1]]$quantities$jnll_comp), ncol(mod_list_all[[1]]$quantities$jnll_comp))
jnll_summary[,2] = paste0(rep(rownames(mod_list_all[[1]]$quantities$jnll_comp), ncol(mod_list_all[[1]]$quantities$jnll_comp)), "_$Sp/Srv/Fsh_", rep(1:ncol(mod_list_all[[1]]$quantities$jnll_comp), each = nrow(mod_list_all[[1]]$quantities$jnll_comp)))
jnll_summary[,3] <- rep(mod_list_all[[1]]$data_list$fleet_control$Fleet_name[mod_list_all[[1]]$data_list$fleet_control$Fleet_code], each = nrow(mod_list_all[[1]]$quantities$jnll_comp))

for(i in 1:length(mod_list_all[1:15])){
  jnll_summary[,i+3] <- c(mod_list_all[[i]]$quantities$jnll_comp)
  if(i < 9){
    jnll_summary[228,i+3] = jnll_summary[228,i+3] - (999^2 * 10)
  }
}
jnll_summary <- jnll_summary[which(rowSums(jnll_summary[,4:16]) !=0),]
colnames(jnll_summary) <- c("Likelihood component", "Likelihood component2", "Fleet_name", 1:length(mod_list_all[1:15]))
write.csv(jnll_summary, "Figures/18.5.1/18.5.1.all_nll_components.csv")

# Different in likelihood components
for(i in 1:nrow(jnll_summary)){
  jnll_summary[i,4:11] <- jnll_summary[i,4:11] - min(jnll_summary[i,4:11])
  jnll_summary[i,12:16] <- jnll_summary[i,12:16] - min(jnll_summary[i,12:16])
}
jnll_summary <- jnll_summary[which(rowSums(jnll_summary[,4:16]) !=0),]
# jnll_summary <- jnll_summary[which(round(rowSums(jnll_summary[,4:16]), 2) !=0),]
pop_pen <- which(jnll_summary[,1] == "Recruitment deviates" | jnll_summary[,1] == "Initial abundance deviates")
jnll_summary_pop_pen <- jnll_summary[pop_pen, ]
jnll_summary <- rbind(jnll_summary[-pop_pen, ], jnll_summary_pop_pen)
write.csv(jnll_summary, "Figures/18.5.1/18.5.1.all_delta_nll_components.csv")


# Difference in biomass
ssb_diff_long <- lapply(mod_list_all[2:8], function(x) x$quantities$biomassSSB / mod_list_all[[1]]$quantities$biomassSSB)
biom_diff_long <- lapply(mod_list_all[2:8], function(x) x$quantities$biomass / mod_list_all[[1]]$quantities$biomass)
R_diff_long <- lapply(mod_list_all[2:8], function(x) x$quantities$R / mod_list_all[[1]]$quantities$R)

# All long ms models
mean(sapply(ssb_diff_long[1:7], function(x) mean(x[1,])))
mean(sapply(biom_diff_long[1:7], function(x) mean(x[1,])))
mean(sapply(R_diff_long[1:7], function(x) mean(x[1,])))

# No halibut Model 2
mean(ssb_diff_long[[1]][1,])
mean(biom_diff_long[[1]][1,])

# Halibut Models 3-8
mean(sapply(ssb_diff_long[2:7], function(x) mean(x[1,])))
mean(sapply(biom_diff_long[2:7], function(x) mean(x[1,])))

# Short models
ssb_diff_short <- lapply(mod_list_all[10:15], function(x) x$quantities$biomassSSB / mod_list_all[[9]]$quantities$biomassSSB)
biom_diff_short <- lapply(mod_list_all[10:15], function(x) x$quantities$biomass / mod_list_all[[9]]$quantities$biomass)
R_diff_short <- lapply(mod_list_all[10:15], function(x) x$quantities$R / mod_list_all[[9]]$quantities$R)

# Halibut
mean(sapply(ssb_diff_short[1:4], function(x) mean(x[1,])))
mean(sapply(biom_diff_short[1:4], function(x) mean(x[1,])))
mean(sapply(R_diff_short[1:4], function(x) mean(x[1,])))

# No halibut
mean(ssb_diff_short[[1]][1,])
mean(biom_diff_short[[1]][1,])
sapply(ssb_diff_short[2:4], function(x) mean(x[1,]))
sapply(biom_diff_short[2:4], function(x) mean(x[1,]))


# Time series of total natural mortality
# Pollock
zmax <- sapply(mod_list_all[1:3], function(x) max(x$quantities$M[1,1,1:10,1:42]))
# zmax2 <- sapply(mod_list_all[9:15], function(x) max(x$quantities$M[1,1,1:10,1:26]))

for(i in 1:length(mod_list_all[1:3])){
  plot_mortality(Rceattle = mod_list_all[[i]],
             file = paste0("Figures/18.5.1/M2 Plots/Pollock/18.5.1_mod_",i),
             incl_proj = FALSE,
             zlim = c(0,ifelse(i < 9, max(zmax), max(zmax2))),
             contour = FALSE, spp = 1, maxage = 10,
             title = paste0("Model ",mod_names_all[i]), height = ifelse(i < 9, 2.3, 3))
  
}

# Cod
zmax <- sapply(mod_list_all[1:3], function(x) max((x$quantities$M[3,1,1:12,1:42])))
# zmax2 <- sapply(mod_list_all[9:15], function(x) max((x$quantities$M[2,1,1:12,1:26])))

zmin <- sapply(mod_list_all[1:3], function(x) min((x$quantities$M[3,1,1:12,1:42])))
# zmin2 <- sapply(mod_list_all[9:15], function(x) min((x$quantities$M[2,1,1:12,1:26])))

natage <- sapply(mod_list_all[1:3], function(x) max(x$quantities$NByage[3,1,12:12,1:42]))
# natage2 <- sapply(mod_list_all[9:15], function(x) max(x$quantities$NByage[2,1,12:12,1:26]))

for(i in 1:length(mod_list_all[1:3])){
  plot_mortality(Rceattle = mod_list_all[[i]],
                 file = paste0("Figures/18.5.1/M2 Plots/Cod/18.5.1_mod_",i),
                 incl_proj = FALSE,
                 zlim = c(ifelse(i < 9, min(zmin), min(zmin2)),ifelse(i < 9, max(zmax), max(zmax2))),
                 contour = FALSE, spp = 3, maxage = 12, log = FALSE,
                 title = paste0("Model ",mod_names_all[i]), height = ifelse(i < 9, 2.3, 3))
  
}

# ATF
zmax <- sapply(mod_list_all[1:3], function(x) max((x$quantities$M[2,,1:30,1:42])))
# zmax2 <- sapply(mod_list_all[9:15], function(x) max((x$quantities$M[3,,1:30,1:26])))

for(i in 1:length(mod_list_all[1:3])){
  plot_mortality(Rceattle = mod_list_all[[i]],
                 file = paste0("Figures/18.5.1/M2 Plots/ATF/18.5.1_mod_",i),
                 incl_proj = FALSE,
                 zlim = c(0,ifelse(i < 9, max(zmax), max(zmax2))),
                 contour = FALSE, spp = 2, maxage = 30, log = FALSE,
                 title = paste0("Model ",mod_names_all[i]), height = ifelse(i < 9, 4, 6))
  
}
mod_list_all[[3]]$data_list$UobsWtAge[which(mod_list_all[[3]]$data_list$UobsWtAge$Prey ==2 & mod_list_all[[3]]$data_list$UobsWtAge$Prey_age == 12 & mod_list_all[[3]]$data_list$UobsWtAge$Stomach_proportion_by_weight > 0),]


# Plot comp data of a few key models
for(i in 1:length(mod_list_all[1:3])){
  dir.create(paste0("Figures/18.5.1/Comp plots/Model ",i))
  plot_comp(Rceattle = mod_list_all[[i]],
            file = paste0("Figures/18.5.1/Comp plots/","Model ",i,"/18.5.1_mod_",i))
}

