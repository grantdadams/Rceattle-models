library(Rceattle)
library(wesanderson)
setwd("Model runs/GOA_18.5.1")
load("Models/18_5_1_2020-11-08.RData")

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
abs(sapply(mod_list_all[1:13], function(x) x$opt$objective) - sapply(mod_list_all[1:13], function(x) x$quantities$jnll)>1)
sapply(mod_list_all[1:13], function(x) x$sdrep$sd[1])

# Re-order and name models
# The long time-series models for 1977 to 2018 were: 
#   •	Model 1: a model that did not include predation (single-species models) representing a base model. 
# •	Model 2: a model that did not include halibut predation to allow comparisons in which halibut does not impact the dynamics of groundfish in the GOA. 
# •	Models 3-5: models that included pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide long-time (1917-2018) series model developed by the IPHC. To account for a lack of information on halibut distribution prior to 1993, numbers-at-age prior to 1993 were multiplied by the 50th (model 3), 15th (model 4), and 85th (model 5) quantiles of the distribution of adult halibut in area 3 between 1993 and 2018. 
# •	Models 6-8: as for models 3-5 but using numbers-at-age of Pacific halibut from the areas-as-fleets long-time series model. 

# The five short term models for 1993 to 2018 were: 
#   •	Model 9: a model that does not include predation (model 9) to represent a base single-species model 
# •	Model 10: a model that did not include halibut predation (model 10). 
# •	Model 11: a model with pre-specified mid-year numbers-at-age of Pacific halibut from the coastwide short-time series model. 
# •	Model 12: as for models 11but using numbers-at-age of Pacific halibut from the areas-as-fleets short-time series model 
# •	Model 13: a model relative abundance-at-age of Pacific halibut in area 3 multiplied by an estimated parameter to allow the model to estimate the relative contribution of Pacific halibut predation to describing the dynamics of pollock, Pacific cod, and arrowtooth flounder. 
# Model 14 - SAFE 2018 Assessments





mod_names_long <- c("1. SS", "2. MS-No Halibut", "3. MS-Coast avg", "4. MS-Coast low", "5. MS-Coast high", "6. MS-AAF avg", "7. MS-AAF low", "8. MS-AAF high")
mod_names_short <- c("9. SS", "10. MS-No Halibut", "11. MS-Coast", "12. MS-AAF", "13. MS-Survey")
ms_mod_list <- mod_list_all[c(2:8, 10:13)]
mod_list_long <- mod_list_all[1:8]
mod_list_short <- mod_list_all[9:13]
mod_names_all <- c(mod_names_long, mod_names_short)
line_col <- oce::oce.colorsViridis(13)[1:13] #tableau_color_pal(palette = "Hue Circle", type = c("regular", "ordered-sequential", "ordered-diverging"), direction = 1)(19)[c(1:8, 9,11,13,15,19)] # c(wes_palette("Darjeeling1", 8, type = "continuous"), wes_palette("Darjeeling2", 5, type = "continuous"))
line_col_long <- line_col[1:8]
line_col_short <- line_col[9:13]

# Bounds diagnostics
rel_diff_upper <- abs(mod_list_all[[2]]$opt$diagnostics$Upper - mod_list_all[[2]]$opt$diagnostics$MLE)
rel_diff_upper[which(rel_diff_upper != Inf)]
rel_diff_lower <- abs(mod_list_all[[2]]$opt$diagnostics$Lower - mod_list_all[[2]]$opt$diagnostics$MLE)
rel_diff_lower[which(rel_diff_lower != Inf)]

######################### Plots
# - SAFE vs SS
file_name <- "Figures/18.5.1/18.5.1_SAFE_vs_single_species"
mod_list_all[[1]]$quantities$biomass[1,1:42] <- colSums(mod_list_all[[1]]$quantities$biomassByage[1,3:10,1:42])
mod_list_all[[14]]$quantities$biomass[1:3,1:42] <- t(safe2018biomass[,2:4]) * 1000 * 0.453592
mod_list_all[[14]]$quantities$biomassSSB[1:3,1:42] <- t(safe2018ssb[,2:4]) * 1000 * 0.453592
plot_biomass(mod_list_all[c(1,14)], file = file_name, model_names = c("CEATTLE Single-species", "2018 SAFE (mt)"), right_adj = 0.27, line_col = c(line_col_long[1],1), species = 1)
plot_ssb(mod_list_all[c(1,14)], file = file_name, model_names = c("CEATTLE Single-species", "2018 SAFE (mt)"), right_adj = 0.27, line_col = c(line_col_long[1],1), species = 1)
plot_recruitment(mod_list_all[c(1,14)], file = file_name, add_ci = FALSE, model_names = c(mod_names_long[1], "2018 SAFE"), right_adj = 0.27, line_col = c(line_col_long[1],1), species = 1)

# - Long time series
file_name <- "Figures/18.5.1/18.5.1_models_long"
plot_biomass(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.27, line_col = line_col_long)
plot_ssb(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.27, line_col = line_col_long)
plot_recruitment(mod_list_long, file = file_name, add_ci = TRUE, model_names = mod_names_long, right_adj = 0.27, line_col = line_col_long)
plot_index(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0, line_col = line_col_long)
plot_logindex(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.25, top_adj = 0.27, line_col = line_col_long)
plot_catch(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.25, top_adj = 0, line_col = line_col_long)

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
plot_biomass(mod_list_all[1:13], file = file_name, model_names = mod_names_all, right_adj = 0.17, line_col = line_col, lwd = 2)
plot_ssb(mod_list_all[1:13], file = file_name, add_ci = FALSE, model_names = mod_names_all, right_adj = 0.17, line_col = line_col, lwd = 2)
plot_b_eaten(mod_list_all[1:13], file = file_name, add_ci = FALSE, model_names = mod_names_all, right_adj = 0.17, line_col = line_col, lwd = 2)

plot_b_eaten_prop(mod_list_all[[3]], file = "Figures/18.5.1/mod_3", add_ci = FALSE, model_names = mod_names_all[3], right_adj = 0.17, line_col = line_col, lwd = 2)
plot_biomass(mod_list_all[[3]], file = "Figures/18.5.1/mod_3", model_names = mod_names_all[3], right_adj = 0.17, line_col = line_col, lwd = 2)

plot_recruitment(mod_list_all[1:13], file = file_name, add_ci = FALSE, model_names = mod_names_all, right_adj = 0.17, line_col = line_col, lwd = 2)
plot_logindex(mod_list_all[1:13], file = file_name, model_names = mod_names_all, right_adj = 0, line_col = line_col)
nll_all <- rbind(nll_long, nll_short)
write.csv(nll_all, "Figures/18.5.1/18.5.1.all_model_aic_nll.csv")

# Table of likelihood components
jnll_summary <- data.frame(matrix(NA, nrow = nrow(mod_list_all[[1]]$quantities$jnll_comp), ncol = (length(mod_list_all[1:13])+1)))
jnll_summary[,1] = rownames(mod_list_all[[1]]$quantities$jnll_comp)

for(i in 1:length(mod_list_all[1:13])){
  jnll_summary[,i+1] <- rowSums(mod_list_all[[i]]$quantities$jnll_comp)
  if(i < 9){
    jnll_summary[12,i+1] = jnll_summary[12,i+1] - (999^2 * 10)
  }
}
jnll_summary <- rbind(jnll_summary, c("jnll", sapply(mod_list_all[1:13], function(x) x$opt$objective)))
jnll_summary <- rbind(jnll_summary, c("k", sapply(mod_list_all[1:13], function(x) x$opt$number_of_coefficients[1])))
for(i in 1:length(mod_list_all[1:13])){
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
jnll_summary <- data.frame(matrix(NA, nrow = length(mod_list_all[[1]]$quantities$jnll_comp), ncol = (length(mod_list_all[1:13])+3)))
jnll_summary[,1] = rep(rownames(mod_list_all[[1]]$quantities$jnll_comp), ncol(mod_list_all[[1]]$quantities$jnll_comp))
jnll_summary[,2] = paste0(rep(rownames(mod_list_all[[1]]$quantities$jnll_comp), ncol(mod_list_all[[1]]$quantities$jnll_comp)), "_$Sp/Srv/Fsh_", rep(1:ncol(mod_list_all[[1]]$quantities$jnll_comp), each = nrow(mod_list_all[[1]]$quantities$jnll_comp)))
jnll_summary[,3] <- rep(mod_list_all[[1]]$data_list$fleet_control$Fleet_name[mod_list_all[[1]]$data_list$fleet_control$Fleet_code], each = nrow(mod_list_all[[1]]$quantities$jnll_comp))

for(i in 1:length(mod_list_all[1:13])){
  jnll_summary[,i+3] <- c(mod_list_all[[i]]$quantities$jnll_comp)
  if(i < 9){
    jnll_summary[228,i+3] = jnll_summary[228,i+3] - (999^2 * 10)
  }
}
jnll_summary <- jnll_summary[which(rowSums(jnll_summary[,4:16]) !=0),]
colnames(jnll_summary) <- c("Likelihood component", "Likelihood component2", "Fleet_name", 1:length(mod_list_all[1:13]))
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
ssb_diff_short <- lapply(mod_list_all[10:13], function(x) x$quantities$biomassSSB / mod_list_all[[9]]$quantities$biomassSSB)
biom_diff_short <- lapply(mod_list_all[10:13], function(x) x$quantities$biomass / mod_list_all[[9]]$quantities$biomass)
R_diff_short <- lapply(mod_list_all[10:13], function(x) x$quantities$R / mod_list_all[[9]]$quantities$R)

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
library(ggplot2)
zmax <- sapply(mod_list_all[1:8], function(x) max(x$quantities$M[1,1,1:10,1:42]))
zmax2 <- sapply(mod_list_all[9:13], function(x) max(x$quantities$M[1,1,1:10,1:26]))

for(i in 1:length(mod_list_all[1:13])){
  plot_mortality(Rceattle = mod_list_all[[i]],
             file = paste0("Figures/18.5.1/M2 Plots/Pollock/18.5.1_mod_",i),
             incl_proj = FALSE,
             zlim = c(0,ifelse(i < 9, max(zmax), max(zmax2))),
             contour = FALSE, spp = 1, maxage = 10,
             title = paste0("Model ",mod_names_all[i]), height = ifelse(i < 9, 2.3, 3))
  
}

# Cod
zmax <- sapply(mod_list_all[1:8], function(x) max((x$quantities$M[2,1,1:12,1:42])))
zmax2 <- sapply(mod_list_all[9:13], function(x) max((x$quantities$M[2,1,1:12,1:26])))

zmin <- sapply(mod_list_all[1:8], function(x) min((x$quantities$M[2,1,1:12,1:42])))
zmin2 <- sapply(mod_list_all[9:13], function(x) min((x$quantities$M[2,1,1:12,1:26])))

natage <- sapply(mod_list_all[1:8], function(x) max(x$quantities$NByage[2,1,12:12,1:42]))
natage2 <- sapply(mod_list_all[9:13], function(x) max(x$quantities$NByage[2,1,12:12,1:26]))

for(i in 1:length(mod_list_all[1:13])){
  plot_mortality(Rceattle = mod_list_all[[i]],
                 file = paste0("Figures/18.5.1/M2 Plots/Cod/18.5.1_mod_",i),
                 incl_proj = FALSE,
                 zlim = c(ifelse(i < 9, min(zmin), min(zmin2)),ifelse(i < 9, max(zmax), max(zmax2))),
                 contour = FALSE, spp = 2, maxage = 12, log = FALSE,
                 title = paste0("Model ",mod_names_all[i]), height = ifelse(i < 9, 2.3, 3))
  
}
round(mod_list_all[[3]]$quantities$M2_prop[1,2,,1,,1,42],2)
round(mod_list_all[[3]]$quantities$M2_prop[2,2,,1,,1,42],2)
round(mod_list_all[[3]]$quantities$M2_prop[3,2,,1,,1,42],2)
round(mod_list_all[[3]]$quantities$M2_prop[4,2,,1,,1,42],2)

# ATF
zmax <- sapply(mod_list_all[1:8], function(x) max((x$quantities$M[3,,1:30,1:42])))
zmax2 <- sapply(mod_list_all[9:13], function(x) max((x$quantities$M[3,,1:30,1:26])))

for(i in 1:length(mod_list_all[1:13])){
  plot_mortality(Rceattle = mod_list_all[[i]],
                 file = paste0("Figures/18.5.1/M2 Plots/ATF/18.5.1_mod_",i),
                 incl_proj = FALSE,
                 zlim = c(0,ifelse(i < 9, max(zmax), max(zmax2))),
                 contour = FALSE, spp = 3, maxage = 30, log = FALSE,
                 title = paste0("Model ",mod_names_all[i]), height = ifelse(i < 9, 4, 6))
  
}
mod_list_all[[3]]$data_list$UobsWtAge[which(mod_list_all[[3]]$data_list$UobsWtAge$Prey ==2 & mod_list_all[[3]]$data_list$UobsWtAge$Prey_age == 12 & mod_list_all[[3]]$data_list$UobsWtAge$Stomach_proportion_by_weight > 0),]


# Plot comp data of a few key models
for(i in 1:length(mod_list_all[1:13])){
  dir.create(paste0("Figures/18.5.1/Comp plots/Model ",i))
  plot_comp(Rceattle = mod_list_all[[i]],
            file = paste0("Figures/18.5.1/Comp plots/","Model ",i,"/18.5.1_mod_",i))
}


