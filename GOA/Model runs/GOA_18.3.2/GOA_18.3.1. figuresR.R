library(Rceattle)
library(wesanderson)
setwd("Model runs/GOA_18.3.2")
load("Models/18_3_1.RData")

mod_list_all <- mod_list_all[1:13]
sapply(mod_list_all, function(x) x$opt$objective) - sapply(mod_list_all, function(x) x$quantities$jnll)
sapply(mod_list_all, function(x) x$sdrep$sd[1])

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



mod_names_long <- c("1. SS", "2. MS-No Halibut", "3. MS-Coast avg", "4. MS-Coast low", "5. MS-Coast high", "6. MS-AAF avg", "7. MS-AAF low", "8. MS-AAF high")
mod_names_short <- c("9. SS", "10. MS-No Halibut", "11. MS-Coast", "12. MS-AAF", "13. MS-Survey")
ms_mod_list <- mod_list_all[c(2:8, 10:13)]
mod_list_long <- mod_list_all[1:8]
mod_list_short <- mod_list_all[9:13]
mod_names_all <- c(mod_names_long, mod_names_short)
line_col <- oce::oce.colorsViridis(13)[1:13] #tableau_color_pal(palette = "Hue Circle", type = c("regular", "ordered-sequential", "ordered-diverging"), direction = 1)(19)[c(1:8, 9,11,13,15,19)] # c(wes_palette("Darjeeling1", 8, type = "continuous"), wes_palette("Darjeeling2", 5, type = "continuous"))
line_col_long <- line_col[1:8]
line_col_short <- line_col[9:13]

file_name <- "Figures/18.3.1/18.3.1_models_long"
plot_biomass(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.27, line_col = line_col_long)
plot_ssb(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.27, line_col = line_col_long)
plot_recruitment(mod_list_long, file = file_name, add_ci = TRUE, model_names = mod_names_long, right_adj = 0.27, line_col = line_col_long)
plot_index(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0, line_col = line_col_long)
plot_logindex(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.25, top_adj = 0.27, line_col = line_col_long)
plot_catch(mod_list_long, file = file_name, model_names = mod_names_long, right_adj = 0.25, top_adj = 0, line_col = line_col_long)

nll_long <- data.frame(nll = sapply(mod_list_long, function(x) x$opt$objective),
                       aic = sapply(mod_list_long, function(x) x$opt$AIC))
nll_long$daic <- nll_long$aic - min(nll_long$aic)
write.csv(nll_long, "Figures/18.3.1/18.3.1.long_model_nll.csv")

# Short
file_name <- "Figures/18.3.1/18.3.1_models_short"
plot_biomass(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0.17, line_col = line_col_short)
plot_ssb(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0.17, line_col = line_col_short)
plot_recruitment(mod_list_short, file = file_name, add_ci = TRUE, model_names = mod_names_short, right_adj = 0.17, line_col = line_col_short)
plot_index(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0, line_col = line_col_short)
plot_logindex(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0.1, line_col = line_col_short)
plot_catch(mod_list_short, file = file_name, model_names = mod_names_short, right_adj = 0, line_col = line_col_short)
nll_short <- data.frame(nll = sapply(mod_list_short, function(x) x$opt$objective),
                        aic = sapply(mod_list_short, function(x) x$opt$AIC))
nll_short$daic <- nll_short$aic - min(nll_short$aic)

write.csv(nll_short, "Figures/18.3.1/18.3.1.short_model_nll.csv")

file_name <- "Figures/18.3.1/18.3.1_models_all"
plot_biomass(mod_list_all, file = file_name, model_names = mod_names_all, right_adj = 0.17, line_col = line_col, lwd = 2)
plot_ssb(mod_list_all, file = file_name, add_ci = FALSE, model_names = mod_names_all, right_adj = 0.17, line_col = line_col, lwd = 2)
plot_recruitment(mod_list_all, file = file_name, add_ci = FALSE, model_names = mod_names_all, right_adj = 0.17, line_col = line_col, lwd = 2)
plot_logindex(mod_list_all, file = file_name, model_names = mod_names_all, right_adj = 0, line_col = line_col)


# Table of likelihood components
jnll_summary <- data.frame(matrix(NA, nrow = nrow(mod_list_all[[1]]$quantities$jnll_comp), ncol = (length(mod_list_all)+1)))
jnll_summary[,1] = rownames(mod_list_all[[1]]$quantities$jnll_comp)

for(i in 1:length(mod_list_all)){
  jnll_summary[,i+1] <- rowSums(mod_list_all[[i]]$quantities$jnll_comp)
  if(i < 9){
    jnll_summary[12,i+1] = jnll_summary[12,i+1] - (999^2 * 10)
  }
}
jnll_summary <- rbind(jnll_summary, c("k", sapply(mod_list_all, function(x) x$opt$number_of_coefficients[1])))
write.csv(jnll_summary, "Figures/18.3.1/18.3.1.long_model_jnll_summary.csv")


# Table of likelihood components
jnll_summary <- data.frame(matrix(NA, nrow = nrow(mod_list_all[[1]]$quantities$jnll_comp), ncol = (length(mod_list_all)+1) * 3))
jnll_summary[,1] = rownames(mod_list_all[[1]]$quantities$jnll_comp)

for(i in 1:length(mod_list_all)){
  jnll_summary[,i+1] <- rowSums(mod_list_all[[i]]$quantities$jnll_comp)
  if(i < 9){
    jnll_summary[12,i+1] = jnll_summary[12,i+1] - (999^2 * 10)
  }
}
jnll_summary <- rbind(jnll_summary, c("k", sapply(mod_list_all, function(x) x$opt$number_of_coefficients[1])))
write.csv(jnll_summary, "Figures/18.3.1/18.3.1.long_model_jnll_summary.csv")




# Difference in biomass
ssb_diff_long <- lapply(mod_list_all[2:8], function(x) x$quantities$biomassSSB / mod_list_all[[1]]$quantities$biomassSSB)
biom_diff_long <- lapply(mod_list_all[2:8], function(x) x$quantities$biomass / mod_list_all[[1]]$quantities$biomass)
R_diff_long <- lapply(mod_list_all[2:8], function(x) x$quantities$R / mod_list_all[[1]]$quantities$R)

mean(sapply(ssb_diff_long[1:7], function(x) mean(x[1,])))
mean(sapply(biom_diff_long[1:7], function(x) mean(x[1,])))
mean(sapply(R_diff_long[1:7], function(x) mean(x[1,])))

mean(ssb_diff_long[[1]][1,])
mean(biom_diff_long[[1]][1,])
sapply(ssb_diff_long[2:7], function(x) mean(x[1,]))
sapply(biom_diff_long[2:7], function(x) mean(x[1,]))

ssb_diff_short <- lapply(mod_list_all[10:13], function(x) x$quantities$biomassSSB / mod_list_all[[9]]$quantities$biomassSSB)
biom_diff_short <- lapply(mod_list_all[10:13], function(x) x$quantities$biomass / mod_list_all[[9]]$quantities$biomass)
R_diff_short <- lapply(mod_list_all[10:13], function(x) x$quantities$R / mod_list_all[[9]]$quantities$R)


mean(sapply(ssb_diff_short[1:4], function(x) mean(x[1,])))
mean(sapply(biom_diff_short[1:4], function(x) mean(x[1,])))
mean(sapply(R_diff_short[1:4], function(x) mean(x[1,])))


mean(ssb_diff_short[[1]][1,])
mean(biom_diff_short[[1]][1,])
sapply(ssb_diff_short[2:4], function(x) mean(x[1,]))
sapply(biom_diff_short[2:4], function(x) mean(x[1,]))