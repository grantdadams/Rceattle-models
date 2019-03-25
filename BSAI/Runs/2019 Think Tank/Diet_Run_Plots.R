# Load models
library(Rceattle)
load("2019 Think Tank/Models/ss_no_re.RData")
mod <- mod_objects
load("2019 Think Tank/Models/ms_no_re.RData")
mod1 <- mod_objects
load("2019 Think Tank/Models/ms_diet1.RData")
mod2 <- mod_objects
load("2019 Think Tank/Models/ms_diet2.RData")
mod3 <- mod_objects
load("2019 Think Tank/Models/ms_diet3.RData")
mod4 <- mod_objects
load("2019 Think Tank/Models/ms_diet8.RData")
mod5 <- mod_objects

# Make into list
mod_list <- list(mod, mod1, mod2, mod3, mod4, mod5)
colors <- c("#272727", "#9B1D20", "#009FB7", "#F0C808", "#143642", 5)
model_names = c("SS", "Empirical", "G-L", "G-TV-L", "G-TV-L", "F-LN-TV-L")

jnll <- sapply(mod_list, function(x) x$quantities$jnll)


# Plot
plot_biomass(Rceattle = mod_list, 
             file_name = "2019 Think Tank/Figures/diet_runs", model_names = model_names,
             line_col = colors,
             species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 3)), lwd = 3)

plot_recruitment(Rceattle = mod_list, 
                 file_name = "2019 Think Tank/Figures/diet_runs", model_names = model_names,
             line_col = colors,
             species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 3)), lwd = 3)

plot_recruitment(Rceattle = mod_list, 
                 file_name = "2019 Think Tank/Figures/diet_runs", model_names = model_names,
                 line_col = colors,
                 species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 3)), lwd = 3,
                 ci_col = adjustcolor(colors, alpha.f = 0.5))


plot_selectivity(Rceattle = mod_list, 
                 file_name = "2019 Think Tank/Figures/diet_runs", model_names = model_names,
                 line_col = colors,
                 species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 3)), lwd = 3)




for(a in 1:12){
  plot_mort(Rceattle = mod_list, 
            file_name = "2019 Think Tank/Figures/diet_runs", model_names = model_names,
            line_col = colors,
            species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 3)), lwd = 3, age = a)
  
}

jnl <- cbind(mod$quantities$jnll_comp, mod1$quantities$jnll_comp,mod2$quantities$jnll_comp)
