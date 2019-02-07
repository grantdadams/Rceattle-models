# Load models
library(Rceattle)
load("2019 Think Tank/Models/ss_no_re.RData")
mod <- mod_objects
load("2019 Think Tank/Models/ms_no_re.RData")
mod1 <- mod_objects
load("2019 Think Tank/Models/diet_mod.RData")
mod2 <- mod_objects
# load("2019 Think Tank/Models/ms_diet7.RData")
# mod3 <- mod_objects



plot_biomass(Rceattle = list(mod, mod1, mod2), 
             file_name = "2019 Think Tank/diet_runs", model_names = c("SS", "Empirical", "LN-L"),
             line_col = c("#272727", "#9B1D20", "#009FB7", "#F0C808", "#143642"),
             species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 3)), lwd = 3)

plot_recruitment(Rceattle = list(mod, mod1, mod2), 
                 file_name = "2019 Think Tank/diet_runs", model_names = c("SS", "Empirical", "LN-L"),
             line_col = c("#272727", "#9B1D20", "#009FB7", "#F0C808", "#143642"),
             species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 3)), lwd = 3)

plot_recruitment(Rceattle = list(mod, mod1, mod2), 
                 file_name = "2019 Think Tank/diet_runs", model_names = c("SS", "Empirical", "LN-L"),
                 line_col = c("#272727", "#9B1D20", "#009FB7", "#F0C808", "#143642"),
                 species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 3)), lwd = 3,
                 ci_col = adjustcolor(c("#272727", "#9B1D20", "#009FB7", "#F0C808", "#143642"), alpha.f = 0.5))


plot_selectivity(Rceattle = list(mod, mod1, mod2), 
                 file_name = "2019 Think Tank/diet_runs", model_names = c("SS", "Empirical", "LN-L"),
                 line_col = c("#272727", "#9B1D20", "#009FB7", "#F0C808", "#143642"),
                 species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 3)), lwd = 3)




for(a in 1:12){
  plot_mort(Rceattle = list(mod, mod1, mod2), 
            file_name = "2019 Think Tank/diet_runs", model_names = c("SS", "Empirical", "LN-L"),
            line_col = c("#272727", "#9B1D20", "#009FB7", "#F0C808", "#143642"),
            species = c("Walleye pollock", "Pacific cod", "Arrowtooth flounder", rep(NA, 3)), lwd = 3, age = a)
  
}

jnl <- cbind(mod$quantities$jnll_comp, mod1$quantities$jnll_comp,mod2$quantities$jnll_comp)
