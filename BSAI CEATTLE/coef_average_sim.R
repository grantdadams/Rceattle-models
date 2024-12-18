library(MASS)

var_covar <- matrix(c(1,1,.5,1,4,1,.5,1,1), 3, 3)
cov2cor(var_covar)
n = 1000
sim_dat <- mvrnorm(n, mu = c(1, 2, 3), Sigma = var_covar)

cor(sim_dat)


# Three mods
mods <- list()
mods[[1]] <- lm(sim_dat[,1]  ~ 1)
mods[[2]] <- lm(sim_dat[,1]  ~ sim_dat[,2])
mods[[3]] <- lm(sim_dat[,1]  ~ sim_dat[,3])
mods[[4]] <- lm(sim_dat[,1]  ~ sim_dat[,2] + sim_dat[,3])


aic_vec <- sapply(mods, AIC)
aic_vec