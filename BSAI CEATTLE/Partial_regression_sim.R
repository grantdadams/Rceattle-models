library(MASS)

sigmas <- c(1, 2)
cor = 0.5

# Simulate predictors with cor = 0.5
covar <- matrix(c(1,1, 1, 4), 2 , 2)
cov2cor(covar)
predictors <- mvrnorm(n = 1000, mu = c(2, 6), Sigma = covar)
plot(predictors[,1], predictors[,2])
cor(predictors[,1], predictors[,2])

# Predict y 
beta <- c(2, 2)
y <- predictors %*% beta + rnorm(1000, 0, 8)

# Make four different models
mods <- list()
mods[[1]] <- lm(y ~ 1) #  no intercept
mods[[2]] <- lm(y ~ predictors[,1]) # Predictor 1
mods[[3]] <- lm(y ~ predictors[,2]) # Predictor 2
mods[[4]] <- lm(y ~ predictors) # Predictor 1 and 2

aic_vec <- sapply(mods, AIC)
aic_vec
