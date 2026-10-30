set.seed(123)

# Parameters ----
n_years   <- 50
phi       <- 0.7   # AR1 rho
sigma_k   <- 1.0   # SD of krill interannual variability

sigma_obs <- 0.2   # Observation error SD

beta      <- 0.5
sigma_R   <- 0.4
R0        <- 1000

# Simulate ----
k_dev   <- rnorm(n_years, mean = 0, sd = sigma_k)      # AR1 devs
obs_dev   <- rnorm(n_years, mean = 0, sd = sigma_obs)  # Krill obs error
rec_dev   <- rnorm(n_years, mean = 0, sd = sigma_R)    # Annual log-recruitment deviations

## Calculate ----
k_true <- c()
k_obs <- c()
recruitment <- c()

k_true[1] <- k_dev[1] / sqrt(1 - phi^2) #AR1 start
for (yr in 1:n_years) {

    # Krill AR1
    if (yr >= 2) {
        k_true[yr] <- phi * k_true[yr - 1] + k_dev[yr]
    }

    # Obs krill
    k_obs[yr] <- k_true[yr] + obs_dev[yr]

    # Recruitment
    recruitment[yr] <- R0 * exp(beta * k_true[yr] + rec_dev[yr])
}