
# urm

<!-- badges: start -->
<!-- badges: end -->
```
pak::pak("BenWilliams-NOAA/urm")
```
The goal of `urm` is to provide a single functional RTMB model for AFSC rockfish stocks, using 'best practices'.
It is setup for a single area, single sex, single fleet, single survey with recruitment as log deviates from a mean.

Example data format:

```
data = list(ages = ,        					# vector - rec_age to plus_age
  			    years = ,							    # vector - all years of catch 
            length_bins = , 					# vector 
            spawn_mo = ,    					# scalar - when they spawn
            sex_ratio = 0.5,					# scalar - ratio of females to males
            bias_ramp = , 						# vector - values between 0-1, dim: length(years), set to 1 - if unsure
            bias_switch = ,						# scalar - 1 = use bias ramp, 0 = do not
            waa = ,          					# vector - weight at age (grams)
            maa = ,										# vector - maturity at age 
            slx_type = ,							# vector - length: number of selectivity curves, minimum need 2 (fishery and survey) 
            													# 1=logistic (2 pars), 2=gamma (2 pars), 3=double normal (4 pars), 4=double logistic (4 pars)
            fish_block_ind = , 				# vector - length (years), integer mapping each year to a selectivity curve 
            srv_slx_ind = , 					# integer - pointer to which curve in log_slx_pars belongs to the survey
            catch_obs = ,  						# vector - catch by year, (tons)
            catch_cv = ,   						# vector - catch cv = 0.1 to match the historical weighting of 50
            srv_ind = ,								# vector - survey index, dim: length(years) 1 = use, 0 = ignore
            srv_yrs = ,								# vector - survey years
            srv_obs = ,								# vector - survey obs (unit: tons)
            srv_cv = ,								# vector - survey cv
            srv_wt = 1.0,							# scalar - survey weight
            fish_age_ind = ,					# vector - fish age comp index, dim: length(years) 1 = use, 0 = ignore
            fish_age_yrs = ,					# vector - fish age comp years
            fish_age_obs = ,					# matrix - fishery age comp observations, dim: ages, length(fish_age_yrs)
            fish_age_iss = ,					# vector - fish age comp input sample size, dim: length(fish_age_yrs)
            fish_age_wt = 1.0,					# scalar - fish age comp weight
            srv_age_ind = ,						# vector - survey age comp index, dim length(years) 1 = use, 0 = ignore
            srv_age_yrs = ,						# vector - survey age comp years
            srv_age_obs = ,						# matrix - survey age comp observations, dim: ages, length(srv_age_yrs)
            srv_age_iss = ,						# vector - survey age comp input sample size, dim: length(srv_age_yrs)
            srv_age_wt = 1.0,					# scalar - survey age comp weight
            fish_size_ind = ,					# vector - fish size comp index, dim: length(years) 1 = use, 0 = ignore
            fish_size_yrs = ,					# vector - fish size comp years
            fish_size_obs = ,					# matrix - fish proportions at length, dim: A, length(fish_size_yrs))
            fish_size_iss = ,					# vector - fish size comp input sample size, dim: length(fish_size_yrs)
            fish_size_wt = 1.0,					# scalar - fish size comp weight
            age_error = ,						# matrix - age error (can be a rectangular matrix)
            saa_array = ,						# array  - of size-at-age matrixes (dim: ages, length_bins)
            fish_saa_ind =, 					# vector - which saa_array to use, (dim: length(years)))
            wt_fmort_reg = 0.1,					# fishing mortality regulation weight
            wt_rec_var = 1,						# recruitment variability weight
            mean_M = 0.0614,					# natural mortality prior
            cv_M = 0.1,							# natural mortality cv
            mean_q = 1.15, 						# catchbility prior
            cv_q = 0.447213595,					# catchability cv
            mean_sigmaR = 1.7, 					# sigmaR prior
            cv_sigmaR = 0.2,  					# sigmaR cv
            yield_ratio = yield_ratio) 	        # scalar - yield ratio to pass through to projection module
```

Selectivity parameters example  
[selectivity functions can be found here](https://github.com/BenWilliams-NOAA/RTMButils/blob/main/R/selectivity.R). 
Setup as a a matrix with rows being the params for fishery (w or w/o timeblocks) and survey parameters. 
`srv_slx_ind` identifies the row that corresponds to the survey selectivity parameters.

```
# for a gamma fishery and logistic survey, no time blocks
# 1=logistic (2 pars)
# 2=gamma (2 pars)
# 3=double normal (4 pars)
# 4=double logistic (4 pars)
slx_type = c(2, 1),	
fish_block_ind = rep(1, length(years)), 					
srv_slx_ind = 2, # the position in slx_type that id's the survey slx type
log_slx_pars = log( matrix(c(a50 = log(7.5), delta = 3.0,
                             a50 = log(7.3), delta = 3.8), byrow = TRUE, ncol = 2) )


# for multiple time blocks
# for a gamma fishery and logistic survey, no time blocks
slx_type = c(1, 2, 4, 4, 1)
fish_block_ind = c(rep(1, 10), rep(2, 5), rep(3, 15), rep(4, 10)), 	# in this case length(years) = 40				
srv_slx_ind = 5,              # the position in slx_type that id's the survey slx type
 
log_slx_pars = log( matrix(c(
  # pars[1], pars[2], pars[3], pars[4]
  7.5,       3.0,     0.0,     0.0,   # curve 1 (type 1: logistic)     - fishery time block 1
  5.0,       2.5,     0.0,     0.0,   # curve 2 (type 2: gamma)        - fishery time block 2
  6.0,       0.8,     3.0,     0.5,   # curve 3 (type 4: double logistic)   - fishery time block 3
  6.5,       0.9,     3.5,     0.6,   # curve 4 (type 4: double logistic)   - fishery time block 4
  7.3,       3.8,     0.0,     0.0    # curve 5 (type 1: logistic)     - survey
), nrow = 5, ncol = 4, byrow = TRUE))	

# since not all of the selctivity types need 4 parameters we'll map the unused parameters off
# create a sequence of unique #s for the whole matrix
map_slx = matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE)

# overwrite the dummy parameter slots with NA
map_slx_matrix[1, 3:4] <- NA  # freeze pars 3 & 4 for curve 1
map_slx_matrix[2, 3:4] <- NA  # freeze pars 3 & 4 for curve 2
map_slx_matrix[5, 3:4] <- NA  # freeze pars 3 & 4 for curve 5

# add any other maps (e.g., sigmaR)
mapping = list(log_slx_pars = map_slx,
				sigmaR = factor(NA))
                                  

```

Example parameter inputs:

```
pars = list(log_M = log(0.0614),
             log_slx_pars = log_slx_pars,
             log_q = log(1.15),
             log_mean_R = 3.0,
             log_Rt = rep(0, length(data$years) + nrow(data$age_error) - 1),
             log_mean_F = 0,
             log_Ft = rep(0, length(data$years)),
             sigmaR = 1.7,
             sigmaF = 1.0,
             log_F50 = log(0.05), 
             log_F40 = log(0.06), 
             log_F35 = log(0.07))
```           

Build a bias ramp

```
# 4 inflection points
yr1 = 1980 # last year of no real data
yr2 = 1990 # first year of full data
yr3 = 2024 # max(years) - recruitment age
yr4 = 2026 # last year of data 

bias_ramp = rep(0, length(years))

for(t in 1:length(years)) {
  y = years[t]
  
  if (y > yr1 & y < yr2) {
    # ascending limb
    bias_ramp[t] = (y - yr1) / (yr2 - yr1) 
  } else if (y >= yr2 & y <= yr3) {
    # full data
    bias_ramp[t] = 1.0 
  } else if (y > yr3 & y < yr4) {
    # descending limb
    bias_ramp[t] = 1.0 - ((y - yr3) / (yr4 - yr3)) 
  }
  # (years <= yr1 and >= yr4 stay 0.0)
}

data$bias_ramp = bias_ramp
```

Model run
```
pak::pak("BenWilliams-NOAA/RTMButils")
mod = RTMButils::run_model(urm, data, pars, map = mapping)

# to run the model with random effects, turn then data$bias_switch = 0
mod = RTMButils::run_model(urm, data, pars, map = mapping, random = "log_Rt")
```

### NOAA README

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

### NOAA License

Software code created by U.S. Government employees is not subject to
copyright in the United States (17 U.S.C. §105). The United
States/Department of Commerce reserve all rights to seek and obtain
copyright protection in countries other than the United States for
Software authored in its entirety by the Department of Commerce. To this
end, the Department of Commerce hereby grants to Recipient a
royalty-free, nonexclusive license to use, copy, and create derivative
works of the Software outside of the United States.

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" height="75" alt="NOAA Fisheries">

[U.S. Department of Commerce](https://www.commerce.gov/) | [National
Oceanographic and Atmospheric Administration](https://www.noaa.gov) |
[NOAA Fisheries](https://www.fisheries.noaa.gov/)