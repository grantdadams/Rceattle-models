library(Rceattle)
library(readxl)
setwd("Model runs/GOA_18.5.1/")

################################################
# Data
################################################
# Read the data in
mydata_atf_fixed <- Rceattle::read_data( file = "Data/GOA_18.5.1_arrowtooth_single_species_1970-2018.xlsx")
safe2018est <- as.data.frame(read_xlsx("Data/ATF Tests/2018_SAFE_atf_parameters.xlsx", sheet = 1))
safe2018estMain <- as.data.frame(read_xlsx("Data/ATF Tests/2018_SAFE_atf_parameters.xlsx", sheet = 2))
# Fishery - Double logistic, random walk ascending params
# Survey 1 - Descending logistic, random walk q
# Survey 2 - Logistic, prior on q
# Survey 3 - Logistic, random walk q
# Survey 4 - Selectivity = 1 for age 1, single q
# Survey 5 - Selectivity  = 1 for age 2, single q
# Survey 6 - Selectivity  = 1 for all ages, single q


#######################################
# Mod 0 - Estimate
#######################################
mydata_atf_est <- mydata_atf_fixed
mydata_atf_est$estDynamics = 0

dat <- rearrange_dat(mydata_atf_est)
dat$pop_wt_index
dat$ssb_wt_index

atf_est <- Rceattle::fit_mod(
  cpp_directory = "C:/Users/Grant Adams/Documents/GitHub/Rceattle/inst/executables",
  data_list = mydata_atf_est,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  debug = FALSE, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  silent = TRUE,
  recompile = FALSE,
  phase = "default")
plot_biomass(atf_est)

#######################################
# Mod 2 - Fix n-at-age and parameters - Check likelihoods
#######################################
mydata_atf_fixed <- mydata_atf_fixed
mydata_atf_fixed$estDynamics = 0

# Scale n-at-age to vals
mydata_atf_fixed$msmMode = 0
inits <- build_params(mydata_atf_fixed)

# Fishery selectivity - Non-parametric
inits$sel_coff[1,1:19] <- c(-4.65087351891, -3.75901911880, -2.88721410018, -2.08094554736, -1.39011040645, -0.829995544198, -0.375593174815, -0.00125280471673, 0.276509770849, 0.425214562398, 0.451794548745, 0.444573389256, 0.433263388997, 0.420785121459, 0.408655239792, 0.397245031178, 0.387001735832, 0.377863662879, 0.369603901535)
inits$sel_coff[2,1:19] <- c(-4.35903772245, -3.51769871061, -2.68753068312, -1.89414463489, -1.17342708695, -0.562419140903, -0.0901839194654, 0.230049485795, 0.404394136214, 0.461851259951, 0.453394143228, 0.426415480051, 0.393511946438, 0.359302151175, 0.326174154244, 0.295704889235, 0.269115063399, 0.247647496296, 0.233459438294)

# Fishing mortality
inits$ln_mean_F[2] <- -4.50387519715
inits$F_dev[2,] <- c(-2.10118573832, -2.08609460688, -2.07238231061, -2.06153201768, -2.05425215475, -0.478702455147, -0.554059609917, -0.842930032347, -1.09310650402, -0.725258859188, -1.18097262406, 0.161044900859, 0.987223711945, 0.286614090823, -0.280016927229, -0.205809302710, 0.898169196703, 0.739521803541, 0.573780621201, 0.533751335827, 0.397891658976, -0.158772136148, 0.0682657340178, -0.618072494943, -1.50607467815, -1.80756793220, -0.461027239290, -0.468819449538, -1.19352794105, -0.137203438670, 0.0892802018383, 0.515014341803, 0.450957213491, 0.835436982352, 0.567635305760, 0.754611773107, 0.420270563691, 0.185132760047, 0.407125221340, 0.817849584284, 0.632051274971, 0.693441583899, 1.02999120951, 0.331551923921, 0.547692253355, 0.846605872459, 0.736627174417, 0.858810526778, 0.695620271202, 0.669992593886, 0.940512939417, 0.553316796356, 0.630390169204, 1.19224912175, 0.598489973600, 0.685794462012, 0.754653304508)

# Recruitment
inits$ln_mn_rec = 19.4535948326
inits$rec_dev[1:58] <- c(-0.739710902648, -0.237138698571, -0.268353357901, -0.301867192388, -0.337505720529, -0.375031912909, -0.414088611502, -0.454340440749, -0.495357027284, -0.536600282121, -0.577367504712, -0.616819962668, -0.653608862842, -0.685885882083, -0.711623905743, -0.706079505905, -0.692292752822, -0.647376957152, -0.584069818184, -0.551065401423, -0.560477810220, -0.588151167004, -0.625849890608, -0.659791642825, -0.693804018794, -0.718442253025, -0.730745178336, -0.703450418372, -0.640373884111, -0.526387774500, -0.371823217485, -0.165854200884, 0.100405270028, 0.164069868753, 0.0363368301871, 0.210413173685, 0.306921959568, 0.446140456923, 0.543776378424, 0.446017997671, 0.268735102643, 0.191866395446, 0.215939022922, 0.453604639234, 0.734287225895, 0.564850056242, 0.599553011941, 0.741336430642, 0.605042509596, 0.475610843795, 0.622614043434, 0.519527794187, 0.289041744625, 0.353741926375, 0.389949546224, 0.382605126510, 0.651344120618, 0.837038020887, 0.873969353410, 1.14567938117, 0.692245495666, 0.622527730009, 0.513015167763, 0.590386638062, 0.600923019971, 0.566808345383, 0.218103299351, 0.210533753689, -0.109161997485, -0.125284188942, 0.105061535089, 0.293061518087, 0.269651324027, 0.127461439969, -0.264736348518, 0.0903210770001, 1.16145763022e-007)

# Survey 1 - Descending logistic, random walk q
inits$ln_sel_slp[2,1,1] <- safe2018estMain$log_slp2_srv1
inits$sel_inf[2,1,1] <- safe2018estMain$inf2_srv1
inits$ln_srv_q[1] <- safe2018estMain$log_q1_mean
inits$ln_srv_q_dev[1,] <- safe2018est$log_q1_dev

# fishsel_params_f[1]:
2.54851086776
# fishsel_params_f[2]:
4.48722728638
# fishsel_params_m[1]:
1.02306194559
# fishsel_params_m[2]:
11.3489233262
# srv_params_f[1]:
1.29423268707
# srv_params_f[2]:
3.25807092424
# srv_params_m[1]:
0.499999956106
# srv_params_m[2]:
4.48573166827
# srv1desc_params_f[1]:
2.54846729550
# srv1desc_params_f[2]:
5.50152564450
# srv1desc_params_m[1]:
2.50323938448
# srv1desc_params_m[2]:

library(Rceattle)
atf_fixed <- Rceattle::fit_mod(data_list = mydata_atf_fixed,
                               inits = inits, # Initial parameters = 0
                               file = NULL, # Don't save
                               debug = 1, # Estimate
                               random_rec = FALSE, # No random recruitment
                               msmMode = 0, # Single species mode
                               silent = TRUE,
                               recompile = FALSE,
                               phase = "default")
round(atf_fixed$quantities$jnll_comp,4)[1:8,1:6]

safe_jnll <- read.csv( file = "Data/2018_SAFE_nll_components.csv")
rownames(safe_jnll) <- safe_jnll[,1]
safe_jnll = safe_jnll[,-1]
round(safe_jnll,4)[1:8,1:6]

# Save bits
srv_biom <- atf_fixed$data_list$srv_biom
srv_biom$est <- atf_fixed$quantities$srv_bio_hat

# Look at index
library(readxl)
safe_2018_index <- as.data.frame(read_xlsx("Data/2018_safe_expected_survey.xlsx", sheet = 1))
srv_biom$SAFE <- NA
index_cols <- data.frame(Index = c(7,1,2,3,4,5,6), Col = c(2,3,4,5,6,7,8))
for(i in 1:nrow(index_cols)){
  sub <- which(srv_biom$Fleet_code == index_cols$Index[i])
  yrs <- srv_biom$Year[sub]
  bio_hat <- safe_2018_index[which(safe_2018_index$Year %in% yrs),index_cols$Col[i]]
  srv_biom$SAFE[sub] <- bio_hat
}
srv_biom
srv_biom$RE <- (srv_biom$est - srv_biom$SAFE)/srv_biom$SAFE
write.csv(srv_biom, file = "srv_biom.csv")


# Check selectivities
# Srv 1 is all good

# Srv 2
# - Srv selectivity for srv 2 is good
atf_fixed$quantities$sel[2,1,,1] - c(0.130663, 0.21864, 0.343155, 0.495111, 0.6503, 0.782476, 0.878728, 0.941117, 0.978559, 1)

# - Catchability is good
atf_fixed$quantities$srv_q[2,] - 0.847681

# Srv 3
# - Srv selectivity for srv 3 is good
atf_fixed$quantities$sel[3,1,,1] - c( 0.00561133, 0.0229488, 0.0890586, 0.28926, 0.628943, 0.87613, 0.967484, 0.992343, 0.998508, 1)

# - Catchability is good
atf_fixed$quantities$srv_q[3,] - c(0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.635507, 0.634447, 0.627027, 0.633928, 0.639367, 0.650636, 0.671494, 0.695191, 0.720276, 0.752022, 0.781426, 0.814788, 0.849029, 0.87149, 0.888081, 0.88689, 0.881087, 0.868459, 0.854833, 0.851308, 0.844487, 0.823881, 0.775554, 0.722421, 0.666528, 0.599173, 0.54263, 0.489501, 0.449416, 0.422951, 0.421468)


# Srv 4
# - Srv selectivity for srv 4 is good
atf_fixed$quantities$sel[4,1,,1]

# - Catchability is not good
atf_fixed$quantities$srv_q[4,1] - 0.335939


# Srv 5
# - Srv selectivity for srv 5 is good
atf_fixed$quantities$sel[5,1,,1]

# - Catchability is not good
atf_fixed$quantities$srv_q[5,1] - 0.418653


# Srv 6
# - Srv selectivity for srv 6 is good
atf_fixed$quantities$sel[6,1,,1] # All 1s

# - Catchability is good
atf_fixed$quantities$srv_q[6,] - 0.82806

# Something is up with surveys 2,3,6
# Selectivities are correct
# Catchabilities are correct




comp_hat <- mydata_atf_fixed$comp_data[,1:8]
comp_hat <- cbind(comp_hat, atf_fixed$quantities$comp_hat)

age_hat <- mydata_atf_fixed$comp_data[,1:8]
age_hat <- cbind(age_hat, atf_fixed$quantities$age_hat)

write.csv(comp_hat, file = "comp_hat.csv")
write.csv(t(atf_fixed$quantities$NByage[1,1,,]), file = "n_hat.csv")
write.csv(t(atf_fixed$quantities$Zed[1,1,,]), file = "zed_hat.csv")
write.csv(age_hat, file = "c_hat.csv")

# Last item, q3 dev like
sum(0.5*(((safe2018est$log_q3_dev[2:49] - safe2018est$log_q3_dev[1:48])/0.05)^2))





plot_biomass(list(atf_fixed, atf_est), model_names = c("F", "E"))
