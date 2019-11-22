yrs <- ms_mod_list[[1]]$data_list$styr:ms_mod_list[[1]]$data_list$endyr
nyrs <- length(yrs)

par(mfrow = c(4,1), mar = c(2.2,4.5,0.2,0.5))

# Pollock
legend_vec <- c("a) Walleye pollock", "b) Pacific cod", "c) Female arrowtooth", "d) Male arrowtooth" )

for(i in 1:4){
  sp = i
  sex = 1
  
  if(i == 4){
    sp = 3
    sex = 2
  }
  
  ms_zed <- ms_mod_list[[1]]$quantities$Zed[sp,sex,1,1:nyrs]
  ms_zed_noh <- ms_mod_list[[7]]$quantities$Zed[sp,sex,1,1:nyrs]
  ss_zed <- ss_run_base$quantities$Zed[sp,sex,1,1:nyrs]
  all_vec <- c(ms_zed, ms_zed_noh, ss_zed)
  plot(y = ms_zed, x =  yrs, type = "l", lwd = 2, xlab = NA, ylab = NA, ylim = c(min(all_vec), max(all_vec) + .4))
  lines(y = ss_zed, x =  yrs, lty = 3, lwd = 2)
  lines(y = ms_zed_noh, x =  yrs, lty = 2, lwd = 2)
  
  legend("topleft", legend_vec[i], bty = "n", cex = 1.5)
  
  if(i == 1){
    legend("topright", c("MS-C avg", "MS-no halibut", "ss"), lty = c(1,2,3), bty = "n", cex = 1.5)
  }
  if(i == 2){
    mtext(side = 2, "Age 1 mortality (M1 + M2 + F)", line = 2.5)
  }
}


# Cod
ms_zed <- ms_mod_list[[1]]$quantities$Zed[2,1,1,1:nyrs]
ss_zed <- ss_run_base$quantities$Zed[2,1,1,1:nyrs]
plot(y = ms_zed, x =  yrs, type = "l", lwd = 2, xlab = NA, ylab = NA, ylim = c(0.35, 0.7))
abline(h = mean(ms_zed), lty = 2, lwd = 2)
lines(y = ss_zed, x =  yrs, lty = 3, lwd = 2)
legend("topleft", "b) Pacific cod", bty = "n", cex = 1.5)
legend("topright", c("ms-M", "avg M", "ss-M"), lty = c(1,2,3), bty = "n", cex = 1.5)

# Xlab
mtext(side = 2, "Age 1 mortality (M1 + M2 + F)", line = 2.5)

# Female ATF
ms_zed <- ms_mod_list[[1]]$quantities$Zed[3,1,1,1:nyrs]
ss_zed <- ss_run_base$quantities$Zed[3,1,1,1:nyrs]
plot(y = ms_zed, x =  yrs, type = "l", lwd = 2,ylab = NA, ylim = c(0.1, 0.4), xlab = "Year")
abline(h = mean(ms_zed), lty = 2, lwd = 2)
lines(y = ss_zed, x =  yrs, lty = 3, lwd = 2)
legend("topleft", "c) Female arrowtooth", bty = "n", cex = 1.5)
legend("topright", c("ms-M", "avg M", "ss-M"), lty = c(1,2,3), bty = "n", cex = 1.5)

# Male ATF
ms_zed <- ms_mod_list[[1]]$quantities$Zed[3,2,1,1:nyrs]
ss_zed <- ss_run_base$quantities$Zed[3,2,1,1:nyrs]
plot(y = ms_zed, x =  yrs, type = "l", lwd = 2,ylab = NA, ylim = c(0.3, 0.8), xlab = "Year")
abline(h = mean(ms_zed), lty = 2, lwd = 2)
lines(y = ss_zed, x =  yrs, lty = 3, lwd = 2)
legend("topleft", "c) Male arrowtooth", bty = "n", cex = 1.5)
legend("topright", c("ms-M", "avg M", "ss-M"), lty = c(1,2,3), bty = "n", cex = 1.5)


# Proportion of pollock M2
yrs <- mydata$styr : mydata$endyr
nyrs <- length(yrs)
par(mfrow = c(3,1), mar = c(2.2,4.5,0.2,0.5))
M2 <- ms_mod_list[[1]]$quantities$M2
M2_prop <- ms_mod_list[[1]]$quantities$M2_prop
legends <- c("a) Walleye pollock", "b) Pacific cod", "c) Arrowtooth F")

for(prey in 1:3){
  M2_prop_pol <- colSums(M2_prop[1,prey,1,1,,1,1:nyrs]) / M2[prey,1,1,1:nyrs]
  M2_prop_cod <- colSums(M2_prop[2,prey,1,1,,1,1:nyrs]) / M2[prey,1,1,1:nyrs]
  M2_prop_atff <- colSums(M2_prop[3,prey,1,1,,1,1:nyrs]) / M2[prey,1,1,1:nyrs]
  M2_prop_atfm <- colSums(M2_prop[3,prey,2,1,,1,1:nyrs]) / M2[prey,1,1,1:nyrs]
  
  plot(NA, NA, ylab = "Proportion of age-1 M2", xlab = "Year", ylim = c(0,1), xlim = range(yrs))
  lines(y = M2_prop_pol, x = yrs, lwd = 2)
  lines(y = M2_prop_cod, x = yrs, lwd = 2, lty = 2)
  lines(y = M2_prop_atff, x = yrs, lwd = 2, lty = 3)
  # lines(y = M2_prop_atfm, x = yrs, lwd = 2, lty = 4)
  legend("topleft", legends[prey], bty = "n", cex = 1.5)
  legend("topright", legend = c("Walleye pollock", "Pacific cod", "Arrowtooth"), lty = c(1, 2, 3), lwd = rep(2, 3), bty = "n")
}



# Figure 3 - biomass eaten
par(mfrow = c(3,1), mar = c(2.2,4.5,0.2,0.5))
# Pollock
ms_zed <- (ms_mod_list[[1]]$quantities$B_eaten[1,1,1,1:nyrs])/1000000
plot(y = ms_zed, x =  yrs, type = "l", lwd = 2, xlab = NA, ylab = NA, ylim = c(0, max(c(ms_zed))), xaxt = "n")
abline(h = mean(ms_zed), lty = 2, col = "grey70", lwd = 2)
legend("bottomright", "a) Walleye pollock", bty = "n", cex = 1.5)

# Cod
ms_zed <- (ms_mod_list[[1]]$quantities$B_eaten[2,1,1,1:nyrs])/1000000
plot(y = ms_zed, x =  yrs, type = "l", lwd = 2, xlab = NA, ylab = NA, ylim = c(0, max(c(ms_zed))), xaxt = "n")
abline(h =  mean(ms_zed), lty = 2, col = "grey70", lwd = 2)
legend("bottomright", "b) Pacific cod", bty = "n", cex = 1.5)

# Xlab
mtext(side = 2, "Age-1 biomass eaten by predators (million mt)", line = 2.5)

# ATF
ms_zed <- (ms_mod_list[[1]]$quantities$B_eaten[3,1,1,1:nyrs] + ms_mod_list[[1]]$quantities$B_eaten[3,2,1,1:nyrs])/1000000
plot(y = ms_zed, x =  yrs, type = "l", lwd = 2, xlab = NA, ylab = NA, ylim = c(0, max(c(ms_zed))))
abline(h = mean(ms_zed), lty = 2, col = "grey70", lwd = 2)
legend("bottomright", "c) Arrowtooth flounder", bty = "n", cex = 1.5)
