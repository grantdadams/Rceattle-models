# Table2 and 3:
#   Pollock <40cm = RFR = 0.49, set foraging day = 365
#   Pollock >=40cm = RFR = 0.56, set foraging day = 365
#   
#   Pcod <55cm = RFR = 0.41, set foraging day = 365
#   Pcod >=55cm = RFR = 0.47, set foraging day = 353
#   
#   ATF <40cm = RFR = 0.79, set foraging day = 365
#   ATF >=40cm = RFR = 1.07, set foraging day = 348
#   
#   Holsman et al. 2018 table 4
#   ATF <40cm = RFR = 0.415, set foraging day = 365
#   ATF >=40cm = RFR = 0.47, set foraging day = 365
data_names <- c("GOA_18.3.1_small_pcod_removed_aaf_halibut_total_diet2.xlsx", "GOA_18.3.1_small_pcod_removed_coastwide_halibut_total_diet2.xlsx", "GOA_18.3.1_small_pcod_removed_survey_halibut_total_diet2.xlsx", "GOA_18.3.1_small_pcod_removed_coastwide_halibut_total_no_halibut_uobs.xlsx")

# Set parameters in table to index
size <- c(40, 55, 40, 40)
rfr <- data.frame(juv = c(0.49, 0.41, 0.79, 0.415), adult = c(0.56, 0.47, 1.07, 0.47))
forage_days <- data.frame(juv = rep(365, 4), adult = c(365, 353, 348, 365))

# Adjust data
for(i in 1:length(data_names)){
  
  mydata <- Rceattle::read_data( file = data_names[i])
  
  # Get weights
  weights <- mydata$wt[which(mydata$wt$Wt_index %in% mydata$pop_wt_index),]
  
  # Convert to lengths
  lengths <- weights
  pyrs <- weights
  
  # Change round wt (kg) to net wt (lbs) for halibut
  wt_mult <- ifelse(lengths$Species == 4, 0.75 * 2.20462 , 1)
  lt_mult <- ifelse(lengths$Species == 3, 10 , 1)
  
  # Convert wt to length and get pyrs
  for(l in 6:ncol(lengths)){
    lengths[, l] <- as.numeric(lt_mult * (weights[, l] * wt_mult / mydata$aLW[1,lengths$Species] )^(1/mydata$aLW[2,lengths$Species]))
    
    pyrs[, l] <- ifelse(lengths[, l] < size[pyrs$Species], rfr[pyrs$Species,1] * forage_days[pyrs$Species,1], rfr[pyrs$Species,2] * forage_days[pyrs$Species,2] )
  }
  
  # Assing to object and save
  mydata$Pyrs <- pyrs[,c(3,5,4,6:ncol(pyrs))]
  
  write_data(mydata, file = data_names[i])
}
