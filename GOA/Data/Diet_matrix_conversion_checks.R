# Problem - convert length-based proportion to age-based
library(reshape2)
library(tidyr)

# - Predator - 3 ages,  6 lengths
# - Prey - 5 ages,  10 lengths
PredLengths = 6
PredAges = 3

PreyLengths = 10
PreyAges = 4

# - Generate random Alks
# -- Pred
alk_pred <- matrix(rbeta(PredLengths * PredAges,2,2), nc = PredAges)
alk_pred <- sweep(alk_pred, 1, rowSums(alk_pred), FUN="/") # Divide by rowSums to make prob of age conditional on length
colnames(alk_pred) <- paste0("PredAge",  1:PredAges)
rownames(alk_pred) <- paste0("PredLength",  1:PredLengths)
rowSums(alk_pred) # Should be 1

# -- Prey
alk_prey <- matrix(rbeta(PreyLengths * PreyAges,2,2), nc = PreyAges)
alk_prey <- sweep(alk_prey, 1, rowSums(alk_prey), FUN="/") # Divide by rowSums to make prob of age conditional on length
colnames(alk_prey) <- paste0("PreyAge",  1:PreyAges)
rownames(alk_prey) <- paste0("PreyLength",  1:PreyLengths)
rowSums(alk_prey) # Should be 1

# Proportion prey-at-length in stomach of predator-at-length,  conditional of predator-at-length relative to all other prey
PropPreyLength <- matrix(runif(PreyLengths*PredLengths,  0, (PreyLengths:1)/(PreyLengths*4)),  ncol = PredLengths,  nrow = PreyLengths, byrow = FALSE) # Trying to simulate smaller percentage with larger prey age
CountPreyLength <- matrix(1,  ncol = PredLengths,  nrow = PreyLengths, byrow = FALSE) 

rownames(PropPreyLength) <- paste0("PreyLength",  1:PreyLengths)
colnames(PropPreyLength) <- paste0("PredLength",  1:PredLengths)
rownames(CountPreyLength) <- paste0("PreyLength",  1:PreyLengths)
colnames(CountPreyLength) <- paste0("PredLength",  1:PredLengths)


colSums(PropPreyLength) # Note: Column sums should not sum to 1 because there are other prey species/lengths not accounted for

# This is wrong
PropPreyAge1 <- t(alk_prey) %*% PropPreyLength %*% alk_pred
CountPreyAge <- t(alk_prey) %*% CountPreyLength %*% alk_pred

colSums(PropPreyAge1) # Col sums should be less than 1

# Weighted mean?
# - Make the ALKs long
alk_prey_df <- as.data.frame(alk_prey)
colnames(alk_prey_df) <- 1:PreyAges
alk_prey_df$PreyLength = 1:PreyLengths
alk_prey_df <- alk_prey_df %>% pivot_longer(1:PreyAges, names_to = "PreyAge",values_to = "PreyAgeLengthProb")

alk_pred_df <- as.data.frame(alk_pred)
colnames(alk_pred_df) <- 1:PredAges
alk_pred_df$PredLength = 1:PredLengths
alk_pred_df <- alk_pred_df %>% pivot_longer(1:PredAges, names_to = "PredAge",values_to = "PredAgeLengthProb")

# - All combo of pred lengths and prey lengths
alk_long <- merge(alk_pred_df, alk_prey_df)
alk_long <- alk_long[with(alk_long, order(PredLength, PredAge, PreyLength, PreyAge)),]

# - Make propLength long
PropPreyLength_df <- as.data.frame(PropPreyLength)
colnames(PropPreyLength_df) <- 1:PredLengths
PropPreyLength_df$PreyLength = 1:PreyLengths
PropPreyLength_df <- PropPreyLength_df %>% pivot_longer(1:PredLengths, names_to = "PredLength",values_to = "PropLength")

# - Merge propLength with ALKs
propLengthLong <- merge(alk_long, PropPreyLength_df, all = TRUE)
propLengthLong <- propLengthLong[with(propLengthLong, order(PredLength, PredAge, PreyLength, PreyAge)),]

# - Take weighted mean
propAge1 = propLengthLong %>%
  group_by(PredAge, PreyAge) %>%
  summarize(propAge = weighted.mean(PropLength, PredAgeLengthProb * PreyAgeLengthProb))

# - Take mean, multiplied by probs
propAge2 = propLengthLong %>%
  group_by(PredAge, PreyAge) %>%
  summarize(propAge = mean(PropLength * PredAgeLengthProb * PreyAgeLengthProb))

# - Make into matrix
PropPreyAge2 <- propAge1 %>% pivot_wider(names_from = PredAge, values_from = propAge)
colnames(PropPreyAge2) <- c("PreyAge", paste0("PredAge",1:PredAges))
colSums(PropPreyAge2)

PropPreyAge3 <- propAge2 %>% pivot_wider(names_from = PredAge, values_from = propAge)
colnames(PropPreyAge3) <- c("PreyAge", paste0("PredAge",1:PredAges))
colSums(PropPreyAge3)

PropPreyAge4 <- PropPreyAge1/CountPreyAge



#### Compare
round(PropPreyAge1,3) # Multiplied matrices
round(PropPreyAge2[,2:4],3) # Weighted mean
round(PropPreyAge3[,2:4],3) # "Scaled" mean
round(PropPreyAge4,3) # Weighted mean

