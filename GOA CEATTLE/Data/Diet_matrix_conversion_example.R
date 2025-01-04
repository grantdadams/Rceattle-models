# Problem - convert length-based proportion to age-based

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
rownames(PropPreyLength) <- paste0("PreyLength",  1:PreyLengths)
colnames(PropPreyLength) <- paste0("PredLength",  1:PredLengths)

colSums(PropPreyLength) # Note: Column sums should not sum to 1 because there are other prey species/lengths not accounted for

# This is wrong
PropPreyAge <- t(alk_prey) %*% PropPreyLength %*% alk_pred
PropPreyAge

colSums(PropPreyAge) # Col sums should be less than 1

