# Problem - convert length to age
# Age to length
ALK1 <- matrix(c(1,0,0,0,0,0,
                       0.7,0.2,0.1,0,0,0,
                       0,0.1,0.2, 0.2,0.2,0.3), 3, 6, byrow = TRUE)
rownames(ALK1) <- paste0("Age", 1:3)
colnames(ALK1) <- paste0("Length", 1:6)

n_at_age <- c(1:3)

# Get n-at-length
n_at_length <- n_at_age %*% ALK1

# Get age from length
n_at_age2 <- ALK1 %*% c(n_at_length) # Doesnt work
n_at_age2 != n_at_age

ALK1_standardized <- t(t(ALK1)/colSums(ALK1))# Divide each column by sum of that column
n_at_age2 <- ALK1_standardized %*% c(n_at_length) # Doesnt work
n_at_age2 != n_at_age


age_2_length_pred

length_2_age_pred <- (matrix(c(1,0,0,
                                 0.8,0.2,0,
                               0.3,0.5,0.2,
                               0.1,0.3,0.6,
                               0,0.2,0.8,
                               0,0,1), 6,3, byrow = TRUE))
colnames(length_2_age_pred) <- paste0("PredAge", 1:3)
rownames(length_2_age_pred) <- paste0("PredLength", 1:6)

# - Prey
age_2_length_prey <- matrix(c(1,0,0,0,
                               0.7,0.2,0.1,0,
                               0,0.2,0.3, 0.5,
                               0,0,0.4,0.6,
                               0,0,0,1), 5, 4, byrow = TRUE)
rownames(age_2_length_prey) <- paste0("PreyAge", 1:5)
colnames(age_2_length_prey) <- paste0("PreyLength", 1:4)
age_2_length_prey

length_2_age_prey <- (matrix(c(1,0,0,0,0,
                                  0.7,0.2,0.1,0,0,
                                  0,0,0.2,0.3, 0.5,
                                  0,0,0,0.4,0.6), 4, 5, byrow = TRUE))
colnames(length_2_age_prey) <- paste0("PreyAge", 1:5)
rownames(length_2_age_prey) <- paste0("PreyLength", 1:4)


n_at_age_pred <- 3:1
n_at_age_prey <- 5:1


n_at_age_pred %*% age_2_length_pred
sum(n_at_age_pred %*% age_2_length_pred)

n_at_age_pred %*% t(length_2_age_pred)
sum(n_at_age_pred %*% t(length_2_age_pred))

# Prey comp by length
PropPreyLength <- matrix(runif(4*6, 0,0.3), ncol = 6, nrow = 4)
rownames(PropPreyLength) <- paste0("PreyLength", 1:4)
colnames(PropPreyLength) <- paste0("PredLength", 1:6)
PropPreyLength

PropPreyAge <- matrix(0, nrow = 5, ncol = 3)
rownames(PropPreyAge) <- paste0("PreyAge", 1:5)
colnames(PropPreyAge) <- paste0("PredAge", 1:3)
PropPreyAge2 <-  PropPreyAge # Hal pred, other prey
PropPreyAge3 <-  PropPreyAge # Hal prey, other pred
PropPreyAge4 <-  PropPreyAge # Hal pred and prey

for(pred_l in 1:6){
  for(prey_l in 1:4){
    for(prey_a in 1:5){
      for(pred_a in 1:3){
        PropPreyAge[prey_a, pred_a] <- PropPreyAge[prey_a, pred_a] + PropPreyLength[prey_l, pred_l] * age_2_length_pred[pred_a, pred_l] * age_2_length_prey[prey_a, prey_l]
        
        PropPreyAge2[prey_a, pred_a] <- PropPreyAge2[prey_a, pred_a] + PropPreyLength[prey_l, pred_l] * length_2_age_pred[pred_l, pred_a] * age_2_length_prey[prey_a, prey_l] # Hal pred
        
        PropPreyAge3[prey_a, pred_a] <- PropPreyAge3[prey_a, pred_a] + PropPreyLength[prey_l, pred_l] * age_2_length_pred[pred_a, pred_l] * length_2_age_prey[prey_l, prey_a] # Hal prey
        
        PropPreyAge4[prey_a, pred_a] <- PropPreyAge4[prey_a, pred_a] + PropPreyLength[prey_l, pred_l] * length_2_age_pred[pred_l, pred_a] * length_2_age_prey[prey_l, prey_a] # Hal pred and prey
      }
    }
  }
}

PropPreyAge
age_2_length_prey %*% PropPreyLength %*% t(age_2_length_pred)

PropPreyAge2 # Hal pred
age_2_length_prey %*% PropPreyLength %*% t(t(length_2_age_pred))

PropPreyAge3 # Hal prey
t(length_2_age_prey) %*% PropPreyLength %*% t(age_2_length_pred)

PropPreyAge4 # Hal pred and prey
t(length_2_age_prey) %*% PropPreyLength %*% t(t(length_2_age_pred))
sum(t(length_2_age_prey) %*% PropPreyLength %*% length_2_age_pred)


pollock_alk <- (pollock_alk[,c(6:12)])
pollock_alk <- as.matrix(pollock_alk)
n_at_length <- c(1:7)


n_at_age <- t(t(pollock_alk)/colSums(pollock_alk)) %*% n_at_length
