srv_comp_ctl <- cbind( mydata$srv_comp[which(mydata$srv_comp$Survey_code > 4), 1:8])
srv_sel <- ss_run$quantities$srv_sel
NByage <- ss_run$quantities$NByage
srv_q <- ss_run$quantities$srv_q
Zed <- ss_run$quantities$Zed
age_error <- mydata$age_error


age_trans_matrix <- mydata$age_trans_matrix

srv_comp_hat <- ss_run$quantities$srv_age_obs_hat[which(mydata$srv_comp$Survey_code > 4),]
srv_comp_hat[,] <- 0

srv_age_obs_hat <- ss_run$quantities$srv_age_obs_hat[which(mydata$srv_comp$Survey_code > 4),]
srv_age_hat <- ss_run$quantities$srv_age_hat[which(mydata$srv_comp$Survey_code > 4),]

nyrs_hind <- mydata$endyr - mydata$styr + 1
srv_hat <- c()

for(comp_ind in 1: nrow(srv_comp_hat)){
  
  srv = srv_comp_ctl[comp_ind, 2];            # Temporary survey index
  sp = srv_comp_ctl[comp_ind, 3];             # Temporary index of species
  sex = srv_comp_ctl[comp_ind, 4];                # Temporary index for comp sex [0 = combined, 1 = female, 2 = male]
  srv_comp_type = srv_comp_ctl[comp_ind, 5];      # Temporary index for comp type [0 = age, 1 = length]
  srv_yr = srv_comp_ctl[comp_ind, 6] - mydata$styr + 1;      # Temporary index for years of data
  mo = srv_comp_ctl[comp_ind, 7];                   # Temporary index for month
  
  if(srv_yr < nyrs_hind){
    
    srv_hat[comp_ind] = 0;                       # Initialize
    
    # Total numbers
    for (age in 1:mydata$nages[sp]) {
      srv_age_hat[comp_ind, age ] = NByage[sp, age, srv_yr]  * srv_sel[srv, age, srv_yr] * srv_q[srv] * exp( - mo/12 * Zed[sp, age, srv_yr]);
      srv_hat[comp_ind] = srv_hat[comp_ind] +  srv_age_hat[comp_ind, age ];   # Total numbers
    }
    
    # Add in aging error
    for (obs_age in 1:mydata$nages[sp]) {
      srv_age_obs_hat[comp_ind, obs_age] = 0 
      for (true_age in 1:mydata$nages[sp]) {
        srv_age_obs_hat[comp_ind, obs_age] = srv_age_obs_hat[comp_ind, obs_age] + srv_age_hat[comp_ind, true_age ] * age_error[sp, true_age, obs_age]
      }
    }
    
    
    #  Normalize survey catch-at-age
    if (srv_comp_type == 0) {
      for (age in 1:mydata$nages[sp]) {
        srv_comp_hat[comp_ind, age] = srv_age_obs_hat[comp_ind, age] / srv_hat[comp_ind];
      }
    }
    
    # Convert from catch-at-age to catch-at-length and normalize
    if ( srv_comp_type == 1) {
      for (ln in 1:mydata$nlengths[sp]) {
        srv_comp_hat[comp_ind, ln ] = 0
        for (age in 1:mydata$nages[sp]) {
          srv_comp_hat[comp_ind, ln ] = srv_comp_hat[comp_ind, ln ] + srv_age_obs_hat[comp_ind, age] * age_trans_matrix[age, ln, srv_alk_index[sp]];
        }
      }
      
      # Normalize
      for (ln in 1:mydata$nlengths[sp]) {
        # srv_comp_hat[comp_ind, ln] = srv_comp_hat[comp_ind, ln] / srv_hat[comp_ind];
      }
    }
  }
}
