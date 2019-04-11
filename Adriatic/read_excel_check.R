read_excel <- function( file = "Rceattle_data.xlsx" ){
  '%!in%' <- function(x,y)!('%in%'(x,y))
  ## setup a list object
  
  data_list <- list()
  
  
  sheet1 <- readxl::read_xlsx( file, sheet = "control")
  sheet1 <- as.data.frame(sheet1)
  
  # control
  data_list$nspp <- sheet1[1,2]
  data_list$styr <- sheet1[2,2]
  data_list$endyr <- sheet1[3,2]
  data_list$projyr <- sheet1[4,2]
  data_list$nages <- as.numeric(as.character(sheet1[5, 2:(data_list$nspp + 1)]))
  data_list$nlengths <- as.numeric(as.character(sheet1[6, 2:(data_list$nspp + 1)]))
  data_list$pop_wt_index <- sheet1[7, 2:(data_list$nspp + 1)]
  data_list$pop_alk_index <- sheet1[8, 2:(data_list$nspp + 1)]
  data_list$other_food <- sheet1[9, 2:(data_list$nspp + 1)]
  data_list$stom_tau <- sheet1[10, 2:(data_list$nspp + 1)]
  
  
  # srv and fsh bits
  srv_bits <- c("srv_control", "srv_biom", "srv_emp_sel", "srv_comp", "fsh_control", "fsh_biom", "fsh_emp_sel", "fsh_comp")
  for(i in 1:length(srv_bits)){
    sheet <- as.data.frame(readxl::read_xlsx( file, sheet = srv_bits[i]))
    sheet <- sheet[rowSums(is.na(sheet)) != ncol(sheet), ]
    
    data_list[[srv_bits[i]]] <- sheet
  }
  
  data_list$srv_control$Nselages <- suppressWarnings(as.numeric(data_list$srv_control$Nselages))
  data_list$fsh_control$Nselages <- suppressWarnings(as.numeric(data_list$fsh_control$Nselages))
  
  # age_trans_matrix
  age_trans_matrix <- as.data.frame(readxl::read_xlsx( file, sheet = "age_trans_matrix"))
  unique_alk <- unique(as.character(age_trans_matrix$ALK_index))
  alk <- array(NA, dim = c(max(data_list$nages, na.rm = T), max(data_list$nlengths, na.rm = T), length(unique_alk)))
  
  
  for(i in 1:nrow(age_trans_matrix)){
    
    alk_ind <- as.numeric(as.character(age_trans_matrix$ALK_index[i]))
    age <- as.numeric(as.character(age_trans_matrix$Age[i]))
    sp <- as.numeric(as.character(age_trans_matrix$Species[i]))
    
    alk[age, 1:data_list$nlengths[sp], alk_ind] <- as.numeric(as.character(age_trans_matrix[i, (1:data_list$nlengths[sp]) + 3]))
  }
  data_list$age_trans_matrix <- alk
  
  
  
  # age_error
  age_error <- as.data.frame(readxl::read_xlsx( file, sheet = "age_error"))
  arm <- array(NA, dim = c(data_list$nspp, max(data_list$nages, na.rm = T), max(data_list$nages, na.rm = T)))
  
  
  for(i in 1:nrow(age_error)){
    true_age <- as.numeric(as.character(age_error$True_age[i]))
    sp <- as.numeric(as.character(age_error$Species[i]))
    
    arm[sp, true_age, 1:data_list$nages[sp]] <- as.numeric(as.character(age_error[i, (1:data_list$nages[sp]) + 2]))
  }
  data_list$age_error <- arm
  
  
  
  # wt
  wt_matrix <- as.data.frame(readxl::read_xlsx( file, sheet = "wt"))
  unique_wt <- unique(as.character(wt_matrix$Wt_index))
  wt <- array(NA, dim = c(length(data_list$styr:data_list$endyr), max(data_list$nages, na.rm = T), length(unique_wt)))
  
  
  for(i in 1:nrow(wt_matrix)){
    
    wt_ind <- as.numeric(as.character(wt_matrix$Wt_index[i]))
    sp <- as.numeric(as.character(wt_matrix$Species[i]))
    yr <- as.numeric(as.character(wt_matrix$Year[i])) - data_list$styr + 1
    
    wt[yr, 1:data_list$nages[sp], wt_ind] <- as.numeric(as.character(wt_matrix[i, (1:data_list$nages[sp]) + 4]))
  }
  data_list$wt <- wt
  
  
  
  # pmature
  pmature <- as.data.frame(readxl::read_xlsx( file, sheet = "pmature"))
  data_list$pmature <- pmature
  
  
  # propF
  propF <- as.data.frame(readxl::read_xlsx( file, sheet = "propF"))
  data_list$propF <- propF
  
  
  # M1_base
  M1_base <- as.data.frame(readxl::read_xlsx( file, sheet = "M1_base"))
  data_list$M1_base <- M1_base
  
  
  # Mn_LatAge
  Mn_LatAge <- as.data.frame(readxl::read_xlsx( file, sheet = "Mn_LatAge"))
  data_list$Mn_LatAge <- Mn_LatAge
  
  
  # aLW
  aLW <- as.data.frame(readxl::read_xlsx( file, sheet = "aLW"))
  data_list$aLW <- aLW
  
  
  # bioenergetics_control
  bioenergetics_control <- as.data.frame(readxl::read_xlsx( file, sheet = "bioenergetics_control"))
  
  for(i in 1:nrow(bioenergetics_control)){
    data_list[[bioenergetics_control$Object[i]]] <- as.numeric(as.character(bioenergetics_control[i,((1:data_list$nspp) + 1)]))
  }
  
  
  
  # Temperature stuff
  Temp_data <- as.data.frame(readxl::read_xlsx( file, sheet = "Temp_data"))
  data_list$Tyrs <- Temp_data$Tyrs
  data_list$BTempC <- Temp_data$BTempC
  
  
  
  
  # Diet information
  # Pyrs
  pyrs_matrix <- as.data.frame(readxl::read_xlsx( file, sheet = "Pyrs"))
  Pyrs <- array(NA, dim = c( length(data_list$styr:data_list$endyr) , max(data_list$nages), data_list$nspp ))
  
  
  for(i in 1:nrow(pyrs_matrix)){
    sp <- as.numeric(as.character(pyrs_matrix$Species[i]))
    yr <- as.numeric(as.character(pyrs_matrix$Year[i])) - data_list$styr + 1
    
    Pyrs[yr, 1:data_list$nages[sp], sp] <- as.numeric(as.character(pyrs_matrix[i, (1:data_list$nages[sp]) + 2]))
  }
  data_list$Pyrs <- Pyrs
  
  
  
  # Diet UobsAge
  UobsAge_matrix <- as.data.frame(readxl::read_xlsx( file, sheet = "UobsAge"))
  
  # without year
  if(ncol(UobsAge_matrix) == 5){
    UobsAge <- array(0, dim = c( data_list$nspp, data_list$nspp, max(data_list$nages), max(data_list$nages)))
    dims <- dim(data_list$UobsAge)
    
    for(i in 1:nrow(UobsAge_matrix)){
      pred <- as.numeric(as.character(UobsAge_matrix$Pred[i]))
      prey <- as.numeric(as.character(UobsAge_matrix$Prey[i]))
      pred_age <- as.numeric(as.character(UobsAge_matrix$Pred_age[i]))
      prey_age <- as.numeric(as.character(UobsAge_matrix$Prey_age[i]))
      stom <- as.numeric(as.character(UobsAge_matrix$Stomach_proportion_by_number[i]))
      
      UobsAge[pred, prey, pred_age, prey_age] <- stom
    }
    
    data_list$UobsAge <- UobsAge
  }
  
  # with year
  if(ncol(UobsAge_matrix) == 6){
    UobsAge <- array(0, dim = c( data_list$nspp, data_list$nspp, max(data_list$nages), max(data_list$nages)))
    dims <- dim(data_list$UobsAge)
    
    for(i in 1:nrow(UobsAge_matrix)){
      pred <- as.numeric(as.character(UobsAge_matrix$Pred[i]))
      prey <- as.numeric(as.character(UobsAge_matrix$Prey[i]))
      pred_age <- as.numeric(as.character(UobsAge_matrix$Pred_age[i]))
      prey_age <- as.numeric(as.character(UobsAge_matrix$Prey_age[i]))
      year <- as.numeric(as.character(UobsAge_matrix$Year[i]))
      stom <- as.numeric(as.character(UobsAge_matrix$Stomach_proportion_by_number[i]))
      
      UobsAge[pred, prey, pred_age, prey_age, year] <- stom
    }
    
    data_list$UobsAge <- UobsAge
  }
  
  
  # Diet UobsWtAge
  UobsWtAge_matrix <- as.data.frame(readxl::read_xlsx( file, sheet = "UobsWtAge"))
  
  # without year
  if(ncol(UobsWtAge_matrix) == 5){
    UobsWtAge <- array(0, dim = c( data_list$nspp, data_list$nspp, max(data_list$nages), max(data_list$nages)))
    dims <- dim(data_list$UobsWtAge)
    
    for(i in 1:nrow(UobsWtAge_matrix)){
      pred <- as.numeric(as.character(UobsWtAge_matrix$Pred[i]))
      prey <- as.numeric(as.character(UobsWtAge_matrix$Prey[i]))
      pred_age <- as.numeric(as.character(UobsWtAge_matrix$Pred_age[i]))
      prey_age <- as.numeric(as.character(UobsWtAge_matrix$Prey_age[i]))
      stom <- as.numeric(as.character(UobsWtAge_matrix$Stomach_proportion_by_weight[i]))
      
      UobsWtAge[pred, prey, pred_age, prey_age] <- stom
    }
    
    data_list$UobsWtAge <- UobsWtAge
  }
  
  # with year
  if(ncol(UobsWtAge_matrix) == 6){
    UobsWtAge <- array(0, dim = c( data_list$nspp, data_list$nspp, max(data_list$nages), max(data_list$nages)))
    dims <- dim(data_list$UobsWtAge)
    
    for(i in 1:nrow(UobsWtAge_matrix)){
      pred <- as.numeric(as.character(UobsWtAge_matrix$Pred[i]))
      prey <- as.numeric(as.character(UobsWtAge_matrix$Prey[i]))
      pred_age <- as.numeric(as.character(UobsWtAge_matrix$Pred_age[i]))
      prey_age <- as.numeric(as.character(UobsWtAge_matrix$Prey_age[i]))
      year <- as.numeric(as.character(UobsWtAge_matrix$Year[i]))
      stom <- as.numeric(as.character(UobsWtAge_matrix$Stomach_proportion_by_weight[i]))
      
      UobsWtAge[pred, prey, pred_age, prey_age, year] <- stom
    }
    
    data_list$UobsWtAge <- UobsWtAge
  }
  
  
  # START HERE!
  
  
  # Diet Uobs
  Uobs_matrix <- as.data.frame(readxl::read_xlsx( file, sheet = "Uobs"))
  
  # without year
  if(ncol(Uobs_matrix) == 5){
    Uobs <- array(0, dim = c( data_list$nspp, data_list$nspp, max(data_list$nlengths), max(data_list$nlengths)))
    dims <- dim(data_list$Uobs)
    
    for(i in 1:nrow(Uobs_matrix)){
      pred <- as.numeric(as.character(Uobs_matrix$Pred[i]))
      prey <- as.numeric(as.character(Uobs_matrix$Prey[i]))
      pred_length <- as.numeric(as.character(Uobs_matrix$Pred_length[i]))
      prey_length <- as.numeric(as.character(Uobs_matrix$Prey_length[i]))
      stom <- as.numeric(as.character(Uobs_matrix$Stomach_proportion_by_number[i]))
      
      Uobs[pred, prey, pred_length, prey_length] <- stom
    }
    
    data_list$Uobs <- Uobs
  }
  
  # with year
  if(ncol(Uobs_matrix) == 6){
    Uobs <- array(0, dim = c( data_list$nspp, data_list$nspp, max(data_list$nlengths), max(data_list$nlengths)))
    dims <- dim(data_list$Uobs)
    
    for(i in 1:nrow(Uobs_matrix)){
      pred <- as.numeric(as.character(Uobs_matrix$Pred[i]))
      prey <- as.numeric(as.character(Uobs_matrix$Prey[i]))
      pred_length <- as.numeric(as.character(Uobs_matrix$Pred_length[i]))
      prey_length <- as.numeric(as.character(Uobs_matrix$Prey_length[i]))
      year <- as.numeric(as.character(Uobs_matrix$Year[i]))
      stom <- as.numeric(as.character(Uobs_matrix$Stomach_proportion_by_number[i]))
      
      Uobs[pred, prey, pred_length, prey_length, year] <- stom
    }
    
    data_list$Uobs <- Uobs
  }
  
  
  
  # Diet UobsWt
  UobsWt_matrix <- as.data.frame(readxl::read_xlsx( file, sheet = "UobsWt"))
  
  # without year
  if(ncol(UobsWt_matrix) == 5){
    UobsWt <- array(0, dim = c( data_list$nspp, data_list$nspp, max(data_list$nlengths), max(data_list$nlengths)))
    dims <- dim(data_list$UobsWt)
    
    for(i in 1:nrow(UobsWt_matrix)){
      pred <- as.numeric(as.character(UobsWt_matrix$Pred[i]))
      prey <- as.numeric(as.character(UobsWt_matrix$Prey[i]))
      pred_length <- as.numeric(as.character(UobsWt_matrix$Pred_length[i]))
      prey_length <- as.numeric(as.character(UobsWt_matrix$Prey_length[i]))
      stom <- as.numeric(as.character(UobsWt_matrix$Stomach_proportion_by_weight[i]))
      
      UobsWt[pred, prey, pred_length, prey_length] <- stom
    }
    
    data_list$UobsWt <- UobsWt
  }
  
  # with year
  if(ncol(UobsWt_matrix) == 6){
    UobsWt <- array(0, dim = c( data_list$nspp, data_list$nspp, max(data_list$nlengths), max(data_list$nlengths)))
    dims <- dim(data_list$UobsWt)
    
    for(i in 1:nrow(UobsWt_matrix)){
      pred <- as.numeric(as.character(UobsWt_matrix$Pred[i]))
      prey <- as.numeric(as.character(UobsWt_matrix$Prey[i]))
      pred_length <- as.numeric(as.character(UobsWt_matrix$Pred_length[i]))
      prey_length <- as.numeric(as.character(UobsWt_matrix$Prey_length[i]))
      year <- as.numeric(as.character(UobsWt_matrix$Year[i]))
      stom <- as.numeric(as.character(UobsWt_matrix$Stomach_proportion_by_weight[i]))
      
      UobsWt[pred, prey, pred_length, prey_length, year] <- stom
    }
    
    data_list$UobsWt <- UobsWt
  }
  
  
  # write the data
  return(data_list)
}

