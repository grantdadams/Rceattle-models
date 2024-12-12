
# Annual biomass of GOA Pacific cod consumed by predators
# Annual biomass of GOA pollock consumed by predators
# Annual ration of GOA Pacific cod
# Annual ration of GOA pollock
# Total natural mortality (m1+m2) of age-1 GOA Pacific cod
# Total natural mortality (m1+m2) of age-1 GOA pollock 
years <- tmp$styr:(tmp$nyrs-1 + tmp$styr)

# M / Z * (1.0 - exp(-Z)) * N * wt; // 5.5.
M2 <- (tmp$M2_2)
Z <- (tmp$M2_2 + c(tmp$M1_2) + tmp$F_2)
N <- tmp$N_2[1:45,]
wt <- tmp$wt_2[1:45,1:12]

B_eaten <- M2/Z * (1-exp(-Z)) * N * wt
B_eaten
tmp$Eage_2



# -- Pollock and cod
for(i in 1:3){
  sp = i
  
  spname = BS2017SS$spnames[sp]
  nages <- BS2017SS$data_list$nages[sp]
  
  Matage1 = data.frame(Year = years,
                       MS_M = tmp[[paste0("M2_", sp)]][,1] + (tmp[[paste0("M1_", sp)]][1]))
  
  Biomass = data.frame(Year = years,
                       MS_Biomass = tmp[[paste0("Biomass_", sp)]][1,],
                       MS_SSB = tmp[[paste0("BiomassSSB_", sp)]][1,]
  )
  
  B_eaten = data.frame(Year = years,
                       Biomass_eaten_as_prey = rowSums(tmp[[paste0("Eage_", sp)]]))
  
  Ration = data.frame(Year = years,
                      `Age4plusTotal` = weighted_ration_ebs(tmp, spp = sp, minage = 4),
                      `Age1plusTotal` = weighted_ration_ebs(tmp, spp = sp, minage = 1)
  )
  
  
  # Indices to share
  library(writexl)
  write_xlsx(
    list(Age1_M = Matage1,
         Biomass = Biomass,
         B_eaten = B_eaten,
         Ration = Ration
    )
    ,paste0("Results/EBS ", spname, " ESP Indices.xlsx")
  )
}

#'
#'
#'
#'Figs for EcoCons CEATTLE
#'
#'
weighted_ration_ebs <- function(model, spp = 1, minage = 4, maxage = NULL){
  yrs <- model$styr:(model$nyrs-1 + model$styr)
  
  if(is.null(maxage)){
    maxage = model[[paste0("nages_", spp)]]
  }
  
  return(rowSums(model[[paste0("biomassByage_", spp)]][1:length(yrs),minage:maxage] * model[[paste0("ration2_", spp)]][1:length(yrs),minage:maxage]))
}
