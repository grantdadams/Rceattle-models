#TODO - Take weighted average of annual age proportion using annual density
#TODO - Set rowsums of ALKS to zero
#TODO - Expand data to missing halibut ages

##### Load libraries and directories
library(Rceattle)
library(readxl)
library(dplyr)
library(tidyr)

model_dir <- "Model runs/GOA_18.5.1/"
diet_dir  <- "Data/Diet"
halibut_alk_dir <- "Data/Halibut 2018/GOA A-L keys"


#### Read in data
# - Read in CEATTLE data for ALKs
mydata_pollock <- Rceattle::read_data( file = paste0(model_dir, "Data/GOA_18.5.1_pollock_single_species_1970-2018.xlsx"))
mydata_pcod_est <- Rceattle::read_data( file = paste0(model_dir, "Data/GOA_18.5.1_pcod_single_species_1977-2018.xlsx"))
mydata_atf_est <- Rceattle::read_data( file = paste0(model_dir, "Data/GOA_18.5.1_arrowtooth_single_species_1961-2018.xlsx"))

# Ages 2-15+, Lengths 15-100 cm
halibut_alk_files <- list.files(halibut_alk_dir, pattern = "*.dat")
halibut_alk_list <- lapply(paste0(halibut_alk_dir,"/",halibut_alk_files), read.table)
halibut_alk_list <- c(list(halibut_alk_list[[1]]), halibut_alk_list) # Assuming 1990 is the same as 1993
halibut_alk_list <- c(halibut_alk_list[1:4], list(halibut_alk_list[[5]]), halibut_alk_list[5:12], list(halibut_alk_list[[12]])) # Assuming 2001 is the same as 2003 and 2019 is the same as 2017
years <- c(1990, 1993, 1996, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)
for(i in 1:length(halibut_alk_list)){
  halibut_alk_list[[i]] <- as.data.frame(t(halibut_alk_list[[i]])) # Make ages x lengths
  colnames(halibut_alk_list[[i]]) <- 1:ncol(halibut_alk_list[[i]]) # Should be 86 
  halibut_alk_list[[i]]$PredAge <- c(2:15)
  halibut_alk_list[[i]]$Yr = years[i]
}
halibut_alk_wide <- do.call(rbind.data.frame, halibut_alk_list)
halibut_alk_wide$Pred_species <- "P_Halibut"
halibut_alk_wide$PredSex <- 1 # Females
halibut_alk_tall <- halibut_alk_wide %>% pivot_longer(1:86, names_to = "PredBINalk",values_to = "PredAgeLengthProb")

# Add males
halibut_alk_tall_males <- halibut_alk_tall
halibut_alk_tall_males$PredSex <- 2
halibut_alk_tall <- rbind(halibut_alk_tall, halibut_alk_tall_males)

# -  Read in diet DATA
load("Data/Diet/Kirstin biomass weighting/cpue_files_noObserver/cpue_allsp_noObs.Rdata") # Load CPUE
load(file.path(diet_dir,"GOAPredPreyL.Rdata")) # Subset of 100 fish per year of prey lengths
# PRED_LEN is in cm # PREY_SZ1_CM is prey length in cm
load("Data/Diet/Kirstin biomass weighting/Agg_files_noObserver/PACIFIC COD_GOA_byPred_preyWT_noObs.Rdata") # Gravimetric composition COD
preyWT <- agg.preyWT
load("Data/Diet/Kirstin biomass weighting/Agg_files_noObserver/WALLEYE POLLOCK_GOA_byPred_preyWT_noObs.Rdata") # Gravimetric composition POLLOCK
preyWT <- rbind(preyWT, agg.preyWT)
load("Data/Diet/Kirstin biomass weighting/Agg_files_noObserver/ARROWTOOTH FLOUNDR_GOA_byPred_preyWT_noObs.Rdata") # Gravimetric composition ATF
preyWT <- rbind(preyWT, agg.preyWT)
load("Data/Diet/Kirstin biomass weighting/Agg_files_noObserver/PACIFIC HALIBUT_GOA_byPred_preyWT_noObs.Rdata") # Gravimetric composition HALIBUT
preyWT <- rbind(preyWT, agg.preyWT)


#### Update species names
# - Prey spp wt
preyWT$Pred_species <- ifelse(preyWT$Species_name == "Arrow or Kam", "Arrowtooth", # All Arrow or Kam in GOA is Arrow per Kerim
                              ifelse(preyWT$Species_name == "Arrowtooth" , "Arrowtooth",
                                     ifelse(preyWT$Species_name == "W. Pollock", "W_Pollock",
                                            ifelse(preyWT$Species_name == "P. Cod", "P_Cod",
                                                   ifelse(preyWT$Species_name == "P. Halibut", "P_Halibut", NA)))))


# - CPUE
cpue$Pred_species <- ifelse(cpue$CN == "walleye pollock", "W_Pollock", 
                            ifelse(cpue$CN == "Pacific cod", "P_Cod", 
                                   ifelse(cpue$CN == "arrowtooth flounder", "Arrowtooth",
                                          ifelse(cpue$CN == "Pacific halibut", "P_Halibut", NA))))

# - Pred/prey length
GOAPredPreyL$Pred_species <- ifelse(GOAPredPreyL$CEATTLE_PRED == "Arrow or Kam", "Arrowtooth",
                                    ifelse(GOAPredPreyL$CEATTLE_PRED == "Arrowtooth" , "Arrowtooth",
                                           ifelse(GOAPredPreyL$CEATTLE_PRED == "W. Pollock", "W_Pollock",
                                                  ifelse(GOAPredPreyL$CEATTLE_PRED == "P. Cod", "P_Cod",
                                                         ifelse(GOAPredPreyL$CEATTLE_PRED == "P. Halibut", "P_Halibut", NA)))))

GOAPredPreyL$Prey_species <- ifelse(GOAPredPreyL$CEATTLE_PREY == "Arrow or Kam", "Arrowtooth",
                                    ifelse(GOAPredPreyL$CEATTLE_PREY == "Arrowtooth" , "Arrowtooth",
                                           ifelse(GOAPredPreyL$CEATTLE_PREY == "W. Pollock", "W_Pollock",
                                                  ifelse(GOAPredPreyL$CEATTLE_PREY == "P. Cod", "P_Cod",
                                                         ifelse(GOAPredPreyL$CEATTLE_PREY == "P. Halibut", "P_Halibut", NA)))))


#### Bins
# Length bins in ALKs (cm)
atf_lbin <- c(0, 10, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 75, 999) # 26
pollock_lbin = c(0, 17, 28, 36, 43, 51, 56, 999) # 7, Length transition 3 (From Shelikof Strait survey 1992-98)	
pcod_lbin <- c(0, seq(1.5, 116.5, by = 2), 999) # 0.5 is start of first bin and 116.5-999 is last bin
halibut_lbin = c(0,seq(16, 100, by = 2), 999) 

# - ALK length bins, had to do a dimension reduction above or lost data
pcod_lbin_alk <- c(0, seq(1.5, 116.5, by = 1), 999) # 0.5 is start of first bin and 116.5-999 is last bin
halibut_lbin_alk = c(0,seq(16, 100, by = 1), 999) 


####  Convert fork length measurements (mm to cm):
# - cpue
cpue$PredBIN = as.numeric(as.character(cpue$BIN))
cpue$PredLength = cpue$PredBIN / 10

# - Proportion of prey sp in pred
preyWT$PredLength = preyWT$PredL # In cm

# - Pred length and prey length
GOAPredPreyL$PredLength = GOAPredPreyL$PRED_LEN # In cm

#FIXME prey length is in mm standard length, need to convert to fork length. Maybe it already is?
GOAPredPreyL$PreyLength = GOAPredPreyL$PREY_SZ1 / 10 # mm to cm


#### Aggregate data to length bins:
# - cpue
cpue$FLBin_halibut = cut(cpue$PredLength, breaks = halibut_lbin)
cpue$FLBin_atf = cut(cpue$PredLength, breaks = atf_lbin)
cpue$FLBin_pollock = cut(cpue$PredLength, breaks = pollock_lbin)
cpue$FLBin_cod = cut(cpue$PredLength, breaks = pcod_lbin)

levels(cpue$FLBin_halibut) = 1:length(halibut_lbin)
levels(cpue$FLBin_atf) = 1:length(atf_lbin)
levels(cpue$FLBin_pollock) = 1:length(pollock_lbin)
levels(cpue$FLBin_cod) = 1:length(pcod_lbin)

# -- Make a generic "PredBIN"
cpue$PredBIN = ifelse(cpue$Pred_species == "P_Halibut", cpue$FLBin_halibut,
                      ifelse(cpue$Pred_species == "P_Cod", cpue$FLBin_cod,
                             ifelse(cpue$Pred_species == "W_Pollock", cpue$FLBin_pollock,
                                    ifelse(cpue$Pred_species == "Arrowtooth", cpue$FLBin_atf, NA))))


cpue = cpue %>% group_by(STRATUM, VESSEL, CRUISE, HAUL, YEAR, Pred_species, PredBIN) %>% 
  summarise( NUM_KM2 = sum(NUM_KM2)) 

# - Proportion of prey sp in pred-at-length
preyWT$FLBin_halibut = cut(preyWT$PredLength, breaks = halibut_lbin)
preyWT$FLBin_atf = cut(preyWT$PredLength, breaks = atf_lbin)
preyWT$FLBin_pollock = cut(preyWT$PredLength, breaks = pollock_lbin)
preyWT$FLBin_cod = cut(preyWT$PredLength, breaks = pcod_lbin)

levels(preyWT$FLBin_halibut) = 1:length(halibut_lbin)
levels(preyWT$FLBin_atf) = 1:length(atf_lbin)
levels(preyWT$FLBin_pollock) = 1:length(pollock_lbin)
levels(preyWT$FLBin_cod) = 1:length(pcod_lbin)

# -- Make a generic "PredBIN"
preyWT$PredBIN = ifelse(preyWT$Pred_species == "P_Halibut", preyWT$FLBin_halibut,
                        ifelse(preyWT$Pred_species == "P_Cod", preyWT$FLBin_cod,
                               ifelse(preyWT$Pred_species == "W_Pollock", preyWT$FLBin_pollock,
                                      ifelse(preyWT$Pred_species == "Arrowtooth", preyWT$FLBin_atf, NA))))



# -- Save only columns we care about
preyWT$Arrowtooth <- preyWT$Arrowtooth + preyWT$`Arrow or Kam` # Per kerim, not much Kam in GOA
preyWT <- subset(preyWT, select = c("Vessel", "Cruise", "Haul", "Yr", "Strata", "PredBIN", "Pred_species", "Obs_TWT", "Arrowtooth" , "W. Pollock", "P. Cod", "P. Halibut"))

# -- Rename column names
colnames(preyWT) <- c("Vessel", "Cruise", "Haul", "Yr", "Strata", "PredBIN", "Pred_species", "Obs_TWT", "Arrowtooth" , "W_Pollock", "P_Cod", "P_Halibut")


# - Pred length and prey length
# -- For predators 
GOAPredPreyL$FLBin_halibut = cut(GOAPredPreyL$PredLength, breaks = halibut_lbin)
GOAPredPreyL$FLBin_atf = cut(GOAPredPreyL$PredLength, breaks = atf_lbin)
GOAPredPreyL$FLBin_pollock = cut(GOAPredPreyL$PredLength, breaks = pollock_lbin)
GOAPredPreyL$FLBin_cod = cut(GOAPredPreyL$PredLength, breaks = pcod_lbin)

levels(GOAPredPreyL$FLBin_halibut) = 1:length(halibut_lbin)
levels(GOAPredPreyL$FLBin_atf) = 1:length(atf_lbin)
levels(GOAPredPreyL$FLBin_pollock) = 1:length(pollock_lbin)
levels(GOAPredPreyL$FLBin_cod) = 1:length(pcod_lbin)

# -- Make a generic "PredBIN"
GOAPredPreyL$PredBIN = ifelse(GOAPredPreyL$Pred_species == "P_Halibut", GOAPredPreyL$FLBin_halibut,
                              ifelse(GOAPredPreyL$Pred_species == "P_Cod", GOAPredPreyL$FLBin_cod,
                                     ifelse(GOAPredPreyL$Pred_species == "W_Pollock", GOAPredPreyL$FLBin_pollock,
                                            ifelse(GOAPredPreyL$Pred_species == "Arrowtooth", GOAPredPreyL$FLBin_atf, NA))))

# -- For prey 
# Convert SL to FL (Cod and ATF assumed the same)
# Weights also need to be converted because they were based of a SL ^ b rather than a FL ^ b, where paremeters were estimated from RACE W ~ FL data
# -- Pollock SL = 92.6% FL
GOAPredPreyL$PredLength[which(GOAPredPreyL$Prey_species == "W_Pollock")] <- GOAPredPreyL$PreyLength[which(GOAPredPreyL$Prey_species == "W_Pollock")] / 0.926
GOAPredPreyL$preyWt_kg[which(GOAPredPreyL$Prey_species == "W_Pollock")] <- GOAPredPreyL$LW_a[which(GOAPredPreyL$Prey_species == "W_Pollock")] * GOAPredPreyL$PreyLength[which(GOAPredPreyL$Prey_species == "W_Pollock")] ^ GOAPredPreyL$LW_b[which(GOAPredPreyL$Prey_species == "W_Pollock")]

# -- Halibut
GOAPredPreyL$PredLength[which(GOAPredPreyL$Prey_species == "P_Halibut")] <- 2.353334 + GOAPredPreyL$PreyLength[which(GOAPredPreyL$Prey_species == "P_Halibut")] * 1.059473 # From Fisheries, Aquatic Science, and Technology Laboratory, Unpublished Data
GOAPredPreyL$preyWt_kg[which(GOAPredPreyL$Prey_species == "P_Halibut")] <- GOAPredPreyL$LW_a[which(GOAPredPreyL$Prey_species == "P_Halibut")] * GOAPredPreyL$PreyLength[which(GOAPredPreyL$Prey_species == "P_Halibut")] ^ GOAPredPreyL$LW_b[which(GOAPredPreyL$Prey_species == "P_Halibut")]

# - Convert to bins
GOAPredPreyL$FLBin_halibut_prey = cut(GOAPredPreyL$PreyLength, breaks = halibut_lbin)
GOAPredPreyL$FLBin_atf_prey = cut(GOAPredPreyL$PreyLength, breaks = atf_lbin)
GOAPredPreyL$FLBin_pollock_prey = cut(GOAPredPreyL$PreyLength, breaks = pollock_lbin)
GOAPredPreyL$FLBin_cod_prey = cut(GOAPredPreyL$PreyLength, breaks = pcod_lbin)

levels(GOAPredPreyL$FLBin_halibut_prey) = 1:length(halibut_lbin)
levels(GOAPredPreyL$FLBin_atf_prey) = 1:length(atf_lbin)
levels(GOAPredPreyL$FLBin_pollock_prey) = 1:length(pollock_lbin)
levels(GOAPredPreyL$FLBin_cod_prey) = 1:length(pcod_lbin)

# - Make a generic "PredBIN"
GOAPredPreyL$PreyBIN = ifelse(GOAPredPreyL$Prey_species == "P_Halibut", GOAPredPreyL$FLBin_halibut_prey,
                              ifelse(GOAPredPreyL$Prey_species == "P_Cod", GOAPredPreyL$FLBin_cod_prey,
                                     ifelse(GOAPredPreyL$Prey_species == "W_Pollock", GOAPredPreyL$FLBin_pollock_prey,
                                            ifelse(GOAPredPreyL$Prey_species == "Arrowtooth", GOAPredPreyL$FLBin_atf_prey, NA))))


#### Exclude data from 1981, 1984 and 1987 (survey methods were standardized in 1990):
cpue = subset(cpue, YEAR >= 1990)
preyWT = subset(preyWT, Yr >= 1990)
GOAPredPreyL = subset(GOAPredPreyL, YEAR >= 1990)

#### Remove all empty stomachs:
preyWT = subset(preyWT, Obs_TWT > 0) # remove hidden empties


#### Subset to management areas (610 to 659): Likely biased
# - Pollock model 610 to 640
# - Cod model 610 to 659
# - ATF 610 to 650


#### Numerical estimator of relative density (cpue)
# - Get area of each stratum
statum_area = data.frame(STRATUM = c(10, 11, 12, 13, 20, 21, 22, 30, 31, 32, 33, 35, 40, 41, 50, 110, 111, 112, 120, 121, 122, 130, 131, 132, 133, 134, 140, 141, 142, 143, 150, 151, 210, 220, 221, 230, 231, 232, 240, 241, 250, 251, 310, 320, 330, 340, 341, 350, 351, 410, 420, 430, 440, 450, 510, 520, 530, 540, 550),
                         AreaKM2 = c(833, 13681, 6876, 12399, 7941, 7302, 10792, 5766, 15401, 9887, 5260, 2200, 9947, 6714, 6546, 4245, 8154, 2278, 11104, 7735, 5011, 7912, 7336, 10981, 12077, 5026, 7346, 5277, 9032, 7728, 4196, 6888, 2788, 10018, 1528, 6659, 1623, 3208, 3043, 2127, 1125, 3927, 2531, 1604, 2912, 1107, 1521, 2344, 733, 2006, 1953, 1745, 1469, 1033, 1937, 3066, 3494, 1887, 1206))

# - Sum NUM_KM2 across length bins
cpue_aggregate = cpue %>% group_by(STRATUM, VESSEL, CRUISE, HAUL, YEAR, Pred_species) %>% 
  summarise( AGG_NUM_KM2 = sum(NUM_KM2)) 

# - Calculate average density per strata
cpue_aggregate = merge(cpue_aggregate, statum_area, by = "STRATUM") # Merge with area

relative_density = cpue_aggregate %>% group_by(STRATUM, YEAR, Pred_species) %>% 
  summarise( relative_density = mean(AGG_NUM_KM2) * mean(AreaKM2)) # Multiply average cpue across length bins per strata by stratum area

annual_density = relative_density %>% group_by(YEAR, Pred_species) %>% 
  summarise( Annual_density = sum(relative_density)) # Multiply average cpue across length bins per strata by stratum area
colnames(annual_density) <- c("Yr", "Pred_species", "Annual_density")


#### Weighting factor of length subsampling (preyWT and cpue)
# - Calculate proportions of fish subsampled within each size PredBIN, year, and haul (food habits):
sumSizeDiets = preyWT %>%
  group_by(Yr, Strata, Haul, Pred_species, PredBIN) %>% 
  summarise(sumFreq_Diets = length(Obs_TWT)) # Calculate number in each PredBIN

propSizeDiets = sumSizeDiets %>%
  group_by(Yr, Strata, Haul, Pred_species) %>%
  mutate(propFreq_Diets = sumFreq_Diets/sum(sumFreq_Diets)) # Calculate proportion in each PredBIN
propSizeDiets = na.omit(propSizeDiets)

# - Calculate proportions of fish subsampled within each size PredBIN, year, and haul (bottom trawl survey):
propSizeLengths = cpue %>%
  group_by(YEAR, STRATUM, HAUL, Pred_species) %>% 
  mutate(propFreq_Lengths = NUM_KM2/sum(NUM_KM2)) 
propSizeLengths = na.omit(propSizeLengths)

# - Update names to merge
colnames(propSizeLengths) <- c("Strata","Vessel", "Cruise", "Haul", "Yr" ,"Pred_species", "PredBIN", "NUM_KM2", "propFreq_Lengths" )



# - Calculate sample weights from dividing length-based proportions of fish caught by those subsampled for gut content analysis in each survey year and grid cell:
LengthProp = merge(propSizeDiets, propSizeLengths)
LengthProp[is.na(LengthProp)] = 0

LengthProp$WT = LengthProp$propFreq_Lengths/LengthProp$propFreq_Diets
LengthProp$WT[is.infinite(LengthProp$WT)] = 0

# # - Error check (propFreq_Diets should == 0 if propFreq_Lengths == 0)
LengthPropErrorCheck = merge(propSizeDiets, propSizeLengths, all = TRUE)
LengthPropErrorCheck[is.na(LengthPropErrorCheck)] = 0
error_check <- LengthPropErrorCheck[which(LengthPropErrorCheck$propFreq_Lengths == 0 & LengthPropErrorCheck$propFreq_Diets > 0),]
table(error_check$Pred_species)/table(LengthPropErrorCheck$Pred_species)


#### Merge with relative density and length weights
# - Update names to merge
# preyWT, LengthProp, relative_density

# - Update colnames for mergine
colnames(relative_density) <- c("Strata", "Yr", "Pred_species", "RelativeDensity_N")

preyWTweighted = preyWT %>% 
  left_join(LengthProp) %>% 
  left_join(relative_density)

preyWTweighted = na.omit(preyWTweighted) # FIXME - Not sure about these na.omits vs setting to 0


#### Calculate average proportion of a prey sp in the stomach of a predator-at-length
# - Make prey spp, and prey wt 2 colums
preyWTweightedTall <- preyWTweighted %>% pivot_longer(c(Arrowtooth, W_Pollock, P_Cod, P_Halibut), names_to = "Prey_species", values_to = "Prey_wt")


# - Calculate proportion
propPreySppAnnual = preyWTweightedTall %>%
  group_by(Pred_species, PredBIN, Yr, Prey_species) %>%
  summarize(propPrey_num = sum(Prey_wt * WT * RelativeDensity_N),
            propPrey_den = sum( Obs_TWT * WT * RelativeDensity_N)) %>%
  mutate(propPreySpp = propPrey_num / propPrey_den)

propPreySpp = preyWTweightedTall %>%
  group_by(Pred_species, PredBIN, Prey_species) %>%
  summarize(propPrey_num = sum(Prey_wt * WT * RelativeDensity_N),
            propPrey_den = sum( Obs_TWT * WT * RelativeDensity_N)) %>%
  mutate(propPreySpp = propPrey_num / propPrey_den)


#### Calculate average proportion by weight of a prey-at-length compared to all lengths of that species in the stomach of a predator-at-length
propPreyLength = GOAPredPreyL %>%
  group_by(Pred_species, Prey_species, PredBIN, PreyBIN) %>%
  ## Calculate the numerator, summing together rows that have the same r, l, and i
  summarize(propPreyLength_num = sum(preyWt_kg)) %>%
  mutate(propPreyLength_den = sum(propPreyLength_num)) %>%
  mutate(propPreyLength = propPreyLength_num / propPreyLength_den)


#### Calculate average proportion by weight of a prey-at-length compared to prey/length of that species in the stomach of a predator-at-length
# - Merge datasets
propPrey = merge(propPreyLength, propPreySpp, all = TRUE)
propPreyAnnual <- merge(propPreyLength, propPreySppAnnual, all = TRUE)

# - Multiply proportion of sp by weight, and proportion of prey length by weight
propPrey$propPrey <- propPrey$propPreySpp * propPrey$propPreyLength
propPreyAnnual$propPrey <- propPreyAnnual$propPreySpp * propPreyAnnual$propPreyLength

propPrey[is.na(propPrey)] = 0 # Convert NAs to zeros
propPreyAnnual[is.na(propPreyAnnual)] = 0 # Convert NAs to zeros


#### Convert length bins from diet data to length bins used for diet
propPrey <- propPrey[-which(propPrey$PreyBIN == 0),] # Get rid of no prey lengths
propPreyAnnual <- propPreyAnnual[-which(propPreyAnnual$PreyBIN == 0),] # Get rid of no prey lengths


# Create data.frame of prey species, pred species, l bin diet, l bind diet prey, lbin diet alk, lbin diet pred alk
# -- Cod and Halibut ALK and Diet length bins are different
alk_lbin_diet_lbin_cod <- data.frame(Pred_species = rep("P_Cod", length(pcod_lbin_alk[-1])), FL = pcod_lbin_alk[1:length(pcod_lbin_alk[-1])] +0.5, BINalk = 1:length(pcod_lbin_alk[-1])) # - Note FL is the lower limit of the length bin
alk_lbin_diet_lbin_cod$BIN = cut(alk_lbin_diet_lbin_cod$FL, breaks = pcod_lbin)
levels(alk_lbin_diet_lbin_cod$BIN) = 1:length(pcod_lbin)

alk_lbin_diet_lbin_halibut <- data.frame(Pred_species = rep("P_Halibut", length(halibut_lbin_alk[-1])), FL = halibut_lbin_alk[1:length(halibut_lbin_alk[-1])] + 0.5,  BINalk = 1:length(halibut_lbin_alk[-1])) # - Note FL is the lower limit of the length bin
alk_lbin_diet_lbin_halibut$BIN = cut(alk_lbin_diet_lbin_halibut$FL, breaks = halibut_lbin)
levels(alk_lbin_diet_lbin_halibut$BIN) = 1:length(halibut_lbin)

# -- ATF and Pollock ALK and Diet Bins are the same
# -- Pollock
alk_lbin_diet_lbin_pollock <- data.frame(Pred_species = rep("W_Pollock", length(pollock_lbin[-1])), FL = pollock_lbin[1:length(pollock_lbin[-1])] + 0.5,  BINalk = 1:length(pollock_lbin[-1])) # - Note FL is the lower limit of the length bin
alk_lbin_diet_lbin_pollock$BIN = cut(alk_lbin_diet_lbin_pollock$FL, breaks = pollock_lbin)
levels(alk_lbin_diet_lbin_pollock$BIN) = 1:length(pollock_lbin)

# -- ATF
alk_lbin_diet_lbin_atf <- data.frame(Pred_species = rep("Arrowtooth", length(atf_lbin[-1])), FL = atf_lbin[1:length(atf_lbin[-1])] + 0.5,  BINalk = 1:length(atf_lbin[-1])) # - Note FL is the lower limit of the length bin
alk_lbin_diet_lbin_atf$BIN = cut(alk_lbin_diet_lbin_atf$FL, breaks = atf_lbin)
levels(alk_lbin_diet_lbin_atf$BIN) = 1:length(atf_lbin)

# -- Combine data
alk_lbin_diet_lbin <- rbind(alk_lbin_diet_lbin_pollock, alk_lbin_diet_lbin_atf, alk_lbin_diet_lbin_cod, alk_lbin_diet_lbin_halibut)

# -- Make a pred and prey version
alk_lbin_diet_lbin_pred <- alk_lbin_diet_lbin
colnames(alk_lbin_diet_lbin_pred) <- c("Pred_species", "PredFL", "PredBINalk", "PredBIN")

alk_lbin_diet_lbin_prey <- alk_lbin_diet_lbin
colnames(alk_lbin_diet_lbin_prey) <- c("Prey_species", "PreyFL", "PreyBINalk", "PreyBIN")

# -- Make all combinations of Pred/Prey PredLength/PreyLength
alk_lbin_diet_lbin <- merge(alk_lbin_diet_lbin_pred, alk_lbin_diet_lbin_prey)
alk_lbin_diet_lbin_annual <- merge(alk_lbin_diet_lbin, data.frame(Yr = years))

# -- Merge with diet proportion
propPrey <- propPrey[,c("Pred_species", "Prey_species", "PredBIN", "PreyBIN", "propPrey")]
propPreyAnnual <- propPreyAnnual[,c("Yr", "Pred_species", "Prey_species", "PredBIN", "PreyBIN", "propPrey")]

propPreyLength <- merge(alk_lbin_diet_lbin, propPrey, all = TRUE)
propPreyLengthAnnual <- merge(alk_lbin_diet_lbin_annual, propPreyAnnual, all = TRUE)

propPreyLength[is.na(propPreyLength)] = 0 # Convert NAs to zeros
propPreyLengthAnnual[is.na(propPreyLengthAnnual)] = 0 # Convert NAs to zeros


#### Convert length to age
# -- Get ALKs and make long
# --- Cod
pcod_alk <- mydata_pcod_est$age_trans_matrix[which(mydata_pcod_est$age_trans_matrix$ALK_name == "Cod"),]
pcod_alk <- pcod_alk[,1:(5+mydata_pcod_est$nlengths[1])]
pcod_alk_long <- pcod_alk %>% pivot_longer(c(6:(5+mydata_pcod_est$nlengths[1])), names_to = "BINalk", values_to = "Prob")
pcod_alk_long$BINalk <- substring(pcod_alk_long$BINalk, 8, 10)

# --- Pollock
pollock_alk <- mydata_pollock$age_trans_matrix[which(mydata_pollock$age_trans_matrix$ALK_name == "Pollock"),]
pollock_alk <- pollock_alk[,1:(5+mydata_pollock$nlengths[1])]
pollock_alk_long <- pollock_alk %>% pivot_longer(c(6:(5+mydata_pollock$nlengths[1])), names_to = "BINalk", values_to = "Prob")
pollock_alk_long$BINalk <- substring(pollock_alk_long$BINalk, 8, 10)

# --- ATF male
atf_male_alk <- mydata_atf_est$age_trans_matrix[which(mydata_atf_est$age_trans_matrix$ALK_name == "ATF_males"),]
atf_male_alk <- atf_male_alk[,1:(5+mydata_atf_est$nlengths[1])]
atf_male_alk_long <- atf_male_alk %>% pivot_longer(c(6:(5+mydata_atf_est$nlengths[1])), names_to = "BINalk", values_to = "Prob")
atf_male_alk_long$BINalk <- substring(atf_male_alk_long$BINalk, 8, 10)

# --- ATF female
atf_female_alk <- mydata_atf_est$age_trans_matrix[which(mydata_atf_est$age_trans_matrix$ALK_name == "ATF_females"),]
atf_female_alk <- atf_female_alk[,1:(5+mydata_atf_est$nlengths[1])]
atf_female_alk_long <- atf_female_alk %>% pivot_longer(c(6:(5+mydata_atf_est$nlengths[1])), names_to = "BINalk", values_to = "Prob")
atf_female_alk_long$BINalk <- substring(atf_female_alk_long$BINalk, 8, 10)


# -- Combine ALKs and update for merging with pred-prey data
alk_long <- rbind(pollock_alk_long, pcod_alk_long, atf_female_alk_long, atf_male_alk_long)
alk_long <- alk_long[,c("ALK_name","Sex", "Age", "BINalk", "Prob")]
colnames(alk_long) <- c("Pred_species", "Sex","Age", "BINalk", "AgeLengthProb")

alk_long$Pred_species <- ifelse(alk_long$Pred_species == "ATF_males", "Arrowtooth",
                                ifelse(alk_long$Pred_species == "ATF_females" , "Arrowtooth",
                                       ifelse(alk_long$Pred_species == "Pollock", "W_Pollock",
                                              ifelse(alk_long$Pred_species == "Cod", "P_Cod",
                                                     ifelse(alk_long$Pred_species == "Halibut", "P_Halibut", NA)))))


# -- Make a pred and prey version
alk_long_pred <- alk_long
colnames(alk_long_pred) <- c("Pred_species", "PredSex", "PredAge", "PredBINalk", "PredAgeLengthProb")

alk_long_prey <- alk_long
colnames(alk_long_prey) <- c("Prey_species", "PreySex", "PreyAge", "PreyBINalk", "PreyAgeLengthProb")

# -- Extend the alk for all years to merge with halibut
alk_long_pred_annual <- merge(alk_long_pred, data.frame(Yr = years), all = TRUE)
alk_long_prey_annual <- merge(alk_long_prey, data.frame(Yr = years), all = TRUE)

# -- Merge with halibut
halibut_alk_tall <- halibut_alk_tall[,c("Pred_species", "PredSex", "PredAge", "PredBINalk", "PredAgeLengthProb", "Yr")]
halibut_alk_tall_prey <- halibut_alk_tall
colnames(halibut_alk_tall_prey) <- c("Prey_species", "PreySex", "PreyAge", "PreyBINalk", "PreyAgeLengthProb", "Yr")


alk_long_pred_annual <- rbind(alk_long_pred_annual, halibut_alk_tall)
alk_long_prey_annual <- rbind(alk_long_prey_annual, halibut_alk_tall_prey)

# -- Make into character
alk_long_prey_annual$PreyAge <- as.numeric(alk_long_prey_annual$PreyAge)
alk_long_prey_annual$PreyBINalk <- as.numeric(alk_long_prey_annual$PreyBINalk)

alk_long_prey_annual$PreyAge <- as.numeric(alk_long_prey_annual$PreyAge)
alk_long_prey_annual$PreyBINalk <- as.numeric(alk_long_prey_annual$PreyBINalk)

##### Annual proportion by age
# -- Create data.frame to fill
pred_ages <- unique(alk_long_pred_annual[c("Pred_species", "PredSex", "PredAge")])
prey_ages <- unique(alk_long_prey_annual[c("Prey_species", "PreySex", "PreyAge")])
propPreyAgeAnnual <- merge(pred_ages, prey_ages, all = TRUE)
propPreyAgeAnnual <- merge(propPreyAgeAnnual, data.frame(Yr = years), all = TRUE)
propPreyAgeAnnual$propPreyAge = 0 # Initialize

for(pred in unique(alk_long_pred_annual$Pred_species)){ # Pred loop
  for(prey in unique(alk_long_prey_annual$Prey_species)){ # Prey loop
    
    # Get sex of alk
    pred_sexes <- as.numeric(unique(alk_long_pred_annual$PredSex[which(alk_long_pred_annual$Pred_species == pred)]))
    prey_sexes <- as.numeric(unique(alk_long_prey_annual$PreySex[which(alk_long_prey_annual$Prey_species == prey)]))
    
    # Get nages of pred and prey
    pred_ages <- as.numeric(unique(alk_long_pred_annual$PredAge[which(alk_long_pred_annual$Pred_species == pred)]))
    prey_ages <- as.numeric(unique(alk_long_prey_annual$PreyAge[which(alk_long_prey_annual$Prey_species == prey)]))
    
    # Get length bins of alk
    pred_lengths <- as.numeric(unique(alk_long_pred_annual$PredBINalk[which(alk_long_pred_annual$Pred_species == pred)]))
    prey_lengths <- as.numeric(unique(alk_long_prey_annual$PreyBINalk[which(alk_long_prey_annual$Prey_species == prey)]))
    
    for(pred_sex in pred_sexes){
      for(prey_sex in prey_sexes){
        for(yr in years){ # Year loop
          
          # Get alk values
          pred_alk <- alk_long_pred_annual$PredAgeLengthProb[which(alk_long_pred_annual$Pred_species == pred &
                                                                     alk_long_pred_annual$PredSex == pred_sex &
                                                                     alk_long_pred_annual$Yr == yr)]
          prey_alk <- alk_long_prey_annual$PreyAgeLengthProb[which(alk_long_prey_annual$Prey_species == prey &
                                                                     alk_long_prey_annual$PreySex == prey_sex &
                                                                     alk_long_prey_annual$Yr == yr)]
          
          # - Make into matrix
          pred_alk <- matrix(pred_alk, nrow = length(pred_ages), ncol = length(pred_lengths), byrow = TRUE)
          prey_alk <- matrix(prey_alk, nrow = length(prey_ages), ncol = length(prey_lengths), byrow = TRUE)
          
          # Get length-specific diet proportions
          prop_prey_length_tmp <- propPreyLengthAnnual[which(propPreyLengthAnnual$Pred_species == pred &
                                                            propPreyLengthAnnual$Prey_species == prey &
                                                            propPreyLengthAnnual$Yr == yr),]
          
          prop_prey_length_mat <- matrix(prop_prey_length_tmp$propPrey, ncol = length(pred_lengths), nrow = length(prey_lengths)) # Rows =  pred_lengths, cols = prey_lengths
          
          # Multiply length based diet proportion by alks to get age based
          prop_prey_age_mat <- prey_alk %*% prop_prey_length_mat %*% t(pred_alk)
          
          # Assign to data.frame
          propPreyAgeAnnual$propPreyAge[which(propPreyAgeAnnual$Pred_species == pred & 
                                                propPreyAgeAnnual$PredSex == pred_sex &
                                                propPreyAgeAnnual$Prey_species == prey &
                                                propPreyAgeAnnual$PreySex == prey_sex &
                                                propPreyAgeAnnual$Yr == yr)] <- as.numeric(t(prop_prey_age_mat))
          
          rm(prop_prey_length_mat, prop_prey_length_tmp, prey_alk, pred_alk)
        }
      }
    }
  }
}


#### Reduce annual age specific prop to average by taking weighted average given relative density
propPreyAgeAnnual <- merge(propPreyAgeAnnual, annual_density, all = TRUE)

# - Take weighted mean
propPreyAge = propPreyAgeAnnual %>%
  group_by(Pred_species, PredSex, PredAge, Prey_species, PreySex, PreyAge) %>%
  summarize(propPreyAge = weighted.mean(propPreyAge, Annual_density))
