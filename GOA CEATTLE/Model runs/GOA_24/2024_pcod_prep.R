library(Rceattle)
library(r4ss)

####################################################
## COMPARISON OF MULTIPLE MODELS
## from ?SSplotComparisons
####################################################
# directories where models were run need to be defined
setwd("Model runs/GOA_24")
# dir0 <- "C://Users//Grant Adams//Documents//GitHub//RceattleRuns//GOA//Data//Pcod 2018//GOApcod_Appendix2.3//2018 GOA Pacific cod figures and Files//Model18.10.44" # 3.24 Model


dir0 <- "Data/SAFE data/goa_pcod" # 3.24 Model

mod0 <- SS_output(dir=dir0)
SSexecutivesummary(mod0)

# 1 -1 1 1 0 FshTrawl  # 1
# 1 -1 1 1 0 FshLL  # 2
# 1 -1 1 1 0 FshPot  # 3
# 3 1 1 2 0 Srv  # 4
# 3 1 1 2 0 LLSrv  # 5
# 3 1 1 2 0 IPHCLL  # 6
# 3 1 1 2 0 ADFG  # 7

# Weight ----
# * Fleets ----
wt_flt_1 <- mod0$wtatage[which(mod0$wtatage$fleet == 1),]
wt_flt_1$Wt_name = "Pcod_trawl_fishery"
wt_flt_1$Wt_index = 1
wt_flt_1$Species = 1
wt_flt_1$Sex = 0

wt_flt_2 <- mod0$wtatage[which(mod0$wtatage$fleet == 2),]
wt_flt_2$Wt_name = "Pcod_longline_fishery"
wt_flt_2$Wt_index = 2
wt_flt_2$Species = 1
wt_flt_2$Sex = 0

wt_flt_3 <- mod0$wtatage[which(mod0$wtatage$fleet == 3),]
wt_flt_3$Wt_name = "Pcod_pot_fishery"
wt_flt_3$Wt_index = 3
wt_flt_3$Species = 1
wt_flt_3$Sex = 0

wt_flt_4 <- mod0$wtatage[which(mod0$wtatage$fleet == 4),]
wt_flt_4$Wt_name = "Pcod_bt_survey"
wt_flt_4$Wt_index = 4
wt_flt_4$Species = 1
wt_flt_4$Sex = 0

wt_flt_5 <- mod0$wtatage[which(mod0$wtatage$fleet == 5),]
wt_flt_5$Wt_name = "Pcod_ll_survey"
wt_flt_5$Wt_index = 5
wt_flt_5$Species = 1
wt_flt_5$Sex = 0

# wt_flt_6 <- mod0$wtatage[which(mod0$wtatage$fleet == 6),]
# wt_flt_6$Wt_name = "Pcod_iphcll"
# wt_flt_6$Wt_index = 6
# wt_flt_6$Species = 1
# wt_flt_6$Sex = 0
# 
# wt_flt_7 <- mod0$wtatage[which(mod0$wtatage$fleet == 7),]
# wt_flt_7$Wt_name = "Pcod_adfg"
# wt_flt_7$Wt_index = 7
# wt_flt_7$Species = 1
# wt_flt_7$Sex = 0
# 
# wt_flt_8 <- mod0$wtatage[which(mod0$wtatage$fleet == 8),]
# wt_flt_8$Wt_name = "Pcod_spawn"
# wt_flt_8$Wt_index = 8
# wt_flt_8$Species = 1
# wt_flt_8$Sex = 0
# 
# wt_flt_9 <- mod0$wtatage[which(mod0$wtatage$fleet == 9),]
# wt_flt_9$Wt_name = "Pcod_Seine"
# wt_flt_9$Wt_index = 9
# wt_flt_9$Species = 1
# wt_flt_9$Sex = 0


#  -- Biomass weight
wt_flt_b <- mod0$wtatage[which(mod0$wtatage$fleet == 0),]
wt_flt_b$Wt_name = "Pcod_biomass_begin"
wt_flt_b$Wt_index = 6
wt_flt_b$Species = 1
wt_flt_b$Sex = 0

wt_flt_ssb <- mod0$wtatage[which(mod0$wtatage$fleet == -2),]
wt_flt_ssb$Wt_name = "Pcod_spawn_wt"
wt_flt_ssb$Wt_index = 7
wt_flt_ssb$Species = 1
wt_flt_ssb$Sex = 0

wt_flt_generic <- mod0$wtatage[which(mod0$wtatage$fleet == -1),]
wt_flt_generic$Wt_name = "Pcod_generic_wt"
wt_flt_generic$Wt_index = 8
wt_flt_generic$Species = 1
wt_flt_generic$Sex = 0


wt_all <- rbind(
  wt_flt_1, wt_flt_2, wt_flt_3, # Fisheries
  wt_flt_4, wt_flt_5, # wt_flt_6, wt_flt_7, wt_flt_8, wt_flt_9, # Surveys
  wt_flt_b, wt_flt_ssb, wt_flt_generic)
wt_all = wt_all[,c("Wt_name","Wt_index","Species","Sex", "year",1:10)]
wt_all <- wt_all[which(wt_all$year >= 1977 & wt_all$year <= 2024),]
write.csv(wt_all, file = "Data/SAFE data/2024pcod_wtatage.csv")


# Time series of biomass ----
write.csv(mod0$timeseries, file = "Data/SAFE data/2024pcod_time_series.csv")


# Time series of numbers at age ----
# - We want middle based on when predation occurs
write.csv(mod0$natage, file = "Data/SAFE data/2024pcod_natage.csv")


# Age_length_key ---- 
library(writexl)
alkbegin <- t((as.data.frame(mod0$ALK[,,1]))) # Subseason 1 ALK 
alkmid <- t(as.data.frame(mod0$ALK[,,2])) # Subseason 1 ALK (guessing we want this)

# Reorganize
alkbegin <- alkbegin[,ncol(alkbegin):1]
alkmid <- alkmid[,ncol(alkmid):1]

# Remove age-0
alkbegin <- alkbegin[-1,]
alkmid <- alkmid[-1,]

alkbegin <- cbind(data.frame(Age = 1:10), alkbegin)
alkmid <- cbind(data.frame(Age = 1:10), alkmid)


alk <- list(begininng = alkbegin, mid = alkmid)
writexl::write_xlsx(alk, path = "Data/SAFE data/2024pcod__alk.xlsx")

