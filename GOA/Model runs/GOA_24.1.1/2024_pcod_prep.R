library(Rceattle)
library(r4ss)


# May 2019 - Grant Adams
library(r4ss)

####################################################
## COMPARISON OF MULTIPLE MODELS
## from ?SSplotComparisons
####################################################
# directories where models were run need to be defined
setwd("C:/Users/Grant Adams/Documents/GitHub/RceattleRuns/GOA")
# dir0 <- "C://Users//Grant Adams//Documents//GitHub//RceattleRuns//GOA//Data//Pcod 2018//GOApcod_Appendix2.3//2018 GOA Pacific cod figures and Files//Model18.10.44" # 3.24 Model


dir0 <- "~/GitHub/RceattleRuns/GOA/Data/Pcod 2021/Model21.2B/" # 3.24 Model

mod0 <- SS_output(dir=dir0)
SSexecutivesummary(mod0)

# 1 -1 1 1 0 FshTrawl  # 1
# 1 -1 1 1 0 FshLL  # 2
# 1 -1 1 1 0 FshPot  # 3
# 3 1 1 2 0 Srv  # 4
# 3 1 1 2 0 LLSrv  # 5
# 3 1 1 2 0 IPHCLL  # 6
# 3 1 1 2 0 ADFG  # 7

wt_flt_1 <- mod0$wtatage[which(mod0$wtatage$Fleet == 1),]
wt_flt_1$Wt_name = "Pcod_trawl_fishery"
wt_flt_1$Wt_index = 10
wt_flt_1$Species = 1
wt_flt_1$Sex = 0

wt_flt_2 <- mod0$wtatage[which(mod0$wtatage$Fleet == 2),]
wt_flt_2$Wt_name = "Pcod_longline_fishery"
wt_flt_2$Wt_index = 11
wt_flt_2$Species = 1
wt_flt_2$Sex = 0

wt_flt_3 <- mod0$wtatage[which(mod0$wtatage$Fleet == 3),]
wt_flt_3$Wt_name = "Pcod_pot_fishery"
wt_flt_3$Wt_index = 12
wt_flt_3$Species = 1
wt_flt_3$Sex = 0

wt_flt_4 <- mod0$wtatage[which(mod0$wtatage$Fleet == 4),]
wt_flt_4$Wt_name = "Pcod_bt_survey"
wt_flt_4$Wt_index = 8
wt_flt_4$Species = 1
wt_flt_4$Sex = 0

wt_flt_5 <- mod0$wtatage[which(mod0$wtatage$Fleet == 5),]
wt_flt_5$Wt_name = "Pcod_ll_survey"
wt_flt_5$Wt_index = 9
wt_flt_5$Species = 1
wt_flt_5$Sex = 0

wt_flt_b <- mod0$wtatage[which(mod0$wtatage$Fleet == 0),]
wt_flt_b$Wt_name = "Pcod_biomass_begin"
wt_flt_b$Wt_index = 13
wt_flt_b$Species = 1
wt_flt_b$Sex = 0

wt_flt_ssb <- mod0$wtatage[which(mod0$wtatage$Fleet == -2),]
wt_flt_ssb$Wt_name = "Pcod_spawn_wt"
wt_flt_ssb$Wt_index = 14
wt_flt_ssb$Species = 1
wt_flt_ssb$Sex = 0

wt_flt_generic <- mod0$wtatage[which(mod0$wtatage$Fleet == -1),]
wt_flt_generic$Wt_name = "Pcod_generic_wt"
wt_flt_generic$Wt_index = 15
wt_flt_generic$Species = 1
wt_flt_generic$Sex = 0


wt_all <- rbind(wt_flt_4, wt_flt_5, wt_flt_1, wt_flt_2, wt_flt_3, wt_flt_b, wt_flt_ssb, wt_flt_generic)
wt_all = wt_all[,c("Wt_name","Wt_index","Species","Sex", "Yr",1:10)]
wt_all <- wt_all[which(wt_all$Yr >= 1977 & wt_all$Yr <= 2021),]
write.csv(wt_all, file = "Data/Pcod 2021/Model21.2B_wtatage.csv")

# Time series of biomass
write.csv(mod0$timeseries, file = "Data/Pcod 2021/Model21.2B_time_series.csv")


# Age_length_key
library(writexl)
alkbegin <- t((as.data.frame(mod0$ALK[,,1]))) # Subseason 1 ALK (guessing its 
alkmid <- t(as.data.frame(mod0$ALK[,,2])) # Subseason 1 ALK (guessing its 

# Reorganize
alkbegin <- alkbegin[,ncol(alkbegin):1]
alkmid <- alkmid[,ncol(alkmid):1]

# Remove age-0
alkbegin <- alkbegin[-1,]
alkmid <- alkmid[-1,]

alkbegin <- cbind(data.frame(Age = 1:10), alkbegin)
alkmid <- cbind(data.frame(Age = 1:10), alkmid)


alk <- list(begininng = alkbegin, mid = alkmid)
writexl::write_xlsx(alk, path = "Data//Pcod 2021//Model21.2B_alk.xlsx")



# N-at-age
natage <- mod0$natage
natage <- natage[which(natage$Yr >= 1977 & natage$Yr <= 2021),]

natage_list <- list(begininng = natage[which(natage$`Beg/Mid` == "B"),], mid = natage[which(natage$`Beg/Mid` == "M"),])
writexl::write_xlsx(natage_list, path = "Data/Pcod 2021/Model21.2B_natage.xlsx")

