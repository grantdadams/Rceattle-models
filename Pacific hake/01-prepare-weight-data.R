library(Rceattle)
library(r4ss)

dir0 <- "Data/hake-2022-model-files" # 3.24 Model

mod0 <- SS_output(dir=dir0)
SSexecutivesummary(mod0)

# 1	-1	1	1	0	Fishery        	#_1
# 3	 1	1	2	0	Acoustic_Survey	#_2
# 3	 1	1	2	0	Age1_Survey	#_3

# Weight ----
# * Fleets ----
wt_flt_1 <- mod0$wtatage[which(mod0$wtatage$fleet == 1),]
wt_flt_1$Wt_name = "Fishery"
wt_flt_1$Wt_index = 3
wt_flt_1$Species = 1
wt_flt_1$Sex = 0

wt_flt_2 <- mod0$wtatage[which(mod0$wtatage$fleet == 2),]
wt_flt_2$Wt_name = "Acoustic_Survey"
wt_flt_2$Wt_index = 5
wt_flt_2$Species = 1
wt_flt_2$Sex = 0

wt_flt_3 <- mod0$wtatage[which(mod0$wtatage$fleet == 3),]
wt_flt_3$Wt_name = "Age1_Survey"
wt_flt_3$Wt_index = 6
wt_flt_3$Species = 1
wt_flt_3$Sex = 0

#  -- Biomass
wt_flt_b <- mod0$wtatage[which(mod0$wtatage$fleet == 0),]
wt_flt_b$Wt_name = "Biomass_begin"
wt_flt_b$Wt_index = 2
wt_flt_b$Species = 1
wt_flt_b$Sex = 0

wt_flt_ssb <- mod0$wtatage[which(mod0$wtatage$fleet == -2),]
wt_flt_ssb$Wt_name = "Spawn_wt"
wt_flt_ssb$Wt_index = 4
wt_flt_ssb$Species = 1
wt_flt_ssb$Sex = 0

wt_flt_generic <- mod0$wtatage[which(mod0$wtatage$fleet == -1),]
wt_flt_generic$Wt_name = "Biomass_mid"
wt_flt_generic$Wt_index = 1
wt_flt_generic$Species = 1
wt_flt_generic$Sex = 0


wt_all <- rbind(
    wt_flt_1, wt_flt_2, wt_flt_3, # Fisheries ans surveys
    wt_flt_b, wt_flt_ssb, wt_flt_generic)
wt_all = wt_all[,c("Wt_name","Wt_index","Species","Sex", "year",1:20)]
wt_all <- wt_all[which(wt_all$year >= 1966 & wt_all$year <= 2024),]
write.csv(wt_all, file = "Data/2023hake_wtatage.csv")

# Time series of biomass
write.csv(mod0$timeseries, file = "Data/2023hake_time_series.csv")
