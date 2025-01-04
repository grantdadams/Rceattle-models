library(Rceattle)
library(r4ss)
library(plyr)

###########################################################
# READ IN DATA
###########################################################
# CW Long
halibut_cw_long_report <- r4ss::SS_output(dir = "Data/Halibut 2020/Long CW")
halibut_cw_long_natage <- halibut_cw_long_report$natage
halibut_cw_long_natage <- halibut_cw_long_natage[which(halibut_cw_long_natage$`Beg/Mid` == "M"),]
halibut_cw_long_natage <- halibut_cw_long_natage[which(halibut_cw_long_natage$Yr >= 1977 & halibut_cw_long_natage$Yr <= 2021),]
write.csv(halibut_cw_long_natage, file = "Data/Halibut 2020/Long CW/2020_natage_cw_long.csv")

# AAF Long
halibut_aaf_long_report <- r4ss::SS_output(dir = "Data/Halibut 2020/Long AAF")
halibut_aaf_long_natage <- halibut_aaf_long_report$natage
halibut_aaf_long_natage <- halibut_aaf_long_natage[which(halibut_aaf_long_natage$`Beg/Mid` == "M"),]
halibut_aaf_long_natage <- halibut_aaf_long_natage[which(halibut_aaf_long_natage$Yr >= 1977 & halibut_aaf_long_natage$Yr <= 2021),]
write.csv(halibut_aaf_long_natage, file = "Data/Halibut 2020/Long AAF/2020_natage_aaf_long.csv")

# CW Short
halibut_cw_short_report <- r4ss::SS_output(dir = "Data/Halibut 2020/Short CW")
halibut_cw_short_natage <- halibut_cw_short_report$natage
halibut_cw_short_natage <- halibut_cw_short_natage[which(halibut_cw_short_natage$`Beg/Mid` == "M"),]
halibut_cw_short_natage <- halibut_cw_short_natage[which(halibut_cw_short_natage$Yr >= 1992 & halibut_cw_short_natage$Yr <= 2021),]
write.csv(halibut_cw_short_natage, file = "Data/Halibut 2020/Short CW/2020_natage_cw_short.csv")

# AAF short
halibut_aaf_short_report <- r4ss::SS_output(dir = "Data/Halibut 2020/Short AAF")
halibut_aaf_short_natage <- halibut_aaf_short_report$natage
halibut_aaf_short_natage <- halibut_aaf_short_natage[which(halibut_aaf_short_natage$`Beg/Mid` == "M"),]
halibut_aaf_short_natage <- halibut_aaf_short_natage[which(halibut_aaf_short_natage$Yr >= 1992 & halibut_aaf_short_natage$Yr <= 2021),]
write.csv(halibut_aaf_short_natage, file = "Data/Halibut 2020/Short AAF/2020_natage_aaf_short.csv")

