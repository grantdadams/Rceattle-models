# May 2019 - Grant Adams
library(r4ss)

####################################################
## COMPARISON OF MULTIPLE MODELS
## from ?SSplotComparisons
####################################################
# directories where models were run need to be defined
setwd("../") # Move back one
dir0 <- '~/Documents/GitHub/RceattleRuns/GOA/Data/Pcod 2018/GOApcod_Appendix2.3/2018 GOA Pacific cod figures and Files/Model18.09.35' # 3.24 Model
mod0 <- SS_output(dir=dir0)

n1 <- mod0$natage
n1 <- n1[which(n1$`Beg/Mid` == "B"),]
write.csv(n1, file = "natage.csv")
