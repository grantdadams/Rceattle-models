setwd("~/Documents/GitHub/RceattleRuns/GOA/ATF 2018")

# Catch time series
catch <- read.csv("Groundfish Total Catch by Fishery_ATF.csv")

catch <- aggregate(catch$Weight.Posted..Sum., by=list(Category=catch$Year), FUN=sum)

# Survey biomass
biom <- read.csv("Biomass by Regulatory Area - GOA.csv")
biom <- aggregate(biom$Area.Biomass, by=list(Category=biom$Year), FUN=sum)

# Weighting factor by M
par(mfrow=c(1,2))
nages<-21
getProp<-function(Mf=.3,Mm=.3,nagesIN=nages){
  f<-exp(-Mf*(1:nagesIN))
  m<-exp(-Mm*(1:nagesIN))
  propf<-f/(f+m)
  propm<-m/(f+m)
  plot(propf,ylim=c(0,1),type="l",lty=1);lines(propm,lty=2,lwd=2)
  return(data.frame(age=1:nagesIN,propf,propm))
}
PP<-getProp(Mf=.2,Mm=.35)


# Fsh leng comp
male_dat <- as.data.frame(readxl::read_excel("GOA/ATF 2018/ATF_fsh_length_comp.xlsx", sheet = "males", col_names = FALSE))
female_dat <- as.data.frame(readxl::read_excel("GOA/ATF 2018/ATF_fsh_length_comp.xlsx", sheet = "females", col_names = FALSE))

total_fsh_length <- male_dat * PP[,3] + female_dat * PP[,2] 
write.csv(total_fsh_length, file = "GOA/ATF 2018/ATF_total_fsh_length.csv")


# srv age comp
male_dat <- as.data.frame(readxl::read_excel("GOA/ATF 2018/ATF_srv_age_comp.xlsx", sheet = "males", col_names = FALSE))
female_dat <- as.data.frame(readxl::read_excel("GOA/ATF 2018/ATF_srv_age_comp.xlsx", sheet = "females", col_names = FALSE))

total_fsh_length <- male_dat * PP[,3] + female_dat * PP[,2] 
write.csv(total_fsh_length, file = "GOA/ATF 2018/ATF_total_srv_age.csv")


# srv length comp
male_dat <- as.data.frame(readxl::read_excel("GOA/ATF 2018/ATF_srv_length_comp.xlsx", sheet = "males", col_names = FALSE))
female_dat <- as.data.frame(readxl::read_excel("GOA/ATF 2018/ATF_srv_length_comp.xlsx", sheet = "females", col_names = FALSE))

total_fsh_length <- male_dat * PP[,3] + female_dat * PP[,2] 
write.csv(total_fsh_length, file = "GOA/ATF 2018/ATF_total_srv_length.csv")

# ATF alk
male_dat <- as.data.frame(readxl::read_excel("GOA/ATF 2018/ATF_alk.xlsx", sheet = "males", col_names = FALSE))
female_dat <- as.data.frame(readxl::read_excel("GOA/ATF 2018/ATF_alk.xlsx", sheet = "females", col_names = FALSE))

total_fsh_length <- male_dat * PP[,3] + female_dat * PP[,2] 
write.csv(total_fsh_length, file = "GOA/ATF 2018/ATF_alk_total.csv")

# Weight at age

female_length_age <- 664.4564/10 * (1 - exp(-(0.1535 * (1:21 - -0.6253)))) 
male_length_age <- 494.8802/10 * (1 - exp(-(0.2161 * (1:21 - -0.7776)))) 

average_length_age <- female_length_age * PP[,2] + male_length_age * PP[,3]

female_weight_age <- 0.004312 * female_length_age ^ 3.186
male_weight_age <- 0.004312 * male_length_age ^ 3.186

average_weight_age <- female_weight_age * PP[,2] + male_weight_age * PP[,3]

write.csv(data.frame(Length = average_length_age, Wt = average_weight_age), file = "GOA/ATF 2018/ATF_weight_age.csv")


# Mortality
average_m <- 0.2 * PP[,2] + 0.35 * PP[,3]
