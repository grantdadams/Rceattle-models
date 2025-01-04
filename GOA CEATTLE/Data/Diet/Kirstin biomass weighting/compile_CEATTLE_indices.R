# Convert diet indices into indices for Bering and GOA CEATTLE models:

require(dplyr)
require(ggplot2)
require(reshape2)

rm(list=ls())

# setup
diet_path <- "/Users/kholsman/Documents/D_AFSC_Files/AFSC_data/01_SRVY_AFSC_data_all/Newest/Out_SMRY_QRYS"
binSET    <- list(juv=c(40,50,40,70),adult=c(80,90,70,100))
nrgMeta   <- c("BT_fish","SST_fish","mnPredL","mnPredW","W_use", "RFR_C1", "RFR", "G_potential_ggd")
plotvars <-  c("BT_fish","SST_fish", "mnPredL","mnPredW","W_use","RFR_C1","RFR","G_potential_ggd")
lvls      <- c("Juv","Adult")

# load data
load(file.path(diet_path,"Biom_weighted/annual_mn_bybin/GOA_c_ED_annual_mn_bybin.Rdata"))
load(file.path(diet_path,"Biom_weighted/annual_mn/GOA_c_ED_annual_mn.Rdata"))

# Get mn vals:
GOA_yr <- GOA_annual_mn%>%
          filter(CN!="sablefish")%>%
          select(YEAR, REGION,SN, CN, 
                 BT_fish = BT,  
                 SST_fish = SST,RFR_C1, RFR, 
                 G_potential_ggd = G_ggd)

# get mn by bin vals:

GOA_yr_bin <- GOA_annual_mn_bybin%>%
  filter(CN!="sablefish")%>%
  select(YEAR,BIN_cm_mid, REGION,SN, CN, sum_propB_yk, 
         BT_fish = BT, 
         SST_fish = SST, 
         mnPredL=PredL, 
         mnPredW=PredW,W_use, RFR_C1, RFR, 
         G_potential_ggd = G_ggd)



GOA_yr%>%filter(CN == "walleye pollock")
GOA_yr_bin%>%filter(CN == "walleye pollock")


# Juv and adult vals:
BINS    <- sort(unique(GOA_yr_bin$BIN_cm_mid))
spIN    <- sort(unique(GOA_yr_bin$CN))
spIN    <- factor(spIN,levels=levels(GOA_yr_bin$CN))


  dd <- GOA_yr_bin%>%
    group_by(YEAR,CN,BIN_cm_mid)%>%
    filter(CN%in%spIN)%>%
    select(YEAR,CN,BIN_cm_mid,nrgMeta)%>%
    mutate(Bin = factor("Adult",levels=c("Juv","Adult","RM")))
  
  for(s in 1:length(spIN)){
    dd$Bin[dd$CN==spIN[s]&dd$BIN_cm_mid<=binSET[[1]][s]]<-factor("Juv",levels=c("Juv","Adult","RM"))
    dd$Bin[dd$CN==spIN[s]&dd$BIN_cm_mid>binSET[[2]][s]] <-factor("RM",levels=c("Juv","Adult","RM"))
  }
  
  dd<- dd[dd$Bin!="RM",]
  dd$Bin<- factor(dd$Bin,levels=c("Juv","Adult"))
  dd <- dd%>%
    group_by(YEAR,CN,Bin)%>%
    summarize_at(nrgMeta,mean,na.rm=T)
  nspp     <- length(spIN)

  GOA_yr_juvAdult <- dd
  
# now scaled versions:
    for(i in 1:length(plotvars)){
      for(s in 1:nspp){
        for(b in 1:length(lvls)){
          ddd <- data.frame(dd)[dd$Bin==lvls[b]&dd$CN==spIN[s],names(dd)==plotvars[i]] 
          dd[dd$Bin==lvls[b]&dd$CN==spIN[s],plotvars[i]]<- as.numeric(scale(  ddd))
        }
      }  
    }
  
  GOA_yr_juvAdult_scaled <- dd

# save it:
  save(GOA_yr_juvAdult, file=file.path(diet_path,"ForGrant/2019_data_2020_06_19/GOA_yr_juvAdult.Rdata"))
  save(GOA_yr_juvAdult_scaled, file=file.path(diet_path,"ForGrant/2019_data_2020_06_19/GOA_yr_juvAdult_scaled.Rdata"))
  save(GOA_yr_bin, file=file.path(diet_path,"ForGrant/2019_data_2020_06_19/GOA_yr_bin.Rdata"))
  save(GOA_yr_juvAdult, file=file.path(diet_path,"ForGrant/2019_data_2020_06_19/GOA_annual_mn.Rdata"))
  
#plot it:
  p <- ggplot(GOA_yr,aes(x= YEAR, y=RFR_C1, color = YEAR)) 
  p +  geom_point(aes(x= YEAR, y=RFR_C1, color = factor(YEAR))) +
    facet_grid(CN~REGION, scales="free_y") + theme_minimal()
  
  
  p <- ggplot(GOA_yr_bin,aes(x= mnPredL, y=RFR, color = YEAR)) 
  p +  geom_point(aes(x= mnPredL, y=RFR, color = factor(YEAR))) +
    facet_grid(CN~REGION, scales="free_y") + theme_minimal()
