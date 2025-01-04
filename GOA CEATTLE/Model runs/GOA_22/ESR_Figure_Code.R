library(ggplot2)

#'
#'
#'
#'Figs for EcoCons CEATTLE
#'
#'
weighted_ration <- function(Rceattle, spp = 1, endyr = 2022, minage = 4, maxage = max(Rceattle$data_list$nages)){
  yrs <- Rceattle$data_list$styr:endyr
  
  return(apply(Rceattle$quantities$ration[spp,,minage:maxage,1:length(yrs)] * Rceattle$quantities$biomassByage[spp,,minage:maxage,1:length(yrs)], 3, sum))
}

ESR_plot_biomass_consumed <- function(Rceattle, endyr = 2022){
  library(dplyr)
  library(ggplot2)
  
  yrs <- Rceattle$data_list$styr:endyr
  
  Biomass_eaten <- rbind(
    data.frame(Year = yrs,Biomass=apply(Rceattle$quantities$B_eaten_as_prey[1,,,1:length(yrs)], 3, sum)/1e6,Species = "a) Prey: Walleye pollock"),
    data.frame(Year = yrs,Biomass=apply(Rceattle$quantities$B_eaten_as_prey[3,,,1:length(yrs)], 3, sum)/1e6,Species = "b) Prey: Pacific cod"),
    data.frame(Year = yrs,Biomass=apply(Rceattle$quantities$B_eaten_as_prey[2,,,1:length(yrs)], 3, sum)/1e6,Species = "c) Prey: Arrowtooth flounder"))
  
  
  Biomass_eaten$Species <- factor( Biomass_eaten$Species,levels = c("a) Prey: Walleye pollock","b) Prey: Pacific cod","c) Prey: Arrowtooth flounder"))
  
  mn<- Biomass_eaten%>%
    group_by(Species)%>%
    summarize(mn = mean(Biomass,na.rm=T),
              sd = sd(Biomass,na.rm=T))
  
  Biomass_eaten <- merge(Biomass_eaten,mn,by="Species")
  Biomass_eaten$upper <- Biomass_eaten$mn + Biomass_eaten$sd
  Biomass_eaten$lower <- Biomass_eaten$mn - Biomass_eaten$sd
  
  ggplot(data=Biomass_eaten,aes(x=Year,y=Biomass,color=Species,fill=Species)) +
    
    geom_line(data=Biomass_eaten,aes(x=Year,y=upper),color="gray",show.legend=FALSE,linetype="dashed")+
    geom_line(data=Biomass_eaten,aes(x=Year,y=mn),color="gray",show.legend=FALSE)+
    geom_line(data=Biomass_eaten,aes(x=Year,y=lower),color="gray",show.legend=FALSE,linetype="dashed")+
    geom_smooth(method = lm, formula = y ~ splines::bs(x, round(length(yrs)/5,0)), se = TRUE,show.legend=FALSE,alpha=.2)+
    geom_point(show.legend=FALSE)+
    facet_grid(Species~.,scales="free_y")+
    scale_color_viridis_d(begin = 0,end=.6)+
    scale_fill_viridis_d(begin = 0,end=.6) + 
    theme(panel.background = element_rect(fill = NA, color = "grey"), panel.grid.major = element_blank(), legend.key=element_blank(), strip.background = element_blank())+
    ylab("Biomass eaten by all model predators (million t)") # for the y axis label
}


ESR_plot_annual_ration <- function(Rceattle, minage = 4, endyr = 2022){
  library(dplyr)
  library(ggplot2)
  
  yrs <- Rceattle$data_list$styr:endyr
  maxage <- max(Rceattle$data_list$nages)
  
  Annual_ration <- rbind(
    data.frame(Year = yrs, Ration=weighted_ration(Rceattle, spp = 1, endyr, minage)/1e6, Species = "a) Predator: Walleye pollock"),
    data.frame(Year = yrs, Ration=weighted_ration(Rceattle, spp = 3, endyr, minage)/1e6, Species = "b) Predator: Pacific cod"),
    data.frame(Year = yrs, Ration=weighted_ration(Rceattle, spp = 2, endyr, minage)/1e6, Species = "c) Predator: Arrowtooth flounder"))
  Annual_ration$Species <- factor( Annual_ration$Species,levels = c("a) Predator: Walleye pollock","b) Predator: Pacific cod","c) Predator: Arrowtooth flounder"))
  
  mn<- Annual_ration %>%
    group_by(Species)%>%
    summarize(mn = mean(Ration,na.rm=T),
              sd = sd(Ration,na.rm=T))
  Annual_ration <- merge(Annual_ration,mn,by="Species")
  Annual_ration$upper <- Annual_ration$mn + Annual_ration$sd
  Annual_ration$lower <- Annual_ration$mn - Annual_ration$sd
  
  ggplot(data=Annual_ration,aes(x=Year,y=Ration,color=Species,fill=Species))+
    geom_line(data=Annual_ration,aes(x=Year,y=upper),color="gray",show.legend=FALSE,linetype="dashed")+
    geom_line(data=Annual_ration,aes(x=Year,y=mn),color="gray",show.legend=FALSE)+
    geom_line(data=Annual_ration,aes(x=Year,y=lower,),color="gray",show.legend=FALSE,linetype="dashed")+
    geom_point(show.legend=FALSE)+
    geom_smooth(method = lm, formula = y ~ splines::bs(x, round(length(yrs)/5,0)), se = TRUE,show.legend=FALSE,alpha=.2)+
    #geom_line(data=Annual_ration,aes(x=Year,y=Ration,color=Species),show.legend=FALSE)+
    
    #geom_smooth(,method="loess")
    facet_grid(Species~.,scales="free_y")+
    scale_color_viridis_d(begin = 0,end=.6)+
    scale_fill_viridis_d(begin = 0,end=.6)+
    theme(panel.background = element_rect(fill = NA, color = "grey"), 
          panel.grid.major = element_blank(), legend.key=element_blank(), 
          strip.background = element_blank()) + 
    ylab("Annual ration (adult; age 4 +)") # for the y axis label
  
}
# energetic demand:



ESR_plot_M_age <-function(msModel, ssModel, age = 1, endyr = 2022){
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  
  yrs <- msModel$data_list$styr:endyr
  
  M2 <- rbind(
    data.frame(Year = c(yrs, 2050), SSM= c(ssModel$quantities$M[1,1,age,1:length(yrs)], 1) , MSM= c(msModel$quantities$M[1,1,age,1:length(yrs)], NA), Species = "a) Prey: Walleye pollock"),
    data.frame(Year = c(yrs, 2050),SSM= c(ssModel$quantities$M[3,1,age,1:length(yrs)], .4) , MSM= c(msModel$quantities$M[3,1,age,1:length(yrs)], NA), Species = "b) Prey: Pacific cod"),
    data.frame(Year = c(yrs, 2050),SSM= c(ssModel$quantities$M[2,1,age,1:length(yrs)], 0.15) , MSM= c(msModel$quantities$M[2,1,age,1:length(yrs)], NA), Species = "c) Prey: Arrowtooth (F)"),
    data.frame(Year = c(yrs, 2050),SSM= c(ssModel$quantities$M[2,2,age,1:length(yrs)], 0.3) , MSM= c(msModel$quantities$M[2,2,age,1:length(yrs)], NA), Species = "d) Prey: Arrowtooth (M)"))
  M2$Species <- factor( M2$Species,levels = c("a) Prey: Walleye pollock","b) Prey: Pacific cod","c) Prey: Arrowtooth (F)", "d) Prey: Arrowtooth (M)"))
  
  M2<- melt(M2%>%dplyr::select(Year,Species,SSM,MSM),id=c("Year","Species"))
  M2<- M2%>%dplyr::rename(Model=variable,totalM=value)
  M2$Model <-factor(M2$Model,levels =c("MSM","SSM"))
  
  ggplot(data=M2,aes(x=Year,y=totalM,color=Species,fill=Species,linetype=Model))+
    geom_line(data=M2%>%filter(Model=="SSM"),show.legend=FALSE)+
    xlim(range(yrs))+
    geom_point(data=M2%>%filter(Model=="MSM"),show.legend=FALSE)+
    geom_smooth(data=M2%>%filter(Model=="MSM"),method = lm, formula = y ~ splines::bs(x, round(length(yrs)/5,0)), se = TRUE,show.legend=FALSE,alpha=.2)+
    facet_grid(Species~.,scales="free_y")+
    scale_color_viridis_d(begin = 0,end=.6)+
    scale_fill_viridis_d(begin = 0,end=.6)+
    ylab("Mortality rate (M1+M2)") + 
    theme(panel.background = element_rect(fill = NA, color = "grey"), panel.grid.major = element_blank(), legend.key=element_blank(), strip.background = element_blank())
  
}

ESR_plot_propM<-function(Rceattle, age = 1, species = 1, sex = 1, endyr = 2022){
  
  yrs <- Rceattle$data_list$styr:endyr
  M2age <- Rceattle$quantities$M2[species,sex,age,1:length(yrs)]
  nspp <- Rceattle$data_list$nspp
  
  M2_pred_pollock  <- colSums(Rceattle$quantities$M2_prop[1 + (nspp * 0), species + (nspp * (sex-1)),,age,1:length(yrs)])
  M2_pred_cod  <- colSums(Rceattle$quantities$M2_prop[3 + (nspp * 0), species + (nspp * (sex-1)),,age,1:length(yrs)])
  M2_pred_atf  <- apply(Rceattle$quantities$M2_prop[2 + (nspp * c(0,1)), species + (nspp * (sex-1)),,age,1:length(yrs)], 3, sum)
  
  M2_pred_pollock_prop <- M2_pred_pollock / M2age
  M2_pred_cod_prop <- M2_pred_cod / M2age
  M2_pred_atf_prop <- M2_pred_atf / M2age
  
  par(mar=c(2,1,0,0)) # margins of graph: (bottom,left, top, right)
  par(mgp=c(2,.5,0)) # axis margins: (distance of lab, distance of numbers, axis line)
  par(oma=c(2,3.5,1,1))# outer margins of graph: (bottom,left, top, right)
  layout(1)
  layout.show(1)
  
  plot(yrs, M2_pred_pollock_prop,ylim=c(0,1),type="l",lwd=2,axes=FALSE,ylab="proportion of total M2",xlab="Year")
  lines(yrs,M2_pred_cod_prop,ylim=c(0,1),type="l",lwd=2,lty=2)
  lines(yrs,M2_pred_atf_prop,ylim=c(0,1),type="l",lwd=2,lty=3, col=1)
  
  axis(1);axis(1,at=c(1900,2050))
  axis(2,las=2);axis(2,at=c(-2,2))
  mtext(side=2,"Proportion of total M2",font=2,line=2)
  mtext(side=1,"Year",font=2,line=2)
  
  legend(1994,0.6,c("Walleye pollock","Pacific cod","Arrowtooth flounder"),bty = "n",lty=1:3,lwd=2, cex = 0.8)
}