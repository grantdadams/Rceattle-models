library(ggplot2)

#'
#'
#'
#'Figs for ESP CEATTLE
#'
#'
weighted_ration <- function(Rceattle, spp = 1, endyr = 2022, minage = 4, maxage = max(Rceattle$data_list$nages)){
  yrs <- Rceattle$data_list$styr:endyr
  
  return(apply(Rceattle$quantities$ration[spp,,minage:maxage,1:length(yrs)] * Rceattle$quantities$biomassByage[spp,,minage:maxage,1:length(yrs)], 3, sum))
}



ESP_plot <-function(msModel, ssModel, age = 1, minage = 4, endyr = 2023, species = 1){
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  
  yrs <- msModel$data_list$styr:endyr
  
  
  # - M-at-age ----
  M2 <- rbind(
    data.frame(Year = c(yrs, 2050), SSM= c(ssModel$quantities$M[1,1,age,1:length(yrs)], 1) , MSM= c(msModel$quantities$M[1,1,age,1:length(yrs)], NA), Spp = 1,  Species = "Prey: Walleye pollock"),
    data.frame(Year = c(yrs, 2050),SSM= c(ssModel$quantities$M[3,1,age,1:length(yrs)], .4) , MSM= c(msModel$quantities$M[3,1,age,1:length(yrs)], NA), Spp = 3,  Species = "Prey: Pacific cod"),
    data.frame(Year = c(yrs, 2050),SSM= c(ssModel$quantities$M[2,1,age,1:length(yrs)], 0.15) , MSM= c(msModel$quantities$M[2,1,age,1:length(yrs)], NA), Spp = 2,  Species = "Prey: Females"),
    data.frame(Year = c(yrs, 2050),SSM= c(ssModel$quantities$M[2,2,age,1:length(yrs)], 0.3) , MSM= c(msModel$quantities$M[2,2,age,1:length(yrs)], NA), Spp = 2,  Species = "Prey: Males"))
  M2$Species <- factor( M2$Species,levels = c("Prey: Walleye pollock","Prey: Pacific cod","Prey: Females", "Prey: Males"))
  
  M2 <- M2 %>% filter(Spp == species)
  M2<- melt(M2%>%dplyr::select(Year,Species,SSM,MSM),id=c("Year","Species"))
  M2<- M2%>%dplyr::rename(Model=variable,totalM=value)
  M2$Model <-factor(M2$Model,levels =c("MSM","SSM"))
  
  g1 <- ggplot(data=M2,aes(x=Year,y=totalM,color=Species,fill=Species,linetype=Model))+
    geom_line(data=M2%>%filter(Model=="SSM"),show.legend=FALSE)+
    xlim(range(yrs))+
    geom_point(data=M2%>%filter(Model=="MSM"),show.legend=FALSE)+
    geom_smooth(data=M2%>%filter(Model=="MSM"),method = lm, formula = y ~ splines::bs(x, round(length(yrs)/5,0)), se = TRUE,show.legend=FALSE,alpha=.2)+
    facet_grid(Species~.,scales="free_y")+
    scale_color_viridis_d(begin = 0,end=.6)+
    scale_fill_viridis_d(begin = 0,end=.6)+
    ylab("Mortality rate (M1+M2)") + 
    theme(panel.background = element_rect(fill = NA, color = "grey"), panel.grid.major = element_blank(), legend.key=element_blank(), strip.background = element_blank())
  
  
  
  # B-eaten ----
  yrs <- msModel$data_list$styr:endyr
  
  Biomass_eaten <- rbind(
    data.frame(Year = yrs,Biomass=apply(msModel$quantities$B_eaten_as_prey[1,,,1:length(yrs)], 3, sum)/1e6, Spp = 1, Species = "Prey: Walleye pollock"),
    data.frame(Year = yrs,Biomass=apply(msModel$quantities$B_eaten_as_prey[3,,,1:length(yrs)], 3, sum)/1e6, Spp = 3,Species = "Prey: Pacific cod"),
    data.frame(Year = yrs,Biomass=apply(msModel$quantities$B_eaten_as_prey[2,,,1:length(yrs)], 3, sum)/1e6, Spp = 2,Species = "Prey: Arrowtooth flounder"))
  
  
  Biomass_eaten$Species <- factor( Biomass_eaten$Species,levels = c("Prey: Walleye pollock", "Prey: Pacific cod", "Prey: Arrowtooth flounder"))
  
  mn<- Biomass_eaten%>%
    group_by(Species)%>%
    summarize(mn = mean(Biomass,na.rm=T),
              sd = sd(Biomass,na.rm=T))
  
  Biomass_eaten <- merge(Biomass_eaten,mn,by="Species")
  Biomass_eaten$upper <- Biomass_eaten$mn + Biomass_eaten$sd
  Biomass_eaten$lower <- Biomass_eaten$mn - Biomass_eaten$sd
  
  Biomass_eaten <- Biomass_eaten %>%
    filter(Spp == species)
  
  g2 <- ggplot(data=Biomass_eaten,aes(x=Year,y=Biomass,color=Species,fill=Species)) +
    
    geom_line(data=Biomass_eaten,aes(x=Year,y=upper),color="gray",show.legend=FALSE,linetype="dashed")+
    geom_line(data=Biomass_eaten,aes(x=Year,y=mn),color="gray",show.legend=FALSE)+
    geom_line(data=Biomass_eaten,aes(x=Year,y=lower),color="gray",show.legend=FALSE,linetype="dashed")+
    geom_smooth(method = lm, formula = y ~ splines::bs(x, round(length(yrs)/5,0)), se = TRUE,show.legend=FALSE,alpha=.2)+
    geom_point(show.legend=FALSE)+
    facet_grid(Species~.,scales="free_y")+
    scale_color_viridis_d(begin = 0,end=.6)+
    scale_fill_viridis_d(begin = 0,end=.6) + 
    theme(panel.background = element_rect(fill = NA, color = "grey"), panel.grid.major = element_blank(), legend.key=element_blank(), strip.background = element_blank())+
    ylab("Biomass eaten by predators (million t)") # for the y axis label
  
  
  # Ration ----
  maxage <- max(msModel$data_list$nages)
  
  Annual_ration <- rbind(
    data.frame(Year = yrs, Ration=weighted_ration(msModel, spp = 1, endyr, minage)/1e6, Spp = 1, Species = "Predator: Walleye pollock"),
    data.frame(Year = yrs, Ration=weighted_ration(msModel, spp = 3, endyr, minage)/1e6, Spp = 3, Species = "Predator: Pacific cod"),
    data.frame(Year = yrs, Ration=weighted_ration(msModel, spp = 2, endyr, minage)/1e6, Spp = 2, Species = "Predator: Arrowtooth flounder"))
  Annual_ration$Species <- factor( Annual_ration$Species,levels = c("Predator: Walleye pollock", "Predator: Pacific cod", "Predator: Arrowtooth flounder"))
  
  mn<- Annual_ration %>%
    group_by(Species)%>%
    summarize(mn = mean(Ration,na.rm=T),
              sd = sd(Ration,na.rm=T))
  Annual_ration <- merge(Annual_ration,mn,by="Species")
  Annual_ration$upper <- Annual_ration$mn + Annual_ration$sd
  Annual_ration$lower <- Annual_ration$mn - Annual_ration$sd
  
  Annual_ration <- Annual_ration %>%
    filter(Spp == species)
  
  g3 <- ggplot(data=Annual_ration,aes(x=Year,y=Ration,color=Species,fill=Species))+
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
  
  cowplot::plot_grid(g1, g2, g3, ncol = 1)
  
}