library(FIMS)
library(Rceattle)
library(dplyr)
library(tidyr)

data("data1")
head(data1)

mydata_yfs <- Rceattle::read_data( file = "Data/yfs_single_species_2022.xlsx")
# Prepare the package data for being used in a FIMS model
data_4_model <- FIMSFrame(data1)



unique(data1$type)

# Landings
landings <- data.frame(type = "landings", name = "Fishery_wt", age = NA, length = NA,
                       datestart = paste0(mydata_yfs$catch_data$Year,"-01-01"),
                       dateend = paste0(mydata_yfs$catch_data$Year,"-12-31"),
                       value = mydata_yfs$catch_data$Catch,
                       unit = "mt",
                       uncertainty = mydata_yfs$catch_data$Log_sd)

# Index
head(data1 %>% filter(type == "index"))
index <- data.frame(type = "index", name = "Survey_wt", age = NA, length = NA,
                    datestart = paste0(mydata_yfs$index_data$Year,"-01-01"),
                    dateend = paste0(mydata_yfs$index_data$Year,"-12-31"),
                    value = mydata_yfs$index_data$Observation,
                    unit = "mt",
                    uncertainty = mydata_yfs$index_data$Log_sd)


# Weight
head(data1 %>% filter(type == "weight-at-age"))
weight_tmp <- mydata_yfs$weight %>% pivot_longer(cols = starts_with("Age"),
                                                 names_to = "Age",
                                                 values_to = "values")
weight <- data.frame(type = "weight-at-age", name = weight_tmp$Wt_name,
                     age = as.numeric(gsub("Age", "", weight_tmp$Age)),
                     length = NA,
                     datestart = paste0(weight_tmp$Year,"-01-01"),
                     dateend = paste0(weight_tmp$Year,"-12-31"),
                     value = weight_tmp$values/1000,
                     unit = "mt",
                     uncertainty = NA)

# Comp
head(data1 %>% filter(type == "age"))
comp_tmp <- mydata_yfs$comp_data %>%
  dplyr::filter(Age0_Length1 == 0) %>%
  pivot_longer(cols = starts_with("Comp"),
               names_to = "Comp",
               values_to = "values")

weight <- data.frame(type = "weight-at-age", name = weight_tmp$Wt_name,
                     age = as.numeric(gsub("Age", "", weight_tmp$Age)),
                     length = NA,
                     datestart = paste0(weight_tmp$Year,"-01-01"),
                     dateend = paste0(weight_tmp$Year,"-12-31"),
                     value = weight_tmp$values/1000,
                     unit = "proportion",
                     uncertainty = NA)
