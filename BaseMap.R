#load libraries
library(tidyverse)
library(lubridate)
library(here)
library(dplyr)
library(sf)

#check wd
getwd()

#load csvs
points <- read_csv("./Data/whaleshark_data/dataset_points.csv")
citation <- read_csv("./Data/whaleshark_data/dataset_citation.csv")


gom <- points %>% filter(latitude >= 17 & latitude <= 30) %>% filter (longitude >= -98 & longitude <= -79)

gom_clean <- gom %>% separate(date_time, into = c('date','time'), sep=" ")

gom_clean$date <- ymd(gom_clean$date)

write.csv(gom_clean, "./Data/whaleshark_data/cleaned_gom_data.csv", row.names=FALSE)

#removing unneccessary data
gom_final<- gom_clean %>% 
  select(dataset_id, row_id, latitude, longitude, species_name, date, time, last_mod)

#loading packages
library(sf)
library(mapview); mapviewOptions(fgb = FALSE)
library(leaflet)

#Creating Baseline Maps
gom_sf<-st_as_sf(gom_final, coords = c("longitude","latitude"), crs=4326)

ggplot(gom_sf)+
  geom_sf(aes(geometry=geometry))

mapview(gom_sf)
