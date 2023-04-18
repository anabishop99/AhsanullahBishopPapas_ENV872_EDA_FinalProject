#load libraries
library(tidyverse)
library(lubridate)

#check wd
getwd()

#load csvs
points <- read_csv("./Data/whaleshark_data/dataset_points.csv")
citation <- read_csv("./Data/whaleshark_data/dataset_citation.csv")


gom <- points %>% filter(latitude >= 17 & latitude <= 30) %>% filter (longitude >= -98 & longitude <= -79)

gom_clean <- gom %>% separate(date_time, into = c('date','time'), sep=" ")

gom_clean$date <- ymd(gom_clean$date)

write.csv(gom_clean, "./Data/whaleshark_data/cleaned_gom_data.csv", row.names=FALSE)