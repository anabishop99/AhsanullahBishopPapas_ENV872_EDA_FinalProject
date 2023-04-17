library(tidyverse)
library(lubridate)
library(trend)
library(zoo)
library(tseries)
library(dplyr)

#read in data
gom_sightings <- read_csv("./Data/whaleshark_data/cleaned_gom_data.csv")

## calculate number of sightings per month/year
#isolate month and year
gom_sightings <- mutate(gom_sightings, month = month(date)) %>% 
  mutate(gom_sightings, year = year(date)) %>% 
  mutate(month_yr = my(paste(month,"-",year)))

#sum number of sightings per month-year
gom_sightings <- gom_sightings %>% group_by(month_yr) %>% mutate(total_sightings = n())

#need to fill in all missing months/years
#generate sequence of all months and years between August 2002 and December 2009
dates <- as.data.frame(seq(as.Date("2002-08-01"), as.Date("2009-12-01"), "months"))
colnames(dates)[1] ="month_yr"

#Select month/yr and total sightings from gom_sightings 
sightings <- gom_sightings %>% select(month_yr, total_sightings)
sightings <- distinct(sightings)
 
#left join that to the new sequence of months and years
sightings_all_months <- left_join(dates, sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
sightings_all_months[is.na(sightings_all_months)] <- 0

#initial timeseries
ggplot(sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings") +
  theme_minimal()
