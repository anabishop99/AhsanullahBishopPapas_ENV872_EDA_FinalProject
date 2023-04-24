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
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings in the Gulf of Mexico Over Time") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

### LOOK AT INDIVIDUAL LAT/LONGS ### - Take gom_sightings df, isolate areas I want, then 
# repeat workflow from Lines 20-33

# note - breaking into each lat won't really work b/c some only have a few sightings

# latitudes: range from 20-29; break in half at 25 and compare two categories since there are a
# ton of obvs at lat = 21 and 27+
lower_lats <- gom_sightings %>% filter(latitude >= 20 & latitude < 25) #n = 241
upper_lats <- gom_sightings %>% filter(latitude >= 25 & latitude <= 30) #n = 40

## lower latitude ts
lower_lat_dates <- as.data.frame(seq(as.Date("2002-08-01"), as.Date("2009-12-01"), "months"))
colnames(lower_lat_dates)[1] ="month_yr"

#Select month/yr and total sightings from gom_sightings 
lower_lat_sightings <- lower_lats %>% select(month_yr, total_sightings)
lower_lat_sightings <- distinct(lower_lat_sightings)

#left join that to the new sequence of months and years
lower_lat_sightings_all_months <- left_join(lower_lat_dates, lower_lat_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
lower_lat_sightings_all_months[is.na(lower_lat_sightings_all_months)] <- 0

lower_lat_plot <- ggplot(lower_lat_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings in the Lower Latitudes of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

## upper latitude ts
upper_lat_dates <- as.data.frame(seq(as.Date("2003-10-01"), as.Date("2009-08-01"), "months"))
colnames(upper_lat_dates)[1] ="month_yr"

#Select month/yr and total sightings from gom_sightings 
upper_lat_sightings <- upper_lats %>% select(month_yr, total_sightings)
upper_lat_sightings <- distinct(upper_lat_sightings)

#left join that to the new sequence of months and years
upper_lat_sightings_all_months <- left_join(upper_lat_dates, upper_lat_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
upper_lat_sightings_all_months[is.na(upper_lat_sightings_all_months)] <- 0

upper_lat_plot <- ggplot(upper_lat_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings in the Upper Latitudes of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# longitudes: range from -80--96; break in half at 89 and compare two categories

eastern_longs <- gom_sightings %>% filter(longitude > -86 & longitude <= -80) #n = 12
western_longs <- gom_sightings %>% filter(longitude >= -97 & longitude <= -86) #n = 269

## eastern longitude ts
eastern_long_dates <- as.data.frame(seq(as.Date("2004-10-01"), as.Date("2009-08-01"), "months"))
colnames(eastern_long_dates)[1] ="month_yr"

#Select month/yr and total sightings from gom_sightings 
eastern_long_sightings <- eastern_longs %>% select(month_yr, total_sightings)
eastern_long_sightings <- distinct(eastern_long_sightings)

#left join that to the new sequence of months and years
eastern_long_sightings_all_months <- left_join(eastern_long_dates, eastern_long_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
eastern_long_sightings_all_months[is.na(eastern_long_sightings_all_months)] <- 0

eastern_long_plot <- ggplot(eastern_long_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings in the Eastern Longitudes of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

## western longitude ts
western_long_dates <- as.data.frame(seq(as.Date("2002-08-01"), as.Date("2009-12-01"), "months"))
colnames(western_long_dates)[1] ="month_yr"

#Select month/yr and total sightings from gom_sightings 
western_long_sightings <- western_longs %>% select(month_yr, total_sightings)
western_long_sightings <- distinct(western_long_sightings)

#left join that to the new sequence of months and years
western_long_sightings_all_months <- left_join(western_long_dates, western_long_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
western_long_sightings_all_months[is.na(western_long_sightings_all_months)] <- 0

western_long_plot <- ggplot(western_long_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings in the Western Longitudes of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

# compare all plots together
library(cowplot)
plot_grid(upper_lat_plot, lower_lat_plot, eastern_long_plot, western_long_plot, nrow = 4, align = 'v', rel_heights = c(1, 1, 1, 1))

#need to figure out how to view all plots together
# bad but usable idea - set each date range to the range with the biggest date. Not preferable, this is messy
# or, try to figure out how to align plots







############# Do it again but with each lat/long increment ##################
lat_20 <- gom_sightings %>% filter(latitude >= 20 & latitude < 21) # n = 2
lat_21 <- gom_sightings %>% filter(latitude >= 21 & latitude < 22) # n = 236
lat_22 <- gom_sightings %>% filter(latitude >= 22 & latitude < 23) # n = 1
lat_23 <- gom_sightings %>% filter(latitude >= 23 & latitude < 24) # n = 0
lat_24 <- gom_sightings %>% filter(latitude >= 24 & latitude < 25) # n = 2
lat_25 <- gom_sightings %>% filter(latitude >= 25 & latitude < 26) # n = 3
lat_26 <- gom_sightings %>% filter(latitude >= 26 & latitude < 27) # n = 2
lat_27 <- gom_sightings %>% filter(latitude >= 27 & latitude < 28) # n = 12
lat_28 <- gom_sightings %>% filter(latitude >= 28 & latitude < 29) # n = 18
lat_29 <- gom_sightings %>% filter(latitude >= 29 & latitude < 30) # n = 5

## set same date range for all using min/max of all lats. come back to make sure this is okay
all_lat_dates <- as.data.frame(seq(as.Date("2002-08-01"), as.Date("2009-12-01"), "months"))
colnames(all_lat_dates)[1] ="month_yr"

## lat 20

#Select month/yr and total sightings from gom_sightings 
lat_20_sightings <- lat_20 %>% select(month_yr, total_sightings)
lat_20_sightings <- distinct(lat_20_sightings)

#left join that to the new sequence of months and years
lat_20_sightings_all_months <- left_join(all_lat_dates, lat_20_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
lat_20_sightings_all_months[is.na(lat_20_sightings_all_months)] <- 0

lat_20_plot <- ggplot(lat_20_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings Between Lat 20-21 of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

## lat 21

#Select month/yr and total sightings from gom_sightings 
lat_21_sightings <- lat_21 %>% select(month_yr, total_sightings)
lat_21_sightings <- distinct(lat_21_sightings)

#left join that to the new sequence of months and years
lat_21_sightings_all_months <- left_join(all_lat_dates, lat_21_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
lat_21_sightings_all_months[is.na(lat_21_sightings_all_months)] <- 0

lat_21_plot <- ggplot(lat_21_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings Between Lat 21-22 of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

## lat 22

#Select month/yr and total sightings from gom_sightings 
lat_22_sightings <- lat_22 %>% select(month_yr, total_sightings)
lat_22_sightings <- distinct(lat_22_sightings)

#left join that to the new sequence of months and years
lat_22_sightings_all_months <- left_join(all_lat_dates, lat_22_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
lat_22_sightings_all_months[is.na(lat_22_sightings_all_months)] <- 0

lat_22_plot <- ggplot(lat_22_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings Between Lat 22-23 of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

## lat 23

#Select month/yr and total sightings from gom_sightings 
lat_23_sightings <- lat_23 %>% select(month_yr, total_sightings)
lat_23_sightings <- distinct(lat_23_sightings)

#left join that to the new sequence of months and years
lat_23_sightings_all_months <- left_join(all_lat_dates, lat_23_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
lat_23_sightings_all_months[is.na(lat_23_sightings_all_months)] <- 0

lat_23_plot <- ggplot(lat_23_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings Between Lat 23-24 of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## lat 24

#Select month/yr and total sightings from gom_sightings 
lat_24_sightings <- lat_24 %>% select(month_yr, total_sightings)
lat_24_sightings <- distinct(lat_24_sightings)

#left join that to the new sequence of months and years
lat_24_sightings_all_months <- left_join(all_lat_dates, lat_24_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
lat_24_sightings_all_months[is.na(lat_24_sightings_all_months)] <- 0

lat_24_plot <- ggplot(lat_24_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings Between Lat 24-25 of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

## lat 25

#Select month/yr and total sightings from gom_sightings 
lat_25_sightings <- lat_25 %>% select(month_yr, total_sightings)
lat_25_sightings <- distinct(lat_25_sightings)

#left join that to the new sequence of months and years
lat_25_sightings_all_months <- left_join(all_lat_dates, lat_25_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
lat_25_sightings_all_months[is.na(lat_25_sightings_all_months)] <- 0

lat_25_plot <- ggplot(lat_25_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings Between Lat 25-26 of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

## lat 26

#Select month/yr and total sightings from gom_sightings 
lat_26_sightings <- lat_26 %>% select(month_yr, total_sightings)
lat_26_sightings <- distinct(lat_26_sightings)

#left join that to the new sequence of months and years
lat_26_sightings_all_months <- left_join(all_lat_dates, lat_26_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
lat_26_sightings_all_months[is.na(lat_26_sightings_all_months)] <- 0

lat_26_plot <- ggplot(lat_26_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings Between Lat 26-27 of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

## lat 27

#Select month/yr and total sightings from gom_sightings 
lat_27_sightings <- lat_27 %>% select(month_yr, total_sightings)
lat_27_sightings <- distinct(lat_27_sightings)

#left join that to the new sequence of months and years
lat_27_sightings_all_months <- left_join(all_lat_dates, lat_27_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
lat_27_sightings_all_months[is.na(lat_27_sightings_all_months)] <- 0

lat_27_plot <- ggplot(lat_27_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings Between Lat 27-28 of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## lat 28

#Select month/yr and total sightings from gom_sightings 
lat_28_sightings <- lat_28 %>% select(month_yr, total_sightings)
lat_28_sightings <- distinct(lat_28_sightings)

#left join that to the new sequence of months and years
lat_28_sightings_all_months <- left_join(all_lat_dates, lat_28_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
lat_28_sightings_all_months[is.na(lat_28_sightings_all_months)] <- 0

lat_28_plot <- ggplot(lat_28_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings Between Lat 28-29 of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## lat 29

#Select month/yr and total sightings from gom_sightings 
lat_29_sightings <- lat_29 %>% select(month_yr, total_sightings)
lat_29_sightings <- distinct(lat_29_sightings)

#left join that to the new sequence of months and years
lat_29_sightings_all_months <- left_join(all_lat_dates, lat_29_sightings, by ="month_yr")

#Fill in all NAs as a total sighting of 0
lat_29_sightings_all_months[is.na(lat_29_sightings_all_months)] <- 0

lat_29_plot <- ggplot(lat_29_sightings_all_months, aes(x = month_yr, y = total_sightings)) +
  geom_line() +
  labs(x = "Date", y = "Total Monthly Sightings", title = "Total Monthly Sightings Between Lat 29-30 of the Gulf of Mexico") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# compare all plots together - figure out how to fit all of this together
plot_grid(lat_20_plot, lat_21_plot, lat_22_plot, lat_23_plot, lat_24_plot, lat_25_plot, lat_26_plot, lat_27_plot, lat_28_plot, lat_29_plot, nrow = 10, align = 'v', rel_heights = c(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1))

