library(tidyverse)
library(readr)
library(lubridate)

# define a function to turn strings into datetimes
parse_datetime <- function(s, format="%Y-%m-%d %H:%M:%S") {
  as.POSIXct(as.character(s), format=format)
}


csvs <- Sys.glob('*-tripdata.csv')
trips <- data.frame()
for (csv in csvs) {
  print(csv)
  tmp <- read_csv(csv, na='\\N')
  
  # the date format changed to something ugly in 2014-09 which read_csv doesn't recognize as a datetime,
  # so manually convert the date from a string to a datetime
  if (typeof(tmp$starttime) == "character")
    tmp <- mutate(tmp,
                  starttime=parse_datetime(starttime, "%m/%d/%Y %H:%M"),
                  stoptime=parse_datetime(stoptime, "%m/%d/%Y %H:%M"))
  
  trips <- rbind(trips, tmp)
}

# replace spaces in column names with underscores
names(trips) <- gsub(' ', '_', names(trips))

# add a column for year/month/day (without time of day)
trips <- mutate(trips, ymd=as.Date(starttime))

# recode gender as a factor 0->"Unknown", 1->"Male", 2->"Female"
trips <- mutate(trips, gender=factor(gender, levels=c(0,1,2), labels=c("Unknown","Male","Female")))
# -----------------------------------------------------------------

# load weather data from belvedere tower in central park
# https://www.ncei.noaa.gov/orders/cdo/762757.csv
# ordered from
# http://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USW00094728/detail
weather <- read.table('weather_2015.csv', header=T, sep=',')

# extract just a few columns, lowercase column names, and parse dates
weather <- select(weather, DATE, PRCP, SNWD, SNOW, TMAX, TMIN)
names(weather) <- tolower(names(weather))
weather <- mutate(weather,
                  ymd = as.Date(parse_datetime(date, "%Y%m%d")))
weather <- tbl_df(weather)

#---------------------------------------------------------------

holidays <- read_csv('holiday_2014.csv') %>% 
  mutate(is_holiday = TRUE)

# creating a column to categorize whether a day is in the 
# weekend or not

trips <- trips %>%
  mutate(month = month(ymd)) %>%
  mutate(day = wday(ymd)) %>%
  mutate(is_weekend = day == 1 | day == 7)

# creation of a column to categorize whether a day is a holiday
# or not
trips <- left_join(trips, holidays)
trips$is_holiday <- replace_na(trips$is_holiday, FALSE)

# getting num_trips
num_trips <- trips %>%
  group_by(ymd) %>%
  summarize(num_trips = n(), 
            is_weekend = as.logical(mean(is_weekend)),
            is_holiday = as.logical(mean(is_holiday))) %>%
  mutate(day = 1:366)

weather <- weather %>%
  mutate(day = 1:365) %>%
  mutate(tmin = tmin / 10)

# joining by dates wasn't working out 
# creation of a column to categorize whether a day is a holiday
# or not
num_trips <- left_join(num_trips, holidays)
num_trips_weather <- left_join(num_trips, weather,
                               by = 'day')


# RMSE AND RSQ
rmse <- rmse(model7,num_trips_weather)
rsq <- rsquare(model7,num_trips_weather)

paste(c('RMSE = ' , rmse))
paste(c('R-SQUARE = ', rsq))

# Graph 1
num_trips_weather %>% 
  add_predictions(model7) %>%
  ggplot(aes(x = ymd.x)) +
  geom_point(aes(y = num_trips), color = 'blue') +
  geom_line(aes(y = pred), color = 'red') + 
  geom_smooth(aes(y = pred), color = 'red', lwd = 1.5) + 
  xlab('Date') + 
  ylab('Number of Trips') + 
  ggtitle('Number of Trips by Day in 2015 + Model')

# Graph 1.5 
num_trips_weather %>% 
  add_predictions(model7) %>%
  ggplot(aes(x = ymd.x)) +
  geom_point(aes(y = num_trips), color = 'blue') + 
  geom_smooth(aes(y = pred), color = 'red', lwd = 1.5) + 
  xlab('Date') + 
  ylab('Number of Trips') + 
  ggtitle('Number of Trips by Day in 2015 + Smoothed Model')

# Graph 2
num_trips_weather %>%
  add_predictions(model7) %>%
  ggplot(aes(x = pred, y = num_trips)) + 
  geom_point() + 
  geom_abline(aes(slope = 1, intercept = 0), 
              color = 'red', lwd = 1.5) + 
  xlab('Predicted Number of Trips') + 
  ylab('Actual Number of Trips') + 
  ggtitle('Predicted vs. Actual Number of Trips by Day in 2015')
