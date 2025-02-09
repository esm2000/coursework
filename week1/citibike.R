library(tidyverse)
library(lubridate)

########################################
# READ AND TRANSFORM THE DATA
########################################

# read one month of data
trips <- read_csv('201402-citibike-tripdata.csv')

# replace spaces in column names with underscores
names(trips) <- gsub(' ', '_', names(trips))

# convert dates strings to dates
# trips <- mutate(trips, starttime = mdy_hms(starttime), stoptime = mdy_hms(stoptime))

# recode gender as a factor 0->"Unknown", 1->"Male", 2->"Female"
trips <- mutate(trips, gender = factor(gender, levels=c(0,1,2), labels = c("Unknown","Male","Female")))


########################################
# YOUR SOLUTIONS BELOW
########################################

# count the number of trips (= rows in the data frame)
trips %>% summarize(count = n())

# find the earliest and latest birth years (see help for max and min to deal with NAs)
filter(trips, birth_year == max(birth_year))[2,14]

mutate(trips, birth = as.numeric(birth_year)) %>%
  summarize(min = min(birth, na.rm = TRUE))

# use filter and grepl to find all trips that either start or end on broadway
filter(trips, grepl("Broadway",start_station_name) | grepl("Broadway", end_station_name))

# do the same, but find all trips that both start and end on broadway
filter(trips, grepl("Broadway",start_station_name) & grepl("Broadway", end_station_name))

# find all unique station names
unique(select(trips, start_station_name))

# count the number of trips by gender, the average trip time by gender, and the standard deviation in trip time by gender
# do this all at once, by using summarize() with multiple arguments
summarize(group_by(trips,gender),count = n(),mean = mean(tripduration / 60 ), standard_deviation = sd(tripduration / 60))

# find the 10 most frequent station-to-station trips

trips %>%
  mutate(stations = paste(start_station_name, end_station_name, sep = " || ")) %>%
  group_by(stations) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

# find the top 3 end stations for trips starting from each start station
trips %>%
  group_by(start_station_name) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(3)

# find the top 3 most common station-to-station trips by gender

trips %>%
  mutate(stations = paste(start_station_name, end_station_name, sep = " || ")) %>%
  group_by(gender) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(3)

# find the day with the most trips

trips %>%
  mutate(date = as_date(starttime)) %>%
  group_by(date) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(1)

# tip: first add a column for year/month/day without time of day (use as.Date or floor_date from the lubridate package)

# compute the average number of trips taken during each of the 24 hours of the day across the entire month

trips %>%
  mutate(hour = hour(starttime)) %>%
  group_by(hour) %>%
  summarize(mean = n()/28)
  
# what time(s) of day tend to be peak hour(s)?

trips %>%
  mutate(hour = hour(starttime)) %>%
  group_by(hour) %>%
  summarize(mean = n()/28) %>%
  arrange(desc(mean)) %>%
  head(3)
  

