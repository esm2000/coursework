########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)
library(lubridate)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

View(trips)

# plot the distribution of trip times across all rides
trips %>% 
  mutate(trip_times = tripduration / 60) %>%
  ggplot(aes(x = trip_times)) + 
  geom_histogram(color = 'white') +
  xlim(0,50) + 
  xlab("Trip Time (min)") + 
  scale_y_continuous(label = comma) +
  ylab("Number of Trips") 
  

# plot the distribution of trip times by rider type
trips %>%
  mutate(trip_times = tripduration / 60) %>%
  ggplot(aes(group = usertype, 
             x = trip_times, 
             fill = usertype,
             )) + 
  geom_histogram(color = 'white') + 
  xlim(0,75) + 
  facet_wrap(~ usertype) + 
  ylab("Number of Trips") + 
  scale_y_continuous(label = comma) + 
  xlab("Trip Time (min)") + 
  labs(fill = " ")
  
  
# plot the total number of trips over each day
trips %>%
  group_by(ymd) %>%
  summarize(number_of_trips = n()) %>%
  ggplot(aes(x = ymd, y = number_of_trips)) +
  geom_line() + 
  xlab("Date") + 
  ylab('Number of Trips') + 
  scale_y_continuous(label = comma) 


# plot the total number of trips (on the y axis) by age (on the x axis) and gender (indicated with color)

trips %>%
  mutate(age = (year(ymd) - birth_year)) %>%
  group_by(age, gender) %>%
  summarize(number_of_trips = n()) %>%
  ggplot(aes(x = age, y = number_of_trips, color = gender)) + 
  geom_line() + 
  xlab("Age (years)") + 
  ylab("Number of Trips") + 
  scale_y_continuous(label = comma) + 
  labs(color = "Gender")
  
# plot the ratio of male to female trips (on the y axis) by age (on the x axis)
# hint: use the spread() function to reshape things to make it easier to compute this ratio
trips %>% 
  mutate(age = year(ymd) - birth_year) %>%
  group_by(age, gender) %>%
  summarize(count = n()) %>%
  filter(gender == 'Male'| gender == 'Female') %>%
  spread(gender, count) %>%
  filter(!(is.na(Male)), !(is.na(Female))) %>%
  summarize(ratio = Male / Female) %>%
  ggplot(aes(x = age, y = ratio, color = age, fill = age)) +
  geom_col() +
  ylab('Male:Female Ratio') + 
  xlab('Age (Years)') + 
  xlim(c(18,75))


########################################
# plot weather data
########################################
# plot the minimum temperature (on the y axis) over each day (on the x axis)
weather %>%
  ggplot(aes(x = ymd, y = tmin)) +
  geom_line() +
  ylab('Minimum Temperature (Farenheit)') + 
  xlab('Date') + 
  geom_smooth(method = 'loess')
  
# plot the minimum temperature and maximum temperature (on the y axis, with different colors) over each day (on the x axis)
# hint: try using the gather() function for this to reshape things before plotting
weather %>%
  gather('category','temperature', tmin, tmax) %>%
  ggplot(aes(x = ymd, y = temperature, color = category)) + 
  geom_point()  +
  geom_smooth(method = 'loess') +
  labs(color = ' ') + 
  scale_color_manual(labels = c('Maximum Temperature', 
                                'Minimum Temperature'),
                     values = c('tmin' = 'slateblue1',
                                'tmax' = 'salmon1')
                     ) + 
  xlab('Date') + 
  ylab('Temperature')


########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")
View(trips_with_weather)

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
trips_with_weather %>%
  group_by(tmin) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = tmin, y = count)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  xlab('Minimum Temperature') + 
  ylab('Number of Trips') + 
  scale_y_continuous(label = comma)

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this

trips_with_weather %>%
  group_by(tmin, prcp) %>% 
  summarize(count = n()) %>% 
  mutate(tfpre = (prcp > 0)) %>%
  ggplot(aes(x = tmin, y = count, color = tfpre)) +
  geom_line() +
  xlab('Minimum Temperature') + 
  ylab('Number of Trips') + 
  scale_y_continuous(label = comma) + 
  theme(legend.position = 'none')

# add a smoothed fit on top of the previous plot, using geom_smooth

trips_with_weather %>%
  group_by(tmin, prcp) %>% 
  summarize(count = n()) %>% 
  mutate(tfpre = (prcp > 0)) %>%
  ggplot(aes(x = tmin, y = count, color = tfpre)) +
  geom_line() +
  xlab('Minimum Temperature') + 
  ylab('Number of Trips') + 
  scale_y_continuous(label = comma) + 
  theme(legend.position = 'none') + 
  geom_smooth(method = 'lm') 

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package

average_number_of_trips_per_hour <- trips_with_weather %>%
  group_by(hour(starttime)) %>%
  summarize(mean = n()/365) 

View(average_number_of_trips_per_hour)
  



# plot the above

average_number_of_trips_per_hour %>%
  ggplot(aes(x = 1:24, y = mean, fill = 1:24)) + 
  geom_col() + 
  ylab('Average Number of Trips') + 
  xlab('Hour')

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package

average_number_of_trips_per_day <- trips_with_weather %>%
  group_by(wday(starttime, label = T)) %>%
  summarize(mean = n()/52)

days <- c('Sun','Mon','Tues','Wed','Thurs','Fri','Sat'
           
)
days <- factor(days, levels = c('Sat','Fri','Thurs','Wed','Tues','Mon','Sun'))

average_number_of_trips_per_day %>%
  ggplot(aes(x = days, y = mean, fill = 1:7)) + 
  geom_col() + 
  ylab('Average Number of Trips') + 
  xlab('Day') +
  coord_flip() + 
  labs(fill = ' ')
