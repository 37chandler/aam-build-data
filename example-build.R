

# This file builds a fake data set that simulates 
# the number of traffic accidents on Lyndale between Franklin and Lake
# streets in Minneapolis (normalized per 10K trips). Explanatory variables that influence
# crashes are time of day, weather, and day of week. 
#
# Goal is to average about 5 on weekdays with good weather, 2 on weekends. 

library(tidyverse)

num.rows <- 1000
weekday.coefs <- tibble(weekday=c("M","Tu","W","Th","Fr","Sa","Su"),
                        wd_coef=c(3.5,2,1.5,1.5,4,1,0.5))

weather.coefs <- tibble(weather = c("sunny","rainy","snowy"),
                        wx_coef = c(0,1,2))

time.coefs <- tibble(time=c("overnight","morning rush","midday","evening rush"),
                     t_coef=c(2,0.5,0,1.5))

# Use set.seed to make your results reproducible.
set.seed(20210218)

# Make the data
fake.data <- tibble(row=1:num.rows)
fake.data$weekday <- sample(weekday.coefs$weekday,size=num.rows,replace=T)
fake.data$weather <- sample(weather.coefs$weather,size=num.rows,replace=T,prob=c(0.5,0.2,0.3))
fake.data$time <- sample(time.coefs$time,size=num.rows,replace=T)

# Join on the coefs
fake.data <- fake.data %>% 
  left_join(weekday.coefs,by="weekday") %>% 
  left_join(weather.coefs,by="weather") %>% 
  left_join(time.coefs,by="time")

# Add on some unused continuous variables. 
fake.data <- fake.data %>% 
  mutate(consumer_satisfaction = rnorm(num.rows,mean=50,sd=15),
         pedestrian_level = rpois(num.rows,5))

# Now make the response. It'll be the coefs plus some error
fake.data <- fake.data %>% 
  mutate(accidents_per_10K = wd_coef + wx_coef + t_coef + rnorm(num.rows,
                                                                mean=0, # this must be set to 0
                                                                sd=2),
         accidents_per_10K = pmax(accidents_per_10K,0)) # ensure positive

fake.data %>% 
  group_by(weekday) %>% 
  summarize(m=mean(accidents_per_10K)) %>% 
  arrange(m)

# Drop the coefficients
fake.data <- fake.data %>% 
  select(-contains("coef"))

write_tsv(fake.data,"fake_traffic_accident_data.txt")


