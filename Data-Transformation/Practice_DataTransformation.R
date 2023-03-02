# data transformation in R programming
# homework practice
# using flight data and create 5 business questions

# install relevant libraries
library(nycflights13)
library(tidyverse)
library(dplyr)

# load data
data("flights")

glimpse(flights)
View(flights)

# check missing value (NA) in column
check_na <- function(col){
  sum(is.na(col))
}

apply(flights, MARGIN = 2, check_na)

flights %>%
  select(dep_time, dep_delay, arr_time, arr_delay, tailnum, air_time) %>%
  filter(is.na(dep_time))

# check missing value (NA) in rows
total_rows <- nrow(flights)
complete_row <- sum(complete.cases(flights))
missing_rows <- total_rows - complete_row

missing_rows

# since missing value is 9430 rows from total 336776 which is about 2.8%, we drop every rows of missing value (NA)
flights_clean <- drop_na(flights)

flights_clean
View(flights_clean)
View(airlines)

# question 1
# which destination is the most frequent flights from New York airports in 2013?
flights_clean %>%
  distinct(dest)

dest_flights <- flights_clean %>%
  filter(year == 2013) %>%
  count(dest) %>%
  arrange(desc(n))

# question 2
# which airline/ carrier has the highest flight number from New York airports in 2013?
# join flights table with airline table
flights_join <- flights_clean %>%
  left_join(airlines, by = "carrier")

flights_join

re_flights_join <- flights_join %>%
  rename(airlines_name = name)

re_flights_join
View(re_flights_join)

# filter airline
re_flights_join %>%
  distinct(carrier)

re_flights_join %>%
  filter(year == 2013) %>%
  count(carrier) %>%
  arrange(desc(n))

# question 3
# which month has the highest flight number (most frequent) in 2013?
re_flights_join %>%
  filter(year == 2013) %>%
  count(month) %>%
  arrange(desc(n))

# question 4
# which airline/carrier has the most frequent on departure delay from New York airports in 2013?
# calculate the number of departure delays for each carrier
depart_delay <- re_flights_join %>%
  group_by(carrier) %>%
  summarise(num_delays = sum(dep_delay > 0)) 

# find the carrier with the most departure delays
most_delay_carrier <- depart_delay %>%
  filter(num_delays == max(num_delays))

# question 5
# which airline/carrier has the most frequent on arrival delay from New York airports in 2013?
# calculate the number of arrival delays for each carrier
arr_delay <- re_flights_join %>%
  group_by(carrier) %>%
  summarise(num_delays = sum(arr_delay > 0)) 

# find the carrier with the most arrival delays
most_arr_carrier <- arr_delay %>%
  filter(num_delays == max(num_delays))








