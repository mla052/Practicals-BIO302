library("tidyverse")

sykkel <- read_csv('03.csv')

sykkel


#Which is the most popular starting station?
sykkel %>%
  count(start_station_name) %>%
  arrange(desc(n)) %>%
  slice(1:3)

# answer: 
# A tibble: 3 x 2
# start_station_name      n
#  <chr>                <int>
#  1 Møllendalsplass     1502
#  2 Cornerteateret      1238
#  3 Nykirken            1163


#Plot the number of hires and returns from each station
sykkel %>%
  count(start_station_name) %>%
  view() %>%
  ggplot(aes(x = n, y = start_station_name)) + geom_point()

sykkel %>%
  count(end_station_name) %>%
  ggplot(aes(x = n, y = end_station_name)) + geom_point()


#Which is the most popular pair of start and end stations?
sykkel %>%
  count(start_station_name, end_station_name) %>%
  arrange(desc(n)) %>%
  slice(1:3)

# Answer:
# start_station_name end_station_name       n
#   <chr>               <chr>             <int>
#  1 Møllendalsplass    Nonneseterplass    231
#  2 Nonneseterplass    Møllendalsplass    220
#  3 Nykirken           Småstrandgaten     183


# What was the longest/shortest duration of hire?
sykkel %>%
  mutate(duration = ended_at - started_at) %>%
  arrange(desc(duration)) %>%
  slice(1:5) %>%
  select(duration)

# Answer in min longest:
# duration     
#     <drtn>       
#  1 2749.988 mins
#  2 2146.422 mins
#  3 1837.880 mins
#  4 1687.030 mins
#  5 1422.614 mins

sykkel %>%
  mutate(duration = ended_at - started_at) %>%
  arrange(duration) %>%
  slice(5:1) %>%
  select(duration)

# Answer shortest time
#    duration     
#     <drtn>       
#  1 1.056333 mins
#  2 1.045983 mins
#  3 1.045000 mins
#  4 1.033933 mins
#  5 1.029600 mins

# Plot the distribution of hire duration.
sykkel %>%
  count(duration) %>%
  ggplot(aes(x = duration, y = n))  + geom_line() + coord_trans(x = "log10")


# What is the median duration of hire from each station?
sykkel %>%
  group_by(start_station_name) %>%
  summarise(mean_duration = mean(duration), median_duration = median(duration)) %>%
  view()       # se resultater ved å trykke her

# Map this information
sykkel %>%
  group_by(start_station_name, start_station_latitude, start_station_longitude) %>%
  summarise(median_duration = median(duration)) %>%
  ggplot(aes(x = start_station_longitude, y = start_station_latitude)) + geom_point()


# Are there any significant differences in duration between stations.
sykkel %>%
  count(start_station_name) %>%
  view()

lm(data = sykkel, duration ~ start_station_name) %>%
  anova
# p-value is 8.925e-08, there is a significant difference
# but between what stations?

lm(data = sykkel, duration ~ start_station_name) %>%
  summary()
# there is a sinificant difference for Høgskulen på Vestlandet, Lysverket and Verftet. 


# How far does a typical cyclist travel?
sykkel %>%
  group_by(start_station_longitude, start_station_latitude, end_station_longitude, end_station_latitude) %>%
  mutate(distance = sqrt((start_station_longitude - end_station_longitude)^2+(start_station_latitude - end_station_latitude)^2), abs(distance)) %>%
  select(distance) %>%
  ungroup() %>%
  summarise(mean_distance = mean(distance))

# Answer mean distance:
# 0.0152

# What is the relationship between distance travelled and time taken?
sykkel %>%
  mutate(distance = sqrt((start_station_longitude - end_station_longitude)^2+(start_station_latitude - end_station_latitude)^2), abs(distance)) %>%
  ggplot(aes(x = distance, y = duration)) + geom_point() + stat_smooth(method ="lm", formula = y~x, se=F)

# How fast was the fastest cyclist (for simplicity assume a straight line of travel)
sykkel %>%
  mutate(speed = abs(sqrt((start_station_longitude - end_station_longitude)^2+(start_station_latitude - end_station_latitude)^2)) / duration) %>%
  arrange(desc(speed)) %>%
  slice(1:3) %>%
  select(speed)

# Answer: 
#    speed
#     <dbl>
#  1 0.000117 
#  2 0.0000860
#  3 0.0000860

## DAY AND TIME ##








