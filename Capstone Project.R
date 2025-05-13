library(readr)
data_trips <- read_csv("Divvy_Trips_2020_Q1.csv")

View(data_trips)
summary(data_trips)
str(data_trips)
colnames(data_trips)

#prints number of members:
#print(nrow(data_trips[data_trips$member_casual == "member", "member_casual"]))
#to save changes to the csv file:
#write.csv(data_trips, "Divvy_Trips_2020_Q1.csv", row.names = FALSE)

library(ggplot2)

#1 Number of rides by membership type:
ggplot(data_trips, aes(x = member_casual)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of rides by membership type",
       x = "Membership type", y = "Number of rides")


#2 Average ride length by membership type
library(dplyr)

avg_ride_by_usertype <- data_trips %>%
  group_by(member_casual) %>%
  summarise(avg_duration = mean(ride_length, na.rm = TRUE))

View(avg_ride_by_usertype)

ggplot(avg_ride_by_usertype, aes(x=member_casual, y=avg_duration)) +
  geom_col(fill="orange") +
  labs(title = "Average ride length by membership type",
       x = "Membership type", y = "Average time (sec)")


#3 Number of rides by membership type and days
ggplot(data_trips, aes(x = factor(day_of_week), fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of rides by membership type and days",
       x = "Day of week (1 = Sunday, 7 = Saturday)", y = "Number of rides")

#4 Average ride length by days
avg_ride_by_day <- data_trips %>%
  group_by(day_of_week) %>%
  summarise(avg_duration = mean(ride_length, na.rm = TRUE))

View(avg_ride_by_day)

ggplot(avg_ride_by_day, aes(x = factor(day_of_week), y = avg_duration)) +
  geom_col(fill = "tomato") +
  labs(title = "Average ride length by days",
       x = "Day of week (1 = Sunday, 7 = Saturday)", y = "Average time (sec)")


#5 Average ride length by days and membership type
avg_ride_by_day_and_usertype <- data_trips %>%
  group_by(day_of_week, member_casual) %>%
  summarise(avg_duration = mean(ride_length, na.rm = TRUE))

View(avg_ride_by_day_and_usertype)

ggplot(avg_ride_by_day_and_usertype, aes(x = factor(day_of_week), y = avg_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average ride length by days and membership type",
       x = "Day of week (1 = Sunday, 7 = Saturday)", y = "Average time (sec)")


#6 top10 starting stations
top10_stations <- data_trips %>%
  filter(!is.na(start_station_name)) %>%
  group_by(start_station_name) %>%
  summarise(ride_count = n()) %>%
  arrange(desc(ride_count)) %>%
  slice_head(n = 10)

#print(top10_stations)
View(top10_stations)

ggplot(top10_stations, aes(x = reorder(start_station_name, ride_count), y = ride_count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 starting stations",
       x = "Station name", y = "Number of rides") +
  theme_minimal()

