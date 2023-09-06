library(tidyverse)
library(skimr)
library(naniar)
library(glue)
library(ggplot2)
library(patchwork)

# Objective: Identify different patterns between casual and members

# ==============================================================================
#                       Summary Statistics & Exploration
# ==============================================================================

# Create Subsets for Comparison Analysis:

# Create causal subset
casual_trips <- trips %>% 
  filter(member_casual == "casual")

# Create member subset
member_trips <- trips %>% 
  filter(member_casual == "member")

# ------------------------------------------------------------------------------
#               Casual vs. Member - Trip Duration Comparison 
# ------------------------------------------------------------------------------

# Benchmark:

# Get average ride duration
mean(trips$trip_duration)
# 14.827 mins

# Get median ride duration
median(trips$trip_duration)
# 9.8 mins

# Average Comparison:

# Get average ride duration on causal
mean(casual_trips$trip_duration)
# 18.97861 mins

# Get average ride duration on member
mean(member_trips$trip_duration)
# 12.35678 mins

# Median Comparison:

# Get median ride duration on causal
median(casual_trips$trip_duration)
# 11.76667 mins

# Get median ride duration on member
median(member_trips$trip_duration)
# 8.833333 mins

# Max Comparison: 

# Get max ride duration on causal
max(casual_trips$trip_duration)
# 1499.917 mins

# Get max ride duration on member
max(member_trips$trip_duration)
# 1499.933 mins

# Min Comparison:

# Get min ride duration on causal
min(casual_trips$trip_duration)
# 1 mins

# Get min ride duration on member
min(member_trips$trip_duration)
# 1 mins

# ------------------------------------------------------------------------------

# Compare the average trip duration by day between members and casual users
avg_duration_by_wday <- trips %>%
  group_by(member_casual, weekday) %>%
  summarise(trip_duration = mean(trip_duration, na.rm = TRUE))

print(avg_duration_by_wday)

# Compare the median trip duration by day between members and casual users
med_duration_by_wday <- trips %>%
  group_by(member_casual, weekday) %>%
  summarise(trip_duration = median(trip_duration, na.rm = TRUE))

print(med_duration_by_wday)

# Compute the total trip duration for each day by user type and weekday
daily_totals_by_wday <- trips %>%
  group_by(member_casual, weekday, date) %>%  
  summarise(daily_trip_duration = sum(trip_duration, na.rm = TRUE)) 

# Compute the average total trip duration per weekday by user type
avg_daily_totals_by_wday <- daily_totals_by_wday %>%
  group_by(member_casual, weekday) %>%
  summarise(avg_trip_duration = mean(daily_trip_duration, na.rm = TRUE))

print(avg_daily_totals_by_wday)

# Visualize avg_duration_by_wday
avg_duration_by_wday_plot <- ggplot(avg_duration_by_wday, aes(x = weekday, y = trip_duration, color = member_casual)) +
  geom_point(size = 3) +
  geom_line(aes(group = member_casual, linetype = member_casual), size = 1) +
  labs(title = "Average Trip Duration by Weekday", y = "Trip Duration", x = "Weekday") +
  theme_minimal()

# Visualize avg_daily_totals_by_wday
avg_daily_totals_by_wday_plot <- ggplot(avg_daily_totals_by_wday, aes(x = weekday, y = avg_trip_duration, group = member_casual, color = member_casual)) +
  geom_line(aes(linetype = member_casual), size = 1) +
  geom_point(size = 3) +
  labs(title = "Average Daily Total Trip Duration by Weekday",
       y = "Daily Total Trip Duration",
       x = "Weekday") +
  theme_minimal()

grid.arrange(avg_duration_by_wday_plot, avg_daily_totals_by_wday_plot, ncol=1)



# ------------------------------------------------------------------------------
#            Casual vs. Member - Frequency Comparison
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
#            Casual vs. Member - Displacement Distance Comparison
# ------------------------------------------------------------------------------





# ==============================================================================
















































# let's see how many round trips are even out there
round_trips <- combined_trips_data %>% 
  filter(
    !is.na(start_station_name) & start_station_name == end_station_name,
    trip_duration > 10 & trip_duration < 300
  ) 

View(round_trips)

# Grouping data by user type
grouped_round_trips <- round_trips %>%
  group_by(member_casual) %>%
  summarise(count = n())

ggplot(grouped_round_trips, aes(x=member_casual, y=count)) + 
  geom_bar(stat="identity", fill="steelblue") +
  labs(title = "Round Trips by User Type",
       x = "User Type",
       y = "Number of Round Trips") +
  theme_minimal()



# 4762517 trips has start station name 


# TO-DO
# 1. Cross fill station id and station name to make the two data points as complete
#    as possible for referencing coordinates
# 2. 






















# ===================Testing Code Start ===================


num_unique_start_station_name <- length(unique(combined_trips_data$start_station_name))
print(num_unique_start_station_name)
num_unique_start_station_id <- length(unique(combined_trips_data$start_station_id))
print(num_unique_start_station_id)
num_unique_end_station_name <- length(unique(combined_trips_data$end_station_name))
print(num_unique_end_station_name)
num_unique_end_station_id <- length(unique(combined_trips_data$end_station_id))
print(num_unique_end_station_id)


num_unique_end_station_id <- length(unique(combined_trips_data$end_station_id))
print(num_unique_end_station_id)



combined_trips_data %>%
  mutate(
    rounded_start_lat = round(start_lat, 2) != start_lat,
    rounded_start_lng = round(start_lng, 2) != start_lng,
    rounded_coords = rounded_start_lat & rounded_start_lng
  ) %>%
  summarise(rounded_percentage = mean(rounded_coords))

combined_trips_data %>%
  mutate(
    rounded_end_lat = round(end_lat, 2) != end_lat,
    rounded_end_lng = round(end_lng, 2) != end_lng,
    rounded_coords = rounded_end_lat & rounded_end_lng
  ) %>%
  summarise(rounded_percentage = mean(rounded_coords, na.rm = TRUE))

combined_trips_data %>%
  mutate(
    rounded_start_lat = round(start_lat, 2) != start_lat,
    rounded_start_lng = round(start_lng, 2) != start_lng,
    rounded_end_lat = round(end_lat, 2) != end_lat,
    rounded_end_lng = round(end_lng, 2) != end_lng,
    rounded_coords = rounded_start_lat & rounded_start_lng & rounded_end_lat & rounded_end_lng
  ) %>%
  summarise(rounded_percentage = mean(rounded_coords, na.rm = TRUE))


two_decimal_end_coords <- combined_trips_data %>% 
  filter(round(end_lat, 2) == end_lat & round(end_lng, 2) == end_lng)

View(two_decimal_end_coords)


df <- combined_trips_data %>%
  mutate(
    less_accurate_start = round(start_lat, 2) != start_lat,
    less_accurate_end = round(end_lng, 2) != end_lng
  )

df %>%
  group_by(member_casual) %>%
  summarise(
    start_accuracy = mean(less_accurate_start),
    end_accuracy = mean(less_accurate_end, na.rm = TRUE)
  )

# Count unique start coordinates
num_unique_start_coordinates <- combined_trips_data %>%
  select(start_lat, start_lng) %>%
  distinct() %>%
  nrow()

print(paste("Number of unique start coordinates:", num_unique_start_coordinates))

# Count unique end coordinates
num_unique_end_coordinates <- combined_trips_data %>%
  select(end_lat, end_lng) %>%
  distinct() %>%
  nrow()

print(paste("Number of unique end coordinates:", num_unique_end_coordinates))


# === Visualize missing value relationships to variables ===

df_with_missing <- combined_trips_data %>%
  filter(is.na(start_station_id))

View(df_with_missing)

# Calculate the proportion of missing values per hour
missing_by_hour <- df_with_missing %>%
  group_by(hour) %>%
  summarise(missing_prop = mean(is_missing))

# Calculate the proportion of missing values per weekday
missing_by_weekday <- df_with_missing %>%
  group_by(weekday) %>%
  summarise(missing_prop = mean(is_missing))

# Calculate proportion of missing values by month
missing_by_month <- df_with_missing %>%
  group_by(month) %>%
  summarise(missing_prop = mean(is_missing))

# Plot the results
plot_hour <- ggplot(missing_by_hour, aes(x = hour, y = missing_prop)) +
  geom_bar(stat = 'identity') +
  labs(x = "Hour of the Day", y = "Proportion of Missing Values",
       title = "Missing Values by Hour of the Day")

plot_weekday <- ggplot(missing_by_weekday, aes(x = weekday, y = missing_prop)) +
  geom_bar(stat = 'identity') +
  labs(x = "Day of the Week", y = "Proportion of Missing Values",
       title = "Missing Values by Day of the Week")

plot_month <- ggplot(missing_by_month, aes(x = month, y = missing_prop)) +
  geom_bar(stat = "identity") +
  labs(x = "Month", y = "Proportion of Missing Values",
       title = "Missing Values by Month") 

plot_hour / plot_weekday / plot_month

# === Visualize correlation between trip volume and time ===

# Calculate the total number of trips by hour
trips_by_hour <- combined_trips_data %>%
  mutate(hour = hour(started_at)) %>%
  count(hour)

# Calculate the total number of trips by weekday
trips_by_weekday <- combined_trips_data %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  count(weekday)

# Calculate the total number of trips by month
trips_by_month <- combined_trips_data %>%
  mutate(month = month(started_at, label = TRUE)) %>%
  count(month)

# Generate the plots
plot_hour <- ggplot(trips_by_hour, aes(x = hour, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Hour of the Day", y = "Number of Trips",
       title = "Trips Distribution by Hour")

plot_weekday <- ggplot(trips_by_weekday, aes(x = weekday, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Day of the Week", y = "Number of Trips",
       title = "Trips Distribution by Weekday")

plot_month <- ggplot(trips_by_month, aes(x = month, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Month", y = "Number of Trips",
       title = "Trips Distribution by Month")

# Patch the plots together
plot_hour / plot_weekday / plot_month 





