# Import `trips_combined` 
trips <- read_csv("data/interim/trips_combined.csv")

# ==============================================================================
#                        Initial Exploration & Cleaning
# ==============================================================================

skim(trips)
View(trips)

# Investigate incomplete cases
incomplete <- trips %>% 
  filter(!complete.cases(.))

View(incomplete_cases)

# Convert non-id character columns to lowercase
trips <- trips %>% 
  mutate(across(c(rideable_type, start_station_name, end_station_name), tolower))

# Trim trailing and leading spaces in all character columns
trips <- trips %>% mutate(across(where(is.character), trimws))

# Ensure no duplicates in `ride_id`
print(any(duplicated(trips$ride_id)))

# Add trip_duration column
trips <- trips %>% 
  mutate(trip_duration = difftime(ended_at, started_at, units = "mins"))

# Investigate negative duration trips
negative_duration <- trips %>% 
  filter(trip_duration < 0) 

View(negative_duration)

# Investigate trips with duration shorter than 1 min
shorter_than_1min <- trips %>% 
  filter(trip_duration <= 1) 

View(shorter_than_1min)

# Investigate longer trips
longer_than_4hour <- trips %>%
  filter(trip_duration >= 1000) 

View(longer_than_4hour)

# Remove trips with negative or shorter than 1 min duration
trips <- trips %>% 
  filter(trip_duration >= 1)

# Add hour, weekday and month to trips
trips <- trips %>% 
  mutate(
    # Extract the hour and day of the week from the datetime 
    hour = hour(started_at),
    weekday = wday(started_at, label = TRUE),
    month = month(started_at, label = TRUE),
  )

glimpse(trips)

# Check if `member_causal`is suitable as factor
unique(trips$member_casual)

# Check if `rideable_type`is suitable as factor
unique(trips$rideable_type)

rideable_type_counts <- table(trips$rideable_type)
print(rideable_type_counts)

# Make `member_causal` and `rideable_type` factor 
trips$member_casual <- as.factor(trips$member_casual)
trips$rideable_type <- as.factor(trips$rideable_type)

class(trips$member_casual)
class(trips$member_casual)

# Investigate `docked_bike` trips.  
docked_bike <- trips %>% 
  filter(rideable_type == "docked_bike") 

View(docked_bike)

# Observed abnormalities associated with `docked_bike`: there was a significant
# number of extraordinarily short and long `trip_duration`, the longest being
# 41387.25 mins, and 100% of the users were casual. After further research, it was
# concluded that `docked_bikes` are bikes that were taken out of circulation.

# Remove trips with docked_bike
trips <- trips %>% 
  filter(rideable_type != "docked_bike")

skim(trips)

# Investigate NA in end coordinates cases
missing_end_coord <- trips %>% 
  filter(is.na(end_lat) & is.na(end_lng))

View(missing_end_coord)

# Remove trips with NA in end coordinates
trips <- trips %>% 
  filter(!is.na(end_lat) & !is.na(end_lng))

# Several abnormalities were observed in `missing_end_coord`:
# 1. Only classic bikes were present.
# 2. The majority of the users were casual.
# 3. Almost all of the bikes (more than 99%) were used for 1499 minutes or longer.
# Based on these findings, we concluded that the records in `missing_end_coord` represent
# trips where bikes were taken out of circulation. Given that there were only 3,118
# such rows,they were not statistically significant for our analysis, so they were dropped.

# ------------------------------------------------------------------------------

# Next Step - Items Need Further Exploration:
# 1. Missing Station Names and Ids
# 2. Inconsistent Level of Coordinates Precision 

# ==============================================================================
#                              Further Exploration
# ==============================================================================

# 1. Missing Station Names and Ids:

# After further research, we learned that classic bikes dock exclusively at Divvy
# stations whereas electric bikes can dock at non-Divvy stations or public fixtures.
# This difference might explain the missing station ids and names in our data.
# Below, we will explore NAs in start station id and name of different ride types.  

# Determine many classic ride records are missing both start station id and name
classic_missing_station <- trips %>% 
  filter(
    rideable_type == "classic_bike",
    trip_duration > 2,
    is.na(start_station_id) & is.na(start_station_name) |
      is.na(end_station_id) & is.na(end_station_name)
  )

View(classic_missing_station)
# Only 71 records

# Determine how many electric ride are missing both start station id and name
electric_missing_station <- trips %>% 
  filter(
    rideable_type == "electric_bike",
    trip_duration > 2,
    is.na(start_station_id) & is.na(start_station_name) | 
      is.na(end_station_id) & is.na(end_station_name),
  )

View(electric_missing_station)
# 1,262,053 records

# Our hypothesis is confirmed: the flexible return options for electric bikes 
# are the primary cause of missing station IDs and names.

# Next, we'll assess the completeness of station IDs versus names to determine 
# which variable is best for finding higher accuracy coordinates.

# Identify `start_station_name` records that are missing corresponding ids
start_name_missing_id <- trips %>% 
  filter(!is.na(start_station_name) & is.na(start_station_id)) %>% 
  distinct(start_station_name)

View(start_name_missing_id)
# 2 records

# Identify `end_station_name` records that are missing corresponding ids
end_name_missing_id <- trips %>% 
  filter(!is.na(end_station_name) & is.na(end_station_id)) %>% 
  distinct(end_station_name)

View(end_name_missing_id)
# 2 records

# Identify `start_station_id` records that are missing corresponding name
start_id_missing_name <- trips %>% 
  filter(!is.na(start_station_id) & is.na(start_station_name)) %>% 
  distinct(start_station_id)

View(start_id_missing_name)
# 0 record

# Identify `end_station_id` records that are missing corresponding name
end_id_missing_name <- trips %>% 
  filter(!is.na(end_station_id) & is.na(end_station_name)) %>% 
  distinct(end_station_id)

View(end_id_missing_name)
# 0 record

# Check to see if we can find the ids for the 2 station names

# Find id of Stony Island Ave & 63rd St station
stony_island_63_id <- trips %>% 
  filter(
    start_station_name == "stony island ave & 63rd st",
    !is.na(start_station_id)
  )

View(stony_island_63_id)
# stony island ave & 63rd st station id is 653B

# Find id of Elizabeth St & Randolph St station
elizabeth_randolph_id <- trips %>% 
  filter(
    start_station_name == "elizabeth st & randolph st",
    !is.na(start_station_id)
  )

View(elizabeth_randolph_id)
# elizabeth st & randolph st station id is 23001 

# We've identified the two missing station ids. We'll fill these into our dataset
# to ensure a complete mapping of station names to ids during Further Cleaning.

# ==============================================================================
                               
# 2. Inconsistent Level of Coordinates Precision: 

# We observed varying levels of precision in our coordinate data. To prevent potential
# biases in our analysis, we'll explore these coordinate precision levels, their 
# relationship with other variables, and seek higher precision coordinates for the 
# same stations within our dataset.

# Determine the number of records for each level of coordinate precision.
coords_dec0 <- trips %>%
  filter((round(start_lat, 0) == start_lat & round(start_lng, 0) == start_lng) |
           (round(end_lat, 0) == end_lat & round(end_lng, 0) == end_lng))

coords_dec1 <- trips %>%
  filter((round(start_lat, 1) == start_lat & round(start_lng, 1) == start_lng) |
           (round(end_lat, 1) == end_lat & round(end_lng, 1) == end_lng))

coords_dec2 <- trips %>%
  filter((round(start_lat, 2) == start_lat & round(start_lng, 2) == start_lng) |
           (round(end_lat, 2) == end_lat & round(end_lng, 2) == end_lng))

View(coords_dec0)
# 8 zero decimal place coordinates

View(coords_dec1)
# 21,231 of 1 decimal place coordinates

View(coords_dec2)
# 1,409,224 of 2 decimal places coordinates

# Initial analysis indicates many records have 2-decimal place coordinates.
# Given this accuracy corresponds to ~1km radius, it might not suffice for our objectives.

# It's essential to determine if a single station name can be associated with
# coordinates of varying precision levels.

# Identify records with less precise end coordinates, excluding NA in station names
end_coord_imprecise <- trips %>% 
  filter(
    !is.na(end_station_name),
    round(end_lat, 2) == end_lat & round(end_lng, 2) == end_lng
  )
      
View(end_coord_imprecise)

# Select random `end_station_name` from `end_coord_imprecise` and check 
# for more accurate coordinates under the same `start_station_name`.

#Find precise coordinates for station elizabeth st & randolph st
coords_elizabeth_randolph <- trips %>% 
  filter(start_station_name == "elizabeth st & randolph st") %>% 
  distinct(start_lat, start_lng, .keep_all = TRUE)

View(coords_elizabeth_randolph)

#Find precise coordinates for station mulligan ave & wellington ave
coords_mulligan_wellington <- trips %>% 
  filter(start_station_name == "mulligan ave & wellington ave") %>% 
  distinct(start_lat, start_lng, .keep_all = TRUE)

View(coords_mulligan_wellington)

#Find precise coordinates for station lawndale ave & 30th st
coord_lawndale_ave_30th <- trips %>% 
  filter(start_station_name == "lawndale ave & 30th st") %>% 
  distinct(start_lat, start_lng, .keep_all = TRUE)

View(coord_lawndale_ave_30th)

# Precise coordinates were found for all three station names. This confirms our 
# approach to replace less accurate coordinates during Further Cleaning.

# ------------------------------------------------------------------------------

# Next Step - Items Need Further Cleaning:
# 1. Fill Missing Station Ids
# 2. Improve Coordinate Precision

# ==============================================================================
#                              Further Cleaning
# ==============================================================================

# 1. Fill Missing Station Ids:

# Function to fill station ids
fill_ids <- function(data, station_name, id) {
  #create a subset with missing start station ids
  with_missing_id <- data %>% 
    filter(is.na(start_station_id) | is.na(end_station_id))
  
  #create a subset without missing ids
  without_missing_id <- data %>% 
    filter(!is.na(start_station_id) & !is.na(end_station_id))
  
  with_missing_id <- with_missing_id %>% 
    mutate(
      start_station_id = ifelse(start_station_name == station_name, id, start_station_id),
      end_station_id = ifelse(end_station_name == station_name, id, end_station_id)
    )
  
  combined_df <- bind_rows(with_missing_id, without_missing_id)
}

trips <- fill_ids(trips, "stony island ave & 63rd st", "653B")
trips <- fill_ids(trips, "elizabeth st & randolph st", "23001")

skim(trips)

# ==============================================================================

# 2. Improve Coordinate Precision: 

# Now that every station name has an corresponding id, we can proceed to locate
# higher precision coordinates and replace the existing lower precision ones.  

# Identify unique pairs of station name and coordinates of 5 decimal places

# Create a subset for start stations and remove "start" prefix from selected columns
start_stations <- trips %>% 
  select(start_station_name, start_lat, start_lng) %>% 
  filter(!is.na(start_station_name)) %>% 
  rename(station_name = start_station_name, lat = start_lat, lng = start_lng)

View(start_stations)

# Create a subset for end stations and remove "end" prefix from all selected columns
end_stations <- trips %>% 
  select(end_station_name, end_lat, end_lng) %>% 
  filter(!is.na(end_station_name)) %>% 
  rename(station_name = end_station_name, lat = end_lat, lng = end_lng)

View(end_stations)

# Combine the two
stations_combined <- bind_rows(start_stations, end_stations)

# Keep only high precision coordinates (5 decimal places or higher)
stations_precise <- stations_combined %>% 
  filter(round(lat, 5) != lat & round(lng, 5) != lng)

View(stations_precise)

# Keep only median coordinates value while removing duplicated stations
stations_precise <- stations_precise %>% 
  group_by(station_name) %>% 
  summarise(med_lat = median(lat), med_lng = median(lng))

View(stations_precise)

# Investigate stations lacking precise median coordinates

# Create a subset of all unique stations regardless of coordinate precision 
stations_unique <- distinct(stations_combined, station_name)

# Get the difference in rows count between `stations_precise` and `stations_unique`

# stations_precise count
print(nrow(stations_precise))
# 1375 records

# stations_unique count
print(nrow(stations_unique))
# 1855 records

# 480 stations don't have the level of precision we are looking for. We'll delve
# deeper into the records related to these 480 stations. 

# Identify imprecise station by extracting what's not on `stations_precise`
stations_only_on_unique <- setdiff(
  stations_unique$station_name,
  stations_precise$station_name
)
print(stations_only_on_unique)

# Extract trips associated with stations lacking precise coordinates to further
# inspect these records

# Filter such rows from `trips`
stations_imprecise_coords <- trips %>%
  filter(
    start_station_name %in% stations_only_on_unique | 
     end_station_name %in% stations_only_on_unique
  )

View(stations_imprecise_coords)
# 8350 records shown, all but one are electric bike rides

# The only classic bike ride appears to have been returned to an incorrect station, 
# given its end station name is "oh charging stx - test". This observation strengthens 
# our hypothesis, especially since many station names have the prefix "public rack".

# Next, we'll replace the less precise coordinates with the more accurate ones. 
# Improving location accuracy for trips that started and ended at a Divvy station 
# (comprising about 76% of our data) is pivotal. Our plan is to create a subset 
# based on this criterion, and analyze it separately from the rest. This subset
# will be pivotal for answering queries that necessitate high precision. 

# Function to replace low precision coordinates with high precision ones
improve_coords_precision <- function(data, station_coords) {
  
  # Separate the rows that need corrections and those that don't
  data_needing_correction <- data %>%
    filter(
      start_station_name %in% station_coords$station_name | 
        end_station_name %in% station_coords$station_name
    )
  
  data_without_correction <- anti_join(data, data_needing_correction)
  
  # Apply the corrections using the provided logic
  data_corrected <- data_needing_correction %>%
    left_join(station_coords, by = c("start_station_name" = "station_name")) %>%
    mutate(
      start_lat = coalesce(med_lat, start_lat),
      start_lng = coalesce(med_lng, start_lng)
    ) %>%
    select(-med_lat, -med_lng)  # Remove the intermediate columns
  
  data_corrected <- data_corrected %>%
    left_join(station_coords, by = c("end_station_name" = "station_name")) %>%
    mutate(
      end_lat = coalesce(med_lat, end_lat),
      end_lng = coalesce(med_lng, end_lng)
    ) %>%
    select(-med_lat, -med_lng)  # Remove the intermediate columns
  
  # Merge the corrected rows back with the rows that didn't need corrections
  complete_data <- bind_rows(data_without_correction, data_corrected)
  
  return(complete_data)
}

improved <- improve_coords_precision(trips, stations_precise)

View(improved)

# Identify 2 decimal place coordinates on both sets and get a count

# The filter set before improvement
coords_before_improvement_dec2 <- trips %>% 
  filter(
    (!is.na(start_lat) | !is.na(end_lat)),
    (round(start_lat, 2) == start_lat & round(start_lng, 2) == start_lng) |
      (round(end_lat, 2) == end_lat & round(end_lng, 2) == end_lng)
  )  

coords_improved_dec2 <- improved %>%
  filter((round(start_lat, 2) == start_lat & round(start_lng, 2) == start_lng) |
           (round(end_lat, 2) == end_lat & round(end_lng, 2) == end_lng))

nrow(coords_before_improvement_dec2)
# 1409224 records

nrow(coords_improved_dec2)
# 1315099 records

# Coordinates on 94125 rows were improved in precision as a result

# Re-assign the improved data to `trips_cleaned`
trips_cleaned <- improved

# Finally, calculate displacement distance and append to 'trips_cleaned'

# Create function to calculate distance between coordinates
calculate_haversine_distance <- function(start_lat, start_lng, end_lat, end_lng) {
  # Radius of the Earth in kilometers
  R <- 6371.0
  
  # Convert degrees to radians
  deg2rad <- function(deg) {
    return(deg * pi / 180)
  }
  
  start_lat <- deg2rad(start_lat)
  start_lng <- deg2rad(start_lng)
  end_lat <- deg2rad(end_lat)
  end_lng <- deg2rad(end_lng)
  
  # Differences in coordinates
  d_lat <- end_lat - start_lat
  d_lng <- end_lng - start_lng
  
  # Haversine formula
  a <- sin(d_lat / 2)^2 + cos(start_lat) * cos(end_lat) * sin(d_lng / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  # Distance in kilometers
  distance <- R * c
  
  return(distance)
}

# Create and add displacement distance `disp_distance` column to data
trips_cleaned <- trips_cleaned %>%
  mutate(
    disp_distance = calculate_haversine_distance(start_lat, start_lng, end_lat, end_lng),
    date = as.Date(started_at) 
  )



write.csv(trips_cleaned, "data/processed/trips_cleaned.csv", row.names=FALSE)



