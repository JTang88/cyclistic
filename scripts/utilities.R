# The import script after initial EDA & cleaning 
import_trips_factorize_variables <- function(file_path) {
  trips <- read_csv(file_path)
  trips$weekday <- factor(trips$weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), ordered = TRUE)
  trips$month <- factor(trips$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), ordered = TRUE)
  trips$member_casual <- as.factor(trips$member_casual)
  trips$rideable_type <- as.factor(trips$rideable_type)
  
  return(trips)
}

# A wrapped filter function that report outcome
filter_and_report <- function(.data, ...) {
  initial_rows <- nrow(.data)
  result <- filter(.data, ...)
  records_retained <- nrow(result)
  records_removed <- initial_rows - records_retained
  
  percentage_retained <- (records_retained / initial_rows) * 100
  percentage_removed <- (records_removed / initial_rows) * 100
  
  print(glue("{records_retained} records retained ({round(percentage_retained, 5)}%), {records_removed} records removed ({round(percentage_removed, 5)}%)"))
  
  return(result)
}

# function to find missing value within the data in a pairing scenario 
impute_from_pair <- function(data, source_col, impute_col) {
  # Create a reference set with non-missing impute_col values
  reference <- data %>%
    filter(!is.na(.data[[impute_col]])) %>%
    distinct(.data[[source_col]], .keep_all = TRUE)
  
  # Perform imputation by matching on source column
  data_imputed <- data %>%
    left_join(reference, by = source_col, suffix = c("", "_ref")) %>%
    mutate(!!sym(impute_col) := coalesce(.data[[impute_col]], .data[[paste0(impute_col, "_ref")]])) %>%
    select(-ends_with("_ref")) # Removes the additional columns with "_ref" suffix
  
  # Return the dataset with imputed values
  return(data_imputed)
}

# Computes and imputes maximum coordinate precision for start and end points in a dataset up to a specified limit.
add_and_impute_coord_precisions <- function(data, precision_check_limit) {
  # Inner function to calculate precision for a single coordinate vector
  get_precision <- function(coord_vector) {
    sapply(coord_vector, function(coord) {
      precision <- 1
      for (i in (precision_check_limit - 1):1) {
        if (round(coord, digits = i) != coord) {
          precision <- i + 1
          break
        }
      }
      return(precision)
    })
  }
  
  # Function to calculate the maximum precision for latitude and longitude pairs
  get_max_coord_precision <- function(lat_vector, lng_vector) {
    lat_precision <- get_precision(lat_vector)
    lng_precision <- get_precision(lng_vector)
    pmax(lat_precision, lng_precision)
  }
  
  # Mutate the data frame to add precision columns
  data %>% 
    mutate(
      start_coord_precision = get_max_coord_precision(start_lat, start_lng),
      end_coord_precision = get_max_coord_precision(end_lat, end_lng)
    )
}


count_lower_precision_coord <- function(data, precision_threshold) {
  # Counts the rows where either start or end coordinate precision is below the threshold.
  count <- sum(data$start_coord_precision < precision_threshold, 
               data$end_coord_precision < precision_threshold, na.rm = TRUE)
  return(count)
}


# combine stations function
combine_all_stations_instances <- function(data) {
  # Create a subset for start stations and remove "start" prefix from selected columns
  start_stations <- data %>% 
    select(start_station_name, start_lat, start_lng, start_coord_precision) %>% 
    filter(!is.na(start_station_name)) %>% 
    rename(
      station_name = start_station_name,
      lat = start_lat,
      lng = start_lng,
      coord_precision = start_coord_precision
    )
  
  # Create a subset for end stations and remove "end" prefix from all selected columns
  end_stations <- data %>% 
    select(end_station_name, end_lat, end_lng, end_coord_precision) %>% 
    filter(!is.na(end_station_name)) %>% 
    rename(
      station_name = end_station_name,
      lat = end_lat,
      lng = end_lng,
      coord_precision = end_coord_precision
    )
  
  # Combine the two
  return(bind_rows(start_stations, end_stations))
}


# Function to add randomness to the 5th decimal place
add_randomness <- function(coordinate) {
  # Extract the integer part and the first four decimal places
  truncated_coordinate <- trunc(coordinate * 1e4) / 1e4
  
  # Generate a random number starting from the fifth decimal place
  random_fraction <- runif(1, min = 0, max = 1e-4)
  
  # Add the random fraction back to the truncated coordinate
  random_coordinate <- truncated_coordinate + random_fraction
  
  return(random_coordinate)
}

# impute stations coordinates from station_coord_reference to data they arre below precision_threshold
impute_coordinates <- function(data, station_coord_reference, precision_threshold) {
  data <- data %>%
    left_join(station_coord_reference, by = c("start_station_name" = "station_name")) %>%
    left_join(station_coord_reference, by = c("end_station_name" = "station_name"), suffix = c("_start", "_end"))
  
  # Step 2: Impute coordinates with less than precision_threshold
  data <- data %>%
    mutate(
      start_lat = ifelse(
        start_coord_precision < precision_threshold & !is.na(med_lat_start), 
        add_randomness(med_lat_start),
        start_lat
      ),
      start_lng = ifelse(
        start_coord_precision < precision_threshold & !is.na(med_lng_start),
        add_randomness(med_lng_start),
        start_lng
      ),
      end_lat = ifelse(
        end_coord_precision < precision_threshold & !is.na(med_lat_end),
        add_randomness(med_lat_end),
        end_lat
      ),
      end_lng = ifelse(
        end_coord_precision < precision_threshold & !is.na(med_lng_end),
        add_randomness(med_lng_end),
        end_lng
      )
    )
  
  # Remove the extra columns that were created during the join if they are not needed
  data <- data %>%
    select(-matches("_start$|_end$"))
  
  return (data)
}


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



