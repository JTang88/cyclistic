# Write all utility functions here

# The import script after initial data cleaning
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


# Vectorized function to determine the maximum number of decimal places between two numbers
get_max_precision_decimal <- function(lat_vector, lng_vector) {
  # Vectorized function to determine the number of decimal places
  get_precision <- function(coord_vector) {
    sapply(coord_vector, function(coord) {
      precision <- 1
      for (i in 4:1) {
        if (round(coord, digits = i) != coord) {
          precision <- i + 1
          break
        }
      }
      return(precision)
    })
  }
  
  # Get precision for both latitude and longitude
  lat_precision <- get_precision(lat_vector)
  lng_precision <- get_precision(lng_vector)
  
  # Return the larger of the two precisions for each pair of coordinates
  pmax(lat_precision, lng_precision)
}


# Step 2: Function to add randomness to the 5th decimal place
add_randomness <- function(coordinate) {
  # Extract the integer part and the first four decimal places
  truncated_coordinate <- trunc(coordinate * 1e4) / 1e4
  
  # Generate a random number starting from the fifth decimal place
  random_fraction <- runif(1, min = 0, max = 1e-4)
  
  # Add the random fraction back to the truncated coordinate
  random_coordinate <- truncated_coordinate + random_fraction
  
  return(random_coordinate)
}



