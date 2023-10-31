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





