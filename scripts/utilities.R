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


