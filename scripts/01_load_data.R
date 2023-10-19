# List all file names in raw_data folder
file_names <- list.files(
  path = "data/raw",
  pattern = "*.csv",
  full.names = TRUE
)

# Attempted `trips_combined <- map_df(file_names, read_csv)` result in error msg:

# "Error in `dplyr::bind_rows()`:
# ! Can't combine `..9$started_at` <datetime<UTC>> and `..10$started_at` <character>."  

# `started_at` and `ended_at` were interpreted as characters instead of datetime
# for some files. The following code helps identify problematic files by spotting
# anomaly in variable type counts in each file. 

# # ============================================================================

# # Read all files into a list
# files <- map(file_names, read_csv)
# 
# # Print file name and skim the data
# walk2(files, file_names, function(data, name) {
#   print(paste("=============================================== Skim Summary of file:", name, "==============================================="))
#   print(skim(data))
# })

# Use "...=== Skim Summary of file ===..." to identify breaks between flies in 

# ==============================================================================

# Convert `started_at` and `ended_at` to POSIXct, if they are characters
read_and_convert_datetimes <- function(file) {
  data <- read_csv(file)
  
  if(is.character(data$started_at)) {
    data <- mutate(data, started_at = mdy_hm(started_at))
  }
  
  if(is.character(data$ended_at)) {
    data <- mutate(data, ended_at = mdy_hm(ended_at))
  }
  
  data
}

# Combine all files, convert `started_at` and `ended_at` datatype on-the-fly
trips_combined <- map_df(file_names, read_and_convert_datetimes)

write.csv(trips_combined, "data/interim/trips_combined.csv", row.names=FALSE)








