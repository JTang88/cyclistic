# Install and load packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("skimr")
library(skimr)
library(tidyverse)
library(lubridate)


# Get a quick summary for each file as initial exploration 

# List all file names in raw_data folder
file_names <- list.files(
  path = "raw_data",
  pattern = "*.csv",
  full.names = TRUE
)

# Read all files into a list
files <- map(file_names, read_csv)

# Print file name and skim the data
walk2(files, file_names, function(data, name) {
  print(paste("Skim Summary of file:", name))
  print(skim(data))
})

# Observed inconsistencies in 'Column type frequency' suggest that R isn't
# interpreting column types consistently across files. Next, we'll identify
# these inconsistencies by grouping column names with their types and
# summarizing their frequency across all files.

# Display column name, datatypes, and its file name on a table
get_column_types <- function(file) {
  data <- read_csv(file)
  column_types <- map_df(
    data,
    ~ tibble(data_type = class(.x)),
    .id = "column_name"
  )
  mutate(column_types, file = file)
}

# Combine result from each file to one table
column_types_all_files <- map_df(file_names, get_column_types)

print(column_types_all_files)

# Group `column_name` and `data_type` and sum its occurrence in files 
grouped_column_types <- column_types_all_files %>%
  group_by(column_name, data_type) %>%
  summarise(n_files = n(), .groups = "drop")

View(grouped_column_types)

# Each column name and data type group should occur in all 12 files. 
# However, `started_at` and `ended_at` are exceptions, being interpreted as 
# characters in 2 files, rather than their typical POSIXct/POSIXct type.

# Find out in which files `started_at` and `ended_at` are character type  
character_type_files <- column_types_all_files %>%
  filter((column_name == "started_at" & data_type == "character") |
           (column_name == "ended_at" & data_type == "character"))

print(character_type_files)

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
trips <- map_df(file_names, read_and_convert_datetimes)

# change time zone to central time
trips <- trips %>%
  mutate(
    started_at = with_tz(started_at, "America/Chicago"),
    ended_at = with_tz(ended_at, "America/Chicago")
  )

print(trips)






