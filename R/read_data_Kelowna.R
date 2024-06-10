library(dplyr)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)


# Check the current working directory
getwd()

# If necessary, set the working directory to the 'r' folder
#setwd("/Users/alexlin/summer_stat/climate_extrem_RA/R")



# Specify the path to the ZIP file
zipfile <- "../data/kelow.zip"

# Create a temporary directory to extract files
temp_dir <- tempdir()

# Unzip the files into the temporary directory
unzip(zipfile, exdir = temp_dir)

# List all CSV files in the extracted directory
csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store data frames
data_list <- list()


# Define the columns needed
needed_columns <- c("x", "y", "LOCAL_DATE", "TOTAL_PRECIPITATION","STATION_NAME",
                    "MAX_TEMPERATURE", "MIN_TEMPERATURE", "TOTAL_RAIN", "MIN_REL_HUMIDITY")




# Loop through the CSV files and read them into data frames
for (file in csv_files) {
  data <- read.csv(file)%>%
    select(all_of(needed_columns))
  data_list[[file]] <- data
  print(data)
}

# Combine all data frames into one, if needed
combined_data <- bind_rows(data_list, .id = "source")

# Clean up temporary directory
unlink(temp_dir, recursive = TRUE)

# Display the combined data
#print(combined_data)

# Drop the 'source' column
combined_data <- combined_data %>% select(-source)

# Change all station names to 'KELOWNA'
combined_data$STATION_NAME <- "KELOWNA"




# 2. Missing Values Analysis
missing_values <- sapply(combined_data, function(x) sum(is.na(x)))
missing_values
#tail(df, 2)  # This shows the last 2 rows

