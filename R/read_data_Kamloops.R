library(dplyr)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)
# Read the data
#path <- "/Users/alexlin/summer_stat/prof_data/YVR climate daily 2013 to 2024.csv"


# Check the current working directory
getwd()

# If necessary, set the working directory to the 'r' folder
setwd("/Users/alexlin/summer_stat/climate_extreme_RA/R")

# Define file paths
file_paths <- c("../data/KAMLOOPS Daily Climate Data copy.csv",
                "../data/KAMLOOPS Daily Climate Data (1) copy.csv",
                "../data/KAMLOOPS Daily climate data (2) copy.csv",
                "../data/KAMLOOPS 2024 Daily Climate Data.csv")

# Define the columns needed
needed_columns <- c("x", "y", "LOCAL_DATE", "TOTAL_PRECIPITATION", "STATION_NAME",
                    "MAX_TEMPERATURE", "MIN_TEMPERATURE", "TOTAL_RAIN", "MIN_REL_HUMIDITY")

# Function to read and select necessary columns
read_and_select <- function(file_path) {
  read.csv(file_path) %>%
    select(all_of(needed_columns))
}

# Read and combine all data sets
df <- map_dfr(file_paths, read_and_select)

# Display the first few rows of the combined data
head(df)
tail(df)


unique_stations <- df %>%
  distinct(STATION_NAME)
print(unique_stations)



# 1. Summary Statistics
summary_stats <- summary(df)
#summary_stats

# 2. Missing Values Analysis
missing_values <- sapply(df, function(x) sum(is.na(x)))
#missing_values
#tail(df, 2)  # This shows the last 2 rows

