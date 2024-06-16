library(dplyr)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)

# Check the current working directory
getwd()

# If necessary, set the working directory to the 'r' folder
setwd("/Users/alexlin/summer_stat/climate_extreme_RA/R")

# Define file paths
file_paths <- c("../data/FortNelson/Daily climate data.csv",
                "../data/FortNelson/Daily climate data (1).csv",
                "../data/FortNelson/Daily climate data (2).csv",
                "../data/FortNelson/Daily climate data (3).csv")

# Define the columns needed
needed_columns <- c("x", "y", "LOCAL_DATE", "TOTAL_PRECIPITATION","STATION_NAME",
                    "MAX_TEMPERATURE", "MIN_TEMPERATURE", "TOTAL_RAIN", "MIN_REL_HUMIDITY")

# Function to read and select necessary columns
read_and_select <- function(file_path) {
  read.csv(file_path) %>%
    select(all_of(needed_columns))
}

# Read and combine all data sets
df_fn <- map_dfr(file_paths, read_and_select)

# Display the first few rows of the combined data
head(df_fn)
tail(df_fn)

# Check if STATION_NAME is unique
unique_stations <- df_fn %>%
  distinct(STATION_NAME)
print(unique_stations)

# 1. Summary Statistics
summary_stats <- summary(df_fn)
#summary_stats

# 2. Missing Values Analysis
missing_values <- sapply(df_fn, function(x) sum(is.na(x)))
missing_values
#tail(df, 2)  # This shows the last 2 rows

