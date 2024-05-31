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
setwd("/Users/alexlin/summer_stat/climate_extrem_RA/R")

# Define file paths
file_paths <- c("../data/Prince_George 1942 Climate data daily.csv",
                "../data/Prince_George Climate data daily (1).csv",
                "../data/Prince_Geo_2009_to_2024.csv",
                "../data/Prince_Geo_1997_to_2009.csv")

# Define the columns needed
needed_columns <- c("x", "y", "LOCAL_DATE", "TOTAL_PRECIPITATION","STATION_NAME",
                    "MAX_TEMPERATURE", "MIN_TEMPERATURE", "TOTAL_RAIN", "MIN_REL_HUMIDITY")

# Function to read and select necessary columns
read_and_select <- function(file_path) {
  read.csv(file_path) %>%
    select(all_of(needed_columns))
}

# Read and combine all data sets
df_pg <- map_dfr(file_paths, read_and_select)

# Display the first few rows of the combined data
head(df_pg)
tail(df_pg)

# Check if STATION_NAME is unique
unique_stations <- df_pg %>%
  distinct(STATION_NAME)
unique_stations

# 1. Summary Statistics
summary_stats <- summary(df_pg)
#summary_stats

# 2. Missing Values Analysis
missing_values <- sapply(df_pg, function(x) sum(is.na(x)))
#missing_values
#tail(df, 2)  # This shows the last 2 rows
