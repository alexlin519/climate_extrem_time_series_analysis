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
file_paths <- c( "../data/Abbotsford/Daily climate data.csv",
                 "../data/Abbotsford/Daily_Climate _Data_0.csv",
                "../data/Abbotsford/Daily climate data (1).csv",
                "../data/Abbotsford/Daily climate data (2).csv",
                "../data/Abbotsford/Daily climate data (3).csv")


# Define the columns needed
needed_columns <- c("x", "y", "LOCAL_DATE", "TOTAL_PRECIPITATION","STATION_NAME", 
                    "MAX_TEMPERATURE", "MIN_TEMPERATURE", "TOTAL_RAIN", "MIN_REL_HUMIDITY"
                    ,"LOCAL_YEAR", "LOCAL_MONTH", "MEAN_TEMPERATURE")

# Function to read and select necessary columns
read_and_select <- function(file_path) {
  read.csv(file_path) %>%
    select(all_of(needed_columns))
}

# Read and combine all data sets
df_ab <- map_dfr(file_paths, read_and_select)

# Display the first few rows of the combined data
head(df_ab)
tail(df_ab)

# Check if STATION_NAME is unique
unique_stations <- df_ab %>%
  distinct(STATION_NAME)
print(unique_stations)

# 1. Summary Statistics
summary_stats <- summary(df_ab)
summary_stats

# 2. Missing Values Analysis
missing_values <- sapply(df_ab, function(x) sum(is.na(x)))
#missing_values
#tail(df, 2)  # This shows the last 2 rows


station_name<- "Abbotsford"
save_path <- paste0("../output/", station_name, "_raw_filtered_columns.csv")
## Save the selected columns as a CSV file
write.csv(df_ab, file = save_path, row.names = FALSE)

