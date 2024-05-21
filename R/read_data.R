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

path <- "../data/YVR climate daily 2013 to 2024 copy.csv"
df_raw <- read.csv(path)

# Select specific columns
df <- select(df_raw, x, y, LOCAL_DATE, TOTAL_PRECIPITATION, 
             MAX_TEMPERATURE, MIN_TEMPERATURE, TOTAL_RAIN, MIN_REL_HUMIDITY)

# 1. Summary Statistics
summary_stats <- summary(df)
#summary_stats

# 2. Missing Values Analysis
missing_values <- sapply(df, function(x) sum(is.na(x)))
#missing_values
#tail(df, 2)  # This shows the last 2 rows
