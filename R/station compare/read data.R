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
file_paths <- c("../output/Kamloops_percentiles.csv",
                "../output/YVR_percentiles.csv",
                "../output/PriGeog_percentiles.csv")
# Function to read and select necessary columns
read_and_select <- function(file_path) {
  read.csv(file_path) 
}

# Read and combine all datasets
df_station <- map_dfr(file_paths, read_and_select)

# Display the first few rows of the combined data
head(df_station)

summary(df_station %>% filter(Station == "Kamloops"))
summary(df_station %>% filter(Station == "Prince_George"))
summary(df_station %>% filter(Station == "YVR"))

