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
                "../output/PriGeog_percentiles.csv",
                "../output/YVR_era5_90percentiles.csv",
                "../output/Kelowna_percentiles.csv")
# Function to read and select necessary columns
read_and_select <- function(file_path) {
  read.csv(file_path) 
}

# Read and combine all datasets
df_station <- map_dfr(file_paths, read_and_select)


# 
# summary(df_station %>% filter(Station == "Kamloops"))
# summary(df_station %>% filter(Station == "Prince_George"))
# summary(df_station %>% filter(Station == "YVR"))

txt_file <- "../data/q90-yvr.csv"
YVR_30y_based <- read.table(txt_file, header = TRUE, sep = ",")  # Adjust the separator if needed
YVR_30y_based <- YVR_30y_based[, -1, drop=FALSE]
# Rename columns to match the desired format
colnames(YVR_30y_based) <- c("Percentile_90", "Month", "Day")

# Add a new column "Station" with a constant value
YVR_30y_based$Station <- "YVR_30y_based"

# Reorder columns to match the desired structure
YVR_30y_based <- YVR_30y_based[, c("Month", "Day", "Percentile_90", "Station")]

#print(colnames(YVR_30y_based))
#print(colnames(df_station))
# Combine df with df_station
df_station <- rbind(YVR_30y_based, df_station)

# Display the first few rows of the combined data
head(df_station)

