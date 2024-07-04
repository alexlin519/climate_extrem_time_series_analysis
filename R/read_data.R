library(dplyr)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)

# Define the function to read, combine, and save datasets
process_and_save_data <- function(file_paths, station_name) {
  setwd("/Users/alexlin/summer_stat/climate_extreme_RA/R")
  
  # Define the columns needed
  needed_columns <- c("x", "y", "LOCAL_DATE", "TOTAL_PRECIPITATION", "STATION_NAME", 
                      "MAX_TEMPERATURE", "MIN_TEMPERATURE", "TOTAL_RAIN", "MIN_REL_HUMIDITY",
                      "LOCAL_YEAR", "LOCAL_MONTH", "MEAN_TEMPERATURE")
  
  # Function to read and select necessary columns
  read_and_select <- function(file_path) {
    read.csv(file_path) %>%
      select(all_of(needed_columns))
  }
  
  # Read and combine all datasets
  df <- map_dfr(file_paths, read_and_select)
  summary_original <- summary(df)
  
  
  # deal with the non-exist date, call helper function
  df <- deal_with_non_exist_date(df)
  
  
  
  # Define the save path
  save_path <- paste0("../output/", station_name, "_raw_filtered_columns.csv")
  
  # Save the selected columns as a CSV file
  write.csv(df, file = save_path, row.names = FALSE)
  print(paste("Data ",station_name," saved to:", save_path))
  #return the df and all summary
  return(df)
}


deal_with_non_exist_date <- function(df){
  # deal with the non-exist date
  df$LOCAL_DATE <- as.Date(df$LOCAL_DATE)
  df <- df %>%
    group_by(LOCAL_DATE) %>%
    filter(n() == 24) %>%
    ungroup()
  return(df)
}
