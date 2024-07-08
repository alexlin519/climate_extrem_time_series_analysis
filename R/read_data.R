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
  
  # Ensure that data_EHF contains finite values in all columns
  # df <- data_EHF %>%
  #   filter_all(all_vars(is.finite(.)))
  
  summary_original <- summary(df)
  
  # deal with the non-exist date, call helper function
  result <- deal_with_non_exist_date(df)
  #df_complete_final, df_missing, missing_summary
  
  # Define the save path
  save_path <- paste0("../output/", station_name, "_raw_filtered_columns.csv")
  
  # Save the selected columns as a CSV file
  write.csv(result[[1]], file = save_path, row.names = FALSE)
  print(paste("Data ",station_name," saved to:", save_path))
  
  #return the df and all summary
  return(list(result[[1]], summary_original, result[[2]], result[[3]]))
}


deal_with_non_exist_date <- function(df){
  # Ensure the date column is in Date format
  df$LOCAL_DATE <- as.Date(df$LOCAL_DATE, format="%Y-%m-%d")
  # Create a complete sequence of dates
  complete_dates <- data.frame(LOCAL_DATE = seq(min(df$LOCAL_DATE), max(df$LOCAL_DATE), by="day"))

  # Merge the complete dates with the original dataset
  df_complete <- complete_dates %>%
    left_join(df, by = "LOCAL_DATE")
  
  # Identify and fill in missing values with NA
  df_complete <- df_complete %>%
    mutate(TOTAL_PRECIPITATION = ifelse(is.na(TOTAL_PRECIPITATION), NA, TOTAL_PRECIPITATION),
           STATION_NAME = ifelse(is.na(STATION_NAME), "missing", STATION_NAME),
           MAX_TEMPERATURE = ifelse(is.na(MAX_TEMPERATURE), NA, MAX_TEMPERATURE),
           MIN_TEMPERATURE = ifelse(is.na(MIN_TEMPERATURE), NA, MIN_TEMPERATURE))
  
  # Impute temperature by averaging the values from the surrounding dates
  df_complete <- df_complete %>%
    mutate(MAX_TEMPERATURE = ifelse(is.na(MAX_TEMPERATURE),
                                    (lag(MAX_TEMPERATURE, 1) + lead(MAX_TEMPERATURE, 1)) / 2,
                                    MAX_TEMPERATURE),
           MIN_TEMPERATURE = ifelse(is.na(MIN_TEMPERATURE),
                                    (lag(MIN_TEMPERATURE, 1) + lead(MIN_TEMPERATURE, 1)) / 2,
                                    MIN_TEMPERATURE))
  
  # Show the completed dataframe
  print(df_complete)
  
  # Summarize the added missing rows
  missing_summary <- df_complete %>%
    filter(STATION_NAME == "missing") %>%
    summarise(
      count_missing = n(),
      first_missing_date = min(LOCAL_DATE),
      last_missing_date = max(LOCAL_DATE)
    )
  df_missing <- df_complete %>%
    filter(STATION_NAME == "missing")

  df_complete_final <- df_complete %>%
    mutate(STATION_NAME = station_name)
  # Return all 2 df and summary
  return(list(df_complete_final, df_missing, missing_summary))
}



