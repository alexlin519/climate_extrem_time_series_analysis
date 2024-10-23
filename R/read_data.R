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
  print("Deal with non-exist date")
  
  #make sure min(df$LOCAL_DATE) or max(df$LOCAL_DATE) is not NA
  #filter out the NA
  df_part_date <- df %>%
    filter(!is.na(LOCAL_DATE))
  result <- deal_with_non_exist_date(df_part_date)
  print("Deal with non-exist date done")
  #df_complete_final, df_missing, missing_summary
  
  # Define the save path
  save_path <- paste0("../output/", station_name, "_raw_filtered_columns.csv")
  
  # Save the selected columns as a CSV file
  # Save the selected columns as a CSV file if the result is not NA
  
  if (!is.null(result)) {
    write.csv(result[[1]], file = save_path, row.names = FALSE)
    print(paste("Data ", station_name, " saved to:", save_path))
    return(list(result[[1]], summary_original, result[[2]], result[[3]]))
  } else {
    write.csv(df, file = save_path, row.names = FALSE)
    print(paste("Data ", station_name, " saved to:", save_path))
    return(list(df, summary_original,NA,NA))
  }
  
  
}


deal_with_non_exist_date <- function(df_date){
  
  # Ensure the date column is in Date format
  df_date$LOCAL_DATE <- as.Date(df_date$LOCAL_DATE, format="%Y-%m-%d")
  # Remove NA values if any
  df_date <- df_date[!is.na(df_date$LOCAL_DATE), ]
  # Create a complete sequence of dates
  complete_dates <- data.frame(LOCAL_DATE = seq(min(df_date$LOCAL_DATE), max(df_date$LOCAL_DATE), by="day"))

  # Check if the dataframe is already complete
  if (nrow(df_date) == nrow(complete_dates)) {
    # If complete, return NA and print a message
    print("The dataframe is already complete. No missing dates.")
    return(NA)
  }
  # Merge the complete dates with the original dataset
  df_complete <- complete_dates %>%
    left_join(df_date, by = "LOCAL_DATE")
  
  # Identify and fill in missing values with NA
  df_complete <- df_complete %>%
    mutate(TOTAL_PRECIPITATION = ifelse(is.na(TOTAL_PRECIPITATION), NA, TOTAL_PRECIPITATION),
           STATION_NAME = ifelse(is.na(STATION_NAME), "missing", STATION_NAME),
           MAX_TEMPERATURE = ifelse(is.na(MAX_TEMPERATURE), NA, MAX_TEMPERATURE),
           MIN_TEMPERATURE = ifelse(is.na(MIN_TEMPERATURE), NA, MIN_TEMPERATURE))
  
  # Impute temperature by averaging the values from the surrounding dates
  # df_date_complete <- df_date_complete %>%
  #   mutate(MAX_TEMPERATURE = ifelse(is.na(MAX_TEMPERATURE),
  #                                   (lag(MAX_TEMPERATURE, 1) + lead(MAX_TEMPERATURE, 1)) / 2,
  #                                   MAX_TEMPERATURE),
  #          MIN_TEMPERATURE = ifelse(is.na(MIN_TEMPERATURE),
  #                                   (lag(MIN_TEMPERATURE, 1) + lead(MIN_TEMPERATURE, 1)) / 2,
  #                                   MIN_TEMPERATURE))
  
  # Show the completed dataframe
  #print(df_date_complete)
  
  
  # Summarize the added missing rows

  df_missing <- df_complete %>%
    filter(STATION_NAME == "missing")
  #sum up the count by year using LOCAL_DATE
  missing_summary <- df_missing %>%
    mutate(LOCAL_YEAR = year(LOCAL_DATE)) %>%
    group_by(LOCAL_YEAR) %>%
    summarise(count = n())
  
  
  
  df_complete_final <- df_complete %>%
    mutate(STATION_NAME = station_name)
  
  
  # Calculate the number of missing dates in the original dataframe
  missing_dates_count <- nrow(df_complete_final) - nrow(df)
  print("Number of missing dates in the original dataframe:")
  print(missing_dates_count)
  print(nrow(df_missing))
  
  
  # Return all 2 df and summary
  return(list(df_complete_final, df_missing, missing_summary))
}


update_precipitation <- function(main_df, extra_df_path) {
  # Read the extra dataframe
  extra_df <- read.csv(extra_df_path)
  
  # Select necessary columns
  extra_df <- extra_df %>% select(LOCAL_DATE, TOTAL_PRECIPITATION)
  
  # Count NA in the main dataframe before update
  initial_na_count <- sum(is.na(main_df$TOTAL_PRECIPITATION))
  cat("Initial NA count in main dataframe:", initial_na_count, "\n")
  
  # Count NA in the extra dataframe
  extra_na_count <- sum(is.na(extra_df$TOTAL_PRECIPITATION))
  cat("NA count in extra dataframe:", extra_na_count, "\n")
  
  # Convert LOCAL_DATE to Date type if it's not already
  extra_df$LOCAL_DATE <- as.Date(extra_df$LOCAL_DATE)
  main_df$LOCAL_DATE <- as.Date(main_df$LOCAL_DATE)
  
  # Update main_df with values from extra_df where main_df has NA in TOTAL_PRECIPITATION
  main_df <- main_df %>%
    left_join(extra_df, by = "LOCAL_DATE", suffix = c("", ".extra")) %>%
    mutate(TOTAL_PRECIPITATION = ifelse(is.na(TOTAL_PRECIPITATION), TOTAL_PRECIPITATION.extra, TOTAL_PRECIPITATION)) %>%
    select(-TOTAL_PRECIPITATION.extra)
  
  # Count NA in the main dataframe after update
  final_na_count <- sum(is.na(main_df$TOTAL_PRECIPITATION))
  cat("Final NA count in main dataframe:", final_na_count, "\n")
  
  return(main_df)
}

