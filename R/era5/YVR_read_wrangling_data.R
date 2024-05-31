library(dplyr)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)

#Define file paths
file_paths <- c("../data/era5_YVR.csv"
                )
# Function to read and select necessary columns
read_and_select <- function(file_path) {
  read.csv(file_path) 
  #%>%
   # select(all_of(needed_columns))
}

# Read and combine all datasets
era5_yvr <- map_dfr(file_paths, read_and_select)

# Display the first few rows of the combined data
head(era5_yvr)



# Assuming 'eras_yvr' is your dataframe
ordered_era5_yvr <- era5_yvr %>%
  arrange(date)



ordered_era5_yvr <- ordered_era5_yvr %>%
  rename(LOCAL_DATE = date, MAX_TEMPERATURE = max_temp) %>%
  select(-max_temp_hour)
# Display the prepared dataframe
head(ordered_era5_yvr)



# Function to get 15-row window as a list element
get_rolling_window <- function(x, width = 15) {
  n <- length(x)
  windows <- vector("list", n)
  
  for (i in seq_len(n)) {
    start <- max(1, i - (width %/% 2))
    end <- min(n, i + (width %/% 2))
    windows[[i]] <- x[start:end]
  }
  
  return(windows)
}


# Define the function
process_temperature_data <- function(df) {
  
  # Check if necessary columns exist
  required_columns <- c("LOCAL_DATE", "MAX_TEMPERATURE")
  if (!all(required_columns %in% colnames(df))) {
    stop("The dataframe does not contain the necessary columns.")
  }
  
  # Ensure the rolling_window column exists
  df <- df %>%
    mutate(rolling_window = get_rolling_window(MAX_TEMPERATURE))
  
  # Wrangle the dataframe
  df_wrangling <- df %>%
    mutate(
      LOCAL_DATE = as.Date(LOCAL_DATE, format = "%Y-%m-%d"),
      Month = format(LOCAL_DATE, "%m"),
      Day = format(LOCAL_DATE, "%d")
    )
  
  # Group by Month and Day, and concatenate the rolling_window lists
  df_grouped <- df_wrangling %>%
    group_by(Month, Day) %>%
    summarize(
      ROLLING_WINDOW_ALL_YEAR_VALUES = list(reduce(rolling_window, c)),
      .groups = 'drop'
    )
  
  # Calculate the 90th percentile for each day
  df_percentiles <- df_grouped %>%
    mutate(
      Percentile_90 = map_dbl(ROLLING_WINDOW_ALL_YEAR_VALUES, ~ quantile(.x, 0.90, na.rm = TRUE))
    )
  
  return(df_percentiles)
}



era5_yvr_90percentiles <- process_temperature_data(ordered_era5_yvr)



head(era5_yvr_90percentiles)
