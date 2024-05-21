library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)

process_data_for_hw <- function(data) {
  # Ensure Month and Day columns are ordered correctly
  data <- data %>%
    mutate(Month = as.integer(Month),
           Day = as.integer(Day))
  
  # Step 1: Add a logical column indicating if Percentile_90 is smaller than MAX_TEMP_YEAR and select necessary columns
  filtered_data <- data %>%
    mutate(Condition = Percentile_90 < MAX_TEMP_YEAR) %>%
    select(Month, Day, Percentile_90, MAX_TEMP_YEAR, DayOfYear, Condition)
  rm(data)
  
  # Step 2: Filter the data where Condition is TRUE
  cumulative_data <- filtered_data %>%
    filter(Condition == TRUE) %>%
    arrange(Month, Day)
  
  # Step 3: Add a cumulative count column that resets on discontinuity
  cumulative_data <- cumulative_data %>%
    mutate(Date = as.Date(paste("2000", Month, Day, sep = "-"), format = "%Y-%m-%d")) %>%
    arrange(Date) %>%
    mutate(Gap = c(1, diff(Date) != 1), # Identify gaps
           Group = cumsum(Gap), # Group by gaps
           Cumulative_Count = ave(Gap, Group, FUN = seq_along)) %>%
    select(-Date, -Gap, -Group) # Remove helper columns
  
  # Return the updated dataframe
  return(cumulative_data)
}

### day summary FUNCTION for data ready to plot

process_data_by_year <- function(df_v1, year_given) {
  #print(df_v1)
  #df_v1 <- df_v1
  # Step 3: Add a cumulative count column that resets on discontinuity
  df_v1 <- df_v1 %>%
    mutate(Date = as.Date(paste(year_given, Month, Day, sep = "-"), format = "%Y-%m-%d")) %>%
    arrange(Date) %>%
    mutate(Gap = c(1, diff(Date) != 1), # Identify gaps
           Group = cumsum(Gap), # Group by gaps
           Cumulative_Count = ave(Gap, Group, FUN = seq_along))
  #print(df_v1)
  # Summarize the lengths of each case
  case_lengths <- df_v1 %>%
    group_by(Group) %>%
    summarize(Case_Length = n()) %>%
    ungroup()
  
  # Count the number of cases and their lengths
  case_summary <- case_lengths %>%
    group_by(Case_Length) %>%
    summarize(Count = n()) %>%
    ungroup()
  
  # Summarize the lengths of each case by month
  case_lengths_by_month <- df_v1 %>%
    group_by(Group, Month) %>%
    summarize(Case_Length = n()) %>%
    ungroup()
  
  # Count the number of cases and their lengths for each month
  case_summary_by_month <- case_lengths_by_month %>%
    group_by(Month, Case_Length) %>%
    summarize(Count = n()) %>%
    ungroup()
  
  # Return the summaries
  list(case_summary = case_summary, case_summary_by_month = case_summary_by_month)
}

plot_case_summary <- function(summaries, year_single) {
  case_summary_by_month <- summaries$case_summary_by_month
  
  month_plot <- ggplot(case_summary_by_month, aes(x = Case_Length, y = Count, fill = as.factor(Month))) +
    geom_bar(stat = "identity") +
    labs(title = paste("Distribution of Case Lengths by Month for year", year_single),
         x = "HeatWave Last Days",
         y = "Total Count of HeatWave",
         fill = "Month") +
    theme_minimal() +
    scale_fill_brewer(palette = "Paired")
  
  hw_length_plot <- ggplot(case_summary_by_month, aes(x = as.factor(Month), y = Count, fill = as.factor(Case_Length))) +
    geom_bar(stat = "identity") +
    labs(title = paste("Case Counts by Month Colored by Case Length for year", year_single),
         x = "Month",
         y = "Total Count of HeatWave",
         fill = "HeatWave Last Days") +
    theme_minimal() +
    scale_fill_brewer(palette = "Paired")
  
  return(list(month_plot = month_plot, hw_length_plot = hw_length_plot))
}