library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)

## functions for prepareing data for plotting 
#setwd("/Users/alexlin/summer_stat/climate_extrem_RA/reports_station")
# Use the provided dataframe or default to 'df'
  
# Define the function to prepare the data
#return df_for_plot_year
prepare_plot_data <- function(baseline_col, year, temp_col = "MAX_TEMPERATURE", 
                              inputdf_all_day = NULL, input_90percentiles= NULL){
  # Ensure the directory exists
  if (!is.null(inputdf_all_day)) {
    df <- inputdf_all_day
      #print('using input df')
  }
  if (!is.null(input_90percentiles)) {
    df_percentiles <- input_90percentiles
    #print('using input df')
  }
    
  # Add Month and Day columns
  df_for_plot <- df %>%
    mutate(LOCAL_DATE = as.Date(LOCAL_DATE, format = "%Y-%m-%d"),
           Month = format(LOCAL_DATE, "%m"),
           Day = format(LOCAL_DATE, "%d")
    )
  
  # Filter data for the target year
  df_year <- df_for_plot %>%
    filter(format(LOCAL_DATE, "%Y") == as.character(year)) %>%
    group_by(Month, Day) %>%
    summarize(
      MAX_TEMP_YEAR = max(get(temp_col)),
      .groups = 'drop'  # Ensures no unintended grouping in the output
    ) %>%
    ungroup()
  df_baseline <- df_percentiles
  # Merge the 90th percentile data with the target year data
  df_plot_year <- df_baseline %>%
    left_join(df_year, by = c("Month", "Day"))
  
  # Combine Month and Day for plotting
  df_for_plot_year <- df_plot_year %>%
    mutate(
      DayOfYear = as.numeric(format(as.Date(paste0(year, "-", Month, "-", Day), format = "%Y-%m-%d"), "%j"))
      #DayOfYear = paste(Month, Day, sep = "-"
    )
  
  return(df_for_plot_year)
}

#df_plot_year <- prepare_plot_data("Percentile_90", 2021, "MAX_TEMPERATURE")
#df_plot_year

## functions for plotting


# Define the function to plot the data
plot_percentiles_vs_year <- function(df_plot_year,year) {
  
  ggplot(df_plot_year, aes(x = DayOfYear)) +
    geom_line(aes(y = Percentile_90, color = "90th Percentile")) +
    geom_line(aes(y = MAX_TEMP_YEAR, color = "Daily Max Temp")) +
    labs(
      title = paste("90th Percentile of Max Temperature vs.", year, "Max Temperature"),
      x = "Day of Year",
      y = "Temperature (°C)",
      color = "Legend"
    ) +
    scale_color_manual(values = c("90th Percentile" = "blue", "Daily Max Temp" = "red")) +
    theme_minimal(base_size = 11) +
    theme(plot.margin = margin(20, 20, 20, 20)) +
    theme(aspect.ratio = 1/2)
}

plot_ehf95 <- function(df_combined_ehf1,year) {
  # filter the data for the target year
  df_combined_ehf1_filter_year <- df_combined_ehf1 %>%
    filter(LOCAL_YEAR == year)
  # Plot the EHF
  ggplot(df_combined_ehf1_filter_year, aes(x = LOCAL_DATE, y = EHF_95)) +
    geom_line() +
    labs(
      title = paste("Excessive Heat Factor (EHF) Over for", year),
      x = "Date",
      y = "EHF"
    ) +
    theme_minimal()+
    scale_x_date(date_labels = "%b %d", date_breaks = "5 day")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12)) 
}


plot_ecf <- function(df_combined_ehf1,year) {
  # filter the data for the target year
  df_combined_ehf1_filter_year <- df_combined_ehf1 %>%
    filter(LOCAL_YEAR == year)
  # Plot the ECF
  ggplot(df_combined_ehf1_filter_year, aes(x = LOCAL_DATE, y = ECF_05)) +
    geom_line() +
    labs(
      title = paste("Excessive Cold Factor (ECF) Over for", year),
      x = "Date",
      y = "ECF"
    ) +
    theme_minimal()+
    scale_x_date(date_labels = "%b %d", date_breaks = "5 day")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12)) 
}

# Function to format day of the year as "Month Day"
day_of_year_to_date <- function(day_of_year) {
  as.Date(day_of_year - 1, origin = "2000-01-01")
}
plot_ehf_all <- function(df_combined_ehf1, start_year, end_year, specific_year,station_name) {
  # Create an empty data frame to store all years' data
  all_years_data <- data.frame()
  
  # Loop through each year and bind the data
  for (year in start_year:end_year) {
    df_plot_year <- df_combined_ehf1 %>%
      filter(LOCAL_YEAR == year) %>%
      mutate(DayOfYear = as.integer(format(LOCAL_DATE, "%j")),
             Year = year)
    all_years_data <- bind_rows(all_years_data, df_plot_year)
  }
  
  # Plot all years in grey and the specific year in red
  ggplot(all_years_data, aes(x = DayOfYear)) +
    geom_line(data = filter(all_years_data, Year != specific_year), 
              aes(y = EHF_95, group = Year), 
              color = "grey", alpha = 0.5) +
    geom_line(data = filter(all_years_data, Year == specific_year), 
              aes(y = EHF), 
              color = "red") +
    labs(
      title = paste("EHF Over the Years with Highlight on", specific_year,"at",station_name),
      x = "Day of Year",
      y = "EHF"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.margin = margin(20, 20, 20, 20)) +
    theme(aspect.ratio = 1/2) +
    scale_x_continuous(
      breaks = seq(1, 365, by = 5),
      labels = function(x) format(day_of_year_to_date(x), "%b %d")
    )+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12)) 
}


plot_ecf_all <- function(df_combined_ehf1, start_year, end_year, specific_year,station_name) {
  # Create an empty data frame to store all years' data
  all_years_data <- data.frame()
  
  # Loop through each year and bind the data
  for (year in start_year:end_year) {
    df_plot_year <- df_combined_ehf1 %>%
      filter(LOCAL_YEAR == year) %>%
      mutate(DayOfYear = as.integer(format(LOCAL_DATE, "%j")),
             Year = year)
    all_years_data <- bind_rows(all_years_data, df_plot_year)
  }
  
  # Plot all years in grey and the specific year in red
  ggplot(all_years_data, aes(x = DayOfYear)) +
    geom_line(data = filter(all_years_data, Year != specific_year), 
              aes(y = ECF_05, group = Year), 
              color = "grey", alpha = 0.5) +
    geom_line(data = filter(all_years_data, Year == specific_year), 
              aes(y = ECF_05), 
              color = "red") +
    labs(
      title = paste("ECF_05 Over the Years with Highlight on", specific_year,"at",station_name),
      x = "Day of Year",
      y = "ECF_05"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.margin = margin(20, 20, 20, 20)) +
    theme(aspect.ratio = 1/2) +
    scale_x_continuous(
      breaks = seq(1, 365, by = 5),
      labels = function(x) format(day_of_year_to_date(x), "%b %d")
    )+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12)) 
}
