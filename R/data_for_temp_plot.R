library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)

## functions for prepareing data for plotting 

# Define the function to prepare the data
#return df_for_plot_year
prepare_plot_data <- function(baseline_col, year, temp_col = "MAX_TEMPERATURE") {
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
      MAX_TEMP_YEAR = max(get(temp_col))
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
    )
  
  return(df_for_plot_year)
}



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


