
######## week3

# Main function to plot all years in one plot
library(ggplot2)
library(dplyr)


# Main function to plot all years in one plot with a specific year highlighted
plot_all_years_with_baseline <- function(start_year, end_year, specific_year) {
  all_years_data <- data.frame()
  
  for (year in start_year:end_year) {
    df_plot_year <- prepare_plot_data("Percentile_90", year, "MAX_TEMPERATURE")
    df_plot_year$Year <- year
    all_years_data <- bind_rows(all_years_data, df_plot_year)
  }
  
  ggplot(all_years_data, aes(x = DayOfYear)) +
    geom_line(data = filter(all_years_data, Year != specific_year), 
              aes(y = MAX_TEMP_YEAR, group = Year), 
              color = "grey", alpha = 0.5) +
    geom_line(data = filter(all_years_data, Year == specific_year), 
              aes(y = MAX_TEMP_YEAR), 
              color = "grey") +
    geom_line(data = filter(all_years_data, Year == specific_year), 
              aes(y = Percentile_90), 
              color = "blue", linetype = "dashed") +
    labs(
      title = paste("90th Percentile of Max Temperature vs all years from", start_year, "to",end_year),
      x = "Day of Year",
      y = "Temperature (°C)"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.margin = margin(20, 20, 20, 20)) +
    theme(aspect.ratio = 1/2)
}


plot_all_years_highlight_specific_year <- function(start_year, end_year, specific_year) {
  all_years_data <- data.frame()
  
  for (year in start_year:end_year) {
    df_plot_year <- prepare_plot_data("Percentile_90", year, "MAX_TEMPERATURE")
    #print(df_plot_year)
    df_plot_year$Year <- year
    all_years_data <- bind_rows(all_years_data, df_plot_year)
  }
  #print(all_years_data)
  ggplot(all_years_data, aes(x = DayOfYear)) +
    geom_line(data = filter(all_years_data, Year != specific_year), 
              aes(y = MAX_TEMP_YEAR, group = Year), 
              color = "grey", alpha = 0.5) +
    geom_line(data = filter(all_years_data, Year == specific_year), 
              aes(y = MAX_TEMP_YEAR), 
              color = "red") +
    geom_line(data = filter(all_years_data, Year == specific_year), 
              aes(y = Percentile_90), 
              color = "blue", linetype = "dashed") +
    labs(
      title = paste("90th Percentile of Max Temperature vs all years, highlight Year", specific_year),
      x = "Day of Year",
      y = "Temperature (°C)"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.margin = margin(20, 20, 20, 20)) +
    theme(aspect.ratio = 1/2)
}

