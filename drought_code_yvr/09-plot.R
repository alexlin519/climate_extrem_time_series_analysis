library(ggplot2)
library(readr)
library(dplyr)
library(stringr)  # Load stringr for str_wrap function
# Function to categorize months into seasons
categorize_season <- function(month) {
  if (month %in% c(12, 1, 2)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else {
    return("Fall")
  }
}
plot_avg_temp_change <- function(heatwave_data) {
  
  #plot year-month with nexc
  # Load necessary libraries
  
  # Create a new column for year-month
  heatwave_data <- heatwave_data %>%
    mutate(year_month = as.Date(paste(year, month, "01", sep = "-"))) %>%
    mutate(season = sapply(month, categorize_season))
  
  # Plotting average temperature change by year-month
  ts <- ggplot(heatwave_data, aes(x = year_month, y = avgexc)) +
    geom_line() +
    geom_point() +
    labs(title = "Average Temperature Change by Year-Month",
         x = "Year-Month",
         y = "Average Temperature Change (°C)") +
    theme_minimal()+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank())
    
  print(ts)
  
  dens <- ggplot(heatwave_data, aes(x = avgexc, fill = factor(year))) +
    geom_density(alpha = 0.3) +
    labs(title = "Density Plot of avgexc by Year",
         x = "Average Temperature Change (avgexc)",
         y = "Density",
         fill = "Year") +
    theme_minimal()
  
  print(dens)
  # Box plot to compare avgexc across different years
  box <- ggplot(heatwave_data, aes(x = factor(year), y = avgexc)) +
    geom_boxplot() +
    labs(title = "Box Plot of avgexc by Year",
         x = "Year",
         y = "Average Temperature Change (avgexc)") +
    theme_minimal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(box)
  
  
  season_colors <- c("Winter" = "#0000FF",  # Blue
                     "Spring" = "#7FFF00",  # Green
                     "Summer" = "red",  # Red
                     "Fall" = "#ff7f00")    # Orange
  sca <- ggplot(heatwave_data, aes(x = year, y = avgexc, color = factor(season))) +
    geom_point(size = 3, alpha = 0.7) +
    labs(title = "Scatter Plot of Average Temperature Change by Year and Month",
         x = "Year",
         y = "Average Temperature Change (°C)",
         color = "Month") +
    theme_minimal() +
    scale_color_manual(values = season_colors) +
    scale_x_continuous(breaks = seq(1937, 2024, by = 2)) +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank())
  print(sca)
  }