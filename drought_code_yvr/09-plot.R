library(ggplot2)
library(readr)
library(dplyr)
library(ggrepel)
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
  sca <- ggplot(heatwave_data, aes(x = year_month, y = avgexc, color = factor(season))) +
    geom_point(size = 3, alpha = 0.7) +
    labs(title = "Scatter Plot of Average Temperature Change by Year and Month",
         x = "Year",
         y = "Average Temperature Change (°C)",
         color = "Month") +
    theme_minimal() +
    scale_color_manual(values = season_colors) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 year")+
    #scale_x_continuous(breaks = seq(1937, 2024, by = 2)) +
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank())
  print(sca)
}


plot_scatter_color_top <- function(data_1_path,data1_start_year,station1, 
                         data_2_path,data2_start_year,station2,extreme_range) {
  # Load the data
  data_1 <- read_csv(data_1_path)
  data_2 <- read_csv(data_2_path)
  data_1$year <- as.integer(substr(data_1$yrmon, 1, 4))
  data_2$year <- as.integer(substr(data_2$yrmon, 1, 4))
  #filter year
  data_1 <- data_1 %>% filter(year >= data1_start_year)
  data_2 <- data_2 %>% filter(year >= data2_start_year)
  
  # Merge the datasets on yrmon
  df_merged <- merge(data_1, data_2, by = c("yrmon"), suffixes = c("_1", "_2"))
  
  
  # Identify the range of extreme points based on the highest temperatures
  top_extremes <- df_merged %>%
    arrange(desc(max_consec_1 + max_consec_2)) %>%
    slice(extreme_range) %>%
    mutate(label = paste(yrmon, sep = "-"))
  # Create a color palette for the labels
  base_colors <- c('#FF0000', '#008000', '#0000FF', '#000000', '#FF00FF', '#FFA500', '#FFD700', '#D2691E',
                   '#D2B48C', '#808000', '#FFFF00', '#40E0D0', '#00BFFF', '#1E90FF', '#8A2BE2', '#FF1493',
                   '#A52A2A', '#7FFF00', '#8B008B', '#FF69B4','#A9A9A9', '#696969') 
  # Create scatter plot with text annotations and color for top extremes
  plot <- ggplot(df_merged, aes(x = max_consec_1, y = max_consec_2)) +
    geom_point() +
    geom_point(data = top_extremes, aes(x = max_consec_1, y = max_consec_2, color = label), size = 1.5) +
    scale_color_manual(values = base_colors) +
    geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "red", size = 1) +  # Add 45-degree line
    labs(
      title = paste("Max Num of monthly consec dry days:", station1, "vs", station2),
      x = paste("Max Num of consecutive dry day(max_consec) (", station1, ")", sep = ""),
      y = paste("Max Num of consecutive dry day (", station2, ")", sep = "")
    ) +
    theme_minimal()+
    #scale based on max and min of the value
    scale_x_continuous(limits = c(0, max(df_merged$max_consec_1, na.rm = TRUE)),
                       breaks = seq(0, max(df_merged$max_consec_1, na.rm = TRUE), by = 5)) +
    scale_y_continuous(limits = c(0, max(df_merged$max_consec_2, na.rm = TRUE)),
                       breaks = seq(0, max(df_merged$max_consec_2, na.rm = TRUE), by = 5))
  print(plot)
  
  
}


# Define the function
plot_avg_exc_temp <- function(data, year) {
  ggplot(data, aes(x = month, y = avgexc, color = station, size = rank)) +
    geom_point(alpha = 0.6) +
    geom_text_repel(aes(label = ifelse(rank <= 300, rank, "")), vjust = -0.5, size = 3, color = "black",max.overlaps = Inf) +  # Add rank labels
    labs(title = paste("Average Excess Temperature (avgexc) by Station",year, "SUMMER"),
         x = "Year",
         y = "Average Excess Temperature (°C)",
         size = "Rank") +
    theme_minimal() +
    scale_color_manual(values = station_colors) +
    scale_size_continuous(
      name = "Rank",
      trans = scales::trans_new(
        name = "custom",
        transform = function(x) ifelse(x > 300, 300, x),
        inverse = function(x) x,
        domain = c(0, 300)
      ),
      range = c(8.5, 2.5),  # Adjust this range based on the desired size scale
      limits = c(1, 300)
    ) +
    scale_x_continuous(breaks = seq(1, 12, by = 1))
}