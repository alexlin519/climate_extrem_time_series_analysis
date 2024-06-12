library(ggplot2)
library(dplyr)
library(ggrepel)
library(tidyr)
if (!require(gridExtra)) {
  install.packages("gridExtra")
}
library(gridExtra)

line_segment_plot <- function(df_filtered_line_seg, station_only = NULL) {
  
  df_filtered_line_seg <- df_filtered_line_seg[ (df_filtered_line_seg$Heatwave) == "Heatwave",]
  #df_filtered_line_seg <- df_line_seg[!is.na(df_line$daily_max_temp),]
  
  
  #####
  #read and combine yvr 30 yeas based data!!
  #####
  # Select only the necessary columns for plotting
  df_filtered_plot <- df_filtered_line_seg %>%
    select(DayOfYear, Year, station)
  
  # Specify the path to your zip file
  txt_file <- "../data/dates_heatwave.csv"
  df_30yvr_hw_3day <- read.csv(txt_file, header = TRUE, sep = ",")  # Using sep = "" to handle any amount of whitespace
  print(head(df_30yvr_hw_3day))
  

  
  # Convert the begin and end columns to Date type
  df_30yvr_hw_3day <- df_30yvr_hw_3day %>%
    mutate(begin = as.Date(begin),
           end = as.Date(end))
  
  # Generate a sequence of dates for each row and expand the dataframe
  df_expanded <- df_30yvr_hw_3day %>%
    rowwise() %>%
    mutate(dates = list(seq(begin, end, by = "day"))) %>%
    unnest(dates)
  df_expanded <- df_expanded %>%
    filter(format(dates, "%m-%d") >= "04-01" & format(dates, "%m-%d") <= "10-31")
  
  # Create the new dataframe with Year and DayOfYear columns
  
  # Create the new dataframe with Year, DayOfYear, and Station columns
  df_30yvr_hw_3day_new <- df_expanded %>%
    mutate(Year = as.numeric(format(dates, "%Y")),
           DayOfYear = format(dates, "%m-%d"),
           station = "YVR_30y_based") %>%
    select(Year, DayOfYear, station)
  
  
  
  
  #####
  # final data combined, ready for plot
  #####
  df_filtered_plot <- bind_rows(df_filtered_plot, df_30yvr_hw_3day_new)
  print(" head df_filtered_plot")
  print(head(df_filtered_plot))
  print("df_filtered_plot")
  print((df_filtered_plot))
  
  # Filter the dataframe based on the desired criteria
  df_filtered_plot <- df_filtered_plot %>%
    filter(!is.na(station)) %>%
    filter(station %in% station_only)
  head(df_filtered_plot)
  
  # error_rows <- df_filtered_line_seg %>%
  #   filter(!is.finite(DayOfYear) | !is.finite(Year) | is.na(station))
  # 
  # # Print the rows with errors
  # print("Rows with errors:")
  # print(error_rows)

  
  # Ensure the Year column has finite values for calculating breaks
  #df_filtered_line_seg$Year <- as.numeric(as.character(df_filtered_line_seg$Year))#
  # min_year <- min(df_filtered_plot$Year, na.rm = TRUE)
  # max_year <- max(df_filtered_plot$Year, na.rm = TRUE)
  # lseg_plot <- ggplot(df_filtered_plot, aes(x = DayOfYear, y = Year, fill = station)) +
  #   geom_tile(aes(width = 0.999, height = 0.999), alpha = 0.55) +  # Adjust transparency and size of rectangles
  #   labs(title = "Temperature Trends Over 3 Days hw by Station",
  #        x = "Day of Year",
  #        y = "Year") +
  #   scale_fill_manual(values = c("YVR" = "#CE5EF7", "Kamloops" = "green", "YVR_30y_based" = 'red',
  #                                "Prince_George" = "blue",'Kelowna'="orange")) + #'YVR_era5'="orange"
  #   theme_minimal() +
  #   theme(plot.title = element_text(hjust = 0.5),
  #         axis.text.y = element_text(size = 6.5,margin = margin(r = -2.5) ))+
  #   scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)])+
  #   theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1, margin = margin(t = -13)))+
  #   scale_y_continuous(breaks = seq(min(df_filtered_plot$Year),
  #   max(df_filtered_plot$Year), by = 2))  # Add more breaks on y-axis
  # print(lseg_plot)
  
  
  
 
  
  
  
  
  # Define the colors for specific stations
  station_colors <- c("YVR" = "#CE5EF7", "Kamloops" = "green", "YVR_30y_based" = 'red',
                      "Prince_George" = "blue", 'Kelowna' = "orange")
  
  # Define the year ranges for each plot
  year_ranges <- list(1940:1959, 1960:1979, 1980:1999, 2000:2024)
  
  # # Loop through each year range and create a plot
  # for (i in seq_along(year_ranges)) {
  #   year_range <- year_ranges[[i]]
  #   df_year_range <- df_filtered_plot %>% filter(Year %in% year_range)
  #   
  #   # Create a unique identifier for each combination of year and station
  #   df_year_range <- df_year_range %>%
  #     mutate(YearStation = factor(paste(Year, station, sep = "_"), levels = unique(paste(Year, station, sep = "_"))))
  #   
  #   plot <- ggplot(df_year_range, aes(x = DayOfYear, y = YearStation, fill = station)) +
  #     geom_tile(aes(width = 0.999, height = 0.999), alpha = 0.55) +  # Adjust transparency and size of rectangles
  #     labs(title = paste("Temperature Trends Over 3 Days hw by Station -", min(year_range), "to", max(year_range)),
  #          x = "Day Of Year",
  #          y = "Year - Station") +
  #     scale_fill_manual(values = station_colors) + #'YVR_era5'="orange"
  #     theme_minimal() +
  #     theme(plot.title = element_text(hjust = 0.5),
  #           axis.text.y = element_text(size = 6.5, margin = margin(r = -2.5)),
  #           axis.text.x = element_text(size = 6, angle = 45, hjust = 1, margin = margin(t = -13))) +
  #     scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)]) +
  #     scale_y_discrete(labels = rep(unique(df_year_range$Year), each = length(unique(df_year_range$station))))
  #   
  #   # Save the plot
  #   plot_filename <- paste0("Temperature_Trends_", min(year_range), "_to_", max(year_range), ".png")
  #   ggsave(plot_filename, plot, width = 10, height = 8)
  #   
  #   # Print the plot
  #   print(plot)
  #}
  
  
  # Loop through each year range and create a plot
  for (i in seq_along(year_ranges)) {
    year_range <- year_ranges[[i]]
    df_year_range <- df_filtered_plot %>% filter(Year %in% year_range)
    
    # Create a unique identifier for each combination of station and year
    df_year_range <- df_year_range %>%
      mutate(Year = as.factor(Year),
             StationYear = factor(paste(Year, station, sep = "_"), levels = unique(paste(Year, station, sep = "_"))))
    
    plot <- ggplot(df_year_range, aes(x = DayOfYear, y = StationYear, fill = station)) +
      geom_tile(aes(width = 0.999, height = 0.999), alpha = 0.75) +  # Adjust transparency and size of rectangles
      labs(title = paste("Temperature Trends Over 3 Days by Station -", min(year_range), "to", max(year_range)),
           x = "Day Of Year",
           y = "Year - Station") +
      scale_fill_manual(values = station_colors) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.y = element_text(size = 6.5, margin = margin(r = -2.5)),
            axis.text.x = element_text(size = 6, angle = 45, hjust = 1, margin = margin(t = -13))) +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)]) +
      scale_y_discrete(labels = function(x) gsub(".*_", "", x))  # Show only the station name on the y-axis
    
    # Print the plot
    print(plot)
  }
  
  return(df_filtered_plot)
  
}








df_1266<- line_segment_plot(df_line_seg_3day, station_only = c("YVR", "Prince_George","Kamloops","Kelowna",""))

library(ggplot2)
library(dplyr)

# Assuming df_filtered_plot is your dataframe
# Creating a function to generate the plots

# Creating a function to generate the plots
generate_temperature_plots <- function(df, year_ranges, station_colors) {
  for (year_range in year_ranges) {
    df_year_range <- df %>% filter(Year %in% year_range)
    # Ensure each combination of year and station is unique and ordered properly
    df_year_range <- df_year_range %>%
      mutate(Year = as.factor(Year),
             StationYear = paste(Year,station , sep = "_"),
             YearFactor = factor(Year, levels = unique(Year)),
             Group = as.numeric(factor(Year)) %/% 4 %% 2) %>%  # Add a group for background coloring
      arrange(Year, station)
    # Create the plot
    plot <- ggplot(df_year_range, aes(x = DayOfYear, y = StationYear, fill = station)) +
      geom_tile(aes(width = 0.9, height = 0.9), alpha = 0.75) +
      labs(title = paste("!!!!", min(year_range), "to", max(year_range)),
           x = "Day Of Year",
           y = "Year") +
      scale_fill_manual(values = station_colors) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.y = element_text(size = 6.5, margin = margin(r = -2.5)),
            axis.text.x = element_text(size = 6, angle = 45, hjust = 1, margin = margin(t = -13))) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)]) +
    scale_y_discrete(labels = function(x) {
      years <- sub("_.*", "", x)
      idx <- seq_along(years)
      ifelse((idx - 1) %% length(station_colors) == 0, years, "")
    })  # Show only the year part on the y-axis every 4th label
      
    # Print the plot
    print(plot)
  }
}
# Example usage

# Function to create year ranges
create_year_ranges <- function(start_year, end_year, num_splits) {
  years <- seq(start_year, end_year)
  split_size <- ceiling(length(years) / num_splits)
  year_splits <- split(years, ceiling(seq_along(years) / split_size))
  lapply(year_splits, function(x) seq(min(x), max(x)))
}
# Example usage
year_ranges <- create_year_ranges(1940, 2024, 4)

station_colors <- c("blue", "green", "red", "#FF00FF","#FFD700","#1E90FF")  # Define your station colors
length(station_colors)
# Call the function with your data
generate_temperature_plots(df_1266, year_ranges, station_colors)



