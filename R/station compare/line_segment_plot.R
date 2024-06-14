library(ggplot2)
library(dplyr)
library(ggrepel)
library(tidyr)
if (!require(gridExtra)) {
  install.packages("gridExtra")
}
library(gridExtra)

line_segment_data_prepare <- function(df_filtered_line_seg, station_only = NULL) {
  
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
  #print(head(df_filtered_plot))
  print("df_filtered_plot")
  #print((df_filtered_plot))
  
  # Filter the dataframe based on the desired criteria
  df_filtered_plot <- df_filtered_plot %>%
    filter(!is.na(station)) %>%
    filter(station %in% station_only)
  #head(df_filtered_plot)
  
  
  # Define the year ranges for each plot
  year_ranges <- list(1940:1959, 1960:1979, 1980:1999, 2000:2024)
  
  # Loop through each year range and create a plot
  ###
  #old version of the code line_segment_plot.R
  ###
  # for (i in seq_along(year_ranges)) {
  #   year_range <- year_ranges[[i]]
  #   df_year_range <- df_filtered_plot %>% filter(Year %in% year_range)
  #   
  #   # Create a unique identifier for each combination of station and year
  #   df_year_range <- df_year_range %>%
  #     mutate(Year = as.factor(Year),
  #            StationYear = factor(paste(Year, station, sep = "_"), 
  #                                 levels = unique(paste(Year, station, sep = "_"))[order(Year, station)]))  # Order StationYear alphabetically within each year
  #   
  #   plot <- ggplot(df_year_range, aes(x = DayOfYear, y = StationYear, fill = station)) +
  #     geom_tile(aes(width = 0.999, height = 0.999), alpha = 0.75) +  # Adjust transparency and size of rectangles
  #     labs(title = paste("Temperature Trends Over 3 Days by Station -", min(year_range), "to", max(year_range)),
  #          x = "Day Of Year",
  #          y = "Year - Station") +
  #     scale_fill_manual(values = station_colors) +
  #     theme_minimal() +
  #     theme(plot.title = element_text(hjust = 0.5),
  #           axis.text.y = element_text(size = 6.5, margin = margin(r = -2.5)),
  #           axis.text.x = element_text(size = 6, angle = 45, hjust = 1, margin = margin(t = -13))) +
  #     scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)]) +
  #     scale_y_discrete(labels = function(x) gsub(".*_", "", x))  # Show only the station name on the y-axis
  #   
  #   # Print the plot
  #   print(plot)
  # }
  # 
  return(df_filtered_plot)
  
}


# Function to create year ranges
create_year_ranges <- function(start_year, end_year, num_splits) {
  years <- seq(start_year, end_year)
  split_size <- ceiling(length(years) / num_splits)
  year_splits <- split(years, ceiling(seq_along(years) / split_size))
  lapply(year_splits, function(x) seq(min(x), max(x)))
}

# Function to prepare the data
wrangle_data <- function(df, year_range) {
  df_year_range <- df %>% filter(Year %in% year_range)
  df_year_range <- df_year_range %>%
    mutate(Year = as.factor(Year),
           StationYear = paste(Year, station, sep = "_"),
           YearFactor = factor(Year, levels = unique(Year))) %>%
    arrange(Year, station)
  
  hline_data <- df_year_range %>%
    group_by(Year) %>%
    filter(row_number() == 1) %>%
    mutate(y_position = as.numeric(factor(StationYear, levels = unique(df_year_range$StationYear))) - 0.5)
  
  list(df_year_range = df_year_range, hline_data = hline_data)
}

# Function to generate the plot
generate_plot <- function(df_year_range, hline_data, year_range, station_colors) {
  ggplot(df_year_range, aes(x = DayOfYear, y = StationYear, fill = station)) +
    geom_tile(aes(width = 0.9999, height = 0.9999), alpha = 0.75) +
    geom_hline(data = hline_data, size = 0.5, aes(yintercept = as.numeric(y_position)), color = "black", linetype = "solid") +
    labs(title = paste("At least 3 day heatwave from", min(year_range), "to", max(year_range)),
         x = "Day Of Year",
         y = "Year") +
    scale_fill_manual(
      breaks = c("YVR", "Prince_George", "Penticton", "Kelowna", "Kamloops", "Abbotsford", "YVR_30y_based"),
      values = station_colors
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(size = 8, hjust = 1.5, vjust = -1.8, margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.text.x = element_text(size = 6, angle = 45, vjust = 1.6, 
                                     hjust = 1, margin = margin(t = 5, r = 0, b = 0, l = 0))) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)]) +
    scale_y_discrete(labels = function(x) {
      years <- sub("_.*", "", x)
      unique_years <- !duplicated(years)
      labels <- ifelse(unique_years, years, "")
      labels
    })
}

# Main function to generate temperature plots
generate_temperature_plots <- function(df, year_ranges, station_colors) {
  for (year_range in year_ranges) {
    data <- wrangle_data(df, year_range)
    plot <- generate_plot(data$df_year_range, data$hline_data, year_range, station_colors)
    print(plot)
  }
  return(invisible(data$hline_data))
}



