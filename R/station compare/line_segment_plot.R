library(ggplot2)
library(dplyr)
library(ggrepel)
library(tidyr)

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
  
  
  
  # Filter the dataframe based on the desired criteria
  df_filtered_plot <- df_filtered_plot %>%
    filter(!is.na(station)) %>%
    filter(station %in% station_only)
  
  
  # error_rows <- df_filtered_line_seg %>%
  #   filter(!is.finite(DayOfYear) | !is.finite(Year) | is.na(station))
  # 
  # # Print the rows with errors
  # print("Rows with errors:")
  # print(error_rows)

  
  # Ensure the Year column has finite values for calculating breaks
  #df_filtered_line_seg$Year <- as.numeric(as.character(df_filtered_line_seg$Year))#
  min_year <- min(df_filtered_plot$Year, na.rm = TRUE)
  max_year <- max(df_filtered_plot$Year, na.rm = TRUE)
  lseg_plot <- ggplot(df_filtered_plot, aes(x = DayOfYear, y = Year, fill = station)) +
    geom_tile(aes(width = 0.999, height = 0.999), alpha = 0.55) +  # Adjust transparency and size of rectangles
    labs(title = "Temperature Trends Over 3 Days hw by Station",
         x = "Day of Year",
         y = "Year") +
    scale_fill_manual(values = c("YVR" = "#CE5EF7", "Kamloops" = "green", "YVR_30y_based" = 'red',
                                 "Prince_George" = "blue",'Kelowna'="orange")) + #'YVR_era5'="orange"
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(size = 6.5,margin = margin(r = -2.5) ))+
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)])+
    theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1, margin = margin(t = -13)))+
  scale_y_continuous(breaks = seq(min(df_filtered_plot$Year),
  max(df_filtered_plot$Year), by = 2))  # Add more breaks on y-axis
  print(lseg_plot)
}