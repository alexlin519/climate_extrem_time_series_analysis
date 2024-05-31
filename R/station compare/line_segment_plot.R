library(ggplot2)
library(dplyr)
library(ggrepel)

line_segment_plot <- function(df_filtered_line_seg, station_only = NULL) {
  
  # Filter the dataframe based on the station_filter parameter
  if (!is.null(station_only)) {
    df_filtered_line_seg <- df_filtered_line_seg %>%
      filter(station %in% station_only)
  }

  
  # Ensure the Year column has finite values for calculating breaks
  #df_filtered_line_seg$Year <- as.numeric(as.character(df_filtered_line_seg$Year))#
  min_year <- min(df_filtered_line_seg$Year, na.rm = TRUE)
  max_year <- max(df_filtered_line_seg$Year, na.rm = TRUE)
  lseg_plot <- ggplot(df_filtered_line_seg, aes(x = DayOfYear, y = Year, fill = station)) +
    geom_tile(aes(width = 0.999, height = 0.999), alpha = 0.55) +  # Adjust transparency and size of rectangles
    labs(title = "Temperature Trends Over 3 Days hw by Station",
         x = "Day of Year",
         y = "Year") +
    scale_fill_manual(values = c("YVR" = "blue", "Kamloops" = "green", "Prince_George" = "magenta",'YVR_era5'="orange")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4)])
  #  +scale_y_continuous(breaks = #seq(min(df_filtered_line_seg$Year),
  #max(df_filtered_line_seg$Year), by = 3))  # Add more breaks on y-axis
  print(lseg_plot)
}