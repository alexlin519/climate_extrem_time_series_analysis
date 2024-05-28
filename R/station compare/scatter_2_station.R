library(ggplot2)
library(dplyr)
library(ggrepel)

generate_extreme_temp_scatter_plots <- function(filtered_heatmap_all, extreme_range) {
  # List of unique stations
  stations <- unique(filtered_heatmap_all$Station)
  
  # Generate scatter plots for each pair of stations
  for (i in 1:(length(stations)-1)) {
    for (j in (i+1):length(stations)) {
      station1 <- stations[i]
      station2 <- stations[j]
      
      # Filter data for the two stations
      df_station1 <- filtered_heatmap_all %>% filter(Station == station1)
      df_station2 <- filtered_heatmap_all %>% filter(Station == station2)
      
      # Merge data on Date
      df_merged <- merge(df_station1, df_station2, by = c("Year", "DayOfYear"), suffixes = c("_1", "_2"))
      
      # Identify the range of extreme points based on the highest temperatures
      top_extremes <- df_merged %>%
        arrange(desc(Max_Temp_Year_1 + Max_Temp_Year_2)) %>%
        slice(extreme_range) %>%
        mutate(label = paste(Year, DayOfYear, sep = "-"))
      
      # Create a color palette for the labels
      label_colors <- setNames(scales::hue_pal()(nrow(top_extremes)), top_extremes$label)
      
      # Create scatter plot with text annotations and color for top extremes
      plot <- ggplot(df_merged, aes(x = Max_Temp_Year_1, y = Max_Temp_Year_2)) +
        geom_point() +
        geom_point(data = top_extremes, aes(x = Max_Temp_Year_1, y = Max_Temp_Year_2, color = label), size = 3) +
        # geom_text_repel(data = top_extremes, aes(x = Max_Temp_Year_1, y = Max_Temp_Year_2, label = label, color = label), 
        #                 nudge_x = 0.5, nudge_y = 0.5, show.legend = FALSE) +
        scale_color_manual(values = label_colors) +
        labs(
          title = paste("Scatter Plot of Max Temperature:", station1, "vs", station2),
          x = paste("Max Temperature (", station1, ")", sep = ""),
          y = paste("Max Temperature (", station2, ")", sep = "")
        ) +
        theme_minimal()
      
      # Print the plot
      print(plot)
    }
  }
}


