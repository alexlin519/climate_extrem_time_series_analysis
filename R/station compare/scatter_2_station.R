library(ggplot2)
library(dplyr)
library(ggrepel)

generate_extreme_temp_scatter_plots <- function(filtered_heatmap_all, extreme_range) {
  # List of unique stations
  stations <- unique(filtered_heatmap_all$Station)
  
  # List to store the dataframes of the labels to be displayed
  label_dataframes <- list()
  
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
      
      # Add the dataframe to the list
      label_dataframes[[paste(station1, station2, sep = "_vs_")]] <- top_extremes
      
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
  # Return the list of dataframes
  return(label_dataframes)
}





# Assuming label_dataframes is the list of dataframes returned from the function
analyze_extrem_scatter <- function(label_dataframes) {
  # Initialize lists to store the results
  unique_year_counts_list <- list()
  unique_yearmonth_counts_list <- list()
  
  for (name in names(label_dataframes)) {
    df <- label_dataframes[[name]]
    
    # Extract month from DayOfYear
    df <- df %>%
      mutate(Month = sub("^0", "", substr(DayOfYear, 1, 2)))
    
    # Count unique values for each year
    unique_year_counts <- df %>%
      group_by(Year) %>%
      summarise(unique_count = n_distinct(DayOfYear),
                Month = paste(unique(Month), collapse = "\n")) %>%
      rename(!!paste0("unique_count_", name) := unique_count)
    
    # Store the result with the dataframe name as key
    unique_year_counts_list[[name]] <- unique_year_counts 
    
    
    
    # Combine Year and Month into a new column YearMonth
    df <- df %>%
      mutate(YearMonth = paste(Year, Month, sep = "-"))
    
    # Count unique values for each YearMonth
    unique_yearmonth_counts <- df %>%
      group_by(YearMonth) %>%
      summarise(unique_count = n_distinct(DayOfYear)) %>%
      rename(!!paste0("unique_count_", name) := unique_count)
    
    # Store the result with the dataframe name as key
    unique_yearmonth_counts_list[[name]] <- unique_yearmonth_counts
  }
  
  # Combine unique_year_counts dataframes into one
  combined_unique_year_counts <- bind_rows(
    lapply(names(unique_year_counts_list), function(name) {
      df <- unique_year_counts_list[[name]]
      df %>% rename(unique_count = names(df)[2]) %>%
        mutate(Comparison = name)
    })
  )
  
  # Ensure Year is treated as a factor for proper stacking
  combined_unique_year_counts$Year <- as.factor(combined_unique_year_counts$Year)
  
  # Create and display a stacked bar plot
  plot <- ggplot(combined_unique_year_counts, aes(x = Year, y = unique_count, fill = Comparison)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = Month), position = position_stack(vjust = 0.5), size = 3,lineheight = 0.8) +
    labs(title = "Unique Count by Year and Comparison",
         x = "Year",
         y = "Unique Count",
         fill = "Comparison") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) # Adjust text angle and size
  
  print(plot)
  
  # De-select the Plot_Months column before returning the results
  unique_year_counts_list <- lapply(unique_year_counts_list, function(df) {
    df %>% select(-Month)
  })
  
  return(list(unique_year_counts = unique_year_counts_list, unique_yearmonth_counts = unique_yearmonth_counts_list))
}

# Example usage:
# results <- analyze_extrem_scatter(label_dataframes)
