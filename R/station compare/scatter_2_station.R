library(ggplot2)
library(dplyr)
library(ggrepel)
#install.packages("viridis")
library(viridis)
base_colors <- c('#FF0000', '#008000', '#0000FF', '#FF00FF', '#FFA500', '#FFD700', '#D2691E',
                 '#D2B48C', '#808000', '#FFFF00', '#40E0D0', '#00BFFF', '#1E90FF', '#8A2BE2', '#FF1493',
                 '#A52A2A', '#7FFF00', '#8B008B', '#FF69B4','#A9A9A9', '#696969') 

generate_extreme_temp_scatter_plots <- function(filtered_heatmap_all, extreme_range) {
  # List of unique stations excluding 'YVR_era5'
  stations <- setdiff(unique(filtered_heatmap_all$Station), 'YVR_era5')
  
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
        geom_point(data = top_extremes, aes(x = Max_Temp_Year_1, y = Max_Temp_Year_2, color = label), size = 1.5) +
        # geom_text_repel(data = top_extremes, aes(x = Max_Temp_Year_1, y = Max_Temp_Year_2, label = label, color = label), 
        #                 nudge_x = 0.5, nudge_y = 0.5, show.legend = FALSE) +
        #scale_color_manual(values = label_colors) +
        #scale_color_viridis(discrete = TRUE, option = "plasma") + 
        scale_color_manual(values = base_colors) +
        geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "red", size = 1) +  # Add 45-degree line
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



generate_scatter_plots_for_yvr_diff_data <- function(filtered_heatmap_all, extreme_range) {
  
  filtered_stations <- filtered_heatmap_all %>%
    filter(Station %in% c('YVR_era5', 'YVR'))
  
  # List of the selected stations
  stations <- unique(filtered_stations$Station)
  
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
      
      
      ######### Fit a linear model
      fit <- lm(Max_Temp_Year_2 ~ Max_Temp_Year_1, data = df_merged)
      
      ########## Generate 95% prediction intervals
      pred <- predict(fit, newdata = df_merged, interval = "prediction", level = 0.95)
      df_merged <- df_merged %>%
        mutate(pred_lwr = pred[, "lwr"], pred_upr = pred[, "upr"])
      
      # Create scatter plot with text annotations and color for top extremes
      plot <- ggplot(df_merged, aes(x = Max_Temp_Year_1, y = Max_Temp_Year_2)) +
        geom_point() +
        geom_point(data = top_extremes, aes(x = Max_Temp_Year_1, y = Max_Temp_Year_2, color = label), size = 1.5) +
        # geom_text_repel(data = top_extremes, aes(x = Max_Temp_Year_1, y = Max_Temp_Year_2, label = label, color = label), 
        #                 nudge_x = 0.5, nudge_y = 0.5, show.legend = FALSE) +
        #scale_color_manual(values = label_colors) +
        #scale_color_viridis(discrete = TRUE, option = "plasma") + 
        scale_color_manual(values = base_colors) +
        geom_line(aes(y = pred_lwr), linetype = "dotted", color = "blue", size = 0.5) +  # Lower prediction interval
        geom_line(aes(y = pred_upr), linetype = "dotted", color = "blue", size = 0.5) +  # Upper prediction interval
        geom_abline(slope = 1, intercept = 0, linetype = "solid", color = "red", size = 1) +  # Add 45-degree line
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
