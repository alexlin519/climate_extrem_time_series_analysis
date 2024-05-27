library(scales)
library(ggplot2)
#install.packages("reshape2")
library(reshape2)
##bugfree
prepare_heatmap_plot_data <- function(raw_df,baseline_col, target_year, temp_col = "MAX_TEMPERATURE") {
  # Check if df and df_percentiles are data frames
  if (!exists("df") || !is.data.frame(df)) {
    stop("The object 'df' does not exist or is not a data frame.")
  }
  
  if (!exists("df_percentiles") || !is.data.frame(df_percentiles)) {
    stop("The object 'df_percentiles' does not exist or is not a data frame.")
  }
  
  # Add Month and Day columns
  df_for_plot <- raw_df %>%
    mutate(
      LOCAL_DATE = as.Date(LOCAL_DATE, format = "%Y-%m-%d"),
      Month = format(LOCAL_DATE, "%m"),
      Day = format(LOCAL_DATE, "%d")
    )
  
  # Filter data for the target year and summarize max temperature
  df_target_year <- df_for_plot %>%
    filter(format(LOCAL_DATE, "%Y") == as.character(target_year)) %>%
    group_by(Month, Day) %>%
    summarize(
      Max_Temp_Year = ifelse(all(is.na(get(temp_col))), NA, max(get(temp_col), na.rm = TRUE))
    ) %>%
    ungroup()
  
  # Merge the baseline data with the target year data
  df_merged <- df_percentiles %>%
    left_join(df_target_year, by = c("Month", "Day"))
  
  # Combine Month and Day for plotting and add additional columns
  df_for_plot_year <- df_merged %>%
    mutate(
      #DayOfYear = as.numeric(format(as.Date(paste0(target_year, "-", Month, "-", Day), format = "%Y-%m-%d"), "%j")),
      DayOfYear = paste(Month, Day, sep = "-"),
      Year = target_year
    ) %>%
    select(-ROLLING_WINDOW_ALL_YEAR_VALUES)  # Exclude unnecessary columns
  
  return(df_for_plot_year)
}




create_heatmap <- function(df, start_date, end_date) {
  # Add a binary column indicating whether the temperature is higher than the baseline
  df$Temp_Higher <- ifelse(df$Max_Temp_Year > df$Percentile_90, "Higher", "Not Higher")

  # Filter data for the specified date range
  df_filtered <- df %>%
    filter(DayOfYear >= start_date & DayOfYear <= end_date)

  # Reshape data for heatmap
  data_melted_temp <- melt(df_filtered, id.vars = c("Year", "DayOfYear"),
                           measure.vars = "Max_Temp_Year")

  data_melted_temp$Temp_Higher <- ifelse(data_melted_temp$value > df_filtered$Percentile_90,
                                         "Higher", "Not Higher")

  # Create a new column for color mapping
  data_melted_temp$Color <- ifelse(data_melted_temp$Temp_Higher == "Higher", data_melted_temp$value, NA)
  
  #percentile_20 <- quantile(df_filtered$Max_Temp_Year, probs = 0.2, na.rm = TRUE)
  
  # Create heatmap
  heatmap_plot <- ggplot(data_melted_temp, aes(x = DayOfYear, y = Year)) +
    geom_tile(aes(fill = Color)) +
    
    # scale_fill_gradientn(colors = c("blue", "yellow", "red"),
    #                      values = rescale(values),
    #                      na.value = "white",
    #                      name = "Temperature") +
    scale_fill_gradient2(low = "blue", mid = "yellow", high = "red",
                         midpoint = 4+ mean(df_filtered$Max_Temp_Year[df_filtered$Temp_Higher == "Higher"], na.rm = TRUE),
                         #midpoint = percentile_20,
                         na.value = "white", name = "Temperature") +
    labs(title = "Temperature Comparison Heatmap (April 1st to September 30th) Over 50 Years",
         x = "Day of Year", y = "Year") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12)) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)])

  return(heatmap_plot)
}



create_heatmap_diff <- function(df, start_date, end_date) {
  # Add a binary column indicating whether the temperature is higher than the baseline
  df$Temp_Higher <- ifelse(df$Max_Temp_Year > df$Percentile_90, "Higher", "Not Higher")
  
  # Filter data for the specified date range
  df_filtered <- df %>%
    filter(DayOfYear >= start_date & DayOfYear <= end_date)
  
  # Reshape data for heatmap
  
  data_melted_temp <- melt(df_filtered, id.vars = c("Year", "DayOfYear"),
                           measure.vars = "Max_Temp_Year")
  data_melted_temp$Temp_Higher <- ifelse(data_melted_temp$value > df_filtered$Percentile_90,
                                         "Higher", "Not Higher")
  

  df_filtered$Temp_Diff <- df_filtered$Max_Temp_Year - df_filtered$Percentile_90
  
  data_melted_temp$Color <- ifelse(data_melted_temp$Temp_Higher == "Higher",df_filtered$Temp_Diff, NA)
  
  # Create heatmap
  heatmap_plot <- ggplot(data_melted_temp, aes(x = DayOfYear, y = Year)) +
    geom_tile(aes(fill = Color)) +
    # scale_fill_gradientn(colors = c("#0000FF", "blue", "white", "red", "#FF0000"), 
    #                      values = scales::rescale(c(-11, -8, 0, 8, 11)), 
    #                      name = "Temp Difference")+
    scale_fill_viridis_c(na.value = "white",name = "Temp Difference")+
    #scale_fill_gradient(na.value = "white",name = "Temp Difference",low = "blue", high = "red")+
    labs(title = "Temperature Comparison Heatmap (April 1st to September 30th) Over 50 Years",
         x = "Day of Year", y = "Year") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12)) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)])
  
  return(heatmap_plot)
}
