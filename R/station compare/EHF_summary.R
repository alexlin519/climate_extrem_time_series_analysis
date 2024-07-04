library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
setwd("/Users/alexlin/summer_stat/climate_extreme_RA/R")

file_paths <- c("../output/Abbotsford_monthly_max_EHF.csv",
                "../output/YVR_monthly_max_EHF.csv",
                "../output/Kelowna_monthly_max_EHF.csv",
                "../output/Prince_George_monthly_max_EHF.csv",
                "../output/FortNelson_monthly_max_EHF.csv"
                )
# Function to read and select necessary columns
read_and_select <- function(file_path) {
  read.csv(file_path) #%>%
    #select(all_of(needed_columns))
}

# Read and combine all datasets
df_ehf_all <- map_dfr(file_paths, read_and_select)
#print(head(df_ehf_all))
write.csv(df_ehf_all, file = "../output/df_ehf_all.csv", row.names = FALSE)

plot_ehf_and_correlation <- function(data, month, start_year = 1940,end_year = 2024) {
  # Filter for the specified month and years >= start_year
  filtered_data <- data %>% filter(Month == month & LOCAL_YEAR >= start_year)
  
  # Summarize data (optional, depending on what summary you need)
  # For example, calculating the mean max_EHF for each station in the specified month
  summary_data <- filtered_data %>%
    group_by(Station) %>%
    summarize(mean_max_EHF = mean(max_EHF, na.rm = TRUE))
  
  # Get the month name
  month_name <- month.name[month]
  
  # Plot data
  ts_plot <- ggplot(filtered_data, aes(x = LOCAL_YEAR, y = max_EHF, color = Station)) +
    geom_line(alpha = 1) +
    geom_point(alpha = 0.4) +
    labs(title = paste("Max EHF in", month_name, "for All Stations"),
         x = "Year",
         y = "Max EHF") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(start_year,end_year, by = 1)) +  # Show more years on the x-axis
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #print(ts_plot)
  # Prepare data for correlation
  wide_data <- filtered_data %>%
    select(LOCAL_YEAR, Station, max_EHF) %>%
    pivot_wider(names_from = Station, values_from = max_EHF)
  
  # Compute the correlation matrix
  ehf_correlation <- cor(wide_data %>% select(-LOCAL_YEAR),method = "spearman", use = "pairwise.complete.obs")
  
  # Print the correlation matrix
  print(ehf_correlation)
  my_palette <- colorRampPalette(c("white","#FF0000", "blue"))(100)
  
  # Plot the correlation matrix
  corrplot(ehf_correlation,
           method = "number",
           type = "lower",
           #title = "Correlation of Max EHF in August between Stations",
           tl.srt = 0,  # Rotate text labels by 45 degrees
           tl.col = "black",
           col = my_palette)  # Change text color to black for better readability
  title(main = paste("Correlation of Max EHF in", month_name, "between Stations"), line = 2.5)
  
  return((ts_plot))
  }





generate_max_EHF_scatter_plots <- function(df_ehf_all) {
  # List of unique stations
  stations <- unique(df_ehf_all$Station)
  
  # List to store the ggplot objects
  plot_list <- list()
  
  # Generate scatter plots for each pair of stations
  for (i in 1:(length(stations)-1)) {
    for (j in (i+1):length(stations)) {
      station1 <- stations[i]
      station2 <- stations[j]
      
      # Filter data for the two stations
      df_station1 <- df_ehf_all %>% filter(Station == station1)
      df_station2 <- df_ehf_all %>% filter(Station == station2)
      # Merge data on Date
      df_merged <- merge(df_station1, df_station2, by = c("LOCAL_YEAR", "Month"),
                         suffixes = c("_1", "_2"))
      
      # Create scatter plot
      plot <- ggplot() +
        geom_point(data = df_merged, aes(x = max_EHF_1, y = max_EHF_2), color = "blue") +
        #geom_point(data = df_station2, aes(x = max_EHF, y = max_EHF), color = "red") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +  # Add y = x line
        labs(title = paste("Max EHF Comparison:", station1, "vs", station2),
             x = paste("Max EHF -", station1), y = paste("Max EHF -", station2)) +
        theme_minimal()
        
      
      # Add the plot to the list
      plot_list[[paste(station1, station2, sep = "_vs_")]] <- plot
    }
  }
  
  # Return the list of plots
  return(plot_list)
}






# 
# 
# 
# library(ggplot2)
# library(dplyr)
# 
# # Assuming your data frame is named df_wrangling
# # Filter for the month of July (Month == 7)
# july_data <- df_ehf_all %>% filter(Month == 7 & LOCAL_YEAR >= 1940)
# 
# # Summarize data (optional, depending on what summary you need)
# # For example, calculating the mean max_EHF for each station in July
# summary_data <- july_data %>%
#   group_by(Station) %>%
#   summarize(mean_max_EHF = mean(max_EHF, na.rm = TRUE))
# 
# 
# # Plot data
# ggplot(july_data, aes(x = LOCAL_YEAR, y = max_EHF, color = Station)) +
#   geom_line(alpha = 1) +
#   geom_point(alpha = 0.4) +
#   labs(title = "Max EHF in July for All Stations",
#        x = "Year",
#        y = "Max EHF") +
#   theme_minimal()+
#   scale_x_continuous(breaks = seq(1940, 2023, by = 1)) +  # Show more years on the x-axis
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# 
# 
# 
# ## correlation
# july_data_wide <- july_data %>%
#   select(LOCAL_YEAR, Station, max_EHF) %>%
#   pivot_wider(names_from = Station, values_from = max_EHF)
# 
# # Compute the correlation matrix
# ehf_correlation <- cor(july_data_wide %>% select(-LOCAL_YEAR), use = "pairwise.complete.obs")
# 
# # Print the correlation matrix
# print(ehf_correlation)
# #install.packages("corrplot")
# 
# 
# # Plot the correlation matrix
# corrplot(ehf_correlation, method = "number", type = "upper")
# 
# # Create the correlation plot
# corrplot(ehf_correlation, 
#          method = "number", 
#          type = "upper", 
#          #title = "Correlation of Max EHF in August between Stations", 
#          tl.srt = 30,  # Rotate text labels by 45 degrees
#          tl.col = "black")  # Change text color to black for better readability
# # Add title below the plot
# title(main = "Correlation of Max EHF in August between Stations", line = -5)
