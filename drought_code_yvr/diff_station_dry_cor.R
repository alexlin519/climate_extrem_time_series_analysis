# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr)
setwd("/Users/alexlin/summer_stat/climate_extreme_RA/R")


# Define a function to create the plots
plot_consecutive_dry_days <- function(data_path, start_year, end_year, step) {
  data <- read_csv(data_path)
  periods <- seq(start_year, end_year, by = step)
  data <- data %>%
    mutate(Year = as.numeric(substr(yrmon, 1, 4)),
           Month = as.numeric(substr(yrmon, 5, 6)))
  for (i in 1:(length(periods) - 1)) {
    start <- periods[i]
    end <- min(periods[i + 1] - 1, end_year)
    
    data_period <- data %>%
      filter(Year >= start & Year <= end)
    
    plot <- ggplot(data_period, aes(x = as.Date(paste0(substr(yrmon, 1, 4), "-", substr(yrmon, 5, 6), "-01")), y = max_consec)) +
      geom_line() +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 month") +  # change to ensure proper spacing and labeling d
      labs(title = paste("Maximum Consecutive Dry Days by Month Over Time", start, "-", end),
           x = "Date",
           y = "Max Consecutive Dry Days") +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))  # rotate x-axis labels
    print(plot)
  }
  
  # Handle the remaining years after the last full step
  last_start <- periods[length(periods)]
  if (last_start <= end_year) {
    data_period <- data %>%
      filter(Year >= last_start & Year <= end_year)
    
    plot <- ggplot(data_period, aes(x = as.Date(paste0(substr(yrmon, 1, 4), "-", substr(yrmon, 5, 6), "-01")), y = max_consec)) +
      geom_line() +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 month") +  # change to ensure proper spacing and labeling
      labs(title = paste("Maximum Consecutive Dry Days by Month Over Time", last_start, "-", end_year),
           x = "Date",
           y = "Max Consecutive Dry Days") +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  # rotate x-axis labels
    
    print(plot)
  }
  
  
  
  # Box plot to compare avgexc across different years
  box <- ggplot(data, aes(x = factor(Year), y = max_consec)) +
    geom_boxplot() +
    labs(title = "Box Plot of Maximum Consecutive Dry Days by Year",
         x = "Year",
         y = "Max Consecutive Dry Days(max_consec)") +
    theme_minimal()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(box)
}

# 
# # Load the data
# file_path <- "../drought_code_yvr/data/YVRmax-consec-drydays-bymonth.csv"
# 
# data <- read_csv(file_path)
# # Summarize the data
# head(data)
# summary(data)
# 
# # Separate Year and Month from yrmon
# data <- data %>%
#   mutate(Year = as.numeric(substr(yrmon, 1, 4)),
#          Month = as.numeric(substr(yrmon, 5, 6)))%>%
#   filter(Year >= 1940 & Year <= 1980) 
# 
# 
# # Plot the maximum consecutive dry days for each month over time
# ggplot(data, aes(x = yrmon, y = max_consec)) +
#   geom_line() +
#   labs(title = "Maximum Consecutive Dry Days by Month Over Time",
#        x = "Year",
#        y = "Max Consecutive Dry Days") +
#   theme_minimal()
# 
# 
# 
# # Assuming 'max_consec' is the variable representing maximum consecutive dry days for each month
# ts_data <- ts(data$max_consec, start = c(1941, 1), frequency = 12)
# 
# # Plot the time series
# plot(ts_data, xlab = "Year", ylab = "Maximum Consecutive Dry Days",
#      main = "Maximum Consecutive Dry Days for Each Month Over Time")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# data_kelowna <- read_csv(consec_precip_bymonth_path_Kelowna)
# head(data_kelowna)
# 
# 
# 
# # Sample data preparation (assuming data_kelowna is already available)
# # data_kelowna <- ... (your data loading code)
# 
# 
# 
# plot_consecutive_dry_days(data_kelowna, start_year = 1940, end_year = 2024, step = 10)
# 
# # Function to adjust the 'max_consec' values
# adjust_max_consec <- function(value) {
#   while (value > 30) {
#     value <- value - 30
#   }
#   return(value)
# }
# # Apply the function to the 'max_consec' column
# data_kelowna$max_consec <- sapply(data_kelowna$max_consec, adjust_max_consec)
# 
# plot_consecutive_dry_days(data_kelowna, start_year = 1940, end_year = 2024, step = 10)
# 
# 
# # Assuming 'max_consec' is the variable representing maximum consecutive dry days for each month
# ts_data_kelowna <- ts(data_kelowna$max_consec, start = c(1941, 1), frequency = 12)
# 
# # Plot the time series
# plot(ts_data_kelowna, xlab = "Year", ylab = "Maximum Consecutive Dry Days",
#      main = "Maximum Consecutive Dry Days for Each Month Over Time")
# 
# 
# # Calculate the cross-correlation function
# ccf_result <- ccf(ts_data, ts_data_kelowna,plot = FALSE)#, lag.max = 12, )
# 
# # Plot the cross-correlation function
# plot(ccf_result, main="Cross-Correlation Function")

plot_ccf_max_consec <- function(data_1_path,data1_start_year,station1, 
                                data_2_path,data2_start_year,station2,
                                lag_max = 12) {
  # Load the data
  data_1 <- read_csv(data_1_path)
  data_2 <- read_csv(data_2_path)
  # Create the time series object for Kelowna
  ts_data_1 <- ts(data_1$max_consec, start = c(data1_start_year, 1), frequency = 12)
  ts_data_2 <- ts(data_2$max_consec, start = c(data2_start_year, 1), frequency = 12)
  
  # Plot the time series
  #plot(ts_data_kelowna, xlab = "Year", ylab = "Maximum Consecutive Dry Days",
  #     main = "Maximum Consecutive Dry Days for Each Month Over Time")
  
  # Calculate the cross-correlation function
  ccf_result <- ccf(ts_data_1, ts_data_2, plot = FALSE)
  #print(summary(ccf_result))
  # Plot the cross-correlation function
  plot(ccf_result, main = paste("Cross-Correlation Function between", station1, "and", station2))
  
  
  #Identify the lag with the highest correlation
  max_ccf_lag <- ccf_result$lag[which.max(abs(ccf_result$acf))]
  max_ccf_value <- ccf_result$acf[which.max(abs(ccf_result$acf))]
  
  cat("Maximum cross-correlation is", max_ccf_value, "at lag", max_ccf_lag, "\n")
  
  # Identify the lag with the minimum correlation
  min_ccf_lag <- ccf_result$lag[which.min(abs(ccf_result$acf))]
  min_ccf_value <- ccf_result$acf[which.min(abs(ccf_result$acf))]
  
  cat("Minimum cross-correlation is", min_ccf_value, "at lag", min_ccf_lag, "\n")
  
  # # Identify significant lags
  # significant_lags <- ccf_result$lag[which(abs(ccf_result$acf) > significance_threshold)]
  # significant_values <- ccf_result$acf[which(abs(ccf_result$acf) > significance_threshold)]
  # 
  # if (length(significant_lags) > 0) {
  #   cat("Significant correlations (|correlation| >", significance_threshold, ") occur at lags:\n")
  #   for (i in 1:length(significant_lags)) {
  #     cat("Lag:", significant_lags[i], "Correlation:", significant_values[i], "\n")
  #   }
  # } else {
  #   cat("No significant correlations found with the given threshold:",significance_threshold, "\n")
  # }
}


# 
# plot_scatter <- function(data_1_path,data1_start_year,station1, 
#                                 data_2_path,data2_start_year,station2) {
#   # Load the data
#   data_1 <- read_csv(data_1_path)
#   data_2 <- read_csv(data_2_path)
#   # Create the time series object for Kelowna
#   ts_data_1 <- ts(data_1$max_consec, start = c(data1_start_year, 1), frequency = 12)
#   ts_data_2 <- ts(data_2$max_consec, start = c(data2_start_year, 1), frequency = 12)
#   #scatter plot
#   plot(data_1$max_consec,data_2$max_consec,
#        xlab = paste("Max consec dry days in",station1),
#        ylab = paste("Max consec dry days in",station2),
#        main = paste("Scatter plot of max consec dry days between",station1,"and",station2))
# }