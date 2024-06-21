# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr)
setwd("/Users/alexlin/summer_stat/climate_extreme_RA/R")
# Load the data
file_path <- "../drought_code_yvr/data/YVRmax-consec-drydays-bymonth.csv"

data <- read_csv(file_path)
# Summarize the data
head(data)
summary(data)

# Separate Year and Month from yrmon
data <- data %>%
  mutate(Year = as.numeric(substr(yrmon, 1, 4)),
         Month = as.numeric(substr(yrmon, 5, 6)))%>%
  filter(Year >= 1940 & Year <= 1980) 


# Plot the maximum consecutive dry days for each month over time
ggplot(data, aes(x = yrmon, y = max_consec)) +
  geom_line() +
  labs(title = "Maximum Consecutive Dry Days by Month Over Time",
       x = "Year",
       y = "Max Consecutive Dry Days") +
  theme_minimal()



# Assuming 'max_consec' is the variable representing maximum consecutive dry days for each month
ts_data <- ts(data$max_consec, start = c(1941, 1), frequency = 12)

# Plot the time series
plot(ts_data, xlab = "Year", ylab = "Maximum Consecutive Dry Days",
     main = "Maximum Consecutive Dry Days for Each Month Over Time")













data_kelowna <- read_csv(consec_precip_bymonth_path_Kelowna)
head(data_kelowna)



# Sample data preparation (assuming data_kelowna is already available)
# data_kelowna <- ... (your data loading code)


# Define a function to create the plots
plot_consecutive_dry_days <- function(data, start_year, end_year, step) {
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
}

plot_consecutive_dry_days(data_kelowna, start_year = 1940, end_year = 2024, step = 10)

# Function to adjust the 'max_consec' values
adjust_max_consec <- function(value) {
  while (value > 30) {
    value <- value - 30
  }
  return(value)
}
# Apply the function to the 'max_consec' column
data_kelowna$max_consec <- sapply(data_kelowna$max_consec, adjust_max_consec)

plot_consecutive_dry_days(data_kelowna, start_year = 1940, end_year = 2024, step = 10)


# Assuming 'max_consec' is the variable representing maximum consecutive dry days for each month
ts_data_kelowna <- ts(data_kelowna$max_consec, start = c(1941, 1), frequency = 12)

# Plot the time series
plot(ts_data_kelowna, xlab = "Year", ylab = "Maximum Consecutive Dry Days",
     main = "Maximum Consecutive Dry Days for Each Month Over Time")


# Calculate the cross-correlation function
ccf_result <- ccf(ts_data, ts_data_kelowna,plot = FALSE)#, lag.max = 12, )

# Plot the cross-correlation function
plot(ccf_result, main="Cross-Correlation Function")


