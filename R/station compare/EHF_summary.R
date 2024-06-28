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
print(head(df_ehf_all))


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
  ehf_correlation <- cor(wide_data %>% select(-LOCAL_YEAR), use = "pairwise.complete.obs")
  
  # Print the correlation matrix
  print(ehf_correlation)
  
  # Plot the correlation matrix
  corre_plot <- corrplot(ehf_correlation, method = "number", type = "upper",
           title = paste("Correlation of Max EHF in", month_name, "between Stations"))
  return((ts_plot))
  }

# Example usage for July (Month = 7)
ts_plot <- plot_ehf_and_correlation(df_ehf_all, month = 8)
ts_plot











library(ggplot2)
library(dplyr)

# Assuming your data frame is named df_wrangling
# Filter for the month of July (Month == 7)
july_data <- df_ehf_all %>% filter(Month == 7 & LOCAL_YEAR >= 1940)

# Summarize data (optional, depending on what summary you need)
# For example, calculating the mean max_EHF for each station in July
summary_data <- july_data %>%
  group_by(Station) %>%
  summarize(mean_max_EHF = mean(max_EHF, na.rm = TRUE))


# Plot data
ggplot(july_data, aes(x = LOCAL_YEAR, y = max_EHF, color = Station)) +
  geom_line(alpha = 1) +
  geom_point(alpha = 0.4) +
  labs(title = "Max EHF in July for All Stations",
       x = "Year",
       y = "Max EHF") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(1940, 2023, by = 1)) +  # Show more years on the x-axis
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




## correlation
july_data_wide <- july_data %>%
  select(LOCAL_YEAR, Station, max_EHF) %>%
  pivot_wider(names_from = Station, values_from = max_EHF)

# Compute the correlation matrix
ehf_correlation <- cor(july_data_wide %>% select(-LOCAL_YEAR), use = "pairwise.complete.obs")

# Print the correlation matrix
print(ehf_correlation)
#install.packages("corrplot")


# Plot the correlation matrix
corrplot(ehf_correlation, method = "number", type = "upper")

# Create the correlation plot
corrplot(ehf_correlation, 
         method = "number", 
         type = "upper", 
         #title = "Correlation of Max EHF in August between Stations", 
         tl.srt = 30,  # Rotate text labels by 45 degrees
         tl.col = "black")  # Change text color to black for better readability
# Add title below the plot
title(main = "Correlation of Max EHF in August between Stations", line = -5)
