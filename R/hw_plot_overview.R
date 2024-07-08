# helpers.R
library(patchwork)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)

# Function to process and summarize data for a given year
process_and_summarize_year <- function(year) {
  df_plot_year <- prepare_plot_data("Percentile_90", year, "MAX_TEMPERATURE")
  
  summaries <- process_data_by_year(process_data_for_hw(df_plot_year), year)
  
  case_summary <- summaries$case_summary %>%
    mutate(Year = year)
  
  return(case_summary)
}

# df_plot_year <- prepare_plot_data("Percentile_90", year, "MAX_TEMPERATURE")
# df_plot_year # it is only one year
# test1 <- process_data_for_hw(df_plot_year)
# test1
# case_summary


# Function to generate the plot
generate_plot <- function(data, title) {
  plot <- ggplot(data, aes(x = Year, y = Count, fill = as.factor(Case_Length))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = title,
         x = "Year",
         y = "Total Count of HeatWave",
         fill = "HeatWave Last Days") +
    scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 10)) +
    theme_minimal()
  return(plot)
}

# Function to process all years and generate plots
process_all_years_and_generate_plots <- function(start_year, end_year) {
  case_summaries_all_years_count <- data.frame()
  
  for (year in start_year:end_year) {
    case_summary <- process_and_summarize_year(year)
    case_summaries_all_years_count <- bind_rows(case_summaries_all_years_count, case_summary)
  }
  
  #total_plot <- generate_plot(case_summaries_all_years_count, 
   #                           "Total Sum of HW Counts by Year")
  filtered_case_summaries_count <- case_summaries_all_years_count %>%
    filter(Case_Length >= 3)
  filtered_plot <- generate_plot(filtered_case_summaries_count, 
                                 "Total Sum of HW by Year (HW last >= 3 days)")
  print(filtered_plot)
  #return(list(total_plot = total_plot, filtered_plot = filtered_plot))
  return( filtered_case_summaries_count)
}



# Function to plot EHF and 90th Percentile Data
compare_ehf_and_percentile_specific <- function(data_EHF, check_year) {
  #old def data
  df_plot_year <- prepare_plot_data("Percentile_90", check_year, "MAX_TEMPERATURE")
  
  #filter  month column
  df_plot_year <- df_plot_year %>%
    filter(as.numeric(Month) >= 4 & as.numeric(Month) <= 10)
  
  #print(df_plot_year)
  # Create a grouping variable for continuous date ranges
  # data_EHF <- data_EHF %>% 
  #   mutate(date = as.Date(paste(LOCAL_YEAR, DayOfYear, sep = "-"), "%Y-%m-%d")) %>%
  #   arrange(date) %>%
  #   mutate(group = cumsum(c(0, diff(date) != 1)))
  
  #ehf only for specific year
  data_EHF <- data_EHF %>%
    filter(LOCAL_YEAR == check_year) %>%
    filter(as.numeric(Month) >= 4 & as.numeric(Month) <= 10)
  
  
  df_plot_year <- df_plot_year %>%
    mutate(date = as.Date(paste(check_year, Month,Day, sep = "-"), "%Y-%m-%d")) %>%
    arrange(date) %>%
    mutate(group = cumsum(c(0, diff(date) != 1)))
  
  # Plot for stations with EHF data (EHI_sig, EHI_accl, EHF)
  left_plot <- ggplot(data_EHF, aes(x = LOCAL_DATE)) +
    geom_line(aes(y = EHI_sig, color = "EHI_sig")) +
    geom_line(aes(y = EHI_accl, color = "EHI_accl")) +
    # add a y = 1 line for EHF
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    geom_line(aes(y = EHF, color = "EHF")) +
    labs(title = paste("EHF Data for", check_station, "in", check_year),
         x = "Date",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_date(date_breaks = "10 days", date_labels = "%b %d") +
    scale_color_manual(values = c("EHI_sig" = "blue", "EHI_accl" = "green", "EHF" = "red"))+
    scale_y_continuous(breaks = seq(min(c(data_EHF$EHI_accl, data_EHF$EHF,data_EHF$EHI_sig), na.rm = TRUE),
                                    max(c(data_EHF$EHI_accl, data_EHF$EHF,data_EHF$EHI_sig), na.rm = TRUE),
                                    by =5)) + # Adjust the step size as needed
    theme(panel.grid.minor.y = element_blank(),  # Remove minor grid lines
          legend.position = c(0.999, 0.999),       # Move legend to the top right corner inside the plot
          legend.justification = c("right", "top"))  # Justify legend position to the right and top
  
  
  # Plot for stations with 90th percentile data (daily_max_temp, Percentile_90)
  right_plot <- ggplot(df_plot_year, aes(x = date)) +
    geom_line(aes(y = MAX_TEMP_YEAR, color = "daily_max_temp")) +
    geom_line(aes(y = Percentile_90, color = "Percentile_90")) +
    labs(title = paste("Max Temp and 90th Percentile for", check_station, "in", check_year),
         x = "Date",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_manual(values = c("daily_max_temp" = "blue", "Percentile_90" = "green"))+
    # shows all x value axis in terms of date
    scale_x_date(date_breaks = "10 days", date_labels = "%b %d") +
    scale_y_continuous(breaks = seq(min(c(df_plot_year$MAX_TEMP_YEAR, df_plot_year$Percentile_90), na.rm = TRUE),
                                    max(c(df_plot_year$MAX_TEMP_YEAR, df_plot_year$Percentile_90), na.rm = TRUE),
                                    by = 2))+  # Adjust the step size as needed
    theme(panel.grid.minor.y = element_blank(),  # Remove minor grid lines
          legend.position = c(0.999, 0.999),       # Move legend to the top right corner inside the plot
          legend.justification = c("right", "top"))  # Justify legend position to the right and top
  # Set the plot dimensions
  options(repr.plot.width = 14, repr.plot.height = 7)  # Adjust the width and height as needed
  
  # Assuming left_plot and right_plot are your ggplot objects
  combined_plot <- grid.arrange(left_plot, right_plot, ncol = 2)
  
  # Return the combined plot
  return(combined_plot)
}


# # Function to plot EHF and 90th Percentile Data
# compare_ehf_and_percentile_specific <- function(data, check_year) {
#   # Filter the data for the specified year
#   data_year <- data %>% filter(Year == check_year)
#   
#   # Separate data for stations with EHF and 90th percentile columns
#   data_ehf <- data_year %>% filter(station == paste(check_station, "_EHF", sep = ""))
#   data_90percentile <- data_year %>% filter(station == check_station)
#   
#   
#   # Plot for stations with EHF data (EHI_sig, EHI_accl, EHF)
#   left_plot <- ggplot(data_ehf, aes(x = date)) +
#     geom_line(aes(y = EHI_sig, color = "EHI_sig")) +
#     geom_line(aes(y = EHI_accl, color = "EHI_accl")) +
#     # add a y = 1 line for EHF
#     geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
#     geom_line(aes(y = EHF, color = "EHF")) +
#     labs(title = paste("EHF Data for", check_station, "in", check_year),
#          x = "Date",
#          y = "Value") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#     scale_color_manual(values = c("EHI_sig" = "blue", "EHI_accl" = "green", "EHF" = "red"))
#   
#   # Plot for stations with 90th percentile data (daily_max_temp, Percentile_90)
#   right_plot <- ggplot(data_90percentile, aes(x = date)) +
#     geom_line(aes(y = daily_max_temp, color = "daily_max_temp")) +
#     geom_line(aes(y = Percentile_90, color = "Percentile_90")) +
#     labs(title = paste("Max Temp and 90th Percentile for", check_station, "in", check_year),
#          x = "Date",
#          y = "Value") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#     scale_color_manual(values = c("daily_max_temp" = "blue", "Percentile_90" = "green"))+
#     scale_y_continuous(breaks = seq(min(c(data_90percentile$daily_max_temp, data_90percentile$Percentile_90), na.rm = TRUE),
#                                     max(c(data_90percentile$daily_max_temp, data_90percentile$Percentile_90), na.rm = TRUE),
#                                     by = 1))  # Adjust the step size as needed
#   # Set the plot dimensions
#   options(repr.plot.width = 14, repr.plot.height = 7)  # Adjust the width and height as needed
#   
#   # Combine the plots side by side
#   combined_plot <- left_plot + right_plot
#   
#   # Return the combined plot
#   return(combined_plot)
# }