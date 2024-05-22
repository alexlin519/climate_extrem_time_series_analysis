# helpers.R

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
  
  #return(list(total_plot = total_plot, filtered_plot = filtered_plot))
  return( filtered_plot)
}
