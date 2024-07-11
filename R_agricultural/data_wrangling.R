# Load necessary packages
library(dplyr)
library(lubridate)


# Add columns for year and week
data_x <- data_x %>% 
  mutate(
    Year = year(LOCAL_DATE),
    Week = week(LOCAL_DATE)
  )

# Aggregate data by year and week to get the mean for each week
weekly_data <- data_x %>% 
  group_by(Year, Week) %>% 
  summarise(
    week_Mean_Temperature = mean(MEAN_TEMPERATURE, na.rm = TRUE),
    week_Mean_Percentile_95 = mean(Percentile_95, na.rm = TRUE),
    week_Mean_EHF_95 = mean(EHF_95, na.rm = TRUE)
  )

# View the aggregated data
print(weekly_data)
