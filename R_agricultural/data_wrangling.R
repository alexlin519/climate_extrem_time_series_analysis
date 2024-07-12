# Load necessary packages
library(dplyr)
library(lubridate)
library(tidyr)

aggregate_data <- function(data_x){
  # Add columns for year and week
  data_x <- data_x %>% 
    mutate(
      Year = year(LOCAL_DATE),
      Week = week(LOCAL_DATE)
    )
  
  # Aggregate data by year and week to get the mean for each week
  weekly_data_x <- data_x %>% 
    group_by(Year, Week) %>% 
    summarise(
      week_Mean_Temperature = mean(MEAN_TEMPERATURE, na.rm = TRUE),
      week_Mean_Percentile_95 = mean(Percentile_95, na.rm = TRUE),
      week_Mean_EHF_95 = mean(EHF_95, na.rm = TRUE)
    )
  
  # View the aggregated data
  print(weekly_data_x)
}






merged_data_x <- weekly_data_x %>%
  select(-week_Mean_Percentile_95, -week_Mean_EHF_95) %>%
  pivot_wider(names_from = Week, values_from = week_Mean_Temperature, names_prefix = "Week_")
# drop the row when year ==2024 or year == na
merged_data_x <- merged_data_x %>% 
  select(-Week_NA) %>%
  filter(Year != 2024) %>%
  filter(Year != 1935) %>%
  filter(!is.na(Year))
#check the merged data has na
print(sum(is.na(merged_data_x)))

# Interpolate NA values in the wide data frame
# by taking the average of the previous and next values
merged_data_x[,-1] <- na.approx(merged_data_x[,-1], na.rm = FALSE)

y_data <- data_fao %>%
  filter(Element == "Yield")
# Perform linear regression for each crop
unique_crops <- unique(data_fao$Item)
results <- list()
for (crop in unique_crops) {
  crop_data <- y_data %>% filter(Item == crop)

  # Check if crop_data is not empty
  if (nrow(crop_data) > 0) {

    # Merge the temperature and yield data by Year
    merged_data <- inner_join(crop_data, merged_data_x, by = "Year")

    # Extract response variable y and predictor variables x
    y <- merged_data$Value
    x <- merged_data %>% select(starts_with("Week_"))

    #y <- crop_data$Value
    #x <- merged_data_x %>% select(starts_with("Week_"))

    # Print x to show what it looks like
    print("Predictor matrix x:")
    print(x)

    # Add intercept to the model
    #x <- as.data.frame(model.matrix(~ . - 1, x))

    model <- lm(y ~ ., data = x)
    results[[crop]] <- summary(model)
    
    # Extract coefficients and their standard errors, removing the intercept
    coeffs <- summary(model)$coefficients[-1,]
    coeffs_df <- as.data.frame(coeffs)
    coeffs_df$Week <- as.numeric(gsub("Week_", "", rownames(coeffs_df)))
    coeffs_df <- coeffs_df[order(coeffs_df$Week), ]
    
    # Create the plot
    p <- ggplot(coeffs_df, aes(x = Week, y = Estimate)) +
      geom_line(color = "blue") +
      geom_ribbon(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), alpha = 0.2) +
      labs(title = paste("Coefficient by Week for:", crop),
           x = "Week",
           y = "Coefficient Estimate") +
      theme_minimal()+
      heme(panel.grid.minor.y = element_blank(),
           panel.grid.minor.x = element_blank()) 
    
    print(p)
    
  } else {
    results[[crop]] <- "No data available for this crop."
  }
}

# Print results for each crop
for (crop in unique_crops) {
  print(paste("Results for crop:", crop))
  print(results[[crop]])
}








