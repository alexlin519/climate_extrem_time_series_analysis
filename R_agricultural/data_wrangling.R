# Load necessary packages
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(ggplot2)
library(scales)

aggregate_data_week <- function(data_x){
  # Add columns for year and week
  #print("Adding columns for year and week")
  data_x <- data_x %>% 
    mutate(
      Year = year(LOCAL_DATE),
      Week = week(LOCAL_DATE)
    )
  
  # Aggregate data by year and week to get the mean for each week
  weekly_data_x <- data_x %>% 
    group_by(Year, Week,station) %>% 
    summarise(
      week_Mean_Temperature = mean(MEAN_TEMPERATURE, na.rm = TRUE),
      week_Mean_Percentile_95 = mean(Percentile_95, na.rm = TRUE),
      week_Mean_EHF_95 = mean(EHF_95, na.rm = TRUE)
    )
  
  # View the aggregated data
  #print(weekly_data_x)
}

# Function to handle NA values by averaging previous and next year's values
handle_na <- function(data) {
  cols <- grep("^Week_", names(data))
  
  for (col in cols) {
    for (i in 1:nrow(data)) {
      if (is.na(data[i, col])) {
        if (i > 1 && i < nrow(data)) {
          prev_value <- data[i - 1, col]
          next_value <- data[i + 1, col]
          data[i, col] <- mean(c(prev_value, next_value), na.rm = TRUE)
        }
      }
    }
  }
  
  return(data)
}


aggregate_data_month <- function(data_x,agg_method){
  # Define the aggregation function based on the provided method
  agg_func <- match.fun(agg_method)
  
  # Add columns for year and month
  #print("Adding columns for year and month")
  data_x <- data_x %>% 
    mutate(
      Year = year(LOCAL_DATE),
      Month = month(LOCAL_DATE)
    )
  
  # Aggregate data by year and month to get the mean for each month
  monthly_data_x <- data_x %>% 
    group_by(Year, Month,station) %>% 
    summarise(
      maxmon_Mean_Temp = agg_func(MEAN_TEMPERATURE, na.rm = TRUE),
      maxmon_Percentile_95 = agg_func(Percentile_95, na.rm = TRUE),
      maxmon_EHF_95 = agg_func(EHF_95, na.rm = TRUE)
    )
  
  # View the aggregated data
  #print(monthly_data_x)
}

aggregate_data_season <- function(data_x,agg_method ) {
  # Define the aggregation function based on the provided method
  agg_func <- match.fun(agg_method)
  
  # Add columns for year, month, and season
  #print("Adding columns for year, month, and season")
  data_x <- data_x %>% 
    mutate(
      Year = year(LOCAL_DATE),
      Month = month(LOCAL_DATE),
      Season = case_when(
        Month %in% c(12, 1, 2) ~ "Winter",
        Month %in% c(3, 4, 5) ~ "Spring",
        Month %in% c(6, 7, 8) ~ "Summer",
        Month %in% c(9, 10, 11) ~ "Fall"
      )
    )
  
  # Aggregate data by year, season, and station to get the mean for each season
  seasonal_data_x <- data_x %>% 
    group_by(Year, Season, station) %>% 
    summarise(
      maxsea_Mean_Temp = agg_func(MEAN_TEMPERATURE, na.rm = TRUE),
      maxsea_Percentile_95 = agg_func(Percentile_95, na.rm = TRUE),
      maxsea_EHF_95 = agg_func(EHF_95, na.rm = TRUE)
    )
  
  # View the aggregated data
  # print(seasonal_data_x)
}


perform_linear_regression_weekly <- function(weekly_data_x, data_fao,certain_station,predictor_variable) {
  
  # Check if the predictor_variable is valid
  if (!predictor_variable %in% c("week_Mean_Percentile_95", "week_Mean_EHF_95", "week_Mean_Temperature")) {
    stop("Invalid predictor_variable. Choose either 'week_Mean_Percentile_95', 'week_Mean_EHF_95', or 'week_Mean_Temperature'.")
  }
  
  columns_to_keep <- setdiff(c("week_Mean_Percentile_95", "week_Mean_EHF_95", "week_Mean_Temperature"), predictor_variable)
  
  # Process weekly data
  merged_data_x <- weekly_data_x %>%
    filter(station ==  certain_station) %>%
    select(-all_of(columns_to_keep)) %>%
    pivot_wider(names_from = Week, values_from = !!sym(predictor_variable), names_prefix = "Week_") %>%
    filter(Year != 2024) %>%
    filter(!is.na(Year)) # drop row where year column is na
  
  
  # if (station == "Abbotsford") {
  #   # Process weekly data
  #   merged_data_x <- merged_data_x %>%
  #     filter(Year != 2024) %>%
  #     filter(Year != 1935) 
  # }
  # 
  # if (station == "Kelowna") {
  #   # Process weekly data
  #   merged_data_x <- merged_data_x %>%
  #     filter(Year != 2024) %>%
  #     filter(Year != 1899) 
  # }
  
  na_positions <- which(is.na(merged_data_x), arr.ind = TRUE)
  # Check for NAs in the merged data
  if (sum(is.na(merged_data_x)) > 0) {
    for (i in 1:nrow(na_positions)) {
      row <- na_positions[i, 1]
      col <- na_positions[i, 2]
      print(paste("NA value found at row", row, "and column", col))
    }
    num_of_na <- sum(is.na(merged_data_x))
    print(paste("There are",num_of_na," NA in the matrix X in",certain_station,"station"))
    # Interpolate NA values in the wide data frame using custom function
    merged_data_x <- handle_na(merged_data_x)
  }

  # Filter yield data
  y_data <- data_fao %>%
    filter(Element == "Yield")
  
  # Get unique crops
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
      
      # Print x to show what it looks like
      # print(paste("Predictor matrix x for crop:", crop))
      # print(x)
      
      # Add intercept to the model
      model <- lm(y ~ ., data = x)
      results[[crop]] <- list(summary = summary(model))
      
      # Extract coefficients and their standard errors, removing the intercept
      coeffs <- summary(model)$coefficients[-1,]
      coeffs_df <- as.data.frame(coeffs)
      coeffs_df$Week <- as.numeric(gsub("Week_", "", rownames(coeffs_df)))
      coeffs_df <- coeffs_df[order(coeffs_df$Week), ]
      
      # Create the plot
      p <- ggplot(coeffs_df, aes(x = Week, y = Estimate)) +
        geom_line(color = "blue") +
        geom_ribbon(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), alpha = 0.2) +
        labs(title = paste("Coefficient by Week for:", crop, "in", certain_station),
             x = "Week",
             y = "Coefficient Estimate") +
        #shows more x and y values
        scale_x_continuous(breaks = seq(min(coeffs_df$Week), max(coeffs_df$Week), by = 2))+
        scale_y_continuous(breaks = pretty_breaks(n = 16))+ # Specify the number of breaks
        theme_minimal() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())
      
      results[[crop]]$plot <- p
      
    } else {
      results[[crop]] <- list(summary = "No data available for this crop.", plot = NULL)
    }
  }
  
  return(results)
}


perform_linear_regression_monthly <- function(monthly_data_x, data_fao,certain_station,predictor_variable) {
  # Check if the predictor_variable is valid
  if (!predictor_variable %in% c("maxmon_Mean_Temp", "maxmon_Percentile_95", "maxmon_EHF_95")) {
    stop("Invalid predictor_variable. Choose either 'maxmon_Mean_Temp', 'maxmon_Percentile_95', or 'maxmon_EHF_95'.")
  }
  
  columns_to_keep <- setdiff(c("maxmon_Mean_Temp", "maxmon_Percentile_95", "maxmon_EHF_95"), predictor_variable)
  
  # Process weekly data
  merged_data_x <- monthly_data_x %>%
    filter(station ==  certain_station) %>%
    select(-all_of(columns_to_keep)) %>%
    pivot_wider(names_from = Month, values_from = !!sym(predictor_variable), names_prefix = "Month_")
  
  
  # if (station == "Abbotsford") {
  #   # Process weekly data
  #   merged_data_x <- merged_data_x %>%
  #     filter(Year != 2024) %>%
  #     filter(Year != 1935) 
  # }

  #Check for NAs in the merged data
  if (sum(is.na(merged_data_x)) > 0) {
    num_of_na <- sum(is.na(merged_data_x))
    print(paste("There are",num_of_na," NA in the matrix X in",certain_station,"station"))
    # Interpolate NA values in the wide data frame
    merged_data_x <- handle_na(merged_data_x)
  }

  # Filter yield data
  y_data <- data_fao %>%
    filter(Element == "Yield")
  
  # Get unique crops
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
      x <- merged_data %>% select(starts_with("Month_"))
      
      # Print x to show what it looks like
      # print(paste("Predictor matrix x for crop:", crop))
      # print(x)
      
      # Add intercept to the model
      model <- lm(y ~ ., data = x)
      results[[crop]] <- list(summary = summary(model))
      
      # Extract coefficients and their standard errors, removing the intercept
      coeffs <- summary(model)$coefficients[-1,]
      coeffs_df <- as.data.frame(coeffs)
      coeffs_df$Month <- as.numeric(gsub("Month_", "", rownames(coeffs_df)))
      coeffs_df <- coeffs_df[order(coeffs_df$Month), ]
      
      # Create the plot
      p <- ggplot(coeffs_df, aes(x = Month, y = Estimate)) +
        geom_line(color = "blue") +
        geom_ribbon(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), alpha = 0.2) +
        labs(title = paste("Coefficient by Month for:", crop, "in", certain_station),
             x = "Month",
             y = "Coefficient Estimate") +
        #shows more x and y values
        scale_x_continuous(breaks = seq(min(coeffs_df$Month), max(coeffs_df$Month), by = 2))+
        scale_y_continuous(breaks = pretty_breaks(n = 16))+ # Specify the number of breaks
        theme_minimal() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())
      
      results[[crop]]$plot <- p
      
    } else {
      results[[crop]] <- list(summary = "No data available for this crop.", plot = NULL)
    }
  }
  
  return(results)
}



perform_lm_monthly_statcan <- function(monthly_data_x, crop_yield,certain_station,predictor_variable,region) {
  # Check if the predictor_variable is valid
  if (!predictor_variable %in% c("maxmon_Mean_Temp", "maxmon_Percentile_95", "maxmon_EHF_95")) {
    stop("Invalid predictor_variable. Choose either 'maxmon_Mean_Temp', 'maxmon_Percentile_95', or 'maxmon_EHF_95'.")
  }
  
  columns_to_keep <- setdiff(c("maxmon_Mean_Temp", "maxmon_Percentile_95", "maxmon_EHF_95"), predictor_variable)
  
  # Process weekly data
  merged_data_x <- monthly_data_x %>%
    filter(station ==  certain_station) %>%
    select(-all_of(columns_to_keep)) %>%
    pivot_wider(names_from = Month, values_from = !!sym(predictor_variable), names_prefix = "Month_")
  
  
  
  #Check for NAs in the merged data
  if (sum(is.na(merged_data_x)) > 0) {
    num_of_na <- sum(is.na(merged_data_x))
    print(paste("There are",num_of_na," NA in the matrix X in",certain_station,"station"))
    # Interpolate NA values in the wide data frame
    merged_data_x <- handle_na(merged_data_x)
  }
  
  # Filter yield data
  y_data <- crop_yield %>% 
    #RENAME date column to Year
    rename(Year = REF_DATE) %>%
    filter(GEO == region)
  # Get unique crops
  
    
    unique_crops <- unique(crop_yield$Crop_Type)
  results <- list()
  
  for (crop in unique_crops) {
    crop_data <- y_data %>% filter(Crop_Type == crop)
    
    # Check if crop_data is not empty
    if (nrow(crop_data) > 0) {
      # Merge the temperature and yield data by Year
      merged_data <- inner_join(crop_data, merged_data_x, by = "Year")
      
      # Extract response variable y and predictor variables x
      y <- merged_data$yield
      x <- merged_data %>% ungroup %>% select(starts_with("Month_"))
      
      # Print x to show what it looks like
      # print(paste("Predictor matrix x for crop:", crop))
      # print(x)
      
      # Add intercept to the model
      model <- lm(y ~ ., data = x)
      results[[crop]] <- list(summary = summary(model))
      
      # Extract coefficients and their standard errors, removing the intercept
      coeffs <- summary(model)$coefficients[-1,]
      coeffs_df <- as.data.frame(coeffs)
      coeffs_df$Month <- as.numeric(gsub("Month_", "", rownames(coeffs_df)))
      coeffs_df <- coeffs_df[order(coeffs_df$Month), ]
      
      # Create the plot
      p <- ggplot(coeffs_df, aes(x = Month, y = Estimate)) +
        geom_line(color = "blue") +
        geom_ribbon(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), alpha = 0.2) +
        labs(title = paste("Coefficient by Month for:", crop, "in", certain_station),
             x = "Month",
             y = "Coefficient Estimate") +
        #shows more x and y values
        scale_x_continuous(breaks = seq(min(coeffs_df$Month), max(coeffs_df$Month), by = 2))+
        scale_y_continuous(breaks = pretty_breaks(n = 16))+ # Specify the number of breaks
        theme_minimal() +
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank())
      
      results[[crop]]$plot <- p
      
    } else {
      results[[crop]] <- list(summary = "No data available for this crop.", plot = NULL)
    }
  }
  
  return(results)
}







##--original code----
# #filter for tomato
# backup_yield <- backup_yield %>%
#   filter(Crop_Type == "tomatoes")
# 
# data_1940_spec <- data_veg_1940 %>%
#   filter(Crop_Type == "tomatoes")
# 
# library(dplyr)
# 
# # Extract unique years and "Average yield per acre (pounds)" data
# existing_yield_data <- data_1940_spec %>%
#   filter(Estimates == "Average yield per acre (pounds)")
# 
# # Extract years from the data
# all_years <- unique(data_1940_spec$REF_DATE)
# 
# # Extract years where "Average yield per acre (pounds)" data exists
# years_with_yield <- unique(existing_yield_data$REF_DATE)
# 
# # Identify missing years
# missing_years <- setdiff(all_years, years_with_yield)
# 
# # Create a dataframe with missing rows
# missing_data <- data.frame(
#   REF_DATE = missing_years,
#   Crop_Type = "tomatoes",
#   Estimates = "Average yield per acre (pounds)",
#   VALUE = NA,
#   UOM = "Pounds"
# )
# 
# # Combine existing yield data with the missing data
# complete_data1940 <- bind_rows(existing_yield_data, missing_data)
# 
# 
# #merge yield column, allows for missing area data
# mergedveg <- merge(complete_data1940, backup_yield, by = c("REF_DATE", "Crop_Type"), all.x = TRUE, suffixes = c("_Production", "_Area")) # change to ensure missing area data results in NA
# 
# #plot two lines from 2 column
# ggplot(mergedveg, aes(x = REF_DATE)) +
#   geom_line(aes(y = VALUE), color = "blue") +
#   geom_line(aes(y = yield_tp_ah_acre_pounds), color = "red") +
#   labs(title = "Yield of tomatoes over Time",
#        x = "Year",
#        y = "Yield",
#        color = "Estimates") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(min(mergedveg$REF_DATE), max(mergedveg$REF_DATE), by = 1))



## --- function to process data and create long format data frame -----


# Function to filter data and handle missing values
handle_missing_yield <- function(data, crop_type) {
  existing_yield_data <- data %>%
    filter(Estimates == "Average yield per acre (pounds)") %>%
    filter(Crop_Type == crop_type)
  
  all_years <- unique(data$REF_DATE)
  years_with_yield <- unique(existing_yield_data$REF_DATE)
  missing_years <- setdiff(all_years, years_with_yield)
  
  missing_data <- data.frame(
    REF_DATE = missing_years,
    Crop_Type = crop_type,
    Estimates = "Average yield per acre (pounds)",
    VALUE = NA,
    UOM = "Pounds"
  )
  
  complete_data <- bind_rows(existing_yield_data, missing_data)
  return(complete_data)
}

# Function to calculate yield
calculate_yield <- function(production, area, multiplier) {
  ifelse(is.na(production) | is.na(area), NA, production * multiplier / area)
}

# Function to prepare backup yield data
#  if yield is na, then using estimates of  "Marketed production (metric tonnes)" and "Area harvested (hectares)" as replacment
prepare_backup_yield <- function(data) {
  backup_yield <- data %>%
    filter(Estimates %in% c("Area harvested (hectares)", "Marketed production (metric tonnes)")) %>%
    group_by(REF_DATE) %>%
    summarize(
      yield_mp_ah_hec_meton = calculate_yield(
        first(VALUE[Estimates == "Marketed production (metric tonnes)"]),
        first(VALUE[Estimates == "Area harvested (hectares)"]),
        892.197 #
      )
    )
  return(backup_yield)
}

# Function to merge data
merge_data <- function(complete_data, backup_yield) {
  return(merged_data)
}

# Function to plot yield over time
plot_yield_over_time <- function(merged_data) {
  p <- ggplot(merged_data, aes(x = REF_DATE)) +
    geom_line(aes(y = yield_mp_ah_hec_meton, color = "Calculated Yield"), size = 1,alpha = 0.6) +
    geom_point(aes(y = yield_mp_ah_hec_meton, color = "Calculated Yield"), size = 2,alpha = 0.6) +
    geom_line(aes(y = VALUE, color = "Original Average Yield"), size = 1,alpha = 0.7) +
    geom_point(aes(y = VALUE, color = "Original Average Yield"), size = 2,alpha = 0.7) +
    labs(title = paste("Yield per Acre (Pounds) of", merged_data$Crop_Type[1]),
         x = "Year",
         y = "Yield",
         color = "Estimates") +
    theme_minimal() +
    scale_color_manual(values = c("Calculated Yield" = "red","Original Average Yield" = "blue" ),
                       labels = c("Calculated Yield","Original Average Yield" )) +
    scale_x_continuous(breaks = seq(min(merged_data$REF_DATE), max(merged_data$REF_DATE), by = 2))+
    theme(panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank()) 
  return(p)
}


