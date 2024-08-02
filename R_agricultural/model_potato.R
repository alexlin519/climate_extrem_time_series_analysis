### ---calculate the potato yield Moving Average ------------
## Using a Moving Average to Detrend Time Series Data
# Install and load necessary packages
library(zoo)
library(ggplot2)
# Calculate the moving average with a window size of, say, 5 years
data_pot$Moving_Avg <- rollmean(data_pot$VALUE, k = 5, fill = NA)

# Detrend the data_pot by subtracting the moving average from the original yield data_pot
data_pot$Detrended_Yield <- data_pot$VALUE - data_pot$Moving_Avg

#plot
mov_avg <- ggplot(data_pot, aes(x = REF_DATE)) +
  geom_line(aes(y = VALUE, color = "Original Yield")) +
  geom_line(aes(y = Moving_Avg, color = "Moving Average")) +
  geom_line(aes(y = Detrended_Yield, color = "Detrended Yield")) +
  #vertical line for 2021
  geom_vline(xintercept = 2021, linetype = "dashed") +
  labs(title = "Potato Yield and Detrended Yield", y = "Yield", x = "Year") +
  scale_color_manual(values = c("Original Yield" = "blue", "Moving Average" = "green", "Detrended Yield" = "red")) +
  theme_minimal()


#Using a LOESS Smoother (Locally Estimated Scatterplot Smoothing)
# Fit a LOESS smoother to the data
loess_model <- loess(VALUE ~ REF_DATE, data = data_pot, span = 0.2)  # Adjust 'span' for smoothness
data_pot$Loess_Trend_LOESS <- predict(loess_model)

# Detrend the data_pot by subtracting the LOESS trend from the original yield data_pot
data_pot$Detrended_Yield_LOESS <- data_pot$VALUE - data_pot$Loess_Trend_LOESS

#PLOT
LOESS_avg <- ggplot(data_pot, aes(x = REF_DATE)) +
  geom_line(aes(y = VALUE, color = "Original Yield")) +
  geom_line(aes(y = Loess_Trend_LOESS, color = "LOESS Trend")) +
  geom_line(aes(y = Detrended_Yield_LOESS, color = "Detrended Yield (LOESS)")) +
  #vertical line for 2021
  geom_vline(xintercept = 2021, linetype = "dashed") +
  labs(title = "Potato Yield and Detrended Yield (LOESS)", y = "Yield", x = "Year") +
  scale_color_manual(values = c("Original Yield" = "blue", "LOESS Trend" = "green", "Detrended Yield (LOESS)" = "red")) +
  theme_minimal()



### model monthly potato yield with ehf----------------


lm_monthly_potato <- function(monthly_data_x, patato,certain_station,x_var,y_var) {
  # patato yield is column name: VALUE
  # Check if the predictor_variable is valid
  if (!x_var %in% c("maxmon_Mean_Temp", "maxmon_Percentile_95", "maxmon_EHF_95")) {
    stop("Invalid predictor_variable. Choose either 'maxmon_Mean_Temp', 'maxmon_Percentile_95', or 'maxmon_EHF_95'.")
  }
  
  columns_to_keep <- setdiff(c("maxmon_Mean_Temp", "maxmon_Percentile_95", "maxmon_EHF_95"), x_var)
  
  # Process weekly data
  merged_data_x <- monthly_data_x %>%
    filter(station ==  certain_station) %>%
    select(-all_of(columns_to_keep)) %>%
    pivot_wider(names_from = Month, values_from = !!sym(x_var), names_prefix = "Month_")
  
  
  
  #Check for NAs in the merged data
  if (sum(is.na(merged_data_x)) > 0) {
    num_of_na <- sum(is.na(merged_data_x))
    print(paste("There are",num_of_na," NA in the matrix X in",certain_station,"station"))
    # Interpolate NA values in the wide data frame
    merged_data_x <- handle_na(merged_data_x)
  }
  
  # Filter yield data
  y_data <- patato %>% 
    rename(Year = REF_DATE) 
  # Merge the temperature and yield data by Year
  merged_data <- inner_join(y_data, merged_data_x, by = "Year")
    
    # Extract response variable y and predictor variables x
    y <- merged_data[[y_var]]
    x <- merged_data %>% ungroup %>% select(starts_with("Month_"))
    
    # Print x to show what it looks like
    # print(paste("Predictor matrix x for crop:", crop))
    # print(x)
    
    # Add intercept to the model
    model <- lm(y ~ ., data = x)
    result <- summary(model)
    # Extract coefficients and their standard errors, removing the intercept
    coeffs <- summary(model)$coefficients[-1,]
    coeffs_df <- as.data.frame(coeffs)
    coeffs_df$Month <- as.numeric(gsub("Month_", "", rownames(coeffs_df)))
    coeffs_df <- coeffs_df[order(coeffs_df$Month), ]
    
    # Create the plot
    p <- ggplot(coeffs_df, aes(x = Month, y = Estimate)) +
      geom_line(color = "blue") +
      geom_ribbon(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), alpha = 0.2) +
      labs(title = paste("Coefficient by Month for patato in", certain_station),
           x = "Month",
           y = "Coefficient Estimate") +
      #shows more x and y values
      scale_x_continuous(breaks = seq(min(coeffs_df$Month), max(coeffs_df$Month), by = 2))+
      scale_y_continuous(breaks = pretty_breaks(n = 16))+ # Specify the number of breaks
      theme_minimal() +
      theme(panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank())
    print(p)
  return(result)
}


lm_onemonth_potato <- function(monthly_data_x, patato, certain_station, x_var, y_var) {
  if (!x_var %in% c("maxmon_Mean_Temp", "maxmon_Percentile_95", "maxmon_EHF_95")) {
    stop("Invalid predictor_variable. Choose either 'maxmon_Mean_Temp', 'maxmon_Percentile_95', or 'maxmon_EHF_95'.")
  }
  
  columns_to_keep <- setdiff(c("maxmon_Mean_Temp", "maxmon_Percentile_95", "maxmon_EHF_95"), x_var)
  
  # Filter and transform data for the specific station
  merged_data_x <- monthly_data_x %>%
    filter(station == certain_station) %>%
    select(-all_of(columns_to_keep)) %>%
    pivot_wider(names_from = Month, values_from = !!sym(x_var), names_prefix = "Month_")
  
  # Check for NAs in the merged data and handle them
  if (sum(is.na(merged_data_x)) > 0) {
    num_of_na <- sum(is.na(merged_data_x))
    print(paste("There are", num_of_na, "NA in the matrix X in", certain_station, "station"))
    merged_data_x <- handle_na(merged_data_x)
  }
  
  # Prepare yield data
  y_data <- patato %>% rename(Year = REF_DATE)
  merged_data <- inner_join(y_data, merged_data_x, by = "Year")
  
  # Extract response variable y
  y <- merged_data[[y_var]]
  
  # Initialize a list to store results for each month
  results <- list()
  coeffs_df_list <- list()
  
  # Iterate over each month and fit a model
  for (month in 1:12) {
    month_col <- paste0("Month_", month)
    if (month_col %in% colnames(merged_data)) {
      x <- merged_data %>% select(all_of(month_col))
      colnames(x) <- "Predictor"
      
      model <- lm(y ~ Predictor, data = x)
      result <- summary(model)
      
      coeffs <- result$coefficients
      coeffs_df <- data.frame(
        Month = month,
        Estimate = coeffs[2, "Estimate"],
        `Std. Error` = coeffs[2, "Std. Error"],
        `t value` = coeffs[2, "t value"],
        `Pr(>|t|)` = coeffs[2, "Pr(>|t|)"]
      )
      coeffs_df_list[[month]] <- coeffs_df
      results[[month]] <- result
    }
  }
  
  # Create the scatter plots with fitted linear models for each month
  plots <- list()
  for (month in 1:12) {
    month_col <- paste0("Month_", month)
    if (!is.null(results[[month]])) {
      # Extract data for the current month
      month_data <- merged_data %>% select(Year, y_var, !!sym(month_col))
      colnames(month_data)[2:3] <- c("Response", "Predictor")
      
      # Fit the model
      model <- results[[month]]
      
      # Create the plot
      p <- ggplot(month_data, aes(x = Predictor, y = Response)) +
        geom_point(color = "blue") +
        geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "red") +
        labs(title = paste("Scatter plot and LM for Month", month, "in", certain_station),
             x = x_var,
             y = y_var) +
        theme_minimal()
      
      plots[[month]] <- p
      print(p)
    }
  }
  return(results)
}



### model seasonal potato yield with ehf----------------
lm_season_potato <- function(season_data_x, patato, certain_station, x_var, y_var) {
    if (!x_var %in% c("maxsea_Mean_Temp", "maxsea_Percentile_95", "maxsea_EHF_95")) {
      stop("Invalid predictor_variable. Choose either 'maxsea_Mean_Temp', 'maxsea_Percentile_95', or 'maxsea_EHF_95'.")
    }
    
    columns_to_keep <- setdiff(c("maxsea_Mean_Temp", "maxsea_Percentile_95", "maxsea_EHF_95"), x_var)
    
    # Filter and transform data for the specific station, and add a Season column
    merged_data_x <- season_data_x %>%
      filter(station == certain_station) %>%
      select(-all_of(columns_to_keep)) %>%
      pivot_wider(names_from = Season, values_from = !!sym(x_var), names_prefix = "Season_")
    
    # Check for NAs in the merged data and handle them
    if (sum(is.na(merged_data_x)) > 0) {
      num_of_na <- sum(is.na(merged_data_x))
      print(paste("There are", num_of_na, "NA in the matrix X in", certain_station, "station"))
      merged_data_x <- handle_na(merged_data_x)
    }
    
    # Prepare yield data
    y_data <- patato %>% rename(Year = REF_DATE)
    merged_data <- inner_join(y_data, merged_data_x, by = "Year")
    
    # Extract response variable y
    y <- merged_data[[y_var]]
    
    # Initialize a list to store results for each season
    results <- list()
    
    # Iterate over each season and fit a model
    for (season in c("Winter", "Spring", "Summer", "Fall")) {
      season_col <- paste0("Season_", season)
      if (season_col %in% colnames(merged_data)) {
        x <- merged_data %>% select(all_of(season_col))
        colnames(x) <- "Predictor"
        
        model <- lm(y ~ Predictor, data = x)
        result <- summary(model)
        results[[season]] <- result
      }
    }
    
    # Create the scatter plots with fitted linear models for each season
    for (season in c("Winter", "Spring", "Summer", "Fall")) {
      season_col <- paste0("Season_", season)
      if (!is.null(results[[season]])) {
        # Extract data for the current season
        season_data <- merged_data %>% select(Year, y_var, !!sym(season_col))
        colnames(season_data)[2:3] <- c("Response", "Predictor")
        
        # Create the plot
        p <- ggplot(season_data, aes(x = Predictor, y = Response)) +
          geom_point(color = "blue") +
          geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "red") +
          labs(title = paste("Scatter plot and LM for", season, "in", certain_station),
               x = x_var,
               y = y_var) +
          theme_minimal()
        
        print(p)
      }
    }
    return(results)
  }
  
