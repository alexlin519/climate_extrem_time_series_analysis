#plot date version
ggplot(df_lab, aes(x = date, y = max_temp)) +
  geom_line() +
  geom_point(aes(color = heat_wave)) +
  geom_hline(yintercept = T1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = T2, linetype = "dashed", color = "blue") +
  labs(title = "Ensuring Valid Heat Waves",
       x = "Date",
       y = "Maximum Temperature (°C)",
       color = "Heat Wave") +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  theme_minimal()

# Create a sample data frame with specific fake data to cover all scenarios
dates <- seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = 99)
max_temp <- c(
  36,37,38,35,40,36,                   #normal heat wave 0
  25, 26, 24, 25, 26, 27, 25, 26, 25,  # Normal temperatures
  35, 36, 37, 35, 36, 38,              # Heat wave 1 (6 days, all above T1)
  32, 33, 31, 34, 32, 31, 33,          # Mixed temperatures above T2, avg above T1 (7 days)
  25, 26, 25, 26,                      # Normal temperatures
  28, 29, 30, 29,                      # Below T1 but above T2, not part of a heat wave (4 days)
  35, 36, 37,                          # Start of heat wave (3 days)
  30, 31, 32,                          # Below T1 but above T2, still part of the heat wave (3 days)
  35, 36, 37,                          # Continuation of heat wave (3 days)
  25, 26, 25, 26, 27, 26, 25, 26, 25,  # Normal temperatures
  25, 26, 27, 28, 29, 27, 28, 30, 35,  # Normal temperatures
  31, 32, 30, 24, 40, 32, 31, 32, 30,  # Mixed temperatures
  28, 29, 27, 26, 25, 28, 29, 30, 29,  # Below T2
  25, 26, 25, 26, 25, 27, 26, 25, 26,  # Normal temperatures
  25, 26, 25, 26, 25, 27, 26, 25, 26   # Normal temperatures
)

df_lab <- data.frame(date = dates, max_temp = max_temp)

# View the first few rows of the data frame
head(df_lab, 20)





step2


# Provided T1 and T2 values
T1 <- 33
T2 <- 28

# Initialize the heat_wave column: days above T2 as TRUE, others as FALSE
df_lab$heat_wave <- df_lab$max_temp > T2

# Identify and mark single days above T2 but below T1 as FALSE
for (i in 1:nrow(df_lab)) {
  if (df_lab$heat_wave[i] == TRUE && 
      (i == 1 || df_lab$heat_wave[i-1] == FALSE) && 
      (i == nrow(df_lab) || df_lab$heat_wave[i+1] == FALSE)) {
    df_lab$heat_wave[i] <- FALSE
  }
}

# Visualize the result
library(ggplot2)

ggplot(df_lab, aes(x = date, y = max_temp)) +
  geom_line() +
  geom_point(aes(color = heat_wave)) +
  geom_hline(yintercept = T1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = T2, linetype = "dashed", color = "blue") +
  labs(title = "Step 2: Marking Single Days Above T2 but Below T1",
       x = "Date",
       y = "Maximum Temperature (°C)",
       color = "Heat Wave") +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  theme_minimal()







step 3
# Provided T1 and T2 values
T1 <- 33
T2 <- 28
# Create a column to indicate if the day is part of a heat wave
df_lab$heat_wave <- df_lab$max_temp > T2
# Initialize tracking variables
# Initialize tracking variables
current_heat_wave <- c()
any_above_T1 <- FALSE

# Updated loop for handling consecutive days
for (i in 1:nrow(df_lab)) {
  if (df_lab$heat_wave[i] == TRUE) {
    current_heat_wave <- c(current_heat_wave, df_lab$date[i])
    if (df_lab$max_temp[i] > T1) {
      any_above_T1 <- TRUE
    }
  } else {
    if (length(current_heat_wave) >= 3 && !any_above_T1) {
      df_lab$heat_wave[df_lab$date %in% current_heat_wave] <- FALSE
    }
    current_heat_wave <- c()
    any_above_T1 <- FALSE
  }
}

# Check the last period if the loop ended during a heat wave
if (length(current_heat_wave) >= 3 && !any_above_T1) {
  df_lab$heat_wave[df_lab$date %in% current_heat_wave] <- FALSE
}

# Add the index column for visualization
df_lab$index <- 1:nrow(df_lab)

ggplot(df_lab, aes(x = date, y = max_temp)) +
  geom_line() +
  geom_point(aes(color = heat_wave)) +
  geom_text(aes(label = index), vjust = -1, size = 3) +
  geom_hline(yintercept = T1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = T2, linetype = "dashed", color = "blue") +
  labs(title = "Ensuring Valid Heat Waves with Data Point Indices",
       x = "Date",
       y = "Maximum Temperature (°C)",
       color = "Heat Wave") +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  theme_minimal()

df_lab <- subset(df_lab, select = -heat_wave)

















# Create a sample data frame with specific fake data to cover all scenarios
dates <- seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = 99)
max_temp <- c(
  36,37,38,35,40,36,                   #normal heat wave 0
  25, 26, 24, 25, 26, 27, 25, 26, 25,  # Normal temperatures
  35, 36, 37, 35, 36, 38,              # Heat wave 1 (6 days, all above T1)
  32, 33, 31, 34, 32, 31, 33,          # Mixed temperatures above T2, avg above T1 (7 days)
  25, 26, 25, 26,                      # Normal temperatures
  28, 29, 30, 29,                      # Below T1 but above T2, not part of a heat wave (4 days)
  35, 36, 37,                          # Start of heat wave (3 days)
  30, 31, 32,                          # Below T1 but above T2, still part of the heat wave (3 days)
  35, 36, 37,                          # Continuation of heat wave (3 days)
  25, 26, 25, 26, 27, 26, 25, 26, 25,  # Normal temperatures
  25, 26, 27, 28, 29, 27, 28, 30, 35,  # Normal temperatures
  31, 32, 30, 24, 40, 32, 31, 32, 30,  # Mixed temperatures
  28, 29, 27, 26, 25, 28, 29, 30, 29,  # Below T2
  25, 26, 25, 26, 25, 27, 26, 25, 26,  # Normal temperatures
  25, 26, 25, 26, 25, 27, 26, 25, 26   # Normal temperatures
)

df_lab <- data.frame(date = dates, max_temp = max_temp)
































#getwd()

# If necessary, set the working directory to the 'r' folder
#setwd("/Users/alexlin/summer_stat/climate_extrem_RA/R")

# Define file paths
file_paths <- c(#"../output/Kamloops_percentiles.csv",
                #"../output/PriGeog_percentiles.csv",
                "../output/YVR_heatmap_data.csv"
                )
# Function to read and select necessary columns
read_and_select <- function(file_path) {
  read.csv(file_path) 
}

# Read and combine all datasets
df_station <- map_dfr(file_paths, read_and_select)
df_station
# Add a binary column indicating whether the temperature is higher than the baseline
df_station$Temp_Higher <- ifelse(df_station$Max_Temp_Year > df_station$Percentile_90, "Higher", "Not Higher")

# Filter data for the specified date range
df_station_filtered <- df_station %>%
  filter(DayOfYear >= "04-01" & DayOfYear <= "10-31")
df_station_filtered <- df_station_filtered %>%
  mutate(Temp_Diff = Max_Temp_Year - Percentile_90)
# Reshape data for heatmap
data_melted_temp <- melt(df_station_filtered, id.vars = c("Year", "DayOfYear"),
                         measure.vars = "Temp_Diff")


data_melted_temp$Temp_Higher <- ifelse(data_melted_temp$value > 0,
                                       "Higher", "Not Higher")
data_melted_temp

# Create a new column for color mapping
#data_melted_temp$Color <- ifelse(data_melted_temp$Temp_Higher == "Higher", data_melted_temp$value, NA)


day_last <- 3

# Identify streaks and reclassify
data_melted_temp <- data_melted_temp %>%
  group_by(Year) %>%
  mutate(streak = with(rle(Temp_Higher == "Higher"), rep(lengths, lengths))) %>%
  mutate(Heatwave = ifelse(Temp_Higher == "Higher" & streak >= day_last, "Heatwave", Temp_Higher)) %>%
  ungroup()


data_melted_temp
data_melted_temp$Color <- ifelse(data_melted_temp$Heatwave == "Heatwave", data_melted_temp$value, NA)
data_melted_temp





heatmap_plot <- ggplot(data_melted_temp, aes(x = DayOfYear, y = Year)) +
  geom_tile(aes(fill = Color)) +
  scale_fill_gradientn(colors = c("blue", "yellow", "red"),
                       values = scales::rescale(c(0, 3, 8)), 
                       na.value = "white", name = "Temperature")+                 
  # scale_fill_gradient2(low = scales::alpha("blue", 0.8), mid = "yellow", high = "red",
  #                      midpoint = 2+mean(df_station_filtered$Temp_Diff[data_melted_temp$Heatwave == "Heatwave"], na.rm = TRUE),
  #                      na.value = "white", name = "Temperature") +
  labs(title = "Temperature Comparison Heatmap (April 1st to September 30th) Over 50 Years",
       x = "Day of Year", y = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12)) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)])
#scale_x_continuous(breaks = seq(1, 365, by = 5))

 # scale_x_continuous(breaks = seq(1, 365, by = 5)) # Using scale_x_continuous for numeric DayOfYear


heatmap_plot













library(dplyr)
library(zoo)
library(reshape2)
library(ggplot2)
library(scales)

#create_heatmap_diff_filter_hw <- function(df, heat_wave_length, start_date, end_date) {
  # Add a binary column indicating whether the temperature is higher than the baseline
  df <- df %>%
    mutate(Temp_Higher = ifelse(Max_Temp_Year > Percentile_90, "Higher", "Not Higher"))
  
  # Filter data for the specified date range
  df_filtered <- df %>%
    filter(DayOfYear >= start_date & DayOfYear <= end_date)
  
  # Create Temp_Diff column if it doesn't exist in df_filtered
  df_filtered <- df_filtered %>%
    mutate(Temp_Diff = Max_Temp_Year - Percentile_90)
  
  # Reshape data for heatmap
  data_melted_temp <- melt(df_filtered, id.vars = c("Year", "DayOfYear"),
                           measure.vars = "Max_Temp_Year")
  
  # Join Temp_Diff and Percentile_90 with the melted data
  data_melted_temp <- data_melted_temp %>%
    left_join(df_filtered %>% select(Year, DayOfYear, Temp_Diff, Percentile_90), by = c("Year", "DayOfYear"))
  
  # Calculate Temp_Higher based on the melted data's value and Percentile_90
  data_melted_temp <- data_melted_temp %>%
    mutate(Temp_Higher = ifelse(value > Percentile_90, "Higher", "Not Higher"))
  
  # Identify streaks and reclassify
  data_melted_temp <- data_melted_temp %>%
    group_by(Year) %>%
    mutate(streak = with(rle(Temp_Higher == "Higher"), rep(lengths, lengths))) %>%
    mutate(Temp_Higher = ifelse(Temp_Higher == "Higher" & streak < heat_wave_length, "Not Higher", Temp_Higher)) %>%
    mutate(Heatwave = ifelse(Temp_Higher == "Higher" & streak >= heat_wave_length, "Heatwave", Temp_Higher)) %>%
    ungroup()
  
  # Assign color values based on the Heatwave classification
  data_melted_temp <- data_melted_temp %>%
    mutate(Color = ifelse(Heatwave == "Heatwave", Temp_Diff, NA))
  
  # Plot the heatmap
  heatmap_plot <- ggplot(data_melted_temp, aes(x = DayOfYear, y = Year)) +
    geom_tile(aes(fill = Color)) +
    scale_fill_viridis_c(na.value = "white", name = "Temp Difference") +
    labs(title = "Temperature Comparison Heatmap (April 1st to September 30th) Over 50 Years",
         x = "Day of Year", y = "Year") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12)) +
    scale_x_continuous(breaks = seq(1, 365, by = 5))
  print('5')
  return(heatmap_plot)
}

# Example usage:
create_heatmap_diff_filter_hw(combined_df, heat_wave_length = 5, start_date = 91, end_date = 273) # Example date range for April 1st to September 30th




# Assuming your DataFrame is named df
# and your variable names are var1 and var2

var1 <- "variable1"
var2 <- "variable2"

# Create the filename by concatenating the variable names
filename <- paste0(var1, "_", var2, "_data.csv")

# Save the DataFrame to a CSV file with the constructed filename
write.csv(df, filename, row.names = FALSE)













library(dplyr)
library(tidyr)
library(ggplot2)

# Example dataframes (replace with your actual data)
df1 <- data.frame(
  Year = c(1951, 1983, 1994, 2015, 2017,2020,  2021),
  unique_count_Kamloops_vs_Prince_George = c(2, 2, 3, 1, 2, 2, 1)
)

df2 <- data.frame(
  Year = c(1951, 1967, 2018, 2020,1996, 1998, 2014),
  unique_count_Kamloops_vs_YVR = c(1, 1, 1, 2, 1, 2, 1)
)
# Combine the dataframes into one
combined_df <- bind_rows(
  df1 %>% rename(unique_count = unique_count_Kamloops_vs_Prince_George) %>% mutate(Comparison = "Kamloops vs Prince George"),
  df2 %>% rename(unique_count = unique_count_Kamloops_vs_YVR) %>% mutate(Comparison = "Kamloops vs YVR")
)

# Ensure Year is treated as a factor for proper stacking
combined_df$Year <- as.factor(combined_df$Year)

# Create a stacked bar plot
ggplot(combined_df, aes(x = Year, y = unique_count, fill = Comparison)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Unique Count by Year and Comparison",
       x = "Year",
       y = "Unique Count",
       fill = "Comparison") +
  theme_minimal()



























library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(0)

# Generate fake data
years <- 1950:2025
days_of_year <- 91:273  # April 1st (91) to September 30th (273)
stations <- c('Station_A', 'Station_B', 'Station_C')

generate_data <- function(years, days_of_year, stations) {
  data <- expand.grid(Year = years, Day_of_Year = days_of_year, Station = stations)
  data$Temperature <- rnorm(nrow(data), mean = 4, sd = 1)
  
  # Introduce some extreme values
  extreme_indices <- sample(1:nrow(data), size = 0.05 * nrow(data))
  data$Temperature[extreme_indices] <- rnorm(length(extreme_indices), mean = 8, sd = 0.5)
  
  return(data)
}

df <- generate_data(years, days_of_year, stations)

# Filter for temperatures over 5
df_filtered <- df %>% filter(Temperature > 5)
library(ggplot2)

# Plot heatmap
ggplot(df_filtered, aes(x = Day_of_Year, y = Year, fill = Temperature)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("blue", "yellow", "red"), limits = c(2, 10)) +
  facet_wrap(~ Station) +
  labs(title = "Temperature Comparison Heatmap (April 1st to September 30th) Over 50C",
       x = "Day of Year", y = "Year", fill = "Temperature") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Load necessary libraries
library(ggplot2)

# Create example data
data <- data.frame(
  Year = rep(1950:2025, each = 3),
  Temperature = runif(228, min = 2, max = 10),
  Station = rep(c('Station_A', 'Station_B', 'Station_C'), times = 76)
)

# Create a line segment plot
ggplot(data, aes(x = Year, y = Temperature, color = Station, group = Station)) +
  geom_line() + 
  geom_point() +
  labs(title = "Temperature Trends Over Years by Station",
       x = "Year",
       y = "Temperature") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(0)

# Generate fake data
years <- 1950:2025
days_of_year <- 91:273  # April 1st (91) to September 30th (273)
stations <- c('Station_A', 'Station_B', 'Station_C')

generate_data <- function(years, days_of_year, stations) {
  data <- expand.grid(Year = years, Day_of_Year = days_of_year, Station = stations)
  data$Temperature <- rnorm(nrow(data), mean = 4, sd = 1)
  
  # Introduce some extreme values
  extreme_indices <- sample(1:nrow(data), size = 0.05 * nrow(data))
  data$Temperature[extreme_indices] <- rnorm(length(extreme_indices), mean = 8, sd = 0.5)
  
  return(data)
}

df <- generate_data(years, days_of_year, stations)

# Filter for temperatures over 5
df_filtered <- df %>% filter(Temperature > 5)
library(ggplot2)

# Create a line segment plot
ggplot(df_filtered, aes(x = Day_of_Year, y = Temperature, color = Station, group = interaction(Year, Station))) +
  geom_line() + 
  geom_point() +
  labs(title = "Temperature Trends Over Days by Station (April 1st to September 30th)",
       x = "Day of Year",
       y = "Temperature") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))












# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(42)

generate_clustered_data <- function(years, num_clusters = 2, cluster_size = 8, stations) {
  data <- data.frame()
  for (year in years) {
    for (station in stations) {
      cluster_starts <- sample(1:(365 - cluster_size), num_clusters, replace = TRUE)
      for (start in cluster_starts) {
        days <- start:(start + cluster_size - 1)
        temperatures <- round(runif(length(days), min = 10, max = 20), 1)
        data <- rbind(data, data.frame(Year = year, DayOfYear = days, Station = station, Temperature = temperatures))
      }
    }
  }
  return(data)
}

 

# Define parameters
years <- 1950:2025
stations <- c('Station_A', 'Station_B', 'Station_C')

# Generate data
df <- generate_clustered_data(years, stations = stations)

# Plotting the line segment plot with clustered and overlapping data
ggplot(df, aes(x = DayOfYear, y = Year, color = Station)) +
  geom_segment(aes(xend = DayOfYear, yend = Year)) + 
  geom_point() +
  labs(title = "Temperature Trends Over Days by Station with Clustered and Overlapping Data",
       x = "Day of Year",
       y = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




# Plotting the line segment plot with transparency to highlight overlaps
ggplot(df, aes(x = DayOfYear, y = Year, color = Station)) +
  geom_segment(aes(xend = DayOfYear, yend = Year)) + 
  geom_point(alpha = 0.2, size = 2) +  # Adjust transparency with alpha
  labs(title = "Temperature Trends Over Days by Station with Clustered and Overlapping Data",
       x = "Day of Year",
       y = "Year") +
  scale_color_manual(values = c("Station_A" = "blue", "Station_B" = "green", "Station_C" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

###########
###########
###########
###########
###########
###########
#############final
##############
###########
###########
###########
###########
###########
###########

generate_clustered_data <- function(year_range, num_years, num_clusters = 2, cluster_size = 8, stations) {
  # Randomly select a subset of years from the specified range
  years <- sample(year_range, num_years, replace = FALSE)
  
  data <- data.frame()
  for (year in years) {
    for (station in stations) {
      cluster_starts <- sample(1:(365 - cluster_size), num_clusters, replace = TRUE)
      for (start in cluster_starts) {
        days <- start:(start + cluster_size - 1)
        temperatures <- round(runif(length(days), min = 10, max = 20), 1)
        data <- rbind(data, data.frame(Year = year, DayOfYear = days, Station = station, Temperature = temperatures))
      }
    }
  }
  return(data)
}
# Example usage
year_range <- 1970:2024  # Define the range of years to select from
num_years <- 10  # Number of random years to select
stations <- c("Station_A","Station_B","Station_C")  # Limiting to one station
df <- generate_clustered_data(year_range, num_years, num_clusters = 2, cluster_size = 5, stations)  # Reducing cluster size and number
# Plotting the line segment plot with small rectangles to highlight overlaps
ggplot(df, aes(x = DayOfYear, y = Year, fill = Station)) +
  geom_tile(aes(width = 0.99, height = 0.99), alpha = 0.5) +  # Adjust transparency and size of rectangles
  labs(title = "Temperature Trends Over Days by Station with Clustered and Overlapping Data",
       x = "Day of Year",
       y = "Year") +
  scale_fill_manual(values = c("Station_A" = "blue", "Station_B" = "green", "Station_C" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))





# Count overlaps
df_count <- df %>%
  group_by(Year, DayOfYear) %>%
  summarize(Overlap = n(), .groups = 'drop')

# Merge counts back to original dataframe
df <- df %>%
  left_join(df_count, by = c("Year", "DayOfYear"))


# Plotting the line segment plot with color gradient to highlight overlaps
ggplot(df, aes(x = DayOfYear, y = Year)) +
  geom_segment(aes(xend = DayOfYear, yend = Year)) + 
  geom_point(aes(color = Overlap), size = 2) +  # Color by overlap count
  scale_color_gradient(low = "blue", high = "red") +  # Gradient from blue to red
  labs(title = "Temperature Trends Over Days by Station with Clustered and Overlapping Data",
       x = "Day of Year",
       y = "Year",
       color = "Overlap Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))








# Install necessary packages if not already installed
if (!requireNamespace("utils", quietly = TRUE)) {
  install.packages("utils")
}

# Load the required package
library(utils)

# Specify the path to your zip file
txt_file <- "../data/dates_heatwave.csv"
df_30yvr_hw_3day <- read.csv(txt_file, header = TRUE, sep = ",")  # Using sep = "" to handle any amount of whitespace
print(head(df_30yvr_hw_3day))

# Create a new dataframe with Year and DayOfYear columns
library(dplyr)
library(tidyr)

# Convert the begin and end columns to Date type
df_30yvr_hw_3day <- df_30yvr_hw_3day %>%
  mutate(begin = as.Date(begin),
         end = as.Date(end))

# Generate a sequence of dates for each row and expand the dataframe
df_expanded <- df_30yvr_hw_3day %>%
  rowwise() %>%
  mutate(dates = list(seq(begin, end, by = "day"))) %>%
  unnest(dates)

# Create the new dataframe with Year and DayOfYear columns

# Create the new dataframe with Year, DayOfYear, and Station columns
df_30yvr_hw_3day_new <- df_expanded %>%
  mutate(Year = as.numeric(format(dates, "%Y")),
         DayOfYear = format(dates, "%m-%d"),
         station = "YVR_30y_based") %>%
  select(Year, DayOfYear, station)
# Print the new dataframe to check the result







txt_file <- "../data/q90-yvr.csv"
df <- read.table(txt_file, header = TRUE, sep = ",")  # Adjust the separator if needed
df <- df[, -1, drop=FALSE]
# Rename columns to match the desired format
colnames(df) <- c("Percentile_90", "Month", "Day")

# Add a new column "Station" with a constant value
df$Station <- "YVR_30y_based"

# Create a "Date" column from "Month" and "Day"
df$Date <- as.Date(paste("2000", df$Month, df$Day, sep = "-"), format = "%Y-%m-%d")

# Reorder columns to match the desired structure
df <- df[, c("Month", "Day", "Percentile_90", "Station", "Date")]

# Print the first few rows of the modified dataframe to confirm
print(head(df))






















library(ggplot2)
library(dplyr)

# Example data (replace with your actual data)
set.seed(123)
df_merged <- data.frame(
  Year = sample(2000:2020, 100, replace = TRUE),
  DayOfYear = sample(1:365, 100, replace = TRUE),
  Max_Temp_Year_1 = runif(100, min = 20, max = 40),
  Max_Temp_Year_2 = runif(100, min = 20, max = 40)
)

# Define extreme_range dynamically
extreme_range <- 1:90  # This could be any range

# Extract the last value from extreme_range
last_value <- extreme_range[length(extreme_range)]

# Select top extremes
top_extremes <- df_merged %>%
  arrange(desc(Max_Temp_Year_1 + Max_Temp_Year_2)) %>%
  slice(extreme_range) %>%
  mutate(label = paste(Year, DayOfYear, sep = "-"))

######### Fit a linear model
fit <- lm(Max_Temp_Year_2 ~ Max_Temp_Year_1, data = top_extremes)

########## Generate 95% prediction intervals
pred <- predict(fit, newdata = top_extremes, interval = "prediction", level = 0.95)

# Add prediction intervals to the data frame
top_extremes <- top_extremes %>%
  mutate(pred_lwr = pred[, "lwr"], pred_upr = pred[, "upr"])

# Plot with custom colors, star shapes, and smaller points
ggplot(top_extremes, aes(x = Max_Temp_Year_1, y = Max_Temp_Year_2)) +
  geom_point(aes(color = label, shape = label), size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +  # Add 45-degree line
  geom_line(aes(y = pred_lwr), linetype = "dotted", color = "blue", size = 0.5) +  # Lower prediction interval
  geom_line(aes(y = pred_upr), linetype = "dotted", color = "blue", size = 0.5) +  # Upper prediction interval
  labs(title = paste("Top Extremes (Last Value from extreme_range:", last_value, ")"),
       x = "Max Temp Year 1",
       y = "Max Temp Year 2",
       color = "Label",
       shape = "Label") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))











# Define the base colors
base_colors <- c('#FF0000', '#008000', '#0000FF', '#FF00FF', '#FFA500', 
                 '#FFD700', '#D2691E', '#D2B48C', '#808000', '#FFFF00', 
                 '#40E0D0', '#00BFFF', '#1E90FF', '#8A2BE2', '#FF1493',
                 '#A52A2A', '#7FFF00', '#8B008B', '#FF69B4','#A9A9A9', 
                 '#696969','green') 

# Adjust transparency (alpha = 0.5 for 50% transparency)
transparent_colors <- adjustcolor(base_colors, alpha.f = 0.5)

# Pick 4 colors
selected_colors <- transparent_colors[c(1, 5, 13, 22)]

# Define the base colors
selected_colors <- c('#FF0000', '#00FF00', '#0000FF', '#00FFFF')

# Adjust transparency (alpha = 0.5 for 50% transparency)
selected_colors <- adjustcolor(selected_colors, alpha.f = 0.4)

# Function to plot rectangles
plot_rectangles <- function(colors, overlaps, title) {
  plot(1, type="n", xlim=c(0, 10), ylim=c(0, 10), xlab="", ylab="", main=title)
  positions <- list(c(1, 1), c(3, 3), c(5, 5), c(7, 7))
  for (i in 1:length(colors)) {
    rect(positions[[i]][1], positions[[i]][2], positions[[i]][1]+2, positions[[i]][2]+2, col=colors[i], border=NA)
  }
  for (overlap in overlaps) {
    rect(positions[[overlap[1]]][1], positions[[overlap[1]]][2], 
         positions[[overlap[2]]][1]+2, positions[[overlap[2]]][2]+2, col=colors[overlap[2]], border=NA)
  }
}

# Create separate plots for each overlap case
# No overlap
plot_rectangles(selected_colors, list(), "No Overlap")

# Single overlaps
combinations <- combn(1:4, 2)
for (i in 1:ncol(combinations)) {
  plot_rectangles(selected_colors, list(combinations[,i]), paste("Overlap", combinations[,i][1], "and", combinations[,i][2]))
}

# Double overlaps
combinations <- combn(1:4, 3)
for (i in 1:ncol(combinations)) {
  plot_rectangles(selected_colors, list(combinations[,i][1:2], combinations[,i][2:3]), 
                  paste("Overlap", combinations[,i][1], combinations[,i][2], "and", combinations[,i][3]))
}

# Triple overlap
plot_rectangles(selected_colors, list(c(1,2), c(2,3), c(3,4)), "Triple Overlap")

# All overlap
plot_rectangles(selected_colors, list(c(1,2), c(2,3), c(3,4), c(1,4)), "All Overlap")

























# Install necessary libraries if not already installed
if (!require("readxl")) install.packages("readxl", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)

# Load libraries

library(ggplot2)
library(dplyr)


