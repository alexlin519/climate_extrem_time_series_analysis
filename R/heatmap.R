# Load necessary libraries
library(ggplot2)
library(reshape2)

# Create fake data for five years
set.seed(123)
days <- 1:365

generate_yearly_temps <- function(year) {
  # Simulate a sinusoidal temperature pattern
  mean_temp <- 20 + 10 * sin(2 * pi * (days - 80) / 365) + rnorm(365, sd = 3)
  return(mean_temp)
}

data_list <- lapply(1:50, function(year) {
  data.frame(
    DayOfYear = format(as.Date(days, origin="2020-01-01"), "%m-%d"),
    MAX_TEMP_YEAR = generate_yearly_temps(year),
    Year = year
  )
})

data_five_years <- do.call(rbind, data_list)

# Reshape data for heatmap
data_melted <- melt(data_five_years, id.vars = c("Year", "DayOfYear"), 
                    measure.vars = "MAX_TEMP_YEAR")
data_melted
# Create heatmap
ggplot(data_melted, aes(x = DayOfYear, y = Year, fill = value)) +
  geom_tile() +
  #scale_fill_gradient(low = "blue", high = "red", name = "Temperature") +
  scale_fill_viridis_c(name = "Avg Temperature")+ #green yellow blue
  #scale_fill_viridis_c(name = "Avg Temperature", option = "plasma")+ #yello - blue
  labs(title = "Temperature Heatmap Over 5 Years", x = "Day of Year", y = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 15)])

















### some month# Load necessary libraries
library(ggplot2)
library(reshape2)

# Create realistic temperature data for fifty years
set.seed(123)
days <- 1:365

generate_yearly_temps <- function(year) {
  # Simulate a sinusoidal temperature pattern
  mean_temp <- 20 + 10 * sin(2 * pi * (days - 80) / 365) + rnorm(365, sd = 3)
  return(mean_temp)
}

data_list <- lapply(1:50, function(year) {
  data.frame(
    Date = as.Date(days, origin="2020-01-01"),
    DayOfYear = format(as.Date(days, origin="2020-01-01"), "%m-%d"),
    MAX_TEMP_YEAR = generate_yearly_temps(year),
    Year = year
  )
})

data_fifty_years <- do.call(rbind, data_list)

# Filter data for April 1st to September 30th
data_filtered <- data_fifty_years[data_fifty_years$Date >= as.Date("2020-04-01") &
                                    data_fifty_years$Date <= as.Date("2020-09-30"), ]

# Reshape data for heatmap
data_melted_filtered <- melt(data_filtered, id.vars = c("Year", "DayOfYear"), 
                             measure.vars = "MAX_TEMP_YEAR")

# Create heatmap
ggplot(data_melted_filtered, aes(x = DayOfYear, y = Year, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Avg Temperature") + # Use viridis color scale
  labs(title = "Temperature Heatmap (April 1st to September 30th) Over 50 Years", 
       x = "Day of Year", y = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)])





#### baseline diff

# Load necessary libraries
library(ggplot2)
library(reshape2)

# Create realistic temperature data for fifty years
set.seed(123)
days <- 1:365

# Generate a baseline temperature pattern (e.g., average temperature for each day)
baseline_temp <- 20 + 10 * sin(2 * pi * (days - 80) / 365)

generate_yearly_temps <- function(year) {
  # Simulate a sinusoidal temperature pattern with some noise
  mean_temp <- 20 + 10 * sin(2 * pi * (days - 80) / 365) + rnorm(365, sd = 3)
  return(mean_temp)
}

data_list <- lapply(1:50, function(year) {
  data.frame(
    Date = as.Date(days, origin="2020-01-01"),
    DayOfYear = format(as.Date(days, origin="2020-01-01"), "%m-%d"),
    MAX_TEMP_YEAR = generate_yearly_temps(year),
    Year = year
  )
})

data_fifty_years <- do.call(rbind, data_list)

# Merge the baseline temperature into the data
data_fifty_years$Baseline_Temp <- rep(baseline_temp, 50)
data_fifty_years$Temp_Diff <- data_fifty_years$MAX_TEMP_YEAR - data_fifty_years$Baseline_Temp

# Filter data for April 1st to September 30th
data_filtered <- data_fifty_years[data_fifty_years$Date >= as.Date("2020-04-01") &
                                    data_fifty_years$Date <= as.Date("2020-09-30"), ]

# Reshape data for heatmap
data_melted_filtered <- melt(data_filtered, id.vars = c("Year", "DayOfYear"), 
                             measure.vars = "Temp_Diff")

# Create heatmap
ggplot(data_melted_filtered, aes(x = DayOfYear, y = Year, fill = value)) +
  geom_tile() +
  
  #scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Temp Difference") +
  scale_fill_gradientn(colors = c("#0000FF", "blue", "white", "red", "#FF0000"), 
                       values = scales::rescale(c(-11, -8, 0, 8, 11)), 
                       name = "Temp Difference")+
  labs(title = "Temperature Difference Heatmap (April 1st to September 30th) Over 50 Years", 
       x = "Day of Year", y = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)])










### hw day


### version 2
# Add a binary column indicating whether the temperature is higher than the baseline
data_fifty_years$Temp_Higher <- ifelse(data_fifty_years$MAX_TEMP_YEAR > data_fifty_years$Baseline_Temp, "Higher", "Not Higher")

# Filter data for April 1st to September 30th
data_filtered <- data_fifty_years[data_fifty_years$Date >= as.Date("2020-04-01") &
                                    data_fifty_years$Date <= as.Date("2020-09-30"), ]

# Reshape data for heatmap
data_melted_filtered <- melt(data_filtered, id.vars = c("Year", "DayOfYear"), 
                             measure.vars = "Temp_Higher")

# Create heatmap
ggplot(data_melted_filtered, aes(x = DayOfYear, y = Year, fill = value)) +
  geom_tile() +
  scale_fill_manual(values = c("Higher" = "orange", "Not Higher" = "white"), name = "Temp Higher") +
  labs(title = "Temperature Comparison Heatmap (April 1st to September 30th) Over 50 Years", 
       x = "Day of Year", y = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)])

