# Load necessary libraries
library(readr)
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
