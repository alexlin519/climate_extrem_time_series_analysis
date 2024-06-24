library(dplyr)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)
library(ggplot2)
### get the 15day window data points


# Function to get 15-row window as a list element
get_rolling_window <- function(x, width = 15) {
  n <- length(x)
  windows <- vector("list", n)
  
  for (i in seq_len(n)) {
    start <- max(1, i - (width %/% 2))
    end <- min(n, i + (width %/% 2))
    windows[[i]] <- x[start:end]
  }
  
  return(windows)
}
# Apply the function to create the rolling window column

df_wrangling <- df %>%
  mutate(rolling_window = get_rolling_window(MAX_TEMPERATURE))

df_wrangling <- df_wrangling %>%
  mutate(LOCAL_DATE = as.Date(LOCAL_DATE, format = "%Y-%m-%d"),
         Month = format(LOCAL_DATE, "%m"),
         Day = format(LOCAL_DATE, "%d")
  )




### get the all year for same day value, around 165 data points


# Assuming rolling_window column already exists in df_wrangling
# If not, create it first as shown previously

# Group by Month and Day, and concatenate the rolling_window lists
df_grouped <- df_wrangling %>%
  filter(year(LOCAL_DATE) >= 1960 & year(LOCAL_DATE) <= 1990) %>%
  group_by(Month, Day) %>%
  summarize(ROLLING_WINDOW_ALL_YEAR_VALUES = list(reduce(rolling_window, c)), .groups = 'drop')

df_grouped_all <- df_wrangling %>%
  group_by(Month, Day) %>%
  summarize(ROLLING_WINDOW_ALL_YEAR_VALUES = list(reduce(rolling_window, c)), .groups = 'drop')


### get 90th from the 165 all year same day value
# # # # 
# Calculate the 90th percentile for each day
df_percentiles <- df_grouped %>%
  mutate(
    Percentile_90 = map_dbl(ROLLING_WINDOW_ALL_YEAR_VALUES, ~ quantile(.x, 0.90,'na.rm'=TRUE))
  )

df_percentiles_all <- df_grouped_all %>%
  mutate(
    Percentile_90 = map_dbl(ROLLING_WINDOW_ALL_YEAR_VALUES, ~ quantile(.x, 0.90,'na.rm'=TRUE))
  )





# Add a column to distinguish between the two datasets
df_percentiles <- df_percentiles %>%
  mutate(Source = "1960-1990")

df_percentiles_all <- df_percentiles_all %>%
  mutate(Source = "All Years")

# Combine the two data frames
df_90_compare <- bind_rows(df_percentiles, df_percentiles_all)

# Convert Month and Day to a Date for plotting (year is arbitrary, using 2000 as a placeholder)
df_90_compare <- df_90_compare %>%
  mutate(Date = as.Date(paste("2000", Month, Day, sep = "-")))

# Plot the data
compare_plot <- ggplot(df_90_compare, aes(x = Date, y = Percentile_90, color = Source)) +
  geom_line() +
  labs(title = paste(station_name ,"90th Percentile Temperature Comparison"),
       x = "Date",
       y = "90th Percentile Temperature",
       color = "Data Source") +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  theme_minimal()

#print(compare_plot)
