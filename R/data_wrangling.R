library(dplyr)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)

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
  group_by(Month, Day) %>%
  summarize(ROLLING_WINDOW_ALL_YEAR_VALUES = list(reduce(rolling_window, c)), .groups = 'drop')


### get 90th from the 165 all year same day value

# Calculate the 90th percentile for each day
df_percentiles <- df_grouped %>%
  mutate(
    Percentile_90 = map_dbl(ROLLING_WINDOW_ALL_YEAR_VALUES, ~ quantile(.x, 0.90,'na.rm'=TRUE))
  )







