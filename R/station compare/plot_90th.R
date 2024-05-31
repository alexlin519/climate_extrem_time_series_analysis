# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create a dummy year (e.g., 2000) for the purpose of plotting Month and Day
df_station <- df_station %>%
  mutate(Date = as.Date(paste("2000", Month, Day, sep = "-"), format = "%Y-%m-%d"))

# Create the line chart with distinct line types
three_stations_90th <- ggplot(df_station, aes(x = Date, y = Percentile_90, color = Station, linetype = Station, group = Station)) +
  geom_line(size = 0.6) +  # Adjust line size for better visibility
  labs(title = "Percentile 90 Temperature by Station",
       x = "Date",
       y = "Percentile 90 Temperature") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Kamloops" = "red", "Prince_George" = "green", "YVR" = "blue","YVR_era5" = 'purple')) +
 scale_linetype_manual(values = c("Kamloops" =
                                    "solid", "Prince_George" = "solid","YVR_era5" = "solid",
                                  "YVR" = "solid"))




