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
  scale_color_manual(values = c("Kamloops" = "red", "Prince_George" = "green",
                                "YVR" = "blue","YVR_era5" = 'purple',"YVR_30y_based" = 'orange')) +
 scale_linetype_manual(values = c("Kamloops" =
                                    "longdash", "Prince_George" = "longdash","YVR_era5" = "solid",
                                  "YVR" = "solid","YVR_30y_based" = "solid"))





# Filter the dataframe to include only the desired stations
desired_stations <- c("YVR", "YVR_30y_based")  # Replace with the stations you want to include
df_YVR30 <- df_station[df_station$Station %in% desired_stations, ]

# Create the plot with the filtered dataframe
two_stations_90th_yvr_30 <- ggplot(df_YVR30, aes(x = Date, y = Percentile_90, color = Station, linetype = Station)) +
  geom_line(size = 0.8,alpha = 0.7) +  # Increase the line size for better visibility
  scale_color_manual(values = c( "YVR" = "blue", "YVR_30y_based" = "orange")) +
  scale_linetype_manual(values = c( "YVR" = "solid", "YVR_30y_based" = "solid")) +
  theme_minimal() +
  labs(title = "Percentile_90 Over Time by Station",
       x = "Date",
       y = "Percentile_90",
       color = "Station",
       linetype = "Station") +
  theme(legend.position = "right")+
  scale_x_date(date_labels = "%b %d", date_breaks = "15 days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






