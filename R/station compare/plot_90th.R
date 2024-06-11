# Load necessary libraries
library(ggplot2)
library(dplyr)

base_colors <- c('#FF0000', '#008000', '#0000FF', '#000000', '#FF00FF', '#FFA500', '#FFD700', '#D2691E',
                 '#D2B48C', '#808000', '#FFFF00', '#40E0D0', '#00BFFF', '#1E90FF', '#8A2BE2', '#FF1493',
                 '#A52A2A', '#7FFF00', '#8B008B', '#FF69B4','#A9A9A9', '#696969') 

# Create a dummy year (e.g., 2000) for the purpose of plotting Month and Day
df_station <- df_station %>%
  mutate(Date = as.Date(paste("2000", Month, Day, sep = "-"), format = "%Y-%m-%d"))


# Define the colors for specific stations
station_colors <- c("Kamloops" = "red", "Prince_George" = "green", "YVR" = "blue",
                    "Kelowna" = '#FFD700', 'Penticton' = '#FF00FF', 'Abbotsford' = '#696969')

# Filter the data to include only stations that have a defined color
df_station <- df_station %>% filter(Station %in% names(station_colors))



# Create the line chart with distinct line types
three_stations_90th <- ggplot(df_station, aes(x = Date, y = Percentile_90, color = Station, group = Station)) +
  geom_line(size = 0.6) +  # Adjust line size for better visibility
  labs(title = "Percentile 90 Temperature by Station",
       x = "Date",
       y = "Percentile 90 Temperature") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = station_colors) 
 # + scale_linetype_manual(values = c("Kamloops" =
 #                                    "solid", "Prince_George" = "solid","Kelowna" = "solid",
 #                                  "YVR" = "solid"))





# Define the colors for specific stations
station_colors_filter <- c("Kamloops" = "red",
                    "Kelowna" = '#FFD700', 'Penticton' = '#FF00FF', 'Abbotsford' = '#696969')

# Filter the data to include only stations that have a defined color
filter_df_station <- df_station %>% filter(Station %in% names(station_colors_filter))



# Create the line chart with distinct line types
three_stations_90th_filter <- ggplot(filter_df_station, aes(x = Date, y = Percentile_90, color = Station, group = Station)) +
  geom_line(size = 0.6) +  # Adjust line size for better visibility
  labs(title = "Percentile 90 Temperature by Station",
       x = "Date",
       y = "Percentile 90 Temperature") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = station_colors) 
# + scale_linetype_manual(values = c("Kamloops" =
#                                    "solid", "Prince_George" = "solid","Kelowna" = "solid",
#                                  "YVR" = "solid"))






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






