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
  mutate(rolling_window_max = get_rolling_window(MAX_TEMPERATURE),
         rolling_window_mean = get_rolling_window(MEAN_TEMPERATURE))

df_wrangling <- df_wrangling %>%
  mutate(LOCAL_DATE = as.Date(LOCAL_DATE, format = "%Y-%m-%d"),
         Month = format(LOCAL_DATE, "%m"),
         Day = format(LOCAL_DATE, "%d")
  )

#head(df_wrangling)


# Drop rows until the first non-missing value in both 'maxtemp' and 'mintemp'
df_wrangling <- df_wrangling[which(!is.na(df_wrangling$MAX_TEMPERATURE) & 
                       !is.na(df_wrangling$MIN_TEMPERATURE))[1]:nrow(df_wrangling), ]

rownames(df_wrangling) <- NULL
## missing value
missing_maxtem <- which(is.na(df_wrangling$MAX_TEMPERATURE))
missing_mintem <- which(is.na(df_wrangling$MIN_TEMPERATURE))

#missing_maxtem
imx = missing_maxtem
imix = missing_mintem
print(length(imx))
#print(length(imix))

# icheckmx = sort(c(imx, imx - 1, imx + 1))
# icheckmx = unique(icheckmx)
# icheckmx
maxtmp = df_wrangling$MAX_TEMPERATURE
#Below works if first and last values are not missing
maxtemp = maxtmp
if (length(missing_maxtem) > 0) {
  for (i in missing_maxtem) {
    i1 = i - 1
    while ( is.na(maxtmp[i1])) { i1 = i1 - 1 }
    # Ensure i1 is within bounds
    if (i1 <= 0) next
    
    mx1 = maxtmp[i1]
    
    i2 = i + 1
    while ( is.na(maxtmp[i2])) { i2 = i2 + 1 }
    # Ensure i2 is within bounds
    if (i2 > length(maxtmp)) next
    
    mx2 = maxtmp[i2]
    
    # Ensure both mx1 and mx2 are valid before averaging
      maxtmp[i] <- (mx1 + mx2) / 2
    
  }
}
df_wrangling$MAX_TEMPERATURE <- maxtmp

mintmp = df_wrangling$MIN_TEMPERATURE
#Below works if first and last values are not missing
mintemp = mintmp
if (length(missing_mintem) > 0) {
  for (i in imix) {
  i1 = i - 1
  while (i1 > 0 && is.na(mintmp[i1])) { i1 = i1 - 1 }
  if (i1 <= 0) next
  mx1 = mintmp[i1]
  i2 = i + 1
  while (i2 <= length(mintmp) && is.na(mintmp[i2])) { i2 = i2 + 1 }
  if (i2 > length(mintmp)) next  # skip if no next non-NA value
  mx2 = mintmp[i2]
  # Ensure both mx1 and mx2 are valid before averaging
  if (!is.na(mx1) && !is.na(mx2)) {
    mintemp[i] <- (mx1 + mx2) / 2
  }
  #cat(mintemp[(i - 1):(i + 1)], "\n")
  }
  }
df_wrangling$MIN_TEMPERATURE <- mintemp



# Update the MEAN_TEMPERATURE column
df_wrangling <- df_wrangling %>%
  mutate(MEAN_TEMPERATURE = if_else(is.na(MEAN_TEMPERATURE), (MAX_TEMPERATURE + MIN_TEMPERATURE) / 2, MEAN_TEMPERATURE))
missing_meantem <- which(is.na(df_wrangling$MEAN_TEMPERATURE))
#missing_meantem

### get the all year for same day value, around 165 data points


# Assuming rolling_window column already exists in df_wrangling
# If not, create it first as shown previously

# Group by Month and Day, and concatenate the rolling_window lists
df_grouped <- df_wrangling %>%
  filter(year(LOCAL_DATE) >= 1961 & year(LOCAL_DATE) <= 1990) %>%
  group_by(Month, Day) %>%
  summarize(ROLLING_WINDOW_ALL_YEAR_VALUES = list(reduce(rolling_window_max, c)), .groups = 'drop')

df_grouped_all <- df_wrangling %>%
  group_by(Month, Day) %>%
  summarize(ROLLING_WINDOW_ALL_YEAR_VALUES = list(reduce(rolling_window_max, c)), .groups = 'drop')

df_grouped_mean <- df_wrangling %>%
  filter(year(LOCAL_DATE) >= 1961 & year(LOCAL_DATE) <= 1990) %>%
  group_by(Month, Day) %>%
  summarize(ROLLWIN_30YEAR_MEAN = list(reduce(rolling_window_mean, c)), .groups = 'drop')

### get 90th from the 165 all year same day value
# # # # 
# Calculate the 90th percentile for each day
df_percentiles <- df_grouped %>%
  mutate(
    Percentile_90 = map_dbl(ROLLING_WINDOW_ALL_YEAR_VALUES, ~ quantile(.x, 0.90,'na.rm'=TRUE)),
    Percentile_95 = map_dbl(ROLLING_WINDOW_ALL_YEAR_VALUES, ~ quantile(.x, 0.95,'na.rm'=TRUE))
    
  )

df_percentiles_all <- df_grouped_all %>%
  mutate(
    Percentile_90 = map_dbl(ROLLING_WINDOW_ALL_YEAR_VALUES, ~ quantile(.x, 0.90,'na.rm'=TRUE)),
    Percentile_95 = map_dbl(ROLLING_WINDOW_ALL_YEAR_VALUES, ~ quantile(.x, 0.95,'na.rm'=TRUE))
  )

df_percentiles_mean <- df_grouped_mean %>%
  mutate(
    Percentile_90 = map_dbl(ROLLWIN_30YEAR_MEAN, ~ quantile(.x, 0.90,'na.rm'=TRUE)),
    Percentile_95 = map_dbl(ROLLWIN_30YEAR_MEAN, ~ quantile(.x, 0.95,'na.rm'=TRUE)),
    Percentile_05 = map_dbl(ROLLWIN_30YEAR_MEAN, ~ quantile(.x, 0.05,'na.rm'=TRUE))
  )
#final decision is use  df_percentiles and df_percentiles_mean(for ehf, as t is DMT  daily mean temperature)


# Add a column to distinguish between the two datasets
df_percentiles <- df_percentiles %>%
  mutate(Source = "1960-1990")

df_percentiles_all <- df_percentiles_all %>%
  mutate(Source = "All Years")

# Combine the two data frames
df_90_compare <- bind_rows(df_percentiles, df_percentiles_all)

# Convert Month and Day to a Date for plotting (year is arbitrary, using 2000 as a placeholder)
df_90_compare <- df_90_compare %>%
  mutate(Date = as.Date(paste("2088", Month, Day, sep = "-")))

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

compare_plot_95 <- ggplot(df_90_compare, aes(x = Date, y = Percentile_95, color = Source)) +
  geom_line() +
  labs(title = paste(station_name ,"95th Percentile Temperature Comparison"),
       x = "Date",
       y = "95th Percentile Temperature",
       color = "Data Source") +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  theme_minimal()




### new def of ehf

#step 1: merge the 90th percentile for each day to df_wrangling
# Assuming df_percentiles and df_wrangling are your data frames

# Convert Month and Day to characters if they are factors
df_percentiles_mean$Month <- as.character(df_percentiles_mean$Month)
df_percentiles_mean$Day <- as.character(df_percentiles_mean$Day)
df_wrangling$Month <- as.character(df_wrangling$Month)
df_wrangling$Day <- as.character(df_wrangling$Day)

# Merge the data frames based on Month and Day
df_combined_ehf1 <- merge(df_wrangling, df_percentiles_mean[, c("Month", "Day", "Percentile_90","Percentile_95","Percentile_05")], by = c("Month", "Day"), all.x = TRUE)
df_combined_ehf1 <- df_combined_ehf1[, c("Month", "Day","LOCAL_DATE","LOCAL_YEAR","STATION_NAME",
                                         "MEAN_TEMPERATURE", "Percentile_90","Percentile_95","Percentile_05")]

# View the combined data frame
#head(df_combined_ehf1)



##step 2: calculate the EHF


# Load necessary libraries

# Ensure the LOCAL_DATE is in Date format
df_combined_ehf1$LOCAL_DATE <- as.Date(df_combined_ehf1$LOCAL_DATE)

# Ensure the MEAN_TEMPERATURE column is numeric
df_combined_ehf1$MEAN_TEMPERATURE <- as.numeric(df_combined_ehf1$MEAN_TEMPERATURE)

# Order the data frame by LOCAL_DATE
df_combined_ehf1 <- df_combined_ehf1 %>%
  arrange(LOCAL_DATE)

# Compute the rolling average of MEAN_TEMPERATURE for today and the next two days
df_combined_ehf1 <- df_combined_ehf1 %>%
  mutate(Rolling_3d_Avg_Temp = rollmean(MEAN_TEMPERATURE, k = 3, fill = NA, align = "left"))
#df_combined_ehf1$Rolling_3d_Avg_Temp <- format(df_combined_ehf1$Rolling_3d_Avg_Temp, scientific = FALSE)

# Calculate the difference between the rolling average and the Percentile_90
df_combined_ehf1 <- df_combined_ehf1 %>%
  mutate(EHI_sig = Rolling_3d_Avg_Temp - Percentile_90,
         EHI_sig_95 = Rolling_3d_Avg_Temp - Percentile_95,
         ECI_sig_05 = Rolling_3d_Avg_Temp - Percentile_05)

# Compute the 30-day rolling average of MEAN_TEMPERATURE
df_combined_ehf1 <- df_combined_ehf1 %>%
  mutate(Rolling_Avg_30Day = rollmean(MEAN_TEMPERATURE, k = 30, fill = NA, align = "right")%>% lag(1))

df_combined_ehf1 <- df_combined_ehf1 %>%
  mutate(EHI_accl = Rolling_3d_Avg_Temp - Rolling_Avg_30Day)

# Add the new column EHF
df_combined_ehf1 <- df_combined_ehf1 %>%
  mutate(EHF = EHI_sig * pmax(1, EHI_accl),
         EHF_95 = EHI_sig_95 * pmax(1, EHI_accl),
         ECF_05 = -ECI_sig_05 * pmin(-1,EHI_accl))
# View the updated data frame
#head(df_combined_ehf1)


# define the Heatday: if EHF < 0 for this rows and previous 2 rows, then it is not a Heatday,
# otherwise it is a Heatday                 
df_combined_ehf1 <- df_combined_ehf1 %>%
  mutate(Heatday = ifelse(EHF < 0 & lag(EHF, 1) < 0 & lag(EHF, 2) < 0, "No", "Yes"),
         Heatday_95 = ifelse(EHF_95 < 0 & lag(EHF_95, 1) < 0 & lag(EHF_95, 2) < 0, "No", "Yes"),
         Coldday = ifelse(ECF_05 < 0 & lag(ECF_05, 1) < 0 & lag(ECF_05, 2) < 0, "Yes", "No"))



## step 3: aggregate to monthly maxima
## aggregate to monthly maxima
# Aggregate to monthly maxima in terms of EHF
monthly_max_EHF <- df_combined_ehf1 %>%
  group_by(Month, LOCAL_YEAR) %>%
  summarize(max_EHF = max(EHF, na.rm = TRUE),
            max_EHF_95 = max(EHF_95, na.rm = TRUE),
            min_ECF_05 = min(ECF_05, na.rm = TRUE))
monthly_max_EHF <- monthly_max_EHF %>%
  mutate(Station = station_name)



# Convert NA values to 'no'
df_combined_ehf1$Heatday[is.na(df_combined_ehf1$Heatday)] <- "No"
df_combined_ehf1$Heatday_95[is.na(df_combined_ehf1$Heatday_95)] <- "No"
df_combined_ehf1$Coldday[is.na(df_combined_ehf1$Coldday)] <- "No"

#add a column call station
df_combined_ehf1$station <- station_name

heat_wave_length <- 3

# Identify streaks and reclassify
EHF_melted_temp_3_day <- df_combined_ehf1 %>%
  group_by(LOCAL_YEAR) %>%
  mutate(streak = with(rle(Heatday == "Yes"), rep(lengths, lengths))) %>%
  mutate(Heatday = ifelse(Heatday == "Yes" & streak < heat_wave_length, "too_short", Heatday)) %>%
  mutate(Heatwave = ifelse(Heatday == "Yes" & streak >= heat_wave_length, "Heatwave", Heatday)) %>%
  ungroup()
EHF_melted_temp_3_day <- EHF_melted_temp_3_day %>%
  group_by(LOCAL_YEAR) %>%
  mutate(streak = with(rle(Heatday_95 == "Yes"), rep(lengths, lengths))) %>%
  mutate(Heatday_95 = ifelse(Heatday_95 == "Yes" & streak < heat_wave_length, "too_short", Heatday_95)) %>%
  mutate(Heatwave_95 = ifelse(Heatday_95 == "Yes" & streak >= heat_wave_length, "Heatwave", Heatday_95)) %>%
  ungroup()

EHF_ECF_melted_temp_3_day <- EHF_melted_temp_3_day %>%
  group_by(LOCAL_YEAR) %>%
  mutate(streak = with(rle(Coldday == "Yes"), rep(lengths, lengths))) %>%
  mutate(Coldday = ifelse(Coldday == "Yes" & streak < heat_wave_length, "too_short", Coldday)) %>%
  mutate(ColdSpell_05 = ifelse(Coldday == "Yes" & streak >= heat_wave_length, "ColdSpell", Coldday)) %>%
  ungroup()

# Create the folder with the station name
output_dir <- paste0("../output/", station_name)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Save the updated dataframe to a file in the created folder
save_path <- paste0(output_dir, "/EHF_heatmap_3_dayHW.csv")
write.csv(EHF_ECF_melted_temp_3_day, save_path, row.names = FALSE)


heat_wave_length <- 5
# Identify streaks and reclassify
EHF_melted_temp_5_day <- df_combined_ehf1 %>%
  group_by(LOCAL_YEAR) %>%
  mutate(streak = with(rle(Heatday == "Yes"), rep(lengths, lengths))) %>%
  mutate(Heatday = ifelse(Heatday == "Yes" & streak < heat_wave_length, "too_short", Heatday)) %>%
  mutate(Heatwave = ifelse(Heatday == "Yes" & streak >= heat_wave_length, "Heatwave", Heatday)) %>%
  ungroup()

EHF_melted_temp_5_day <- EHF_melted_temp_5_day %>%
  group_by(LOCAL_YEAR) %>%
  mutate(streak = with(rle(Heatday_95 == "Yes"), rep(lengths, lengths))) %>%
  mutate(Heatday_95 = ifelse(Heatday_95 == "Yes" & streak < heat_wave_length, "too_short", Heatday_95)) %>%
  mutate(Heatwave_95 = ifelse(Heatday_95 == "Yes" & streak >= heat_wave_length, "Heatwave", Heatday_95)) %>%
  ungroup()

EHF_ECF_melted_temp_5_day <- EHF_melted_temp_5_day %>%
  group_by(LOCAL_YEAR) %>%
  mutate(streak = with(rle(Coldday == "Yes"), rep(lengths, lengths))) %>%
  mutate(Coldday = ifelse(Coldday == "Yes" & streak < heat_wave_length, "too_short", Coldday)) %>%
  mutate(ColdSpell_05 = ifelse(Coldday == "Yes" & streak >= heat_wave_length, "ColdSpell", Coldday)) %>%
  ungroup()

# Create the folder with the station name
output_dir <- paste0("../output/", station_name)
#dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
# Save the updated dataframe to a file in the created folder
save_path <- paste0(output_dir, "/EHF_heatmap_5_dayHW.csv")
write.csv(EHF_ECF_melted_temp_5_day, save_path, row.names = FALSE)


# Year station  DayOfYear
