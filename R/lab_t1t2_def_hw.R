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

