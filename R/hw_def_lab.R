
library(ggplot2)
# Create a sample data frame with specific fake data to cover all scenarios
dates <- seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = 101)
max_temp <- c(
  36,37,24,                            #normal 2 day peak but no heat wave 
  35,40,36,                            #normal heat wave 0
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
  31, 32, 30, 24, 
  40, 37, 38, 30, 30,  # Mixed temperatures index 68- 72
  28.5, 29, 27, 26, 25, 28, 29, 30, 29,  # Below T2
  25, 26, 25, 32, # Normal temperatures index -85
  34, 33.5, 34, 32, 28.1, 28.1, # avg below T1, have to drop the last 3
  25, 26,36,37,24,                            #normal 2 day peak but no heat wave 
  32,40,32, 26,30   # Normal temperatures
)

df_lab <- data.frame(date = dates, max_temp = max_temp)

T1 <- 33
T2 <- 28
# Create a column to indicate if the day is part of a heat wave
df_lab$higher_t2 <- df_lab$max_temp > T2
# Initialize tracking variables
# Initialize tracking variables
current_heat_wave <- c()
any_above_T1_count <- 0
any_above_T1 <- TRUE

df_lab_avg <- df_lab
# Initialize a list to store temperatures for each window
window_temps_list <- list()
window_length <- 3  # change to set the window length

# Initialize t1_capture column in df_lab_avg
df_lab_avg$t1_capture <- FALSE
df_lab_avg$avg_above_t1 <- FALSE




# Loop for handling consecutive days
for (i in 1:nrow(df_lab_avg)) {
  
  # Define the window range for each iteration
  window_start <- max(1, i - window_length)  # start 6 days before the current day
  window_end <- min(nrow(df_lab_avg), i + window_length)  # end 3 days after the current day
  window_indices <- window_start:window_end
  
  # Get the temperatures within the window
  window_temps <- df_lab_avg$max_temp[window_indices]
  window_temps_list[[i]] <- window_temps  # Store the temperatures in the list
  print(window_temps)
  cat("Iteration:", i, "Current Temp:", df_lab_avg$max_temp[i], "Window Temps:", window_temps, "\n")
  
  # Capture logic avg
  # if (df_lab_avg$max_temp[i] > T1 && !df_lab_avg$t1_capture[i]) {
  #   # Mark the current and previous temperatures that are above t1 as TRUE
  #   df_lab_avg$t1_capture[window_indices[df_lab_avg$max_temp[window_indices] > T1]] <- TRUE
  # }
  # Capture logic
  if (df_lab_avg$max_temp[i] > T1) {
    if (sum(window_temps > T1) == 1){
      # Case 1: Only one extreme temp above T1
      # Expand to neighbors until the average temp falls below T2
      neighbor_indices <- i
      left_index <- i - 1
      right_index <- i + 1
      direction <- "left"
      
      while (TRUE) {
        if (direction == "left" && left_index > 0) {
          if (df_lab_avg$max_temp[left_index] > T2) {
            neighbor_indices <- c(neighbor_indices, left_index)
          }
          left_index <- left_index - 1
          direction <- "right"
        } else if (direction == "right" && right_index <= nrow(df_lab_avg)) {
          if (df_lab_avg$max_temp[right_index] > T2) {
            neighbor_indices <- c(neighbor_indices, right_index)
          }
          right_index <- right_index + 1
          direction <- "left"
        } else {
          break
        }
      
        # Calculate the average temp of the neighbors
        avg_temp <- mean(df_lab_avg$max_temp[neighbor_indices])
        
        # Check if the average temp is above T2
        if (avg_temp < T1) {
          break
        }
      }
        # Mark the neighbors and current point as TRUE
        df_lab_avg$t1_capture[neighbor_indices] <- TRUE
        df_lab_avg$avg_above_t1[neighbor_indices] <- TRUE
    }
    else {
      # Case 2: Multiple extreme temps above T1
      # Mark the current and previous temperatures that are above T1 as TRUE
      df_lab_avg$t1_capture[window_indices[df_lab_avg$max_temp[window_indices] > T1]] <- TRUE
    }
    }
  
  
  
  
  # Check if the current day is part of a heat wave
  if (df_lab_avg$higher_t2[i] == TRUE) {
    current_heat_wave <- c(current_heat_wave, i)
    if (df_lab_avg$max_temp[i] > T1) {
      any_above_T1 <- TRUE
      any_above_T1_count <- any_above_T1_count + 1
    }
  } else {
      if ((length(current_heat_wave) <= 3 && any_above_T1_count < 3) | !any_above_T1) {
        df_lab_avg$higher_t2[current_heat_wave] <- FALSE
      }
      current_heat_wave <- c()
      any_above_T1_count <- 0
      any_above_T1 <- FALSE
    }
}


# Add the same window period check for the last period if the loop ended during a heat wave
if (length(current_heat_wave) < 3 && any_above_T1_count < 3) {
  df_lab_avg$higher_t2[current_heat_wave] <- FALSE
}

# Add the index column for visualization
df_lab_avg$index <- 1:nrow(df_lab_avg)
ggplot(df_lab_avg, aes(x = date, y = max_temp)) +
  geom_line() +
  geom_point(aes(color = higher_t2)) +
  #geom_text(aes(label = index), vjust = -1, size = 3) +
  #geom_text(aes(label = t1_capture), vjust = -1, size = 3) +
  geom_hline(yintercept = T1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = T2, linetype = "dashed", color = "blue") +
  labs(title = "avg",
       x = "Date",
       y = "Maximum Temperature (°C)",
       color = "Heat Wave") +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
  theme_minimal()



