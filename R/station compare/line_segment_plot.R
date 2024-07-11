library(ggplot2)
library(dplyr)
library(ggrepel)
library(tidyr)
if (!require(gridExtra)) {
  install.packages("gridExtra")
}
library(gridExtra)

line_segment_data_prepare <- function(df_filtered_line_seg, station_only = NULL) {
  # if exist Heatwave_95
  if ("Heatwave_95" %in% colnames(df_filtered_line_seg)) {
    df_filtered_line_seg <- df_filtered_line_seg[ (df_filtered_line_seg$Heatwave_95) == "Heatwave",]
    return(df_filtered_line_seg)
  } else {
    df_filtered_line_seg <- df_filtered_line_seg[ (df_filtered_line_seg$Heatwave) == "Heatwave",]
  }
  #df_filtered_line_seg <- df_line_seg[!is.na(df_line$daily_max_temp),]
  
  
  #####
  #read and combine yvr 30 yeas based data!!
  #####
  # Select only the necessary columns for plotting
  df_filtered_plot <- df_filtered_line_seg %>%
    select(DayOfYear, Year, station,daily_max_temp,Percentile_90)
  
  # Specify the path to your zip file
  txt_file <- "../data/dates_heatwave.csv"
  df_30yvr_hw_3day <- read.csv(txt_file, header = TRUE, sep = ",")  # Using sep = "" to handle any amount of whitespace
  print(head(df_30yvr_hw_3day))
  

  
  # Convert the begin and end columns to Date type
  df_30yvr_hw_3day <- df_30yvr_hw_3day %>%
    mutate(begin = as.Date(begin),
           end = as.Date(end))
  
  # Generate a sequence of dates for each row and expand the dataframe
  df_expanded <- df_30yvr_hw_3day %>%
    rowwise() %>%
    mutate(dates = list(seq(begin, end, by = "day"))) %>%
    unnest(dates)
  df_expanded <- df_expanded %>%
    filter(format(dates, "%m-%d") >= "04-01" & format(dates, "%m-%d") <= "10-31")
  
  # Create the new dataframe with Year and DayOfYear columns
  
  # Create the new dataframe with Year, DayOfYear, and Station columns
  df_30yvr_hw_3day_new <- df_expanded %>%
    mutate(Year = as.numeric(format(dates, "%Y")),
           DayOfYear = format(dates, "%m-%d"),
           station = "YVR_30y_based") %>%
    select(Year, DayOfYear, station)
  
  
  
  
  #####
  # final data combined, ready for plot
  #####
  df_filtered_plot <- bind_rows(df_filtered_plot, df_30yvr_hw_3day_new)
  print(" head df_filtered_plot")
  #print(head(df_filtered_plot))
  print("df_filtered_plot")
  #print((df_filtered_plot))
  
  # Filter the dataframe based on the desired criteria
  df_filtered_plot <- df_filtered_plot %>%
    filter(!is.na(station)) %>%
    filter(station %in% station_only)
  #head(df_filtered_plot)
  
  
  return(df_filtered_plot)
  
}

line_segment_EHF_data_prepare <- function(df_filtered_line_seg, station_only = NULL) {
  df_filtered_line_seg_95 <- df_filtered_line_seg[ (df_filtered_line_seg$Heatwave_95) == "Heatwave",]
  df_filtered_line_seg <- df_filtered_line_seg[ (df_filtered_line_seg$Heatwave) == "Heatwave",]
  
  
  # Filter the dataframe based on the desired criteria
  df_filtered_plot <- df_filtered_line_seg %>%
    filter(!is.na(station)) %>%
    filter(station %in% station_only)
  
  df_filtered_plot_95 <- df_filtered_line_seg_95 %>%
    filter(!is.na(station)) %>%
    filter(station %in% station_only)
  
  return(list(df_filtered_plot, df_filtered_plot_95))
  
}

# Function to create year ranges
create_year_ranges <- function(start_year, end_year, num_splits) {
  years <- seq(start_year, end_year)
  split_size <- ceiling(length(years) / num_splits)
  year_splits <- split(years, ceiling(seq_along(years) / split_size))
  lapply(year_splits, function(x) seq(min(x), max(x)))
}

# Function to prepare the data
wrangle_data <- function(df, year_range) {
  df_year_range <- df %>% filter(Year %in% year_range)
  df_year_range <- df_year_range %>%
    mutate(Year = as.factor(Year),
           StationYear = paste(Year, station, sep = "_"),
           YearFactor = factor(Year, levels = unique(Year))) %>%
    arrange(Year, station)
  
  hline_data <- df_year_range %>%
    group_by(Year) %>%
    filter(row_number() == 1) %>%
    mutate(y_position = as.numeric(factor(StationYear, levels = unique(df_year_range$StationYear))) - 0.5)
  
  list(df_year_range = df_year_range, hline_data = hline_data)
}

# Function to generate the plot
generate_plot <- function(day_title,df_year_range, hline_data, year_range, station_colors) {
  ggplot(df_year_range, aes(x = DayOfYear, y = StationYear, fill = station)) +
    geom_tile(aes(width = 0.9999, height = 0.9999), alpha = 0.75) +
    geom_hline(data = hline_data, size = 0.5, aes(yintercept = as.numeric(y_position)), color = "black", linetype = "solid") +
    labs(title = paste("At least ",day_title, " heatwave from", min(year_range), "to", max(year_range)),
         x = "Day Of Year",
         y = "Year") +
    scale_fill_manual(
      #breaks = c("YVR", "Prince_George", "Penticton", "Kelowna", "Kamloops", "Abbotsford", "YVR_30y_based"),
      breaks <- names(station_colors),
      values = station_colors
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(size = 8, hjust = 1.5, vjust = -1.8, margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.text.x = element_text(size = 6, angle = 45, vjust = 1.6, 
                                     hjust = 1, margin = margin(t = 5, r = 0, b = 0, l = 0))) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)]) +
    scale_y_discrete(labels = function(x) {
      years <- sub("_.*", "", x)
      unique_years <- !duplicated(years)
      labels <- ifelse(unique_years, years, "")
      labels
    })
}

# Main function to generate temperature plots
generate_temperature_plots <- function(day_title,df, year_ranges, station_colors) {
  for (year_range in year_ranges) {
    data <- wrangle_data(df, year_range)
    plot <- generate_plot(day_title,data$df_year_range, data$hline_data, year_range, station_colors)
    print(plot)
  }
  return(invisible(data$hline_data))
}









# Function to generate the plot
generate_plot_EHF  <- function(day_title,df_year_range, hline_data, year_range, station_colors) {
  ggplot(df_year_range, aes(x = DayOfYear, y = StationYear, fill = station)) +
    geom_tile(aes(width = 0.9999, height = 0.9999), alpha = 0.75) +
    geom_hline(data = hline_data, size = 0.5, aes(yintercept = as.numeric(y_position)), color = "black", linetype = "solid") +
    labs(title = paste("At least ",day_title, " heatwave from", min(year_range), "to", max(year_range)),
         x = "Day Of Year",
         y = "Year") +
    scale_fill_manual(
      #breaks = c("YVR", "Prince_George", "Penticton", "Kelowna", "Kamloops", "Abbotsford", "YVR_30y_based"),
      breaks <- names(station_colors),
      values = station_colors
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(size = 14, hjust = 1.5, vjust = -1.8, margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.text.x = element_text(size = 9, angle = 45, vjust = 1.6, 
                                     hjust = 1, margin = margin(t = 5, r = 0, b = 0, l = 0))) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3)]) +
    scale_y_discrete(labels = function(x) {
      years <- sub("_.*", "", x)
      unique_years <- !duplicated(years)
      labels <- ifelse(unique_years, years, "")
      labels
    })
}

# Main function to generate temperature plots
generate_temperature_plots_EHF <- function(day_title,df, year_ranges, station_colors) {
  for (year_range in year_ranges) {
    data <- wrangle_data(df, year_range)
    plot <- generate_plot_EHF(day_title,data$df_year_range, data$hline_data, year_range, station_colors)
    print(plot)
  }
  return(invisible(data$hline_data))
}




###
# 2 station compare
###

# Function to generate the plot
generate_plot_2station <- function(day_title,df_year_range, hline_data, year_range, station_colors) {
  ggplot(df_year_range, aes(x = DayOfYear, y = StationYear, fill = station)) +
    geom_tile(aes(width = 0.9999, height = 0.9999), alpha = 0.75) +
    geom_hline(data = hline_data, size = 0.5, aes(yintercept = as.numeric(y_position)), color = "black", linetype = "solid") +
    labs(title = paste("At least ",day_title, " heatwave from", min(year_range), "to", max(year_range)),
         x = "Day Of Year",
         y = "Year") +
    scale_fill_manual(
      #breaks = c("YVR", "Prince_George", "Penticton", "Kelowna", "Kamloops", "Abbotsford", "YVR_30y_based"),
      breaks <- names(station_colors),
      values = station_colors
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          #axis.text.y = element_text(size = 14, hjust = 1.5, vjust = -1.8, margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.text.x = element_text(size = 6, angle = 45, vjust = 1.6, 
                                     hjust = 1, margin = margin(t = 5, r = 0, b = 0, l = 0))) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 3)]) +
    scale_y_discrete(labels = function(x) {
      years <- sub("_.*", "", x)
      unique_years <- !duplicated(years)
      labels <- ifelse(unique_years, years, "")
      labels
        }
    )
}

# Main function to generate temperature plots
generate_temperature_plots_2station <- function(day_title,df, year_ranges, station_colors) {
  for (year_range in year_ranges) {
    data <- wrangle_data(df, year_range)
    plot <- generate_plot_2station(day_title,data$df_year_range, data$hline_data, year_range, station_colors)
    print(plot)
  }
  return(invisible(data$hline_data))
}


# Function to plot EHF and 90th Percentile Data
compare_ehf_and_percentile_specific <- function(data, check_year, check_station) {
  # Filter the data for the specified year
  data_year <- data %>% filter(Year == check_year)
  
  # Separate data for stations with EHF and 90th percentile columns
  data_ehf <- data_year %>% filter(station == paste(check_station, "_EHF", sep = ""))
  data_90percentile <- data_year %>% filter(station == check_station)
  
  # Function to adjust date breaks
  adjust_date_breaks <- function(dates) {
    date_range <- as.integer(difftime(max(dates), min(dates), units = "days"))
    if (date_range <= 14) {
      return("1 day")
    } else if (date_range <= 30) {
      return("1 day")
    } else if (date_range <= 90) {
      return("2 days")
    } else if (date_range <= 180) {
      return("3 days")
    } else {
      return("5 days")
    }
  }
  
  # Adjust date breaks
  date_breaks <- adjust_date_breaks(as.Date(paste(data_ehf$Year, data_ehf$DayOfYear, sep = "-"), "%Y-%m-%d"))
  
  # Create a grouping variable for continuous date ranges
  data_ehf <- data_ehf %>% 
    mutate(date = as.Date(paste(Year, DayOfYear, sep = "-"), "%Y-%m-%d")) %>%
    arrange(date) %>%
    mutate(group = cumsum(c(0, diff(date) != 1)))
  
  data_90percentile <- data_90percentile %>%
    mutate(date = as.Date(paste(Year, DayOfYear, sep = "-"), "%Y-%m-%d")) %>%
    arrange(date) %>%
    mutate(group = cumsum(c(0, diff(date) != 1)))
  
  # Plot for stations with EHF data (EHI_sig, EHI_accl, EHF)
  left_plot <- ggplot(data_ehf, aes(x = date, group = group)) +
    geom_line(aes(y = EHI_sig, color = "EHI_sig")) +
    geom_line(aes(y = EHI_accl, color = "EHI_accl")) +
    # add a y = 1 line for EHF
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    geom_line(aes(y = EHF, color = "EHF")) +
    labs(title = paste("EHF Data for", check_station, "in", check_year),
         x = "Date",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_date(date_breaks = date_breaks, date_labels = "%b %d") +
    scale_color_manual(values = c("EHI_sig" = "blue", "EHI_accl" = "green", "EHF" = "red"))
  
  # Adjust date breaks for 90th percentile data
  date_breaks_90 <- adjust_date_breaks(as.Date(paste(data_90percentile$Year, data_90percentile$DayOfYear, sep = "-"), "%Y-%m-%d"))
  
  # Plot for stations with 90th percentile data (daily_max_temp, Percentile_90)
  right_plot <- ggplot(data_90percentile, aes(x = date, group = group)) +
    geom_line(aes(y = daily_max_temp, color = "daily_max_temp")) +
    geom_line(aes(y = Percentile_90, color = "Percentile_90")) +
    labs(title = paste("Max Temp and 90th Percentile for", check_station, "in", check_year),
         x = "Date",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_date(date_breaks = date_breaks_90, date_labels = "%b %d") +
    scale_color_manual(values = c("daily_max_temp" = "blue", "Percentile_90" = "green"))+
    scale_y_continuous(breaks = seq(min(c(data_90percentile$daily_max_temp, data_90percentile$Percentile_90), na.rm = TRUE),
                                    max(c(data_90percentile$daily_max_temp, data_90percentile$Percentile_90), na.rm = TRUE),
                                    by = 1))  # Adjust the step size as needed
  # Set the plot dimensions
  options(repr.plot.width = 14, repr.plot.height = 7)  # Adjust the width and height as needed
  
  # Assuming left_plot and right_plot are your ggplot objects
  combined_plot <- grid.arrange(left_plot, right_plot, ncol = 2)
  
  # Return the combined plot
  return(combined_plot)
}