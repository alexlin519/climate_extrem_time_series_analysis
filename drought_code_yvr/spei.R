
library(SPEI) 
library(lubridate)
library(tibble)
library(dplyr)
library(magrittr)
# create SPEI time series for different stations
load_dataframe <- function(path) {
  # Load all objects from the specified .RData file
  load(path)
  
  # Get all objects currently in the environment
  objs <- ls()
  
  # Identify the object that is a dataframe
  df_name <- objs[sapply(objs, function(x) is.data.frame(get(x)))]
  # Return the dataframe by name
  if (length(df_name) == 1) {
    return(get(df_name))
  } else {
    stop("There is either no dataframe or more than one dataframe in the .RData file.")
  }
}



calculate_monthly_aggregates <- function(station_data, lat, end_month) {
  df_drop_toprow <- station_data[-(1:59),]
  station_monthly <- as_tibble(df_drop_toprow) %>%
    dplyr::group_by(yrmon) %>%
    dplyr::summarise(
      month_totprec = sum(totprec),
      month_meantemp = mean(meantemp)) %>%
    mutate(PET = thornthwaite(Tave = month_meantemp, lat = lat)) %>%
    mutate(BAL = month_totprec - PET) %>%
    filter(yrmon <= end_month)
  
  return(station_monthly)
}
calculate_spei <- function(monthly_data, start_year, end_year) {
  # Convert to time series
  station_bal <- ts(monthly_data, start = c(start_year, 3), end = c(end_year, 12), frequency = 12)
  
  # Calculate SPEI
  station_spei <- spei(station_bal[,"BAL"], scale = 12, distribution = "log-Logistic", 
                       ref.start = c(start_year, 3), ref.end = c(end_year - 1, 12), na.rm = TRUE)
  
  # Create a dataframe for fitted SPEI values
  station_spei_df <- data.frame(fitted = station_spei$fitted)
  station_spei_df$date <- as.Date(paste0(floor(time(station_spei$fitted)), "-", 
                                         sprintf("%02d", 
                                                 1 + round((time(station_spei$fitted) - floor(time(station_spei$fitted))) * 12)), "-01"))
  results <- list(station_spei_df, station_spei)
  return(results)
}
save_station_results <- function(station_name, monthly_spei_data, bal_ts,spei_model_result) {
  full_file_path <- paste0("../output/Rdata/",  station_name, "_spei.RData")
  save(file = full_file_path, monthly_spei_data, spei_fitted,spei_model_result)
  return(full_file_path)
}
calculate_spei_all <- function(station_data, station_name, lat,start_year, end_year, end_month) {
  # Step 2: Calculate monthly aggregates and PET/BAL
  station_monthly <- calculate_monthly_aggregates(station_data, lat, end_month)
  # Step 3: Calculate SPEI
  results <- calculate_spei(station_monthly, start_year, end_year)
  station_spei_df <- results[[1]]
  spei_model_result <- results[[2]]
  # Step 4: Save results
  save_path <- save_station_results(station_name, station_monthly, station_spei_df,spei_model_result)
  results <- list(station_monthly, station_spei_df)
  return(results) #save_path
}


# Example usage
#for yvr
imputed_df <- load_dataframe("../output/Rdata/YVR_imputed.RData")
results <- calculate_spei_all(imputed_df, "YVR", 49.195, 1937,2023, 202301)
spei_monthly <- results[[1]]
spei_ts <- results[[2]]
#for abbotsford
imputed_df <- load_dataframe("../output/Rdata/Abbotsford_imputed.RData")
results <- calculate_spei_all(imputed_df, "Abbotsford", 49.31, 1937,2023, 202301)
spei_monthly <- results[[1]]
spei_ts <- results[[2]]
# for Fornelson
imputed_df <- load_dataframe("../output/Rdata/FortNelson_imputed.RData")
results <- calculate_spei_all(imputed_df, "FortNelson",58.617, 1937,2023, 202301)
spei_monthly <- results[[1]]
spei_ts <- results[[2]]
#for prince george
imputed_df <- load_dataframe("../output/Rdata/Prince_George_imputed.RData")
results <- calculate_spei_all(imputed_df, "Prince_George", 54.115, 1937,2023, 202301)
spei_monthly <- results[[1]]
spei_ts <- results[[2]]
#for kelowna
imputed_df <- load_dataframe("../output/Rdata/Kelowna_imputed.RData")
results <- calculate_spei_all(imputed_df, "Kelowna", 50.05, 1937,2023, 202301)
spei_monthly <- results[[1]]
spei_ts <- results[[2]]

