library(tibble)
library(dplyr)
compute_cumulative_precip <- function(file_path_temp_precip, station_name) {
  # Load the required dataset
  load(file_path_temp_precip)
  
  # Remove the first 59 days
  # iomit <- 1:59
  # yvr2 <- yvr[-iomit,]
  yvr2 <- yvr[!is.na(yvr$totprec), ]
  
  # Handle precipitation data
  precip <- yvr2$totprec
  precip[is.na(precip)] <- 0
  
  # Accumulate next 3 days and 5-days
  n <- length(precip)
  cum_3day <- rep(0, n)
  cum_5day <- rep(0, n)
  
  for (i in 1:(n-2)) {
    cum_3day[i] <- sum(precip[i:(i+2)])
  }
  for (i in 1:(n-4)) {
    cum_5day[i] <- sum(precip[i:(i+4)])
  }
  
  cum_3day[(n-1):n] <- cum_3day[n-2]
  cum_5day[(n-3):n] <- cum_5day[n-4]
  
  # Add calculated fields to the dataframe
  yvr2$yrmon <- 100 * yvr2$year + yvr2$month
  yvr2$cum_3day <- cum_3day
  yvr2$cum_5day <- cum_5day
  
  # Summarize by month

  byMonth <- as_tibble(yvr2) %>%
    dplyr::group_by(yrmon) %>%
    dplyr::summarise(max_3day = max(cum_3day), max_5day = max(cum_5day))
  
  # Define the specific path where you want to save the data (replace with your actual path)
  save_path <- "../drought_code_yvr/data"
  # Construct the file name with station name
  file_name <- paste0(station_name, "_max5day_precip_bymonth.csv")
  # Create the full path to the file using file.path()
  full_file_path <- file.path(save_path, file_name)
  
  # Save the data and missing information
  write.csv(file = full_file_path, byMonth, row.names = FALSE)
  return(full_file_path)
}


# 
# # 5 day and 3-day precipaition
# # YVR has no precipotation data for first two months of 1937: 59 days
# 
# load("yvr-temp-precip.RData")
# 
# names(yvr)
# #[1] "year"     "month"    "yyyymmdd" "mintemp"  "meantemp" "maxtemp"  "totprec" 
# 
# #======================================================================
# 
# iomit = 1:59
# yvr2 = yvr[-iomit,]
# # first look at distribution of precipitation over days
# precip = yvr2$totprec
# 
# summary(precip)
# #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# #  0.000   0.000   0.000   3.098   3.400  91.600      63
# 
# precip[is.na(precip)] = 0
# #summary(precip)
# #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# #  0.000   0.000   0.000   3.092   3.400  91.600 
# 
# #length(precip)
# # 31819
# 
# # percent ==0
# #sum(precip==0)
# # 17564   17564/31819 = 0.552
# 
# # percent <=1 mm (check that unit is mm) (true for German data)
# 
# #sum(precip<=1)
# # 20639    20639 / 31819 = 0.648
# 
# #======================================================================
# 
# # Accumulate next 3 days and 5-days
# # *** Note that loop starts from last day
# 
# n = length(precip)
# cum_3day = rep(0,n)
# cum_5day = rep(0,n)
# for(i in 1:(n-2)) { cum_3day[i] = sum(precip[i:(i+2)]) }
# for(i in 1:(n-4)) { cum_5day[i] = sum(precip[i:(i+4)]) }
# cum_3day[(n-1):n] = cum_3day[n-2]
# cum_5day[(n-3):n] = cum_5day[n-4]
# 
# yvr2$yrmon = 100*yvr2$year+yvr2$mont
# yvr2$cum_3day = cum_3day
# yvr2$cum_5day = cum_5day
# 
# library(tibble)
# library(dplyr)
# library(magrittr)
# byMonth = as_tibble(yvr2) %>% 
#    dplyr::group_by(yrmon) %>%
#    dplyr::summarise(max_3day=max(cum_3day), max_5day=max(cum_5day))
# 
# head(byMonth)
# #   yrmon max_3day max_5day
# #   <dbl>    <dbl>    <dbl>
# #1 193703     17       19.3
# #2 193704     41.9     55.4
# #3 193705     20.3     27.2
# #4 193706     55.1     69.1
# #5 193707      0.8      0.8
# #6 193708     21.8     25.7
# 
# write.csv(file="yvr-max5day-precip-bymonth.csv", byMonth, row.names=F)
# 
