library(dplyr)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)


# Define file paths
file_paths <- c("../output/YVR_heatmap_3_dayHW.csv",
                "../output/Kamloops_heatmap_3_dayHW.csv",
                "../output/Prince_George_heatmap_3_dayHW.csv",
                "../output/era5_YVR_heatmap_3_dayHW.csv",
                "../output/Kelowna_heatmap_3_dayHW.csv",
                "../output/Abbotsford_heatmap_3_dayHW.csv",
                "../output/Penticton_heatmap_3_dayHW.csv",
                "../output/FortNelson_heatmap_3_dayHW.csv")
# Function to read and select necessary columns
read_and_select <- function(file_path) {
  read.csv(file_path) 
}

# Read and combine all datasets
df_line_seg_3day <- map_dfr(file_paths, read_and_select)
unique(df_line_seg_3day$station)



file_paths_95 <- c(
  "../output/Abbotsford_heatmap_3_dayHW_95.csv",
  "../output/YVR_heatmap_3_dayHW_95.csv",
  "../output/Prince_George_heatmap_3_dayHW_95.csv",
  "../output/Kelowna_heatmap_3_dayHW_95.csv",
  "../output/FortNelson_heatmap_3_dayHW_95.csv")

# Read and combine all datasets
df_line_seg_3day_95 <- map_dfr(file_paths_95, read_and_select)
unique(df_line_seg_3day_95$station)



file_paths_EHF <- c("../output/YVR/EHF_heatmap_3_dayHW.csv",
                    "../output/Prince_George/EHF_heatmap_3_dayHW.csv",
                    "../output/Kelowna/EHF_heatmap_3_dayHW.csv",
                    "../output/Abbotsford/EHF_heatmap_3_dayHW.csv",
                    "../output/FortNelson/EHF_heatmap_3_dayHW.csv")


read_and_select_EHF <- function(file_path) {
  read.csv(file_path) 
  #%>%
   # select(LOCAL_YEAR, LOCAL_DATE, Heatwave, station,EHI_sig,EHI_accl,EHF)
  #,Heatwave_95,EHF_95,EHI_sig_95,Percentile_90,Percentile_95) 
}
# Read and combine all datasets
df_EHF_line_seg_3d <- map_dfr(file_paths_EHF, read_and_select_EHF)

#rename the columns and add _EHF to all station columns value
df_EHF_line_seg_3d <- df_EHF_line_seg_3d %>%
  rename(Year = LOCAL_YEAR) %>%
  rename(DayOfYear = LOCAL_DATE) %>%
  mutate_at(vars(station), funs(paste0(., "_EHF")))

# Use the sub() function to remove the year part from the LOCAL_DATE column
df_EHF_line_seg_3d$DayOfYear <- sub("^\\d{4}-", "", df_EHF_line_seg_3d$DayOfYear)
