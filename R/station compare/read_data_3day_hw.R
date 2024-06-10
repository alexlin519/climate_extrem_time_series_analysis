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
                "../output/Kelowna_heatmap_3_dayHW.csv")
# Function to read and select necessary columns
read_and_select <- function(file_path) {
  read.csv(file_path) 
}

# Read and combine all datasets
df_line_seg_3day <- map_dfr(file_paths, read_and_select)
unique(df_line_seg_3day$station)

