library(dplyr)
library(lubridate)
library(zoo)
library(purrr)
library(tidyr)

# Define file paths
file_paths <- c("../output/YVR_heatmap_5_dayHW.csv",
                "../output/Kamloops_heatmap_5_dayHW.csv",
                "../output/Prince_George_heatmap_5_dayHW.csv",
                "../output/era5_YVR_heatmap_5_dayHW.csv",
                "../output/Kelowna_heatmap_5_dayHW.csv",
                "../output/Abbotsford_heatmap_5_dayHW.csv",
                "../output/Penticton_heatmap_5_dayHW.csv",
                "../output/FortNelson_heatmap_5_dayHW.csv")
# Function to read and select necessary columns
read_and_select <- function(file_path) {
  read.csv(file_path) 
}

# Read and combine all datasets
df_line_seg_5day <- map_dfr(file_paths, read_and_select)
