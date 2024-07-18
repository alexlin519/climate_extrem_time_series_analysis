# Load necessary libraries
library(tidyverse)

# Read the CSV file
file_paths <- c("../data/agri/FAOSTAT_data.csv",
                "../data/agri/FAOSTAT_data_4more.csv")
# Define the columns needed
needed_columns <- c("Element", "Item", "Year", "Unit", "Value", "Flag", "Flag.Description")
read_and_select <- function(file_path) {
  read.csv(file_path) %>%
    select(all_of(needed_columns))
}
# Read and combine all datasets
data_fao <- map_dfr(file_paths, read_and_select)
head(data_fao)
unique(data_fao$Item)
# Check the structure of the dataset
str(data_fao)

# Get a summary of the dataset
summary(data_fao)


# Check for missing values
sum(is.na(data_fao))

# Remove rows with missing values (if necessary)
data_fao <- na.omit(data_fao)


# Plotting the Value over Years for a specific Item, loop for all items
# change y aixs as unit is different for each Element Value
# Plotting the Value over Years for a specific Item, loop for all items
# change y aixs as unit is different for each Element Value

#make the for loop into function
plot_fao_data <- function(data_fao) {
  for (item in unique(data_fao$Item)) {
    plot <- ggplot(data = data_fao %>% filter(Item == item), aes(x = Year, y = Value, color = Element)) +
      geom_line() +
      labs(title = paste("Trends Over Time for", item), x = "Year", y = "Value")
    #print(plot)
    # Plotting the Value over Years for each Element (e.g., "Apples")
    options(repr.plot.width = 35, repr.plot.height = 8)  # Adjust width and height
    facet_plot <- ggplot(data = data_fao %>% filter(Item == item), aes(x = Year, y = Value)) +
      geom_line() +
      facet_wrap(~ Element, scales = "free_y") +
      labs(title = paste("Trends Over Time for", item), x = "Year", y = "Value") +
      theme_minimal()+
      scale_x_continuous(breaks = seq(min(data_fao$Year), max(data_fao$Year), by = 3))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))  # Rotate x-axis labels
    # Change 'by' to your preferred interval
    
    print(facet_plot)
  }
}


# Read the CSV file
file_paths <- c("../output/Abbotsford/EHF_heatmap_5_dayHW.csv",
                "../output/Kelowna/EHF_heatmap_5_dayHW.csv")
# Define the columns needed
#Month	Day	LOCAL_DATE	LOCAL_YEAR	STATION_NAME	MEAN_TEMPERATURE	Percentile_90	
#Percentile_95	Rolling_3d_Avg_Temp	EHI_sig	EHI_sig_95	Rolling_Avg_30Day	EHI_accl	
#EHF	EHF_95	Heatday	Heatday_95	station	streak	Heatwave	Heatwave_95
needed_columns <- c("LOCAL_DATE", "MEAN_TEMPERATURE", "Percentile_95", "EHF_95","station")
read_and_select <- function(file_path) {
  read.csv(file_path) %>%
    select(all_of(needed_columns))
}
# Read and combine all datasets
data_x <- map_dfr(file_paths, read_and_select)



#read crop production data
# Read the CSV file
file_paths <- c("../data/agri/Canola_crop.csv",
                "../data/agri/Barley_crop.csv")
# Define the columns needed
needed_columns <- c("REF_DATE", "VALUE",'GEO',"Type.of.crop")

# Function to read and select necessary columns, and add crop type
read_and_select <- function(file_path) {
  read.csv(file_path) %>%
    select(all_of(needed_columns)) %>%
    rename(Production = VALUE) %>%
    rename(Crop_Type = Type.of.crop)
}

# Read and combine all datasets
data_statcan_crop <- map_dfr(file_paths, read_and_select)

# Impute missing values with the mean of neighboring values
data_statcan_crop <- data_statcan_crop %>%
  group_by(Crop_Type,GEO) %>%
  mutate(Production = na.approx(Production, rule = 2))
















# File paths for Canola, Barley, and Fruit data
file_paths <- c("../data/agri/Fruit_prod.csv")

# Define the columns needed
#unit is ton
needed_columns <- c("REF_DATE", "VALUE", "Commodity","Estimates")

# Function to read and select necessary columns, and add crop type
read_and_select_fru <- function(file_path) {
  read_csv(file_path) %>%
    select(all_of(needed_columns))%>% 
    rename(Production = VALUE) %>%
    rename(Crop_Type = Commodity)
}


# Read and combine all datasets
data_statcan_fruit <- map_dfr(file_paths, read_and_select_fru)

#check num of missing
sum(is.na(data_statcan_fruit))


# Impute missing values with the mean of neighboring values
data_statcan_fruit <- data_statcan_fruit %>%
  group_by(Crop_Type,Estimates) %>%
  mutate(Production = na.approx(Production, rule = 2))

