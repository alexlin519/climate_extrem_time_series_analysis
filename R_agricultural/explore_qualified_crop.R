# Load necessary libraries
library(tidyverse)
library(zoo)

# Read the CSV file
file_paths <- c("../data/agri/veg_data.csv")
# Define the columns needed
# Define the columns needed
needed_columns <- c("REF_DATE", "Estimates","VALUE","Commodity","UOM")
read_and_select <- function(file_path) {
  read.csv(file_path) %>%
    select(all_of(needed_columns))%>% 
    rename(Crop_Type = Commodity)
}
# Read and combine all datasets
data_veg <- map_dfr(file_paths, read_and_select)
#na
sum(is.na(data_veg$VALUE))
# Impute missing values with the mean of neighboring values
data_veg <- data_veg %>%
  group_by(Crop_Type,Estimates) %>%
  mutate(VALUE = na.approx(VALUE, rule = 2))
sum(is.na(data_veg$VALUE))

# Clean the Crop_Type column by removing "Fresh" and the numbers
data_veg$Crop_Type <- gsub("Fresh ", "", data_veg$Crop_Type)
data_veg$Crop_Type <- gsub("\\s*\\[.*\\]", "", data_veg$Crop_Type)


# Summarize the data by Crop_Type to get the year range
veg_year<- data_veg %>%
  group_by(Crop_Type) %>%
  summarize(Start_Year = min(REF_DATE), End_Year = max(REF_DATE))
veg_year



full_veg <- data_veg




# Define the columns needed
needed_columns <- c("REF_DATE", "Harvest.disposition","VALUE",'GEO',"Type.of.crop","UOM")

# Function to read and select necessary columns, and add crop type
read_and_select <- function(file_path) {
  read.csv(file_path) %>%
    select(all_of(needed_columns)) %>%
    #rename(Production = VALUE) %>%
    rename(Crop_Type = Type.of.crop)
}

# Read and combine all datasets
data_statcan_crop <- map_dfr(file_paths, read_and_select)


#filter Harvest.disposition to be Average yield (kilograms per hectare)
crop_yield <- data_statcan_crop %>% filter(Harvest.disposition == "Average yield (kilograms per hectare)")
total_produ <- data_statcan_crop %>% filter(Harvest.disposition == "Production (metric tonnes)")

#rename
crop_yield <- crop_yield %>% rename(yield = VALUE)
total_produ <- total_produ %>% rename(Production = VALUE)
# Impute missing values with the mean of neighboring values
crop_yield <- crop_yield %>%
  group_by(Crop_Type,GEO) %>%
  mutate(yield = na.approx(yield, rule = 2))

total_produ <- total_produ %>%
  group_by(Crop_Type,GEO) %>%
  mutate(Production = na.approx(Production, rule = 2))







# Convert the data types
data_statcan_fruit$REF_DATE <- as.integer(data_statcan_fruit$REF_DATE)
data_statcan_fruit$VALUE <- as.numeric(data_statcan_fruit$VALUE)

#check num of missing
sum(is.na(data_statcan_fruit))







# Separate the production and area data
production_data <- subset(data_statcan_fruit, Estimates == "Marketed production")
area_data <- subset(data_statcan_fruit, Estimates == "Cultivated area, total")

# Merge production and area data by year and crop type
#merged_data <- merge(production_data, area_data, by = c("REF_DATE", "Crop_Type"))
merged_data <- merge(production_data, area_data, by = c("REF_DATE", "Crop_Type"), all.x = TRUE, suffixes = c("_Production", "_Area")) # change to ensure missing area data results in NA

# Rename the columns for clarity
colnames(merged_data) <- c("Year", "Crop_Type", "Production_Tons", "Production_Estimates", "Production_UOM", "Area_Hectares", "Area_Estimates", "Area_UOM")

# Calculate the yield in kilograms per hectare
merged_data$Production_Kg <- merged_data$Production_Tons * 1000
merged_data$Yield_kg_per_ha <- merged_data$Production_Kg / merged_data$Area_Hectares

# Select relevant columns
final_data <- merged_data[, c("Year", "Crop_Type", "Production_Tons",
                              "Production_Kg", "Area_Hectares", "Yield_kg_per_ha")]





