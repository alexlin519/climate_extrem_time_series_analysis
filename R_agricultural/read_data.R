# Load necessary libraries
library(tidyverse)
library(zoo)

###-------FAO data----------------
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

###------------EHF data ----------------
# Read the CSV file
file_paths <- c("../output/Abbotsford/EHF_heatmap_5_dayHW.csv",
                "../output/Kelowna/EHF_heatmap_5_dayHW.csv",
                "../output/FortStJoh/EHF_heatmap_5_dayHW.csv")
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


###------------new crop data ----------------
#read crop production data
# Read the CSV file
# file_paths <- c("../data/agri/Canola_crop.csv",
#                 "../data/agri/Barley_crop.csv")
#file_paths <- c("../data/agri/Field_Crops_Data.csv")
file_paths <- c("../data/agri/FieldCropsEstimates.csv")

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







###------------new fruit data ----------------

# File paths for Canola, Barley, and Fruit data
file_paths <- c("../data/agri/Fruit_prod.csv",
                "../data/agri/Fruit_area.csv")

# Define the columns needed
#unit is ton
needed_columns <- c("REF_DATE", "VALUE", "Commodity","Estimates","UOM")

# Function to read and select necessary columns, and add crop type
read_and_select_fru <- function(file_path) {
  read_csv(file_path) %>%
    select(all_of(needed_columns))%>% 
    rename(Crop_Type = Commodity)
}

# Read and combine all datasets
data_statcan_fruit <- map_dfr(file_paths, read_and_select_fru)



# Convert the data types
data_statcan_fruit$REF_DATE <- as.integer(data_statcan_fruit$REF_DATE)
data_statcan_fruit$VALUE <- as.numeric(data_statcan_fruit$VALUE)

#check num of missing
sum(is.na(data_statcan_fruit))







# Impute missing values with the mean of neighboring values
data_statcan_fruit <- data_statcan_fruit %>%
  group_by(Crop_Type,Estimates) %>%
  mutate(VALUE = na.approx(VALUE, rule = 2))

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
# Clean the Crop_Type column by removing "Fresh" and the numbers
final_data$Crop_Type <- gsub("Fresh ", "", final_data$Crop_Type)
final_data$Crop_Type <- gsub("\\s*\\[.*\\]", "", final_data$Crop_Type)

full_fruits <- final_data


###------------potato only data ----------------
# File paths for Potato data
file_paths <- c("../data/agri/Potato_Data.csv")

# Define the columns needed
needed_columns <- c("REF_DATE", "VALUE","UOM")

# Function to read and select necessary columns, and add crop type
read_and_select_pot <- function(file_path) {
  read_csv(file_path) %>%
    select(all_of(needed_columns))
}

# Read and combine all datasets
data_pot <- map_dfr(file_paths, read_and_select_pot)

#create new column called crop type
data_pot$Crop_Type <- "Potato"
#plot
ggplot(data = data_pot, aes(x = REF_DATE, y = VALUE)) +
  geom_line() +
  labs(title = "Trends Over Time for Potato", x = "Year", y = "Value")

# moving avg 5 year window
pot_soomth <- data_pot %>%
  group_by(Crop_Type) %>%
  mutate(VALUE = rollmean(VALUE, k = 5, fill = NA))
ggplot(data = pot_soomth, aes(x = REF_DATE, y = VALUE)) +
  geom_line() +
  labs(title = "Trends Over Time for Potato", x = "Year", y = "Value")


