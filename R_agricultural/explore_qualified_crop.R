# Load necessary libraries
library(tidyverse)
library(zoo)

# Read the CSV file
#file_paths <- c("../data/agri/veg_data.csv")
file_paths <- c("../data/agri/Vegetable_Data.csv")
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
# Impute missing VALUEs with the mean of neighboring VALUEs
data_veg <- data_veg %>%
  group_by(Crop_Type,Estimates) %>%
  mutate(VALUE = na.approx(VALUE, rule = 2))
sum(is.na(data_veg$VALUE))

# Clean the Crop_Type column by removing "Fresh" and the numbers
data_veg$Crop_Type <- gsub("Fresh ", "", data_veg$Crop_Type)
data_veg$Crop_Type <- gsub("\\s*\\[.*\\]", "", data_veg$Crop_Type)


# Summarize the data by Crop_Type to get the year range
veg_year<- data_veg %>%
  group_by(Crop_Type,Estimates) %>%
  summarize(Start_Year = min(REF_DATE), End_Year = max(REF_DATE))
veg_year

#NUM of crop type
length(unique(data_veg$Crop_Type))
#filter some veg out
data_veg_2 <- data_veg %>% 
  filter(Crop_Type != "broccoli" & Crop_Type != "Brussels sprouts" & Crop_Type != "eggplants (except Chinese eggplants" & 
  Crop_Type != "French shallots and green onions" &  Crop_Type != "garlic" &  
  Crop_Type != "Other fresh fine herbs" &  Crop_Type != "Other fresh melons" &  
  Crop_Type != "Other fresh vegetables" &  Crop_Type != "parsley" &  
  Crop_Type != "peppers" &  Crop_Type != "pumpkins" &  Crop_Type != "radishes" &  
  Crop_Type != "squash and zucchini" &  Crop_Type != "sweet potatoes" &  
  Crop_Type != "Total fresh vegetables" &  Crop_Type != "watermelons" &  
  Crop_Type != "leeks")
length(unique(data_veg_2$Crop_Type))
veg_year_2<- data_veg_2 %>%
  group_by(Crop_Type,Estimates) %>%
  summarize(Start_Year = min(REF_DATE), End_Year = max(REF_DATE))


full_veg <- data_veg
unique(data_veg_2$Estimates)



data_veg_2 <- data_veg_2 %>%
  #filter(Crop_Type == "asparagus") 
  filter(Crop_Type == "tomatoes") 
data_veg_2_yield_p <- data_veg_2 %>%
  filter(Estimates == "Average yield per acre (pounds)" ) %>%
  select(VALUE,Crop_Type,REF_DATE) %>%
  rename(Average_yield_per_acre_pounds = VALUE) 
data_veg_2_yield_k <- data_veg_2 %>%
  filter(Estimates == "Average yield per hectare (kilograms)" ) %>%
  select(VALUE,Crop_Type,REF_DATE) %>%
  rename(Average_yield_per_hectare_kilograms = VALUE)




#--------------------------------------------
#final version
#--------------------------------------------

library(dplyr)

# Split the data into different components for calculation
total_production_acres <- data_veg_2 %>% filter(Estimates == "Total production (tons)")
area_harvested_acres <- data_veg_2 %>% filter(Estimates == "Area harvested (acres)")
total_production_hectares <- data_veg_2 %>% filter(Estimates == "Total production (metric tonnes)")
area_harvested_hectares <- data_veg_2 %>% filter(Estimates == "Area harvested (hectares)")
marketed_production_acres <- data_veg_2 %>% filter(Estimates == "Marketed production (tons)")
area_planted_acres <- data_veg_2 %>% filter(Estimates == "Area planted (acres)")
marketed_production_hectares <- data_veg_2 %>% filter(Estimates == "Marketed production (metric tonnes)")
area_planted_hectares <- data_veg_2 %>% filter(Estimates == "Area planted (hectares)")


# Perform a full join to combine the datasets and calculate yield per acre
combined_data_tp_ap_acres <- total_production_acres %>%
  full_join(area_planted_acres, by = c("REF_DATE", "Crop_Type")) %>%
  rename(tp_tons = VALUE.x, ap_acres = VALUE.y) %>%
  mutate(yield_tp_ap_acre_pounds = ifelse(is.na(tp_tons) | is.na(ap_acres), NA, tp_tons * 2000 / ap_acres)) %>%
  select(REF_DATE, Crop_Type, Average_yield_per_acre_pounds)

# Perform a full join to combine the datasets and calculate yield per hectare
combined_data_tp_ap_hectares <- total_production_hectares %>%
  full_join(area_planted_hectares, by = c("REF_DATE", "Crop_Type")) %>%
  rename(tp_metric_tonnes = VALUE.x, ap_hectares = VALUE.y) %>%
  mutate(yield_tp_ap_hectare_kg = ifelse(is.na(tp_metric_tonnes) | is.na(ap_hectares), NA, tp_metric_tonnes * 1000 / ap_hectares)) %>%
  select(REF_DATE, Crop_Type, Average_yield_per_hectare_kg)


# Perform a full join to combine the datasets and calculate marketed yield per acre
combined_data_mp_ah_acres <- marketed_production_acres %>%
  full_join(area_harvested_acres, by = c("REF_DATE", "Crop_Type")) %>%
  rename(mp_tons = VALUE.x, ah_acres = VALUE.y) %>%
  mutate(yield_mp_ah_acre_pounds = ifelse(is.na(mp_tons) | is.na(ah_acres), NA, mp_tons * 2000 / ah_acres)) %>%
  select(REF_DATE, Crop_Type, Marketed_yield_per_acre_pounds)

# Perform a full join to combine the datasets and calculate marketed yield per hectare
combined_data_mp_ah_hectares <- marketed_production_hectares %>%
  full_join(area_harvested_hectares, by = c("REF_DATE", "Crop_Type")) %>%
  rename(mp_metric_tonnes = VALUE.x, ah_hectares = VALUE.y) %>%
  mutate(yield_mp_ah_hectare_kg = ifelse(is.na(mp_metric_tonnes) | is.na(ah_hectares), NA, mp_metric_tonnes * 1000 / ah_hectares)) %>%
  select(REF_DATE, Crop_Type, Marketed_yield_per_hectare_kg)

# Combine all datasets by performing a full join
final_combined_data <- combined_data_tp_ap_acres %>%
  full_join(combined_data_tp_ap_hectares, by = c("REF_DATE", "Crop_Type")) %>%
  full_join(combined_data_mp_ah_acres, by = c("REF_DATE", "Crop_Type")) %>%
  full_join(combined_data_mp_ah_hectares, by = c("REF_DATE", "Crop_Type"))%>%
  full_join(data_veg_2_yield_p, by = c("REF_DATE", "Crop_Type")) %>%
  full_join(data_veg_2_yield_k, by = c("REF_DATE", "Crop_Type")) %>%
  select(-Estimates.x,-Estimates.y)

# View the resulting dataframe
print(final_combined_data)

#column name
colnames(final_combined_data)







#plot
library(tidyr)
library(ggplot2)









calculate_yields <- function(data) {
  library(dplyr)
  # Ensure the VALUE column is numeric
  data$VALUE <- as.numeric(data$VALUE)
  
  # Define a helper function to calculate yields
  calculate_yield <- function(production, area, multiplier) {
    ifelse(is.na(production) | is.na(area), NA, production * multiplier / area)
  }
  
  # Calculate yields
  yields <- data %>%
    filter(Estimates %in% c("Total production (tons)", "Area harvested (acres)",
                            "Total production (metric tonnes)", "Area harvested (hectares)",
                            "Marketed production (tons)", "Area planted (acres)",
                            "Marketed production (metric tonnes)", "Area planted (hectares)")) %>%
    group_by(REF_DATE, Crop_Type) %>%
    summarize(
      yield_tp_ah_acre_pounds = calculate_yield(
        first(VALUE[Estimates == "Total production (tons)"]),
        first(VALUE[Estimates == "Area harvested (acres)"]),
        2000 #
      ),
      yield_tp_ah_hectare_kg = calculate_yield(
        first(VALUE[Estimates == "Total production (metric tonnes)"]),
        first(VALUE[Estimates == "Area harvested (hectares)"]),
        892.197 # 2204.62 / 2.471
      ),
      yield_mp_ap_acre_pounds = calculate_yield(
        first(VALUE[Estimates == "Marketed production (tons)"]),
        first(VALUE[Estimates == "Area planted (acres)"]),
        2000 #
      ),
      yield_mp_ap_hectare_kg = calculate_yield(
        first(VALUE[Estimates == "Marketed production (metric tonnes)"]),
        first(VALUE[Estimates == "Area planted (hectares)"]),
        892.197 # 2204.62 / 2.471
      ),
      yield_mp_ah_acre_pounds = calculate_yield(
        first(VALUE[Estimates == "Marketed production (tons)"]),
        first(VALUE[Estimates == "Area harvested (acres)"]),
        2000 #
      ),
      yield_mp_ah_hectare_kg = calculate_yield(
        first(VALUE[Estimates == "Marketed production (metric tonnes)"]),
        first(VALUE[Estimates == "Area harvested (hectares)"]),
        892.197 # 2204.62 / 2.471
      ),
      yield_tp_ap_acre_pounds = calculate_yield(
        first(VALUE[Estimates == "Total production (tons)"]),
        first(VALUE[Estimates == "Area planted (acres)"]),
        2000 #
      ),
      yield_tp_ap_hectare_kg = calculate_yield(
        first(VALUE[Estimates == "Total production (metric tonnes)"]),
        first(VALUE[Estimates == "Area planted (hectares)"]),
        892.197 # 2204.62 / 2.471
      )
    )
  
  return(yields)
}

# Example usage:
calculated_yields <- calculate_yields(data_veg_2)

colnames(calculated_yields)

final_full <- calculated_yields %>%
  full_join(data_veg_2_yield_p, by = c("REF_DATE", "Crop_Type")) %>%
  full_join(data_veg_2_yield_k, by = c("REF_DATE", "Crop_Type")) %>%
  select(-Estimates.x,-Estimates.y)

# Reshape the data to long format
long_data <- final_full %>%
  gather(key = "Yield_Type", value = "Yield_Value", -REF_DATE, -Crop_Type)

#filter date
long_data <- long_data %>%
  filter(REF_DATE > 1980)

custom_colors <- c(
  "Average_yield_per_acre_pounds" = "red",
  "Average_yield_per_hectare_kilograms" = "blue",
  "yield_tp_ah_acre_pounds" = "#008000",
  "yield_tp_ah_hectare_kg" = "#FF00FF",
  "yield_mp_ap_acre_pounds" = "#FFFF00",
  "yield_mp_ap_hectare_kg" = "#A52A2A",
  "yield_mp_ah_acre_pounds" = "#696969",
  "yield_mp_ah_hectare_kg" = "#40E0D0",
  "yield_tp_ap_acre_pounds" = "#FF1493",
  "yield_tp_ap_hectare_kg" = "#1E90FF"
)

# Plot all yields in one plot
ggplot(long_data, aes(x = REF_DATE, y = Yield_Value, color = Yield_Type, linetype = Crop_Type)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = custom_colors) +
  labs(title = "Yields Over Time",
       x = "Year",
       y = "Yield Value",
       color = "Yield Type",
       linetype = "Crop Type") +
  theme_minimal()

#filter estimate
long_data_some <- long_data %>%
  filter(Yield_Type == "Average_yield_per_acre_pounds" | 
           Yield_Type == "yield_mp_ah_hectare_kg" |
           Yield_Type == "yield_tp_ap_acre_pounds" )

# Plot all yields in one plot
ggplot(long_data_some, aes(x = REF_DATE, y = Yield_Value, color = Yield_Type, linetype = Crop_Type)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = custom_colors) +
  labs(title = "Yields Over Time",
       x = "Year",
       y = "Yield Value",
       color = "Yield Type",
       linetype = "Crop Type") +
  theme_minimal()




#yield_mp_ah_hectare_kg is final decision
