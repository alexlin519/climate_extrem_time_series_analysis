# Load necessary libraries
library(tidyverse)
library(zoo)
library(tidyr)
library(ggplot2)
library(dplyr)


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



#### -----------------code----------------
# data_veg_2 <- data_veg_2 %>%
#   #filter(Crop_Type == "asparagus") 
#   filter(Crop_Type == "tomatoes") 
# 
# 
# # Example usage:
# calculated_yields <- calculate_yields(data_veg_2)
# 
# 
# final_full <- calculated_yields %>%
#   full_join(data_veg_2_yield_p, by = c("REF_DATE", "Crop_Type")) %>%
#   full_join(data_veg_2_yield_k, by = c("REF_DATE", "Crop_Type")) %>%
#   select(-Estimates.x,-Estimates.y)
# 
# # Reshape the data to long format
# long_data <- final_full %>%
#   gather(key = "Yield_Type", value = "Yield_Value", -REF_DATE, -Crop_Type)
# 
# #filter date
# long_data <- long_data %>%
#   filter(REF_DATE > 1980)
# 
# custom_colors <- c(
#   "Average_yield_per_acre_pounds" = "red",
#   "Average_yield_per_hectare_kilograms" = "blue",
#   "yield_tp_ah_acre_pounds" = "#008000",
#   "yield_tp_ah_hectare_kg" = "#FF00FF",
#   "yield_mp_ap_acre_pounds" = "#FFFF00",
#   "yield_mp_ap_hectare_kg" = "#A52A2A",
#   "yield_mp_ah_acre_pounds" = "#696969",
#   "yield_mp_ah_hectare_kg" = "#40E0D0",
#   "yield_tp_ap_acre_pounds" = "#FF1493",
#   "yield_tp_ap_hectare_kg" = "#1E90FF"
# )
# 
# # Plot all yields in one plot
# ggplot(long_data, aes(x = REF_DATE, y = Yield_Value, color = Yield_Type, linetype = Crop_Type)) +
#   geom_line() +
#   geom_point() +
#   scale_color_manual(values = custom_colors) +
#   labs(title = "Yields Over Time",
#        x = "Year",
#        y = "Yield Value",
#        color = "Yield Type",
#        linetype = "Crop Type") +
#   theme_minimal()
# 
# #filter estimate
# long_data_some <- long_data %>%
#   filter(Yield_Type == "Average_yield_per_acre_pounds" | 
#            Yield_Type == "yield_mp_ah_hectare_kg" |
#            Yield_Type == "yield_tp_ap_acre_pounds" )
# 
# # Plot all yields in one plot
# ggplot(long_data_some, aes(x = REF_DATE, y = Yield_Value, color = Yield_Type, linetype = Crop_Type)) +
#   geom_line() +
#   geom_point() +
#   scale_color_manual(values = custom_colors) +
#   labs(title = "Yields Over Time",
#        x = "Year",
#        y = "Yield Value",
#        color = "Yield Type",
#        linetype = "Crop Type") +
#   theme_minimal()
# 
# 


#yield_mp_ah_hectare_kg is final decision

####-----------function------------



# Function to process the data and create the long format data frame
process_data <- function(data_veg_2,type_crop) {
  data_veg_2 <- data_veg_2 %>%
    filter(Crop_Type == type_crop)
  
  calculated_yields <- calculate_yields(data_veg_2)
  
  data_veg_2_yield_p <- data_veg_2 %>%
    filter(Estimates == "Average yield per acre (pounds)" ) %>%
    select(VALUE,Crop_Type,REF_DATE) %>%
    rename(Average_yield_per_acre_pounds = VALUE) 
  data_veg_2_yield_k <- data_veg_2 %>%
    filter(Estimates == "Average yield per hectare (kilograms)" ) %>%
    select(VALUE,Crop_Type,REF_DATE) %>%
    rename(Average_yield_per_hectare_kilograms = VALUE)
  
  final_full <- calculated_yields %>%
    full_join(data_veg_2_yield_p, by = c("REF_DATE", "Crop_Type")) %>%
    full_join(data_veg_2_yield_k, by = c("REF_DATE", "Crop_Type")) %>%
    select(-Estimates.x, -Estimates.y)
  
  long_data <- final_full %>%
    gather(key = "Yield_Type", value = "Yield_Value", -REF_DATE, -Crop_Type) %>%
    filter(REF_DATE > 1980)
  
  return(long_data)
}

# Function to plot the data
plot_yields <- function(long_data, custom_colors) {
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
}

# Function to filter specific yield types and plot
plot_selected_yields <- function(long_data, selected_yield_types, custom_colors) {
  long_data_some <- long_data %>%
    filter(Yield_Type %in% selected_yield_types)
  
  plot_yields(long_data_some, custom_colors)
}





#yield_mp_ah_hectare_kg