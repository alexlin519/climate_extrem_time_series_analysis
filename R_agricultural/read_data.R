# Load necessary libraries
library(tidyverse)

# Read the CSV file
file_paths <- c("../data/FAOSTAT_data.csv",
                "../data/FAOSTAT_data_4more.csv")
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
for (item in unique(data_fao$Item)) {
  plot <- ggplot(data = data_fao %>% filter(Item == item), aes(x = Year, y = Value, color = Element)) +
    geom_line() +
    labs(title = paste("Trends Over Time for", item), x = "Year", y = "Value")
  print(plot)
  # Plotting the Value over Years for each Element (e.g., "Apples")
  facet_plot <- ggplot(data = data_fao %>% filter(Item == item), aes(x = Year, y = Value)) +
    geom_line() +
    facet_wrap(~ Element, scales = "free_y") +
    labs(title = paste("Trends Over Time for", item), x = "Year", y = "Value") +
    theme_minimal()
  print(facet_plot)
  }
