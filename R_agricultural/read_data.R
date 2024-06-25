# Load necessary libraries
library(tidyverse)

# Read the CSV file
data_fao <- read_csv("../data/FAOSTAT_data.csv")

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



filtered_data <- 

# Plotting the Value over Years for each Element by Item
ggplot(data = data_fao %>% filter(Item %in% c("Apples", "Peaches")), aes(x = Year, y = Value, color = Item)) +
  geom_line() +
  facet_wrap(~ Element, scales = "free_y") +
  labs(title = "Trends Over Time by Element and Item", x = "Year", y = "Value") +
  theme_minimal()
