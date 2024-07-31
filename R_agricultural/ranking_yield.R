library(ggrepel)
top_n_highest_lowest <- function(data, n, col_var){
  # Top n highest yield values
  top_n_highest <- data %>% 
    arrange(desc((.data[[col_var]]))) %>% 
    head(n)%>% 
    mutate(Rank = 1:n)
  
  # Top n lowest yield values
  top_n_lowest <- data %>% 
    arrange((.data[[col_var]])) %>% 
    head(n)%>% 
    mutate(Rank = 1:n)
  
  # Combine the two dataframes for plotting
  top_yields <- bind_rows(
    top_n_highest %>% mutate(Type = "Highest"),
    top_n_lowest %>% mutate(Type = "Lowest")
  )
  # # Plot the data
  # ggplot(top_yields, aes(x = reorder(REF_DATE, VALUE), y = VALUE, fill = Type)) +
  #   geom_bar(stat = "identity") +
  #   labs(title = paste("Top", n, "Highest and Lowest Potato",col_var),
  #        x = "Year",
  #        y = "Yield (Hundredweight per harvested acres)") +
  #   theme_minimal() +
  #   coord_flip()
  
  # Plot the data
  ggplot(top_yields, aes(x = reorder(REF_DATE, .data[[col_var]]), y = .data[[col_var]], color = Type)) +
    geom_point(size = 4) +
    geom_text(aes(label = paste("Year:", REF_DATE, "\nValue:", .data[[col_var]])), 
              nudge_y = 0.5, size = 3) +
    labs(title = paste("Top", n, "Highest and Lowest Potato Yields for", col_var),
         x = "Year",
         y = col_var) +
    theme_minimal() +
    coord_flip()
  
  
  
}
#eg
top_n_highest_lowest(data_pot, 5, "VALUE")
top_n_highest_lowest(data_pot, 10, "Detrended_Yield_LOESS")







library(dplyr)
library(ggplot2)

# Define the function
top_n_highest_lowest <- function(data, n, col_var) {
  # Top n highest values
  top_n_highest <- data %>% 
    arrange(desc(.data[[col_var]])) %>% 
    head(n) %>% 
    mutate(Rank = 1:n)
  
  # Top n lowest values
  top_n_lowest <- data %>% 
    arrange(.data[[col_var]]) %>% 
    head(n) %>% 
    mutate(Rank = 1:n)
  
  # Combine the two dataframes for plotting
  top_yields <- bind_rows(
    top_n_highest %>% mutate(Type = "Highest"),
    top_n_lowest %>% mutate(Type = "Lowest")
  )
  
  return(top_yields)
}

# Assuming your dataframe is already loaded and named df
data_pot <- df %>% filter(Crop_Type == "Potato")

# Call the function to get the top yields
n <- 40
col_var <- "Detrended_Yield_LOESS"
top_yields <- top_n_highest_lowest(data_pot, n, col_var)

# Plot with moving averages and scatter points for top yields
avg_w_rank <- mov_avg +
  geom_point(data = top_yields, aes(x = REF_DATE, y = .data[[col_var]], color = Type), size = 2.5) +
  
  #geom_text(data = top_yields, aes(x = REF_DATE, y = .data[[col_var]], label = paste("Year:", REF_DATE, "\nValue:", .data[[col_var]])),
  #          nudge_y = 0.5, size = 1.5) +
  geom_text_repel(data = top_yields,aes(x = REF_DATE, y = .data[[col_var]],label = paste("Year:", REF_DATE, "\nValue:", .data[[col_var]])), vjust = -0.5, size = 3,max.overlaps = Inf) +  # Add rank labels
  
  labs(title = "Potato Yield and Detrended Yield with Top Highest and Lowest Yields",
       y = "Yield", x = "Year") +
  scale_color_manual(values = c("Original Yield" = "blue", "Moving Average" = "green", "Detrended Yield" = "red", "Highest" = "purple", "Lowest" = "orange")) +
  theme_minimal()

print(avg_w_rank)

