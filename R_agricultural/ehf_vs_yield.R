
preprocess_ehf_data <- function(df_combined_ehf1, top_yields) {
  ehf_extreme_yield <- df_combined_ehf1 %>%
    left_join(top_yields, by = c("LOCAL_YEAR" = "REF_DATE")) %>%
    filter(!is.na(Rank)) %>%
    rename(Year = LOCAL_YEAR) %>%
    mutate(DayOfYear = as.integer(format(LOCAL_DATE, "%j")))
  return(ehf_extreme_yield)
}



#plot_ehf_extreme_yield(ehf_extreme_yield, start_year, end_year, 1990,station_name) 

rank_colors <- c("1" = "red", "2" = "orange", "3" = "yellow", "4" = "green", "5" = "blue")
# base_colors <- c('#FF0000', '#008000', '#0000FF', '#000000', '#FF00FF', '#FFA500', '#FFD700', '#D2691E',
#                  '#D2B48C', '#808000', '#FFFF00', '#40E0D0', '#00BFFF', '#1E90FF', '#8A2BE2', '#FF1493',
#                  '#A52A2A', '#7FFF00', '#8B008B', '#FF69B4','#A9A9A9', '#696969') 

plot_ehf_extreme_yield <- function(ehf_extreme_yield, start_year, end_year, station_name,ranking, top_n = NULL, lowest_n = NULL) {
  # Create an empty data frame to store all years' data
  all_years_data <- ehf_extreme_yield 
  # Loop through each year and bind the data
  # for (year in start_year:end_year) {
  #   df_plot_year <- ehf_extreme_yield %>%
  #     filter(LOCAL_YEAR == year) %>%
  #     mutate(DayOfYear = as.integer(format(LOCAL_DATE, "%j")),
  #            Year = year)
  #   all_years_data <- bind_rows(all_years_data, df_plot_year)
  # }
  
  # Filter data based on top_n or lowest_n if specified
  if (!is.null(top_n)) {
    all_years_data <- all_years_data %>%
      filter(Type == "Highest") %>%
      filter(Rank %in% ranking)
    title_rank <- paste("Top", top_n, "Highest")
  } else if (!is.null(lowest_n)) {
    all_years_data <- all_years_data %>%
      filter(Type == "Lowest") %>%
      filter(Rank %in% ranking)
    title_rank <- paste("Top", lowest_n, "Lowest")
  }
  
  all_years_data$Rank <- as.factor(all_years_data$Rank)
  
  ggplot(all_years_data, aes(x = DayOfYear)) +
    geom_line(aes(y = EHF, color = Rank,group = Year), size = 1, alpha = 0.7) +
    scale_color_manual(values = rank_colors, name = "Rank") +
    labs(
      title = paste("EHF Over the Years with", "at", station_name), #title_rank,
      x = "Day of Year",
      y = "EHF"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.margin = margin(20, 20, 20, 20)) +
    theme(aspect.ratio = 1/2) +

    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12))
  
}




plot_ehf_all(ehf_potato, start_year, end_year, 1995,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield:", data_pot %>% filter(REF_DATE == 1995) %>% pull(Detrended_Yield_LOESS)), color = "red", size = 5, hjust = 0, vjust = 0)
plot_ehf_all(ehf_potato, start_year, end_year, 1989,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield_LOESS:", data_pot %>% filter(REF_DATE == 1989) %>% pull(Detrended_Yield_LOESS)), color = "red", size = 5, hjust = 0, vjust = 0)
plot_ehf_all(ehf_potato, start_year, end_year, 1915,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield_LOESS:", data_pot %>% filter(REF_DATE == 1915) %>% pull(Detrended_Yield_LOESS)), color = "red", size = 5, hjust = 0, vjust = 0)
plot_ehf_all(ehf_potato, start_year, end_year, 1992,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield_LOESS:", data_pot %>% filter(REF_DATE == 1992) %>% pull(Detrended_Yield_LOESS)), color = "red", size = 5, hjust = 0, vjust = 0)
plot_ehf_all(ehf_potato, start_year, end_year, 1979,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield_LOESS:", data_pot %>% filter(REF_DATE == 1979) %>% pull(Detrended_Yield_LOESS)), color = "red", size = 5, hjust = 0, vjust = 0)
plot_ehf_extreme_yield(ehf_extreme_yield,start_year, end_year, "Kelowna",  (1:5),  lowest_n   = 2)

plot_ehf_all(ehf_potato, start_year, end_year, 1990,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield_LOESS:", data_pot %>% filter(REF_DATE == 1990) %>% pull(Detrended_Yield_LOESS)), color = "red", size = 5, hjust = 0, vjust = 0)
plot_ehf_all(ehf_potato, start_year, end_year, 1981,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield_LOESS:", data_pot %>% filter(REF_DATE == 1981) %>% pull(Detrended_Yield_LOESS)), color = "red", size = 5, hjust = 0, vjust = 0)
plot_ehf_all(ehf_potato, start_year, end_year, 1993,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield_LOESS:", data_pot %>% filter(REF_DATE == 1993) %>% pull(Detrended_Yield_LOESS)), color = "red", size = 5, hjust = 0, vjust = 0)
plot_ehf_all(ehf_potato, start_year, end_year, 1997,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield_LOESS:", data_pot %>% filter(REF_DATE == 1997) %>% pull(Detrended_Yield_LOESS)), color = "red", size = 5, hjust = 0, vjust = 0)
plot_ehf_all(ehf_potato, start_year, end_year, 1951,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield_LOESS:", data_pot %>% filter(REF_DATE == 1951) %>% pull(Detrended_Yield)), color = "red", size = 5, hjust = 0, vjust = 0)

```
```{r, echo=FALSE,message=FALSE, warning=FALSE,fig.width=10}
#2021 1993 2006 1925 1941 1992 1988 2023 1949 1934 1958 1994 1987 1960
plot_ehf_all(ehf_potato, start_year, end_year, 2021,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield_LOESS:", data_pot %>% filter(REF_DATE == 2021) %>% pull(Detrended_Yield_LOESS)), color = "red", size = 5, hjust = 0, vjust = 0)

plot_ehf_all(ehf_potato, start_year, end_year, 2006,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield_LOESS:", data_pot %>% filter(REF_DATE == 2006) %>% pull(Detrended_Yield_LOESS)), color = "red", size = 5, hjust = 0, vjust = 0)

plot_ehf_all(ehf_potato, start_year, end_year, 1925,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield_LOESS:", data_pot %>% filter(REF_DATE == 1925) %>% pull(Detrended_Yield_LOESS)), color = "red", size = 5, hjust = 0, vjust = 0)

plot_ehf_all(ehf_potato, start_year, end_year, 1941,station_name) + annotate("text", x = 200, y = 100, label =paste( "Detrended_Yield_LOESS:", data_pot %>% filter(REF_DATE == 1941) %>% pull(Detrended_Yield_LOESS)), color = "red", size = 5, hjust = 0, vjust = 0)
```

```{r, echo=FALSE,message=FALSE, warning=FALSE,fig.width=10}
# Assuming top_yields is a data frame with columns REF_DATE and Rank
year_of_interest <- 1941

# Extract Detrended Yield
detrended_yield <- data_pot %>% 
  filter(REF_DATE == year_of_interest) %>% 
  pull(Detrended_Yield_LOESS)

# Check the rank of the year in top_yields
rank_info <- top_yields %>% 
  filter(REF_DATE == year_of_interest) %>% 
  select(Rank,Type)


rank_text <- if(length(rank_info) == 0) {
  paste("Out of", max(top_yields$Rank), "Ranking")
} else {
  paste("Rank:", rank_info$Type[1], rank_info$Rank[1])
}
# Create annotation text
annotation_text <- paste("Detrended Yield:", detrended_yield, "\n", rank_text)

# Add annotation to the plot
plot_ehf_all(ehf_potato, start_year, end_year,year_of_interest ,station_name)+ annotate("text", x = 200, y = 100, label = annotation_text, color = "red", size = 5, hjust = 0, vjust = 0)



```

