library(evd)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fExtremes)

gev_analysis_plots <- function(data, station,var_title) {
  
  # Step 1: Calculate Spearman correlation matrix (Optional)
  sp <- cor(data, method = "spearman")
  cat("\nSpearman correlation matrix\n")
  print(round(sp, 2))
  
  # Step 2: Fit the GEV model to the selected station data
  gev_model <- fgev(data[[station]])
  
  # Print GEV model summary
  print(gev_model)
  
  
  
  # Step 3: Extract the estimated parameters
  location <- gev_model$estimate["loc"]
  scale <- gev_model$estimate["scale"]
  shape <- gev_model$estimate["shape"]
  
  # Station names for plot titles
  station_names <- list(
    ab = "Abbotsford",
    ke = "Kelowna",
    pg = "Prince George",
    yv = "YVR",
    fn = "Fort Nelson"
  )
  
  station_full_name <- station_names[[station]]
  
  
  # Step 4: Determine tail behavior based on the shape parameter
  if (shape > 0) {
    tail_behavior <- "Heavy-tailed"
  } else if (shape == 0) {
    tail_behavior <- "Light-tailed (Gumbel distribution)"
  } else {
    tail_behavior <- "Short-tailed"
  }
  
  # Print tail behavior
  cat("\nTail behavior based on shape parameter (", round(shape, 3), "): ", tail_behavior, "\n", sep = "")
  
  
  
  # Step 5: QQ Plot
  n <- nrow(data)
  ii <- ((1:n) - 0.5) / n
  quant_gev <- q_gev(ii, xi = shape, mu = location, sigma = scale)
  
  qq_plot <- ggplot() +
    geom_point(aes(x = quant_gev, y = sort(data[[station]]))) +
    geom_abline(intercept = 0, slope = 1, color = "blue") +
    labs(title = paste(station_full_name, "QQ Plot for",var_title),
         x = "Theoretical Quantiles (GEV)",
         y = "Sample Quantiles") +
    theme_minimal()
  
  # Step 6: Return Level Plot
  T <- c(20, 50, 100, 200)
  return_levels <- q_gev(1 - 1/T, xi = shape, mu = location, sigma = scale)
  
  return_level_plot <- ggplot(data.frame(T = T, return_levels = return_levels)) +
    geom_line(aes(x = T, y = return_levels)) +
    geom_point(aes(x = T, y = return_levels)) +
    scale_x_log10() +
    labs(title = paste(station_full_name, "Return Level Plot for",var_title,"(Log Scale)"),
         x = "Return Period (Years) [Log Scale]",
         y = "Return Level") +
    theme_minimal()
  
  # Step 7: Tail Behavior Plot
  p_values <- seq(0.99, 0.99999, length.out = 100)
  tail_quantiles <- q_gev(p_values, xi = shape, mu = location, sigma = scale)
  
  tail_behavior_plot <- ggplot(data.frame(p = p_values, quantile = tail_quantiles), 
                               aes(x = -log(1 - p), y = quantile)) +
    geom_line(color = "red") +
    labs(x = "-log(1-p) (Tail Probability Scale)",
         y = "Quantile",
         title = paste(station_full_name, "Tail Behavior Plot for",var_title)) +
    theme_minimal()
  
  # Return a list of plot objects
  return(list(qq_plot = qq_plot, 
              return_level_plot = return_level_plot, 
              tail_behavior_plot = tail_behavior_plot))
}

# Open a new graphics device
#graphics.off()
#dev.new()





