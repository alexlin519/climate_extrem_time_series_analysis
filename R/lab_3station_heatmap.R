# Example data for three stations
station1 <- matrix(runif(100, min=0, max=1), nrow=10)
station2 <- matrix(runif(100, min=0, max=1), nrow=10)
station3 <- matrix(runif(100, min=0, max=1), nrow=10)
# Load required libraries
library(ggplot2)
library(reshape2)

# Example data for three stations
set.seed(0)
station1 <- matrix(runif(100, min=0, max=1), nrow=10)
station2 <- matrix(runif(100, min=0, max=1), nrow=10)
station3 <- matrix(runif(100, min=0, max=1), nrow=10)

# Convert matrices to data frames
df1 <- melt(station1)
df2 <- melt(station2)
df3 <- melt(station3)

# Add station identifier
df1$Station <- "Station 1"
df2$Station <- "Station 2"
df3$Station <- "Station 3"

# Combine data frames
df <- rbind(df1, df2, df3)

# Create the plot
ggplot() +
  geom_tile(data=df1, aes(Var1, Var2, fill=value), alpha=0.6) + 
  scale_fill_gradient(low="white", high="red") +
  geom_tile(data=df2, aes(Var1, Var2, fill=value), alpha=0.4) + 
  scale_fill_gradient(low="white", high="blue") +
  geom_tile(data=df3, aes(Var1, Var2, fill=value), alpha=0.3) + 
  scale_fill_gradient(low="white", high="green") +
  labs(title="Overlayed Temperature Distributions for Three Stations", 
       x="X Axis", y="Y Axis") +
  theme_minimal()
