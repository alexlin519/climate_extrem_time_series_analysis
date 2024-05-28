
























# Generate example data for three stations

# Station 1: Horizontal gradient
station1 <- matrix(rep(seq(0, 1, length.out=10), each=10), nrow=10, ncol=10)

# Station 2: Vertical gradient
station2 <- t(matrix(rep(seq(0, 1, length.out=10), each=10), nrow=10, ncol=10))

# Station 3: Checkerboard pattern
station3 <- matrix(rep(c(0, 1), times=5, each=5), nrow=10, ncol=10)

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

# Plotting with manual color specification
ggplot() +
  geom_tile(data = df1, aes(Var1, Var2, fill = value), alpha = 0.6) +
  scale_fill_gradient(low = "white", high = "grey", name="Station 1") +
  new_scale("fill") +
  geom_tile(data = df2, aes(Var1, Var2, fill = value), alpha = 0.6) +
  scale_fill_gradient(low = "white", high = "grey", name="Station 2") +
  new_scale("fill") +
  geom_tile(data = df3, aes(Var1, Var2, fill = value), alpha = 0.6) +
  scale_fill_gradient(low = "white", high = "grey", name="Station 3") +
  labs(title = "Overlayed Temperature Distributions for Three Stations",
       x = "X Axis", y = "Y Axis") +
  theme_minimal() +
  annotate("text", x = 2, y = 10, label = "Purple = Red + Blue\nStation 1 + Station 2", color = "purple", size = 3) +
  annotate("text", x = 2, y = 9, label = "Yellow = Red + Green\nStation 1 + Station 3", color = "yellow", size = 3) +
  annotate("text", x = 2, y = 8, label = "Cyan = Blue + Green\nStation 2 + Station 3", color = "cyan", size = 3) +
  annotate("text", x = 2, y = 7, label = "White/Gray = Red + Blue + Green\nAll Stations", color = "gray", size = 3)

























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

# Plotting with manual color specification
ggplot() +
  geom_tile(data = df1, aes(Var1, Var2, fill = value), alpha = 0.6) +
  scale_fill_gradient(low = "white", high = "red", name="Station 1") +
  new_scale("fill") +
  geom_tile(data = df2, aes(Var1, Var2, fill = value), alpha = 0.4) +
  scale_fill_gradient(low = "white", high = "blue", name="Station 2") +
  new_scale("fill") +
  geom_tile(data = df3, aes(Var1, Var2, fill = value), alpha = 0.3) +
  scale_fill_gradient(low = "white", high = "green", name="Station 3") +
  labs(title = "Overlayed Temperature Distributions for Three Stations",
       x = "X Axis", y = "Y Axis") +
  theme_minimal()+
  annotate("text", x = 2, y = 9, label = "Purple = Red + Blue\nStation 1 + Station 2", color = "purple", size = 3) +
  annotate("text", x = 2, y = 8, label = "Yellow = Red + Green\nStation 1 + Station 3", color = "yellow", size = 3) +
  annotate("text", x = 2, y = 7, label = "Cyan = Blue + Green\nStation 2 + Station 3", color = "cyan", size = 3)

