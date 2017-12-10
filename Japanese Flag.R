# First we load the libraries
library(ggplot2)
library(dplyr)
library(rpart)
library(caret)
## Loading required package: lattice
library(e1071)

# Let's create 50k points on a 3x2 grid
x <- runif(50000, min = 0, max = 3)
y <- runif(50000, min = 0, max = 2)

# Flag colour palette
japanPalette <- c("red", "white")

# Flag dataframe
japan_flag <- as.data.frame(x = x)
japan_flag$y <- y

# Now we add the colour
japan_flag <-mutate(japan_flag, flag_colour = ifelse( (x - 1.5)^2 + (y-1)^2 > 3/10, "white", "red"))
ggplot(japan_flag) + 
  geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) + 
  coord_fixed(ratio = 1) + 
  scale_colour_manual(values = japanPalette)





