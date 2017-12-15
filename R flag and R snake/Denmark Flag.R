# First we load the libraries
library(ggplot2)
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
# Let's create 200k points on a 16x10 grid
x <- runif(200000, min = 0, max = 16)
y <- runif(200000, min = 0, max = 10)

# We create the dataframe
flag <- as.data.frame(x = x)
flag$y <- y

# Now we add the colour
flag <-mutate(flag, flag_colour = ifelse(((x > 5) & (x<=7)) | ((y > 4) & (y<=6)), "cross", "bckgd"))

DenmarkPalette <- c("red", "white")

ggplot(flag) + 
  geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) + 
  coord_fixed(ratio = 1) + 
  scale_colour_manual(values = DenmarkPalette)




