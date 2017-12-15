# Now we create the flag of norway
# Let's create 200k points on a 21x16 grid
x <- runif(200000, min = 0, max = 21)
y <- runif(200000, min = 0, max = 16)

flag <- as.data.frame(x = x)
flag$y <- y

# Now we add the colour, however this flags contain two crosses
flag <-mutate(flag, flag_colour = ifelse(((x > 6) & (x<=10)) | ((y > 6) & (y<=10)), "cross", "bckgd"))
crossed_flag <- flag[which(flag$flag_colour == "cross"),]
flag[which(flag$flag_colour == "cross"),] <- mutate(crossed_flag, flag_colour = ifelse(((x > 7) & (x<=9)) | ((y > 7) & (y<=9)), "inner_cross", "cross"))

IcelandPalette <- c("blue", "white", "red")
ggplot(flag) + 
  geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) + 
  coord_fixed(ratio = 1) + 
  scale_colour_manual(values = IcelandPalette)