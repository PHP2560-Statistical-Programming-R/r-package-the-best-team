#' R Flags
#' @param Number of points
#' @return Japanese flag
#' @export
Japan <- function(a = 20000){
  install_packages <- function(names)
  {
    for(name in names)
    {
      if (!(name %in% installed.packages()))
        install.packages(name, repos="http://cran.us.r-project.org")

      library(name, character.only=TRUE)
    }
  }

  install_packages(c("ggplot2","dplyr","rpart","caret","e1071"))

  # Let's create 50k points on a 3x2 grid
  x <- runif(a, min = 0, max = 3)
  y <- runif(a, min = 0, max = 2)

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
}


#' R Flags
#' @param Number of points
#' @return Sweden flag
#' @export
Sweden <- function(a = 20000){
  install_packages <- function(names)
  {
    for(name in names)
    {
      if (!(name %in% installed.packages()))
        install.packages(name, repos="http://cran.us.r-project.org")

      library(name, character.only=TRUE)
    }
  }

  install_packages(c("ggplot2","dplyr","rpart","caret","e1071"))

  # Let's create 200k points on a 16x10 grid
  x <- runif(a, min = 0, max = 16)
  y <- runif(a, min = 0, max = 10)

  # We create the dataframe
  flag <- as.data.frame(x = x)
  flag$y <- y

  # Now we add the colour
  flag <-mutate(flag, flag_colour = ifelse(((x > 5) & (x<=7)) | ((y > 4) & (y<=6)), "cross", "bckgd"))

  SwedenPalette <- c("blue", "yellow")

  ggplot(flag) +
    geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) +
    coord_fixed(ratio = 1) +
    scale_colour_manual(values = SwedenPalette)
}


#' R Flags
#' @param Number of points
#' @return Denmark flag
#' @export
Denmark <- function(a = 20000){
  install_packages <- function(names)
  {
    for(name in names)
    {
      if (!(name %in% installed.packages()))
        install.packages(name, repos="http://cran.us.r-project.org")

      library(name, character.only=TRUE)
    }
  }

  install_packages(c("ggplot2","dplyr","rpart","caret","e1071"))

  # Let's create 200k points on a 16x10 grid
  x <- runif(a, min = 0, max = 16)
  y <- runif(a, min = 0, max = 10)

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
}


#' R Flags
#' @param Number of points
#' @return Finland flag
#' @export
Finland <- function(a = 20000){
  install_packages <- function(names)
  {
    for(name in names)
    {
      if (!(name %in% installed.packages()))
        install.packages(name, repos="http://cran.us.r-project.org")

      library(name, character.only=TRUE)
    }
  }

  install_packages(c("ggplot2","dplyr","rpart","caret","e1071"))

  # Let's create 200k points on a 16x10 grid
  x <- runif(a, min = 0, max = 16)
  y <- runif(a, min = 0, max = 10)

  # We create the dataframe
  flag <- as.data.frame(x = x)
  flag$y <- y

  # Now we add the colour
  flag <-mutate(flag, flag_colour = ifelse(((x > 5) & (x<=7)) | ((y > 4) & (y<=6)), "cross", "bckgd"))

  FinlandPalette <- c("white", "blue")

  ggplot(flag) +
    geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) +
    coord_fixed(ratio = 1) +
    scale_colour_manual(values = FinlandPalette)

}



#' R Flags
#' @param Number of points
#' @return Norway flag
#' @export
Norway <- function(a = 20000){
  install_packages <- function(names)
  {
    for(name in names)
    {
      if (!(name %in% installed.packages()))
        install.packages(name, repos="http://cran.us.r-project.org")

      library(name, character.only=TRUE)
    }
  }

  install_packages(c("ggplot2","dplyr","rpart","caret","e1071"))

  # Let's create 200k points on a 21x16 grid
  x <- runif(a, min = 0, max = 21)
  y <- runif(a, min = 0, max = 16)

  flag <- as.data.frame(x = x)
  flag$y <- y

  # Now we add the colour, however this flags contain two crosses
  flag <-mutate(flag, flag_colour = ifelse(((x > 6) & (x<=10)) | ((y > 6) & (y<=10)), "cross", "bckgd"))
  crossed_flag <- flag[which(flag$flag_colour == "cross"),]
  flag[which(flag$flag_colour == "cross"),] <- 
  mutate(crossed_flag, flag_colour = ifelse(((x > 7) & (x<=9)) | ((y > 7) & (y<=9)), "inner_cross", "cross"))

  NorwayPalette <- c("red", "white", "blue")
  ggplot(flag) +
    geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) +
    coord_fixed(ratio = 1) +
    scale_colour_manual(values = NorwayPalette)
}



#' R Flags
#' @param Number of points
#' @return Iceland flag
#' @export
Iceland <- function(a = 20000){
  install_packages <- function(names)
  {
    for(name in names)
    {
      if (!(name %in% installed.packages()))
        install.packages(name, repos="http://cran.us.r-project.org")

      library(name, character.only=TRUE)
    }
  }

  install_packages(c("ggplot2","dplyr","rpart","caret","e1071"))

  # Let's create 200k points on a 21x16 grid
  x <- runif(a, min = 0, max = 21)
  y <- runif(a, min = 0, max = 16)

  flag <- as.data.frame(x = x)
  flag$y <- y

  # Now we add the colour, however this flags contain two crosses
  flag <-mutate(flag, flag_colour = ifelse(((x > 6) & (x<=10)) | ((y > 6) & (y<=10)), "cross", "bckgd"))
  crossed_flag <- flag[which(flag$flag_colour == "cross"),]
  flag[which(flag$flag_colour == "cross"),] <- 
  mutate(crossed_flag, flag_colour = ifelse(((x > 7) & (x<=9)) | ((y > 7) & (y<=9)), "inner_cross", "cross"))

  IcelandPalette <- c("blue", "white", "red")
  ggplot(flag) +
    geom_point(aes(x = x, y = y, color = flag_colour), size = 0.1) +
    coord_fixed(ratio = 1) +
    scale_colour_manual(values = IcelandPalette)
}


