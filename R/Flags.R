#' @title R Flags
#' @description R flag is a simple drawing game. 
#' Players can choose the number of points and name of a country to get a country flag.
#' 
#' @param n Number of points
#' @return Country flag with a fixed number of points.
#' 
#' @note Welcome to add more flags
#'       
#' @author Wei Wang
#' @export   
#' @example Japan()
#' 
Japan <- function(a = 20000){
  
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

#' @title R Flags
#' @description R flag is a simple drawing game. 
#' Players can choose the number of points and name of a country to get a country flag.
#' 
#' @param n Number of points
#' @return Country flag with a fixed number of points.
#' 
#' @note Welcome to add more flags
#'       
#' @author Wei Wang
#' @export   
#' @example Sweden()
Sweden <- function(a = 20000){

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

#' @title R Flags
#' @description R flag is a simple drawing game. 
#' Players can choose the number of points and name of a country to get a country flag.
#' 
#' @param n Number of points
#' @return Country flag with a fixed number of points.
#' 
#' @note Welcome to add more flags
#'       
#' @author Wei Wang
#' @export   
#' @example Denmark()
Denmark <- function(a = 20000){


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


#' @title R Flags
#' @description R flag is a simple drawing game. 
#' Players can choose the number of points and name of a country to get a country flag.
#' 
#' @param n Number of points
#' @return Country flag with a fixed number of points.
#' 
#' @note Welcome to add more flags
#'       
#' @author Wei Wang
#' @export   
#' @example Finland()
Finland <- function(a = 20000){
  

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



#' @title R Flags
#' @description R flag is a simple drawing game. 
#' Players can choose the number of points and name of a country to get a country flag.
#' 
#' @param n Number of points
#' @return Country flag with a fixed number of points.
#' 
#' @note Welcome to add more flags
#'       
#' @author Wei Wang
#' @export   
#' @example Norway()
Norway <- function(a = 20000){
 
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


#' @title R Flags
#' @description R flag is a simple drawing game. 
#' Players can choose the number of points and name of a country to get a country flag.
#' 
#' @param n Number of points
#' @return Country flag with a fixed number of points.
#' 
#' @note Welcome to add more flags
#'       
#' @author Wei Wang
#' @export   
#' @example Iceland()
Iceland <- function(a = 20000){


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


