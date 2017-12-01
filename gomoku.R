gomoku <- function(n = 19) {


  if (!interactive()) return()
  
#Setting of the game
  par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))#add points to the plot where the lines should be located
  segments(1, 1:n, n, 1:n)#draw horizontal lines
  segments(1:n, 1, 1:n, n)#draw vertical lines
  points(rep(c(4, 10, 16), 3), rep(c(4, 10, 16), each = 3),
         pch = 19, cex = 1.2)#draw the black point with the shape of solid circle
  box() #draw the outline of the plot
  
  
#Playing the game
  playedlist <- NULL #record the points have been stepped on
  i <- 1 #rounds that will have be played
  black = list()
  white = list()
  repeat {
    for (j in 1:2) {
      repeat {
        l <- locator(1) #record the location where the mouse clicks
        l$x <- min(n, max(1, round(l$x))) #modify the x-location to where nearest point
        l$y <- min(n, max(1, round(l$y))) #modify the y-location to where nearest point
        xy <- paste(l, collapse = ":") #record the step
        if (!is.element(xy, playedlist)) #break when the point had chessman on it
          break
      }
      playedlist <- c(playedlist, xy) #add the step to the playlist if it is successfully played
      points(l, cex = 3, pch = c(19, 21)[j], bg = c("black", "white")[j]) #draw the step (black first)
                                                                          #black as solid circle, while as filled circle
      if(j == 1){
        black[[i]] = c(l$x, l$y)
        win = sapply(black, judge_five, black)
        if(is.element(1, win)){
          cat("Black Wins!\n")
          return(-1)
        }
      }

      if(j == 2){
        white[[i]] = c(l$x, l$y)
        win = sapply(white, judge_five, white)
        if(is.element(1, win)){
          cat("White Wins!\n")
          return(-1)
        }
      }

      if (2*(i) >= n^2) break #break when the chessboard has been filled
    }
    i = i+1 #enter the next round
    if (2*(i-1) >= n^2) break #bread when the chessboard has been filled
  }
}

judge_five = function(x, location){
  x1 = c((x[1]-2):(x[1]+2))
  y1 = c((x[2]-2):(x[2]+2))
  line1 = line(x1, y1)

  x2 = x1
  y2 = rev(y1)
  line2 = line(x2, y2)

  x3 = rep(x[1], 5)
  y3 = y1
  line3 = line(x3, y3)

  x4 = x1
  y4 = rep(x[2], 5)
  line4 = line(x4, y4)

  line = list(line1, line2, line3, line4)

  for(i in 1:4){
    judge = sum(is.element(line[[i]], location))
    if (judge == 5)
      return(1)
    if(i == 4)
      return(0)
  }
}


line = function(x,y){
  a = list()
  for(i in 1:5){
    a[[i]] = c(x[i],y[i])
  }
  return(a)
}

