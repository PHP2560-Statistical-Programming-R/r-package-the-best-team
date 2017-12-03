gomoku <- function(n = 19) {


  if (!interactive()) return() #check if R is running interactively; if not, quit the game
  if(n < 5) stop("Hmm, n is too small for the game to play!")
  if(n %% 2 < 1) stop("Sorry, n must be a odd number!")
  
#Setting of the game
  par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
       ylab = "", bty = "o", lab = c(n, n, 1))#add points to the plot where the lines should be located
  segments(1, 1:n, n, 1:n)#draw horizontal lines
  segments(1:n, 1, 1:n, n)#draw vertical lines
  temp = c(round((n+1)/5),(n+1)/2, round(4*(n+1)/5),round(4*(n+1)/5))
  points(rep(temp, 3), rep(temp, each = 3),
         pch = 19, cex = 6/sqrt(n))#draw the black point with the shape of solid circle
  box() #draw the outline of the plot
  
  
#Playing the game
  playedlist <- NULL #record the points have been stepped on
  i <- 1 #rounds that will have be played
  black = list() #record the black chessman
  white = list() #record the while chessman
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
      points(l, cex = 3*19/n, pch = c(19, 21)[j], bg = c("black", "white")[j]) #draw the step (black first)
                                                                          #black as solid circle, while as filled circle
      #check the black chessmen
      if(j == 1){
        black[[i]] = c(l$x, l$y)#update the black chessmen set
        if(if_win(black)){
          cat("Black Wins!\n") #if there are, game over, black wins
          return(-1)
        }
      }

      #check the white chessman
      if(j == 2){
        white[[i]] = c(l$x, l$y)#update the white chessmen set
        if(if_win(white)==1){
          cat("White Wins!\n")#If there are, game over, white wins
          return(-1)
        }
       
      }
      
      if (2*(i) >= n^2) break #break when the chessboard has been filled
    }
    i = i+1 #enter the next round
    if (2*(i-1) >= n^2) break #bread when the chessboard has been filled
  }
}

##Judging if there is a winner
judge_five = function(x, location){
  #line1 is a y=x kind of line, the point x is the middle of the five points
  x1 = c((x[1]-2):(x[1]+2))
  x1 = as.double(x1)
  y1 = c((x[2]-2):(x[2]+2))
  y1 = as.double(y1)
  line1 = make_line(x1, y1)
  
  #line2 is a y=-x kind of line, the point x is the middle of the five points
  x2 = x1
  y2 = rev(y1)
  line2 = make_line(x2, y2)
  
  #line3 is a horizontal line, the point x is the middle of the five points
  x3 = rep(x[1], 5)
  y3 = y1
  line3 = make_line(x3, y3)
  
  #line4 is a vertical line, the point x is the middle of the five points
  x4 = x1
  y4 = rep(x[2], 5)
  line4 = make_line(x4, y4)

  #save the lines in a list
  line = list(line1, line2, line3, line4)
  
  #check if there are five continuous points in the set
  for(i in 1:4){
    judge = sum(is.element(line[[i]], location))
    if (judge == 5)
      return(1)
    if(i == 4)
      return(0)
  }
}

##Generate five points based on their x-value and y-value
make_line = function(x,y){
  a = list()
  for(i in 1:length(x)){
    a[[i]] = c(x[i],y[i])
  }
  return(a)
}

