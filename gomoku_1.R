gomoku <- function(n = 19) {
  
  
  if (!interactive()) return() #check if R is running interactively; if not, quit the game
  if(n < 5) stop("Hmm, n is too small for the game to play!")
  if(n %% 2 < 1) stop("Sorry, n must be a odd number!")
  if (.Platform$OS.type == "windows") 
    x11() else x11(type = "Xlib")
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
  playedlist <- c("0:0") #record the points have been stepped on
  i <- 1 #rounds that will have be played
  black = list() #record the black chessman
  white = list() #record the while chessman
  chess_color = c("black", "white")
  player = get(chess_color[choose])
  computer = get(chess_color[3-choose])
  repeat {
    for (j in 1:2) {
      if(choose == 1){
        #player play
      l = player_play(playedlist, n)
      xy <- paste(l, collapse = ":")
      playedlist <- c(playedlist, xy) #add the step to the playlist if it is successfully played
      points(l, cex = 3*19/n, pch = c(19, 21)[choose], bg = c("black", "white")[choose]) #draw the step (black first)
      #black as solid circle, while as filled circle
      
      #check if player wins
      player[[i]] = c(l$x, l$y)
      print(player)
      if(if_win(player)){
        cat("Player Wins!\n") #if there are, game over, black wins
        return(-1)
      }
      
      
      #computer play
      new = computer_play(player, computer, playedlist, n)
      new = unlist(new)
      l$x = new[1]
      l$y = new[2]
      xy <- paste(l, collapse = ":")
      playedlist <- c(playedlist, xy)
      points(l, cex = 3*19/n, pch = c(19, 21)[3-choose], bg = c("black", "white")[3-choose]) #draw the step (black first)
      #black as solid circle, while as filled circle
      
      #check if computer wins
      computer[[i]] = c(l$x, l$y)#update the black chessmen set
      
      if(if_win(computer)){
        cat("Computer Wins!\n") #if there are, game over, black wins
        return(-1)
      }
      j = j+2
      }
      else{
        #computer play
        new = computer_play(player, computer, playedlist, n)
        new = unlist(new)
        l$x = new[1]
        l$y = new[2]
        xy <- paste(l, collapse = ":")
        playedlist <- c(playedlist, xy)
        points(l, cex = 3*19/n, pch = c(19, 21)[3-choose], bg = c("black", "white")[3-choose]) #draw the step (black first)
        #black as solid circle, while as filled circle
        
        #check if computer wins
        computer[[i]] = c(l$x, l$y)#update the black chessmen set
        
        if(if_win(computer)){
          cat("Computer Wins!\n") #if there are, game over, black wins
          return(-1)
        }
        #player play
        l = player_play(playedlist)
        xy <- paste(l, collapse = ":")
        playedlist <- c(playedlist, xy) #add the step to the playlist if it is successfully played
        points(l, cex = 3*19/n, pch = c(19, 21)[choose], bg = c("black", "white")[choose]) #draw the step (black first)
        #black as solid circle, while as filled circle
        
        #check if player wins
        player[[i]] = c(l$x, l$y)
        if(if_win(player)){
          cat("Player Wins!\n") #if there are, game over, black wins
          return(-1)
        }
        j = j+2
      }
  
      
      
      
      
      
      
      if (2*(i) >= n^2) break #break when the chessboard has been filled

    i = i+1 #enter the next round
    if (2*(i-1) >= n^2) break #bread when the chessboard has been filled
    }
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

