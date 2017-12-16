#' @title A classic chessborad game. 
#' 
#' @description Play with your friend or play with computer.
#' Player who has five stones in a row wins.
#' Players choose a position to locate their stones alternatively, with 
#' black going first.
#' Player who first has five continuous stones in a row (horizontally, 
#' vertically or diagonally).
#' 
#' @param n numbers of rows and columns in the chessboard. The default value
#' is 19, which is the standard size of a gomoku chessboard.
#' 
#' @note (1)Please do not roll the mouse scroll after the graphics device pops out.
#'       (2)Please close the game in a normal way instead of closing the graphics
#' device directly.
#'       (3)The value of n should be larger than 5 so the board can accomadate at
#'       least five stones.
<<<<<<< HEAD
#'       (4)The value of code{n} should be an odd number.
#'       
#' @return If you keep playing the game, nothing will be returned; If you close
#' the game by clicking "quit", a table containing all the results will be displayed;
#' if there are no results, a character string "Game Closed, No Record" will return.       
=======
#'       (4)The value of n should be an odd number.
>>>>>>> 347a4292c932e9b0d80c5d6c39b9d9eb6cb42888
#' 
#' @author Yimo Zhang
#' 
#' @references /url{https://github.com/yihui/fun/blob/master/R/gomoku.R}, where
#' the basic idea of this function came from.
#' /url{http://www.sohu.com/a/156507478_466874}, where the idea of ploting taiji.png
#' is borrowd.
#' 
#' 
#' @return If you keep playing the game, nothing will be returned; If you close
#' the game in a normal way (click quit to stop), a character string "Game Closed" will be returned.
#' @export   
#' @example gomoku(n = 21)


gomoku = function(n = 19)
{
  
  if (!interactive()) return() #check if R is running interactively; if not, quit the game
  if(n < 5) stop("Hmm, n is too small for the game to play!")
  if(n %% 2 < 1) stop("Sorry, n must be a odd number!")
  
<<<<<<< HEAD
  result_table = tibble("Date"= as.character(Sys.Date()), "Time" = as.character(format(Sys.time(), "%X")), "Game" = "None", "Result" = "None")

  #Choose to play with your friend or computer
=======
  x11()
  
  
  install_packages = function(names)
  {
    for(name in names)
    {
      if (!(name %in% installed.packages()))
        install.packages(name, repos="http://cran.us.r-project.org")
      
      library(name, character.only=TRUE)
    }
  }
  
  install_packages(c("stringr","ggplot2","Cairo","ggmap","grid","scales","png","jpeg"))
  
  
>>>>>>> 347a4292c932e9b0d80c5d6c39b9d9eb6cb42888
  first_choose = function(){
    repeat{
      options(locatorBell = FALSE)
      l = locator(1)
      x = l$x
      y = l$y
      if(x>37 & x<63 & y>23 & y<31){return(1)}
      if(x>30 & x<70 & y>8 & y<16){return(2)}
    }
  }
  
  
  second_choose = function(){
    repeat{
      options(locatorBell = FALSE)
      l = locator(1)
      x = l$x
      y = l$y
      if(x>42 & x<58 & y>23 & y<31){return(3)}
      if(x>40 & x<60 & y>8 & y<16){return(4)}
    }
  }
  
  
  third_choose = function(){
    repeat{
      options(locatorBell = FALSE)
      l = locator(1)
      x = l$x
      y = l$y
      if(x>40 & x<61 & y>23 & y<31){return(5)}
      if(x>39 & x<61 & y>8 & y<16){return(6)}
    }
  }
########################Backstage functions############################### 
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
  
  #check if wins
  if_win = function(set){
    win = sapply(set, judge_five, set)#check if there are five continuous points in black chessmen set
    return(is.element(1, win))
  }
  
  #player play
  player_play = function(playlist, n){
    repeat {
      options(locatorBell = FALSE)
      l = locator(1)
      l$x <- min(n, max(1, round(l$x))) #modify the x-location to where nearest point
      l$y <- min(n, max(1, round(l$y))) #modify the y-location to where nearest point
      xy <- paste(l, collapse = ":") #record the step
      if (!is.element(xy, playlist)) #break when the point had chessman on it
        break
    }
    return(l)
  }
  
  #computer play
  computer_play = function(player, computer, playlist, n){
    get_4 = get_function(4, player, computer, n)
    if(!is.list(get_4)){get_3 = get_function(3, player, computer, n)} else{return(get_4)}
    if(!is.list(get_3)){get_2 = get_function(2, player, computer, n)} else{return(get_3)}
    if(!is.list(get_2)){get_1 = get_function(1, player, computer, n)} else{return(get_2)}
    if(!is.list(get_1)){
      repeat{
        new = list(c(sample(1:n,1),sample(1:n,1)))
        xy <- paste(new, collapse = ":") #record the step
        if (!is.element(xy, playlist)) #break when the point had chessman on it
          break
      } }else{return(get_1)}
    return(new)
  }
  
  
  
  
  #Get the avalible spot; if not available , return 0
  get_function = function(num, set1, set2 = NULL, n = NULL){
    get_num = lapply(set1, judge, num, set1, set2, n)
    temp = sapply(get_num, is.list)
    if(is.element(1, temp)){
      equal_to_one = which(temp == 1)
      get_one = sample(1:length(equal_to_one),1)
      get_list = get_num[[equal_to_one[get_one]]]
    } else {return (0)}
    return(get_list)
  }
  
  
  #Check if there are num continuous points
  judge = function(x, num, set1, set2 = NULL, n = NULL){
    #line1 is a y=x kind of line, the point x is the middle of the five points
    x1 = c(x[1]:(x[1]+num-1))
    y1 = c(x[2]:(x[2]+num-1))
    x1 = as.double(x1)
    y1 = as.double(y1)
    line1 = make_line(x1, y1)
    
    #line2 is a y=-x kind of line, the point x is the middle of the five points
    x2 = x1
    y2 = c(x[2]:(x[2]-num+1))
    line2 = make_line(x2, y2)
    
    #line3 is a horizontal line, the point x is the middle of the five points
    x3 = rep(x[1], num)
    y3 = y1
    line3 = make_line(x3, y3)
    
    #line4 is a vertical line, the point x is the middle of the five points
    x4 = x1
    y4 = rep(x[2], num)
    line4 = make_line(x4, y4)
    
    #save the lines in a list
    line = list(line1, line2, line3, line4)
    
    #check if there are five continuous points in the set
    
    for(i in 1:4){
      judge = sum(is.element(line[[i]], set1))
      temp = check_blank(num, i, x, set2, set1, n)
      if(judge == num & is.list(temp))
        return(temp)
      if(i==4)
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
  
  #Check if the near points are filled
  check_blank = function(num_point, index, point, location, location2, n){
    i = num_point
    x = point
    if(index == 1)
    {
      left = list(c(x[1]-1, x[2]-1))
      right = list(c(x[1]+i, x[2]+i))
    }
    if(index == 2)
    {
      left = list(c(x[1]-1, x[2]+1))
      right = list(c(x[1]+i, x[2]-i))
    }
    if(index == 3)
    {
      left = list(c(x[1], x[2]-1))
      right = list(c(x[1], x[2]+i))
    }
    if(index == 4){
      left = list(c(x[1]-1, x[2]))
      right = list(c(x[1]+i, x[2]))
    }
    
    left = list(as.double(unlist(left)))
    right = list(as.double(unlist(right)))
    if_left = sum(is.element(left, location),!within_boundary(left, n),is.element(left, location2))
    if_right = sum(is.element(right, location),!within_boundary(right, n), is.element(right,location2))
    if(if_left == 0){return(left)}
    if(if_right == 0){return(right)}
    return(0)
  }
  
  
  #Check if the point is located inside the chessboard
  within_boundary = function(x, n){
    x = unlist(x)
    judge_l = x<1
    judge_r = x>n
    if(sum(judge_l)>0 || sum(judge_r)>0){return(0)}
    else{return(1)}
  }
  
  #check availability
  check_blank_hard = function(num_point, index, point, player, computer, n){
    i = num_point
    x = point
    if(index == 1)
    {
      left = list(c(x[1]-1, x[2]-1))
      right = list(c(x[1]+i, x[2]+i))
    }
    if(index == 2)
    {
      left = list(c(x[1]-1, x[2]+1))
      right = list(c(x[1]+i, x[2]-i))
    }
    if(index == 3)
    {
      left = list(c(x[1], x[2]-1))
      right = list(c(x[1], x[2]+i))
    }
    if(index == 4)
    {
      left = list(c(x[1]-1, x[2]))
      right = list(c(x[1]+i, x[2]))
    }
    
    left = list(as.double(unlist(left)))
    right = list(as.double(unlist(right)))
    if_left = sum(is.element(left, player),!within_boundary(left, n),is.element(left, computer))
    if_right = sum(is.element(right, player),!within_boundary(right, n), is.element(right,computer))
    
    if(if_left + if_right == 0){return(list(left, right, 2))}
    if(if_left == 0){return(list(left, 1))}
    if(if_right == 0){(return(list(right, 1)))}
    return(0)
  }
  
  
  #Get the avalible spot; if not available , return 0
  get_function_hard = function(num, player, computer, n = NULL){
    get_num = lapply(player, judge_hard, num, player, computer, n)
    temp = sapply(get_num, length)
    two_ava = which(temp == 3)
    one_ava = which(temp == 2)
    if(length(two_ava) > 0){
      get_one = sample(1:length(two_ava),1)
      get_list = get_num[[two_ava[get_one]]]
      random = sample(1:2, 1)
      final = get_list[[random]]
      return(final)
    }
    if(length(one_ava) > 0){
      get_one = sample(1:length(one_ava),1)
      get_list = get_num[[one_ava[get_one]]]
      final = get_list[[1]]
      return(final)
    }
    else {return (0)}
  }   
  
  #Check if the point is located inside the chessboard
  within_boundary = function(x, n){
    x = unlist(x)
    judge_l = x<1
    judge_r = x>n
    if(sum(judge_l)>0 || sum(judge_r)>0){return(0)}
    else{return(1)}
  }
  
  ##Generate five points based on their x-value and y-value
  make_line = function(x,y){
    a = list()
    for(i in 1:length(x)){
      a[[i]] = c(x[i],y[i])
    }
    return(a)
  }
  
  #computer play
  computer_play_hard = function(player, computer, playlist, n){
    m = n+1
    i = 0
    get_4 = get_function_hard(4, player, computer, n)
    if(!is.list(get_4)){get_3 = get_function_hard(3, player, computer, n)} else{return(get_4)}
    if(!is.list(get_3)){get_2 = get_function_hard(2, player, computer, n)} else{return(get_3)}
    if(!is.list(get_2)){
      repeat{
        spot = vector()
        left = m/2 - i
        right = m/2 +i
        x = rep(left:right,2*i+1)
        y = rep(left:right, each = 2*i+1)
        for(j in 1:length(x)){
          spot[[j]] = paste(list(x[j],y[j]), collapse = ":")
        }
        i = i+1
        index = is.element(spot, playlist)
        temp = which(index == 0)
        if (length(temp)>0) #break when the point had chessman on it
          break
      } }else{return(get_2)}
    random = sample(temp,1)
    return(list(c(x[random],y[random])))
  }
  
  
  
  #Check if there are num continuous points
  judge_hard = function(x, num, player, computer, n = NULL){
    #line1 is a y=x kind of line, the point x is the middle of the five points
    x1 = c(x[1]:(x[1]+num-1))
    x1 = as.double(x1)
    y1 = c(x[2]:(x[2]+num-1))
    y1 = as.double(y1)
    line1 = make_line(x1, y1)
    
    #line2 is a y=-x kind of line, the point x is the middle of the five points
    x2 = x1
    y2 = c(x[2]:(x[2]-num+1))
    line2 = make_line(x2, y2)
    
    #line3 is a horizontal line, the point x is the middle of the five points
    x3 = rep(x[1], num)
    y3 = y1
    line3 = make_line(x3, y3)
    
    #line4 is a vertical line, the point x is the middle of the five points
    x4 = x1
    y4 = rep(x[2], num)
    line4 = make_line(x4, y4)
    
    #save the lines in a list
    line = list(line1, line2, line3, line4)
    
    #check if there are five continuous points in the set
    get_num = list()
    
    for(i in 1:4){
      if_line = sum(is.element(line[[i]], player))
      if(if_line == num)
        get_num[[i]] = check_blank_hard(num, i, x, player, computer, n)
      else {get_num[[i]] = 0}
    }
    
    temp = sapply(get_num, length)
    two_ava = which(temp == 3)
    one_ava = which(temp == 2)
    
    if(length(two_ava) > 0){
      get_one = sample(1:length(two_ava),1)
      get_list = get_num[[two_ava[get_one]]]
      return(get_list)
    }
    if(length(one_ava) > 0){
      get_one = sample(1:length(one_ava),1)
      get_list = get_num[[one_ava[get_one]]]
      return(get_list)
    }
    return(0)
  }
  #######################Backstage function end###########################
  
  #######################plot function####################################
  stage0 = function(){
    
    n = 100
    x=c(1:n)
    y=c(1:n)
    taiji = readPNG(system.file("img","taiji.png",package = "LittleGames"))
    windowsFonts(JP1 = windowsFont("Pristina"))
    bg = readJPEG(system.file("img","wood.jpg",package = "LittleGames"))
    colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
    par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(bg,0,0,1+n,1+n)
    text(x = seq(32,68, length.out = 6), y=rep(8*n/9,6), col = colfunc(6),
         cex = 3.5, label = unlist(strsplit("GOMOKU", NULL)), family = "JP1", lwd = 2.5)
    text(x = 50, y = 28, col = "black", label = "BATTLE", family = "JP1", cex =2.5, lwd = 2.5)
    text(x = 50, y = 13, col = "white", family = "JP1", cex = 2.5, label = "COMPUTER", lwd = 2.5)
    rasterImage(taiji,30, 35, 70, 75)
  }
  
  
  stage1 = function(){
    taiji = readPNG(system.file("img","taiji.png",package = "LittleGames"))
    windowsFonts(JP1 = windowsFont("Pristina"))
    bg = readJPEG(system.file("img","wood.jpg",package = "LittleGames"))
    colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
    colfunc1 = colorRampPalette(c("black","gray90"))
    n = 100
    x=c(1:n)
    y=c(1:n)
    par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(bg,0,0,1+n,1+n)
    text(x = seq(25,75, length.out = 10), y=rep(8*n/9,10), col = colfunc(10),
         cex = 3.5, label = unlist(strsplit("DIFFICULTY", NULL)), family = "JP1", lwd = 2.5)
    text(x = 50, y = 28, label = "EASY", col = "black", family = "JP1", cex = 2.5, lwd = 2.5)
    text(x = 50, y = 13, label = "HARD", col = "white", cex=2.5, family = "JP1", lwd = 2.5)
    rasterImage(taiji,30, 35, 70, 75)
  }
  
  stage2 = function(){
    taiji = readPNG(system.file("img","taiji.png",package = "LittleGames"))
    windowsFonts(JP1 = windowsFont("Pristina"))
    bg = readJPEG(system.file("img","wood.jpg",package = "LittleGames"))
    colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
    colfunc1 = colorRampPalette(c("black","gray90"))
    n = 100
    x=c(1:n)
    y=c(1:n)
    par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))
    rasterImage(bg,0,0,1+n,1+n)
    text(x = seq(25,75, length.out = 8), y=rep(8*n/9,8), col = colfunc(8),
         cex = 3.5, label = unlist(strsplit("COMPUTER", NULL)), family = "JP1", lwd = 2.5)
    text(x = 50, y = 28, label = "BLACK", col = "black", family = "JP1", cex = 2.5, lwd = 2.5)
    text(x = 50, y = 13, label = "WHITE", col = "white", cex=2.5, family = "JP1", lwd = 2.5)
    rasterImage(taiji,30, 35, 70, 75)
    
  }
  
  gameover = function(result, img){
    colfunc <- colorRampPalette(c("white","goldenrod3", "white","goldenrod3","white"))
    colfunc1 = colorRampPalette(c("black","gray90"))
    windowsFonts(JP1 = windowsFont("Pristina"))
    bg = readJPEG(system.file("img","wood.jpg",package = "LittleGames"))
    n = 100
    x=c(1:n)
    y=c(1:n)
    par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))#add points to the plot where the lines should be located
    rasterImage(bg,0,0,1+n,1+n)
    text(x = 50, y = 28, label = "PLAY AGAIN", col = "black", family = "JP1", cex = 2.5, lwd = 2.5)
    text(x = 50, y = 13, label = "QUIT", col = "white", cex = 2.5, family = "JP1", lwd = 2.5)
    rasterImage(img, 30, 35, 70, 75)
    
    if(result == "White Wins!"){text(x = 50, y = 8*n/9, label = toupper(result), cex = 3.5, col = "white", family = "JP1")
    }
    if(result == "Black Wins!"){text(x = 50, y = 8*n/9, label = toupper(result), cex = 3.5, col = "black", family = "JP1")
    }
    if(result == "You Win!"){
      result = toupper(result)
      len = str_count(result)
      text(x = seq(25,75, length.out = len), y = rep(8*n/9,len), label = unlist(strsplit(result, NULL)), cex = 3.5, col = colfunc(len), family = "JP1", lwd = 5)
    }
    if(result == "You Lose!"){
      result = toupper(result)
      len = str_count(result)
      text(x = seq(25,75, length.out = len), y = rep(8*n/9,len), label = unlist(strsplit(result, NULL)), cex = 3.5, col = colfunc1(len), family = "JP1", lwd = 2.5)
    }
  }  
#############################Plot functions over########################################
  
  
############################Begin functions###########################################
  gomoku_self <- function(n = 19) {
    
    #Setting of the game
    img = readJPEG(system.file("img","wood.jpg",package = "LittleGames"))
    par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))#add points to the plot where the lines should be located
    rasterImage(img,0,0,1+n,1+n)
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
          options(locatorBell = FALSE)
          l = locator(1)
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
            return("Black Wins!")
          }
        }
        
        #check the white chessman
        if(j == 2){
          white[[i]] = c(l$x, l$y)#update the white chessmen set
          if(if_win(white)==1){
            return("White Wins!")
          }
        }
        
        if (2*(i) >= n^2) break #break when the chessboard has been filled
      }
      i = i+1 #enter the next round
      if (2*(i-1) >= n^2) break #bread when the chessboard has been filled
    }
  }
  
  
  gomoku_easy <- function(n = 19, choose = 1) {
    
    
    if (!interactive()) return() #check if R is running interactively; if not, quit the game
    if(n < 5) stop("Hmm, n is too small for the game to play!")
    if(n %% 2 < 1) stop("Sorry, n must be a odd number!")
    #Setting of the game
    img = readJPEG(system.file("img","wood.jpg",package = "LittleGames"))
    par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))#add points to the plot where the lines should be located
    rasterImage(img,0,0,1+n,1+n)
    segments(1, 1:n, n, 1:n)#draw horizontal lines
    segments(1:n, 1, 1:n, n)#draw vertical lines
    temp = c(round((n+1)/5),(n+1)/2, round(4*(n+1)/5),round(4*(n+1)/5))
    points(rep(temp, 3), rep(temp, each = 3),
           pch = 19, cex = 6/sqrt(n))#draw the black point with the shape of solid circle
    box() #draw the outline of the plot
    
    
    
    #Playing the game
    l = list()
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
          if(if_win(player)){
            return("You Win!")
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
            return("You Lose!")
          }
          j = j+2
        }
        if(choose == 2){
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
            return("You Lose!")
          }
          #player play
          l = player_play(playedlist, n)
          xy <- paste(l, collapse = ":")
          playedlist <- c(playedlist, xy) #add the step to the playlist if it is successfully played
          points(l, cex = 3*19/n, pch = c(19, 21)[choose], bg = c("black", "white")[choose]) #draw the step (black first)
          #black as solid circle, while as filled circle
          
          #check if player wins
          player[[i]] = c(l$x, l$y)
          if(if_win(player)){
            return("You Win!")
          }
          j = j+2
        }
        if (2*(i) >= n^2) break #break when the chessboard has been filled
        
        i = i+1 #enter the next round
        if (2*(i-1) >= n^2) break #bread when the chessboard has been filled
      }
    }
  }
  
  
  gomoku_hard <- function(n = 19, choose = 1) {
    
    
    if (!interactive()) return() #check if R is running interactively; if not, quit the game
    if(n < 5) stop("Hmm, n is too small for the game to play!")
    if(n %% 2 < 1) stop("Sorry, n must be a odd number!")
    #Setting of the game
    img = readJPEG(system.file("img","wood.jpg",package = "LittleGames"))
    par(mar = rep(0, 4)) #No blank space for the main plot and the margin of plot
    plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "",
         ylab = "", bty = "o", lab = c(n, n, 1))#add points to the plot where the lines should be located
    rasterImage(img,0,0,1+n,1+n)
    segments(1, 1:n, n, 1:n)#draw horizontal lines
    segments(1:n, 1, 1:n, n)#draw vertical lines
    temp = c(round((n+1)/5),(n+1)/2, round(4*(n+1)/5),round(4*(n+1)/5))
    points(rep(temp, 3), rep(temp, each = 3),
           pch = 19, cex = 6/sqrt(n))#draw the black point with the shape of solid circle
    box() #draw the outline of the plot
    
    
    
    #Playing the game
    l = list()
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
          if(if_win(player)){
            return("You Win!")
          }
          
          
          #computer play
          new = computer_play_hard(player, computer, playedlist, n)
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
            return("You Lose!")
          }
          j = j+2
        }
        if(choose == 2){
          #computer play
          new = computer_play_hard(player, computer, playedlist, n)
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
            return("You Lose!")
          }
          #player play
          l = player_play(playedlist, n)
          xy <- paste(l, collapse = ":")
          playedlist <- c(playedlist, xy) #add the step to the playlist if it is successfully played
          points(l, cex = 3*19/n, pch = c(19, 21)[choose], bg = c("black", "white")[choose]) #draw the step (black first)
          #black as solid circle, while as filled circle
          
          #check if player wins
          player[[i]] = c(l$x, l$y)
          if(if_win(player)){
            return("You Win!")
          }
          j = j+2
        }
        if (2*(i) >= n^2) break #break when the chessboard has been filled
        
        i = i+1 #enter the next round
        if (2*(i-1) >= n^2) break #bread when the chessboard has been filled
      }
    }
  }
###############################Begin functions over#####################################3  
  
  
  
  gomoku_begin = function(n, r_table){
  x11() #open the graphics  
  
  click = 0
  result = ""
  
  #Choose whom to play wit
  stage0()
  
  click = first_choose()
  
  if(click == 1){result = gomoku_self(n)}
  
  
  if(click == 2){
    #Choose color
    stage1()
    click = second_choose()
    
    if(click == 3){
      stage2()
      click = third_choose()
      if(click == 5){result = gomoku_easy(choose = 1)}
      if(click == 6){result = gomoku_easy(choose = 2)}
    }
    
    if(click == 4){
      stage2()
      click = third_choose()
      if(click == 5){result = gomoku_hard(choose = 1)}
      if(click == 6){result = gomoku_hard(choose = 2)}
    }
  }
  

  if(result == "White Wins!"){
    img = readPNG(system.file("img","white_wins.png", package="LittleGames"))
    game = "Battle"
    }
  if(result == "Black Wins!"){
    img = readPNG(system.file("img","black_wins.png",package = "LittleGames"))
    game = "Battle"
    }
  if(result == "You Win!"){
    img = readPNG(system.file("img","happy_face.png", package = "LittleGames"))
    game = "Computer"
    }
  if(result == "You Lose!"){
    img = readPNG(system.file("img","sad_face.png",package = "LittleGames"))
    game = "Computer"
    }
  
  
  result_table <<- result_table%>%
    rbind(c(as.character(Sys.Date()), as.character(format(Sys.time(), "%X")), game, result))
  
  
  
  gameover(result = result, img = img)
  repeat{
    options(locatorBell = FALSE)
    l=locator(1)
    x = l$x
    y = l$y
<<<<<<< HEAD
    if(x>40 & x<60 & y>8 & y<16){return(result_table)}
=======
    if(x>40 & x<60 & y>8 & y<16){
      dev.off()
      return("Game Closed")}
>>>>>>> 347a4292c932e9b0d80c5d6c39b9d9eb6cb42888
    if(x>32 & x<69 & y>23 & y<31){
      dev.off()
      return(gomoku_begin(n, result_table))}
  }
  }
  #############################gomoku begin#############################  
  
  
  r_table = gomoku_begin(n, result_table)
  if(nrow(r_table) == 1){return("Game Closed, No Record")}
  else{
    r_table = r_table%>%
      filter(Game != "None")
    return(r_table)
  }
  
}









